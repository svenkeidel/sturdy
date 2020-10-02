{-# LANGUAGE Arrows #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC
  -fspecialise-aggressively
  -flate-specialise
  -fsimpl-tick-factor=500
  -fno-warn-orphans
  -fno-warn-partial-type-signatures
#-}
module Debug.Server.DebugServer where

import           Prelude hiding (not,Bounded,fail,(.),exp,read,IO)
import qualified Prelude as P

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Environment as Env
import qualified Control.Arrow.Fix as Fix
import           Control.Arrow.Fix.Chaotic(innermost,outermost)
import qualified Control.Arrow.Fix.Context as Ctx
--import           Control.Arrow.Fix.Stack as Stack
import           Control.Arrow.Fix.Stack (ArrowStack,ArrowStackDepth,ArrowStackElements,widenInput,maxDepth,reuseByMetric, StackPointer)
import qualified Control.Arrow.Fix.Stack as Stack
import           Control.Arrow.IO
import qualified Control.Arrow.Trans as Trans
import           Control.Arrow.Transformer.IO
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.FiniteEnvStore hiding (Env)
import           Control.Arrow.Transformer.Abstract.LogError
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Component as Comp
import           Control.Arrow.Transformer.Abstract.Fix.Context
import           Control.Arrow.Transformer.Abstract.Fix.Stack --as Stack
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable as Cache
import           Control.Arrow.Transformer.Abstract.Fix.Metrics as Metric
import           Control.Arrow.Transformer.Abstract.Fix.ControlFlow
import           Control.Arrow.Transformer.Abstract.Terminating
import           Control.Arrow.Reader as Reader
import           Data.Empty
import           Data.Label
import           Data.Text (Text)
import           Data.HashSet(HashSet)

import qualified Data.Abstract.Widening as W
import           Data.Abstract.Terminating(Terminating)

import           TypedAnalysis
import           Syntax (LExpr,Expr(App),Literal)
import           GenericInterpreter as Generic

--Websocket Server Imports
import qualified Control.Concurrent             as Concurrent
import qualified Control.Exception              as Exception
import qualified Control.Monad                  as Monad
import qualified Data.List                      as List
import qualified Data.Maybe                     as Maybe
import qualified Data.Text                      as Text
import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS
import qualified Safe

--JSON Parser Imports
import           Data.Aeson
import           Data.Text.Encoding
import           GHC.Generics
import           Data.Aeson.TH
import           Data.Text.Lazy.Encoding

import           Data.ByteString.Builder(toLazyByteString)
import           Parser
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString               as B
import           Control.Arrow.Transformer.Debug(DebugState(..), DebugT, ArrowDebug, sendMessage, receiveMessage, getState, getStep, setStep)
import           Control.Arrow.Transformer.State
import           Control.Arrow.Fix.ControlFlow
import           Text.Printf

--CFG Imports
import           Data.Graph.Inductive(Gr)
import           Data.Graph.Inductive.Graph(mkGraph, LNode, LEdge, labNodes, labEdges, Graph)


--Env/Store/Stack Imports
import           Data.Abstract.MonotoneStore as MS
import           Data.Abstract.MonotoneVersioned
import           Data.Hashable
import           Data.HashMap.Strict (HashMap, toList)
import           Data.Abstract.Closure (Closure)
import           Data.Hashed.Lazy (Hashed(..))
import           Data.HashMap.Strict (HashMap)

import qualified Debug.Trace as Debug

type InterpT c x y =
  (ValueT Val
    (TerminatingT
      (LogErrorT Text
        (EnvStoreT Text Addr Val
          (FixT 
           (DebugT
            (MetricsT Metric.Monotone In
             (ComponentT Comp.Component  In
               (StackT Stack In
                 (CacheT Cache.Monotone In Out
                   (ContextT Ctx
                     (ControlFlowT Expr
                       c)))))))))))) x y


-- |Typed for websocket connection
type ClientId = Int
type Client   = (ClientId, WS.Connection)
type State    = [Client]

data DebugMessage
  = LoadSourceCodeRequest {path :: String}                              -- |Client sends path of required source code
  | LoadSourceCodeResponse {code :: FilePath}                           -- |Server sends source code of file
  | StartDebuggerRequest {code :: String}                               -- |Debugging of the received code gets started
  | ContinueRequest {}                                                  -- |Server executes analysis until next breakpoint is reached
  | StepRequest {}                                                      -- |Server evaluates the next expression
  | BreakpointResponse                                                  -- |Server sends breakpoint response at each reached breakpoint 
                        {stack           :: [([(Text, Text)], [Text])], 
                        cfgNodes        :: [(Int,Text)],                
                        cfgEdges        :: [(Int, Int)], 
                        latestStore     :: [(Text,Text)], 
                        latestEnv       :: [(Text,Text)]}
  | RefreshRequest                                                      -- |Client sends refresh request to clear values and refresh debug state
  | RefreshResponse {success :: Bool}                                   -- |Was the refresh successful?
  | CurrentExpressionResponse {expr :: Text}                            -- |The expression that gets evaluated
  | EvaluatedExpressionResponse {expr :: Text, val :: Text}             -- |Evaluated expression and its value
  | ExceptionResponse {exception :: Text}                               -- |Exceptions
  deriving (Show, Generic)

instance ToJSON DebugMessage 
instance FromJSON DebugMessage 


---------------------     Websocket Server     ----------------------------

-- |Starts websocket server on port 3004
startServer :: P.IO ()
startServer = do
  putStrLn "Server started!"
  state <- Concurrent.newMVar []
  Warp.run 3004 $ WS.websocketsOr
    WS.defaultConnectionOptions
    (wsApp state)
    httpApp

-- |Initializes debugState and starts listening after a client connected
wsApp :: Concurrent.MVar State -> WS.ServerApp
wsApp stateRef pendingConn = do
  conn <- WS.acceptRequest pendingConn
  clientId <- connectClient conn stateRef
  WS.forkPingThread conn 30
  let debugState = DebugState conn clientId stateRef False
  Exception.finally
    (listen debugState)
    (disconnectClient clientId stateRef)

-- |Exception
httpApp :: Wai.Application
httpApp _ respond = respond $ Wai.responseLBS Http.status400 [] "Not a websocket request"

-- |Increases clientID
nextId :: State -> ClientId
nextId = Maybe.maybe 0 ((+) 1) . Safe.maximumMay . List.map fst

-- |Establish connection of client
connectClient :: WS.Connection -> Concurrent.MVar State -> P.IO ClientId
connectClient conn stateRef = Concurrent.modifyMVar stateRef $ \state -> do
  let clientId = nextId state
  return ((clientId, conn) : state, clientId)

-- |Helper function for disconnectClient
withoutClient :: ClientId -> State -> State
withoutClient clientId = List.filter ((/=) clientId . fst)

-- |Called after client disconnects
disconnectClient :: ClientId -> Concurrent.MVar State -> P.IO ()
disconnectClient clientId stateRef = Concurrent.modifyMVar_ stateRef $ \state ->
  return $ withoutClient clientId state

-- |Sends a websocket message
sendResponse :: DebugState -> Text.Text -> P.IO ()
sendResponse debugState msg = do
  WS.sendTextData (conn debugState) msg 

-- |Listening loop, evaluates received messages 
listen :: DebugState -> P.IO ()
listen debugState = do
  print ((show (clientId debugState)) ++ "connected!")         
  msg <- WS.receiveData (conn debugState)
  let decodedMessage = Maybe.fromJust (decode'' msg) :: DebugMessage
  case decodedMessage of

    -- |Respond with source code of required file
    LoadSourceCodeRequest { path = p } -> do
      contents <- Parser.loadSourceCode p
      expressions <- Parser.loadSchemeFile p
      let responseObject = encodeDebugMessage (LoadSourceCodeResponse (contents))
      sendResponse debugState responseObject
    
    -- |Start debugging with received code
    StartDebuggerRequest {code = code } -> do
      let ?sensitivity = 0
      let ?debugState = debugState
      expressions <- Parser.loadSchemeFileWithCode code
      evalDebug ([expressions])
    
    -- |Respond with exception
    ContinueRequest {} -> do
      let responseObject = encodeDebugMessage (ExceptionResponse "Debugging was not started!")
      sendResponse debugState responseObject

    -- |Respond with exception
    StepRequest {} -> do
      let responseObject = encodeDebugMessage (ExceptionResponse "Debugging was not started!")
      sendResponse debugState responseObject

    -- |Refresh debugState
    RefreshRequest -> do
      let responseObject = encodeDebugMessage (RefreshResponse True)
      sendResponse debugState responseObject
      Exception.finally
        (listen debugState)
        (disconnectClient (clientId debugState) (stateRef debugState))

  -- |Loop
  listen debugState



---------------------     Debugging Functions     ----------------------------


evalDebug :: (?sensitivity::Int, ?debugState::DebugState) => [LExpr] -> P.IO ()
evalDebug expr =
  let ?cacheWidening = (storeErrWidening, W.finite) in
  let ?fixpointAlgorithm = transform $
       Fix.fixpointAlgorithm $
         debug .
         Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of App _ _ l:_ -> Just l; _ -> Nothing) .
         recordControlFlowGraph' (\(_,(_,exprs)) -> case exprs of e':_ -> Just e'; _ -> Nothing) .
         Fix.filter' isFunctionBody innermost in
  do _ <- Trans.run (Generic.runFixed :: InterpT IO [Expr] Val) (?debugState, (empty, (empty, e0)))
     return ()
  where
    e0 = generate (sequence expr)
{-# INLINE evalDebug #-}


-- | Debugging combinator
debug :: (?debugState :: DebugState,
          ArrowChoice c, ArrowIO c, ArrowDebug c, ArrowStack In c, ArrowStackDepth c, ArrowStackElements In c, ArrowCFG (CFG Expr) c)
      => Fix.FixpointCombinator c ((TypedAnalysis.Store,Errors),(Env,[Expr]))
                                  ((TypedAnalysis.Store,Errors), Terminating Val)
debug f = proc input@((store,errors),(env,exprs)) -> do
  -- |Send current expression to client
  let currentExpressionResponse = createCurrentExpressionResponse exprs
  sendMessage -< (currentExpressionResponse)

  -- |Check if breakpoint or step reached
  step <- getStep -< ()
  case exprs of 
    e:es | isBreakpoint' e -> do
      loop -< ((store,errors),(env,es))
    _ | step -> do
      loop -< input
    _ -> do
      output <- f -< input
      let evaluatedExpressionResponse = createEvaluatedExpressionResponse input output
      sendMessage -< (evaluatedExpressionResponse)
      returnA -< output

  where
    loop = proc input@((store,errors),(env,exprs)) -> do
      setStep -< (False)

      -- |Get CFG and extract nodes + edges
      cfg <- getCFG -< ()
      let nodes = getNodes cfg
      let edges = getEdges cfg

      -- |Get Stack and extract latest environment + latest store + expressions and store of every stack element
      stackElems <- Stack.elems -< ()
      case stackElems of
        [] -> do
          liftIO print -< "Stack empty!"
          -- |Create breakpoint response without stack and send it to client
          let breakpointResponse = createBreakpointResponse [] nodes edges [] []
          sendMessage -< (breakpointResponse)
        _ -> do 
          let latestStore = getStore stackElems
          let latestEnv = getEnvironment stackElems
          let stack = extractStoreAndExprsFromStack stackElems  

          -- |Create breakpoint response and send it to client
          let breakpointResponse = createBreakpointResponse stack nodes edges latestStore latestEnv
          sendMessage -< (breakpointResponse)




      -- |Continue if continue or step were called
      msg <- receiveMessage -< ()
      let decodedMessage = decodeTextMessage msg
      case decodedMessage of 
        ContinueRequest {} -> do
          output <- f -< input
          let evaluatedExpressionResponse = createEvaluatedExpressionResponse input output
          sendMessage -< (evaluatedExpressionResponse)
          returnA -< output
        StepRequest {} -> do
          setStep -< (True)
          output <- f -< input
          let evaluatedExpressionResponse = createEvaluatedExpressionResponse input output
          sendMessage -< (evaluatedExpressionResponse)
          returnA -< output
        _ -> do
          loop -< input 

{-# INLINE debug #-}





---------------------     Helper Functions     ----------------------------

decodeTextMessage :: Text.Text -> DebugMessage
decodeTextMessage msg = Maybe.fromJust (decode'' msg) :: DebugMessage

decode'' :: FromJSON a => Text.Text -> Maybe a
decode'' = decode . toLazyByteString . Data.Text.Encoding.encodeUtf8Builder

toStrict1 :: BL.ByteString -> B.ByteString
toStrict1 = B.concat . BL.toChunks

-- |Check if expression is breakpoint
isBreakpoint' :: Expr -> Bool
isBreakpoint' expr  
  | (show $ expr) == "breakpoint" = True
  | otherwise = False

createBreakpointResponse :: [([(Text, Text)], [Text])] -> [(Int,Text)] -> [(Int, Int)] -> [(Text,Text)] -> [(Text,Text)] -> Text.Text
createBreakpointResponse stack nodeList edgeList latestStore latestEnv = Data.Text.Encoding.decodeUtf8 (toStrict1 (encode (BreakpointResponse (stack) (nodeList) (edgeList) (latestStore) (latestEnv))))

createCurrentExpressionResponse :: [Expr] -> Text.Text
createCurrentExpressionResponse exprs = Data.Text.Encoding.decodeUtf8 (toStrict1 (encode (CurrentExpressionResponse (Text.pack(show exprs)))))

createEvaluatedExpressionResponse :: In -> Out -> Text.Text
createEvaluatedExpressionResponse (((_, _), (_, exprs))) (((_, _), (val))) = Data.Text.Encoding.decodeUtf8 (toStrict1 (encode (EvaluatedExpressionResponse (Text.pack(show exprs)) (Text.pack(show val)))))

getNodes :: forall stmt. (Show stmt) => (CFG stmt) -> [(Int, Text)]   
getNodes (CFG expr) = mapNodes (labNodes (expr))

getEdges :: (CFG stmt) -> [(Int,Int)] 
getEdges (CFG expr) = mapEdges (labEdges expr)

mapNodes :: forall stmt. (Show stmt) => [LNode stmt] -> [(Int, Text)]
mapNodes nodeList = map mapSingleNode nodeList 

mapSingleNode :: forall stmt. (Show stmt) =>  LNode stmt -> (Int, Text)
mapSingleNode (id, label) = (id, (Text.pack(show label)))

mapEdges :: [LEdge ()] -> [(Int,Int)]
mapEdges edgeList = map mapSingleEdge edgeList

mapSingleEdge :: LEdge () -> (Int, Int)
mapSingleEdge (first, second, _) = (first, second)

extractStoreAndExprsFromStack :: [In] -> [([(Text, Text)], [Text])]
extractStoreAndExprsFromStack list = map extractStoreAndExprsFromStackElem list

extractStoreAndExprsFromStackElem :: In -> ([(Text, Text)], [Text])
extractStoreAndExprsFromStackElem stackElem =  (uglifyStore (extractStoreFromStackElem stackElem) ,(extractExprsFromStackElem stackElem))

extractExprsFromStackElem :: In -> [Text]
extractExprsFromStackElem (((_, _), (_, expr))) = map (Text.pack . show) expr

extractStoreFromStackElem :: In -> [(Addr,Val)]            
extractStoreFromStackElem ((((MS.Store (Versioned map _)), _), (_, _))) = toList map

extractEnvFromStackElem :: In -> [(Text, Addr)]            
extractEnvFromStackElem (((_, _), ((Hashed hashMap _), _))) = toList hashMap

-- |Get last element of list
listLast :: [a] -> a
listLast [x] = x
listLast (_:xs) = listLast xs
listLast [] = error "Can't do last of an empty list!"

convertStore :: [(Addr,Val)] -> [(Text,Text)]
convertStore list = map convertStoreElem list

convertStoreElem :: (Addr, Val) -> (Text, Text)
convertStoreElem (addr, val) = ((Text.pack(show addr)), (Text.pack(show val)))

convertEnv :: [(Text, Addr)]  -> [(Text,Text)]
convertEnv list = map convertEnvElem list

convertEnvElem :: (Text, Addr) -> (Text, Text)
convertEnvElem (text, addr) = ((Text.pack(show text)), (Text.pack(show addr)))

extractStackElems :: [In] -> [Text]
extractStackElems (((_, _), (_, expr)):_) = map (Text.pack . show) expr

uglifyStore :: [(Addr,Val)] -> [(Text,Text)]
uglifyStore list = map uglifyStoreElem list 

-- |Uglify means: Do not abbreviate too long expression, required for stack
uglifyStoreElem :: (Addr,Val) -> (Text,Text)
uglifyStoreElem (addr, (ClosureVal val)) = ((Text.pack(show addr)) , (Text.pack(show val)))
uglifyStoreElem (addr, val) = ((Text.pack(show addr)) , (Text.pack(show val)))

-- |Get environment from stack elems
getEnvironment :: [In] -> [(Text,Text)]
getEnvironment stackElems = convertEnv (extractEnvFromStackElem (listLast stackElems))

-- |Get store from stack elems
getStore :: [In] -> [(Text,Text)]
getStore stackElems = convertStore (extractStoreFromStackElem (listLast stackElems))

encodeDebugMessage :: DebugMessage -> Text
encodeDebugMessage obj = Data.Text.Encoding.decodeUtf8 (toStrict1 (encode obj))


