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
module TypedAnalysis.Chaotic where

import           Prelude hiding (not,Bounded,fail,(.),exp,read,IO)
import qualified Prelude as P

import           Control.Category
import           Control.Arrow
import           Control.Arrow.Environment as Env
import qualified Control.Arrow.Fix as Fix
import           Control.Arrow.Fix.Chaotic(IterationStrategy,chaotic,innermost',outermost')
import qualified Control.Arrow.Fix.Context as Ctx
import           Control.Arrow.Fix.Stack as Stack
import           Control.Arrow.IO
import qualified Control.Arrow.Trans as Trans
import           Control.Arrow.Transformer.IO
import           Control.Arrow.Transformer.Value
import           Control.Arrow.Transformer.Abstract.FiniteEnvStore hiding (Env)
import           Control.Arrow.Transformer.Abstract.LogError
import           Control.Arrow.Transformer.Abstract.Fix
import           Control.Arrow.Transformer.Abstract.Fix.Component as Comp
import           Control.Arrow.Transformer.Abstract.Fix.Context
import           Control.Arrow.Transformer.Abstract.Fix.Stack as Stack
import           Control.Arrow.Transformer.Abstract.Fix.Cache.Immutable as Cache
import           Control.Arrow.Transformer.Abstract.Fix.Metrics as Metric
import           Control.Arrow.Transformer.Abstract.Fix.ControlFlow
import           Control.Arrow.Transformer.Abstract.Terminating

import           Data.Empty
import           Data.Label
import           Data.Text (Text)

import qualified Data.Abstract.Widening as W
import           Data.Abstract.Terminating(Terminating)

import           TypedAnalysis
import           Syntax (LExpr,Expr(App))
import           GenericInterpreter as Generic

--import           Control.Arrow.Transformer.Debug(DebugT)


--imports for debugger
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

import Data.Aeson
import Data.Text.Encoding
import GHC.Generics
import Data.Aeson.TH
import Data.Text.Lazy.Encoding

import Data.ByteString.Builder(toLazyByteString)

import Parser

import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString               as B

import Control.Arrow.Transformer.Debug(DebugT, ArrowDebug, sendMessage)
import          Control.Arrow.Transformer.State

type InterpT c x y =
  (ValueT Val
    (TerminatingT
      (LogErrorT Text
        (EnvStoreT Text Addr Val
          (FixT 
           (DebugT
            (MetricsT Metric.Monotone In
             (ComponentT Comp.Component  In
               (StackT Stack.Stack In
                 (CacheT Cache.Monotone In Out
                   (ContextT Ctx
                     (ControlFlowT Expr
                       c)))))))))))) x y

{--
evalChaotic :: (?sensitivity :: Int) => IterationStrategy _ In Out -> [(Text,Addr)] -> [LExpr] -> (CFG Expr, (Metric.Monotone In, Out'))
evalChaotic iterationStrat env0 e =
  let ?cacheWidening = (storeErrWidening, W.finite) in
  let ?fixpointAlgorithm = transform $
        Fix.fixpointAlgorithm $
        -- Fix.trace printIn printOut .
        Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of App _ _ l:_ -> Just l; _ -> Nothing) .
        Fix.recordEvaluated .
        -- CFlow.recordControlFlowGraph' (\(_,(_,exprs)) -> case exprs of e':_ -> Just e'; _ -> Nothing) .
        -- Fix.filter' isFunctionBody (Fix.trace printIn printOut . chaotic iterationStrat)
        Fix.filter' isFunctionBody (chaotic iterationStrat) in
  second snd $ Trans.run (extend' (Generic.runFixed :: InterpT (->) [Expr] Val)) (empty,(empty,(env0,e0)))
  where
    e0 = generate (sequence e)
{-# INLINE evalChaotic #-}
-}

{--
evalInner :: Eval
evalInner = evalChaotic innermost'

evalOuter :: Eval
evalOuter = evalChaotic outermost'

evalInner' :: Eval'
evalInner' exprs = let (metrics,(cfg,res)) = evalInner [] exprs in (metrics,(cfg,snd res))

evalOuter':: Eval'
evalOuter' exprs = let (metrics,(cfg,res)) = evalOuter [] exprs in (metrics,(cfg,snd res))

eval' :: (?sensitivity :: Int) => [(Text,Addr)] -> [LExpr] -> (Errors,Terminating Val)
eval' env exprs = snd $ snd $ snd $ evalInner env exprs
-}


--------------------------------------------------
--------------------------------------------------
--------------------------------------------------

type ClientId = Int
type Client   = (ClientId, WS.Connection)
type State    = [Client]

type Socket = () -- Placeholder
type Breakpoints = [Expr] -- Placeholder
type StackElems = [In] -- Placeholder

data DebugState = DebugState {
  breakpoints :: Breakpoints,  
  conn :: WS.Connection,
  clientId :: ClientId,
  stateRef :: Concurrent.MVar State,
  expressionList :: [LExpr],
  debugPhase :: Int
}


data Message
  = ReachedBreakpoint {breakpoint :: Expr, env :: Env}
  | Continue
  | GetStackRequest
  | GetStackResponse StackElems




---------------------     Websocket Messages     ----------------------------

data TestMessage
  = InitializeDebuggerRequest {operation :: Text, path :: String}
  | InitializeDebuggerResponse {operation :: Text, code :: Text}
  | ContinueRequest {operation :: Text, bps :: [Text], start :: Bool}
  | ContinueResponse {operation :: Text, debugInfo :: Text}
  | RefreshRequest {operation :: Text}
  | RefreshResponse {operation :: Text, success :: Bool}
  deriving (Show, Eq)


instance ToJSON TestMessage where
    toJSON (InitializeDebuggerRequest operation path) = object ["operation" .= operation, "path" .= path]
    toJSON (InitializeDebuggerResponse operation code) = object ["operation" .= operation, "code" .= code] 
    toJSON (ContinueRequest operation bps start) = object ["operation" .= operation, "bps" .= bps, "start" .= start]
    toJSON (ContinueResponse operation debugInfo) = object ["operation" .= operation, "debugInfo" .= debugInfo]
    toJSON (RefreshRequest operation) = object ["operation" .= operation]
    toJSON (RefreshResponse operation success) = object ["operation" .= operation, "success" .= success]

instance FromJSON TestMessage where
    parseJSON v = parseInitializeDebuggerRequest v 
               <> parseInitializeDebuggerResponse v 
               <> parseContinueRequest v
               <> parseContinueResponse v 
               <> parseRefreshRequest v 
               <> parseRefreshResponse v
      where
        parseInitializeDebuggerRequest = withObject "InitializeDebuggerRequest" $ \obj -> InitializeDebuggerRequest <$> obj .: "operation" <*> obj .: "path"
        parseInitializeDebuggerResponse = withObject "InitializeDebuggerResponse" $ \obj -> InitializeDebuggerResponse <$> obj .: "operation" <*> obj .: "code"
        parseContinueRequest = withObject "ContinueRequest" $ \obj -> ContinueRequest <$> obj .: "operation" <*> obj .: "bps" <*> obj .: "start"
        parseContinueResponse = withObject "ContinueResponse" $ \obj -> ContinueResponse <$> obj .: "operation" <*> obj.: "debugInfo"
        parseRefreshRequest = withObject "RefreshRequest" $ \obj -> RefreshRequest <$> obj .: "operation"
        parseRefreshResponse = withObject "RefreshResponse" $ \obj -> RefreshResponse <$> obj .: "operation" <*> obj .: "success"





changeExpressions :: DebugState -> [LExpr] -> DebugState
changeExpressions debugState expressions = debugState { expressionList = expressions }

changeDebugPhase :: DebugState -> Int -> DebugState
changeDebugPhase debugState debugPhase = debugState { debugPhase = debugPhase }


{--

-- | Main entry point for the debugger
startDebugger :: [LExpr] -> P.IO ()                   
startDebugger expr = do
  _setupDebugState -- TODO: Implement
  let ?sensitivity = 0
  let ?debugState = _debugState -- TODO: Implement
  evalDebug expr
  -- socket <- createSocket -< 1234
  -- loop
  -- where
  --   loop = do
  --     msg <- Client.receive -< ()
  --     case msg of
  --       StartProgram file breakpoints -> do
  --         prog <- loadSchemeFile file
  --         let ?sensitivity = 0
  --         let debugState = DebugState socket breakpoints
  --         evalDebug debugState prog
  --         loop
  --       _ ->
  --         closeSocket socket

evalDebug :: (?sensitivity::Int, ?debugState::DebugState) => [LExpr] -> P.IO ()
evalDebug expr =
  let ?cacheWidening = (storeErrWidening, W.finite) in
  let ?fixpointAlgorithm = transform $
       Fix.fixpointAlgorithm $
         debug .
         Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of App _ _ l:_ -> Just l; _ -> Nothing) .
         Fix.filter' isFunctionBody (chaotic innermost') in
  do _ <- Trans.run (extend' (Generic.runFixed :: InterpT IO [Expr] Val)) (empty,(empty,([],e0)))
     return ()
  where
    e0 = generate (sequence expr)

-- | Send message to debugging client
send :: (?debugState :: DebugState) => Message -> P.IO ()
send = _send -- TODO: Implement

-- | Receive message from debugging client
receive :: (?debugState :: DebugState) => () -> P.IO Message
receive = _receive -- TODO: Implement

-- | Debugging combinator
debug :: (?debugState :: DebugState,
          ArrowChoice c, ArrowIO c, ArrowStackElements In c)
      => Fix.FixpointCombinator c ((Store,Errors),(Env,[Expr]))
                                  ((Store,Errors), Terminating Val)
debug f = proc input@((store,errors),(env,exprs)) -> do
  case exprs of
    expr:_ | expr `P.elem` breakpoints ?debugState -> do
      -- We reached a breakpoint
      liftIO send -< ReachedBreakpoint expr env
      loop -< input
      f -< input
    _ ->
      f -< input
  where
    loop = proc input@((store,errors),(env,expr)) -> do
      msg <- liftIO receive -< ()
      case msg of
        Continue ->
          returnA -< ()
        GetStackRequest -> do
          stack <- Stack.elems -< ()
          liftIO send -< GetStackResponse stack
          loop -< input


-}



startServer :: P.IO ()
startServer = do
  putStrLn "hello world"
  state <- Concurrent.newMVar []
  Warp.run 3001 $ WS.websocketsOr
    WS.defaultConnectionOptions
    (wsApp state)
    httpApp

wsApp :: Concurrent.MVar State -> WS.ServerApp
wsApp stateRef pendingConn = do
  conn <- WS.acceptRequest pendingConn
  clientId <- connectClient conn stateRef
  WS.forkPingThread conn 30
  let debugState = DebugState [] conn clientId stateRef [] 0
  Exception.finally
    (listen debugState)
    (disconnectClient clientId stateRef)


httpApp :: Wai.Application
httpApp _ respond = respond $ Wai.responseLBS Http.status400 [] "Not a websocket request"

nextId :: State -> ClientId
nextId = Maybe.maybe 0 ((+) 1) . Safe.maximumMay . List.map fst

connectClient :: WS.Connection -> Concurrent.MVar State -> P.IO ClientId
connectClient conn stateRef = Concurrent.modifyMVar stateRef $ \state -> do
  let clientId = nextId state
  return ((clientId, conn) : state, clientId)


extractClient :: ClientId -> State -> State
extractClient clientId = List.filter ((==) clientId . fst)

withoutClient :: ClientId -> State -> State
withoutClient clientId = List.filter ((/=) clientId . fst)

disconnectClient :: ClientId -> Concurrent.MVar State -> P.IO ()
disconnectClient clientId stateRef = Concurrent.modifyMVar_ stateRef $ \state ->
  return $ withoutClient clientId state

listen :: DebugState -> P.IO ()
listen debugState = do
  print (clientId debugState)
  
  msg <- WS.receiveData (conn debugState)
  let dec = Maybe.fromJust (decode'' msg) :: TestMessage
  print "JETZT KOMMT PRINT DEC "
  print (dec)
  print (operation dec)
  let op = operation dec
  case op of

    "InitializeDebuggerRequest" -> do
      let expressions = Parser.loadSchemeFile (path dec)
      -- TODO richtige Expressions an den Client senden, wie kann man die expr. zum string casten?
      let object = InitializeDebuggerResponse "InitializeDebuggerResponse" "BLA CODE"
      print object
      let encodedObject = encode object
      let textObject = Data.Text.Encoding.decodeUtf8 (toStrict1 encodedObject)
      
      sendResponse debugState textObject
      --debugPhase auf 1 setzen
    
    "ContinueRequest" -> do
      sendResponse debugState "bla"
      --checken ob debug phase auf 1 ist
      --evalDebug aufrufen
      let ?sensitivity = 0
      let ?debugState = debugState 
      evalDebug (expressionList debugState)


    "RefreshRequest" -> do
      --debug state neu initialisieren und listen aufrufen
      sendResponse debugState "REFRESHHHH"


  listen debugState



  

sendResponse :: DebugState -> Text.Text -> P.IO ()
sendResponse debugState msg = do
  print msg
  putStrLn "hier in sendResponse"

  WS.sendTextData (conn debugState) msg             
  



evalDebug :: (?sensitivity::Int, ?debugState::DebugState) => [LExpr] -> P.IO ()
evalDebug expr =
  let ?cacheWidening = (storeErrWidening, W.finite) in
  let ?fixpointAlgorithm = transform $
       Fix.fixpointAlgorithm $
         --debug .        --TODO
         Ctx.recordCallsite ?sensitivity (\(_,(_,exprs)) -> case exprs of App _ _ l:_ -> Just l; _ -> Nothing) .
         Fix.filter' isFunctionBody (chaotic innermost') in
  do _ <- Trans.run (extend' (Generic.runFixed :: InterpT IO [Expr] Val)) (empty,(empty,(empty,e0)))
     return ()
  where
    e0 = generate (sequence expr)


-- | Debugging combinator
debug :: (?debugState :: DebugState,
          ArrowChoice c, ArrowIO c, ArrowStackElements In c, ArrowDebug c)
      => Fix.FixpointCombinator c ((Store,Errors),(Env,[Expr]))
                                  ((Store,Errors), Terminating Val)
debug f = proc input@((store,errors),(env,exprs)) -> do
  sendMessage -< (Text.pack "BLA")
  f -< input
  
  
{--  
  case exprs of
    expr:_ | expr `P.elem` breakpoints ?debugState -> do
      -- We reached a breakpoint
      liftIO send -< ReachedBreakpoint expr env
      loop -< input
      f -< input
    _ ->
      f -< input
  where
    loop = proc input@((store,errors),(env,expr)) -> do
      msg <- liftIO receive -< ()
      case msg of
        Continue ->
          returnA -< ()
        GetStackRequest -> do
          stack <- Stack.elems -< ()
          liftIO send -< GetStackResponse stack
          loop -< input
-}


---------------------     Helper Functions     ----------------------------

decode'' :: FromJSON a => Text.Text -> Maybe a
decode'' = decode . toLazyByteString . Data.Text.Encoding.encodeUtf8Builder


toStrict1 :: BL.ByteString -> B.ByteString
toStrict1 = B.concat . BL.toChunks







