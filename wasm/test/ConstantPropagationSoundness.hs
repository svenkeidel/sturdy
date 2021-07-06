{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module ConstantPropagationSoundness where

import qualified Concrete as Concrete
import qualified ConcreteInterpreter as Concrete
import           Data
import qualified GenericInterpreter as Generic
import           ConstantPropagation as Abstract
import qualified ConstantPropagationValue as Abstract
import qualified UnitAnalysisValue as Abstract (Exc(..))
import           Control.Arrow.Transformer.Abstract.KnownAddressMemory

import           Data.Abstract.Except (Except)
import qualified Data.Abstract.Except as Except
import           Data.Abstract.MonotoneErrors (toSet)
import           Data.Abstract.Terminating
import qualified Data.Concrete.Error as Concrete
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Order
import           Data.Text.Lazy (pack)
import qualified Data.Vector as Vec

import qualified Language.Wasm.Interpreter as Wasm
import           Language.Wasm.Validate (ValidModule)

type AbsResult = (Except (HashSet Generic.Err)
                  (Terminating
                    (JoinVector Abstract.Value,
                        (Memories,
                          (StaticGlobalState Abstract.Value,
                            Except (Abstract.Exc Abstract.Value) (JoinList Abstract.Value, JoinList Abstract.Value))))))

convertAbsResult :: Abstract.Result -> AbsResult
convertAbsResult (_,(errs,Terminating (a, (mems, (gs, (Except.Success (stack,vals))))))) =
    let errsSet = toSet errs in
    if HashSet.null errsSet
    then Except.Success (Terminating (a, (mems, (gs, (Except.Success (stack, JoinList vals))))))
    else Except.SuccessOrFail errsSet (Terminating (a, (mems, (gs, (Except.Success (stack, JoinList vals))))))
convertAbsResult (_,(errs,Terminating (a, (mems, (gs, (Except.SuccessOrFail e (stack,vals))))))) =
    let errsSet = toSet errs in
    if HashSet.null errsSet
    then Except.Success (Terminating (a, (mems, (gs, (Except.SuccessOrFail e (stack, JoinList vals))))))
    else Except.SuccessOrFail errsSet (Terminating (a, (mems, (gs, (Except.SuccessOrFail e (stack, JoinList vals))))))
convertAbsResult (_,(errs,Terminating (a, (mems, (gs, Except.Fail e))))) =
    let errsSet = toSet errs in
    if HashSet.null errsSet
    then Except.Success (Terminating (a, (mems, (gs, Except.Fail e))))
    else Except.SuccessOrFail errsSet (Terminating (a, (mems, (gs, Except.Fail e))))
convertAbsResult (_,(errs,NonTerminating)) =
    let errsSet = toSet errs in
    if HashSet.null errsSet
    then Except.Success NonTerminating
    else Except.Fail errsSet

alphaResults :: [Concrete.Result] -> AbsResult
alphaResults = foldr1 (⊔) . map alphaResult

alphaResult :: Concrete.Result -> AbsResult
alphaResult = alphaError (alphaTerminating (alphaTuple
                           alphaVector
                           (alphaIgnoreFst 
                            (alphaTuple alphaMemories
                            (alphaTuple alphaGlobals
                             (alphaExcept (alphaTuple alphaJoinList alphaList)))))))

alphaTerminating :: (concA -> absA) -> concA -> Terminating absA
alphaTerminating alphaA = Terminating . alphaA

alphaVector :: JoinVector Concrete.Value -> JoinVector Abstract.Value
alphaVector (JoinVector v) = JoinVector $ Vec.map alphaVal v

alphaError :: (concA -> absA) -> Concrete.Error Generic.Err concA -> (Except (HashSet Generic.Err) absA)
alphaError alphaA = either
                      (\err -> Except.Fail $ HashSet.singleton err)
                      (\res -> Except.Success $ alphaA res)
                    . Concrete.toEither

alphaGlobals :: StaticGlobalState Concrete.Value -> StaticGlobalState Abstract.Value
alphaGlobals (StaticGlobalState f g) = StaticGlobalState f (Vec.map alphaGlob g)

alphaMemories :: Concrete.Memories -> Memories
alphaMemories = undefined

alphaGlob :: GlobInst Concrete.Value -> GlobInst Abstract.Value
alphaGlob (GlobInst m v) = GlobInst m (alphaVal v)


alphaVal :: Concrete.Value -> Abstract.Value
alphaVal (Concrete.Value v) = Abstract.constantValue v

alphaTuple :: (concA -> absA) -> (concB -> absB) -> (concA,concB) -> (absA,absB)
alphaTuple alphaA alphaB (a,b) = (alphaA a, alphaB b)

alphaIgnoreFst :: (concB -> absB) -> (concA,concB) -> absB
alphaIgnoreFst alphaB (_,b) = alphaB b

alphaExcept :: (concA -> absA) -> Concrete.Error (Generic.Exc Concrete.Value) concA -> Except (Abstract.Exc Abstract.Value) absA
alphaExcept alphaA = either
                       (Except.Fail . alphaExc)
                       (Except.Success . alphaA)
                     . Concrete.toEither

alphaExc :: (Generic.Exc Concrete.Value) -> (Abstract.Exc Abstract.Value)
alphaExc exc = Abstract.Exc $ HashSet.singleton $ convert exc
    where convert (Generic.Jump i vs) = Generic.Jump i $ map alphaVal vs
          convert (Generic.CallReturn vs) = Generic.CallReturn $ map alphaVal vs

alphaJoinList :: JoinList Concrete.Value -> JoinList Abstract.Value
alphaJoinList (JoinList l) = alphaList l

alphaList :: [Concrete.Value] -> JoinList Abstract.Value
alphaList l = JoinList $ map alphaVal l

isSoundlyAbstracted :: ValidModule -> String -> [[Concrete.Value]] -> IO Bool
isSoundlyAbstracted valMod func argsList = do
    -- instatiate the module
    Right (concModInst, concState, concMem, concTab) <- Concrete.instantiateConcrete valMod
    Right (absModInst, absState, absMem, absTab) <- instantiateAbstract valMod
    -- compute the abstract arguments
    let (JoinList absArgs) = foldr1 (⊔) $ (map (JoinList . map alphaVal)) $ argsList
    -- run concrete interpreter for each argument list
    let concResults = map (Concrete.invokeExported concState concMem concTab concModInst (pack func)) argsList
    -- run abstract interpreter with abstract argument list
    let absResultRaw = Abstract.invokeExported absState absTab absModInst absMem (pack func) absArgs
    let absResult = convertAbsResult absResultRaw
    -- check soundness
    return $ alphaResults concResults ⊑ absResult
