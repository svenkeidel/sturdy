{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Soundness where

import           Abstract (BaseValue(..))
import qualified Abstract as Abstract
import qualified Concrete as Concrete
import qualified ConcreteInterpreter as Concrete
import qualified GenericInterpreter as Generic
import qualified UnitAnalysis as UnitA

import           Control.Arrow.Transformer.Abstract.Stack (AbsList(..))
import qualified Control.Arrow.Transformer.Abstract.WasmFrame as Frame

import qualified Data.Abstract.Error as AE
import           Data.Abstract.Except (Except)
import qualified Data.Abstract.Except as Except
import           Data.Abstract.FreeCompletion
import           Data.Abstract.DiscretePowerset (Pow(..))
import           Data.Abstract.Terminating
import qualified Data.Concrete.Error as CE
import qualified Data.HashSet as HashSet
import           Data.Order
import           Data.Text.Lazy (pack)
import           Data.Vector (Vector)
import qualified Data.Vector as Vec

import qualified Language.Wasm.Interpreter as Wasm
import           Language.Wasm.Validate (ValidModule)

type ConcResult = CE.Error
                    String
                    (Vector Concrete.Value,
                      (Concrete.GlobalState Concrete.Value,
                       CE.Error
                         (Generic.Exc Concrete.Value)
                         ([Concrete.Value], [Concrete.Value])))
type AbsResult = (AE.Error
                   (Pow String)
                   (Frame.Vector UnitA.Value,
                     (FreeCompletion (Abstract.GlobalState UnitA.Value),
                      Except
                        (UnitA.Exc UnitA.Value)
                        (AbsList UnitA.Value, AbsList UnitA.Value))))

alphaResult :: [ConcResult] -> AbsResult
alphaResult = alphaError alphaString1
                         (alphaTuple alphaLocals1
                           (alphaTuple alphaGlobalState1
                             (alphaExcept1 alphaExc1
                                           (alphaTuple alphaStack1 (AbsList . map alphaVal1)))))

alphaValue :: [Concrete.Value] -> UnitA.Value
alphaValue = foldr1 (⊔) . map alphaVal1 -- . HashSet.toList

alphaVal1 :: Concrete.Value -> UnitA.Value
alphaVal1 (Concrete.Value v) = UnitA.Value $ Lower $ case v of
                        Wasm.VI32 _ -> VI32 ()
                        Wasm.VI64 _ -> VI64 ()
                        Wasm.VF32 _ -> VF32 ()
                        Wasm.VF64 _ -> VF64 ()

alphaTuple :: (concA -> absA) -> (concB -> absB) -> (concA,concB) -> (absA,absB)
alphaTuple alphaA alphaB (a,b) = (alphaA a, alphaB b)

alphaError :: (Complete absE, Complete absA) =>
    (concE -> absE) -> (concA -> absA) -> [CE.Error concE concA] -> AE.Error absE absA
alphaError alphaE alphaA = foldr1 (⊔) . map alphaError1 -- . HashSet.toList
    where alphaError1 = either (AE.Fail . alphaE) (AE.Success . alphaA) . CE.toEither

alphaString1 :: String -> Pow String
alphaString1 = Pow . HashSet.singleton

alphaLocals :: [Vector Concrete.Value] -> Frame.Vector UnitA.Value
alphaLocals = foldr1 (⊔) . map alphaLocals1 -- . HashSet.toList
alphaLocals1 :: Vector Concrete.Value -> Frame.Vector UnitA.Value
alphaLocals1 = Frame.Vector . Vec.map alphaVal1

alphaStack :: [[Concrete.Value]] -> AbsList UnitA.Value
alphaStack = foldr1 (⊔) . map alphaStack1 -- . HashSet.toList
alphaStack1 :: [Concrete.Value] -> AbsList UnitA.Value
alphaStack1 = AbsList . map alphaVal1

alphaGlobalState :: [Concrete.GlobalState Concrete.Value] -> FreeCompletion (Abstract.GlobalState UnitA.Value)
alphaGlobalState = foldr1 (⊔) . map alphaGlobalState1 -- . HashSet.toList
alphaGlobalState1 :: Concrete.GlobalState Concrete.Value
                           -> FreeCompletion (Abstract.GlobalState UnitA.Value)
alphaGlobalState1 (Concrete.GlobalState f t _ g) = Lower $ Abstract.GlobalState f t (Vec.map alphaGlob g)
          where alphaGlob (Concrete.GlobInst m v) = Lower $ Concrete.GlobInst m (alphaVal1 v)

alphaExcept :: (Complete absE, Complete absA) =>
    (concE -> absE) -> (concA -> absA) -> [CE.Error concE concA] -> Except absE absA
alphaExcept alphaE alphaA = foldr1 (⊔) . map (alphaExcept1 alphaE alphaA) -- . HashSet.toList
alphaExcept1 :: (concE -> absE) -> (concA -> absA) -> CE.Error concE concA -> Except absE absA
alphaExcept1 alphaE alphaA = either (Except.Fail . alphaE) (Except.Success . alphaA) . CE.toEither

alphaExc1 :: (Generic.Exc Concrete.Value) -> (UnitA.Exc UnitA.Value)
alphaExc1 exc = UnitA.Exc $ HashSet.singleton $ convert exc
    where convert (Generic.Jump i vs) = Generic.Jump i $ map alphaVal1 vs
          convert (Generic.CallReturn vs) = Generic.CallReturn $ map alphaVal1 vs
          convert (Generic.Trap s) = Generic.Trap s


isSoundlyAbstracted :: ValidModule -> String -> [[Concrete.Value]] -> IO Bool
isSoundlyAbstracted mod func argsList = do
    Right (concModInst, concStore) <- Concrete.instantiate mod
    Right (absModInst, absStore) <- UnitA.instantiate mod
    let (AbsList absArgs) = foldr1 (⊔) $ (map (AbsList . map alphaVal1)) $ argsList
    let concResults = map (snd . Concrete.invokeExported concStore concModInst (pack func)) argsList
    let (Terminating temp) = UnitA.invokeExported absStore absModInst (pack func) absArgs
    let absResult = resultToAbsList $ temp
    return $ alphaResult concResults ⊑ absResult

    where
        resultToAbsList (AE.Success (locals, (state, (Except.Success (stack, vs))))) =
            (AE.Success (locals, (state, (Except.Success (stack, (AbsList vs))))))
        resultToAbsList (AE.Success (locals, (state, (Except.SuccessOrFail e (stack, vs))))) =
            (AE.Success (locals, (state, (Except.SuccessOrFail e (stack, (AbsList vs))))))
        resultToAbsList (AE.Success (locals, (state, (Except.Fail e)))) =
            (AE.Success (locals, (state, (Except.Fail e))))
        resultToAbsList (AE.Fail e) = AE.Fail e
