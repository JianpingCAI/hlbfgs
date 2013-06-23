module Main where

import Foreign.C
import Test.HUnit
import qualified Data.Vector.Storable as S
import Data.Maybe (isJust)
import Control.Monad (replicateM, liftM)
import System.Exit
import System.Random (randomRIO)

import HLBFGS

--------------------------------------------------------------------------------
-- Test problem definition -----------------------------------------------------
--------------------------------------------------------------------------------

cost
  :: S.Vector CDouble
  -> S.Vector CDouble
  -> CDouble
cost xs ys = S.sum . sq $ S.zipWith (-) xs ys
  where sq = S.map (**2)

gradient
  :: S.Vector CDouble
  -> S.Vector CDouble
  -> S.Vector CDouble
gradient xs ys = S.map (*2) $ S.zipWith (-) ys xs

--------------------------------------------------------------------------------
-- Utilities -------------------------------------------------------------------
--------------------------------------------------------------------------------

eps = 1e-15

epsEqual
  :: S.Vector CDouble
  -> S.Vector CDouble
  -> Bool
epsEqual xs x = r < eps
  where r = if mxnrm == 0 then 0 else cost xs x / mxnrm
        xnrm  = S.sum . S.map (**2) $ x
        xsnrm = S.sum . S.map (**2) $ xs
        mxnrm = max xsnrm xnrm

--------------------------------------------------------------------------------
-- Tests and execution ---------------------------------------------------------
--------------------------------------------------------------------------------

testFailure
  :: String
  -> Maybe a
  -> Test
testFailure msg s =
  let check = isJust s in TestCase (assertBool msg check)

testConverged
  :: String
  -> Maybe (CInt, Bool, S.Vector CDouble)
  -> Test
testConverged msg result =
  let check =
        case result of
          Just (_,b,_) -> b
          Nothing      -> False
  in TestCase (assertBool msg check)

testSolution
  :: String
  -> Maybe (CInt, Bool, S.Vector CDouble)
  -> S.Vector CDouble
  -> Test
testSolution msg result xs =
  let check =
        case result of
          Just (_,_,x) -> epsEqual xs x
          Nothing      -> False
  in TestCase (assertBool msg check)

testSuite :: IO ()
testSuite = do
  -- problem setup
  let dom = (-1.0,1.0)
      n   = 10  :: CInt
      m   = 6   :: CInt
      ep  = 0.01
      nit = 100
  xs <- S.fromList `liftM` (replicateM (fromIntegral n) (randomRIO dom) :: IO [CDouble])
  x0 <- S.fromList `liftM` (replicateM (fromIntegral n) (randomRIO dom) :: IO [CDouble])
  let f = cost xs
      g = gradient xs
  result <- runSolver False n m nit ep x0 f g
  counts <- runTestTT $ TestList
    [ testFailure     "Test: init or solution error"  result
    , testConverged   "Test: convergence"             result
    , testSolution    "Test: solution accuracy"       result xs
    ]
  if failures counts > 0
    then exitFailure
    else exitSuccess

main :: IO ()
main = testSuite
