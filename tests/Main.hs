module Main where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.List
import Data.Ord

import qualified Validator.Tests as V

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

-- properties :: TestTree
-- properties = testGroup "Properties" [scProps, qcProps]

-- scProps = testGroup "(checked by SmallCheck)"
--   [ SC.testProperty "sort == sort . reverse" $
--       \list -> sort (list :: [Int]) == sort (reverse list)
--   , SC.testProperty "Fermat's little theorem" $
--       \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
--   -- the following property does not hold
--   , SC.testProperty "Fermat's last theorem" $
--       \x y z n ->
--         (n :: Integer) >= 3 SC.==> x^n + y^n /= (z^n :: Integer)
--   ]

unitTests = testGroup "Unit tests" V.testCollection