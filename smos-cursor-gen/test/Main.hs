module Main where

import Test.Hspec
import Test.Hspec.Core.Spec
import Test.QuickCheck

import Spec

main :: IO ()
main = hspec $ modifyMaxShrinks (max 100) spec

-- Until we can use hspec 2.5.8
modifyMaxShrinks :: (Int -> Int) -> SpecWith a -> SpecWith a
modifyMaxShrinks = modifyArgs . modify
  where
    modify :: (Int -> Int) -> Args -> Args
    modify f args = args {maxShrinks = f (maxShrinks args)}

modifyArgs :: (Args -> Args) -> SpecWith a -> SpecWith a
modifyArgs = modifyParams . modify
  where
    modify :: (Args -> Args) -> Params -> Params
    modify f p = p {paramsQuickCheckArgs = f (paramsQuickCheckArgs p)}
