{-# LANGUAGE OverloadedStrings #-}

module Smos.Server.DB.CompressedSpec (spec) where

import Codec.Compression.Zstd as Zstd
import Control.Monad
import qualified Data.ByteString as SB
import Data.GenValidity.ByteString ()
import Smos.Server.DB.Compressed
import Test.Syd
import Test.Syd.Validity

spec :: Spec
spec = do
  describe "Compressed" $ do
    let goldenFile i = "test_resources/compressed/hello-world-" ++ show i ++ ".dat"
    let levels = [1 .. maxCLevel]
    describe "decompressByteString" $ do
      it "roundtrips with compressByteString" $
        forAllValid $ \bs ->
          decompressByteString (compressByteString 1 bs) `shouldBe` Right bs
      forM_ levels $ \i ->
        it ("can still decompress the output from the previous version at compression level " <> show i) $ do
          c <- decompressByteString . Compressed <$> SB.readFile (goldenFile i)
          c `shouldBe` Right "hello world"
    describe "compressByteString" $
      forM_ levels $ \i ->
        it ("still outputs the compressed version of 'hello world' the same way on compression level " <> show i) $
          pureGoldenByteStringFile (goldenFile i) (compressedByteString (compressByteString i "hello world"))
