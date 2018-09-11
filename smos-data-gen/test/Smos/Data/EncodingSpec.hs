{-# LANGUAGE TypeApplications #-}

module Smos.Data.EncodingSpec
    ( spec
    ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Test.Hspec
import Test.Validity

import Text.Show.Pretty

import Control.Monad

import Smos.Data
import Smos.Data.Gen ()

spec :: Spec
spec = do
    describe "smosFileYamlBS" $
        it "produces bytestrings that can be roundtripped with parseSmosFile" $
        forAllValid $ \sf ->
            let bs = smosFileYamlBS sf
            in case parseSmosFile bs of
                   Left pe ->
                       expectationFailure $
                       unlines
                           [ "Parsing should not have failed"
                            , "encoding the following value:", ppShow sf
                           , "produced the folling bytestring:"
                           , T.unpack $ TE.decodeUtf8 bs, "but parsing failed with the following error:", show pe
                           ]
                   Right sf' ->
                       unless (sf' == sf) $
                       expectationFailure $
                       unlines ["expected:", ppShow sf', "actual:", ppShow sf]
