module Smos.Data.EncodingSpec
  ( spec,
  )
where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Smos.Data
import Smos.Data.Gen ()
import Test.Syd
import Test.Syd.Validity
import Text.Show.Pretty

spec :: Spec
spec = do
  roundtripSpec "smosFileYamlBS" smosFileYamlBS
  roundtripSpec "smosFileJSONBS" (LB.toStrict . smosFileJSONBS)
  roundtripSpec "smosFileJSONPrettyBS" (LB.toStrict . smosFileJSONPrettyBS)

roundtripSpec :: String -> (SmosFile -> ByteString) -> Spec
roundtripSpec name func =
  describe name $
    it "produces bytestrings that can be roundtripped with parseSmosFile" $
      forAllValid $
        \sf ->
          let bs = func sf
              prettyBs = T.unpack $ TE.decodeUtf8 bs
           in case parseSmosFile bs of
                Left pe ->
                  expectationFailure $
                    unlines
                      [ "Parsing should not have failed",
                        "encoding the following value:",
                        ppShow sf,
                        "produced the folling bytestring:",
                        prettyBs,
                        "but parsing failed with the following error:",
                        show pe
                      ]
                Right sf' ->
                  unless (sf' == sf) $
                    expectationFailure $
                      unlines
                        [ name ++ " should have roundtripped with parseSmosFile",
                          "started with:",
                          ppShow sf,
                          "encoding produced the following value:",
                          prettyBs,
                          "expected:",
                          ppShow sf',
                          "actual:",
                          ppShow sf
                        ]
