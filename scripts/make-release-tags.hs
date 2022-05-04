#!/usr/bin/env stack
-- stack --resolver lts-18.16 script

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import qualified Data.ByteString.Lazy as LB
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Version
import Data.Yaml
import Path
import Path.IO
import System.Process.Typed

main :: IO ()
main = do
  here <- getCurrentDir
  dirs <- fst <$> listDir here
  packageYamls <- forM (filter (isJust . stripPrefix "smos" . fromRelDir . dirname) dirs) $ \d -> do
    packageYamlFile <- resolveFile d "package.yaml"
    decodeFileThrow (fromAbsFile packageYamlFile)
  tagsContents <- readProcessStdout_ $ shell "git tag --color=never"
  let allTags = T.lines $ TE.decodeUtf8 $ LB.toStrict tagsContents :: [Text]
  forM_ packageYamls $ \PackageYaml {..} -> do
    let tagName = concat [packageYamlName, "-", showVersion packageYamlVersion]
    unless (T.pack tagName `elem` allTags) $ do
      let tagMessage = unwords [packageYamlName, "version", showVersion packageYamlVersion]
      putStrLn $ unwords ["Creating tag", tagName, "with message", show tagMessage]
      runProcess_ $ shell $ unwords ["git tag --annotate", tagName, "--message", show tagMessage]

data PackageYaml = PackageYaml {packageYamlName :: String, packageYamlVersion :: Version}
  deriving (Show, Eq)

instance FromJSON PackageYaml where
  parseJSON = withObject "PackageYaml" $ \o -> PackageYaml <$> o .: "name" <*> o .: "version"
