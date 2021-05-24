#!/usr/bin/env stack
-- stack --resolver lts-16.12 script

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import qualified Data.ByteString.Lazy as LB
import Data.List
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
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
  let versions = flip mapMaybe packageYamls $ \PackageYaml {..} ->
        let tagName = concat [packageYamlName, "-", showVersion packageYamlVersion]
         in if T.pack tagName `elem` allTags then Nothing else Just (packageYamlName, showVersion packageYamlVersion)
  today <- utctDay <$> getCurrentTime
  let versionListItem (name, version) =
        concat
          [ "- <a name=\"",
            name,
            "-",
            version,
            "\">[",
            name,
            " ",
            version,
            "](#",
            name,
            "-",
            version,
            ")"
          ]
  let section =
        unlines $ sort $ map versionListItem versions
  putStrLn section

data PackageYaml = PackageYaml {packageYamlName :: String, packageYamlVersion :: Version}
  deriving (Show, Eq)

instance FromJSON PackageYaml where
  parseJSON = withObject "PackageYaml" $ \o -> PackageYaml <$> o .: "name" <*> o .: "version"
