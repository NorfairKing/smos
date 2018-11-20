module Smos.ConfigSpec where

import TestImport

import Smos.OptParse.Types
import Smos.Report.OptParse

spec :: Spec
spec = do
    configSpecWithExt ".dhall" $
        parseDhallConfig configurationType configurationDefaults
    configSpecWithExt ".yaml" parseYamlConfig
    configSpecWithExt ".json" parseJSONConfig

configSpecWithExt :: String -> (Path Abs File -> IO Configuration) -> Spec
configSpecWithExt ext parseConf = do
    extFiles <- runIO $ getResourcesWithExtension ext
    describe ext $
        forM_ extFiles $ \df -> it (fromAbsFile df) $ do void $ parseConf df

getResourcesWithExtension :: String -> IO [Path Abs File]
getResourcesWithExtension ext = do
    resourcesDir <- resolveDir' $ "test_resources/config/" ++ drop 1 ext
    fs <- forgivingAbsence $ snd <$> listDirRecur resourcesDir
    pure $ filter ((== ext) . fileExtension) $ fromMaybe [] fs
