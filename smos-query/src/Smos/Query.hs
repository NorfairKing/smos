{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Query
  ( smosQuery,
    smosQueryWithInstructions,
    execute,
  )
where

import Smos.Query.Commands
import Smos.Query.OptParse
import System.IO

smosQuery :: IO ()
smosQuery = getInstructions >>= smosQueryWithInstructions

smosQueryWithInstructions :: Instructions -> IO ()
smosQueryWithInstructions (Instructions dispatch settings) = do
  let env = makeEnvFromSettings settings
  runReaderT (execute dispatch) env

makeEnvFromSettings :: Settings -> Env
makeEnvFromSettings Settings {..} =
  Env
    { envInputHandle = stdin,
      envOutputHandle = stdout,
      envErrorHandle = stderr,
      envColourSettings = settingColourSettings,
      envDirectoryConfig = settingDirectoryConfig
    }

execute :: Dispatch -> Q ()
execute =
  \case
    DispatchEntry es -> smosQueryEntry es
    DispatchReport es -> smosQueryReport es
    DispatchWork ws -> smosQueryWork ws
    DispatchWaiting ws -> smosQueryWaiting ws
    DispatchNext ns -> smosQueryNext ns
    DispatchClock cs -> smosQueryClock cs
    DispatchAgenda as -> smosQueryAgenda as
    DispatchProjects ps -> smosQueryProjects ps
    DispatchStuck ps -> smosQueryStuck ps
    DispatchLog ss -> smosQueryLog ss
    DispatchStats ss -> smosQueryStats ss
    DispatchTags ts -> smosQueryTags ts
