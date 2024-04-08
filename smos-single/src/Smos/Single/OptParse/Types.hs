{-# LANGUAGE OverloadedStrings #-}

module Smos.Single.OptParse.Types where

import Autodocodec
import Path
import Smos.Data
import Smos.Directory.OptParse.Types

data Flags = Flags
  { flagTaskPieces :: ![String],
    flagTaskFile :: !(Maybe FilePath),
    flagWorkflowDir :: !(Maybe FilePath)
  }

data Configuration = Configuration
  { confWorkflowDir :: !(Maybe FilePath)
  }

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> optionalFieldOrNull "workflow-dir" "The workflow directory" .= confWorkflowDir

data Environment = Environment
  { envWorkflowDir :: !(Maybe FilePath)
  }

data Settings = Settings
  { setTask :: !Header,
    setTaskFile :: !(Maybe (Path Rel File)),
    setWorkflowDirSpec :: !WorkflowDirSpec
  }
