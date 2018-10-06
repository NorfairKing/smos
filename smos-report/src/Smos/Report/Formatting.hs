module Smos.Report.Formatting where

import qualified Data.ByteString as SB
import Data.Foldable
import qualified Data.Sequence as S
import Data.Sequence (Seq)
import Data.Text (Text)

import Rainbow
import Rainbox as Box

type Table = Seq (Chunk Text)

formatAsTable :: [[Text]] -> Seq (Chunk Text)
formatAsTable =
    Box.render .
    tableByRows .
    S.fromList .
    map (Box.intersperse (separator mempty 1) .
         S.fromList . map (mkCell . chunk))

mkCell :: Chunk Text -> Cell
mkCell c = Cell (S.singleton (S.singleton c)) bottom left mempty

putTableLn :: Seq (Chunk Text) -> IO ()
putTableLn myChunks = do
    printer <- byteStringMakerFromEnvironment
    mapM_ SB.putStr $ chunksToByteStrings printer $ toList myChunks
