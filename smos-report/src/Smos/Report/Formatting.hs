module Smos.Report.Formatting where

import Data.List

import Text.PrettyPrint.Boxes

formatAsTable :: [[String]] -> String
formatAsTable list =
    let boxes = transpose $ fmap text <$> list
        table = hsep 1 center1 $ fmap (vcat left) boxes
     in render table
