module TreeEncoding where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Foreign.Object as FO
import Cursor.Tree.Types (Forest, Tree(..))
import Data.Argonaut.Core (Json)
import Data.Argonaut.Core as J
import Data.Codec as C
import Data.Codec.Argonaut.Common (JsonCodec)
import Data.Codec.Argonaut.Common as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..))
import Data.Profunctor (dimap)

forestCodec :: forall a. JsonCodec a -> JsonCodec (Forest a)
forestCodec ac = CA.list (treeCodec ac)

treeCodec :: forall a. JsonCodec a -> JsonCodec (Tree a)
treeCodec ac =
  CA.fix \tc ->
    C.composeCodecFlipped migrateCodec
      $ dimap fromTree toTree
          ( CA.object "Tree"
              ( CAR.record
                  { entry: ac
                  , forest: CA.list tc
                  }
              )
          )
  where
  migrateCodec :: JsonCodec Json
  migrateCodec = C.basicCodec (Right <<< migrate) identity

  -- When the subforest is empty, the json will just contain the entry only, instead of an object with an empty list in the 'forest' key.
  -- So we want to normalise this first, then apply the regular 'treeCodec' inner codec to it.
  migrate :: Json -> Json
  migrate j =
    let
      toForestObject = J.fromObject $ FO.fromFoldable [ Tuple "entry" j, Tuple "forest" (J.fromArray []) ]
    in
      case J.toObject j of
        Nothing -> toForestObject
        Just o -> case FO.lookup "entry" o of
          Nothing -> toForestObject
          Just _ -> j

  toTree { entry, forest } = Tree { rootLabel: entry, subForest: forest }

  fromTree (Tree { rootLabel, subForest }) = { entry: rootLabel, forest: subForest }

smosFileCodec :: JsonCodec (Tree String)
smosFileCodec = treeCodec CA.string --  forestCodec CA.string
