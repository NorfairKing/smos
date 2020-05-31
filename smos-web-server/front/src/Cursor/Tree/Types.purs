module Cursor.Tree.Types where

import Prelude
import Data.Maybe
import Data.Symbol
import Data.Newtype
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NE
import Data.List
import Data.Lens.Record (prop)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens

newtype Tree a
  = Tree { rootLabel :: a, subForest :: Forest a }

type Forest a
  = List (Tree a)

type TreeCursor a b
  = { treeAbove :: (Maybe (TreeAbove b))
    , treeCurrent :: a
    , treeBelow :: (CForest b)
    }

treeCursorAboveL :: forall a b. Lens' (TreeCursor a b) (Maybe (TreeAbove b))
treeCursorAboveL = lens _.treeAbove (\tc ta -> tc { treeAbove = ta })

treeCursorCurrentL :: forall a b a'. Lens (TreeCursor a b) (TreeCursor a' b) a a'
treeCursorCurrentL = lens _.treeCurrent (\tc a -> tc { treeCurrent = a })

treeCursorBelowL :: forall a b. Lens' (TreeCursor a b) (CForest b)
treeCursorBelowL = lens _.treeBelow (\tc tb -> tc { treeBelow = tb })

-- TODO: get rid of the prefixed record field names
newtype TreeAbove b
  = TreeAbove
  { treeAboveLefts :: List (CTree b)
  , treeAboveAbove :: (Maybe (TreeAbove b))
  , treeAboveNode :: b
  , treeAboveRights :: List (CTree b)
  }

derive instance newtypeTreeAbove :: Newtype (TreeAbove b) _

treeAboveLeftsL :: forall b. Lens' (TreeAbove b) (List (CTree b))
treeAboveLeftsL = _Newtype <<< prop (SProxy :: _ "treeAboveRights")

treeAboveAboveL :: forall b. Lens' (TreeAbove b) (Maybe (TreeAbove b))
treeAboveAboveL = _Newtype <<< prop (SProxy :: _ "treeAboveAbove")

treeNodeNodeL :: forall b. Lens' (TreeAbove b) b
treeNodeNodeL = _Newtype <<< prop (SProxy :: _ "treeAboveNode")

treeAboveRightsL :: forall b. Lens' (TreeAbove b) (List (CTree b))
treeAboveRightsL = _Newtype <<< prop (SProxy :: _ "treeAboveRights")

data TreeCursorSelection
  = SelectNode
  | SelectChild Int TreeCursorSelection

newtype CTree a
  = CTree { rootLabel :: a, subForest :: CForest a }

makeCTree :: forall a. Tree a -> CTree a
makeCTree = cTree false

cTree :: forall a. Boolean -> Tree a -> CTree a
cTree b (Tree t) = CTree { rootLabel: t.rootLabel, subForest: cForest b t.subForest }

rebuildCTree :: forall a. CTree a -> Tree a
rebuildCTree (CTree cn) = Tree { rootLabel: cn.rootLabel, subForest: (rebuildCForest cn.subForest) }

data CForest a
  = EmptyCForest
  | ClosedForest (NonEmptyList (Tree a))
  | OpenForest (NonEmptyList (CTree a))

makeCForest :: forall a. Forest a -> CForest a
makeCForest = cForest true

cForest :: forall a. Boolean -> Forest a -> CForest a
cForest b f =
  if b then
    openForest $ map (cTree b) f
  else
    closedForest f

rebuildCForest :: forall a. CForest a -> Forest a
rebuildCForest = case _ of
  EmptyCForest -> Nil
  ClosedForest f -> NE.toList f
  OpenForest ct -> NE.toList (map rebuildCTree ct)

emptyCForest :: forall a. CForest a
emptyCForest = EmptyCForest

openForest :: forall a. List (CTree a) -> CForest a
openForest ts = maybe emptyCForest OpenForest (NE.fromList ts)

closedForest :: forall a. List (Tree a) -> CForest a
closedForest ts = maybe emptyCForest ClosedForest (NE.fromList ts)

lengthCForest :: forall a. CForest a -> Int
lengthCForest = case _ of
  EmptyCForest -> 0
  ClosedForest ts -> NE.length ts
  OpenForest ts -> NE.length ts

unpackCForest :: forall a. CForest a -> List (CTree a)
unpackCForest = case _ of
  EmptyCForest -> Nil
  ClosedForest ts -> NE.toList (map makeCTree ts)
  OpenForest ts -> NE.toList ts
