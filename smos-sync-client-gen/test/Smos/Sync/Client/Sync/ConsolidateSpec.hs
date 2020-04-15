module Smos.Sync.Client.Sync.ConsolidateSpec
  ( spec,
  )
where

import Control.Monad
import qualified Data.Map as M
import Data.Mergeful
import Data.Mergeful.Timed
import qualified Data.Set as S
import Smos.API
import Smos.Sync.Client.Command.Sync
import Smos.Sync.Client.Contents
import Smos.Sync.Client.ContentsMap (ContentsMap (..))
import Smos.Sync.Client.ContentsMap.Gen ()
import Smos.Sync.Client.Env
import Smos.Sync.Client.MetaMap (MetaMap (..))
import Smos.Sync.Client.MetaMap.Gen ()
import Smos.Sync.Client.Sync.Gen ()
import Test.Hspec
import Test.Validity
import Text.Show.Pretty

spec :: Spec
spec = do
  describe "consolidateInitialSyncedItemsWithFiles" $ do
    it "contains all the contents that received from the server"
      $ forAllValid
      $ \syncedItems ->
        forAllValid $ \contents -> do
          let cs' = consolidateInitialSyncedItemsWithFiles syncedItems contents
          unless
            ( M.keysSet (makeAlreadySyncedMap syncedItems)
                `S.isSubsetOf` M.keysSet (contentsMapFiles $ makeContentsMap cs')
            )
            $ expectationFailure
            $ unlines
              [ "The items received from the server were not a subset of the resulting store",
                "synced items from server:",
                ppShow syncedItems,
                "consolidated store:",
                ppShow cs'
              ]
    it "contains all the contents that are found locally"
      $ forAllValid
      $ \syncedItems ->
        forAllValid $ \contents -> do
          let cs' = consolidateInitialSyncedItemsWithFiles syncedItems contents
          unless
            ( M.keysSet (contentsMapFiles contents)
                `S.isSubsetOf` M.keysSet (contentsMapFiles $ makeContentsMap cs')
            )
            $ expectationFailure
            $ unlines
              [ "The contents found locally were not a subset of the resulting store",
                "local contents:",
                ppShow contents,
                "consolidated store:",
                ppShow cs'
              ]
  describe "consolidateMetaMapWithFiles" $ do
    it "contains all the contents that are found locally"
      $ forAllValid
      $ \syncedItems ->
        forAllValid $ \contents -> do
          let cs' = consolidateMetaMapWithFiles syncedItems contents
          unless
            ( M.keysSet (contentsMapFiles contents)
                `S.isSubsetOf` M.keysSet (contentsMapFiles $ makeContentsMap cs')
            )
            $ expectationFailure
            $ unlines
              [ "The contents found locally were not a subset of the resulting store",
                "local contents:",
                ppShow contents,
                "consolidated store:",
                ppShow cs'
              ]
    it "contains all the items that were added, marked as added"
      $ forAllValid
      $ \metaMap ->
        forAllValid $ \contents -> do
          let cs' = consolidateMetaMapWithFiles metaMap contents
          let addedItems =
                map (uncurry SyncFile)
                  $ M.toList
                  $ contentsMapFiles contents `M.difference` metaMapFiles metaMap
          M.elems (clientStoreAddedItems cs') `shouldBe` addedItems
    it "contains all the items that were changed, marked as unchanged"
      $ forAllValid
      $ \metaMap ->
        forAllValid $ \contents -> do
          let cs' = consolidateMetaMapWithFiles metaMap contents
          let changedItems =
                M.mapMaybe id $
                  M.intersectionWithKey
                    ( \path sfm bs ->
                        if isUnchanged sfm bs
                          then Just (SyncFile path bs)
                          else Nothing
                    )
                    (metaMapFiles metaMap)
                    (contentsMapFiles contents)
          M.elems (M.map timedValue $ clientStoreSyncedItems cs') `shouldBe` M.elems changedItems
    it "contains all the items that were changed, marked as changed"
      $ forAllValid
      $ \metaMap ->
        forAllValid $ \contents -> do
          let cs' = consolidateMetaMapWithFiles metaMap contents
          let changedItems =
                M.mapMaybe id $
                  M.intersectionWithKey
                    ( \path sfm bs ->
                        if isUnchanged sfm bs
                          then Nothing
                          else Just (SyncFile path bs)
                    )
                    (metaMapFiles metaMap)
                    (contentsMapFiles contents)
          M.elems (M.map timedValue $ clientStoreSyncedButChangedItems cs')
            `shouldBe` M.elems changedItems
    it "contains all the items that were deleted, marked as deleted"
      $ forAllValid
      $ \metaMap ->
        forAllValid $ \contents -> do
          let cs' = consolidateMetaMapWithFiles metaMap contents
          let deletedItems = metaMapFiles metaMap `M.difference` contentsMapFiles contents
              deletedItemsKeys = S.fromList $ M.elems $ M.map syncFileMetaUUID deletedItems
          M.keysSet (clientStoreDeletedItems cs') `shouldBe` deletedItemsKeys
