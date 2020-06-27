module Smos.Scheduler.RenderSpec (spec) where

import Control.Monad.Reader
import Data.GenValidity.Path ()
import Data.GenValidity.Time ()
import Smos.Scheduler.Render
import Smos.Scheduler.Render.Gen ()
import Smos.Scheduler.Template.Gen ()
import Test.Hspec
import Test.Validity

spec :: Spec
spec = do
  let rendersValid func = forAllValid $ \c -> forAllValid $ \st ->
        shouldBeValid $ runReaderT (func st) c
  describe "renderTemplate" $ it "produces valid results" $ rendersValid renderTemplate
  describe "renderEntryTemplate" $ it "produces valid results" $ rendersValid renderEntryTemplate
  describe "renderHeaderTemplate" $ it "produces valid results" $ rendersValid renderHeaderTemplate
  describe "renderContentsTemplate" $ it "produces valid results" $ rendersValid renderContentsTemplate
  describe "renderTimestampsTemplate" $ it "produces valid results" $ rendersValid renderTimestampsTemplate
  describe "renderTimestampTemplate" $ it "produces valid results" $ rendersValid renderTimestampTemplate
  describe "renderPropertiesTemplate" $ it "produces valid results" $ rendersValid renderPropertiesTemplate
  describe "renderPropertyValueTemplate" $ it "produces valid results" $ rendersValid renderPropertyValueTemplate
  describe "renderStateHistoryTemplate" $ it "produces valid results" $ rendersValid renderStateHistoryTemplate
  describe "renderStateHistoryEntryTemplate" $ it "produces valid results" $ rendersValid renderStateHistoryEntryTemplate
  describe "renderTodoStateTemplate" $ it "produces valid results" $ rendersValid renderTodoStateTemplate
  describe "renderUTCTimeTemplate" $ it "produces valid results" $ rendersValid renderUTCTimeTemplate
  describe "renderTagsTemplate" $ it "produces valid results" $ rendersValid renderTagsTemplate
  describe "renderTagTemplate" $ it "produces valid results" $ rendersValid renderTagTemplate
  describe "renderTextTemplate" $ it "produces valid results" $ rendersValid renderTextTemplate
  describe "renderPathTemplate" $ it "produces valid results" $ rendersValid renderPathTemplate
  describe "renderTimeTemplateNow" $ it "produces valid results" $ rendersValid renderTimeTemplateNow
