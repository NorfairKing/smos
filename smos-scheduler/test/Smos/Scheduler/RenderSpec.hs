{-# LANGUAGE OverloadedStrings #-}

module Smos.Scheduler.RenderSpec
  ( spec,
  )
where

import Control.Monad.Reader
import Data.GenValidity.Path ()
import Data.GenValidity.Time ()
import qualified Data.Text as T
import Data.Time
import Smos.Scheduler.Render
import Smos.Scheduler.Render.Gen ()
import Smos.Scheduler.Template
import Smos.Scheduler.Template.Gen ()
import Test.Syd
import Test.Syd.Validity

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
  describe "renderTodoStateTemplate" $ it "produces valid results" $ rendersValid renderTodoStateTemplate
  describe "renderUTCTimeTemplate" $ it "produces valid results" $ rendersValid renderUTCTimeTemplate
  describe "renderTagsTemplate" $ it "produces valid results" $ rendersValid renderTagsTemplate
  describe "renderTagTemplate" $ it "produces valid results" $ rendersValid renderTagTemplate
  describe "renderTextTemplate" $ it "produces valid results" $ rendersValid renderTextTemplate
  describe "renderPathTemplate" $ it "produces valid results" $ rendersValid renderPathTemplate
  describe "renderTimeTemplateNow" $ do
    it "produces valid results" $ rendersValid renderTimeTemplateNow
    it "works for only literal text" $
      forAllValid $
        \rc ->
          forAllValid $ \t ->
            runReaderT (renderTimeTemplateNow (Template [TLit t])) rc `shouldBe` Success t
    it "works for any formatting string relative to the exact context" $
      forAllValid $
        \fs ->
          forAllValid $
            \now ->
              let rc = RenderContext {renderContextTime = now}
               in runReaderT (renderTimeTemplateNow (Template [TTime (T.pack fs)])) rc `shouldBe` Success (T.pack $ formatTime defaultTimeLocale fs now)
    it "works for this case for relative timestamp templates" $ do
      let rel lt fs ft out =
            let now = ZonedTime lt utc
                rc = RenderContext {renderContextTime = now}
             in runReaderT (renderTimeTemplateNow (Template [TRelTime fs ft])) rc `shouldBe` Success out
      rel (LocalTime (fromGregorian 2020 7 19) (TimeOfDay 12 0 0)) "%F" "monday" "2020-07-20"
      rel (LocalTime (fromGregorian 2020 7 22) (TimeOfDay 13 0 0)) "%F" "tuesday" "2020-07-28"
      rel (LocalTime (fromGregorian 2020 7 22) (TimeOfDay 14 0 0)) "%V" "tuesday" "31"
