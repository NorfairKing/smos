{-# LANGUAGE OverloadedStrings #-}

module Smos.Docs.Site.Handler.Sitemap
  ( getRobotsR,
    getSitemapR,
  )
where

import Data.Text (Text)
import Smos.Docs.Site.Foundation
import Yesod.Sitemap

getRobotsR :: Handler Text
getRobotsR = robots RobotsR

getSitemapR :: Handler TypedContent
getSitemapR = sitemapList urls

urls :: [SitemapUrl (Route App)]
urls =
  concat
    [ [ SitemapUrl
          { sitemapLoc = HomeR,
            sitemapLastMod = Nothing,
            sitemapChangeFreq = Just Weekly,
            sitemapPriority = Just 0.9
          },
        SitemapUrl
          { sitemapLoc = SmosR,
            sitemapLastMod = Nothing,
            sitemapChangeFreq = Just Monthly,
            sitemapPriority = Just 0.5
          },
        SitemapUrl
          { sitemapLoc = SmosFileR,
            sitemapLastMod = Nothing,
            sitemapChangeFreq = Just Monthly,
            sitemapPriority = Just 0.5
          },
        SitemapUrl
          { sitemapLoc = SmosKeybindingsR,
            sitemapLastMod = Nothing,
            sitemapChangeFreq = Just Monthly,
            sitemapPriority = Just 0.5
          },
        SitemapUrl
          { sitemapLoc = SmosActionsR,
            sitemapLastMod = Nothing,
            sitemapChangeFreq = Just Monthly,
            sitemapPriority = Just 0.5
          },
        SitemapUrl
          { sitemapLoc = SmosSingleR,
            sitemapLastMod = Nothing,
            sitemapChangeFreq = Just Monthly,
            sitemapPriority = Just 0.5
          },
        SitemapUrl
          { sitemapLoc = SmosArchiveR,
            sitemapLastMod = Nothing,
            sitemapChangeFreq = Just Monthly,
            sitemapPriority = Just 0.5
          },
        SitemapUrl
          { sitemapLoc = SmosCalendarImportR,
            sitemapLastMod = Nothing,
            sitemapChangeFreq = Just Monthly,
            sitemapPriority = Just 0.5
          },
        SitemapUrl
          { sitemapLoc = SmosSchedulerR,
            sitemapLastMod = Nothing,
            sitemapChangeFreq = Just Monthly,
            sitemapPriority = Just 0.5
          },
        SitemapUrl
          { sitemapLoc = SmosSchedulerTemplateR,
            sitemapLastMod = Nothing,
            sitemapChangeFreq = Just Monthly,
            sitemapPriority = Just 0.5
          },
        SitemapUrl
          { sitemapLoc = SmosSyncClientR,
            sitemapLastMod = Nothing,
            sitemapChangeFreq = Just Monthly,
            sitemapPriority = Just 0.5
          },
        SitemapUrl
          { sitemapLoc = SmosServerR,
            sitemapLastMod = Nothing,
            sitemapChangeFreq = Just Monthly,
            sitemapPriority = Just 0.5
          },
        SitemapUrl
          { sitemapLoc = SmosWebServerR,
            sitemapLastMod = Nothing,
            sitemapChangeFreq = Just Monthly,
            sitemapPriority = Just 0.5
          },
        SitemapUrl
          { sitemapLoc = SmosQueryR,
            sitemapLastMod = Nothing,
            sitemapChangeFreq = Just Monthly,
            sitemapPriority = Just 0.5
          }
      ],
      map
        ( \command ->
            SitemapUrl
              { sitemapLoc = SmosQueryCommandR command,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Monthly,
                sitemapPriority = Just 0.4
              }
        )
        [ "next",
          "work",
          "agenda",
          "projects",
          "stuck",
          "waiting",
          "clock",
          "log",
          "stats",
          "tags",
          "report",
          "entry"
        ],
      map
        ( \dp ->
            SitemapUrl
              { sitemapLoc = PageR dp,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Yearly,
                sitemapPriority = Just 0.3
              }
        )
        docPages
    ]
