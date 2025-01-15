{-# LANGUAGE OverloadedStrings #-}

module Smos.Docs.Site.Handler.Sitemap
  ( getRobotsR,
    getSitemapR,
  )
where

import qualified Data.Map as M
import Data.Text (Text)
import Language.Haskell.TH.Load
import Smos.Docs.Site.Foundation
import Yesod.Sitemap

getRobotsR :: Handler Text
getRobotsR = robots RobotsR

getSitemapR :: Handler TypedContent
getSitemapR = do
  urls <- loadIO getUrls
  sitemapList urls

getUrls :: Load [SitemapUrl (Route App)]
getUrls =
  ( \dps cls ->
      concat
        [ [ SitemapUrl
              { sitemapLoc = HomeR,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Yearly,
                sitemapPriority = Just 0.9
              },
            SitemapUrl
              { sitemapLoc = ChangelogR,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Yearly,
                sitemapPriority = Just 0.5
              },
            SitemapUrl
              { sitemapLoc = ChangelogLatestR,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Yearly,
                sitemapPriority = Just 0.5
              },
            SitemapUrl
              { sitemapLoc = ChangelogAllR,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Yearly,
                sitemapPriority = Just 0.5
              }
          ],
          map
            ( \day ->
                SitemapUrl
                  { sitemapLoc = ChangelogReleaseR day,
                    sitemapLastMod = Nothing,
                    sitemapChangeFreq = Just Never,
                    sitemapPriority = Just 0.1
                  }
            )
            cls,
          [ SitemapUrl
              { sitemapLoc = CurrentVersionsR,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Yearly,
                sitemapPriority = Just 0.4
              },
            SitemapUrl
              { sitemapLoc = SmosR,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Yearly,
                sitemapPriority = Just 0.5
              },
            SitemapUrl
              { sitemapLoc = SmosFileR,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Yearly,
                sitemapPriority = Just 0.5
              },
            SitemapUrl
              { sitemapLoc = SmosKeybindingsR,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Yearly,
                sitemapPriority = Just 0.5
              },
            SitemapUrl
              { sitemapLoc = SmosActionsR,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Yearly,
                sitemapPriority = Just 0.5
              },
            SitemapUrl
              { sitemapLoc = SmosQueryR,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Yearly,
                sitemapPriority = Just 0.5
              },
            SitemapUrl
              { sitemapLoc = SmosQueryFilterR,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Yearly,
                sitemapPriority = Just 0.5
              },
            SitemapUrl
              { sitemapLoc = SmosQueryColumnR,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Yearly,
                sitemapPriority = Just 0.5
              },
            SitemapUrl
              { sitemapLoc = SmosQuerySorterR,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Yearly,
                sitemapPriority = Just 0.5
              }
          ],
          map
            ( \command ->
                SitemapUrl
                  { sitemapLoc = SmosQueryCommandR command,
                    sitemapLastMod = Nothing,
                    sitemapChangeFreq = Just Yearly,
                    sitemapPriority = Just 0.4
                  }
            )
            [ "entry",
              "report",
              "waiting",
              "next",
              "ongoing",
              "clock",
              "agenda",
              "projects",
              "stuck",
              "work",
              "free",
              "log",
              "stats",
              "tags"
            ],
          [ SitemapUrl
              { sitemapLoc = SmosSingleR,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Yearly,
                sitemapPriority = Just 0.5
              },
            SitemapUrl
              { sitemapLoc = SmosArchiveR,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Yearly,
                sitemapPriority = Just 0.5
              }
          ],
          map
            ( \command ->
                SitemapUrl
                  { sitemapLoc = SmosArchiveCommandR command,
                    sitemapLastMod = Nothing,
                    sitemapChangeFreq = Just Yearly,
                    sitemapPriority = Just 0.4
                  }
            )
            [ "file",
              "export"
            ],
          [ SitemapUrl
              { sitemapLoc = SmosNotifyR,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Yearly,
                sitemapPriority = Just 0.5
              },
            SitemapUrl
              { sitemapLoc = SmosCalendarImportR,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Yearly,
                sitemapPriority = Just 0.5
              },
            SitemapUrl
              { sitemapLoc = SmosSchedulerR,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Yearly,
                sitemapPriority = Just 0.5
              },
            SitemapUrl
              { sitemapLoc = SmosSchedulerTemplateR,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Yearly,
                sitemapPriority = Just 0.5
              },
            SitemapUrl
              { sitemapLoc = SmosSchedulerNixosR,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Yearly,
                sitemapPriority = Just 0.5
              }
          ],
          map
            ( \command ->
                SitemapUrl
                  { sitemapLoc = SmosSchedulerCommandR command,
                    sitemapLastMod = Nothing,
                    sitemapChangeFreq = Just Yearly,
                    sitemapPriority = Just 0.4
                  }
            )
            [ "check",
              "sample",
              "schedule",
              "next"
            ],
          [ SitemapUrl
              { sitemapLoc = SmosSyncR,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Yearly,
                sitemapPriority = Just 0.5
              },
            SitemapUrl
              { sitemapLoc = SmosSyncNixosR,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Yearly,
                sitemapPriority = Just 0.5
              }
          ],
          map
            ( \command ->
                SitemapUrl
                  { sitemapLoc = SmosSyncCommandR command,
                    sitemapLastMod = Nothing,
                    sitemapChangeFreq = Just Yearly,
                    sitemapPriority = Just 0.4
                  }
            )
            [ "register",
              "login",
              "sync"
            ],
          [ SitemapUrl
              { sitemapLoc = SmosServerR,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Yearly,
                sitemapPriority = Just 0.5
              },
            SitemapUrl
              { sitemapLoc = SmosWebServerR,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Yearly,
                sitemapPriority = Just 0.5
              },
            SitemapUrl
              { sitemapLoc = SmosGitHubR,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Yearly,
                sitemapPriority = Just 0.5
              }
          ],
          map
            ( \command ->
                SitemapUrl
                  { sitemapLoc = SmosGitHubCommandR command,
                    sitemapLastMod = Nothing,
                    sitemapChangeFreq = Just Yearly,
                    sitemapPriority = Just 0.4
                  }
            )
            [ "list",
              "import"
            ],
          [ SitemapUrl
              { sitemapLoc = SmosJobHuntR,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Yearly,
                sitemapPriority = Just 0.5
              }
          ],
          map
            ( \command ->
                SitemapUrl
                  { sitemapLoc = SmosJobHuntCommandR command,
                    sitemapLastMod = Nothing,
                    sitemapChangeFreq = Just Yearly,
                    sitemapPriority = Just 0.4
                  }
            )
            [ "init",
              "email"
            ],
          [ SitemapUrl
              { sitemapLoc = NixosModuleR,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Yearly,
                sitemapPriority = Just 0.5
              },
            SitemapUrl
              { sitemapLoc = HomeManagerModuleR,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Yearly,
                sitemapPriority = Just 0.5
              },
            SitemapUrl
              { sitemapLoc = DependencyGraphR,
                sitemapLastMod = Nothing,
                sitemapChangeFreq = Just Yearly,
                sitemapPriority = Just 0.5
              }
          ],
          map
            ( \(urlPieces, _) ->
                SitemapUrl
                  { sitemapLoc = PageR urlPieces,
                    sitemapLastMod = Nothing,
                    sitemapChangeFreq = Just Yearly,
                    sitemapPriority = Just 0.3
                  }
            )
            (M.toList dps)
        ]
  )
    <$> docPages
    <*> (M.keys <$> changelogs)
