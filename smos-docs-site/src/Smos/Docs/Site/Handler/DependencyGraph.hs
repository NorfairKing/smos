module Smos.Docs.Site.Handler.DependencyGraph
  ( getDependencyGraphR,
  )
where

import Smos.Docs.Site.DependencyGraph
import Smos.Docs.Site.Handler.Import

getDependencyGraphR :: Handler TypedContent
getDependencyGraphR = case dependencyGraph of
  Nothing -> notFound
  Just contents -> sendResponse (TypedContent typeSvg (toContent contents))
