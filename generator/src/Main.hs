{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import       Data.Monoid (mappend)
import       Data.Maybe (fromMaybe)
import       Hakyll

config :: Configuration
config = defaultConfiguration
  { providerDirectory = "../site"
  , storeDirectory = "../hakyll-cache"
  , tmpDirectory = "../hakyll-cache/tmp"
  , destinationDirectory = "../site-dist"
  }

renderHtml :: Rules ()
renderHtml = do
  route   $ setExtension "html"
  compile $ pandocCompiler
    >>= loadAndApplyTemplate "templates/default.html" defaultContext

copyFiles :: Rules ()
copyFiles = do
  route   idRoute
  compile copyFileCompiler

main :: IO ()
main = hakyllWith config $ do
  match "404.md" renderHtml
  match "wiki/*" renderHtml
  match "pages/*" renderHtml

  match "robots.txt" copyFiles
  match "images/*" copyFiles

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html"  postContext
      >>= loadAndApplyTemplate "templates/default.html" postContext

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  create ["style.css"] $ do
      route idRoute
      compile $ do
          csses <- loadAll "css/*.css"
          makeItem $ unlines $ map itemBody csses

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx =
            listField "posts" postContext (return posts) `mappend`
            constField "title" "Archives"            `mappend`
            defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postContext (return posts) `mappend`
            defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx

  match "templates/*" $ compile templateBodyCompiler

authorContext :: forall a. Context a
authorContext = field "author" $ \item -> do
  metadata <- getMetadata (itemIdentifier item)
  return $ fromMaybe "Soares Chen" $ lookupString "author" metadata

postContext :: Context String
postContext = mconcat
  [ dateField "date" "%B %e, %Y"
  , authorContext
  , defaultContext
  ]
