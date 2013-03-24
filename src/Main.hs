--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend)
import Hakyll
import Metaplasm.Config
import System.FilePath (combine, takeFileName)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  let engineConf = defaultEngineConfiguration

  let siteConf = SiteConfiguration
        { siteRoot = "http://meta.plasm.us/"
        , siteGaId = "UA-36422511-1"
        }

  let feedConf = FeedConfiguration
        { feedTitle = "meta.plasm.us"
        , feedDescription = "Mostly Scala, some computational humanities"
        , feedAuthorName = "Travis Brown"
        , feedAuthorEmail = "travisrobertbrown@gmail.com"
        , feedRoot = "http://meta.plasm.us"
        }

  match "images/*.png" $ do
    route idRoute
    compile copyFileCompiler

  match (fromList $ vendorScriptFiles engineConf) $ do
    route $ customRoute (combine "js/vendor" . takeFileName . toFilePath)
    compile copyFileCompiler

  match (fromList $ lessFiles engineConf) $ do
    route $ setExtension "css"
    compile $ getResourceString
      >>= withItemBody 
        (unixFilter (lessCommand engineConf) $ "-" : (lessOptions engineConf))

  match (fromList ["content/about.rst", "content/contact.markdown"]) $ do
    route $ stripContent `composeRoutes` setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" (siteCtx siteConf)
      >>= relativizeUrls

  match "content/posts/*" $ do
    let ctx = postCtx siteConf
    route $ stripContent `composeRoutes` setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/post.html" ctx
      >>= saveSnapshot "content"
      >>= loadAndApplyTemplate "templates/default.html" ctx
      >>= relativizeUrls

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      let archiveCtx =
            field "posts" (\_ -> postList siteConf recentFirst) `mappend`
            constField "title" "Archives" `mappend`
            (siteCtx siteConf)

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls


  match "content/index.html" $ do
    route $ stripContent
    compile $ do
      let indexCtx = field "posts" $ \_ ->
            postList siteConf $ fmap (take 3) . recentFirst

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" (postCtx siteConf)
        >>= relativizeUrls

  create ["atom.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = (postCtx siteConf) `mappend` bodyField "description"
      posts <- fmap (take 10) . recentFirst =<< loadAllSnapshots "content/posts/*" "content"
      renderAtom feedConf feedCtx posts

  match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
siteCtx :: SiteConfiguration -> Context String
siteCtx siteConf =
  constField "root" (siteRoot siteConf) `mappend`
  constField "gaId" (siteGaId siteConf) `mappend`
  defaultContext


--------------------------------------------------------------------------------
postCtx :: SiteConfiguration -> Context String
postCtx siteConf = dateField "date" "%e %B %Y" `mappend` siteCtx siteConf


--------------------------------------------------------------------------------
postList :: SiteConfiguration -> ([Item String] -> Compiler [Item String]) -> Compiler String
postList siteConf sortFilter = do
  posts <- sortFilter =<< loadAll "content/posts/*"
  itemTpl <- loadBody "templates/post-item.html"
  list <- applyTemplateList itemTpl (postCtx siteConf) posts
  return list


--------------------------------------------------------------------------------
stripContent :: Routes
stripContent = gsubRoute "content/" $ const ""

