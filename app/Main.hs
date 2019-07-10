{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative (Alternative (..), (<$>))
import Control.Monad (filterM)
import Data.FIT.Parse (parseBytes)
import Data.List (intersperse, isSuffixOf)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Map.Strict as MS
import Data.Monoid (mappend)
import Data.Running (Coord (..), PointRecord (..))
import Data.Running.KML
import Hakyll
import Metaplasm.Config
import Metaplasm.Running
import Metaplasm.Tags
import Skylighting (defaultSyntaxMap)
import Skylighting.Parser (addSyntaxDefinition, parseSyntaxDefinition)
import System.FilePath (combine, splitExtension, takeFileName)
import Text.Pandoc.Options (writerSyntaxMap)
import Text.XML

hakyllConf :: Configuration
hakyllConf = defaultConfiguration
  { deployCommand = "rsync -ave 'ssh' _site/* meta.plasm.us:www/meta.plasm.us"
  }

siteConf :: SiteConfiguration
siteConf = SiteConfiguration
  { siteRoot = "https://meta.plasm.us"
  , siteGaId = "UA-36422511-1"
  }

feedConf :: String -> FeedConfiguration
feedConf title = FeedConfiguration
  { feedTitle = "meta.plasm.us: " ++ title
  , feedDescription = "Mostly Scala, some computational humanities"
  , feedAuthorName = "Travis Brown"
  , feedAuthorEmail = "travisrobertbrown@gmail.com"
  , feedRoot = "https://meta.plasm.us"
  }

main :: IO ()
main = hakyllWith hakyllConf $ do
  scalaSyntaxDef <- preprocess $ parseSyntaxDefinition "syntax/scala.xml"

  let syntaxDef = either (error "Missing syntax/scala.xml") id scalaSyntaxDef
  let engineConf = defaultEngineConfiguration
  let writerOptions = defaultHakyllWriterOptions {
    writerSyntaxMap = addSyntaxDefinition syntaxDef defaultSyntaxMap
  }

  let pandocHtml5Compiler =
        pandocCompilerWith defaultHakyllReaderOptions writerOptions

  tags <- buildTags "content/posts/*" (fromCapture "tags/*/index.html")

  let postTagsCtx = postCtx tags

  match "content/tmp/*" $ do
    route stripContent
    compile copyFileCompiler

  match "images/*.png" $ do
    route idRoute
    compile copyFileCompiler

  match "lib/Font-Awesome/webfonts/*" $ do
    route $ customRoute (combine "webfonts" . takeFileName . toFilePath)
    compile copyFileCompiler

  match "extra/*" $ do
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

  match "content/about/index.md" $ do
    route $ stripContent `composeRoutes` setExtension "html"
    compile $ pandocHtml5Compiler
      >>= loadAndApplyTemplate "templates/about.html"  siteCtx
      >>= loadAndApplyTemplate "templates/default.html" siteCtx
      >>= relativizeUrls
      >>= deIndexUrls

  tagsRules tags $ \tag pattern -> do
    let title = "Posts tagged " ++ tag

    route idRoute
    compile $ do
      list <- postList tags (\t -> recentFirst t >>= filterM (fmap (elem tag) . getTags . itemIdentifier))
      let ctx =
            constField "tag" tag `mappend`
            constField "posts" list `mappend`
            constField "feedTitle" title `mappend`
            constField "title" title `mappend`
            constField "feedUrl" ("/tags/" ++ tag ++ "/index.xml") `mappend`
            siteCtx
      makeItem ""
        >>= loadAndApplyTemplate "templates/tag-posts.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls
        >>= deIndexUrls

    version "rss" $ do
      let feedCtx = postCtx tags `mappend` bodyField "description"
      route $ setExtension "xml"
      compile $ loadAllSnapshots pattern "content"
        >>= fmap (take 10) . recentFirst
        >>= renderAtom (feedConf title) feedCtx

  match "content/posts/*" $ do
    route $ directorizeDate `composeRoutes` stripContent `composeRoutes` setExtension "html"
    compile $ do
      compiled <- pandocHtml5Compiler
      full <- loadAndApplyTemplate "templates/post.html" postTagsCtx compiled
      teaser <- loadAndApplyTemplate "templates/post-teaser.html" postTagsCtx $ dropMore compiled
      saveSnapshot "content" full
      saveSnapshot "teaser" teaser
      loadAndApplyTemplate "templates/default.html" (postCtx tags) full
        >>= relativizeUrls
        >>= deIndexUrls

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      let archiveCtx =
            field "posts" (\_ -> postList tags recentFirst) `mappend`
            constField "title" "Archives" `mappend` siteCtx

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  match "content/index.html" $ do
    route stripContent
    compile $ do
      tpl <- loadBody "templates/post-item-full.html"
      body <- readTemplate . itemBody <$> getResourceBody
      loadAllSnapshots "content/posts/*" "teaser"
        >>= fmap (take 100) . recentFirst
        >>= applyTemplateList tpl (postCtx tags)
        >>= makeItem
        >>= applyTemplate body (siteCtx `mappend` bodyField "posts")
        >>= loadAndApplyTemplate "templates/default.html" siteCtx
        >>= relativizeUrls
        >>= deIndexUrls

  create ["atom.xml"] $ do
    route idRoute
    compile $ do
      let feedCtx = postCtx tags `mappend` bodyField "description"
      posts <- mapM deIndexUrls =<< fmap (take 10) . recentFirst =<<
        loadAllSnapshots "content/posts/*" "content"
      renderAtom (feedConf "blog") feedCtx (posts)

  tracks <- fitTracks "content/running/*.FIT"

  match "content/running/*.FIT" $ do
    route $ fitRoute tracks
    compile $ do
      Item _ maybePoints <- fmap parseBytes <$> getResourceLBS
      let points = either (error "FIT parsing failure") id maybePoints
      let ctx = fitCtx points siteCtx
      body <- fitBody tracks points 1.0 4
      makeItem body
        >>= loadAndApplyTemplate "templates/running/run.html" ctx
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls
        >>= deIndexUrls

  match "content/running/index.html" $ do
    route stripContent
    compile $ do
      tpl <- loadBody "templates/post-item-full.html"
      body <- readTemplate . itemBody <$> getResourceBody
      take 10 . fitRecentFirst tracks <$> loadAllSnapshots "content/running/*.FIT" "content"
        >>= applyTemplateList tpl siteCtx
        >>= makeItem
        >>= applyTemplate body (siteCtx `mappend` bodyField "posts")
        >>= loadAndApplyTemplate "templates/default.html" (constField "gMapsApiScript" gMapsApiScript `mappend` siteCtx)
        >>= relativizeUrls
        >>= deIndexUrls
      
  match "content/running/all/index.html" $ do
    route stripContent
    compile $ do
      let onlyClose ((PointRecord _ (Coord (lat, lng)) _ _) : _) = sqrt ((38.976646 - lat) ** 2 +  (-76.936947 - lng) ** 2) < 0.2
      let onlyCloseTracks = M.filter onlyClose tracks
      let points = concat $ M.elems onlyCloseTracks
      body <- fitBody onlyCloseTracks points 0.5 3
      let ctx = fitCtx (concat $ M.elems tracks) siteCtx
      makeItem body
        >>= loadAndApplyTemplate "templates/running/run.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls
        >>= deIndexUrls

  match "templates/*" $ compile templateCompiler
  match "templates/*/*" $ compile templateCompiler

siteCtx :: Context String
siteCtx =
  deIndexedUrlField "url" `mappend`
  constField "root" (siteRoot siteConf) `mappend`
  constField "gaId" (siteGaId siteConf) `mappend`
  constField "feedTitle" "Posts" `mappend`
  constField "feedUrl" "/atom.xml" `mappend`
  constField "gMapsApiScript" "" `mappend`
  defaultContext

postCtx :: Tags -> Context String
postCtx tags =
  dateField "date" "%e %B %Y" `mappend`
  dateField "datetime" "%Y-%m-%d" `mappend`
  (tagsFieldWith' getTags) "tags" tags `mappend`
  siteCtx

postList :: Tags -> ([Item String] -> Compiler [Item String]) -> Compiler String
postList tags sortFilter = do
  posts <- sortFilter =<< loadAll "content/posts/*"
  itemTpl <- loadBody "templates/post-item.html"
  list <- applyTemplateList itemTpl (postCtx tags) posts
  return list

stripContent :: Routes
stripContent = gsubRoute "content/" $ const ""

directorizeDate :: Routes
directorizeDate = customRoute (\i -> directorize $ toFilePath i)
  where
    directorize path = dirs ++ "/index" ++ ext 
      where
        (dirs, ext) = splitExtension $ concat $
          (intersperse "/" date) ++ ["/"] ++ (intersperse "-" rest)
        (date, rest) = splitAt 3 $ splitOn "-" path

stripIndex :: String -> String
stripIndex url = if "index.html" `isSuffixOf` url && elem (head url) ("/." :: String)
  then take (length url - 10) url else url

deIndexUrls :: Item String -> Compiler (Item String)
deIndexUrls item = return $ fmap (withUrls stripIndex) item

deIndexedUrlField :: String -> Context a
deIndexedUrlField key = field key
  $ fmap (stripIndex . maybe empty toUrl) . getRoute . itemIdentifier

dropMore :: Item String -> Item String
dropMore = fmap (unlines . takeWhile (/= "<!-- MORE -->") . lines)

