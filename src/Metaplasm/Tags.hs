{-# LANGUAGE OverloadedStrings #-}
module Metaplasm.Tags where

import Data.List (intercalate, intersperse)
import Data.Maybe (catMaybes)
import Control.Monad (forM)
import Data.Monoid (mconcat)
import Hakyll
import Text.Blaze.Html (toHtml, toValue, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


renderSimpleTagList :: Tags -> Compiler (String)
renderSimpleTagList = renderTags makeLink (intercalate " | ")
  where
    makeLink tag url _ _ _ = renderHtml $
      H.a ! A.href (toValue url) $ toHtml tag

tagsFieldWith' :: (Identifier -> Compiler [String])
  -> String
  -> Tags
  -> Context a
tagsFieldWith' getTags' key tags = field key $ \item -> do
  tags' <- getTags' $ itemIdentifier item
  links <- forM tags' $ \tag -> do
    route' <- getRoute $ tagsMakeId tags tag
    return $ renderLink tag route'
  return $ renderHtml $ mconcat $ intersperse " | " $ catMaybes $ links
  where
    renderLink _   Nothing         = Nothing
    renderLink tag (Just filePath) = Just $
      H.a ! A.href (toValue $ toUrl filePath) $ toHtml tag

