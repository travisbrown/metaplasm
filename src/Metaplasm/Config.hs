{-# LANGUAGE OverloadedStrings #-}
module Metaplasm.Config where

import Hakyll

data EngineConfiguration = EngineConfiguration
  { lessCommand :: String
  , lessFiles :: [Identifier]
  , lessOptions :: [String]
  , vendorScriptFiles :: [Identifier]
  }

defaultEngineConfiguration :: EngineConfiguration
defaultEngineConfiguration = EngineConfiguration
  { lessCommand = "lessc"
  , lessFiles = [ "css/main.less" ]
  , lessOptions = ["--compress"]
  , vendorScriptFiles = map (fromFilePath . (modulePath ++))
    [ "jquerymin/jquery-1.10.1.min.js"
    , "boot-scripts/bootstrap.min.js"
    , "modernizr/modernizr-2.6.2.min.js"
    ]
  }
  where
    modulePath = "lib/initializr/war/builder/modules/"

data SiteConfiguration = SiteConfiguration
  { siteRoot :: String
  , siteGaId :: String
  }

