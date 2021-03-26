{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module STBackup.Web where

#ifndef ghcjs_HOST_OS

import qualified Language.Javascript.JSaddle.Warp as JSaddle
import Miso (JSM, syncPoint)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import qualified Network.Wai.Handler.Warp as Warp
import Network.WebSockets (defaultConnectionOptions)
import STBackup.Web.ProxySettings (apiProxy)
import Network.HTTP.Types (status404)
import Network.Wai (responseLBS)
import STBackup.Config
    ( ApiServerConfig(_apiServerPort),
      WebServerConfig(_webServerPort) )
import STBackup.Config.Toml
    ( readApiServerConfig, readWebServerConfig )
import STBackup.Web.CommandLineParser (parseCommandLine, _configFile)

run :: JSM () -> IO ()
run f = do
  options <- parseCommandLine
  webConfig <- readWebServerConfig (_configFile options)
  apiConfig <- readApiServerConfig (_configFile options)

  manager <- newManager defaultManagerSettings

  let settings = Warp.setPort (_webServerPort webConfig)
           . Warp.setTimeout 3600
           $ Warp.defaultSettings
   in JSaddle.jsaddleWithAppOr
        defaultConnectionOptions
        (f >> syncPoint)
        (apiProxy (_apiServerPort apiConfig)
         manager (\_ resp -> resp $ responseLBS status404 [] ""))
    >>= Warp.runSettings settings

#else

run :: IO () -> IO ()
run f = f

#endif
