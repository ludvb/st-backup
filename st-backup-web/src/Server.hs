{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (status200, status404)
import Network.Wai (rawPathInfo, responseFile, responseLBS)
import qualified Network.Wai.Handler.Warp as Warp
import STBackup.Config
  ( ApiServerConfig (_apiServerPort),
    WebServerConfig (_webServerPort),
  )
import STBackup.Config.Toml
  ( readApiServerConfig,
    readWebServerConfig,
  )
import STBackup.Web.CommandLineParser (parseCommandLine, _configFile)
import STBackup.Web.ProxySettings (apiProxy)

run :: IO ()
run = do
  options <- parseCommandLine
  webConfig <- readWebServerConfig (_configFile options)
  apiConfig <- readApiServerConfig (_configFile options)

  manager <- newManager defaultManagerSettings

  let settings =
        Warp.setPort (_webServerPort webConfig)
          . Warp.setTimeout 3600
          $ Warp.defaultSettings
      app = apiProxy (_apiServerPort apiConfig) manager $
        \req resp -> case rawPathInfo req of
          "/" ->
            resp $
              responseFile
                status200
                [("Content-Type", "text/html")]
                "../http/index.html"
                Nothing
          "/all.js" ->
            resp $
              responseFile
                status200
                [("Content-Type", "script/javascript")]
                "../http/all.js"
                Nothing
          _ -> resp $ responseLBS status404 [] ""
   in Warp.runSettings settings app

main :: IO ()
main = run
