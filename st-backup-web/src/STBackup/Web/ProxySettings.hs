{-# LANGUAGE OverloadedStrings #-}

module STBackup.Web.ProxySettings where

import qualified Data.ByteString.Char8 as C
import Network.HTTP.Client (Manager)
import Network.HTTP.ReverseProxy
  ( ProxyDest (ProxyDest, pdHost, pdPort),
    WaiProxyResponse (WPRApplication, WPRModifiedRequest),
    defaultWaiProxySettings,
    waiProxyToSettings,
  )
import Network.Wai (Application, Request (rawPathInfo))

apiProxy :: Int -> Manager -> Application -> Application
apiProxy apiPort manager defaultApp =
  waiProxyToSettings getDest defaultWaiProxySettings manager
  where
    getDest req = do
      let _ : x : xs = C.split '/' (rawPathInfo req)
      if x == "api"
        then
          let req' = req {rawPathInfo = C.intercalate "/" xs}
           in pure $ WPRModifiedRequest req' proxyDest
        else pure $ WPRApplication defaultApp
    proxyDest = ProxyDest {pdHost = "localhost", pdPort = apiPort}
