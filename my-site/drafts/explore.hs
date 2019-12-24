#!/usr/bin/env stack
{- stack script
 --resolver nightly-2019-12-22
 --install-ghc
 --package: servant-server, warp
 --ghc-options -Wall
-}
{-# LANGUAGE DataKinds, TypeOperators, TypeApplications #-}

module FileServer where

import Network.Wai.Handler.Warp( defaultSettings, runSettings, setBeforeMainLoop, setPort)
import Servant (Proxy(Proxy), Raw, serve, serveDirectoryWebApp)

main :: IO ()
main = runSettings settings . serve (Proxy @Raw) $ serveDirectoryWebApp "."
  where port = 8080
        msg = "serving on http://localhost:" ++ show port ++ "/{pathToFile}"
        settings = setPort port $ setBeforeMainLoop (putStrLn msg) defaultSettings
