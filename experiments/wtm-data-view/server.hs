#! /usr/bin/env stack
{-
  stack --resolver lts-12.13
        --install-ghc runghc
        --package wai-app-static
-}
import Network.Wai.Handler.Warp (run)
import Network.Wai.Application.Static

main :: IO ()
main = run 8000 (staticApp (defaultFileServerSettings "."))
