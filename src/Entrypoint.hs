{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Entrypoint (main) where

import Data.Data (Proxy (Proxy))
import GHC.Generics
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.HTTP.ReverseProxy (ProxyDest (ProxyDest), WaiProxyResponse (WPRProxyDest), defaultOnExc, waiProxyTo)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Environment

handleReplay :: Handler String
handleReplay = return "OK"

forwardRequest :: Int -> Request -> IO WaiProxyResponse
forwardRequest forwardPort req =
  do
    print req
    pure $ WPRProxyDest $ ProxyDest "127.0.0.1" forwardPort

type TunnelingAPI = "replay" :> Get '[PlainText] String

type API = TunnelingAPI :<|> Raw

api :: Proxy API
api = Proxy

server :: Server TunnelingAPI
server = handleReplay

-- No idea why this works. source: https://github.com/parsonsmatt/incremental-servant/issues/1
reverseProxy :: Manager -> Int -> ServerT Raw m
reverseProxy manager forwardPort = Tagged $ waiProxyTo (forwardRequest forwardPort) defaultOnExc manager

app :: Manager -> Int -> Application
-- app manager = serve api $ server :<|> waiProxyTo forwardRequest defaultOnExc manager
app manager forwardPort = serve api $ server :<|> reverseProxy manager forwardPort

main :: IO ()
main = do
  args <- getArgs
  let forwardPort = read $ head args :: Int
  let str = "Tunneling Proxy Server! Forwarding requests to: " ++ show forwardPort
  putStrLn str
  manager <- newManager defaultManagerSettings
  run 30001 (app manager forwardPort)
