{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Entrypoint (main) where

import Control.Concurrent.STM.TVar (TVar, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Data (Proxy (Proxy))
import GHC.Generics
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.HTTP.ReverseProxy (ProxyDest (ProxyDest), WaiProxyResponse (WPRProxyDest), defaultOnExc, waiProxyTo)
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.Environment

-- servant docs for addingin memory storage: https://docs.servant.dev/_/downloads/en/stable/pdf/
newtype State = State
  { requests :: TVar [Request]
  }

type AppM = ReaderT State Handler

handleReplay :: Int -> AppM String
handleReplay forwardPort = do
  State {requests = r} <- ask
  request <- liftIO $ readTVarIO r
  let str = "replaying: " ++ show request
  liftIO $ print str
  -- This can fail with exception hehe
  liftIO $ pure $ WPRProxyDest $ ProxyDest "127.0.0.1" forwardPort
  return "OK"

forwardRequest :: Int -> Request -> ReaderT State IO WaiProxyResponse
forwardRequest forwardPort req = do
  State {requests = r} <- ask
  liftIO $ atomically $ readTVar r >>= writeTVar r . (req :)
  pure $ WPRProxyDest $ ProxyDest "127.0.0.1" forwardPort

type TunnelingAPI = "replay" :> Get '[PlainText] String

type API = TunnelingAPI :<|> Raw

nt :: State -> AppM a -> Handler a
nt s x = runReaderT x s

foo :: State -> ReaderT State IO a -> IO a
foo s x = runReaderT x s

api :: Proxy API
api = Proxy

server :: Int -> ServerT TunnelingAPI AppM
server = handleReplay

-- No idea why this works. source: https://github.com/parsonsmatt/incremental-servant/issues/1
reverseProxy :: Manager -> Int -> State -> ServerT Raw AppM
reverseProxy manager forwardPort s =
  Tagged $
    waiProxyTo requestFunc defaultOnExc manager
  where
    requestFunc req = foo s (forwardRequest forwardPort req)

app :: Manager -> Int -> State -> Application
-- app manager = serve api $ server :<|> waiProxyTo forwardRequest defaultOnExc manager
-- app manager forwardPort = serve api $ server :<|> reverseProxy manager forwardPort
app manager forwardPort s = serve api $ hoistServer api (nt s) (server forwardPort :<|> reverseProxy manager forwardPort s)

main :: IO ()
main = do
  args <- getArgs
  let forwardPort = read $ head args :: Int
  let str = "Tunneling Proxy Server! Forwarding requests to: " ++ show forwardPort
  putStrLn str
  manager <- newManager defaultManagerSettings
  initialRequests <- newTVarIO []
  run 30001 (app manager forwardPort (State {requests = initialRequests}))
