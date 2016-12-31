{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Servant.Client

import System.Environment (lookupEnv)

import qualified Data.Text    as T

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Twfy.Api

-- TODO: add applicative opt parsing for urls, api key etc and commands for different API calls

-- TODO: take from command line or env
readApiKey :: IO (Maybe ApiKey)
readApiKey = do
  envKey <- lookupEnv "TWFY_API_KEY"
  let textKey = fmap T.pack envKey
  return textKey

displayResult :: Show a => (Either ServantError a) -> IO ()
displayResult result = do
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right something -> do
      print something

runQuery :: Show a => ClientM a -> IO ()
runQuery cs = do
  manager <- newManager tlsManagerSettings
  let env = (ClientEnv manager (BaseUrl Https "www.theyworkforyou.com" 443 "/api"))
  runClientM cs env >>= displayResult
  return ()

run :: Maybe ApiKey -> IO ()
run apiKey = do
  runQuery $ getConstituencies apiKey
  runQuery $ getConstituency apiKey (Just "Manchester, Gorton") Nothing

main :: IO ()
main = do
  apiKey <- readApiKey
  run apiKey
