{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Servant.Client (ServantError)
import System.Environment (lookupEnv)
import qualified Data.Text as T

import Twfy.Client

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

run :: ApiKey -> IO ()
run apiKey = do
  c <- client apiKey Nothing Nothing
  putStrLn $ show c
  getConstituencies c >>=  displayResult
  getConstituency c Nothing (Just "TW8 0QU") >>= displayResult
  getMP c Nothing (Just "Manchester, Gorton") Nothing (Just False) >>= displayResult
  getMPs c Nothing (Just "Labour") Nothing >>= displayResult

main :: IO ()
main = do
  apiKey <- readApiKey
  maybe (putStrLn "Can't run without API KEY") run apiKey
