{-|
Module      : Twfy.Client
Description : Client
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Twfy.Client
       (
         A.ApiKey
       , Client(..)
       , client
       , getConstituency
       , getConstituencies
       , getMP
       , getMPs
       )
       where

import Servant.Client hiding(Client(..), client)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.Common.BaseUrl (parseBaseUrl)

import qualified Data.Text as T

import qualified Twfy.Api as A
import Twfy.Data.Constituency
import Twfy.Data.MP

-- | Client - very subject to change, use the 'client' function to create
data Client = Client {
  clientApiKey :: A.ApiKey -- ^ API Key
  , clientApiUri :: String -- ^ API URL
  , clientManager :: Manager -- ^ HTTP client manager
  , clientEnv :: ClientEnv -- ^ Servant client env
  }

instance Show Client where
  show c = let k = show (clientApiKey c)
               u = show (clientApiUri c)
           in "twfy-client: " ++ k ++ " - " ++ u

-- | Create a client.
--
client :: A.ApiKey -- ^ API Key
       -> Maybe String -- ^ API URI - defaults to "https://www.theyworkforyou.com/api"
       -> Maybe Manager -- ^ HTTP Client manager - uses default TLS settings if not provided
       -> IO Client
client k u m = let defaultUri = "https://www.theyworkforyou.com/api"
                   defaultManagerAction = newManager tlsManagerSettings
                   key = k
                   uri = maybe defaultUri id u
               in do
  baseUrl <- parseBaseUrl uri
  manager <- maybe defaultManagerAction return m
  let env = ClientEnv manager baseUrl
  return $ Client key uri manager env

-- | The 'getConstituency' function retreives a constituency based on name or post code
getConstituency :: Client
                -> Maybe T.Text -- ^ Name
                -> Maybe T.Text -- ^ Post code
                -> IO (Either ServantError Constituency)
getConstituency c = do
  let key = Just $ clientApiKey c
      env = clientEnv c
  (\name postCode -> (flip runClientM) env (A.getConstituency key name postCode))

-- | The 'getConstituencies' function retreives all constituencies
getConstituencies :: Client
                  -> IO (Either ServantError [Constituency])
getConstituencies c = do
  let key = Just $ clientApiKey c
      env = clientEnv c
  (flip runClientM) env (A.getConstituencies key)

-- | The 'getMP' function retreives an MP
getMP :: Client
         -> Maybe Int -- ^ Id
         -> Maybe T.Text -- ^ Constituency name
         -> Maybe T.Text -- ^ Post code
         -> Maybe Bool -- ^ Always return
         -> IO (Either ServantError MP)
getMP c = do
  let key = Just $ clientApiKey c
      env = clientEnv c
  (\mpId constituencyName postCode alwaysReturn -> (flip runClientM) env (A.getMP key mpId constituencyName postCode alwaysReturn))

-- | The 'getMPs' function retreives a list of MPs
getMPs :: Client
         -> Maybe T.Text -- ^ Name search
         -> Maybe T.Text -- ^ Party name
         -> Maybe T.Text -- ^ Date
         -> IO (Either ServantError [MP])
getMPs c = do
  let key = Just $ clientApiKey c
      env = clientEnv c
  (\personName partyName date -> (flip runClientM) env (A.getMPs key personName partyName date))
