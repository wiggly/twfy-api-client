{-|
Module      : Twfy.Api
Description : Client API definition
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Twfy.Api
       (
         TwfyAPI
       , ApiKey
       , getConstituency
       , getConstituencies
       )
       where

import Data.Proxy
import Servant.API
import Servant.Client
import qualified Data.Text as T

import Twfy.Data.JsonIso8859
import Twfy.Data.Constituency

-- | API Key
type ApiKey = T.Text

-- | Servant API definition
type TwfyAPI = "getConstituency"
               :> QueryParam "key" ApiKey
               :> QueryParam "name" T.Text
               :> QueryParam "postcode" T.Text
               :> Get '[JsonIso8859] Constituency
               :<|>
               "getConstituencies"
               :> QueryParam "key" ApiKey
               :> Get '[JsonIso8859] [Constituency]

twfyAPI :: Proxy TwfyAPI
twfyAPI = Proxy


-- TODO: can we factor out the API key somehow and pass it in when constructing the client?
-- |The 'getConstituency' function retreives a constituency based on name or post code
getConstituency :: Maybe ApiKey -- ^ API key
                   -> Maybe T.Text -- ^ Name
                   -> Maybe T.Text -- ^ Post code
                   -> ClientM Constituency

-- |The 'getConstituencies' function retreives all constituencies
getConstituencies :: Maybe ApiKey -- ^ API key
                  -> ClientM [Constituency]

(getConstituency :<|> getConstituencies) = client twfyAPI
