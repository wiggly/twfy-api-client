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
import qualified Data.Text    as T

import Twfy.Data.JsonIso8859
import Twfy.Data.Constituency

type ApiKey = T.Text

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

getConstituency :: Maybe ApiKey -> Maybe T.Text -> Maybe T.Text -> ClientM Constituency

getConstituencies :: Maybe ApiKey -> ClientM [Constituency]

(getConstituency :<|> getConstituencies) = client twfyAPI
