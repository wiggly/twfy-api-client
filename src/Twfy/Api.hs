{-|
Module      : Twfy.Api
Description : Client API definition

Servant client API for <https://www.theyworkforyou.com/ They Work For You>

Different calls are being implemented as needed for now so if you require any of the API calls that are not yet implemented please either submit a pull request via github or let me know you want it and I will add it myself.

TODO: getPerson, getMPInfo, getMPsInfo, getLord, getLords, getMLA, getMLAs, getMSP, getMSPs, getGeometry, getBoundary, getCommittee, getDebates, getWrans, getWMS, getHansard, getComments
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
       , getMP
       , getMPs
       )
       where

import Data.Proxy
import Servant.API
import Servant.Client
import qualified Data.Text as T

import Twfy.Data.JsonIso8859
import Twfy.Data.Constituency
import Twfy.Data.MP

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
               :<|>
               "getMP"
               :> QueryParam "key" ApiKey
               :> QueryParam "id" Int
               :> QueryParam "constituency" T.Text
               :> QueryParam "postcode" T.Text
               :> QueryParam "always_return" Bool
               :> Get '[JsonIso8859] MP
               :<|>
               "getMPs"
               :> QueryParam "key" ApiKey
               :> QueryParam "search" T.Text
               :> QueryParam "party" T.Text
               :> QueryParam "date" T.Text -- TODO: change this to a date
               :> Get '[JsonIso8859] [MP]

twfyAPI :: Proxy TwfyAPI
twfyAPI = Proxy

-- TODO: convertURL

-- | The 'getConstituency' function retreives a constituency based on name or post code
getConstituency :: Maybe ApiKey -- ^ API key
                   -> Maybe T.Text -- ^ Name
                   -> Maybe T.Text -- ^ Post code
                   -> ClientM Constituency

-- | The 'getConstituencies' function retreives all constituencies
getConstituencies :: Maybe ApiKey -- ^ API key
                  -> ClientM [Constituency]

-- | The 'getMP' function retreives an MP
getMP :: Maybe ApiKey -- ^ API key
         -> Maybe Int -- ^ Id
         -> Maybe T.Text -- ^ Constituency name
         -> Maybe T.Text -- ^ Post code
         -> Maybe Bool -- ^ Always return
         -> ClientM MP

-- | The 'getMPs' function retreives a list of MPs
getMPs :: Maybe ApiKey -- ^ API key
         -> Maybe T.Text -- ^ Name search
         -> Maybe T.Text -- ^ party name
         -> Maybe T.Text -- ^ Date
         -> ClientM [MP]

-- TODO: getPerson
-- TODO: getMPInfo
-- TODO: getMPsInfo
-- TODO: getLord
-- TODO: getLords
-- TODO: getMLA
-- TODO: getMLAs
-- TODO: getMSP
-- TODO: getMSPs
-- TODO: getGeometry
-- TODO: getBoundary
-- TODO: getCommittee
-- TODO: getDebates
-- TODO: getWrans
-- TODO: getWMS
-- TODO: getHansard
-- TODO: getComments

(getConstituency :<|> getConstituencies :<|> getMP :<|> getMPs) = client twfyAPI
