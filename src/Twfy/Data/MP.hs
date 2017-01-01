{-|
Module      : Twfy.Data.MP
Description : MP data types

MP data types
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Twfy.Data.MP
       (
         MP(..)
       )
       where

import GHC.Generics
import Data.Aeson.TH
import qualified Data.Text as T
import Twfy.Util.Json (recordNameToJsonName)

-- | MP Data
--
-- Does not yet include office data
data MP = MP {

  -- | Member ID (TODO:who allocates these?). Arrives as text but appears to be integer
  mpMemberId :: T.Text

  -- | House (TODO: semantics?). Arrives as text but appears to be integer
  , mpHouse :: T.Text

  -- | Constituency name
  , mpConstituency :: T.Text

    -- | Party name. (TODO: what does independant show?)
  , mpParty :: T.Text

    -- | Date entered house. (TODO: convert to date - format appears to be YYYY-MM-DD)
  , mpEnteredHouse :: T.Text

    -- | Date left house. (TODO: convert to date - format appears to be YYYY-MM-DD and 9999-12-31 for Still Here)
  , mpLeftHouse :: T.Text

    -- | Reason for entering house
  , mpEnteredReason :: T.Text

    -- | Reason for leaving house
  , mpLeftReason :: T.Text

    -- | Person ID (TODO:who allocates these?). Arrives as text but appears to be integer
  , mpPersonId :: T.Text

    -- | Last update time stamp. (TODO: convert to time - format appears to be ''YYYY-MM-DD HH:MM:SS')
  , mpLastupdate :: T.Text

    -- | Title
  , mpTitle :: T.Text

    -- | Given name
  , mpGivenName :: T.Text

    -- | Family name
  , mpFamilyName :: T.Text

    -- | Full name
  , mpFullName :: T.Text

    -- | URL (TODO: looks like a URL Path for the TWFY site, should maybe create a URL here somehow)
  , mpUrl :: T.Text

    -- | Image (TODO: looks like a URL Path for the TWFY site, should maybe create a URL here somehow)
  , mpImage :: T.Text

    -- | Image height
  , mpImageHeight :: Int

    -- | Image width
  , mpImageWidth :: Int

    -- TODO: , mpOffice :: [Office]
  }
        deriving (Show,Generic)
$(deriveJSON defaultOptions { fieldLabelModifier = recordNameToJsonName } ''MP)

-- SAMPLE output from API - remaining stuff not included above
-- {
--   "office":[
--     {
--       "moffice_id":"uk.parliament.data/Member/4389/OppositionPost/1177",
--       "dept":"",
--       "position":"Shadow Minister (Housing)",
--       "from_date":"2016-10-10",
--       "to_date":"9999-12-31",
--       "person":"25343","source":""
--     },
--     {
--       "moffice_id":"uk.parliament.data/Member/4389/Committee/328",
--       "dept":"Women and Equalities Committee",
--       "position":"Member",
--       "from_date":"2015-07-06",
--       "to_date":"9999-12-31",
--       "person":"25343","source":""
--     }
--     ]
--   }
