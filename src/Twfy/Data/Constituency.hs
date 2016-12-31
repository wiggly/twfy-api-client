{-|
Module      : Twfy.Data.Constituency
Description : Constituency data types

Constituency data types
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Twfy.Data.Constituency
       (
         Constituency(..)
       )
       where

import GHC.Generics
import Data.Aeson.TH
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Twfy.Util.Json (recordNameToJsonName)

-- | Constituency Data
data Constituency = Constiutency {
  -- | Constituency name
  consituencyName :: T.Text
  -- | BBC ID for constituency. Arrives as text but appears to be integer
  , constituencyBbcConstituencyId :: Maybe T.Text
    -- | Guardian ID for constituency. Arrives as text but appears to be integer
  , constituencyGuardianId :: Maybe T.Text
    -- | Guardian name for constituency
  , constituencyGuardianName :: Maybe T.Text
    -- | Guardian election results URL
  , constituencyGuardianElectionResults :: Maybe T.Text
    -- | TOOD: find out what this is. Arrives as text but appears to be integer.
  , constituencyPaId :: Maybe T.Text
  }
                  deriving (Generic)
$(deriveJSON defaultOptions { fieldLabelModifier = recordNameToJsonName } ''Constituency)

instance Show Constituency where
  -- | Convert a constituency to a String
  show c = let name = Just $ consituencyName c
               bbc = fmap (mappend "bbc_id: ") $ constituencyBbcConstituencyId c
               guardian_id = fmap (mappend "guardian_id: ") $ constituencyGuardianId c
               guardian_name = fmap (mappend "guardian_name: ") $ constituencyGuardianName c
               parts = catMaybes [ name, bbc, guardian_id, guardian_name ]
           in show $ T.intercalate " - " parts
