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

data Constituency = Constiutency {
  consituencyName :: T.Text
  , constituencyBbcConstituencyId :: Maybe T.Text -- looks like an integer
  , constituencyGuardianElectionResults :: Maybe T.Text -- URL
  , constituencyGuardianId :: Maybe T.Text -- looks like an integer
  , constituencyGuardianName :: Maybe T.Text
  , constituencyPaId :: Maybe T.Text -- looks like an integer
  }
                  deriving (Generic)
$(deriveJSON defaultOptions { fieldLabelModifier = recordNameToJsonName } ''Constituency)

instance Show Constituency where
  show c = let name = Just $ consituencyName c
               bbc = fmap (mappend "bbc_id: ") $ constituencyBbcConstituencyId c
               guardian_id = fmap (mappend "guardian_id: ") $ constituencyGuardianId c
               guardian_name = fmap (mappend "guardian_name: ") $ constituencyGuardianName c
               parts = catMaybes [ name, bbc, guardian_id, guardian_name ]
           in show $ T.intercalate " - " parts
