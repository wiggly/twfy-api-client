{-|
Module      : Twfy.Data.JsonIso8859
Description : Content-Type not provided by Servant

Instances for Accept and MimeUnrender to allow Servant client to read 'text/javascript; charset=iso-8859-1'
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Twfy.Data.JsonIso8859
       (
         JsonIso8859
       )
       where

import Prelude ()
import Prelude.Compat

import Servant
import Data.Aeson

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Media as Media

-- | Type for 'text/plain; charset=iso8859-1'
data JsonIso8859

instance Accept JsonIso8859 where
  contentType _ = "text" Media.// "javascript" Media./: ("charset", "iso-8859-1")

instance FromJSON a => MimeUnrender JsonIso8859 a where
  mimeUnrender _ = eitherDecode . from8859ToUtf8

from8859ToUtf8 :: LBS.ByteString -> LBS.ByteString
from8859ToUtf8 iso = let isoText = TE.decodeLatin1 $ LBS.toStrict iso
                         utf8 = TE.encodeUtf8 isoText
                     in LBS.fromStrict utf8
