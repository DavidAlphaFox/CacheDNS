{-# LANGUAGE OverloadedStrings #-}
module CacheDNS.APP.CmdParser 
    ( readConfig
    ) where

import Control.Applicative
import Data.Monoid ((<>))
import Prelude hiding (id)

import Data.Maybe
import qualified Data.ByteString.Lazy as L
import Data.Aeson (decode', FromJSON(..), Value(..), (.:),(.:?) )

import CacheDNS.APP.Types

instance FromJSON DNSServer where 
    parseJSON (Object v) = DNSServer <$> v .: "host"
                                     <*> v .: "port"
                                     <*> v .:? "method"
                                     <*> v .:? "password"
    parseJSON _ = empty
instance FromJSON GlobalConfig where 
    parseJSON (Object v) = GlobalConfig <$> v .: "server"
                                        <*> v .: "stype"
                                        <*> v .:? "nserver"
                                        <*> v .:? "cserver" 
                                        <*> v .:? "chinese"
    parseJSON _ = empty

readConfig :: FilePath -> IO (Maybe GlobalConfig)
readConfig fp = decode' <$> L.readFile fp