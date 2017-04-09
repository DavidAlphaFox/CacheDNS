{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module CacheDNS.APP.CmdParser 
    ( parseConfigOptions
    ) where

import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Control.Applicative
import Control.Exception (Exception, IOException, catch)
import Data.Monoid ((<>))
import Prelude hiding (id)

import Data.Maybe
import qualified Data.ByteString.Lazy as L
import Data.Aeson (decode', FromJSON(..), Value(..), (.:),(.:?) )

import Options.Applicative

import CacheDNS.APP.Types

data Options = Options
    { _config :: Maybe String
    } deriving (Show)


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
    
configOptions :: Parser Options
configOptions = Options
    <$> optional (strOption (long "config" <> short 'c' <> metavar "CONFIG"
               <> help "path to config file"))

parseConfigOptions :: IO (Maybe GlobalConfig)
parseConfigOptions = do
    o <- execParser $ info (helper <*> configOptions)
                      (fullDesc <> header "CacheDNS - a fast DNS Cache")
    let configFile = fromMaybe "CacheDNS.json" (_config o)
    mconfig <- readConfig configFile `catch` \(e :: IOException) ->
        hPutStrLn stderr ("ERROR: Failed to load " <> show e) >> exitFailure
    return mconfig