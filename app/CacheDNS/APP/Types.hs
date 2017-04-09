{-# LANGUAGE OverloadedStrings #-}
module CacheDNS.APP.Types
    ( ServerConfig(..)
    , DNSKey(..)
    , DNSServer(..)
    , GlobalConfig(..)
    )
where
import qualified CacheDNS.DNS as DNS 

data ServerConfig = ServerConfig { server_host :: Maybe String
                                 , server_port :: Maybe String
                                 } deriving (Show)

data DNSServer = DNSServer { host :: Maybe String
                           , port :: Maybe String
                           , method :: Maybe String
                           , password :: Maybe String  
                           } deriving (Show)

data GlobalConfig = GlobalConfig { server :: DNSServer
                                 , stype :: String 
                                 , nserver :: Maybe [DNSServer]
                                 , cserver :: Maybe [DNSServer]
                                 , chinese :: Maybe String   
                                 } deriving (Show)

type DNSKey = (DNS.Domain,Int)