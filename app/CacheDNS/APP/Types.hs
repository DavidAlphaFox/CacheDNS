{-# LANGUAGE OverloadedStrings #-}
module CacheDNS.APP.Types
    ( ServerConfig(..)
    , HostPort(..)
    , DNSKey(..)
    )
where
import qualified CacheDNS.DNS as DNS 

data ServerConfig = ServerConfig { server_host :: Maybe String
                                 , server_port :: Maybe String
                                 } deriving (Show)

data HostPort = HostPort { host :: Maybe String
                         , port :: Maybe String
                         } deriving (Show)

type DNSKey = (DNS.Domain,Int)