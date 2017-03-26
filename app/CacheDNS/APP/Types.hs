{-# LANGUAGE OverloadedStrings #-}
module CacheDNS.APP.Types
    ( ServerConfig(..)
    , HostPort(..)
    )
where

data ServerConfig = ServerConfig { server_host :: Maybe String
                                 , server_port :: Maybe String
                                 } deriving (Show)

data HostPort = HostPort { host :: Maybe String
                         , port :: Maybe String
                         } deriving (Show)