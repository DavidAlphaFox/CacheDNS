{-# LANGUAGE OverloadedStrings #-}
module CacheDNS.APP.Types
    ( ServerConfig(..)
    )
where

data ServerConfig = ServerConfig { server_host :: Maybe String
                                 , server_port :: Maybe String
                                 } deriving (Show)
