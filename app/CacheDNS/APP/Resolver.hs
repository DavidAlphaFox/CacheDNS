{-# LANGUAGE OverloadedStrings #-}
module CacheDNS.APP.Resolver
    ( loopQuery
    ) 
where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM

import System.Log.Logger

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS

import qualified CacheDNS.DNS as DNS
import qualified CacheDNS.IPC.Mailbox as MB

import qualified CacheDNS.APP.JobQueue as JQ
import qualified CacheDNS.APP.CacheManager as CM
name :: String 
name = "CacheDNS.Resolver"
loopQuery :: JQ.JobQueue -> CM.DNSCache -> IO()
loopQuery queue cache = do 
    infoM name  $ "loopQuery....."
    mail <- JQ.fetchJob queue
    rs <- DNS.makeResolvSeed DNS.defaultResolvConf
    r <- DNS.withResolver rs $ \resolver -> do
        DNS.lookupRaw resolver (qname mail) (qtype mail)
    case r of
        Left e -> return ()
        Right response -> do 
            CM.insertDNS response cache
            atomically $ do 
                MB.writeMailbox (mailbox mail) ((qname mail),(qtype mail))
    where 
        message (m,mb) = m
        mailbox (m,mb) = mb
        qname mail = DNS.qname $ head $ DNS.question $ message mail
        qtype mail = DNS.qtype $ head $ DNS.question $ message mail