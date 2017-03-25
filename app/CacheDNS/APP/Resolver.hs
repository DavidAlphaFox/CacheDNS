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

loopQuery :: JQ.JobQueue -> IO()
loopQuery queue = do 
    infoM "CacheDNS.Resolver" $ "loopQuery....."
    mail <- JQ.fetchJob queue
    rs <- DNS.makeResolvSeed DNS.defaultResolvConf
    r <- withResolver rs $ \resolver -> do
        let (qn,qt) = mail
        DNS.lookupRaw resolver qn qt
    infoM "CacheDNS.Resolver" $  (show r)
    case r of
        Right message -> do
            let response = head $ answer message
            let rn = rrname response
                rd = rdata response
            
            maybeQueries <- atomically $ do
                queries  <- readTVar database
                let qs = M.lookup rn queries 
                modifyTVar database $ M.delete rn
                return qs
            case maybeQueries of
                Nothing -> return ()
                Just l -> do
                    infoM "CacheDNS.Resolver" $ (show l)
                    let l2 = L.map loopResponse l
                    infoM "CacheDNS.Resolver" $ (show l2)
                    return ()
                    where
                        loopResponse (m,sa) = do
                            let reqID = identifier $ header m
                            let hd = header message
                                nhd = hd {identifier = reqID}
                                m2 = message {header = nhd}
                            (m2,sa)
        Left e -> return ()