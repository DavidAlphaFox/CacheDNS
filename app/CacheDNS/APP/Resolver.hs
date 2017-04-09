{-# LANGUAGE OverloadedStrings #-}
module CacheDNS.APP.Resolver
    ( loopQuery
    , createResolver
    ) 
where

import Control.Applicative

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM

import System.Log.Logger
import Network
import Network.Socket hiding (recv, recvFrom, send, sendTo)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.List as L
import Data.Maybe

import qualified CacheDNS.DNS as DNS
import qualified CacheDNS.IPC.Mailbox as MB

import qualified CacheDNS.APP.JobQueue as JQ
import qualified CacheDNS.APP.CacheManager as CM
import qualified CacheDNS.APP.DNSHelper as DH
import CacheDNS.APP.Types 

data ResolverServer = ResolverServer { resolvers :: [DNS.ResolvSeed] }

name :: String 
name = "CacheDNS.Resolver"

createResolver :: [DNSServer] -> IO ResolverServer
createResolver hps = do 
    rs <- mapM DNS.makeResolvSeed $ L.map toResolvSeed hps
    return $ ResolverServer { resolvers = rs }
    where 
        toPortNum :: Maybe String -> Maybe PortNumber
        toPortNum (Just pn) = Just $ fromInteger $ toInteger (read pn :: Int)
        toPortNum Nothing = Nothing
        toResolvSeed dnsServer  =  do
            let h = fromJust $ host dnsServer
                p = toPortNum $ port dnsServer
            case p of
                Just pn -> DNS.defaultResolvConf { DNS.resolvInfo = DNS.RCHostPort h pn }
                Nothing -> DNS.defaultResolvConf { DNS.resolvInfo = DNS.RCHostName h}
            
            
resolv :: Int -> [DNS.Resolver] -> DNS.Domain -> DNS.TYPE -> IO (Either DNS.DNSError DNS.DNSMessage)
resolv 1 resolvers dom qt = DNS.lookupRaw (head resolvers) dom qt
resolv _ resolvers dom qt = do
    asyncs <- mapM async actions
    snd <$> waitAnyCancel asyncs
  where
    actions = map (\res -> DNS.lookupRaw res dom qt) resolvers

loopQuery :: JQ.JobQueue -> CM.DNSCache -> ResolverServer ->  IO()
loopQuery queue cache server = do 
    infoM name  $ "loopQuery....."
    mail <- JQ.fetchJob queue
    let (qname,qtype) = message mail
    r <- DNS.withResolvers (resolvers server) $ \resolvers -> do 
        resolv serverCount resolvers qname (DNS.intToType qtype)
    case r of
        -- here should retry and notify sender to remove task
        Left e -> return ()
        Right response -> do 
            CM.insertDNS response cache
    atomically $ do 
            MB.writeMailbox (mailbox mail) (qname,qtype)
    where 
        message (m,mb) = m
        mailbox (m,mb) = mb
        serverCount = L.length $ resolvers server