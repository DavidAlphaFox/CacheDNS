{-# LANGUAGE OverloadedStrings #-}
module CacheDNS.APP.CacheManager
    ( DNSCache
    , newDNSCache
    , insertDNS
    , lookupDNS
    )
where

import Control.Applicative

import System.IO
import System.Log.Logger

import qualified Data.ByteString as BS
import qualified Data.List as L
import Data.Ratio
import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX

import qualified CacheDNS.DNS as DNS
import qualified CacheDNS.Cache.AtomicLRU as Cache

import CacheDNS.APP.Types
import qualified CacheDNS.APP.DNSHelper as DH

data DNSCache = DNSCache { lru :: Cache.AtomicLRU DNSKey (Integer,DNS.DNSMessage)}

timeInMicros :: IO Integer
timeInMicros = numerator . toRational . (* 1000000) <$> getPOSIXTime

timeInMillis :: IO Integer
timeInMillis = (`div` 1000) <$> timeInMicros

timeInSeconds :: IO Integer
timeInSeconds = (`div` 1000) <$> timeInMillis

newDNSCache :: Maybe Integer -> IO DNSCache
newDNSCache size = do
    lru <- Cache.newAtomicLRU size
    return DNSCache {lru = lru}

insertDNS :: DNS.DNSMessage -> DNSCache -> IO ()
insertDNS message cache = do
    let c = lru cache
    timestamp <- timeInSeconds
    case DH.ttl message of
        Just ttl -> Cache.insert (qname ,qtype) ( (expired timestamp ttl),message) c
        Nothing -> Cache.insert (qname ,qtype) ( (expired timestamp 600),message) c
    where 
        expired ts tl = ts + (toInteger tl)
        qname  = fromJust $ DH.qname message 
        qtype = fromJust $ DH.qtypeInt message 
        
lookupDNS :: DNSKey -> DNSCache -> IO (Maybe DNS.DNSMessage)
lookupDNS key cache = do
    let c = lru cache
    timestamp <- timeInSeconds
    val <- Cache.lookup key c
    case val of
        Nothing -> return Nothing
        Just (expired,message) -> do 
            if timestamp > expired then do
                Cache.delete key c
                return Nothing
            else return $ Just message
