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
import Data.Ratio
import Data.Time
import Data.Time.Clock.POSIX

import qualified CacheDNS.DNS as DNS
import qualified CacheDNS.Cache.AtomicLRU as Cache

data DNSCache = DNSCache { lru :: Cache.AtomicLRU (DNS.Domain , Int) (Integer,DNS.DNSMessage)}

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
    let expired = timestamp + (toInteger (ttl message))
    Cache.insert ((domain message),(rtype message)) (expired,message) c
    where 
        ans m = head $ DNS.answer message
        domain m = DNS.rrname $ ans m
        ttl m = DNS.rrttl $ ans m
        rtype m = DNS.typeToInt $ DNS.rrtype $ ans m
        
lookupDNS :: DNS.Domain -> DNS.TYPE -> DNSCache -> IO (Maybe DNS.DNSMessage)
lookupDNS domain rtype cache = do
    let c = lru cache
        key = (domain, (DNS.typeToInt rtype))
    timestamp <- timeInSeconds
    val <- Cache.lookup key c
    case val of
        Nothing -> return Nothing
        Just (expired,message) -> do 
            if timestamp > expired then do
                Cache.delete key c
                return Nothing
            else return $ Just message
