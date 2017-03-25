{-# LANGUAGE OverloadedStrings #-}
module Main where
-- from Haskell or other Library
import Control.Monad
import Control.Monad.STM
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM

import Control.Exception

import System.IO
import System.Log.Logger

import Network.Socket hiding (recv, recvFrom, send, sendTo)
import Network.Socket.ByteString 

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS

import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C

-- from My Library
import CacheDNS.DNS as DNS
import CacheDNS.Cache.AtomicLRU (AtomicLRU)
import CacheDNS.Cache.AtomicLRU as Cache

-- from Main APP
import CacheDNS.APP.Log as Log
import CacheDNS.APP.Resolver as Resolver
import CacheDNS.APP.CacheManager as CM


data ServerConfig = ServerConfig { server_host :: Maybe String
                                 , server_port :: Maybe String
                                 }
data ServerShared = ServerShared { cache :: (AtomicLRU (String,String) (Integer,DNS.DNSMessage))
                                 , resolver :: ResolverQueue
                                 }
nameM = "CacheDNS"                                

main :: IO ()
main = do
    Log.setup [("", INFO)] 
    conf <- C.load [C.Required "application.conf"]
    
    server_host <- C.lookup conf "server.host" :: IO (Maybe String)
    server_port <- C.lookup conf "server.port" :: IO (Maybe String)
    cache_size <- C.lookup conf "server.cache_size" :: IO (Maybe Integer)

    let serverConfig = ServerConfig { server_host = server_host
                                    , server_port = server_port
                                    }
    cache <- newAtomicLRU cache_size
    resolver <- Resolver.newResolver
    forkIO $ forever $ Resolver.loopQuery resolver
    let serverShared = ServerShared { cache = cache
                                    , resolver = resolver 
                                    }
    udp serverConfig serverShared


udp :: ServerConfig -> ServerShared -> IO ()
udp c s = do
    let nameF = nameM ++ ".udp"
    let maxLength = 512 -- 512B is max length of UDP message
                      -- due ot rfc1035
  
    infoM nameF $ "starting UDP server"
    let hints = defaultHints { addrSocketType = Datagram, addrFlags = [AI_ADDRCONFIG, AI_PASSIVE]}
    addr:_ <- getAddrInfo (Just hints) (server_host c) (server_port c)
    bracket 
        (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
        close
        (\sock -> do
            bind sock (addrAddress addr)
            infoM nameF $ "bound to " ++ (show $ addrAddress addr)
            forever $ do
                (a, sa) <- recvFrom sock maxLength
                Resolver.addQuery (a, sa) (resolver s)
                return ()
        )