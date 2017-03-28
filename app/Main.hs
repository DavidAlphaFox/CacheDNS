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

import System.Posix.Daemonize
import System.IO
import System.Log.Logger

import qualified Data.List as L
import Data.Maybe
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C

-- from Main APP
import qualified CacheDNS.APP.Log as Log
import qualified CacheDNS.APP.Resolver as Resolver
import qualified CacheDNS.APP.CacheManager as CM
import qualified CacheDNS.APP.JobQueue as JQ
import qualified CacheDNS.APP.UDPServer as UDPServer
import CacheDNS.APP.Types

name = "CacheDNS"                                

loadUPStream :: C.Config -> IO (Maybe [HostPort])
loadUPStream conf = do
    upstream <- C.lookup conf "resolver.upstream" :: IO (Maybe [String])
    return $ Just $ L.map toHostPort $ fromJust upstream
    where 
        toHostPort v = 
            let (host,port) = L.break (== ':') v
            in 
            HostPort { host = Just $ host 
                     , port = Just $ (L.drop 1 port)
                     }
serviceLoop :: C.Config -> IO ()
serviceLoop conf = do 
    Log.setup [("", INFO)]
        
    host <- C.lookup conf "server.host" :: IO (Maybe String)
    port <- C.lookup conf "server.port" :: IO (Maybe String)
    cache_size <- C.lookup conf "server.cache_size" :: IO (Maybe Integer)
    upstream <- loadUPStream conf 
    let serverConfig = ServerConfig { server_host = host
                                    , server_port = port 
                                    }
    cache <- CM.newDNSCache cache_size
    jobs <- JQ.newJobQueue
    rs <- Resolver.createResolver $ fromJust upstream
    replicateM_ 4 $ forkIO $ forever $ Resolver.loopQuery jobs cache rs
    UDPServer.serve serverConfig cache jobs

main :: IO ()
main = do
    conf <- C.load [C.Required "application.conf"]
    -- daemonize $ serviceLoop conf
    serviceLoop conf

