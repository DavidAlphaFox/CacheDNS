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


import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C

-- from Main APP
import qualified CacheDNS.APP.Log as Log
import qualified CacheDNS.APP.Resolver as Resolver
import qualified CacheDNS.APP.CacheManager as CM
import qualified CacheDNS.APP.JobQueue as JQ
import qualified CacheDNS.APP.UDPServer as UDPServer
import CacheDNS.APP.Types

nameM = "CacheDNS"                                

main :: IO ()
main = do
    Log.setup [("", INFO)] 
    conf <- C.load [C.Required "application.conf"]
    
    host <- C.lookup conf "server.host" :: IO (Maybe String)
    port <- C.lookup conf "server.port" :: IO (Maybe String)
    cache_size <- C.lookup conf "server.cache_size" :: IO (Maybe Integer)

    let serverConfig = ServerConfig { server_host = host
                                    , server_port = port 
                                    }
    cache <- CM.newDNSCache cache_size
    jobs <- JQ.newJobQueue
    forkIO $ forever $ Resolver.loopQuery jobs cache
    UDPServer.serve serverConfig cache jobs
    return ()
