{-# LANGUAGE OverloadedStrings #-}
module CacheDNS.APP.CryptoUDPServer
    ( serve
    )
where

import Control.Monad
import Control.Monad.STM
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception

import Network.Socket hiding (recv, recvFrom, send, sendTo)
import Network.Socket.ByteString 

import System.IO
import System.Log.Logger


import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe

import qualified CacheDNS.APP.CacheManager as CM
import qualified CacheDNS.DNS as DNS 
import qualified CacheDNS.IPC.Mailbox as MB

import CacheDNS.APP.Types
import qualified CacheDNS.APP.JobQueue as JQ
import qualified CacheDNS.APP.DNSHelper as DH

data CryptoUDPServer = CryptoUDPServer { db :: TVar (M.Map DNSKey [(Int,SockAddr)])
                                       , mailbox :: MB.Mailbox DNSKey
                                       , method :: String
                                       , password :: String
                                       }