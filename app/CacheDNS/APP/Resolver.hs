{-# LANGUAGE OverloadedStrings #-}
module CacheDNS.APP.Resolver
    ( ResolverQueue
    , newResolver
    , addQuery
    , loopQuery
    ) 
where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM

import System.Log.Logger

import Network.Socket hiding (recv, recvFrom, send, sendTo)
import Network.Socket.ByteString 

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Map as M
import Data.List as L

import CacheDNS.DNS as DNS
import CacheDNS.Cache.AtomicLRU as Cache
import CacheDNS.IPC.Mailbox as Mailbox

import CacheDNS.APP.Log as Log

data ResolverQueue = ResolverQueue { jobs :: Mailbox.Mailbox (BS.ByteString,DNS.TYPE)
                                    ,db :: TVar (M.Map DNS.Domain [(DNS.DNSMessage,SockAddr)])
                                   } 
newResolver :: IO ResolverQueue 
newResolver = do 
    jobs <- Mailbox.newMailboxIO
    db <- atomically $  newTVar M.empty
    return ResolverQueue{ jobs = jobs 
                        , db = db 
                        }

addQuery :: (BS.ByteString, SockAddr) -> ResolverQueue -> IO ()
addQuery (a,sa) queue = do
    let database = db queue
        mailbox = jobs queue

    case DNS.decode (BSL.fromStrict a) of
        Right message -> do
            let query = head $ question message
            let qn = qname query
                qt = qtype query

            atomically $ do
                queries  <- readTVar database
                case M.lookup qn queries of
                    Nothing -> do
                        Mailbox.writeMailbox mailbox (qn,qt)
                        modifyTVar database $ M.insert qn [(message,sa)]
                    Just l -> modifyTVar database $ M.insert qn ((message,sa):l)
            return ()
        Left e -> return ()
                        
loopQuery :: ResolverQueue -> IO()
loopQuery queue = do 
    infoM "CacheDNS.Resolver" $ "loopQuery....."
    let mailbox = jobs queue
        database = db queue
    mail <- atomically $ do Mailbox.readMailbox mailbox
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