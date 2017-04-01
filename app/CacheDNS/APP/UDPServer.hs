{-# LANGUAGE OverloadedStrings #-}
module CacheDNS.APP.UDPServer
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

data UDPServer = UDPServer { db :: TVar (M.Map DNSKey [(Int,SockAddr)])
                            ,mailbox :: MB.Mailbox DNSKey
                            }
name :: String
name = "CacheDNS.udp"

hint :: DNS.DNSMessage -> CM.DNSCache -> IO (Maybe DNS.DNSMessage)
hint message cache = do
    val <- CM.lookupDNS (qname,qtype)  cache 
    case val of
        Nothing -> return Nothing
        Just r -> do
            infoM  name $ "Hinted: " ++ (show qname)
            let rid = DH.identifier message
                hd = DNS.header r 
                newHeader = hd { DNS.identifier = rid}
            return $ Just r{DNS.header = newHeader}
    where 
        qname  = fromJust $ DH.qname message 
        qtype = fromJust $ DH.qtypeInt message 

createUDPServer :: IO UDPServer 
createUDPServer = do 
    db <- atomically $  newTVar M.empty
    mailbox  <- MB.newMailboxIO
    return UDPServer {db = db, mailbox = mailbox}

asyncQuery :: DNS.DNSMessage -> SockAddr -> JQ.JobQueue -> UDPServer -> IO()
asyncQuery message sa jobs server = do
    infoM name $ "asyncQuery: " ++ (show qname)
    let database  =  db server 
        key = (qname,qtype) :: DNSKey
    atomically $ do
        queries  <- readTVar database
        case M.lookup key queries of
            Nothing -> modifyTVar database $ M.insert key [((DH.identifier message),sa)]
            Just l -> modifyTVar database $ M.insert key (((DH.identifier message),sa):l)
    JQ.addJob key (mailbox server) jobs
    where 
        qname  = fromJust $ DH.qname message 
        qtype = fromJust $ DH.qtypeInt message 

loopServe :: Socket -> CM.DNSCache -> JQ.JobQueue -> UDPServer -> IO ()
loopServe sock cache jobs server = do
    -- 512B is max length of UDP message
    -- due ot rfc1035
    race_  sender receiver
    where
        sendAnswer message sa = void $ forkIO $ do
          void $ sendTo sock (BSL.toStrict $ DNS.encode message) sa
        loopResponse message (reqID,sa) = 
            let 
                hd = DNS.header message
                nhd = hd {DNS.identifier = reqID}
                m2 = message {DNS.header = nhd}
            in
              sendAnswer m2 sa
        receiver = do
            -- infoM (name ++ ".receiver") $ "receiver running ..."
            let maxLength = 512
            (a,sa) <- recvFrom sock maxLength
            case DNS.decode (BSL.fromStrict a) of
                Right m -> do
                    r <- hint m cache
                    case r of
                        Nothing -> asyncQuery m sa jobs server
                        Just h -> sendAnswer h sa
                Left e -> return ()
            receiver
        sender = do
            let database = db server
            let mb = mailbox server
            -- infoM (name ++ ".sender") $ "sender running ..."
            key <- atomically $ MB.readMailbox mb
            maybeQueries <-  atomically $ do
                queries  <- readTVar database
                let qs = M.lookup key queries 
                modifyTVar database $ M.delete key
                return qs

            case maybeQueries of
                Nothing -> return ()
                Just l -> do
                    val <- CM.lookupDNS key cache 
                    case val of
                        Nothing -> return ()
                        Just r -> do
                            let l2 = L.map (loopResponse r) l
                            return ()
            sender

serve :: ServerConfig -> CM.DNSCache -> JQ.JobQueue -> IO ()
serve conf cache jobs = do
    server <- createUDPServer
    infoM name $ "starting UDP server"
    let hints = defaultHints { addrSocketType = Datagram, addrFlags = [AI_ADDRCONFIG, AI_PASSIVE]}
    addr:_ <- getAddrInfo (Just hints) (server_host conf) (server_port conf)
    bracket 
        (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
        close
        (\sock -> do
            bind sock (addrAddress addr)
            infoM name $ "bound to " ++ (show $ addrAddress addr)
            forever $ loopServe sock cache jobs server
        )
