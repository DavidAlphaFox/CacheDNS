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

import CacheDNS.APP.Types
import qualified CacheDNS.APP.CacheManager as CM
import qualified CacheDNS.DNS as DNS 
import qualified CacheDNS.IPC.Mailbox as MB
import qualified CacheDNS.APP.JobQueue as JQ

data UDPServer = UDPServer { db :: TVar (M.Map (DNS.Domain,Int) [(DNS.DNSMessage,SockAddr)])
                            ,mailbox :: MB.Mailbox (DNS.Domain,DNS.TYPE)
                            }
name :: String
name = "CacheDNS.udp"

hint :: DNS.DNSMessage -> CM.DNSCache -> IO (Maybe DNS.DNSMessage)
hint message cache = do
    val <- CM.lookupDNS (domain message) (rtype message) cache 
    case val of
        Nothing -> return Nothing
        Just r -> do
            infoM  name $ "Hinted: " ++ (show (domain message))
            let rid = identifier message
                hd = DNS.header r 
                newHeader = hd { DNS.identifier = rid}
            return $ Just r{DNS.header = newHeader}
    where 
        question m = head $ DNS.question message
        domain m = DNS.qname $ question m
        rtype m =  DNS.qtype $ question m
        identifier m = DNS.identifier $ DNS.header m

createUDPServer :: IO UDPServer 
createUDPServer = do 
    db <- atomically $  newTVar M.empty
    mailbox  <- MB.newMailboxIO
    return UDPServer {db = db, mailbox = mailbox}

asyncQuery :: DNS.DNSMessage -> SockAddr -> JQ.JobQueue -> UDPServer -> IO()
asyncQuery message sa jobs server = do
    infoM name $ "asyncQuery: " ++ (show (domain message))
    let database  =  db server 
    atomically $ do
        queries  <- readTVar database
        let key = ((domain message),(rtype message))
        case M.lookup key queries of
            Nothing -> modifyTVar database $ M.insert key [(message,sa)]
            Just l -> modifyTVar database $ M.insert key ((message,sa):l)
    JQ.addJob message (mailbox server) jobs
    where 
        question m = head $ DNS.question m
        domain m = DNS.qname $ question m
        rtype m = DNS.typeToInt $ DNS.qtype $ question m

loopServe :: Socket -> CM.DNSCache -> JQ.JobQueue -> UDPServer -> IO ()
loopServe sock cache jobs server = do
    -- 512B is max length of UDP message
    -- due ot rfc1035
    race_  sender receiver
    where 
        loopResponse message (m,sa) = 
            let reqID = DNS.identifier $ DNS.header m 
                hd = DNS.header message
                nhd = hd {DNS.identifier = reqID}
                m2 = message {DNS.header = nhd}
            in
            void $ sendTo sock (BSL.toStrict $ DNS.encode m2) sa
        requestKey (qn,qt) = (qn,DNS.typeToInt qt)
        receiver = do
            -- infoM (name ++ ".receiver") $ "receiver running ..."
            let maxLength = 512
            (a,sa) <- recvFrom sock maxLength
            case DNS.decode (BSL.fromStrict a) of
                Right m -> do 
                    r <- hint m cache
                    case r of
                        Nothing -> asyncQuery m sa jobs server
                        Just h -> void $ sendTo sock (BSL.toStrict $ DNS.encode h) sa
                Left e -> return ()
            receiver
        sender = do
            let database = db server
            let mb = mailbox server
            -- infoM (name ++ ".sender") $ "sender running ..."
            mail <- atomically $ MB.readMailbox mb
            maybeQueries <- atomically $ do
                queries  <- readTVar database
                let qs = M.lookup (requestKey mail) queries 
                modifyTVar database $ M.delete (requestKey mail)
                return qs
            case maybeQueries of
                Nothing -> return ()
                Just l -> do
                    let (qn,qt) = mail 
                    val <- CM.lookupDNS qn qt cache 
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
