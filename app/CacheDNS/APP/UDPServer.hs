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

import CacheDNS.APP.Types
import qualified CacheDNS.APP.CacheManager as CM
import qualified CacheDNS.DNS as DNS 
import qualified CacheDNS.IPC.Mailbox as MB
import qualified CacheDNS.APP.JobQueue as JQ

-- data DNSQueries = DNSQueries { db :: TVar (M.Map DNS.Domain [(DNS.DNSMessage,SockAddr)])}
data UDPServer = UDPServer { db :: TVar (M.Map DNS.Domain [(DNS.DNSMessage,SockAddr)])
                            ,mailbox :: MB.Mailbox DNS.Domain 
                            }
name :: String
name = "CacheDNS.udp"

hint :: DNS.DNSMessage -> CM.DNSCache -> IO (Maybe DNS.DNSMessage)
hint message cache = do
    val <- CM.lookupDNS (domain message) (rtype message) cache 
    case val of
        Nothing -> return Nothing
        Just r -> do
            let rid = identifier message
                hd = DNS.header r 
                newHeader = hd { DNS.identifier = rid}
            return $ Just r{DNS.header = newHeader}
    where 
        question m = head $ DNS.question message
        domain m = DNS.qname $ question m
        rtype m =  DNS.qtype $ question m
        identifier m = DNS.identifier $ DNS.header m

create :: IO UDPServer 
create = do 
    db <- atomically $  newTVar M.empty
    mailbox  <- MB.newMailboxIO
    return UDPServer {db = db, mailbox = mailbox}

serve :: ServerConfig -> CM.DNSCache -> JQ.JobQueue -> IO ()
serve conf cache jobs = do
    let maxLength = 512 -- 512B is max length of UDP message
                      -- due ot rfc1035
    server <- create
    
    infoM name $ "starting UDP server"
    let hints = defaultHints { addrSocketType = Datagram, addrFlags = [AI_ADDRCONFIG, AI_PASSIVE]}
    addr:_ <- getAddrInfo (Just hints) (server_host conf) (server_port conf)
    bracket 
        (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
        close
        (\sock -> do
            bind sock (addrAddress addr)
            infoM name $ "bound to " ++ (show $ addrAddress addr)
            forever $ do
                (a, sa) <- recvFrom sock maxLength
                case DNS.decode (BSL.fromStrict a) of
                    Left e -> return ()
                    Right m -> do 
                        r <- hint m cache
                        infoM name $ (show r)
                        case r of
                            Nothing -> JQ.addJob m (mailbox server) jobs
                            Just h -> void $ sendTo sock (BSL.toStrict $ DNS.encode h) sa
        )