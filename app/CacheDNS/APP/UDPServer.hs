{-# LANGUAGE OverloadedStrings #-}
module CacheDNS.APP.UDPServer
    ( serve
    )
where

import Network.Socket hiding (recv, recvFrom, send, sendTo)
import Network.Socket.ByteString 

import System.IO
import System.Log.Logger

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM

import Control.Exception

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Map as M

import CacheDNS.APP.Types
import qualified CacheDNS.APP.CacheManager as CM
import qualified CacheDNS.DNS as DNS 
import qualified CacheDNS.IPC.Mailbox as MB

-- data DNSQueries = DNSQueries { db :: TVar (M.Map DNS.Domain [(DNS.DNSMessage,SockAddr)])}
data UDPServer = UDPServer { db :: TVar (M.Map DNS.Domain [(DNS.DNSMessage,SockAddr)])
                            ,mailbox :: MB.Mailbox DNS.Domain 
                            }
name :: String
name = "CacheDNS.udp"

hint :: DNS.DNSMessage -> CM.DNSCache -> IO (Maybe DNS.DNSMessage)
hint message cache = do
    let key = ((domain message),(rtype message)
    CM.lookupDNS key cache
    where 
        question m = head $ DNS.question message
        domain m = DNS.qname $ question m
        rtype m = DNS.typeToInt $ DNS.qype $ question m

create :: IO UDPServer 
create = do 
    db <- atomically $  newTVar M.empty
    mailbox  <- Mailbox.newMailboxIO
    return UDPServer {db = db, mailbox = mailbox}

serve :: ServerConfig -> CM.DNSCache -> JobQueue -> IO ()
serve conf cache jobs = do
    let maxLength = 512 -- 512B is max length of UDP message
                      -- due ot rfc1035
    
    infoM nameF $ "starting UDP server"
    let hints = defaultHints { addrSocketType = Datagram, addrFlags = [AI_ADDRCONFIG, AI_PASSIVE]}
    addr:_ <- getAddrInfo (Just hints) (server_host conf) (server_port conf)
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