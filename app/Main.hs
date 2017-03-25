module Main where
-- from Haskell or other Library
import Control.Monad
import Control.Monad.STM
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar

import Control.Exception

import System.IO
import System.Log.Logger

import Network.Socket hiding (recv, recvFrom, send, sendTo)
import Network.Socket.ByteString 

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS

-- from My Library
import Network.DNS
import Network.DNS.Decode as Decode

-- from Main APP
import qualified Log


data ServerConfig = ServerConfig { server_host :: Maybe String
                                 , server_port :: String
                                 }
nameM = "CacheDNS"                                

main :: IO ()
main = do
    Log.setup [("", INFO)] 
    let serverConfig = ServerConfig { server_host = (Just "0.0.0.0")
                                    , server_port = "5354"
                                    }
    udp serverConfig


udp :: ServerConfig  -> IO ()
udp c = do
    let nameF = nameM ++ ".udp"
    let maxLength = 512 -- 512B is max length of UDP message
                      -- due ot rfc1035
  
    infoM nameF $ "starting UDP server"
    let hints = defaultHints { addrSocketType = Datagram, addrFlags = [AI_ADDRCONFIG, AI_PASSIVE]}
    addr:_ <- getAddrInfo (Just hints) (server_host c) (Just $ server_port c)
    bracket 
        (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
        close
        (\sock -> do
            bind sock (addrAddress addr)
            infoM nameF $ "bound to " ++ (show $ addrAddress addr)
            forever $ do
                (a, sa) <- recvFrom sock maxLength
                let d = Decode.decode (BSL.fromStrict a)
                infoM nameF $ (show d)
                return ()
        )