{-# LANGUAGE OverloadedStrings #-}
module CacheDNS.APP.JobQueue
    ( JobQueue
    , newJobQueue
    , addJob
    , fetchJob
    ) 
where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM

import qualified Data.ByteString as BS

import qualified CacheDNS.DNS as DNS 
import qualified CacheDNS.IPC.Mailbox as MB

data JobQueue = JobQueue { queue :: MB.Mailbox (DNS.DNSMessage, (MB.Mailbox DNS.Domain))} 

newJobQueue :: IO JobQueue 
newJobQueue = do 
    queue <- MB.newMailboxIO
    return JobQueue{ queue = queue }

addJob :: DNS.DNSMessage -> (MB.Mailbox DNS.Domain) -> JobQueue -> IO ()
addJob message mb jobs = do 
    let mailbox = queue jobs
    atomically $ MB.writeMailbox mailbox (message,mb)
    

fetchJob :: JobQueue -> IO (DNS.DNSMessage, (MB.Mailbox DNS.Domain))
fetchJob jobs = do
    let mailbox = queue jobs
    mail <- atomically $ MB.readMailbox mailbox
    return mail