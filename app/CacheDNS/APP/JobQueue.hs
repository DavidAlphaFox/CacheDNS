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
import CacheDNS.APP.Types

data JobQueue = JobQueue { mailbox :: MB.Mailbox (DNSKey, MB.Mailbox DNSKey) } 

newJobQueue :: IO JobQueue 
newJobQueue = do 
    mailbox <- MB.newMailboxIO
    return JobQueue{ mailbox = mailbox }

addJob :: DNSKey -> MB.Mailbox DNSKey -> JobQueue -> IO ()
addJob key mb jobs = do 
    atomically $ MB.writeMailbox (mailbox jobs) (key,mb)
    

fetchJob :: JobQueue -> IO (DNSKey, MB.Mailbox DNSKey)
fetchJob jobs = do
    mail <- atomically $ MB.readMailbox (mailbox jobs)
    return mail