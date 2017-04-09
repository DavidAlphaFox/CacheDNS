module CacheDNS.Concurrent.CondVar 
    ( CondVar
    , newCondVar
    , wakup
    , wait
    , reset
    ) 
where

import Control.Applicative ((<$>))
import Control.Concurrent.STM
import Data.IORef (newIORef, readIORef, atomicModifyIORef', IORef)

newtype CondVar = CondVar (TMVar Int)

newCondVar :: IO CondVar
newCondVar = CondVar <$> newEmptyTMVarIO

wakup :: CondVar -> IO ()
wakup (CondVar var) = atomically $ putTMVar var 0

wait :: CondVar -> IO Int
wait (CondVar var) = atomically $ readTMVar var

reset :: CondVar -> IO Int
reset (CondVar var) = atomically $ do 
    empty <- isEmptyTMVar var
    if empty then return 0
    else takeTMVar var