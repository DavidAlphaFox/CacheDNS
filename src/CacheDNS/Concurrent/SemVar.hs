{-# LANGUAGE BangPatterns #-}
module CacheDNS.Concurrent.SemVar
    ( SemVar
    , newSemVar
    , wait
    , release
    )
where

import Control.Applicative ((<$>))
import Control.Concurrent.STM

data Semaphore = Semaphore { value ::  (TVar Int)
                           , limit :: Int
                           }
type SemVar = Semaphore

newSemVar :: Int -> IO SemVar
newSemVar l = do
    v <-  newTVarIO 0
    return $ Semaphore { value = v, limit = l}

wait :: SemVar -> IO ()
wait var = atomically $ do
    let v  = value var
        lim =  limit var
    x <- readTVar v
    -- 如果x是小于限制lim的时候
    check ( x < lim)
    -- 使用Bang patterns 
    -- 立刻进行计算
    let !x' = x + 1
    -- 写入Tvar
    writeTVar v x'

release :: SemVar -> IO ()
release var = atomically $ do 
    let v  = value var
        lim =  limit var
    
    x <- readTVar v
    -- 可能会死锁
    check (x > 0)
    
    let !x' = x - 1
    writeTVar v x'
