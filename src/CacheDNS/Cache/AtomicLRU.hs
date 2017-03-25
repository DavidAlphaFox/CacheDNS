module CacheDNS.Cache.AtomicLRU
    ( AtomicLRU
    , newAtomicLRU
    , fromList
    , toList
    , maxSize
    , insert
    , lookup
    , delete
    , pop
    , size
    , modifyAtomicLRU
    , modifyAtomicLRU'
    )
where

import Prelude hiding ( lookup )

import CacheDNS.Cache.LRU.Atomic
