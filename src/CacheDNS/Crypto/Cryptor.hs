{-# LANGUAGE OverloadedStrings #-}
module  CacheDNS.Crypto.Cryptor
    (getCryptor
    )
where

import qualified Data.ByteString.Char8 as C
import Data.ByteString (ByteString)
import CacheDNS.Crypto.Common as Common

getCryptor :: String -> String
          -> IO (ByteString -> IO ByteString, ByteString -> IO ByteString)
getCryptor method key  = Common.getSSLCryptor method $ C.pack key