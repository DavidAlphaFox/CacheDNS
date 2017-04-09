{-# LANGUAGE OverloadedStrings #-}
module  CacheDNS.Crypto.Common
    ( getSSLCryptor
    ) 
where

import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C

import Data.IntMap.Strict (fromList, (!))
import Data.List (sortBy)
import Data.Maybe (fromJust)
import Data.Binary.Get (runGet, getWord64le)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))

import Control.Concurrent.MVar ( newEmptyMVar, isEmptyMVar
                                         , putMVar, readMVar)

import Crypto.Hash.MD5 (hash)
import OpenSSL (withOpenSSL)
import OpenSSL.EVP.Cipher (getCipherByName, CryptoMode(..))
import OpenSSL.EVP.Internal (cipherInitBS, cipherUpdateBS)
import OpenSSL.Random (randBytes)

methodSupported :: HM.HashMap String (Int, Int)
methodSupported = HM.fromList
    [ ("aes-128-cfb", (16, 16))
    , ("aes-192-cfb", (24, 16))
    , ("aes-256-cfb", (32, 16))
    , ("bf-cfb", (16, 8))
    , ("camellia-128-cfb", (16, 16))
    , ("camellia-192-cfb", (24, 16))
    , ("camellia-256-cfb", (32, 16))
    , ("cast5-cfb", (16, 8))
    , ("des-cfb", (8, 8))
    , ("idea-cfb", (16, 8))
    , ("rc2-cfb", (16, 8))
    , ("rc4", (16, 0))
    , ("seed-cfb", (16, 16))
    ]

evpBytesToKey :: ByteString -> Int -> Int -> (ByteString, ByteString)
evpBytesToKey password keyLen ivLen =
    let ms' = S.concat $ ms False []
        key = S.take keyLen ms'
        iv  = S.take ivLen $ S.drop keyLen ms'
     in (key, iv)
  where
    ms :: Bool -> [ByteString] -> [ByteString]
    ms False _ = ms True [hash password] -- 计算出password的md5的Hash
    ms True m
        | S.length (S.concat m) < keyLen + ivLen =
            -- 如果长度小于 keyLen和ivLen的和
            -- 用m的最后一位和password合并后再次计算md5值
            ms True (m ++ [hash (last m <> password)])
        | otherwise = m

getSSLCryptor :: String -> ByteString
             -> IO (ByteString -> IO ByteString, ByteString -> IO ByteString)
getSSLCryptor method password = do
    let (m0, m1) = fromJust $ HM.lookup method methodSupported
    random_iv <- withOpenSSL $ randBytes 32
    -- 得到iv
    let cipher_iv = S.take m1 random_iv
    -- 从现有的password生成Key
    let (key, _) = evpBytesToKey password m0 m1
    -- 生成加密和解密的上下文
    cipherCtx <- newEmptyMVar
    decipherCtx <- newEmptyMVar
    -- 得到加密算法
    cipherMethod <- fmap fromJust $ withOpenSSL $ getCipherByName method
    -- 生成上下问
    ctx <- cipherInitBS cipherMethod key cipher_iv Encrypt
    let
        encrypt "" = return ""
        encrypt buf = do
            empty <- isEmptyMVar cipherCtx
            if empty
                then do
                    putMVar cipherCtx ()
                    ciphered <- withOpenSSL $ cipherUpdateBS ctx buf
                    return $ cipher_iv <> ciphered
                else withOpenSSL $ cipherUpdateBS ctx buf
        decrypt "" = return ""
        decrypt buf = do
            empty <- isEmptyMVar decipherCtx
            if empty
                then do
                    -- 需要在buf中先拿到IV才能初始化解谜的上下文
                    let decipher_iv = S.take m1 buf
                    dctx <- cipherInitBS cipherMethod key decipher_iv Decrypt
                    putMVar decipherCtx dctx
                    if S.null (S.drop m1 buf)
                        then return ""
                        else withOpenSSL $ cipherUpdateBS dctx (S.drop m1 buf)
                else do
                    dctx <- readMVar decipherCtx
                    withOpenSSL $ cipherUpdateBS dctx buf

    return (encrypt, decrypt)
