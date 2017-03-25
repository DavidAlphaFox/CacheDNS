{-# LANGUAGE RecordWildCards, CPP #-}

module Network.DNS.Encode (
    encode
  , encodeVC
  , composeQuery
  , composeQueryAD
  ) where

import Control.Monad (when)
import Control.Monad.State (State, modify, execState, gets)
import Data.Binary (Word16)
import Data.Bits ((.|.), bit, shiftL, setBit)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.IP (IP(..),fromIPv4, fromIPv6b)
import Data.List (dropWhileEnd)
import Data.Monoid ((<>))
import Network.DNS.Internal
import Network.DNS.StateBinary

#if __GLASGOW_HASKELL__ < 709
import Data.Monoid (mconcat)
#endif

----------------------------------------------------------------

-- | Composing query. First argument is a number to identify response.

composeQuery :: Int -> [Question] -> ByteString
composeQuery idt qs = encode qry
  where
    hdr = header defaultQuery
    qry = defaultQuery {
        header = hdr {
           identifier = idt
         }
      , question = qs
      }

composeQueryAD :: Int -> [Question] -> ByteString
composeQueryAD idt qs = encode qry
  where
      hdr = header defaultQuery
      flg = flags hdr
      qry = defaultQuery {
          header = hdr {
              identifier = idt,
              flags = flg {
                  authenData = True
              }
           }
        , question = qs
        }

----------------------------------------------------------------

-- | Composing DNS data.
-- 将DNS消息编码成字节流
encode :: DNSMessage -> ByteString
encode msg = runSPut (encodeDNSMessage msg)

encodeVC :: ByteString -> ByteString
encodeVC query =
    let len = BB.toLazyByteString $ BB.int16BE $ fromIntegral $ BL.length query
    in len <> query

----------------------------------------------------------------

encodeDNSMessage :: DNSMessage -> SPut
encodeDNSMessage msg = encodeHeader hdr
                    <> encodeNums
                    <> mconcat (map encodeQuestion qs)
                    <> mconcat (map encodeRR an)
                    <> mconcat (map encodeRR au)
                    <> mconcat (map encodeRR ad)
  where
    -- 根据RFC 1035
    -- 头部后面应该跟随 有多少个question，多少个answer
    -- 多少个authority 和多少个additional
    -- 一共占4个16bit
    encodeNums = mconcat $ fmap putInt16 [length qs
                                         ,length an
                                         ,length au
                                         ,length ad
                                         ]
    hdr = header msg
    qs = question msg
    an = answer msg
    au = authority msg
    ad = additional msg

-- DNS的头部ID占16位，标志位占16位
-- RFC 1035
encodeHeader :: DNSHeader -> SPut
encodeHeader hdr = encodeIdentifier (identifier hdr)
                <> encodeFlags (flags hdr)
  where
    encodeIdentifier = putInt16

-- 将包头中的标志位打包
encodeFlags :: DNSFlags -> SPut
encodeFlags DNSFlags{..} = put16 word
  where
    -- Word16实现了Enum的toEnum
    word16 :: Enum a => a -> Word16
    word16 = toEnum . fromEnum
    -- 修改byte用bit或来进行操作
    -- modify :: MonadState s m => (s -> s) -> m ()
    set :: Word16 -> State Word16 ()
    set byte = modify (.|. byte)
    -- 从左向右逐个执行
    st :: State Word16 ()
    st = sequence_
              [ set (word16 rcode)
              , when authenData          $ set (bit 5)
              , when recAvailable        $ set (bit 7)
              , when recDesired          $ set (bit 8)
              , when trunCation          $ set (bit 9)
              , when authAnswer          $ set (bit 10)
              , set (word16 opcode `shiftL` 11)
              , when (qOrR==QR_Response) $ set (bit 15)
              ]
    -- execState 会执行sequence序列
    -- 0作为初始值
    word = execState st 0
-- 编码应答部分
encodeQuestion :: Question -> SPut
encodeQuestion Question{..} = encodeDomain qname
                           <> putInt16 (typeToInt qtype)
                           <> put16 1

putRData :: RData -> SPut
putRData rd = do
    addPositionW 2 -- "simulate" putInt16
    rDataBuilder <- encodeRDATA rd
    -- fixmed: SPut must hold length
    let rdataLength = fromIntegral . BL.length . BB.toLazyByteString $ rDataBuilder
    let rlenBuilder = BB.int16BE rdataLength
    return rlenBuilder <> return rDataBuilder

encodeRR :: ResourceRecord -> SPut
encodeRR ResourceRecord{..} = mconcat [ encodeDomain rrname
                                      , putInt16 (typeToInt rrtype)
                                      , put16 1
                                      , putInt32 rrttl
                                      , putRData rdata
                                      ]

encodeRR OptRecord{..} = mconcat [ encodeDomain BS.empty
                                 , putInt16 (typeToInt OPT)
                                 , putInt16 orudpsize
                                 , putInt32 $ if ordnssecok
                                              then setBit 0 15
                                              else 0
                                 , putRData rdata
                                 ]

encodeRDATA :: RData -> SPut
encodeRDATA rd = case rd of
    (RD_A ip)          -> mconcat $ map putInt8 (fromIPv4 ip)
    (RD_AAAA ip)       -> mconcat $ map putInt8 (fromIPv6b ip)
    (RD_NS dom)        -> encodeDomain dom
    (RD_CNAME dom)     -> encodeDomain dom
    (RD_DNAME dom)     -> encodeDomain dom
    (RD_PTR dom)       -> encodeDomain dom
    (RD_MX prf dom)    -> mconcat [putInt16 prf, encodeDomain dom]
    (RD_TXT txt)       -> putByteStringWithLength txt
    (RD_OTH bytes)     -> putByteString bytes
    (RD_OPT opts)      -> mconcat $ fmap encodeOData opts
    (RD_SOA d1 d2 serial refresh retry expire min') -> mconcat
        [ encodeDomain d1
        , encodeDomain d2
        , putInt32 serial
        , putInt32 refresh
        , putInt32 retry
        , putInt32 expire
        , putInt32 min'
        ]
    (RD_SRV prio weight port dom) -> mconcat
        [ putInt16 prio
        , putInt16 weight
        , putInt16 port
        , encodeDomain dom
        ]
    (RD_TLSA u s m d) -> mconcat
        [ put8 u
        , put8 s
        , put8 m
        , putByteString d
        ]

encodeOData :: OData -> SPut
encodeOData (OD_ClientSubnet srcNet scpNet ip) = let dropZeroes = dropWhileEnd (==0)
                                                     (fam,raw) = case ip of
                                                                    IPv4 ip4 -> (1,dropZeroes $ fromIPv4 ip4)
                                                                    IPv6 ip6 -> (2,dropZeroes $ fromIPv6b ip6)
                                                     dataLen = 2 + 2 + length raw
                                                 in mconcat [putInt16 (optTypeToInt ClientSubnet)
                                                            ,putInt16 dataLen
                                                            ,putInt16 fam
                                                            ,putInt8 srcNet
                                                            ,putInt8 scpNet
                                                            ,mconcat $ fmap putInt8 raw
                                                            ]
encodeOData (OD_Unknown code bs) = mconcat [putInt16 code
                                           ,putInt16 $ BS.length bs
                                           ,putByteString bs
                                           ]

-- In the case of the TXT record, we need to put the string length
putByteStringWithLength :: BS.ByteString -> SPut
putByteStringWithLength bs = putInt8 (fromIntegral $ BS.length bs) -- put the length of the given string
                          <> putByteString bs

----------------------------------------------------------------

rootDomain :: Domain
rootDomain = BS.pack "."
-- 编码Domain
-- RFC 1035种Domain的编码是不包含"."的
encodeDomain :: Domain -> SPut
encodeDomain dom
    --  如果dom是空的，或者是root那么直接放入0
    | (BS.null dom || dom == rootDomain) = put8 0
    | otherwise = do
        -- 得到所有未知
        mpos <- wsPop dom
        -- 得到当前的位置
        cur <- gets wsPosition
        case mpos of
            -- 如果有位置，直接使用DNS中的ptr指到相应位置上
            Just pos -> encodePointer pos
            -- 没找到，就在当前位置放置Domain
            Nothing  -> wsPush dom cur >>
                        mconcat [ encodePartialDomain hd
                                -- 这个地方是个递归调用
                                , encodeDomain tl
                                ]
  where
    -- 在“.”处断开，DNS查询中是不包含“.”的
    (hd, tl') = BS.break (=='.') dom
    -- 去掉"."
    tl = if BS.null tl' then tl' else BS.drop 1 tl'
-- ptr需要16进制高两位设置为11
-- pos是从整个包头的位置开始的偏移，不是从当前section的偏移
encodePointer :: Int -> SPut
encodePointer pos = putInt16 (pos .|. 0xc000)

encodePartialDomain :: Domain -> SPut
encodePartialDomain = putByteStringWithLength
