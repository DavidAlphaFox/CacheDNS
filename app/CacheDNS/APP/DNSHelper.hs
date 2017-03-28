{-# LANGUAGE OverloadedStrings #-}
module CacheDNS.APP.DNSHelper
    ( identifier
    , qname
    , qtype
    , qtypeInt
    , ttl
    )
where
import qualified Data.List as L
import qualified CacheDNS.DNS as DNS 


question :: DNS.DNSMessage  -> Maybe DNS.Question
question message = 
    if (L.length q) > 0 then Just $ head q
    else Nothing
    where 
        q = DNS.question message

answer :: DNS.DNSMessage -> Maybe DNS.ResourceRecord
answer message =
    if (L.length ans) > 0 then L.find rr ans
    else Nothing
    where 
        ans = DNS.answer message
        rr DNS.ResourceRecord { } = True
        rr DNS.OptRecord {} = False
      
identifier :: DNS.DNSMessage -> Int
identifier message = DNS.identifier $ DNS.header message

qname :: DNS.DNSMessage -> Maybe DNS.Domain
qname message =
    case question message of
        Just q -> Just $ DNS.qname q
        Nothing -> Nothing

qtype :: DNS.DNSMessage -> Maybe DNS.TYPE
qtype message =
    case question message of
        Just q -> Just $ DNS.qtype q
        Nothing -> Nothing

qtypeInt :: DNS.DNSMessage -> Maybe Int
qtypeInt message =
    case qtype message of
        Just t -> Just $ DNS.typeToInt t
        Nothing -> Nothing
        
ttl :: DNS.DNSMessage -> Maybe Int 
ttl message = 
    case answer message of
        Just ans -> Just $ DNS.rrttl ans
        Nothing -> Nothing