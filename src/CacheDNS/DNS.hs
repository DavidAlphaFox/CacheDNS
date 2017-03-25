-- | A thread-safe DNS library for both clients and servers written
--   in pure Haskell.
--   The Network.DNS module re-exports all other exposed modules for
--   convenience.
--   Applications will most likely use the high-level interface, while
--   library/daemon authors may need to use the lower-level one.
--   EDNS0 and TCP fallback are not supported yet.
--
module CacheDNS.DNS (
  -- * High level
    module CacheDNS.DNS.Lookup
  -- | The "Network.DNS.Lookup" module contains simple functions to
  --   perform various DNS lookups. If you simply want to resolve a
  --   hostname ('lookupA'), or find a domain's MX record
  --   ('lookupMX'), this is the easiest way to do it.

  , module CacheDNS.DNS.Resolver
  -- | The "Network.DNS.Resolver" module is slightly more low-level
  --   than "Network.DNS.Lookup". If you need to do something unusual,
  --   you may need to use the 'lookup', 'lookupAuth', or 'lookupRaw'
  --   functions.

  , module CacheDNS.DNS.Utils
  -- | The "Network.DNS.Utils" module contains utility functions used
  --   for processing DNS data.

  , module CacheDNS.DNS.Types
  -- | All of the types that the other modules use.

  -- * Low level
  , module CacheDNS.DNS.Decode
  -- | Decoding a response.

  , module CacheDNS.DNS.Encode
  -- | Encoding a query.

  ) where

import CacheDNS.DNS.Lookup
import CacheDNS.DNS.Resolver
import CacheDNS.DNS.Utils
import CacheDNS.DNS.Types
import CacheDNS.DNS.Decode
import CacheDNS.DNS.Encode
