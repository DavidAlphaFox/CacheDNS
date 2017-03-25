module Transport.IPC.Mailbox (
    Mailbox,
    newMailbox,
    newMailboxIO,
    writeMailbox,
    readMailbox,
    tryReadMailbox,
    peekMailbox,
    tryPeekMailbox,
    selectMailbox,
    trySelectMailbox,
    handleMailbox,
    findMailbox,
    tryFindMailbox,
    unGetMailbox,
    isEmptyMailbox,
) where

-- local imports

-- external imports

import Control.Concurrent.STM
{-
  对Mailbox来说
  永远都是写入Mailbox的_write
  当读取的时候，优先读取Mailbox的_read
  如果_read的数据是空的，那么将_write读取出来，并进行翻转放入_read中
  这样会减少读取和写入的冲突
-}
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

{-|
A 'Mailbox' is a variation of a 'Control.Concurrent.TQueue', with the added feature of selective message retrieval.
-}
data Mailbox m = Mailbox
    {-# UNPACK #-} !(TVar [m])
    {-# UNPACK #-} !(TVar [m])

instance Eq (Mailbox m) where
  Mailbox a _ == Mailbox b _ = a == b

{-|
Build and returns a new instance of 'Mailbox'
-}
-- mailbox 包含两个TVar
-- 一个用于读，一个用于写
newMailbox :: STM (Mailbox m)
newMailbox = do

  _read  <- newTVar []
  _write <- newTVar []
  return (Mailbox _read _write)

{-|
@IO@ version of 'newMailbox'.  This is useful for creating top-level
'Mailbox's using 'System.IO.Unsafe.unsafePerformIO', because using
'atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
possible.
-}
newMailboxIO :: IO (Mailbox m)
newMailboxIO = do
  _read  <- newTVarIO []
  _write <- newTVarIO []
  return (Mailbox _read _write)

{-|
Write a value to a 'Mailbox'.
-}
writeMailbox :: Mailbox m -> m -> STM ()
writeMailbox (Mailbox _read _write) msg = do
    -- 读出已经有的消息了
    listend <- readTVar _write
    -- 新的消息放入其中
    -- 放在已经有的消息的前面
    writeTVar _write (msg:listend)

{-|
Read the next value from the 'Mailbox'.
-}
--  从相应的Mailbox中获得相应的数据
readMailbox :: Mailbox m -> STM m
readMailbox (Mailbox _read _write) = do
  -- 从_read中的TVar中读取出相应的数据
  xs <- readTVar _read
  case xs of
    -- 有可读取的数据的时候，先处理读取
    (x:xs') -> do writeTVar _read xs'
                  return x
    -- 从write中去读取              
    [] -> do ys <- readTVar _write
             case ys of
               [] -> retry
              -- write有数据
              -- 将write进行翻转
              -- 将翻转的结果写入read中
               _  -> case reverse ys of
                       [] -> error "readMailbox"
                       (z:zs) -> do writeTVar _write []
                                    writeTVar _read zs
                                    return z

{-|
A version of 'readMailbox' which does not retry. Instead it
returns @Nothing@ if no value is available.
-}
tryReadMailbox :: Mailbox m -> STM (Maybe m)
tryReadMailbox mailbox = fmap Just (readMailbox mailbox) `orElse` return Nothing

{-|
Get the next value from the @Mailbox@ without removing it,
retrying if the channel is empty.
-}
peekMailbox :: Mailbox m -> STM m
peekMailbox mailbox = do
  msg <- readMailbox mailbox
  unGetMailbox mailbox msg
  return msg

{-|
A version of 'peekMailbox' which does not retry. Instead it
returns @Nothing@ if no value is available.
-}
tryPeekMailbox :: Mailbox m -> STM (Maybe m)
tryPeekMailbox mailbox = do
  maybeMsg <- tryReadMailbox mailbox
  case maybeMsg of
    Nothing -> return Nothing
    Just msg -> do
      unGetMailbox mailbox msg
      return $ Just msg

{-|
Find the next message in the mailbox that matches the supplied test
function or block until there is a message that does. When a message
matches (e.g., test functions returns @Just v@), return it.
-}
selectMailbox :: Mailbox m -> (m -> Maybe v) -> STM v
selectMailbox (Mailbox _read _write) testFn = do
    -- 从_read中读取出message
    readMessages <- readTVar _read
    -- 得到一个全新的消息
    -- 之后返回剩余的消息
    let (maybeReadMsg,newRead) = extract testFn readMessages
    case maybeReadMsg of
        Just msg -> do
            writeTVar _read newRead
            return msg
        Nothing -> do
            writeMessages <- readTVar _write
            let (maybeWriteMsg,newWrite) = extract testFn writeMessages
            case maybeWriteMsg of
                Just msg -> do
                    writeTVar _write newWrite
                    return msg
                -- 重试内存事务   
                Nothing -> retry

{-|
A version of 'selectMailbox' which does not retry. Instead it
returns @Nothing@ if no value is available.
-}
trySelectMailbox :: Mailbox m -> (m -> Maybe v)-> STM (Maybe v)
trySelectMailbox mailbox testFn = fmap Just (selectMailbox mailbox testFn) `orElse` return Nothing

{-|
Wait until there is a message in the mailbox matching the supplied test
function (using `selectMailbox`), then when a message is found, handle
it in the `IO` monad with the supplied function.
-}
handleMailbox :: Mailbox m -> (m -> Maybe v) -> (v -> IO r) -> IO r
handleMailbox mailbox testFn handleFn = do
    selectedMsg <- atomically $ selectMailbox mailbox testFn
    handleFn selectedMsg

{-|
Find the next value from the @Mailbox@ matching @testFn@ without removing it,
retrying if the channel is empty.
-}
findMailbox :: Mailbox m -> (m -> Maybe v) -> STM v
findMailbox (Mailbox _read write) testFn = do
    readMessages <- readTVar _read
    case find testFn readMessages of
        Just msg -> do
            return msg
        Nothing -> do
            writeMessages <- readTVar write
            case find testFn writeMessages of
                Just msg -> do
                    return msg
                Nothing -> retry

{-|
A version of 'findMailbox' which does not retry. Instead it
returns @Nothing@ if no value is available.
-}
tryFindMailbox :: Mailbox m -> (m -> Maybe v) -> STM (Maybe v)
tryFindMailbox (Mailbox _read write) testFn = do
    readMessages <- readTVar _read
    case find testFn readMessages of
        Just msg -> do
            return $ Just msg
        Nothing -> do
            writeMessages <- readTVar write
            case find testFn writeMessages of
                Just msg -> do
                    return $ Just msg
                Nothing -> return Nothing

{-|
Put a data item back onto a mailbox, where it will be the next item read.
-}
unGetMailbox :: Mailbox m -> m -> STM ()
unGetMailbox (Mailbox _read _write) msg = do
-- 将消息拿出后，再次放回去
  xs <- readTVar _read
  writeTVar _read (msg:xs)

{-|
Returns 'True' if the supplied 'Mailbox' is empty.
-}
isEmptyMailbox :: Mailbox m -> STM Bool
isEmptyMailbox (Mailbox _read write) = do
  xs <- readTVar _read
  case xs of
    (_:_) -> return False
    [] -> do ys <- readTVar write
             case ys of
               [] -> return True
               _  -> return False

--
-- Internal helpers
--

{-|
Extract the first element from a list matching the
provided test and return a new list without the matching
element.
-}
-- 接受一个函数和一个数组
-- 循环的检测匹配的消息
extract :: (m -> Maybe v) -> [m] -> (Maybe v,[m])
extract _ [] = (Nothing,[])
extract test (x:xs) =
    case test x of
        Nothing -> let (result,rest) = extract test xs
                 in (result,x:rest)
        Just v -> (Just v,xs)

{-|
Find the first element from a list matching the
provided test, or Nothing if there is no match.
-}
find :: (m -> Maybe v) -> [m] -> Maybe v
find _ [] = Nothing
find testFn (x:xs) =
    case testFn x of
        Nothing -> find testFn xs
        Just v -> Just v
