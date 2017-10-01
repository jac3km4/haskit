{-# LANGUAGE LambdaCase #-}
module LuaJIT.ByteCode
    ( ByteCode(..)
    , Constant(..)
    , KNum(..)
    , Frame(..)
    , Chunk(..)
    , Opcode
    , compile
    , loadByteCode
    , putByteCode
    , islt
    , isge
    , isle
    , isgt
    , iseqv
    , isnev
    , iseqs
    , isnes
    , iseqn
    , isnen
    , iseqp
    , isnep
    , istc
    , isfc
    , ist
    , isf
    , mov
    , bnot
    , unm
    , len
    , addvn
    , subvn
    , mulvn
    , divvn
    , modvn
    , addnv
    , subnv
    , mulnv
    , divnv
    , modnv
    , addvv
    , subvv
    , mulvv
    , divvv
    , modvv
    , pow
    , cat
    , kstr
    , kcdata
    , kshort
    , knum
    , kpri
    , knil
    , uget
    , usetv
    , usets
    , usetn
    , usetp
    , uclo
    , fnew
    , tnew
    , tdup
    , gget
    , gset
    , tgetv
    , tgets
    , tgetb
    , tsetv
    , tsets
    , tsetb
    , tsetm
    , callm
    , call
    , callmt
    , callt
    , iterc
    , itern
    , varg
    , isnext
    , retm
    , ret
    , ret0
    , ret1
    , fori
    , jfori
    , forl
    , iforl
    , jforl
    , iterl
    , iiterl
    , jiterl
    , loop
    , iloop
    , jloop
    , jmp
    , funcf
    , ifuncf
    , jfuncf
    , funcv
    , ifuncv
    , jfuncv
    , funcc
    , funccw) where
import           Control.Monad          (forM_, replicateM_)
import           Data.Binary            (Put)
import           Data.Binary.Put
import           Data.Bits
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BL
import           Data.ByteString.Unsafe (unsafeUseAsCString)
import           Data.Int               (Int64)
import           Data.Word
import           Foreign.C.Types
import qualified Foreign.Lua            as Lua
import           Foreign.Lua.Types      (liftLua)
import           Foreign.Ptr            (Ptr, nullPtr)

foreign import ccall "lauxlib.h luaL_loadbufferx"
    luaL_loadbufferx :: Lua.LuaState -> Ptr CChar -> CSize -> Ptr CChar -> Ptr CChar -> IO Lua.StatusCode

data ByteCode = ByteCode [Frame]
    deriving (Show)

data Constant
    = Child
    | Table
    | Str String
    deriving (Show)

data KNum
    = KI64 Int64
    | KU64 Word64
    deriving (Show)

data Frame =
    Frame { frameFlags :: Word8
          , paramCount :: Word8
          , frameSize  :: Word8
          , uvCount    :: Word8
          , knums      :: [KNum]
          , kgcs       :: [Constant]
          , bytecode   :: [Opcode]
          } deriving (Show)

data Chunk = Chunk { chunkName :: String, chunkFrame :: Frame }

data Args = ARGS_AD Word8 Word16 | ARGS_ABC Word8 Word8 Word8
    deriving (Show)

data Opcode = Opcode Word8 Args
    deriving (Show)

-- creates a global scope with references to all the chunks
compile :: [Chunk] -> ByteCode
compile chunks =
    ByteCode $ (fmap chunkFrame chunks) ++ [global $ zipWith child [0..] chunks]
    where
      global children =
        Frame { frameFlags = 3
              , paramCount = 0
              , frameSize = 1
              , uvCount = 0
              , knums = []
              , kgcs = foldMap fst children
              , bytecode = foldMap snd children ++ [ret0 0 1]
              }
      child i chunk =
        ([Str (chunkName chunk), Child], [fnew 0 (i * 2), gset 0 (i * 2 + 1)])

loadByteCode :: ByteCode -> Lua.Lua Lua.StatusCode
loadByteCode bc =
    let strict = BL.toStrict . runPut $ putByteCode bc
    in liftLua $ \l ->
        unsafeUseAsCString strict $ \ptr ->
            luaL_loadbufferx l ptr (fromIntegral $ BS.length strict) nullPtr nullPtr

version :: Word8
version = 1

putByteCode :: ByteCode -> Put
putByteCode (ByteCode frames) = do
    putWord8 0x1b
    putWord8 0x4c
    putWord8 0x4a
    putWord8 version
    putULEB128 2 -- BigEndian, Strip Debug, FFI
    forM_ frames $ \frame -> do
        let frameBs = runPut $ putFrame frame
        putULEB128 . fromIntegral $ BL.length frameBs
        putLazyByteString frameBs
    putULEB128 0

putFrame :: Frame -> Put
putFrame frame = do
    let numKgc = length $ kgcs frame
    let numKn = length $ knums frame
    let numBc = length $ bytecode frame
    putWord8 $ frameFlags frame -- Child, Vararg, FFI (Uses BC_KCDATA)
    putWord8 $ paramCount frame
    putWord8 $ frameSize frame
    putWord8 $ uvCount frame
    putULEB128 $ fromIntegral numKgc
    putULEB128 $ fromIntegral numKn
    putULEB128 $ fromIntegral numBc
    forM_ (bytecode frame) $ putWord32host . opcode
    replicateM_ (fromIntegral $ uvCount frame) $ do
        putWord16host 0
    forM_ (kgcs frame) $ \case
        Child ->
            putULEB128 0
        Table ->
            putULEB128 1
        Str v -> do
            putULEB128 . fromIntegral $ 5 + length v
            putStringUtf8 v
    forM_ (knums frame) putKnum

putKnum :: KNum -> Put
putKnum (KI64 n) =
    let num = (n `shiftL` 1) .|. 0
    in putULEB128 $ fromIntegral num

putULEB128 :: Integer -> Put
putULEB128 n
    | n' == 0 = putWord8 byte
    | otherwise = putWord8 (byte .|. 0x80) *> putULEB128 n'
    where
      byte = fromIntegral (n .&. 0x7F)
      n' = n `shiftR` 7

opcode :: Opcode -> Word32
opcode (Opcode op (ARGS_AD a d)) =
    fromIntegral (d .&. 0xFF00) `shiftL` 24 .|.
    fromIntegral (d .&. 0x00FF) `shiftL` 16 .|.
    fromIntegral a `shiftL` 8 .|.
    fromIntegral op
opcode (Opcode op (ARGS_ABC a b c)) =
    fromIntegral b `shiftL` 24 .|.
    fromIntegral c `shiftL` 16 .|.
    fromIntegral a `shiftL` 8 .|.
    fromIntegral op

islt a d = Opcode 0 $ ARGS_AD a d
isge a d = Opcode 1 $ ARGS_AD a d
isle a d = Opcode 2 $ ARGS_AD a d
isgt a d = Opcode 3 $ ARGS_AD a d
iseqv a d = Opcode 4 $ ARGS_AD a d
isnev a d = Opcode 5 $ ARGS_AD a d
iseqs a d = Opcode 6 $ ARGS_AD a d
isnes a d = Opcode 7 $ ARGS_AD a d
iseqn a d = Opcode 8 $ ARGS_AD a d
isnen a d = Opcode 9 $ ARGS_AD a d
iseqp a d = Opcode 10 $ ARGS_AD a d
isnep a d = Opcode 11 $ ARGS_AD a d
istc a d = Opcode 12 $ ARGS_AD a d
isfc a d = Opcode 13 $ ARGS_AD a d
ist a d = Opcode 14 $ ARGS_AD a d
isf a d = Opcode 15 $ ARGS_AD a d
mov a d = Opcode 16 $ ARGS_AD a d
bnot a d = Opcode 17 $ ARGS_AD a d
unm a d = Opcode 18 $ ARGS_AD a d
len a d = Opcode 19 $ ARGS_AD a d
addvn a b c = Opcode 20 $ ARGS_ABC a b c
subvn a b c = Opcode 21 $ ARGS_ABC a b c
mulvn a b c = Opcode 22 $ ARGS_ABC a b c
divvn a b c = Opcode 23 $ ARGS_ABC a b c
modvn a b c = Opcode 24 $ ARGS_ABC a b c
addnv a b c = Opcode 25 $ ARGS_ABC a b c
subnv a b c = Opcode 26 $ ARGS_ABC a b c
mulnv a b c = Opcode 27 $ ARGS_ABC a b c
divnv a b c = Opcode 28 $ ARGS_ABC a b c
modnv a b c = Opcode 29 $ ARGS_ABC a b c
addvv a b c = Opcode 30 $ ARGS_ABC a b c
subvv a b c = Opcode 31 $ ARGS_ABC a b c
mulvv a b c = Opcode 32 $ ARGS_ABC a b c
divvv a b c = Opcode 33 $ ARGS_ABC a b c
modvv a b c = Opcode 34 $ ARGS_ABC a b c
pow a b c = Opcode 35 $ ARGS_ABC a b c
cat a b c = Opcode 36 $ ARGS_ABC a b c
kstr a d = Opcode 37 $ ARGS_AD a d
kcdata a d = Opcode 38 $ ARGS_AD a d
kshort a d = Opcode 39 $ ARGS_AD a d
knum a d = Opcode 40 $ ARGS_AD a d
kpri a d = Opcode 41 $ ARGS_AD a d
knil a d = Opcode 42 $ ARGS_AD a d
uget a d = Opcode 43 $ ARGS_AD a d
usetv a d = Opcode 44 $ ARGS_AD a d
usets a d = Opcode 45 $ ARGS_AD a d
usetn a d = Opcode 46 $ ARGS_AD a d
usetp a d = Opcode 47 $ ARGS_AD a d
uclo a d = Opcode 48 $ ARGS_AD a d
fnew a d = Opcode 49 $ ARGS_AD a d
tnew a d = Opcode 50 $ ARGS_AD a d
tdup a d = Opcode 51 $ ARGS_AD a d
gget a d = Opcode 52 $ ARGS_AD a d
gset a d = Opcode 53 $ ARGS_AD a d
tgetv a b c = Opcode 54 $ ARGS_ABC a b c
tgets a b c = Opcode 55 $ ARGS_ABC a b c
tgetb a b c = Opcode 56 $ ARGS_ABC a b c
tsetv a b c = Opcode 57 $ ARGS_ABC a b c
tsets a b c = Opcode 58 $ ARGS_ABC a b c
tsetb a b c = Opcode 59 $ ARGS_ABC a b c
tsetm a d = Opcode 60 $ ARGS_AD a d
callm a b c = Opcode 61 $ ARGS_ABC a b c
call a b c = Opcode 62 $ ARGS_ABC a b c
callmt a d = Opcode 63 $ ARGS_AD a d
callt a d = Opcode 64 $ ARGS_AD a d
iterc a b c = Opcode 65 $ ARGS_ABC a b c
itern a b c = Opcode 66 $ ARGS_ABC a b c
varg a b c = Opcode 67 $ ARGS_ABC a b c
isnext a d = Opcode 68 $ ARGS_AD a d
retm a d = Opcode 69 $ ARGS_AD a d
ret a d = Opcode 70 $ ARGS_AD a d
ret0 a d = Opcode 71 $ ARGS_AD a d
ret1 a d = Opcode 72 $ ARGS_AD a d
fori a d = Opcode 73 $ ARGS_AD a d
jfori a d = Opcode 74 $ ARGS_AD a d
forl a d = Opcode 75 $ ARGS_AD a d
iforl a d = Opcode 76 $ ARGS_AD a d
jforl a d = Opcode 77 $ ARGS_AD a d
iterl a d = Opcode 78 $ ARGS_AD a d
iiterl a d = Opcode 79 $ ARGS_AD a d
jiterl a d = Opcode 80 $ ARGS_AD a d
loop a d = Opcode 81 $ ARGS_AD a d
iloop a d = Opcode 82 $ ARGS_AD a d
jloop a d = Opcode 83 $ ARGS_AD a d
jmp a d = Opcode 84 $ ARGS_AD a d
funcf a d = Opcode 85 $ ARGS_AD a d
ifuncf a d = Opcode 86 $ ARGS_AD a d
jfuncf a d = Opcode 87 $ ARGS_AD a d
funcv a d = Opcode 88 $ ARGS_AD a d
ifuncv a d = Opcode 89 $ ARGS_AD a d
jfuncv a d = Opcode 90 $ ARGS_AD a d
funcc a d = Opcode 91 $ ARGS_AD a d
funccw a d = Opcode 92 $ ARGS_AD a d
