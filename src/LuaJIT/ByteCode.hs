{-# LANGUAGE LambdaCase #-}
module LuaJIT.ByteCode
  ( ByteCode(..)
  , KGC(..)
  , KNum(..)
  , Frame(..)
  , Instruction
  , loadByteCode
  , dumpByteCode
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
  , funccw
  ) where
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

data ByteCode =
  ByteCode [Frame]
  deriving (Show)

data KGC
  = Child
  | Table
  | Str String
  deriving (Show)

data KNum
  = KI64 Int64
  | KU64 Word64
  deriving (Show)

data Frame = Frame
  { frameFlags :: Word8
  , paramCount :: Word8
  , frameSize  :: Word8
  , uvCount    :: Word8
  , knums      :: [KNum]
  , kgcs       :: [KGC]
  , bytecode   :: [Instruction]
  } deriving (Show)

data Args
  = TwoArgs Word8 Word16
  | ThreeArgs Word8 Word8 Word8
  deriving (Show)

data Instruction =
  Instruction Word8 Args
  deriving (Show)

loadByteCode :: ByteCode -> Lua.Lua Lua.StatusCode
loadByteCode bc =
  let strict = BL.toStrict . runPut $ putByteCode bc
  in liftLua $ \l ->
       unsafeUseAsCString strict $ \ptr ->
         luaL_loadbufferx
           l
           ptr
           (fromIntegral $ BS.length strict)
           nullPtr
           nullPtr

dumpByteCode :: FilePath -> ByteCode -> IO ()
dumpByteCode path bc = BL.writeFile path . runPut $ putByteCode bc

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
  forM_ (bytecode frame) $ putWord32host . encode
  replicateM_ (fromIntegral $ uvCount frame) $ do putWord16host 0
  forM_ (kgcs frame) $ \case
    Child -> putULEB128 0
    Table -> putULEB128 1
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

encode :: Instruction -> Word32
encode (Instruction op (TwoArgs a d)) =
  fromIntegral (d .&. 0xFF00) `shiftL` 24 .|.
  fromIntegral (d .&. 0x00FF) `shiftL` 16 .|.
  fromIntegral a `shiftL` 8 .|.
  fromIntegral op
encode (Instruction op (ThreeArgs a b c)) =
  fromIntegral b `shiftL` 24 .|. fromIntegral c `shiftL` 16 .|.
  fromIntegral a `shiftL` 8 .|.
  fromIntegral op

islt a d = Instruction 0 $ TwoArgs a d
isge a d = Instruction 1 $ TwoArgs a d
isle a d = Instruction 2 $ TwoArgs a d
isgt a d = Instruction 3 $ TwoArgs a d
iseqv a d = Instruction 4 $ TwoArgs a d
isnev a d = Instruction 5 $ TwoArgs a d
iseqs a d = Instruction 6 $ TwoArgs a d
isnes a d = Instruction 7 $ TwoArgs a d
iseqn a d = Instruction 8 $ TwoArgs a d
isnen a d = Instruction 9 $ TwoArgs a d
iseqp a d = Instruction 10 $ TwoArgs a d
isnep a d = Instruction 11 $ TwoArgs a d
istc a d = Instruction 12 $ TwoArgs a d
isfc a d = Instruction 13 $ TwoArgs a d
ist a d = Instruction 14 $ TwoArgs a d
isf a d = Instruction 15 $ TwoArgs a d
mov a d = Instruction 16 $ TwoArgs a d
bnot a d = Instruction 17 $ TwoArgs a d
unm a d = Instruction 18 $ TwoArgs a d
len a d = Instruction 19 $ TwoArgs a d
addvn a b c = Instruction 20 $ ThreeArgs a b c
subvn a b c = Instruction 21 $ ThreeArgs a b c
mulvn a b c = Instruction 22 $ ThreeArgs a b c
divvn a b c = Instruction 23 $ ThreeArgs a b c
modvn a b c = Instruction 24 $ ThreeArgs a b c
addnv a b c = Instruction 25 $ ThreeArgs a b c
subnv a b c = Instruction 26 $ ThreeArgs a b c
mulnv a b c = Instruction 27 $ ThreeArgs a b c
divnv a b c = Instruction 28 $ ThreeArgs a b c
modnv a b c = Instruction 29 $ ThreeArgs a b c
addvv a b c = Instruction 30 $ ThreeArgs a b c
subvv a b c = Instruction 31 $ ThreeArgs a b c
mulvv a b c = Instruction 32 $ ThreeArgs a b c
divvv a b c = Instruction 33 $ ThreeArgs a b c
modvv a b c = Instruction 34 $ ThreeArgs a b c
pow a b c = Instruction 35 $ ThreeArgs a b c
cat a b c = Instruction 36 $ ThreeArgs a b c
kstr a d = Instruction 37 $ TwoArgs a d
kcdata a d = Instruction 38 $ TwoArgs a d
kshort a d = Instruction 39 $ TwoArgs a d
knum a d = Instruction 40 $ TwoArgs a d
kpri a d = Instruction 41 $ TwoArgs a d
knil a d = Instruction 42 $ TwoArgs a d
uget a d = Instruction 43 $ TwoArgs a d
usetv a d = Instruction 44 $ TwoArgs a d
usets a d = Instruction 45 $ TwoArgs a d
usetn a d = Instruction 46 $ TwoArgs a d
usetp a d = Instruction 47 $ TwoArgs a d
uclo a d = Instruction 48 $ TwoArgs a d
fnew a d = Instruction 49 $ TwoArgs a d
tnew a d = Instruction 50 $ TwoArgs a d
tdup a d = Instruction 51 $ TwoArgs a d
gget a d = Instruction 52 $ TwoArgs a d
gset a d = Instruction 53 $ TwoArgs a d
tgetv a b c = Instruction 54 $ ThreeArgs a b c
tgets a b c = Instruction 55 $ ThreeArgs a b c
tgetb a b c = Instruction 56 $ ThreeArgs a b c
tsetv a b c = Instruction 57 $ ThreeArgs a b c
tsets a b c = Instruction 58 $ ThreeArgs a b c
tsetb a b c = Instruction 59 $ ThreeArgs a b c
tsetm a d = Instruction 60 $ TwoArgs a d
callm a b c = Instruction 61 $ ThreeArgs a b c
call a b c = Instruction 62 $ ThreeArgs a b c
callmt a d = Instruction 63 $ TwoArgs a d
callt a d = Instruction 64 $ TwoArgs a d
iterc a b c = Instruction 65 $ ThreeArgs a b c
itern a b c = Instruction 66 $ ThreeArgs a b c
varg a b c = Instruction 67 $ ThreeArgs a b c
isnext a d = Instruction 68 $ TwoArgs a d
retm a d = Instruction 69 $ TwoArgs a d
ret a d = Instruction 70 $ TwoArgs a d
ret0 a d = Instruction 71 $ TwoArgs a d
ret1 a d = Instruction 72 $ TwoArgs a d
fori a d = Instruction 73 $ TwoArgs a d
jfori a d = Instruction 74 $ TwoArgs a d
forl a d = Instruction 75 $ TwoArgs a d
iforl a d = Instruction 76 $ TwoArgs a d
jforl a d = Instruction 77 $ TwoArgs a d
iterl a d = Instruction 78 $ TwoArgs a d
iiterl a d = Instruction 79 $ TwoArgs a d
jiterl a d = Instruction 80 $ TwoArgs a d
loop a d = Instruction 81 $ TwoArgs a d
iloop a d = Instruction 82 $ TwoArgs a d
jloop a d = Instruction 83 $ TwoArgs a d
jmp a d = Instruction 84 $ TwoArgs a d
funcf a d = Instruction 85 $ TwoArgs a d
ifuncf a d = Instruction 86 $ TwoArgs a d
jfuncf a d = Instruction 87 $ TwoArgs a d
funcv a d = Instruction 88 $ TwoArgs a d
ifuncv a d = Instruction 89 $ TwoArgs a d
jfuncv a d = Instruction 90 $ TwoArgs a d
funcc a d = Instruction 91 $ TwoArgs a d
funccw a d = Instruction 92 $ TwoArgs a d
