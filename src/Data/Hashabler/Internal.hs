{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash , UnliftedFFITypes #-}
-- For mixType:
{-# LANGUAGE KindSignatures, PolyKinds #-}
module Data.Hashabler.Internal where

-- To avoid circular dependencies.


import Data.Word
import Data.Int
import Data.Bits
import Data.Char
import Data.List

-- For ByteString & Text instances:
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy.Internal as BL (foldlChunks, ByteString)
#if MIN_VERSION_bytestring(0,10,4)
import qualified Data.ByteString.Short.Internal as BSh
#endif
import qualified Data.Text as T
import qualified Data.Text.Internal as T
import qualified Data.Text.Array as T (Array(..))
import qualified Data.Primitive as P
import qualified Data.Text.Lazy as TL (foldlChunks, Text)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (peekByteOff)

import Control.Exception(assert)

-- For casting of floating point values:
import Data.Array.ST (newArray, readArray, MArray, STUArray)
import Data.Array.Unsafe (castSTUArray)
import GHC.ST (runST, ST)

import Data.Version(Version, versionBranch)
import Data.Unique(Unique, hashUnique)

-- for reading the bytes of ByteStrings:
import System.IO.Unsafe (unsafeDupablePerformIO)

-- For getting our Int from ThreadId:
import Foreign.C (CInt(..))
import GHC.Conc(ThreadId(..))
import GHC.Prim(ThreadId#)

-- For TypeRep
import Data.Typeable
import GHC.Fingerprint.Type(Fingerprint(..))
#if  __GLASGOW_HASKELL__ >= 710
#else 
-- __GLASGOW_HASKELL__ >= 702
import Data.Typeable.Internal(TypeRep(..))
#endif

import System.Mem.StableName
import Data.Ratio (Ratio, denominator, numerator)

-- For Integer:
#ifdef MIN_VERSION_integer_gmp
import GHC.Exts (Int(..))
import GHC.Integer.GMP.Internals (Integer(..))
# if MIN_VERSION_integer_gmp(1,0,0)
import GHC.Integer.GMP.Internals (BigNat(BN#))
# endif
#endif

-- For GHC 7.10 Natural and Void:
#if MIN_VERSION_base(4,8,0)
import Data.Void (Void, absurd)
import GHC.Natural (Natural(..))
import GHC.Exts (Word(..))
#endif

-- For WORD_SIZE_IN_BITS constant:
-- TODO Use Data.Primitive.MachDeps ?
#include "MachDeps.h"
import MachDeps

-- Do error just once, and assume 32 else 64 below:
#if WORD_SIZE_IN_BITS == 32
-- for fast div by power of two:
#define LOG_SIZEOF_WORD 2
#elif WORD_SIZE_IN_BITS == 64
#define LOG_SIZEOF_WORD 3
#else
#error We only know how to support 32-bit and 64-bit systems, sorry.
#endif

import Unsafe.Coerce

-- COMMENTED BELOW, WHEN FOUND NOT BENEFICIAL:
-- These should be fine in all cases:
coerceInt32Word32 :: Int32 -> Word32
coerceInt32Word32 = 
#  if WORD_SIZE_IN_BITS == 32
    unsafeCoerce
#  else
    fromIntegral -- TODO or is unsafeCoerce okay on 64-bit?
#  endif
coerceInt64Word64 :: Int64 -> Word64
coerceInt64Word64 = unsafeCoerce
-- coerceIntWord :: Int -> Word
-- coerceIntWord = unsafeCoerce

-- For when argument is known in-bounds:
-- unsafeCoerceWord8 :: Word -> Word8
-- unsafeCoerceWord8 = unsafeCoerce

-- For 32-bit:
#if WORD_SIZE_IN_BITS == 32
unsafeCoerceIntWord32 :: Int -> Word32
unsafeCoerceIntWord32 = unsafeCoerce
-- unsafeCoerceWordWord32 :: Word -> Word32
-- unsafeCoerceWordWord32 = unsafeCoerce
#endif
  -- But why is that? The unsafeCoerce version simply has more instructions AFAICT!


#if MIN_VERSION_base(4,7,0)
-- Exported from Data.Word in base >= 4.7
#else
byteSwap32 :: Word32 -> Word32
byteSwap32 = _byteSwap32
# if WORD_SIZE_IN_BITS == 64
byteSwap64 :: Word64 -> Word64
byteSwap64 = _byteSwap64
# endif
#endif

-- TODO This is probably so slow it deserves a warning...
_byteSwap32 :: Word32 -> Word32
_byteSwap32 = \w-> 
    let mask0 = 0xFF000000
        mask1 = 0x00FF0000
        mask2 = 0x0000FF00
        mask3 = 0x000000FF
     in (unsafeShiftR (w .&. mask0) 24) .|.
        (unsafeShiftR (w .&. mask1) 8)  .|.
        (unsafeShiftL (w .&. mask2) 8)  .|.
        (unsafeShiftL (w .&. mask3) 24)

_byteSwap64 :: Word64 -> Word64
_byteSwap64 = \w-> 
    let mask0 = 0xFF00000000000000
        mask1 = 0x00FF000000000000
        mask2 = 0x0000FF0000000000
        mask3 = 0x000000FF00000000
        mask4 = 0x00000000FF000000
        mask5 = 0x0000000000FF0000
        mask6 = 0x000000000000FF00
        mask7 = 0x00000000000000FF
     in (unsafeShiftR (w .&. mask0) 56) .|.
        (unsafeShiftR (w .&. mask1) 40) .|.
        (unsafeShiftR (w .&. mask2) 24) .|.
        (unsafeShiftR (w .&. mask3) 8)  .|.
        (unsafeShiftL (w .&. mask4) 8)  .|.
        (unsafeShiftL (w .&. mask5) 24) .|.
        (unsafeShiftL (w .&. mask6) 40) .|.
        (unsafeShiftL (w .&. mask7) 56)

-- TODO BENCHMARKING for 'abs' see: http://graphics.stanford.edu/~seander/bithacks.html#IntegerAbs  and  http://stackoverflow.com/q/22445019/176841


foreign import ccall unsafe "rts_getThreadId" getThreadId :: ThreadId# -> CInt 


{-
-- see also the non-powers of two mapping methods outlined:
--  http://www.isthe.com/chongo/tech/comp/fnv/#FNV-1a
-}







-- EXTRACTING BYTES FROM DIFFERENT TYPES ----------------------------
-- NOTE we're to hash the resulting Word8s from left to right

-- TODO check inlining on these:

bytes16 :: Word16 -> (Word8, Word8)
{-# INLINE bytes16 #-}
bytes16 wd = (shifted 8, fromIntegral wd)
     where shifted = fromIntegral . unsafeShiftR wd

bytes32 :: Word32 -> (Word8,Word8,Word8,Word8)
{-# INLINE bytes32 #-}
bytes32 wd = (shifted 24, shifted 16, shifted 8, fromIntegral wd)
     where shifted = fromIntegral . unsafeShiftR wd

bytes64 :: Word64 -> (Word8,Word8,Word8,Word8,Word8,Word8,Word8,Word8)
{-# INLINE bytes64 #-}
bytes64 = \wd64->
#  if WORD_SIZE_IN_BITS == 32
    _bytes64_32 wd64
#  else
    _bytes64_64 wd64
#  endif

_bytes64_64 :: Word64 -> (Word8,Word8,Word8,Word8,Word8,Word8,Word8,Word8)
{-# INLINE _bytes64_64 #-}
_bytes64_64 wd = ( shifted 56, shifted 48, shifted 40, shifted 32
                 , shifted 24, shifted 16, shifted 8, fromIntegral wd)
     where shifted = fromIntegral . unsafeShiftR wd

-- faster for 32-bit archs
_bytes64_32 :: Word64 -> (Word8,Word8,Word8,Word8,Word8,Word8,Word8,Word8)
{-# INLINE _bytes64_32 #-}
_bytes64_32 wd = 
    let (wd0, wd1) = words32 wd
        (b0,b1,b2,b3) = bytes32 wd0
        (b4,b5,b6,b7) = bytes32 wd1
     in (b0,b1,b2,b3,b4,b5,b6,b7)

-- TODO can this be done more efficiently on 32-bit machines?
words32 :: Word64 -> (Word32, Word32)
{-# INLINE words32 #-}
words32 wd64 = (fromIntegral $ unsafeShiftR wd64 32, fromIntegral wd64)

-- These appear to return bytes in big endian on my machine (little endian),
-- but TODO verify what happens on a BE machine.
-- See: http://stackoverflow.com/a/7002812/176841 . 
-- Someone just kill me now...
floatToWord :: Float -> Word32
floatToWord x = runST (castViaSTArray x)

doubleToWord :: Double -> Word64
doubleToWord x = runST (castViaSTArray x)

castViaSTArray :: (MArray (STUArray s) a (ST s),
                   MArray (STUArray s) b (ST s)) => a -> ST s b
{-# INLINE castViaSTArray #-}
castViaSTArray x = newArray (0 :: Int,0) x >>= castSTUArray >>= flip readArray 0


-- HASHABLE CLASS AND INSTANCES -------------------------------------

newtype Hash32 a = Hash32 { hashWord32 :: Word32 }
    deriving (Show, Read, Eq)
newtype Hash64 a = Hash64 { hashWord64 :: Word64 }
    deriving (Show, Read, Eq)
data Hash128 a = Hash128 { hashWord128_0 :: !Word64, hashWord128_1 :: !Word64 }
    deriving (Show, Read, Eq)


-- | A class of types that can be converted into a hash value.  We expect all
-- instances to display "good" hashing properties (w/r/t avalanche, bit
-- independence, etc.) when passed to an ideal hash function.
--
-- We try to ensure that bytes are extracted from values in a way that is
-- portable across architectures (where possible), and straightforward to
-- replicate on other platforms and in other languages. Portable instances are
-- also instances of 'StableHashable', and non-portable instances are
-- __NOTE__-ed in instance docs here as well.
--
-- See the section <#principled "Defining Hashable instances"> for details of
-- what we expect from instances.
class Hashable a where
    -- | Add the bytes from the second argument into the hash, producing a new
    -- hash value. This is essentially a left fold of the methods of
    -- 'HashState' over individual bytes extracted from @a@.
    --
    -- For some instances of 'HashState', this method might be a complete
    -- hashing algorithm, or might comprise the core of a hashing algorithm
    -- (perhaps with some final mixing), or might do something completely apart
    -- from hashing (e.g. simply cons bytes into a list for debugging).
    --
    -- Implementations must ensure that, for the same data: 
    --    
    -- - @Word16\/32\/64@ arguments passed into the methods of 'HashState', and...
    --
    -- - the choice of @mix@ function itself...
    --
    -- ...are consistent across architectures of different word size and
    -- endianness. For example do not define an instance which conditionally
    -- implements 'mix64' only on 64-bit architectures.
    hash :: (HashState h)=> h -> a -> h


-- | A class for defining how a hash function consumes input data. Bytes are
-- fed to these methods in our 'Hashable' instances, which promise to call
-- these methods in a platform-independent way.
--
-- Instances of 'HashState' only need to define 'mix8', but may additionally
-- handle @mix@-ing in larger word chunks for performance reasons. For instance
-- a hash function which operates on four bytes at a time might make use of
-- 'mix32', and perhaps in 'mix8' pad with three additional 0s.
--
-- Endianness is normalized in 'Hashable' instances, so these mix methods can
-- expect to receive identical words across platforms.
class HashState h where
    -- | Mix in one byte.
    mix8 :: h -> Word8 -> h
    -- | Mix in a 2-byte word. Defaults to two 'mix8' on bytes from most to
    -- least significant.
    mix16 :: h -> Word16 -> h
    -- | Mix in a 4-byte word. Defaults to four 'mix8' on bytes from most to
    -- least significant.
    mix32 :: h -> Word32 -> h
    -- | Mix in a 8-byte word. Defaults to two 'mix32' on 32-byte words from
    -- most to least significant.
    mix64 :: h -> Word64 -> h

    -- Hash functions are likely to take individual bytes, or chunks of 32 or
    -- 64 bits, so I think these defaults make sense.
    {-# INLINE mix16 #-}
    mix16 h = \wd16-> 
       let (wd8_0,wd8_1) = bytes16 wd16
        in h `mix8` wd8_0 `mix8` wd8_1

    {-# INLINE mix32 #-}
    mix32 h = \wd32->
       let (b0,b1,b2,b3) = bytes32 wd32
        in h `mix8` b0 `mix8` b1 `mix8` b2 `mix8` b3 

    {-# INLINE mix64 #-}
    mix64 h = \wd64->
       let (w32_0, w32_1) = words32 wd64
        in h `mix32` w32_0 `mix32` w32_1




-- FNV HASH -------------------------------------------------

-- The special FNV primes required for different hash output sizes:

fnvPrime32 :: Word32
fnvPrime64 :: Word64
{-# INLINE fnvPrime32 #-}
{-# INLINE fnvPrime64 #-}
fnvPrime32 = 16777619
fnvPrime64 = 1099511628211
-- fnvPrime128 = 309485009821345068724781371
-- fnvPrime256 = 374144419156711147060143317175368453031918731002211


fnvOffsetBasis32 :: FNV32
fnvOffsetBasis64 :: FNV64
{-# INLINE fnvOffsetBasis32 #-}
{-# INLINE fnvOffsetBasis64 #-}
fnvOffsetBasis32 = FNV32 2166136261
fnvOffsetBasis64 = FNV64 14695981039346656037
-- fnvOffsetBasis128 = FNV128 144066263297769815596495629667062367629
-- fnvOffsetBasis256 = FNV256 100029257958052580907070968620625704837092796014241193945225284501741471925557


newtype FNV32 = FNV32 { fnv32 :: Word32 }
    deriving (Eq, Ord, Read, Show)

newtype FNV64 = FNV64 { fnv64 :: Word64 }
    deriving (Eq, Ord, Read, Show)


-- | @
-- 'mix8' ('FNV32' h32) b = 'FNV32' $ (h32 ``xor`` fromIntegral b) * 'fnvPrime32'
-- @
instance HashState FNV32 where
    {-# INLINE mix8 #-}
    mix8 (FNV32 h32) = \b-> FNV32 $ (h32 `xor` fromIntegral b) * fnvPrime32


-- | Hash a value using the standard spec-prescribed 32-bit seed value.
--
-- @
--   hashFNV32 = 'Hash32' . fnv32 . 'hash' 'fnvOffsetBasis32'
-- @
hashFNV32 :: Hashable a=> a -> Hash32 a
{-# INLINE hashFNV32 #-}
hashFNV32 = Hash32 . fnv32 . hash fnvOffsetBasis32


-- | @
-- 'mix8' ('FNV64' h64) b = 'FNV64' $ (h64 ``xor`` fromIntegral b) * 'fnvPrime64'
-- @
instance HashState FNV64 where
    {-# INLINE mix8 #-}
    mix8 (FNV64 h64) = \b-> FNV64 $ (h64 `xor` fromIntegral b) * fnvPrime64


-- | Hash a value using the standard spec-prescribed 64-bit seed value. This
-- may be slow on 32-bit machines.
--
-- @
--   hashFNV64 = 'Hash64' . fnv64 . 'hash' 'fnvOffsetBasis64'
-- @
hashFNV64 :: Hashable a=> a -> Hash64 a
{-# INLINE hashFNV64 #-}
hashFNV64 = Hash64 . fnv64 . hash fnvOffsetBasis64


-- ------------------------------------------------------------------
-- NUMERIC TYPES:

-- TODO TESTING: for 7.8 and below, see if we can get a small value into J#, and then test that it hashes to the same as the literal small value
--                (look at code; simple */div or +/- don't seem to do it)

-- NOTE: non-obviously, but per our rule about variable-width values, this must
-- also be wrapped in a `mixConstructor`; consider the hashes of (0xDEAD,
-- 0xBEEF) and (0xDE, 0xADBEEF). The way we mix in the sign handles this.
-- 
-- I would rather truncate to 8-bit "limbs" but using 32-bit limbs seems like a
-- good tradeoff: on 64-bit platforms we just do a conditional instead of on
-- avg 4 extra hash ops, and on 32-bit no extra work is required.
--
-- | Arbitrary-precision integers are hashed as follows: the magnitude is
-- represented with 32-bit chunks (at least one, for zero; but no more than
-- necessary), then bytes are added to the hash from most to least significant
-- (including all initial padding 0s). Finally 'mixConstructor' is called on
-- the resulting hash value, with @(1::Word8)@ if the @Integer@ was negative,
-- otherwise with @0@.
instance Hashable Integer where
    {-# INLINE hash #-}
-- integer-gmp implementation: --------------------------------------
#ifdef MIN_VERSION_integer_gmp
    hash h = \i-> case i of
      (S# n#) ->
        let magWord = magnitudeAsWord (I# n#)
            sign = _signByte (I# n#)
         in mixConstructor sign $ 
#           if WORD_SIZE_IN_BITS == 32
              h `mix32` magWord
#           else
              -- only hash enough 32-bit chunks as needed to represent magnitude
              h `mixSignificantMachWord64` magWord
              -- TODO benchmark and try unsafeCoerce on 64-bit
#           endif

-- GHC 7.10: ------------------------
--
#   if MIN_VERSION_integer_gmp(1,0,0)
        -- NOTE: these used only when out of range of Int:
      (Jp# bn) -> mixConstructor 0 $ hash32BigNatBytes h bn
      (Jn# bn) -> mixConstructor 1 $ hash32BigNatBytes h bn

-- GHC 7.8 and below: ---------------
--
-- J# is more or less directly the gmp arbitrary precision int type, where:
--     1) sz# is number of limbs, or negative of that for negative
--     2) limbs stored little endian (i.e. i[0] is least significant limb)
--     3) whenever sz# is non-zero , the most significant limb is non-zero; the
--        value 0 is represented by sz# == 0, in which case ba# is ignored
--     4) a limb is machine Word size.
-- And some Integer-specific caveats/notes:
--     5) J# may be used for even small integers
--     6) ba# may be over-allocated, so size should be ignored
#   else
--    Note, 5 and 3 together mean that we have to special case for sz == 0,
--    even though I can't get that case to occur in practice:
      (J# 0# _) -> mixConstructor 0 (h `mix32` 0)
      (J# sz# ba#) -> 
             -- Note, (abs minBound == minBound) but I don't think that value
             -- is possible since we wouldn't even be able to specify the size
             -- (maxBound+1) as an Int value.
         let numLimbs = abs (I# sz#)
             sign = _signByte (I# sz#)
          in assert ((I# sz#) /= minBound) $
              mixConstructor sign $ 
               hash32BigNatByteArrayBytes h numLimbs (P.ByteArray ba#)
#   endif

-- other Integer implementations: -----------------------------------
#else
    -- For non-gmp Integer; quite slow.
    hash = _hash32Integer
#endif


-- TODO benchmark against conditional
-- Helper to quickly (hopefully) extract sign bit (1 for negative, 0 otherwise)
-- from Int. Assumes two's complement.
_signByte :: Int -> Word8
{-# INLINE _signByte #-}
_signByte n = fromIntegral ((fromIntegral n :: Word) 
                              `unsafeShiftR` (WORD_SIZE_IN_BITS - 1))

-- Exposed for testing. In particular we ensure that magnitudeAsWord minBound
-- is correct.
-- TODO make Int -> Word, and use fromIntegral at usage site, maybe
magnitudeAsWord :: Int 
#             if WORD_SIZE_IN_BITS == 32
                -> Word32
#             else
                -> Word64
#             endif
magnitudeAsWord = fromIntegral . abs

#if WORD_SIZE_IN_BITS == 64
-- Helper for hashing a 64-bit word, possibly omiting the first 32-bit chunk
-- (if 0). We use this when normalizing big natural representations.
mixSignificantMachWord64 :: (HashState h)=> h -> Word64 -> h
{-# INLINE mixSignificantMachWord64 #-}
mixSignificantMachWord64 h w64 = 
     let (word32_0, word32_1) = words32 w64
      in if word32_0 == 0 
          then h `mix32` word32_1
          else h `mix32` word32_0 `mix32`  word32_1
#endif


-- Very slow Integer-implementation-agnostic hashing:
_hash32Integer :: (HashState h)=> h -> Integer -> h
_hash32Integer h i = 
    let (sgn, limbs) = _integerWords i
     in mixConstructor sgn $ 
         foldl' hash h limbs

-- Convert an opaque Integer into a gmp-like format, except that we order our
-- list of limbs returned from most to least significant:
_integerWords :: Integer -> (Word8, [Word32])
_integerWords nSigned = (sign , go (abs nSigned) []) where
    sign = if nSigned < 0 then 1 else 0
           -- we will hash at least one limb (even if zero):
    go nMag acc = let (nMag', w32) = splitAtLastWord nMag
                   in (if nMag' == 0 then id else go nMag') (w32:acc)

    splitAtLastWord :: Integer -> (Integer, Word32)
    splitAtLastWord x = 
      assert (x >= 0) $
        (x `shiftR` 32, fromIntegral x)


#ifdef MIN_VERSION_integer_gmp
-- GHC 7.10:
# if MIN_VERSION_integer_gmp(1,0,0)
-- Internal. Hashable instances will require a 'mixConstructor'. We use the same
-- chunks-of-32-bits scheme as in the Integer instance.
--
-- Invariants of BigNat (from docs):
--   - ByteArray# size is an exact multiple of Word# size
--   - limbs are stored in least-significant-limb-first order,
--   - the most-significant limb must be non-zero, except for
--      0 which is represented as a 1-limb.
--      - NOTE, though: Jp#/Jn# in Integer on GHC 7.10 guarantee that contained
--        BigNat are non-zero
hash32BigNatBytes :: (HashState h)=> h -> BigNat -> h
{-# INLINE hash32BigNatBytes #-}
hash32BigNatBytes h (BN# ba#) = 
    let ba = P.ByteArray ba#
        szBytes = P.sizeofByteArray ba
        numLimbs = szBytes `unsafeShiftR` LOG_SIZEOF_WORD
     in assert (numLimbs >= 1 && (numLimbs * SIZEOF_HSWORD) == szBytes) $
         hash32BigNatByteArrayBytes h numLimbs ba


-- | The @BigNat@'s value is represented in 32-bit chunks (at least one, for
-- zero; but no more than necessary), then bytes are added to the hash from
-- most to least significant (including all initial padding 0s). Finally
-- @'mixConstructor' 0@ is called on the resulting hash value.
--
-- Exposed only in GHC 7.10.
instance Hashable BigNat where
    {-# INLINE hash #-}
    hash h = mixConstructor 0 . hash32BigNatBytes h

# endif


-- Hashing of internals of BigNat-format ByteArrays of at least 1 limb, for old
-- and new style Integer from integer-gmp.
hash32BigNatByteArrayBytes :: (HashState h)=> h -> Int -> P.ByteArray -> h
{-# INLINE hash32BigNatByteArrayBytes #-}
hash32BigNatByteArrayBytes h numLimbs ba = 
  assert (numLimbs > 0) $
    let mostSigLimbIx = numLimbs - 1
        -- NOTE: to correctly handle small-endian, we must read in Word-size
        -- chunks (not just Word32 size)
        go !h' (-1) = h'
        go !h' !ix = let wd = P.indexByteArray ba ix
                      in go (h' `mixWord` wd) (ix - 1)
#  if WORD_SIZE_IN_BITS == 32
        mixWord = mix32
     in go h mostSigLimbIx
#  else
        mixWord h' wd = let (wd32_0, wd32_1) = words32 wd
                         in h' `mix32` wd32_0 `mix32` wd32_1
        -- handle dropping possibly-empty most-significant Word32, before
        -- processing remaining limbs:
        h0 = let mostSigLimb = P.indexByteArray ba mostSigLimbIx
              in h `mixSignificantMachWord64` mostSigLimb
        ix0 = mostSigLimbIx - 1
     in go h0 ix0
#  endif

#endif

-- Also GHC 7.10:
#if MIN_VERSION_base(4,8,0)
-- | The @Natural@'s value is represented in 32-bit chunks (at least one, for
-- zero; but no more than necessary), then bytes are added to the hash from
-- most to least significant (including all initial padding 0s). Finally
-- @'mixConstructor' 0@ is called on the resulting hash value.
--
-- Exposed only in GHC 7.10
instance Hashable Natural where
    {-# INLINE hash #-}
    hash h nat = case nat of
# if defined (MIN_VERSION_integer_gmp) && MIN_VERSION_integer_gmp(1,0,0)
        -- For Word-size natural
        (NatS# wd#) -> mixConstructor 0 $
#         if WORD_SIZE_IN_BITS == 32
            h `mix32` (fromIntegral $ W# wd#)  -- TODO benchmark unsafeCoerce
#         else
            h `mixSignificantMachWord64` (fromIntegral $ W# wd#)  -- TODO benchmark unsafeCoerce on 64-bit
#         endif
        -- Else using a BigNat (which instance calls required mixConstructor):
        (NatJ# bn)  -> hash h bn
# else
        -- Natural represented with non-negative Integer:
        (Natural n) -> hash h n
# endif

-- This is the instance in void-0.7:
--
-- | > hash _ _ = absurd
--
-- Exposed only in GHC 7.10
instance Hashable Void where
    hash _ = absurd

#endif


-- | > hash s a = s `hash` numerator a `hash` denominator a
instance (Integral a, Hashable a) => Hashable (Ratio a) where
    {-# INLINE hash #-}
    hash s a = s `hash` numerator a `hash` denominator a


-- ---------
-- Architecture-dependent types, with special handling.

-- | __NOTE__: @Int@ has platform-dependent size. When hashing on 64-bit machines
-- if the @Int@ value to be hashed falls in the 32-bit Int range, we first cast
-- it to an Int32. This should help ensure that programs that are correct
-- across architectures will also produce the same hash values.
instance Hashable Int where
    {-# INLINE hash #-}
    hash h i =
#     if WORD_SIZE_IN_BITS == 32
        mix32 h $ unsafeCoerceIntWord32 i
#     else
        _hash32_Int_64 h (fromIntegral i)
#     endif

-- | __NOTE__: @Word@ has platform-dependent size. When hashing on 64-bit
-- machines if the @Word@ value to be hashed falls in the 32-bit Word range, we
-- first cast it to a Word32. This should help ensure that programs that are
-- correct across architectures will also produce the same hash values.
instance Hashable Word where
    {-# INLINE hash #-}
    hash h w =
#     if WORD_SIZE_IN_BITS == 32
        hash h (fromIntegral w :: Word32)
#     else
        _hash32_Word_64 h (fromIntegral w) -- TODO benchmarking unsafeCoerce on 64-bit
#     endif


-- TODO Benchmarking + try unsafeCoerce on 64-bit
-- NOTE: the expressions in the conditionals alone make these quite slow on
--       32-bit machines, so don't worry about benchmarking this directly.
_hash32_Int_64 :: (HashState h)=> h -> Int64 -> h
{-# INLINE _hash32_Int_64 #-}
_hash32_Int_64 h = \i->
    -- Can we losslessly cast to 32-bit representation?
    if i <= (fromIntegral (maxBound :: Int32)) && 
       i >= (fromIntegral (minBound :: Int32)) -- TODO benchmark and maybe use (.&.), and check ==0
        then hash h (fromIntegral i :: Int32)
        else hash h i

_hash32_Word_64 :: (HashState h)=> h -> Word64 -> h
{-# INLINE _hash32_Word_64 #-}
_hash32_Word_64 h = \w->
    -- Can we losslessly cast to 32-bit representation?
    if w <= (fromIntegral (maxBound :: Word32))
        then hash h (fromIntegral w :: Word32)
        else hash h w



-- | Hash a Float as IEEE 754 single-precision format bytes. This is terribly
-- slow; direct complaints to http://hackage.haskell.org/trac/ghc/ticket/4092
instance Hashable Float where
    {-# INLINE hash #-}
    hash h = \x-> assert (isIEEE x) $
        mix32 h $ floatToWord x

-- | Hash a Double as IEEE 754 double-precision format bytes. This is terribly
-- slow; direct complaints to http://hackage.haskell.org/trac/ghc/ticket/4092
instance Hashable Double where
    {-# INLINE hash #-}
    hash h = \x-> assert (isIEEE x) $
        mix64 h $ doubleToWord x


-- GHC uses two's complement representation for signed ints; C has this
-- undefined, I guess; just cast to Word and hash.

instance Hashable Int8 where
    {-# INLINE hash #-}
    hash h = mix8 h . fromIntegral

instance Hashable Int16 where
    {-# INLINE hash #-}
    hash h = mix16 h . fromIntegral

instance Hashable Int32 where
    {-# INLINE hash #-}
    hash h = mix32 h . coerceInt32Word32


instance Hashable Int64 where
    {-# INLINE hash #-}
    hash h = mix64 h . coerceInt64Word64

-- Straightforward hashing of different Words and byte arrays:

instance Hashable Word8 where
    {-# INLINE hash #-}
    hash = mix8

instance Hashable Word16 where
    {-# INLINE hash #-}
    hash = mix16

instance Hashable Word32 where
    {-# INLINE hash #-}
    hash = mix32

instance Hashable Word64 where
    {-# INLINE hash #-}
    hash = mix64


-- ------------------------------------------------------------------
-- ARRAYS AND LIST:


-- Since below have variable-length, we'll use this helper (which is also
-- useful for multi-constructor types):

-- | > mixConstructor n h = h `mix8` (0xFF - n)
mixConstructor :: (HashState h)
               => Word8  -- ^ Constructor number. We recommend starting from 0 and incrementing.
               -> h      -- ^ Hash state value to mix our byte into
               -> h      -- ^ New hash state
{-# INLINE mixConstructor #-}
mixConstructor n = \h-> h `mix8` (0xFF - n)

-- | Strict @ByteString@
instance Hashable B.ByteString where
    {-# INLINE hash #-}
    hash h = mixConstructor 0 .
        hashByteString h

-- TODO benchmarks for fusion:
-- | Lazy @ByteString@
instance Hashable BL.ByteString where
    {-# INLINE hash #-}
    hash h = mixConstructor 0 .
        BL.foldlChunks hashByteString h

#if MIN_VERSION_bytestring(0,10,4)
-- | Exposed only in bytestring >= v0.10.4
instance Hashable BSh.ShortByteString where
    {-# INLINE hash #-}
    hash h  = 
      \(BSh.SBS ba_) -> -- when MIN_VERSION_base(4,3,0)
        let ba = P.ByteArray ba_
         in mixConstructor 0 $
              hashByteArray h (P.sizeofByteArray ba) ba
#endif

-- | Strict @Text@, hashed as big endian UTF-16.
instance Hashable T.Text where
    {-# INLINE hash #-}
    hash h = mixConstructor 0 .
        hashText h

-- TODO benchmarks for fusion:
-- | Lazy @Text@, hashed as big endian UTF-16.
instance Hashable TL.Text where
    {-# INLINE hash #-}
    hash h = mixConstructor 0 .
        TL.foldlChunks hashText h

-- | Here we hash each byte of the array in turn. If using this to hash some
-- data stored internally as a @ByteArray#@, be aware that depending on the
-- size and alignment requirements of that data, as well as the endianness of
-- your machine, this might result in different hash values across different
-- architectures.
instance Hashable P.ByteArray where
    {-# INLINE hash #-}
    hash h = \ba-> mixConstructor 0 $
        hashByteArray h (P.sizeofByteArray ba) ba

-- ------------------------------------------------------------------
-- MISC THINGS:



-- TODO look at core
-- | Hash a @Char@ as big endian UTF-16. Note that Char permits values in the
-- reserved unicode range U+D800 to U+DFFF; these Char values are added to the
-- hash just as if they were valid 16-bit characters.
instance Hashable Char where
    {-# INLINE hash #-}
    hash h = go where
      -- Encoding a unicode code point in UTF-16. adapted from
      -- Data.Text.Internal.Unsafe.Char.unsafeWrite:
    --go c | n .&. complement 0xFFFF == 0 =  -- TODO try this, etc. TODO try look at core & try unsafeCoerce
      go c | n < 0x10000 = h `mix16` fromIntegral n
              -- TODO MODIFY lo AND CALL mix32, 
           | otherwise = h `mix16` lo `mix16` hi

        where n = ord c
              m = n - 0x10000
              lo = fromIntegral $ (m `unsafeShiftR` 10) + 0xD800
              hi = fromIntegral $ (m .&. 0x3FF) + 0xDC00


-- | __NOTE__: no promise of consistency across runs or platforms.
instance Hashable ThreadId where
    {-# INLINE hash #-}
    hash h = \(ThreadId tid)-> 
        hash h (fromIntegral $ getThreadId tid :: Word)

-- | __NOTE__: no promise of consistency across platforms or GHC versions.
instance Hashable TypeRep where
    {-# INLINE hash #-}
    hash h = hash h . typeRepInt32

typeRepInt32 :: TypeRep -> Int32
{-# INLINE typeRepInt32 #-}
typeRepInt32 = 
# if __GLASGOW_HASKELL__ >= 710
    -- Fingerprint is just the MD5, so taking any Int from it is fine
    (\(Fingerprint i64 _) -> fromIntegral i64) . typeRepFingerprint
# else
-- __GLASGOW_HASKELL__ >= 702
    -- Fingerprint is just the MD5, so taking any Int from it is fine
    \(TypeRep (Fingerprint i64 _) _ _) -> fromIntegral i64
# endif


-- | __NOTE__: No promise of stability across runs or platforms. Implemented via
-- 'hashStableName'.
instance Hashable (StableName a) where
    {-# INLINE hash #-}
    hash h = \x-> hash h $ hashStableName x

-- | The (now deprecated) @versionTags@ field is ignored, and we follow the
-- 'Eq' instance which does not ignore trailing zeros.
instance Hashable Version where
    {-# INLINE hash #-}
    hash h = \x-> hash h $ versionBranch x

-- | __NOTE__: No promise of stability across runs or platforms. Implemented via
-- 'hashUnique'.
instance Hashable Unique where
    {-# INLINE hash #-}
    hash h = \x-> hash h $ hashUnique x

-- ------------------------------------------------------------------
-- ALGEBRAIC DATA TYPES:


-- ---------
-- Sum types

-- | > hash h = hash h . \b-> if b then (1::Word8) else 0
instance Hashable Bool where
    {-# INLINE hash #-}
    hash h = hash h . \b-> if b then (1::Word8) else 0


instance Hashable Ordering where
    {-# INLINE hash #-}
    hash h = flip mixConstructor h . fromIntegral . fromEnum

instance Hashable a => Hashable [a] where
    -- TODO OPTIMIZE (see notes below)
    {-# INLINE hash #-}
    hash h = mixConstructor 0 .
        hashFoldl' h

instance Hashable a => Hashable (Maybe a) where
    {-# INLINE hash #-}
    hash h Nothing  = mixConstructor 0 h
    hash h (Just a) = mixConstructor 1 $ hash h a

instance (Hashable a, Hashable b) => Hashable (Either a b) where
    {-# INLINE hash #-}
    hash h = either (mx 0) (mx 1) where
        mx n = mixConstructor n . hash h


-- ---------
-- Tuples (product types)

-- Per our rules, this must perturb the hash value by at least a byte, even
-- though its value is entirely "fixed" by its type. Consider [()]; the
-- instance relies on () following the rule.
--
-- | > hash = const . mixConstructor 0
instance Hashable () where
    {-# INLINE hash #-}
    hash = const . mixConstructor 0

instance (Hashable a1, Hashable a2) => Hashable (a1, a2) where
    {-# INLINE hash #-}
    hash h (a,b) = h `hash` a `hash` b

instance (Hashable a1, Hashable a2, Hashable a3) => Hashable (a1, a2, a3) where
    {-# INLINE hash #-}
    hash h (a,b,c) = h `hash` a `hash` b `hash` c

instance (Hashable a1, Hashable a2, Hashable a3, Hashable a4) => Hashable (a1, a2, a3, a4) where
    {-# INLINE hash #-}
    hash h (a,b,c,d) = h `hash` a `hash` b `hash` c `hash` d

instance (Hashable a1, Hashable a2, Hashable a3, Hashable a4, Hashable a5) => Hashable (a1, a2, a3, a4, a5) where
    {-# INLINE hash #-}
    hash h (a,b,c,d,e) = h `hash` a `hash` b `hash` c `hash` d `hash` e

instance (Hashable a1, Hashable a2, Hashable a3, Hashable a4, Hashable a5, Hashable a6) => Hashable (a1, a2, a3, a4, a5, a6) where
    {-# INLINE hash #-}
    hash h (a,b,c,d,e,f) = h `hash` a `hash` b `hash` c `hash` d `hash` e `hash` f

instance (Hashable a1, Hashable a2, Hashable a3, Hashable a4, Hashable a5, Hashable a6, Hashable a7) => Hashable (a1, a2, a3, a4, a5, a6, a7) where
    {-# INLINE hash #-}
    hash h (a,b,c,d,e,f,g) = h `hash` a `hash` b `hash` c `hash` d `hash` e `hash` f `hash` g

instance (Hashable a1, Hashable a2, Hashable a3, Hashable a4, Hashable a5, Hashable a6, Hashable a7, Hashable a8) => Hashable (a1, a2, a3, a4, a5, a6, a7, a8) where
    {-# INLINE hash #-}
    hash hsh (a,b,c,d,e,f,g,h) = hsh `hash` a `hash` b `hash` c `hash` d `hash` e `hash` f `hash` g `hash` h

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i)=> Hashable (a, b, c, d, e, f, g, h, i) where
    {-# INLINE hash #-}
    hash hsh (a, b, c, d, e, f, g, h, i) = hsh `hash` a `hash` b `hash` c `hash` d `hash` e `hash` f `hash` g `hash` h `hash` i

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j)=> Hashable (a, b, c, d, e, f, g, h, i, j) where
    {-# INLINE hash #-}
    hash hsh (a, b, c, d, e, f, g, h, i, j) = hsh `hash` a `hash` b `hash` c `hash` d `hash` e `hash` f `hash` g `hash` h `hash` i `hash` j

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k)=> Hashable (a, b, c, d, e, f, g, h, i, j, k) where
    {-# INLINE hash #-}
    hash hsh (a, b, c, d, e, f, g, h, i, j, k) = hsh `hash` a `hash` b `hash` c `hash` d `hash` e `hash` f `hash` g `hash` h `hash` i `hash` j `hash` k

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l)=> Hashable (a, b, c, d, e, f, g, h, i, j, k, l) where
    {-# INLINE hash #-}
    hash hsh (a, b, c, d, e, f, g, h, i, j, k, l) = hsh `hash` a `hash` b `hash` c `hash` d `hash` e `hash` f `hash` g `hash` h `hash` i `hash` j `hash` k `hash` l

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l, Hashable m)=> Hashable (a, b, c, d, e, f, g, h, i, j, k, l, m) where
    {-# INLINE hash #-}
    hash hsh (a, b, c, d, e, f, g, h, i, j, k, l, m) = hsh `hash` a `hash` b `hash` c `hash` d `hash` e `hash` f `hash` g `hash` h `hash` i `hash` j `hash` k `hash` l `hash` m

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l, Hashable m, Hashable n)=> Hashable (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
    {-# INLINE hash #-}
    hash hsh (a, b, c, d, e, f, g, h, i, j, k, l, m, n) = hsh `hash` a `hash` b `hash` c `hash` d `hash` e `hash` f `hash` g `hash` h `hash` i `hash` j `hash` k `hash` l `hash` m `hash` n

instance (Hashable a, Hashable b, Hashable c, Hashable d, Hashable e, Hashable f, Hashable g, Hashable h, Hashable i, Hashable j, Hashable k, Hashable l, Hashable m, Hashable n, Hashable o)=> Hashable (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
    {-# INLINE hash #-}
    hash hsh (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) = hsh `hash` a `hash` b `hash` c `hash` d `hash` e `hash` f `hash` g `hash` h `hash` i `hash` j `hash` k `hash` l `hash` m `hash` n `hash` o




-- | A helper for implementing 'typeHash' by 'xor'-ing type parameters with a
-- new random hash value. E.g.:
--
-- @
--  instance (StableHashable a, StableHashable b) => StableHashable (a, b) where
--      typeHash = mixType (mixType (TypeHash 12071780118071628513))
--                   \         \                \__ a new random value for (,)
--                    \         \____ mix in the type hash for 'a'
--                     \__ mix in the type hash for 'b'
-- @
mixType :: forall a t. StableHashable a=> TypeHash t -> TypeHash (t a)
mixType (TypeHash t)= TypeHash (t `xor` (typeHashWord (typeHash :: TypeHash a)))


-- | A value that uniquely identifies a 'StableHashable' type. This serves to
-- both version a type with respect to its 'Hashable' instance, and distinguish
-- types from each other (similar to 'TypeRep') across program runs, platforms
-- and library versions.
newtype TypeHash (a :: k) = TypeHash { typeHashWord :: Word64 }
    deriving (Eq, Read, Show, Bits)

-- > typeHashOf _ = typeHash
typeHashOf :: StableHashable a=> a -> TypeHash a
typeHashOf _ = typeHash

-- > typeHashOfProxy _ = typeHash
typeHashOfProxy :: StableHashable a=> proxy a -> TypeHash a
typeHashOfProxy _ = typeHash

-- | Types whose hashes can be compared across platforms. This is somewhat like
-- a limited, but cross-platform 'Typeable'.
--
-- Instances are expected to be universally-unique, and should be generated
-- randomly. Type parameters can be hashed together using 'mixType', like:
--
-- > instance (StableHashable b) => StableHashable (A b) where
-- >     typeHash = mixType (TypeHash 530184177609460980)
--
-- When 'Hashable' instances change, the 'TypeHash' must be changed to a new
-- random value. This lets us \"version\" a set of hashes; if we store a
-- 'TypeHash' along with a set of hashes in program /A/, in program /B/ we can
-- compare the stored value with our own 'TypeHash' and verify that hashes we
-- generate in program /B/ can be meaningfully compared.
--
-- Note, obviously this doesn't ensure that values were hashed with the same
-- hashing algorithm, and you should come up with your own means to serialize
-- that information if you need to.
class Hashable a=> StableHashable a where
    typeHash :: TypeHash a

-- | The value here depends on whether we're on a 32 or 64-bit platform. See
-- also the instance documentation for 'Hashable'.
instance StableHashable Int where
#  if WORD_SIZE_IN_BITS == 32
    typeHash = TypeHash 14906715774445347101
#  else
    typeHash = TypeHash 13382803217769822997
#  endif
-- | The value here depends on whether we're on a 32 or 64-bit platform. See
-- also the instance documentation for 'Hashable'.
instance StableHashable Word where
#  if WORD_SIZE_IN_BITS == 32
    typeHash = TypeHash 10996918434311873249
#  else
    typeHash = TypeHash 943142231655442729
#  endif

instance StableHashable Integer where
    typeHash = TypeHash 96690694942656444

#ifdef MIN_VERSION_integer_gmp
# if MIN_VERSION_integer_gmp(1,0,0)
instance StableHashable BigNat where
    typeHash = TypeHash 2111364012200171327
# endif
#endif
#if MIN_VERSION_base(4,8,0)
instance StableHashable Natural where
    typeHash = TypeHash 11915819290390802320
instance StableHashable Void where
    typeHash = TypeHash 13639848524738715571
#endif

instance (Integral a, StableHashable a) => StableHashable (Ratio a) where
    typeHash = mixType (TypeHash 330184177609460989)
instance StableHashable Float where
    typeHash = TypeHash 7785239337948379302
instance StableHashable Double where
    typeHash = TypeHash 12403185125095650454
instance StableHashable Int8 where
    typeHash = TypeHash 17471749136236681265
instance StableHashable Int16 where
    typeHash = TypeHash 14440046210595456836
instance StableHashable Int32 where
    typeHash = TypeHash 13431688382274668720
instance StableHashable Int64 where
    typeHash = TypeHash 14806970576519879451
instance StableHashable Word8 where
    typeHash = TypeHash 6643259480050182665
instance StableHashable Word16 where
    typeHash = TypeHash 3765569608911963661
instance StableHashable Word32 where
    typeHash = TypeHash 6402686280864807547
instance StableHashable Word64 where
    typeHash = TypeHash 14652873008202722152
instance StableHashable B.ByteString where
    typeHash = TypeHash 171314019417081845
instance StableHashable BL.ByteString where
    typeHash = TypeHash 10099361054646539018
#if MIN_VERSION_bytestring(0,10,4)
instance StableHashable BSh.ShortByteString where
    typeHash = TypeHash 15680327781389053206
#endif
instance StableHashable T.Text where
    typeHash = TypeHash 14746544807555826150
instance StableHashable TL.Text where
    typeHash = TypeHash 10657741718622626930
instance StableHashable P.ByteArray where
    typeHash = TypeHash 9976019413528454024
instance StableHashable Char where
    typeHash = TypeHash 4949001641339870281
instance StableHashable Version where
    typeHash = TypeHash 702645064027678737
instance StableHashable Bool where
    typeHash = TypeHash 2172478990419421580
instance StableHashable Ordering where
    typeHash = TypeHash 9293112338546135338
instance StableHashable a => StableHashable [a] where
    typeHash = mixType (TypeHash 8959911929979074606)
instance StableHashable a => StableHashable (Maybe a) where
    typeHash = mixType (TypeHash 17404804613103585388)
instance (StableHashable a, StableHashable b) => StableHashable (Either a b) where
    typeHash = mixType (TypeHash 2275317158072284048)
instance StableHashable () where
    typeHash = TypeHash 6095166973227743591
instance (StableHashable a, StableHashable b) => StableHashable (a, b) where
    typeHash = mixType $ mixType (TypeHash 12071780118071628513)
instance (StableHashable a, StableHashable b, StableHashable c) => StableHashable (a, b, c) where
    typeHash = mixType $ mixType $ (TypeHash 4618299809208311661)
instance (StableHashable a, StableHashable b, StableHashable c, StableHashable d) => StableHashable (a, b, c, d) where
    typeHash = mixType $mixType $mixType $   (TypeHash 2134528412514930125)
instance (StableHashable a, StableHashable b, StableHashable c, StableHashable d, StableHashable e) => StableHashable (a, b, c, d, e) where
    typeHash = mixType $mixType $mixType $mixType $    (TypeHash 6145113094462899758)
instance (StableHashable a, StableHashable b, StableHashable c, StableHashable d, StableHashable e, StableHashable f) => StableHashable (a, b, c, d, e, f) where
    typeHash = mixType $mixType $mixType $mixType $mixType $     (TypeHash 44771254230381456)
instance (StableHashable a, StableHashable b, StableHashable c, StableHashable d, StableHashable e, StableHashable f, StableHashable g) => StableHashable (a, b, c, d, e, f, g) where
    typeHash = mixType $mixType $mixType $mixType $mixType $mixType $      (TypeHash 9917360176431723073)
instance (StableHashable a, StableHashable b, StableHashable c, StableHashable d, StableHashable e, StableHashable f, StableHashable g, StableHashable h) => StableHashable (a, b, c, d, e, f, g, h) where
    typeHash = mixType $mixType $mixType $mixType $mixType $mixType $mixType $       (TypeHash 2303481052083416811)
instance (StableHashable a, StableHashable b, StableHashable c, StableHashable d, StableHashable e, StableHashable f, StableHashable g, StableHashable h, StableHashable i)=> StableHashable (a, b, c, d, e, f, g, h, i) where
    typeHash = mixType $mixType $mixType $mixType $mixType $mixType $mixType $mixType $        (TypeHash 6307505215888440984)
instance (StableHashable a, StableHashable b, StableHashable c, StableHashable d, StableHashable e, StableHashable f, StableHashable g, StableHashable h, StableHashable i, StableHashable j)=> StableHashable (a, b, c, d, e, f, g, h, i, j) where
    typeHash = mixType $mixType $mixType $mixType $mixType $mixType $mixType $mixType $mixType $         (TypeHash 16862409449834578942)
instance (StableHashable a, StableHashable b, StableHashable c, StableHashable d, StableHashable e, StableHashable f, StableHashable g, StableHashable h, StableHashable i, StableHashable j, StableHashable k)=> StableHashable (a, b, c, d, e, f, g, h, i, j, k) where
    typeHash = mixType $mixType $mixType $mixType $mixType $mixType $mixType $mixType $mixType $mixType $          (TypeHash 12571504671032409264)
instance (StableHashable a, StableHashable b, StableHashable c, StableHashable d, StableHashable e, StableHashable f, StableHashable g, StableHashable h, StableHashable i, StableHashable j, StableHashable k, StableHashable l)=> StableHashable (a, b, c, d, e, f, g, h, i, j, k, l) where
    typeHash = mixType $mixType $mixType $mixType $mixType $mixType $mixType $mixType $mixType $mixType $mixType $           (TypeHash 18057402240390888799)
instance (StableHashable a, StableHashable b, StableHashable c, StableHashable d, StableHashable e, StableHashable f, StableHashable g, StableHashable h, StableHashable i, StableHashable j, StableHashable k, StableHashable l, StableHashable m)=> StableHashable (a, b, c, d, e, f, g, h, i, j, k, l, m) where
    typeHash = mixType $mixType $mixType $mixType $mixType $mixType $mixType $mixType $mixType $mixType $mixType $mixType $            (TypeHash 1509508551579382043)
instance (StableHashable a, StableHashable b, StableHashable c, StableHashable d, StableHashable e, StableHashable f, StableHashable g, StableHashable h, StableHashable i, StableHashable j, StableHashable k, StableHashable l, StableHashable m, StableHashable n)=> StableHashable (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
    typeHash = mixType $mixType $mixType $mixType $mixType $mixType $mixType $mixType $mixType $mixType $mixType $mixType $mixType $             (TypeHash 17220521008040723494)
instance (StableHashable a, StableHashable b, StableHashable c, StableHashable d, StableHashable e, StableHashable f, StableHashable g, StableHashable h, StableHashable i, StableHashable j, StableHashable k, StableHashable l, StableHashable m, StableHashable n, StableHashable o)=> StableHashable (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
    typeHash = mixType $mixType $mixType $mixType $mixType $mixType $mixType $mixType $mixType $mixType $mixType $mixType $mixType $mixType $mixType $               (TypeHash 17803228377227705691)



-- WISHLIST:
--   - :: Word64 -> (Word32,Word32)  for 32-bit machines.

-- TODO PERFORMANCE:
--   consider the accursedUnutterablePerformIO bullshit from bytestring; shaves off 4ns here:

-- This is about twice as fast as a loop with single byte peeks:
hashByteString :: (HashState h)=> h -> B.ByteString -> h
{-# INLINE hashByteString #-}
hashByteString h = \(B.PS fp off lenBytes) -> unsafeDupablePerformIO $
      withForeignPtr fp $ \base ->
        let !bytesRem = lenBytes .&. 7  -- lenBytes `mod` 8
            -- index where we begin to read (bytesRem < 8) individual bytes:
            !bytesIx = off+lenBytes-bytesRem
            !ixFinal = off+lenBytes-1

            hash8ByteLoop !hAcc !ix 
                | ix == bytesIx = hashRemainingBytes hAcc bytesIx
                | otherwise     = assert (ix < bytesIx) $ do
                    w64Dirty <- peekByteOff base ix
                    let w64 = if littleEndian
                                then byteSwap64 w64Dirty
                                else w64Dirty

                    hash8ByteLoop (hAcc `mix64` w64) (ix + 8)
            
            hashRemainingBytes !hAcc !ix 
                | ix > ixFinal  = return hAcc 
                | otherwise     = assert (ix <= ixFinal) $ do
                    byt <- peekByteOff base ix
                    hashRemainingBytes (hAcc `mix8` byt) (ix+1)
        
         in hash8ByteLoop h off 


-- NOTE: we can't simply call hashByteArray here; Text is stored as
-- machine-endian UTF-16 (as promised by public Data.Text.Foreign), so we need
-- to read Word16 here in order to hash as Big-Endian UTF-16.
hashText :: (HashState h)=> h -> T.Text -> h
{-# INLINE hashText #-}
hashText h = \(T.Text (T.Array ba_) off lenWord16) -> 
    let ba = P.ByteArray ba_
        !word16sRem = lenWord16 .&. 3         -- lenWord16 `mod` 4
        -- index where we begin to read (word16sRem < 4) individual Word16s:
        !word16sIx = off+lenWord16-word16sRem
        !ixFinal = off+lenWord16-1

        hash4Word16sLoop !hAcc !ix 
            | ix == word16sIx = hashRemainingWord16s hAcc word16sIx
            | otherwise     = assert (ix < word16sIx) $
                -- CAREFUL: Word16s are stored in machine-endian, so we must
                -- read them out as Word16:
                let w0 = P.indexByteArray ba ix
                    w1 = P.indexByteArray ba (ix+1)
                    w2 = P.indexByteArray ba (ix+2)
                    w3 = P.indexByteArray ba (ix+3)
                 in hash4Word16sLoop (hAcc `mix16` w0 `mix16` w1 `mix16` w2 `mix16` w3) (ix + 4)
        
        hashRemainingWord16s !hAcc !ix 
            | ix > ixFinal  = hAcc 
            | otherwise     = assert (ix <= ixFinal) $
                let w0 = P.indexByteArray ba ix
                 in hashRemainingWord16s (hAcc `mix16` w0) (ix+1)
     in hash4Word16sLoop h off 

hashByteArray :: (HashState h)=> h -> Int -> P.ByteArray -> h
{-# INLINE hashByteArray #-}
hashByteArray h !lenBytes ba = 
    let !bytesRem = lenBytes .&. 7         -- lenBytes `mod` 8
        -- index where we begin to read (bytesRem < 8) individual bytes:
        !bytesIx = lenBytes-bytesRem
        !ixFinal = lenBytes-1
        -- bytesIx in elements of Word32:
        !bytesIxWd = bytesIx `unsafeShiftR` 3  -- `div` 8

        -- Index `ix` in terms of elements of Word32 or Word64, depending on
        -- WORD_SIZE_IN_BITS
        hash8ByteLoop !hAcc !ix 
            | ix == bytesIxWd = hashRemainingBytes hAcc bytesIx
            | otherwise     = assert (ix < bytesIxWd) $
                    let w64Dirty = P.indexByteArray ba ix
                        w64 = if littleEndian
                                then byteSwap64 w64Dirty
                                else w64Dirty

                     in hash8ByteLoop (hAcc `mix64` w64) (ix + 1)
        
        hashRemainingBytes !hAcc !ix 
            | ix > ixFinal  = hAcc 
            | otherwise     = assert (ix <= ixFinal) $
                let b0 = P.indexByteArray ba ix
                 in hashRemainingBytes (hAcc `mix8` b0) (ix+1)
     in hash8ByteLoop h 0 




---------------- LIST INSTANCE SCRATCH WORK:
-- 
-- We need to look at how inlining progresses and figure out a way to have our
-- list instance be optimal. See scratch work below.



-- TODO more different benchmarks of different types of lists, and ways of
-- constructing, and examine which of these two to use (and when):
--   We might be able to NOINLINE hashLeftUnfolded version (if performance
--   unaffected), and then re-write to hashFoldl' version based on argument
--   TODO :
--     or use our own rules so that we can get both fusion and unrolling?
--     (or would that not be helpful, since values already in a register?)

-- 7.10
--   APPLIED TO (take 250 $ iterate (+1) (1::Word8))  339.4 ns  !! MATCHING BASELINE
--   APPLIED TO ([1.. 250 :: Word8])                  1.766 μs
-- 7.8
--   APPLIED TO (take 250 $ iterate (+1) (1::Word8))  8.938 μs  -- NOTE: in general, 7.8 seems to do poorly applying folds to this in the context of criterion benchmarks
--   APPLIED TO ([1.. 250 :: Word8])                  846.5 ns
hashFoldl' :: (Hashable a, HashState h)=> h -> [a] -> h
-- hashFoldl' :: Word32 -> [Word8] -> Word32  -- NOTE: tested above w/ this monomorphic sig
{-# INLINE hashFoldl' #-}
hashFoldl' = foldl' (\h' a-> h' `hash` a)

-- 7.10
--   APPLIED TO ([1.. 250 :: Word8])                  675.6 ns
-- 7.8
--   APPLIED TO ([1.. 250 :: Word8])                  729.6 ns
hashLeftUnfolded :: (Hashable a, HashState h)=> h -> [a] -> h
-- hashLeftUnfolded :: Word32 -> [Word8] -> Word32  -- NOTE: tested above w/ this monomorphic sig
{-# INLINE hashLeftUnfolded #-}
hashLeftUnfolded = go
    where go !h [] = h
          -- This seems to be sweet spot on my machine:
          go !h (a1:a2:a3:a4:a5:a6:as) = go (h `hash` a1 `hash` a2 `hash` a3 `hash` a4 `hash` a5 `hash` a6) as
          go !h (a1:as) = go (h `hash` a1) as
