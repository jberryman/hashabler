{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash , UnliftedFFITypes #-}
module Data.Hashable.FNV1a 

-- ** Defining principled Hashable instances
{- | 
 Special care needs to be taken when defining instances of Hashable for your
 own types, especially for recursive types and types with multiple
 constructors. First instances need to ensure that *distinct values produce
 distinct hash values*. Here's an example of a *bad* implementation for 'Maybe':
 .
 > instance (Hashable a)=> Hashable (Maybe a) where              -- BAD!
 >     hash32WithSalt h (Just a) = h `hash32WithSalt` a          -- BAD!
 >     hash32WithSalt h Nothing  = h `hash32WithSalt` (1::Word8) -- BAD!
 .
 Here @Just (1::Word8)@ hashes to the same value as @Nothing@. TODO mention how we only can make those two assumptions about @a@ here
 .
 Second and more tricky, instances should not permit a function 
 @f :: a -> (a,a)@ such that 
 @x `hash` y == x `hash` y1 `hash` y2 where (y1,y2) = f y@... or something.
 The idea is we want to avoid the following kinds of collisions:
 .
 > hash [Just 1, Nothing] == hash [Just 1]     -- BAD!
 > hash ([1,2], [3])      == hash ([1], [2,3]  -- BAD!)
 .
 Maybe what we mean is that where @a@ is a 'Monoid', we expect replacing
 `mappend` with the hash operation to always yield *different* values. This
 needs clarifying; please help.
 .
 Here are a few rules of thumb which should result in principled instances for
 your own types (This is a work-in-progress; please help):
 .
 - If all values of a type have a static structure, i.e. the arrangement and
   number of child parts to be hashed is knowable from the type, then one may
   simply hash each child element of the type in turn. This is the case for
   product types like tuples (where the arity is reflected in the type), or
   primitive numeric values composed of a static number of bits.
 . 
 Otherwise if the type has variable structure, e.g. if it has multiple
 constructors or is an array type...
 .
 - Every possible value of a type should inject at least one byte of entropy
   *apart* from any recursive calls to child elements; we can ensure this is
   the case by hashing an initial or final distinct byte for each distinct
   constructor of our type
 .
 A final important note: we're not concerned with collisions between values of
 *different types*; in fact in many cases "equivalent" values of different
 types intentionally hash to the same value. This also means instances cannot
 rely on the hashing of child elements being uncorrelated. That might be one
 interpretation of the mistake in our faulty @Maybe@ instance above
 -}
 -- *** TODO notes on Generic deriving
    where


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

-- for reading the bytes of ByteStrings:
import System.IO.Unsafe (unsafeDupablePerformIO)

-- For getting our Int from ThreadId:
import Foreign.C (CInt(..))
import GHC.Conc(ThreadId(..))
import GHC.Prim(ThreadId#)

-- For TypeRep
import Data.Typeable
#if __GLASGOW_HASKELL__ >= 702
import Data.Typeable.Internal(TypeRep(..))
import GHC.Fingerprint.Type(Fingerprint(..))
#endif

import System.Mem.StableName
import Data.Ratio (Ratio, denominator, numerator)

-- For Integer:
#ifdef VERSION_integer_gmp
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
#include "MachDeps.h"
-- Do error just once, and assume 32 else 64 below:
#if WORD_SIZE_IN_BITS == 32
-- for fast div by power of two:
#define LOG_SIZEOF_WORD 2
#elif WORD_SIZE_IN_BITS == 64
#define LOG_SIZEOF_WORD 3
#else
#error We only know how to support 32-bit and 64-bit systems, sorry.
#endif


-- TODO BENCHMARKING for 'abs' see: http://graphics.stanford.edu/~seander/bithacks.html#IntegerAbs  and  http://stackoverflow.com/q/22445019/176841

foreign import ccall unsafe "rts_getThreadId" getThreadId :: ThreadId# -> CInt 


{-
-- see also the non-powers of two mapping methods outlined:
--  http://www.isthe.com/chongo/tech/comp/fnv/#FNV-1a

For test vectors:
    http://www.isthe.com/chongo/src/fnv/test_fnv.c
-}

-- TODO TESTING:
--  have a dir of test vectors, separated into files for each type
--  have a utility that both tests AND creates new vectors
--  when generating new, have it first run a check, and complain with a warning message about bumping the major version if they differ.
--
--  TODO TESTING HASH GOODNESS:
--    - do scatter plot distribution graph thing from arbitrary quickcheck values.
--    - compare with Hashable



-- FNV CONSTANTS ----------------------------------------------------

-- The special FNV primes required for different hash output sizes:

fnvPrime32 :: Word32
fnvPrime64 :: Word64
{-# INLINE fnvPrime32 #-}
{-# INLINE fnvPrime64 #-}
fnvPrime32 = 16777619
fnvPrime64 = 1099511628211
-- fnvPrime128 = 309485009821345068724781371
-- fnvPrime256 = 374144419156711147060143317175368453031918731002211

-- The arbitrary initial seed values for different output hash sizes. These
-- values are part of the spec, but there is nothing special about them;
-- supposedly, in terms of hash quality, any non-zero value seed should be
-- fine:

fnvOffsetBasis32 :: Word32
fnvOffsetBasis64 :: Word64
{-# INLINE fnvOffsetBasis32 #-}
{-# INLINE fnvOffsetBasis64 #-}
fnvOffsetBasis32 = 2166136261
fnvOffsetBasis64 = 14695981039346656037
-- fnvOffsetBasis128 = 144066263297769815596495629667062367629
-- fnvOffsetBasis256 = 100029257958052580907070968620625704837092796014241193945225284501741471925557

-- FNV HASH KERNELS -------------------------------------------------

infixl <#
(<#) :: Word32 -> Word8 -> Word32
{-# INLINE (<#) #-}
(<#) h32 b = (h32 `xor` fromIntegral b) * fnvPrime32

-- TODO 64-bit hashing




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

-- TODO benchmark on different arch's and make conditional (but expose both for
--      testing equivalence w/ quickcheck)
-- faster for 64-bit archs?
bytes64 :: Word64 -> (Word8,Word8,Word8,Word8,Word8,Word8,Word8,Word8)
{-# INLINE bytes64 #-}
bytes64 wd = ( shifted 56, shifted 48, shifted 40, shifted 32
             , shifted 24, shifted 16, shifted 8, fromIntegral wd)
     where shifted = fromIntegral . unsafeShiftR wd

-- faster for 32-bit archs?
bytes64_alt :: Word64 -> (Word8,Word8,Word8,Word8,Word8,Word8,Word8,Word8)
{-# INLINE bytes64_alt #-}
bytes64_alt wd = 
    let (wd0, wd1) = words32 wd
        (b0,b1,b2,b3) = bytes32 wd0
        (b4,b5,b6,b7) = bytes32 wd1
     in (b0,b1,b2,b3,b4,b5,b6,b7)

words32 :: Word64 -> (Word32, Word32)
{-# INLINE words32 #-}
words32 wd64 = (fromIntegral $ unsafeShiftR wd64 32, fromIntegral wd64)

-- These appear to return bytes in big endian on my machine (little endian),
-- but TODO verify what happens on a BE machine.

-- Get raw IEEE bytes from floating point types.
-- TODO better, if possible
bytesFloat :: Float -> (Word8,Word8,Word8,Word8)
{-# INLINE bytesFloat #-}
bytesFloat = bytes32 . floatToWord

bytesDouble :: Double -> (Word8,Word8,Word8,Word8,Word8,Word8,Word8,Word8)
{-# INLINE bytesDouble #-}
bytesDouble = bytes64_alt . doubleToWord
--bytesDouble = bytes64 . doubleToWord -- TODO conditional upon arch


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


-- TODO rename these methods:
--        - rename seed -> hash, and use a newtype wrapper
--          - make (<#) polymorphic for different size hash values
--             (<#) :: Hash h=> h -> Word8 -> h
--        - call it "combine" or "hashedWith" or "hasingIn"
--        - possibly use (<#) operator
--        - mention that we're "mixing right hand data into left-hand side hash"
--
-- TODO ENDIANNESS make a note and revisit instances: USE BIG ENDIAN (mostly because of network byte order)
--        - e.g. see Double and Float

-- | A class of types that can be converted into a hash value. For relevant
-- instances of primitive types, we expect 'hash32' and 'hash64' to produce
-- values following the FNV1a spec. We expect all other instances to display
-- "good" hashing properties (w/r/t avalanche, bit independence, etc.) where
-- "good" is only evidenced by our test suite, for now.
--
-- See the section "Defining Hashable instances" for details of what we expect
-- from instances.
class Hashable a where
    -- | Produce a 32-bit hash value using the supplied seed. The seed should
    -- be non-zero although this is not checked.
    hash32WithSalt :: Word32 -> a -> Word32

    -- TODO
    -- | Produce a 64-bit hash value using the supplied seed. The seed should
    -- be non-zero although this is not checked.
  --hash64WithSalt :: Word64 -> a -> Word64

    -- NOTE: these are just here so we can override them with faster
    -- implementations, and pre-computed bits.

    -- | Hash a value using the standard spec-prescribed 32-bit seed value.
    --
    -- > hash32 = hash32WithSalt 2166136261
    hash32 :: Hashable a=> a -> Word32
    {-# INLINE hash32 #-}
    hash32 a = hash32WithSalt fnvOffsetBasis32 a
    
    -- TODO
    -- hash64 :: Hashable a=> a -> Word64
  --{-# INLINE hash64 #-}
  --hash64 a = hash64WithSalt fnvOffsetBasis64 a


-- ------------------------------------------------------------------
-- NUMERIC TYPES:

-- TODO TESTING: make sure we test against many different serialized pos/neg/zero values here:
-- TODO TESTING: these should match Word/Word32/Word64 hash values, followed by a mixConstructor 0
-- TODO TESTING: for 7.8 and below, see if we can get a small value into J#, and then test that it hashes to the same as the literal small value
--                (look at code; simple */div or +/- don't seem to do it)

-- NOTE: non-obviously, but per our rule about variable-width values, this must
-- also be wrapped in a `mixConstructor`; consider the hashes of (0xDEAD,
-- 0xBEEF) and (0xDE, 0xADBEEF). The way we mix in the sign handles this.  -- TODO TESTING (this)
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
-- integer-gmp implementation: --------------------------------------
#if defined(VERSION_integer_gmp)
    hash32WithSalt seed i = case i of
      (S# n#) ->
        let magWord = fromIntegral $ abs (I# n#)
            sign = _signByte (I# n#)
         in mixConstructor sign $ 
#           if WORD_SIZE_IN_BITS == 32
              hash32WithSalt seed (magWord :: Word32)
#           else
              -- only hash enough 32-bit chunks as needed to represent magnitude
              _hash32SigWords32 seed magWord
#           endif

-- GHC 7.10: ------------------------
--
#   if MIN_VERSION_integer_gmp(1,0,0)
        -- NOTE: these used only when out of range of Int:
      (Jp# bn) -> mixConstructor 0 $ hash32BigNatBytes seed bn
      (Jn# bn) -> mixConstructor 1 $ hash32BigNatBytes seed bn

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
      (J# 0# _) -> mixConstructor 0 $ hash32WithSalt seed (0 :: Word32)
      (J# sz# ba#) -> 
         let numLimbs = abs (I# sz#)
             sign = _signByte (I# sz#)
          in mixConstructor sign $ 
               hash32BigNatByteArrayBytes seed numLimbs (P.ByteArray ba#)
#   endif

-- other Integer implementations: -----------------------------------
#else
    -- For non-gmp Integer; quite slow.
    hash32WithSalt = _hash32WithSaltInteger
#endif


-- TODO TESTING quickcheck against conditional
-- TODO benchmark against conditional
-- Helper to quickly (hopefully) extract sign bit (1 for negative, 0 otherwise)
-- from Int. Assumes two's complement.
_signByte :: Int -> Word8
{-# INLINE _signByte #-}
_signByte n = fromIntegral ((fromIntegral n :: Word) 
                              `unsafeShiftR` (WORD_SIZE_IN_BITS - 1))

-- Helper for hashing a 64-bit word, possibly omiting the first 32-bit chunk
-- (if 0). We use this when normalizing big natural representations.
_hash32SigWords32 :: Word32 -> Word64 -> Word32
{-# INLINE _hash32SigWords32 #-}
_hash32SigWords32 seed w64 = 
     let (word32_0, word32_1) = words32 w64
      in if word32_0 == 0 
          then hash32WithSalt seed word32_1
          else hash32WithSalt seed w64

-- TODO TESTING (LOTS! let quickcheck generate widths, and bit values directly)
-- Very slow Integer-implementation-agnostic hashing:
_hash32WithSaltInteger :: Word32 -> Integer -> Word32
_hash32WithSaltInteger h i = 
    let (sgn, limbs) = _integerWords i
     in mixConstructor sgn $ 
         foldl' hash32WithSalt h limbs

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


#ifdef VERSION_integer_gmp
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
hash32BigNatBytes :: Word32 -> BigNat -> Word32
{-# INLINE hash32BigNatBytes #-}
hash32BigNatBytes seed (BN# ba#) = 
    let ba = P.ByteArray ba#
        szBytes = P.sizeofByteArray ba
        numLimbs = szBytes `unsafeShiftR` LOG_SIZEOF_WORD
     in assert (numLimbs >= 1 && (numLimbs * SIZEOF_HSWORD) == szBytes) $
         hash32BigNatByteArrayBytes seed numLimbs ba


-- | The @BigNat@'s value is represented in 32-bit chunks (at least one, for
-- zero; but no more than necessary), then bytes are added to the hash from
-- most to least significant (including all initial padding 0s). Finally
-- @'mixConstructor' 0@ is called on the resulting hash value.
--
-- Exposed only in GHC 7.10.
instance Hashable BigNat where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed = mixConstructor 0 . hash32BigNatBytes seed

# endif


-- Hashing of internals of BigNat-format ByteArrays of at least 1 limb, for old
-- and new style Integer from integer-gmp.
hash32BigNatByteArrayBytes :: Word32 -> Int -> P.ByteArray -> Word32
{-# INLINE hash32BigNatByteArrayBytes #-}
hash32BigNatByteArrayBytes seed numLimbs ba = 
  assert (numLimbs > 0) $
    let mostSigLimbIx = numLimbs - 1
        -- NOTE: to correctly handle small-endian, we must read in Word-size
        -- chunks (not just Word32 size)
        go !h (-1) = h
        go !h !ix = let bytsBE = wordBytes $ P.indexByteArray ba ix
                     in go (hash32WithSalt h bytsBE) (ix - 1)
#  if WORD_SIZE_IN_BITS == 32
        wordBytes = bytes32
     in go seed mostSigLimbIx
#  else
        wordBytes = bytes64
        -- handle dropping possibly-empty most-significant Word32, before
        -- processing remaining limbs:
        h0 = let mostSigLimb = P.indexByteArray ba mostSigLimbIx
              in _hash32SigWords32 seed (fromIntegral mostSigLimb)
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
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed nat = case nat of
# if MIN_VERSION_integer_gmp(1,0,0)
        -- For Word-size natural
        (NatS# wd#) -> mixConstructor 0 $
#         if WORD_SIZE_IN_BITS == 32
            hash32WithSalt seed (W# wd#)
#         else
            _hash32SigWords32 seed (fromIntegral $ W# wd#)
#         endif
        -- Else using a BigNat (which instance calls required mixConstructor):
        (NatJ# bn)  -> hash32WithSalt seed bn
# else
        -- Natural represented with non-negative Integer:
        (Natural n) -> hash32WithSalt seed n
# endif

-- This is the instance in void-0.7:
--
-- | > hash32WithSalt _ _ = absurd
--
-- Exposed only in GHC 7.10
instance Hashable Void where
    hash32WithSalt _ = absurd

#endif


-- | Hash in numerator, then denominator.
instance (Integral a, Hashable a) => Hashable (Ratio a) where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt s a = s `hash32WithSalt` numerator a `hash32WithSalt` denominator a


-- ---------
-- Architecture-dependent types, with special handling.

-- | @Int@ has architecture-specific size. When hashing on 64-bit machines if
-- the @Int@ value to be hashed falls in the 32-bit Int range, we first cast it
-- to an Int32. This should help ensure that programs that are correct across
-- architectures will also produce the same hash values.
instance Hashable Int where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed i =
#     if WORD_SIZE_IN_BITS == 32
        hash32WithSalt seed (fromIntegral i :: Int32)
#     else
        _hash32WithSalt_Int_64 seed (fromIntegral i)
#     endif

-- | @Word@ has architecture-specific size. When hashing on 64-bit machines if
-- the @Word@ value to be hashed falls in the 32-bit Word range, we first cast
-- it to a Word32. This should help ensure that programs that are correct
-- across architectures will also produce the same hash values.
instance Hashable Word where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed w =
#     if WORD_SIZE_IN_BITS == 32
        hash32WithSalt seed (fromIntegral w :: Word32)
#     else
        _hash32WithSalt_Word_64 seed (fromIntegral w)
#     endif

-- we'll test these internals for equality in 32-bit Int range, against
-- instance for Int32. TODO TESTING

-- NOTE: the expressions in the conditionals alone make these quite slow on
--       32-bit machines, so don't worry about benchmarking this directly.
_hash32WithSalt_Int_64 :: Word32 -> Int64 -> Word32
{-# INLINE _hash32WithSalt_Int_64 #-}
_hash32WithSalt_Int_64 h = \i->
    -- Can we losslessly cast to 32-bit representation?
    if abs i <= (fromIntegral (maxBound :: Int32))
        then hash32WithSalt h (fromIntegral i :: Int32)
        else hash32WithSalt h i

_hash32WithSalt_Word_64 :: Word32 -> Word64 -> Word32
{-# INLINE _hash32WithSalt_Word_64 #-}
_hash32WithSalt_Word_64 h = \w->
    -- Can we losslessly cast to 32-bit representation?
    if w <= (fromIntegral (maxBound :: Word32))
        then hash32WithSalt h (fromIntegral w :: Word32)
        else hash32WithSalt h w



-- | Hash a Float as IEEE 754 single-precision format bytes. This is terribly
-- slow; direct complaints to http://hackage.haskell.org/trac/ghc/ticket/4092
instance Hashable Float where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed x = assert (isIEEE x) $
        hash32WithSalt seed $ bytesFloat x

-- | Hash a Double as IEEE 754 double-precision format bytes. This is terribly
-- slow; direct complaints to http://hackage.haskell.org/trac/ghc/ticket/4092
instance Hashable Double where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed x = assert (isIEEE x) $
        hash32WithSalt seed $ bytesDouble x


-- GHC uses two's complement representation for signed ints; C has this
-- undefined, I guess; just cast to Word and hash.

instance Hashable Int8 where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed i = hash32WithSalt seed (fromIntegral i :: Word8)

instance Hashable Int16 where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed i = hash32WithSalt seed (fromIntegral i :: Word16)

instance Hashable Int32 where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed i = hash32WithSalt seed (fromIntegral i :: Word32)

instance Hashable Int64 where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed i = hash32WithSalt seed (fromIntegral i :: Word64)

-- Straightforward hashing of different Words and byte arrays:

instance Hashable Word8 where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt = (<#)

instance Hashable Word16 where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed = hash32WithSalt seed . bytes16

instance Hashable Word32 where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed = hash32WithSalt seed . bytes32

instance Hashable Word64 where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed = hash32WithSalt seed . bytes64_alt    -- TODO CONDITIONAL on arch


-- ------------------------------------------------------------------
-- ARRAYS AND LIST:


-- Since below have variable-length, we'll use this helper (which is also
-- useful for multi-constructor types):

-- TODO rename, or maybe just remove?
mixConstructor :: Word8  -- ^ Constructor number. Recommend starting from 0 and incrementing.
               -> Word32 -- ^ Hash value TODO remove this comment, or clarify whether this should be applied first or last, or whether it matters.
               -> Word32 -- ^ New hash value
{-# INLINE mixConstructor #-}
mixConstructor n = (<# (0xFF - n))

-- | Strict @ByteString@
instance Hashable B.ByteString where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed = mixConstructor 0 .
        hashBytesUnrolled64 seed

-- TODO benchmarks for fusion:
-- | Lazy @ByteString@
instance Hashable BL.ByteString where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed = mixConstructor 0 .
        BL.foldlChunks hashBytesUnrolled64 seed

#if MIN_VERSION_bytestring(0,10,4)
-- | NOTE: hidden on bytestring < v0.10.4
instance Hashable BSh.ShortByteString where
    hash32WithSalt seed  = 
#   if MIN_VERSION_base(4,3,0)
      \(BSh.SBS ba_) ->
#   else
      \(BSh.SBS ba_ _) ->
#   endif
        let ba = P.ByteArray ba_
         in mixConstructor 0 $
              hashByteArray seed 0 (P.sizeofByteArray ba) ba
#endif

-- | Strict @Text@, hashed as big endian UTF-16.
instance Hashable T.Text where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed = mixConstructor 0 .
        hashText seed

-- TODO benchmarks for fusion:
-- | Lazy @Text@, hashed as big endian UTF-16.
instance Hashable TL.Text where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed = mixConstructor 0 .
        TL.foldlChunks hashText seed

-- | Here we hash each byte of the array in turn. If using this to hash some
-- data stored internally as a @ByteArray#@, be aware that depending on the
-- size and alignment requirements of that data, as well as the endianness of
-- your machine, this might result in different hash values across different
-- architectures.
instance Hashable P.ByteArray where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed = \ba-> mixConstructor 0 $
        hashByteArray seed 0 (P.sizeofByteArray ba) ba

-- ------------------------------------------------------------------
-- MISC THINGS:


-- TODO TESTING matches singleton Text instance (make sure to test double-wide)
--              also String.
--      TESTING that chars in range U+D800 to U+DFFF hash to different values

-- TODO look at core
-- | Hash a @Char@ as big endian UTF-16. Note that Char permits values in the
-- reserved unicode range U+D800 to U+DFFF; these Char values are added to the
-- hash just as if they were valid 16-bit characters.
instance Hashable Char where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed = go where
      -- adapted from Data.Text.Internal.Unsafe.Char.unsafeWrite:
    --go c | n .&. complement 0xFFFF == 0 =  -- TODO try this, etc.
      go c | n < 0x10000 =
               let (b0,b1) = bytes16 $ fromIntegral n
                in seed <# b0 <# b1

           | otherwise =
               let (b0,b1) = bytes16 lo
                   (b2,b3) = bytes16 hi
                in seed <# b0 <# b1 <# b2 <# b3

        where n = ord c
              m = n - 0x10000
              lo = fromIntegral $ (m `unsafeShiftR` 10) + 0xD800
              hi = fromIntegral $ (m .&. 0x3FF) + 0xDC00


instance Hashable ThreadId where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed = \(ThreadId tid)-> 
        hash32WithSalt seed (fromIntegral $ getThreadId tid :: Word)

instance Hashable TypeRep where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed = hash32WithSalt seed . typeRepInt32

-- TODO TEST IF THESE SEEM TO BE CONSISTENT ACROSS RUNS AND MACHINES (AND DIFFERENT VERSIONS OF BASE >= 7.2)
--      AND CONSIDER REMOVING SOME CONDITIONS.
typeRepInt32 :: TypeRep -> Int32
{-# INLINE typeRepInt32 #-}
typeRepInt32 = 
# if __GLASGOW_HASKELL__ >= 702
    -- Fingerprint is just the MD5, so taking any Int from it is fine
    \(TypeRep (Fingerprint i64 _) _ _) -> fromIntegral i64
# else
-- okay at least in base < 4.4 && >= 4:
-- THOUGH NOTE TODO: here the actual key may vary from run to run
    fromIntegral . unsafeDupablePerformIO . typeRepKey
# endif


-- | No promise of stability across runs or platforms. Implemented via hashStableName
instance Hashable (StableName a) where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed = hash32WithSalt seed . hashStableName
    


-- ------------------------------------------------------------------
-- ALGEBRAIC DATA TYPES:


-- ---------
-- Sum types

-- TODO REVISIT
-- | 'True' hashes to @'hash32' (1::Word8)@, 'False' hashes to @'hash32' (0::Word8)@
-- TODO or use fromBool/toBool from Foreign.Marshal.Utils (why?)
instance Hashable Bool where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed b
       | b         = hash32WithSalt seed (1::Word8) 
       | otherwise = hash32WithSalt seed (0::Word8) -- TODO we could omit the xor here.

    -- TODO: actually is it even useful to have these static implementations?
    -- These small-universe types are not going to be useful at the top level,
    -- only as leaves or components of products. TODO REMOVE FROM CLASS
    {-# INLINE hash32 #-}
    hash32 b = 
        let h = if b then 67918732 else 84696351
         in assert (h == hash32WithSalt fnvOffsetBasis32 b) h


instance Hashable Ordering where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed = flip mixConstructor seed . fromIntegral . fromEnum

instance Hashable a => Hashable [a] where
    -- TODO OPTIMIZE (see notes below)
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed = mixConstructor 0 .
        hashFoldl' seed

instance Hashable a => Hashable (Maybe a) where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed Nothing  = mixConstructor 0 seed
    hash32WithSalt seed (Just a) = mixConstructor 1 $ hash32WithSalt seed a
        
instance (Hashable a, Hashable b) => Hashable (Either a b) where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed = either (h 0) (h 1) where
        h n = mixConstructor n . hash32WithSalt seed


-- ---------
-- Tuples (product types)

-- Per our rules, this must perturb the hash value by at least a byte, even
-- though its value is entirely "fixed" by its type. Consider [()]; the
-- instance relies on () following the rule.
-- | > hash32WithSalt = const . mixConstructor 0
instance Hashable () where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt = const . mixConstructor 0

instance (Hashable a1, Hashable a2) => Hashable (a1, a2) where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed (a,b) = seed `hash32WithSalt` a `hash32WithSalt` b
    
instance (Hashable a1, Hashable a2, Hashable a3) => Hashable (a1, a2, a3) where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed (a,b,c) = seed `hash32WithSalt` a `hash32WithSalt` b `hash32WithSalt` c

instance (Hashable a1, Hashable a2, Hashable a3, Hashable a4) => Hashable (a1, a2, a3, a4) where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed (a,b,c,d) = seed `hash32WithSalt` a `hash32WithSalt` b `hash32WithSalt` c `hash32WithSalt` d

instance (Hashable a1, Hashable a2, Hashable a3, Hashable a4, Hashable a5) => Hashable (a1, a2, a3, a4, a5) where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed (a,b,c,d,e) = seed `hash32WithSalt` a `hash32WithSalt` b `hash32WithSalt` c `hash32WithSalt` d `hash32WithSalt` e

instance (Hashable a1, Hashable a2, Hashable a3, Hashable a4, Hashable a5, Hashable a6) => Hashable (a1, a2, a3, a4, a5, a6) where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed (a,b,c,d,e,f) = seed `hash32WithSalt` a `hash32WithSalt` b `hash32WithSalt` c `hash32WithSalt` d `hash32WithSalt` e `hash32WithSalt` f

instance (Hashable a1, Hashable a2, Hashable a3, Hashable a4, Hashable a5, Hashable a6, Hashable a7) => Hashable (a1, a2, a3, a4, a5, a6, a7) where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed (a,b,c,d,e,f,g) = seed `hash32WithSalt` a `hash32WithSalt` b `hash32WithSalt` c `hash32WithSalt` d `hash32WithSalt` e `hash32WithSalt` f `hash32WithSalt` g

instance (Hashable a1, Hashable a2, Hashable a3, Hashable a4, Hashable a5, Hashable a6, Hashable a7, Hashable a8) => Hashable (a1, a2, a3, a4, a5, a6, a7, a8) where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed (a,b,c,d,e,f,g,h) = seed `hash32WithSalt` a `hash32WithSalt` b `hash32WithSalt` c `hash32WithSalt` d `hash32WithSalt` e `hash32WithSalt` f `hash32WithSalt` g `hash32WithSalt` h

-- NOTES:
--   - no overhead from fromIntegral
--              1 iteration  =  9.37 (+0)
--              4 iterations = 12.66 (+3.3)  0.8
--              8 iterations = 17.83 (+8.5)  1.1
--   - baseline for id = 9.494
--   - using Word instead of Word32 explicitly doesn't matter
--   - using all Int and no fromIntegrals doesn't matter
--   - Compiling w/ LLVM was no better.
--   - using unsafeShiftR to get bytes is fast and probably our best choice
--   - largeword is much too slow to be usable
--   - 64-bit multiplication is slow on 32-bit (overhead of ~10ns)
--   - bytes64 is very slow on 32-bit
--   - TODO we could use SIMD vectors for wider than 32 hash bits!
--      - but need to test that this is principled.
--      - We don't have xor exposed in SIMD primops; instead try to target LLVM autovectorization?: http://llvm.org/docs/Vectorizers.html
--
-- TESTING NOTES:
--   - test that Generic instances derived from identically-shaped types match in every way behavior of instances defined here
--   - variable-width & sum types:
--     - test in an automated way [[1,2],[3]] vs [[1]],[2,3]] issue
--     - somehow test problem of marker bits added to *end* i.e. 
--   - quickcheck equivalence of "array-like" instances, as far as possible.
--   - cross-architecture compatibility (against serialized random output from quickcheck)
--   - avalanche etc. properties for both primitive and compount instances
--
-- TODO IMPLEMENTATION:
--   - hash functions with user's seed (we pass this in recursively on e.g. list)
--   - 32- and 64-bit hashes (other lengths are impractical)
--   - arbitrary-length hashes with user-supplied seed
--     - possibly both 32 and 64-bit chunks, for speed on different arches
--     - possibly using SIMD vectors!
--     - NOTE: we need to check seed avalance here.
--   - documentation:
--     - versioning
--     - expectations of randomness
--     - expectation of permanence (re. to serializability)
--     - performance concerns
--
-- WISHLIST:
--   - :: Word64 -> (Word32,Word32)  for 32-bit machines.
--
-- CROSS-PLATFORM NOTES:
--   - Int is a bit weird; it's like Word split and wrapped around
--     - on 64-bit if the first 32 bits of a Word are 1s (it's negative) or all zeros, then we can truncate it to 32 smallest bits (fromIntegral does this conversion both ways)
--       - probably do this with fromIntegral (what is the safe way to check that the conversion is lossless?)
--       - see also: http://stackoverflow.com/questions/15047191/read-write-haskell-integer-in-twos-complement-representation
--     - however if we're trying to be compatible with OTHER IMPLEMENTATIONS, it's not clear we can/should do that
--     - endianness:
--       - the draft here: http://tools.ietf.org/html/draft-eastlake-fnv-08#page-5 ...
--         suggests doing arithmetic in little endian (which is nice for us on intel) so we should be able to do conversions on others
--       - can use byteSwap* functions from Data.Word (available on base 4.7 and above)
--       - we can use ByteString builder to build bytestring from various endiannesses: http://hackage.haskell.org/package/bytestring-0.10.4.1/docs/Data-ByteString-Builder.html#g:4
--          TODO + Benchmark builder with Int
--                  - super slow (200ns)
--               + test Serialized from ghc package (ghc api)
--                  - super slow (~300ns to serialize a Word)
--               x along with SmallByteString (and somehow writing Ints efficiently)
--                  - No reason to serialize things to bytestring; just hash as we go.
--               + Benchmark largeword multpilication and xor
--                  - fromIntegral is very slow; multiplication is outrageously slow
--               + How do 64-bits fare on 32-bit machines?
--               + Are there other choices of basis we can use to extend beyond 32 bit hashes?
--                 - YES! any non-zero offset basis ought to be fine, in terms of hashing.
--                 - but note: if we want cross-platform compatible, then we'll need to use their prescribed bases.
--
-- QUESTIONS:
--   - principled way to test for lossless convertable Ints / Words (maybe look at impl of fromInteger)
--   + fastest way to get access to bytes of integer types

---------------- LIST INSTANCE SCRATCH WORK:


-- testing. TODO NOTE: we're omiting handling of empty vs. full list, so these aren't valid yet! ------

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
hashFoldl' :: Hashable a=> Word32 -> [a] -> Word32
-- hashFoldl' :: Word32 -> [Word8] -> Word32  -- NOTE: tested above w/ this monomorphic sig
{-# INLINE hashFoldl' #-}
hashFoldl' = foldl' (\h' a-> h' `hash32WithSalt` a)

-- 7.10
--   APPLIED TO ([1.. 250 :: Word8])                  675.6 ns
-- 7.8
--   APPLIED TO ([1.. 250 :: Word8])                  729.6 ns
hashLeftUnfolded :: Hashable a=> Word32 -> [a] -> Word32
-- hashLeftUnfolded :: Word32 -> [Word8] -> Word32  -- NOTE: tested above w/ this monomorphic sig
{-# INLINE hashLeftUnfolded #-}
hashLeftUnfolded = go
    where go !h [] = h
          -- This seems to be sweet spot on my machine:
          go !h (a1:a2:a3:a4:a5:a6:as) = go (h `hash32WithSalt` a1 `hash32WithSalt` a2 `hash32WithSalt` a3 `hash32WithSalt` a4 `hash32WithSalt` a5 `hash32WithSalt` a6) as
          go !h (a1:as) = go (h `hash32WithSalt` a1) as


-- benchmark fetching bytes from bytestring in different ways -----------
-- TODO TESTING make sure to test on bytestrings where off /= 0


-- TODO Factor out common code here:


-- This is about twice as fast as a loop with single byte peeks:
hashBytesUnrolled64 :: Word32 -> B.ByteString -> Word32
{-# INLINE hashBytesUnrolled64 #-}
hashBytesUnrolled64 h = \(B.PS fp off lenBytes) -> unsafeDupablePerformIO $
      withForeignPtr fp $ \base ->
        let !bytesRem = lenBytes .&. 7  -- lenBytes `mod` 8
            -- index where we begin to read (bytesRem < 8) individual bytes:
            !bytesIx = off+lenBytes-bytesRem
            !ixFinal = off+lenBytes-1

            hash8ByteLoop !hAcc !ix 
                | ix == bytesIx = hashRemainingBytes hAcc bytesIx
                | otherwise     = assert (ix < bytesIx) $ do
                    b0 <- peekByteOff base ix
                    b1 <- peekByteOff base (ix+1)
                    b2 <- peekByteOff base (ix+2)
                    b3 <- peekByteOff base (ix+3)
                    b4 <- peekByteOff base (ix+4)
                    b5 <- peekByteOff base (ix+5)
                    b6 <- peekByteOff base (ix+6)
                    b7 <- peekByteOff base (ix+7)
                    hash8ByteLoop (hAcc <# b0 <# b1 <# b2 <# b3 <# b4 <# b5 <# b6 <# b7) (ix + 8)
            
            -- TODO we could unroll this for [0..7]
            hashRemainingBytes !hAcc !ix 
                | ix > ixFinal  = return hAcc 
                | otherwise     = assert (ix <= ixFinal) $ do
                    byt <- peekByteOff base ix
                    hashRemainingBytes (hAcc <# byt) (ix+1)
        
         in hash8ByteLoop h off 


-- TODO WE NEED TO HANDLE ENDIAN-NESS! IF WE FETCH BYTE-16s DOES THAT WORK?
--       see Data.Text.Foreign
-- TODO TESTING, quickcheck against hashBytesUnrolled64
--      TESTING, make sure we use characters of variable width. 
--      TESTING, test: hash t == hash (encodeUtf16BE t)  -- TODO or use big endian?

-- NOTE: we can't simply call hashByteArray here; Text is stored as
-- machine-endian UTF-16 (as promised by public Data.Text.Foreign), so we need
-- to read Word16 here in order to hash as Big-Endian UTF-16.
hashText :: Word32 -> T.Text -> Word32
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
                let (b0,b1) = bytes16 $ P.indexByteArray ba ix
                    (b2,b3) = bytes16 $ P.indexByteArray ba (ix+1)
                    (b4,b5) = bytes16 $ P.indexByteArray ba (ix+2)
                    (b6,b7) = bytes16 $ P.indexByteArray ba (ix+3)
                 in hash4Word16sLoop (hAcc <# b0 <# b1 <# b2 <# b3 <# b4 <# b5 <# b6 <# b7) (ix + 4)
        
        -- TODO we could unroll this for [0..3]
        hashRemainingWord16s !hAcc !ix 
            | ix > ixFinal  = hAcc 
            | otherwise     = assert (ix <= ixFinal) $
                let (b0,b1) = bytes16 $ P.indexByteArray ba ix
                 in hashRemainingWord16s (hAcc <# b0 <# b1) (ix+1)
     in hash4Word16sLoop h off 


hashByteArray :: Word32 -> Int -> Int -> P.ByteArray -> Word32
{-# INLINE hashByteArray #-}
hashByteArray h !off !lenBytes ba = 
    let !bytesRem = lenBytes .&. 7         -- lenBytes `mod` 8
        -- index where we begin to read (bytesRem < 8) individual bytes:
        !bytesIx = off+lenBytes-bytesRem
        !ixFinal = off+lenBytes-1

        hash8ByteLoop !hAcc !ix 
            | ix == bytesIx = hashRemainingBytes hAcc bytesIx
            | otherwise     = assert (ix < bytesIx) $
                let b0 = P.indexByteArray ba ix
                    b1 = P.indexByteArray ba (ix+1)
                    b2 = P.indexByteArray ba (ix+2)
                    b3 = P.indexByteArray ba (ix+3)
                    b4 = P.indexByteArray ba (ix+4)
                    b5 = P.indexByteArray ba (ix+5)
                    b6 = P.indexByteArray ba (ix+6)
                    b7 = P.indexByteArray ba (ix+7)
                 in hash8ByteLoop (hAcc <# b0 <# b1 <# b2 <# b3 <# b4 <# b5 <# b6 <# b7) (ix + 8)
        
        -- TODO we could unroll this for [0..7]
        hashRemainingBytes !hAcc !ix 
            | ix > ixFinal  = hAcc 
            | otherwise     = assert (ix <= ixFinal) $
                let b0 = P.indexByteArray ba ix
                 in hashRemainingBytes (hAcc <# b0) (ix+1)
     in hash8ByteLoop h off 
