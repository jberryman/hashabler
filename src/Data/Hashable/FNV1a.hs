{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}  -- TEMPORARY
module Data.Hashable.FNV1a 
    where
    
import Data.Word
import Data.Int
import Data.Bits
import Data.Char
import Control.Exception(assert)
import PrimUtilities

-- TEMPORARY ------------
-- For casting of floating point values:
import Data.Word (Word32, Word64)
import Data.Array.ST (newArray, readArray, MArray, STUArray)
import Data.Array.Unsafe (castSTUArray)
import GHC.ST (runST, ST)

-- These are slow as shit (at least 10ns overhead)
floatToWord :: Float -> Word32
floatToWord x = runST (cast x)

doubleToWord :: Double -> Word64
doubleToWord x = runST (cast x)

{-# INLINE cast #-}
cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0
-- TEMPORARY ------------


{-
-- see also the non-powers of two mapping methods outlined:
--  http://www.isthe.com/chongo/tech/comp/fnv/#FNV-1a

For test vectors:
    http://www.isthe.com/chongo/src/fnv/test_fnv.c
-}

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
-- supposedly any non-zero value seed should be fine:

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


-- UNROLLED 32-BIT HASHING OF DIFFERENT TYPES: ----------------------

-- TODO BENCHMARK hash32WithSalt against these and probably remove:
hash32Word16 :: Word16 -> Word32
{-# INLINE hash32Word16 #-}
hash32Word16 wd = case (fromIntegral $ unsafeShiftR wd 8, fromIntegral wd) of 
  (b0,b1)->
    fnvOffsetBasis32 <# b0 <# b1

hash32Word32 :: Word32 -> Word32
{-# INLINE hash32Word32 #-}
hash32Word32 wd = case bytes32 wd of 
  (b0,b1,b2,b3)->
    fnvOffsetBasis32 <# b0 <# b1 <# b2 <# b3

hash32Word64 :: Word64 -> Word32
{-# INLINE hash32Word64 #-}
hash32Word64 wd = case bytes64_alt wd of
-- fnvInnerLoopTestWord64 wd = case bytes64 wd of  -- NOTE: SLOW ON 32-bit arch
  (b0,b1,b2,b3,b4,b5,b6,b7) ->
    fnvOffsetBasis32 <# b0 <# b1 <# b2 <# b3 <# b4 <# b5 <# b6 <# b7


-- EXTRACTING BYTES FROM DIFFERENT TYPES ----------------------------
-- NOTE we're to hash the resulting Word8s from left to right

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
    let wd0 = fromIntegral $ unsafeShiftR wd 32
        wd1 = fromIntegral wd
        (b0,b1,b2,b3) = bytes32 wd0
        (b4,b5,b6,b7) = bytes32 wd1
     in (b0,b1,b2,b3,b4,b5,b6,b7)


-- Get raw IEEE bytes from floating point types. What a horror show...
-- TODO better, if possible
bytesFloat :: Float -> (Word8,Word8,Word8,Word8)
bytesFloat fl = 
    case decodeFloat_Int fl of
         (man,ex) -> 
            let expWordShifted :: Word32
                expWordShifted = unsafeShiftL (fromIntegral (ex+150)) 23 -- TODO why 150?
                -- expWordShifted = unsafeShiftL (fromIntegral (ex+127)) 23 

                manWord :: Word32
                manWord = fromIntegral man
                -- NOT QUITE! we need to convert from two's complement to signed
                --    how? with a shiftL or something??

                

                ieeeFloat = manWord .|. expWordShifted
                
             in bytes32 ieeeFloat
                {-
                assert (man is max representable in 23 bits) -- or
                assert (bits 23 - 30 of manWord are all 0) -- and/or
                assert (ex+127 >= 0)
                assert (ieeeFloat == manWord `xor` expWordShifted) -- this might sum up above better
                -}

bytesFloatGood :: Float ->  (Word8,Word8,Word8,Word8)
bytesFloatGood = bytes32 . floatToWord
--01001011 00100000
--01001011 10100000 -- off by one!
-- 1001011 00000000000000000000  -- With the one coming from man!!
--                               -- Do we need to just unset that bit?? (and assert it's always set?)

-- http://graphics.stanford.edu/~seander/bithacks.html#IntegerAbs
twosComp2SignedMag :: Int -> Word32
{-# INLINE twosComp2SignedMag #-}
twosComp2SignedMag x = 
    -- TODO test, and also look at core here; see if we can/should factor out
    let v = fromIntegral x 
     in ((v + (v `unsafeShiftR` 31)) `xor` (v `unsafeShiftR` 31)) .|. (v .&. 0x80000000);


-- HASHABLE CLASS AND INSTANCES -------------------------------------


-- | A class of types that can be converted into a hash value. For relevant
-- instances of primitive types, we expect 'hash32' and 'hash64' to produce
-- values following the FNV1a spec. We expect all other instances to display
-- "good" hashing properties (w/r/t avalanche, bit indepepndence, etc.) where
-- "good" is only evidenced by our test suite, for now.
-- TODO revise this in light of what we actual get from test vectors.
class Hashable a where
    -- | Produce a 32-bit hash value using the supplied seed. The seed should
    -- be non-zero although this is not checked.
    hash32WithSalt :: Word32 -> a -> Word32

    -- TODO
    -- | Produce a 64-bit hash value using the supplied seed. The seed should
    -- be non-zero although this is not checked.
    -- hash64WithSalt :: Word64 -> a -> Word64

    -- NOTE: these are just here so we can override them with faster
    -- implementations, and pre-computed bits.

    -- | Hash a value using the standard spec-prescribed 32-bit seed value.
    --
    -- > hash32 = hash32WithSalt 2166136261
    hash32 :: Hashable a=> a -> Word32
    {-# INLINE hash32 #-}
    hash32 = hash32WithSalt fnvOffsetBasis32
    
    -- TODO
    -- hash64 :: Hashable a=> a -> Word64
    -- {-# INLINE hash64 #-}
    -- hash64 = hash64WithSalt fnvOffsetBasis64

-- ---------
-- Instances that ought to match the test vectors from the spec!



-- | 'True' hashes to @'hash32' (1::Word8)@, 'False' hashes to @'hash32' (0::Word8)@
-- TODO or use fromBool/toBool from Foreign.Marshal.Utils (why?)
instance Hashable Bool where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed b
       | b         = hash32WithSalt seed (1::Word8) 
       | otherwise = hash32WithSalt seed (0::Word8)

    {-# INLINE hash32 #-}
    hash32 b = 
        let h = if b then 67918732 else 84696351
         in assert (h == hash32WithSalt fnvOffsetBasis32 b) h

-- ---------
-- Architecture-dependent types, with special handling.
instance Hashable Int where
instance Hashable Word where

                             -- "values represent Unicode (or equivalently ISO/IEC 10646) characters..."
instance Hashable Char where -- maxbound is 1114111 (~21 bits), `ord :: Char -> Int`
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed = hash32WithSalt seed . ord
    -- TODO pre-hash all but last 3 bytes.


-- TODO use decodeFloat_ / decodeDouble_ from Prim to extract bits?
--      or decodeFloat from RealFloat
--         good for Float (calls decodeFloat_Int)
--         BAD for Double (I think)
--      or toRational, then convert Integers?

-- try to match IEEE single-precision type hash?
instance Hashable Float where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed x = assert (isIEEE x) undefined
-- try to match IEEE double-precision type hash?
instance Hashable Double where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed x = assert (isIEEE x) undefined


-- GHC uses two's complement representation for signed ints; C has this
-- undefined; just cast to Word and hash.

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
    hash32WithSalt seed wd = case (fromIntegral $ unsafeShiftR wd 8, fromIntegral wd) of 
      (b0,b1)->
        seed <# b0 <# b1

instance Hashable Word32 where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed wd = case bytes32 wd of 
      (b0,b1,b2,b3)->
        seed <# b0 <# b1 <# b2 <# b3

instance Hashable Word64 where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed wd = case bytes64_alt wd of
 -- hash32WithSalt seed wd = case bytes64 wd of     -- TODO CONDITIONAL on arch
      (b0,b1,b2,b3,b4,b5,b6,b7) ->
        seed <# b0 <# b1 <# b2 <# b3 <# b4 <# b5 <# b6 <# b7
{-
instance Hashable S.ByteString where
instance Hashable L.ByteString where
instance Hashable ShortByteString where
instance Hashable S.Text where
instance Hashable L.Text where

-- ---------
-- These ought to hash in some reasonable and good way. Recursive instances
-- ought to be defined in terms of hash*WithSalt on subterms; this should
-- ensure we don't get e.g. hash [[1],[2,3]] == hash [[1,2],[3]].

instance Hashable Integer where
    -- GHC.Integer.GMP.Internals from integer-gmp (part of GHC distribution)
instance (Integral a, Hashable a) => Hashable (Ratio a) where
instance Hashable Ordering where
instance Hashable () where
instance Hashable ThreadId where
instance Hashable TypeRep where
instance Hashable (StableName a) where

instance Hashable a => Hashable [a] where
instance Hashable a => Hashable (Maybe a) where
instance (Hashable a, Hashable b) => Hashable (Either a b) where
instance (Hashable a1, Hashable a2) => Hashable (a1, a2) where
instance (Hashable a1, Hashable a2, Hashable a3) => Hashable (a1, a2, a3) where
instance (Hashable a1, Hashable a2, Hashable a3, Hashable a4) => Hashable (a1, a2, a3, a4) where
instance (Hashable a1, Hashable a2, Hashable a3, Hashable a4, Hashable a5) => Hashable (a1, a2, a3, a4, a5) where
instance (Hashable a1, Hashable a2, Hashable a3, Hashable a4, Hashable a5, Hashable a6) => Hashable (a1, a2, a3, a4, a5, a6) where
instance (Hashable a1, Hashable a2, Hashable a3, Hashable a4, Hashable a5, Hashable a6, Hashable a7) => Hashable (a1, a2, a3, a4, a5, a6, a where
-}

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
