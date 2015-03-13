{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Hashable.FNV1a 
    where
    
import Data.Word
import Data.Int
import Data.Bits
import Data.Char
import Data.List
import Control.Exception(assert)

-- For casting of floating point values:
import Data.Word (Word32, Word64)
import Data.Array.ST (newArray, readArray, MArray, STUArray)
import Data.Array.Unsafe (castSTUArray)
import GHC.ST (runST, ST)


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
floatToWord x = runST (cast x)

doubleToWord :: Double -> Word64
doubleToWord x = runST (cast x)

cast :: (MArray (STUArray s) a (ST s),
         MArray (STUArray s) b (ST s)) => a -> ST s b
{-# INLINE cast #-}
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0


-- HASHABLE CLASS AND INSTANCES -------------------------------------


-- TODO MAYBE rename these methods:
--        - possibly use (<#) operator
--        - mention that we're "mixing right hand data into left-hand side hash"

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

-- TODO OR USE TUPLE INSTANCES HERE?

-- | Hash a Float as IEEE 754 single-precision format bytes. This is terribly
-- slow; complain here: http://hackage.haskell.org/trac/ghc/ticket/4092
instance Hashable Float where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed x = assert (isIEEE x) $
     case bytesFloat x of 
          (b0,b1,b2,b3)->
            seed <# b0 <# b1 <# b2 <# b3

-- | Hash a Double as IEEE 754 double-precision format bytes. This is terribly
-- slow; complain here: http://hackage.haskell.org/trac/ghc/ticket/4092
instance Hashable Double where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed x = assert (isIEEE x) $
     case bytesDouble x of 
          (b0,b1,b2,b3,b4,b5,b6,b7) ->
            seed <# b0 <# b1 <# b2 <# b3 <# b4 <# b5 <# b6 <# b7


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

-- TODO OR USE TUPLE INSTANCES HERE?
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
-}

-- TUPLES:
-- TODO hmmmm, but we'd probably like tuples of Word8 to simply hash up the words together, no?
--      if so, then do we need the extra method trick?
--      OR: - add a method:
--              combine :: Hashable a=> Hash32 -> a -> Hash32
--              -... wait... do we even need that?
--              if not TODO rename seed -> "hash" and add newtype wrapper.
--          - and/or create a newtype wrapper for hash values
--
-- NOTES FOR DOCUMENTATION
--   sum types must mix in a byte indicating the constructor
--     this should be numbered from 1 ascending (but what about > 255 constructor types??)
--   variable-length types (arrays) should also be "completed" with a byte
--   TODO can this safely come at the end? I THINK SO
--        but what about recursive sum types (lists!) ?
--          we don't want (I think) to hash an extra bite at each (:) level! So what's the rule?

instance (Hashable a1, Hashable a2) => Hashable (a1, a2) where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed (a,b) = seed `hash32WithSalt` a `hash32WithSalt` b
    -- TODO: SHIT! but what if a and b are, e.g. [Word8]
    --       we get into false identical hashes!
    
{-
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
-- TESTING NOTES:
--   - test that Generic instances derived from identically-shaped types match in every way behavior of instances defined here
--   - variable-width & sum types:
--     - test in an automated way [[1,2],[3]] vs [[1]],[2,3]] issue
--     - somehow test problem of marker bits added to *end* i.e. 
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
-- overhead of doing an extra (* prime) at each recursive call:
-- hash32WithSaltExtra :: Word32 -> Word8 -> Word32   -- NO DIFFERENT
hash32WithSaltExtra :: (Hashable a)=> Word32 -> a -> Word32
{-# INLINE hash32WithSaltExtra #-}
hash32WithSaltExtra h a = (h * fnvPrime32) `hash32WithSalt` a

-- testing. NOTE: we're omiting handling of empty vs. full list
hashFoldl', hashFoldl'Extra, hashFoldr, hashFoldrExtra, hashLeftUnfolded, hashLeftUnfoldedExtra :: Word32 -> [Word8] -> Word32
{-# INLINE hashFoldr #-}
{-# INLINE hashFoldrExtra #-}
hashFoldr = foldr (\a h'-> h' `hash32WithSalt` a) -- NOTE: hashing backwards
hashFoldrExtra = foldr (\a h'-> h' `hash32WithSaltExtra` a) -- NOTE: hashing backwards


-- INSERTING A MULTIPLY BETWEEN EACH IS ~ 68% SLOWER
{-# INLINE hashFoldl'Extra #-}
hashFoldl'Extra = foldl' (\h' a-> h' `hash32WithSaltExtra` a) 

-- USE THIS VERSION:
{-# INLINE hashFoldl' #-}
hashFoldl' = foldl' (\h' a-> h' `hash32WithSalt` a)

-- ELSE IF NO FUSION HAPPENS (TODO VERIFY THIS IS WHY ABOVE FAST) THEN REWRITE TO THIS VERSION:
-- This is much faster:
hashLeftUnfolded = go
    where go !h [] = h
          -- go !h (a1:a2:a3:a4:a5:a6:a7:a8:as) = go (h `hash32WithSalt` a1 `hash32WithSalt` a2 `hash32WithSalt` a3 `hash32WithSalt` a4 `hash32WithSalt` a5 `hash32WithSalt` a6 `hash32WithSalt` a7 `hash32WithSalt` a8) as
          -- go !h (a1:a2:a3:a4:a5:a6:a7:as) = go (h `hash32WithSalt` a1 `hash32WithSalt` a2 `hash32WithSalt` a3 `hash32WithSalt` a4 `hash32WithSalt` a5 `hash32WithSalt` a6 `hash32WithSalt` a7) as
          -- This seems to be sweet spot on my machine:
          go !h (a1:a2:a3:a4:a5:a6:as) = go (h `hash32WithSalt` a1 `hash32WithSalt` a2 `hash32WithSalt` a3 `hash32WithSalt` a4 `hash32WithSalt` a5 `hash32WithSalt` a6) as
          go !h (a1:a2:a3:a4:a5:as) = go (h `hash32WithSalt` a1 `hash32WithSalt` a2 `hash32WithSalt` a3 `hash32WithSalt` a4 `hash32WithSalt` a5) as
          go !h (a1:a2:a3:a4:as) = go (h `hash32WithSalt` a1 `hash32WithSalt` a2 `hash32WithSalt` a3 `hash32WithSalt` a4) as
          go !h (a1:a2:a3:as) = go (h `hash32WithSalt` a1 `hash32WithSalt` a2 `hash32WithSalt` a3) as
          go !h (a1:a2:as) = go (h `hash32WithSalt` a1 `hash32WithSalt` a2) as
          go !h (a1:as) = go (h `hash32WithSalt` a1) as


-- This is around 10% slower (which might just be alright!)
hashLeftUnfoldedExtra = go
    where go !h [] = h
          -- go !h (a1:a2:a3:a4:a5:a6:a7:a8:as) = go (h `hash32WithSalt` a1 `hash32WithSalt` a2 `hash32WithSalt` a3 `hash32WithSalt` a4 `hash32WithSalt` a5 `hash32WithSalt` a6 `hash32WithSalt` a7 `hash32WithSalt` a8) as
          -- go !h (a1:a2:a3:a4:a5:a6:a7:as) = go (h `hash32WithSalt` a1 `hash32WithSalt` a2 `hash32WithSalt` a3 `hash32WithSalt` a4 `hash32WithSalt` a5 `hash32WithSalt` a6 `hash32WithSalt` a7) as
          -- This seems to be sweet spot on my machine:
          go !h (a1:a2:a3:a4:a5:a6:as) = go (h `hash32WithSaltExtra` a1 `hash32WithSaltExtra` a2 `hash32WithSaltExtra` a3 `hash32WithSaltExtra` a4 `hash32WithSaltExtra` a5 `hash32WithSaltExtra` a6) as
          go !h (a1:a2:a3:a4:a5:as) = go (h `hash32WithSaltExtra` a1 `hash32WithSaltExtra` a2 `hash32WithSaltExtra` a3 `hash32WithSaltExtra` a4 `hash32WithSaltExtra` a5) as
          go !h (a1:a2:a3:a4:as) = go (h `hash32WithSaltExtra` a1 `hash32WithSaltExtra` a2 `hash32WithSaltExtra` a3 `hash32WithSaltExtra` a4) as
          go !h (a1:a2:a3:as) = go (h `hash32WithSaltExtra` a1 `hash32WithSaltExtra` a2 `hash32WithSaltExtra` a3) as
          go !h (a1:a2:as) = go (h `hash32WithSaltExtra` a1 `hash32WithSaltExtra` a2) as
          go !h (a1:as) = go (h `hash32WithSaltExtra` a1) as

-- a fused foldl' equivalent -- NOTE ~ 2x faster than unfolded
hashLeftNoList :: Word32 -> Word8 -> Word32
hashLeftNoList = go
    where go !h 0 = h
          go !h !b = go (h `hash32WithSalt` b) (b-1)

hashLeftUnfoldedNoList :: Word32 -> Word8 -> Word32
hashLeftUnfoldedNoList = go
    -- note: this only works for args divisible by 5
    where go !h !b 
            | b >= 5 = go (h `hash32WithSalt` b  `hash32WithSalt` (b-1)  `hash32WithSalt` (b-2)  `hash32WithSalt` (b-3)  `hash32WithSalt` (b-4)) (b-5) 
            | b /= 0 = error "please call with arg divisible by 5"
            | otherwise = h

