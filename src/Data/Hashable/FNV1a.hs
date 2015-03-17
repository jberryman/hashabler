{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
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
 *different types*, even if those values are in some way "similar". This also
 means instances cannot rely on the hashing of child elements being
 uncorrelated. That might be one interpretation of the mistake in our faulty
 @Maybe@ instance above
 -}
 -- *** TODO notes on Generic deriving
    where

-- TODO note copy-pasta and thanks to Tibbe & Hashable

import Data.Word
import Data.Int
import Data.Bits
import Data.Char
import Data.List

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

-- TODO BENCHMARK hash32WithSalt against these and remove:
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


-- TODO rename these methods:
--        - rename seed -> hash, and use a newtype wrapper
--        - call it "combine" or "hashedWith" or "hasingIn"
--        - possibly use (<#) operator
--        - mention that we're "mixing right hand data into left-hand side hash"

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

-- ---------
-- Instances that ought to match the test vectors from the spec!



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

-- ---------
-- Architecture-dependent types, with special handling.
instance Hashable Int where
instance Hashable Word where

                             -- "values represent Unicode (or equivalently ISO/IEC 10646) characters..."
instance Hashable Char where -- maxbound is 1114111 (~21 bits), `ord :: Char -> Int`
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed = hash32WithSalt seed . ord
    -- TODO pre-hash all but last 3 bytes.


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

-- THESE HAVE VARIABLE LENGTH, and require:  ------------------------
mixConstructor :: Word8  -- ^ Constructor number. Recommend starting from 0 and incrementing.
               -> Word32 -- ^ Hash value TODO remove this comment, or clarify whether this should be applied first or last, or whether it matters.
               -> Word32 -- ^ New hash value
mixConstructor n h = h `hash32WithSalt` (0xFF - n)

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
#if MIN_VERSION_base(4,3,0)
      \(BSh.SBS ba_) ->
#else
      \(BSh.SBS ba_ _) ->
#endif
        let ba = P.ByteArray ba_
         in mixConstructor 0 $
              hashByteArray seed 0 (P.sizeofByteArray ba) ba
#endif

-- | Strict @Text@
instance Hashable T.Text where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed = mixConstructor 0 .
        hashText seed

-- TODO benchmarks for fusion:
-- | Lazy @Text@
instance Hashable TL.Text where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed = mixConstructor 0 .
        TL.foldlChunks hashText seed

-- | Here we hash each byte of the array in turn. Depending on the size and
-- alignment of data stored, this might include padding bytes and might result
-- in a different value across different architectures.
instance Hashable P.ByteArray where
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed = \ba-> mixConstructor 0 $
        hashByteArray seed 0 (P.sizeofByteArray ba) ba

instance Hashable a => Hashable [a] where
    -- TODO OPTIMIZE (see notes below)
    {-# INLINE hash32WithSalt #-}
    hash32WithSalt seed = mixConstructor 0 .
        hashFoldl' seed

-- ------------------------------------------------------------------

{-
-- ---------
-- These ought to hash in some reasonable and good way. Recursive instances
-- ought to be defined in terms of hash*WithSalt on subterms; this should
-- ensure we don't get e.g. hash [[1],[2,3]] == hash [[1,2],[3]].

instance Hashable Integer where
    -- GHC.Integer.GMP.Internals from integer-gmp (part of GHC distribution)
    -- TODO be careful that Eq values hash to the same!
instance (Integral a, Hashable a) => Hashable (Ratio a) where
instance Hashable Ordering where
instance Hashable () where
instance Hashable ThreadId where
instance Hashable TypeRep where
instance Hashable (StableName a) where

instance Hashable a => Hashable (Maybe a) where
instance (Hashable a, Hashable b) => Hashable (Either a b) where
-}

-- TUPLES:

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


-- BASELINE: a fused foldl' equivalent -- NOTE ~ 2x faster than Unfolded on 7.10
hashLeftNoList :: Word32 -> Word8 -> Word32  -- NOTE: tested w/ this monomorphic sig
{-# INLINE hashLeftNoList #-}
hashLeftNoList = go
    where go !h 0 = h
          go !h !b = go (h `hash32WithSalt` b) (b-1)


-- benchmark fetching bytes from bytestring in different ways -----------
-- TODO TESTING make sure to test on bytestrings where off /= 0


-- TODO Factor out common code here:


-- This is about twice as fast as a loop with single byte peeks:
hashBytesUnrolled64 :: Word32 -> B.ByteString -> Word32
{-# INLINE hashBytesUnrolled64 #-}
hashBytesUnrolled64 h = \(B.PS fp off lenBytes) -> unsafeDupablePerformIO $
      withForeignPtr fp $ \base -> do
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
        
        hash8ByteLoop h off 


-- TODO TESTING, quickcheck against hashBytesUnrolled64
--      TESTING, make sure we use characters of variable width. 
hashText :: Word32 -> T.Text -> Word32
{-# INLINE hashText #-}
hashText h = \(T.Text (T.Array ba_) off16 len16) -> 
    let ba = P.ByteArray ba_
        !lenBytes = len16 `unsafeShiftL` 1 -- len16 * 2
        !off      = off16 `unsafeShiftL` 1 -- off16 * 2
     -- make sure our ByteArray is packed UTF-16 so we're not hashing padding
     in assert (P.sizeOf (0::Word16) == 2 && P.alignment (0::Word16) == 2) $
          hashByteArray h off lenBytes ba

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
            | otherwise     = assert (ix <= ixFinal) $ do
                let b0 = P.indexByteArray ba ix
                 in hashRemainingBytes (hAcc <# b0) (ix+1)
     in hash8ByteLoop h off 
