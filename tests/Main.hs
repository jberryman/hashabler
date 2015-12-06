{-# LANGUAGE CPP #-}
module Main where

import Data.Hashabler
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
#if MIN_VERSION_bytestring(0,10,4)
import qualified Data.ByteString.Short as BSh
#endif
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Internal as T
import qualified Data.Text.Lazy as TL
import qualified Data.ByteString.Char8 as C
import qualified Data.Primitive as P

import Vectors.FNV
import Vectors.SipHash

import Foreign.Marshal.Utils (fromBool)

import System.IO
import System.Environment(getArgs)
import Consistency(generatedVectorsDir, checkGeneratedVectors, regenerateVectors)
import System.Directory
import Control.Monad
import Control.Applicative
import Data.List
import Control.Exception

import Data.Word
import Data.Int
import Test.QuickCheck

#if MIN_VERSION_base(4,8,0)
import GHC.Natural (Natural)
#endif

import System.IO.Unsafe(unsafeDupablePerformIO)
import Prelude

import Data.Bits(xor)

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    case args of
         ("regenerate":these) -> do
            dirExists <- doesDirectoryExist generatedVectorsDir
            unless (dirExists && (not $ null generatedVectorsDir)) $
                error "We can't find generated vectors directory where we expect."

            failures <- checkGeneratedVectors 
            unless (null failures) $ do
                putStrLn "Current implementation does not match existing test vectors. Are you sure you want to re-generate them? [y/n]: "
                yn <- getChar
                if yn == 'y'
                    then putStrLn "Ok, regenerating. NOTE: YOU MUST BUMP MAJOR VERSION NUMBER!"
                    else error "Exiting without regenerating test vectors."
            regenerateVectors (if null these then Nothing else Just these)
         _ -> testsMain

testsMain :: IO ()
testsMain = do
#  ifdef ASSERTIONS_ON
    checkAssertionsOn
#  else
    putStrLn "!!! WARNING !!!: assertions not turned on in library code. configure with -finstrumented if you want to run tests with assertions enabled (it's good to test with both)"
#  endif
    checkMiscUnitTests
    checkHashableInstances
    checkVectors
    checkSiphashSanity
    putStrLn "ALL TESTS PASSED"

checkAssertionsOn :: IO ()
checkAssertionsOn = do
    -- Make sure testing environment is sane:
    assertionsWorking <- try $ assert False $ return ()
    assertionsWorkingInLib <- assertionCanary
    case assertionsWorking of
         Left (AssertionFailed _)
           | assertionsWorkingInLib -> putStrLn "Assertions: On"
         _  -> error "Assertions aren't working"


-- TODO better. Just bootstrap test vectors
mixConstructorFNV32 :: Word8 -> Word32 -> Word32
mixConstructorFNV32 b h32 = (h32 `xor` fromIntegral (0xFF - b)) * fnvPrime32
mixConstructorFNV64 :: Word8 -> Word64 -> Word64
mixConstructorFNV64 b h64 = (h64 `xor` fromIntegral (0xFF - b)) * fnvPrime64


checkVectors :: IO ()
checkVectors = do
    test "Checking FNV spec vectors" $ do
        -- Check test vectors from the official FNV spec/implementation:
        let fnvInputsHashed32 = map (hashWord32 . hashFNV32 . C.pack) fnvIn
            -- Our instances perform a final 'mixConstructor' on hashes of array
            -- types (see notes on "Defining principled Hashable instances") so
            -- we'll need to do this to our test vectors before comparing:
            fnv1a32OutMassaged = map (mixConstructorFNV32 0) fnv1a32Out
        unless (fnvInputsHashed32 == fnv1a32OutMassaged) $
            error "fnvInputsHashed32 /= fnv1a32OutMassaged"
        -- And for 64-bit version:
        let fnvInputsHashed64 = map (hashWord64 . hashFNV64 . C.pack) fnvIn
            fnv1a64OutMassaged = map (mixConstructorFNV64 0) fnv1a64Out
        unless (fnvInputsHashed64 == fnv1a64OutMassaged) $
            error "fnvInputsHashed64 /= fnv1a64OutMassaged"

    test "Checking SipHash spec vectors" $ do
        let outs64 = map (siphash64 siphashKey) siphashInputs
            outs128 = map (siphash128 siphashKey) siphashInputs
        unless (length outs64 > 0 && length outs128 > 0) $
            error "tests invalid"
        unless (outs64 == map Hash64 siphashVectors64) $
            error $ "Some Siphash64 vectors failed: "++(show outs64)
        unless (outs128 == map (uncurry Hash128) siphashVectors128) $
            error $ "Some Siphash128 vectors failed: "++(show outs128)
        
    test "Checking generated vectors for all hash functions" $ do
        failures <- checkGeneratedVectors
        unless (null failures) $
            print failures >> error "Got some failures in checkGeneratedVectors!"


untag64 :: Hash64 a -> Hash64 b
untag64 (Hash64 x) = Hash64 x
untag32 :: Hash32 a -> Hash32 b
untag32 (Hash32 x) = Hash32 x

-- check all codepaths in siphash 'hash' instance, and make sure we're not
-- dropping any input bytes in some way. Sufficient to check siphash64 here, as
-- all share the Hash instance implementation.
checkSiphashSanity :: IO ()
checkSiphashSanity = test "SipHash sanity" $ do
    -- different combinations of tuples of word* sizes
    -- check that altering each individual byte results in different hashes
    unless (length uniqueHashes > 0 && length identicalHashes > 0) $
        error "checkSiphashSanity not valid"
    unless (nub uniqueHashes == uniqueHashes) $ do
        error $ "checkSiphashSanity: not all hashes unique! "++ (show (uniqueHashes \\ nub uniqueHashes))
    unless (length (nub identicalHashes) == 1) $
        error "checkSiphashSanity: all of these should have been identical!"
  where w8s = [ 0xFF, 0x01] :: [Word8]
        w16s = [0xFF03, 0x02FF] :: [Word16]
        w32s = [0xFF050607 , 0x04FF0607 , 0x0405FF07 , 0x040506FF] :: [Word32]
        w64s = [ 0xFF09101112131415 , 0x08FF101112131415 , 0x0809FF1112131415 , 0x080910FF12131415 
               , 0x08091011FF131415 , 0x0809101112FF1415 , 0x080910111213FF15 , 0x08091011121314FF ] :: [Word64]

        uniqueHashes = concat [
                map untag64 [siphash64 siphashKey (w8,w64) | w8 <- w8s, w64 <- w64s ]
              , map untag64 [siphash64 siphashKey (w16,w64) | w16 <- w16s,w64 <- w64s ]
              , map untag64 [siphash64 siphashKey (w16,w8,w64) | w16 <- w16s,w8 <- w8s,w64 <- w64s ]
              , map untag64 [siphash64 siphashKey (w32,w64) | w32 <- w32s,w64 <- w64s ]
              , map untag64 [siphash64 siphashKey (w8,w32,w64) | w8 <- w8s,w32 <- w32s,w64 <- w64s ]
              , map untag64 [siphash64 siphashKey (w8,w32,w32) | w8 <- w8s,w32 <- w32s ]
              , map untag64 [siphash64 siphashKey (w32,w16,w64) | w32 <- w32s,w16 <- w16s,w64 <- w64s ]
              , map untag64 [siphash64 siphashKey (w32,w16,w32) | w32 <- w32s,w16 <- w16s ]
              , map untag64 [siphash64 siphashKey (w8,w16,w32,w64) | w8 <- w8s,w16 <- w16s,w32 <- w32s,w64 <- w64s ]
              , map untag64 [siphash64 siphashKey (w8,w16,w32,w32) | w8 <- w8s,w16 <- w16s,w32 <- w32s ]
              , map untag64 [siphash64 siphashKey (w8,w16,w32,w16) | w8 <- w8s,w32 <- w32s,w16 <- w16s ]
              ]

        identicalHashes = [
              untag64 $ siphash64 siphashKey (0x01 :: Word8, 0x02 :: Word8, 0x03 :: Word8, 0x04 :: Word8, 0x05 :: Word8, 0x06 :: Word8, 0x07 :: Word8, 0x08 :: Word8,   0xDEADBEED :: Word32)
            , untag64 $ siphash64 siphashKey (0x0102 :: Word16, 0x03 :: Word8, 0x04 :: Word8, 0x0506 :: Word16, 0x07 :: Word8, 0x08 :: Word8,   0xDEADBEED :: Word32)
            , untag64 $ siphash64 siphashKey (0x01 :: Word8, 0x02030405 :: Word32, 0x06 :: Word8, 0x07 :: Word8, 0x08 :: Word8,   0xDEADBEED :: Word32)
            , untag64 $ siphash64 siphashKey (0x01020304 :: Word32, 0x05060708 :: Word32,   0xDEADBEED :: Word32)
            , untag64 $ siphash64 siphashKey (0x0102030405060708 :: Word64,   0xDEADBEED :: Word32)
            ]


-- Helpers for below:
bytesFloat :: Float -> (Word8,Word8,Word8,Word8)
{-# INLINE bytesFloat #-}
bytesFloat = bytes32 . floatToWord

bytesDouble :: Double -> (Word8,Word8,Word8,Word8,Word8,Word8,Word8,Word8)
{-# INLINE bytesDouble #-}
bytesDouble = bytes64 . doubleToWord


checkMiscUnitTests :: IO ()
checkMiscUnitTests = do
    -- Basic unit tests for Float, with IEEE byte values pulled from this
    -- calculator: http://www.h-schmidt.net/FloatConverter/IEEE754.html
    test "Getting bytes from Float" $
        let fl = -8.4884356e-11
            flBytes = bytesFloat fl
         in unless (flBytes == (0xae, 0xba, 0xa9, 0xa5)) $
              error $ "bytesFloat: "++(show fl)++" /=  "++(show flBytes)

    -- http://www.binaryconvert.com/result_double.html?hexadecimal=ADBABEAA88AB7FD7
    test "Getting bytes from Double" $
        let dbl = -2.10068275286355115215868722646e-88
            dblBytes = bytesDouble dbl
         in unless ( dblBytes == (0xAD, 0xBA, 0xBE, 0xAA, 0x88, 0xAB, 0x7F, 0xD7)) $
              error $ "bytesDouble: "++(show dbl)++" /= "++(show dblBytes)

    test "Bool" $
        unless (hashFNV32 (fromBool True :: Word8) == (untag32 $ hashFNV32 True)
               && hashFNV32 (fromBool False :: Word8) == (untag32 $ hashFNV32 False)) $
             error "Bool instance not sensible"


    -- Misc. internals exposed for testing ----------------
    test "byteSwap fallback" $
        let x = _byteSwap32 0x12345678 
            y = _byteSwap64 0x1234567821436587
         in unless (x == 0x78563412 && y == 0x8765432178563412) $ 
              error $ "Problem with byteSwap: "++(show x)++" "++(show y)

    quickCheckErr 1000 checkSignByte
    quickCheckErr 1000 checkIntegerFallback

    quickCheckErr 1000 check32BitRangeInt64
    quickCheckErr 1000 check32BitRangeWord64
    -- And make sure we check max and min bounds (esp minBound Int!):
    test "check 32-bit range Ints and Words" $
        unless (check32BitRangeInt64  (Large minBound) &&
                check32BitRangeInt64  (Large maxBound) &&
                check32BitRangeWord64  (Large maxBound)) $
            error "Problem with check32BitRange* functions"
                
    quickCheckErr 1000 checkBytes64Alternatives

    test "Magnitude of Int... " $
            -- we're mostly just concerned with minBound
        let ints = [minBound, minBound+1, (-42), (-1), 0, 1, 42, maxBound ] :: [Int]
            intsMag = map magnitudeAsWord ints
         in unless (map show intsMag == map (dropWhile (=='-') . show) ints) $
                error $ "problem with magnitudeAsWord: "++(show intsMag)
    


-- Checking properties of Hashable instances by way of FNV32 hash.
checkHashableInstances :: IO ()
checkHashableInstances = do
    -- small Integers should match Word32/64 (depending on size) + a mixConstructor 0:
    quickCheckErr 1000 $
        \(Large int64) -> 
           let magWord64 = fromIntegral $ abs (int64::Int64) :: Word64
               signByte = if int64 < 0 then 1 else 0
            in hashFNV32 (fromIntegral int64 :: Integer)
                 == (Hash32 $ mixConstructorFNV32 signByte $ hashWord32
                      (if magWord64 > fromIntegral (maxBound :: Word32)
                          then hashFNV32 magWord64
                          else untag32 $ hashFNV32 (fromIntegral magWord64 :: Word32))
                      )

    -- FNV32 test vectors provide basic sanity for word/int instances. Here
    -- just make sure Word and Int types are equivalent.
    quickCheckErr 1000 $
        \(Large word8)-> let int8 = fromIntegral (word8 :: Word8) :: Int8
                          in hashFNV32 word8 == (untag32 $ hashFNV32 int8)
    quickCheckErr 1000 $
        \(Large word16)-> let int16 = fromIntegral (word16 :: Word16) :: Int16
                           in hashFNV32 word16 == (untag32 $ hashFNV32 int16)
    quickCheckErr 1000 $
        \(Large word32)-> let int32 = fromIntegral (word32 :: Word32) :: Int32
                           in hashFNV32 word32 == (untag32 $ hashFNV32 int32)
    quickCheckErr 1000 $
        \(Large word64)-> let int64 = fromIntegral (word64 :: Word64) :: Int64
                           in hashFNV32 word64 == (untag32 $ hashFNV32 int64)
    -- And for machine-dependenat Word/Int check equivalence to Int/Word32 when
    -- in 32-bit range, else (only relevant on 64-bit machines) to Int/Word64:
    quickCheckErr 1000 $
        \(Large word)->
            hashFNV32 (word :: Word)
                 == (if word > fromIntegral (maxBound :: Word32)
                        then untag32 $ hashFNV32 (fromIntegral word :: Int64)
                        else untag32 $ hashFNV32 (fromIntegral word :: Int32))

#  if MIN_VERSION_base(4,8,0)
    -- Check documented 32-bit chunked, big-endian order hashing of Natural:
    quickCheckErr 1000 $ do
        let checkNat nibbles = 
              let (nat,bytesBE) = naturalNibbles nibbles
                  testSane = (length bytesBE) `mod` 4 == 0
               in testSane &&
                   (hashFNV32 nat) == 
                   (untag32 $ hashFNV32 bytesBE) -- against instance [Word8]
                      
        flip forAll checkNat $ do
            let maxWidth = 100 -- nibbles
            w <- growingElements [1..maxWidth]
            -- a random hex string, dropping any leading zeros:
            hx <- dropWhile (== '0') <$> (replicateM w $ 
                elements $ ['0'..'9']++['A'..'F'])
            return $ if null hx then discard else hx
#  endif

    -- Check that equivalent strict and lazy ByteStrings (of varying chunk
    -- sizes) hash to the same:
    quickCheckErr 100 $ do
        let checkBS (bs,chunkSize) = 
              let wd8s = take bs $ iterate (+1) 0
                  bsStrict = B.pack wd8s
                  bsLazyChunked = BL.fromChunks $ map B.pack $ chunk chunkSize wd8s
               in (hashFNV32 bsStrict) ==
                  (untag32 $ hashFNV32 bsLazyChunked)
        flip forAll checkBS $ do
            let maxBytes = 1000*1000
            bs <- growingElements [1..maxBytes]
            chunkSize <- choose (1,maxBytes+1)
            return (bs,chunkSize)

    -- ...likewise for Text
    quickCheckErr 100 $ do
        let checkBS (bs,chunkSize) = 
              let cs = take bs $ cycle $ take 199 $ iterate succ '0'
                  bsStrict = T.pack cs
                  bsLazyChunked = TL.fromChunks $ map T.pack $ chunk chunkSize cs
               in (hashFNV32 bsStrict) ==
                  (untag32 $ hashFNV32 bsLazyChunked)
        flip forAll checkBS $ do
            let maxBytes = 1000*1000
            bs <- growingElements [1..maxBytes]
            chunkSize <- choose (1,maxBytes+1)
            return (bs,chunkSize)

#  if MIN_VERSION_bytestring(0,10,4)
    -- Check that ShortByteStrings hash like big strict ones:
    quickCheckErr 100 $ do
        let checkBS bs = 
              let wd8s = take bs $ iterate (+1) 0
                  bsStrict = B.pack wd8s
                  bsShort  = BSh.pack wd8s
               in (hashFNV32 bsStrict) ==
                  (untag32 $ hashFNV32 bsShort)
        flip forAll checkBS $ do
            let maxBytes = 1000*1000
            growingElements [1..maxBytes]
#  endif
    
    -- Check that ByteStrings hash like [Word8]
    quickCheckErr 100 $ do
        let checkBS bs = 
              let wd8s = take bs $ iterate (+1) 0
                  bsStrict = B.pack wd8s
               in (hashFNV32 bsStrict) ==
                  (untag32 $ hashFNV32 wd8s)
        flip forAll checkBS $ do
            let maxBytes = 1000*1000
            growingElements [1..maxBytes]

    -- Check that P.ByteArrays hash like [Word8]
    quickCheckErr 100 $ do
        let checkBA bs = 
              let wd8s = take bs $ iterate (+1) 0
                  byteArr = packByteArray wd8s
               in (hashFNV32 byteArr) ==
                  (untag32 $ hashFNV32 wd8s)
        flip forAll checkBA $ do
            let maxBytes = 1000*1000
            growingElements [1..maxBytes]
    
    -- Check that ByteStrings hash like [Word8], after some arbitrary
    -- equivalent transformations (we mainly want to exercise handling of the
    -- length and offset in the ByteString internals)
    quickCheckErr 100 $ do
        let checkBS (bs,takeVal,dropVal) = 
              let wd8s = take bs $ iterate (+1) 0
                  b = B.take takeVal $ B.drop dropVal $ B.pack wd8s
                  l =   take takeVal $   drop dropVal $        wd8s
               in (hashFNV32 b) ==
                  (untag32 $ hashFNV32 l)
        flip forAll checkBS $ do
            let maxBytes = 1000*1000
            bs <- growingElements [1..maxBytes]
            takeVal <- growingElements [1..(maxBytes*2)]
            dropVal <- growingElements [1..(maxBytes*2)]
            return (bs,takeVal,dropVal)

    -- Check that Text is hashed in big endian, making sure we get some
    -- double-size codes, by encoding as a ByteString and comparing the hashes
    -- of both
    quickCheckErr 100 $ do
        let checkBS largeCs = 
              let t = T.pack $ map (toEnum . clean . getLarge) largeCs
                  clean = (`mod` fromEnum (maxBound :: Char))
                  bs = T.encodeUtf16BE t
               in (hashFNV32 t) == (untag32 $ hashFNV32 bs)
        flip forAll checkBS $ do
            let maxChars = 1000
            w <- growingElements [1..maxChars]
            -- Note: some of these characters will be in the reserved range and
            -- re-written in pack, but that's fine:
            vector w
            
    test "Test that Char instance works over reserved range" $
        let reserved = ['\xD800' .. '\xDFFF']
         in unless (2048 == (length $ nub $ map hashFNV32 reserved)) $
              error "Collision in the reserved Unicode range for Char"

    -- Check that String and Text instances are identical for valid unicode
    -- code points:
    quickCheckErr 1000 $ \sDirty -> 
        let s = map T.safe sDirty
         in (hashFNV32 $ T.pack s) == (untag32 $ hashFNV32 s)

    -- checking collisions in an ad hoc way; we especially care about our sum
    -- types with fixed shape. TODO improve; also this is slower than necessary
    quickCheckErr 100 $ checkCollisionsOf (undefined :: Bool) 4
    quickCheckErr 100 $ checkCollisionsOf (undefined :: Ordering) 9
    quickCheckErr 100 $ checkCollisionsOf (undefined :: Either () ()) 4
    quickCheckErr 100 $ checkCollisionsOf (undefined :: Maybe ()) 4
    quickCheckErr 100 $ checkCollisionsOf (undefined :: [()]) 10000
    quickCheckErr 100 $ checkCollisionsOf (undefined :: TreeOfSums1) 100
    quickCheckErr 100 $ checkCollisionsOf (undefined :: TreeOfSums2) 1000
    quickCheckErr 100 $ checkCollisionsOf (undefined :: TreeOfSums3) 9

    -- Check tuples match lists, plus a mixConstructor.
    quickCheckErr 100 $ forAll (vector 2 :: Gen [Word8]) $ \ l@[a,b] ->
          hashFNV32 l == (Hash32 $ mixConstructorFNV32 0 $ hashWord32 $ hashFNV32 (a,b))
    quickCheckErr 100 $ forAll (vector 3 :: Gen [Word8]) $ \ l@[a,b,c] ->
          hashFNV32 l == (Hash32 $ mixConstructorFNV32 0 $ hashWord32 $ hashFNV32 (a,b,c))
    quickCheckErr 100 $ forAll (vector 4 :: Gen [Word8]) $ \ l@[a,b,c,d] ->
          hashFNV32 l == (Hash32 $ mixConstructorFNV32 0 $ hashWord32 $ hashFNV32 (a,b,c,d))
    quickCheckErr 100 $ forAll (vector 5 :: Gen [Word8]) $ \ l@[a,b,c,d,e] ->
          hashFNV32 l == (Hash32 $ mixConstructorFNV32 0 $ hashWord32 $ hashFNV32 (a,b,c,d,e))
    quickCheckErr 100 $ forAll (vector 6 :: Gen [Word8]) $ \ l@[a,b,c,d,e,f] ->
          hashFNV32 l == (Hash32 $ mixConstructorFNV32 0 $ hashWord32 $ hashFNV32 (a,b,c,d,e,f))
    quickCheckErr 100 $ forAll (vector 7 :: Gen [Word8]) $ \ l@[a,b,c,d,e,f,g] ->
          hashFNV32 l == (Hash32 $ mixConstructorFNV32 0 $ hashWord32 $ hashFNV32 (a,b,c,d,e,f,g))
    quickCheckErr 100 $ forAll (vector 8 :: Gen [Word8]) $ \ l@[a,b,c,d,e,f,g,h] ->
          hashFNV32 l == (Hash32 $ mixConstructorFNV32 0 $ hashWord32 $ hashFNV32 (a,b,c,d,e,f,g,h))

    -- TODO more of these here; this should be fine for now though, as all the
    -- variable width types above check in with [a] at some point.
    test "Checking tuple of variable width types" $ 
        let l = [ ([0::Word8,0], []) , ([0] , [0]) , ([],[0::Word8,0]) ]
         in unless ((length $ nub $ map hashFNV32 l) == 3) $
             error "products of lists are unprincipled"


-- For a fairly stupid check of instance sanity w/r/t collisions. Also check
-- these types individually:
type TreeOfSums1 = Either (Maybe (Either Bool Ordering)) (Maybe (Either Bool Ordering))            
type TreeOfSums2 = Either TreeOfSums1 TreeOfSums1
type TreeOfSums3 = Either (Maybe Bool) Bool
            
-- This may return okay collisions on large universes; we can decide whether to use it there or not
checkCollisionsOf :: (Show a, Hashable a, Eq a, Arbitrary a)=> a -> Int -> Property
checkCollisionsOf a n = forAll (nub <$> vector n) check where
    check l = let _ = a `asTypeOf` head l 
               in length l == (length $ nub $ map hashFNV32 l)

chunk :: Int -> [a] -> [[a]]
chunk n = assert (n>0) $ go where
    go [] = []
    go l  = let (as,bs) = splitAt n l
             in as : chunk n bs



#if MIN_VERSION_base(4,8,0)
naturalNibbles :: [Char] -> (Natural, [Word8])
naturalNibbles nibbles = (nat,bytesBE) where
    -- The Natural:
    nat = read $ "0x"++nibbles :: Natural
    -- The Natural's bytes in big endian, padded to 32-bits
    bytesBE = chunkBytes32 $ pad0s++nibbles

    chunkBytes32 = map (read . ("0x"++)) . chunk 2
    w = length nibbles
    pad0s = replicate ((((w+7) `div` 8)*8) - w) '0'
#endif


-- MISC PROPERTIES OF INTERNALS: -----------------
checkSignByte :: Large Int -> Bool
checkSignByte (Large i) = _signByte i == if i < 0 then 1 else 0

-- NOTE: if not using integer-gmp this tests nothing:
checkIntegerFallback :: Large Int -> Large Int -> Bool
checkIntegerFallback (Large base) (Large mul) = 
    let baseInteger = fromIntegral base :: Integer
        integer = baseInteger * ((fromIntegral mul) ^ (2:: Int))
     in (_hash32Integer fnvOffsetBasis32 integer ) == (FNV32 $ hashWord32 $ hashFNV32 integer)

check32BitRangeInt64 :: Large Int32 -> Bool
check32BitRangeInt64 (Large int32) = 
    let int64 = fromIntegral int32 :: Int64 
     in _hash32_Int_64 fnvOffsetBasis32 int64
         == (FNV32 $ hashWord32 $ hashFNV32 int32)

check32BitRangeWord64 :: Large Word32 -> Bool
check32BitRangeWord64 (Large word32) = 
    let word64 = fromIntegral word32 :: Word64 
     in _hash32_Word_64 fnvOffsetBasis32 word64
         == (FNV32 $ hashWord32 $ hashFNV32 word32)

checkBytes64Alternatives :: Large Word64 -> Bool
checkBytes64Alternatives (Large w64) = 
    _bytes64_64 w64 == _bytes64_32 w64





-- Utilites:  ---------------------------------
test :: String -> IO () -> IO ()
test str io = do
    putStr $ str++"..."
    io
    putStrLn " OK"

quickCheckErr :: Testable prop => Int -> prop -> IO ()
quickCheckErr n p = 
    quickCheckWithResult stdArgs{ maxSuccess = n } p
      >>= maybeErr

  where maybeErr (Success _ _ _) = return ()
        maybeErr e = error $ show e

packByteArray :: [Word8] -> P.ByteArray
{-# NOINLINE packByteArray #-}
packByteArray byts = unsafeDupablePerformIO $ do
     aMut <- P.newByteArray (length byts)
     forM_ (zip [0..] byts) $ \(ix,byt)-> P.writeByteArray aMut ix byt
     P.unsafeFreezeByteArray aMut
