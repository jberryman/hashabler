{-# LANGUAGE CPP #-}
module Main where

import Data.Hashable.FNV1a
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
import Vectors.FNV (fnvIn, fnv1a32Out)

import Foreign.Marshal.Utils (fromBool)

import System.IO
import System.Environment(getArgs)
import Control.Exception(assert)
import Consistency(generatedVectorsDir, checkGeneratedVectors, regenerateVectors)
import System.Directory
import Control.Monad
import Data.List

import Data.Word
import Data.Int
import Test.QuickCheck

#if MIN_VERSION_base(4,8,0)
import GHC.Natural (Natural)
#endif

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
    checkVectors
    checkHashableInstances

checkVectors :: IO ()
checkVectors = do
    test "Checking FNV spec vectors" $ do
        -- Check test vectors from the official FNV spec/implementation:
        let fnvInputsHashed = map (fnv32 . hashFNV32 . C.pack) fnvIn
            -- Our instances perform a final 'mixConstructor' on hashes of array
            -- types (see notes on "Defining principled Hashable instances") so
            -- we'll need to do this to our test vectors before comparing:
            fnv1a32OutMassaged = map (fnv32 . mixConstructor 0 . FNV32) fnv1a32Out
        unless (fnvInputsHashed == fnv1a32OutMassaged) $
            error "fnvInputsHashed /= fnv1a32OutMassaged"
        
    test "Checking generated vectors for all hash functions" $ do
        failures <- checkGeneratedVectors
        unless (null failures) $
            print failures >> error "Got some failures in checkGeneratedVectors!"

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
        unless (hashFNV32 (fromBool True :: Word8) == hashFNV32 True
               && hashFNV32 (fromBool False :: Word8) == hashFNV32 False) $
             error "Bool instance not sensible"


    -- Misc. internals exposed for testing ----------------
    test "byteSwap fallback" $
        let x = _byteSwap32 0x12345678 
            y = _byteSwap64 0x1234567821436587
         in unless (x == 0x78563412 && y == 0x8765432178563412) $ 
              error $ "Problem with byteSwap: "++(show x)++" "++(show y)

    quickCheck1000 checkSignByte
    quickCheck1000 checkIntegerFallback
    quickCheck1000 check32BitRangeInt64
    quickCheck1000 check32BitRangeWord64
    quickCheck1000 checkBytes64Alternatives


-- Checking properties of Hashable instances by way of FNV32 hash.
checkHashableInstances :: IO ()
checkHashableInstances = do
    -- small Integers should match Word32/64 (depending on size) + a mixConstructor 0:
    quickCheck1000 $
        \(Large int64) -> 
           let magWord64 = fromIntegral $ abs (int64::Int64) :: Word64
               signByte = if int64 < 0 then 1 else 0
            in hashFNV32 (fromIntegral int64 :: Integer)
                 == mixConstructor signByte
                      (if magWord64 > fromIntegral (maxBound :: Word32)
                          then hashFNV32 magWord64
                          else hashFNV32 (fromIntegral magWord64 :: Word32))

    -- FNV32 test vectors provide basic sanity for word/int instances. Here
    -- just make sure Word and Int types are equivalent.
    quickCheck1000 $
        \(Large word8)-> let int8 = fromIntegral (word8 :: Word8) :: Int8
                          in hashFNV32 word8 == hashFNV32 int8
    quickCheck1000 $
        \(Large word16)-> let int16 = fromIntegral (word16 :: Word16) :: Int16
                           in hashFNV32 word16 == hashFNV32 int16
    quickCheck1000 $
        \(Large word32)-> let int32 = fromIntegral (word32 :: Word32) :: Int32
                           in hashFNV32 word32 == hashFNV32 int32
    quickCheck1000 $
        \(Large word64)-> let int64 = fromIntegral (word64 :: Word64) :: Int64
                           in hashFNV32 word64 == hashFNV32 int64
    -- And for machine-dependenat Word/Int check equivalence to Int/Word32 when
    -- in 32-bit range, else (only relevant on 64-bit machines) to Int/Word64:
    quickCheck1000 $
        \(Large word)->
            hashFNV32 (word :: Word)
                 == (if word > fromIntegral (maxBound :: Word32)
                        then hashFNV32 (fromIntegral word :: Int64)
                        else hashFNV32 (fromIntegral word :: Int32))

#  if MIN_VERSION_base(4,8,0)
    -- Check documented 32-bit chunked, big-endian order hashing of Natural:
    quickCheck1000 $ do
        let checkNat nibbles = 
              let (nat,bytesBE) = naturalNibbles nibbles
                  testSane = (length bytesBE) `mod` 4 == 0
               in testSane &&
                   (hashFNV32 nat) == 
                   (hashFNV32 bytesBE) -- against instance [Word8]
                      
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
    quickCheck $ do
        let checkBS (bs,chunkSize) = 
              let wd8s = take bs $ iterate (+1) 0
                  bsStrict = B.pack wd8s
                  bsLazyChunked = BL.fromChunks $ map B.pack $ chunk chunkSize wd8s
               in (hashFNV32 bsStrict) ==
                  (hashFNV32 bsLazyChunked)
        flip forAll checkBS $ do
            let maxBytes = 1000*1000
            bs <- growingElements [1..maxBytes]
            chunkSize <- choose (1,maxBytes+1)
            return (bs,chunkSize)

    -- ...likewise for Text
    quickCheck $ do
        let checkBS (bs,chunkSize) = 
              let cs = take bs $ cycle $ take 199 $ iterate succ '0'
                  bsStrict = T.pack cs
                  bsLazyChunked = TL.fromChunks $ map T.pack $ chunk chunkSize cs
               in (hashFNV32 bsStrict) ==
                  (hashFNV32 bsLazyChunked)
        flip forAll checkBS $ do
            let maxBytes = 1000*1000
            bs <- growingElements [1..maxBytes]
            chunkSize <- choose (1,maxBytes+1)
            return (bs,chunkSize)

#  if MIN_VERSION_bytestring(0,10,4)
    -- Check that ShortByteStrings hash like big strict ones:
    quickCheck $ do
        let checkBS bs = 
              let wd8s = take bs $ iterate (+1) 0
                  bsStrict = B.pack wd8s
                  bsShort  = BSh.pack wd8s
               in (hashFNV32 bsStrict) ==
                  (hashFNV32 bsShort)
        flip forAll checkBS $ do
            let maxBytes = 1000*1000
            growingElements [1..maxBytes]
#  endif
    
    -- Check that ByteStrings hash like [Word8]
    quickCheck $ do
        let checkBS bs = 
              let wd8s = take bs $ iterate (+1) 0
                  bsStrict = B.pack wd8s
               in (hashFNV32 bsStrict) ==
                  (hashFNV32 wd8s)
        flip forAll checkBS $ do
            let maxBytes = 1000*1000
            growingElements [1..maxBytes]
    
    -- Check that ByteStrings hash like [Word8], after some arbitrary
    -- equivalent transformations (we mainly want to exercise handling of the
    -- length and offset in the ByteString internals)
    quickCheck $ do
        let checkBS (bs,takeVal,dropVal) = 
              let wd8s = take bs $ iterate (+1) 0
                  b = B.take takeVal $ B.drop dropVal $ B.pack wd8s
                  l =   take takeVal $   drop dropVal $        wd8s
               in (hashFNV32 b) ==
                  (hashFNV32 l)
        flip forAll checkBS $ do
            let maxBytes = 1000*1000
            bs <- growingElements [1..maxBytes]
            takeVal <- growingElements [1..(maxBytes*2)]
            dropVal <- growingElements [1..(maxBytes*2)]
            return (bs,takeVal,dropVal)

    -- Check that Text is hashed in big endian, making sure we get some
    -- double-size codes, by encoding as a ByteString and comparing the hashes
    -- of both
    quickCheck $ do
        let checkBS largeCs = 
              let t = T.pack $ map (toEnum . clean . getLarge) largeCs
                  clean = (`mod` fromEnum (maxBound :: Char))
                  bs = T.encodeUtf16BE t
               in (hashFNV32 t) == (hashFNV32 bs)
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
    quickCheck1000 $ \sDirty -> 
        let s = map T.safe sDirty
         in (hashFNV32 $ T.pack s) == hashFNV32 s

    -- checking collisions in an ad hoc way; we especially care about our sum
    -- types with fixed shape. TODO improve; also this is slower than necessary
    quickCheck $ checkCollisionsOf (undefined :: Bool) 4
    quickCheck $ checkCollisionsOf (undefined :: Ordering) 9
    quickCheck $ checkCollisionsOf (undefined :: Either () ()) 4
    quickCheck $ checkCollisionsOf (undefined :: Maybe ()) 4
    quickCheck $ checkCollisionsOf (undefined :: [()]) 10000
    quickCheck $ checkCollisionsOf (undefined :: TreeOfSums1) 100
    quickCheck $ checkCollisionsOf (undefined :: TreeOfSums2) 1000
    quickCheck $ checkCollisionsOf (undefined :: TreeOfSums3) 9

    -- Check tuples match lists, plus a mixConstructor.
    quickCheck $ forAll (vector 2 :: Gen [Word8]) $ \ l@[a,b] ->
          hashFNV32 l == (mixConstructor 0 $ hashFNV32 (a,b))
    quickCheck $ forAll (vector 3 :: Gen [Word8]) $ \ l@[a,b,c] ->
          hashFNV32 l == (mixConstructor 0 $ hashFNV32 (a,b,c))
    quickCheck $ forAll (vector 4 :: Gen [Word8]) $ \ l@[a,b,c,d] ->
          hashFNV32 l == (mixConstructor 0 $ hashFNV32 (a,b,c,d))
    quickCheck $ forAll (vector 5 :: Gen [Word8]) $ \ l@[a,b,c,d,e] ->
          hashFNV32 l == (mixConstructor 0 $ hashFNV32 (a,b,c,d,e))
    quickCheck $ forAll (vector 6 :: Gen [Word8]) $ \ l@[a,b,c,d,e,f] ->
          hashFNV32 l == (mixConstructor 0 $ hashFNV32 (a,b,c,d,e,f))
    quickCheck $ forAll (vector 7 :: Gen [Word8]) $ \ l@[a,b,c,d,e,f,g] ->
          hashFNV32 l == (mixConstructor 0 $ hashFNV32 (a,b,c,d,e,f,g))
    quickCheck $ forAll (vector 8 :: Gen [Word8]) $ \ l@[a,b,c,d,e,f,g,h] ->
          hashFNV32 l == (mixConstructor 0 $ hashFNV32 (a,b,c,d,e,f,g,h))

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
     in (_hash32Integer fnvOffsetBasis32 integer :: FNV32) == hashFNV32 integer

check32BitRangeInt64 :: Large Int32 -> Bool
check32BitRangeInt64 (Large int32) = 
    let int64 = fromIntegral int32 :: Int64 
     in _hash32_Int_64 fnvOffsetBasis32 int64
         == hashFNV32 int32

check32BitRangeWord64 :: Large Word32 -> Bool
check32BitRangeWord64 (Large word32) = 
    let word64 = fromIntegral word32 :: Word64 
     in _hash32_Word_64 fnvOffsetBasis32 word64
         == hashFNV32 word32

checkBytes64Alternatives :: Large Word64 -> Bool
checkBytes64Alternatives (Large w64) = 
    _bytes64_64 w64 == _bytes64_32 w64





-- Utilites:  ---------------------------------
test :: String -> IO () -> IO ()
test str io = do
    putStr $ str++"..."
    io
    putStrLn " OK"

quickCheck1000 :: Testable prop => prop -> IO ()
quickCheck1000 = quickCheckWith stdArgs{ maxSuccess = 1000 } -- , chatty = False }
