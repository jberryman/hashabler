module Main where

import Data.Hashable.FNV1a
import qualified Data.ByteString.Char8 as C
import Vectors.FNV (fnvIn, fnv1a32Out)

import System.IO
import System.Environment(getArgs)
import Control.Exception(assert)
import Consistency(generatedVectorsDir, checkGeneratedVectors, regenerateVectors)
import System.Directory
import Control.Monad

import Data.Word
import Data.Int
import Test.QuickCheck

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


-- TODO
-- _byteSwap32 0x12345678 == 0x78563412
-- _byteSwap64 0x1234567821436587 == 0x8765432178563412


-- Utilites:  ---------------------------------
test :: String -> IO () -> IO ()
test str io = do
    putStr $ str++"..."
    io
    putStrLn " OK"

quickCheck1000 = quickCheckWith stdArgs{ maxSuccess = 1000 } -- , chatty = False }
