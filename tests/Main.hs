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

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    case args of
         ["regenerate"] -> do
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
            regenerateVectors
         _ -> testsMain

testsMain :: IO ()
testsMain = do
    -- Check test vectors from the official FNV spec/implementation:
    let fnvInputsHashed = map (fnv32 . hashFNV32 . C.pack) fnvIn
        -- Our instances perform a final 'mixConstructor' on hashes of array
        -- types (see notes on "Defining principled Hashable instances") so
        -- we'll need to do this to our test vectors before comparing:
        fnv1a32OutMassaged = map (fnv32 . mixConstructor 0 . FNV32) fnv1a32Out
    print $ fnvInputsHashed == fnv1a32OutMassaged
        
    failures <- checkGeneratedVectors
    unless (null failures) $
        print failures >> error "Got some failures!"
