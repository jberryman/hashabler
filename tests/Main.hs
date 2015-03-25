module Main where

import Data.Hashable.FNV1a
import qualified Data.ByteString.Char8 as C
import Vectors.FNV (fnvIn, fnv1a32Out)

main :: IO ()
main = do
    -- 
    let fnvInputsHashed = map (fnv32 . hashFNV32 . C.pack) fnvIn
        -- Our instances perform a final 'mixConstructor' on hashes of array
        -- types (see notes on "Defining principled Hashable instances") so
        -- we'll need to do this to our test vectors before comparing:
        fnv1a32OutMassaged = map (fnv32 . mixConstructor 0 . FNV32) fnv1a32Out
    print $ fnvInputsHashed == fnv1a32OutMassaged
        

