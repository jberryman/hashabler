{-# LANGUAGE TemplateHaskell #-}
module MachDeps (littleEndian) where

-- technique stolen from: http://stackoverflow.com/a/6129295/176841 and
-- https://hackage.haskell.org/package/cpu-0.1.0/docs/src/System-Endian.html#getSystemEndianness

import Language.Haskell.TH.Syntax(Lift(..))
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.Ptr
import System.IO.Unsafe (unsafePerformIO)
import Data.Word

-- TODO TESTING: CORRECT ON 64-BIT LE MACHINES
-- | Compile-time constant; True on LE, False on BE, and compile-time error otherwise.
littleEndian :: Bool
littleEndian = 
    $( 
      let cst :: Word32
          cst = 0x01020304
          check :: [Word8] -> Bool
          check x
              | x == [1,2,3,4] = False -- BE
              | x == [4,3,2,1] = True  -- LE
              | otherwise = error $ "Very interesting endianness! "
                        ++"(unfortunately we don't support it yet)"
       in lift $ unsafePerformIO $ alloca $ \p -> do
            poke p cst
            let wd8Ptrs = map (castPtr p `plusPtr`) [0..3]
            mapM peek wd8Ptrs >>= return . check
           
      )


