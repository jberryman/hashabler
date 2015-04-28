{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Data.Word
import Data.Hashabler

main = print $ (hash32Times 1000000000) (9999::Word32)

hash32Times :: Hashable a=> Int -> a -> Word32
{-# INLINE hash32Times #-}
hash32Times iters =
  \a->  let go !h !0 = h
            go !h !n = go (h `hash` a) (n-1)
         in fnv32 $ go fnvOffsetBasis32 iters 
