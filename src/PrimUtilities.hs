{-# LANGUAGE MagicHash, UnboxedTuples #-}
module PrimUtilities where

import GHC.Exts


-- A bit obnoxious we don't have functions 
--   :: Float -> Word32
--   :: Double -> Word64
-- Also 'floating-bits' doesn't work.
--
-- See http://stackoverflow.com/q/6976684/176841


-- | "Convert to integers. First Int# in result is the mantissa; second is the
-- exponent."
decodeFloat_Int :: Float -> (Int, Int)
{-# INLINE decodeFloat_Int #-}
decodeFloat_Int (F# f#) = 
    case decodeFloat_Int# f# of 
         (# m#, e# #) -> (I# m# , I# e#)

-- | "Convert to integer. First component of the result is -1 or 1, indicating
-- the sign of the mantissa. The next two are the high and low 32 bits of the
-- mantissa respectively, and the last is the exponent."
decodeDouble_2Int :: Double -> (Int, Word, Word, Int)
{-# INLINE decodeDouble_2Int #-}
decodeDouble_2Int (D# d#) = 
    case decodeDouble_2Int# d# of
         (# w#, x#, y#, z# #) -> (I# w#, W# x#, W# y#, I# z#)

-- TODO how does this work (e.g. what happens if called on 32-bit arch? and is
-- it faster on 64-bit arches?
-- decodeDouble_Int64# :: Double# -> (#Int#, Int##)
