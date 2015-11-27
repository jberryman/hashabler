{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE RecordWildCards, BangPatterns, CPP #-}
module Data.Hashabler.SipHash (
    siphash64
  , siphash128
  , SipKey(..)
  ) where

-- We use the identity monad for non-recursive binding and utilize name
-- shadowing and RecordWildcards so we can easily translate the implicitly
-- stateful siphash reference implementation here:
--
-- https://github.com/veorq/SipHash/blob/master/siphash24.c
--  
import Data.Functor.Identity
import Data.Word (Word64)
import Data.Bits
import Control.Exception(assert)

import Data.Hashabler.Internal


{- Hard-coded for now. TODO later make configurable if desired
-- /* default: SipHash-2-4 */
-- #define cROUNDS 2
-- #define dROUNDS 4
cROUNDS, dROUNDS :: Int
cROUNDS = 2
dROUNDS = 4
-}

-- #define ROTL(x,b) (uint64_t)( ((x) << (b)) | ( (x) >> (64 - (b))) )
rotl :: Word64 -> Int -> Word64
{-# INLINE rotl #-}
rotl x b = (x `unsafeShiftL` b) .|. (x `unsafeShiftR` (64 - b))


sipRound :: Word64 -> Word64 -> Word64 -> Word64 -> (Word64, Word64, Word64, Word64)
{-# INLINE sipRound #-}
sipRound v0 v1 v2 v3 = runIdentity $ do
    v0 <- return $ v0 + v1 
    v1 <- return $ rotl v1 13
    v1 <- return $ v1 `xor` v0
    v0 <- return $ rotl v0 32

    v2 <- return $ v2 + v3
    v3 <- return $ rotl v3 16
    v3 <- return $ v3 `xor` v2

    v0 <- return $ v0 + v3
    v3 <- return $ rotl v3 21
    v3 <- return $ v3 `xor` v0

    v2 <- return $ v2 + v1
    v1 <- return $ rotl v1 17
    v1 <- return $ v1 `xor` v2
    v2 <- return $ rotl v2 32
    return (v0, v1, v2, v3)


-- | A 128-bit secret key. This should be generated randomly and must be kept
-- secret.
data SipKey = SipKey !Word64 !Word64
  deriving (Read, Show, Eq)

data SipState = SipState {
                    v0 :: !Word64
                  , v1 :: !Word64
                  , v2 :: !Word64
                  , v3 :: !Word64
                  -- when (number of bytes <= bytesRemaining) bytes come in, we
                  -- shift m left just enough to accomodate them.
                  , mPart  :: !Word64
                  , bytesRemaining :: !Word64  -- ^ bytes remaining for a full 'm'
                  , inlen :: !Word64  -- ^ we'll accumulate this as we consume
                  } deriving Eq

instance HashState SipState where
    mix8 st m = siphashForWord st m
    mix16 st m = siphashForWord st m
    mix32 st m = siphashForWord st m
    mix64 st m = siphashForWord st m


-- Corresponds to body of loop:
--   for ( ; in != end; in += 8 )
-- with special handling for the way we accumulate chunks of input until it
-- fills a Word64. For now we choose to hash in the data we've accumulated as
-- soon as an incoming chunk won't fit into mPart, rather than splitting the
-- incoming chunk and only hashing in "full" mparts. The issue with the latter
-- is we don't want to get "out of phase", e.g. we start receiving 64-bit
-- chunks while bytesRemaining == 4. 
--   TODO play with hashing different structures and benchmark, think of some
--   pathological inputs, experiment with splitting inputs but padding with a
--   single byte so that we eventually can get back into phase.
siphashForWord :: (Integral m, 
#                  if MIN_VERSION_base(4,7,0)
                    FiniteBits m
#                  else
                    Bits m
#                  endif
                   )=> SipState -> m -> SipState
{-# INLINE siphashForWord #-}
siphashForWord (SipState{ .. }) m = runIdentity $
  assert (bytesRemaining > 0 && bytesRemaining <= 8) $ 
    case compare bytesRemaining mSize of
         -- room in mPart with room leftover
         GT -> do mPart <- orMparts mPart m
                  bytesRemaining <- return $ bytesRemaining - mSize
                  inlen <- return $ inlen + mSize
                  return $ SipState{ .. }
         -- m will exactly fill mPart
         EQ -> do m <- orMparts mPart m
                  -- reset mPart:
                  let mPart = 0
                      bytesRemaining = 8
                  (v0,v1,v2,v3) <- sipMix v0 v1 v2 v3 m
                  inlen <- return $ inlen + mSize
                  return $ SipState{ .. }
         -- not enought room in mPart.
         LT | mSize == 8 -> do  -- ...and m fills next Word64 too.
                  -- first mix in our mPart (padded with zeros)
                  (v0,v1,v2,v3) <- sipMix v0 v1 v2 v3 mPart
                  -- ...then our m
                  (v0,v1,v2,v3) <- sipMix v0 v1 v2 v3 (fromIntegral m)
                  -- reset mPart:
                  let mPart = 0
                      bytesRemaining = 8
                  inlen <- return $ inlen + mSize
                  return $ SipState{ .. }
            | otherwise -> do
                  -- first mix in our mPart
                  (v0,v1,v2,v3) <- sipMix v0 v1 v2 v3 mPart
                  -- ...then pass along m as our mPart
                  let mPart = fromIntegral m
                      bytesRemaining = 8 - mSize
                  inlen <- return $ inlen + mSize
                  return $ SipState{ .. }
  where
    {-# INLINE mSizeBits #-}
    mSizeBits =
#    if MIN_VERSION_base(4,7,0)
      finiteBitSize m
#    else
      bitSize m
#    endif

    {-# INLINE mSize #-}
    mSize = case mSizeBits of  8 -> 1 ; 16 -> 2 ; 32 -> 4 ; 64 -> 8 ; _ -> error "Impossible size!"

    {-# INLINE orMparts #-}
    orMparts mPart m = return $ 
        (mPart `unsafeShiftL` mSizeBits) .|. (fromIntegral m)

    {-# INLINE sipMix #-}
    sipMix v0 v1 v2 v3 m = do
        v3 <- return $ v3 `xor` m
     -- for( i=0; i<cROUNDS; ++i ) SIPROUND;
        (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3
        (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3

        v0 <- return $ v0 `xor` m
        return (v0,v1,v2,v3)


-- | An implementation of 64-bit siphash-2-4.
--
-- This function is fast on 64-bit machines, and provides very good hashing
-- properties and protection against hash flooding attacks.
siphash64 :: Hashable a => SipKey -> a -> Hash64 a
{-# INLINE siphash64 #-}
siphash64 (SipKey k0 k1) = \a-> runIdentity $ do
    let v0 = 0x736f6d6570736575
        v1 = 0x646f72616e646f6d
        v2 = 0x6c7967656e657261
        v3 = 0x7465646279746573

    v3 <- return $ v3 `xor` k1;
    v2 <- return $ v2 `xor` k0;
    v1 <- return $ v1 `xor` k1;
    v0 <- return $ v0 `xor` k0;

    -- Initialize rest of SipState:
    let mPart = 0
        bytesRemaining = 8
        inlen = 0
    SipState{ .. } <- return $ hash (SipState { .. }) a

    let !b = inlen `unsafeShiftL` 56
    b <- return $ b .|. mPart

    v3 <- return $ v3 `xor` b
    -- for( i=0; i<cROUNDS; ++i ) SIPROUND;
    (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3
    (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3
    v0 <- return $ v0 `xor` b

    -- 0xff may be "Any non-zero value":
    v2 <- return $ v2 `xor` 0xff

--   for( i=0; i<dROUNDS; ++i ) SIPROUND;
    (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3
    (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3
    (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3
    (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3

    return $! Hash64 $! v0 `xor` v1 `xor` v2 `xor` v3



-- TODO if we extend this approach beyond 128-bits, then re-combine as much as
-- possible (at least factor out up until final mixing.

-- | An implementation of 128-bit siphash-2-4.
--
-- This function is fast on 64-bit machines, and provides very good hashing
-- properties and protection against hash flooding attacks.
siphash128 :: Hashable a => SipKey -> a -> Hash128 a
{-# INLINE siphash128 #-}
siphash128 (SipKey k0 k1) = \a-> runIdentity $ do
    let v0 = 0x736f6d6570736575
        v1 = 0x646f72616e646f6d
        v2 = 0x6c7967656e657261
        v3 = 0x7465646279746573

    v3 <- return $ v3 `xor` k1;
    v2 <- return $ v2 `xor` k0;
    v1 <- return $ v1 `xor` k1;
    v0 <- return $ v0 `xor` k0;

    -- N.B. ADDED in 128:
    v1 <- return $ v1 `xor` 0xee

    -- Initialize rest of SipState:
    let mPart = 0
        bytesRemaining = 8
        inlen = 0
    SipState{ .. } <- return $ hash (SipState { .. }) a

    let !b = inlen `unsafeShiftL` 56
    b <- return $ b .|. mPart

    v3 <- return $ v3 `xor` b
    -- for( i=0; i<cROUNDS; ++i ) SIPROUND;
    (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3
    (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3
    v0 <- return $ v0 `xor` b

    -- N.B. 0xff CHANGED to 0xee in 128:
    -- 0xee may be "Any non-zero value":
    v2 <- return $ v2 `xor` 0xee

--   for( i=0; i<dROUNDS; ++i ) SIPROUND;
    (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3
    (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3
    (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3
    (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3

    let !b0 = v0 `xor` v1 `xor` v2 `xor` v3

    -- N.B. ADDED in 128:
    v1 <- return $ v1 `xor` 0xdd
--   for( i=0; i<dROUNDS; ++i ) SIPROUND;
    (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3
    (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3
    (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3
    (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3

    let !b1 = v0 `xor` v1 `xor` v2 `xor` v3

    return $! Hash128 b0 b1
