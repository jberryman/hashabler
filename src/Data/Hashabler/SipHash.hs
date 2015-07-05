{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE RecordWildCards, BangPatterns, CPP #-}
module Data.Hashabler.SipHash (
    siphash
  ) where

-- We use the identity monad for non-recursive binding and utilize name
-- shadowing and RecordWildcards so we can easily translate the implicitly
-- stateful siphash reference implementation here:
--
-- https://github.com/veorq/SipHash/blob/master/siphash24.c
--  
--  TODO after we implement tests, play a bit with tweaking algorithm (e.g. order of independent operations)
--       look at core

-- TODO need to depend on mtl or transformers for base < 4.8
import Data.Functor.Identity
import Data.Word (Word64)
import Data.Bits
import Control.Exception(assert)

import Data.Hashabler.Internal


#ifdef EXPORT_INTERNALS
-- Our implementation of 'hash' for ByteString (and all variable-length types)
-- calls `mixConstructor` after hashing all bytes. We do an ugly hack here
-- where when testing we specialize siphash to operate on ByteString, and use
-- hashByteString directly so that we avoid the final mixConstructor and can
-- use the test vectors from the siphash reference implementation directly.
-- TODO once we bootstrap our implementation we can generate our own vectors
-- and then ditch this hack.
import Data.ByteString
#endif

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



type SipKey = (Word64,Word64)
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

instance Hash SipState where
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
    case compare fill 0 of
         -- room in mPart with room leftover
         GT -> do mPart <- orMparts mPart m
                  let bytesRemaining = fill
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
    mSizeBits =
#    if MIN_VERSION_base(4,7,0)
      finiteBitSize m
#    else
      bitSize m
#    endif

    mSize = case mSizeBits of  8 -> 1 ; 16 -> 2 ; 32 -> 4 ; 64 -> 8 ; _ -> error "Impossible size!"

    fill = assert (bytesRemaining > 0 && bytesRemaining <= 8) $ 
              bytesRemaining - mSize

    orMparts mPart m = return $ 
        (mPart `unsafeShiftL` mSizeBits) .|. (fromIntegral m)

    sipMix v0 v1 v2 v3 m = do
        v3 <- return $ v3 `xor` m
     -- for( i=0; i<cROUNDS; ++i ) SIPROUND;
        (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3
        (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3

        v0 <- return $ v0 `xor` m
        return (v0,v1,v2,v3)


# ifdef EXPORT_INTERNALS
-- | USERS SHOULD NOT SEE THIS SIGNATURE
-- specialized for testing. See note in imports.
siphash :: SipKey -> ByteString -> Word64
# else

-- | An implementation of 64-bit siphash-2-4.
--
-- TODO docs
siphash :: Hashable a => SipKey -> a -> Word64
# endif
{-# INLINE siphash #-}
siphash (k0,k1) = \a-> runIdentity $ do
    let v0 = 0x736f6d6570736575
        v1 = 0x646f72616e646f6d
        v2 = 0x6c7967656e657261
        v3 = 0x7465646279746573

{- TODO this at the end
The "end" byte:
--   const uint8_t *end = in + inlen - ( inlen % sizeof( uint64_t ) );
Bytes remaining after 64 bit chunks:
--   const int left = inlen & 7;
'left' pushed all the way left:
--   b = ( ( uint64_t )inlen ) << 56;
-}
    v3 <- return $ v3 `xor` k1;
    v2 <- return $ v2 `xor` k0;
    v1 <- return $ v1 `xor` k1;
    v0 <- return $ v0 `xor` k0;

-- TODO
-- #ifdef DOUBLE
--   v1 ^= 0xee;
-- #endif

    -- Initialize rest of SipState:
    let mPart = 0
        bytesRemaining = 8
        inlen = 0
    SipState{ .. } <- return $ 
#     ifdef EXPORT_INTERNALS
        -- specialized for testing. See note in imports.
        hashByteString
#     else
        hash 
#     endif
          (SipState { .. }) a


{- TODO
A this point we start to use length (which we should know at this point too):
This is folding in remaining bytes, but I don't totally understand this:
--   switch( left )
--   {
--   case 7: b |= ( ( uint64_t )in[ 6] )  << 48;
--   case 6: b |= ( ( uint64_t )in[ 5] )  << 40;
--   case 5: b |= ( ( uint64_t )in[ 4] )  << 32;
--   case 4: b |= ( ( uint64_t )in[ 3] )  << 24;
--   case 3: b |= ( ( uint64_t )in[ 2] )  << 16;
--   case 2: b |= ( ( uint64_t )in[ 1] )  <<  8;
--   case 1: b |= ( ( uint64_t )in[ 0] ); break;
--   case 0: break;
--   }
-}

    let !b = inlen `unsafeShiftL` 56
    -- TODO OR remaining bytes into `b` as per above

    v3 <- return $ v3 `xor` b
    -- for( i=0; i<cROUNDS; ++i ) SIPROUND;
    (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3
    (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3
    v0 <- return $ v0 `xor` b

{-
-- #ifndef DOUBLE
--   v2 ^= 0xff;
-- #else
--   v2 ^= 0xee;  -- TODO
-- #endif
-}
    -- 0xff may be "Any non-zero value":
    v2 <- return $ v2 `xor` 0xff

--   for( i=0; i<dROUNDS; ++i ) SIPROUND;
    (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3
    (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3
    (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3
    (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3

    return $! v0 `xor` v1 `xor` v2 `xor` v3

{- TODO
-- #ifdef DOUBLE
--   v1 ^= 0xdd;

--   for( i=0; i<dROUNDS; ++i ) SIPROUND;

--   b = v0 ^ v1 ^ v2  ^ v3;
--   U64TO8_LE( out+8, b );
-- #endif

--   return 0;
-- }
-}


