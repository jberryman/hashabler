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
                  , bytesRemaining :: !Int  -- ^ bytes remaining for a full 'm'
                  , inlen :: !Word64  -- ^ we'll accumulate this as we consume
                  } deriving Eq

-- Corresponds to for loop:
--   for ( ; in != end; in += 8 )
instance Hash SipState where
    mix8 SipState{ .. } =  assert (bytesRemaining > 0 && bytesRemaining <= 8) $
        undefined -- TODO
    mix16 SipState{ .. } = assert (bytesRemaining > 0 && bytesRemaining <= 8) $
        undefined -- TODO
    mix32 SipState{ .. } = assert (bytesRemaining > 0 && bytesRemaining <= 8) $
        undefined -- TODO
    mix64 SipState{ .. } m = assert (bytesRemaining > 0 && bytesRemaining <= 8) $
        case bytesRemaining of
             8 -> runIdentity $ do
                    v3 <- return $ v3 `xor` m
                 -- for( i=0; i<cROUNDS; ++i ) SIPROUND;
                    (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3
                    (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3

                    v0 <- return $ v0 `xor` m
                    inlen <- return $ inlen + 8
                    return $ SipState{ .. }
                    
             _ -> undefined -- TODO
              {-
                  pad with zeros in anticipation of the next being 64 bits?
                  or split?
                  or insert a single padding byte and split (hoping that this will get us out of bad phase)
                    (can we do this in a way that is compatible with the way we handle remaining bytes?)
                    ( we don't have to worry about this messing with the spec since our only leftovers should be the final bytes.)
                    -}

# ifdef EXPORT_INTERNALS
-- | USERS SHOULD NOT SEE THIS SIGNATURE
-- specialized for testing. See note in imports.
siphash :: SipKey -> ByteString -> Word64
# else

-- | TODO docs
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


