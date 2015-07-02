{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE RecordWildCards, BangPatterns #-}
module Data.Hashabler.SipHash
    where

-- We use the identity monad for non-recursive binding and utilize name
-- shadowing and RecordWildcards so we can easily translate the implicitly
-- stateful siphash reference implementation here:
--
-- https://github.com/veorq/SipHash/blob/master/siphash24.c

-- TODO need to depend on mtl or transformers for base < 4.8
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
rotl x b = (x `unsafeShiftL` b) .|. (x `unsafeShiftR` (64 - b))

-- #define U32TO8_LE(p, v)                                         \
--   (p)[0] = (uint8_t)((v)      ); (p)[1] = (uint8_t)((v) >>  8); \
--   (p)[2] = (uint8_t)((v) >> 16); (p)[3] = (uint8_t)((v) >> 24);

-- #define U64TO8_LE(p, v)                        \
--   U32TO8_LE((p),     (uint32_t)((v)      ));   \
--   U32TO8_LE((p) + 4, (uint32_t)((v) >> 32));

-- Here we're doing another byteswap before writing to a byte array. We can
-- omit that since we'll just produce a Word64
u64To8 = undefined

{- NOTE: no need for this; Hashabler instance will supply bytes "in order"
-- #define U8TO64_LE(p)            \
--   (((uint64_t)((p)[0])      ) | \
--    ((uint64_t)((p)[1]) <<  8) | \
--    ((uint64_t)((p)[2]) << 16) | \
--    ((uint64_t)((p)[3]) << 24) | \
--    ((uint64_t)((p)[4]) << 32) | \
--    ((uint64_t)((p)[5]) << 40) | \
--    ((uint64_t)((p)[6]) << 48) | \
--    ((uint64_t)((p)[7]) << 56))
-- Reads a Word64 and reverses if little endian
-- TODO a test for little endian on 32-bit, make sure it behaves the same
-- TODO make this a utility and use elsewhere:
-- NOTE: this is a pointer to a Word64, instead of a byte, so we'll need to + 1, not + 8
u8To64 :: Ptr Word64 -> IO Word64
u8To64 ptr = do
    w64Dirty <- peekByteOff base ix
    return $! if littleEndian
                then byteSwap64 w64Dirty
                else w64Dirty
-}

-- #define SIPROUND                                        \
--   do {                                                  \
--     v0 += v1; v1=ROTL(v1,13); v1 ^= v0; v0=ROTL(v0,32); \
--     v2 += v3; v3=ROTL(v3,16); v3 ^= v2;                 \
--     v0 += v3; v3=ROTL(v3,21); v3 ^= v0;                 \
--     v2 += v1; v1=ROTL(v1,17); v1 ^= v2; v2=ROTL(v2,32); \
--   } while(0)
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



-- int  siphash( uint8_t *out, const uint8_t *in, uint64_t inlen, const uint8_t *k )
-- {
--   /* "somepseudorandomlygeneratedbytes" */
--   uint64_t v0 = 0x736f6d6570736575ULL;
--   uint64_t v1 = 0x646f72616e646f6dULL;
--   uint64_t v2 = 0x6c7967656e657261ULL;
--   uint64_t v3 = 0x7465646279746573ULL;
--   uint64_t b;
{-
Extract the two 64-bit portions of the keys:
-}
--   uint64_t k0 = U8TO64_LE( k );
--   uint64_t k1 = U8TO64_LE( k + 8 );
--   uint64_t m;
--   int i;
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
                  , accumulatedLength :: !Word64
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
                    accumulatedLength <- return $ accumulatedLength + 8
                    return $ SipState{ .. }
                    
             _ -> undefined -- TODO
              {-
                  pad with zeros in anticipation of the next being 64 bits?
                  or split?
                  or insert a single padding byte and split (hoping that this will get us out of bad phase)
                    (can we do this in a way that is compatible with the way we handle remaining bytes?)
                    ( we don't have to worry about this messing with the spec since our only leftovers should be the final bytes.)
                    -}

siphash :: Hashable a=> a -> SipKey -> Word64
siphash a (k0,k1) = runIdentity $ do
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

--   v3 ^= k1;
--   v2 ^= k0;
--   v1 ^= k1;
--   v0 ^= k0;
    v3 <- return $ v3 `xor` k1;
    v2 <- return $ v2 `xor` k0;
    v1 <- return $ v1 `xor` k1;
    v0 <- return $ v0 `xor` k0;

-- TODO
-- #ifdef DOUBLE
--   v1 ^= 0xee;
-- #endif

{-
Process all 64-bit chunks (n.b. no use of b or inlen at this point (except for
pointer arithmetic)):
-}
--   for ( ; in != end; in += 8 )
--   {
--     m = U8TO64_LE( in );
--     v3 ^= m;

--     for( i=0; i<cROUNDS; ++i ) SIPROUND;

--     v0 ^= m;
--   }
    SipState{ .. } <- return $ hash (SipState v0 v1 v2 v3 0 8 0) a


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

{-
Do some final mixing
--   v3 ^= b;
--   for( i=0; i<cROUNDS; ++i ) SIPROUND;
--   v0 ^= b;
-}
    let !b = accumulatedLength `unsafeShiftL` 56
    -- TODO OR remaining bytes into `b` as per above

    v3 <- return $ v3 `xor` b
    -- for( i=0; i<cROUNDS; ++i ) SIPROUND;
    (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3
    (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3
    v0 <- return $ v0 `xor` b

{-
0xff may be "Any non-zero value":
-- #ifndef DOUBLE
--   v2 ^= 0xff;
-- #else
--   v2 ^= 0xee;  -- TODO
-- #endif
-}
    v2 <- return $ v2 `xor` 0xFF

--   for( i=0; i<dROUNDS; ++i ) SIPROUND;
    (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3
    (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3
    (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3
    (v0,v1,v2,v3) <- return $ sipRound v0 v1 v2 v3

--   b = v0 ^ v1 ^ v2  ^ v3;
--   U64TO8_LE( out, b );
    return $ v0 `xor` v1 `xor` v2 `xor` v3

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


