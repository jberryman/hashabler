{-# LANGUAGE CPP #-}
module Data.Hashabler (
{- | 
  The core of this library consists of 
     
  - the 'Hashable' class which defines how hashable chunks of bytes are
    delivered to the data-consuming portion of a hash function; new instances
    can be defined to support the hashing of new datatypes using an existing
    algorithm

  - the 'HashState' class which implements the data-consuming portion of a
    particular hashing algorithm, consuming bytes delivered in 'hash'; a new
    instance can be defined to implement a new hashing function that works on
    existing 'Hashable' types.

  We also include implementations for the following hash functions:
  'hashFNV32', 'hashFNV64', 'siphash64', and 'siphash128'.

  Please see the project description for more information, including motivation.
 -}

  -- * Hash Functions
  -- | Hashes of different widths.
    Hash32(..), Hash64(..), Hash128(..)
  
  -- ** Hashing with the SipHash algorithm
{- | 
 #siphash#
 
 SipHash is a fast hashing algorithm with very good mixing properties, designed
 to be very secure against hash-flooding DOS attacks. SipHash is a good choice
 whenever your application may be hashing untrusted user data.
-}
  , SipKey
  , siphash64
  , siphash128

  -- ** Hashing with the FNV-1a algorithm
  -- | The FNV-1a hash (see <http://www.isthe.com/chongo/tech/comp/fnv/>) is
  -- a fast and extremely simple hashing algorithm with fairly good mixing
  -- properties. Its simplicity makes it a good choice if you need to implement
  -- the same hashing routines on multiple platforms e.g. to verify a hash
  -- generated in JS on a web client with a hash stored on your server.
  --
  -- If you are hashing untrusted user data and are concerned with hash
  -- flooding attacks, <#siphash consider SipHash instead>; performance is
  -- about the same in the current implementation.
  , hashFNV32
  , hashFNV64
  -- *** FNV-1a Internal Parameters
  -- | Magic FNV primes:
  , fnvPrime32
  , fnvPrime64
  -- | The arbitrary initial seed values for different output hash sizes. These
  -- values are part of the spec, but there is nothing special about them;
  -- supposedly, in terms of hash quality, any non-zero value seed should be
  -- fine passed to 'hash':
  , fnvOffsetBasis32
  , fnvOffsetBasis64


  -- * Hashable types
  , Hashable(..)
  -- ** Creating your own Hashable instances
{- | 
 #principled#

 When defining 'Hashable' instances for your own algebraic data types you
 should do the following.

 For types with /a single constructor/, simply call 'hash' on each of the
 constructor's children, for instance:

 > instance (Hashable a, Hashable b, Hashable c) => Hashable (a, b, c) where
 >     hash h (a,b,c) = h `hash` a `hash` b `hash` c

 And when a type has /multiple constructors/ you should additionally call
 'mixConstructor' with a different argument for each constructor.

 > instance (Hashable a, Hashable b) => Hashable (Eithers a b) where
 >     hash h (Lefts a0 a1)     = mixConstructor 0 (h `hash` a0 `hash` a1)
 >     hash h (Rights b0 b1 b2) = mixConstructor 1 (h `hash` b0 `hash` b1 `hash` b2)

 In the future we may offer a way to derive instances like this automatically.
-}
  , mixConstructor
  
  -- * Implementing new hash functions
  , HashState(..)

  -- ** Detailed discussion of principled Hashable instances
{- |
 This is a work-in-progress, and purely IYI.

 Special care needs to be taken when defining instances of Hashable for your
 own types, especially for recursive types and types with multiple
 constructors. First instances need to ensure that 
 /distinct values produce distinct hash values/. Here's an example of a /bad/
 implementation for 'Maybe':
 
 > instance (Hashable a)=> Hashable (Maybe a) where -- BAD!
 >     hash h (Just a) = h `hash` a                 -- BAD!
 >     hash h Nothing  = h `hash` (1::Word8)        -- BAD!

 Here @Just (1::Word8)@ hashes to the same value as @Nothing@.

 Second and more tricky, instances should not permit a function 
 @f :: a -> (a,a)@ such that 
 @x `hash` y == x `hash` y1 `hash` y2 where (y1,y2) = f y@... or something.
 The idea is we want to avoid the following kinds of collisions:

 > hash [Just 1, Nothing] == hash [Just 1]          -- BAD!
 > hash ([1,2], [3])      == hash ([1], [2,3])      -- BAD!

 Maybe what we mean is that where @a@ is a 'Monoid', we expect replacing
 `mappend` with the hash operation to always yield /different/ values. This
 needs clarifying; please help.

 Here are a few rules of thumb which should result in principled instances for
 your own types (This is a work-in-progress; please help):

 - If all values of a type have a static structure, i.e. the arrangement and
   number of child parts to be hashed is knowable from the type, then one may
   simply hash each child element of the type in turn. This is the case for
   product types like tuples (where the arity is reflected in the type), or
   primitive numeric values composed of a static number of bits.

 Otherwise if the type has variable structure, e.g. if it has multiple
 constructors or is an array type...

 - Every possible value of a type should inject at least one byte of entropy
   /apart/ from any recursive calls to child elements; we can ensure this is
   the case by hashing an initial or final distinct byte for each distinct
   constructor of our type

 To ensure hashing remains consistent across platforms, instances should not
 compile-time-conditionally call different @mix@-family 'HashState' functions.
 This rule doesn't matter for instances like 'FNV32' which mix in data one byte
 at a time, but other 'HashState' instances may operate on multiple bytes at a time,
 perhaps using padding bytes, so this becomes important.

 A final important note: we're not concerned with collisions between values of
 /different types/; in fact in many cases "equivalent" values of different
 types intentionally hash to the same value. This also means instances cannot
 rely on the hashing of child elements being uncorrelated. That might be one
 interpretation of the mistake in our faulty @Maybe@ instance above
 -}
  
  
  
#ifdef EXPORT_INTERNALS
  -- * Internal functions exposed for testing; you shouldn't see these
  , hashFoldl'
  , hashLeftUnfolded
  , magnitudeAsWord
  , FNV32(..), FNV64(..)
  , bytes32, bytes64, floatToWord, doubleToWord
  , _byteSwap32, _byteSwap64, _hash32Integer, _hash32_Word_64, _hash32_Int_64
  , _bytes64_32 , _bytes64_64, _signByte
#endif
    ) where

import Data.Hashabler.Internal
import Data.Hashabler.SipHash
