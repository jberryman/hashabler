{-# LANGUAGE CPP #-}
module Data.Hashabler (
{- | 
  The core of this library consists of 
     
  - the 'Hashable' class which defines how hashable chunks of bytes are
    delivered to a hash function; new instances can be defined to support
    the hashing of new datatypes using an existing algorithm

  - the 'Hash' class which implements a particular hashing algorithm,
    consuming bytes delivered in 'hash'; new instances can be defined to
    support hashing existing 'Hashable' types with a new algorithm.

  Currently we implement only the 32 and 64-bit variations of the 
  <http://www.isthe.com/chongo/tech/comp/fnv/ FNV-1a non-cryptographic hashing algorithm> 
  ('hashFNV32' and 'hashFNV64'), which have good hashing properties and are
  easy to implement in different languages and on different platforms.

  Please see the project description for more information.
 -}
  -- * The Hashable and Hash classes
    Hashable(..)
  , Hash(..)

  -- * Hashing with the FNV-1a algorithm
  , FNV32(..)
  , hashFNV32
  , FNV64(..)
  , hashFNV64
  -- ** Internals
  -- *** FNV Primes
  , fnvPrime32
  , fnvPrime64
  -- *** Standard seed values
  -- | The arbitrary initial seed values for different output hash sizes. These
  -- values are part of the spec, but there is nothing special about them;
  -- supposedly, in terms of hash quality, any non-zero value seed should be
  -- fine passed to 'hash':
  , fnvOffsetBasis32
  , fnvOffsetBasis64
  
  -- * Hashing with the SipHash algorithm
  -- | SipHash is a fast hashing algorithm with very good mixing properties,
  -- designed to be very secure against hash-flooding DOS attacks. SipHash is a
  -- good choice whenever your application is hashing untrusted user data.
  , SipHash64(..), siphash64
  , SipHash128(..), siphash128
  , SipKey

  -- * Creating Hash and Hashable instances
  , mixConstructor
  -- ** Defining principled Hashable instances
{- | 
 #principled#

 Special care needs to be taken when defining instances of Hashable for your
 own types, especially for recursive types and types with multiple
 constructors. First instances need to ensure that /distinct values produce
 distinct hash values/. Here's an example of a /bad/ implementation for 'Maybe':
 
 > instance (Hashable a)=> Hashable (Maybe a) where              -- BAD!
 >     hash h (Just a) = h `hash` a          -- BAD!
 >     hash h Nothing  = h `hash` (1::Word8) -- BAD!

 Here @Just (1::Word8)@ hashes to the same value as @Nothing@.

 Second and more tricky, instances should not permit a function 
 @f :: a -> (a,a)@ such that 
 @x `hash` y == x `hash` y1 `hash` y2 where (y1,y2) = f y@... or something.
 The idea is we want to avoid the following kinds of collisions:

 > hash [Just 1, Nothing] == hash [Just 1]     -- BAD!
 > hash ([1,2], [3])      == hash ([1], [2,3]  -- BAD!)

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
 compile-time-conditionally call different @mix@-family 'Hash' functions.
 This rule doesn't matter for instances like 'FNV32' which mix in data one byte
 at a time, but other 'Hash' instances may operate on multiple bytes at a time,
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
  , bytes32, bytes64, floatToWord, doubleToWord
  , _byteSwap32, _byteSwap64, _hash32Integer, _hash32_Word_64, _hash32_Int_64
  , _bytes64_32 , _bytes64_64, _signByte
#endif
    ) where

import Data.Hashabler.Internal
import Data.Hashabler.SipHash
