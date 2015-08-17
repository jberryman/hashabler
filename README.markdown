# Hashabler [![Build Status](https://travis-ci.org/jberryman/hashabler.svg)](https://travis-ci.org/jberryman/hashabler) 

This is a haskell library for principled, cross-platform & extensible hashing
of types, which includes an implementation of the FNV-1a algorithm. It is a
rewrite of the [hashable](http://hackage.haskell.org/package/hashable) library 
by Milan Straka and Johan Tibell, having the following goals:

- Extensibility; it should be easy to implement a new hashing algorithm on any
  Hashable type, for instance if one needed more hash bits

- Honest hashing of values, and principled hashing of algebraic data types (see
  e.g. hashable issues 30)

- Cross-platform consistent hash values, with a versioning guarantee. Where
  possible we ensure morally identical data hashes to indentical values
  regardless of processor word size and endianness.

- Make implementing identical hash routines in other languages as painless as
  possible. We provide an implementation of a simple hashing algorithm (FNV-1a)
  and make an effort define Hashable instances in a way that is well-documented
  and sensible, so that e.g. one can (hopefully) easily implement string
  hashing routine in JavaScript that will match the way we hash strings here.

It is available [on hackage](http://hackage.haskell.org/package/hashabler), and
can be installed with:

    cabal install hashabler

**Versioning**: Except for instances where we specifically note that we make no
promise of consistency, changes to hash values entail a major version number
bump.
