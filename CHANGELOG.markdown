1.3.0
---
* Performance improvements
* Implement a faster siphash-1-3

1.2.1
---
* Eq, Read, Show for SipKey

1.2
---
* SipKey is now a new data type with unboxed fields, instead of tuple
* export a new mixType helper for defining StableHashable instances

1.1
---
* Parameterize `Hash32/64/128` types by the type of their hashed source data,
  to enforce legitimate equality comparisons of hashes.
* Add `StableHashable` for distinguishing types with respect to their hashes
  across platforms and programs.

1.0
---
* Added a `mix64` method, in use in, `Word/Int64` `ByteString` types and `P.ByteArray`
* `Hash` renamed `HashState` and roll clarified in docs
* `instance Hashable` of `Float` and `Double` now use `mix32` and `mix64` respectively.
* Implemented 64-bit and 128-bit siphash
* Instances up to 15-tuples. Now web scale!

0.1.0.2
---
* (initial release)

