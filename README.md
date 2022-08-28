# idris2-base64

A base64 library for [Idris2](https://github.com/idris-lang/Idris2). Currently a WIP:

- Some `partial` helper functions are used in combination with `assert_total` when doing bit-shifts that should be provably total. I just need to figure out how to write the proofs; I'm reasonably confident that the implementation is correct.
- Only `tryAtob` and `tryAtob'` are available for decoding. I would also like to make `atob` and `atob'` available, which would require proof that the string/character list is well-formed base64 data. Again, this requires some work with proofs.
