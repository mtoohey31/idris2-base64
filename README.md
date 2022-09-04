# idris2-base64

A base64 library for [Idris2](https://github.com/idris-lang/Idris2). Currently a WIP: some `partial` helper functions are used in combination with `assert_total` when doing bit-shifts that should be provably total. I just need to figure out how to write the proofs; I'm reasonably confident that the implementation is correct.
