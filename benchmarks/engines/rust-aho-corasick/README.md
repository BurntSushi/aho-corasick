This directory contains a Rust runner program for benchmarking the
[`aho-corasick` crate][rust-aho-corasick]. The `aho-corasick` crate
principally implements the [Aho-Corasick algorithm][aho-corasick], although
it has other algorithms for multiple substring search, such as [Teddy], which
was ported from the Hyperscan project.

The `aho-corasick` crate is used by [Rust's `regex` crate][rust-regex] to
implement fast prefilters that permit finding candidates very quickly and only
needing to use the regex engine to confirm the match. The Teddy algorithm is
particularly excellent here. (Sometimes `aho-corasick` is used as the regex
engine itself, for example, when the regex is just an alternation of literals.)

Since the `aho-corasick` crate only supports searching for literal strings, this
engine should only be used for regex patterns that are literals. This is up to
the author of the benchmark definition, as this runner program will always
treat regex patterns as literals.

This also means that this runner program cannot support all benchmark models.
Only the `compile`, `count`, `count-spans` and `grep` models are supported.

Finally, this runner program supports measuring two different Aho-Corasick
implementations: `nfa` and `dfa`. The former follows failure transitions at
search time and is thus usually slower, where as the latter builds a full
transition table by pre-computing all failure transitions. The latter tends
to be faster at search time, but can use orders (plural) of magnitude more
memory. In both the `nfa` and `dfa` engines, prefilters inside of Aho-Corasick
are disabled.

[rust-aho-corasick]: https://github.com/BurntSushi/aho-corasick
[aho-corasick]: https://en.wikipedia.org/wiki/Aho%E2%80%93Corasick_algorithm
[Teddy]: https://github.com/BurntSushi/aho-corasick/tree/4e7fa3b85dd3a3ce882896f1d4ee22b1f271f0b4/src/packed/teddy
[rust-regex]: https://github.com/rust-lang/regex
