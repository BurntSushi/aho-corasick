**UNDER DEVELOPMENT**

This crate provides a fast implementation of the
[Aho-Corasick](http://en.wikipedia.org/wiki/Aho%E2%80%93Corasick_string_matching_algorithm)
algorithm. Its intended use case is for fast substring matching, particularly
when matching multiple substrings in a search text. This is achieved by
compiling the substrings into a finite state machine.

This implementation provides optimal algorithmic time complexity. Construction
of the finite state machine is `O(p)` where `p` is the length of the substrings
concatenated. Matching against search text is `O(n + p + m)`, where `n` is
the length of the search text and `m` is the number of matches.

[![Build status](https://api.travis-ci.org/BurntSushi/aho-corasick.png)](https://travis-ci.org/BurntSushi/aho-corasick)
[![](http://meritbadge.herokuapp.com/aho-corasick)](https://crates.io/crates/aho-corasick)

Dual-licensed under MIT or the [UNLICENSE](http://unlicense.org).


### Documentation

[http://burntsushi.net/rustdoc/aho-corasick/](http://burntsushi.net/rustdoc/aho-corasick/).


### Alternatives

Aho-Corasick is useful for matching multiple substrings against many long
strings. If your long string is fixed, then you might consider building a
[suffix array](https://github.com/BurntSushi/suffix)
of the search text (which takes `O(n)` time). Matches can then be found in
`O(plogn)` time.
