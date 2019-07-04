#!/bin/sh

set -ex

cargo build --verbose
cargo doc --verbose
cargo test --verbose
if [ "$TRAVIS_RUST_VERSION" = "stable" ]; then
  rustup component add rustfmt
  cargo fmt -- --check
fi
if [ "$TRAVIS_RUST_VERSION" = "nightly" ]; then
  cargo build --manifest-path aho-corasick-debug/Cargo.toml
  cargo bench --verbose --manifest-path bench/Cargo.toml -- --test
fi
