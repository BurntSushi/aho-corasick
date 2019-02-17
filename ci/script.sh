#!/bin/sh

set -ex

cargo build --verbose
cargo doc --verbose
cargo test --verbose
if [ "$TRAVIS_RUST_VERSION" = "nightly" ]; then
  cargo bench --verbose --manifest-path bench/Cargo.toml -- --test
fi
