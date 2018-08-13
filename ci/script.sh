#!/bin/sh

set -ex

# Newer libc's do not compile on rust 1.12
if [ "$TRAVIS_RUST_VERSION" = "1.12.0" ]; then
    cargo generate-lockfile
    cargo update -p libc --precise "0.2.41"
fi

cargo build --verbose
cargo doc --verbose

# If we're testing on an older version of Rust, then only check that we
# can build the crate. This is because the dev dependencies might be updated
# more frequently, and therefore might require a newer version of Rust.
#
# This isn't ideal. It's a compromise.
if [ "$TRAVIS_RUST_VERSION" = "1.12.0" ]; then
  exit
fi

cargo test --verbose
if [ "$TRAVIS_RUST_VERSION" = "nightly" ]; then
  cargo bench --verbose --no-run
fi
