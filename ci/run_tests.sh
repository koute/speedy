#!/bin/bash

set -euo pipefail
IFS=$'\n\t'

export RUST_BACKTRACE=1
export QUICKCHECK_TESTS=5000

set +e
echo "$(rustc --version)" | grep -q "nightly"
if [ "$?" = "0" ]; then
    export IS_NIGHTLY=1
else
    export IS_NIGHTLY=0
fi
set -e

echo "Is Rust from nightly: $IS_NIGHTLY"

cargo check --no-default-features
cargo build

if [ "$IS_NIGHTLY" = "1" ]; then
    cargo test --features external_doc
    cargo test -p static-tests --release
else
    cargo test
fi

cd speedy-derive
cargo test
