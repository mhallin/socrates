#!/bin/sh

set -e

(cd socrates-cli && cargo build)

(cd socrates-core && cargo test)
