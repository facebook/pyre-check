#!/usr/bin/env bash

set -x
set -e

ROOT="$(mktemp -d)"
# Only the "client" subdirectory includes files we want to check.
cp -a client "${ROOT}"
pushd "${ROOT}"
TERM=dumb pyre --source-directory . --search-path "${VIRTUAL_ENV}/lib/python3.6/site-packages" check
popd
