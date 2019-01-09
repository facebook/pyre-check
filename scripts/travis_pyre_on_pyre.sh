#/usr/bin/env sh

set -x
set -e

ROOT="$(mktemp -d)"
# Only the "client" subdirectory includes files we want to check.
cp -a client "${ROOT}"
pushd "${ROOT}"
TERM=dumb pyre --source-directory . check
popd
