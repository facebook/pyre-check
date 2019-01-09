#/usr/bin/env sh

set -x
set -e

pip install pyre-check black
pyre --version
black --version
