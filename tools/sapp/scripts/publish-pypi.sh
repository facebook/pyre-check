#! /bin/sh
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Script to publish package to PyPI. Note that version bump
# in `setup.py` is manual and the script assumes you have
# `wheel` and `twine` packages installed.

set -e
set -x

cd "$(dirname "$0")/.." || exit 1

(cd sapp/ui/frontend && npm run build)

rm -rf dist
python setup.py sdist bdist_wheel
python -m twine upload dist/*
