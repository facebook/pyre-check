#!/usr/bin/env bash

set -x
set -e

pip install pyre-check black
pyre --version
black --version
