#!/usr/bin/env bash

set -x
set -e

pip install pyre-check black pywatchman psutil ipython libcst
pyre --version
black --version
