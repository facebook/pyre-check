#!/usr/bin/env bash

set -x
set -e

pip install pyre-check black==19.3b0 pywatchman psutil ipython libcst
pyre --version
black --version
