#!/usr/bin/env bash

set -x
set -e

pip install pyre-check black pywatchman ipython
pyre --version
black --version
