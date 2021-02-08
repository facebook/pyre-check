#!/bin/bash
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Source this file to run it:
# . ./setup.sh

# This file should ONLY include standard setup steps which would be used by a
# real Flask project. Do not add anything Pysa/Pyre specific.

python3 -m venv venv
# shellcheck disable=SC1091
. venv/bin/activate
pip install Flask
rm ../../.pyre_configuration
