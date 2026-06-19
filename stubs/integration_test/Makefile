# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

.PHONY: all
all:
	# Pysa integration test.
	PYRE_BINARY="$(CURDIR)/../../source/_build/default/main.exe" PYREFLY_BINARY="$${PYREFLY_BINARY:-$(CURDIR)/../../source/pyrefly.exe}" ./run.py
