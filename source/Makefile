# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

.PHONY: all
all: configure dev

.PHONY: dev
dev:
	@../scripts/generate-version-number.sh development
	dune build @install -j auto --profile dev

.PHONY: test
test: all
	@OUNIT_SHARDS="1" dune runtest -j auto --profile dev

.PHONY: stubs_integration_test
stubs_integration_test: all
	make -C ../stubs/integration_test

.PHONY: pysa_cache_test
pysa_cache_test: all
	PYRE_BINARY="$(shell pwd)/_build/default/main.exe" ../stubs/integration_test/run_cache_test.py

.PHONY: fbtest
fbtest: all
	if [ -d "$(shell pwd)/facebook" ]; then make -C ../facebook; fi

.PHONY: release
release:
	@../scripts/generate-version-number.sh
	dune build @install -j auto --profile release

.PHONY: clean
clean:
	dune clean
	@if [ -f dune ]; then rm dune; fi

.PHONY: configure
configure: dune;

dune: dune.in
	../scripts/setup.sh --configure
