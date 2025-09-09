# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

.PHONY: dev
dev: configure
	@../scripts/generate-version-number.sh development
	dune build @install -j auto --profile dev

.PHONY: test
test: dev
	PYRE_CODE_ROOT="$(CURDIR)/.." OUNIT_SHARDS="1" dune runtest -j auto --profile dev

.PHONY: stubs_integration_test
stubs_integration_test: dev
	make -C ../stubs/integration_test

.PHONY: pysa_cache_test
pysa_cache_test: dev
	PYRE_BINARY="$(shell pwd)/_build/default/main.exe" ../stubs/integration_test/run_cache_test.py

.PHONY: fbtest
fbtest: dev
	if [ -d "$(shell pwd)/facebook" ]; then make -C ../facebook; fi

.PHONY: release
release:
	@../scripts/generate-version-number.sh
	dune build @install -j auto --profile release

.PHONY: release_test
release_test: release
	@OUNIT_SHARDS="1" dune runtest -j auto --profile release

.PHONY: clean
clean:
	dune clean
	@if [ -f dune ]; then rm dune; fi
	@if [ -f interprocedural_analyses/taint/test/dune ]; then rm interprocedural_analyses/taint/test/dune; fi

.PHONY: configure
configure: dune;

dune: dune.in interprocedural_analyses/taint/test/dune.in $(wildcard **/dune)
	../scripts/setup.sh --configure
	dune build @fmt --auto-promote || true

interprocedural_analyses/taint/test/dune: dune
