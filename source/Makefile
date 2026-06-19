# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

.PHONY: dev
dev: pyrefly
	@../scripts/generate-version-number.sh development
	dune build @install -j auto --profile dev

.PHONY: test
test: dev
	PYRE_CODE_ROOT="$(CURDIR)/.." OUNIT_SHARDS="4" dune runtest -j auto --profile dev --always-show-command-line

.PHONY: stubs_integration_test
stubs_integration_test: dev
	make -C ../stubs/integration_test

.PHONY: pysa_cache_test
pysa_cache_test: dev
	PYRE_BINARY="$(CURDIR)/_build/default/main.exe" PYREFLY_BINARY="$${PYREFLY_BINARY:-$(CURDIR)/pyrefly.exe}" ../stubs/integration_test/run_cache_test.py

.PHONY: fbtest
fbtest: dev
	if [ -d "$(CURDIR)/facebook" ]; then make -C ../facebook; fi

.PHONY: release
release: pyrefly
	@../scripts/generate-version-number.sh
	dune build @install -j auto --profile release

.PHONY: release_test
release_test: release
	@PYRE_CODE_ROOT="$(CURDIR)/.." OUNIT_SHARDS="4" dune runtest -j auto --profile release --always-show-command-line

.PHONY: clean
clean:
	dune clean
	rm -f dune interprocedural_analyses/taint/test/dune pyrefly.exe .build_type

.PHONY: pyrefly
pyrefly: configure
	@if [ ! -f .build_type ]; then \
		echo "error: source/.build_type not found; run ./facebook/scripts/setup.sh --local (Meta) or ./scripts/setup.sh --local (OSS) before 'make'." >&2; \
		exit 1; \
	fi
	@if [ "$$(cat .build_type)" = "facebook" ]; then fbpython ../facebook/scripts/refresh_pyrefly.py; fi

.PHONY: configure
configure: dune;

dune: dune.in interprocedural_analyses/taint/test/dune.in $(wildcard **/dune)
	../scripts/setup.sh --configure
	dune build @fmt --auto-promote || true

interprocedural_analyses/taint/test/dune: dune
