# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

export OCAMLFIND_IGNORE_DUPS_IN=$(dir $(OCAML_TOPLEVEL_PATH))ocaml/compiler-libs
export MACOSX_DEPLOYMENT_TARGET=10.11
export HACK_OPTIONS="--directory=hack_parallel --file=Makefile.hack_parallel"

.PHONY: all
all: configure dev

.PHONY: dev
dev: hack_parallel
	@./scripts/generate-version-number.sh development
	dune build @install -j auto --profile dev

.PHONY: test
test: hack_parallel
	@OUNIT_SHARDS="1" dune runtest -j auto --profile dev

.PHONY: python_tests
python_tests:
	./scripts/run-python-tests.sh

.PHONY: server_integration_test
server_integration_test: all
	PYRE_BINARY="$(shell pwd)/_build/default/main.exe" ./scripts/run_integration_test.py command/test/integration/fake_repository/

.PHONY: facebook_integration_tests
facebook_integration_tests: all
	if [ -d "$(shell pwd)/facebook" ]; then make -C facebook; fi

.PHONY: release
release: hack_parallel
	@./scripts/generate-version-number.sh
	dune build @install -j auto --profile release

.PHONY: clean
clean: clean_hack_parallel
	dune clean
	@if [ -f dune ]; then rm dune; fi

.PHONY: clean_hack_parallel
clean_hack_parallel:
	@make "$(HACK_OPTIONS)" clean && make "$(HACK_OPTIONS)" remove

.PHONY: hack_parallel
hack_parallel:
	@if [ ! -d hack_parallel/_build ]; then echo 'Hack_parallel is not installed...';\
	make "$(HACK_OPTIONS)"; make "$(HACK_OPTIONS)" install; fi

.PHONY: configure
configure: dune;

dune: dune.in
	./scripts/setup.sh --configure

.PHONY: lint
lint:
	find -name "*.ml" | grep -v build | grep -v hack | xargs ocp-indent -i
