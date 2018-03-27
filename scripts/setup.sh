#!/bin/bash

# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -e
set -x

die() {
  printf '%s: %s\n' "$(basename "$0")" "$*" >&2
  exit 1
}

# Switch to pyre directory.
cd "$(dirname "$0")/.."

COMPILER="4.06.0"
# Parse arguments.
arguments=("$@")
for index in "${!arguments[@]}";
do
  case ${arguments[$index]} in
    "--local")
      if  [[ ! -d "$HOME/.opam" ]]; then
        mkdir -p "$HOME/local/opam"
        ln -s "$HOME/local/opam" "$HOME/.opam"
      fi
      OPAM_ROOT="$HOME/.opam/"
      ;;
    "--repository")
      OPAM_REPOSITORY="${arguments[$index+1]}"
      ;;
    "--configure")
      CONFIGURE=1
      ;;
    "--release")
      COMPILER="4.06.0+flambda"
      ;;
    "--build-type")
      REQUESTED_BUILD_TYPE="${arguments[$index+1]}"
      ;;
  esac
done

# Check if this is an internal build, and honor the user request if possible.
case "${REQUESTED_BUILD_TYPE}" in
  facebook)
    BUILD="facebook"
    ;;
  external)
    BUILD="external"
    ;;
  *)
    # This includes both the case in which the user made no specific
    # request, and invalid requests.
    if [ -e ".facebook" ]; then
      BUILD="facebook"
    else
      BUILD="external"
    fi
    ;;
esac

sed "s/%VERSION%/$BUILD/" Makefile.template > Makefile

# Abort early on `--configure`.
if [ -n "${CONFIGURE+x}" ]; then exit 0; fi

# Set default values.
if [ -z "${OPAM_ROOT+x}" ]; then OPAM_ROOT="$(mktemp -d)"; fi
if [ -z "${OPAM_REPOSITORY+x}" ]; then OPAM_REPOSITORY="https://opam.ocaml.org"; fi

# Extract packaged repository.
if [ ${OPAM_REPOSITORY: -7} == ".tar.gz" ]; then
  temporary_repository="$(mktemp -d)"
  tar xf "${OPAM_REPOSITORY}" -C "$temporary_repository" --strip-components=1

  # Get compiler if available.
  compiler="${OPAM_REPOSITORY%/*}/$COMPILER.tar.gz"
  if [ -e "$compiler" ]; then
    cp "$compiler" .
    trap 'rm -rf ./$COMPILER.tar.gz' EXIT
  fi

  OPAM_REPOSITORY=$temporary_repository
fi

# Seting up OCaml environment.
opam init --yes --compiler "$COMPILER" --root "$OPAM_ROOT" default "$OPAM_REPOSITORY" \
  && eval "$(opam config --root "$OPAM_ROOT" env)" \
  && opam update \
  && ocaml_succeeded=1
test $ocaml_succeeded = 1 \
  || die 'Unable to setup OCaml environment'

opam install --yes \
  core \
  yojson \
  ppx_deriving \
  ppx_deriving_yojson \
  ppx_compare \
  ppx_hash \
  ounit \
  menhir \
  sedlex \
  utop \
  && opam_install_dependencies_succeeded=1
test $opam_install_dependencies_succeeded = 1 \
  || die 'Could not install depenencies'

# Build and install hack parallel.
(cd hack_parallel \
  && OCAMLPARAM=_,annot=1,bin-annot=1,g=1 make \
  && make remove \
  && make install) \
  && install_hack_parallel=1
test $install_hack_parallel = 1 \
  || die 'Could not install hack_parallel'

# Build and run tests.
jobs="$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 1)"

make --jobs "$jobs" || die 'Could not build pyre'
make --jobs "$jobs" test || die 'Pyre tests failed'
