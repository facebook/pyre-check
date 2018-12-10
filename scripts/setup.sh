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

COMPILER_VERSION="4.06.0"
DEVELOPMENT_COMPILER="${COMPILER_VERSION}"
RELEASE_COMPILER="${COMPILER_VERSION}+flambda"
MAKE_ARGUMENTS=""

# Compatibility settings with MacOS.
if [[ "${MACHTYPE}" = *apple* ]]; then
  export MACOSX_DEPLOYMENT_TARGET=10.11
fi

# Switch to pyre directory.
cd "$(dirname "$0")/.."

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
    "--temporary-root")
      OPAM_ROOT="$(mktemp -d)"
      ;;
    "--opam-root")
      OPAM_ROOT="${arguments[$index+1]}"
      ;;
    "--repository")
      OPAM_REPOSITORY="${arguments[$index+1]}"
      ;;
    "--configure")
      CONFIGURE=1
      ;;
    "--environment-only")
      ENVIRONMENT_ONLY=1
      ;;
    "--development")
      COMPILER="${DEVELOPMENT_COMPILER}"
      ;;
    "--release")
      COMPILER="${RELEASE_COMPILER}"
      MAKE_ARGUMENTS="release"
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
    if [ -e "facebook" ]; then
      BUILD="facebook"
    else
      BUILD="external"
    fi
    ;;
esac

# Set default values.
OPAM_ROOT="${OPAM_ROOT:-${HOME}/.opam/}"
OPAM_REPOSITORY="${OPAM_REPOSITORY:-https://opam.ocaml.org}";

# Always set the build type.
sed "s/%VERSION%/$BUILD/" dune.in > dune

# Perform only minimal initialization on `--configure`.
if [[ -n "${CONFIGURE}" ]]; then
  # Switch compilers, if requested.
  if [[ -n "${COMPILER}" ]]; then
    if [[ -z "${OPAM_ROOT}" ]]; then
      echo "Warning: cannot switch compilers without a valid OPAM_ROOT."
    else
      # This requires a "eval `opam config env --root ${OPAM_ROOT}`"
      # by the caller. opam will print a message with the
      # instructions.
      opam switch set "${COMPILER}" --root "${OPAM_ROOT}"
    fi
  fi

  exit 0
fi

# Set default compiler. This must be done after --configure.
COMPILER="${COMPILER:-${DEVELOPMENT_COMPILER}}"

# Error checking.
# This check is borrowed from ocamlbuild's Makefile, printing it here
# for better user visibility.
if [[ "${CHECK_IF_PREINSTALLED}" != 'false' ]] && which ocamlc &>/dev/null; then
  test_ocamlbuild_location="$(ocamlc -where)/ocamlbuild"
  if [[ -d "${test_ocamlbuild_location}" ]]; then
    set +x
    echo "OCamlbuild will refuse to install since it is already present at ${test_ocamlbuild_location}."
    echo 'If you want to bypass this safety check, run:'
    echo '  CHECK_IF_PREINSTALLED=false ./scripts/setup.sh'
    exit 1
  fi
fi

# Extract packaged repository.
if [ ${OPAM_REPOSITORY: -7} == ".tar.gz" ]; then
  temporary_repository="$(mktemp -d)"
  tar xf "${OPAM_REPOSITORY}" -C "$temporary_repository" --strip-components=1

  # Get compiler if available.
  compiler="${OPAM_REPOSITORY%/*}/$COMPILER_VERSION.tar.gz"
  if [ -e "$compiler" ]; then
    cp "$compiler" .
    trap 'rm -rf ./$COMPILER_VERSION.tar.gz' EXIT
  fi

  OPAM_REPOSITORY=$temporary_repository
fi

opam_version=$(opam --version)
# Setting up OCaml environment.
if [[ ${opam_version:0:1} == "2" ]] ; then
  opam init --yes --reinit --disable-sandboxing --compiler "$COMPILER" --root "$OPAM_ROOT" default "$OPAM_REPOSITORY" \
    && eval "$(opam env --yes --switch "$COMPILER" --root "$OPAM_ROOT" --set-root --set-switch)" \
    && opam update \
    && ocaml_succeeded=1
else
  opam init --yes --compiler "$COMPILER" --root "$OPAM_ROOT" default "$OPAM_REPOSITORY" \
    && eval "$(opam config --root "$OPAM_ROOT" env)" \
    && opam update \
    && ocaml_succeeded=1
fi
test "$ocaml_succeeded" = 1 \
  || die 'Unable to setup OCaml environment'

opam install --yes \
  core \
  dune \
  yojson \
  ppx_deriving \
  ppx_deriving_yojson \
  ppx_compare \
  ppx_hash \
  ounit \
  menhir \
  utop \
  && opam_install_dependencies_succeeded=1
test "$opam_install_dependencies_succeeded" = 1 \
  || die 'Could not install dependencies'

# Build and install hack parallel.
(cd hack_parallel \
  && OCAMLPARAM=_,annot=1,bin-annot=1,g=1 make \
  && make remove \
  && make install) \
  && install_hack_parallel=1
test "$install_hack_parallel" = 1 \
  || die 'Could not install hack_parallel'

if [[ -n "${ENVIRONMENT_ONLY}" ]]; then
  echo 'Environment built successfully, stopping here as requested.'
  exit 0
fi

# Build and run tests.
jobs="$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 1)"

make ${MAKE_ARGUMENTS} --jobs "$jobs" || die 'Could not build pyre'
make --jobs "$jobs" test || die 'Pyre tests failed'
make python_tests || die 'Python tests for Pyre failed'
make server_integration_test || die 'Server integration test failed'
