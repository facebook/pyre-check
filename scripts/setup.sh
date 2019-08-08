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

COMPILER_VERSION="4.08.0"
DEVELOPMENT_COMPILER="${COMPILER_VERSION}"
RELEASE_COMPILER="${COMPILER_VERSION}+flambda"
MAKE_ARGUMENTS="dev"

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
    echo "Pyre only supports opam 2.0.0 and above, please update your opam version."
    exit 1
fi
test "$ocaml_succeeded" = 1 \
  || die 'Unable to setup OCaml environment'

opam install --yes \
  base64.3.1.0 \
  conf-sqlite3 \
  core.v0.12.3 \
  dune.1.10.0 \
  yojson.1.7.0 \
  ppx_deriving_yojson.3.5.1 \
  ounit.2.0.8 \
  menhir \
  utop \
  && opam_install_dependencies_succeeded=1
test "$opam_install_dependencies_succeeded" = 1 \
  || die 'Could not install dependencies'

if [[ -n "${ENVIRONMENT_ONLY}" ]]; then
  echo 'Environment built successfully, stopping here as requested.'
  exit 0
fi

# Build and run tests.
jobs="$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 1)"

make ${MAKE_ARGUMENTS} --jobs "$jobs" || die 'Could not build pyre'
make --jobs "$jobs" test || die 'Pyre tests failed'
if [[ "${BUILD}" == 'external' ]] && ! command -v watchman; then
  echo 'Skipping integration test in external mode, since watchman is not installed'
else
  make server_integration_test || die 'Server integration test failed'
fi

exit 0
