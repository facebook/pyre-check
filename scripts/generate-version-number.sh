#!/bin/bash
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -e

# Compatibility settings with MacOS.
if [[ "${MACHTYPE}" = *apple* ]]; then
  READLINK=greadlink
else
  READLINK=readlink
fi

SCRIPTS_DIRECTORY="$(dirname "$("${READLINK}" -f "$0")")"

# Gather build information.
if [[ "${1}" == 'development' ]]; then
  # Use a less precise date to avoid rebuilding the binary at every
  # 'make' invocation during development.
  BUILD_INFO="$(uname -s -m) @ $(date "+%a %b %d %Y") (development build)"
  shift
else
  BUILD_INFO="$(uname -s -m) @ $(date)"
fi
echo "Build info: ${BUILD_INFO}"

# Buck builds pass a `--out-dir <DIR>` argument indicating where to write
# artifacts.
while [ $# -gt 0 ]; do
  if [[ "$1" =~ ^--out-dir=(.*)$ ]]; then
    _OUT_DIR="${BASH_REMATCH[1]}"
  else
    echo "Unexpected arg: $1" && exit 1
  fi
  shift
done

# Gather version information.
VERSION=""
if HG_VERSION="$(hg log -r . -T '{node}')"; then
  VERSION="${HG_VERSION}"
  echo "HG revision: ${VERSION}"
elif GIT_VERSION="$(git rev-parse HEAD)"; then
  VERSION="${GIT_VERSION}"
  echo "Git commit: ${VERSION}"
else
  echo 'Cannot determine version information'
  exit 1
fi

OUT_DIR="${_OUT_DIR:-"$SCRIPTS_DIRECTORY"/../source}"
cat > "$OUT_DIR"/version.ml <<EOF
open Core

let build_info () =
  "${BUILD_INFO}"

let version () =
  "${VERSION}"

let log_version_banner () =
  Log.info "Running as pid: %d" (Pid.to_int (Core_unix.getpid ()));
  Log.info "Version: %s" (version ());
  Log.info "Build info: %s" (build_info ())
EOF

# Note: [Embedded Build Info]
# ===========================
#
# Buck executables automatically link build info that defines the symbols
# `BuildInfo_kRevision` and `BuildInfo_kRevisionCommitTimeUnix` and so there's
# no need to generate `get_build_id.c` for buck builds.
#
# See [1^] for details. For example, try
# ```
#  buck2 build @fbcode//mode/opt \
#   -c build_info.revision="$(hg log -r . -T {node})" \
#      fbcode//tools/pyre/source:main --show-output
# ```
# and inspect the results with something like
# ```
#  fbcode/tools/build_info/build_info -binary \
#     buck-out/v2/gen/fbcode/ca1f7c2b2d91f85d/tools/pyre/source/__main__/main.opt
# ```
#
# [^1]: [Build Info](https://www.internalfb.com/intern/wiki/Buck-users/fbcode-build-info/).

if [[ -n "$_OUT_DIR" ]]; then
  :
else
  OUT_DIR="$SCRIPTS_DIRECTORY"/../source/hack_parallel/hack_parallel/utils
  cat > "$OUT_DIR"/get_build_id.c <<EOF
const char* const BuildInfo_kRevision = "${VERSION}";
const unsigned long BuildInfo_kRevisionCommitTimeUnix = 0ul;
EOF
fi
