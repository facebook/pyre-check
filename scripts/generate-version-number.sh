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
else
  BUILD_INFO="$(uname -s -m) @ $(date)"
fi
echo "Build info: ${BUILD_INFO}"

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

# Add version information to the file.
cat > "${SCRIPTS_DIRECTORY}/../source/version.ml" <<EOF
open Core

let build_info () =
  "${BUILD_INFO}"

let version () =
  "${VERSION}"

let log_version_banner () =
  Log.info "Running as pid: %d" (Pid.to_int (Unix.getpid ()));
  Log.info "Version: %s" (version ());
  Log.info "Build info: %s" (build_info ())
EOF


cat > "${SCRIPTS_DIRECTORY}/../source/hack_parallel/hack_parallel/utils/get_build_id.c" <<EOF
const char* const BuildInfo_kRevision = "${VERSION}";
const unsigned long BuildInfo_kRevisionCommitTimeUnix = 0ul;
EOF
