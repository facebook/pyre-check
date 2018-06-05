#!/bin/bash

# Copyright (c) 2018-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -e
set -x

# Compatibility settings with MacOS.
if [[ "${MACHTYPE}" = *apple* ]]; then
  READLINK=greadlink
else
  READLINK=readlink
fi

SCRIPTS_DIRECTORY="$(dirname "$("${READLINK}" -f "$0")")"

# Gather build information.
if [[ "${1}" == 'development' ]]; then
  # Avoid rebuilding the binary at every 'make' invocation during development.
  BUILD_INFO="development build"
else
  BUILD_INFO="$(uname -s -m) @ $(date)"
fi

# Gather version information.
VERSION=""
if HG_VERSION="$(hg log -r . -T '{node}')"; then
  VERSION="${HG_VERSION}"
elif GIT_VERSION="$(git rev-parse HEAD)"; then
  VERSION="${GIT_VERSION}"
else
  echo 'Cannot determine version information'
  exit 1
fi

# Add version information to the file.
cat > "${SCRIPTS_DIRECTORY}/../version.ml" <<EOF
let build_info () =
  "${BUILD_INFO}"

let version () =
  "${VERSION}"
EOF
