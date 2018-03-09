#!/bin/bash

# Copyright (c) 2018-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

set -e

# global constants
PACKAGE_NAME="pyre-check"
PACKAGE_VERSION="0.0.1"
AUTHOR='Facebook'
AUTHOR_EMAIL='mleogrande@fb.com'
MAINTAINER='Facebook'
MAINTAINER_EMAIL='mleogrande@fb.com'
URL='https://github.com/facebookexperimental/pyre-check'
# https://www.python.org/dev/peps/pep-0008/#package-and-module-names
MODULE_NAME="pyre_check"

# helpers
die() {
  printf '%s: %s, exiting.\n' "$(basename "$0")" "$*" >&2
  exit 1
}
error_trap () {
  die "Command '${BASH_COMMAND}' failed at ${BASH_SOURCE[0]}:${BASH_LINENO[0]}"
}

trap error_trap ERR

# create build tree
SCRIPTS_DIRECTORY="$(dirname "$(readlink -f "$0")")"
cd "${SCRIPTS_DIRECTORY}/"
BUILD_ROOT="${SCRIPTS_DIRECTORY}/buildroot"
rm -rf "${BUILD_ROOT}"
mkdir "${BUILD_ROOT}"
cd "${BUILD_ROOT}"

# copy source files
mkdir "${MODULE_NAME}"
cp "${SCRIPTS_DIRECTORY}"/*.py "${BUILD_ROOT}/${MODULE_NAME}"

# copy binary files
BINARY_FILE="${SCRIPTS_DIRECTORY}/../_build/all/main.native"
if [[ ! -f "${BINARY_FILE}" ]]; then
  echo "The binary file ${BINARY_FILE} does not exist."
  echo "Have you run 'make' in the toplevel directory?"
  exit 1
fi
mkdir -p "${BUILD_ROOT}/bin"
cp "${BINARY_FILE}" "${BUILD_ROOT}/bin"

# create setup.py file
cat > "${BUILD_ROOT}/setup.py" <<HEREDOC
from setuptools import setup, find_packages

setup(
    name='${PACKAGE_NAME}',
    version='${PACKAGE_VERSION}',
    description='A performant type checker for Python',

    url='${URL}',
    author='${AUTHOR}',
    author_email='${AUTHOR_EMAIL}',
    maintainer='${MAINTAINER}',
    maintainer_email='${MAINTAINER_EMAIL}',
    license='MIT',

    classifiers=[
        'Development Status :: 3 - Alpha',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: MIT License',
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.4',
        'Programming Language :: Python :: 3.5',
        'Programming Language :: Python :: 3.6',
    ],
    keywords='typechecker development',

    packages=find_packages(exclude=['tests']),
    data_files=[('bin', ['bin/main.native'])],
    python_requires='>=3',
    entry_points={
        'console_scripts': [
            'pyre = ${MODULE_NAME}.pyre:main',
        ],
    }
)
HEREDOC

# create setup.cfg file
cat > "${BUILD_ROOT}/setup.cfg" <<HEREDOC
[metadata]
license_file = ../../LICENSE
HEREDOC

# build
python setup.py bdist_wheel

# copy artifacts outside of the the build directory
mkdir -p "${SCRIPTS_DIRECTORY}/dist"
cp "${BUILD_ROOT}"/dist/* "${SCRIPTS_DIRECTORY}/dist/"

# cleanup
cd "${SCRIPTS_DIRECTORY}"
rm -rf "${BUILD_ROOT}"

printf '\nAll done. Build artifacts are available in: %s/dist/\n' "${SCRIPTS_DIRECTORY}"
exit 0
