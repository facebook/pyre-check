# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import glob
import json
import os
import sys
from pathlib import Path
from typing import List, Mapping, Tuple

from setuptools import find_packages, setup


if sys.version_info < (3, 6):
    sys.exit("Error: {PACKAGE_NAME} only runs on Python 3.6 and above.")


def get_all_stubs(root: Path) -> List[Tuple[str, List[str]]]:
    if not os.path.isdir(root):
        return []
    result = []
    for absolute_directory, _, _ in os.walk(root):
        relative_directory, files = get_data_files(
            directory=absolute_directory, extension_glob="*.pyi"
        )
        if not files:
            continue
        target = os.path.join("lib", "pyre_check", relative_directory)
        result.append((target, files))
    return result


def get_data_files(directory: str, extension_glob: str) -> Tuple[str, List[str]]:
    # We need to relativize data_files, see https://github.com/pypa/wheel/issues/92.
    relative_directory = os.path.relpath(os.path.abspath(directory), os.getcwd())
    return (
        relative_directory,
        glob.glob(os.path.join(relative_directory, extension_glob)),
    )


def find_taint_stubs() -> List[Tuple[str, List[str]]]:
    _, taint_stubs = get_data_files(
        directory=os.path.join(os.getcwd(), "taint"), extension_glob="*"
    )
    _, third_party_taint_stubs = get_data_files(
        directory=os.path.join(os.getcwd(), "third_party_taint"), extension_glob="*"
    )
    taint_stubs += third_party_taint_stubs
    if not taint_stubs:
        return []
    return [(os.path.join("lib", "pyre_check", "taint"), taint_stubs)]


def run(
    package_name: str,
    package_version: str,
    module_name: str,
    runtime_dependencies: List[str],
    sapp_dependencies: Mapping[str, List[str]],
    long_description: str,
    **kwargs: object
) -> None:
    setup(
        name=package_name,
        version=package_version,
        description="A performant type checker for Python",
        long_description=long_description,
        long_description_content_type="text/markdown",
        url="https://pyre-check.org/",
        download_url="https://github.com/facebook/pyre-check",
        author="Facebook",
        author_email="pyre@fb.com",
        maintainer="Facebook",
        maintainer_email="pyre@fb.com",
        license="MIT",
        classifiers=[
            "Development Status :: 3 - Alpha",
            "Environment :: Console",
            "Intended Audience :: Developers",
            "License :: OSI Approved :: MIT License",
            "Operating System :: MacOS",
            "Operating System :: POSIX :: Linux",
            "Programming Language :: Python",
            "Programming Language :: Python :: 3",
            "Programming Language :: Python :: 3.5",
            "Programming Language :: Python :: 3.6",
            "Topic :: Software Development",
        ],
        keywords="typechecker development",
        packages=find_packages(exclude=["tests", "pyre-check"]),
        data_files=[("bin", ["bin/pyre.bin"])]
        + get_all_stubs(root=Path.cwd() / "typeshed")
        + get_all_stubs(root=Path.cwd() / "stubs/django")
        + get_all_stubs(root=Path.cwd() / "stubs/lxml")
        + find_taint_stubs(),
        python_requires=">=3.6",
        install_requires=runtime_dependencies,
        extras_require=sapp_dependencies,
        entry_points=dict(  # noqa we need to do this to make this .format-able
            console_scripts=[
                "pyre = " + module_name + ".client.pyre:main",
                "pyre-upgrade = " + module_name + ".tools.upgrade.upgrade:main",
                "sapp = pyre_check.tools.sapp.sapp.cli:cli [sapp]",
            ]
        ),
        **kwargs
    )


def main() -> None:
    with open("README.md") as f:
        long_description = f.read()

    run(
        package_name="{PACKAGE_NAME}",
        package_version="{PACKAGE_VERSION}",
        module_name="{MODULE_NAME}",
        runtime_dependencies=json.loads("""{RUNTIME_DEPENDENCIES}"""),
        sapp_dependencies=json.loads("""{SAPP_DEPENDENCIES}"""),
        long_description=long_description,
    )


if __name__ == "__main__":
    main()
