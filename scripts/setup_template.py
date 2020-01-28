# flake8: noqa
# fmt: off
import glob
import os
import sys
from pathlib import Path

from setuptools import find_packages, setup


if sys.version_info < (3, 6):
    sys.exit('Error: {PACKAGE_NAME} only runs on Python 3.6 and above.')

def get_all_stubs(root: str):
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


def get_data_files(directory: str, extension_glob: str):
    # We need to relativize data_files, see https://github.com/pypa/wheel/issues/92.
    relative_directory = os.path.relpath(os.path.abspath(directory), os.getcwd())
    return (
        relative_directory,
        glob.glob(os.path.join(relative_directory, extension_glob)),
    )


def find_taint_stubs():
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


with open("README.md") as f:
    long_description = f.read()


setup(
    name="{PACKAGE_NAME}",
    version="{PACKAGE_VERSION}",
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
    python_requires='>=3.6',
    install_requires={RUNTIME_DEPENDENCIES},
    extras_require={SAPP_DEPENDENCIES},
    entry_points={{
        "console_scripts": [
            "pyre = {MODULE_NAME}.client.pyre:main",
            "pyre-upgrade = {MODULE_NAME}.tools.upgrade.upgrade:main",
            "sapp = pyre_check.tools.sapp.sapp.cli:cli [sapp]",
        ]
    }},
)
