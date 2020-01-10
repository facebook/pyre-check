import glob
import json
import os
import sys
from pathlib import Path
from typing import List, Tuple

from setuptools import find_packages, setup


if sys.version_info < (3, 5):
    sys.exit("Error: pyre-check only runs on Python 3.5 and above.")


def get_data_files(directory: str, extension_glob: str) -> Tuple[str, List[str]]:
    # We need to relativize data_files, see https://github.com/pypa/wheel/issues/92.
    relative_directory = os.path.relpath(os.path.abspath(directory), os.getcwd())
    return (
        relative_directory,
        glob.glob(os.path.join(relative_directory, extension_glob)),
    )


def get_all_stubs(root: str) -> List[Tuple[str, List[str]]]:
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

with open(
    os.path.join("pyre_check", "tools", "sapp", "requirements.json")
) as json_file:
    extra_requirements = json.load(json_file)

PYRE_EMAIL = "pyre@fb.com"
AUTHOR = "Facebook"

setup(
    name="pyre-check",
    version=os.environ["PACKAGE_VERSION"],
    description="A performant type checker for Python",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://pyre-check.org/",
    download_url="https://github.com/facebook/pyre-check",
    author=AUTHOR,
    author_email=PYRE_EMAIL,
    maintainer=AUTHOR,
    maintainer_email=PYRE_EMAIL,
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
    + get_all_stubs(root=Path.cwd() / "stubs" / "django")
    + get_all_stubs(root=Path.cwd() / "stubs" / "lxml")
    + find_taint_stubs(),
    python_requires=">=3.5",
    install_requires=["pywatchman", "psutil", "libcst", "pyre_extensions"],
    extras_require=extra_requirements,
    entry_points={
        "console_scripts": [
            "pyre = pyre_check.client.pyre:main",
            "pyre-upgrade = pyre_check.tools.upgrade.upgrade:main",
            "sapp = pyre_check.tools.sapp.sapp.cli:cli [sapp]",
        ]
    },
)
