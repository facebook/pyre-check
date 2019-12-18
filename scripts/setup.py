import glob
import os
import sys

from setuptools import find_packages, setup


if sys.version_info < (3, 5):
    sys.exit("Error: pyre-check only runs on Python 3.5 and above.")


def find_typeshed_files(base):
    if not os.path.isdir(base):
        return []
    typeshed_root = os.path.join(base, "typeshed")
    if not os.path.isdir(typeshed_root):
        return []
    result = []
    for absolute_directory, _, _ in os.walk(typeshed_root):
        relative_directory = os.path.relpath(absolute_directory, base)
        files = glob.glob(os.path.join(relative_directory, "*.pyi"))
        if not files:
            continue
        target = os.path.join("lib", "pyre_check", relative_directory)
        result.append((target, files))
    return result


with open("README.md") as f:
    long_description = f.read()


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
    data_files=[("bin", ["bin/pyre.bin"])] + find_typeshed_files(os.getcwd()),
    python_requires=">=3.5",
    install_requires=["pywatchman", "psutil", "libcst", "pyre_extensions"],
    entry_points={
        "console_scripts": [
            "pyre = pyre_check.client.pyre:main",
            "pyre-upgrade = pyre_check.tools.upgrade.upgrade:main",
        ]
    },
)
