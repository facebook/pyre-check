# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import json
from pathlib import Path

from setuptools import find_packages, setup


def _requirements():
    with open("requirements.json") as json_file:
        data = json.load(json_file)
        return data["sapp"]


setup(
    name="fb-sapp",
    version="0.2.7",
    description="Static Analysis Post-Processor for processing taint analysis results.",
    long_description=Path("README.md").read_text(),
    long_description_content_type="text/markdown",
    install_requires=_requirements(),
    entry_points={"console_scripts": ["sapp = sapp.cli:cli"]},
    packages=find_packages(),
    url="https://pyre-check.org/",
    author="Facebook",
    maintainer_email="pyre@fb.com",
    package_data={
        "sapp.ui": [
            "frontend/build/*",
            "frontend/build/static/css/*",
            "frontend/build/static/js/*",
        ],
    },
)
