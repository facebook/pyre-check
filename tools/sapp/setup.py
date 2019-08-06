# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from setuptools import find_packages, setup


with open("requirements.txt") as f:
    requirements = f.read().splitlines()

setup(
    name="sapp",
    version="0.1",
    install_requires=requirements,
    entry_points={"console_scripts": ["sapp = sapp.cli:cli"]},
    packages=find_packages(),
    url="https://pyre-check.org/",
    maintainer="Jerry Liu, Zachary Landau, Dominik Gabi",
    maintainer_email="jerryliu55@fb.com, zacharyl@fb.com, dominik@fb.com",
)
