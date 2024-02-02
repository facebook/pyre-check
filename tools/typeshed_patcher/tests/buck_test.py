# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from pathlib import Path

import testslide

from ..buck import generate_mapped_source
from ..typeshed import MemoryBackedTypeshed


class BuckTest(testslide.TestCase):
    def test_generate_mapped_source(self) -> None:
        sample_typeshed = MemoryBackedTypeshed(
            {
                Path("stdlib/math.pyi"): "",
                Path("stdlib/os/path.pyi"): "",
                Path("stubs/ujson/ujson.pyi"): "",
                Path("stubs/mysqlclient/MySQLdb/__init__.pyi"): "",
            }
        )
        mapped_source = generate_mapped_source(sample_typeshed)
        self.assertDictEqual(
            mapped_source.mapping,
            {
                Path("math.pyi"): Path("stdlib/math.pyi"),
                Path("os/path.pyi"): Path("stdlib/os/path.pyi"),
                Path("ujson/ujson.pyi"): Path("stubs/ujson/ujson.pyi"),
                Path("mysqlclient/MySQLdb/__init__.pyi"): Path(
                    "stubs/mysqlclient/MySQLdb/__init__.pyi"
                ),
            },
        )
