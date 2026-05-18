# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# Test for classes where __str__ is assigned from another method via a
# class-level assignment (e.g., `__str__ = __unicode__`) within a conditional
# branch. Pyrefly reports both the assignment (ClassField) and the def as
# separate definitions, generating a $N suffix on the method name.

import os

from pysa import _test_source

# pyre-ignore[16]: os.name not available in test stubs
PY3: bool = os.name == "posix"


class Base(Exception):
    def __unicode__(self) -> str:
        return "unicode"

    if PY3:
        # pyre-ignore[8]: Incompatible attribute type
        __str__ = __unicode__
    else:

        def __str__(self) -> str:
            return "str"


class Sub(Base):
    pass


def str_of_sub(x: Sub):
    return str(x)


def str_of_base(x: Base):
    return str(x)


def source_via_str():
    x = _test_source()
    return str(x)
