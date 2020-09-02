#!/usr/bin/env python3
# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


class C(metaclass=Missing):
    pass


def bar(c: C) -> int:
    return c.__hash__()
