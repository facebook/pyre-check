# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


def no_precondition_for_if(x):
    if x:
        True
    else:
        False


def no_precondition_for_condition(x):
    return 0 if x else 1


def some_source(name):
    pass


def issue1():
    x = some_source("my-data")
    if x:
        return


def issue2():
    x = some_source("other")
    return 0 if x else 1
