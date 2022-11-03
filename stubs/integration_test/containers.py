# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from integration_test.taint import source, sink


def dictionary_update():
    result = {}
    argument = {"source": source()}
    result.update(argument)
    sink(result)


def list_append():
    l = []
    l.append(source())
    sink(l[0])


def list_update():
    l = []
    tainted_list = [source()]
    l.extend(tainted_list)
    sink(l[0])


def list_insert():
    l = [1] * 10
    l.insert(5, source())
    sink(l[5])


def set_add():
    s = {1}
    s.add(source())
    for element in s:
        sink(element)


def set_intersection_update():
    s = {1}
    s.intersection_update({source()})
    for element in s:
        sink(element)


def set_update():
    s = {1}
    s.update({source()})
    for element in s:
        sink(element)
