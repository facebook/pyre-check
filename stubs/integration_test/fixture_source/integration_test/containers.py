# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# flake8: noqa

from integration_test.taint import source, sink
import collections


def dictionary_constructor(x: str):
    d = dict({"a": source(), "b": 0})
    sink(d["a"])  # This is an issue.
    sink(d["b"])  # This is an issue (false positive).

    d = dict({"a": {"b": source()}})
    sink(d["a"]["a"])  # This is an issue (false positive).
    sink(d["a"]["b"])  # This is an issue.
    sink(d["b"]["a"])  # This is an issue (false positive).
    sink(d["b"]["b"])  # This is an issue (false positive).

    d = dict({source(): 0})
    sink(d.keys())  # This is an issue.
    sink(d[x])  # This is NOT an issue.

    d = dict({(source(), 0): 0})
    for key in d.keys():
        sink(key[0])  # This is an issue.
        sink(key[1])  # This is NOT an issue.

    d = dict(a=source())
    sink(d["a"])  # This is an issue.
    sink(d["b"])  # This is an issue (false positive)

    d = dict(a={"b": source()})
    sink(d["a"]["a"])  # This is NOT an issue.
    sink(d["a"]["b"])  # This is an issue.
    sink(d["b"]["a"])  # This is NOT an issue.
    sink(d["b"]["b"])  # This is an issue (false positive)


def dictionary_update():
    result = {}
    argument = {"source": source()}
    result.update(argument)
    sink(result)

    result = {"old": {"a": source()}}
    argument = {"new": {"b": source()}}
    result.update(argument)
    sink(result["old"]["a"])  # This is an issue.
    sink(result["old"]["b"])  # This is an issue (false positive).
    sink(result["new"]["a"])  # This is an issue (false positive).
    sink(result["new"]["b"])  # This is an issue.
    sink(result["unknown"])  # This is an issue (false positive).

    result = {}
    argument = [("a", {"b": source()})]
    result.update(argument)
    sink(result["a"])  # This is an issue.
    sink(result["a"]["b"])  # This is an issue.
    sink(result["a"]["c"])  # This is an issue (false positive).
    sink(result["b"]["b"])  # This is an issue (false positive).

    result = {}
    argument = {"b": source()}
    result.update(a=argument)
    sink(result["a"])  # This is an issue.
    sink(result["a"]["b"])  # This is an issue.
    sink(result["a"]["c"])  # This is NOT an issue.
    sink(result["b"]["b"])  # This is an issue (false positive).


def dict_setdefault(i: int):
    d = {0: 0}
    d.setdefault(source(), 0)
    sink(d[0])  # This is NOT an issue.
    sink(d.keys())  # This is an issue.

    d = {0: 0}
    d.setdefault(i, source())
    sink(d[i])

    d = {0: 0}
    d.setdefault(i, {"a": source()})
    sink(d[i]["a"])  # This is an issue.
    sink(d[i]["b"])  # This is NOT an issue.

    d = {0: 0}
    result = d.setdefault(i, {"a": source()})
    sink(result["a"])  # This is an issue.
    sink(result["b"])  # This is NOT an issue.

    d = {0: {"a": source()}}
    result = d.setdefault(i, 0)
    sink(result["a"])  # This is an issue.
    sink(result["b"])  # This is NOT an issue.


def dict_get():
    d = {"a": {0: source()}}
    sink(d.get("a")[0])  # This is an issue.
    sink(d.get("a")[1])  # This is NOT an issue.
    sink(d.get("b")[0])  # This is currently an issue (false positive).
    sink(d.get("b")[1])  # This is NOT an issue.


def dict_get_default():
    d = {}
    sink(d.get("a", {0: source()})[0])  # This is an issue.
    sink(d.get("a", {0: source()})[1])  # This is NOT an issue.


def ordereddict_popitem():
    d = collections.OrderedDict()
    d["a"] = {"bad": source(), "good": ""}
    e = d.popitem()
    sink(e["bad"])  # This is an issue.
    sink(e["good"])  # This is NOT an issue.


def defaultdict_constructor():
    d = collections.defaultdict(lambda: source())
    sink(d["a"])  # This is an issue.

    def factory():
        return {"bad": source(), "good": ""}

    d = collections.defaultdict(factory)
    sink(d["a"])  # This is an issue.
    sink(d["a"]["good"])  # This is NOT an issue.
    sink(d["a"]["bad"])  # This is an issue.

    d = collections.defaultdict(str, {"bad": source(), "good": ""})
    sink(d["bad"])  # This is an issue.
    sink(d["good"])  # This is an issue (false positive).

    d = collections.defaultdict(dict, {"bad": {"bad": source()}})
    sink(d["good"]["good"])  # This is an issue (false positive).
    sink(d["good"]["bad"])  # This is an issue (false positive).
    sink(d["bad"]["good"])  # This is an issue (false positive).
    sink(d["bad"]["bad"])  # This is an issue.

    d = collections.defaultdict(str, {source(): ""})
    sink(d.keys())  # This is an issue.
    sink(d[x])  # This is NOT an issue.

    d = collections.defaultdict(str, {(source(), 0): ""})
    for key in d.keys():
        sink(key[0])  # This is an issue.
        sink(key[1])  # This is NOT an issue.

    d = collections.defaultdict(str, bad=source())
    sink(d["bad"])  # This is an issue.
    sink(d["good"])  # This is an issue (false positive)

    d = collections.defaultdict(bad={"bad": source()})
    sink(d["good"]["good"])  # This is NOT an issue.
    sink(d["good"]["bad"])  # This is an issue (false positive).
    sink(d["bad"]["good"])  # This is NOT an issue.
    sink(d["bad"]["bad"])  # This is an issue.


def list_constructor(i: int):
    l = list([0, source()])
    sink(l[0])  # This is an issue (false positive).
    sink(l[1])  # This is an issue.

    l = list([{"a": source()}])
    sink(l[0]["a"])  # This is an issue.
    sink(l[0]["b"])  # This is NOT an issue.

    l = list([{"a": source(), "c": 0}, {"b": source(), "c": 0}])
    sink(l[i]["a"])  # This is an issue.
    sink(l[i]["b"])  # This is an issue.
    sink(l[i]["c"])  # This is NOT an issue.

    l = list({(source(), 0)})
    sink(l[i][0])  # This is an issue.
    sink(l[i][1])  # This is NOT an issue.

    l = list({source(): 0})
    sink(l[i])

    l = list({(source(), 0): 0})
    sink(l[i][0])  # This is an issue.
    sink(l[i][1])  # This is NOT an issue.


def list_append():
    l = []
    l.append(source())
    sink(l[0])

    l = []
    l.append({"a": source()})
    sink(l[0]["a"])  # This is an issue.
    sink(l[0]["b"])  # This is NOT an issue.


def list_extend(i: int):
    l = []
    tainted_list = [source()]
    l.extend(tainted_list)
    sink(l[0])

    l = []
    tainted_list = [{"a": source()}]
    l.extend(tainted_list)
    sink(l[0]["a"])  # This is an issue.
    sink(l[0]["b"])  # This is NOT an issue.

    l = [{"a": source(), "c": 0}]
    tainted_list = [{"b": source(), "c": 0}]
    l.extend(tainted_list)
    sink(l[i]["a"])  # This is an issue.
    sink(l[i]["b"])  # This is an issue.
    sink(l[i]["c"])  # This is NOT an issue.

    l = []
    l.extend({(source(), 0)})
    sink(l[i][0])  # This is an issue.
    sink(l[i][1])  # This is NOT an issue.

    l = []
    l.extend({source(): 0})
    sink(l[i])


def list_insert(i: int):
    l = [1] * 10
    l.insert(5, source())
    sink(l[5])

    l = [{}] * 10
    l.insert(5, {"a": source(), "b": 0})
    sink(l[5]["a"])  # This is an issue.
    sink(l[5]["b"])  # This is NOT an issue.

    l = [0, source(), 0]
    l.insert(0, 0)
    sink(l[2])  # This is an issue.


def list_iadd(i: int):
    l = [0]
    l += [source()]
    sink(l[1])

    l = [{"a": source()}]
    l += [{"b": source()}]
    sink(l[0]["a"])  # This is an issue.
    sink(l[1]["b"])  # This is an issue.
    sink(l[0]["b"])  # This is currently an issue (false positive).
    sink(l[i]["b"])  # This is an issue.
    sink(l[i]["c"])  # This is NOT an issue.


def list_add(i: int):
    l = [0] + [source()]
    sink(l[1])

    l = [{"a": source()}] + [{"b": source()}]
    sink(l[0]["a"])  # This is an issue.
    sink(l[1]["b"])  # This is an issue.
    sink(l[0]["b"])  # This is currently an issue (false positive).
    sink(l[i]["b"])  # This is an issue.
    sink(l[i]["c"])  # This is NOT an issue.


def list_imul():
    l = [0, source(), 0]
    l *= 4
    sink(l[10])  # This is an issue.
    sink(l[9])  # This is currently an issue (false positive).


def list_sort(i: int):
    l = [0, 1, 2, source(), 4, 5]
    l.sort()
    sink(l[0])
    sink(l[i])


def list_remove():
    l = [0, source(), 0]
    l.remove(0)
    sink(l[0])  # This is an issue.
    sink(l[1])  # This is currently an issue (false positive).


def set_add():
    s = {1}
    s.add(source())
    for element in s:
        sink(element)

    s = set()
    s.add((source(), 0))
    for element in s:
        sink(element[0])  # This is an issue.
        sink(element[1])  # This is NOT an issue.


def set_intersection_update():
    s = {1}
    s.intersection_update({source()})
    for element in s:
        sink(element)

    s = {(source(), 0, 0)}
    s.intersection_update({(0, source(), 0)})
    for element in s:
        sink(element[0])  # This is an issue.
        sink(element[1])  # This is an issue.
        sink(element[2])  # This is NOT an issue.


def frozenset_union():
    s = frozenset()
    s = s.union(source())
    for element in s:
        sink(element)

    s = frozenset([(0, 0, 0), (source(), 0, 0)])
    s = s.union({(0, source(), 0)})
    for element in s:
        sink(element[0])  # This is an issue.
        sink(element[1])  # This is an issue.
        sink(element[2])  # This is NOT an issue.


def set_update():
    s = {1}
    s.update({source()})
    for element in s:
        sink(element)

    s = {1}
    s.update({source(): 0})
    for element in s:
        sink(element)


def set_iand():
    s = {(source(), 0, 0)}
    s &= {(0, source(), 0)}
    for element in s:
        sink(element[0])  # This is an issue.
        sink(element[1])  # This is an issue.
        sink(element[2])  # This is NOT an issue.


def deque_append(i: int):
    d = collections.deque()
    d.append(source())
    sink(d[0])

    d = collections.deque()
    d.append({"a": source()})
    d.appendleft({"b": source()})
    sink(d[i]["a"])  # This is an issue.
    sink(d[i]["b"])  # This is an issue.
    sink(d[i]["c"])  # This is NOT an issue.


def deque_extend(i: int):
    d = collections.deque()
    d.extend([source()])
    sink(d[0])

    d = collections.deque()
    d.append({"a": source()})
    d.extend([{"b": source()}])
    sink(d[i]["a"])  # This is an issue.
    sink(d[i]["b"])  # This is an issue.
    sink(d[i]["c"])  # This is NOT an issue.


def tuple_constructor(i: int):
    l = tuple([0, source()])
    sink(l[0])  # This is an issue (false positive).
    sink(l[1])  # This is an issue.

    l = tuple([{"a": source()}])
    sink(l[0]["a"])  # This is an issue.
    sink(l[0]["b"])  # This is NOT an issue.

    l = tuple([{"a": source(), "c": 0}, {"b": source(), "c": 0}])
    sink(l[i]["a"])  # This is an issue.
    sink(l[i]["b"])  # This is an issue.
    sink(l[i]["c"])  # This is NOT an issue.

    l = tuple({(source(), 0)})
    sink(l[i][0])  # This is an issue.
    sink(l[i][1])  # This is NOT an issue.

    l = tuple({source(): 0})
    sink(l[i])

    l = tuple({(source(), 0): 0})
    sink(l[i][0])  # This is an issue.
    sink(l[i][1])  # This is NOT an issue.
