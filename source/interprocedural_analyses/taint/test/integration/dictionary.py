# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import __test_sink, __test_source
from typing import Any, Dict, Generic, Iterable, Mapping, Optional, TypeVar, cast


def dictionary_source():
    result = {"a": __test_source()}
    return result


def dictionary_entry_sink(arg):
    result = {"a": __test_sink(arg)}


def dictionary_tito(arg):
    result = {"a": arg}
    return result


def dictionary_assignment_source():
    d = {}
    d["a"] = __test_source()
    return d["a"]


def dictionary_non_source():
    d = {}
    d["a"] = __test_source()
    return d["b"]


def dictionary_assign_to_index():
    d = {}
    d["a"] = __test_source()
    return d


def dictionary_nested_assignment_1():
    d = {}
    d["a"]["b"] = __test_source()
    return d["a"]["b"]


def dictionary_nested_assignment_2():
    d = {}
    d["a"]["b"] = __test_source()
    return d["a"]


def dictionary_nested_non_source_assignment():
    d = {}
    d["a"]["b"] = __test_source()
    return d["a"]["a"]


tainted_dictionary: Dict[Any, Any] = {}


def update_tainted_dictionary():
    tainted_dictionary.update({"a": __test_source()})


def update_dictionary_indirectly(arg):
    tainted_dictionary.update(arg)


def indirect_flow_from_source_to_global_dictionary():
    update_dictionary_indirectly({"a": __test_source()})


def update_parameter(arg):
    arg.update({"a": __test_source})


def flow_through_keywords():
    tainted_map = {"a": __test_source()}
    new_map = {**tainted_map}
    __test_sink(tainted_map["a"])


class SpecialSetitemDict(Dict[Any, Any]):
    def __setitem__(self, key: Any, value: Any) -> None:
        __test_sink(key)


def tainted_setitem(d: SpecialSetitemDict) -> SpecialSetitemDict:
    d[__test_source()] = 1
    return d


def forward_comprehension_value_source():
    d = {"a": __test_source() for x in []}
    return d


def forward_comprehension_key_source():
    d = {__test_source(): 0 for x in []}
    return d


def forward_comprehension_value_sink(arg):
    d = {"a": __test_sink(x) for x in [arg]}


def forward_comprehension_key_sink(arg):
    d = {__test_sink(x): 0 for x in [arg]}


def lists_of_dictionary_iteration_is_precise():
    list_of_dicts = [
        {"with_feature": __test_source(), "without_feature": 0} for x in []
    ]
    for dict in list_of_dicts:
        __test_sink(dict["with_feature"])
        __test_sink(dict["without_feature"])


def reassignment_removes_backwards_taint(d):
    d["a"] = 0
    __test_sink(d["a"])


def copy_untainted_values_with_tainted_keys():
    d = {__test_source(): 1}
    values_not_tainted = {}
    for key in d:
        values_not_tainted[key] = d[key]
    return values_not_tainted


def dict_with_tainted_key_flows_to_sink():
    d = {__test_source(): 1}
    __test_sink(d)


def sink_dictionary_through_keys(d: Dict[str, str]) -> None:
    [__test_sink(k) for k in d]


def get_keys(d: Dict[str, str]) -> Iterable[str]:
    return [k for k in d]


def return_comprehension_with_tained_keys():
    d = {__test_source(): 1}
    return [k for k in d]


def return_comprehension_with_untainted_keys():
    d = {1: __test_source()}
    return [k for k in d]


def backwards_model_for_dictionary_comprehension(d) -> None:
    inferred = {k: d[k] for k in d}
    sink_dictionary_through_keys(inferred)


def test_keys_and_values():
    tainted_values = {"benign": ("benign", __test_source())}
    # Should be an issue.
    __test_sink(tainted_values.values())
    # Shouldn't be an issue.
    __test_sink(tainted_values.keys())
    for item in tainted_values.values():
        __test_sink(item[0])

    tainted_keys = {__test_source(): ""}
    # Should be an issue.
    __test_sink(tainted_keys.keys())
    # Shouldn't be an issue.
    __test_sink(tainted_keys.values())


def backwards_field_assignment(external):
    d = {}
    d["index"] = external
    return d


def return_tito_literally(external):
    return {"index": external}


def test_with_issue_in_dict_comprehension():
    sources = [__test_source()]
    {"k": s for s in sources if __test_sink(s)}


TV = TypeVar("_T")


def to_map(x: Dict[str, TV]) -> Mapping[str, TV]:
    return x


class Service(Generic[TV]):
    async def async_get_many_dict(self, keys: Iterable[str]) -> Dict[str, TV]:
        return {key: cast(TV, key) for key in keys}

    async def async_get_dict(self, key: str) -> Optional[TV]:
        return (await self.async_get_many_dict(keys={key})).get(key)

    async def async_get_mapping(self, key: str) -> Optional[TV]:
        return to_map(await self.async_get_many_dict(keys={key})).get(key)


def test_service_with_dict():
    service = Service()
    __test_sink(service.async_get_dict(__test_source()))


def test_service_with_mapping():
    service = Service()
    __test_sink(service.async_get_mapping(__test_source()))


def tito_with_index(d: Dict[str, str]) -> str:
    result = d["a"]
    return result


def test_index_from_tito():
    d = {"a": __test_source(), "b": __test_source()}
    __test_sink(tito_with_index(d))


def test_items():
    key_is_tainted = {__test_source(): ""}
    value_is_tainted = {"a": __test_source()}
    for k, v in key_is_tainted.items():
        # Should be an issue.
        __test_sink(k)
        # Should not be an issue.
        __test_sink(v)

    for k, v in value_is_tainted.items():
        # Should not be an issue.
        __test_sink(k)
        # Should be an issue.
        __test_sink(v)


def test_items_backward_keys(x, y):
    key_is_tainted = {x: "a"}
    value_is_tainted = {"b": y}
    for k, v in key_is_tainted.items():
        __test_sink(k)

    for k, v in value_is_tainted.items():
        __test_sink(k)


def test_items_backward_values(x, y):
    key_is_tainted = {x: "a"}
    value_is_tainted = {"b": y}
    for k, v in key_is_tainted.items():
        __test_sink(v)

    for k, v in value_is_tainted.items():
        __test_sink(v)


def test_with_issue_in_dict_items_comprehension():
    sources = {"k": __test_source()}
    return {k: v for k, v in sources.items()}
