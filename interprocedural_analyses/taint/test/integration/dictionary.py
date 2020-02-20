from typing import Any, Dict, Iterable, Tuple


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
        res[key] = d[key]
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
        # TODO(T61722447) we need to make iteration read [*][1] here.
        __test_sink(item[0])

    tainted_keys = {__test_source(): ""}
    # Should be an issue.
    __test_sink(tainted_keys.keys())
    # Shouldn't be an issue.
    __test_sink(tainted_keys.values())
