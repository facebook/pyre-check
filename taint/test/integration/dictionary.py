# @nolint


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
