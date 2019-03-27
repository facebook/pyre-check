# @nolint


def dictionary_source():
    result = {"a": __test_source()}
    return result


def dictionary_entry_sink(arg):
    result = {"a": __test_sink(arg)}


def dictionary_tito(arg):
    result = {"a": arg}
    return result
