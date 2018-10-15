# @nolint


def dictionary_source():
    result = {
        "a": __testSource(),
    }
    return result


def dictionary_entry_sink(arg):
    result = {
        "a": __testSink(arg)
    }

def dictionary_tito(arg):
    result = {
        "a": arg
    }
    return result
