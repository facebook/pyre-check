def source():
    return 0


def source_with_inferred():
    a = source()
    return a


def inferred_is_propagated():
    return source_with_inferred()
