# @nolint


def source_via_format():
    taint = __test_source()
    return f"{taint} is bad"
