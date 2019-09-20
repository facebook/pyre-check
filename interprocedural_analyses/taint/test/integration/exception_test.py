def test_parameter_flow(ex: Exception):
    return str(ex)


def test_constructed_exception():
    ex = Exception("message")
    return str(ex)


def test_caught_exception():
    try:
        return ""
    except Exception as ex:
        return str(ex)
