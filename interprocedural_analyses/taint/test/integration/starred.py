# flake8: noqa
def sink(json):
    __test_sink(json)


def test():
    query = {"json": __test_source()}
    sink(query)
