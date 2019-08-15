# flake8: noqa


# Integration test illustrating flows through an inherited constructor


class MyBaseClass:
    def __init__(self, argument: str) -> None:
        __test_sink(argument)


class MyDerivedClass(MyBaseClass):
    variable = ""


def test() -> None:
    # This flow should be detected:
    derived = MyDerivedClass(__test_source())
