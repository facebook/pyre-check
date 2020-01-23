from dataclasses import dataclass


@dataclass
class DataClass:
    bad: int
    benign: str


def bad_is_tainted():
    context = DataClass(bad=__test_source(), benign=1)
    __test_sink(context)
    return context


def benign_is_untainted():
    context = DataClass(bad=__test_source(), benign=1)
    __test_sink(context.benign)
    return context


@dataclass
class DataClassWIthInit:
    bad: int

    def __init__(self, bad: int) -> None:
        self.bad = bad
        __test_sink(bad)


def issue_in_dataclass_constructor() -> None:
    DataClassWIthInit(bad=__test_source())
