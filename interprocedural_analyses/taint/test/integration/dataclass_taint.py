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
