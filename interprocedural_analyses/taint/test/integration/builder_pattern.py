from typing import Mapping, Optional


class Builder:
    def __init__(self) -> None:
        self._saved: Optional[str] = None
        self._not_saved: Optional[str] = None

    def set_saved(self, saved: str) -> "Builder":
        self._saved = saved
        return self

    def set_not_saved(self, not_saved: str) -> "Builder":
        self._not_saved = not_saved
        return self

    def async_save(self) -> None:
        __test_sink(self._saved)


def test_no_issue():
    builder = Builder()
    builder.set_not_saved(__test_source()).set_saved("benign").async_save()


def test_issue():
    builder = Builder()
    builder.set_not_saved("benign").set_saved(__test_source()).async_save()
