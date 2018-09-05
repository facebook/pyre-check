from typing import Any


class override_settings:
    def __call__(self, test_func: Any) -> Any:
        ...
