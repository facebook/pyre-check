from typing import Any


class MiddlewareNotUsed(Exception):
    pass


class ValidationError(Exception):
    error_dict: Any

    def update_error_dict(self, error_dict: Any) -> Any:
        ...


class SuspiciousOperation(Exception):
    pass
