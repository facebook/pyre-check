from typing import Any

from django.core.exceptions import ValidationError as ValidationError


class EmailValidator:
    def __call__(self, value: Any) -> None:
        ...


class URLValidator:
    def __call__(self, value: Any) -> None:
        ...


validate_email: EmailValidator = ...
