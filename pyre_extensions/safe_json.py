# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import json
import sys
from typing import Any, Dict, List, Type, TypeVar, cast, IO, Union

if sys.version_info[:2] >= (3, 9):
    # pyre-fixme[21]: Could not find name `_TypedDictMeta` in `typing`.
    from typing import _TypedDictMeta
else:
    # pyre-fixme[21]: Could not find name `_TypedDictMeta` in `typing_extensions`.
    from typing_extensions import _TypedDictMeta
from typing_inspect import get_origin, is_optional_type


if sys.version_info[:2] < (3, 7):
    from typing_inspect import get_last_args as get_args
else:
    from typing_inspect import get_args as get_args


class InvalidJson(json.JSONDecodeError):
    def __init__(self, message: str) -> None:
        super().__init__(message, "", 0)


def _is_primitive(target_type: Type[object]) -> bool:
    return target_type in (int, float, str, bool)


def _is_list(target_type: Type[object]) -> bool:
    return get_origin(target_type) in (List, list)


def _is_dictionary(target_type: Type[object]) -> bool:
    return get_origin(target_type) in (Dict, dict)


def _is_typed_dictionary(target_type: Type[object]) -> bool:
    return isinstance(target_type, _TypedDictMeta)  # pyre-ignore: private API.


def _validate_list(value: object, target_type: Type[List[object]]) -> None:
    if not isinstance(value, list):
        raise InvalidJson(f"`{value}` is not a list")
    (element_type,) = get_args(target_type)
    for element in value:
        _validate_value(element, element_type)


def _validate_dictionary(
    value: object, target_type: Type[Dict[object, object]]
) -> None:
    if not isinstance(value, dict):
        raise InvalidJson(f"`{value}` is not a dictionary")

    key_type, value_type = get_args(target_type)
    for key, value in value.items():
        _validate_value(key, key_type)
        _validate_value(value, value_type)


def _validate_typed_dictionary(value: object, target_type: Type[object]) -> None:
    if not isinstance(value, dict):
        raise InvalidJson(f"`{value}` is not a dictionary")

    for key, value_type in target_type.__annotations__.items():
        if key not in value:
            raise InvalidJson(
                f"{value} of TypedDict {target_type} must contain key {key}"
            )
        _validate_value(value[key], value_type)


def _validate_value(value: object, target_type: Type[object]) -> None:
    if target_type is Any:
        return
    elif _is_list(target_type):
        _validate_list(value, cast(Type[List[object]], target_type))
    elif _is_dictionary(target_type):
        _validate_dictionary(value, cast(Type[Dict[object, object]], target_type))
    elif _is_typed_dictionary(target_type):
        _validate_typed_dictionary(value, target_type)
    elif is_optional_type(target_type):
        if value is None:
            return
        _validate_value(value, get_args(target_type)[0])
    else:
        if target_type not in [int, float, str, bool]:
            raise InvalidJson(f"Invalid value type {target_type}")
        if not isinstance(value, target_type):
            raise InvalidJson(f"`{value}` is not a {target_type}")


T = TypeVar("T")


def load(
    input_: Union[IO[str], IO[bytes]],
    target: Type[T],
    *,
    validate: bool = True,
) -> T:
    return loads(input_.read(), target, validate=validate)


def loads(input_: Union[str, bytes], target: Type[T], *, validate: bool = True) -> T:
    try:
        parsed = json.loads(input_)
        if validate:
            _validate_value(parsed, target)
        return parsed
    except Exception as exception:
        raise InvalidJson(str(exception))


def validate(input: object, target: Type[T]) -> T:
    _validate_value(input, target)
    return cast(T, input)
