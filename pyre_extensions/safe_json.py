# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict

import json
from typing import Dict, List, Type, TypeVar, cast

from typing_inspect import get_last_args, get_origin


class InvalidJson(json.JSONDecodeError):
    def __init__(self, message: str) -> None:
        super().__init__(message, "", 0)


def _is_list(target_type: Type[object]) -> bool:
    return get_origin(target_type) == List


def _is_dictionary(target_type: Type[object]) -> bool:
    return get_origin(target_type) == Dict


def _validate_list(value: object, target_type: Type[List[object]]) -> None:
    if not isinstance(value, list):
        raise InvalidJson(f"`{value}` is not a list")

    element_type, = get_last_args(target_type)
    for element in value:
        _validate_value(element, element_type)


def _validate_dictionary(
    value: object, target_type: Type[Dict[object, object]]
) -> None:
    if not isinstance(value, dict):
        raise InvalidJson(f"`{value}` is not a dictionary")

    key_type, value_type = get_last_args(target_type)
    for key, value in value.items():
        _validate_value(key, key_type)
        _validate_value(value, value_type)


def _validate_value(value: object, target_type: Type[object]) -> None:
    if _is_list(target_type):
        _validate_list(value, cast(Type[List[object]], target_type))
    elif _is_dictionary(target_type):
        _validate_dictionary(value, cast(Type[Dict[object, object]], target_type))
    else:
        if target_type not in [int, float, str, bool]:
            raise InvalidJson(f"Invalid value type {target_type}")
        if not isinstance(value, target_type):
            raise InvalidJson(f"`{value}` is not a {target_type}")


def _validate_toplevel(value: object, target_type: Type[object]) -> None:
    if _is_list(target_type):
        _validate_list(value, cast(Type[List[object]], target_type))
    elif _is_dictionary(target_type):
        _validate_dictionary(value, cast(Type[Dict[object, object]], target_type))
    else:
        raise NotImplementedError(f"Cannot safely parse {input}")


T = TypeVar("T")


def loads(input: str, target: Type[T]) -> T:
    try:
        parsed = json.loads(input)
        _validate_toplevel(parsed, target)
        return parsed
    except Exception as exception:
        raise InvalidJson(str(exception))
