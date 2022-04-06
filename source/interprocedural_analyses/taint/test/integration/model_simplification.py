# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_source
from typing import Any, cast, Dict, List


class RecordSchema:
    _META_PROP = ...


class DictRecord:
    items: Any = {}


class MutableRecord:
    __dict__: Dict[str, Any] = {}


def _is_dataclass_instance(obj) -> bool:
    ...


def fields(obj):
    ...


def asdict(obj: RecordSchema, *, dict_factory: Any = dict) -> Dict[str, Any]:
    """Return the fields of a RecordSchema instance as a new dictionary mapping
    field names to field values.
    """
    return _asdict_inner(obj, dict_factory)


def _asdict_inner(obj: Any, dict_factory: Any) -> Any:
    meta = getattr(obj, RecordSchema._META_PROP, {})
    if _is_dataclass_instance(obj):
        result = []
        for f in fields(obj):
            value = _asdict_inner(getattr(obj, f.name), dict_factory)
            field_meta = meta.get(f.name)
            if value is not None or (field_meta and field_meta.include_none):
                name_override = field_meta and field_meta.name
                result.append((name_override or f.name, value))
        return dict_factory(result)
    elif isinstance(obj, (list, tuple)):
        return type(obj)(cast(List[Any], (_asdict_inner(v, dict_factory) for v in obj)))
    elif isinstance(obj, DictRecord):
        result = []
        # item access in dict is already by serialized name
        for k, v in obj.items():
            value = _asdict_inner(v, dict_factory)
            field_meta = meta.get(k)
            if v is not None or (field_meta and field_meta.include_none):
                result.append((k, value))
        return dict_factory(result)
    elif isinstance(obj, MutableRecord):
        obj = obj.__dict__

        result = []
        for k, v in obj.items():
            value = _asdict_inner(v, dict_factory)
            field_meta = meta.get(k)
            if v is not None or (field_meta and field_meta.include_none):
                name_override = field_meta and field_meta.name
                result.append((name_override or k, value))
        return dict_factory(result)
    else:
        return obj


def asdict_test(obj):
    return asdict(obj)


def obscure_test(obj):
    return type(obj)(_test_source())
