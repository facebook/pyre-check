# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

"""
This module provides a decorator called ``dataclass_merge``. It is intended to decorate
any dataclasses:

  @dataclass_merge
  @dataclass(frozen=True)
  class Foo:
      x: int | None = None
      y: int | None = None

The decorator will introduce an additional ``merge`` static method to the decorated
class:

  classs Foo:
      @staticmethod
      def merge(base: Foo, override: Foo) -> Foo:
          ...

The ``merge`` function attempts to create a "combined" object from its two arguments.
The combination procedure works by looking at each individual field of the two input
dataclass instances, merge the field of the two instances, and assemble the returned
object from all merged fields.

By default, field merging follow the "overwrite" policy, where fields in the
``override`` argument takes priority if it is not ``None``. For example:

  Foo.merge(base=Foo(x=1, y=2), override=Foo(x=3, y=4))     # Returns Foo(x=3, y=4)
  Foo.merge(base=Foo(x=1, y=2), override=Foo(x=3, y=None))  # Returns Foo(x=3, y=2)
  Foo.merge(base=Foo(x=1, y=2), override=Foo(x=None, y=4))  # Returns Foo(x=1, y=4)
  Foo.merge(base=Foo(x=1, y=2), override=Foo())             # Returns Foo(x=1, y=2)

Nested dataclasses will be merged recursively by default:

  @dataclass_merge
  @dataclass(frozen=True)
  class Bar:
      a: Foo
      b: str | None = None

  Bar.merge(Bar(Foo(x=1, y=2), "bar"), Bar(Foo(x=3, y=None)))
  # Returns Bar(a=Foo(x=3, y=2), b="bar")

To fine-tune merging policies, one can attach the ``merge_policy`` metadata to the
corresponding dataclass field:

  from dataclasses import dataclass, field
  from dataclasses_merge import dataclass_merge, Policy

  @dataclass_merge
  @dataclass(frozen=True)
  class Baz:
      x: int | None = field(
          default=None,
          metadata={"merge_policy": Policy.OVERWRITE}
      )
      y: List[int] = field(
          default_factory=list,
          metadata={"merge_policy": Policy.PREPEND}
      )
      z: str | None = field(
          default=None,
          metadata={"merge_policy": Policy.RAISE_WHEN_OVERWRITTEN}
      )
      w: Foo = field(
          default_factory=Foo,
          metadata={"merge_policy": Foo.merge}
      )

The "prepend" policy only works on list-typed fields. It prepends the list in
``override`` to the list in ``base``:

  Baz.merge(Baz(y=[1,2]), Baz(y=[3,4]))  # Returns Baz(y=[3,4,1,2])

The "raise_when_overwritten" policy forbids any overwriting: either the field
in ``base`` or the field in ``override`` can be ``None``, but not both:

  Baz.merge(Baz(z=1), Baz(z=None))     # OK
  Baz.merge(Baz(z=None), Baz(z=1))     # OK
  Baz.merge(Baz(z=None), Baz(z=None))  # OK
  Baz.merge(Baz(z=1), Baz(z=2))        # Excpetion raised!

And finally, clients can provide their own custom merge policy, by passing in a
callable to the ``merge_policy`` metadata. The provided callable will be served
as the "merge" function of the corresponding field.

All APIs provided in this module will only ever attempt to raise the
``DataclassMergeError`` exception. Callbacks passed as ``merge_policy`` are not
subject to this constraint, though.
"""

import dataclasses
import enum
from typing import Iterable, Optional, Type, TypeVar

T = TypeVar("T")


class Policy(str, enum.Enum):
    OVERWRITE: str = "overwrite"
    PREPEND: str = "prepend"
    RAISE_WHEN_OVERWRITTEN: str = "raise_when_overwritten"


class DataclassMergeError(Exception):
    pass


def _is_dataclass_instance(instance: object) -> bool:
    return dataclasses.is_dataclass(instance) and not isinstance(instance, type)


def _assert_is_dataclass_instance(instance: object) -> None:
    if not _is_dataclass_instance(instance):
        raise DataclassMergeError(
            f"Expected dataclass instace but got `{type(instance).__name__}`"
        )


def _get_field(name: str, instance: object) -> object:
    if not hasattr(instance, name):
        raise DataclassMergeError(
            f"Field `{name}` does not exist on object of type {type(instance).__name__}"
        )
    return getattr(instance, name)


def _merge_fields(
    fields: "Iterable[dataclasses.Field[object]]",
    base: object,
    override: object,
) -> Iterable[object]:
    def overwrite(base: Optional[T], override: Optional[T]) -> Optional[T]:
        return base if override is None else override

    def prepend(base: object, override: object, name: str) -> object:
        if not isinstance(base, list):
            raise DataclassMergeError(
                f"Field `{name}` is expected to be a list as it uses the 'prepend' "
                "merge policy. Got `{base}`."
            )
        if not isinstance(override, list):
            raise DataclassMergeError(
                f"Field `{name}` is expected to be a list as it uses the 'prepend' "
                "merge policy. Got `{override}`."
            )
        return override + base

    def raise_when_overwritten(
        base: Optional[T], override: Optional[T], name: str
    ) -> Optional[T]:
        if base is None:
            return override
        elif override is None:
            return base
        else:
            raise DataclassMergeError(
                f"Field `{name}` is not allowed to be overwritten."
            )

    def default_policy(cls: Type[object], base: object, override: object) -> object:
        if hasattr(cls, "merge"):
            # pyre-ignore[16]: Pyre does not recognize hasattr() check
            return cls.merge(base, override)
        return overwrite(base, override)

    def merge(field: "dataclasses.Field[object]") -> object:
        field_name = field.name
        base_value = _get_field(field_name, base)
        override_value = _get_field(field_name, override)

        merge_policy = field.metadata.get("merge_policy", None)
        if merge_policy == Policy.RAISE_WHEN_OVERWRITTEN:
            return raise_when_overwritten(base_value, override_value, field_name)
        elif merge_policy == Policy.PREPEND:
            return prepend(base_value, override_value, field_name)
        elif merge_policy == Policy.OVERWRITE:
            return overwrite(base_value, override_value)
        elif merge_policy is not None:
            return merge_policy(base_value, override_value)
        else:
            return default_policy(field.type, base_value, override_value)

    return (merge(field) for field in fields)


def dataclass_merge(cls: Type[T]) -> Type[T]:
    def merge(base: T, override: T) -> T:
        _assert_is_dataclass_instance(base)
        _assert_is_dataclass_instance(override)
        return cls(
            *_merge_fields(
                dataclasses.fields(cls),
                base,
                override,
            )
        )

    cls.merge = staticmethod(merge)
    return cls
