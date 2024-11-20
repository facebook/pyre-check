/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::simple_test;

simple_test!(
    test_simple_with,
    r#"
from typing import assert_type
from types import TracebackType
class Foo:
    def __enter__(self) -> int:
        ...
    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc_value: BaseException | None,
        traceback: TracebackType | None,
        /
    ) -> None:
        ...

with Foo() as foo:
    assert_type(foo, int)

bar: str = "abc"
with Foo() as bar: # E: EXPECTED int <: str
    assert_type(bar, str)
    "#,
);

simple_test!(
    test_simple_async_with,
    r#"
from typing import assert_type
from types import TracebackType
class Foo:
    async def __aenter__(self) -> int:
        ...
    async def __aexit__(
        self,
        exc_type: type[BaseException] | None,
        exc_value: BaseException | None,
        traceback: TracebackType | None,
        /
    ) -> None:
        ...

async def test() -> None:
    async with Foo() as foo:
        assert_type(foo, int)
    "#,
);

simple_test!(
    test_simple_with_error,
    r#"
def test_sync() -> None:
    with 42 as foo:  # E: has no attribute `__enter__` # E: has no attribute `__exit__`
        pass

async def test_async() -> None:
    async with "abc" as bar:  # E: has no attribute `__aenter__` # E: has no attribute `__aexit__`
        pass
    "#,
);

simple_test!(
    test_simple_with_wrong_enter_type,
    r#"
from types import TracebackType
class Foo:
    __enter__: int = 42
    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc_value: BaseException | None,
        traceback: TracebackType | None,
        /
    ) -> None:
        ...

with Foo() as foo:  # E: Expected `Foo.__enter__` to be a callable, got int
    pass
    "#,
);

simple_test!(
    test_with_wrong_exit_attribute_type,
    r#"
from types import TracebackType
class Foo:
    def __enter__(self) -> int: ...
    __exit__: int = 42

with Foo() as foo:  # E: Expected `Foo.__exit__` to be a callable, got int
    pass
    "#,
);

simple_test!(
    test_with_wrong_exit_argument_count,
    r#"
from typing import assert_type
class Foo:
    def __enter__(self) -> int:
        ...
    def __exit__(self) -> None:
        ...

with Foo() as foo:  # E: Expected 0 positional argument(s)
    pass
    "#,
);

simple_test!(
    test_with_wrong_exit_argument_type,
    r#"
from typing import assert_type
class Foo:
    def __enter__(self) -> int:
        ...
    def __exit__(self, exc_type: int, exc_value: int, traceback: int) -> None:
        ...

with Foo() as foo: # E: type[BaseException] | None <: int # E: BaseException | None <: int # E: TracebackType | None <: int
    pass
    "#,
);

simple_test!(
    test_with_wrong_return_type,
    r#"
from typing import assert_type
from types import TracebackType
class Foo:
    def __enter__(self) -> int:
        ...
    def __exit__(
        self,
        exc_type: type[BaseException] | None,
        exc_value: BaseException | None,
        traceback: TracebackType | None,
        /
    ) -> str:
        ...

with Foo() as foo:  # E: str <: bool | None
    pass
    "#,
);
