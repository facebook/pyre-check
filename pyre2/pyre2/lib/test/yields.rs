/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;

testcase!(
    test_generator,
    r#"
from typing import assert_type, Generator, Literal, Any

def yielding():
    yield 1

f = yielding()

next_f = next(f)
assert_type(next_f, int)
assert_type(f, Generator[Literal[1], Any, None])

"#,
);

testcase!(
    test_generator_with_return,
    r#"

from typing import assert_type, Generator, Literal, Any

def gen_with_return():
    yield 1
    yield 2
    return "done"

assert_type(gen_with_return(), Generator[Literal[1, 2], Any, Literal['done']])

"#,
);

testcase!(
    test_generator_send,
    r#"

from typing import Generator, assert_type

def accumulate(x: int) -> Generator[int, int, None]:
    yield x

gen = accumulate(10)
assert_type(gen, Generator[int, int, None])
gen.send(5)

"#,
);

testcase!(
    test_generator_send_inference,
    r#"

from typing import Generator, assert_type

class Yield: pass
class Send: pass
class Return: pass

def my_generator_nested() -> Generator[Yield, Send, Return]:
    yield Yield()
    return Return()

def my_generator() -> Generator[Yield, Send, Return]:
    s = yield Yield()
    y = yield from  my_generator_nested()

    assert_type(s, Send)
    assert_type(y, Return)

    return Return()

"#,
);

testcase!(
    test_nested_generator_error,
    r#"

from typing import Generator, assert_type

class Yield: pass
class Send: pass
class Return: pass

class Yield2: pass

def my_generator_nested() -> Generator[Yield2, Send, Return]:
    yield Yield() # E: Type of yielded value `Yield` is not assignable to declared return type `Yield2`
    return Return()

def my_generator() -> Generator[Yield, Send, Return]:
    s = yield Yield()
    y = yield from  my_generator_nested() # E: Cannot yield from a generator of type `Generator[Yield2, Send, Return]`

    assert_type(s, Send)
    assert_type(y, Return)

    return Return()

"#,
);

testcase!(
    test_yield_with_iterator,
    r#"
from typing import Iterator, assert_type

def gen_numbers() -> Iterator[int]: 
    yield 1 
    yield 2 
    yield 3

assert_type(gen_numbers(), Iterator[int])
"#,
);

testcase!(
    test_nested_generator,
    r#"
from typing import Generator, assert_type, Literal, Any

def nested_generator():
    yield 1
    yield from another_generator()
    yield 3

def another_generator():
    yield 2

assert_type(nested_generator(), Generator[Literal[1, 2, 3], Any, None])
assert_type(another_generator(), Generator[Literal[2], Any, None])
"#,
);

testcase!(
    test_parametric_generator_type,
    r#"
from typing import Generator, TypeVar, assert_type

T = TypeVar('T')

def f(value: T) -> Generator[T, None, None]:
    while True:
        yield value

assert_type(f(3), Generator[int, None, None])
"#,
);

testcase!(
    test_async_generator_basic_type,
    r#"
from typing import AsyncGenerator, assert_type, Coroutine, Any

async def async_count_up_to() -> AsyncGenerator[int, None]:
    yield 2
assert_type(async_count_up_to(), AsyncGenerator[int, None])
"#,
);

testcase!(
    test_bare_yield,
    r#"
from typing import Generator

def bare_yield() -> Generator[int, None, None]:
    yield  # E: Expected to yield a value of type `int`

"#,
);

testcase!(
    test_async_infer_send,
    r#"
from typing import AsyncGenerator, assert_type

class Yield: pass
class Send: pass


async def my_generator() -> AsyncGenerator[Yield, Send]:
    s = yield Yield()

    assert_type(s, Send)

"#,
);

testcase!(
    test_async_error,
    r#"
from typing import AsyncGenerator, assert_type

class Yield: pass
class Send: pass


def my_generator() -> AsyncGenerator[Yield, Send]: # E: Generator function should return `Generator`
    s = yield Yield()
    assert_type(s, Send)

"#,
);

testcase!(
    test_async_generator_basic_inference,
    r#"
from typing import assert_type, Any, Literal, AsyncGenerator

async def async_count_up_to(): 
    yield 2
    return 4 # E: Return statement with value is not allowed in async generator
assert_type(async_count_up_to(), AsyncGenerator[Literal[2], Any])
"#,
);

testcase!(
    test_inferring_generators_that_return_generators,
    r#"
from typing import Any, Generator, assert_type

def generator() -> Generator[int, None, None]: ...

def generator2(x: int):
    yield x
    return generator()

assert_type(generator2(1), Generator[int, Any, Generator[int, None, None]])
"#,
);

testcase!(
    test_await_simple,
    r#"
from typing import Any, Awaitable, assert_type
class Foo(Awaitable[int]):
    pass
async def bar() -> str: ...

async def test() -> None:
    assert_type(await Foo(), int)
    assert_type(await bar(), str)
"#,
);

testcase!(
    test_await_literal,
    r#"
from typing import Awaitable, Literal
class Foo(Awaitable[Literal[42]]):
    pass
async def test() -> Literal[42]:
    return await Foo()
"#,
);

testcase!(
    test_await_non_awaitable,
    r#"
async def test() -> None:
    await 42  # E: Type `Literal[42]` is not awaitable
"#,
);

testcase!(
    test_await_wrong_await_return_type,
    r#"
class Foo:
    def __await__(self) -> int:
        ...

async def test() -> None:
    await Foo()  # E: Type `Foo` is not awaitable
"#,
);

testcase!(
    test_invalid_global_yield,
    r#"
yield 0  # E: Invalid `yield` outside of a function
yield from 0  # E: Invalid `yield from` outside of a function
"#,
);

testcase!(
    test_missing_return,
    r#"
from typing import Generator
def f() -> Generator[None, None, int]:  # E: Function declared to return `int` but is missing an explicit `return`
    yield None
    "#,
);

testcase!(
    test_bad_return,
    r#"
from typing import Generator
def f() -> Generator[None, None, int]:
    yield None
    return "oops"  # E: Returned type `Literal['oops']` is not assignable to declared return type `int`
    "#,
);

testcase!(
    test_async_iterate,
    r#"
from typing import AsyncGenerator, assert_type
async def gen() -> AsyncGenerator[int, None]:
    yield 2
async def test() -> None:
    async for x in gen():
        assert_type(x, int)
    async for y in [1, 2, 3]:  # E: Type `list[int]` is not an async iterable
        pass
    for z in gen():  # E: Type `AsyncGenerator[int, None]` is not iterable
        pass
"#,
);
