/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase;
use crate::testcase_with_bug;

testcase_with_bug!(
    r#"
TODO zeina: use assert_type instead of reveal_type after I support most of these cases.

TODO: next keyword currently unsupported. Shelving for now as it requires other pyre features to be supported first.
    "#,
    test_generator,
    r#"
from typing import assert_type, Generator, Literal, Any, reveal_type

def yielding():
    yield 1

f = yielding()

next_f = next(f) # E: Could not find name `next`
reveal_type(next_f) # E: revealed type: Error
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
    yield Yield() # E: EXPECTED Yield <: Yield2
    return Return()

def my_generator() -> Generator[Yield, Send, Return]:
    s = yield Yield()
    y = yield from  my_generator_nested() # E: EXPECTED Generator[Yield2, Send, Return] <: Generator[Yield, Send, Return]

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
assert_type(async_count_up_to(), Coroutine[Any, Any, AsyncGenerator[int, None]])
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


def my_generator() -> AsyncGenerator[Yield, Send]:
    s = yield Yield() # E:  Return type of generator must be compatible with `AsyncGenerator[Yield, Send]`
    assert_type(s, Send)

"#,
);

testcase_with_bug!(
    "TODO zeina: The type here should be AsyncGenerator[Literal[2], Any]].",
    test_async_generator_basic_inference,
    r#"
from typing import assert_type, Generator, Coroutine, Any, Literal, AsyncGenerator

async def async_count_up_to(): 
    yield 2
    return 4 # E:  Return statement with type `Literal[4]` is not allowed in async generator 
assert_type(async_count_up_to(), Coroutine[Any, Any, AsyncGenerator[Literal[2], Any]])

"#,
);

testcase!(
    test_inferring_generators_that_return_generators,
    r#"
from typing import Generator, assert_type

def generator() -> Generator[int, None, None]: ...

def generator2(x: int):
    yield x
    return generator()

assert_type(generator2(1), Generator[int, Any, Generator[int, None, None]])
"#,
);
