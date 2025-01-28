/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::testcase_with_bug;

testcase_with_bug!(
    r#"
TODO zeina: use assert_type instead of reveal_type after I support most of these cases.

TODO zeina: 1- We need a generator type; 2- next keyword currently unsupported
    "#,
    test_generator,
    r#"
from typing import assert_type, Generator, Literal, Any, reveal_type

def yielding():
    yield 1

f = yielding()

next_f = next(f) # E: Could not find name `next`
reveal_type(next_f) # E: revealed type: Error
reveal_type(f) # E: revealed type: Generator[Literal[1], Unknown, None]

"#,
);

testcase_with_bug!(
    r#"
TODO zeina: Example of a generator with a return type. The return type here is wrong.
It should be Generator[Literal[1, 2], Any, Literal['done']] or Generator[int, Any, str]
    "#,
    test_generator_with_return,
    r#"

from typing import reveal_type

def gen_with_return():
    yield 1
    yield 2
    return "done"

reveal_type(gen_with_return()) # E: Generator[Literal[1, 2], Unknown, Literal['done']]

"#,
);

testcase_with_bug!(
    r#"
TODO zeina: we should correctly determine the send() type based on the signature of the generator. Additionally, we should correctly handle the return type of the generator.
    "#,
    test_generator_send,
    r#"

from typing import Generator, reveal_type

def accumulate(x: int) -> Generator[int, int, None]:
    yield x

gen = accumulate(10)
reveal_type(gen) # E: revealed type: Generator[int, int, None]
gen.send(5)

"#,
);

testcase_with_bug!(
    r#"
TODO zeina: we should correctly determine the send() type based on the signature of the generator. Additionally, we should correctly handle the return type of the generator.
    "#,
    test_generator_send_inference,
    r#"

from typing import Generator, reveal_type

class Yield: pass
class Send: pass
class Return: pass

def my_generator_nested() -> Generator[Yield, Send, Return]:
    yield Yield()
    return Return()

def my_generator() -> Generator[Yield, Send, Return]:
    s = yield Yield()
    y = yield from  my_generator_nested()

    reveal_type(s) # E: revealed type: Send
    reveal_type(y) # E: revealed type: Return

    return Return()

"#,
);

testcase_with_bug!(
    r#"
TODO zeina: I need to complain here too: y = yield from  my_generator_nested()
    "#,
    test_nested_generator_error,
    r#"

from typing import Generator, reveal_type

class Yield: pass
class Send: pass
class Return: pass

class Yield2: pass

def my_generator_nested() -> Generator[Yield2, Send, Return]:
    yield Yield() # E: type Yield is not assignable to Yield2
    return Return()

def my_generator() -> Generator[Yield, Send, Return]:
    s = yield Yield()
    y = yield from  my_generator_nested() # E: type Generator[Yield2, Send, Return] is not assignable to Generator[Yield, Send, Return]
    
    reveal_type(s) # E: revealed type: Send
    reveal_type(y) # E: revealed type: Return

    return Return()

"#,
);

testcase_with_bug!(
    "TODOs",
    test_yield_with_iterator,
    r#"
from typing import Iterator, reveal_type

def gen_numbers() -> Iterator[int]: 
    yield 1 
    yield 2 
    yield 3

reveal_type(gen_numbers()) # E: revealed type: Iterator[int]

"#,
);

testcase_with_bug!(
    r#"
TODO zeina: showcases return type inference for nested generators.
Type of "nested_generator()" should be "Generator[Literal[1, 2, 3], Unknown, None]"
and Type of "another_generator()" should be "Generator[Literal[2], Any, None]"
    "#,
    test_nested_generator,
    r#"
from typing import Generator, reveal_type

def nested_generator():
    yield 1
    yield from another_generator()
    yield 3

def another_generator():
    yield 2

reveal_type(nested_generator()) # E: revealed type: Generator[Literal[1, 2, 3], Unknown, None]
reveal_type(another_generator()) # E: revealed type: Generator[Literal[2], Unknown, None]

"#,
);

testcase_with_bug!(
    "TODO zeina: This should typecheck. Handle nested generator resulting type.",
    test_basic_generator_type,
    r#"
from typing import Generator, reveal_type

def f(value) -> Generator[int, None, None]:
    while True:
        yield value

reveal_type(f(3)) # E: revealed type: Generator[int, None, None]

"#,
);

testcase_with_bug!(
    "TODO zeina: This should typecheck. Handle nested generator resulting type.",
    test_parametric_generator_type,
    r#"
from typing import Generator, TypeVar, reveal_type

T = TypeVar('T')

def f(value: T) -> Generator[T, None, None]:
    while True:
        yield value

reveal_type(f(3)) # E: revealed type: Generator[int, None, None]

"#,
);

testcase_with_bug!(
    "TODO zeina: This should typecheck; we should first support async generators.",
    test_async_generator_basic_type,
    r#"
from typing import AsyncGenerator, reveal_type # E: Could not import `AsyncGenerator` from `typing`

async def async_count_up_to() -> AsyncGenerator[int, None]:
    yield 2 # E: Yield expression found but the function has an incompatible annotation `Error` # E: YieldFrom expression found but the function has an incompatible annotation `Error`

reveal_type(async_count_up_to()) # E: revealed type: Coroutine[Unknown, Unknown, Error]

"#,
);

testcase_with_bug!(
    "TODO zeina: This should typecheck; we should first support async generators.",
    test_async_generator_basic_inference,
    r#"
from typing import reveal_type

async def async_count_up_to():
    yield 2

reveal_type(async_count_up_to()) # E: revealed type: Coroutine[Unknown, Unknown, Generator[Literal[2], Unknown, None]]

"#,
);

testcase_with_bug!(
    "TODO zeina: We are incorrectly inferring generators that return generators.",
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
