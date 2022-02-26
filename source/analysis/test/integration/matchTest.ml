(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_simple context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      def foo(x: int) -> None:
        y = None
        match x:
          case 1:
            y = 5
          case 2:
            y = "Hello"
        reveal_type(y)
    |}
    ["Revealed type [-1]: Revealed type for `y` is `typing.Union[None, int, str]`."];
  assert_type_errors
    {|
      def foo(x: int) -> None:
        y = None
        match x:
          case 1:
            y = 5
          case _:
            y = "Hello"
        reveal_type(y)
    |}
    ["Revealed type [-1]: Revealed type for `y` is `typing.Union[int, str]`."];
  assert_type_errors
    {|
      def foo(x: int) -> None:
        match x:
          case 1:
            y = 5
          case 2:
            y = "Hello"
        print(y)
    |}
    ["Uninitialized local [61]: Local variable `y` is undefined, or not always defined."];
  assert_type_errors
    {|
      def http_error(status: int) -> str:
        match status:
          case 400:
            return "Bad request"
          case 404:
            return "Not found"
          case 418:
            return "I'm a teapot"
    |}
    ["Incompatible return type [7]: Expected `str` but got implicit return value of `None`."];
  assert_type_errors
    {|
      def http_error(status: int) -> str:
        match status:
          case 400:
            return "Bad request"
          case 404:
            return "Not found"
          case 418:
            return "I'm a teapot"
          case _:
            return "Something's wrong with the Internet"
    |}
    [];
  assert_type_errors
    {|
      def http_error(status: None | int) -> str:
        match status:
          case None:
            return "No status"
          case 418:
            return "I'm a teapot"
          case _:
            reveal_type(status)
            return "Something's wrong with the Internet"
    |}
    ["Revealed type [-1]: Revealed type for `status` is `typing.Optional[int]` (inferred: `int`)."];
  assert_type_errors
    {|
      def test_capture(status: None | int) -> None:
        match status:
          case None:
            pass
          case x:
            reveal_type(status)
            reveal_type(x)
    |}
    [
      "Revealed type [-1]: Revealed type for `status` is `typing.Optional[int]` (inferred: `int`).";
      "Revealed type [-1]: Revealed type for `x` is `int`.";
    ];
  assert_type_errors
    {|
      def test_bools(status: bool) -> None:
        match status:
          case True:
            reveal_type(status)
          case False:
            reveal_type(status)
    |}
    [
      "Revealed type [-1]: Revealed type for `status` is `typing_extensions.Literal[True]`.";
      "Revealed type [-1]: Revealed type for `status` is `typing_extensions.Literal[False]`.";
    ];
  (* TODO(T106580135): The imprecision comes from lack of refinement for equality. *)
  assert_type_errors
    {|
      from typing import Tuple
      def print_point(subject: int | str) -> None:
        match subject:
          case "hello":
            reveal_type(subject)
    |}
    ["Revealed type [-1]: Revealed type for `subject` is `typing.Union[int, str]`."];
  assert_type_errors
    {|
      from typing import Tuple
      def print_point(subject: int) -> None:
        match subject:
          case "hello":
            x = 5
          case _:
            x = 5.0
        reveal_type(x)
    |}
    ["Revealed type [-1]: Revealed type for `x` is `float`."];
  ()


let test_pattern context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import Tuple
      def print_point(point: Tuple[int, int]) -> None:
        match point:
          case (0, 0):
            print("Origin")
          case (0, y_only):
            reveal_type(y_only)
            print(f"Y={y_only}")
          case (x_only, 0):
            reveal_type(x_only)
            print(f"X={x_only}")
          case (x, y):
            reveal_type(x)
            reveal_type(y)
            print(f"X={x}, Y={y}")
          case _:
            raise ValueError("Not a point")
    |}
    [
      "Revealed type [-1]: Revealed type for `y_only` is `int`.";
      "Revealed type [-1]: Revealed type for `x_only` is `int`.";
      "Revealed type [-1]: Revealed type for `x` is `int`.";
      "Revealed type [-1]: Revealed type for `y` is `int`.";
    ];
  assert_type_errors
    {|
      from typing import Tuple
      def foo(pair: Tuple[str, int]) -> None:
        match pair:
          case (x, y):
            reveal_type(x)
            reveal_type(y)
        match pair:
          case (z, 0):
            pass
          case (0, z):
            pass
        reveal_type(z)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `str`.";
      "Revealed type [-1]: Revealed type for `y` is `int`.";
      "Revealed type [-1]: Revealed type for `z` is `typing.Union[int, str]`.";
    ];
  assert_type_errors
    {|
      from typing import List
      def split(x: str) -> List[str]:
        ...
      def test_or_pattern(command: str) -> None:
        num_items: int = 0
        match split(command):
          case ["north"] | ["go", "north"]:
            ...
          case ["get", item] | ["pick", "up", item] | ["pick", item, "up"]:
            reveal_type(item)
            ...
          case ["give", "away", num_items] | ["donate", num_items]:
            ...
    |}
    [
      "Revealed type [-1]: Revealed type for `item` is `str`.";
      "Incompatible variable type [9]: num_items is declared to have type `int` but is used as \
       type `str`.";
      "Incompatible variable type [9]: num_items is declared to have type `int` but is used as \
       type `str`.";
    ];
  assert_type_errors
    {|
      from typing import Tuple
      def test_variable_length_pattern(subject: Tuple[int, str, float]) -> None:
        match subject:
          case [first, *_, last]:
            reveal_type(first)
            reveal_type(last)
    |}
    [
      "Revealed type [-1]: Revealed type for `first` is `int`.";
      "Revealed type [-1]: Revealed type for `last` is `float`.";
    ];
  assert_type_errors
    {|
      from typing import Tuple
      def test_subpattern(point: Tuple[int, int] | None) -> None:
        match point:
          case None:
            pass
          case (x, y) as p:
            reveal_type(x)
            reveal_type(y)
            reveal_type(p)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `int`.";
      "Revealed type [-1]: Revealed type for `y` is `int`.";
      "Revealed type [-1]: Revealed type for `p` is `Tuple[int, int]`.";
    ];
  (* TODO(T106580135): The imprecision comes from lack of refinement for equality. *)
  assert_type_errors
    {|
      from typing import Tuple
      def print_point(point: Tuple[int, int | str]) -> None:
        match point:
          case (1 as x, "hello" as y):
            reveal_type(x)
            reveal_type(y)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `int`.";
      "Revealed type [-1]: Revealed type for `y` is `typing.Union[int, str]`.";
    ];
  (* TODO(T106580135): The imprecision comes from lack of refinement for equality. *)
  assert_type_errors
    {|
      from typing import Tuple
      def print_point(point: Tuple[int, str] | Tuple[str, int]) -> None:
        match point:
          case (1 as x, "hello" as y):
            reveal_type(x)
            reveal_type(y)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `typing.Union[int, str]`.";
      "Revealed type [-1]: Revealed type for `y` is `typing.Union[int, str]`.";
    ];
  (* TODO(T105331662): Refinement on length needed. *)
  assert_type_errors
    {|
      from typing import Tuple
      def print_point(point: Tuple[int, str]) -> None:
        match point:
          case (x, y, z):
            reveal_type(x)
            reveal_type(y)
            reveal_type(z)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `int`.";
      "Revealed type [-1]: Revealed type for `y` is `str`.";
      "Revealed type [-1]: Revealed type for `z` is `typing.Union[int, str]`.";
    ];
  (* TODO(T105331662): Refinement on length needed. *)
  assert_type_errors
    {|
      from typing import Tuple
      def print_point(point: Tuple[int, str] | Tuple[float, complex, bool]) -> None:
        match point:
          case (x, y):
            reveal_type(x)
            reveal_type(y)
          case (u, v, w):
            reveal_type(u)
            reveal_type(v)
            reveal_type(w)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `float`.";
      "Revealed type [-1]: Revealed type for `y` is `typing.Union[complex, str]`.";
      "Revealed type [-1]: Revealed type for `u` is `float`.";
      "Revealed type [-1]: Revealed type for `v` is `typing.Union[complex, str]`.";
      "Revealed type [-1]: Revealed type for `w` is `typing.Union[bool, int, str]`.";
    ];
  ()


let test_class context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from dataclasses import dataclass

      @dataclass
      class Point:
        x: int
        y: int

      def where_is(point: Point) -> None:
        match point:
          case Point(x=0, y=0):
            print("Origin")
          case Point(x=0, y=y):
            reveal_type(y)
            print(f"Y={y}")
          case Point(x=x, y=0):
            reveal_type(x)
            print(f"X={x}")
          case Point():
            print("Somewhere else")
          case _:
            print("Not a point")
    |}
    [
      "Revealed type [-1]: Revealed type for `y` is `int`.";
      "Revealed type [-1]: Revealed type for `x` is `int`.";
    ];
  assert_type_errors
    {|
      from dataclasses import dataclass

      @dataclass
      class Foo:
        x: int

      @dataclass
      class Bar:
        y: float

      def test_narrowing(an_object: Foo | Bar) -> None:
        match an_object:
          case Foo(x=1) as foo_object:
            reveal_type(foo_object)
          case Bar(x=1):
            # Bad pattern, Bar does not have attribute x.
            ...
          case Bar() as bar_object:
            reveal_type(bar_object)
    |}
    [
      "Revealed type [-1]: Revealed type for `foo_object` is `Foo`.";
      "Undefined attribute [16]: `Bar` has no attribute `x`.";
      "Revealed type [-1]: Revealed type for `bar_object` is `Bar`.";
    ];
  assert_type_errors
    {|
      from dataclasses import dataclass
      from typing import Tuple

      @dataclass
      class Point:
        x: int
        y: int

      def test_multiple_subpatterns(line: Tuple[Point, Point]) -> None:
        match line:
          case Point(x=0, y=0), Point (x=x, y=y) as p:
            reveal_type(x)
            reveal_type(y)
            reveal_type(p)
          case Point(x=x1, y=y1) as p1, Point(x=x2, y=y2) as p2:
            reveal_type(x1)
            reveal_type(y1)
            reveal_type(p1)
            reveal_type(x2)
            reveal_type(y2)
            reveal_type(p2)
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `int`.";
      "Revealed type [-1]: Revealed type for `y` is `int`.";
      "Revealed type [-1]: Revealed type for `p` is `Point`.";
      "Revealed type [-1]: Revealed type for `x1` is `int`.";
      "Revealed type [-1]: Revealed type for `y1` is `int`.";
      "Revealed type [-1]: Revealed type for `p1` is `Point`.";
      "Revealed type [-1]: Revealed type for `x2` is `int`.";
      "Revealed type [-1]: Revealed type for `y2` is `int`.";
      "Revealed type [-1]: Revealed type for `p2` is `Point`.";
    ];
  ()


let test_mapping context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import Dict
      def process(action: Dict[str, object]) -> None:
        match action:
          case {"text": message, "color": c}:
            print(message)
          case {"sleep": duration}:
            print("Sleeping")
          case {"sound": url, "format": "ogg"}:
            reveal_type(url)
            print("Playing sound")
          case {"sound": _, "format": _}:
            print("Unsupported audio format")
    |}
    ["Revealed type [-1]: Revealed type for `url` is `object`."];
  assert_type_errors
    {|
      from typing import Dict
      def process(action: Dict[str, str|int]) -> None:
        match action:
          case {"text": str() as message, "color": str() as c}:
            reveal_type(message)
            reveal_type(c)
          case {"sleep": int() as duration}:
            reveal_type(duration)
          case {"sound": str() as url, "format": "ogg"}:
            reveal_type(url)
          case {"sound": _, "format": _}:
            print("Unsupported audio format")
    |}
    [
      "Revealed type [-1]: Revealed type for `message` is `str`.";
      "Revealed type [-1]: Revealed type for `c` is `str`.";
      "Revealed type [-1]: Revealed type for `duration` is `int`.";
      "Revealed type [-1]: Revealed type for `url` is `str`.";
    ];
  assert_type_errors
    {|
      from typing import Dict
      def process(action: Dict[str, int]) -> None:
        match action:
          case {"a": _, **rest}:
            reveal_type(rest)
    |}
    ["Revealed type [-1]: Revealed type for `rest` is `Dict[str, int]`."];
  assert_type_errors
    {|
      from typing import Mapping
      def process(action: Mapping[str, int]) -> None:
        match action:
          case {"a": _, **rest}:
            reveal_type(rest)
    |}
    ["Revealed type [-1]: Revealed type for `rest` is `typing.Dict[str, int]`."];
  ()


let test_guard context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import Tuple
      def test_guard(command: Tuple[str, int|str]) -> None:
        match command:
          case ["type", x] if isinstance(x, int):
            reveal_type(x)
            print("int")
          case ["type", y] if isinstance(y, str):
            reveal_type(y)
            print("str")
    |}
    [
      "Revealed type [-1]: Revealed type for `x` is `int`.";
      "Revealed type [-1]: Revealed type for `y` is `str`.";
    ];
  ()


let test_star context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import List
      def process(numbers: List[int]) -> None:
        match numbers:
          case [first, *rest, last]:
            reveal_type(first)
            reveal_type(rest)
            reveal_type(last)
    |}
    [
      "Revealed type [-1]: Revealed type for `first` is `int`.";
      "Revealed type [-1]: Revealed type for `rest` is `List[int]`.";
      "Revealed type [-1]: Revealed type for `last` is `int`.";
    ];
  assert_type_errors
    {|
      from typing import List
      def process(numbers: List[int]) -> None:
        match numbers:
          case [*_, last]:
            reveal_type(last)
    |}
    ["Revealed type [-1]: Revealed type for `last` is `int`."];
  assert_type_errors
    {|
      from typing import List
      def process(numbers: List[int]) -> None:
        match numbers:
          case [first, *rest]:
            reveal_type(first)
            reveal_type(rest)
    |}
    [
      "Revealed type [-1]: Revealed type for `first` is `int`.";
      "Revealed type [-1]: Revealed type for `rest` is `List[int]`.";
    ];
  assert_type_errors
    {|
      from typing import List
      def process(numbers: List[int]) -> None:
        match numbers:
          case [*rest]:
            reveal_type(rest)
    |}
    ["Revealed type [-1]: Revealed type for `rest` is `List[int]`."];
  (* TODO(T105342940): For rest, the imprecision comes from literal slice resolution. It should
     technically be possible to resolve to List[bool, complex, float]. *)
  assert_type_errors
    {|
      from typing import Tuple
      def process(numbers: Tuple[int, float, complex, bool, str]) -> None:
        match numbers:
          case [first, *rest, last]:
            reveal_type(first)
            reveal_type(rest)
            reveal_type(last)
    |}
    [
      "Revealed type [-1]: Revealed type for `first` is `int`.";
      "Revealed type [-1]: Revealed type for `rest` is `typing.List[typing.Union[bool, complex, \
       float, int, str]]`.";
      "Revealed type [-1]: Revealed type for `last` is `str`.";
    ];
  ()


let test_enum context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from enum import Enum

      class Color(Enum):
        RED = 0
        GREEN = 1
        BLUE = 2

      def foo(color: Color) -> None:
        match color:
          case Color.RED:
            print("I see red!")
            reveal_type(color)
          case Color.GREEN:
            print("Grass is green")
          case Color.BLUE:
            print("I'm feeling the blues :(")
          case RED:
            reveal_type(RED)
    |}
    [
      "Revealed type [-1]: Revealed type for `color` is `Color`.";
      "Revealed type [-1]: Revealed type for `RED` is `Color`.";
    ];
  (* TODO(T83684046): We should consider throwing a more direct error here about a non-exhaustive
     match. *)
  assert_type_errors
    {|
      from enum import Enum

      class Color(Enum):
        RED = 0
        GREEN = 1

      def test_incomplete_enum_match(color: Color) -> int:
        match color:
          case Color.RED:
            return 1
    |}
    ["Incompatible return type [7]: Expected `int` but got implicit return value of `None`."];
  (* TODO(T83684046): We could consider throwing an error here. *)
  assert_type_errors
    {|
      from enum import Enum

      class Color(Enum):
        RED = 0
        GREEN = 1

      def print_color(color: Color) -> None:
        match color:
          case Color.RED:
            print("RED")
    |}
    [];
  (* TODO(T83684046): A wildcard should be possible to avoid without Pyre complaining. *)
  assert_type_errors
    {|
      from enum import Enum

      class Color(Enum):
        RED = 0
        GREEN = 1

      def test_exhaustiveness(color: Color) -> int:
        match color:
          case Color.RED:
            return 1
          case Color.GREEN:
            return 2
          case _:
            assert False, "unreachable"
    |}
    [];
  ()


let test_match_args context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import Literal, Tuple
      class Foo():
        __match_args__ : Tuple[Literal["hello"], Literal["world"]] = ("hello", "world")
        hello: int = 0
        world: str = ""

      def f(x: None | Foo) -> None:
        match x:
          case Foo(h, w):
            reveal_type(h)
            reveal_type(w)
    |}
    [
      "Revealed type [-1]: Revealed type for `h` is `int`.";
      "Revealed type [-1]: Revealed type for `w` is `str`.";
    ];
  assert_type_errors
    {|
      from typing import Literal, Tuple
      class Foo():
        __match_args__ = ("hello", "world")
        hello: int = 0
        world: str = ""

      def f(x: None | Foo) -> None:
        match x:
          case Foo(h, w):
            reveal_type(h)
            reveal_type(w)
    |}
    [
      "Revealed type [-1]: Revealed type for `h` is `int`.";
      "Revealed type [-1]: Revealed type for `w` is `str`.";
    ];
  assert_type_errors
    {|
      from dataclasses import dataclass

      @dataclass
      class Point:
        x: int
        y: int

      def where_is(point: Point) -> None:
        match point:
          case Point(0, 0):
            print("Origin")
          case Point(0, y):
            reveal_type(y)
            print(f"Y={y}")
          case Point(x, 0):
            reveal_type(x)
            print(f"X={x}")
          case Point():
            print("Somewhere else")
          case _:
            print("Not a point")
    |}
    [
      "Revealed type [-1]: Revealed type for `y` is `int`.";
      "Revealed type [-1]: Revealed type for `x` is `int`.";
    ];
  assert_type_errors
    {|
      def foo(x: int|str) -> None:
          match x:
              case int(y):
                  reveal_type(y)
    |}
    ["Revealed type [-1]: Revealed type for `y` is `int`."];
  ()


let test_syntax context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      def foo(subject: int) -> None:
        match subject:
          case x:
            pass
          case x:
            pass
    |}
    ["Parsing failure [404]: This pattern makes remaining patterns unreachable."];
  assert_type_errors
    {|
      def foo(subject: int) -> None:
        match subject:
          case _:
            pass
          case 42:
            pass
    |}
    ["Parsing failure [404]: This pattern makes remaining patterns unreachable."];
  ()


let test_isinstance context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import List, Dict
      def foo(subject: List[str] | Dict[int, str]) -> None:
        match subject:
          case {1:"x", 2:"y"}:
            reveal_type(subject)
          case ["x", "y"]:
            reveal_type(subject)
    |}
    [
      "Revealed type [-1]: Revealed type for `subject` is `Dict[int, str]`.";
      "Revealed type [-1]: Revealed type for `subject` is `List[str]`.";
    ];
  assert_type_errors
    {|
      from typing import Sequence, Mapping
      class MySequence(Sequence[str]):
        ...
      class MyMapping(Mapping[int, str]):
        ...
      def foo(subject: MySequence | MyMapping) -> None:
        match subject:
          case {}:
            reveal_type(subject)
          case []:
            reveal_type(subject)
    |}
    [
      "Revealed type [-1]: Revealed type for `subject` is `MyMapping`.";
      "Revealed type [-1]: Revealed type for `subject` is `MySequence`.";
    ];
  ()


let () =
  "match"
  >::: [
         "simple" >:: test_simple;
         "pattern" >:: test_pattern;
         "class" >:: test_class;
         "mapping" >:: test_mapping;
         "guard" >:: test_guard;
         "star" >:: test_star;
         "enum" >:: test_enum;
         "match_args" >:: test_match_args;
         "syntax" >:: test_syntax;
         "isinstance" >:: test_isinstance;
       ]
  |> Test.run
