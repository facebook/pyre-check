(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest
open Core

let test_check_union context =
  let assert_type_errors = assert_type_errors ~context in
  assert_type_errors
    {|
      from typing import Union
      class A:
        attr: bool = True
      class B:
        attr: str = "q"
      def foo(y: Union[A,B]) -> None:
        y.attr = False
    |}
    [
      "Incompatible attribute type [8]: Attribute `attr` declared in class `B` has type `str` but \
       is used as type `bool`.";
    ];
  assert_type_errors
    {|
      from typing import Union
      class A:
        attr: bool = True
      class B:
        attr: str = "q"
      def foo(y: Union[A,B]) -> None:
        y.attr = 2
    |}
    [
      "Incompatible attribute type [8]: Attribute `attr` declared in class `A` has type `bool` but \
       is used as type `int`.";
      "Incompatible attribute type [8]: Attribute `attr` declared in class `B` has type `str` but \
       is used as type `int`.";
      "Incompatible variable type [9]: y.attr is declared to have type `Union[bool, str]` but is \
       used as type `int`.";
    ];
  assert_type_errors
    {|
      from typing import Union
      class A:
        attr1: int = 2
      class B:
        attr: str = "q"
      def foo(y: Union[A,B]) -> None:
        y.attr1 = 3
    |}
    ["Undefined attribute [16]: `B` has no attribute `attr1`."];
  assert_type_errors
    {|
      from typing import Union
      class A:
        attr: Union[bool, int] = True
      class B:
        attr: Union[str, int] = "q"
      def foo(y: Union[A,B]) -> None:
        y.attr = 3
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Union[str, int]:
        return 1.0
    |}
    ["Incompatible return type [7]: Expected `Union[int, str]` but got `float`."];
  assert_type_errors
    {|
      from builtins import condition
      import typing
      def foo() -> typing.Union[str, int]:
        if condition():
          return 1
        else:
          return 'foo'
    |}
    [];
  assert_type_errors
    {|
      import typing
      def takes_int(a: int) -> None: ...
      def takes_str(a: str) -> None: ...

      def foo(a: typing.Union[str, int]) -> None:
        if isinstance(a, str):
          takes_str(a)
        else:
          takes_int(a)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(a: typing.Union[str, int, float]) -> int:
        if isinstance(a, int):
          return a
        else:
          return a
    |}
    ["Incompatible return type [7]: Expected `int` but got `Union[float, str]`."];
  assert_type_errors
    {|
      import typing
      T = typing.TypeVar('T', int, str)
      def foo(a: T) -> float:
        return a
    |}
    ["Incompatible return type [7]: Expected `float` but got `Variable[T <: [int, str]]`."];
  assert_type_errors
    {|
      import typing
      variable: typing.Union[typing.Optional[int], typing.Optional[str]] = None
      def ret_opt_int() -> typing.Optional[int]:
          return None
      variable = ret_opt_int()
    |}
    [];
  assert_type_errors
    {|
      import typing
      def foo(x: typing.Union[int, Undefined]) -> None:
        pass
      foo(1)
    |}
    ["Unbound name [10]: Name `Undefined` is used but not defined in the current scope."];
  assert_type_errors
    {|
      from builtins import Attributes, OtherAttributes
      import typing
      def foo(x: typing.Union[Attributes, OtherAttributes]) -> int:
        return x.int_attribute
    |}
    [];
  assert_type_errors
    {|
      from builtins import Attributes, OtherAttributes
      import typing
      def foo(x: typing.Union[Attributes, OtherAttributes]) -> int:
        return x.str_attribute
    |}
    [
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
      "Undefined attribute [16]: Item `Attributes` of `typing.Union[Attributes, OtherAttributes]` \
       has no attribute `str_attribute`.";
    ];
  assert_type_errors
    {|
      from builtins import Attributes, OtherAttributes
      import typing
      def foo(x: typing.Union[OtherAttributes, Attributes]) -> int:
        return x.str_attribute
    |}
    [
      "Incompatible return type [7]: Expected `int` but got `unknown`.";
      "Undefined attribute [16]: Item `Attributes` of `typing.Union[Attributes, OtherAttributes]` \
       has no attribute `str_attribute`.";
    ];
  assert_type_errors
    {|
      import typing
      class Foo:
        def derp(self) -> int: ...
      class Bar:
        def derp(self) -> int: ...
      def baz(x: typing.Union[Foo, Bar]) -> int:
        return x.derp()
    |}
    [];
  assert_type_errors
    {|
      import typing
      class Foo:
        def derp(self) -> int: ...
      class Bar:
        def herp(self) -> int: ...
      def baz(x: typing.Union[Foo, Bar]) -> int:
        return x.derp()
    |}
    ["Undefined attribute [16]: Item `Bar` of `typing.Union[Bar, Foo]` has no attribute `derp`."];

  (* We require that all elements in a union have the same method for `in`. *)
  assert_type_errors
    {|
      import typing
      class Equal:
        def __eq__(self, other: object) -> typing.List[int]:
          ...
      class GetItem:
        def __getitem__(self, x: int) -> Equal:
          ...
      class Contains:
        def __contains__(self, a: object) -> bool:
          ...
      def foo(a: typing.Union[GetItem, Contains]) -> None:
        5 in a
    |}
    [
      "Unsupported operand [58]: `in` is not supported for right operand type \
       `typing.Union[Contains, GetItem]`.";
    ];
  assert_type_errors
    {|
      import typing
      def f(x: typing.Optional[typing.Union[int, str]]) -> None:
        return g(x)

      def g(x: typing.Union[typing.Optional[int], typing.Optional[str]]) -> None:
        return f(x)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def f(x: typing.Union[int, typing.Tuple[int, int], typing.Optional[str]]) -> None:
        return g(x)

      def g(x: typing.Optional[typing.Union[int, str, typing.Tuple[int, int]]]) -> None:
        return f(x)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def f(x: typing.Union[typing.Optional[int], typing.Tuple[int, int], typing.Optional[str]]) \
          -> None:
        return g(x)

      def g(x: typing.Optional[typing.Union[int, str, typing.Tuple[int, int]]]) -> None:
        return f(x)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def f(x: typing.Union[typing.Optional[int]]) -> None:
        return g(x)

      def g(x: typing.Optional[int]) -> None:
        return f(x)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def f(x: typing.Union[int, str, None]) -> None:
        return g(x)

      def g(x: typing.Optional[typing.Union[int, str]]) -> None:
        return f(x)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def f(x: typing.Union[int, typing.Union[str, typing.Optional[typing.Tuple[int, int]]]]) -> \
          None:
        return g(x)

      def g(x: typing.Optional[typing.Union[int, str, typing.Tuple[int, int]]]) -> None:
        return f(x)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def f(x: typing.Union[int, typing.Optional[str]]) -> None:
        return g(x)

      def g(x: typing.Union[str, typing.Optional[int]]) -> None:
        return f(x)
    |}
    [];
  assert_type_errors
    {|
      import typing
      def f(x: typing.Union[int, str, typing.Tuple[int, int]]) -> None:
        pass

      x: typing.Union[int, typing.Optional[str]] = ...
      f(x)
    |}
    [
      "Incompatible parameter type [6]: In call `f`, for 1st positional only parameter expected \
       `Union[Tuple[int, int], int, str]` but got `Union[None, int, str]`.";
    ];
  assert_type_errors
    {|
      import typing
      class A:
        def __call__(self, x: int) -> bool:
          return True
      class B:
        def __call__(self, x: int) -> str:
          return "True"
      def f(x: typing.Union[A, B]) -> None:
        return x(8)

    |}
    ["Incompatible return type [7]: Expected `None` but got `Union[bool, str]`."];
  ()


let test_large_union_non_quadratic_time context =
  let large_union number_of_elements =
    let definitions =
      List.init number_of_elements ~f:(fun i -> Format.asprintf "      class Foo%03d: ..." i)
      |> String.concat ~sep:"\n"
    in
    let union =
      List.init number_of_elements ~f:(fun i -> Format.asprintf "Foo%03d" i)
      |> String.concat ~sep:","
    in
    Format.asprintf "\n%s\n      LargeUnion = Union[%s]" definitions union
  in
  let timer = Timer.start () in
  assert_type_errors
    ~context
    (Format.asprintf
       {|
      from typing import Literal, Union%s

      def bar(xs: LargeUnion) -> None:
        reveal_type(xs.__str__)
    |}
       (large_union 150))
    [
      "Revealed type [-1]: Revealed type for `xs.__str__` is \
       `Union[BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo000], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo001], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo002], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo003], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo004], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo005], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo006], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo007], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo008], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo009], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo010], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo011], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo012], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo013], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo014], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo015], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo016], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo017], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo018], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo019], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo020], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo021], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo022], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo023], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo024], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo025], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo026], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo027], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo028], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo029], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo030], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo031], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo032], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo033], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo034], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo035], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo036], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo037], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo038], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo039], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo040], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo041], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo042], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo043], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo044], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo045], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo046], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo047], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo048], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo049], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo050], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo051], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo052], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo053], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo054], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo055], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo056], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo057], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo058], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo059], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo060], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo061], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo062], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo063], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo064], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo065], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo066], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo067], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo068], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo069], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo070], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo071], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo072], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo073], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo074], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo075], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo076], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo077], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo078], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo079], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo080], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo081], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo082], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo083], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo084], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo085], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo086], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo087], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo088], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo089], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo090], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo091], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo092], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo093], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo094], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo095], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo096], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo097], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo098], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo099], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo100], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo101], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo102], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo103], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo104], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo105], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo106], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo107], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo108], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo109], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo110], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo111], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo112], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo113], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo114], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo115], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo116], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo117], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo118], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo119], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo120], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo121], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo122], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo123], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo124], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo125], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo126], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo127], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo128], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo129], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo130], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo131], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo132], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo133], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo134], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo135], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo136], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo137], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo138], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo139], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo140], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo141], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo142], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo143], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo144], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo145], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo146], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo147], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo148], \
       BoundMethod[typing.Callable(object.__str__)[[Named(self, object)], str], Foo149]]`.";
    ];
  (* Currently, with the efficient fold_divide_and_conquer, this test takes 1.4 seconds, with the
     inefficient fold it takes 60 seconds. Choosing a conservative 10 second cutoff*)
  let time = Timer.stop_in_sec timer in
  assert_bool
    (sprintf "analysis took %f seconds, more than 10. Check for a quadratic join" time)
    (Float.( <=. ) time 10.0);
  ()


let () =
  "union"
  >::: [
         "check_union" >:: test_check_union;
         "large_union_non_quadratic_time" >:: test_large_union_non_quadratic_time;
       ]
  |> Test.run
