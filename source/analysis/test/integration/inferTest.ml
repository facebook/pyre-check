(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Test
open OUnit2
open IntegrationTest

let test_check_missing_parameter context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  let assert_strict_type_errors = assert_strict_type_errors ~context in
  (* No annotation given *)
  assert_default_type_errors {|
      def foo(x):
        return 1
    |} [];
  assert_strict_type_errors
    {|
      def foo(x) -> int:
        return 1
    |}
    ["Missing parameter annotation [2]: Parameter `x` has no type specified."];
  assert_strict_type_errors {|
      def foo(_x) -> int:
        return 1
    |} [];
  assert_strict_type_errors
    {|
      def foo(x = 1) -> int:
        return 1
    |}
    ["Missing parameter annotation [2]: Parameter `x` has type `int` but no type is specified."];

  (* typing.Any given *)
  assert_strict_type_errors
    {|
      import typing
      def foo(x: typing.Any) -> int:
        return 1
    |}
    ["Missing parameter annotation [2]: Parameter `x` must have a type other than `Any`."];
  assert_strict_type_errors
    {|
      import typing
      def foo(x: typing.Dict[str, typing.Any], y: typing.Dict[int, typing.Any]) -> int:
        return 1
    |}
    [
      "Missing parameter annotation [2]: Parameter `y` must have a type "
      ^ "that does not contain `Any`.";
    ];
  assert_strict_type_errors
    {|
      import typing
      MyType = typing.Any
      def foo(x: MyType) -> int:
        return 1
    |}
    ["Prohibited any [33]: `MyType` cannot alias to `Any`."];
  assert_strict_type_errors
    {|
      import typing
      def foo(x: typing.Any = unknown) -> int:
        return 1
    |}
    [
      "Missing parameter annotation [2]: Parameter `x` must have a type other than `Any`.";
      "Unbound name [10]: Name `unknown` is used but not defined in the current scope.";
    ];
  assert_strict_type_errors
    {|
      import typing
      def foo(x: typing.Dict[typing.Any, str]) -> int:
        return 1
    |}
    [
      "Missing parameter annotation [2]: Parameter `x` must have a type "
      ^ "that does not contain `Any`.";
    ];

  (* Special cases *)
  assert_type_errors
    {|
      def foo(x, *, force_named) -> int:
        return 1
    |}
    [
      "Missing parameter annotation [2]: Parameter `x` has no type specified.";
      "Missing parameter annotation [2]: Parameter `force_named` has no type specified.";
    ];
  assert_type_errors
    {|
      def foo(x: UnknownType) -> int:
        return 1
    |}
    ["Unbound name [10]: Name `UnknownType` is used but not defined in the current scope."];
  assert_type_errors
    {|
      import typing
      def foo(x: typing.Any, *args: typing.Any, **kwargs: typing.Any) -> int:
        return 1
    |}
    ["Missing parameter annotation [2]: Parameter `x` must have a type other than `Any`."]


let test_check_missing_return context =
  let assert_type_errors = assert_type_errors ~context in
  let assert_default_type_errors = assert_default_type_errors ~context in
  let assert_strict_type_errors = assert_strict_type_errors ~context in
  assert_type_errors
    {|
      def foo():
        return 1
    |}
    ["Missing return annotation [3]: Returning `int` but no return type is specified."];
  assert_strict_type_errors
    {|
      def foo():
        return 1
    |}
    ["Missing return annotation [3]: Returning `int` but no return type is specified."];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Any:
        return 1
    |}
    ["Missing return annotation [3]: Returning `int` but type `Any` is specified."];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Dict[str, typing.Any]:
        return {}

      def bar() -> typing.Dict[typing.Any, typing.Any]:
        return {}
    |}
    [
      "Missing return annotation [3]: Return type must be specified as type "
      ^ "that does not contain `Any`.";
    ];
  assert_type_errors
    {|
      import typing
      MyType = typing.Any
      def foo() -> MyType:
        return 1
    |}
    ["Prohibited any [33]: `MyType` cannot alias to `Any`."];
  assert_type_errors
    ~update_environment_with:[{ handle = "export.py"; source = "MyType = typing.List[typing.Any]" }]
    {|
      import typing
      from export import MyType
      MyTypeLocal = typing.List[typing.Any]
      def foo() -> MyType:
        return []
      def bar() -> MyTypeLocal:
        return []
    |}
    ["Prohibited any [33]: `MyTypeLocal` cannot alias to a type containing `Any`."];
  assert_type_errors
    {|
      def foo() -> None:
        return 1
    |}
    ["Incompatible return type [7]: Expected `None` but got `int`."];
  assert_type_errors
    {|
      def foo():
        return
    |}
    ["Missing return annotation [3]: Returning `None` but no return type is specified."];
  assert_type_errors
    {|
      def foo():
        return None
    |}
    ["Missing return annotation [3]: Returning `None` but no return type is specified."];
  assert_type_errors {|
      def foo() -> None:
        return None
    |} [];
  assert_type_errors
    {|
      def foo(a: int):
        if a > 10:
          return None
        else:
          return 1
    |}
    [
      "Missing return annotation [3]: Returning `typing.Optional[int]` but no return type is "
      ^ "specified.";
    ];
  assert_type_errors
    {|
      def foo() -> None:
        if a > 10:
          return None
        else:
          return 1
    |}
    [
      "Unbound name [10]: Name `a` is used but not defined in the current scope.";
      "Unsupported operand [58]: `>` is not supported for operand types `unknown` and `int`.";
      "Incompatible return type [7]: Expected `None` but got `int`.";
    ];
  assert_type_errors
    {|
      import typing
      def foo(x) -> typing.Any:
        return x
    |}
    [
      "Missing return annotation [3]: Return type must be specified as type other than `Any`.";
      "Missing parameter annotation [2]: Parameter `x` has no type specified.";
    ];
  assert_type_errors
    {|
      def foo(x):
        return x
    |}
    [
      "Missing return annotation [3]: Return type is not specified.";
      "Missing parameter annotation [2]: Parameter `x` has no type specified.";
    ];
  assert_type_errors
    {|
      def unknown_call():
        pass
      def foo():
        x = unknown_call()
        return x
    |}
    [
      "Missing return annotation [3]: Returning `None` but no return type is specified.";
      "Missing return annotation [3]: Return type is not specified.";
    ];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Any:
        x = unknown_call()
        return x
    |}
    [
      "Missing return annotation [3]: Return type must be specified as type other than `Any`.";
      "Unbound name [10]: Name `unknown_call` is used but not defined in the current scope.";
    ];
  assert_type_errors
    {|
       import typing
       def foo(x: typing.Any) -> typing.Any:
         return x
     |}
    [
      "Missing return annotation [3]: Return type must be specified as type other than `Any`.";
      "Missing parameter annotation [2]: Parameter `x` must have a type other than `Any`.";
    ];
  assert_type_errors
    {|
      import typing
      def foo() -> typing.Tuple[int, typing.Any]:
        return (1, 2)
    |}
    [
      "Missing return annotation [3]: Return type must be specified as type "
      ^ "that does not contain `Any`.";
    ];
  assert_type_errors
    {|
      def foo() -> list:
        return []
    |}
    [
      "Invalid type parameters [24]: Generic type `list` expects 1 type parameter, use \
       `typing.List` to avoid runtime subscripting errors.";
    ];

  (* Don't report in non-debug mode. *)
  assert_default_type_errors {|
      def foo():
        return 1
    |} [];
  assert_default_type_errors {|
      def foo():
        pass
    |} [];
  assert_default_type_errors {|
      def foo(x):
        return x
    |} [];
  assert_default_type_errors
    {|
      1 + 'asdf'  # report in top-level function
    |}
    ["Unsupported operand [58]: `+` is not supported for operand types `int` and `str`."];
  assert_type_errors
    {|
      from builtins import condition
      import typing
      def foo() -> typing.Any:
        if condition():
          return 1
    |}
    [
      "Missing return annotation [3]: Returning `typing.Optional[int]` but "
      ^ "type `Any` is specified.";
    ];
  assert_type_errors
    {|
      from builtins import int_to_bool
      import typing
      def foo(optional: typing.Optional[int]) -> typing.Any:
        return optional or int_to_bool(optional)
    |}
    [
      "Missing return annotation [3]: Returning `typing.Union[bool, int]` "
      ^ "but type `Any` is specified.";
      "Incompatible parameter type [6]: In call `int_to_bool`, for 1st positional only parameter \
       expected `int` but got `Optional[int]`.";
    ];
  assert_type_errors
    {|
      from builtins import int_to_bool
      import typing
      def foo(optional: typing.Optional[int]) -> typing.Any:
        return optional and int_to_bool(optional)
    |}
    [
      "Missing return annotation [3]: Returning `typing.Union[None, bool, int]` but type `Any` is \
       specified.";
    ];

  (* Joining. *)
  assert_type_errors
    {|
      from builtins import condition
      def foo():
        if condition():
          return 1
        else:
          return 'asdf'
    |}
    [
      "Missing return annotation [3]: Returning `typing.Union[int, str]` but no return type is "
      ^ "specified.";
    ];
  assert_type_errors
    {|
      from builtins import condition
      def foo():
        if condition():
          return 1
        elif condition():
          return 'asdf'
        else:
          return None
    |}
    [
      "Missing return annotation [3]: Returning `typing.Union[None, int, str]` but no return type \
       is specified.";
    ];
  assert_type_errors
    {|
      from builtins import condition
      def foo():
        if condition():
          return 1
        else:
          return 2.0
    |}
    ["Missing return annotation [3]: Returning `float` but no return type is specified."];
  assert_type_errors
    {|
      from builtins import condition
      def foo():
        if condition():
          return None
        else:
          return 'asdf'
    |}
    [
      "Missing return annotation [3]: Returning `typing.Optional[str]` but no return type is "
      ^ "specified.";
    ];
  assert_type_errors
    {|
      from builtins import condition, A, B
      def foo():
        if condition():
          return A()
        else:
          return B()
    |}
    ["Missing return annotation [3]: Returning `A` but no return type is specified."]


let () =
  "infer"
  >::: [
         "check_missing_parameter" >:: test_check_missing_parameter;
         "check_missing_return" >:: test_check_missing_return;
       ]
  |> Test.run
