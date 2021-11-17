(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Core
open Pyre
open Analysis
open Ast
open Test

let test_decorators_to_skip _ =
  let assert_decorators_to_skip source expected =
    assert_equal
      ~cmp:[%equal: Reference.t list]
      ~printer:[%show: Reference.t list]
      expected
      (trim_extra_indentation source
      |> InlineDecorator.decorators_to_skip ~path:(PyrePath.create_absolute "/root/test.py")
      |> List.sort ~compare:[%compare: Reference.t])
  in
  assert_decorators_to_skip
    {|
    @SkipDecoratorWhenInlining
    def foo.skip_this_decorator(f): ...

    @SkipObscure
    @SkipDecoratorWhenInlining
    @SkipOverrides
    def bar.skip_this_decorator2(f): ...

    @SkipObscure
    @SkipOverrides
    def bar.dont_skip(self: TaintInTaintOut[LocalReturn]): ...

    @Sanitize
    def bar.dont_skip2(self: TaintInTaintOut[LocalReturn]): ...

    def baz.dont_skip3(): ...
  |}
    [!&"bar.skip_this_decorator2"; !&"foo.skip_this_decorator"];
  assert_decorators_to_skip {|
    @CouldNotParse
  |} [];
  ()


let test_requalify_name _ =
  let open Expression in
  let assert_requalified ~old_qualifier ~new_qualifier name expected =
    assert_equal
      ~cmp:[%compare.equal: Name.t]
      ~printer:[%show: Name.t]
      expected
      (InlineDecorator.requalify_name ~old_qualifier ~new_qualifier name)
  in
  assert_requalified
    ~old_qualifier:!&"test.foo"
    ~new_qualifier:!&"test.foo.bar"
    (Name.Identifier "$local_test?foo$hello")
    (Name.Identifier "$local_test?foo?bar$hello");
  assert_requalified
    ~old_qualifier:!&"test.foo"
    ~new_qualifier:!&"test.foo.bar"
    (Name.Identifier "$local_test$hello")
    (Name.Identifier "$local_test$hello");
  assert_requalified
    ~old_qualifier:!&"test.foo"
    ~new_qualifier:!&"test.foo.bar"
    (Name.Identifier "hello")
    (Name.Identifier "hello");
  ()


let test_replace_signature _ =
  let assert_signature_replaced ~new_signature given expected =
    match
      ( (expected >>| fun expected -> parse expected |> Source.statements),
        parse given |> Source.statements,
        parse new_signature |> Source.statements )
    with
    | ( expected,
        [{ Node.value = Define given; _ }],
        [{ Node.value = Define { signature = { name = callee_name; _ } as new_signature; _ }; _ }] )
      ->
        let actual =
          InlineDecorator.replace_signature_if_always_passing_on_arguments
            ~callee_name:(Reference.show callee_name)
            ~new_signature
            given
        in
        let expected =
          match expected with
          | Some [{ Node.value = Define expected; _ }] -> Some expected
          | _ -> None
        in
        let printer = [%show: Statement.statement option] in
        let equal_optional_statements left right =
          match left, right with
          | Some left, Some right ->
              Statement.location_insensitive_compare
                (Node.create_with_default_location left)
                (Node.create_with_default_location right)
              = 0
          | None, None -> true
          | _ -> false
        in
        assert_equal
          ~cmp:equal_optional_statements
          ~printer
          ~pp_diff:(diff ~print:(fun format x -> Format.fprintf format "%s" (printer x)))
          (expected >>| fun x -> Statement.Statement.Define x)
          (actual >>| fun x -> Statement.Statement.Define x)
    | _ -> failwith "expected one statement each"
  in
  assert_signature_replaced
    ~new_signature:"def foo(y: str, z: int = 7) -> None: ..."
    {|
      def wrapper( *args, **kwargs) -> None:
        foo( *args, **kwargs)
        bar(args)
        baz(kwargs)
  |}
    (Some
       {|
      def wrapper(y: str, z: int = 7) -> None:
        _args = (y, z)
        _kwargs = {"y": y, "z": z}
        foo(y, z)
        bar(_args)
        baz(_kwargs)
  |});
  assert_signature_replaced
    ~new_signature:"def foo(y: str) -> None: ..."
    {|
      def wrapper( *args, **kwargs) -> None:
        foo("extra argument", *args, **kwargs)
        bar(args)
  |}
    None;
  (* Give up if the wrapper has anything more complex than `*args, **kwargs`. *)
  assert_signature_replaced
    ~new_signature:"def foo(y: str) -> None: ..."
    {|
      def wrapper(extra: int, *args, **kwargs) -> None:
        foo( *args, **kwargs)
  |}
    None;
  (* The wrapper still has `*args, **kwargs` but also gives them a type annotation. If it always
     passes on both to the callee, we use the callee's signature. *)
  assert_signature_replaced
    ~new_signature:"def foo(y: str) -> None: ..."
    {|
      def wrapper( *args: int, **kwargs: str) -> None:
        foo( *args, **kwargs)
        bar(args)
  |}
    (Some
       {|
      def wrapper(y: str) -> None:
        _args = (y,)
        _kwargs = {"y": y}
        foo(y)
        bar(_args)
  |});
  (* If the callee expects `args` and `kwargs`, make sure not to confuse them with our synthetic
     locals `_args` and `_kwargs`.

     Note that we conservatively store all the arguments to both `_args` and `_kwargs` so that we
     catch any flows to sinks within the decorator. We are more precise about what we pass to the
     callee since false positives there might be more annoying. *)
  assert_signature_replaced
    ~new_signature:"def foo(y: str, *args: int, **kwargs: str) -> None: ..."
    {|
      def wrapper( *args, **kwargs) -> None:
        foo( *args, **kwargs)
        baz(kwargs)
  |}
    (Some
       {|
      def wrapper(y: str, *args: int, **kwargs: str) -> None:
        _args = (y, *args)
        _kwargs = {"y": y, **kwargs}
        foo(y, *args, **kwargs)
        baz(_kwargs)
  |});
  (* Ensure that the return type is unchanged regardless of the new signature. *)
  assert_signature_replaced
    ~new_signature:"def foo(y: str) -> None: ..."
    {|
      def wrapper( *args, **kwargs) -> int:
        foo( *args, **kwargs)
        return 1
  |}
    (Some
       {|
      def wrapper(y: str) -> int:
        _args = (y,)
        _kwargs = {"y": y}
        foo(y)
        return 1
  |});
  assert_signature_replaced
    ~new_signature:"def foo(some_parameter: int, x: str, y: bool) -> None: ..."
    {|
      def wrapper(some_parameter: int, *args: object, **kwargs: object) -> int:
        foo(some_parameter, *args, **kwargs)
        print(some_parameter)
        print(args, kwargs)
  |}
    (Some
       {|
      def wrapper(some_parameter: int, x: str, y: bool) -> int:
        _args = (x, y)
        _kwargs = {"x": x, "y": y}
        foo(some_parameter, x, y)
        print(some_parameter)
        print(_args, _kwargs)
  |});
  assert_signature_replaced
    ~new_signature:"def foo(prefix1: int, prefix2: str, x: str, y: bool) -> None: ..."
    {|
      def wrapper(some_parameter1: int, some_parameter2: str, *args: object, **kwargs: object) -> int:
        foo(some_parameter1, some_parameter2, *args, **kwargs)
        print(some_parameter1, some_parameter2)
        print(args, kwargs)
  |}
    (Some
       {|
      def wrapper(prefix1: int, prefix2: str, x: str, y: bool) -> int:
        _args = (x, y)
        _kwargs = {"x": x, "y": y}
        foo(prefix1, prefix2, x, y)
        print(prefix1, prefix2)
        print(_args, _kwargs)
  |});
  (* Don't get confused if the original function uses `x` as a parameter name. *)
  assert_signature_replaced
    ~new_signature:"def foo(some_parameter: int, x: str, y: bool) -> None: ..."
    {|
      def wrapper(x: int, *args: object, **kwargs: object) -> int:
        foo(x, *args, **kwargs)
        print(x)
        print(args, kwargs)
  |}
    (Some
       {|
      def wrapper(some_parameter: int, x: str, y: bool) -> int:
        _args = (x, y)
        _kwargs = {"x": x, "y": y}
        foo(some_parameter, x, y)
        print(some_parameter)
        print(_args, _kwargs)
  |});
  assert_signature_replaced
    ~new_signature:"def foo(some_parameter: int, x: str, y: bool) -> None: ..."
    {|
      def wrapper(x: int, *args: object, **kwargs: object) -> int:
        foo(x, "extra argument", *args, **kwargs)
        print(x)
        print(args, kwargs)
  |}
    None;
  assert_signature_replaced
    ~new_signature:"def foo(some_parameter: int, x: str, y: bool) -> None: ..."
    {|
      def wrapper(x: int, *args: object, **kwargs: object) -> int:
        foo("not x", *args, **kwargs)
        print(x)
        print(args, kwargs)
  |}
    None;
  assert_signature_replaced
    ~new_signature:"def foo() -> None: ..."
    {|
      def wrapper(x: int, *args: object, **kwargs: object) -> int:
        foo(x, *args, **kwargs)
        print(x)
        print(args, kwargs)
  |}
    None;
  assert_signature_replaced
    ~new_signature:"def foo(some_parameter: int, *args: int, **kwargs: str) -> None: ..."
    {|
      def wrapper(some_parameter: int, *args: object, **kwargs: object) -> int:
        foo(some_parameter, *args, **kwargs)
        print(some_parameter)
        print(args, kwargs)
  |}
    (Some
       {|
      def wrapper(some_parameter: int, *args: int, **kwargs: str) -> int:
        _args = ( *args,)
        _kwargs = { **kwargs}
        foo(some_parameter, *args, **kwargs)
        print(some_parameter)
        print(_args, _kwargs)
  |});
  assert_signature_replaced
    ~new_signature:"async def foo(y: str, z: int) -> None: ..."
    {|
      async def wrapper( *args, **kwargs) -> None:
        await foo( *args, **kwargs)
  |}
    (Some
       {|
      async def wrapper(y: str, z: int) -> None:
        _args = (y, z)
        _kwargs = {"y": y, "z": z}
        await foo(y, z)
  |});
  ()


let test_rename_local_variables _ =
  let assert_renamed ~pairs given expected =
    match parse expected |> Source.statements, parse given |> Source.statements with
    | [{ Node.value = Define expected; _ }], [{ Node.value = Define given; _ }] ->
        let actual = InlineDecorator.rename_local_variables ~pairs given in
        let printer = [%show: Statement.statement] in
        assert_equal
          ~cmp:(fun left right ->
            Statement.location_insensitive_compare
              (Node.create_with_default_location left)
              (Node.create_with_default_location right)
            = 0)
          ~printer
          ~pp_diff:(diff ~print:(fun format x -> Format.fprintf format "%s" (printer x)))
          (Statement.Statement.Define expected)
          (Statement.Statement.Define actual)
    | _ -> failwith "expected one statement each"
  in
  assert_renamed
    ~pairs:["y", "y_renamed"; "z", "z_renamed"; "not_found", "not_found_renamed"]
    {|
    def wrapper(y: str, z: int) -> None:
      x = y + z
      foo(y, z)
  |}
    {|
    def wrapper(y: str, z: int) -> None:
      x = y_renamed + z_renamed
      foo(y_renamed, z_renamed)
  |};
  assert_renamed
    ~pairs:["y", "y_renamed"; "y", "y_duplicate"]
    {|
    def wrapper(y: str, z: int) -> None:
      x = y + z
      foo(y, z)
  |}
    {|
    def wrapper(y: str, z: int) -> None:
      x = y + z
      foo(y, z)
  |};
  ()


let test_uniquify_names _ =
  let assert_uniquified given expected =
    assert_equal
      ~cmp:[%equal: Reference.t list]
      ~printer:[%show: Reference.t list]
      (List.map expected ~f:Reference.create)
      (List.map given ~f:Reference.create
      |> InlineDecorator.uniquify_names ~get_reference:Fn.id ~set_reference:(fun reference _ ->
             reference))
  in
  assert_uniquified
    ["a.b"; "a.c"; "a.b"; "a.b"; "a.c"; "foo"]
    ["a.b3"; "a.c2"; "a.b2"; "a.b"; "a.c"; "foo"];
  assert_uniquified [] [];
  ()


let () =
  "inline"
  >::: [
         "decorators_to_skip" >:: test_decorators_to_skip;
         "requalify_name" >:: test_requalify_name;
         "replace_signature" >:: test_replace_signature;
         "rename_local_variables" >:: test_rename_local_variables;
         "uniquify_names" >:: test_uniquify_names;
       ]
  |> Test.run
