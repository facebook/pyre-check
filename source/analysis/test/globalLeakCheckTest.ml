(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Analysis
open Test

let run_check_module
    ~type_environment
    ({ Ast.Source.module_path = { Ast.ModulePath.qualifier; _ }; _ } as source)
  =
  source
  |> Preprocessing.defines ~include_toplevels:false ~include_nested:true
  |> List.map ~f:(GlobalLeakCheck.check_define ~type_environment ~qualifier)
  |> List.concat


let assert_global_leak_errors ~context =
  let check ~environment ~source =
    run_check_module ~type_environment:(TypeEnvironment.read_only environment) source
  in
  assert_errors ~context ~check


let test_forward context =
  let assert_global_leak_errors = assert_global_leak_errors ~context in
  assert_global_leak_errors {|
      def foo():
         x = y
    |} [];
  assert_global_leak_errors {|
      my_global: int = 1
      def foo():
        x = 1
    |} [];
  assert_global_leak_errors
    (* my_global here is not actually a global, this is a valid assignment *)
    {|
      my_global: int = 1
      def foo():
        my_global = 2
    |}
    [];
  assert_global_leak_errors
    {|
      my_global: int = 1
      def foo():
        non_local = 2
        def inner():
          non_local = 3
        inner()
    |}
    [];
  assert_global_leak_errors
    {|
      my_global: int = 1
      def foo():
        global my_global
        my_global = 2
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global`."];
  assert_global_leak_errors
    {|
      my_global: int = 1
      def foo():
        global my_global
        my_global, y = 2, 3
    |}
    [ (* TODO (T142189949): leaks should be detected for tuple deconstruction into a global *) ];
  assert_global_leak_errors
    {|
      my_global: int = 1
      def foo():
        global my_global
        x, my_global = 2, 3
    |}
    [ (* TODO (T142189949): leaks should be detected for tuple deconstruction into a global *) ];
  assert_global_leak_errors
    {|
      my_global: int = 1
      def foo():
        def inner():
          global my_global
          my_global = 2
        inner()
    |}
    ["Global leak [3100]: Data is leaked to global `test.my_global`."];
  assert_global_leak_errors
    {|
      my_global: List[int] = []
      def foo():
        my_global.append(123)
    |}
    [ (* TODO (T142189949): leaks should be detected for global mutable function calls *) ];
  assert_global_leak_errors
    {|
      my_global: Dict[str, int] = {}
      def foo():
        my_global["x"] = 1
    |}
    [ (* TODO (T142189949): leaks should be detected for global mutable function calls *) ];
  assert_global_leak_errors
    {|
      class MyClass:
        x: int
        def __init__(self, x: int) -> None:
          self.x = x

      my_global: MyClass = MyClass(1)

      def foo():
        my_global.x = 2
    |}
    [ (* TODO (T142189949): leaks should be detected on object attribute mutations *) ];
  assert_global_leak_errors
    {|
    class MyClass:
      x: int
      def __init__(self, x: int) -> None:
        self.x = x

    def foo():
      MyClass.x = 2
    |}
    [ (* TODO (T142189949): leaks should be detected on class attribute mutations *) ];
  assert_global_leak_errors
    {|
      my_global: int = 1
      def foo():
         y = my_global
    |}
    [ (* TODO (T142189949): leaks should be detected on global assignment to a local variable *) ];
  assert_global_leak_errors
    {|
      my_global: int = 1
      def foo():
         x, y = my_global, 1
    |}
    [ (* TODO (T142189949): leaks should be detected on global assignment to a local variable
         through tuple deconstruction *) ];

  ()


let () = "global_leaks" >::: ["forward" >:: test_forward] |> Test.run
