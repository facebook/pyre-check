(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Test

let configuration = Configuration.create ()


let environment source =
  let environment = Environment.Builder.create ~configuration () in
  Environment.populate
    ~configuration
    (Environment.reader ~configuration environment) [parse source];
  environment


let test_lookup _ =
  let source =
    {|
      def foo(x):
          return 1
      def boo(x):
          return foo(x)
    |}
  in
  let environment = environment source in
  let parsed = parse source in
  let configuration = (Configuration.create ~debug:true ~infer:false ()) in
  let environment = (Environment.reader ~configuration environment) in
  let { TypeCheck.lookup; _ } = TypeCheck.check configuration environment parsed in
  assert_is_some lookup;
  assert_equal
    (Lookup.get_definition
       (Option.value_exn lookup)
       { Ast.Location.line = 5; column = 13 })
    (Some {
        Ast.Location.start = { Ast.Location.line = 2; column = 0 };
        stop = { Ast.Location.line = 3; column = 12 };
        path = "test.py"
      })


let test_lookup_across_files _ =
  let use_source =
    {|
      from define import zoo
      def boo(x):
          return zoo(x)
    |}
  in
  let define_source =
    {|
      def zoo(x):
          return 1
    |}
  in
  let environment = Environment.Builder.create ~configuration () in
  Environment.populate
    ~configuration
    (Environment.reader ~configuration environment) [
    parse ~qualifier:(Source.qualifier ~path:"use.py") ~path:"use.py" use_source;
    parse ~qualifier:(Source.qualifier ~path:"define.py") ~path:"define.py" define_source;
  ];
  let parsed = parse use_source in
  let configuration = (Configuration.create ~debug:true ~infer:false ()) in
  let environment = (Environment.reader ~configuration environment) in
  let { TypeCheck.lookup; _ } = TypeCheck.check configuration environment parsed in
  assert_is_some lookup;
  assert_equal
    (Lookup.get_definition
       (Option.value_exn lookup)
       { Ast.Location.line = 4; column = 13 })
    (Some {
        Ast.Location.start = { Ast.Location.line = 2; column = 0 };
        stop = { Ast.Location.line = 3; column = 12 };
        path = "define.py"
      })


let test_lookup_method _ =
  let source =
    {|
      class Foo(object):
          def a()->int:
              return 1

      def boo():
          x = Foo()
          return x.a()
    |}
  in
  let configuration = (Configuration.create ~debug:true ~infer:false ()) in
  let environment = environment source |> Environment.reader ~configuration in
  let parsed = parse source in
  let { TypeCheck.lookup; _ } = TypeCheck.check configuration environment parsed in
  assert_is_some lookup;
  assert_equal
    (Lookup.get_definition
       (Option.value_exn lookup)
       { Ast.Location.line = 8; column = 13 })
    (Some {
        Ast.Location.start = { Ast.Location.line = 3; column = 4 };
        stop = { Ast.Location.line = 4; column = 16 };
        path = "test.py"
      })

let () =
  "lookup">:::[
    "lookup">::test_lookup;
    "lookup_across_files">::test_lookup_across_files;
    "lookup_method">::test_lookup_method;
  ]
  |> run_test_tt_main
