(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Analysis
open Ast
open Scope
open Test

let test_global _ =
  let assert_global ~expected source_text =
    let { Source.statements; _ } = Test.parse ~handle:"test.py" source_text in
    match statements with
    | [{ Node.value = Statement.Statement.Define define; _ }] ->
        let { Scope.globals; _ } = Scope.of_define_exn define in
        let expected = List.sort expected ~compare:Identifier.compare in
        let actual = Set.to_list globals in
        assert_equal
          ~cmp:(List.equal Identifier.equal)
          ~printer:(List.to_string ~f:Fn.id)
          expected
          actual
    | _ -> failwith "assert_global only accepts single-define sources"
  in
  assert_global {|
       def foo():
         pass
    |} ~expected:[];
  assert_global {|
       def foo():
         x = 1
    |} ~expected:[];
  assert_global {|
       def foo():
         global x
    |} ~expected:["x"];
  assert_global {|
       def foo():
         global x, x
    |} ~expected:["x"];
  assert_global {|
       def foo():
         global x, y
    |} ~expected:["x"; "y"];
  assert_global
    {|
       def foo():
         global x
         x = 1
         global y
         y = 2
    |}
    ~expected:["x"; "y"];
  assert_global
    {|
       def foo(flag: bool):
         if flag:
           global x
         else:
           pass
    |}
    ~expected:["x"];
  assert_global
    {|
       def foo(flag: bool):
         if flag:
           global x
         else:
           global y
    |}
    ~expected:["x"; "y"];
  assert_global
    {|
       def foo(flag: bool):
         while flag:
           global x
           x = 1
    |}
    ~expected:["x"];
  assert_global
    {|
       def foo():
         for x in range(10):
           global y
           y = x
    |}
    ~expected:["y"];
  assert_global
    {|
       def foo():
         with f() as x:
           global y
           y = x
    |}
    ~expected:["y"];
  assert_global
    {|
       def foo():
         try:
           global x
           print(y)
         except:
           global y
         finally:
           global z
    |}
    ~expected:["x"; "y"; "z"];
  assert_global
    {|
       def foo():
         for i in range(10):
           global x
           for j in range(20):
             global y
             print(x + y)
    |}
    ~expected:["x"; "y"];

  (* Global discovery does not go beyond nesting *)
  assert_global
    {|
       def foo():
         def bar():
           global x
           return x
         pass
    |}
    ~expected:[];
  assert_global
    {|
       def foo():
         class Bar:
           global x
         pass
    |}
    ~expected:[];
  assert_global
    {|
       def foo():
         class Bar:
           def __init__(self):
             global x
         pass
    |}
    ~expected:[];
  ()


let test_nonlocal _ =
  let assert_nonlocal ~expected source_text =
    let { Source.statements; _ } = Test.parse ~handle:"test.py" source_text in
    match statements with
    | [{ Node.value = Statement.Statement.Define define; _ }] ->
        let { Scope.nonlocals; _ } = Scope.of_define_exn define in
        let expected = List.sort expected ~compare:Identifier.compare in
        let actual = Set.to_list nonlocals in
        assert_equal
          ~cmp:(List.equal Identifier.equal)
          ~printer:(List.to_string ~f:Fn.id)
          expected
          actual
    | _ -> failwith "assert_nonlocal only accepts single-define sources"
  in
  assert_nonlocal {|
       def foo():
         pass
    |} ~expected:[];
  assert_nonlocal {|
       def foo():
         x = 1
    |} ~expected:[];
  assert_nonlocal {|
       def foo():
         nonlocal x
    |} ~expected:["x"];
  assert_nonlocal {|
       def foo():
         nonlocal x, x
    |} ~expected:["x"];
  assert_nonlocal {|
       def foo():
         nonlocal x, y
    |} ~expected:["x"; "y"];
  assert_nonlocal
    {|
       def foo():
         nonlocal x
         x = 1
         nonlocal y
         y = 2
    |}
    ~expected:["x"; "y"];
  assert_nonlocal
    {|
       def foo(flag: bool):
         if flag:
           nonlocal x
         else:
           pass
    |}
    ~expected:["x"];
  assert_nonlocal
    {|
       def foo(flag: bool):
         if flag:
           nonlocal x
         else:
           nonlocal y
    |}
    ~expected:["x"; "y"];
  assert_nonlocal
    {|
       def foo(flag: bool):
         while flag:
           nonlocal x
           x = 1
    |}
    ~expected:["x"];
  assert_nonlocal
    {|
       def foo():
         for x in range(10):
           nonlocal y
           y = x
    |}
    ~expected:["y"];
  assert_nonlocal
    {|
       def foo():
         with f() as x:
           nonlocal y
           y = x
    |}
    ~expected:["y"];
  assert_nonlocal
    {|
       def foo():
         try:
           nonlocal x
           print(y)
         except:
           nonlocal y
         finally:
           nonlocal z
    |}
    ~expected:["x"; "y"; "z"];
  assert_nonlocal
    {|
       def foo():
         for i in range(10):
           nonlocal x
           for j in range(20):
             nonlocal y
             print(x + y)
    |}
    ~expected:["x"; "y"];

  (* Nonlocal discovery does not go beyond nesting *)
  assert_nonlocal
    {|
       def foo():
         def bar():
           nonlocal x
           return x
         pass
    |}
    ~expected:[];
  assert_nonlocal
    {|
       def foo():
         class Bar:
           nonlocal x
         pass
    |}
    ~expected:[];
  assert_nonlocal
    {|
       def foo():
         class Bar:
           def __init__(self):
             nonlocal x
         pass
    |}
    ~expected:[];
  ()


module ExpectBinding = struct
  type t = {
    kind: Binding.Kind.t;
    location: Location.t;
    annotation: Expression.Expression.t option;
  }

  let create ?annotation kind location = { kind; location; annotation }
end

let assert_binding ~expected ~actual name =
  match actual, expected with
  | None, None -> ()
  | Some binding, None ->
      let message =
        Format.asprintf
          "Expected binding to not exist but found: %a"
          Sexp.pp_hum
          (Binding.sexp_of_t binding)
      in
      assert_failure message
  | None, Some _ ->
      let message = Format.asprintf "Expected binding to exist for %s but not found" name in
      assert_failure message
  | ( Some { Binding.kind; location; annotation; _ },
      Some
        {
          ExpectBinding.kind = expected_kind;
          location = expected_location;
          annotation = expected_annotation;
        } ) ->
      assert_equal
        ~cmp:[%compare.equal: Binding.Kind.t]
        ~printer:(fun kind -> Sexp.to_string_hum (Binding.Kind.sexp_of_t kind))
        expected_kind
        kind;
      assert_equal ~cmp:Location.equal ~printer:Location.show expected_location location;
      assert_equal
        ~cmp:(Option.equal Expression.equal)
        ~printer:(fun annotation ->
          Sexp.to_string_hum (Option.sexp_of_t Expression.sexp_of_t annotation))
        expected_annotation
        annotation


let location (start_line, start_column) (stop_line, stop_column) =
  {
    Location.path = !&"test";
    start = { Location.line = start_line; column = start_column };
    stop = { Location.line = stop_line; column = stop_column };
  }


let int_annotation = +Expression.Expression.Name (Expression.Name.Identifier "int")

let str_annotation = +Expression.Expression.Name (Expression.Name.Identifier "str")

let test_define_local_bindings _ =
  let assert_bindings ~expected source_text =
    let { Source.statements; _ } = Test.parse ~handle:"test.py" source_text in
    match statements with
    | [{ Node.value = Statement.Statement.Define define; _ }] ->
        let scope = Scope.of_define_exn define in
        List.iter expected ~f:(fun (name, expected) ->
            let actual = Scope.lookup_bindings scope name in
            assert_binding ~expected ~actual name)
    | _ -> failwith "assert_bindings only accepts single-define sources"
  in
  assert_bindings {|
    def foo():
      pass
  |} ~expected:["x", None];
  assert_bindings
    {|
    def foo(x):
      return x
  |}
    ~expected:["x", Some (ExpectBinding.create Binding.Kind.ParameterName (location (2, 8) (2, 9)))];
  assert_bindings
    {|
    def foo(x: int):
      return x
  |}
    ~expected:
      [
        ( "x",
          Some
            (ExpectBinding.create
               Binding.Kind.ParameterName
               (location (2, 8) (2, 9))
               ~annotation:int_annotation) );
      ];
  assert_bindings
    {|
    def foo(x: int, y: str):
      return x
  |}
    ~expected:
      [
        ( "x",
          Some
            (ExpectBinding.create
               Binding.Kind.ParameterName
               (location (2, 8) (2, 9))
               ~annotation:int_annotation) );
        ( "y",
          Some
            (ExpectBinding.create
               Binding.Kind.ParameterName
               (location (2, 16) (2, 17))
               ~annotation:str_annotation) );
      ];
  assert_bindings
    {|
    def foo():
      x = 1
      return x
  |}
    ~expected:["x", Some (ExpectBinding.create Binding.Kind.AssignTarget (location (3, 2) (3, 3)))];
  assert_bindings
    {|
    def foo():
      x: int = 1
      return x
  |}
    ~expected:
      [
        ( "x",
          Some
            (ExpectBinding.create
               Binding.Kind.AssignTarget
               (location (3, 2) (3, 3))
               ~annotation:int_annotation) );
      ];
  assert_bindings
    {|
    def foo():
      x, y = 1, "abc"
      return x
  |}
    ~expected:
      [
        "x", Some (ExpectBinding.create Binding.Kind.AssignTarget (location (3, 2) (3, 3)));
        "y", Some (ExpectBinding.create Binding.Kind.AssignTarget (location (3, 5) (3, 6)));
      ];
  assert_bindings
    {|
    def foo():
      [x, y] = [1, 42]
      return x + y
  |}
    ~expected:
      [
        "x", Some (ExpectBinding.create Binding.Kind.AssignTarget (location (3, 3) (3, 4)));
        "y", Some (ExpectBinding.create Binding.Kind.AssignTarget (location (3, 6) (3, 7)));
      ];
  assert_bindings
    {|
    def foo():
      def bar():
        pass
      pass
  |}
    ~expected:["bar", Some (ExpectBinding.create Binding.Kind.DefineName (location (3, 2) (4, 8)))];
  assert_bindings
    {|
    def foo():
      class Bar:
        pass
      pass
  |}
    ~expected:["Bar", Some (ExpectBinding.create Binding.Kind.ClassName (location (3, 2) (4, 8)))];
  assert_bindings
    {|
    def foo(flag: bool):
      if flag:
        x = 1
      else:
        y = 2
  |}
    ~expected:
      [
        "x", Some (ExpectBinding.create Binding.Kind.AssignTarget (location (4, 4) (4, 5)));
        "y", Some (ExpectBinding.create Binding.Kind.AssignTarget (location (6, 4) (6, 5)));
      ];
  assert_bindings
    {|
    def foo():
      for x in range(10):
        y: int = x
  |}
    ~expected:
      [
        "x", Some (ExpectBinding.create Binding.Kind.ForTarget (location (3, 6) (3, 7)));
        ( "y",
          Some
            (ExpectBinding.create
               Binding.Kind.AssignTarget
               (location (4, 4) (4, 5))
               ~annotation:int_annotation) );
      ];
  assert_bindings
    {|
    def foo(flag: bool):
      import bar
  |}
    ~expected:["bar", Some (ExpectBinding.create Binding.Kind.ImportName (location (3, 2) (3, 12)))];
  assert_bindings
    {|
    def foo(flag: bool):
      import bar as b
  |}
    ~expected:
      [
        "b", Some (ExpectBinding.create Binding.Kind.ImportName (location (3, 2) (3, 17)));
        "bar", None;
      ];
  assert_bindings
    {|
    def foo(flag: bool):
      import bar as b, baz
  |}
    ~expected:
      [
        "b", Some (ExpectBinding.create Binding.Kind.ImportName (location (3, 2) (3, 22)));
        "bar", None;
        "baz", Some (ExpectBinding.create Binding.Kind.ImportName (location (3, 2) (3, 22)));
      ];
  assert_bindings
    {|
    def foo():
      try:
        x = 1
      except KeyError as y:
        print(y)
      except Exception:
        pass
      finally:
        z: int = 2
  |}
    ~expected:
      [
        "x", Some (ExpectBinding.create Binding.Kind.AssignTarget (location (4, 4) (4, 5)));
        ( "y",
          Some
            (ExpectBinding.create
               Binding.Kind.ExceptTarget
               (location (3, 2) (10, 14))
               ~annotation:(+Expression.Expression.Name (Expression.Name.Identifier "KeyError"))) );
        ( "z",
          Some
            (ExpectBinding.create
               Binding.Kind.AssignTarget
               (location (10, 4) (10, 5))
               ~annotation:int_annotation) );
      ];
  assert_bindings
    {|
    def foo():
      while bar():
        x = 1
  |}
    ~expected:["x", Some (ExpectBinding.create Binding.Kind.AssignTarget (location (4, 4) (4, 5)))];
  assert_bindings
    {|
    def foo():
      with open("abc.txt", "r") as x:
        y = x
  |}
    ~expected:
      [
        "x", Some (ExpectBinding.create Binding.Kind.WithTarget (location (3, 31) (3, 32)));
        "y", Some (ExpectBinding.create Binding.Kind.AssignTarget (location (4, 4) (4, 5)));
      ];

  (* Bindings in nested scope should not leak into the nesting scope *)
  assert_bindings {|
    def foo():
      def bar():
        x = 1
  |} ~expected:["x", None];
  assert_bindings {|
    def foo():
      class Bar:
        x = 1
  |} ~expected:["x", None];
  assert_bindings {|
    def foo():
      return (lambda x: x)(42)
  |} ~expected:["x", None];
  assert_bindings {|
    def foo():
      return [x for x in range(42)]
  |} ~expected:["x", None];
  ()


let test_expression_local_bindings _ =
  let assert_bindings ~expected source_text =
    let { Source.statements; _ } = Test.parse ~handle:"test.py" source_text in
    match statements with
    | [{ Node.value = Statement.Statement.Expression expression; _ }] ->
        let scope = Scope.of_expression_exn expression in
        List.iter expected ~f:(fun (name, expected) ->
            let actual = Scope.lookup_bindings scope name in
            assert_binding ~expected ~actual name)
    | _ -> failwith "assert_bindings only accepts single-define sources"
  in
  assert_bindings "lambda _: None" ~expected:["x", None; "y", None];
  assert_bindings
    "lambda x: x"
    ~expected:
      [
        "x", Some (ExpectBinding.create Binding.Kind.ParameterName (location (1, 7) (1, 8)));
        "y", None;
      ];
  assert_bindings
    "lambda x, y: x"
    ~expected:
      [
        "x", Some (ExpectBinding.create Binding.Kind.ParameterName (location (1, 7) (1, 8)));
        "y", Some (ExpectBinding.create Binding.Kind.ParameterName (location (1, 10) (1, 11)));
      ];
  assert_bindings
    "(x for x in range(10))"
    ~expected:
      ["x", Some (ExpectBinding.create Binding.Kind.ComprehensionTarget (location (1, 7) (1, 8)))];
  assert_bindings
    "[x for x in range(10)]"
    ~expected:
      ["x", Some (ExpectBinding.create Binding.Kind.ComprehensionTarget (location (1, 7) (1, 8)))];
  assert_bindings
    "{x for x in range(10)}"
    ~expected:
      ["x", Some (ExpectBinding.create Binding.Kind.ComprehensionTarget (location (1, 7) (1, 8)))];
  assert_bindings
    "{x: 42 for x in range(10)}"
    ~expected:
      ["x", Some (ExpectBinding.create Binding.Kind.ComprehensionTarget (location (1, 11) (1, 12)))];
  assert_bindings
    "[y for x in foo for y in x]"
    ~expected:
      [
        "x", Some (ExpectBinding.create Binding.Kind.ComprehensionTarget (location (1, 7) (1, 8)));
        "y", Some (ExpectBinding.create Binding.Kind.ComprehensionTarget (location (1, 20) (1, 21)));
      ];
  ()


let () =
  "scope"
  >::: [
         "globals" >:: test_global;
         "nonlocals" >:: test_nonlocal;
         "define_local_bindings" >:: test_define_local_bindings;
         "expression_local_bindings" >:: test_expression_local_bindings;
       ]
  |> Test.run
