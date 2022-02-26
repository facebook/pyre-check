(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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
  assert_global
    {|
       def foo():
         match subject:
           case 1:
             global x
    |}
    ~expected:["x"];
  assert_global
    {|
       def foo():
         match subject:
           case 1:
             global x
           case 2:
             global y
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
  assert_nonlocal
    {|
       def foo():
         match subject:
           case 1:
             nonlocal x
    |}
    ~expected:["x"];
  assert_nonlocal
    {|
       def foo():
         match subject:
           case 1:
             nonlocal x
           case 2:
             nonlocal y
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
  }

  let create kind location = { kind; location }
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
  | ( Some { Binding.kind; location; _ },
      Some { ExpectBinding.kind = expected_kind; location = expected_location } ) ->
      assert_equal
        ~cmp:[%compare.equal: Binding.Kind.t]
        ~printer:(fun kind -> Sexp.to_string_hum (Binding.Kind.sexp_of_t kind))
        expected_kind
        kind;
      assert_equal ~cmp:Location.equal ~printer:Location.show expected_location location


let location (start_line, start_column) (stop_line, stop_column) =
  {
    Location.start = { Location.line = start_line; column = start_column };
    stop = { Location.line = stop_line; column = stop_column };
  }


let int_annotation start end_ =
  Node.create
    (Expression.Expression.Name (Expression.Name.Identifier "int"))
    ~location:(location start end_)


let str_annotation start end_ =
  Node.create
    (Expression.Expression.Name (Expression.Name.Identifier "str"))
    ~location:(location start end_)


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
    ~expected:
      [
        ( "x",
          Some
            (ExpectBinding.create
               Binding.Kind.(ParameterName { star = None; index = 0; annotation = None })
               (location (2, 8) (2, 9))) );
      ];
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
               Binding.Kind.(
                 ParameterName
                   { star = None; index = 0; annotation = Some (int_annotation (2, 11) (2, 14)) })
               (location (2, 8) (2, 14))) );
      ];
  assert_bindings
    {|
    def foo(x: int, y: str):
      return x + int(y)
  |}
    ~expected:
      [
        ( "x",
          Some
            (ExpectBinding.create
               Binding.Kind.(
                 ParameterName
                   { star = None; index = 0; annotation = Some (int_annotation (2, 11) (2, 14)) })
               (location (2, 8) (2, 14))) );
        ( "y",
          Some
            (ExpectBinding.create
               Binding.Kind.(
                 ParameterName
                   { star = None; index = 1; annotation = Some (str_annotation (2, 19) (2, 22)) })
               (location (2, 16) (2, 22))) );
      ];
  assert_bindings
    {|
    def foo( *args: int):
      return args[0]
  |}
    ~expected:
      [
        ( "args",
          Some
            (ExpectBinding.create
               Binding.Kind.(
                 ParameterName
                   {
                     star = Some Star.Once;
                     index = 0;
                     annotation = Some (int_annotation (2, 16) (2, 19));
                   })
               (location (2, 10) (2, 19))) );
      ];
  assert_bindings
    {|
    def foo( **kwargs: int):
      return kwargs["derp"]
  |}
    ~expected:
      [
        ( "kwargs",
          Some
            (ExpectBinding.create
               Binding.Kind.(
                 ParameterName
                   {
                     star = Some Star.Twice;
                     index = 0;
                     annotation = Some (int_annotation (2, 19) (2, 22));
                   })
               (location (2, 11) (2, 22))) );
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
               Binding.Kind.(
                 ParameterName
                   { star = None; index = 0; annotation = Some (int_annotation (2, 11) (2, 14)) })
               (location (2, 8) (2, 14))) );
        ( "y",
          Some
            (ExpectBinding.create
               Binding.Kind.(
                 ParameterName
                   { star = None; index = 1; annotation = Some (str_annotation (2, 19) (2, 22)) })
               (location (2, 16) (2, 22))) );
      ];
  assert_bindings
    {|
    def foo():
      x = 1
      return x
  |}
    ~expected:
      ["x", Some (ExpectBinding.create (Binding.Kind.AssignTarget None) (location (3, 2) (3, 3)))];
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
               (Binding.Kind.AssignTarget (Some (int_annotation (3, 5) (3, 8))))
               (location (3, 2) (3, 3))) );
      ];
  assert_bindings
    {|
    def foo():
      x, y = 1, "abc"
      return x
  |}
    ~expected:
      [
        "x", Some (ExpectBinding.create (Binding.Kind.AssignTarget None) (location (3, 2) (3, 3)));
        "y", Some (ExpectBinding.create (Binding.Kind.AssignTarget None) (location (3, 5) (3, 6)));
      ];
  assert_bindings
    {|
    def foo():
      [x, y] = [1, 42]
      return x + y
  |}
    ~expected:
      [
        "x", Some (ExpectBinding.create (Binding.Kind.AssignTarget None) (location (3, 3) (3, 4)));
        "y", Some (ExpectBinding.create (Binding.Kind.AssignTarget None) (location (3, 6) (3, 7)));
      ];
  assert_bindings
    {|
    def foo():
      x, *y = 1, 2, 3
      return x + sum(y)
  |}
    ~expected:
      [
        "x", Some (ExpectBinding.create (Binding.Kind.AssignTarget None) (location (3, 2) (3, 3)));
        "y", Some (ExpectBinding.create (Binding.Kind.AssignTarget None) (location (3, 6) (3, 7)));
      ];
  assert_bindings
    {|
    def foo():
      def bar():
        pass
      pass
  |}
    ~expected:
      [
        ( "bar",
          let signature =
            {
              Statement.Define.Signature.name = !&"bar";
              parameters = [];
              decorators = [];
              return_annotation = None;
              async = false;
              generator = false;
              parent = None;
              nesting_define = None;
            }
          in
          Some (ExpectBinding.create (Binding.Kind.DefineName signature) (location (3, 2) (4, 8)))
        );
      ];
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
        "x", Some (ExpectBinding.create (Binding.Kind.AssignTarget None) (location (4, 4) (4, 5)));
        "y", Some (ExpectBinding.create (Binding.Kind.AssignTarget None) (location (6, 4) (6, 5)));
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
               (Binding.Kind.AssignTarget (Some (int_annotation (4, 7) (4, 10))))
               (location (4, 4) (4, 5))) );
      ];
  assert_bindings
    {|
    def foo():
      for x, *y in [[1], [2,3], [4,5,6]]:
        pass
  |}
    ~expected:
      [
        "x", Some (ExpectBinding.create Binding.Kind.ForTarget (location (3, 6) (3, 7)));
        "y", Some (ExpectBinding.create Binding.Kind.ForTarget (location (3, 10) (3, 11)));
      ];
  assert_bindings
    {|
    def foo(flag: bool):
      import bar
      import _baz
  |}
    ~expected:
      [
        ( "bar",
          Some
            (ExpectBinding.create Binding.Kind.(ImportName Import.Module) (location (3, 9) (3, 12)))
        );
        ( "_baz",
          Some
            (ExpectBinding.create Binding.Kind.(ImportName Import.Module) (location (4, 9) (4, 13)))
        );
      ];
  assert_bindings
    {|
    def foo(flag: bool):
      import bar.baz
  |}
    ~expected:
      [
        ( "bar",
          Some
            (ExpectBinding.create Binding.Kind.(ImportName Import.Module) (location (3, 9) (3, 16)))
        );
      ];
  assert_bindings
    {|
    def foo(flag: bool):
      import bar as b
  |}
    ~expected:
      [
        ( "b",
          Some
            (ExpectBinding.create Binding.Kind.(ImportName Import.Module) (location (3, 9) (3, 17)))
        );
        "bar", None;
      ];
  assert_bindings
    {|
    def foo(flag: bool):
      from bar import b
      from bar import _c
  |}
    ~expected:
      [
        ( "b",
          Some
            (ExpectBinding.create
               Binding.Kind.(ImportName (Import.From !&"bar"))
               (location (3, 18) (3, 19))) );
        ( "_c",
          Some
            (ExpectBinding.create
               Binding.Kind.(ImportName (Import.From !&"bar"))
               (location (4, 18) (4, 20))) );
        "bar", None;
      ];
  assert_bindings
    {|
    def foo(flag: bool):
      from bar import b as baz
  |}
    ~expected:
      [
        "b", None;
        "bar", None;
        ( "baz",
          Some
            (ExpectBinding.create
               Binding.Kind.(ImportName (Import.From !&"bar"))
               (location (3, 18) (3, 26))) );
      ];
  assert_bindings
    {|
    def foo(flag: bool):
      import bar as b, baz
  |}
    ~expected:
      [
        ( "b",
          Some
            (ExpectBinding.create Binding.Kind.(ImportName Import.Module) (location (3, 9) (3, 17)))
        );
        "bar", None;
        ( "baz",
          Some
            (ExpectBinding.create
               Binding.Kind.(ImportName Import.Module)
               (location (3, 19) (3, 22))) );
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
        "x", Some (ExpectBinding.create (Binding.Kind.AssignTarget None) (location (4, 4) (4, 5)));
        ( "y",
          Some
            (ExpectBinding.create
               (Binding.Kind.ExceptTarget
                  (Some
                     (Node.create
                        ~location:(location (5, 9) (5, 17))
                        (Expression.Expression.Name
                           (Expression.create_name "KeyError" ~location:(location (5, 9) (5, 17)))))))
               (location (3, 2) (10, 14))) );
        ( "z",
          Some
            (ExpectBinding.create
               (Binding.Kind.AssignTarget (Some (int_annotation (10, 7) (10, 10))))
               (location (10, 4) (10, 5))) );
      ];
  assert_bindings
    {|
    def foo():
      while bar():
        x = 1
  |}
    ~expected:
      ["x", Some (ExpectBinding.create (Binding.Kind.AssignTarget None) (location (4, 4) (4, 5)))];
  assert_bindings
    {|
    def foo():
      with open("abc.txt", "r") as x:
        y = x
  |}
    ~expected:
      [
        "x", Some (ExpectBinding.create Binding.Kind.WithTarget (location (3, 31) (3, 32)));
        "y", Some (ExpectBinding.create (Binding.Kind.AssignTarget None) (location (4, 4) (4, 5)));
      ];
  assert_bindings
    {|
    def foo():
      y = (x := 1)
  |}
    ~expected:
      [
        "y", Some (ExpectBinding.create (Binding.Kind.AssignTarget None) (location (3, 2) (3, 3)));
        "x", Some (ExpectBinding.create Binding.Kind.WalrusTarget (location (3, 7) (3, 8)));
      ];
  assert_bindings
    {|
    def foo():
      if (match := pattern.search(data)) is not None:
        pass
  |}
    ~expected:
      ["match", Some (ExpectBinding.create Binding.Kind.WalrusTarget (location (3, 6) (3, 11)))];
  assert_bindings
    {|
    def foo():
      while chunk := file.read(8192):
        pass
  |}
    ~expected:
      ["chunk", Some (ExpectBinding.create Binding.Kind.WalrusTarget (location (3, 8) (3, 13)))];
  assert_bindings
    {|
    def foo():
      return [y := expensive(), y ** 2, y ** 3]
  |}
    ~expected:
      ["y", Some (ExpectBinding.create Binding.Kind.WalrusTarget (location (3, 10) (3, 11)))];
  assert_bindings
    {|
    def foo():
      yield [y for x in data if (y := f(x)) is not None]
  |}
    ~expected:
      ["y", Some (ExpectBinding.create Binding.Kind.WalrusTarget (location (3, 29) (3, 30)))];
  assert_bindings
    {|
    def foo():
      match subject:
        case 1:
          x = 1
  |}
    ~expected:
      ["x", Some (ExpectBinding.create Binding.Kind.(AssignTarget None) (location (5, 6) (5, 7)))];
  assert_bindings
    {|
    def foo():
      match subject:
        case 1:
          x = 1
        case 2:
          y = 1
  |}
    ~expected:
      [
        "x", Some (ExpectBinding.create Binding.Kind.(AssignTarget None) (location (5, 6) (5, 7)));
        "y", Some (ExpectBinding.create Binding.Kind.(AssignTarget None) (location (7, 6) (7, 7)));
      ];
  assert_bindings
    {|
    def foo():
      match subject:
        case x:
          pass
  |}
    ~expected:["x", Some (ExpectBinding.create Binding.Kind.MatchTarget (location (4, 9) (4, 10)))];
  assert_bindings
    {|
    def foo():
      match subject:
        case [_, *x]:
          pass
  |}
    ~expected:["x", Some (ExpectBinding.create Binding.Kind.MatchTarget (location (4, 13) (4, 15)))];
  (* TODO(T107008455): Add location to MatchMapping's rest for better location. *)
  assert_bindings
    {|
    def foo():
      match subject:
        case {1:1, **x}:
          pass
  |}
    ~expected:["x", Some (ExpectBinding.create Binding.Kind.MatchTarget (location (4, 9) (4, 19)))];
  assert_bindings
    {|
    def foo():
      match subject:
        case Foo(a=x, b=y):
          pass
  |}
    ~expected:
      [
        "x", Some (ExpectBinding.create Binding.Kind.MatchTarget (location (4, 15) (4, 16)));
        "y", Some (ExpectBinding.create Binding.Kind.MatchTarget (location (4, 20) (4, 21)));
      ];
  assert_bindings
    {|
    def foo():
      match subject:
        case [x] | [_, x]:
          pass
  |}
    ~expected:["x", Some (ExpectBinding.create Binding.Kind.MatchTarget (location (4, 10) (4, 11)))];
  assert_bindings
    {|
    def foo():
      match subject:
        case _ if (x := subject):
          pass
  |}
    ~expected:
      ["x", Some (ExpectBinding.create Binding.Kind.WalrusTarget (location (4, 15) (4, 16)))];
  assert_bindings
    {|
    def foo():
      match (x := subject):
        case _:
          pass
  |}
    ~expected:["x", Some (ExpectBinding.create Binding.Kind.WalrusTarget (location (3, 9) (3, 10)))];

  (* Bindings in nested scope should not leak into the nesting scope (except for walrus operators) *)
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
  assert_bindings
    {|
    def foo():
      if any((comment := line).startswith('#') for line in lines):
        pass
  |}
    ~expected:
      ["comment", Some (ExpectBinding.create Binding.Kind.WalrusTarget (location (3, 10) (3, 17)))];
  assert_bindings
    {|
    def foo():
      if all((nonblank := line).strip() == '' for line in lines):
        pass
  |}
    ~expected:
      ["nonblank", Some (ExpectBinding.create Binding.Kind.WalrusTarget (location (3, 10) (3, 18)))];
  assert_bindings
    {|
    def foo():
      partial_sums = [total := total + v for v in values]
      return total
  |}
    ~expected:
      [
        "total", Some (ExpectBinding.create Binding.Kind.WalrusTarget (location (3, 18) (3, 23)));
        ( "partial_sums",
          Some (ExpectBinding.create (Binding.Kind.AssignTarget None) (location (3, 2) (3, 14))) );
      ];
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
        ( "x",
          Some
            (ExpectBinding.create
               Binding.Kind.(ParameterName { star = None; index = 0; annotation = None })
               (location (1, 7) (1, 8))) );
        "y", None;
      ];
  assert_bindings
    "lambda x, y: x"
    ~expected:
      [
        ( "x",
          Some
            (ExpectBinding.create
               Binding.Kind.(ParameterName { star = None; index = 0; annotation = None })
               (location (1, 7) (1, 8))) );
        ( "y",
          Some
            (ExpectBinding.create
               Binding.Kind.(ParameterName { star = None; index = 1; annotation = None })
               (location (1, 10) (1, 11))) );
      ];
  assert_bindings
    "lambda *args, **kwargs: x"
    ~expected:
      [
        ( "args",
          Some
            (ExpectBinding.create
               Binding.Kind.(ParameterName { star = Some Star.Once; index = 0; annotation = None })
               (location (1, 8) (1, 12))) );
        ( "kwargs",
          Some
            (ExpectBinding.create
               Binding.Kind.(ParameterName { star = Some Star.Twice; index = 1; annotation = None })
               (location (1, 16) (1, 22))) );
      ];
  assert_bindings
    "lambda: (x := 1)"
    ~expected:["x", Some (ExpectBinding.create Binding.Kind.WalrusTarget (location (1, 9) (1, 10)))];
  assert_bindings
    "lambda line: (m := re.match(pattern, line)) and m.group(1)"
    ~expected:
      ["m", Some (ExpectBinding.create Binding.Kind.WalrusTarget (location (1, 14) (1, 15)))];
  assert_bindings
    "(x for x in range(10))"
    ~expected:
      ["x", Some (ExpectBinding.create Binding.Kind.ComprehensionTarget (location (1, 7) (1, 8)))];
  assert_bindings
    "(x for x, *y in [[1], [2,3]])"
    ~expected:
      [
        "x", Some (ExpectBinding.create Binding.Kind.ComprehensionTarget (location (1, 7) (1, 8)));
        "y", Some (ExpectBinding.create Binding.Kind.ComprehensionTarget (location (1, 11) (1, 12)));
      ];
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
  assert_bindings
    "[y for x in foo if (y := f(x)) is not None]"
    ~expected:
      [
        "x", Some (ExpectBinding.create Binding.Kind.ComprehensionTarget (location (1, 7) (1, 8)));
        (* Binding `y` belongs to the enclosing scope *)
        "y", None;
      ];
  ()


module LookupSite = struct
  type t =
    | Global
    | Define of Reference.t
end

module ExpectAccess = struct
  type t = {
    kind: Access.Kind.t;
    binding: ExpectBinding.t;
  }

  let create kind binding = { kind; binding }
end

let assert_access ~actual ~expected name =
  match actual, expected with
  | None, None -> ()
  | Some access, None ->
      let message =
        Format.asprintf
          "Expected access to not exist but found: %a"
          Sexp.pp_hum
          (Access.sexp_of_t access)
      in
      assert_failure message
  | None, Some _ ->
      let message = Format.asprintf "Expected access to exist for %s but not found" name in
      assert_failure message
  | ( Some { Access.binding; kind; _ },
      Some { ExpectAccess.kind = expected_kind; binding = expected_binding } ) ->
      assert_equal
        ~cmp:[%compare.equal: Access.Kind.t]
        ~printer:(fun kind -> Sexp.to_string_hum [%message (kind : Access.Kind.t)])
        expected_kind
        kind;
      assert_binding ~actual:(Some binding) ~expected:(Some expected_binding) name


let test_scope_stack_lookup _ =
  let assert_bindings ~expected ~site source_text =
    let source =
      Test.parse ~handle:"test.py" source_text |> Preprocessing.populate_nesting_defines
    in
    let scope_stack = ScopeStack.create source in
    let scope_stack =
      match site with
      | LookupSite.Global -> scope_stack
      | LookupSite.Define name ->
          let all_defines =
            Preprocessing.defines ~include_nested:true ~include_toplevels:false source
          in
          let find_define name =
            match
              List.find all_defines ~f:(fun { Node.value; _ } ->
                  Reference.equal name (Statement.Define.name value))
            with
            | None ->
                let message =
                  Format.sprintf "Cannot find define with name %s" (Reference.show name)
                in
                failwith message
            | Some define -> define
          in
          let defines =
            (* Collect all defines that (transitively) nest the define whose name is `name` *)
            let rec walk_nesting sofar = function
              | None -> sofar
              | Some name ->
                  let {
                    Node.value =
                      {
                        Statement.Define.signature =
                          { Statement.Define.Signature.nesting_define; _ };
                        _;
                      } as define;
                    _;
                  }
                    =
                    find_define name
                  in
                  (* Shallow nests come before deep nests *)
                  walk_nesting (define :: sofar) nesting_define
            in
            walk_nesting [] (Some name)
          in
          List.fold defines ~init:scope_stack ~f:(fun scope_stack define ->
              let scope = Scope.of_define_exn define in
              ScopeStack.extend scope_stack ~with_:scope)
    in
    List.iter expected ~f:(fun (name, expected) ->
        let actual = ScopeStack.lookup scope_stack name in
        assert_access ~actual ~expected name)
  in
  assert_bindings "" ~site:LookupSite.Global ~expected:["x", None];

  assert_bindings
    {|
    x = 1
  |}
    ~site:LookupSite.Global
    ~expected:
      [
        ( "x",
          Some
            (ExpectBinding.create (Binding.Kind.AssignTarget None) (location (2, 0) (2, 1))
            |> ExpectAccess.create Access.Kind.CurrentScope) );
        "y", None;
      ];

  assert_bindings
    {|
    x = 1
    def foo():
      y = 1
  |}
    ~site:(LookupSite.Define !&"foo")
    ~expected:
      [
        ( "x",
          Some
            (ExpectBinding.create (Binding.Kind.AssignTarget None) (location (2, 0) (2, 1))
            |> ExpectAccess.create Access.(Kind.OuterScope Locality.Local)) );
        ( "y",
          Some
            (ExpectBinding.create (Binding.Kind.AssignTarget None) (location (4, 2) (4, 3))
            |> ExpectAccess.create Access.Kind.CurrentScope) );
      ];

  assert_bindings
    {|
    x = 1
    def foo():
      x = 2
  |}
    ~site:(LookupSite.Define !&"foo")
    ~expected:
      [
        ( "x",
          Some
            (ExpectBinding.create (Binding.Kind.AssignTarget None) (location (4, 2) (4, 3))
            |> ExpectAccess.create Access.Kind.CurrentScope) );
      ];

  assert_bindings
    {|
    x = 1
    def foo():
      global x
      x = 2
  |}
    ~site:(LookupSite.Define !&"foo")
    ~expected:
      [
        ( "x",
          Some
            (ExpectBinding.create (Binding.Kind.AssignTarget None) (location (2, 0) (2, 1))
            |> ExpectAccess.create Access.(Kind.OuterScope Locality.Global)) );
      ];

  assert_bindings
    {|
    x = 1
    def foo():
      x = 2
      def bar():
        pass
  |}
    ~site:(LookupSite.Define !&"bar")
    ~expected:
      [
        ( "x",
          Some
            (ExpectBinding.create (Binding.Kind.AssignTarget None) (location (4, 2) (4, 3))
            |> ExpectAccess.create Access.(Kind.OuterScope Locality.Local)) );
      ];

  assert_bindings
    {|
    x = 1
    def foo():
      x = 2
      def bar():
        x = 3
  |}
    ~site:(LookupSite.Define !&"bar")
    ~expected:
      [
        ( "x",
          Some
            (ExpectBinding.create (Binding.Kind.AssignTarget None) (location (6, 4) (6, 5))
            |> ExpectAccess.create Access.Kind.CurrentScope) );
      ];

  assert_bindings
    {|
    x = 1
    def foo():
      x = 2
      def bar():
        global x
        x = 3
  |}
    ~site:(LookupSite.Define !&"bar")
    ~expected:
      [
        ( "x",
          Some
            (ExpectBinding.create (Binding.Kind.AssignTarget None) (location (2, 0) (2, 1))
            |> ExpectAccess.create Access.(Kind.OuterScope Locality.Global)) );
      ];

  assert_bindings
    {|
    x = 1
    def foo():
      x = 2
      def bar():
        nonlocal x
        x = 3
  |}
    ~site:(LookupSite.Define !&"bar")
    ~expected:
      [
        ( "x",
          Some
            (ExpectBinding.create (Binding.Kind.AssignTarget None) (location (4, 2) (4, 3))
            |> ExpectAccess.create Access.(Kind.OuterScope Locality.Nonlocal)) );
      ];

  (* Global search will ignore local/nonlocal bindings *)
  assert_bindings
    {|
    def foo():
      x = 1
      def bar():
        global x
        x = 2
  |}
    ~site:(LookupSite.Define !&"bar")
    ~expected:["x", None];
  assert_bindings
    {|
    def foo():
      x = 1
      def bar():
        nonlocal x
        def baz():
          global x
          x = 2
  |}
    ~site:(LookupSite.Define !&"baz")
    ~expected:["x", None];

  (* Nonlocal search will ignore global bindings *)
  assert_bindings
    {|
    x = 1
    def foo():
      def bar():
        nonlocal x
        x = 3
  |}
    ~site:(LookupSite.Define !&"bar")
    ~expected:["x", None];

  (* We don't care global/nonlocal declarations in the outer scope -- the binding is local from the
     perspective of the current scope *)
  assert_bindings
    {|
    x = 1
    def foo():
      global x
      def bar():
        pass
  |}
    ~site:(LookupSite.Define !&"bar")
    ~expected:
      [
        ( "x",
          Some
            (ExpectBinding.create (Binding.Kind.AssignTarget None) (location (2, 0) (2, 1))
            |> ExpectAccess.create Access.(Kind.OuterScope Locality.Local)) );
      ];
  assert_bindings
    {|
    def foo():
      x = 1
      def bar():
        nonlocal x
        def baz():
          pass
  |}
    ~site:(LookupSite.Define !&"baz")
    ~expected:
      [
        ( "x",
          Some
            (ExpectBinding.create (Binding.Kind.AssignTarget None) (location (3, 2) (3, 3))
            |> ExpectAccess.create Access.(Kind.OuterScope Locality.Local)) );
      ];
  assert_bindings
    {|
    x = 1
    def foo():
      global x
      def bar():
        x = 2
  |}
    ~site:(LookupSite.Define !&"bar")
    ~expected:
      [
        ( "x",
          Some
            (ExpectBinding.create (Binding.Kind.AssignTarget None) (location (6, 4) (6, 5))
            |> ExpectAccess.create Access.Kind.CurrentScope) );
      ];
  assert_bindings
    {|
    def foo():
      x = 1
      def bar():
        nonlocal x
        def baz():
          x = 2
  |}
    ~site:(LookupSite.Define !&"baz")
    ~expected:
      [
        ( "x",
          Some
            (ExpectBinding.create (Binding.Kind.AssignTarget None) (location (7, 6) (7, 7))
            |> ExpectAccess.create Access.Kind.CurrentScope) );
      ];

  (* Demonstrate that we are able to find annotations of captured local variables *)
  assert_bindings
    {|
    def foo() -> int:
      x: int = 1
      def bar(y: int) -> int:
        return x + y
      return bar(42)
  |}
    ~site:(LookupSite.Define !&"bar")
    ~expected:
      [
        ( "x",
          Some
            (ExpectBinding.create
               (Binding.Kind.AssignTarget (Some (int_annotation (3, 5) (3, 8))))
               (location (3, 2) (3, 3))
            |> ExpectAccess.create Access.(Kind.OuterScope Locality.Local)) );
        ( "y",
          Some
            (ExpectBinding.create
               Binding.Kind.(
                 ParameterName
                   { star = None; index = 0; annotation = Some (int_annotation (4, 13) (4, 16)) })
               (location (4, 10) (4, 16))
            |> ExpectAccess.create Access.Kind.CurrentScope) );
      ];

  (* Demonstrate that we are able to find annotations of captured parameters *)
  assert_bindings
    {|
    def foo(x: int) -> int:
      def bar(y: int) -> int:
        return x + y
      return bar(42)
  |}
    ~site:(LookupSite.Define !&"bar")
    ~expected:
      [
        ( "x",
          Some
            (ExpectBinding.create
               Binding.Kind.(
                 ParameterName
                   { star = None; index = 0; annotation = Some (int_annotation (2, 11) (2, 14)) })
               (location (2, 8) (2, 14))
            |> ExpectAccess.create Access.(Kind.OuterScope Locality.Local)) );
        ( "y",
          Some
            (ExpectBinding.create
               Binding.Kind.(
                 ParameterName
                   { star = None; index = 0; annotation = Some (int_annotation (3, 13) (3, 16)) })
               (location (3, 10) (3, 16))
            |> ExpectAccess.create Access.Kind.CurrentScope) );
      ];

  (* Demonstrate that we are able to correctly handle global declarations that does not follow
     control flow *)
  assert_bindings
    {|
    x = 1
    def foo(flag: bool):
      if flag:
        x = 2
      else:
        global x
  |}
    ~site:(LookupSite.Define !&"foo")
    ~expected:
      [
        ( "x",
          Some
            (ExpectBinding.create (Binding.Kind.AssignTarget None) (location (2, 0) (2, 1))
            |> ExpectAccess.create Access.(Kind.OuterScope Locality.Global)) );
      ];

  (* Demonstrate that we are able to correctly find bindings that does not follow control flow *)
  assert_bindings
    {|
    def foo():
      def bar():
        return x
      x: int = 1
      return bar()
  |}
    ~site:(LookupSite.Define !&"bar")
    ~expected:
      [
        ( "x",
          Some
            (ExpectBinding.create
               (Binding.Kind.AssignTarget (Some (int_annotation (5, 5) (5, 8))))
               (location (5, 2) (5, 3))
            |> ExpectAccess.create Access.(Kind.OuterScope Locality.Local)) );
      ];

  ()


let () =
  "scope"
  >::: [
         "globals" >:: test_global;
         "nonlocals" >:: test_nonlocal;
         "define_local_bindings" >:: test_define_local_bindings;
         "expression_local_bindings" >:: test_expression_local_bindings;
         "scope_stack_lookup" >:: test_scope_stack_lookup;
       ]
  |> Test.run
