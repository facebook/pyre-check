(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open Ast
open Core
open Test
open UnannotatedGlobal

let test_collection context =
  let assert_collected_names ~expected source_text =
    let source = parse ~handle:"test.py" source_text in
    let actual =
      Collector.from_source source |> List.map ~f:(fun { Collector.Result.name; _ } -> name)
    in
    assert_equal
      ~ctxt:context
      ~cmp:[%compare.equal: string list]
      ~printer:(String.concat ~sep:", ")
      expected
      actual
  in

  assert_collected_names {|
       x = 1
       y = 2
       z = 3
    |} ~expected:["x"; "y"; "z"];
  assert_collected_names
    {|
      x, y = 1, 2
      z[3] = 4
      u, (v, w) = derp
    |}
    ~expected:["x"; "y"];
  assert_collected_names
    {|
       def foo(): pass
       def bar(): pass
       def foo(): pass
    |}
    ~expected:["foo"; "bar"; "foo"];
  assert_collected_names
    {|
       class Foo: pass
       class Bar:
         class Nested: pass
       class Baz:
         def foo(self): ...
    |}
    ~expected:["Foo"; "Bar"; "Baz"];
  assert_collected_names
    {|
       import x
       import y as z
       from u.v import w
       from a.b import c as d
    |}
    ~expected:["x"; "z"; "w"; "d"];
  assert_collected_names
    {|
       if derp():
         x = 1
         z = 2
       else:
         x = 3
         y = 4
    |}
    ~expected:["x"; "z"; "x"; "y"];
  assert_collected_names
    {|
       try:
         x = 1
         z = 2
       except:
         y = 3
    |}
    ~expected:["x"; "z"; "y"];
  assert_collected_names
    {|
       with derp():
         x = 1
         import y
    |}
    ~expected:["x"; "y"];
  ()


let test_import context =
  let assert_imports ~expected source_text =
    let source = parse ~handle:"test.py" source_text in
    let actual =
      Collector.from_source source
      |> List.filter_map ~f:(function
             | { Collector.Result.name; unannotated_global = Imported import } -> Some (name, import)
             | _ -> None)
    in
    assert_equal
      ~ctxt:context
      ~cmp:[%compare.equal: (Identifier.t * ImportEntry.t) list]
      ~printer:(fun result ->
        Sexp.to_string_hum [%message (result : (Identifier.t * ImportEntry.t) list)])
      expected
      actual
  in

  assert_imports
    "import foo"
    ~expected:["foo", ImportEntry.Module { target = !&"foo"; implicit_alias = true }];
  assert_imports
    "import foo as bar"
    ~expected:["bar", ImportEntry.Module { target = !&"foo"; implicit_alias = false }];
  assert_imports
    "import foo.bar"
    ~expected:["foo", ImportEntry.Module { target = !&"foo.bar"; implicit_alias = true }];
  assert_imports
    "import foo.bar as baz"
    ~expected:["baz", ImportEntry.Module { target = !&"foo.bar"; implicit_alias = false }];
  assert_imports
    "from foo import bar"
    ~expected:["bar", ImportEntry.Name { from = !&"foo"; target = "bar"; implicit_alias = true }];
  assert_imports
    "from foo import bar as baz"
    ~expected:["baz", ImportEntry.Name { from = !&"foo"; target = "bar"; implicit_alias = false }];
  assert_imports
    "from foo.bar import baz as qux"
    ~expected:
      ["qux", ImportEntry.Name { from = !&"foo.bar"; target = "baz"; implicit_alias = false }];

  assert_imports
    "import a as b, c.d"
    ~expected:
      [
        "b", ImportEntry.Module { target = !&"a"; implicit_alias = false };
        "c", ImportEntry.Module { target = !&"c.d"; implicit_alias = true };
      ];
  assert_imports
    "from a.b import c as d, e"
    ~expected:
      [
        "d", ImportEntry.Name { from = !&"a.b"; target = "c"; implicit_alias = false };
        "e", ImportEntry.Name { from = !&"a.b"; target = "e"; implicit_alias = true };
      ];

  assert_imports "from foo import *" ~expected:[];
  ()


let () = "node" >::: ["collection" >:: test_collection; "import" >:: test_import] |> Test.run
