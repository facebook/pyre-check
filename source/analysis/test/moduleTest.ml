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

let test_collected_names context =
  let assert_collected_names ~expected source_text =
    let source = parse ~handle:"test.py" source_text in
    let actual =
      Module.UnannotatedGlobal.raw_alist_of_source source |> List.map ~f:(fun (name, _) -> name)
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
  assert_collected_names {|
       x, y, z = (1, 2, 3)
    |} ~expected:["x"; "y"; "z"];
  (* TODO(T191635350): Pyre's global scope analysis cannot handle nested target patterns. *)
  assert_collected_names {|
       x, (y, z) = (1, (2, 3))
    |} ~expected:[];
  ()


let test_collected_imports context =
  let assert_imports ~expected source_text =
    let source = parse ~handle:"test.py" source_text in
    let actual =
      Module.UnannotatedGlobal.raw_alist_of_source source
      |> List.filter_map ~f:(function
             | name, Module.UnannotatedGlobal.Imported import -> Some (name, import)
             | _ -> None)
    in
    assert_equal
      ~ctxt:context
      ~cmp:[%compare.equal: (Ast.Identifier.t * Module.UnannotatedGlobal.import) list]
      ~printer:(fun result ->
        Sexp.to_string_hum
          [%message (result : (Ast.Identifier.t * Module.UnannotatedGlobal.import) list)])
      expected
      actual
  in
  let open Module.UnannotatedGlobal in
  assert_imports
    "import foo"
    ~expected:["foo", ImportModule { target = !&"foo"; implicit_alias = true }];
  assert_imports
    "import foo as bar"
    ~expected:["bar", ImportModule { target = !&"foo"; implicit_alias = false }];
  assert_imports
    "import foo.bar"
    ~expected:["foo", ImportModule { target = !&"foo.bar"; implicit_alias = true }];
  assert_imports
    "import foo.bar as baz"
    ~expected:["baz", ImportModule { target = !&"foo.bar"; implicit_alias = false }];
  assert_imports
    "from foo import bar"
    ~expected:["bar", ImportFrom { from = !&"foo"; target = "bar"; implicit_alias = true }];
  assert_imports
    "from foo import bar as baz"
    ~expected:["baz", ImportFrom { from = !&"foo"; target = "bar"; implicit_alias = false }];
  assert_imports
    "from foo.bar import baz as qux"
    ~expected:["qux", ImportFrom { from = !&"foo.bar"; target = "baz"; implicit_alias = false }];

  assert_imports
    "import a as b, c.d"
    ~expected:
      [
        "b", ImportModule { target = !&"a"; implicit_alias = false };
        "c", ImportModule { target = !&"c.d"; implicit_alias = true };
      ];
  assert_imports
    "from a.b import c as d, e"
    ~expected:
      [
        "d", ImportFrom { from = !&"a.b"; target = "c"; implicit_alias = false };
        "e", ImportFrom { from = !&"a.b"; target = "e"; implicit_alias = true };
      ];

  assert_imports "from foo import *" ~expected:[];
  assert_imports
    "from builtins import int, str as s"
    ~expected:
      [
        "int", ImportFrom { from = Ast.Reference.empty; target = "int"; implicit_alias = true };
        "s", ImportFrom { from = Ast.Reference.empty; target = "str"; implicit_alias = false };
      ];
  ()


let test_exports context =
  let assert_exports ?(is_stub = false) ~expected source_text =
    let actual =
      let handle = if is_stub then "test.pyi" else "test.py" in
      parse ~handle source_text |> Module.Metadata.create |> Module.Metadata.get_all_exports
    in
    let expected = List.sort expected ~compare:[%compare: Ast.Identifier.t * Module.Export.t] in
    assert_equal
      ~ctxt:context
      ~cmp:[%compare.equal: (Ast.Identifier.t * Module.Export.t) list]
      ~printer:(fun exports ->
        Sexp.to_string_hum [%message (exports : (Ast.Identifier.t * Module.Export.t) list)])
      expected
      actual
  in

  let open Module.Export in
  assert_exports
    {|
      import a.b
      import c.d as e
      from f.g import h
      from i.j import k as l
    |}
    ~expected:
      [
        "a", Module !&"a";
        "e", Module !&"c.d";
        "h", NameAlias { from = !&"f.g"; name = "h" };
        "l", NameAlias { from = !&"i.j"; name = "k" };
      ];
  assert_exports
    {|
      import a.b
      import c.d as e
      from f.g import h
      from i.j import k as l
    |}
    ~is_stub:true
    ~expected:["e", Module !&"c.d"; "l", NameAlias { from = !&"i.j"; name = "k" }];
  assert_exports
    {|
       from typing import Any
       def foo() -> None: pass
       def bar(x: str) -> Any:
         return x
       def __getattr__(name: str) -> Any: ...
    |}
    ~expected:
      [
        "Any", NameAlias { from = !&"typing"; name = "Any" };
        "foo", Name (Name.Define { is_getattr_any = false });
        "bar", Name (Name.Define { is_getattr_any = false });
        "__getattr__", Name (Name.Define { is_getattr_any = true });
      ];
  assert_exports
    {|
       import typing
       def __getattr__(name) -> typing.Any: ...
    |}
    ~expected:
      ["typing", Module !&"typing"; "__getattr__", Name (Name.Define { is_getattr_any = true })];
  (* Unannotated __getattr__ doesn't count *)
  assert_exports
    {|
       def __getattr__(name): ...
    |}
    ~expected:["__getattr__", Name (Name.Define { is_getattr_any = false })];
  (* __getattr__ with wrong annotations doesn't count *)
  assert_exports
    {|
       def __getattr__(name: int) -> Any: ...
    |}
    ~expected:["__getattr__", Name (Name.Define { is_getattr_any = false })];
  assert_exports
    {|
       def __getattr__(name: str) -> int: ...
    |}
    ~expected:["__getattr__", Name (Name.Define { is_getattr_any = false })];
  (* __getattr__ with wrong param count doesn't count *)
  assert_exports
    {|
       def __getattr__(name: str, value: int) -> Any: ...
    |}
    ~expected:["__getattr__", Name (Name.Define { is_getattr_any = false })];
  assert_exports
    {|
       class Bar: pass
       baz = 42
    |}
    ~expected:["Bar", Name Name.Class; "baz", Name Name.GlobalVariable];
  assert_exports
    {|
       if derp():
         x = 42
       else:
         if herp():
           y = 43
         z = 44
    |}
    ~expected:
      ["x", Name Name.GlobalVariable; "y", Name Name.GlobalVariable; "z", Name Name.GlobalVariable];
  assert_exports
    {|
       try:
         try:
           x = 42
         except:
           y = 43
       except:
         z = 43
       finally:
         w = 44
    |}
    ~expected:
      [
        "x", Name Name.GlobalVariable;
        "y", Name Name.GlobalVariable;
        "z", Name Name.GlobalVariable;
        "w", Name Name.GlobalVariable;
      ];
  assert_exports
    {|
       import foo
       foo = 42
    |} (* Last definition wins *)
    ~expected:["foo", Name Name.GlobalVariable];
  assert_exports
    {|
       if derp():
         from bar import foo
       else:
         foo = 42
    |}
    (* Unfortunately since we don't really follow control we can't really join the two. *)
    ~expected:["foo", Name Name.GlobalVariable];
  (* Stub files with explicit __all__ should re-export unaliased imports listed in __all__ *)
  assert_exports
    {|
       from f.g import h
       from i.j import k
       __all__ = ["h"]
    |}
    ~is_stub:true
    ~expected:["__all__", Name Name.GlobalVariable; "h", NameAlias { from = !&"f.g"; name = "h" }];
  (* Stub files with explicit __all__: import module also re-exported when in __all__ *)
  assert_exports
    {|
       import a.b
       __all__ = ["a"]
    |}
    ~is_stub:true
    ~expected:["__all__", Name Name.GlobalVariable; "a", Module !&"a"];
  (* Stub files without __all__: unaliased imports are still filtered *)
  assert_exports
    {|
       from f.g import h
       from i.j import k as l
    |}
    ~is_stub:true
    ~expected:["l", NameAlias { from = !&"i.j"; name = "k" }];
  ()


let () =
  "module"
  >::: [
         "collected_names" >:: test_collected_names;
         "collected_imports" >:: test_collected_imports;
         "exports" >:: test_exports;
       ]
  |> Test.run
