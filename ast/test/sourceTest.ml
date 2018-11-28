(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2

open Ast
open Statement
open Ignore
open Test


let test_parse _ =
  let test_path = "test.py" in

  let assert_mode line expected_mode =
    let { Source.Metadata.local_mode; _ } = Source.Metadata.parse test_path [line] in
    assert_equal local_mode expected_mode
  in
  assert_mode " # pyre-placeholder-stub" Source.PlaceholderStub;
  assert_mode "  # pyre-ignore-all-errors " Source.Declare;
  assert_mode "\t# pyre-ignore-all-errors" Source.Declare;
  assert_mode " # pyre-strict" Source.Strict;
  assert_mode " # pyre-durp" Source.Default;
  assert_mode " # pyre-ignore-all-errors[42, 7,   15] " (Source.DefaultButDontCheck [42; 7; 15]);
  (* prevent typos from being hidden as do-not-checks *)
  assert_mode " # pyre-ignore-all-errors[42, 7,   15" Source.Default;

  let assert_ignore lines expected_ignore_lines =
    let { Source.Metadata.ignore_lines; _ } = Source.Metadata.parse test_path lines in
    assert_equal
      ~printer:(fun ignores -> List.to_string ~f:show ignores)
      expected_ignore_lines
      (List.rev ignore_lines)
  in

  let create_ignore ignored_line codes kind start_line start_column end_line end_column =
    let location =
      let start = { Location.line = start_line; column = start_column } in
      let stop = { Location.line = end_line; column = end_column } in
      { Location.path = String.hash test_path; start; stop }
    in
    create ~ignored_line ~codes ~kind ~location
  in

  assert_ignore
    ["def foo() -> int: return 1.0  # pyre-ignore"]
    [create_ignore 1 [] PyreIgnore 1 32 1 43];

  assert_ignore
    ["def foo() -> str: return 1.0  # pyre-ignore"]
    [create_ignore 1 [] PyreIgnore 1 32 1 43];

  assert_ignore
    ["def foo() -> str: return  # pyre-ignore"]
    [create_ignore 1 [] PyreIgnore 1 28 1 39];

  assert_ignore
    ["def foo() -> typing.List[str]: return 1  # pyre-ignore"]
    [create_ignore 1 [] PyreIgnore 1 43 1 54];

  assert_ignore
    ["def foo() -> str: return 1.0  # pyre-ignore"; "def bar() -> int: return ''  # pyre-ignore"]
    [
      create_ignore 1 [] PyreIgnore 1 32 1 43;
      create_ignore 2 [] PyreIgnore 2 31 2 42
    ];

  assert_ignore
    ["class A: pass"; "def foo() -> A: return 1  # pyre-ignore"]
    [create_ignore 2 [] PyreIgnore 2 28 2 39];

  assert_ignore
    ["def foo() -> str: return bar()  # pyre-ignore"]
    [create_ignore 1 [] PyreIgnore 1 34 1 45];

  assert_ignore
    ["def foo() -> other:  # pyre-ignore"; "result = 0";
     "if True:"; "result = durp()"; "return result"]
    [create_ignore 1 [] PyreIgnore 1 23 1 34];

  assert_ignore
    ["def foo() -> int: return 1.0  # type: ignore"]
    [create_ignore 1 [] TypeIgnore 1 32 1 44];

  assert_ignore
    ["def foo() -> str: return 1.0  # type: ignore"]
    [create_ignore 1 [] TypeIgnore 1 32 1 44];

  assert_ignore
    ["def foo() -> str: return 1.0  # pyre-ignore[7]"]
    [create_ignore 1 [7] PyreIgnore 1 32 1 46];

  assert_ignore
    ["def foo() -> str: return  # pyre-ignore[7]"]
    [create_ignore 1 [7] PyreIgnore 1 28 1 42];

  assert_ignore
    ["def foo() -> typing.List[str]: return 1  # pyre-ignore[7]"]
    [create_ignore 1 [7] PyreIgnore 1 43 1 57];

  assert_ignore
    [
      "def foo() -> str: return 1.0  # pyre-ignore[7]";
      "def bar() -> int: return ''  # pyre-ignore[7]"
    ]
    [
      create_ignore 1 [7] PyreIgnore 1 32 1 46;
      create_ignore 2 [7] PyreIgnore 2 31 2 45
    ];

  assert_ignore
    [
      "def foo() -> str: return 1  # pyre-ignore[7, 1, 2]"
    ]
    [create_ignore 1 [7; 1; 2] PyreIgnore 1 30 1 50];

  assert_ignore
    [
      "def foo() -> str: return 1  # pyre-fixme[7, 1, 2]"
    ]
    [create_ignore 1 [7; 1; 2] PyreFixme 1 30 1 49];

  assert_ignore
    [
      "def foo() -> str: return 1  # pyre-fixme[7, 1, 2]: something about Class[Derp4]"
    ]
    [create_ignore 1 [7; 1; 2] PyreFixme 1 30 1 79];

  (* Comment on preceding line. *)
  assert_ignore
    ["# pyre-ignore[7]"; "def foo() -> str: return"]
    [create_ignore 2 [7] PyreIgnore 1 2 1 16];

  (* Don't include ignore keywords inside quotes *)
  assert_ignore
    ["def foo() -> int: return 1.0  # haha no 'pyre-ignore's here"]
    [];
  assert_ignore
    ["def foo() -> int: return 1.0  # 'quote before is OK' pyre-ignore"]
    [create_ignore 1 [] PyreIgnore 1 53 1 64];
  assert_ignore
    ["def foo() -> int: return 1.0  # 'still in quotes' 'pyre-ignore'"]
    []


let test_qualifier _ =
  let qualifier modules =
    List.map
      ~f:Expression.Access.create
      modules
    |> List.concat
  in

  assert_equal
    (Source.qualifier ~handle: (File.Handle.create "module.py"))
    (qualifier ["module"]);

  assert_equal
    (Source.qualifier ~handle: (File.Handle.create "module/submodule.py"))
    (qualifier ["module"; "submodule"]);

  assert_equal
    (Source.qualifier ~handle: (File.Handle.create "builtins.pyi"))
    (qualifier []);

  assert_equal
    (Source.qualifier ~handle: (File.Handle.create "module/builtins.pyi"))
    (qualifier ["module"]);

  assert_equal
    (Source.qualifier ~handle: (File.Handle.create "module/__init__.pyi"))
    (qualifier ["module"])


let test_expand_relative_import _ =
  let assert_export ~handle ~from ~expected =
    let handle = File.Handle.create handle in
    let qualifier = Source.qualifier ~handle in
    let from =
      match parse_single_statement ("from " ^ from ^ " import something") with
      | { Node.value = Import { Import.from = Some from; _ }; _ } -> from
      | _ -> failwith "Could not parse import"
    in
    assert_equal
      ~cmp:Access.equal
      ~printer:Access.show
      (parse_single_access expected)
      (Source.expand_relative_import ~qualifier ~handle ~from)
  in

  assert_export ~handle:"module/qualifier.py" ~from:"." ~expected:"module";
  assert_export
    ~handle:"module/submodule/qualifier.py"
    ~from:".other"
    ~expected:"module.submodule.other";
  assert_export
    ~handle:"module/submodule/qualifier.py"
    ~from:"..other"
    ~expected:"module.other";
  (* `__init__` modules are special. *)
  assert_export ~handle:"module/__init__.py" ~from:"." ~expected:"module"


let test_signature_hash _ =
  let assert_hash_equal ?(equal = true) left right =
    let parse source =
      let { Source.statements; _ } = parse source in
      let metadata =
        String.split ~on:'\n' source
        |> Source.Metadata.parse "test.py"
      in
      Source.create ~metadata statements
    in
    let equal = if equal then (=) else (<>) in
    assert_equal
      ~cmp:equal
      (Source.signature_hash (parse left))
      (Source.signature_hash (parse right))
  in
  let assert_hash_unequal = assert_hash_equal ~equal:false in

  (* Metadata. *)
  assert_hash_equal "# pyre-strict" "# pyre-strict";
  assert_hash_unequal "# pyre-strict" "";
  assert_hash_unequal "# pyre-strict" "# pyre-declare-but-dont-check";

  (* Assignments. *)
  assert_hash_equal "a = 1" "a = 1";
  assert_hash_equal "a: int = 1" "a: int = 1";
  assert_hash_unequal "a: str = 1" "a: int = 1";
  assert_hash_unequal "a = 2" "a = 1";

  (* Defines. *)
  assert_hash_equal
    {|
      @decorator
      def define(parameter: int) -> str:
        1
    |}
    {|
      @decorator
      def define(parameter: int) -> str:
        2  # Body does not matter.
    |};
  assert_hash_unequal "def foo(): ..." "def bar(): ...";
  assert_hash_unequal "def foo(a: int): ..." "def foo(a: str): ...";
  assert_hash_unequal "def foo(a: int = 1): ..." "def foo(a: int = 2): ..."; (* Yerps... :( *)
  assert_hash_unequal
    {|
      @decorator
      def foo(): ...
    |}
    {|
      @other_decorator
      def foo(): ...
    |};
  assert_hash_unequal "def foo() -> int: ..." "def foo() -> str: ...";
  assert_hash_unequal "def foo(): ..." "async def foo(): ...";

  (* Classes. *)
  assert_hash_equal
    {|
      @decorator
      class B(A):
        attribute: int = 1
    |}
    {|
      @decorator
      class B(A):
        attribute: int = 1
    |};
  assert_hash_unequal "class A: ..." "class B: ...";
  assert_hash_unequal "class A(B): ..." "class A(C): ...";
  assert_hash_unequal
    {|
      class A:
        attribute: int = 1
    |}
    {|
      class A:
        attribute: str = 1
    |};
  assert_hash_unequal
    {|
      @decorator
      class A: ...
    |}
    {|
      @other_decorator
      class A: ...
    |};

  (* If. *)
  assert_hash_equal
    {|
      if test:
        attribute = 1
      else:
        attribute = 2
    |}
    {|
      if test:
        attribute = 1
      else:
        attribute = 2
    |};
  assert_hash_unequal
    {|
      if test:
        attribute = 1
    |}
    {|
      if other_test:
        attribute = 1
    |};
  assert_hash_unequal
    {|
      if test:
        attribute = 1
    |}
    {|
      if test:
        attribute = 2
    |};
  assert_hash_unequal
    {|
      if test:
        attribute = 1
      else:
        attribute = 2
    |}
    {|
      if test:
        attribute = 1
      else:
        attribute = 3
    |};

  (* Imports. *)
  assert_hash_equal "from a import b" "from a import b";
  assert_hash_equal "import a" "import a";
  assert_hash_unequal "from a import b" "from a import c";
  assert_hash_unequal "import a" "import b";

  (* With. *)
  assert_hash_equal
    {|
      with resource:
        attribute = 1
    |}
    {|
      with resource:
        attribute = 1
    |};
  assert_hash_unequal
    {|
      with resource:
        attribute = 1
    |}
    {|
      with resource:
        attribute = 2
    |}


let () =
  "metadata">:::[
    "parse">::test_parse;
  ]
  |> Test.run;
  "source">:::[
    "qualifier">::test_qualifier;
    "expand_relative_import">::test_expand_relative_import;
    "signature_hash">::test_signature_hash;
  ]
  |> Test.run
