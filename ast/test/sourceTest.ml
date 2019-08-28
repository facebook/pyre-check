(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open OUnit2
open Ast
open Statement
open Ignore
open Test

let test_parse _ =
  let qualifier = !&"test" in
  let assert_mode line expected_mode =
    let { Source.Metadata.local_mode; _ } = Source.Metadata.parse ~qualifier [line] in
    assert_equal local_mode expected_mode
  in
  assert_mode " # pyre-placeholder-stub" Source.PlaceholderStub;
  assert_mode "  # pyre-ignore-all-errors " Source.Declare;
  assert_mode "\t# pyre-ignore-all-errors" Source.Declare;
  assert_mode " # pyre-strict" Source.Strict;
  assert_mode " # pyre-unsafe" Source.Unsafe;
  assert_mode " # pyre-durp" Source.Default;
  assert_mode " # pyre-ignore-all-errors[42, 7,   15] " (Source.DefaultButDontCheck [42; 7; 15]);

  (* Prevent typos from being treated as error suppressors. *)
  assert_mode " # pyre-ignore-all-errors[42, 7,   15" Source.Default;
  let assert_ignore lines expected_ignore_lines =
    let { Source.Metadata.ignore_lines; _ } = Source.Metadata.parse ~qualifier lines in
    assert_equal
      ~printer:(fun ignores -> List.to_string ~f:show ignores)
      expected_ignore_lines
      (List.rev ignore_lines)
  in
  let create_ignore ignored_line codes kind start_line start_column end_line end_column =
    let location =
      let start = { Location.line = start_line; column = start_column } in
      let stop = { Location.line = end_line; column = end_column } in
      { Location.path = qualifier; start; stop }
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
    [create_ignore 2 [] PyreIgnore 2 31 2 42; create_ignore 1 [] PyreIgnore 1 32 1 43];
  assert_ignore
    ["class A: pass"; "def foo() -> A: return 1  # pyre-ignore"]
    [create_ignore 2 [] PyreIgnore 2 28 2 39];
  assert_ignore
    ["def foo() -> str: return bar()  # pyre-ignore"]
    [create_ignore 1 [] PyreIgnore 1 34 1 45];
  assert_ignore
    [ "def foo() -> other:  # pyre-ignore";
      "result = 0";
      "if True:";
      "result = durp()";
      "return result" ]
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
    [ "def foo() -> str: return 1.0  # pyre-ignore[7]";
      "def bar() -> int: return ''  # pyre-ignore[7]" ]
    [create_ignore 2 [7] PyreIgnore 2 31 2 45; create_ignore 1 [7] PyreIgnore 1 32 1 46];
  assert_ignore
    ["def foo() -> str: return 1  # pyre-ignore[7, 1, 2]"]
    [create_ignore 1 [7; 1; 2] PyreIgnore 1 30 1 50];
  assert_ignore
    ["def foo() -> str: return 1  # pyre-fixme[7, 1, 2]"]
    [create_ignore 1 [7; 1; 2] PyreFixme 1 30 1 49];
  assert_ignore
    ["def foo() -> str: return 1  # pyre-fixme[7, 1, 2]: something about Class[Derp4]"]
    [create_ignore 1 [7; 1; 2] PyreFixme 1 30 1 79];

  (* Comment on preceding line. *)
  assert_ignore
    ["# pyre-ignore[7]"; "def foo() -> str: return"]
    [create_ignore 2 [7] PyreIgnore 1 2 1 16];

  (* Don't include ignore keywords inside quotes *)
  assert_ignore ["def foo() -> int: return 1.0  # haha no 'pyre-ignore's here"] [];
  assert_ignore
    ["def foo() -> int: return 1.0  # 'quote before is OK' pyre-ignore"]
    [create_ignore 1 [] PyreIgnore 1 53 1 64];
  assert_ignore ["def foo() -> int: return 1.0  # 'still in quotes' 'pyre-ignore'"] [];

  (* Ignores apply to next non-comment line *)
  assert_ignore
    ["# pyre-ignore[7]"; "# another comment"; "def foo() -> str: return"]
    [create_ignore 3 [7] PyreIgnore 1 2 1 16];
  assert_ignore
    ["# pyre-ignore[7]"; "def foo() -> str: return  # applies to this line"]
    [create_ignore 2 [7] PyreIgnore 1 2 1 16];
  assert_ignore
    ["# pyre-ignore[6]"; "# pyre-ignore[7]"; "def foo() -> str: return"]
    [create_ignore 3 [6] PyreIgnore 1 2 1 16; create_ignore 3 [7] PyreIgnore 2 2 2 16];
  assert_ignore
    [ "# pyre-fixme[3]";
      "# pyre-fixme[2]";
      "# pyre-fixme[2]";
      "def foo(x: typing.Any, y:typing.Any) -> typing.Any";
      " return x" ]
    [ create_ignore 4 [3] PyreFixme 1 2 1 15;
      create_ignore 4 [2] PyreFixme 2 2 2 15;
      create_ignore 4 [2] PyreFixme 3 2 3 15 ]


let test_qualifier _ =
  let qualifier = Reference.create_from_list in
  assert_equal (SourcePath.qualifier_of_relative "module.py") (qualifier ["module"]);
  assert_equal
    (SourcePath.qualifier_of_relative "module/submodule.py")
    (qualifier ["module"; "submodule"]);
  assert_equal (SourcePath.qualifier_of_relative "builtins.pyi") (qualifier []);
  assert_equal (SourcePath.qualifier_of_relative "module/builtins.pyi") (qualifier ["module"]);
  assert_equal (SourcePath.qualifier_of_relative "module/__init__.pyi") (qualifier ["module"])


let test_expand_relative_import _ =
  let assert_export ~relative ~from ~expected =
    let from =
      match parse_single_statement ("from " ^ from ^ " import something") with
      | { Node.value = Import { Import.from = Some from; _ }; _ } -> from
      | _ -> failwith "Could not parse import"
    in
    let source = Source.create ~relative [] in
    assert_equal
      ~cmp:Reference.equal
      ~printer:Reference.show
      (Reference.create expected)
      (Source.expand_relative_import source ~from)
  in
  assert_export ~relative:"module/qualifier.py" ~from:"." ~expected:"module";
  assert_export
    ~relative:"module/submodule/qualifier.py"
    ~from:".other"
    ~expected:"module.submodule.other";
  assert_export ~relative:"module/submodule/qualifier.py" ~from:"..other" ~expected:"module.other";

  (* `__init__` modules are special. *)
  assert_export ~relative:"module/__init__.py" ~from:"." ~expected:"module"


let test_localize_configuration _ =
  let unsafe_source =
    Source.create ~metadata:(Source.Metadata.create_for_testing ~local_mode:Source.Unsafe ()) []
  in
  let strict_source =
    Source.create ~metadata:(Source.Metadata.create_for_testing ~local_mode:Source.Strict ()) []
  in
  let default_configuration = Configuration.Analysis.create ~debug:true () in
  let strict_configuration = Configuration.Analysis.create ~debug:true ~strict:true () in
  assert_true
    (Configuration.Analysis.equal
       (Source.localize_configuration ~source:unsafe_source default_configuration)
       default_configuration);
  assert_true
    (Configuration.Analysis.equal
       (Source.localize_configuration ~source:strict_source default_configuration)
       strict_configuration);
  assert_true
    (Configuration.Analysis.equal
       (Source.localize_configuration ~source:unsafe_source strict_configuration)
       default_configuration);
  assert_true
    (Configuration.Analysis.equal
       (Source.localize_configuration ~source:strict_source strict_configuration)
       strict_configuration);
  ()


let () =
  "metadata" >::: ["parse" >:: test_parse] |> Test.run;
  "source"
  >::: [ "qualifier" >:: test_qualifier;
         "expand_relative_import" >:: test_expand_relative_import;
         "localize_configuration" >:: test_localize_configuration ]
  |> Test.run
