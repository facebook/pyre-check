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
    let show_local_mode = function
      | Some { Node.value = mode; _ } -> Source.show_local_mode mode
      | None -> "None"
    in
    assert_equal ~printer:show_local_mode expected_mode local_mode
  in
  let create_mode line start stop mode =
    let location =
      let start = { Location.line; column = start } in
      let stop = { Location.line; column = stop } in
      { Location.start; stop }
    in
    Node.create ~location mode
  in
  assert_mode " # pyre-placeholder-stub" (Some (create_mode 1 1 24 Source.PlaceholderStub));
  assert_mode " # pyre-ignore-all-errors " (Some (create_mode 1 1 25 Source.Declare));
  assert_mode "# pyre-ignore-all-errors " (Some (create_mode 1 0 24 Source.Declare));
  assert_mode "  # pyre-ignore-all-errors" None;
  assert_mode "\t# pyre-ignore-all-errors" None;
  assert_mode " # pyre-strict" (Some (create_mode 1 1 14 Source.Strict));
  assert_mode " ## pyre-strict" (Some (create_mode 1 1 15 Source.Strict));
  assert_mode " #? pyre-strict" None;
  assert_mode " # pyre-stric" None;
  assert_mode "  # pyre-strict" None;
  assert_mode "\t# pyre-strict" None;
  assert_mode " # pyre-strict comment comment" None;
  assert_mode " # comment comment pyre-strict" None;
  assert_mode " # pyre-unsafe" (Some (create_mode 1 1 14 Source.Unsafe));
  assert_mode " # pyre-durp" None;
  assert_mode " # pyre-debug" (Some (create_mode 1 1 13 Source.Debug));
  assert_mode " # pyre-ignore-all-errors[42, 7,   15] " None;

  let assert_mode_errors lines expected_mode_errors =
    let { Source.Metadata.unused_local_modes; _ } = Source.Metadata.parse ~qualifier lines in
    assert_equal unused_local_modes expected_mode_errors
  in
  assert_mode_errors ["# pyre-strict"; "# derp"] [];
  assert_mode_errors ["# pyre-strict"; "# pyre-unsafe"] [create_mode 2 0 13 Source.Unsafe];
  assert_mode_errors ["# pyre-strict"; "# pyre-strict"] [create_mode 2 0 13 Source.Strict];
  assert_mode_errors
    ["# pyre-strict"; "derp"; "# pyre-unsafe"; "# pyre-debug"]
    [create_mode 3 0 13 Source.Unsafe; create_mode 4 0 12 Source.Debug];

  let assert_ignore_codes line expected_codes =
    let { Source.Metadata.ignore_codes; _ } = Source.Metadata.parse ~qualifier [line] in
    assert_equal ignore_codes expected_codes
  in
  assert_ignore_codes " # pyre-ignore-all-errors[42, 7,   15] " [42; 7; 15];
  assert_ignore_codes " # pyre-ignore-all-errors[42, 7,   15]: Comment" [42; 7; 15];

  (* Prevent typos from being treated as error suppressors. *)
  assert_ignore_codes " # pyre-ignore-all-errors[42, 7,   15" [];
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
      { Location.start; stop }
    in
    create ~ignored_line ~codes ~kind ~location
  in
  assert_ignore
    ["def foo() -> int: return 1.0  # pyre-ignore"]
    [create_ignore 1 [] PyreIgnore 1 30 1 43];
  assert_ignore
    ["def foo() -> int: return 1.0  #     pyre-ignore"]
    [create_ignore 1 [] PyreIgnore 1 30 1 47];
  assert_ignore ["def foo() -> int: return 1.0  # noqa pyre-ignore"] [];
  assert_ignore
    ["def foo() -> str: return 1.0  # pyre-ignore"]
    [create_ignore 1 [] PyreIgnore 1 30 1 43];
  assert_ignore
    ["def foo() -> str: return  # pyre-ignore"]
    [create_ignore 1 [] PyreIgnore 1 26 1 39];
  assert_ignore
    ["def foo() -> typing.List[str]: return 1  # pyre-ignore"]
    [create_ignore 1 [] PyreIgnore 1 41 1 54];
  assert_ignore
    ["def foo() -> str: return 1.0  # pyre-ignore"; "def bar() -> int: return ''  # pyre-ignore"]
    [create_ignore 2 [] PyreIgnore 2 29 2 42; create_ignore 1 [] PyreIgnore 1 30 1 43];
  assert_ignore
    ["class A: pass"; "def foo() -> A: return 1  # pyre-ignore"]
    [create_ignore 2 [] PyreIgnore 2 26 2 39];
  assert_ignore
    ["def foo() -> str: return bar()  # pyre-ignore"]
    [create_ignore 1 [] PyreIgnore 1 32 1 45];
  assert_ignore
    [
      "def foo() -> other:  # pyre-ignore";
      "result = 0";
      "if True:";
      "result = durp()";
      "return result";
    ]
    [create_ignore 1 [] PyreIgnore 1 21 1 34];
  assert_ignore
    ["def foo() -> int: return 1.0  # type: ignore"]
    [create_ignore 1 [] TypeIgnore 1 30 1 44];
  assert_ignore
    ["def foo() -> str: return 1.0  # type: ignore"]
    [create_ignore 1 [] TypeIgnore 1 30 1 44];
  assert_ignore
    ["def foo() -> str: return 1.0  # pyre-ignore[7]"]
    [create_ignore 1 [7] PyreIgnore 1 30 1 46];
  assert_ignore
    ["def foo() -> str: return  # pyre-ignore[7]"]
    [create_ignore 1 [7] PyreIgnore 1 26 1 42];
  assert_ignore
    ["def foo() -> typing.List[str]: return 1  # pyre-ignore[7]"]
    [create_ignore 1 [7] PyreIgnore 1 41 1 57];
  assert_ignore
    [
      "def foo() -> str: return 1.0  # pyre-ignore[7]";
      "def bar() -> int: return ''  # pyre-ignore[7]";
    ]
    [create_ignore 2 [7] PyreIgnore 2 29 2 45; create_ignore 1 [7] PyreIgnore 1 30 1 46];
  assert_ignore
    ["def foo() -> str: return 1  # pyre-ignore[7, 1, 2]"]
    [create_ignore 1 [7; 1; 2] PyreIgnore 1 28 1 50];
  assert_ignore
    ["def foo() -> str: return 1  # pyre-fixme[7, 1, 2]"]
    [create_ignore 1 [7; 1; 2] PyreFixme 1 28 1 49];
  assert_ignore
    ["def foo() -> str: return 1  # pyre-fixme[7, 1, 2]: something about Class[Derp4]"]
    [create_ignore 1 [7; 1; 2] PyreFixme 1 28 1 79];

  (* Comment on preceding line. *)
  assert_ignore
    ["# pyre-ignore[7]"; "def foo() -> str: return"]
    [create_ignore 2 [7] PyreIgnore 1 0 1 16];

  (* Don't include ignore keywords inside quotes *)
  assert_ignore ["def foo() -> int: return 1.0  # haha no 'pyre-ignore's here"] [];
  assert_ignore ["def foo() -> int: return 1.0  # 'quote before is not OK' pyre-ignore"] [];
  assert_ignore ["def foo() -> int: return 1.0  # 'still in quotes' 'pyre-ignore'"] [];

  (* Ignores apply to next non-comment line *)
  assert_ignore
    ["# pyre-ignore[7]"; "# another comment"; "def foo() -> str: return"]
    [create_ignore 3 [7] PyreIgnore 1 0 1 16];
  assert_ignore
    ["# pyre-ignore[7]"; "def foo() -> str: return  # applies to this line"]
    [create_ignore 2 [7] PyreIgnore 1 0 1 16];
  assert_ignore
    ["# pyre-ignore[6]"; "# pyre-ignore[7]"; "def foo() -> str: return"]
    [create_ignore 3 [6] PyreIgnore 1 0 1 16; create_ignore 3 [7] PyreIgnore 2 0 2 16];
  assert_ignore
    [
      "# pyre-fixme[3]";
      "# pyre-fixme[2]";
      "# pyre-fixme[2]";
      "def foo(x: typing.Any, y:typing.Any) -> typing.Any";
      " return x";
    ]
    [
      create_ignore 4 [3] PyreFixme 1 0 1 15;
      create_ignore 4 [2] PyreFixme 2 0 2 15;
      create_ignore 4 [2] PyreFixme 3 0 3 15;
    ]


let test_mode _ =
  let assert_mode ~configuration local_mode expected_mode =
    let actual_mode = Source.mode ~configuration ~local_mode in
    assert_equal actual_mode expected_mode
  in
  let configuration = Configuration.Analysis.create ~source_path:[] () in
  assert_mode ~configuration None Source.Unsafe;
  assert_mode ~configuration (Some (Node.create_with_default_location Source.Strict)) Source.Strict;
  assert_mode ~configuration (Some (Node.create_with_default_location Source.Debug)) Source.Debug;

  let configuration = Configuration.Analysis.create ~strict:true ~source_path:[] () in
  assert_mode ~configuration None Source.Strict;
  assert_mode ~configuration (Some (Node.create_with_default_location Source.Unsafe)) Source.Unsafe;
  assert_mode ~configuration (Some (Node.create_with_default_location Source.Strict)) Source.Strict;
  assert_mode ~configuration (Some (Node.create_with_default_location Source.Debug)) Source.Debug;

  let configuration = Configuration.Analysis.create ~debug:true ~source_path:[] () in
  assert_mode ~configuration None Source.Debug;
  assert_mode ~configuration (Some (Node.create_with_default_location Source.Strict)) Source.Debug;
  assert_mode ~configuration (Some (Node.create_with_default_location Source.Unsafe)) Source.Debug


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
    let source_path = Source.create ~relative [] |> fun { Source.source_path; _ } -> source_path in
    assert_equal
      ~cmp:Reference.equal
      ~printer:Reference.show
      (Reference.create expected)
      (Node.value (SourcePath.expand_relative_import source_path ~from))
  in
  assert_export ~relative:"module/qualifier.py" ~from:"." ~expected:"module";
  assert_export
    ~relative:"module/submodule/qualifier.py"
    ~from:".other"
    ~expected:"module.submodule.other";
  assert_export ~relative:"module/submodule/qualifier.py" ~from:"..other" ~expected:"module.other";

  (* `__init__` modules are special. *)
  assert_export ~relative:"module/__init__.py" ~from:"." ~expected:"module"


let () =
  "metadata" >::: ["parse" >:: test_parse; "mode" >:: test_mode] |> Test.run;
  "source"
  >::: ["qualifier" >:: test_qualifier; "expand_relative_import" >:: test_expand_relative_import]
  |> Test.run
