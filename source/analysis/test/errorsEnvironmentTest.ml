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
open Test

let instantiate_and_stringify ~lookup errors =
  let to_string error =
    let description = AnalysisError.Instantiated.concise_description error in
    let path = AnalysisError.Instantiated.path error in
    let line = AnalysisError.Instantiated.location error |> Location.WithPath.line in
    Format.sprintf "%s %d: %s" path line description
  in
  List.map errors ~f:(AnalysisError.instantiate ~show_error_traces:false ~lookup)
  |> List.map ~f:to_string


let test_postprocessing context =
  let code header_comment =
    Format.asprintf
      {|
        %s

        def foo(x):
            return x

        def bar(x: int) -> str:
            return x

        # pyre-ignore[7]
        def ignored_bad_return(x: int) -> str:
            return x

        # pyre-ignore[7]
        def has_unused_ignore(x: int) -> int:
            return x
      |}
      header_comment
  in
  let project =
    ScratchProject.setup
      ~context
      [
        "has_parse_error.py", {|
          def foo(
        |};
        "unsafe.py", code "# pyre-unsafe";
        "strict.py", code "# pyre-strict";
      ]
  in
  ScratchProject.ReadWrite.errors_environment project
  |> ErrorsEnvironment.populate_all_errors ~scheduler:(Test.mock_scheduler ());
  let errors =
    ScratchProject.errors_environment project
    |> ErrorsEnvironment.ReadOnly.get_all_errors
    |> instantiate_and_stringify
         ~lookup:
           (ScratchProject.ast_environment project |> AstEnvironment.ReadOnly.get_real_path_relative)
  in
  (* Note that the output is always sorted, we can depend on this in tests *)
  assert_equal
    ~ctxt:context
    ~printer:[%show: string list]
    [
      "has_parse_error.py 2: Parsing failure [404]: '(' was never closed";
      "strict.py 4: Missing return annotation [3]: Return type must be annotated.";
      "strict.py 4: Missing parameter annotation [2]: Parameter must be annotated.";
      "strict.py 8: Incompatible return type [7]: Expected `str` but got `int`.";
      "strict.py 10: Unused ignore [0]: The `pyre-ignore[7]` or `pyre-fixme[7]` comment is not \
       suppressing type errors, please remove it.";
      "strict.py 12: Incompatible return type [7]: Expected `str` but got `int`.";
      "strict.py 14: Unused ignore [0]: The `pyre-ignore[7]` or `pyre-fixme[7]` comment is not \
       suppressing type errors, please remove it.";
      (* Note that the strict-mode-only errors on line 4 are not present for unsafe.py *)
      "unsafe.py 8: Incompatible return type [7]: Expected `str` but got `int`.";
      "unsafe.py 10: Unused ignore [0]: The `pyre-ignore[7]` or `pyre-fixme[7]` comment is not \
       suppressing type errors, please remove it.";
      "unsafe.py 12: Incompatible return type [7]: Expected `str` but got `int`.";
      "unsafe.py 14: Unused ignore [0]: The `pyre-ignore[7]` or `pyre-fixme[7]` comment is not \
       suppressing type errors, please remove it.";
    ]
    errors


let () = "environment" >::: ["postprocessing" >:: test_postprocessing] |> Test.run
