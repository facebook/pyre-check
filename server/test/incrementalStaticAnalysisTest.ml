(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Test
open Analysis
open OUnit2

let assert_static_analysis_errors ~context ~source ~check ~expected =
  Annotated.Class.AttributeCache.clear ();
  let descriptions =
    let errors =
      let configuration, source_paths, environment =
        let project = ScratchProject.setup ~context ~external_sources:[] ["__init__.py", source] in
        let _, _, environment = ScratchProject.build_environment project in
        let configuration = ScratchProject.configuration_of project in
        let source_paths = ScratchProject.source_paths_of project in
        configuration, source_paths, environment
      in
      Server.IncrementalStaticAnalysis.run_additional_check
        ~configuration
        ~scheduler:(Scheduler.mock ())
        ~environment
        ~source_paths
        ~check
    in
    List.map errors ~f:(fun error ->
        Error.Instantiated.description error ~show_error_traces:false ~concise:true)
  in
  assert_equal
    ~cmp:(List.equal String.equal)
    ~printer:(String.concat ~sep:"\n")
    expected
    descriptions


let test_run_awaitable_check context =
  assert_static_analysis_errors
    ~context
    ~source:
      {|
     import typing
     class C:
       async def awaitable(self) -> int: ...
     async def bar(c: C) -> int:
       # Ensure that we need the resolution shared memory.
       x = c.awaitable()
  |}
    ~check:"awaitable"
    ~expected:["Unawaited awaitable [1001]: Awaitable assigned to `x` is never awaited."];
  assert_static_analysis_errors
    ~context
    ~source:{|
    y = 1
    y
|}
    ~check:"deobfuscation"
    ~expected:["Deobfuscation [1002]: \n1\n"];
  assert_static_analysis_errors ~context ~source:{|x = 1|} ~check:"nonexistent" ~expected:[]


let () =
  "incrementalStaticAnalysis" >::: ["run_awaitable_check" >:: test_run_awaitable_check] |> Test.run
