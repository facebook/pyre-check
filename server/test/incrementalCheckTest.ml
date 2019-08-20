open Core
open Test
open Ast
open Analysis
open OUnit2

let assert_incremental_check_errors ~context ~initial_sources ~updated_sources ~expected =
  let trim (is_external, relative, content) =
    is_external, (relative, Test.trim_extra_indentation content)
  in
  let initial_sources = List.map initial_sources ~f:trim in
  let updated_sources = List.map updated_sources ~f:trim in
  (* Setup a server. *)
  let ( ({ ScratchProject.module_tracker; _ } as project),
        ({ Server.State.ast_environment; _ } as state) )
    =
    ServerTest.initialize_server ~context ~initial_sources
  in
  let update_module_tracker (is_external, source) =
    ScratchProject.add_source project ~is_external source
  in
  List.iter updated_sources ~f:update_module_tracker;

  (* This is kind of convoluted - I wanted to go through the module tracker API to avoid having to
     leak information from ScratchProject. *)
  let configuration = ScratchProject.configuration_of project in
  let paths =
    List.map updated_sources ~f:(fun (_, (relative, _)) ->
        SourcePath.qualifier_of_relative relative)
    |> List.filter_map ~f:(ModuleTracker.lookup module_tracker)
    |> List.map ~f:(SourcePath.full_path ~configuration)
  in
  let errors =
    let description error =
      Error.instantiate
        error
        ~lookup:(AstEnvironment.ReadOnly.get_relative (AstEnvironment.read_only ast_environment))
      |> Error.Instantiated.description ~show_error_traces:false ~concise:false
    in
    Server.IncrementalCheck.recheck ~state ~configuration paths |> snd |> List.map ~f:description
  in
  assert_equal ~printer:(String.concat ~sep:"\n") expected errors


let test_incremental_check context =
  (* We warn on extraneous ignores. *)
  assert_incremental_check_errors
    ~context
    ~initial_sources:
      [ ( false,
          "a.py",
          {|
        # pyre-ignore
        def unused_pyre_ignore() -> int:
          return 0
      |}
        ) ]
    ~updated_sources:
      [ ( false,
          "a.py",
          {|
        # pyre-ignore
        def still_unused_pyre_ignore() -> int:
          return 0
      |}
        ) ]
    ~expected:["Unused ignore [0]: Pyre ignore is extraneous."];

  (* If an external source is updated, it is filtered properly. *)
  assert_incremental_check_errors
    ~context
    ~initial_sources:
      [ ( true,
          "external.py",
          {|
        # pyre-ignore
        def unused_pyre_ignore() -> int:
          return 0
      |}
        ) ]
    ~updated_sources:
      [ ( true,
          "external.py",
          {|
        # pyre-ignore
        def still_unused_pyre_ignore() -> int:
          return 0
      |}
        ) ]
    ~expected:[]


let () = "incremental_check" >::: ["incremental_check" >:: test_incremental_check] |> Test.run
