(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module is responsible for suppressing errors based on `# pyre-fixme/pyre-ignore` comments,
   or filtering them based on the strictness of the mode.

   This is also where we emit a special error for files that fail to parse. *)

open Core
open Ast
open Pyre
module Error = AnalysisError

module Ignore = struct
  include Ignore
  include Hashable.Make (Ignore)
end

let to_unused_ignore_error ~ignores_to_used_errors ~qualifier ({ Ignore.location; _ } as ignore) =
  let make_unused_ignore_error unused_codes =
    {
      Error.location = Location.with_module ~module_reference:qualifier location;
      kind = Error.UnusedIgnore unused_codes;
      signature =
        { Node.location; value = Statement.Define.Signature.create_toplevel ~qualifier:None };
    }
  in
  match ignore, Hashtbl.find_multi ignores_to_used_errors ignore with
  | { Ignore.kind = TypeIgnore; _ }, _ ->
      (* Don't throw unused ignore errors for `# type: ignore` comments. *)
      None
  | unused_ignore, [] -> Ignore.codes unused_ignore |> make_unused_ignore_error |> Option.some
  | { Ignore.codes = []; _ }, _ :: _ ->
      (* A `pyre-ignore/fixme` without any codes is considered used if there exists at least one
         error. So, don't emit an unused-ignore error. *)
      None
  | { Ignore.codes = _ :: _ as ignored_codes; _ }, errors_covered_by_ignore ->
      let used_codes = List.map errors_covered_by_ignore ~f:Error.code |> Int.Set.of_list in
      let unused_codes = Set.diff (Int.Set.of_list ignored_codes) used_codes in
      Set.to_list unused_codes
      |> make_unused_ignore_error
      |> Option.some_if (not (Set.is_empty unused_codes))


let errors_with_ignores_covering_error ~errors all_ignores =
  let line_number_to_ignores_lookup =
    let line_ignore_pairs ignore =
      Ignore.lines_covered_by_ignore ignore |> List.map ~f:(fun line -> line, ignore)
    in
    all_ignores |> List.concat_map ~f:line_ignore_pairs |> Int.Table.of_alist_multi
  in
  let is_covered_by_ignore ~error ignore =
    let ignored_codes = Ignore.codes ignore in
    List.is_empty ignored_codes || List.mem ~equal:( = ) ignored_codes (Error.code error)
  in
  let ignores_covering_error
      ({ Error.location = { Location.WithModule.start = { Location.line; _ }; _ }; _ } as error)
    =
    Hashtbl.find line_number_to_ignores_lookup line
    >>| List.filter ~f:(is_covered_by_ignore ~error)
    |> Option.value ~default:[]
  in
  List.map errors ~f:(fun error -> error, ignores_covering_error error)


let handle_ignores_and_fixmes
    ~qualifier
    { Source.typecheck_flags = { Source.TypecheckFlags.ignore_lines; _ }; _ }
    errors
  =
  let errors_and_ignores_covering_error = errors_with_ignores_covering_error ~errors ignore_lines in
  let unignored_errors =
    List.filter_map errors_and_ignores_covering_error ~f:(fun (error, ignores) ->
        Option.some_if (List.is_empty ignores) error)
  in
  let unused_ignore_errors =
    let ignores_to_used_errors =
      List.concat_map errors_and_ignores_covering_error ~f:(fun (error, ignores) ->
          List.map ignores ~f:(fun ignore -> ignore, error))
      |> Ignore.Table.of_alist_multi
    in
    List.filter_map ignore_lines ~f:(to_unused_ignore_error ~ignores_to_used_errors ~qualifier)
  in
  List.rev_append unused_ignore_errors unignored_errors


let add_local_mode_errors
    ~define
    {
      Source.typecheck_flags =
        { Source.TypecheckFlags.unused_local_modes; local_mode = actual_mode; _ };
      module_path = { ModulePath.qualifier; _ };
      _;
    }
    errors
  =
  let add_error errors unused_mode =
    match actual_mode with
    | Some actual_mode ->
        Error.create
          ~location:(Location.with_module ~module_reference:qualifier (Node.location unused_mode))
          ~kind:(Error.UnusedLocalMode { unused_mode; actual_mode })
          ~define
        :: errors
    | None ->
        Log.debug "Impossible unused local mode error - no local mode is set.";
        errors
  in
  List.fold ~f:add_error ~init:errors unused_local_modes


let filter_errors
    ~mode
    ~global_resolution
    ~typecheck_flags:{ Source.TypecheckFlags.ignore_codes; _ }
    errors_by_define
  =
  let filter errors =
    let keep_error error = not (Error.suppress ~mode ~ignore_codes error) in
    List.filter ~f:keep_error errors
  in
  List.map errors_by_define ~f:(fun errors -> filter errors |> List.sort ~compare:Error.compare)
  |> List.concat_map ~f:(Error.join_at_define ~resolution:global_resolution)
  |> Error.join_at_source ~resolution:global_resolution


(* TODO: Take `Source.TypecheckFlags.t` instead of `Source.t` to prevent this function from relying
   on the actual AST. *)
let run_on_source
    ~environment
    ~source:
      ({
         Source.typecheck_flags = { local_mode; _ } as typecheck_flags;
         module_path = { ModulePath.qualifier; _ };
         _;
       } as source)
    errors_by_define
  =
  let suppress_errors_based_on_comments
      ~controls:{ EnvironmentControls.TypeCheckControls.include_suppressed_errors; _ }
      errors
    =
    if include_suppressed_errors then
      errors
    else
      handle_ignores_and_fixmes ~qualifier source errors
  in
  let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment in
  let controls = TypeEnvironment.ReadOnly.controls environment in
  let mode = Source.mode ~configuration:(EnvironmentControls.configuration controls) ~local_mode in
  filter_errors ~mode ~global_resolution ~typecheck_flags errors_by_define
  |> add_local_mode_errors ~define:(Source.top_level_define_node source) source
  |> suppress_errors_based_on_comments ~controls:(EnvironmentControls.type_check_controls controls)
  |> List.map
       ~f:(Error.dequalify (Preprocessing.dequalify_map source) ~resolution:global_resolution)
  |> List.sort ~compare:Error.compare


let run_on_qualifier environment ~dependency qualifier =
  let ast_environment = TypeEnvironment.ReadOnly.ast_environment environment in
  match AstEnvironment.ReadOnly.get_raw_source ?dependency ast_environment qualifier with
  | None -> []
  | Some
      ( Result.Ok { Source.module_path = { ModulePath.is_external; _ }; _ }
      | Result.Error { AstEnvironment.ParserError.module_path = { ModulePath.is_external; _ }; _ }
        )
    when is_external ->
      []
  | Some (Result.Error { AstEnvironment.ParserError.is_suppressed; _ }) when is_suppressed -> []
  | Some (Result.Error { AstEnvironment.ParserError.message; location; _ }) ->
      let location_with_module =
        {
          Location.WithModule.module_reference = qualifier;
          start = Location.start location;
          stop = Location.stop location;
        }
      in
      let define =
        Statement.Define.create_toplevel
          ~unbound_names:[]
          ~qualifier:(Some qualifier)
          ~statements:[]
        |> Node.create ~location
      in
      [
        AnalysisError.create
          ~location:location_with_module
          ~kind:(AnalysisError.ParserFailure message)
          ~define;
      ]
  | Some
      (Result.Ok
        {
          Source.typecheck_flags =
            {
              Source.TypecheckFlags.local_mode = Some { Node.value = Source.Declare; _ };
              unused_local_modes = [];
              _;
            };
          _;
        }) ->
      []
  | Some (Result.Ok source) ->
      let unannotated_global_environment =
        TypeEnvironment.ReadOnly.unannotated_global_environment environment
      in
      let errors_by_define =
        UnannotatedGlobalEnvironment.ReadOnly.get_define_names
          ?dependency
          unannotated_global_environment
          qualifier
        |> List.map ~f:(TypeEnvironment.ReadOnly.get_errors ?dependency environment)
      in
      run_on_source ~environment ~source errors_by_define


let run ~scheduler ~environment sources =
  let timer = Timer.start () in
  let number_of_sources = List.length sources in
  Log.log ~section:`Progress "Postprocessing %d sources..." number_of_sources;
  let map _ modules =
    List.length modules, List.concat_map modules ~f:(run_on_qualifier environment ~dependency:None)
  in
  let reduce (left_count, left_errors) (right_count, right_errors) =
    let number_sources = left_count + right_count in
    Log.log ~section:`Progress "Postprocessed %d of %d sources" number_sources number_of_sources;
    number_sources, List.append left_errors right_errors
  in
  let _, errors =
    Scheduler.map_reduce
      scheduler
      ~policy:
        (Scheduler.Policy.fixed_chunk_count
           ~minimum_chunks_per_worker:1
           ~minimum_chunk_size:100
           ~preferred_chunks_per_worker:5
           ())
      ~initial:(0, [])
      ~map
      ~reduce
      ~inputs:sources
      ()
  in
  Statistics.performance ~name:"check_Postprocessing" ~phase_name:"Postprocessing" ~timer ();
  errors
