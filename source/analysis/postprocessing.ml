(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Pyre
module Error = AnalysisError

(* General idea: Keep two hash tables - one for unused ignores, and one from ignored lines -> list
   of ignores affecting the line. For each error, process the ignores on that line one by one, and
   remove the used codes from the map of unused ignores. Since the hash tables are initialized with
   only the sources we're considering, this is sufficient to determine all ignored errors and unused
   ignores. *)
let ignore ~qualifier { Source.metadata = { Source.Metadata.ignore_lines; _ }; _ } errors =
  let unused_ignores, ignore_lookup =
    let unused_ignores = Location.Table.create () in
    let ignore_lookup = Location.Table.create () in
    List.iter ignore_lines ~f:(fun ignore ->
        Hashtbl.add_multi ignore_lookup ~key:(Ignore.key ignore) ~data:ignore);
    let register_unused_ignore ignore =
      match Ignore.kind ignore with
      | Ignore.TypeIgnore ->
          (* # type: ignore's don't throw unused ignore errors. *)
          ()
      | _ -> Hashtbl.set unused_ignores ~key:(Ignore.location ignore) ~data:ignore
    in
    List.iter ignore_lines ~f:register_unused_ignore;
    unused_ignores, ignore_lookup
  in
  let errors =
    let not_ignored error =
      let ignored = ref false in
      let error_code = Error.code error in
      let process_ignore ignore =
        let codes = Ignore.codes ignore in
        if List.is_empty codes then (
          Hashtbl.remove unused_ignores (Ignore.location ignore);
          ignored := true
          (* We need to be a bit careful to support the following pattern:
           *  # pyre-ignore[7, 5]
           *  line_that_only_errors_on_7() *))
        else if List.mem ~equal:( = ) codes error_code then (
          begin
            match Hashtbl.find unused_ignores (Ignore.location ignore) with
            | Some ({ Ignore.codes; _ } as ignore) ->
                let new_codes = List.filter codes ~f:(fun code -> not (code = error_code)) in
                if List.is_empty new_codes then
                  Hashtbl.remove unused_ignores (Ignore.location ignore)
                else
                  Hashtbl.set
                    unused_ignores
                    ~key:(Ignore.location ignore)
                    ~data:{ ignore with Ignore.codes = new_codes }
            | _ -> ()
          end;
          ignored := true)
      in
      let key =
        (* Don't care about module name here since we always operate within the same module *)
        Error.key error |> fun { Location.WithModule.start; stop; _ } -> { Location.start; stop }
      in
      Hashtbl.find ignore_lookup key >>| List.iter ~f:process_ignore |> ignore;
      not !ignored
    in
    List.filter ~f:not_ignored errors
  in
  let unused_ignore_errors =
    let to_error unused_ignore =
      {
        Error.location = Location.with_module ~qualifier (Ignore.location unused_ignore);
        kind = Error.UnusedIgnore (Ignore.codes unused_ignore);
        signature =
          {
            Node.location = Ignore.location unused_ignore;
            value = Statement.Define.Signature.create_toplevel ~qualifier:None;
          };
      }
    in
    List.map (Hashtbl.data unused_ignores) ~f:to_error
  in
  List.rev_append unused_ignore_errors errors


let add_local_mode_errors
    ~define
    {
      Source.metadata = { Source.Metadata.unused_local_modes; local_mode = actual_mode; _ };
      source_path = { SourcePath.qualifier; _ };
      _;
    }
    errors
  =
  let add_error errors unused_mode =
    match actual_mode with
    | Some actual_mode ->
        Error.create
          ~location:(Location.with_module ~qualifier (Node.location unused_mode))
          ~kind:(Error.UnusedLocalMode { unused_mode; actual_mode })
          ~define
        :: errors
    | None ->
        Log.debug "Impossible unused local mode error - no local mode is set.";
        errors
  in
  List.fold ~f:add_error ~init:errors unused_local_modes


let filter_errors
    ~configuration
    ~global_resolution
    ~metadata:{ Source.Metadata.local_mode; ignore_codes; _ }
    errors_by_define
  =
  let mode = Source.mode ~configuration ~local_mode in
  let filter errors =
    let keep_error error = not (Error.suppress ~mode ~ignore_codes error) in
    List.filter ~f:keep_error errors
  in
  List.map errors_by_define ~f:(fun errors -> filter errors |> List.sort ~compare:Error.compare)
  |> List.concat_map ~f:(Error.join_at_define ~resolution:global_resolution)
  |> Error.join_at_source ~resolution:global_resolution


(* TODO: Take `Source.Metadata.t` instead of `Source.t` to prevent this function from relying on the
   actual AST. *)
let run_on_source
    ~configuration
    ~global_resolution
    ~source:({ Source.metadata; source_path = { SourcePath.qualifier; _ }; _ } as source)
    errors_by_define
  =
  filter_errors ~configuration ~global_resolution ~metadata errors_by_define
  |> add_local_mode_errors ~define:(Source.top_level_define_node source) source
  |> ignore ~qualifier source
  |> List.map
       ~f:(Error.dequalify (Preprocessing.dequalify_map source) ~resolution:global_resolution)
  |> List.sort ~compare:Error.compare


let run ~scheduler ~configuration ~environment sources =
  let number_of_sources = List.length sources in
  Log.log ~section:`Progress "Postprocessing %d sources..." number_of_sources;
  let map _ modules =
    let ast_environment = TypeEnvironment.ReadOnly.ast_environment environment in
    let run_on_module module_name =
      match AstEnvironment.ReadOnly.get_raw_source ast_environment module_name with
      | None -> []
      | Some
          ( Result.Ok { Source.source_path = { SourcePath.is_external; _ }; _ }
          | Result.Error
              { AstEnvironment.ParserError.source_path = { SourcePath.is_external; _ }; _ } )
        when is_external ->
          []
      | Some (Result.Error { AstEnvironment.ParserError.is_suppressed; _ }) when is_suppressed -> []
      | Some (Result.Error { AstEnvironment.ParserError.message; _ }) ->
          let location =
            {
              Location.start = { Location.line = 1; column = 1 };
              stop = { Location.line = 1; column = 1 };
            }
          in
          let location_with_module =
            {
              Location.WithModule.path = module_name;
              start = Location.start location;
              stop = Location.stop location;
            }
          in
          let define =
            Statement.Define.create_toplevel
              ~unbound_names:[]
              ~qualifier:(Some module_name)
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
              Source.metadata =
                {
                  Source.Metadata.local_mode = Some { Node.value = Source.Declare; _ };
                  unused_local_modes = [];
                  _;
                };
              _;
            }) ->
          []
      | Some (Result.Ok source) ->
          let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment in
          let errors_by_define =
            let unannotated_global_environment =
              GlobalResolution.unannotated_global_environment global_resolution
            in
            UnannotatedGlobalEnvironment.ReadOnly.all_defines_in_module
              unannotated_global_environment
              module_name
            |> List.map ~f:(TypeEnvironment.ReadOnly.get_errors environment)
          in
          run_on_source ~configuration ~global_resolution ~source errors_by_define
    in
    List.length modules, List.concat_map modules ~f:run_on_module
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
        (Scheduler.Policy.fixed_chunk_size
           ~minimum_chunk_size:10
           ~minimum_chunks_per_worker:2
           ~preferred_chunk_size:250
           ())
      ~initial:(0, [])
      ~map
      ~reduce
      ~inputs:sources
      ()
  in
  errors
