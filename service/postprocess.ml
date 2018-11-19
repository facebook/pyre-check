(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Analysis
open Pyre

open PostprocessSharedMemory


let register_ignores ~configuration scheduler handles =
  (* Invalidate keys before updating *)
  let remove_ignores handles =
    let keys = List.map ~f:File.Handle.show handles in
    List.filter_map ~f:IgnoreKeys.get keys
    |> List.concat
    |> IgnoreLines.KeySet.of_list
    |> IgnoreLines.remove_batch;
    keys
    |> IgnoreKeys.KeySet.of_list
    |> IgnoreKeys.remove_batch
  in
  let remove_modes handles =
    ErrorModes.KeySet.of_list handles
    |> ErrorModes.remove_batch
  in
  let timer = Timer.start () in
  remove_ignores handles;
  remove_modes handles;

  (* Register new values *)
  let register_ignores_for_handle handle =
    let key = File.Handle.show handle in
    (* Register new ignores. *)
    match Ast.SharedMemory.Sources.get handle with
    | Some source ->
        let ignore_lines = Source.ignore_lines source in
        List.iter
          ~f:(fun ignore_line -> IgnoreLines.add (Ignore.key ignore_line) ignore_line)
          ignore_lines;
        IgnoreKeys.add key (List.map ~f:Ignore.key ignore_lines)
    | _ ->
        ()
  in
  let register_local_mode handle =
    match Ast.SharedMemory.Sources.get handle with
    | Some { metadata = { Ast.Source.Metadata.local_mode; _ }; _ } ->
        ErrorModes.add handle local_mode
    | _ ->
        ()
  in
  let register handles =
    List.iter handles ~f:register_ignores_for_handle;
    List.iter handles ~f:(register_local_mode);
  in
  Scheduler.iter scheduler ~configuration ~f:register ~inputs:handles;
  Statistics.performance ~name:"registered ignores" ~timer ()


let ignore ~configuration scheduler handles errors =
  let error_lookup =
    let add_to_lookup lookup error =
      Map.add_multi ~key:(Error.key error) ~data:(Error.code error) lookup
    in
    List.fold errors ~init:Location.Reference.Map.empty ~f:add_to_lookup in
  let errors =
    let not_ignored error =
      IgnoreLines.get (Error.key error)
      >>| (fun ignore_instance ->
          not (List.is_empty (Ignore.codes ignore_instance) ||
               List.mem ~equal:(=) (Ignore.codes ignore_instance) (Error.code error)))
      |> Option.value ~default:true
    in
    List.filter ~f:not_ignored errors
  in
  let unused_ignores =
    let get_unused_ignores path =
      let ignores =
        let key_to_ignores sofar key =
          IgnoreLines.get key
          >>| (fun ignore -> ignore :: sofar)
          |> Option.value ~default:sofar
        in
        List.fold
          (IgnoreKeys.get (File.Handle.show path) |> Option.value ~default:[])
          ~init:[]
          ~f:key_to_ignores
      in
      let filter_active_ignores sofar ignore =
        match Ignore.kind ignore with
        | Ignore.TypeIgnore -> sofar
        | _ ->
            begin
              match Map.find error_lookup (Ignore.key ignore) with
              | Some codes ->
                  let unused_codes =
                    let find_unused sofar code =
                      if List.mem ~equal:(=) codes code then sofar else code :: sofar
                    in
                    List.fold ~init:[] ~f:find_unused (Ignore.codes ignore)
                  in
                  if List.is_empty (Ignore.codes ignore) || List.is_empty unused_codes then
                    sofar
                  else
                    { ignore with Ignore.codes = unused_codes } :: sofar
              | _ -> ignore :: sofar
            end
      in
      List.fold ~init:[] ~f:filter_active_ignores ignores
    in
    let map _ = List.concat_map ~f:get_unused_ignores in
    Scheduler.map_reduce
      scheduler
      ~configuration
      ~map
      ~reduce:List.append
      ~initial:[]
      ~inputs:handles
      ()
  in
  let create_unused_ignore_error errors unused_ignore =
    let error =
      {
        Error.location =
          Location.instantiate
            (Ignore.location unused_ignore)
            ~lookup:(fun hash -> Ast.SharedMemory.Handles.get ~hash);
        kind = Error.UnusedIgnore (Ignore.codes unused_ignore);
        define = {
          Node.location = Ignore.location unused_ignore;
          value = Statement.Define.create_toplevel ~qualifier:[] ~statements:[];
        };
      }
    in
    error :: errors
  in
  List.fold ~init:errors ~f:create_unused_ignore_error unused_ignores
