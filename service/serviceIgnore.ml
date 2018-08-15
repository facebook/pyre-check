(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Analysis
open Pyre

module IgnoreSharedMemory = ServiceIgnoreSharedMemory
module Scheduler = ServiceScheduler

open IgnoreSharedMemory


let remove_ignores handles =
  let keys = List.map ~f:File.Handle.show handles in
  List.filter_map ~f:IgnoreKeys.get keys
  |> List.concat
  |> IgnoreLines.KeySet.of_list
  |> IgnoreLines.remove_batch


let register_ignores handle =
  let key = File.Handle.show handle in
  (* Register new ignores. *)
  match Ast.SharedMemory.get_source handle with
  | Some source ->
      let ignore_lines = Source.ignore_lines source in
      List.iter
        ~f:(fun ignore_line -> IgnoreLines.add (Ignore.key ignore_line) ignore_line)
        ignore_lines;
      IgnoreKeys.add key (List.map ~f:Ignore.key ignore_lines)
  | _ ->
      ()


let register_mode ~configuration handle =
  let key = File.Handle.show handle in
  let mode =
    match Ast.SharedMemory.get_source handle with
    | Some source -> Source.mode source ~configuration
    | _ -> Source.Default
  in
  ErrorModes.add key mode


let register ~configuration scheduler handles =
  let timer = Timer.start () in
  remove_ignores handles;

  let register handles =
    List.iter handles ~f:register_ignores;
    List.iter handles ~f:(register_mode ~configuration);
  in
  if Scheduler.is_parallel scheduler then
    Scheduler.iter scheduler ~configuration ~f:register handles
  else
    register handles;
  Statistics.performance ~name:"registered ignores" ~timer ()


let postprocess handles errors =
  let error_lookup = Location.Reference.Table.create () in
  let errors_with_ignore_suppression =
    let add_to_lookup ~key ~code =
      match Hashtbl.find error_lookup key with
      | Some codes -> Hashtbl.set ~key ~data:(code :: codes) error_lookup
      | _ -> Hashtbl.set ~key ~data:[code] error_lookup
    in
    let not_ignored error =
      add_to_lookup
        ~key:(Error.key error)
        ~code:(Error.code error);
      IgnoreLines.get (Error.key error)
      >>| (fun ignore_instance ->
          not (List.is_empty (Ignore.codes ignore_instance) ||
               List.mem ~equal:(=) (Ignore.codes ignore_instance) (Error.code error)))
      |> Option.value ~default:true
    in
    List.filter ~f:not_ignored errors
  in
  let unused_ignores =
    let paths_from_handles =
      let get_path paths handle =
        Ast.SharedMemory.get_source handle
        >>| (fun { Source.path; _ } -> path :: paths)
        |> Option.value ~default:paths
      in
      List.fold ~init:[] ~f:get_path
    in
    let get_unused_ignores sofar path =
      let ignores =
        let key_to_ignores sofar key =
          IgnoreLines.get key
          >>| (fun ignore -> ignore :: sofar)
          |> Option.value ~default:sofar
        in
        List.fold ~init:[] ~f:key_to_ignores (IgnoreKeys.get path |> Option.value ~default:[])
      in
      let unused_ignores =
        let filter_active_ignores sofar ignore =
          match Ignore.kind ignore with
          | Ignore.TypeIgnore -> sofar
          | _ ->
              begin
                match Hashtbl.find error_lookup (Ignore.key ignore) with
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
      sofar @ unused_ignores
    in
    List.fold ~init:[] ~f:get_unused_ignores (paths_from_handles handles)
  in
  let create_unused_ignore_error errors unused_ignore =
    let error =
      {
        Error.location =
          Location.instantiate
            (Ignore.location unused_ignore)
            ~lookup:(fun hash -> Ast.SharedMemory.get_path ~hash);
        kind = Error.UnusedIgnore (Ignore.codes unused_ignore);
        define = {
          Node.location = Ignore.location unused_ignore;
          value = Statement.Define.create_toplevel ~qualifier:[] ~statements:[];
        };
      }
    in
    error :: errors
  in
  List.fold ~init:errors_with_ignore_suppression ~f:create_unused_ignore_error unused_ignores
