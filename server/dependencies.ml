(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Analysis
open Configuration.Analysis
open Pyre

let compute_dependencies
    ~state:{ State.environment; scheduler; _ }
    ~configuration:({ incremental_transitive_dependencies; _ } as configuration)
    source_paths
  =
  let timer = Timer.start () in
  let qualifiers = List.map source_paths ~f:(fun { SourcePath.qualifier; _ } -> qualifier) in
  let old_signature_hashes, new_signature_hashes =
    let signature_hashes ~default =
      let table = Reference.Table.create () in
      let add_signature_hash qualifier =
        let signature_hash =
          Ast.SharedMemory.Sources.get qualifier >>| Source.signature_hash |> Option.value ~default
        in
        Hashtbl.set table ~key:qualifier ~data:signature_hash
      in
      List.iter qualifiers ~f:add_signature_hash;
      table
    in
    let old_signature_hashes = signature_hashes ~default:0 in
    Ast.SharedMemory.Sources.remove qualifiers;
    Service.Parser.parse_sources ~configuration ~scheduler ~preprocessing_state:None source_paths
    |> ignore;
    let new_signature_hashes = signature_hashes ~default:(-1) in
    old_signature_hashes, new_signature_hashes
  in
  let handles = List.map source_paths ~f:(fun { SourcePath.relative; _ } -> relative) in
  Log.log
    ~section:`Server
    "Handling type check request for files %a"
    Sexp.pp
    [%message (handles : string list)];
  let signature_hash_changed handle =
    (* If the hash is not found, then the handle was not part of handles, hence its hash cannot
       have changed. *)
    Hashtbl.find old_signature_hashes handle
    >>= (fun old_hash ->
          Hashtbl.find new_signature_hashes handle >>| fun new_hash -> old_hash <> new_hash)
    |> Option.value ~default:false
  in
  let dependents =
    let modules = List.filter qualifiers ~f:signature_hash_changed in
    let get_dependencies =
      if incremental_transitive_dependencies then
        Dependencies.transitive_of_list
      else
        Dependencies.of_list
    in
    get_dependencies ~get_dependencies:(Environment.dependencies environment) ~modules
    |> Fn.flip Set.diff (Reference.Set.of_list qualifiers)
  in
  Statistics.performance
    ~name:"Computed dependencies"
    ~timer
    ~randomly_log_every:100
    ~normals:["changed files", String.concat handles ~sep:", "]
    ~integers:
      [ "number of dependencies", Reference.Set.length dependents;
        "number of files", List.length handles ]
    ();
  Log.log
    ~section:`Server
    "Inferred affected modules: %a"
    Sexp.pp
    [%message (dependents : Reference.Set.t)];
  dependents
