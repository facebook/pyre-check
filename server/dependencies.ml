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
    ~state:{ State.environment = (module Handler: Environment.Handler); scheduler; _ }
    ~configuration:({ incremental_transitive_dependencies; _ } as configuration)
    files =
  let timer = Timer.start () in
  let handle file =
    try
      Some (File.handle file ~configuration)
    with File.NonexistentHandle _ ->
      None
  in
  let handles = List.filter_map files ~f:handle in
  let old_signature_hashes, new_signature_hashes =
    let signature_hashes ~default =
      let table = File.Handle.Table.create () in
      let add_signature_hash file =
        try
          let handle = File.handle file ~configuration in
          let signature_hash =
            Ast.SharedMemory.Sources.get handle
            >>| Source.signature_hash
            |> Option.value ~default
          in
          Hashtbl.set table ~key:handle ~data:signature_hash
        with (File.NonexistentHandle _) ->
          Log.log ~section:`Server "Unable to get handle for %a" File.pp file
      in
      List.iter files ~f:add_signature_hash;
      table
    in
    let old_signature_hashes = signature_hashes ~default:0 in

    (* Update the tracked handles, if necessary. *)
    let newly_introduced_handles =
      List.filter
        handles
        ~f:(fun handle -> Option.is_none (Ast.SharedMemory.Sources.get handle))
    in
    if not (List.is_empty newly_introduced_handles) then
      Ast.SharedMemory.HandleKeys.add
        ~handles:(File.Handle.Set.of_list newly_introduced_handles |> Set.to_tree);
    Ast.SharedMemory.Sources.remove ~handles;
    let targets =
      let find_target file = Path.readlink (File.path file) in
      List.filter_map files ~f:find_target
    in
    Ast.SharedMemory.SymlinksToPaths.remove ~targets;
    Service.Parser.parse_sources
      ~configuration
      ~scheduler
      ~preprocessing_state:None
      ~files
    |> ignore;
    let new_signature_hashes = signature_hashes ~default:(-1) in
    old_signature_hashes, new_signature_hashes
  in

  let dependents =
    Log.log
      ~section:`Server
      "Handling type check request for files %a"
      Sexp.pp [%message (handles: File.Handle.t list)];
    let signature_hash_changed handle =
      (* If the hash is not found, then the handle was not part of
         handles, hence its hash cannot have changed. *)
      Hashtbl.find old_signature_hashes handle
      >>= (fun old_hash ->
          Hashtbl.find new_signature_hashes handle
          >>| fun new_hash -> old_hash <> new_hash)
      |> Option.value ~default:false
    in
    let deferred_files =
      let modules =
        List.filter handles ~f:signature_hash_changed
        |> List.map ~f:(fun handle -> Source.qualifier ~handle)
      in
      let get_dependencies =
        if incremental_transitive_dependencies then
          Dependencies.transitive_of_list
        else
          Dependencies.of_list
      in
      get_dependencies
        ~get_dependencies:Handler.dependencies
        ~modules
      |> File.Handle.Set.filter_map ~f:SharedMemory.Sources.QualifiersToHandles.get
      |> Fn.flip Set.diff (File.Handle.Set.of_list handles)
    in
    Statistics.performance
      ~name:"Computed dependencies"
      ~timer
      ~randomly_log_every:100
      ~normals:["changed files", List.to_string ~f:File.Handle.show handles]
      ~integers:[
        "number of dependencies", File.Handle.Set.length deferred_files;
        "number of files", List.length handles;
      ]
      ();
    deferred_files
  in
  Log.log
    ~section:`Server
    "Inferred affected files: %a"
    Sexp.pp [%message (dependents: File.Handle.Set.t)];
  let to_file handle =
    Ast.SharedMemory.Sources.get handle
    >>= (fun { Ast.Source.handle; _ } -> File.Handle.to_path ~configuration handle)
    >>| File.create
  in
  File.Set.filter_map dependents ~f:to_file
