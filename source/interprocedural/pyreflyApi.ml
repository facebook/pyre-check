(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Module that implements the PyrePysaApi using the results from a pyrefly run with
   --report-pysa. *)

open Core
open Pyre
open Data_structures

module FormatError = struct
  type t =
    | UnexpectedJsonType of {
        json: Yojson.Safe.t;
        message: string;
      }
    | UnsupportedVersion of { version: int }
  [@@deriving show]
end

module Error = struct
  type t =
    | InvalidJsonError of string
    | IOError of string
    | FormatError of FormatError.t
  [@@deriving show]
end

exception
  PyreflyFileFormatError of {
    path: PyrePath.t;
    error: Error.t;
  }

module JsonUtil = struct
  let read_json_file_exn path =
    try Yojson.Safe.from_file (PyrePath.absolute path) with
    | Yojson.Json_error message ->
        raise (PyreflyFileFormatError { path; error = Error.InvalidJsonError message })
    | Sys_error message -> raise (PyreflyFileFormatError { path; error = Error.IOError message })


  let as_string = function
    | `String value -> Ok value
    | json ->
        Error (FormatError.UnexpectedJsonType { json; message = Format.sprintf "expected string" })


  let get_string_member json key =
    match Yojson.Safe.Util.member key json with
    | `String value -> Ok value
    | _ ->
        Error
          (FormatError.UnexpectedJsonType
             {
               json;
               message = Format.sprintf "expected an object with key `%s` containing a string" key;
             })


  let get_optional_string_member json key =
    match Yojson.Safe.Util.member key json with
    | `String value -> Ok (Some value)
    | `Null -> Ok None
    | _ ->
        Error
          (FormatError.UnexpectedJsonType
             { json; message = Format.sprintf "expected the key `%s` to contain a string" key })


  let get_int_member json key =
    match Yojson.Safe.Util.member key json with
    | `Int value -> Ok value
    | _ ->
        Error
          (FormatError.UnexpectedJsonType
             {
               json;
               message = Format.sprintf "expected an object with key `%s` containing an integer" key;
             })


  let check_format_version ~expected json =
    let open Core.Result.Monad_infix in
    get_int_member json "format_version"
    >>= function
    | version when Int.equal version expected -> Ok ()
    | version -> Error (FormatError.UnsupportedVersion { version })


  let get_object_member json key =
    match Yojson.Safe.Util.member key json with
    | `Assoc bindings -> Ok bindings
    | _ ->
        Error
          (FormatError.UnexpectedJsonType
             {
               json;
               message = Format.sprintf "expected an object with key `%s` containing an object" key;
             })


  let check_object = function
    | `Assoc _ -> Ok ()
    | json ->
        Error (FormatError.UnexpectedJsonType { json; message = "expected root to be an object" })
end

(* Represents the `pyrefly_python::module_path::ModulePathDetails` rust type. *)
module ModulePath = struct
  type t =
    | Filesystem of ArtifactPath.t
    | Namespace of PyrePath.t
    | Memory of PyrePath.t
    | BundledTypeshed of PyrePath.t
  [@@deriving show]

  let from_json = function
    | `Assoc [("FileSystem", `String path)] ->
        Ok (Filesystem (path |> PyrePath.create_absolute |> ArtifactPath.create))
    | `Assoc [("Namespace", `String path)] -> Ok (Namespace (PyrePath.create_absolute path))
    | `Assoc [("Memory", `String path)] -> Ok (Memory (PyrePath.create_absolute path))
    | `Assoc [("BundledTypeshed", `String path)] ->
        Ok (BundledTypeshed (PyrePath.create_absolute path))
    | json -> Error (FormatError.UnexpectedJsonType { json; message = "expected a module path" })


  let artifact_file_path ~pyrefly_directory = function
    | Filesystem path -> Some path
    | Namespace _ -> None (* directory *)
    | Memory _ -> None (* not handled *)
    | BundledTypeshed path ->
        Some
          (pyrefly_directory
          |> PyrePath.append ~element:"typeshed"
          |> PyrePath.append ~element:(PyrePath.absolute path)
          |> ArtifactPath.create)
end

(* Represents the unique identifier assigned to a module by pyrefly. This represents a specific
   module name and path. *)
module ModuleId : sig
  type t [@@deriving compare, sexp, hash, show]

  val from_int : int -> t
end = struct
  type t = int [@@deriving compare, sexp, hash, show]

  let from_int = Fn.id
end

(* Represents the unique identifier assigned to a module by pysa. This represents a specific module
   name and path. Note that this is converted into `Ast.Reference.t` during the taint analysis for
   backward compatibility with the old Pyre1 API, which assumes a module name can only map to one
   source file. *)
module Qualifier : sig
  type t [@@deriving compare, sexp, hash, show]

  val create : path:string option -> Ast.Reference.t -> t

  (* The taint analysis uses Reference.t to uniquely represent modules, so we use
     to_reference/from_reference to convert to that type *)
  val to_reference : t -> Ast.Reference.t

  val from_reference : Ast.Reference.t -> t

  module Map : Map.S with type Key.t = t
end = struct
  module T = struct
    type t = {
      path: string option;
      module_name: Ast.Reference.t;
    }
    [@@deriving compare, sexp, hash, show]

    let create ~path module_name = { path; module_name }

    let to_reference { path; module_name } =
      match path with
      | None -> module_name
      | Some path ->
          Format.sprintf "%s:%s" path (Ast.Reference.show module_name) |> Ast.Reference.create


    let from_reference reference =
      if List.exists (Ast.Reference.as_list reference) ~f:(fun s -> String.contains s ':') then
        reference
        |> Ast.Reference.show
        |> String.rsplit2_exn ~on:':'
        |> fun (path, reference) ->
        { path = Some path; module_name = Ast.Reference.create reference }
      else
        { path = None; module_name = reference }
  end

  include T
  module Map = Map.Make (T)
end

module QualifierSharedMemoryKey = struct
  type t = Qualifier.t [@@deriving compare]

  let to_string key =
    key |> Qualifier.to_reference |> Analysis.SharedMemoryKeys.ReferenceKey.to_string


  let from_string s =
    s |> Analysis.SharedMemoryKeys.ReferenceKey.from_string |> Qualifier.from_reference
end

(* Represents the path to a pyrefly module information file *)
module ModuleInfoPath : sig
  type t [@@deriving compare, sexp, hash, show]

  val create : string -> t

  val raw : t -> string
end = struct
  type t = string [@@deriving compare, sexp, hash, show]

  let create = Fn.id

  let raw = Fn.id
end

(* Represents the content of the `pyrefly.pysa.json` file. This matches the
   `pyrefly::report::pysa::PysaProjectFile` rust type. *)
module ProjectFile = struct
  module Module = struct
    type t = {
      module_id: ModuleId.t;
      module_name: Ast.Reference.t;
      module_path: ModulePath.t;
      info_path: ModuleInfoPath.t option;
    }
    [@@deriving show]

    let from_json json =
      let open Core.Result.Monad_infix in
      JsonUtil.get_int_member json "module_id"
      >>= fun module_id ->
      JsonUtil.get_string_member json "module_name"
      >>= fun module_name ->
      JsonUtil.get_optional_string_member json "info_path"
      >>= fun info_path ->
      JsonUtil.get_object_member json "source_path"
      >>= fun source_path ->
      ModulePath.from_json (`Assoc source_path)
      >>| fun module_path ->
      {
        module_id = ModuleId.from_int module_id;
        module_name = Ast.Reference.create module_name;
        module_path;
        info_path = Option.map ~f:ModuleInfoPath.create info_path;
      }
  end

  module ModuleMap = Map.Make (ModuleId)

  type t = { modules: Module.t ModuleMap.t }

  let from_json json =
    let open Core.Result.Monad_infix in
    let parse_modules modules =
      modules
      |> List.map ~f:snd
      |> List.map ~f:Module.from_json
      |> Result.all
      >>| List.map ~f:(fun ({ Module.module_id; _ } as module_) -> module_id, module_)
      >>| ModuleMap.of_alist_exn
    in
    JsonUtil.check_object json
    >>= fun () ->
    JsonUtil.check_format_version ~expected:1 json
    >>= fun () ->
    JsonUtil.get_object_member json "modules" >>= parse_modules >>| fun modules -> { modules }


  let from_path_exn path =
    let () = Log.debug "Parsing pyrefly project file `%a`" PyrePath.pp path in
    let json = JsonUtil.read_json_file_exn path in
    match from_json json with
    | Ok project -> project
    | Error error -> raise (PyreflyFileFormatError { path; error = Error.FormatError error })


  (* TODO(T225700656): Remove this, this is to silence the unused value warning *)
  let _ = Module.pp, ModuleId.show, ModuleInfoPath.show
end

(* Represents information from pyrefly about a given module, stored as a `module:id.json` file. This
   matches the `pyrefly::report::pysa::PysaModuleFile` rust type. *)
module ModuleInfoFile = struct
  type t = {
    (* TODO(T225700656): module_id, module_name and source_path are already specified in the Project
       module. We should probably remove those from the file format. *)
    module_id: ModuleId.t;
    module_name: Ast.Reference.t;
    source_path: ModulePath.t;
    type_of_expression: string Ast.Location.Map.t;
  }

  let from_json json =
    let open Core.Result.Monad_infix in
    let parse_type_of_expression type_of_expression =
      type_of_expression
      |> List.map ~f:(fun (location, ty) ->
             Ast.Location.from_string location
             |> Result.map_error ~f:(fun error ->
                    FormatError.UnexpectedJsonType { json = `String location; message = error })
             >>= fun location -> JsonUtil.as_string ty >>| fun ty -> location, ty)
      |> Result.all
      >>| Ast.Location.Map.of_alist_exn
    in
    JsonUtil.check_object json
    >>= fun () ->
    JsonUtil.check_format_version ~expected:1 json
    >>= fun () ->
    JsonUtil.get_int_member json "module_id"
    >>= fun module_id ->
    JsonUtil.get_string_member json "module_name"
    >>= fun module_name ->
    JsonUtil.get_object_member json "source_path"
    >>= fun source_path ->
    ModulePath.from_json (`Assoc source_path)
    >>= fun source_path ->
    JsonUtil.get_object_member json "type_of_expression"
    >>= parse_type_of_expression
    >>| fun type_of_expression ->
    {
      module_id = ModuleId.from_int module_id;
      module_name = Ast.Reference.create module_name;
      source_path;
      type_of_expression;
    }


  let from_path_exn ~pyrefly_directory path =
    let path =
      pyrefly_directory
      |> PyrePath.append ~element:"modules"
      |> PyrePath.append ~element:(ModuleInfoPath.raw path)
    in
    let () = Log.debug "Parsing pyrefly module info file %a" PyrePath.pp path in
    let json = JsonUtil.read_json_file_exn path in
    match from_json json with
    | Ok module_info -> module_info
    | Error error -> raise (PyreflyFileFormatError { path; error = Error.FormatError error })
end

module ModuleInfosSharedMemory = struct
  module Module = struct
    type t = {
      source_path: ArtifactPath.t option (* The path of source code as seen by the analyzer. *);
    }
  end

  include
    Hack_parallel.Std.SharedMemory.FirstClass.WithCache.Make
      (QualifierSharedMemoryKey)
      (struct
        type t = Module.t

        let prefix = Hack_parallel.Std.Prefix.make ()

        let description = "pyrefly module infos"
      end)
end

module QualifiersWithSourceSharedMemory =
  Hack_parallel.Std.SharedMemory.FirstClass.NoCache.Make
    (Memory.SingletonKey)
    (struct
      type t = Qualifier.t list

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = "pyrefly all modules"
    end)

module TypeOfExpressionsSharedMemory = struct
  module Key = struct
    type t = {
      qualifier: Qualifier.t;
      location: Ast.Location.t;
    }
    [@@deriving compare, sexp]

    let to_string key = key |> sexp_of_t |> Core.Sexp.to_string

    let from_string sexp_string = sexp_string |> Core.Sexp.of_string |> t_of_sexp
  end

  include
    Hack_parallel.Std.SharedMemory.FirstClass.WithCache.Make
      (Key)
      (struct
        type t = string (* TODO(T225700656): For now, pyrefly exports types as strings *)

        let prefix = Hack_parallel.Std.Prefix.make ()

        let description = "pyrefly type of expressions"
      end)
end

module OptionalSource = struct
  type 'a t =
    | Some of 'a
    | NoSource
end

module AstsSharedMemory =
  Hack_parallel.Std.SharedMemory.FirstClass.NoCache.Make
    (QualifierSharedMemoryKey)
    (struct
      type t = Analysis.Parsing.ParseResult.t OptionalSource.t

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = "pyrefly asts"
    end)

(* API handle stored in the main process. The type `t` should not be sent to workers, since it's
   expensive to copy. *)
module ReadWrite = struct
  module Module = struct
    type t = {
      module_id: ModuleId.t;
      module_name: Ast.Reference.t;
      source_path: ArtifactPath.t option; (* The path of source code as seen by the analyzer. *)
      pyrefly_info_path: ModuleInfoPath.t option;
    }
    [@@deriving show]

    let from_project
        ~pyrefly_directory
        { ProjectFile.Module.module_id; module_name; module_path; info_path }
      =
      {
        module_id;
        module_name;
        source_path = ModulePath.artifact_file_path ~pyrefly_directory module_path;
        pyrefly_info_path = info_path;
      }
  end

  type t = {
    qualifier_to_module_map: Module.t Qualifier.Map.t;
    module_infos_shared_memory: ModuleInfosSharedMemory.t;
    qualifiers_with_source_shared_memory: QualifiersWithSourceSharedMemory.t;
    asts_shared_memory: AstsSharedMemory.t;
    type_of_expressions_shared_memory: TypeOfExpressionsSharedMemory.t;
  }

  (* Build a mapping from a qualifier (module name + path prefix) to the module id, source path,
     etc. *)
  let create_module_mapping ~pyrefly_directory { ProjectFile.modules } =
    let make_unique_qualifiers (module_name, modules) =
      match modules with
      | [module_info] -> [Qualifier.create ~path:None module_name, module_info]
      | _ ->
          (* From a list of modules with the same module name, make unique qualifiers for each
             module, using the path as a prefix *)
          let number_modules = List.length modules in
          let pyre_path_elements path = path |> PyrePath.absolute |> String.split ~on:'/' in
          let module_path_elements = function
            | ModulePath.Filesystem path -> path |> ArtifactPath.raw |> pyre_path_elements
            | ModulePath.Namespace path -> "namespace:/" :: pyre_path_elements path
            | ModulePath.Memory path -> "memory:/" :: pyre_path_elements path
            | ModulePath.BundledTypeshed path -> "typeshed:/" :: pyre_path_elements path
          in
          let rec find_shortest_unique_prefix ~prefix_length modules_with_path =
            let map =
              modules_with_path
              |> List.fold ~init:SerializableStringMap.empty ~f:(fun sofar (path, module_info) ->
                     SerializableStringMap.update
                       (List.take path prefix_length |> List.rev |> String.concat ~sep:"/")
                       (function
                         | None -> Some module_info
                         | Some existing -> Some existing)
                       sofar)
            in
            if Int.equal number_modules (SerializableStringMap.cardinal map) then
              SerializableStringMap.to_alist map
            else if prefix_length >= 1000 then
              failwith "Could not make a unique qualifier for a module after 1000 iterations"
            else
              find_shortest_unique_prefix ~prefix_length:(prefix_length + 1) modules_with_path
          in
          modules
          |> List.map ~f:(fun ({ ProjectFile.Module.module_path; _ } as module_info) ->
                 List.rev (module_path_elements module_path), module_info)
          |> find_shortest_unique_prefix ~prefix_length:1
          |> List.map ~f:(fun (path, module_info) ->
                 Qualifier.create ~path:(Some path) module_name, module_info)
    in
    let add_to_module_name_mapping
        ~key:_
        ~data:({ ProjectFile.Module.module_name; _ } as module_info)
        sofar
      =
      Map.update sofar module_name ~f:(function
          | None -> [module_info]
          | Some existing -> module_info :: existing)
    in
    modules
    |> Map.fold ~init:Ast.Reference.Map.empty ~f:add_to_module_name_mapping
    |> Map.to_alist
    |> List.map ~f:make_unique_qualifiers
    |> List.concat
    |> Qualifier.Map.of_alist_exn
    |> Qualifier.Map.map ~f:(Module.from_project ~pyrefly_directory)


  let parse_module_info_files
      ~scheduler
      ~scheduler_policies
      ~pyrefly_directory
      ~qualifier_to_module_map
    =
    let handle = TypeOfExpressionsSharedMemory.create () in
    let scheduler_policy =
      Scheduler.Policy.from_configuration_or_default
        scheduler_policies
        Configuration.ScheduleIdentifier.ParsePyreflyModuleInfo
        ~default:
          (Scheduler.Policy.fixed_chunk_count
             ~minimum_chunks_per_worker:1
             ~minimum_chunk_size:1
             ~preferred_chunks_per_worker:1
             ())
    in
    let parse_module_info (qualifier, pyrefly_info_path) =
      let { ModuleInfoFile.type_of_expression; module_id = _; module_name = _; source_path = _ } =
        ModuleInfoFile.from_path_exn ~pyrefly_directory pyrefly_info_path
      in
      Map.iteri type_of_expression ~f:(fun ~key:location ~data:ty ->
          TypeOfExpressionsSharedMemory.add
            handle
            { TypeOfExpressionsSharedMemory.Key.qualifier; location }
            ty)
    in
    let inputs =
      qualifier_to_module_map
      |> Map.to_alist
      |> List.filter_map ~f:(fun (qualifier, { Module.pyrefly_info_path; _ }) ->
             match pyrefly_info_path with
             | Some pyrefly_info_path -> Some (qualifier, pyrefly_info_path)
             | None -> None)
    in
    let () =
      Scheduler.map_reduce
        scheduler
        ~policy:scheduler_policy
        ~initial:()
        ~map:(List.iter ~f:parse_module_info)
        ~reduce:(fun () () -> ())
        ~inputs
        ()
    in
    handle


  let write_module_infos_to_shared_memory ~qualifier_to_module_map =
    let timer = Timer.start () in
    let () = Log.info "Writing modules to shared memory..." in
    let handle = ModuleInfosSharedMemory.create () in
    let () =
      Map.to_alist qualifier_to_module_map
      |> List.iter ~f:(fun (qualifier, { Module.source_path; _ }) ->
             ModuleInfosSharedMemory.add
               handle
               qualifier
               { ModuleInfosSharedMemory.Module.source_path })
    in
    Log.info "Wrote modules to shared memory: %.3fs" (Timer.stop_in_sec timer);
    handle


  let parse_source_files
      ~scheduler
      ~scheduler_policies
      ~configuration
      ~module_infos_shared_memory
      ~qualifier_to_module_map
    =
    let timer = Timer.start () in
    let () = Log.info "Parsing source files..." in
    let handle = AstsSharedMemory.create () in
    let controls =
      Analysis.EnvironmentControls.create
        ~populate_call_graph:false
        ~string_annotation_preserve_location:false
        configuration
    in
    let parse_module qualifier =
      let parse_result =
        match ModuleInfosSharedMemory.get module_infos_shared_memory qualifier with
        | None -> failwith "unexpected: missing module info for qualifier"
        | Some { ModuleInfosSharedMemory.Module.source_path = None } -> OptionalSource.NoSource
        | Some { ModuleInfosSharedMemory.Module.source_path = Some source_path } ->
            let load_result =
              try Ok (ArtifactPath.raw source_path |> File.create |> File.content_exn) with
              | Sys_error error ->
                  Error
                    (Format.asprintf
                       "Cannot open file `%a` due to: %s"
                       ArtifactPath.pp
                       source_path
                       error)
            in
            let pyre1_module_path =
              Ast.ModulePath.create
                ~should_type_check:true
                {
                  Ast.ModulePath.Raw.relative = source_path |> ArtifactPath.raw |> PyrePath.absolute;
                  priority = 0;
                }
            in
            let parse_result =
              Analysis.Parsing.parse_result_of_load_result ~controls pyre1_module_path load_result
            in
            let () =
              match parse_result with
              | Ok _ -> ()
              | Error { Analysis.Parsing.ParseResult.Error.location; message; _ } ->
                  Log.error
                    "%a:%a: %s"
                    PyrePath.pp
                    (source_path |> ArtifactPath.raw)
                    Ast.Location.pp
                    location
                    message
            in
            OptionalSource.Some parse_result
      in
      AstsSharedMemory.add handle qualifier parse_result
    in
    let scheduler_policy =
      Scheduler.Policy.from_configuration_or_default
        scheduler_policies
        Configuration.ScheduleIdentifier.ParseSourceFiles
        ~default:
          (Scheduler.Policy.fixed_chunk_count
             ~minimum_chunks_per_worker:1
             ~minimum_chunk_size:1
             ~preferred_chunks_per_worker:1
             ())
    in
    let () =
      Scheduler.map_reduce
        scheduler
        ~policy:scheduler_policy
        ~initial:()
        ~map:(List.iter ~f:parse_module)
        ~reduce:(fun () () -> ())
        ~inputs:(Map.keys qualifier_to_module_map)
        ()
    in
    Log.info "Parsed source files: %.3fs" (Timer.stop_in_sec timer);
    Statistics.performance
      ~name:"Parsed source files"
      ~phase_name:"Parsing source files"
      ~command:"analyze"
      ~timer
      ();
    handle


  let create_from_directory
      ~scheduler
      ~scheduler_policies
      ~configuration
      ~decorator_configuration:_
      pyrefly_directory
    =
    let timer = Timer.start () in
    let () = Log.info "Parsing results from pyrefly..." in
    let project =
      ProjectFile.from_path_exn (PyrePath.append pyrefly_directory ~element:"pyrefly.pysa.json")
    in
    let qualifier_to_module_map = create_module_mapping ~pyrefly_directory project in
    let type_of_expressions_shared_memory =
      parse_module_info_files
        ~scheduler
        ~scheduler_policies
        ~pyrefly_directory
        ~qualifier_to_module_map
    in
    Log.info "Parsed results from pyrefly: %.3fs" (Timer.stop_in_sec timer);
    Statistics.performance
      ~name:"Parsed results from pyrefly"
      ~phase_name:"Parsing results from pyrefly"
      ~command:"analyze"
      ~timer
      ~integers:["modules", Map.length qualifier_to_module_map]
      ();

    let module_infos_shared_memory = write_module_infos_to_shared_memory ~qualifier_to_module_map in

    let qualifiers_with_source_shared_memory =
      let handle = QualifiersWithSourceSharedMemory.create () in
      let qualifiers =
        qualifier_to_module_map
        |> Map.to_alist
        |> List.filter ~f:(fun (_, { Module.source_path; _ }) -> Option.is_some source_path)
        |> List.map ~f:fst
      in
      let () = QualifiersWithSourceSharedMemory.add handle Memory.SingletonKey.key qualifiers in
      handle
    in

    let asts_shared_memory =
      parse_source_files
        ~scheduler
        ~scheduler_policies
        ~configuration
        ~module_infos_shared_memory
        ~qualifier_to_module_map
    in

    {
      qualifier_to_module_map;
      module_infos_shared_memory;
      qualifiers_with_source_shared_memory;
      asts_shared_memory;
      type_of_expressions_shared_memory;
    }


  (* TODO(T225700656): Remove this, this is to silence the unused value warning *)
  let unused
      {
        qualifier_to_module_map;
        module_infos_shared_memory;
        qualifiers_with_source_shared_memory;
        asts_shared_memory;
        type_of_expressions_shared_memory;
      }
    =
    let _ = Module.pp, Qualifier.show, Qualifier.pp in
    let _ =
      ( qualifier_to_module_map,
        module_infos_shared_memory,
        qualifiers_with_source_shared_memory,
        asts_shared_memory,
        type_of_expressions_shared_memory )
    in
    ()


  let _ = unused
end

module ReadOnly = struct
  type t = {
    module_infos_shared_memory: ModuleInfosSharedMemory.t;
    qualifiers_with_source_shared_memory: QualifiersWithSourceSharedMemory.t;
    asts_shared_memory: AstsSharedMemory.t;
  }

  let of_read_write_api
      {
        ReadWrite.module_infos_shared_memory;
        qualifiers_with_source_shared_memory;
        asts_shared_memory;
        _;
      }
    =
    { module_infos_shared_memory; qualifiers_with_source_shared_memory; asts_shared_memory }


  (* This is called when we expect the key to be present in shared memory *)
  let assert_shared_memory_key_exists message = function
    | Some value -> value
    | None -> failwith (Format.sprintf "unexpected: %s" message)


  let absolute_source_path_of_qualifier { module_infos_shared_memory; _ } qualifier =
    ModuleInfosSharedMemory.get module_infos_shared_memory (Qualifier.from_reference qualifier)
    |> assert_shared_memory_key_exists "missing module info for qualifier"
    |> fun { ModuleInfosSharedMemory.Module.source_path; _ } ->
    source_path
    (* TODO(T225700656): We currently return the artifact path, it should be translated back into a
       source path by buck *)
    >>| ArtifactPath.raw
    >>| PyrePath.absolute


  (* Return all qualifiers with source code *)
  let explicit_qualifiers { qualifiers_with_source_shared_memory; _ } =
    QualifiersWithSourceSharedMemory.get
      qualifiers_with_source_shared_memory
      Memory.SingletonKey.key
    |> assert_shared_memory_key_exists "missing qualifiers with source in shared memory"
    |> List.map ~f:Qualifier.to_reference


  let source_of_qualifier { asts_shared_memory; _ } qualifier =
    AstsSharedMemory.get asts_shared_memory (Qualifier.from_reference qualifier)
    |> assert_shared_memory_key_exists "missing source for qualifier"
    |> function
    | OptionalSource.Some (Result.Ok source) -> Some source
    | OptionalSource.Some (Result.Error _) -> None
    | OptionalSource.NoSource -> None
end
