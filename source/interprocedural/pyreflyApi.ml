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

(* Represents the unique identifier for a module (e.g specific module name + path) *)
module ModuleId : sig
  type t [@@deriving compare, sexp, hash, show]

  val from_int : int -> t
end = struct
  type t = int [@@deriving compare, sexp, hash, show]

  let from_int = Fn.id
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

module ModulesSharedMemory = struct
  module Module = struct
    type t = { source_path: ArtifactPath.t (* The path of source code as seen by the analyzer. *) }
  end

  include
    Hack_parallel.Std.SharedMemory.FirstClass.WithCache.Make
      (Analysis.SharedMemoryKeys.ReferenceKey)
      (struct
        type t = Module.t

        let prefix = Hack_parallel.Std.Prefix.make ()

        let description = "pyrefly modules"
      end)
end

module TypeOfExpressionsSharedMemory = struct
  module Key = struct
    type t = {
      module_name: Ast.Reference.t;
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

module AstsSharedMemory =
  Hack_parallel.Std.SharedMemory.FirstClass.NoCache.Make
    (Analysis.SharedMemoryKeys.ReferenceKey)
    (struct
      type t = Analysis.Parsing.ParseResult.t

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
      source_path: ArtifactPath.t; (* The path of source code as seen by the analyzer. *)
      pyrefly_info_path: ModuleInfoPath.t;
    }
    [@@deriving show]
  end

  type t = {
    modules: Module.t Ast.Reference.Map.t;
    modules_shared_memory: ModulesSharedMemory.t;
    asts_shared_memory: AstsSharedMemory.t;
    type_of_expressions_shared_memory: TypeOfExpressionsSharedMemory.t;
  }

  (* Build a mapping from module name (i.e, qualifier) to the module id, source path, etc. *)
  let create_module_mapping ~pyrefly_directory { ProjectFile.modules } =
    let handle_duplicate_module_names
        ({ Module.source_path = left_source_path; module_name; _ } as left)
        ({ Module.source_path = right_source_path; _ } as right)
      =
      let left_path_string = left_source_path |> ArtifactPath.raw |> PyrePath.absolute in
      let right_path_string = right_source_path |> ArtifactPath.raw |> PyrePath.absolute in
      (* Prefer .pyi files over .py files, for consistency with pyre1 *)
      if
        String.is_suffix ~suffix:".pyi" left_path_string
        && String.is_suffix ~suffix:".py" right_path_string
      then
        left
      else if
        String.is_suffix ~suffix:".py" left_path_string
        && String.is_suffix ~suffix:".pyi" right_path_string
      then
        right
      else (* TODO(T225700656): Handle source files with conflicting module name. *)
        let () =
          Log.warning
            "Ignoring source file `%s` because module `%a` already maps to `%s`"
            right_path_string
            Ast.Reference.pp
            module_name
            left_path_string
        in
        left
    in
    let add_module
        ~key:module_id
        ~data:{ ProjectFile.Module.module_name; module_path; info_path; _ }
        sofar
      =
      match ModulePath.artifact_file_path ~pyrefly_directory module_path with
      | None -> sofar
      | Some source_path ->
          (* Modules with an actual source path should have an associated info file *)
          let pyrefly_info_path =
            Option.value_exn ~message:"Missing pyrefly info path for module" info_path
          in
          let new_module = { Module.module_id; module_name; source_path; pyrefly_info_path } in
          Map.update sofar module_name ~f:(function
              | Some existing_module -> handle_duplicate_module_names existing_module new_module
              | None -> new_module)
    in
    Map.fold ~init:Ast.Reference.Map.empty ~f:add_module modules


  let parse_module_info_files ~scheduler ~scheduler_policies ~pyrefly_directory ~modules =
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
    let parse_module_info (module_name, pyrefly_info_path) =
      let { ModuleInfoFile.type_of_expression; module_id = _; module_name = _; source_path = _ } =
        ModuleInfoFile.from_path_exn ~pyrefly_directory pyrefly_info_path
      in
      Map.iteri type_of_expression ~f:(fun ~key:location ~data:ty ->
          TypeOfExpressionsSharedMemory.add
            handle
            { TypeOfExpressionsSharedMemory.Key.module_name; location }
            ty)
    in
    let inputs =
      modules
      |> Map.data
      |> List.map ~f:(fun { Module.module_name; pyrefly_info_path; _ } ->
             module_name, pyrefly_info_path)
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


  let write_modules_to_shared_memory ~modules =
    let timer = Timer.start () in
    let () = Log.info "Writing modules to shared memory..." in
    let handle = ModulesSharedMemory.create () in
    let () =
      Map.data modules
      |> List.iter ~f:(fun { Module.module_name; source_path; _ } ->
             ModulesSharedMemory.add handle module_name { ModulesSharedMemory.Module.source_path })
    in
    Log.info "Wrote modules to shared memory: %.3fs" (Timer.stop_in_sec timer);
    handle


  let parse_source_files
      ~scheduler
      ~scheduler_policies
      ~configuration
      ~modules_shared_memory
      ~modules
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
    let parse_module module_name =
      let { ModulesSharedMemory.Module.source_path; _ } =
        Option.value_exn (ModulesSharedMemory.get modules_shared_memory module_name)
      in
      let load_result =
        try Ok (ArtifactPath.raw source_path |> File.create |> File.content_exn) with
        | Sys_error error ->
            Error
              (Format.asprintf "Cannot open file `%a` due to: %s" ArtifactPath.pp source_path error)
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
      AstsSharedMemory.add handle module_name parse_result
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
        ~inputs:(Map.keys modules)
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
    let modules = create_module_mapping ~pyrefly_directory project in
    let type_of_expressions_shared_memory =
      parse_module_info_files ~scheduler ~scheduler_policies ~pyrefly_directory ~modules
    in
    Log.info "Parsed results from pyrefly: %.3fs" (Timer.stop_in_sec timer);
    Statistics.performance
      ~name:"Parsed results from pyrefly"
      ~phase_name:"Parsing results from pyrefly"
      ~command:"analyze"
      ~timer
      ~integers:["modules", Map.length modules]
      ();

    let modules_shared_memory = write_modules_to_shared_memory ~modules in

    let asts_shared_memory =
      parse_source_files
        ~scheduler
        ~scheduler_policies
        ~configuration
        ~modules_shared_memory
        ~modules
    in

    { modules; modules_shared_memory; asts_shared_memory; type_of_expressions_shared_memory }


  (* TODO(T225700656): Remove this, this is to silence the unused value warning *)
  let unused
      { modules; modules_shared_memory; asts_shared_memory; type_of_expressions_shared_memory }
    =
    let _ = modules, modules_shared_memory, asts_shared_memory, type_of_expressions_shared_memory in
    ()


  let _ = unused
end
