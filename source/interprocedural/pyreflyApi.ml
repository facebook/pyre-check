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

type t = Handle

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
  let read_json_file path =
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
    | Filesystem of PyrePath.t
    | Namespace of PyrePath.t
    | Memory of PyrePath.t
    | BundledTypeshed of PyrePath.t
  [@@deriving show]

  let from_json = function
    | `Assoc [("FileSystem", `String path)] -> Ok (Filesystem (PyrePath.create_absolute path))
    | `Assoc [("Namespace", `String path)] -> Ok (Namespace (PyrePath.create_absolute path))
    | `Assoc [("Memory", `String path)] -> Ok (Memory (PyrePath.create_absolute path))
    | `Assoc [("BundledTypeshed", `String path)] ->
        Ok (BundledTypeshed (PyrePath.create_absolute path))
    | json -> Error (FormatError.UnexpectedJsonType { json; message = "expected a module path" })
end

module ModuleId : sig
  type t [@@deriving compare, sexp, hash, show]

  val from_int : int -> t
end = struct
  type t = int [@@deriving compare, sexp, hash, show]

  let from_int = Fn.id
end

(* Represents the content of the `pyrefly.pysa.json` file. This must match the
   `pyrefly::report::pysa::PysaProjectFile` rust type. *)
module ProjectFile = struct
  module Module = struct
    type t = {
      module_id: ModuleId.t;
      module_name: Ast.Reference.t;
      module_path: ModulePath.t;
      info_path: PyrePath.t option;
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
        info_path = Option.map ~f:PyrePath.create_absolute info_path;
      }
  end

  module ModuleMap = Data_structures.SerializableMap.Make (ModuleId)

  type t = { modules: Module.t ModuleMap.t } [@@deriving show]

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
end

(* Represents information from pyrefly about a given module, stored as a `module:id.json` file. This
   must match the `pyrefly::report::pysa::PysaModuleFile` rust type. *)
module ModuleInfoFile = struct
  type t = {
    (* TODO(T225700656): module_id, module_name and source_path are already specified in the Project
       module. We should probably remove those from the file format. *)
    module_id: ModuleId.t;
    module_name: Ast.Reference.t;
    source_path: ModulePath.t;
    type_of_expression: string Ast.Location.SerializableMap.t;
  }
  [@@deriving show]

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
      >>| Ast.Location.SerializableMap.of_alist_exn
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
end

let create_from_directory directory =
  let () = Log.info "Parsing results from pyrefly..." in
  let project =
    let project_file_path = PyrePath.append directory ~element:"pyrefly.pysa.json" in
    let () = Log.debug "Parsing pyrefly project file `%a`" PyrePath.pp project_file_path in
    let json = JsonUtil.read_json_file project_file_path in
    match ProjectFile.from_json json with
    | Ok project -> project
    | Error error ->
        raise (PyreflyFileFormatError { path = project_file_path; error = Error.FormatError error })
  in
  let () =
    ProjectFile.ModuleMap.iter
      (fun _ { ProjectFile.Module.module_id = _; module_name = _; info_path; _ } ->
        match info_path with
        | None -> ()
        | Some info_path ->
            let () = Log.dump "Parsing info path %a" PyrePath.pp info_path in
            let json = JsonUtil.read_json_file info_path in
            let module_info =
              match ModuleInfoFile.from_json json with
              | Ok module_info -> module_info
              | Error error ->
                  raise
                    (PyreflyFileFormatError { path = info_path; error = Error.FormatError error })
            in
            Log.dump "Parsed module info: %a" ModuleInfoFile.pp module_info)
      project.modules
  in
  (* TODO: Store information in shared memory. *)
  (* TODO: Parse AST and store them in shared memory. *)
  Handle
