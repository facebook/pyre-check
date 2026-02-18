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
open Ast
module Pyre1Api = Analysis.PyrePysaEnvironment
module PyreflyType = Pyre1Api.PyreflyType
module PysaType = Pyre1Api.PysaType
module AstResult = Pyre1Api.AstResult
module ScalarTypeProperties = Pyre1Api.ScalarTypeProperties
module FunctionParameter = Pyre1Api.ModelQueries.FunctionParameter
module FunctionParameters = Pyre1Api.ModelQueries.FunctionParameters
module FunctionSignature = Pyre1Api.ModelQueries.FunctionSignature
module AccessPath = Analysis.TaintAccessPath

module FormatError = struct
  type t =
    | UnexpectedJsonType of {
        json: Yojson.Safe.t;
        message: string;
      }
    | UnsupportedVersion of { version: int }
    | UnparsableString of string
  [@@deriving show]
end

module Error = struct
  type t =
    | InvalidJsonError of string
    | IOError of string
    | FormatError of FormatError.t
  [@@deriving show]
end

let fixup_location { Location.start; stop } =
  (* WARNING: Pysa uses 0-indexed column numbers while Pyrefly uses 1-indexed column numbers. *)
  let decrement_column { Location.line; column } = { Location.line; column = column - 1 } in
  { Location.start = decrement_column start; stop = decrement_column stop }


let parse_location location =
  match Location.from_string location with
  | Ok location -> Ok (fixup_location location)
  | Error error ->
      Error (FormatError.UnexpectedJsonType { json = `String location; message = error })


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


  let as_list = function
    | `List elements -> Ok elements
    | json -> Error (FormatError.UnexpectedJsonType { json; message = "expected a list" })


  let as_string = function
    | `String string -> Ok string
    | json -> Error (FormatError.UnexpectedJsonType { json; message = "expected a string" })


  let get_optional_string_member json key =
    match Yojson.Safe.Util.member key json with
    | `String value -> Ok (Some value)
    | `Null -> Ok None
    | _ ->
        Error
          (FormatError.UnexpectedJsonType
             { json; message = Format.sprintf "expected the key `%s` to contain a string" key })


  let get_optional_bool_member ~default json key =
    match Yojson.Safe.Util.member key json with
    | `Bool value -> Ok value
    | `Null -> Ok default
    | _ ->
        Error
          (FormatError.UnexpectedJsonType
             { json; message = Format.sprintf "expected the key `%s` to contain a boolean" key })


  let get_optional_list_member json key =
    match Yojson.Safe.Util.member key json with
    | `List elements -> Ok elements
    | `Null -> Ok []
    | _ ->
        Error
          (FormatError.UnexpectedJsonType
             { json; message = Format.sprintf "expected the key `%s` to contain a list" key })


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


  let get_optional_member json key =
    match Yojson.Safe.Util.member key json with
    | `Null -> None
    | json -> Some json


  let check_format_version ~expected json =
    let open Core.Result.Monad_infix in
    get_int_member json "format_version"
    >>= function
    | version when Int.equal version expected -> Ok ()
    | version -> Error (FormatError.UnsupportedVersion { version })


  let get_member json key =
    match Yojson.Safe.Util.member key json with
    | `Null ->
        Error
          (FormatError.UnexpectedJsonType
             { json; message = Format.sprintf "expected an object with key `%s`" key })
    | value -> Ok value


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


  let get_optional_object_member json key =
    match Yojson.Safe.Util.member key json with
    | `Assoc bindings -> Ok bindings
    | `Null -> Ok []
    | _ ->
        Error
          (FormatError.UnexpectedJsonType
             {
               json;
               message = Format.sprintf "expected an object with key `%s` containing an object" key;
             })


  let get_list_member json key =
    match Yojson.Safe.Util.member key json with
    | `List elements -> Ok elements
    | _ ->
        Error
          (FormatError.UnexpectedJsonType
             {
               json;
               message = Format.sprintf "expected an object with key `%s` containing a list" key;
             })


  let check_object = function
    | `Assoc bindings -> Ok bindings
    | json ->
        Error (FormatError.UnexpectedJsonType { json; message = "expected root to be an object" })
end

(* Path of a module, equivalent of the `pyrefly_python::module_path::ModulePathDetails` rust
   type. *)
module ModulePath = struct
  type t =
    | Filesystem of ArtifactPath.t
    | Namespace of PyrePath.t
    | Memory of PyrePath.t
    | BundledTypeshed of PyrePath.t
    | BundledTypeshedThirdParty of PyrePath.t
  [@@deriving compare, equal, show]

  let from_json = function
    | `Assoc [("FileSystem", `String path)] ->
        Ok (Filesystem (path |> PyrePath.create_absolute |> ArtifactPath.create))
    | `Assoc [("Namespace", `String path)] -> Ok (Namespace (PyrePath.create_absolute path))
    | `Assoc [("Memory", `String path)] -> Ok (Memory (PyrePath.create_absolute path))
    | `Assoc [("BundledTypeshed", `String path)] ->
        Ok (BundledTypeshed (PyrePath.create_absolute path))
    | `Assoc [("BundledTypeshedThirdParty", `String path)] ->
        Ok (BundledTypeshedThirdParty (PyrePath.create_absolute path))
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
    | BundledTypeshedThirdParty path ->
        Some
          (pyrefly_directory
          |> PyrePath.append ~element:"typeshed_third_party"
          |> PyrePath.append ~element:(PyrePath.absolute path)
          |> ArtifactPath.create)
end

(* Unique identifier for a module, assigned by pyrefly. This maps to a source file. *)
module ModuleId : sig
  type t [@@deriving compare, equal, sexp, hash, show]

  val from_int : int -> t

  val to_int : t -> int

  val max : t -> t -> t

  val increment : t -> t
end = struct
  type t = int [@@deriving compare, equal, sexp, hash, show]

  let from_int = Fn.id

  let to_int = Fn.id

  let max = Int.max

  let increment id = id + 1
end

(* Unique identifier for a class within a module, assigned by pyrefly. *)
module LocalClassId : sig
  type t [@@deriving compare, equal, sexp, hash, show]

  val from_int : int -> t

  val to_int : t -> int

  val of_string : string -> t
end = struct
  type t = int [@@deriving compare, equal, sexp, hash, show]

  let from_int = Fn.id

  let to_int = Fn.id

  let of_string = Int.of_string
end

(* Unique identifier for a class, assigned by pyrefly. *)
module GlobalClassId = struct
  (* TODO(T225700656): Potentially encode this in a single integer to save space. *)
  type t = {
    module_id: ModuleId.t;
    local_class_id: LocalClassId.t;
  }
  [@@deriving compare, equal, show]

  let from_json json =
    let open Core.Result.Monad_infix in
    JsonUtil.get_int_member json "module_id"
    >>= fun module_id ->
    JsonUtil.get_int_member json "class_id"
    >>| fun class_id ->
    { module_id = ModuleId.from_int module_id; local_class_id = LocalClassId.from_int class_id }


  let from_optional_json = function
    | Some json ->
        let open Core.Result.Monad_infix in
        json |> from_json >>| Option.some
    | None -> Ok None
end

module GlobalClassIdSharedMemoryKey = struct
  type t = GlobalClassId.t [@@deriving compare]

  let to_string { GlobalClassId.module_id; local_class_id } =
    Format.sprintf "%d|%d" (ModuleId.to_int module_id) (LocalClassId.to_int local_class_id)
end

(* Unique identifier for a function within a module, which needs to be consistent between here and
   the outputs of Pyrefly because the outputs often use this as the key to associate information
   with (e.g., call graphs). *)
module LocalFunctionId : sig
  type t =
    (* Function declared with a `def` statement. *)
    | Function of Location.t
    (* Implicit function containing all top level statement. *)
    | ModuleTopLevel
    (* Implicit function containing the class body. *)
    | ClassTopLevel of LocalClassId.t
    (* Function-like class field that is not a `def` statement. *)
    | ClassField of {
        class_id: LocalClassId.t;
        name: string;
      }
    (* Decorated target, which represents an artificial function containing all decorators of a
       function, inlined as an expression. For e.g, `@foo` on `def bar()` -> `return foo(bar)` *)
    | FunctionDecoratedTarget of Location.t
  [@@deriving compare, equal, show, sexp]

  val from_string : string -> (t, FormatError.t) result

  val create_function : Location.t -> t

  val is_class_field : t -> bool

  module Map : Map.S with type Key.t = t
end = struct
  module T = struct
    (* TODO(T225700656): Potentially use a TextRange (2 ints) instead of Location.t (4 ints) *)
    type t =
      | Function of Location.t
      | ModuleTopLevel
      | ClassTopLevel of LocalClassId.t
      | ClassField of {
          class_id: LocalClassId.t;
          name: string;
        }
      | FunctionDecoratedTarget of Location.t
    [@@deriving compare, equal, show, sexp]
  end

  include T

  let from_string string =
    let open Core.Result.Monad_infix in
    match String.lsplit2 string ~on:':' with
    | None when String.equal string "MTL" -> Ok ModuleTopLevel
    | Some ("F", location) -> parse_location location >>| fun location -> Function location
    | Some ("CTL", class_id) -> Ok (ClassTopLevel (LocalClassId.of_string class_id))
    | Some ("CF", class_field) -> (
        match String.lsplit2 class_field ~on:':' with
        | Some (class_id, name) ->
            Ok (ClassField { class_id = LocalClassId.of_string class_id; name })
        | None -> Error (FormatError.UnparsableString string))
    | Some ("FDT", location) ->
        parse_location location >>| fun location -> FunctionDecoratedTarget location
    | _ -> Error (FormatError.UnparsableString string)


  let create_function location = Function location

  let is_class_field = function
    | ClassField _ -> true
    | _ -> false


  module Map = Map.Make (T)
end

(* Unique identifier for a callable (function or method) *)
module GlobalCallableId = struct
  type t = {
    module_id: ModuleId.t;
    local_function_id: LocalFunctionId.t;
  }
  [@@deriving compare, equal, show]

  let _ = pp, LocalFunctionId.show

  let from_json json =
    let open Core.Result.Monad_infix in
    JsonUtil.get_int_member json "module_id"
    >>= fun module_id ->
    JsonUtil.get_string_member json "function_id"
    >>= LocalFunctionId.from_string
    >>| fun local_function_id -> { module_id = ModuleId.from_int module_id; local_function_id }


  let from_optional_json = function
    | Some json ->
        let open Core.Result.Monad_infix in
        from_json json >>| Option.some
    | None -> Ok None
end

module PyreflyTarget = struct
  type t =
    | Function of GlobalCallableId.t
    | AllOverrides of GlobalCallableId.t
    | OverrideSubset of {
        base_method: GlobalCallableId.t;
        subset: t list;
      }
    | FormatString
  [@@deriving compare, equal, show]

  let rec from_json json =
    let open Core.Result.Monad_infix in
    match json with
    | `Assoc [("Function", global_callable_id)] ->
        GlobalCallableId.from_json global_callable_id
        >>| fun global_callable_id -> Function global_callable_id
    | `Assoc [("AllOverrides", global_callable_id)] ->
        GlobalCallableId.from_json global_callable_id
        >>| fun global_callable_id -> AllOverrides global_callable_id
    | `Assoc [("OverrideSubset", override_subset)] ->
        JsonUtil.get_object_member override_subset "base_method"
        >>= (fun base_method -> GlobalCallableId.from_json (`Assoc base_method))
        >>= fun base_method ->
        JsonUtil.get_list_member override_subset "subset"
        >>| List.map ~f:from_json
        >>= Result.all
        >>| fun subset -> OverrideSubset { base_method; subset }
    | `String "FormatString" -> Ok FormatString
    | _ -> Error (FormatError.UnexpectedJsonType { json; message = "Unknown type of target" })
end

module ModuleIdSharedMemoryKey = struct
  type t = ModuleId.t [@@deriving compare]

  let to_string module_id = string_of_int (ModuleId.to_int module_id)
end

module GlobalCallableIdSharedMemoryKey = struct
  type t = GlobalCallableId.t [@@deriving compare]

  let to_string { GlobalCallableId.module_id; local_function_id } =
    Format.asprintf "%d|%a" (ModuleId.to_int module_id) LocalFunctionId.pp local_function_id
end

(* Unique identifier for a module, assigned by pysa. This maps to a specific source file. Note that
   this is converted into `Reference.t` during the taint analysis for backward compatibility with
   the old Pyre1 API, which assumes a module name can only map to one source file. *)
module ModuleQualifier : sig
  type t [@@deriving compare, equal, sexp, hash, show]

  val create : path:string option -> Reference.t -> t

  (* The taint analysis uses Reference.t to uniquely represent modules, so we use
     to_reference/from_reference to convert to that type *)
  val to_reference : t -> Reference.t

  (* This is marked `unchecked` because it doesn't actually validate that the reference is a valid
     module qualifier. *)
  val from_reference_unchecked : Reference.t -> t

  module Map : Map.S with type Key.t = t
end = struct
  module T = struct
    (* For now, this is stored as a reference internally, because Pyre1 uses references everywhere.
       It doesn't really make a lot of sense though, because the path might have dots. For instance:
       'a/b.py:a.b' will be stored as ['a/b', 'py:a', 'b']. *)
    type t = Reference.t [@@deriving compare, equal, sexp, hash, show]
  end

  include T

  let create ~path module_name =
    let () =
      (* Sanity check our naming scheme *)
      if
        List.exists (Reference.as_list module_name) ~f:(fun s ->
            String.contains s ':'
            || String.contains s '#'
            || String.contains s '$'
            || String.contains s '@')
      then
        failwith "unexpected: module name contains an invalid character (`:#$@`)"
    in
    match path with
    | None -> module_name
    | Some path -> Format.sprintf "%s:%s" path (Reference.show module_name) |> Reference.create


  let to_reference = Fn.id

  let from_reference_unchecked = Fn.id

  module Map = Map.Make (T)
end

module ModuleQualifierSharedMemoryKey = struct
  type t = ModuleQualifier.t [@@deriving compare]

  let to_string key =
    key |> ModuleQualifier.to_reference |> Analysis.SharedMemoryKeys.ReferenceKey.to_string
end

(* Filename for a pyrefly module information file. *)
module ModuleInfoFilename : sig
  type t [@@deriving compare, equal, sexp, hash, show]

  val create : string -> t

  val raw : t -> string
end = struct
  type t = string [@@deriving compare, equal, sexp, hash, show]

  let create = Fn.id

  let raw = Fn.id
end

(* Content of the `pyrefly.pysa.json` file exported by pyrefly. This matches the
   `pyrefly::report::pysa::PysaProjectFile` rust type. *)
module ProjectFile = struct
  module Module = struct
    type t = {
      module_id: ModuleId.t;
      module_name: Reference.t;
      absolute_source_path: ModulePath.t;
          (* Filesystem path to the source file for the module, as seen by the analyzer *)
      relative_source_path: string option; (* Relative path from a root or search path *)
      info_filename: ModuleInfoFilename.t option;
      is_test: bool;
      is_interface: bool;
      is_init: bool;
    }
    [@@deriving equal, show]

    let from_json json =
      let open Core.Result.Monad_infix in
      JsonUtil.get_int_member json "module_id"
      >>= fun module_id ->
      JsonUtil.get_string_member json "module_name"
      >>= fun module_name ->
      JsonUtil.get_optional_string_member json "info_filename"
      >>= fun info_filename ->
      JsonUtil.get_object_member json "source_path"
      >>= fun source_path ->
      ModulePath.from_json (`Assoc source_path)
      >>= fun absolute_source_path ->
      JsonUtil.get_optional_string_member json "relative_source_path"
      >>= fun relative_source_path ->
      JsonUtil.get_optional_bool_member ~default:false json "is_test"
      >>= fun is_test ->
      JsonUtil.get_optional_bool_member ~default:false json "is_interface"
      >>= fun is_interface ->
      JsonUtil.get_optional_bool_member ~default:false json "is_init"
      >>| fun is_init ->
      {
        module_id = ModuleId.from_int module_id;
        module_name = Reference.create module_name;
        absolute_source_path;
        relative_source_path;
        info_filename = Option.map ~f:ModuleInfoFilename.create info_filename;
        is_test;
        is_interface;
        is_init;
      }
  end

  module ModuleMap = Map.Make (ModuleId)

  type t = {
    modules: Module.t ModuleMap.t;
    builtin_module_id: ModuleId.t;
    object_class_id: LocalClassId.t;
    dict_class_id: LocalClassId.t;
    typing_module_id: ModuleId.t;
    typing_mapping_class_id: LocalClassId.t;
  }

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
    >>= fun _ ->
    JsonUtil.check_format_version ~expected:1 json
    >>= fun () ->
    JsonUtil.get_object_member json "modules"
    >>= parse_modules
    >>= fun modules ->
    JsonUtil.get_int_member json "builtin_module_id"
    >>= fun builtin_module_id ->
    JsonUtil.get_int_member json "object_class_id"
    >>= fun object_class_id ->
    JsonUtil.get_int_member json "dict_class_id"
    >>= fun dict_class_id ->
    JsonUtil.get_int_member json "typing_module_id"
    >>= fun typing_module_id ->
    JsonUtil.get_int_member json "typing_mapping_class_id"
    >>| fun typing_mapping_class_id ->
    {
      modules;
      builtin_module_id = ModuleId.from_int builtin_module_id;
      object_class_id = LocalClassId.from_int object_class_id;
      dict_class_id = LocalClassId.from_int dict_class_id;
      typing_module_id = ModuleId.from_int typing_module_id;
      typing_mapping_class_id = LocalClassId.from_int typing_mapping_class_id;
    }


  let from_path_exn path =
    let () = Log.debug "Parsing pyrefly project file `%a`" PyrePath.pp path in
    let json = JsonUtil.read_json_file_exn path in
    match from_json json with
    | Ok project -> project
    | Error error -> raise (PyreflyFileFormatError { path; error = Error.FormatError error })
end

module ClassWithModifiers = struct
  type t = {
    class_name: GlobalClassId.t;
    modifiers: Pyre1Api.TypeModifier.t list;
  }
  [@@deriving equal, show]

  let from_json json =
    let open Core.Result.Monad_infix in
    JsonUtil.get_object_member json "class"
    >>= fun class_name ->
    GlobalClassId.from_json (`Assoc class_name)
    >>= fun class_name ->
    JsonUtil.get_optional_list_member json "modifiers"
    >>| List.map ~f:JsonUtil.as_string
    >>= Result.all
    >>| List.map ~f:(fun modifier ->
            match Pyre1Api.TypeModifier.from_string modifier with
            | Some modifier -> Ok modifier
            | None ->
                Error
                  (FormatError.UnexpectedJsonType
                     { json = `String modifier; message = "expected a modifier" }))
    >>= Result.all
    >>| fun modifiers -> { class_name; modifiers }
end

module ClassNamesResult = struct
  type t = {
    classes: ClassWithModifiers.t list;
    is_exhaustive: bool;
  }
  [@@deriving equal, show]

  let from_json json =
    let open Core.Result.Monad_infix in
    JsonUtil.get_list_member json "classes"
    >>| List.map ~f:ClassWithModifiers.from_json
    >>= Result.all
    >>= fun classes ->
    JsonUtil.get_optional_bool_member ~default:false json "is_exhaustive"
    >>| fun is_exhaustive -> { classes; is_exhaustive }


  let from_optional_json = function
    | None -> Ok None
    | Some json -> from_json json |> Result.map ~f:Option.some
end

let parse_scalar_type_properties json =
  let open Core.Result.Monad_infix in
  JsonUtil.get_optional_bool_member ~default:false json "is_bool"
  >>= fun is_boolean ->
  JsonUtil.get_optional_bool_member ~default:false json "is_int"
  >>= fun is_integer ->
  JsonUtil.get_optional_bool_member ~default:false json "is_float"
  >>= fun is_float ->
  JsonUtil.get_optional_bool_member ~default:false json "is_enum"
  >>| fun is_enumeration ->
  ScalarTypeProperties.create
    ~is_boolean
    ~is_integer
      (* TODO(T225700656): pyre1 considers integers to be valid floats. We preserve that behavior
         for now. *)
    ~is_float:(is_float || is_integer)
    ~is_enumeration


module JsonType = struct
  type t = {
    string: string;
    scalar_properties: ScalarTypeProperties.t;
    class_names: ClassNamesResult.t option;
  }
  [@@deriving equal, show]

  let from_json json =
    let open Core.Result.Monad_infix in
    JsonUtil.get_string_member json "string"
    >>= fun string ->
    parse_scalar_type_properties json
    >>= fun scalar_properties ->
    JsonUtil.get_optional_member json "class_names"
    |> ClassNamesResult.from_optional_json
    >>| fun class_names -> { string; scalar_properties; class_names }


  let to_pysa_type { string; scalar_properties; class_names } =
    let class_names =
      match class_names with
      | Some { ClassNamesResult.classes; is_exhaustive } ->
          Some
            {
              PyreflyType.ClassNamesFromType.classes =
                List.map
                  ~f:
                    (fun {
                           ClassWithModifiers.class_name =
                             { GlobalClassId.module_id; local_class_id };
                           modifiers;
                         } ->
                    {
                      PyreflyType.ClassWithModifiers.module_id = ModuleId.to_int module_id;
                      class_id = LocalClassId.to_int local_class_id;
                      modifiers;
                    })
                  classes;
              is_exhaustive;
            }
      | None -> None
    in
    PysaType.from_pyrefly_type { PyreflyType.string; scalar_properties; class_names }


  let pysa_type_none =
    PysaType.from_pyrefly_type
      {
        PyreflyType.string = "None";
        scalar_properties = ScalarTypeProperties.none;
        class_names = None;
      }
end

module ClassFieldDeclarationKind = struct
  type t =
    | DeclaredByAnnotation
    | DeclaredWithoutAnnotation
    | AssignedInBody
    | DefinedWithoutAssign
    | DefinedInMethod
  [@@deriving equal, compare, show]

  let from_string = function
    | "DeclaredByAnnotation" -> Ok DeclaredByAnnotation
    | "DeclaredWithoutAnnotation" -> Ok DeclaredWithoutAnnotation
    | "AssignedInBody" -> Ok AssignedInBody
    | "DefinedWithoutAssign" -> Ok DefinedWithoutAssign
    | "DefinedInMethod" -> Ok DefinedInMethod
    | s ->
        Error
          (FormatError.UnexpectedJsonType
             { json = `String s; message = "expected declaration kind" })
end

module CapturedVariable = struct
  type t = {
    name: string;
    outer_function: GlobalCallableId.t;
  }
  [@@deriving equal, show]

  let from_json json =
    let open Core.Result.Monad_infix in
    JsonUtil.get_string_member json "name"
    >>= fun name ->
    JsonUtil.get_object_member json "outer_function"
    >>= fun outer_function ->
    GlobalCallableId.from_json (`Assoc outer_function)
    >>| fun outer_function -> { name; outer_function }
end

(* Information from pyrefly about all definitions in a given module, stored as a
   `<root>/definitions/<module>:<id>.json` file. This matches the
   `pyrefly::report::pysa::PysaModuleDefinitions` rust type. *)
module ModuleDefinitionsFile = struct
  module ParentScope = struct
    type t =
      | TopLevel
      | Class of Location.t
      | Function of Location.t
    [@@deriving equal, show]

    let from_json json =
      let open Core.Result.Monad_infix in
      match json with
      | `String "TopLevel" -> Ok TopLevel
      | `Assoc [("Class", `Assoc [("location", `String location_string)])] ->
          parse_location location_string >>| fun location -> Class location
      | `Assoc [("Function", `Assoc [("location", `String location_string)])] ->
          parse_location location_string >>| fun location -> Function location
      | _ -> Error (FormatError.UnexpectedJsonType { json; message = "expected parent_scope" })
  end

  module FunctionParameter = struct
    type t =
      | PosOnly of {
          name: string option;
          annotation: JsonType.t;
          required: bool;
        }
      | Pos of {
          name: string;
          annotation: JsonType.t;
          required: bool;
        }
      | VarArg of {
          name: string option;
          annotation: JsonType.t;
        }
      | KwOnly of {
          name: string;
          annotation: JsonType.t;
          required: bool;
        }
      | Kwargs of {
          name: string option;
          annotation: JsonType.t;
        }
    [@@deriving equal, show]

    let from_json json =
      let open Core.Result.Monad_infix in
      (match json with
      | `Assoc [(kind, json)] -> Ok (kind, json)
      | _ ->
          Error (FormatError.UnexpectedJsonType { json; message = "expected function parameter" }))
      >>= fun (kind, json) ->
      JsonUtil.get_object_member json "annotation"
      >>| (fun bindings -> `Assoc bindings)
      >>= JsonType.from_json
      >>= fun annotation ->
      JsonUtil.get_optional_bool_member ~default:false json "required"
      >>= fun required ->
      match kind with
      | "PosOnly" ->
          JsonUtil.get_optional_string_member json "name"
          >>| fun name -> PosOnly { name; annotation; required }
      | "Pos" ->
          JsonUtil.get_string_member json "name" >>| fun name -> Pos { name; annotation; required }
      | "VarArg" ->
          JsonUtil.get_optional_string_member json "name"
          >>| fun name -> VarArg { name; annotation }
      | "KwOnly" ->
          JsonUtil.get_string_member json "name"
          >>| fun name -> KwOnly { name; annotation; required }
      | "Kwargs" ->
          JsonUtil.get_optional_string_member json "name"
          >>| fun name -> Kwargs { name; annotation }
      | _ ->
          Error (FormatError.UnexpectedJsonType { json; message = "expected function parameter" })
  end

  module FunctionParameters = struct
    type t =
      | List of FunctionParameter.t list
      | Ellipsis
      | ParamSpec
    [@@deriving equal, show]

    let from_json json =
      let open Core.Result.Monad_infix in
      match json with
      | `Assoc [("List", `List parameters)] ->
          List.map ~f:FunctionParameter.from_json parameters
          |> Result.all
          >>| fun parameters -> List parameters
      | `String "Ellipsis" -> Ok Ellipsis
      | `String "ParamSpec" -> Ok ParamSpec
      | _ ->
          Error (FormatError.UnexpectedJsonType { json; message = "expected function parameters" })
  end

  module FunctionSignature = struct
    type t = {
      parameters: FunctionParameters.t;
      return_annotation: JsonType.t;
    }
    [@@deriving equal, show]

    let from_json json =
      let open Core.Result.Monad_infix in
      JsonUtil.get_member json "parameters"
      >>= FunctionParameters.from_json
      >>= fun parameters ->
      JsonUtil.get_object_member json "return_annotation"
      >>| (fun bindings -> `Assoc bindings)
      >>= JsonType.from_json
      >>| fun return_annotation -> { parameters; return_annotation }
  end

  let parse_decorator_callees bindings =
    let extract_global_callable_id_from_target_exn = function
      | PyreflyTarget.Function global_callable_id -> global_callable_id
      | target ->
          Format.asprintf "Unexpected type of decorator callee: `%a`" PyreflyTarget.pp target
          |> failwith
    in
    let open Core.Result.Monad_infix in
    let parse_binding (key, value) =
      parse_location key
      >>= fun location ->
      JsonUtil.as_list value
      >>| List.map ~f:(fun json ->
              json
              |> PyreflyTarget.from_json
              |> Result.map ~f:extract_global_callable_id_from_target_exn)
      >>= Result.all
      >>| fun callables -> location, callables
    in
    List.map ~f:parse_binding bindings |> Result.all >>| Location.SerializableMap.of_alist_exn


  module FunctionDefinition = struct
    type t = {
      name: string;
      local_function_id: LocalFunctionId.t;
      parent: ParentScope.t;
      undecorated_signatures: FunctionSignature.t list;
      captured_variables: CapturedVariable.t list;
      is_overload: bool;
      is_staticmethod: bool;
      is_classmethod: bool;
      is_property_getter: bool;
      is_property_setter: bool;
      is_stub: bool;
      is_def_statement: bool;
      is_toplevel: bool;
      is_class_toplevel: bool;
      overridden_base_method: GlobalCallableId.t option;
      defining_class: GlobalClassId.t option;
      decorator_callees: GlobalCallableId.t list Location.SerializableMap.t;
    }
    [@@deriving equal, show]

    let from_json ~local_function_id json =
      let open Core.Result.Monad_infix in
      JsonUtil.get_string_member json "name"
      >>= fun name ->
      ParentScope.from_json (Yojson.Safe.Util.member "parent" json)
      >>= fun parent ->
      JsonUtil.get_list_member json "undecorated_signatures"
      >>| List.map ~f:FunctionSignature.from_json
      >>= Result.all
      >>= fun undecorated_signatures ->
      JsonUtil.get_optional_list_member json "captured_variables"
      >>| List.map ~f:CapturedVariable.from_json
      >>= Result.all
      >>= fun captured_variables ->
      JsonUtil.get_optional_bool_member ~default:false json "is_overload"
      >>= fun is_overload ->
      JsonUtil.get_optional_bool_member ~default:false json "is_staticmethod"
      >>= fun is_staticmethod ->
      JsonUtil.get_optional_bool_member ~default:false json "is_classmethod"
      >>= fun is_classmethod ->
      JsonUtil.get_optional_bool_member ~default:false json "is_property_getter"
      >>= fun is_property_getter ->
      JsonUtil.get_optional_bool_member ~default:false json "is_property_setter"
      >>= fun is_property_setter ->
      JsonUtil.get_optional_bool_member ~default:false json "is_stub"
      >>= fun is_stub ->
      JsonUtil.get_optional_bool_member ~default:true json "is_def_statement"
      >>= fun is_def_statement ->
      JsonUtil.get_optional_member json "overridden_base_method"
      |> GlobalCallableId.from_optional_json
      >>= fun overridden_base_method ->
      JsonUtil.get_optional_member json "defining_class"
      |> GlobalClassId.from_optional_json
      >>= fun defining_class ->
      JsonUtil.get_optional_object_member json "decorator_callees"
      >>= parse_decorator_callees
      >>| fun decorator_callees ->
      {
        name;
        local_function_id;
        parent;
        undecorated_signatures;
        captured_variables;
        is_overload;
        is_staticmethod;
        is_classmethod;
        is_property_getter;
        is_property_setter;
        is_stub;
        is_def_statement;
        is_toplevel = false;
        is_class_toplevel = false;
        overridden_base_method;
        defining_class;
        decorator_callees;
      }


    let create_module_toplevel () =
      {
        name = Ast.Statement.toplevel_define_name;
        local_function_id = LocalFunctionId.ModuleTopLevel;
        parent = ParentScope.TopLevel;
        undecorated_signatures =
          [
            {
              FunctionSignature.parameters = FunctionParameters.List [];
              return_annotation =
                {
                  JsonType.string = "None";
                  scalar_properties = ScalarTypeProperties.none;
                  class_names = None;
                };
            };
          ];
        captured_variables = [];
        is_overload = false;
        is_staticmethod = false;
        is_classmethod = false;
        is_property_getter = false;
        is_property_setter = false;
        is_stub = false;
        is_def_statement = false;
        is_toplevel = true;
        is_class_toplevel = false;
        overridden_base_method = None;
        defining_class = None;
        decorator_callees = Location.SerializableMap.empty;
      }


    let create_class_toplevel ~name_location ~local_class_id =
      {
        name = Ast.Statement.class_toplevel_define_name;
        local_function_id = LocalFunctionId.ClassTopLevel local_class_id;
        parent = ParentScope.Class name_location;
        undecorated_signatures =
          [
            {
              FunctionSignature.parameters = FunctionParameters.List [];
              return_annotation =
                {
                  JsonType.string = "None";
                  scalar_properties = ScalarTypeProperties.none;
                  class_names = None;
                };
            };
          ];
        captured_variables = [];
        is_overload = false;
        is_staticmethod = false;
        is_classmethod = false;
        is_property_getter = false;
        is_property_setter = false;
        is_stub = false;
        is_def_statement = false;
        is_toplevel = false;
        is_class_toplevel = true;
        overridden_base_method = None;
        defining_class = None;
        decorator_callees = Location.SerializableMap.empty;
      }
  end

  module ClassMro = struct
    type t =
      | Resolved of GlobalClassId.t list
      | Cyclic
    [@@deriving equal, show]

    let from_json = function
      | `Assoc [("Resolved", `List classes)] ->
          let open Core.Result.Monad_infix in
          classes
          |> List.map ~f:GlobalClassId.from_json
          |> Result.all
          >>| fun classes -> Resolved classes
      | `String "Cyclic" -> Ok Cyclic
      | json -> Error (FormatError.UnexpectedJsonType { json; message = "expected a class mro" })
  end

  module JsonClassField = struct
    type t = {
      name: string;
      type_: JsonType.t;
      explicit_annotation: string option;
      location: Location.t option;
      declaration_kind: ClassFieldDeclarationKind.t option;
    }
    [@@deriving equal, show]

    let from_json ~name json =
      let open Core.Result.Monad_infix in
      JsonUtil.get_member json "type"
      >>= JsonType.from_json
      >>= fun type_ ->
      JsonUtil.get_optional_string_member json "explicit_annotation"
      >>= fun explicit_annotation ->
      JsonUtil.get_optional_string_member json "location"
      >>= (function
            | Some location -> parse_location location >>| Option.some
            | None -> Ok None)
      >>= fun location ->
      JsonUtil.get_optional_string_member json "declaration_kind"
      >>= (function
            | Some declaration_kind ->
                ClassFieldDeclarationKind.from_string declaration_kind >>| Option.some
            | None -> Ok None)
      >>| fun declaration_kind -> { name; type_; explicit_annotation; location; declaration_kind }
  end

  module ClassDefinition = struct
    type t = {
      name: string;
      local_class_id: LocalClassId.t;
      name_location: Location.t;
      parent: ParentScope.t;
      bases: GlobalClassId.t list;
      mro: ClassMro.t;
      is_synthesized: bool;
      is_dataclass: bool;
      is_named_tuple: bool;
      is_typed_dict: bool;
      fields: JsonClassField.t list;
      decorator_callees: GlobalCallableId.t list Location.SerializableMap.t;
    }
    [@@deriving equal, show]

    let from_json ~name_location json =
      let open Core.Result.Monad_infix in
      JsonUtil.get_string_member json "name"
      >>= fun name ->
      ParentScope.from_json (Yojson.Safe.Util.member "parent" json)
      >>= fun parent ->
      JsonUtil.get_int_member json "class_id"
      >>= fun class_id ->
      JsonUtil.get_optional_list_member json "bases"
      >>| List.map ~f:GlobalClassId.from_json
      >>= Result.all
      >>= fun bases ->
      JsonUtil.get_member json "mro"
      >>= ClassMro.from_json
      >>= fun mro ->
      JsonUtil.get_optional_bool_member ~default:false json "is_synthesized"
      >>= fun is_synthesized ->
      JsonUtil.get_optional_bool_member ~default:false json "is_dataclass"
      >>= fun is_dataclass ->
      JsonUtil.get_optional_bool_member ~default:false json "is_named_tuple"
      >>= fun is_named_tuple ->
      JsonUtil.get_optional_bool_member ~default:false json "is_typed_dict"
      >>= fun is_typed_dict ->
      JsonUtil.get_optional_object_member json "fields"
      >>| List.map ~f:(fun (name, json) -> JsonClassField.from_json ~name json)
      >>= Result.all
      >>= fun fields ->
      JsonUtil.get_optional_object_member json "decorator_callees"
      >>= parse_decorator_callees
      >>| fun decorator_callees ->
      {
        name;
        local_class_id = LocalClassId.from_int class_id;
        name_location;
        parent;
        bases;
        mro;
        is_synthesized;
        is_dataclass;
        is_named_tuple;
        is_typed_dict;
        fields;
        decorator_callees;
      }
  end

  module JsonGlobalVariable = struct
    type t = {
      name: string;
      type_: JsonType.t option;
      location: Location.t;
    }

    let from_json ~name json =
      let open Core.Result.Monad_infix in
      JsonUtil.get_optional_member json "type"
      |> Option.map ~f:JsonType.from_json
      |> (function
           | Some result -> Result.map ~f:Option.some result
           | None -> Ok None)
      >>= fun type_ ->
      JsonUtil.get_string_member json "location"
      >>= parse_location
      >>| fun location -> { name; type_; location }
  end

  type t = {
    (* TODO(T225700656): module_name and source_path are already specified in the Project module. We
       should probably remove those from the file format. *)
    module_id: ModuleId.t;
    function_definitions: FunctionDefinition.t LocalFunctionId.Map.t;
    class_definitions: ClassDefinition.t Location.Map.t;
    global_variables: JsonGlobalVariable.t list;
  }

  let from_json json =
    let open Core.Result.Monad_infix in
    let parse_function_definitions function_definitions =
      function_definitions
      |> List.map ~f:(fun (local_function_id, function_definition) ->
             LocalFunctionId.from_string local_function_id
             >>= fun local_function_id ->
             FunctionDefinition.from_json ~local_function_id function_definition
             >>| fun function_definition -> local_function_id, function_definition)
      |> Result.all
      >>| LocalFunctionId.Map.of_alist_exn
    in
    let parse_class_definitions class_definitions =
      class_definitions
      |> List.map ~f:(fun (location, class_definition) ->
             parse_location location
             >>= fun location ->
             ClassDefinition.from_json ~name_location:location class_definition
             >>| fun class_definition -> location, class_definition)
      |> Result.all
      >>| Location.Map.of_alist_exn
    in
    let parse_global_variables global_variables =
      global_variables
      |> List.map ~f:(fun (name, json) -> JsonGlobalVariable.from_json ~name json)
      |> Result.all
    in
    JsonUtil.check_object json
    >>= fun _ ->
    JsonUtil.check_format_version ~expected:1 json
    >>= fun () ->
    JsonUtil.get_int_member json "module_id"
    >>= fun module_id ->
    JsonUtil.get_object_member json "function_definitions"
    >>= parse_function_definitions
    >>= fun function_definitions ->
    JsonUtil.get_object_member json "class_definitions"
    >>= parse_class_definitions
    >>= fun class_definitions ->
    JsonUtil.get_object_member json "global_variables"
    >>= parse_global_variables
    >>| fun global_variables ->
    {
      module_id = ModuleId.from_int module_id;
      function_definitions;
      class_definitions;
      global_variables;
    }


  let from_path_exn ~pyrefly_directory path =
    let path =
      pyrefly_directory
      |> PyrePath.append ~element:"definitions"
      |> PyrePath.append ~element:(ModuleInfoFilename.raw path)
    in
    let () = Log.debug "Parsing pyrefly module definitions file %a" PyrePath.pp path in
    let json = JsonUtil.read_json_file_exn path in
    match from_json json with
    | Ok module_info -> module_info
    | Error error -> raise (PyreflyFileFormatError { path; error = Error.FormatError error })
end

(* Information from pyrefly about type of expressions in a given module, stored as a
   `<root>/type_of_expressions/<module>:<id>.json` file. This matches the
   `pyrefly::report::pysa::PysaModuleTypeOfExpressions` rust type. *)
module ModuleTypeOfExpressions = struct
  type t = {
    (* TODO(T225700656): module_name and source_path are already specified in the Project module. We
       should probably remove those from the file format. *)
    module_id: ModuleId.t;
    type_of_expression: JsonType.t Location.Map.t;
  }

  let from_json json =
    let open Core.Result.Monad_infix in
    let parse_type_of_expression type_of_expression =
      type_of_expression
      |> List.map ~f:(fun (location, type_) ->
             parse_location location
             >>= fun location -> JsonType.from_json type_ >>| fun type_ -> location, type_)
      |> Result.all
      >>| Location.Map.of_alist_exn
    in
    JsonUtil.check_object json
    >>= fun _ ->
    JsonUtil.check_format_version ~expected:1 json
    >>= fun () ->
    JsonUtil.get_int_member json "module_id"
    >>= fun module_id ->
    JsonUtil.get_object_member json "type_of_expression"
    >>= parse_type_of_expression
    >>| fun type_of_expression -> { module_id = ModuleId.from_int module_id; type_of_expression }


  let from_path_exn ~pyrefly_directory path =
    let path =
      pyrefly_directory
      |> PyrePath.append ~element:"type_of_expressions"
      |> PyrePath.append ~element:(ModuleInfoFilename.raw path)
    in
    let () = Log.debug "Parsing pyrefly module type-of-expressions file %a" PyrePath.pp path in
    let json = JsonUtil.read_json_file_exn path in
    match from_json json with
    | Ok module_info -> module_info
    | Error error -> raise (PyreflyFileFormatError { path; error = Error.FormatError error })
end

(* Mapping from function id to to call graph information. This represents the
   `pyrefly::report::pysa::PysaModuleCallGraphs` rust type. *)
module ModuleCallGraphs = struct
  module JsonImplicitReceiver = struct
    type t =
      | TrueWithClassReceiver
      | TrueWithObjectReceiver
      | False

    let from_json = function
      | `String "TrueWithClassReceiver" -> Ok TrueWithClassReceiver
      | `String "TrueWithObjectReceiver" -> Ok TrueWithObjectReceiver
      | `String "False" -> Ok False
      | json ->
          Error (FormatError.UnexpectedJsonType { json; message = "expected implicit receiver" })


    let is_true = function
      | TrueWithClassReceiver -> true
      | TrueWithObjectReceiver -> true
      | False -> false
  end

  module JsonCallTarget = struct
    type t = {
      target: PyreflyTarget.t;
      implicit_receiver: JsonImplicitReceiver.t;
      implicit_dunder_call: bool;
      receiver_class: GlobalClassId.t option;
      is_class_method: bool;
      is_static_method: bool;
      return_type: ScalarTypeProperties.t;
    }

    let from_json json =
      let open Core.Result.Monad_infix in
      JsonUtil.get_member json "target"
      >>= PyreflyTarget.from_json
      >>= fun target ->
      (match JsonUtil.get_optional_member json "implicit_receiver" with
      | Some implicit_receiver_json -> JsonImplicitReceiver.from_json implicit_receiver_json
      | None -> Ok JsonImplicitReceiver.False)
      >>= fun implicit_receiver ->
      JsonUtil.get_optional_bool_member ~default:false json "implicit_dunder_call"
      >>= fun implicit_dunder_call ->
      (match JsonUtil.get_optional_member json "receiver_class" with
      | Some receiver_class -> GlobalClassId.from_json receiver_class >>| Option.some
      | None -> Ok None)
      >>= fun receiver_class ->
      JsonUtil.get_optional_bool_member ~default:false json "is_class_method"
      >>= fun is_class_method ->
      JsonUtil.get_optional_bool_member ~default:false json "is_static_method"
      >>= fun is_static_method ->
      (match JsonUtil.get_optional_member json "return_type" with
      | Some return_type -> parse_scalar_type_properties return_type
      | None -> Ok ScalarTypeProperties.none)
      >>| fun return_type ->
      {
        target;
        implicit_receiver;
        implicit_dunder_call;
        receiver_class;
        is_class_method;
        is_static_method;
        return_type;
      }
  end

  module JsonUnresolved = struct
    let from_json = function
      | `String "False" -> Ok CallGraph.Unresolved.False
      | `Assoc [("True", `String reason)] as json -> (
          match CallGraph.Unresolved.reason_from_string reason with
          | Some reason -> Ok (CallGraph.Unresolved.True reason)
          | None ->
              Error (FormatError.UnexpectedJsonType { json; message = "unknown unresolved reason" })
          )
      | json -> Error (FormatError.UnexpectedJsonType { json; message = "expected unresolved" })
  end

  module JsonHigherOrderParameter = struct
    type t = {
      index: int;
      call_targets: JsonCallTarget.t list;
      unresolved: CallGraph.Unresolved.t;
    }

    let from_json json =
      let open Core.Result.Monad_infix in
      let parse_call_target_list targets =
        targets |> List.map ~f:JsonCallTarget.from_json |> Result.all
      in
      JsonUtil.get_optional_list_member json "call_targets"
      >>= parse_call_target_list
      >>= fun call_targets ->
      JsonUtil.get_int_member json "index"
      >>= fun index ->
      JsonUtil.get_optional_member json "unresolved"
      |> (function
           | Some json -> JsonUnresolved.from_json json
           | None -> Ok CallGraph.Unresolved.False)
      >>| fun unresolved -> { index; call_targets; unresolved }
  end

  module JsonHigherOrderParameterMap = struct
    module Map = SerializableMap.Make (Int)

    type t = JsonHigherOrderParameter.t Map.t

    let empty = Map.empty

    let data = Map.data

    let from_json json =
      let open Core.Result.Monad_infix in
      json
      |> JsonUtil.check_object
      >>| List.map ~f:(fun (index, higher_order_parameter) ->
              let index = int_of_string index in
              JsonHigherOrderParameter.from_json higher_order_parameter
              >>| fun higher_order_parameter -> index, higher_order_parameter)
      >>= Result.all
      >>| Map.of_alist_exn
  end

  module JsonGlobalVariable = struct
    type t = {
      module_id: ModuleId.t;
      name: string;
    }

    let from_json json =
      let open Core.Result.Monad_infix in
      JsonUtil.get_int_member json "module_id"
      >>= fun module_id ->
      JsonUtil.get_string_member json "name"
      >>| fun name -> { module_id = ModuleId.from_int module_id; name }
  end

  module JsonCallCallees = struct
    type t = {
      call_targets: JsonCallTarget.t list;
      init_targets: JsonCallTarget.t list;
      new_targets: JsonCallTarget.t list;
      higher_order_parameters: JsonHigherOrderParameterMap.t;
      unresolved: CallGraph.Unresolved.t;
    }

    let from_json json =
      let open Core.Result.Monad_infix in
      let parse_call_target_list targets =
        targets |> List.map ~f:JsonCallTarget.from_json |> Result.all
      in
      JsonUtil.get_optional_list_member json "call_targets"
      >>= parse_call_target_list
      >>= fun call_targets ->
      JsonUtil.get_optional_list_member json "init_targets"
      >>= parse_call_target_list
      >>= fun init_targets ->
      JsonUtil.get_optional_list_member json "new_targets"
      >>= parse_call_target_list
      >>= fun new_targets ->
      JsonUtil.get_optional_member json "higher_order_parameters"
      |> (function
           | Some json -> JsonHigherOrderParameterMap.from_json json
           | None -> Ok JsonHigherOrderParameterMap.empty)
      >>= fun higher_order_parameters ->
      JsonUtil.get_optional_member json "unresolved"
      |> (function
           | Some json -> JsonUnresolved.from_json json
           | None -> Ok CallGraph.Unresolved.False)
      >>| fun unresolved ->
      { call_targets; init_targets; new_targets; higher_order_parameters; unresolved }
  end

  module JsonAttributeAccessCallees = struct
    type t = {
      if_called: JsonCallCallees.t;
      property_setters: JsonCallTarget.t list;
      property_getters: JsonCallTarget.t list;
      global_targets: JsonGlobalVariable.t list;
      is_attribute: bool;
    }

    let from_json json =
      let open Core.Result.Monad_infix in
      let parse_call_target_list targets =
        targets |> List.map ~f:JsonCallTarget.from_json |> Result.all
      in
      JsonUtil.get_member json "if_called"
      >>= JsonCallCallees.from_json
      >>= fun if_called ->
      JsonUtil.get_optional_list_member json "property_setters"
      >>= parse_call_target_list
      >>= fun property_setters ->
      JsonUtil.get_optional_list_member json "property_getters"
      >>= parse_call_target_list
      >>= fun property_getters ->
      JsonUtil.get_optional_list_member json "global_targets"
      >>| List.map ~f:JsonGlobalVariable.from_json
      >>= Result.all
      >>= fun global_targets ->
      JsonUtil.get_optional_bool_member ~default:false json "is_attribute"
      >>| fun is_attribute ->
      { if_called; property_setters; property_getters; global_targets; is_attribute }
  end

  module JsonIdentifierCallees = struct
    type t = {
      if_called: JsonCallCallees.t;
      global_targets: JsonGlobalVariable.t list;
      captured_variables: CapturedVariable.t list;
    }

    let from_json json =
      let open Core.Result.Monad_infix in
      JsonUtil.get_member json "if_called"
      >>= JsonCallCallees.from_json
      >>= fun if_called ->
      JsonUtil.get_optional_list_member json "global_targets"
      >>| List.map ~f:JsonGlobalVariable.from_json
      >>= Result.all
      >>= fun global_targets ->
      JsonUtil.get_optional_list_member json "captured_variables"
      >>| List.map ~f:CapturedVariable.from_json
      >>= Result.all
      >>| fun captured_variables -> { if_called; global_targets; captured_variables }
  end

  module JsonDefineCallees = struct
    type t = { define_targets: JsonCallTarget.t list }

    let from_json json =
      let open Core.Result.Monad_infix in
      let parse_call_target_list targets =
        targets |> List.map ~f:JsonCallTarget.from_json |> Result.all
      in
      JsonUtil.get_list_member json "define_targets"
      >>= parse_call_target_list
      >>| fun define_targets -> { define_targets }
  end

  module JsonFormatStringArtificialCallees = struct
    type t = { targets: JsonCallTarget.t list }

    let from_json json =
      let open Core.Result.Monad_infix in
      let parse_call_target_list targets =
        targets |> List.map ~f:JsonCallTarget.from_json |> Result.all
      in
      JsonUtil.get_list_member json "targets"
      >>= parse_call_target_list
      >>| fun targets -> { targets }
  end

  module JsonFormatStringStringifyCallees = struct
    type t = {
      targets: JsonCallTarget.t list;
      unresolved: CallGraph.Unresolved.t;
    }

    let from_json json =
      let open Core.Result.Monad_infix in
      let parse_call_target_list targets =
        targets |> List.map ~f:JsonCallTarget.from_json |> Result.all
      in
      JsonUtil.get_optional_list_member json "targets"
      >>= parse_call_target_list
      >>= fun targets ->
      JsonUtil.get_optional_member json "unresolved"
      |> (function
           | Some json -> JsonUnresolved.from_json json
           | None -> Ok CallGraph.Unresolved.False)
      >>| fun unresolved -> { targets; unresolved }
  end

  module JsonReturnShimCallees = struct
    type t = {
      targets: JsonCallTarget.t list;
      arguments: CallGraph.ReturnShimCallees.argument_mapping list;
    }

    let parse_argument_mapping = function
      | `String "ReturnExpression" -> Ok CallGraph.ReturnShimCallees.ReturnExpression
      | `String "ReturnExpressionElement" -> Ok CallGraph.ReturnShimCallees.ReturnExpressionElement
      | argument_mapping ->
          Error
            (FormatError.UnexpectedJsonType
               { json = argument_mapping; message = "Unknown argument mapping" })


    let from_json json =
      let open Core.Result.Monad_infix in
      let parse_list ~parse_element list = list |> List.map ~f:parse_element |> Result.all in
      JsonUtil.get_optional_list_member json "targets"
      >>= parse_list ~parse_element:JsonCallTarget.from_json
      >>= fun targets ->
      JsonUtil.get_optional_list_member json "arguments"
      >>= parse_list ~parse_element:parse_argument_mapping
      >>| fun arguments -> { targets; arguments }
  end

  module JsonExpressionCallees = struct
    type t =
      | Call of JsonCallCallees.t
      | Identifier of JsonIdentifierCallees.t
      | AttributeAccess of JsonAttributeAccessCallees.t
      | Define of JsonDefineCallees.t
      | FormatStringArtificial of JsonFormatStringArtificialCallees.t
      | FormatStringStringify of JsonFormatStringStringifyCallees.t
      | Return of JsonReturnShimCallees.t

    let from_json json =
      let open Core.Result.Monad_infix in
      match json with
      | `Assoc [("Call", call_callees)] ->
          JsonCallCallees.from_json call_callees >>| fun call_callees -> Call call_callees
      | `Assoc [("Identifier", identifier_callees)] ->
          JsonIdentifierCallees.from_json identifier_callees
          >>| fun identifier_callees -> Identifier identifier_callees
      | `Assoc [("AttributeAccess", attribute_access_callees)] ->
          JsonAttributeAccessCallees.from_json attribute_access_callees
          >>| fun attribute_access_callees -> AttributeAccess attribute_access_callees
      | `Assoc [("Define", define_callees)] ->
          JsonDefineCallees.from_json define_callees >>| fun define_callees -> Define define_callees
      | `Assoc [("FormatStringArtificial", format_string_callees)] ->
          JsonFormatStringArtificialCallees.from_json format_string_callees
          >>| fun format_string_callees -> FormatStringArtificial format_string_callees
      | `Assoc [("FormatStringStringify", format_string_callees)] ->
          JsonFormatStringStringifyCallees.from_json format_string_callees
          >>| fun format_string_callees -> FormatStringStringify format_string_callees
      | `Assoc [("Return", return_callees)] ->
          JsonReturnShimCallees.from_json return_callees
          >>| fun return_shim_callees -> Return return_shim_callees
      | _ ->
          Error (FormatError.UnexpectedJsonType { json; message = "expected expression callees" })
  end

  module JsonCallGraph = struct
    type t = JsonExpressionCallees.t ExpressionIdentifier.Map.t

    let from_json json =
      let open Core.Result.Monad_infix in
      json
      |> JsonUtil.check_object
      >>| List.map ~f:(fun (expression_identifier, expression_callees) ->
              ExpressionIdentifier.from_json_key expression_identifier
              |> Result.map_error ~f:(fun error ->
                     FormatError.UnexpectedJsonType
                       { json = `String expression_identifier; message = error })
              >>| ExpressionIdentifier.map_location ~f:fixup_location
              >>= fun expression_identifier ->
              JsonExpressionCallees.from_json expression_callees
              >>| fun expression_callees -> expression_identifier, expression_callees)
      >>= Result.all
      >>| ExpressionIdentifier.Map.of_alist_exn
  end

  type t = {
    module_id: ModuleId.t;
    call_graphs: JsonCallGraph.t LocalFunctionId.Map.t;
  }

  let from_json json =
    let open Core.Result.Monad_infix in
    let parse_call_graphs call_graphs =
      call_graphs
      |> List.map ~f:(fun (function_id, call_graph) ->
             LocalFunctionId.from_string function_id
             >>= fun function_id ->
             JsonCallGraph.from_json call_graph >>| fun call_graph -> function_id, call_graph)
      |> Result.all
      >>| LocalFunctionId.Map.of_alist_exn
    in
    JsonUtil.check_object json
    >>= fun _ ->
    JsonUtil.check_format_version ~expected:1 json
    >>= fun () ->
    JsonUtil.get_int_member json "module_id"
    >>= fun module_id ->
    JsonUtil.get_object_member json "call_graphs"
    >>= parse_call_graphs
    >>| fun call_graphs -> { module_id = ModuleId.from_int module_id; call_graphs }


  let from_path_exn ~pyrefly_directory path =
    let path =
      pyrefly_directory
      |> PyrePath.append ~element:"call_graphs"
      |> PyrePath.append ~element:(ModuleInfoFilename.raw path)
    in
    let () = Log.debug "Parsing pyrefly module call-graphs file %a" PyrePath.pp path in
    let json = JsonUtil.read_json_file_exn path in
    match from_json json with
    | Ok module_info -> module_info
    | Error error -> raise (PyreflyFileFormatError { path; error = Error.FormatError error })
end

(* Set of type errors parsed from pyrefly. This represents the
   `pyrefly::report::pysa::PysaTypeErrorsFile` rust type. *)
module TypeErrors = struct
  module JsonError = struct
    type t = {
      module_id: ModuleId.t;
      location: Location.t;
      kind: string;
      message: string;
    }

    let from_json json =
      let open Core.Result.Monad_infix in
      JsonUtil.get_int_member json "module_id"
      >>= fun module_id ->
      JsonUtil.get_string_member json "location"
      >>= parse_location
      >>= fun location ->
      JsonUtil.get_string_member json "kind"
      >>= fun kind ->
      JsonUtil.get_string_member json "message"
      >>| fun message -> { module_id = ModuleId.from_int module_id; location; kind; message }
  end

  type t = { errors: JsonError.t list }

  let from_json json =
    let open Core.Result.Monad_infix in
    JsonUtil.check_object json
    >>= fun _ ->
    JsonUtil.check_format_version ~expected:1 json
    >>= fun () ->
    JsonUtil.get_list_member json "errors"
    >>= fun errors -> List.map ~f:JsonError.from_json errors |> Result.all


  let from_path_exn ~pyrefly_directory =
    let path = pyrefly_directory |> PyrePath.append ~element:"errors.json" in
    let () = Log.debug "Parsing pyrefly type errors file %a" PyrePath.pp path in
    let json = JsonUtil.read_json_file_exn path in
    match from_json json with
    | Ok errors -> { errors }
    | Error error -> raise (PyreflyFileFormatError { path; error = Error.FormatError error })
end

(* Information about a module, stored in shared memory. *)
module ModuleInfosSharedMemory = struct
  module Module = struct
    type t = {
      module_id: ModuleId.t;
      absolute_source_path: ArtifactPath.t option;
          (* Filesystem path to the source file for the module, as seen by the analyzer *)
      relative_source_path: string option; (* Relative path from a root or search path *)
      pyrefly_info_filename: ModuleInfoFilename.t option;
      is_test: bool; (* Is this a test file? *)
      is_stub: bool; (* Is this a stub file (e.g, `a.pyi`)? *)
    }
  end

  include
    Hack_parallel.Std.SharedMemory.FirstClass.WithCache.Make
      (ModuleQualifierSharedMemoryKey)
      (struct
        type t = Module.t

        let prefix = Hack_parallel.Std.Prefix.make ()

        let description = "pyrefly module infos"
      end)
end

(* List of module qualifiers, stored in shared memory. *)
module QualifiersSharedMemory = struct
  module Value = struct
    type t = {
      module_qualifier: ModuleQualifier.t;
      has_source: bool;
      has_info: bool;
    }
  end

  include
    Hack_parallel.Std.SharedMemory.FirstClass.NoCache.Make
      (Memory.SingletonKey)
      (struct
        type t = Value.t list

        let prefix = Hack_parallel.Std.Prefix.make ()

        let description = "pyrefly all modules"
      end)
end

(* Type of expression at a given module qualifier and location, stored in shared memory. *)
module TypeOfExpressionsSharedMemory = struct
  module Key = struct
    type t = {
      module_qualifier: ModuleQualifier.t;
      location: Location.t;
    }
    [@@deriving compare, sexp]

    let to_string key = key |> sexp_of_t |> Core.Sexp.to_string
  end

  include
    Hack_parallel.Std.SharedMemory.FirstClass.WithCache.Make
      (Key)
      (struct
        type t = PysaType.t

        let prefix = Hack_parallel.Std.Prefix.make ()

        let description = "pyrefly type of expressions"
      end)
end

(* Unique identifier for a class or define (e.g, `def foo(): ..`) in a module.
 *
 * - In most cases, this will be a dotted path such as `my.module.MyClass.foo`.
 * - Property setters have a suffix `@setter` to be different from property getters.
 * - The code at the top level of a module is considered part of an implicit function called `$toplevel`.
 * - The code within a class scope is considered part of an implicit function called `$class_toplevel`.
 * - The name might have a suffix with an index such as `$1`, `$2` to differentiate multiple definitions
 *   with the same name.
 * - If the name of the class or define might clash with another definition in a different module, we will add a
 *   separator between the module name and local name, such as `my.module#MyClass.foo`. *)
module FullyQualifiedName : sig
  type t [@@deriving compare, equal, sexp, hash, show]

  val create
    :  module_qualifier:ModuleQualifier.t ->
    local_name:string list ->
    add_module_separator:bool ->
    t

  val create_module_toplevel : module_qualifier:ModuleQualifier.t -> t

  val create_class_toplevel : t -> t

  val to_reference : t -> Reference.t

  (* This is marked `unchecked` because it doesn't actually validate that the reference is a valid
     fully qualifide name. *)
  val from_reference_unchecked : Reference.t -> t

  val last : t -> string

  val prefix : t -> t option

  module Map : Map.S with type Key.t = t
end = struct
  module T = struct
    (* Same as ModuleQualifier, it is stored as a reference, but it doesn't really make sense
       either. *)
    type t = Reference.t [@@deriving compare, equal, sexp, hash, show]
  end

  include T

  let create ~module_qualifier ~local_name ~add_module_separator =
    let () =
      (* Sanity check *)
      if List.exists local_name ~f:(fun s -> String.contains s '#' || String.contains s ':') then
        failwith "unexpected: local name contains an invalid character (`:#`)"
    in
    if not add_module_separator then
      Reference.combine
        (ModuleQualifier.to_reference module_qualifier)
        (Reference.create_from_list local_name)
    else
      Format.asprintf
        "%a#%a"
        Reference.pp
        (ModuleQualifier.to_reference module_qualifier)
        Reference.pp
        (Reference.create_from_list local_name)
      |> Reference.create


  let create_module_toplevel ~module_qualifier =
    create
      ~module_qualifier
      ~local_name:[Ast.Statement.toplevel_define_name]
      ~add_module_separator:false


  let create_class_toplevel name =
    Reference.combine name (Reference.create_from_list [Ast.Statement.class_toplevel_define_name])


  let to_reference = Fn.id

  let from_reference_unchecked = Fn.id

  let last = Reference.last

  let prefix = Reference.prefix

  module Map = Map.Make (T)
end

module FullyQualifiedNameSharedMemoryKey = struct
  type t = FullyQualifiedName.t [@@deriving compare]

  let to_string key =
    key |> FullyQualifiedName.to_reference |> Analysis.SharedMemoryKeys.ReferenceKey.to_string
end

module NameLocation = struct
  type t =
    | DefineName of
        Location.t (* Location of the name AST node, i.e location of `foo` in `def foo():` *)
    | ModuleTopLevel
    | ClassName of
        Location.t (* Location of the class AST node, i.e location of `Foo` in `class Foo:` *)
    | UnknonwnForClassField
  [@@deriving show]
end

module CallableMetadata = struct
  type t = {
    (* TODO(T225700656): This should be ModuleQualifier.t, but it is a Reference.t because it is
       exposed publicly. *)
    module_qualifier: Reference.t;
    name_location: NameLocation.t;
    is_overload: bool;
    is_staticmethod: bool;
    is_classmethod: bool;
    is_property_getter: bool;
    is_property_setter: bool;
    is_toplevel: bool; (* Is this the body of a module? *)
    is_class_toplevel: bool; (* Is this the body of a class? *)
    is_stub: bool; (* Is this a stub definition, e.g `def foo(): ...` *)
    is_def_statement: bool; (* Is this associated with a `def ..` statement? *)
    parent_is_class: bool;
  }
  [@@deriving show]
end

let assert_shared_memory_key_exists message = function
  | Some value -> value
  | None -> failwith (Format.sprintf "unexpected: %s" (message ()))


let strip_invalid_locations =
  List.filter ~f:(fun { Node.location; _ } -> not (Location.equal Location.any location))


module CallableMetadataSharedMemory = struct
  module Value = struct
    type t = {
      metadata: CallableMetadata.t;
      local_function_id: LocalFunctionId.t;
      (* This is the original name, without the fully qualified suffixes like `$2` or `@setter`. *)
      name: string;
      overridden_base_method: GlobalCallableId.t option;
      defining_class: GlobalClassId.t option;
      (* The list of callees for each decorator *)
      decorator_callees: GlobalCallableId.t list Location.SerializableMap.t;
      captured_variables: CapturedVariable.t list;
    }

    let _unused_fields { name = _; defining_class = _; _ } = ()
  end

  include
    Hack_parallel.Std.SharedMemory.FirstClass.WithCache.Make
      (FullyQualifiedNameSharedMemoryKey)
      (struct
        type t = Value.t

        let prefix = Hack_parallel.Std.Prefix.make ()

        let description = "pyrefly callable metadata"
      end)
end

module ClassMetadataSharedMemory = struct
  module Metadata = struct
    type t = {
      module_qualifier: ModuleQualifier.t;
      (* Location of the name AST node, i.e location of `Foo` in `class Foo():` *)
      name_location: Location.t;
      local_class_id: LocalClassId.t;
      (* True if this class was synthesized (e.g., from namedtuple), false if from actual `class X:`
         statement *)
      is_synthesized: bool;
      is_dataclass: bool;
      is_named_tuple: bool;
      is_typed_dict: bool;
      (* For a given class, its list of immediate parents. Empty if the class has no parents (it is
         implicitly ['object']) *)
      parents: GlobalClassId.t list;
      (* For a given class, its resolved MRO (Method Resolution Order). *)
      mro: ModuleDefinitionsFile.ClassMro.t;
      (* The list of callees for each decorator *)
      decorator_callees: GlobalCallableId.t list Location.SerializableMap.t;
    }
    [@@deriving show]

    let _ = pp
  end

  include
    Hack_parallel.Std.SharedMemory.FirstClass.WithCache.Make
      (FullyQualifiedNameSharedMemoryKey)
      (struct
        type t = Metadata.t

        let prefix = Hack_parallel.Std.Prefix.make ()

        let description = "pyrefly class metadata"
      end)
end

module ModuleCallablesSharedMemory =
  Hack_parallel.Std.SharedMemory.FirstClass.NoCache.Make
    (ModuleQualifierSharedMemoryKey)
    (struct
      type t = FullyQualifiedName.t list

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = "pyrefly callables in module"
    end)

module ModuleClassesSharedMemory =
  Hack_parallel.Std.SharedMemory.FirstClass.NoCache.Make
    (ModuleQualifierSharedMemoryKey)
    (struct
      type t = FullyQualifiedName.t list

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = "pyrefly classes in module"
    end)

module GlobalVariable = struct
  type t = {
    type_: PysaType.t option;
    location: Location.t;
  }
  [@@deriving equal, compare, show]

  let _ = pp
end

module ModuleGlobalsSharedMemory =
  Hack_parallel.Std.SharedMemory.FirstClass.WithCache.Make
    (ModuleQualifierSharedMemoryKey)
    (struct
      type t = GlobalVariable.t SerializableStringMap.t

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = "pyrefly global variables in module"
    end)

module ModuleIdToQualifierSharedMemory = struct
  include
    Hack_parallel.Std.SharedMemory.FirstClass.WithCache.Make
      (ModuleIdSharedMemoryKey)
      (struct
        type t = ModuleQualifier.t

        let prefix = Hack_parallel.Std.Prefix.make ()

        let description = "pyrefly module id to module qualified"
      end)

  let get_module_qualifier handle module_id =
    module_id |> get handle |> assert_shared_memory_key_exists (fun () -> "unknown module id")
end

module ClassIdToQualifiedNameSharedMemory = struct
  include
    Hack_parallel.Std.SharedMemory.FirstClass.WithCache.Make
      (GlobalClassIdSharedMemoryKey)
      (struct
        type t = FullyQualifiedName.t

        let prefix = Hack_parallel.Std.Prefix.make ()

        let description = "pyrefly class id to fully qualified name"
      end)

  let get_class_name handle class_id =
    class_id |> get handle |> assert_shared_memory_key_exists (fun () -> "unknown class id")
end

module CallableIdToQualifiedNameSharedMemory = struct
  include
    Hack_parallel.Std.SharedMemory.FirstClass.WithCache.Make
      (GlobalCallableIdSharedMemoryKey)
      (struct
        type t = FullyQualifiedName.t

        let prefix = Hack_parallel.Std.Prefix.make ()

        let description = "pyrefly callable id to fully qualified name"
      end)

  let get_opt = get

  let get handle id =
    get_opt handle id |> assert_shared_memory_key_exists (fun () -> "unknown callable id")
end

module ClassField = struct
  type t = {
    type_: PysaType.t;
    explicit_annotation: string option;
    location: Location.t option;
    declaration_kind: ClassFieldDeclarationKind.t option;
  }
  [@@deriving equal, compare, show]

  let _ = pp
end

module ClassFieldsSharedMemory =
  Hack_parallel.Std.SharedMemory.FirstClass.WithCache.Make
    (FullyQualifiedNameSharedMemoryKey)
    (struct
      type t = ClassField.t SerializableStringMap.t

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = "pyrefly class fields"
    end)

module PysaClassSummary = struct
  type t = {
    class_name: FullyQualifiedName.t;
    metadata: ClassMetadataSharedMemory.Metadata.t;
  }
end

module CallableAstSharedMemory =
  Hack_parallel.Std.SharedMemory.FirstClass.NoCache.Make
    (FullyQualifiedNameSharedMemoryKey)
    (struct
      type t = Statement.Define.t Ast.Node.t AstResult.t

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = "pyrefly ast of callables"
    end)

(* Define signature of each callable, resulting from parsing the source file. *)
module CallableDefineSignatureSharedMemory =
  Hack_parallel.Std.SharedMemory.FirstClass.NoCache.Make
    (FullyQualifiedNameSharedMemoryKey)
    (struct
      type t = Statement.Define.Signature.t Ast.Node.t AstResult.t

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = "pyrefly define signature of callables"
    end)

(* Undecorated signatures of each callable, provided by pyrefly. *)
module CallableUndecoratedSignaturesSharedMemory =
  Hack_parallel.Std.SharedMemory.FirstClass.NoCache.Make
    (FullyQualifiedNameSharedMemoryKey)
    (struct
      type t = FunctionSignature.t list

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = "pyrefly undecorated signatures of callables"
    end)

module ClassDecoratorsSharedMemory =
  Hack_parallel.Std.SharedMemory.FirstClass.NoCache.Make
    (FullyQualifiedNameSharedMemoryKey)
    (struct
      type t = Ast.Expression.t list AstResult.t

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = "pyrefly class decorators"
    end)

(* API handle stored in the main process. The type `t` should not be sent to workers, since it's
   expensive to copy. *)
module ReadWrite = struct
  module Module = struct
    type t = {
      module_id: ModuleId.t;
      module_name: Reference.t;
      absolute_source_path: ArtifactPath.t option;
          (* Filesystem path to the source file for the module, as seen by the analyzer *)
      relative_source_path: string option; (* Relative path from a root or search path *)
      pyrefly_info_filename: ModuleInfoFilename.t option;
      is_test: bool;
      is_stub: bool;
    }
    [@@deriving compare, equal, show]

    let from_project
        ~pyrefly_directory
        {
          ProjectFile.Module.module_id;
          module_name;
          absolute_source_path;
          relative_source_path;
          info_filename;
          is_test;
          is_interface;
          _;
        }
      =
      {
        module_id;
        module_name;
        absolute_source_path = ModulePath.artifact_file_path ~pyrefly_directory absolute_source_path;
        relative_source_path;
        pyrefly_info_filename = info_filename;
        is_test;
        is_stub = is_interface;
      }
  end

  type t = {
    pyrefly_directory: PyrePath.t;
    qualifier_to_module_map: Module.t ModuleQualifier.Map.t;
    module_infos_shared_memory: ModuleInfosSharedMemory.t;
    qualifiers_shared_memory: QualifiersSharedMemory.t;
    type_of_expressions_shared_memory: TypeOfExpressionsSharedMemory.t option;
    callable_metadata_shared_memory: CallableMetadataSharedMemory.t;
    class_metadata_shared_memory: ClassMetadataSharedMemory.t;
    class_fields_shared_memory: ClassFieldsSharedMemory.t;
    class_decorators_shared_memory: ClassDecoratorsSharedMemory.t;
    module_callables_shared_memory: ModuleCallablesSharedMemory.t;
    module_classes_shared_memory: ModuleClassesSharedMemory.t;
    module_globals_shared_memory: ModuleGlobalsSharedMemory.t;
    module_id_to_qualifier_shared_memory: ModuleIdToQualifierSharedMemory.t;
    class_id_to_qualified_name_shared_memory: ClassIdToQualifiedNameSharedMemory.t;
    callable_id_to_qualified_name_shared_memory: CallableIdToQualifiedNameSharedMemory.t;
    callable_ast_shared_memory: CallableAstSharedMemory.t;
    callable_define_signature_shared_memory: CallableDefineSignatureSharedMemory.t;
    callable_undecorated_signatures_shared_memory: CallableUndecoratedSignaturesSharedMemory.t;
    object_class: FullyQualifiedName.t;
    dict_class_id: GlobalClassId.t;
    typing_mapping_class_id: GlobalClassId.t;
  }

  (* Build a mapping from unique module qualifiers (module name + path prefix) to module. *)
  let create_module_qualifiers ~pyrefly_directory ~add_toplevel_modules modules =
    let make_unique_qualifiers ~key:module_name ~data:modules =
      match modules with
      | [module_info] ->
          [
            ( ModuleQualifier.create ~path:None module_name,
              Module.from_project ~pyrefly_directory module_info );
          ]
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
            | ModulePath.BundledTypeshedThirdParty path ->
                "typeshed-third-party:/" :: pyre_path_elements path
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
          |> List.map ~f:(fun ({ ProjectFile.Module.absolute_source_path; _ } as module_info) ->
                 List.rev (module_path_elements absolute_source_path), module_info)
          |> find_shortest_unique_prefix ~prefix_length:1
          |> List.map ~f:(fun (path, module_info) ->
                 ( ModuleQualifier.create ~path:(Some path) module_name,
                   Module.from_project ~pyrefly_directory module_info ))
    in
    let add_to_module_name_mapping sofar ({ ProjectFile.Module.module_name; _ } as module_info) =
      Map.update sofar module_name ~f:(function
          | None -> [module_info]
          | Some existing -> module_info :: existing)
    in
    (* For every module `a.b.c`, make sure that the module `a` exists. If not, then create an
       implicit empty module `a`. *)
    let add_implicit_top_level_modules qualifier_to_module_map =
      let last_module_id =
        qualifier_to_module_map
        |> Map.data
        |> List.concat
        |> List.fold ~init:(ModuleId.from_int 0) ~f:(fun sofar (_, { Module.module_id; _ }) ->
               ModuleId.max sofar module_id)
      in
      let add_implicit_module ((qualifier_to_module_map, last_module_id) as sofar) module_name =
        if Reference.length module_name >= 2 then
          let module_head = Option.value_exn (Reference.head module_name) in
          if not (Map.mem qualifier_to_module_map module_head) then
            let last_module_id = ModuleId.increment last_module_id in
            let qualifier_to_module_map =
              Map.add_exn
                qualifier_to_module_map
                ~key:module_head
                ~data:
                  [
                    ( ModuleQualifier.create ~path:None module_head,
                      {
                        Module.module_id = last_module_id;
                        module_name = module_head;
                        absolute_source_path = None;
                        relative_source_path = None;
                        pyrefly_info_filename = None;
                        is_test = false;
                        is_stub = false;
                      } );
                  ]
            in
            qualifier_to_module_map, last_module_id
          else
            sofar
        else
          sofar
      in
      List.fold
        ~init:(qualifier_to_module_map, last_module_id)
        ~f:add_implicit_module
        (Map.keys qualifier_to_module_map)
      |> fst
    in
    modules
    |> List.fold ~init:Reference.Map.empty ~f:add_to_module_name_mapping
    |> Map.mapi ~f:make_unique_qualifiers
    |> (if add_toplevel_modules then add_implicit_top_level_modules else Fn.id)
    |> Map.data
    |> List.concat
    |> ModuleQualifier.Map.of_alist_exn


  let parse_modules ~pyrefly_directory =
    let timer = Timer.start () in
    let () = Log.info "Parsing module list from pyrefly..." in
    let {
      ProjectFile.modules;
      builtin_module_id;
      object_class_id;
      dict_class_id;
      typing_module_id;
      typing_mapping_class_id;
    }
      =
      ProjectFile.from_path_exn (PyrePath.append pyrefly_directory ~element:"pyrefly.pysa.json")
    in
    let qualifier_to_module_map =
      create_module_qualifiers ~pyrefly_directory ~add_toplevel_modules:true (Map.data modules)
    in
    let object_class_id =
      { GlobalClassId.module_id = builtin_module_id; local_class_id = object_class_id }
    in
    let dict_class_id =
      { GlobalClassId.module_id = builtin_module_id; local_class_id = dict_class_id }
    in
    let typing_mapping_class_id =
      { GlobalClassId.module_id = typing_module_id; local_class_id = typing_mapping_class_id }
    in
    Log.info "Parsed module list from pyrefly: %.3fs" (Timer.stop_in_sec timer);
    Statistics.performance
      ~name:"Parsed module list from pyrefly"
      ~phase_name:"Parsing module list from pyrefly"
      ~timer
      ~integers:["modules", Map.length qualifier_to_module_map]
      ();
    qualifier_to_module_map, object_class_id, dict_class_id, typing_mapping_class_id


  let write_module_infos_to_shared_memory ~qualifier_to_module_map =
    let timer = Timer.start () in
    let () = Log.info "Writing modules to shared memory..." in
    let module_infos_shared_memory = ModuleInfosSharedMemory.create () in
    let module_id_to_qualifier_shared_memory = ModuleIdToQualifierSharedMemory.create () in
    let () =
      Map.to_alist qualifier_to_module_map
      |> List.iter
           ~f:(fun
                ( qualifier,
                  {
                    Module.module_id;
                    absolute_source_path;
                    relative_source_path;
                    pyrefly_info_filename;
                    is_test;
                    is_stub;
                    _;
                  } )
              ->
             ModuleInfosSharedMemory.add
               module_infos_shared_memory
               qualifier
               {
                 ModuleInfosSharedMemory.Module.module_id;
                 absolute_source_path;
                 relative_source_path;
                 pyrefly_info_filename;
                 is_test;
                 is_stub;
               };
             ModuleIdToQualifierSharedMemory.add
               module_id_to_qualifier_shared_memory
               module_id
               qualifier)
    in
    Log.info "Wrote modules to shared memory: %.3fs" (Timer.stop_in_sec timer);
    module_infos_shared_memory, module_id_to_qualifier_shared_memory


  let parse_source_files
      ~scheduler
      ~scheduler_policies
      ~configuration
      ~module_infos_shared_memory
      ~qualifier_to_module_map
      ~module_callables_shared_memory
      ~module_classes_shared_memory
      ~callable_metadata_shared_memory
      ~class_metadata_shared_memory
    =
    let timer = Timer.start () in
    let () = Log.info "Parsing source files..." in
    let callable_ast_shared_memory = CallableAstSharedMemory.create () in
    let callable_define_signature_shared_memory = CallableDefineSignatureSharedMemory.create () in
    let class_decorators_shared_memory = ClassDecoratorsSharedMemory.create () in
    let controls =
      Analysis.EnvironmentControls.create
        ~populate_call_graph:false
        ~string_annotation_preserve_location:false
        configuration
    in
    let store_callable_asts callables define_result =
      List.iter callables ~f:(fun callable ->
          CallableAstSharedMemory.add callable_ast_shared_memory callable define_result;
          let signature_result =
            AstResult.map_node ~f:(fun { Statement.Define.signature; _ } -> signature) define_result
          in
          CallableDefineSignatureSharedMemory.add
            callable_define_signature_shared_memory
            callable
            signature_result;
          ())
    in
    let store_class_decorators classes decorator_result =
      List.iter classes ~f:(fun class_name ->
          ClassDecoratorsSharedMemory.add class_decorators_shared_memory class_name decorator_result)
    in
    let collect_callable_asts_from_source ~qualifier ~callables ~source =
      let callables_with_locations, callables_without_locations =
        List.partition_map
          ~f:(fun callable ->
            let {
              CallableMetadataSharedMemory.Value.metadata = { name_location; _ };
              local_function_id;
              _;
            }
              =
              CallableMetadataSharedMemory.get callable_metadata_shared_memory callable
              |> assert_shared_memory_key_exists (fun () ->
                     Format.asprintf
                       "missing callable metadata: `%a`"
                       FullyQualifiedName.pp
                       callable)
            in
            match local_function_id, name_location with
            | LocalFunctionId.Function _, NameLocation.DefineName location ->
                Either.First (location, callable)
            | LocalFunctionId.ModuleTopLevel, _ -> Either.Second (local_function_id, callable)
            | LocalFunctionId.ClassTopLevel _, NameLocation.ClassName location ->
                Either.First (location, callable)
            | LocalFunctionId.ClassField _, NameLocation.UnknonwnForClassField ->
                Either.Second (local_function_id, callable)
            | _ -> failwith "unreachable")
          callables
      in
      let location_to_callable = Location.Map.of_alist_exn callables_with_locations in
      let id_to_callable = LocalFunctionId.Map.of_alist_exn callables_without_locations in
      (* For a given `def ..` statement, we need to find the matching definition provided by
         Pyrefly. *)
      let find_define_callable
          (location_to_callable, id_to_callable, callable_to_define)
          ({
             Node.value = { Statement.Define.signature = { name; parameters; _ }; body; _ };
             location = { Location.start = define_start; stop = define_stop } as define_location;
           } as define)
        =
        if Reference.equal name (Reference.create_from_list [Statement.toplevel_define_name]) then
          let callable = Map.find_exn id_to_callable LocalFunctionId.ModuleTopLevel in
          let id_to_callable = Map.remove id_to_callable LocalFunctionId.ModuleTopLevel in
          let callable_to_define =
            Map.add_exn callable_to_define ~key:callable ~data:(AstResult.Some define)
          in
          location_to_callable, id_to_callable, callable_to_define
        else
          let search_result =
            (* Pyrefly gives us the location of the name AST node, i.e the location of 'foo' in `def
               foo(): ...`, but the Pyre AST does not give us the location of the name, only the
               location of the whole `def foo(): ...` statement. Since we can't match on the exact
               location, we try to find a definition whose location is between the `def` keyword and
               the first parameter or first statement. *)
            let first_parameter_or_statement_position =
              match strip_invalid_locations parameters, strip_invalid_locations body with
              | { Node.location = { Location.start; _ }; _ } :: _, _ -> start
              | _, { Node.location = { Location.start; _ }; _ } :: _ -> start
              | _ -> define_stop
            in
            Map.binary_search_subrange
              location_to_callable
              ~compare:(fun ~key:{ Location.start = location; _ } ~data:_ bound ->
                Location.compare_position location bound)
              ~lower_bound:(Maybe_bound.Excl define_start)
              ~upper_bound:(Maybe_bound.Incl first_parameter_or_statement_position)
            |> Map.to_alist
            |> List.filter ~f:(fun ({ Location.start = name_start; stop = name_stop }, _) ->
                   Location.compare_position define_start name_start < 0
                   && Location.compare_position name_stop first_parameter_or_statement_position <= 0)
          in
          match search_result with
          | [] ->
              (* This definition is not visible by pyrefly. It might be guarded by a `if
                 TYPE_CHECKING` or `if sys.version`. *)
              location_to_callable, id_to_callable, callable_to_define
          | [(location, callable)] ->
              let location_to_callable = Map.remove location_to_callable location in
              let callable_to_define =
                Map.add_exn callable_to_define ~key:callable ~data:(AstResult.Some define)
              in
              location_to_callable, id_to_callable, callable_to_define
          | _ ->
              Format.asprintf
                "Found multiple definitions matching with define `%a` at location %a of module `%a`"
                Reference.pp
                name
                Location.pp
                define_location
                ModuleQualifier.pp
                qualifier
              |> failwith
      in
      (* We create an implicit function containing all statements in the body of each class, called
         the "class top level define". However, some classes are synthesized out of thin air (for
         instance, `X = namedtuple('X')` creates a class `X`). Those won't have a top level define
         in the source. Assign `AstResult.Synthesized` for those. *)
      let add_toplevel_define_for_synthesized_class
          (location_to_callable, id_to_callable, callable_to_define)
          (location, callable)
        =
        if String.equal (FullyQualifiedName.last callable) Statement.class_toplevel_define_name then
          (* We create an implicit function containing all statements in the body of each class,
             called the "class top level define". However, some classes are synthesized out of thin
             air (for instance, `X = namedtuple('X')` creates a class `X`). Those won't have a top
             level define in the source. Let's create a dummy definition for those. *)
          let class_name = Option.value_exn (FullyQualifiedName.prefix callable) in
          let { ClassMetadataSharedMemory.Metadata.is_synthesized; _ } =
            ClassMetadataSharedMemory.get class_metadata_shared_memory class_name
            |> Option.value_exn
                 ~message:"unexpected: class toplevel define on a class that has no metadata info"
          in
          if is_synthesized then
            let location_to_callable = Map.remove location_to_callable location in
            let callable_to_define =
              Map.add_exn callable_to_define ~key:callable ~data:AstResult.Synthesized
            in
            location_to_callable, id_to_callable, callable_to_define
          else
            location_to_callable, id_to_callable, callable_to_define
        else
          location_to_callable, id_to_callable, callable_to_define
      in
      (* Function-like class fields (for instance, the generated `__init__` of dataclasses, or
         function-declared attributes like `foo: Callable[..., int]`) don't have a location. Assign
         `AstResult.Synthesized` for those. *)
      let add_synthesized_class_fields
          (location_to_callable, id_to_callable, callable_to_define)
          (local_function_id, callable)
        =
        if LocalFunctionId.is_class_field local_function_id then
          let id_to_callable = Map.remove id_to_callable local_function_id in
          let callable_to_define =
            Map.add_exn callable_to_define ~key:callable ~data:AstResult.Synthesized
          in
          location_to_callable, id_to_callable, callable_to_define
        else
          location_to_callable, id_to_callable, callable_to_define
      in
      let defines =
        Preprocessing.defines
          ~include_stubs:true
          ~include_nested:true
          ~include_toplevels:true
          ~include_methods:true
          source
      in
      let location_to_callable, id_to_callable, callable_to_define =
        List.fold
          ~init:(location_to_callable, id_to_callable, FullyQualifiedName.Map.empty)
          ~f:find_define_callable
          defines
      in
      let location_to_callable, id_to_callable, callable_to_define =
        List.fold
          ~init:(location_to_callable, id_to_callable, callable_to_define)
          ~f:add_toplevel_define_for_synthesized_class
          (Map.to_alist location_to_callable)
      in
      let location_to_callable, id_to_callable, callable_to_define =
        List.fold
          ~init:(location_to_callable, id_to_callable, callable_to_define)
          ~f:add_synthesized_class_fields
          (Map.to_alist id_to_callable)
      in
      if not (Map.is_empty location_to_callable && Map.is_empty id_to_callable) then
        let id_or_location, callable =
          if not (Map.is_empty location_to_callable) then
            Map.min_elt_exn location_to_callable
            |> fun (location, callable) -> Location.show location, callable
          else
            Map.min_elt_exn id_to_callable
            |> fun (local_function_id, callable) -> LocalFunctionId.show local_function_id, callable
        in
        Format.asprintf
          "Could not find AST of function `%a` at `%s` in module `%a`"
          FullyQualifiedName.pp
          callable
          id_or_location
          ModuleQualifier.pp
          qualifier
        |> failwith
      else
        Map.iteri
          ~f:(fun ~key:callable ~data:define_result ->
            let define_result = AstResult.map ~f:Preprocessing.drop_nested_body define_result in
            CallableAstSharedMemory.add callable_ast_shared_memory callable define_result;
            CallableDefineSignatureSharedMemory.add
              callable_define_signature_shared_memory
              callable
              (AstResult.map_node
                 ~f:(fun { Statement.Define.signature; _ } -> signature)
                 define_result);
            ())
          callable_to_define
    in
    let collect_class_decorators_from_source ~qualifier ~classes ~source =
      let location_to_class =
        classes
        |> List.map ~f:(fun class_name ->
               ClassMetadataSharedMemory.get class_metadata_shared_memory class_name
               |> assert_shared_memory_key_exists (fun () -> "missing class metadata for class")
               |> fun { ClassMetadataSharedMemory.Metadata.name_location; _ } ->
               name_location, class_name)
        |> Location.Map.of_alist_exn
      in
      (* For a given `class ..` statement, we need to find the matching definition provided by
         Pyrefly. *)
      let find_matching_class_for_ast
          (location_to_class, class_to_statement)
          ({
             Node.value = { Statement.Class.name; base_arguments; body; _ };
             location =
               { Location.start = class_start; stop = class_stop } as class_statement_location;
           } as class_statement)
        =
        (* Pyrefly gives us the location of the name AST node, i.e the location of 'Foo' in `class
           Foo: ...`, but the Pyre AST does not give us the location of the name, only the location
           of the whole `class Foo: ...` statement. Since we can't match on the exact location, we
           try to find a class statement whose location is between the `class` keyword and the first
           base or first statement. *)
        let search_result =
          let first_base_or_statement_position =
            let base_argument_expressions =
              List.map ~f:(fun { Expression.Call.Argument.value; _ } -> value) base_arguments
            in
            match
              strip_invalid_locations base_argument_expressions, strip_invalid_locations body
            with
            | { Node.location = { Location.start; _ }; _ } :: _, _ -> start
            | _, { Node.location = { Location.start; _ }; _ } :: _ -> start
            | _ -> class_stop
          in
          Map.binary_search_subrange
            location_to_class
            ~compare:(fun ~key:{ Location.start = location; stop = _ } ~data:_ bound ->
              Location.compare_position location bound)
            ~lower_bound:(Maybe_bound.Excl class_start)
            ~upper_bound:(Maybe_bound.Incl first_base_or_statement_position)
          |> Map.to_alist
          |> List.filter ~f:(fun ({ Location.start = name_start; stop = name_stop }, _) ->
                 Location.compare_position class_start name_start < 0
                 && Location.compare_position name_stop first_base_or_statement_position <= 0)
        in
        match search_result with
        | [] ->
            (* This definition is not visible by pyrefly. It might be guarded by a `if
               TYPE_CHECKING` or `if sys.version`. *)
            location_to_class, class_to_statement
        | [(location, class_name)] ->
            let location_to_class = Map.remove location_to_class location in
            let class_to_statement =
              Map.add_exn class_to_statement ~key:class_name ~data:class_statement
            in
            location_to_class, class_to_statement
        | _ ->
            Format.asprintf
              "Found multiple class statements matching with class `%a` at location %a of module \
               `%a`"
              Reference.pp
              name
              Location.pp
              class_statement_location
              ModuleQualifier.pp
              qualifier
            |> failwith
      in
      let add_synthesized_class (location_to_class, class_to_statement) (location, class_name) =
        let { ClassMetadataSharedMemory.Metadata.is_synthesized; _ } =
          ClassMetadataSharedMemory.get class_metadata_shared_memory class_name
          |> assert_shared_memory_key_exists (fun () -> "missing class metadata for class")
        in
        if is_synthesized then
          let class_statement =
            {
              Statement.Class.name = FullyQualifiedName.to_reference class_name;
              base_arguments = [];
              parent = NestingContext.create_toplevel ();
              body = [];
              decorators = [];
              top_level_unbound_names = [];
              type_params = [];
            }
            |> Node.create ~location
          in
          let location_to_class = Map.remove location_to_class location in
          let class_to_statement =
            Map.add_exn class_to_statement ~key:class_name ~data:class_statement
          in
          location_to_class, class_to_statement
        else
          location_to_class, class_to_statement
      in
      let class_statements = Preprocessing.classes source in
      let remaining_classes, class_to_statement =
        List.fold
          ~init:(location_to_class, FullyQualifiedName.Map.empty)
          ~f:find_matching_class_for_ast
          class_statements
      in
      let remaining_classes, class_to_statement =
        List.fold
          ~init:(remaining_classes, class_to_statement)
          ~f:add_synthesized_class
          (Map.to_alist remaining_classes)
      in
      if not (Map.is_empty remaining_classes) then
        let location, class_name = Map.min_elt_exn remaining_classes in
        Format.asprintf
          "Could not find AST of class `%a` at location %a in module `%a`"
          FullyQualifiedName.pp
          class_name
          Location.pp
          location
          ModuleQualifier.pp
          qualifier
        |> failwith
      else
        Map.iteri
          ~f:(fun ~key:class_name ~data:{ Node.value = { Statement.Class.decorators; _ }; _ } ->
            ClassDecoratorsSharedMemory.add
              class_decorators_shared_memory
              class_name
              (AstResult.Some decorators))
          class_to_statement
    in
    let parse_module qualifier =
      let module_info =
        ModuleInfosSharedMemory.get module_infos_shared_memory qualifier
        |> Option.value_exn ~message:"missing module info for qualifier"
      in
      let callables = ModuleCallablesSharedMemory.get module_callables_shared_memory qualifier in
      let classes = ModuleClassesSharedMemory.get module_classes_shared_memory qualifier in
      match module_info, callables, classes with
      | _, None, _ -> ()
      | _, _, None -> ()
      | { ModuleInfosSharedMemory.Module.absolute_source_path = None; _ }, Some _, Some _ ->
          failwith "unexpected: no source path for module with callables"
      | { ModuleInfosSharedMemory.Module.is_test = true; _ }, Some callables, Some classes ->
          let () = store_callable_asts callables AstResult.TestFile in
          let () = store_class_decorators classes AstResult.TestFile in
          ()
      | ( {
            ModuleInfosSharedMemory.Module.absolute_source_path = Some source_path;
            is_test = false;
            _;
          },
          Some callables,
          Some classes ) -> (
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
          let open Result.Monad_infix in
          let {
            Configuration.Analysis.python_version =
              { Configuration.PythonVersion.major; minor; micro };
            system_platform;
            _;
          }
            =
            configuration
          in
          let sys_platform = Option.value system_platform ~default:"linux" in
          let parse_result =
            Analysis.Parsing.parse_result_of_load_result
              ~controls
              ~post_process:false
              pyre1_module_path
              load_result
            >>| Analysis.Preprocessing.replace_version_specific_code
                  ~major_version:major
                  ~minor_version:minor
                  ~micro_version:micro
            >>| Analysis.Preprocessing.replace_platform_specific_code ~sys_platform
            >>| Analysis.Preprocessing.mangle_private_attributes
          in
          match parse_result with
          | Ok ({ Source.module_path; _ } as source) ->
              (* Remove the qualifier created by pyre, it is wrong *)
              let module_path = { module_path with qualifier = Reference.empty } in
              let source = { source with module_path } in
              let () = collect_callable_asts_from_source ~qualifier ~callables ~source in
              let () = collect_class_decorators_from_source ~qualifier ~classes ~source in
              ()
          | Error { Analysis.Parsing.ParseResult.Error.location; message; _ } ->
              let () =
                Log.error
                  "%a:%a: %s"
                  PyrePath.pp
                  (source_path |> ArtifactPath.raw)
                  Location.pp
                  location
                  message
              in
              let () = store_callable_asts callables AstResult.ParseError in
              let () = store_class_decorators classes AstResult.ParseError in
              ())
    in
    let scheduler_policy =
      Scheduler.Policy.from_configuration_or_default
        scheduler_policies
        Configuration.ScheduleIdentifier.PyreflyParseSources
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
    Statistics.performance ~name:"Parsed source files" ~phase_name:"Parsing source files" ~timer ();
    ( callable_ast_shared_memory,
      callable_define_signature_shared_memory,
      class_decorators_shared_memory )


  (* Logic to assign fully qualified names to classes and defines. *)
  module DefinitionCollector = struct
    module Definition = struct
      type t =
        | Function of ModuleDefinitionsFile.FunctionDefinition.t
        | Class of ModuleDefinitionsFile.ClassDefinition.t
      [@@deriving equal, show]

      let name = function
        | Function { ModuleDefinitionsFile.FunctionDefinition.name; _ } -> name
        | Class { ModuleDefinitionsFile.ClassDefinition.name; _ } -> name
    end

    module ScopeId = struct
      module T = struct
        type t =
          | Location of Location.t
          | Synthesized of LocalFunctionId.t
        [@@deriving compare, equal, show, sexp]

        let _ = pp

        let of_definition = function
          | Definition.Function
              {
                ModuleDefinitionsFile.FunctionDefinition.local_function_id =
                  LocalFunctionId.Function location;
                _;
              } ->
              Location location
          | Definition.Function { ModuleDefinitionsFile.FunctionDefinition.local_function_id; _ } ->
              Synthesized local_function_id
          | Definition.Class { ModuleDefinitionsFile.ClassDefinition.name_location; _ } ->
              Location name_location
      end

      include T
      module Map = Map.Make (T)
    end

    module QualifiedDefinition = struct
      type t = {
        qualified_name: FullyQualifiedName.t;
        local_name: Reference.t; (* a non-unique name, more user-friendly. *)
        definition: Definition.t;
        name_location: NameLocation.t;
      }
    end

    (* Returns a list of path elements for a function or class in the AST. For instance, ['MyClass',
       'foo'] for a method foo in MyClass. *)
    let rec create_local_path ~function_definitions ~class_definitions sofar parent =
      match parent with
      | ModuleDefinitionsFile.ParentScope.TopLevel -> sofar
      | ModuleDefinitionsFile.ParentScope.Class parent_location ->
          let ({ ModuleDefinitionsFile.ClassDefinition.parent; _ } as class_definition) =
            Map.find_exn class_definitions parent_location
          in
          let sofar = Definition.Class class_definition :: sofar in
          create_local_path ~function_definitions ~class_definitions sofar parent
      | ModuleDefinitionsFile.ParentScope.Function parent_location ->
          let ({ ModuleDefinitionsFile.FunctionDefinition.parent; _ } as function_definition) =
            Map.find_exn function_definitions (LocalFunctionId.create_function parent_location)
          in
          let sofar = Definition.Function function_definition :: sofar in
          create_local_path ~function_definitions ~class_definitions sofar parent


    let local_path_of_function
        ~function_definitions
        ~class_definitions
        ({ ModuleDefinitionsFile.FunctionDefinition.parent; _ } as function_definition)
      =
      create_local_path
        ~function_definitions
        ~class_definitions
        [Definition.Function function_definition]
        parent


    let local_path_of_class
        ~function_definitions
        ~class_definitions
        ({ ModuleDefinitionsFile.ClassDefinition.parent; _ } as class_definition)
      =
      create_local_path
        ~function_definitions
        ~class_definitions
        [Definition.Class class_definition]
        parent


    (* Represents a tree of definitions, where children of a node are the functions and classes
       nested under that definition. For instance, all methods are nested under their parent
       class. *)
    module Tree = struct
      module Node = struct
        type 'a t = {
          value: 'a;
          children: 'a t ScopeId.Map.t;
        }
      end

      type 'a t = { children: 'a ScopeId.Map.t }

      let empty = { children = ScopeId.Map.empty }

      let add_local_path path { children } =
        let rec add_to_children children = function
          | [] -> children
          | value :: tail ->
              Map.update children (ScopeId.of_definition value) ~f:(function
                  | None -> { value; children = add_to_children ScopeId.Map.empty tail }
                  | Some { Node.value = existing_value; children = existing_children } ->
                      if not (Definition.equal existing_value value) then
                        failwith "Found multiple definitions with the same location"
                      else
                        {
                          value = existing_value;
                          children = add_to_children existing_children tail;
                        })
        in
        { children = add_to_children children path }


      let from_definitions ~function_definitions ~class_definitions =
        let tree =
          Map.fold
            ~init:empty
            ~f:(fun ~key:_ ~data:class_definition sofar ->
              add_local_path
                (local_path_of_class ~function_definitions ~class_definitions class_definition)
                sofar)
            class_definitions
        in
        Map.fold
          ~init:tree
          ~f:(fun ~key:_ ~data:function_definition sofar ->
            add_local_path
              (local_path_of_function ~function_definitions ~class_definitions function_definition)
              sofar)
          function_definitions


      module QualifiedNode = struct
        type t = {
          definition: Definition.t;
          unique_name: string;
          name_overlaps_module: bool;
        }
        [@@deriving show]

        let _ = pp
      end

      let create_qualified_names { children } =
        let rec qualify_children children =
          let name_indices = SerializableStringMap.empty in
          let add_definition (name_indices, sofar) (location, { Node.value = definition; children })
            =
            let name =
              match definition with
              | Definition.Function
                  { ModuleDefinitionsFile.FunctionDefinition.name; is_property_setter; _ } ->
                  (* We need to differentiate property setters from property getters *)
                  let name =
                    if is_property_setter then
                      Format.sprintf "%s@setter" name
                    else
                      name
                  in
                  name
              | Definition.Class { ModuleDefinitionsFile.ClassDefinition.name; _ } -> name
            in
            (* We might find multiple definitions with the same name, for instance:
             * ```
             * def foo(): return
             * def foo(): return
             * ```
             * We assign an unique index to each definition. *)
            let name_indices =
              SerializableStringMap.update
                name
                (function
                  | None -> Some 0
                  | Some index -> Some (index + 1))
                name_indices
            in
            let name =
              match SerializableStringMap.find name name_indices with
              | 0 -> name
              | index -> Format.sprintf "%s$%d" name (index + 1)
            in
            let node =
              {
                Node.value =
                  { QualifiedNode.definition; unique_name = name; name_overlaps_module = false };
                children = qualify_children children;
              }
            in
            name_indices, (location, node) :: sofar
          in
          children
          |> Map.to_alist ~key_order:`Increasing
          |> List.fold ~init:(name_indices, []) ~f:add_definition
          |> snd
          |> ScopeId.Map.of_alist_exn
        in
        { children = qualify_children children }


      (* We might find a definition in a module that would have the same fully qualified name as
       * another definition in another module. For instance:
       *
       * ```
       * # a.py
       * class b:
       *   def c(): ...
       * # a/b.py
       * def c(): ...
       * ```
       *
       * They would both have a fully qualified name `a.b.c`.
       * In those cases, we add a separator `#` in the fully qualified name, between the
       * module and local name, so it would be `a#b.c` instead of `a.b.c`. To detect the name clash, we
       * need to check if our fully qualified name is also a valid module qualifier. *)
      let check_module_overlaps ~module_qualifier ~module_exists { children } =
        let rec set_overlap_flag { Node.value; children } =
          {
            Node.value = { value with QualifiedNode.name_overlaps_module = true };
            children = Map.map ~f:set_overlap_flag children;
          }
        in
        let rec check_node
            ~parent
            { Node.value = { QualifiedNode.unique_name; _ } as value; children }
          =
          let name_overlaps_module =
            FullyQualifiedName.create
              ~module_qualifier
              ~local_name:(List.rev (unique_name :: parent))
              ~add_module_separator:false
            |> FullyQualifiedName.to_reference
            |> ModuleQualifier.from_reference_unchecked
            |> module_exists
          in
          if name_overlaps_module then
            set_overlap_flag { Node.value; children }
          else
            {
              Node.value;
              children = Map.map children ~f:(check_node ~parent:(unique_name :: parent));
            }
        in
        { children = Map.map children ~f:(check_node ~parent:[]) }


      let collect_definitions ~module_qualifier { children } =
        let get_name_location = function
          | Definition.Function { local_function_id = LocalFunctionId.Function location; _ } ->
              NameLocation.DefineName location
          | Definition.Function { local_function_id = LocalFunctionId.ModuleTopLevel; _ } ->
              NameLocation.ModuleTopLevel
          | Definition.Function { local_function_id = LocalFunctionId.ClassTopLevel _; _ } ->
              failwith "unreachable"
          | Definition.Function { local_function_id = LocalFunctionId.ClassField _; _ } ->
              NameLocation.UnknonwnForClassField
          | Definition.Function { local_function_id = LocalFunctionId.FunctionDecoratedTarget _; _ }
            ->
              failwith "unexpected decorated target in function definitions"
          | Definition.Class { name_location; _ } -> NameLocation.ClassName name_location
        in
        let rec add_definition
            sofar
            ~parent_qualified_name
            ~parent_local_name
            {
              Node.value = { QualifiedNode.definition; unique_name; name_overlaps_module };
              children;
            }
          =
          let symbol_name = Definition.name definition in
          let sofar =
            {
              QualifiedDefinition.qualified_name =
                FullyQualifiedName.create
                  ~module_qualifier
                  ~local_name:(List.rev (unique_name :: parent_qualified_name))
                  ~add_module_separator:name_overlaps_module;
              local_name = Reference.create_from_list (List.rev (symbol_name :: parent_local_name));
              name_location = get_name_location definition;
              definition;
            }
            :: sofar
          in
          Map.fold children ~init:sofar ~f:(fun ~key:_ ~data:node sofar ->
              add_definition
                sofar
                ~parent_qualified_name:(unique_name :: parent_qualified_name)
                ~parent_local_name:(symbol_name :: parent_local_name)
                node)
        in
        let definitions =
          Map.fold children ~init:[] ~f:(fun ~key:_ ~data:node sofar ->
              add_definition sofar ~parent_qualified_name:[] ~parent_local_name:[] node)
        in
        List.rev definitions
    end

    let add_toplevel_defines ~module_qualifier definitions =
      let add_toplevel
          sofar
          ({ QualifiedDefinition.definition; qualified_name; local_name; _ } as
          qualified_definition)
        =
        let sofar = qualified_definition :: sofar in
        match definition with
        | Definition.Class
            {
              local_class_id;
              name_location;
              (* Don't add a toplevel define for synthesized classes, such as `MyTuple =
                 namedtuple('MyTuple', ['x', 'y'])` *)
              is_synthesized = false;
              _;
            } ->
            {
              QualifiedDefinition.definition =
                Definition.Function
                  (ModuleDefinitionsFile.FunctionDefinition.create_class_toplevel
                     ~name_location
                     ~local_class_id);
              qualified_name = FullyQualifiedName.create_class_toplevel qualified_name;
              local_name =
                Reference.combine
                  local_name
                  (Reference.create_from_list [Ast.Statement.class_toplevel_define_name]);
              name_location = NameLocation.ClassName name_location;
            }
            :: sofar
        | _ -> sofar
      in
      let definitions = List.fold ~f:add_toplevel ~init:[] definitions in
      let definitions = List.rev definitions in
      {
        QualifiedDefinition.definition =
          Definition.Function (ModuleDefinitionsFile.FunctionDefinition.create_module_toplevel ());
        qualified_name = FullyQualifiedName.create_module_toplevel ~module_qualifier;
        local_name = Reference.create_from_list [Ast.Statement.toplevel_define_name];
        name_location = NameLocation.ModuleTopLevel;
      }
      :: definitions
  end

  module DefinitionCount = struct
    type t = {
      number_callables: int;
      number_classes: int;
    }

    let empty = { number_callables = 0; number_classes = 0 }

    let add
        { number_callables = left_number_callables; number_classes = left_number_classes }
        { number_callables = right_number_callables; number_classes = right_number_classes }
      =
      {
        number_callables = left_number_callables + right_number_callables;
        number_classes = left_number_classes + right_number_classes;
      }
  end

  let collect_classes_and_definitions
      ~scheduler
      ~scheduler_policies
      ~pyrefly_directory
      ~qualifier_to_module_map
      ~module_infos_shared_memory
    =
    let timer = Timer.start () in
    let callable_metadata_shared_memory = CallableMetadataSharedMemory.create () in
    let class_metadata_shared_memory = ClassMetadataSharedMemory.create () in
    let class_fields_shared_memory = ClassFieldsSharedMemory.create () in
    let module_callables_shared_memory = ModuleCallablesSharedMemory.create () in
    let module_classes_shared_memory = ModuleClassesSharedMemory.create () in
    let module_globals_shared_memory = ModuleGlobalsSharedMemory.create () in
    let class_id_to_qualified_name_shared_memory = ClassIdToQualifiedNameSharedMemory.create () in
    let callable_id_to_qualified_name_shared_memory =
      CallableIdToQualifiedNameSharedMemory.create ()
    in
    let () = Log.info "Collecting classes and definitions..." in
    (* First step: collect classes and definitions, and assign them fully qualified names. *)
    let collect_definitions_and_assign_names (module_qualifier, pyrefly_info_filename) =
      let {
        ModuleDefinitionsFile.function_definitions;
        class_definitions;
        module_id;
        global_variables;
        _;
      }
        =
        ModuleDefinitionsFile.from_path_exn ~pyrefly_directory pyrefly_info_filename
      in
      let definitions =
        DefinitionCollector.Tree.from_definitions ~function_definitions ~class_definitions
        |> DefinitionCollector.Tree.create_qualified_names
        |> DefinitionCollector.Tree.check_module_overlaps
             ~module_qualifier
             ~module_exists:(fun qualifier ->
               ModuleInfosSharedMemory.get module_infos_shared_memory qualifier |> Option.is_some)
        |> DefinitionCollector.Tree.collect_definitions ~module_qualifier
        |> DefinitionCollector.add_toplevel_defines ~module_qualifier
      in
      let store_definition
          (callables, classes)
          { DefinitionCollector.QualifiedDefinition.qualified_name; definition; name_location; _ }
        =
        match definition with
        | Function
            {
              name;
              local_function_id;
              captured_variables;
              is_overload;
              is_staticmethod;
              is_classmethod;
              is_property_getter;
              is_property_setter;
              is_stub;
              is_def_statement;
              is_toplevel;
              is_class_toplevel;
              overridden_base_method;
              defining_class;
              decorator_callees;
              _;
            } ->
            CallableMetadataSharedMemory.add
              callable_metadata_shared_memory
              qualified_name
              {
                CallableMetadataSharedMemory.Value.metadata =
                  {
                    module_qualifier = ModuleQualifier.to_reference module_qualifier;
                    name_location;
                    is_overload;
                    is_staticmethod;
                    is_classmethod;
                    is_property_getter;
                    is_property_setter;
                    is_toplevel;
                    is_class_toplevel;
                    is_stub;
                    is_def_statement;
                    parent_is_class = Option.is_some defining_class;
                  };
                name;
                local_function_id;
                overridden_base_method;
                defining_class;
                decorator_callees;
                captured_variables;
              };
            CallableIdToQualifiedNameSharedMemory.add
              callable_id_to_qualified_name_shared_memory
              { GlobalCallableId.module_id; local_function_id }
              qualified_name;
            qualified_name :: callables, classes
        | Class
            {
              name = class_name;
              local_class_id;
              name_location;
              is_synthesized;
              is_dataclass;
              is_named_tuple;
              is_typed_dict;
              fields;
              bases;
              mro;
              decorator_callees;
              _;
            } ->
            ClassMetadataSharedMemory.add
              class_metadata_shared_memory
              qualified_name
              {
                ClassMetadataSharedMemory.Metadata.module_qualifier;
                name_location;
                local_class_id;
                is_synthesized;
                is_dataclass;
                is_named_tuple;
                is_typed_dict;
                parents = bases;
                mro;
                decorator_callees;
              };
            let fields =
              fields
              |> List.map
                   ~f:(fun
                        {
                          ModuleDefinitionsFile.JsonClassField.name;
                          type_;
                          explicit_annotation;
                          location;
                          declaration_kind;
                        }
                      ->
                     let name =
                       if Identifier.is_private_name name then
                         Identifier.mangle_private_name ~class_name name
                       else
                         name
                     in
                     ( name,
                       {
                         ClassField.type_ = JsonType.to_pysa_type type_;
                         explicit_annotation;
                         location;
                         declaration_kind;
                       } ))
              |> SerializableStringMap.of_alist_exn
            in
            ClassFieldsSharedMemory.add class_fields_shared_memory qualified_name fields;
            ClassIdToQualifiedNameSharedMemory.add
              class_id_to_qualified_name_shared_memory
              { GlobalClassId.module_id; local_class_id }
              qualified_name;
            callables, qualified_name :: classes
      in
      let callables, classes = List.fold definitions ~init:([], []) ~f:store_definition in
      ModuleCallablesSharedMemory.add module_callables_shared_memory module_qualifier callables;
      ModuleClassesSharedMemory.add module_classes_shared_memory module_qualifier classes;
      let global_variables =
        global_variables
        |> List.map ~f:(fun { ModuleDefinitionsFile.JsonGlobalVariable.name; type_; location } ->
               name, { GlobalVariable.type_ = type_ >>| JsonType.to_pysa_type; location })
        |> SerializableStringMap.of_alist_exn
      in
      ModuleGlobalsSharedMemory.add module_globals_shared_memory module_qualifier global_variables;
      {
        DefinitionCount.number_callables = List.length callables;
        number_classes = List.length classes;
      }
    in
    let scheduler_policy =
      Scheduler.Policy.from_configuration_or_default
        scheduler_policies
        Configuration.ScheduleIdentifier.PyreflyCollectDefinitions
        ~default:
          (Scheduler.Policy.fixed_chunk_count
             ~minimum_chunks_per_worker:1
             ~minimum_chunk_size:1
             ~preferred_chunks_per_worker:1
             ())
    in
    let inputs =
      qualifier_to_module_map
      |> Map.to_alist
      |> List.filter_map ~f:(fun (qualifier, { Module.pyrefly_info_filename; _ }) ->
             match pyrefly_info_filename with
             | Some pyrefly_info_filename -> Some (qualifier, pyrefly_info_filename)
             | None -> None)
    in
    let map modules =
      List.map ~f:collect_definitions_and_assign_names modules
      |> List.fold ~init:DefinitionCount.empty ~f:DefinitionCount.add
    in
    let { DefinitionCount.number_callables; number_classes } =
      Scheduler.map_reduce
        scheduler
        ~policy:scheduler_policy
        ~initial:DefinitionCount.empty
        ~map
        ~reduce:DefinitionCount.add
        ~inputs
        ()
    in
    (* Second step: record undecorated signatures. This currently requires parsing module info files
       again, which is wasteful. *)
    let callable_undecorated_signatures_shared_memory =
      CallableUndecoratedSignaturesSharedMemory.create ()
    in
    let parse_class_parents_and_undecorated_signatures (module_qualifier, pyrefly_info_filename) =
      let { ModuleDefinitionsFile.function_definitions; class_definitions; module_id; _ } =
        ModuleDefinitionsFile.from_path_exn ~pyrefly_directory pyrefly_info_filename
      in
      let get_function_name local_function_id =
        CallableIdToQualifiedNameSharedMemory.get
          callable_id_to_qualified_name_shared_memory
          { GlobalCallableId.module_id; local_function_id }
      in
      let fold_function_parameters (position, excluded, sofar) = function
        | ModuleDefinitionsFile.FunctionParameter.PosOnly { name; annotation; required } ->
            ( position + 1,
              (match name with
              | Some name -> name :: excluded
              | None -> excluded),
              FunctionParameter.PositionalOnly
                {
                  name;
                  position;
                  annotation = JsonType.to_pysa_type annotation;
                  has_default = not required;
                }
              :: sofar )
        | Pos { name; annotation; required } ->
            ( position + 1,
              name :: excluded,
              FunctionParameter.Named
                {
                  name;
                  position;
                  annotation = JsonType.to_pysa_type annotation;
                  has_default = not required;
                }
              :: sofar )
        | VarArg { name; annotation = _ } ->
            position + 1, excluded, FunctionParameter.Variable { name; position } :: sofar
        | KwOnly { name; annotation; required } ->
            ( position + 1,
              name :: excluded,
              FunctionParameter.KeywordOnly
                { name; annotation = JsonType.to_pysa_type annotation; has_default = not required }
              :: sofar )
        | Kwargs { name; annotation } ->
            ( position + 1,
              [],
              FunctionParameter.Keywords
                { name; annotation = JsonType.to_pysa_type annotation; excluded }
              :: sofar )
      in
      let convert_function_signature
          { ModuleDefinitionsFile.FunctionSignature.parameters; return_annotation }
        =
        let parameters =
          match parameters with
          | ModuleDefinitionsFile.FunctionParameters.List parameters ->
              let parameters =
                parameters
                |> List.fold ~init:(0, [], []) ~f:fold_function_parameters
                |> fun (_, _, parameters) -> parameters |> List.rev
              in
              FunctionParameters.List parameters
          | ModuleDefinitionsFile.FunctionParameters.Ellipsis -> FunctionParameters.Ellipsis
          | ModuleDefinitionsFile.FunctionParameters.ParamSpec -> FunctionParameters.ParamSpec
        in
        {
          FunctionSignature.parameters;
          return_annotation = JsonType.to_pysa_type return_annotation;
        }
      in
      let toplevel_undecorated_signature =
        {
          FunctionSignature.parameters = FunctionParameters.List [];
          return_annotation = JsonType.pysa_type_none;
        }
      in
      let add_function
          ~key:local_function_id
          ~data:{ ModuleDefinitionsFile.FunctionDefinition.undecorated_signatures; _ }
        =
        let qualified_name = get_function_name local_function_id in
        let undecorated_signatures =
          List.map ~f:convert_function_signature undecorated_signatures
        in
        CallableUndecoratedSignaturesSharedMemory.add
          callable_undecorated_signatures_shared_memory
          qualified_name
          undecorated_signatures
      in
      let add_undecorated_signature_for_class
          ~key:_
          ~data:{ ModuleDefinitionsFile.ClassDefinition.local_class_id; _ }
        =
        let class_name =
          ClassIdToQualifiedNameSharedMemory.get_class_name
            class_id_to_qualified_name_shared_memory
            { GlobalClassId.module_id; local_class_id }
        in
        CallableUndecoratedSignaturesSharedMemory.add
          callable_undecorated_signatures_shared_memory
          (FullyQualifiedName.create_class_toplevel class_name)
          [toplevel_undecorated_signature];
        ()
      in
      let () = Map.iteri ~f:add_function function_definitions in
      let () = Map.iteri ~f:add_undecorated_signature_for_class class_definitions in
      let () =
        CallableUndecoratedSignaturesSharedMemory.add
          callable_undecorated_signatures_shared_memory
          (FullyQualifiedName.create_module_toplevel ~module_qualifier)
          [toplevel_undecorated_signature]
      in
      ()
    in
    let scheduler_policy =
      Scheduler.Policy.from_configuration_or_default
        scheduler_policies
        Configuration.ScheduleIdentifier.PyreflyParseClassParents
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
        ~map:(List.iter ~f:parse_class_parents_and_undecorated_signatures)
        ~reduce:(fun () () -> ())
        ~inputs
        ()
    in
    Log.info "Collected classes and definitions: %.3fs" (Timer.stop_in_sec timer);
    Statistics.performance
      ~name:"Collected classes and definitions"
      ~phase_name:"Collecting classes and definitions"
      ~timer
      ~integers:["callables", number_callables; "classes", number_classes]
      ();
    ( callable_metadata_shared_memory,
      class_metadata_shared_memory,
      class_fields_shared_memory,
      module_callables_shared_memory,
      module_classes_shared_memory,
      module_globals_shared_memory,
      class_id_to_qualified_name_shared_memory,
      callable_id_to_qualified_name_shared_memory,
      callable_undecorated_signatures_shared_memory )


  let create_from_directory ~scheduler ~scheduler_policies ~configuration pyrefly_directory =
    let qualifier_to_module_map, object_class_id, dict_class_id, typing_mapping_class_id =
      parse_modules ~pyrefly_directory
    in

    let module_infos_shared_memory, module_id_to_qualifier_shared_memory =
      write_module_infos_to_shared_memory ~qualifier_to_module_map
    in

    let qualifiers_shared_memory =
      let handle = QualifiersSharedMemory.create () in
      let qualifiers =
        qualifier_to_module_map
        |> Map.to_alist
        |> List.map
             ~f:(fun (module_qualifier, { Module.absolute_source_path; pyrefly_info_filename; _ })
                ->
               {
                 QualifiersSharedMemory.Value.module_qualifier;
                 has_source = Option.is_some absolute_source_path;
                 has_info = Option.is_some pyrefly_info_filename;
               })
      in
      let () = QualifiersSharedMemory.add handle Memory.SingletonKey.key qualifiers in
      handle
    in

    let ( callable_metadata_shared_memory,
          class_metadata_shared_memory,
          class_fields_shared_memory,
          module_callables_shared_memory,
          module_classes_shared_memory,
          module_globals_shared_memory,
          class_id_to_qualified_name_shared_memory,
          callable_id_to_qualified_name_shared_memory,
          callable_undecorated_signatures_shared_memory )
      =
      collect_classes_and_definitions
        ~scheduler
        ~scheduler_policies
        ~pyrefly_directory
        ~qualifier_to_module_map
        ~module_infos_shared_memory
    in

    let ( callable_ast_shared_memory,
          callable_define_signature_shared_memory,
          class_decorators_shared_memory )
      =
      parse_source_files
        ~scheduler
        ~scheduler_policies
        ~configuration
        ~module_infos_shared_memory
        ~qualifier_to_module_map
        ~module_callables_shared_memory
        ~module_classes_shared_memory
        ~callable_metadata_shared_memory
        ~class_metadata_shared_memory
    in

    let object_class =
      ClassIdToQualifiedNameSharedMemory.get_class_name
        class_id_to_qualified_name_shared_memory
        object_class_id
    in

    {
      pyrefly_directory;
      qualifier_to_module_map;
      module_infos_shared_memory;
      qualifiers_shared_memory;
      type_of_expressions_shared_memory = None;
      callable_metadata_shared_memory;
      class_metadata_shared_memory;
      class_fields_shared_memory;
      class_decorators_shared_memory;
      module_callables_shared_memory;
      module_classes_shared_memory;
      module_globals_shared_memory;
      module_id_to_qualifier_shared_memory;
      class_id_to_qualified_name_shared_memory;
      callable_id_to_qualified_name_shared_memory;
      callable_ast_shared_memory;
      callable_define_signature_shared_memory;
      callable_undecorated_signatures_shared_memory;
      object_class;
      dict_class_id;
      typing_mapping_class_id;
    }


  let parse_type_of_expressions
      ({ pyrefly_directory; qualifier_to_module_map; _ } as read_write_api)
      ~scheduler
      ~scheduler_policies
    =
    let timer = Timer.start () in
    let () = Log.info "Parsing type of expressions from pyrefly..." in
    let type_of_expressions_shared_memory = TypeOfExpressionsSharedMemory.create () in
    let scheduler_policy =
      Scheduler.Policy.from_configuration_or_default
        scheduler_policies
        Configuration.ScheduleIdentifier.PyreflyParseTypeOfExpressions
        ~default:
          (Scheduler.Policy.fixed_chunk_count
             ~minimum_chunks_per_worker:1
             ~minimum_chunk_size:1
             ~preferred_chunks_per_worker:1
             ())
    in
    let parse_module_info (module_qualifier, pyrefly_info_filename) =
      let { ModuleTypeOfExpressions.type_of_expression; module_id = _ } =
        ModuleTypeOfExpressions.from_path_exn ~pyrefly_directory pyrefly_info_filename
      in
      Map.iteri type_of_expression ~f:(fun ~key:location ~data:type_ ->
          let type_ = JsonType.to_pysa_type type_ in
          TypeOfExpressionsSharedMemory.add
            type_of_expressions_shared_memory
            { TypeOfExpressionsSharedMemory.Key.module_qualifier; location }
            type_)
    in
    let inputs =
      qualifier_to_module_map
      |> Map.to_alist
      |> List.filter_map ~f:(fun (qualifier, { Module.pyrefly_info_filename; _ }) ->
             match pyrefly_info_filename with
             | Some pyrefly_info_filename -> Some (qualifier, pyrefly_info_filename)
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
    Log.info "Parsed type of expressions from pyrefly: %.3fs" (Timer.stop_in_sec timer);
    Statistics.performance
      ~name:"Parsed type of expressions from pyrefly"
      ~phase_name:"Parsing type of expressions from pyrefly"
      ~timer
      ();
    {
      read_write_api with
      type_of_expressions_shared_memory = Some type_of_expressions_shared_memory;
    }


  (* Remove information about the project from the shared memory. This should reduce considerably
     the shared memory **heap** size. However, note that the shared memory does NOT allow removing
     entries from the **hash table**, so all entries are kept. *)
  let cleanup
      {
        pyrefly_directory = _;
        qualifier_to_module_map;
        module_infos_shared_memory;
        qualifiers_shared_memory;
        type_of_expressions_shared_memory = _;
        callable_metadata_shared_memory;
        class_metadata_shared_memory;
        class_fields_shared_memory;
        class_decorators_shared_memory;
        module_callables_shared_memory;
        module_classes_shared_memory;
        module_globals_shared_memory;
        module_id_to_qualifier_shared_memory;
        class_id_to_qualified_name_shared_memory;
        callable_id_to_qualified_name_shared_memory;
        callable_ast_shared_memory;
        callable_define_signature_shared_memory;
        callable_undecorated_signatures_shared_memory;
        object_class = _;
        dict_class_id = _;
        typing_mapping_class_id = _;
      }
      ~scheduler
    =
    let scheduler_policy =
      Scheduler.Policy.fixed_chunk_count
        ~minimum_chunks_per_worker:1
        ~minimum_chunk_size:1
        ~preferred_chunks_per_worker:1
        ()
    in
    let cleanup_callable ~module_id callable_name =
      let { CallableMetadataSharedMemory.Value.local_function_id; _ } =
        CallableMetadataSharedMemory.get callable_metadata_shared_memory callable_name
        |> assert_shared_memory_key_exists (fun () ->
               Format.asprintf "missing callable metadata: `%a`" FullyQualifiedName.pp callable_name)
      in
      CallableMetadataSharedMemory.remove callable_metadata_shared_memory callable_name;
      CallableAstSharedMemory.remove callable_ast_shared_memory callable_name;
      CallableDefineSignatureSharedMemory.remove
        callable_define_signature_shared_memory
        callable_name;
      CallableUndecoratedSignaturesSharedMemory.remove
        callable_undecorated_signatures_shared_memory
        callable_name;
      CallableIdToQualifiedNameSharedMemory.remove
        callable_id_to_qualified_name_shared_memory
        { GlobalCallableId.module_id; local_function_id };
      ()
    in
    let cleanup_class ~module_id class_name =
      let { ClassMetadataSharedMemory.Metadata.local_class_id; _ } =
        ClassMetadataSharedMemory.get class_metadata_shared_memory class_name
        |> assert_shared_memory_key_exists (fun () -> "missing class metadata")
      in
      ClassMetadataSharedMemory.remove class_metadata_shared_memory class_name;
      ClassFieldsSharedMemory.remove class_fields_shared_memory class_name;
      ClassDecoratorsSharedMemory.remove class_decorators_shared_memory class_name;
      ClassIdToQualifiedNameSharedMemory.remove
        class_id_to_qualified_name_shared_memory
        { GlobalClassId.module_id; local_class_id };
      ()
    in
    let cleanup_module module_qualifier =
      let { ModuleInfosSharedMemory.Module.module_id; pyrefly_info_filename; _ } =
        ModuleInfosSharedMemory.get module_infos_shared_memory module_qualifier
        |> assert_shared_memory_key_exists (fun () -> "missing module info")
      in
      ModuleInfosSharedMemory.remove module_infos_shared_memory module_qualifier;
      ModuleIdToQualifierSharedMemory.remove module_id_to_qualifier_shared_memory module_id;
      let () =
        if Option.is_some pyrefly_info_filename then
          ModuleCallablesSharedMemory.get module_callables_shared_memory module_qualifier
          |> assert_shared_memory_key_exists (fun () -> "missing module callables")
          |> List.iter ~f:(cleanup_callable ~module_id)
      in
      let () =
        if Option.is_some pyrefly_info_filename then
          ModuleClassesSharedMemory.get module_classes_shared_memory module_qualifier
          |> assert_shared_memory_key_exists (fun () -> "missing module classes")
          |> List.iter ~f:(cleanup_class ~module_id)
      in
      ModuleCallablesSharedMemory.remove module_callables_shared_memory module_qualifier;
      ModuleClassesSharedMemory.remove module_classes_shared_memory module_qualifier;
      ModuleGlobalsSharedMemory.remove module_globals_shared_memory module_qualifier;
      ()
    in
    Scheduler.map_reduce
      scheduler
      ~policy:scheduler_policy
      ~initial:()
      ~map:(List.iter ~f:cleanup_module)
      ~reduce:(fun () () -> ())
      ~inputs:(Map.keys qualifier_to_module_map)
      ();
    QualifiersSharedMemory.remove qualifiers_shared_memory Memory.SingletonKey.key;
    (* TODO(T225700656): Clean up TypeOfExpressionsSharedMemory (this requires storing all locations
       with a type) *)
    Memory.SharedMemory.collect `aggressive;
    ()
end

(* Read-only API that can be sent to workers. Cheap to copy. *)
module ReadOnly = struct
  type t = {
    pyrefly_directory: PyrePath.t;
    module_infos_shared_memory: ModuleInfosSharedMemory.t;
    qualifiers_shared_memory: QualifiersSharedMemory.t;
    callable_metadata_shared_memory: CallableMetadataSharedMemory.t;
    class_metadata_shared_memory: ClassMetadataSharedMemory.t;
    class_fields_shared_memory: ClassFieldsSharedMemory.t;
    class_decorators_shared_memory: ClassDecoratorsSharedMemory.t;
    module_callables_shared_memory: ModuleCallablesSharedMemory.t;
    module_classes_shared_memory: ModuleClassesSharedMemory.t;
    module_globals_shared_memory: ModuleGlobalsSharedMemory.t;
    callable_ast_shared_memory: CallableAstSharedMemory.t;
    callable_define_signature_shared_memory: CallableDefineSignatureSharedMemory.t;
    callable_undecorated_signatures_shared_memory: CallableUndecoratedSignaturesSharedMemory.t;
    type_of_expressions_shared_memory: TypeOfExpressionsSharedMemory.t option;
    module_id_to_qualifier_shared_memory: ModuleIdToQualifierSharedMemory.t;
    class_id_to_qualified_name_shared_memory: ClassIdToQualifiedNameSharedMemory.t;
    callable_id_to_qualified_name_shared_memory: CallableIdToQualifiedNameSharedMemory.t;
    object_class: FullyQualifiedName.t;
    dict_class_id: GlobalClassId.t;
    typing_mapping_class_id: GlobalClassId.t;
  }

  let of_read_write_api
      {
        ReadWrite.pyrefly_directory;
        module_infos_shared_memory;
        qualifiers_shared_memory;
        callable_metadata_shared_memory;
        class_metadata_shared_memory;
        class_fields_shared_memory;
        class_decorators_shared_memory;
        module_callables_shared_memory;
        module_classes_shared_memory;
        module_globals_shared_memory;
        callable_ast_shared_memory;
        callable_define_signature_shared_memory;
        callable_undecorated_signatures_shared_memory;
        type_of_expressions_shared_memory;
        module_id_to_qualifier_shared_memory;
        class_id_to_qualified_name_shared_memory;
        callable_id_to_qualified_name_shared_memory;
        object_class;
        dict_class_id;
        typing_mapping_class_id;
        _;
      }
    =
    {
      pyrefly_directory;
      module_infos_shared_memory;
      qualifiers_shared_memory;
      callable_metadata_shared_memory;
      class_metadata_shared_memory;
      class_fields_shared_memory;
      class_decorators_shared_memory;
      module_callables_shared_memory;
      module_classes_shared_memory;
      module_globals_shared_memory;
      callable_ast_shared_memory;
      callable_define_signature_shared_memory;
      callable_undecorated_signatures_shared_memory;
      type_of_expressions_shared_memory;
      module_id_to_qualifier_shared_memory;
      class_id_to_qualified_name_shared_memory;
      callable_id_to_qualified_name_shared_memory;
      object_class;
      dict_class_id;
      typing_mapping_class_id;
    }


  let artifact_path_of_qualifier { module_infos_shared_memory; _ } qualifier =
    if Reference.equal qualifier Analysis.PyrePysaEnvironment.artificial_decorator_define_module
    then
      None
    else
      ModuleInfosSharedMemory.get
        module_infos_shared_memory
        (ModuleQualifier.from_reference_unchecked qualifier)
      |> assert_shared_memory_key_exists (fun () -> "missing module info for qualifier")
      |> fun { ModuleInfosSharedMemory.Module.absolute_source_path; _ } -> absolute_source_path


  let absolute_source_path_of_qualifier api qualifier =
    (* TODO(T225700656): We currently return the artifact path, it should be translated back into a
       source path by buck *)
    artifact_path_of_qualifier api qualifier >>| ArtifactPath.raw >>| PyrePath.absolute


  let relative_path_of_qualifier { module_infos_shared_memory; _ } qualifier =
    ModuleInfosSharedMemory.get
      module_infos_shared_memory
      (ModuleQualifier.from_reference_unchecked qualifier)
    |> assert_shared_memory_key_exists (fun () -> "missing module info for qualifier")
    |> fun { ModuleInfosSharedMemory.Module.relative_source_path; _ } -> relative_source_path


  (* Return all qualifiers with source code *)
  let explicit_qualifiers { qualifiers_shared_memory; _ } =
    QualifiersSharedMemory.get qualifiers_shared_memory Memory.SingletonKey.key
    |> assert_shared_memory_key_exists (fun () -> "missing qualifiers with source in shared memory")
    |> List.filter_map ~f:(fun { QualifiersSharedMemory.Value.module_qualifier; has_source; _ } ->
           Option.some_if has_source (ModuleQualifier.to_reference module_qualifier))


  let is_test_qualifier { module_infos_shared_memory; _ } qualifier =
    ModuleInfosSharedMemory.get
      module_infos_shared_memory
      (ModuleQualifier.from_reference_unchecked qualifier)
    |> assert_shared_memory_key_exists (fun () -> "missing module info for qualifier")
    |> fun { ModuleInfosSharedMemory.Module.is_test; _ } -> is_test


  let is_stub_qualifier { module_infos_shared_memory; _ } qualifier =
    ModuleInfosSharedMemory.get
      module_infos_shared_memory
      (ModuleQualifier.from_reference_unchecked qualifier)
    |> assert_shared_memory_key_exists (fun () -> "missing module info for qualifier")
    |> fun { ModuleInfosSharedMemory.Module.is_stub; _ } -> is_stub


  let get_class_names_for_qualifier
      ({ module_classes_shared_memory; _ } as api)
      ~exclude_test_modules
      qualifier
    =
    if exclude_test_modules && is_test_qualifier api qualifier then
      []
    else
      ModuleClassesSharedMemory.get
        module_classes_shared_memory
        (ModuleQualifier.from_reference_unchecked qualifier)
      |> assert_shared_memory_key_exists (fun () -> "missing module classes for qualifier")
      |> List.map ~f:FullyQualifiedName.to_reference


  let all_classes { qualifiers_shared_memory; module_classes_shared_memory; _ } ~scheduler =
    let qualifiers =
      QualifiersSharedMemory.get qualifiers_shared_memory Memory.SingletonKey.key
      |> assert_shared_memory_key_exists (fun () ->
             "missing qualifiers with source in shared memory")
      |> List.filter_map ~f:(fun { QualifiersSharedMemory.Value.module_qualifier; has_info; _ } ->
             Option.some_if has_info module_qualifier)
    in
    let get_class_names_for_qualifier module_qualifier =
      ModuleClassesSharedMemory.get module_classes_shared_memory module_qualifier
      |> assert_shared_memory_key_exists (fun () -> "missing module classes for qualifier")
      |> List.map ~f:FullyQualifiedName.to_reference
      |> List.map ~f:Reference.show
    in
    let scheduler_policy =
      Scheduler.Policy.fixed_chunk_count
        ~minimum_chunks_per_worker:1
        ~minimum_chunk_size:1
        ~preferred_chunks_per_worker:1
        ()
    in
    Scheduler.map_reduce
      scheduler
      ~policy:scheduler_policy
      ~initial:[]
      ~map:(List.concat_map ~f:get_class_names_for_qualifier)
      ~reduce:List.append
      ~inputs:qualifiers
      ()


  let all_global_variables { qualifiers_shared_memory; module_globals_shared_memory; _ } ~scheduler =
    let qualifiers =
      QualifiersSharedMemory.get qualifiers_shared_memory Memory.SingletonKey.key
      |> assert_shared_memory_key_exists (fun () ->
             "missing qualifiers with source in shared memory")
      |> List.filter_map ~f:(fun { QualifiersSharedMemory.Value.module_qualifier; has_info; _ } ->
             Option.some_if has_info module_qualifier)
    in
    let get_globals_for_qualifier module_qualifier =
      ModuleGlobalsSharedMemory.get module_globals_shared_memory module_qualifier
      |> assert_shared_memory_key_exists (fun () -> "missing module globals for qualifier")
      |> SerializableStringMap.keys
      |> List.map ~f:(Reference.create ~prefix:(ModuleQualifier.to_reference module_qualifier))
    in
    let scheduler_policy =
      Scheduler.Policy.fixed_chunk_count
        ~minimum_chunks_per_worker:1
        ~minimum_chunk_size:1
        ~preferred_chunks_per_worker:1
        ()
    in
    Scheduler.map_reduce
      scheduler
      ~policy:scheduler_policy
      ~initial:[]
      ~map:(List.concat_map ~f:get_globals_for_qualifier)
      ~reduce:List.append
      ~inputs:qualifiers
      ()


  let get_define_names_for_qualifier
      ({ module_callables_shared_memory; _ } as api)
      ~exclude_test_modules
      qualifier
    =
    if exclude_test_modules && is_test_qualifier api qualifier then
      []
    else
      ModuleCallablesSharedMemory.get
        module_callables_shared_memory
        (ModuleQualifier.from_reference_unchecked qualifier)
      |> assert_shared_memory_key_exists (fun () -> "missing module callables for qualifier")
      |> List.map ~f:FullyQualifiedName.to_reference


  let get_qualifier_top_level_define_name _ qualifier =
    let module_qualifier = ModuleQualifier.from_reference_unchecked qualifier in
    FullyQualifiedName.create_module_toplevel ~module_qualifier |> FullyQualifiedName.to_reference


  let class_immediate_parents
      { class_metadata_shared_memory; class_id_to_qualified_name_shared_memory; object_class; _ }
      class_name
    =
    (* TOOD(T225700656): Update the API to take a reference and return a reference. *)
    let class_name = FullyQualifiedName.from_reference_unchecked (Reference.create class_name) in
    let get_parents_from_class_metadata { ClassMetadataSharedMemory.Metadata.parents; _ } =
      match parents with
      | [] when not (FullyQualifiedName.equal class_name object_class) -> [object_class]
      | parents ->
          List.map
            ~f:
              (ClassIdToQualifiedNameSharedMemory.get_class_name
                 class_id_to_qualified_name_shared_memory)
            parents
    in
    ClassMetadataSharedMemory.get class_metadata_shared_memory class_name
    |> assert_shared_memory_key_exists (fun () -> "missing class metadata for class")
    |> get_parents_from_class_metadata
    |> List.map ~f:FullyQualifiedName.to_reference
    |> List.map ~f:Reference.show


  let class_mro
      { class_metadata_shared_memory; class_id_to_qualified_name_shared_memory; object_class; _ }
      class_name
    =
    (* TOOD(T225700656): Update the API to take a reference and return a reference. *)
    let class_name = FullyQualifiedName.from_reference_unchecked (Reference.create class_name) in
    let get_mro_from_class_metadata { ClassMetadataSharedMemory.Metadata.mro; _ } =
      match mro with
      | _ when FullyQualifiedName.equal class_name object_class -> []
      | ModuleDefinitionsFile.ClassMro.Cyclic ->
          (* Failed to resolve the mro because the class hierarchy is cyclic. Fallback to
             [object]. *)
          [object_class]
      | ModuleDefinitionsFile.ClassMro.Resolved mro ->
          let mro =
            List.map
              ~f:
                (ClassIdToQualifiedNameSharedMemory.get_class_name
                   class_id_to_qualified_name_shared_memory)
              mro
          in
          (* Pyrefly does not include 'object' in the mro. *)
          mro @ [object_class]
    in
    ClassMetadataSharedMemory.get class_metadata_shared_memory class_name
    |> assert_shared_memory_key_exists (fun () -> "missing class metadata for class")
    |> get_mro_from_class_metadata
    |> List.map ~f:FullyQualifiedName.to_reference
    |> List.map ~f:Reference.show


  let get_callable_metadata { callable_metadata_shared_memory; _ } define_name =
    CallableMetadataSharedMemory.get
      callable_metadata_shared_memory
      (FullyQualifiedName.from_reference_unchecked define_name)
    |> assert_shared_memory_key_exists (fun () ->
           Format.asprintf "missing callable metadata: `%a`" Reference.pp define_name)
    |> fun { CallableMetadataSharedMemory.Value.metadata; _ } -> metadata


  let get_overriden_base_method
      { callable_metadata_shared_memory; callable_id_to_qualified_name_shared_memory; _ }
      method_reference
    =
    match method_reference with
    | Analysis.PyrePysaEnvironment.MethodReference.Pyre1 _ -> None
    | Analysis.PyrePysaEnvironment.MethodReference.Pyrefly { define_name; is_property_setter } ->
        CallableMetadataSharedMemory.get
          callable_metadata_shared_memory
          (FullyQualifiedName.from_reference_unchecked define_name)
        |> assert_shared_memory_key_exists (fun () ->
               Format.asprintf "missing callable metadata: `%a`" Reference.pp define_name)
        |> fun { CallableMetadataSharedMemory.Value.overridden_base_method; _ } ->
        overridden_base_method
        >>| CallableIdToQualifiedNameSharedMemory.get callable_id_to_qualified_name_shared_memory
        >>| fun define_name ->
        Analysis.PyrePysaEnvironment.MethodReference.Pyrefly
          { define_name = FullyQualifiedName.to_reference define_name; is_property_setter }


  let get_callable_captures
      { callable_metadata_shared_memory; callable_id_to_qualified_name_shared_memory; _ }
      define_name
    =
    CallableMetadataSharedMemory.get
      callable_metadata_shared_memory
      (FullyQualifiedName.from_reference_unchecked define_name)
    |> assert_shared_memory_key_exists (fun () ->
           Format.asprintf "missing callable metadata: `%a`" Reference.pp define_name)
    |> fun { CallableMetadataSharedMemory.Value.captured_variables; _ } ->
    List.map
      ~f:(fun { CapturedVariable.name; outer_function } ->
        AccessPath.CapturedVariable.FromFunction
          {
            name;
            defining_function =
              CallableIdToQualifiedNameSharedMemory.get
                callable_id_to_qualified_name_shared_memory
                outer_function
              |> FullyQualifiedName.to_reference;
          })
      captured_variables


  let get_callable_return_annotations
      { callable_undecorated_signatures_shared_memory; _ }
      ~define_name
      ~define:_
    =
    CallableUndecoratedSignaturesSharedMemory.get
      callable_undecorated_signatures_shared_memory
      (FullyQualifiedName.from_reference_unchecked define_name)
    |> assert_shared_memory_key_exists (fun () ->
           Format.asprintf "missing callable metadata: `%a`" Reference.pp define_name)
    |> List.map ~f:(fun { FunctionSignature.return_annotation; _ } -> return_annotation)


  let get_callable_parameter_annotations
      { callable_undecorated_signatures_shared_memory; _ }
      ~define_name
      parameters
    =
    let signatures =
      CallableUndecoratedSignaturesSharedMemory.get
        callable_undecorated_signatures_shared_memory
        (FullyQualifiedName.from_reference_unchecked define_name)
      |> assert_shared_memory_key_exists (fun () ->
             Format.asprintf "missing callable metadata: `%a`" Reference.pp define_name)
    in
    let normalize_root = function
      | AccessPath.Root.PositionalParameter { position; positional_only = true; _ } ->
          AccessPath.Root.PositionalParameter { position; positional_only = true; name = "" }
      | AccessPath.Root.StarStarParameter _ ->
          AccessPath.Root.StarStarParameter { excluded = Identifier.SerializableSet.empty }
      | root -> root
    in
    let fold_signature_parameter sofar parameter =
      let root = normalize_root (FunctionParameter.root parameter) in
      match FunctionParameter.annotation parameter with
      | None -> sofar
      | Some annotation ->
          AccessPath.Root.Map.update
            root
            (function
              | None -> Some [annotation]
              | Some existing -> Some (annotation :: existing))
            sofar
    in
    let fold_signatures sofar { FunctionSignature.parameters; _ } =
      match parameters with
      | FunctionParameters.Ellipsis
      | FunctionParameters.ParamSpec ->
          sofar
      | FunctionParameters.List signature_parameters ->
          List.fold ~init:sofar ~f:fold_signature_parameter signature_parameters
    in
    let root_annotations_map =
      List.fold ~init:AccessPath.Root.Map.empty ~f:fold_signatures signatures
    in
    List.map
      parameters
      ~f:(fun ({ AccessPath.NormalizedParameter.root; _ } as normalized_parameter) ->
        let annotations =
          AccessPath.Root.Map.find_opt (normalize_root root) root_annotations_map
          |> Option.value ~default:[]
        in
        normalized_parameter, annotations)


  let get_callable_decorator_callees
      { callable_metadata_shared_memory; callable_id_to_qualified_name_shared_memory; _ }
      define_name
      location
    =
    CallableMetadataSharedMemory.get
      callable_metadata_shared_memory
      (FullyQualifiedName.from_reference_unchecked define_name)
    |> assert_shared_memory_key_exists (fun () ->
           Format.asprintf "missing callable metadata: `%a`" Reference.pp define_name)
    |> (fun { CallableMetadataSharedMemory.Value.decorator_callees; _ } -> decorator_callees)
    |> Location.SerializableMap.find_opt location
    >>| List.map
          ~f:(CallableIdToQualifiedNameSharedMemory.get callable_id_to_qualified_name_shared_memory)
    >>| List.map ~f:FullyQualifiedName.to_reference


  let get_methods_for_qualifier
      ({ module_callables_shared_memory; callable_metadata_shared_memory; _ } as api)
      ~exclude_test_modules
      qualifier
    =
    if exclude_test_modules && is_test_qualifier api qualifier then
      []
    else
      let convert_to_method_reference callable =
        callable
        |> CallableMetadataSharedMemory.get callable_metadata_shared_memory
        |> assert_shared_memory_key_exists (fun () ->
               Format.asprintf "missing callable metadata: `%a`" FullyQualifiedName.pp callable)
        |> fun { CallableMetadataSharedMemory.Value.metadata = { is_property_setter; _ }; _ } ->
        Analysis.PyrePysaEnvironment.MethodReference.Pyrefly
          { define_name = FullyQualifiedName.to_reference callable; is_property_setter }
      in
      ModuleCallablesSharedMemory.get
        module_callables_shared_memory
        (ModuleQualifier.from_reference_unchecked qualifier)
      |> assert_shared_memory_key_exists (fun () -> "missing module callables for qualifier")
      |> List.map ~f:convert_to_method_reference


  let get_define_opt { callable_ast_shared_memory; _ } define_name =
    CallableAstSharedMemory.get
      callable_ast_shared_memory
      (FullyQualifiedName.from_reference_unchecked define_name)
    |> assert_shared_memory_key_exists (fun () -> "missing callable ast")


  let get_undecorated_signatures { callable_undecorated_signatures_shared_memory; _ } define_name =
    CallableUndecoratedSignaturesSharedMemory.get
      callable_undecorated_signatures_shared_memory
      (FullyQualifiedName.from_reference_unchecked define_name)
    |> assert_shared_memory_key_exists (fun () -> "missing callable undecorated signature")


  let get_class_summary { class_metadata_shared_memory; _ } class_name =
    let class_name = FullyQualifiedName.from_reference_unchecked (Reference.create class_name) in
    let metadata =
      ClassMetadataSharedMemory.get class_metadata_shared_memory class_name
      |> assert_shared_memory_key_exists (fun () -> "missing class metadata for class")
    in
    { PysaClassSummary.class_name; metadata }


  let get_class_decorators_opt { class_decorators_shared_memory; _ } class_name =
    ClassDecoratorsSharedMemory.get
      class_decorators_shared_memory
      (FullyQualifiedName.from_reference_unchecked (Reference.create class_name))
    |> assert_shared_memory_key_exists (fun () -> "missing callable ast")


  let get_class_attributes
      { class_fields_shared_memory; _ }
      ~include_generated_attributes:_
      ~only_simple_assignments:_
      class_name
    =
    (* TODO(T225700656): Support include_generated_attributes and only_simple_assignments
       options. *)
    ClassFieldsSharedMemory.get
      class_fields_shared_memory
      (FullyQualifiedName.from_reference_unchecked (Reference.create class_name))
    >>| SerializableStringMap.keys


  let get_class_attribute_inferred_type { class_fields_shared_memory; _ } ~class_name ~attribute =
    ClassFieldsSharedMemory.get
      class_fields_shared_memory
      (FullyQualifiedName.from_reference_unchecked (Reference.create class_name))
    |> assert_shared_memory_key_exists (fun () -> "missing class fields for class")
    |> SerializableStringMap.find_opt attribute
    |> assert_shared_memory_key_exists (fun () -> "missing class field")
    |> fun { ClassField.type_; _ } -> type_


  let get_class_attribute_explicit_annotation
      { class_fields_shared_memory; _ }
      ~class_name
      ~attribute
    =
    ClassFieldsSharedMemory.get
      class_fields_shared_memory
      (FullyQualifiedName.from_reference_unchecked (Reference.create class_name))
    |> assert_shared_memory_key_exists (fun () -> "missing class fields for class")
    |> SerializableStringMap.find_opt attribute
    |> assert_shared_memory_key_exists (fun () -> "missing class field")
    |> fun { ClassField.explicit_annotation; _ } -> explicit_annotation


  let get_global_inferred_type { module_globals_shared_memory; _ } ~qualifier ~name =
    ModuleGlobalsSharedMemory.get
      module_globals_shared_memory
      (ModuleQualifier.from_reference_unchecked qualifier)
    |> assert_shared_memory_key_exists (fun () -> "missing module globals")
    |> SerializableStringMap.find_opt name
    |> assert_shared_memory_key_exists (fun () -> "missing global variable")
    |> fun { GlobalVariable.type_; _ } -> type_


  let target_from_define_name api ~override define_name =
    let { CallableMetadata.is_property_setter; parent_is_class; _ } =
      get_callable_metadata api define_name
    in
    let kind =
      if is_property_setter then
        Target.PyreflyPropertySetter
      else
        Target.Normal
    in
    if parent_is_class then
      if override then
        Target.create_override_from_reference ~kind define_name
      else
        Target.create_method_from_reference ~kind define_name
    else
      Target.create_function ~kind define_name


  let instantiate_call_graph
      ({
         module_id_to_qualifier_shared_memory;
         callable_id_to_qualified_name_shared_memory;
         class_id_to_qualified_name_shared_memory;
         _;
       } as api)
      ~method_has_overrides
      ~global_is_string_literal
      ~attribute_targets
      ~callable
      json_call_graph
    =
    let open ModuleCallGraphs in
    let open CallGraph in
    let rec instantiate_target = function
      | PyreflyTarget.Function callable_id ->
          let fully_qualified_name =
            CallableIdToQualifiedNameSharedMemory.get
              callable_id_to_qualified_name_shared_memory
              callable_id
          in
          let target =
            target_from_define_name
              api
              ~override:false
              (FullyQualifiedName.to_reference fully_qualified_name)
          in
          [target]
      | PyreflyTarget.AllOverrides callable_id ->
          let fully_qualified_name =
            CallableIdToQualifiedNameSharedMemory.get
              callable_id_to_qualified_name_shared_memory
              callable_id
          in
          let target =
            target_from_define_name
              api
              ~override:true
              (FullyQualifiedName.to_reference fully_qualified_name)
          in
          let target_method =
            match target with
            | Target.Regular (Target.Regular.Override target_method) -> target_method
            | _ -> failwith "unreachable"
          in
          if not (method_has_overrides target_method) then
            (* Pyrefly believes this method has overrides, but the override graph disagrees. This
               must mean we have a model with `@SkipOverrides`. *)
            [Target.Regular (Target.Regular.Method target_method)]
          else
            [target]
      | PyreflyTarget.OverrideSubset { base_method = base_method_id; subset } ->
          let fully_qualified_base_method =
            CallableIdToQualifiedNameSharedMemory.get
              callable_id_to_qualified_name_shared_memory
              base_method_id
          in
          let target =
            target_from_define_name
              api
              ~override:false
              (FullyQualifiedName.to_reference fully_qualified_base_method)
          in
          let target_method =
            match target with
            | Target.Regular (Target.Regular.Method target_method) -> target_method
            | _ -> failwith "unreachable"
          in
          if not (method_has_overrides target_method) then
            (* Pyrefly believes this method has overrides, but the override graph disagrees. This
               must mean we have a model with `@SkipOverrides`. *)
            [target]
          else
            subset |> List.map ~f:instantiate_target |> List.concat |> List.cons target
      | PyreflyTarget.FormatString -> [Target.ArtificialTargets.format_string]
    in
    let instantiate_call_target
        {
          JsonCallTarget.target;
          implicit_receiver;
          implicit_dunder_call;
          receiver_class;
          is_class_method;
          is_static_method;
          return_type;
        }
      =
      let targets = instantiate_target target in
      List.map
        ~f:(fun target ->
          {
            CallTarget.target;
            implicit_receiver = JsonImplicitReceiver.is_true implicit_receiver;
            implicit_dunder_call;
            index = 0;
            receiver_class =
              receiver_class
              >>| ClassIdToQualifiedNameSharedMemory.get_class_name
                    class_id_to_qualified_name_shared_memory
              >>| FullyQualifiedName.to_reference
              >>| Ast.Reference.show;
            is_class_method;
            is_static_method;
            return_type = Some return_type;
          })
        targets
    in
    let instantiate_global_target { JsonGlobalVariable.module_id; name } =
      let module_qualifier =
        ModuleIdToQualifierSharedMemory.get_module_qualifier
          module_id_to_qualifier_shared_memory
          module_id
      in
      let object_reference =
        Reference.create ~prefix:(ModuleQualifier.to_reference module_qualifier) name
      in
      let object_target = Target.create_object object_reference in
      if Target.Set.mem object_target attribute_targets || global_is_string_literal object_reference
      then
        Some (CallTarget.create ~return_type:None object_target)
      else
        None
    in
    let instantiate_captured_variable { CapturedVariable.name; outer_function } =
      AccessPath.CapturedVariable.FromFunction
        {
          name;
          defining_function =
            CallableIdToQualifiedNameSharedMemory.get
              callable_id_to_qualified_name_shared_memory
              outer_function
            |> FullyQualifiedName.to_reference;
        }
    in
    let instantiate_higher_order_parameter
        { JsonHigherOrderParameter.index; call_targets; unresolved }
      =
      {
        CallGraph.HigherOrderParameter.index;
        call_targets = call_targets |> List.map ~f:instantiate_call_target |> List.concat;
        unresolved;
      }
    in
    let instantiate_call_callees
        {
          JsonCallCallees.call_targets;
          init_targets;
          new_targets;
          higher_order_parameters;
          unresolved;
        }
      =
      (* TODO(T225700656): Support higher order parameters, shim targets, unresolved calles. *)
      {
        CallCallees.call_targets =
          call_targets |> List.map ~f:instantiate_call_target |> List.concat;
        init_targets = init_targets |> List.map ~f:instantiate_call_target |> List.concat;
        new_targets = new_targets |> List.map ~f:instantiate_call_target |> List.concat;
        decorated_targets = [];
        higher_order_parameters =
          higher_order_parameters
          |> JsonHigherOrderParameterMap.data
          |> List.map ~f:instantiate_higher_order_parameter
          |> CallGraph.HigherOrderParameterMap.from_list;
        shim_target = None;
        unresolved;
        recognized_call = CallGraph.CallCallees.RecognizedCall.False;
      }
    in
    let instantiate_identifier_callees
        { JsonIdentifierCallees.if_called; global_targets; captured_variables }
      =
      let if_called = instantiate_call_callees if_called in
      let global_targets = List.filter_map ~f:instantiate_global_target global_targets in
      let captured_variables = List.map ~f:instantiate_captured_variable captured_variables in
      { IdentifierCallees.global_targets; captured_variables; if_called }
    in
    let instantiate_attribute_access_callees
        {
          JsonAttributeAccessCallees.if_called;
          property_setters;
          property_getters;
          global_targets;
          is_attribute;
        }
      =
      let if_called = instantiate_call_callees if_called in
      let global_targets = List.filter_map ~f:instantiate_global_target global_targets in
      {
        AttributeAccessCallees.property_targets =
          List.append property_setters property_getters
          |> List.map ~f:instantiate_call_target
          |> List.concat;
        global_targets;
        is_attribute;
        if_called;
      }
    in
    let instantiate_define_callees { JsonDefineCallees.define_targets } =
      {
        CallGraph.DefineCallees.define_targets =
          define_targets |> List.map ~f:instantiate_call_target |> List.concat;
        decorated_targets = [];
      }
    in
    let instantiate_format_string_artificial_callees { JsonFormatStringArtificialCallees.targets } =
      {
        CallGraph.FormatStringArtificialCallees.targets =
          targets |> List.map ~f:instantiate_call_target |> List.concat;
      }
    in
    let instantiate_format_string_stringify_callees
        { JsonFormatStringStringifyCallees.targets; unresolved = _ }
      =
      {
        CallGraph.FormatStringStringifyCallees.targets =
          targets |> List.map ~f:instantiate_call_target |> List.concat;
      }
    in
    let instantiate_return_shim_callees { JsonReturnShimCallees.targets; arguments } =
      {
        CallGraph.ReturnShimCallees.call_targets =
          targets |> List.map ~f:instantiate_call_target |> List.concat;
        arguments;
      }
    in
    let instantiate_expression_callees = function
      | JsonExpressionCallees.Call callees ->
          ExpressionCallees.Call (instantiate_call_callees callees)
      | JsonExpressionCallees.Identifier callees ->
          ExpressionCallees.Identifier (instantiate_identifier_callees callees)
      | JsonExpressionCallees.AttributeAccess callees ->
          ExpressionCallees.AttributeAccess (instantiate_attribute_access_callees callees)
      | JsonExpressionCallees.Define callees ->
          ExpressionCallees.Define (instantiate_define_callees callees)
      | JsonExpressionCallees.FormatStringArtificial callees ->
          ExpressionCallees.FormatStringArtificial
            (instantiate_format_string_artificial_callees callees)
      | JsonExpressionCallees.FormatStringStringify callees ->
          ExpressionCallees.FormatStringStringify
            (instantiate_format_string_stringify_callees callees)
      | JsonExpressionCallees.Return callees ->
          ExpressionCallees.Return (instantiate_return_shim_callees callees)
    in
    let instantiate_call_edge expression_identifier callees call_graph =
      let callees = instantiate_expression_callees callees in
      DefineCallGraph.add_callees
        ~debug:false
        ~caller:callable
        ~on_existing_callees:DefineCallGraph.OnExistingCallees.Fail
        ~expression_identifier
        ~callees
        ~expression_for_logging:None
        call_graph
    in
    ExpressionIdentifier.Map.fold instantiate_call_edge json_call_graph DefineCallGraph.empty


  let parse_call_graphs
      ({ pyrefly_directory; callable_metadata_shared_memory; module_infos_shared_memory; _ } as api)
      ~scheduler
      ~scheduler_policies
      ~method_has_overrides
      ~global_is_string_literal
      ~store_shared_memory
      ~attribute_targets
      ~skip_analysis_targets
      ~definitions
      ~create_dependency_for
      ~redirect_to_decorated
      ~transform_call_graph
    =
    let timer = Timer.start () in
    let () = Log.info "Parsing call graphs from pyrefly..." in
    let module CallableWithId = struct
      type t = {
        callable: Target.t;
        local_function_id: LocalFunctionId.t;
      }
    end
    in
    (* Partition functions per module *)
    let module_definitions_map =
      let fold_definition module_definitions_map callable =
        if Target.should_skip_analysis ~skip_analysis_targets callable then
          module_definitions_map
        else
          let {
            CallableMetadataSharedMemory.Value.metadata = { module_qualifier; _ };
            local_function_id;
            _;
          }
            =
            Target.define_name_exn callable
            |> FullyQualifiedName.from_reference_unchecked
            |> CallableMetadataSharedMemory.get callable_metadata_shared_memory
            |> assert_shared_memory_key_exists (fun () ->
                   Format.asprintf
                     "missing callable metadata: `%a`"
                     Target.pp_pretty_with_kind
                     callable)
          in
          let module_definitions_map =
            Map.update
              module_definitions_map
              (ModuleQualifier.from_reference_unchecked module_qualifier)
              ~f:(fun sofar ->
                { CallableWithId.callable; local_function_id } :: Option.value ~default:[] sofar)
          in
          let module_definitions_map =
            match redirect_to_decorated callable with
            | Some decorated_target ->
                let decorated_function_id =
                  match local_function_id with
                  | LocalFunctionId.Function position ->
                      LocalFunctionId.FunctionDecoratedTarget position
                  | _ ->
                      Format.asprintf
                        "Unexpected local function id for decorated function: `%a`"
                        LocalFunctionId.pp
                        local_function_id
                      |> failwith
                in
                Map.update
                  module_definitions_map
                  (ModuleQualifier.from_reference_unchecked module_qualifier)
                  ~f:(fun sofar ->
                    {
                      CallableWithId.callable = decorated_target;
                      local_function_id = decorated_function_id;
                    }
                    :: Option.value ~default:[] sofar)
            | None -> module_definitions_map
          in
          module_definitions_map
      in
      List.fold ~init:ModuleQualifier.Map.empty ~f:fold_definition definitions
    in
    (* Map-reduce over all modules *)
    let scheduler_policy =
      Scheduler.Policy.from_configuration_or_default
        scheduler_policies
        Configuration.ScheduleIdentifier.PyreflyParseCallGraphs
        ~default:
          (Scheduler.Policy.fixed_chunk_count
             ~minimum_chunks_per_worker:1
             ~minimum_chunk_size:1
             ~preferred_chunks_per_worker:1
             ())
    in
    let parse_module_call_graphs sofar (module_qualifier, callables) =
      let { ModuleInfosSharedMemory.Module.pyrefly_info_filename; _ } =
        ModuleInfosSharedMemory.get module_infos_shared_memory module_qualifier
        |> assert_shared_memory_key_exists (fun () -> "missing module info")
      in
      let pyrefly_info_filename =
        match pyrefly_info_filename with
        | None ->
            Format.asprintf
              "Could not find call graphs for module `%a`: missing pyrefly info file"
              ModuleQualifier.pp
              module_qualifier
            |> failwith
        | Some pyrefly_info_filename -> pyrefly_info_filename
      in
      let { ModuleCallGraphs.call_graphs = function_id_to_call_graph_map; module_id = _ } =
        ModuleCallGraphs.from_path_exn ~pyrefly_directory pyrefly_info_filename
      in
      List.fold
        ~init:sofar
        ~f:
          (fun (define_call_graphs, whole_program_call_graph)
               { CallableWithId.callable; local_function_id } ->
          match Map.find function_id_to_call_graph_map local_function_id with
          | None ->
              Format.asprintf "Could not find call graph for `%a`" Target.pp callable |> failwith
          | Some call_graph ->
              let call_graph =
                instantiate_call_graph
                  api
                  ~method_has_overrides
                  ~global_is_string_literal
                  ~attribute_targets
                  ~callable
                  call_graph
                |> transform_call_graph api callable
              in
              let define_call_graphs =
                if store_shared_memory then
                  CallGraph.SharedMemory.AddOnly.add define_call_graphs callable call_graph
                else
                  define_call_graphs
              in
              let whole_program_call_graph =
                CallGraph.WholeProgramCallGraph.add_or_exn
                  whole_program_call_graph
                  ~callable
                  ~callees:
                    (CallGraph.DefineCallGraph.all_targets
                       ~use_case:create_dependency_for
                       call_graph)
              in
              define_call_graphs, whole_program_call_graph)
        callables
    in
    let reduce
        (left_define_call_graphs, left_whole_program_call_graph)
        (right_define_call_graphs, right_whole_program_call_graph)
      =
      ( CallGraph.SharedMemory.AddOnly.merge_same_handle_disjoint_keys
          ~smaller:left_define_call_graphs
          ~larger:right_define_call_graphs,
        CallGraph.WholeProgramCallGraph.merge_disjoint
          left_whole_program_call_graph
          right_whole_program_call_graph )
    in
    let define_call_graphs = CallGraph.SharedMemory.create () |> CallGraph.SharedMemory.add_only in
    let empty_define_call_graphs = CallGraph.SharedMemory.AddOnly.create_empty define_call_graphs in
    let define_call_graphs, whole_program_call_graph =
      Scheduler.map_reduce
        scheduler
        ~policy:scheduler_policy
        ~initial:(empty_define_call_graphs, CallGraph.WholeProgramCallGraph.empty)
        ~map:(fun modules_and_callables ->
          List.fold
            modules_and_callables
            ~init:(empty_define_call_graphs, CallGraph.WholeProgramCallGraph.empty)
            ~f:parse_module_call_graphs)
        ~reduce
        ~inputs:(Map.to_alist module_definitions_map)
        ()
    in
    Log.info "Parsed call graphs from pyrefly: %.3fs" (Timer.stop_in_sec timer);
    Statistics.performance
      ~name:"Parsed call graphs from pyrefly"
      ~phase_name:"Parsing call graphs from pyrefly"
      ~timer
      ();
    let define_call_graphs = CallGraph.SharedMemory.from_add_only define_call_graphs in
    { CallGraph.SharedMemory.whole_program_call_graph; define_call_graphs }


  let parse_type_errors
      { pyrefly_directory; module_id_to_qualifier_shared_memory; module_infos_shared_memory; _ }
    =
    let instantiate_error
        {
          TypeErrors.JsonError.module_id;
          location =
            {
              start = { line = start_line; column = start_column };
              stop = { line = stop_line; column = stop_column };
            };
          kind;
          message;
        }
      =
      let module_qualifier =
        ModuleIdToQualifierSharedMemory.get_module_qualifier
          module_id_to_qualifier_shared_memory
          module_id
      in
      let { ModuleInfosSharedMemory.Module.relative_source_path; _ } =
        ModuleInfosSharedMemory.get module_infos_shared_memory module_qualifier
        |> assert_shared_memory_key_exists (fun () -> "invalid module id")
      in
      {
        Analysis.AnalysisError.Instantiated.line = start_line;
        column = start_column;
        stop_line;
        stop_column;
        path = Option.value ~default:"?" relative_source_path;
        code = 0;
        name = kind;
        description = message;
        concise_description = message;
        define = "";
      }
    in
    let { TypeErrors.errors } = TypeErrors.from_path_exn ~pyrefly_directory in
    List.map ~f:instantiate_error errors


  let get_type_of_expression { type_of_expressions_shared_memory; _ } ~qualifier ~location =
    match type_of_expressions_shared_memory with
    | None ->
        failwith
          "Using `PyreflyApi.ReadOnly.get_type_of_expression` before calling \
           `parse_type_of_expressions`."
    | Some type_of_expressions_shared_memory ->
        TypeOfExpressionsSharedMemory.get
          type_of_expressions_shared_memory
          {
            TypeOfExpressionsSharedMemory.Key.module_qualifier =
              ModuleQualifier.from_reference_unchecked qualifier;
            location;
          }


  module Type = struct
    let scalar_properties _ pysa_type =
      match PysaType.as_pyrefly_type pysa_type with
      | None ->
          failwith "ReadOnly.Type.type_properties: trying to use a pyre1 type with a pyrefly API."
      | Some { PyreflyType.scalar_properties; _ } -> scalar_properties


    let get_class_names { class_id_to_qualified_name_shared_memory; _ } pysa_type =
      match PysaType.as_pyrefly_type pysa_type with
      | None ->
          failwith "ReadOnly.Type.get_class_names: trying to use a pyre1 type with a pyrefly API."
      | Some { PyreflyType.class_names = None; _ } ->
          Analysis.PyrePysaEnvironment.ClassNamesFromType.not_a_class
      | Some { PyreflyType.class_names = Some { classes; is_exhaustive }; _ } ->
          let get_class_with_modifiers
              { Pyre1Api.PyreflyType.ClassWithModifiers.module_id; class_id; modifiers }
            =
            let class_name =
              ClassIdToQualifiedNameSharedMemory.get
                class_id_to_qualified_name_shared_memory
                {
                  GlobalClassId.module_id = ModuleId.from_int module_id;
                  local_class_id = LocalClassId.from_int class_id;
                }
              |> assert_shared_memory_key_exists (fun () -> "missing class id")
              |> FullyQualifiedName.to_reference
              |> Reference.show
            in
            { Pyre1Api.ClassWithModifiers.class_name; modifiers }
          in
          {
            Analysis.PyrePysaEnvironment.ClassNamesFromType.classes =
              List.map ~f:get_class_with_modifiers classes;
            is_exhaustive;
          }


    let is_dictionary_or_mapping { dict_class_id; typing_mapping_class_id; _ } pysa_type =
      match PysaType.as_pyrefly_type pysa_type with
      | None ->
          failwith
            "ReadOnly.Type.is_dictionary_or_mapping: trying to use a pyre1 type with a pyrefly API."
      | Some { PyreflyType.class_names = None; _ } -> false
      | Some { PyreflyType.class_names = Some { classes; _ }; _ } ->
          List.exists
            classes
            ~f:(fun { PyreflyType.ClassWithModifiers.module_id; class_id; modifiers } ->
              let global_class_id =
                {
                  GlobalClassId.module_id = ModuleId.from_int module_id;
                  local_class_id = LocalClassId.from_int class_id;
                }
              in
              List.for_all modifiers ~f:(Pyre1Api.TypeModifier.equal Pyre1Api.TypeModifier.Optional)
              && (GlobalClassId.equal global_class_id dict_class_id
                 || GlobalClassId.equal global_class_id typing_mapping_class_id))
  end

  module ClassSummary = struct
    let has_custom_new { callable_metadata_shared_memory; _ } { PysaClassSummary.class_name; _ } =
      (* TODO(T225700656): We don't currently store a mapping from class to methods. For now, we use
         a dirty solution, we just check if class_name + __new__ is a valid function name. This
         might not work in tricky cases (if the class name clashes with a module name, for
         instance) *)
      let class_name = FullyQualifiedName.to_reference class_name in
      let method_name =
        Ast.Reference.create ~prefix:class_name "__new__"
        |> FullyQualifiedName.from_reference_unchecked
      in
      CallableMetadataSharedMemory.get callable_metadata_shared_memory method_name
      |> function
      | None -> false
      | Some { CallableMetadataSharedMemory.Value.metadata = { is_def_statement; _ }; _ } ->
          is_def_statement


    let is_dataclass _ { PysaClassSummary.metadata = { is_dataclass; _ }; _ } = is_dataclass

    let is_named_tuple _ { PysaClassSummary.metadata = { is_named_tuple; _ }; _ } = is_named_tuple

    let is_typed_dict _ { PysaClassSummary.metadata = { is_typed_dict; _ }; _ } = is_typed_dict

    let get_ordered_fields_with_predicate
        { class_fields_shared_memory; _ }
        { PysaClassSummary.class_name; _ }
        ~predicate
      =
      let fields =
        ClassFieldsSharedMemory.get class_fields_shared_memory class_name
        |> assert_shared_memory_key_exists (fun () -> "missing class fields for class")
      in
      let compare_by_location
          (_, { ClassField.location = left; _ })
          (_, { ClassField.location = right; _ })
        =
        Ast.Location.compare (Option.value_exn left) (Option.value_exn right)
      in
      fields
      |> SerializableStringMap.to_alist
      |> List.filter ~f:(fun (_, { ClassField.declaration_kind; _ }) -> predicate declaration_kind)
      |> List.filter ~f:(fun (_, { ClassField.location; _ }) -> Option.is_some location)
      |> List.sort ~compare:compare_by_location
      |> List.map ~f:fst


    let dataclass_ordered_attributes api class_summary =
      get_ordered_fields_with_predicate api class_summary ~predicate:(function
          | Some ClassFieldDeclarationKind.DeclaredByAnnotation
          | Some ClassFieldDeclarationKind.AssignedInBody ->
              (* Fields may be initialized via assignments. *)
              true
          | _ -> false)


    let typed_dictionary_attributes api class_summary =
      get_ordered_fields_with_predicate api class_summary ~predicate:(function
          | Some ClassFieldDeclarationKind.DeclaredByAnnotation -> true
          | _ -> false)


    let named_tuple_attributes api class_summary =
      get_ordered_fields_with_predicate api class_summary ~predicate:(function
          | Some ClassFieldDeclarationKind.DeclaredByAnnotation -> true
          | Some ClassFieldDeclarationKind.DeclaredWithoutAnnotation -> true
          | _ -> false)
  end

  let named_tuple_attributes api class_name =
    if class_name |> get_class_summary api |> ClassSummary.is_named_tuple api then
      let mro = class_name :: class_mro api class_name in
      List.find_map
        ~f:(fun parent ->
          let attributes =
            parent |> get_class_summary api |> ClassSummary.named_tuple_attributes api
          in
          if List.is_empty attributes then
            None
          else
            Some attributes)
        mro
    else
      None
end

(* List of symbols exported from the 'builtins' module. To keep it short, this only contains symbols
   that users might want to annotate. *)
let builtins_symbols =
  String.Set.of_list
    [
      "__build_class__";
      "__import__";
      "abs";
      "aiter";
      "all";
      "anext";
      "any";
      "ascii";
      "BaseException";
      "bin";
      "bool";
      "bytearray";
      "bytes";
      "callable";
      "chr";
      "classmethod";
      "compile";
      "complex";
      "delattr";
      "dict";
      "dir";
      "enumerate";
      "eval";
      "Exception";
      "exec";
      "filter";
      "float";
      "format";
      "frozenset";
      "function";
      "GeneratorExit";
      "getattr";
      "globals";
      "hasattr";
      "hash";
      "hex";
      "id";
      "input";
      "int";
      "isinstance";
      "issubclass";
      "iter";
      "KeyboardInterrupt";
      "len";
      "list";
      "map";
      "max";
      "memoryview";
      "min";
      "next";
      "object";
      "open";
      "ord";
      "pow";
      "print";
      "property";
      "range";
      "repr";
      "reversed";
      "round";
      "set";
      "setattr";
      "slice";
      "sorted";
      "staticmethod";
      "str";
      "sum";
      "super";
      "SystemExit";
      "tuple";
      "type";
      "vars";
      "zip";
    ]


(* Unlike Pyre1, Pyrefly prefixes all builtins symbols with 'builtins.'. This function automatically
   adds 'builtins.' in front of references that are likely builtins symbols. The main use case is
   for taint models in .pysa files. *)
let add_builtins_prefix name =
  if Set.mem builtins_symbols (Reference.first name) then
    Reference.create_from_list ("builtins" :: Reference.as_list name)
  else
    name


let strip_builtins_prefix name =
  match Reference.as_list name with
  | "builtins" :: tail -> Reference.create_from_list tail
  | _ -> name


(* Given a fully qualified name for a function, method, class, attribute or global variable, return
   its 'symbolic' name. This removes any path prefix and suffixes such as `@setter` and `$2`. *)
let target_symbolic_name reference =
  (* We could technically get the proper module name and symbol name using shared memory, but we use
     basic string operations for performance reasons. *)
  let reference = Reference.as_list reference in
  let reference =
    (* First, strip the potential path prefixed to the module name, if any. *)
    if List.exists reference ~f:(fun s -> String.contains s ':') then
      reference
      |> String.concat ~sep:"."
      |> String.rsplit2_exn ~on:':'
      |> snd
      |> String.split ~on:'.'
    else
      reference
  in
  let reference =
    (* If '#' was added to distinguish symbols for different modules, replace it with a dot. *)
    if List.exists reference ~f:(fun s -> String.contains s '#') then
      reference
      |> String.concat ~sep:"."
      |> String.substr_replace_all ~pattern:"#" ~with_:"."
      |> String.split ~on:'.'
    else
      reference
  in
  let reference =
    (* Strip '$2' and '@setter' suffixes. Preserve '$toplevel' and '$class_toplevel' *)
    let strip_suffix name =
      let name =
        if Option.value ~default:(-1) (String.index name '$') >= 1 then
          String.rsplit2_exn name ~on:'$' |> fst
        else
          name
      in
      let name =
        if String.contains name '@' then
          String.rsplit2_exn name ~on:'@' |> fst
        else
          name
      in
      name
    in
    List.map ~f:strip_suffix reference
  in
  Reference.create_from_list reference


module ModelQueries = struct
  module Function = Pyre1Api.ModelQueries.Function
  module Global = Pyre1Api.ModelQueries.Global

  let resolve_qualified_name_to_global
      {
        ReadOnly.module_infos_shared_memory;
        callable_metadata_shared_memory;
        class_metadata_shared_memory;
        callable_undecorated_signatures_shared_memory;
        class_fields_shared_memory;
        module_globals_shared_memory;
        _;
      }
      ~is_property_getter:_
      ~is_property_setter
      name
    =
    (* TODO(T225700656): For now, we only support looking up symbols in module names that are unique
       (i.e, the module qualifier is not prefixed by the path) *)
    let name =
      if is_property_setter then
        Reference.create (Format.asprintf "%a@setter" Reference.pp name)
      else
        name
    in
    let is_module_qualifier name =
      ModuleInfosSharedMemory.get
        module_infos_shared_memory
        (ModuleQualifier.from_reference_unchecked name)
      |> Option.is_some
    in
    let is_class_name name =
      ClassMetadataSharedMemory.get
        class_metadata_shared_memory
        (FullyQualifiedName.from_reference_unchecked name)
      |> Option.is_some
    in
    let is_class_attribute name =
      let last_name = Reference.last name in
      Reference.prefix name
      >>| FullyQualifiedName.from_reference_unchecked
      >>= ClassFieldsSharedMemory.get class_fields_shared_memory
      >>| (fun fields -> SerializableStringMap.mem last_name fields)
      |> Option.value ~default:false
    in
    let is_module_global_variable name =
      let last_name = Reference.last name in
      Reference.prefix name
      >>| ModuleQualifier.from_reference_unchecked
      >>= ModuleGlobalsSharedMemory.get module_globals_shared_memory
      >>| (fun globals -> SerializableStringMap.mem last_name globals)
      |> Option.value ~default:false
    in
    (* Check if this is a valid function first. *)
    match
      CallableMetadataSharedMemory.get
        callable_metadata_shared_memory
        (FullyQualifiedName.from_reference_unchecked name)
    with
    | Some
        {
          CallableMetadataSharedMemory.Value.metadata =
            { is_property_getter; is_property_setter; parent_is_class; _ };
          _;
        } ->
        let undecorated_signatures =
          CallableUndecoratedSignaturesSharedMemory.get
            callable_undecorated_signatures_shared_memory
            (FullyQualifiedName.from_reference_unchecked name)
          |> assert_shared_memory_key_exists (fun () ->
                 "missing undecorated signatures for callable")
        in
        Some
          (Global.Function
             {
               Function.define_name = name;
               imported_name = None;
               undecorated_signatures = Some undecorated_signatures;
               is_property_getter;
               is_property_setter;
               is_method = parent_is_class;
             })
    | None ->
        if is_module_qualifier name then
          Some Global.Module
        else if is_class_name name then
          Some (Global.Class { class_name = Reference.show name })
        else if is_class_attribute name then
          (* Functions might also be considered class attributes by pyrefly, so this should be
             checked last. *)
          Some (Global.ClassAttribute { name })
        else if is_module_global_variable name then
          Some (Global.ModuleGlobal { name })
        else
          None


  let class_method_signatures
      {
        ReadOnly.class_metadata_shared_memory;
        module_callables_shared_memory;
        callable_define_signature_shared_memory;
        _;
      }
      class_name
    =
    match
      ClassMetadataSharedMemory.get
        class_metadata_shared_memory
        (FullyQualifiedName.from_reference_unchecked class_name)
    with
    | Some { ClassMetadataSharedMemory.Metadata.module_qualifier; _ } ->
        (* TODO(T225700656): We should check if the callable is a method using the parent_is_class
           field. *)
        let is_method_for_class callable_name =
          Reference.equal
            (callable_name
            |> FullyQualifiedName.to_reference
            |> Reference.prefix
            |> Option.value ~default:Reference.empty)
            class_name
          && not
               (String.equal
                  (FullyQualifiedName.last callable_name)
                  Statement.class_toplevel_define_name)
        in
        let add_signature callable_name =
          let signature =
            CallableDefineSignatureSharedMemory.get
              callable_define_signature_shared_memory
              callable_name
            |> assert_shared_memory_key_exists (fun () -> "missing signature for callable")
            |> AstResult.to_option
            >>| Node.value
          in
          FullyQualifiedName.to_reference callable_name, signature
        in
        ModuleCallablesSharedMemory.get module_callables_shared_memory module_qualifier
        |> assert_shared_memory_key_exists (fun () -> "missing module callables for qualifier")
        |> List.filter ~f:is_method_for_class
        |> List.map ~f:add_signature
        |> Option.some
    | None -> None
end

module InContext = struct
  type t =
    | FunctionScope of {
        api: ReadOnly.t;
        module_qualifier: ModuleQualifier.t;
        define_name: Ast.Reference.t;
        call_graph: CallGraph.DefineCallGraph.t;
      }
    | StatementScope of {
        api: ReadOnly.t;
        module_qualifier: ModuleQualifier.t;
        (* Define name of the function. Note that this might be a decorated target, which is not
           visible by pyrefly. *)
        define_name: Ast.Reference.t;
        call_graph: CallGraph.DefineCallGraph.t;
        statement_key: int;
      }

  let create_at_function_scope api ~module_qualifier ~define_name ~call_graph =
    FunctionScope
      {
        api;
        module_qualifier = ModuleQualifier.from_reference_unchecked module_qualifier;
        define_name;
        call_graph;
      }


  let create_at_statement_scope api ~module_qualifier ~define_name ~call_graph ~statement_key =
    StatementScope
      {
        api;
        module_qualifier = ModuleQualifier.from_reference_unchecked module_qualifier;
        define_name;
        statement_key;
        call_graph;
      }


  let pyre_api = function
    | FunctionScope { api; _ } -> api
    | StatementScope { api; _ } -> api


  let define_name = function
    | FunctionScope { define_name; _ } -> define_name
    | StatementScope { define_name; _ } -> define_name


  let call_graph = function
    | FunctionScope { call_graph; _ } -> call_graph
    | StatementScope { call_graph; _ } -> call_graph


  let module_qualifier = function
    | FunctionScope { module_qualifier; _ } -> ModuleQualifier.to_reference module_qualifier
    | StatementScope { module_qualifier; _ } -> ModuleQualifier.to_reference module_qualifier


  let is_global _ ~reference:_ =
    (* TODO(T225700656): Support is_global *)
    failwith "unimplemented: PyreflyApi.InContext.is_global"


  let resolve_reference _ _ =
    (* TODO(T225700656): Support resolve_reference *)
    failwith "unimplemented: PyreflyApi.InContext.resolve_reference"


  let resolve_assignment api _ = api

  let resolve_generators api _ = api

  let resolve_expression_to_type _ _ =
    (* TODO(T225700656): Support resolve_expression_to_type *)
    failwith "unimplemented: PyreflyApi.InContext.resolve_expression_to_type"


  let resolve_attribute_access _ ~base_type:_ ~attribute:_ =
    (* TODO(T225700656): Support resolve_attribute_access *)
    failwith "unimplemented: PyreflyApi.InContext.resolve_attribute_access"


  let fallback_attribute _ ?accessed_through_class:_ ?type_for_lookup:_ ~name:_ _ =
    (* TODO(T225700656): Support fallback_attribute *)
    failwith "unimplemented: PyreflyApi.InContext.fallback_attribute"


  let root_of_identifier pyrefly_in_context ~location ~identifier =
    match
      CallGraph.DefineCallGraph.resolve_identifier
        ~location
        ~identifier
        (call_graph pyrefly_in_context)
    with
    | Some { CallGraph.IdentifierCallees.captured_variables = captured_variable :: _; _ } ->
        AccessPath.Root.CapturedVariable captured_variable
    | _ -> AccessPath.Root.Variable identifier


  let propagate_captured_variable pyrefly_in_context ~defining_function ~name =
    if Reference.equal defining_function (define_name pyrefly_in_context) then
      (* We have reached the function that originated the variable. We should treat it as a regular
         variable now. *)
      AccessPath.Root.Variable name
    else
      AccessPath.Root.CapturedVariable
        (AccessPath.CapturedVariable.FromFunction { name; defining_function })
end

(* Exposed for testing purposes *)
module Testing = struct
  module Module = ReadWrite.Module

  let create_module_qualifiers = ReadWrite.create_module_qualifiers

  module Definition = ReadWrite.DefinitionCollector.Definition
  module QualifiedDefinition = ReadWrite.DefinitionCollector.QualifiedDefinition

  let create_fully_qualified_names
      ~module_qualifier
      ~module_exists
      ~class_definitions
      ~function_definitions
    =
    ReadWrite.DefinitionCollector.Tree.from_definitions ~function_definitions ~class_definitions
    |> ReadWrite.DefinitionCollector.Tree.create_qualified_names
    |> ReadWrite.DefinitionCollector.Tree.check_module_overlaps ~module_qualifier ~module_exists
    |> ReadWrite.DefinitionCollector.Tree.collect_definitions ~module_qualifier
    |> ReadWrite.DefinitionCollector.add_toplevel_defines ~module_qualifier
end
