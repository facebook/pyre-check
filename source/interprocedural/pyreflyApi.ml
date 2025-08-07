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


  let get_optional_bool_member ~default json key =
    match Yojson.Safe.Util.member key json with
    | `Bool value -> Ok value
    | `Null -> Ok default
    | _ ->
        Error
          (FormatError.UnexpectedJsonType
             { json; message = Format.sprintf "expected the key `%s` to contain a boolean" key })


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

(* Path of a module, equivalent of the `pyrefly_python::module_path::ModulePathDetails` rust
   type. *)
module ModulePath = struct
  type t =
    | Filesystem of ArtifactPath.t
    | Namespace of PyrePath.t
    | Memory of PyrePath.t
    | BundledTypeshed of PyrePath.t
  [@@deriving compare, equal, show]

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

(* Unique identifier for a module, assigned by pyrefly. This maps to a source file. *)
module ModuleId : sig
  type t [@@deriving compare, equal, sexp, hash, show]

  val from_int : int -> t
end = struct
  type t = int [@@deriving compare, equal, sexp, hash, show]

  let from_int = Fn.id
end

(* Unique identifier for a class within a module, assigned by pyrefly. *)
module ClassId : sig
  type t [@@deriving compare, equal, sexp, hash, show]

  val from_int : int -> t
end = struct
  type t = int [@@deriving compare, equal, sexp, hash, show]

  let from_int = Fn.id
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
  end

  include T
  module Map = Map.Make (T)
end

module ModuleQualifierSharedMemoryKey = struct
  type t = ModuleQualifier.t [@@deriving compare]

  let to_string key =
    key |> ModuleQualifier.to_reference |> Analysis.SharedMemoryKeys.ReferenceKey.to_string


  let from_string s =
    s
    |> Analysis.SharedMemoryKeys.ReferenceKey.from_string
    |> ModuleQualifier.from_reference_unchecked
end

(* Path to a pyrefly module information file. *)
module ModuleInfoPath : sig
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
      module_path: ModulePath.t;
      info_path: ModuleInfoPath.t option;
    }
    [@@deriving equal, show]

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
        module_name = Reference.create module_name;
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

(* Information from pyrefly about a given module, stored as a `module:id.json` file. This matches
   the `pyrefly::report::pysa::PysaModuleFile` rust type. *)
module ModuleInfoFile = struct
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
          Location.from_string location_string
          |> Result.map_error ~f:(fun error ->
                 FormatError.UnexpectedJsonType { json = `String location_string; message = error })
          >>| fun location -> Class location
      | `Assoc [("Function", `Assoc [("location", `String location_string)])] ->
          Location.from_string location_string
          |> Result.map_error ~f:(fun error ->
                 FormatError.UnexpectedJsonType { json = `String location_string; message = error })
          >>| fun location -> Function location
      | _ -> Error (FormatError.UnexpectedJsonType { json; message = "expected parent_scope" })
  end

  module FunctionDefinition = struct
    type t = {
      name: string;
      parent: ParentScope.t;
      is_overload: bool;
      is_staticmethod: bool;
      is_classmethod: bool;
      is_property_getter: bool;
      is_property_setter: bool;
      is_toplevel: bool;
      is_class_toplevel: bool;
    }
    [@@deriving equal, show]

    let from_json json =
      let open Core.Result.Monad_infix in
      JsonUtil.get_string_member json "name"
      >>= fun name ->
      ParentScope.from_json (Yojson.Safe.Util.member "parent" json)
      >>= fun parent ->
      JsonUtil.get_optional_bool_member ~default:false json "is_overload"
      >>= fun is_overload ->
      JsonUtil.get_optional_bool_member ~default:false json "is_staticmethod"
      >>= fun is_staticmethod ->
      JsonUtil.get_optional_bool_member ~default:false json "is_classmethod"
      >>= fun is_classmethod ->
      JsonUtil.get_optional_bool_member ~default:false json "is_property_getter"
      >>= fun is_property_getter ->
      JsonUtil.get_optional_bool_member ~default:false json "is_property_setter"
      >>| fun is_property_setter ->
      {
        name;
        parent;
        is_overload;
        is_staticmethod;
        is_classmethod;
        is_property_getter;
        is_property_setter;
        is_toplevel = false;
        is_class_toplevel = false;
      }


    let create_module_toplevel () =
      {
        name = Ast.Statement.toplevel_define_name;
        parent = ParentScope.TopLevel;
        is_overload = false;
        is_staticmethod = false;
        is_classmethod = false;
        is_property_getter = false;
        is_property_setter = false;
        is_toplevel = true;
        is_class_toplevel = false;
      }


    let create_class_toplevel ~location =
      {
        name = Ast.Statement.class_toplevel_define_name;
        parent = ParentScope.Class location;
        is_overload = false;
        is_staticmethod = false;
        is_classmethod = false;
        is_property_getter = false;
        is_property_setter = false;
        is_toplevel = false;
        is_class_toplevel = true;
      }
  end

  module ClassDefinition = struct
    type t = {
      name: string;
      parent: ParentScope.t;
      class_id: ClassId.t;
    }
    [@@deriving equal, show]

    let from_json json =
      let open Core.Result.Monad_infix in
      JsonUtil.get_string_member json "name"
      >>= fun name ->
      ParentScope.from_json (Yojson.Safe.Util.member "parent" json)
      >>= fun parent ->
      JsonUtil.get_int_member json "class_id"
      >>| fun class_id -> { name; parent; class_id = ClassId.from_int class_id }
  end

  type t = {
    (* TODO(T225700656): module_id, module_name and source_path are already specified in the Project
       module. We should probably remove those from the file format. *)
    module_id: ModuleId.t;
    module_name: Reference.t;
    source_path: ModulePath.t;
    type_of_expression: string Location.Map.t;
    function_definitions: FunctionDefinition.t Location.Map.t;
    class_definitions: ClassDefinition.t Location.Map.t;
  }

  let from_json json =
    let open Core.Result.Monad_infix in
    let parse_type_of_expression type_of_expression =
      type_of_expression
      |> List.map ~f:(fun (location, ty) ->
             Location.from_string location
             |> Result.map_error ~f:(fun error ->
                    FormatError.UnexpectedJsonType { json = `String location; message = error })
             >>= fun location -> JsonUtil.as_string ty >>| fun ty -> location, ty)
      |> Result.all
      >>| Location.Map.of_alist_exn
    in
    let parse_function_definitions function_definitions =
      function_definitions
      |> List.map ~f:(fun (location, function_definition) ->
             Location.from_string location
             |> Result.map_error ~f:(fun error ->
                    FormatError.UnexpectedJsonType { json = `String location; message = error })
             >>= fun location ->
             FunctionDefinition.from_json function_definition
             >>| fun function_definition -> location, function_definition)
      |> Result.all
      >>| Location.Map.of_alist_exn
    in
    let parse_class_definitions class_definitions =
      class_definitions
      |> List.map ~f:(fun (location, class_definition) ->
             Location.from_string location
             |> Result.map_error ~f:(fun error ->
                    FormatError.UnexpectedJsonType { json = `String location; message = error })
             >>= fun location ->
             ClassDefinition.from_json class_definition
             >>| fun class_definition -> location, class_definition)
      |> Result.all
      >>| Location.Map.of_alist_exn
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
    >>= fun type_of_expression ->
    JsonUtil.get_object_member json "function_definitions"
    >>= parse_function_definitions
    >>= fun function_definitions ->
    JsonUtil.get_object_member json "class_definitions"
    >>= parse_class_definitions
    >>| fun class_definitions ->
    {
      module_id = ModuleId.from_int module_id;
      module_name = Reference.create module_name;
      source_path;
      type_of_expression;
      function_definitions;
      class_definitions;
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

(* Information about a module, stored in shared memory. *)
module ModuleInfosSharedMemory = struct
  module Module = struct
    type t = {
      source_path: ArtifactPath.t option (* The path of source code as seen by the analyzer. *);
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

(* List of module qualifiers with source code, stored in shared memory. *)
module QualifiersWithSourceSharedMemory =
  Hack_parallel.Std.SharedMemory.FirstClass.NoCache.Make
    (Memory.SingletonKey)
    (struct
      type t = ModuleQualifier.t list

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = "pyrefly all modules"
    end)

(* Type of expression at a given module qualifier and location, stored in shared memory. *)
module TypeOfExpressionsSharedMemory = struct
  module Key = struct
    type t = {
      module_qualifier: ModuleQualifier.t;
      location: Location.t;
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

(* Abstract Syntax Tree of a module, stored in shared memory. *)
module AstsSharedMemory =
  Hack_parallel.Std.SharedMemory.FirstClass.NoCache.Make
    (ModuleQualifierSharedMemoryKey)
    (struct
      type t = Analysis.Parsing.ParseResult.t OptionalSource.t

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = "pyrefly asts"
    end)

(* Unique identifier for a class or define (e.g, `def foo(): ..`) in a module.
 *
 * - In most cases, this will be a dotted path such as `my.module.MyClass.foo`.
 * - Property setters have a suffix `@setter` to be different from property getters.
 * - Type overloads have a suffix `@overload` to be different from regular methods.
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
end = struct
  (* Same as ModuleQualifier, it is stored as a reference, but it doesn't really make sense
     either. *)
  type t = Reference.t [@@deriving compare, equal, sexp, hash, show]

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
end

module FullyQualifiedNameSharedMemoryKey = struct
  type t = FullyQualifiedName.t [@@deriving compare]

  let to_string key =
    key |> FullyQualifiedName.to_reference |> Analysis.SharedMemoryKeys.ReferenceKey.to_string


  let from_string s =
    s
    |> Analysis.SharedMemoryKeys.ReferenceKey.from_string
    |> FullyQualifiedName.from_reference_unchecked
end

module CallableMetadataSharedMemory = struct
  module Metadata = struct
    type t = {
      location: Location.t;
      is_overload: bool;
      is_staticmethod: bool;
      is_classmethod: bool;
      is_property_getter: bool;
      is_property_setter: bool;
      is_toplevel: bool;
      is_class_toplevel: bool;
    }
    [@@deriving show]
  end

  include
    Hack_parallel.Std.SharedMemory.FirstClass.NoCache.Make
      (FullyQualifiedNameSharedMemoryKey)
      (struct
        type t = Metadata.t

        let prefix = Hack_parallel.Std.Prefix.make ()

        let description = "pyrefly callable metadata"
      end)
end

module ClassMetadataSharedMemory = struct
  module Metadata = struct
    type t = {
      location: Location.t;
      class_id: ClassId.t;
    }
    [@@deriving show]
  end

  include
    Hack_parallel.Std.SharedMemory.FirstClass.NoCache.Make
      (FullyQualifiedNameSharedMemoryKey)
      (struct
        type t = Metadata.t

        let prefix = Hack_parallel.Std.Prefix.make ()

        let description = "pyrefly class metadata"
      end)
end

(* API handle stored in the main process. The type `t` should not be sent to workers, since it's
   expensive to copy. *)
module ReadWrite = struct
  module Module = struct
    type t = {
      module_id: ModuleId.t;
      module_name: Reference.t;
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
    qualifier_to_module_map: Module.t ModuleQualifier.Map.t;
    module_infos_shared_memory: ModuleInfosSharedMemory.t;
    qualifiers_with_source_shared_memory: QualifiersWithSourceSharedMemory.t;
    asts_shared_memory: AstsSharedMemory.t;
    type_of_expressions_shared_memory: TypeOfExpressionsSharedMemory.t;
    callable_metadata_shared_memory: CallableMetadataSharedMemory.t;
    class_metadata_shared_memory: ClassMetadataSharedMemory.t;
  }

  (* Build a mapping from unique module qualifiers (module name + path prefix) to module. *)
  let create_module_qualifiers modules =
    let make_unique_qualifiers (module_name, modules) =
      match modules with
      | [module_info] -> [ModuleQualifier.create ~path:None module_name, module_info]
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
                 ModuleQualifier.create ~path:(Some path) module_name, module_info)
    in
    let add_to_module_name_mapping sofar ({ ProjectFile.Module.module_name; _ } as module_info) =
      Map.update sofar module_name ~f:(function
          | None -> [module_info]
          | Some existing -> module_info :: existing)
    in
    modules
    |> List.fold ~init:Reference.Map.empty ~f:add_to_module_name_mapping
    |> Map.to_alist
    |> List.map ~f:make_unique_qualifiers
    |> List.concat
    |> ModuleQualifier.Map.of_alist_exn


  let parse_modules ~pyrefly_directory =
    let timer = Timer.start () in
    let () = Log.info "Parsing module list from pyrefly..." in
    let project =
      ProjectFile.from_path_exn (PyrePath.append pyrefly_directory ~element:"pyrefly.pysa.json")
    in
    let qualifier_to_module_map =
      create_module_qualifiers (Map.data project.modules)
      |> ModuleQualifier.Map.map ~f:(Module.from_project ~pyrefly_directory)
    in
    Log.info "Parsed module list from pyrefly: %.3fs" (Timer.stop_in_sec timer);
    Statistics.performance
      ~name:"Parsed module list from pyrefly"
      ~phase_name:"Parsing module list from pyrefly"
      ~command:"analyze"
      ~timer
      ~integers:["modules", Map.length qualifier_to_module_map]
      ();
    qualifier_to_module_map


  let parse_type_of_expressions
      ~scheduler
      ~scheduler_policies
      ~pyrefly_directory
      ~qualifier_to_module_map
    =
    let timer = Timer.start () in
    let () = Log.info "Parsing type of expressions from pyrefly..." in
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
    let parse_module_info (module_qualifier, pyrefly_info_path) =
      let {
        ModuleInfoFile.type_of_expression;
        module_id = _;
        module_name = _;
        source_path = _;
        function_definitions = _;
        class_definitions = _;
      }
        =
        ModuleInfoFile.from_path_exn ~pyrefly_directory pyrefly_info_path
      in
      Map.iteri type_of_expression ~f:(fun ~key:location ~data:ty ->
          TypeOfExpressionsSharedMemory.add
            handle
            { TypeOfExpressionsSharedMemory.Key.module_qualifier; location }
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
    Log.info "Parsed type of expressions from pyrefly: %.3fs" (Timer.stop_in_sec timer);
    Statistics.performance
      ~name:"Parsed type of expressions from pyrefly"
      ~phase_name:"Parsing type of expressions from pyrefly"
      ~command:"analyze"
      ~timer
      ();
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
            let parse_result =
              match parse_result with
              | Ok ({ Ast.Source.module_path; _ } as source) ->
                  (* Remove the qualifier created by pyre, it is wrong *)
                  let module_path = { module_path with qualifier = Reference.empty } in
                  Ok { source with module_path }
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
                  parse_result
            in
            OptionalSource.Some parse_result
      in
      AstsSharedMemory.add handle qualifier parse_result
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
    Statistics.performance
      ~name:"Parsed source files"
      ~phase_name:"Parsing source files"
      ~command:"analyze"
      ~timer
      ();
    handle


  (* Logic to assign fully qualified names to classes and defines. *)
  module DefinitionCollector = struct
    module Definition = struct
      type t =
        | Function of ModuleInfoFile.FunctionDefinition.t
        | Class of ModuleInfoFile.ClassDefinition.t
      [@@deriving equal, show]

      let name = function
        | Function { ModuleInfoFile.FunctionDefinition.name; _ } -> name
        | Class { ModuleInfoFile.ClassDefinition.name; _ } -> name
    end

    module QualifiedDefinition = struct
      type t = {
        qualified_name: FullyQualifiedName.t;
        local_name: Reference.t; (* a non-unique name, more user-friendly. *)
        definition: Definition.t;
        location: Location.t;
      }
    end

    (* Returns a list of path elements for a function or class in the AST. For instance, ['MyClass',
       'foo'] for a method foo in MyClass. *)
    let rec create_local_path ~function_definitions ~class_definitions sofar parent =
      match parent with
      | ModuleInfoFile.ParentScope.TopLevel -> sofar
      | ModuleInfoFile.ParentScope.Class parent_location ->
          let ({ ModuleInfoFile.ClassDefinition.parent; _ } as class_definition) =
            Map.find_exn class_definitions parent_location
          in
          let sofar =
            { Node.location = parent_location; value = Definition.Class class_definition } :: sofar
          in
          create_local_path ~function_definitions ~class_definitions sofar parent
      | ModuleInfoFile.ParentScope.Function parent_location ->
          let ({ ModuleInfoFile.FunctionDefinition.parent; _ } as function_definition) =
            Map.find_exn function_definitions parent_location
          in
          let sofar =
            { Node.location = parent_location; value = Definition.Function function_definition }
            :: sofar
          in
          create_local_path ~function_definitions ~class_definitions sofar parent


    let local_path_of_function
        ~function_definitions
        ~class_definitions
        ~location
        ({ ModuleInfoFile.FunctionDefinition.parent; _ } as function_definition)
      =
      create_local_path
        ~function_definitions
        ~class_definitions
        [{ Node.location; value = Definition.Function function_definition }]
        parent


    let local_path_of_class
        ~function_definitions
        ~class_definitions
        ~location
        ({ ModuleInfoFile.ClassDefinition.parent; _ } as class_definition)
      =
      create_local_path
        ~function_definitions
        ~class_definitions
        [{ Node.location; value = Definition.Class class_definition }]
        parent


    (* Represents a tree of definitions, where children of a node are the functions and classes
       nested under that definition. For instance, all methods are nested under their parent
       class. *)
    module Tree = struct
      module Node = struct
        type 'a t = {
          value: 'a;
          children: 'a t Location.Map.t;
        }
      end

      type 'a t = { children: 'a Node.t Location.Map.t }

      let empty = { children = Location.Map.empty }

      let add_local_path path { children } =
        let rec add_to_children children = function
          | [] -> children
          | { Ast.Node.location; value } :: tail ->
              Map.update children location ~f:(function
                  | None -> { value; children = add_to_children Location.Map.empty tail }
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
            ~f:(fun ~key:location ~data:class_definition sofar ->
              add_local_path
                (local_path_of_class
                   ~function_definitions
                   ~class_definitions
                   ~location
                   class_definition)
                sofar)
            class_definitions
        in
        Map.fold
          ~init:tree
          ~f:(fun ~key:location ~data:function_definition sofar ->
            add_local_path
              (local_path_of_function
                 ~function_definitions
                 ~class_definitions
                 ~location
                 function_definition)
              sofar)
          function_definitions


      module QualifiedNode = struct
        type t = {
          definition: Definition.t;
          unique_name: string;
          name_overlaps_module: bool;
        }
        [@@deriving show]
      end

      let create_qualified_names { children } =
        let rec qualify_children children =
          let name_indices = SerializableStringMap.empty in
          let add_definition (name_indices, sofar) (location, { Node.value = definition; children })
            =
            let name =
              match definition with
              | Definition.Function
                  { ModuleInfoFile.FunctionDefinition.name; is_property_setter; is_overload; _ } ->
                  (* For methods, we need to differentiate property setters and overloads *)
                  let name =
                    if is_property_setter then
                      Format.sprintf "%s@setter" name
                    else
                      name
                  in
                  let name =
                    if is_overload then
                      Format.sprintf "%s@overload" name
                    else
                      name
                  in
                  name
              | Definition.Class { ModuleInfoFile.ClassDefinition.name; _ } -> name
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
          |> Location.Map.of_alist_exn
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
        let rec add_definition
            sofar
            ~parent_qualified_name
            ~parent_local_name
            ~location
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
              definition;
              location;
            }
            :: sofar
          in
          Map.fold children ~init:sofar ~f:(fun ~key:location ~data:node sofar ->
              add_definition
                sofar
                ~parent_qualified_name:(unique_name :: parent_qualified_name)
                ~parent_local_name:(symbol_name :: parent_local_name)
                ~location
                node)
        in
        let definitions =
          Map.fold children ~init:[] ~f:(fun ~key:location ~data:node sofar ->
              add_definition sofar ~parent_qualified_name:[] ~parent_local_name:[] ~location node)
        in
        List.rev definitions
    end

    let add_toplevel_defines ~module_qualifier definitions =
      let add_toplevel
          sofar
          ({ QualifiedDefinition.definition; qualified_name; local_name; location; _ } as
          qualified_definition)
        =
        let sofar = qualified_definition :: sofar in
        match definition with
        | Definition.Class _ ->
            {
              QualifiedDefinition.definition =
                Definition.Function
                  (ModuleInfoFile.FunctionDefinition.create_class_toplevel ~location);
              qualified_name = FullyQualifiedName.create_class_toplevel qualified_name;
              local_name =
                Reference.combine
                  local_name
                  (Reference.create_from_list [Ast.Statement.class_toplevel_define_name]);
              location;
            }
            :: sofar
        | _ -> sofar
      in
      let definitions = List.fold ~f:add_toplevel ~init:[] definitions in
      let definitions = List.rev definitions in
      {
        QualifiedDefinition.definition =
          Definition.Function (ModuleInfoFile.FunctionDefinition.create_module_toplevel ());
        qualified_name = FullyQualifiedName.create_module_toplevel ~module_qualifier;
        local_name = Reference.create_from_list [Ast.Statement.toplevel_define_name];
        location = Location.any;
      }
      :: definitions
  end

  module DefinitionCount = struct
    type t = {
      number_callables: int;
      number_classes: int;
    }

    let empty = { number_callables = 0; number_classes = 0 }

    let increment_callable { number_callables; number_classes } =
      { number_callables = number_callables + 1; number_classes }


    let increment_class { number_callables; number_classes } =
      { number_callables; number_classes = number_classes + 1 }


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
    let () = Log.info "Collecting classes and definitions..." in
    let parse_module_info (module_qualifier, pyrefly_info_path) =
      let { ModuleInfoFile.function_definitions; class_definitions; _ } =
        ModuleInfoFile.from_path_exn ~pyrefly_directory pyrefly_info_path
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
          counts
          { DefinitionCollector.QualifiedDefinition.qualified_name; definition; location; _ }
        =
        match definition with
        | Function
            {
              is_overload;
              is_staticmethod;
              is_classmethod;
              is_property_getter;
              is_property_setter;
              is_toplevel;
              is_class_toplevel;
              _;
            } ->
            CallableMetadataSharedMemory.add
              callable_metadata_shared_memory
              qualified_name
              {
                CallableMetadataSharedMemory.Metadata.location;
                is_overload;
                is_staticmethod;
                is_classmethod;
                is_property_getter;
                is_property_setter;
                is_toplevel;
                is_class_toplevel;
              };
            DefinitionCount.increment_callable counts
        | Class { class_id; _ } ->
            ClassMetadataSharedMemory.add
              class_metadata_shared_memory
              qualified_name
              { ClassMetadataSharedMemory.Metadata.location; class_id };
            DefinitionCount.increment_class counts
      in
      List.fold definitions ~init:DefinitionCount.empty ~f:store_definition
    in
    let parse_module_infos modules =
      List.map ~f:parse_module_info modules
      |> List.fold ~init:DefinitionCount.empty ~f:DefinitionCount.add
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
      |> List.filter_map ~f:(fun (qualifier, { Module.pyrefly_info_path; _ }) ->
             match pyrefly_info_path with
             | Some pyrefly_info_path -> Some (qualifier, pyrefly_info_path)
             | None -> None)
    in
    let { DefinitionCount.number_callables; number_classes } =
      Scheduler.map_reduce
        scheduler
        ~policy:scheduler_policy
        ~initial:DefinitionCount.empty
        ~map:parse_module_infos
        ~reduce:DefinitionCount.add
        ~inputs
        ()
    in
    Log.info "Collected classes and definitions: %.3fs" (Timer.stop_in_sec timer);
    Statistics.performance
      ~name:"Collected classes and definitions"
      ~phase_name:"Collecting classes and definitions"
      ~command:"analyze"
      ~timer
      ~integers:["callables", number_callables; "classes", number_classes]
      ();
    callable_metadata_shared_memory, class_metadata_shared_memory


  let create_from_directory
      ~scheduler
      ~scheduler_policies
      ~configuration
      ~decorator_configuration:_
      pyrefly_directory
    =
    let qualifier_to_module_map = parse_modules ~pyrefly_directory in

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

    let callable_metadata_shared_memory, class_metadata_shared_memory =
      collect_classes_and_definitions
        ~scheduler
        ~scheduler_policies
        ~pyrefly_directory
        ~qualifier_to_module_map
        ~module_infos_shared_memory
    in

    let type_of_expressions_shared_memory =
      parse_type_of_expressions
        ~scheduler
        ~scheduler_policies
        ~pyrefly_directory
        ~qualifier_to_module_map
    in

    {
      qualifier_to_module_map;
      module_infos_shared_memory;
      qualifiers_with_source_shared_memory;
      asts_shared_memory;
      type_of_expressions_shared_memory;
      callable_metadata_shared_memory;
      class_metadata_shared_memory;
    }


  (* TODO(T225700656): Remove this, this is to silence the unused value warning *)
  let unused
      {
        qualifier_to_module_map;
        type_of_expressions_shared_memory;
        callable_metadata_shared_memory;
        class_metadata_shared_memory;
        _;
      }
    =
    let _ =
      ( Module.pp,
        CallableMetadataSharedMemory.Metadata.pp,
        ClassMetadataSharedMemory.Metadata.pp,
        DefinitionCollector.Tree.QualifiedNode.pp,
        qualifier_to_module_map,
        type_of_expressions_shared_memory,
        callable_metadata_shared_memory,
        class_metadata_shared_memory )
    in
    ()


  let _ = unused
end

(* Read-only API that can be sent to workers. Cheap to copy. *)
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
    ModuleInfosSharedMemory.get
      module_infos_shared_memory
      (ModuleQualifier.from_reference_unchecked qualifier)
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
    |> List.map ~f:ModuleQualifier.to_reference


  let source_of_qualifier { asts_shared_memory; _ } qualifier =
    AstsSharedMemory.get asts_shared_memory (ModuleQualifier.from_reference_unchecked qualifier)
    |> assert_shared_memory_key_exists "missing source for qualifier"
    |> function
    | OptionalSource.Some (Result.Ok source) -> Some source
    | OptionalSource.Some (Result.Error _) -> None
    | OptionalSource.NoSource -> None
end

(* Exposed for testing purposes *)
module Testing = struct
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
