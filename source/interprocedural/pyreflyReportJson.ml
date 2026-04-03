(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* JSON parsing functions for pyrefly report files. *)

open Core
open Pyre
open Ast
module ScalarTypeProperties = Analysis.PyrePysaEnvironment.ScalarTypeProperties
module PyreflyTypeRep = Analysis.PyrePysaEnvironment.PyreflyType

module JsonUtil = struct
  let read_json_file_exn path =
    try Yojson.Safe.from_file (PyrePath.absolute path) with
    | Yojson.Json_error message ->
        raise
          (PyreflyReport.PyreflyFileFormatError
             { path; error = PyreflyReport.Error.InvalidJsonError message })
    | Sys_error message ->
        raise
          (PyreflyReport.PyreflyFileFormatError
             { path; error = PyreflyReport.Error.IOError message })


  let get_string_member json key =
    match Yojson.Safe.Util.member key json with
    | `String value -> Ok value
    | _ ->
        Error
          (PyreflyReport.FormatError.UnexpectedJsonType
             {
               json;
               message = Format.sprintf "expected an object with key `%s` containing a string" key;
             })


  let as_list = function
    | `List elements -> Ok elements
    | json ->
        Error (PyreflyReport.FormatError.UnexpectedJsonType { json; message = "expected a list" })


  let as_int = function
    | `Int value -> Ok value
    | json ->
        Error
          (PyreflyReport.FormatError.UnexpectedJsonType { json; message = "expected an integer" })


  let as_string = function
    | `String string -> Ok string
    | json ->
        Error (PyreflyReport.FormatError.UnexpectedJsonType { json; message = "expected a string" })


  let get_optional_string_member json key =
    match Yojson.Safe.Util.member key json with
    | `String value -> Ok (Some value)
    | `Null -> Ok None
    | _ ->
        Error
          (PyreflyReport.FormatError.UnexpectedJsonType
             { json; message = Format.sprintf "expected the key `%s` to contain a string" key })


  let get_optional_bool_member ~default json key =
    match Yojson.Safe.Util.member key json with
    | `Bool value -> Ok value
    | `Null -> Ok default
    | _ ->
        Error
          (PyreflyReport.FormatError.UnexpectedJsonType
             { json; message = Format.sprintf "expected the key `%s` to contain a boolean" key })


  let get_optional_list_member json key =
    match Yojson.Safe.Util.member key json with
    | `List elements -> Ok elements
    | `Null -> Ok []
    | _ ->
        Error
          (PyreflyReport.FormatError.UnexpectedJsonType
             { json; message = Format.sprintf "expected the key `%s` to contain a list" key })


  let get_int_member json key =
    match Yojson.Safe.Util.member key json with
    | `Int value -> Ok value
    | _ ->
        Error
          (PyreflyReport.FormatError.UnexpectedJsonType
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
    | version -> Error (PyreflyReport.FormatError.UnsupportedVersion { version })


  let get_member json key =
    match Yojson.Safe.Util.member key json with
    | `Null ->
        Error
          (PyreflyReport.FormatError.UnexpectedJsonType
             { json; message = Format.sprintf "expected an object with key `%s`" key })
    | value -> Ok value


  let get_object_member json key =
    match Yojson.Safe.Util.member key json with
    | `Assoc bindings -> Ok bindings
    | _ ->
        Error
          (PyreflyReport.FormatError.UnexpectedJsonType
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
          (PyreflyReport.FormatError.UnexpectedJsonType
             {
               json;
               message = Format.sprintf "expected an object with key `%s` containing an object" key;
             })


  let get_list_member json key =
    match Yojson.Safe.Util.member key json with
    | `List elements -> Ok elements
    | _ ->
        Error
          (PyreflyReport.FormatError.UnexpectedJsonType
             {
               json;
               message = Format.sprintf "expected an object with key `%s` containing a list" key;
             })


  let check_object = function
    | `Assoc bindings -> Ok bindings
    | json ->
        Error
          (PyreflyReport.FormatError.UnexpectedJsonType
             { json; message = "expected root to be an object" })
end

module ModulePath = struct
  let from_json = function
    | `Assoc [("FileSystem", `String path)] ->
        Ok
          (PyreflyReport.ModulePath.Filesystem
             (path |> PyrePath.create_absolute |> ArtifactPath.create))
    | `Assoc [("Namespace", `String path)] ->
        Ok (PyreflyReport.ModulePath.Namespace (PyrePath.create_absolute path))
    | `Assoc [("Memory", `String path)] ->
        Ok (PyreflyReport.ModulePath.Memory (PyrePath.create_absolute path))
    | `Assoc [("BundledTypeshed", `String path)] ->
        Ok (PyreflyReport.ModulePath.BundledTypeshed (PyrePath.create_absolute path))
    | `Assoc [("BundledTypeshedThirdParty", `String path)] ->
        Ok (PyreflyReport.ModulePath.BundledTypeshedThirdParty (PyrePath.create_absolute path))
    | json ->
        Error
          (PyreflyReport.FormatError.UnexpectedJsonType { json; message = "expected a module path" })
end

module GlobalClassId = struct
  let from_json json =
    let open Core.Result.Monad_infix in
    JsonUtil.get_int_member json "module_id"
    >>= fun module_id ->
    JsonUtil.get_int_member json "class_id"
    >>| fun class_id ->
    {
      PyreflyReport.GlobalClassId.module_id = PyreflyReport.ModuleId.from_int module_id;
      local_class_id = PyreflyReport.LocalClassId.from_int class_id;
    }


  let from_optional_json = function
    | Some json ->
        let open Core.Result.Monad_infix in
        json |> from_json >>| Option.some
    | None -> Ok None
end

module GlobalCallableId = struct
  let from_json json =
    let open Core.Result.Monad_infix in
    JsonUtil.get_int_member json "module_id"
    >>= fun module_id ->
    JsonUtil.get_string_member json "function_id"
    >>= PyreflyReport.LocalFunctionId.from_string
    >>| fun local_function_id ->
    {
      PyreflyReport.GlobalCallableId.module_id = PyreflyReport.ModuleId.from_int module_id;
      local_function_id;
    }


  let from_optional_json = function
    | Some json ->
        let open Core.Result.Monad_infix in
        from_json json >>| Option.some
    | None -> Ok None
end

module PyreflyTarget = struct
  let from_json json =
    let open Core.Result.Monad_infix in
    match json with
    | `Assoc [("Function", global_callable_id)] ->
        GlobalCallableId.from_json global_callable_id
        >>| fun global_callable_id -> PyreflyReport.PyreflyTarget.Function global_callable_id
    | `Assoc [("Overrides", global_callable_id)] ->
        GlobalCallableId.from_json global_callable_id
        >>| fun global_callable_id -> PyreflyReport.PyreflyTarget.Overrides global_callable_id
    | `String "FormatString" -> Ok PyreflyReport.PyreflyTarget.FormatString
    | _ ->
        Error
          (PyreflyReport.FormatError.UnexpectedJsonType { json; message = "Unknown type of target" })
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


module ProjectFile = struct
  module Module = struct
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
      JsonUtil.get_string_member json "python_version"
      >>= fun python_version_string ->
      Configuration.PythonVersion.from_string python_version_string
      |> Result.map_error ~f:(fun error -> PyreflyReport.FormatError.UnparsableString error)
      >>= fun python_version ->
      JsonUtil.get_string_member json "platform"
      >>= fun platform ->
      JsonUtil.get_optional_bool_member ~default:false json "is_test"
      >>= fun is_test ->
      JsonUtil.get_optional_bool_member ~default:false json "is_interface"
      >>= fun is_interface ->
      JsonUtil.get_optional_bool_member ~default:false json "is_init"
      >>= fun is_init ->
      JsonUtil.get_optional_bool_member ~default:false json "is_internal"
      >>| fun is_internal ->
      {
        PyreflyReport.ProjectFile.Module.module_id = PyreflyReport.ModuleId.from_int module_id;
        module_name = Reference.create module_name;
        absolute_source_path;
        relative_source_path;
        info_filename = Option.map ~f:PyreflyReport.ModuleInfoFilename.create info_filename;
        python_version;
        platform;
        is_test;
        is_interface;
        is_init;
        is_internal;
      }
  end

  let from_json json =
    let open Core.Result.Monad_infix in
    let parse_modules modules =
      modules |> List.map ~f:snd |> List.map ~f:Module.from_json |> Result.all
    in
    JsonUtil.check_object json
    >>= fun _ ->
    JsonUtil.check_format_version ~expected:1 json
    >>= fun () ->
    JsonUtil.get_object_member json "modules"
    >>= parse_modules
    >>= fun modules ->
    JsonUtil.get_list_member json "builtin_module_ids"
    >>= fun builtin_module_ids_json ->
    List.map builtin_module_ids_json ~f:(fun j ->
        JsonUtil.as_int j >>| PyreflyReport.ModuleId.from_int)
    |> Result.all
    >>= fun builtin_module_ids ->
    JsonUtil.get_list_member json "object_class_refs"
    >>= fun object_class_refs_json ->
    List.map ~f:GlobalClassId.from_json object_class_refs_json
    |> Result.all
    >>= fun object_class_refs ->
    JsonUtil.get_list_member json "dict_class_refs"
    >>= fun dict_class_refs_json ->
    List.map ~f:GlobalClassId.from_json dict_class_refs_json
    |> Result.all
    >>= fun dict_class_refs ->
    JsonUtil.get_list_member json "typing_module_ids"
    >>= fun typing_module_ids_json ->
    List.map typing_module_ids_json ~f:(fun j ->
        JsonUtil.as_int j >>| PyreflyReport.ModuleId.from_int)
    |> Result.all
    >>= fun typing_module_ids ->
    JsonUtil.get_list_member json "typing_mapping_class_refs"
    >>= fun typing_mapping_class_refs_json ->
    List.map ~f:GlobalClassId.from_json typing_mapping_class_refs_json
    |> Result.all
    >>| fun typing_mapping_class_refs ->
    {
      PyreflyReport.ProjectFile.modules;
      builtin_module_ids;
      object_class_refs;
      dict_class_refs;
      typing_module_ids;
      typing_mapping_class_refs;
    }


  let from_path_exn path =
    let () = Log.debug "Parsing pyrefly project file `%a`" PyrePath.pp path in
    let json = JsonUtil.read_json_file_exn path in
    match from_json json with
    | Ok project -> project
    | Error error ->
        raise
          (PyreflyReport.PyreflyFileFormatError
             { path; error = PyreflyReport.Error.FormatError error })
end

module ClassWithModifiers = struct
  let from_json json =
    let open Core.Result.Monad_infix in
    JsonUtil.get_object_member json "class"
    >>= fun class_name ->
    GlobalClassId.from_json (`Assoc class_name)
    >>= fun { PyreflyReport.GlobalClassId.module_id; local_class_id } ->
    JsonUtil.get_optional_list_member json "modifiers"
    >>| List.map ~f:JsonUtil.as_string
    >>= Result.all
    >>| List.map ~f:(fun modifier ->
            match Analysis.PyrePysaEnvironment.TypeModifier.from_string modifier with
            | Some modifier -> Ok modifier
            | None ->
                Error
                  (PyreflyReport.FormatError.UnexpectedJsonType
                     { json = `String modifier; message = "expected a modifier" }))
    >>= Result.all
    >>| fun modifiers ->
    {
      PyreflyTypeRep.ClassWithModifiers.module_id = PyreflyReport.ModuleId.to_int module_id;
      class_id = PyreflyReport.LocalClassId.to_int local_class_id;
      modifiers;
    }
end

module ClassNamesResult = struct
  let from_json json =
    let open Core.Result.Monad_infix in
    JsonUtil.get_list_member json "classes"
    >>| List.map ~f:ClassWithModifiers.from_json
    >>= Result.all
    >>= fun classes ->
    JsonUtil.get_optional_bool_member ~default:false json "is_exhaustive"
    >>| fun is_exhaustive -> { PyreflyTypeRep.ClassNamesFromType.classes; is_exhaustive }


  let from_optional_json = function
    | None -> Ok None
    | Some json -> from_json json |> Result.map ~f:Option.some
end

module PyreflyType = struct
  let from_json json =
    let open Core.Result.Monad_infix in
    JsonUtil.get_string_member json "string"
    >>= fun string ->
    parse_scalar_type_properties json
    >>= fun scalar_properties ->
    JsonUtil.get_optional_member json "class_names"
    |> ClassNamesResult.from_optional_json
    >>| fun class_names -> { PyreflyTypeRep.string; scalar_properties; class_names }
end

module CapturedVariable = struct
  let from_json json =
    let open Core.Result.Monad_infix in
    JsonUtil.get_string_member json "name"
    >>= fun name ->
    JsonUtil.get_object_member json "outer_function"
    >>= fun outer_function ->
    GlobalCallableId.from_json (`Assoc outer_function)
    >>| fun outer_function -> { PyreflyReport.CapturedVariable.name; outer_function }
end

module ModuleDefinitionsFile = struct
  module ParentScope = struct
    let from_json json =
      let open Core.Result.Monad_infix in
      match json with
      | `String "TopLevel" -> Ok PyreflyReport.ModuleDefinitionsFile.ParentScope.TopLevel
      | `Assoc [("Class", `Assoc [("location", `String location_string)])] ->
          PyreflyReport.parse_location location_string
          >>| fun location -> PyreflyReport.ModuleDefinitionsFile.ParentScope.Class location
      | `Assoc [("Function", `Assoc [("location", `String location_string)])] ->
          PyreflyReport.parse_location location_string
          >>| fun location -> PyreflyReport.ModuleDefinitionsFile.ParentScope.Function location
      | _ ->
          Error
            (PyreflyReport.FormatError.UnexpectedJsonType
               { json; message = "expected parent_scope" })
  end

  module FunctionParameter = struct
    let from_json json =
      let open Core.Result.Monad_infix in
      (match json with
      | `Assoc [(kind, json)] -> Ok (kind, json)
      | _ ->
          Error
            (PyreflyReport.FormatError.UnexpectedJsonType
               { json; message = "expected function parameter" }))
      >>= fun (kind, json) ->
      JsonUtil.get_object_member json "annotation"
      >>| (fun bindings -> `Assoc bindings)
      >>= PyreflyType.from_json
      >>= fun annotation ->
      JsonUtil.get_optional_bool_member ~default:false json "required"
      >>= fun required ->
      match kind with
      | "PosOnly" ->
          JsonUtil.get_optional_string_member json "name"
          >>| fun name ->
          PyreflyReport.ModuleDefinitionsFile.FunctionParameter.PosOnly
            { name; annotation; required }
      | "Pos" ->
          JsonUtil.get_string_member json "name"
          >>| fun name ->
          PyreflyReport.ModuleDefinitionsFile.FunctionParameter.Pos { name; annotation; required }
      | "VarArg" ->
          JsonUtil.get_optional_string_member json "name"
          >>| fun name ->
          PyreflyReport.ModuleDefinitionsFile.FunctionParameter.VarArg { name; annotation }
      | "KwOnly" ->
          JsonUtil.get_string_member json "name"
          >>| fun name ->
          PyreflyReport.ModuleDefinitionsFile.FunctionParameter.KwOnly
            { name; annotation; required }
      | "Kwargs" ->
          JsonUtil.get_optional_string_member json "name"
          >>| fun name ->
          PyreflyReport.ModuleDefinitionsFile.FunctionParameter.Kwargs { name; annotation }
      | _ ->
          Error
            (PyreflyReport.FormatError.UnexpectedJsonType
               { json; message = "expected function parameter" })
  end

  module FunctionParameters = struct
    let from_json json =
      let open Core.Result.Monad_infix in
      match json with
      | `Assoc [("List", `List parameters)] ->
          List.map ~f:FunctionParameter.from_json parameters
          |> Result.all
          >>| fun parameters ->
          PyreflyReport.ModuleDefinitionsFile.FunctionParameters.List parameters
      | `String "Ellipsis" -> Ok PyreflyReport.ModuleDefinitionsFile.FunctionParameters.Ellipsis
      | `String "ParamSpec" -> Ok PyreflyReport.ModuleDefinitionsFile.FunctionParameters.ParamSpec
      | _ ->
          Error
            (PyreflyReport.FormatError.UnexpectedJsonType
               { json; message = "expected function parameters" })
  end

  module FunctionSignature = struct
    let from_json json =
      let open Core.Result.Monad_infix in
      JsonUtil.get_member json "parameters"
      >>= FunctionParameters.from_json
      >>= fun parameters ->
      JsonUtil.get_object_member json "return_annotation"
      >>| (fun bindings -> `Assoc bindings)
      >>= PyreflyType.from_json
      >>| fun return_annotation ->
      { PyreflyReport.ModuleDefinitionsFile.FunctionSignature.parameters; return_annotation }
  end

  let parse_decorator_callees bindings =
    let extract_global_callable_id_from_target_exn = function
      | PyreflyReport.PyreflyTarget.Function global_callable_id
      | PyreflyReport.PyreflyTarget.Overrides global_callable_id ->
          global_callable_id
      | target ->
          Format.asprintf
            "Unexpected type of decorator callee: `%a`"
            PyreflyReport.PyreflyTarget.pp
            target
          |> failwith
    in
    let open Core.Result.Monad_infix in
    let parse_binding (key, value) =
      PyreflyReport.parse_location key
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
        PyreflyReport.ModuleDefinitionsFile.FunctionDefinition.name;
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
  end

  module ClassMro = struct
    let from_json = function
      | `Assoc [("Resolved", `List classes)] ->
          let open Core.Result.Monad_infix in
          classes
          |> List.map ~f:GlobalClassId.from_json
          |> Result.all
          >>| fun classes -> PyreflyReport.ModuleDefinitionsFile.ClassMro.Resolved classes
      | `String "Cyclic" -> Ok PyreflyReport.ModuleDefinitionsFile.ClassMro.Cyclic
      | json ->
          Error
            (PyreflyReport.FormatError.UnexpectedJsonType { json; message = "expected a class mro" })
  end

  module PyreflyClassField = struct
    let from_json ~name json =
      let open Core.Result.Monad_infix in
      JsonUtil.get_member json "type"
      >>= PyreflyType.from_json
      >>= fun type_ ->
      JsonUtil.get_optional_string_member json "explicit_annotation"
      >>= fun explicit_annotation ->
      JsonUtil.get_optional_string_member json "location"
      >>= (function
            | Some location -> PyreflyReport.parse_location location >>| Option.some
            | None -> Ok None)
      >>= fun location ->
      JsonUtil.get_optional_string_member json "declaration_kind"
      >>= (function
            | Some declaration_kind ->
                PyreflyReport.ClassFieldDeclarationKind.from_string declaration_kind >>| Option.some
            | None -> Ok None)
      >>| fun declaration_kind ->
      {
        PyreflyReport.ModuleDefinitionsFile.PyreflyClassField.name;
        type_;
        explicit_annotation;
        location;
        declaration_kind;
      }
  end

  module ClassDefinition = struct
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
      >>| List.map ~f:(fun (name, json) -> PyreflyClassField.from_json ~name json)
      >>= Result.all
      >>= fun fields ->
      JsonUtil.get_optional_object_member json "decorator_callees"
      >>= parse_decorator_callees
      >>| fun decorator_callees ->
      {
        PyreflyReport.ModuleDefinitionsFile.ClassDefinition.name;
        local_class_id = PyreflyReport.LocalClassId.from_int class_id;
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

  module PyreflyGlobalVariable = struct
    let from_json ~name json =
      let open Core.Result.Monad_infix in
      JsonUtil.get_optional_member json "type"
      |> Option.map ~f:PyreflyType.from_json
      |> (function
           | Some result -> Result.map ~f:Option.some result
           | None -> Ok None)
      >>= fun type_ ->
      JsonUtil.get_string_member json "location"
      >>= PyreflyReport.parse_location
      >>| fun location ->
      { PyreflyReport.ModuleDefinitionsFile.PyreflyGlobalVariable.name; type_; location }
  end

  let from_json json =
    let open Core.Result.Monad_infix in
    let parse_function_definitions function_definitions =
      function_definitions
      |> List.map ~f:(fun (local_function_id, function_definition) ->
             PyreflyReport.LocalFunctionId.from_string local_function_id
             >>= fun local_function_id ->
             FunctionDefinition.from_json ~local_function_id function_definition
             >>| fun function_definition -> local_function_id, function_definition)
      |> Result.all
      >>| PyreflyReport.LocalFunctionId.Map.of_alist_exn
    in
    let parse_class_definitions class_definitions =
      class_definitions
      |> List.map ~f:(fun (location, class_definition) ->
             PyreflyReport.parse_location location
             >>= fun location ->
             ClassDefinition.from_json ~name_location:location class_definition
             >>| fun class_definition -> location, class_definition)
      |> Result.all
      >>| Location.Map.of_alist_exn
    in
    let parse_global_variables global_variables =
      global_variables
      |> List.map ~f:(fun (name, json) -> PyreflyGlobalVariable.from_json ~name json)
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
      PyreflyReport.ModuleDefinitionsFile.module_id = PyreflyReport.ModuleId.from_int module_id;
      function_definitions;
      class_definitions;
      global_variables;
    }


  let from_path_exn ~pyrefly_directory path =
    let path =
      pyrefly_directory
      |> PyrePath.append ~element:"definitions"
      |> PyrePath.append ~element:(PyreflyReport.ModuleInfoFilename.raw path)
    in
    let () = Log.debug "Parsing pyrefly module definitions file %a" PyrePath.pp path in
    let json = JsonUtil.read_json_file_exn path in
    match from_json json with
    | Ok module_info -> module_info
    | Error error ->
        raise
          (PyreflyReport.PyreflyFileFormatError
             { path; error = PyreflyReport.Error.FormatError error })
end

module ModuleTypeOfExpressions = struct
  let from_json json =
    let open Core.Result.Monad_infix in
    let parse_function_type_of_expressions (function_id_string, function_json) =
      PyreflyReport.LocalFunctionId.from_string function_id_string
      >>= fun function_id ->
      JsonUtil.check_object function_json
      >>= fun _ ->
      JsonUtil.get_list_member function_json "type_table"
      >>= fun type_table_json ->
      type_table_json
      |> List.map ~f:PyreflyType.from_json
      |> Result.all
      >>= fun type_table ->
      let types = Array.of_list type_table in
      JsonUtil.get_object_member function_json "locations"
      >>= fun locations_entries ->
      locations_entries
      |> List.map ~f:(fun (location_str, type_id_json) ->
             PyreflyReport.parse_location location_str
             >>= fun location ->
             JsonUtil.as_int type_id_json
             >>| fun type_id ->
             {
               PyreflyReport.ModuleTypeOfExpressions.TypeAtLocation.location;
               type_ = PyreflyReport.ModuleTypeOfExpressions.LocalTypeId.of_int type_id;
             })
      |> Result.all
      >>| fun locations ->
      {
        PyreflyReport.ModuleTypeOfExpressions.FunctionTypeOfExpressions.function_id;
        types;
        locations;
      }
    in
    JsonUtil.check_object json
    >>= fun _ ->
    JsonUtil.check_format_version ~expected:1 json
    >>= fun () ->
    JsonUtil.get_int_member json "module_id"
    >>= fun module_id ->
    JsonUtil.get_object_member json "functions"
    >>= fun functions_entries ->
    functions_entries
    |> List.map ~f:parse_function_type_of_expressions
    |> Result.all
    >>| fun functions_list ->
    {
      PyreflyReport.ModuleTypeOfExpressions.module_id = PyreflyReport.ModuleId.from_int module_id;
      functions = functions_list;
    }


  let from_path_exn ~pyrefly_directory path =
    let path =
      pyrefly_directory
      |> PyrePath.append ~element:"type_of_expressions"
      |> PyrePath.append ~element:(PyreflyReport.ModuleInfoFilename.raw path)
    in
    let () = Log.debug "Parsing pyrefly module type-of-expressions file %a" PyrePath.pp path in
    let json = JsonUtil.read_json_file_exn path in
    match from_json json with
    | Ok module_info -> module_info
    | Error error ->
        raise
          (PyreflyReport.PyreflyFileFormatError
             { path; error = PyreflyReport.Error.FormatError error })
end

module ModuleCallGraphs = struct
  module PyreflyImplicitReceiver = struct
    let from_json = function
      | `String "TrueWithClassReceiver" ->
          Ok PyreflyReport.ModuleCallGraphs.PyreflyImplicitReceiver.TrueWithClassReceiver
      | `String "TrueWithObjectReceiver" ->
          Ok PyreflyReport.ModuleCallGraphs.PyreflyImplicitReceiver.TrueWithObjectReceiver
      | `String "False" -> Ok PyreflyReport.ModuleCallGraphs.PyreflyImplicitReceiver.False
      | json ->
          Error
            (PyreflyReport.FormatError.UnexpectedJsonType
               { json; message = "expected implicit receiver" })
  end

  module PyreflyCallTarget = struct
    let from_json json =
      let open Core.Result.Monad_infix in
      JsonUtil.get_member json "target"
      >>= PyreflyTarget.from_json
      >>= fun target ->
      (match JsonUtil.get_optional_member json "implicit_receiver" with
      | Some implicit_receiver_json -> PyreflyImplicitReceiver.from_json implicit_receiver_json
      | None -> Ok PyreflyReport.ModuleCallGraphs.PyreflyImplicitReceiver.False)
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
        PyreflyReport.ModuleCallGraphs.PyreflyCallTarget.target;
        implicit_receiver;
        implicit_dunder_call;
        receiver_class;
        is_class_method;
        is_static_method;
        return_type;
      }
  end

  module PyreflyUnresolved = struct
    let from_json = function
      | `String "False" -> Ok CallGraph.Unresolved.False
      | `Assoc [("True", `String reason)] as json -> (
          match CallGraph.Unresolved.reason_from_string reason with
          | Some reason -> Ok (CallGraph.Unresolved.True reason)
          | None ->
              Error
                (PyreflyReport.FormatError.UnexpectedJsonType
                   { json; message = "unknown unresolved reason" }))
      | json ->
          Error
            (PyreflyReport.FormatError.UnexpectedJsonType { json; message = "expected unresolved" })
  end

  module PyreflyHigherOrderParameter = struct
    let from_json json =
      let open Core.Result.Monad_infix in
      let parse_call_target_list targets =
        targets |> List.map ~f:PyreflyCallTarget.from_json |> Result.all
      in
      JsonUtil.get_optional_list_member json "call_targets"
      >>= parse_call_target_list
      >>= fun call_targets ->
      JsonUtil.get_int_member json "index"
      >>= fun index ->
      JsonUtil.get_optional_member json "unresolved"
      |> (function
           | Some json -> PyreflyUnresolved.from_json json
           | None -> Ok CallGraph.Unresolved.False)
      >>| fun unresolved ->
      { PyreflyReport.ModuleCallGraphs.PyreflyHigherOrderParameter.index; call_targets; unresolved }
  end

  module PyreflyHigherOrderParameterMap = struct
    let from_json json =
      let open Core.Result.Monad_infix in
      json
      |> JsonUtil.check_object
      >>| List.map ~f:(fun (_index, higher_order_parameter) ->
              PyreflyHigherOrderParameter.from_json higher_order_parameter)
      >>= Result.all
  end

  module PyreflyGlobalVariable = struct
    let from_json json =
      let open Core.Result.Monad_infix in
      JsonUtil.get_int_member json "module_id"
      >>= fun module_id ->
      JsonUtil.get_string_member json "name"
      >>| fun name ->
      {
        PyreflyReport.ModuleCallGraphs.PyreflyGlobalVariable.module_id =
          PyreflyReport.ModuleId.from_int module_id;
        name;
      }
  end

  module PyreflyCallCallees = struct
    let from_json json =
      let open Core.Result.Monad_infix in
      let parse_call_target_list targets =
        targets |> List.map ~f:PyreflyCallTarget.from_json |> Result.all
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
           | Some json -> PyreflyHigherOrderParameterMap.from_json json
           | None -> Ok PyreflyReport.ModuleCallGraphs.PyreflyHigherOrderParameterMap.empty)
      >>= fun higher_order_parameters ->
      JsonUtil.get_optional_member json "unresolved"
      |> (function
           | Some json -> PyreflyUnresolved.from_json json
           | None -> Ok CallGraph.Unresolved.False)
      >>| fun unresolved ->
      {
        PyreflyReport.ModuleCallGraphs.PyreflyCallCallees.call_targets;
        init_targets;
        new_targets;
        higher_order_parameters;
        unresolved;
      }
  end

  module PyreflyAttributeAccessCallees = struct
    let from_json json =
      let open Core.Result.Monad_infix in
      let parse_call_target_list targets =
        targets |> List.map ~f:PyreflyCallTarget.from_json |> Result.all
      in
      JsonUtil.get_member json "if_called"
      >>= PyreflyCallCallees.from_json
      >>= fun if_called ->
      JsonUtil.get_optional_list_member json "property_setters"
      >>= parse_call_target_list
      >>= fun property_setters ->
      JsonUtil.get_optional_list_member json "property_getters"
      >>= parse_call_target_list
      >>= fun property_getters ->
      JsonUtil.get_optional_list_member json "global_targets"
      >>| List.map ~f:PyreflyGlobalVariable.from_json
      >>= Result.all
      >>= fun global_targets ->
      JsonUtil.get_optional_bool_member ~default:false json "is_attribute"
      >>| fun is_attribute ->
      {
        PyreflyReport.ModuleCallGraphs.PyreflyAttributeAccessCallees.if_called;
        property_setters;
        property_getters;
        global_targets;
        is_attribute;
      }
  end

  module PyreflyIdentifierCallees = struct
    let from_json json =
      let open Core.Result.Monad_infix in
      JsonUtil.get_member json "if_called"
      >>= PyreflyCallCallees.from_json
      >>= fun if_called ->
      JsonUtil.get_optional_list_member json "global_targets"
      >>| List.map ~f:PyreflyGlobalVariable.from_json
      >>= Result.all
      >>= fun global_targets ->
      JsonUtil.get_optional_list_member json "captured_variables"
      >>| List.map ~f:CapturedVariable.from_json
      >>= Result.all
      >>| fun captured_variables ->
      {
        PyreflyReport.ModuleCallGraphs.PyreflyIdentifierCallees.if_called;
        global_targets;
        captured_variables;
      }
  end

  module PyreflyDefineCallees = struct
    let from_json json =
      let open Core.Result.Monad_infix in
      let parse_call_target_list targets =
        targets |> List.map ~f:PyreflyCallTarget.from_json |> Result.all
      in
      JsonUtil.get_list_member json "define_targets"
      >>= parse_call_target_list
      >>| fun define_targets ->
      { PyreflyReport.ModuleCallGraphs.PyreflyDefineCallees.define_targets }
  end

  module PyreflyFormatStringArtificialCallees = struct
    let from_json json =
      let open Core.Result.Monad_infix in
      let parse_call_target_list targets =
        targets |> List.map ~f:PyreflyCallTarget.from_json |> Result.all
      in
      JsonUtil.get_list_member json "targets"
      >>= parse_call_target_list
      >>| fun targets ->
      { PyreflyReport.ModuleCallGraphs.PyreflyFormatStringArtificialCallees.targets }
  end

  module PyreflyFormatStringStringifyCallees = struct
    let from_json json =
      let open Core.Result.Monad_infix in
      let parse_call_target_list targets =
        targets |> List.map ~f:PyreflyCallTarget.from_json |> Result.all
      in
      JsonUtil.get_optional_list_member json "targets"
      >>= parse_call_target_list
      >>= fun targets ->
      JsonUtil.get_optional_member json "unresolved"
      |> (function
           | Some json -> PyreflyUnresolved.from_json json
           | None -> Ok CallGraph.Unresolved.False)
      >>| fun unresolved ->
      { PyreflyReport.ModuleCallGraphs.PyreflyFormatStringStringifyCallees.targets; unresolved }
  end

  module PyreflyReturnShimCallees = struct
    let parse_argument_mapping = function
      | `String "ReturnExpression" -> Ok CallGraph.ReturnShimCallees.ReturnExpression
      | `String "ReturnExpressionElement" -> Ok CallGraph.ReturnShimCallees.ReturnExpressionElement
      | argument_mapping ->
          Error
            (PyreflyReport.FormatError.UnexpectedJsonType
               { json = argument_mapping; message = "Unknown argument mapping" })


    let from_json json =
      let open Core.Result.Monad_infix in
      let parse_list ~parse_element list = list |> List.map ~f:parse_element |> Result.all in
      JsonUtil.get_optional_list_member json "targets"
      >>= parse_list ~parse_element:PyreflyCallTarget.from_json
      >>= fun targets ->
      JsonUtil.get_optional_list_member json "arguments"
      >>= parse_list ~parse_element:parse_argument_mapping
      >>| fun arguments ->
      { PyreflyReport.ModuleCallGraphs.PyreflyReturnShimCallees.targets; arguments }
  end

  module PyreflyExpressionCallees = struct
    let from_json json =
      let open Core.Result.Monad_infix in
      match json with
      | `Assoc [("Call", call_callees)] ->
          PyreflyCallCallees.from_json call_callees
          >>| fun call_callees ->
          PyreflyReport.ModuleCallGraphs.PyreflyExpressionCallees.Call call_callees
      | `Assoc [("Identifier", identifier_callees)] ->
          PyreflyIdentifierCallees.from_json identifier_callees
          >>| fun identifier_callees ->
          PyreflyReport.ModuleCallGraphs.PyreflyExpressionCallees.Identifier identifier_callees
      | `Assoc [("AttributeAccess", attribute_access_callees)] ->
          PyreflyAttributeAccessCallees.from_json attribute_access_callees
          >>| fun attribute_access_callees ->
          PyreflyReport.ModuleCallGraphs.PyreflyExpressionCallees.AttributeAccess
            attribute_access_callees
      | `Assoc [("Define", define_callees)] ->
          PyreflyDefineCallees.from_json define_callees
          >>| fun define_callees ->
          PyreflyReport.ModuleCallGraphs.PyreflyExpressionCallees.Define define_callees
      | `Assoc [("FormatStringArtificial", format_string_callees)] ->
          PyreflyFormatStringArtificialCallees.from_json format_string_callees
          >>| fun format_string_callees ->
          PyreflyReport.ModuleCallGraphs.PyreflyExpressionCallees.FormatStringArtificial
            format_string_callees
      | `Assoc [("FormatStringStringify", format_string_callees)] ->
          PyreflyFormatStringStringifyCallees.from_json format_string_callees
          >>| fun format_string_callees ->
          PyreflyReport.ModuleCallGraphs.PyreflyExpressionCallees.FormatStringStringify
            format_string_callees
      | `Assoc [("Return", return_callees)] ->
          PyreflyReturnShimCallees.from_json return_callees
          >>| fun return_shim_callees ->
          PyreflyReport.ModuleCallGraphs.PyreflyExpressionCallees.Return return_shim_callees
      | _ ->
          Error
            (PyreflyReport.FormatError.UnexpectedJsonType
               { json; message = "expected expression callees" })
  end

  module PyreflyCallGraph = struct
    let from_json json =
      let open Core.Result.Monad_infix in
      json
      |> JsonUtil.check_object
      >>| List.map ~f:(fun (expression_identifier, expression_callees) ->
              ExpressionIdentifier.from_json_key expression_identifier
              |> Result.map_error ~f:(fun error ->
                     PyreflyReport.FormatError.UnexpectedJsonType
                       { json = `String expression_identifier; message = error })
              >>| ExpressionIdentifier.map_location ~f:PyreflyReport.fixup_location
              >>= fun expression_identifier ->
              PyreflyExpressionCallees.from_json expression_callees
              >>| fun callees ->
              { PyreflyReport.ModuleCallGraphs.CallGraphEdge.expression_identifier; callees })
      >>= Result.all
  end

  let from_json json =
    let open Core.Result.Monad_infix in
    let parse_call_graphs call_graphs =
      call_graphs
      |> List.map ~f:(fun (function_id, call_graph) ->
             PyreflyReport.LocalFunctionId.from_string function_id
             >>= fun function_id ->
             PyreflyCallGraph.from_json call_graph >>| fun call_graph -> function_id, call_graph)
      |> Result.all
      >>| PyreflyReport.LocalFunctionId.Map.of_alist_exn
    in
    JsonUtil.check_object json
    >>= fun _ ->
    JsonUtil.check_format_version ~expected:1 json
    >>= fun () ->
    JsonUtil.get_int_member json "module_id"
    >>= fun module_id ->
    JsonUtil.get_object_member json "call_graphs"
    >>= parse_call_graphs
    >>| fun call_graphs ->
    {
      PyreflyReport.ModuleCallGraphs.module_id = PyreflyReport.ModuleId.from_int module_id;
      call_graphs;
    }


  let from_path_exn ~pyrefly_directory path =
    let path =
      pyrefly_directory
      |> PyrePath.append ~element:"call_graphs"
      |> PyrePath.append ~element:(PyreflyReport.ModuleInfoFilename.raw path)
    in
    let () = Log.debug "Parsing pyrefly module call-graphs file %a" PyrePath.pp path in
    let json = JsonUtil.read_json_file_exn path in
    match from_json json with
    | Ok module_info -> module_info
    | Error error ->
        raise
          (PyreflyReport.PyreflyFileFormatError
             { path; error = PyreflyReport.Error.FormatError error })
end

module TypeErrors = struct
  module PyreflyError = struct
    let from_json json =
      let open Core.Result.Monad_infix in
      JsonUtil.get_string_member json "module_name"
      >>= fun module_name ->
      JsonUtil.get_object_member json "module_path"
      >>= fun module_path ->
      ModulePath.from_json (`Assoc module_path)
      >>= fun module_path ->
      JsonUtil.get_string_member json "location"
      >>= PyreflyReport.parse_location
      >>= fun location ->
      JsonUtil.get_string_member json "kind"
      >>= fun kind ->
      JsonUtil.get_string_member json "message"
      >>| fun message ->
      { PyreflyReport.TypeErrors.PyreflyError.module_name; module_path; location; kind; message }
  end

  let from_json json =
    let open Core.Result.Monad_infix in
    JsonUtil.check_object json
    >>= fun _ ->
    JsonUtil.check_format_version ~expected:1 json
    >>= fun () ->
    JsonUtil.get_list_member json "errors"
    >>= fun errors -> List.map ~f:PyreflyError.from_json errors |> Result.all


  let from_path_exn ~pyrefly_directory =
    let path = pyrefly_directory |> PyrePath.append ~element:"errors.json" in
    let () = Log.debug "Parsing pyrefly type errors file %a" PyrePath.pp path in
    let json = JsonUtil.read_json_file_exn path in
    match from_json json with
    | Ok errors -> { PyreflyReport.TypeErrors.errors }
    | Error error ->
        raise
          (PyreflyReport.PyreflyFileFormatError
             { path; error = PyreflyReport.Error.FormatError error })
end
