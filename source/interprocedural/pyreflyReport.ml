(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Shared types and format-agnostic utilities for pyrefly report files. *)

open Core
open Ast
module Pyre1Api = Analysis.PyrePysaEnvironment
module PyreflyType = Pyre1Api.PyreflyType
module PysaType = Pyre1Api.PysaType
module ScalarTypeProperties = Pyre1Api.ScalarTypeProperties

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
    | InvalidCapnpError of string
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

(* Path of a module, equivalent of the `pyrefly_python::module_path::ModulePathDetails` rust
   type. *)
module ModulePath = struct
  type t =
    | Filesystem of ArtifactPath.t
    | Namespace of PyrePath.t
    | Memory of PyrePath.t
    | BundledTypeshed of PyrePath.t
    | BundledTypeshedThirdParty of PyrePath.t
    | BundledThirdParty of PyrePath.t
  [@@deriving compare, equal, show]

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
    | BundledThirdParty path ->
        Some
          (pyrefly_directory
          |> PyrePath.append ~element:"third_party"
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

  module Map : Map.S with type Key.t = t
end = struct
  module T = struct
    type t = int [@@deriving compare, equal, sexp, hash, show]
  end

  include T

  let from_int = Fn.id

  let to_int = Fn.id

  let of_string = Int.of_string

  module Map = Map.Make (T)
end

(* Index of a function definition within a module, assigned by pyrefly. *)
module FuncDefIndex : sig
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
    | Function of FuncDefIndex.t
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
    | FunctionDecoratedTarget of FuncDefIndex.t
  [@@deriving compare, equal, show, sexp]

  val from_string : string -> (t, FormatError.t) result

  val create_function : FuncDefIndex.t -> t

  val is_class_field : t -> bool

  module Map : Map.S with type Key.t = t
end = struct
  module T = struct
    type t =
      | Function of FuncDefIndex.t
      | ModuleTopLevel
      | ClassTopLevel of LocalClassId.t
      | ClassField of {
          class_id: LocalClassId.t;
          name: string;
        }
      | FunctionDecoratedTarget of FuncDefIndex.t
    [@@deriving compare, equal, show, sexp]
  end

  include T

  let from_string string =
    match String.lsplit2 string ~on:':' with
    | None when String.equal string "MTL" -> Ok ModuleTopLevel
    | Some ("F", func_def_index) -> Ok (Function (FuncDefIndex.of_string func_def_index))
    | Some ("CTL", class_id) -> Ok (ClassTopLevel (LocalClassId.of_string class_id))
    | Some ("CF", class_field) -> (
        match String.lsplit2 class_field ~on:':' with
        | Some (class_id, name) ->
            Ok (ClassField { class_id = LocalClassId.of_string class_id; name })
        | None -> Error (FormatError.UnparsableString string))
    | Some ("FDT", func_def_index) ->
        Ok (FunctionDecoratedTarget (FuncDefIndex.of_string func_def_index))
    | _ -> Error (FormatError.UnparsableString string)


  let create_function func_def_index = Function func_def_index

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
end

module PyreflyTarget = struct
  type t =
    | Function of GlobalCallableId.t
    | Overrides of GlobalCallableId.t
    | FormatString
  [@@deriving compare, equal, show]
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
      python_version: Configuration.PythonVersion.t;
      platform: string;
      is_test: bool;
      is_interface: bool;
      is_init: bool;
      is_internal: bool;
    }
    [@@deriving equal, show]
  end

  type t = {
    modules: Module.t list;
    builtin_module_ids: ModuleId.t list;
    object_class_refs: GlobalClassId.t list;
    dict_class_refs: GlobalClassId.t list;
    typing_module_ids: ModuleId.t list;
    typing_mapping_class_refs: GlobalClassId.t list;
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
end

(* Information from pyrefly about all definitions in a given module, stored as a
   `<root>/definitions/<module>:<id>.json` file. This matches the
   `pyrefly::report::pysa::PysaModuleDefinitions` rust type. *)
module ModuleDefinitionsFile = struct
  module ParentScope = struct
    type t =
      | TopLevel
      | Class of LocalClassId.t
      | Function of FuncDefIndex.t
    [@@deriving equal, show]
  end

  module FunctionParameter = struct
    type t =
      | PosOnly of {
          name: string option;
          annotation: PyreflyType.t;
          required: bool;
        }
      | Pos of {
          name: string;
          annotation: PyreflyType.t;
          required: bool;
        }
      | VarArg of {
          name: string option;
          annotation: PyreflyType.t;
        }
      | KwOnly of {
          name: string;
          annotation: PyreflyType.t;
          required: bool;
        }
      | Kwargs of {
          name: string option;
          annotation: PyreflyType.t;
        }
    [@@deriving equal, show]
  end

  module FunctionParameters = struct
    type t =
      | List of FunctionParameter.t list
      | Ellipsis
      | ParamSpec
    [@@deriving equal, show]
  end

  module FunctionSignature = struct
    type t = {
      parameters: FunctionParameters.t;
      return_annotation: PyreflyType.t;
    }
    [@@deriving equal, show]
  end

  module FunctionDefinition = struct
    type t = {
      name: string;
      name_location: Location.t option;
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

    let create_module_toplevel () =
      {
        name = Ast.Statement.toplevel_define_name;
        name_location = None;
        local_function_id = LocalFunctionId.ModuleTopLevel;
        parent = ParentScope.TopLevel;
        undecorated_signatures =
          [
            {
              FunctionSignature.parameters = FunctionParameters.List [];
              return_annotation =
                {
                  PyreflyType.string = "None";
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


    let create_class_toplevel ~local_class_id =
      {
        name = Ast.Statement.class_toplevel_define_name;
        name_location = None;
        local_function_id = LocalFunctionId.ClassTopLevel local_class_id;
        parent = ParentScope.Class local_class_id;
        undecorated_signatures =
          [
            {
              FunctionSignature.parameters = FunctionParameters.List [];
              return_annotation =
                {
                  PyreflyType.string = "None";
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
  end

  module PyreflyClassField = struct
    type t = {
      name: string;
      type_: PyreflyType.t;
      explicit_annotation: string option;
      location: Location.t option;
      declaration_kind: ClassFieldDeclarationKind.t option;
    }
    [@@deriving equal, show]
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
      fields: PyreflyClassField.t list;
      decorator_callees: GlobalCallableId.t list Location.SerializableMap.t;
    }
    [@@deriving equal, show]
  end

  module PyreflyGlobalVariable = struct
    type t = {
      name: string;
      type_: PyreflyType.t option;
      location: Location.t;
    }
  end

  type t = {
    (* TODO(T225700656): module_name and source_path are already specified in the Project module. We
       should probably remove those from the file format. *)
    module_id: ModuleId.t;
    function_definitions: FunctionDefinition.t LocalFunctionId.Map.t;
    class_definitions: ClassDefinition.t LocalClassId.Map.t;
    global_variables: PyreflyGlobalVariable.t list;
  }
end

(* Information from pyrefly about type of expressions in a given module, stored as a
   `<root>/type_of_expressions/<module>:<id>.json` file. This matches the
   `pyrefly::report::pysa::PysaModuleTypeOfExpressions` rust type. *)
module ModuleTypeOfExpressions = struct
  module LocalTypeId = struct
    type t = int [@@deriving equal]

    let of_int x = x

    let to_index x = x
  end

  module TypeAtLocation = struct
    type t = {
      location: Location.t;
      type_: LocalTypeId.t;
    }
  end

  module FunctionTypeOfExpressions = struct
    type t = {
      function_id: LocalFunctionId.t;
      types: PyreflyType.t array;
      locations: TypeAtLocation.t list;
    }
  end

  type t = {
    (* TODO(T225700656): module_name and source_path are already specified in the Project module. We
       should probably remove those from the file format. *)
    module_id: ModuleId.t;
    functions: FunctionTypeOfExpressions.t list;
  }
end

(* Mapping from function id to to call graph information. This represents the
   `pyrefly::report::pysa::PysaModuleCallGraphs` rust type. *)
module ModuleCallGraphs = struct
  module PyreflyImplicitReceiver = struct
    type t =
      | TrueWithClassReceiver
      | TrueWithObjectReceiver
      | False

    let is_true = function
      | TrueWithClassReceiver -> true
      | TrueWithObjectReceiver -> true
      | False -> false
  end

  module PyreflyCallTarget = struct
    type t = {
      target: PyreflyTarget.t;
      implicit_receiver: PyreflyImplicitReceiver.t;
      implicit_dunder_call: bool;
      receiver_class: GlobalClassId.t option;
      is_class_method: bool;
      is_static_method: bool;
      return_type: ScalarTypeProperties.t;
    }
  end

  module PyreflyHigherOrderParameter = struct
    type t = {
      index: int;
      call_targets: PyreflyCallTarget.t list;
      unresolved: CallGraph.Unresolved.t;
    }
  end

  module PyreflyHigherOrderParameterMap = struct
    type t = PyreflyHigherOrderParameter.t list

    let empty = []

    let data = Fun.id
  end

  module PyreflyGlobalVariable = struct
    type t = {
      module_id: ModuleId.t;
      name: string;
    }
  end

  module PyreflyCallCallees = struct
    type t = {
      call_targets: PyreflyCallTarget.t list;
      init_targets: PyreflyCallTarget.t list;
      new_targets: PyreflyCallTarget.t list;
      higher_order_parameters: PyreflyHigherOrderParameterMap.t;
      unresolved: CallGraph.Unresolved.t;
    }
  end

  module PyreflyAttributeAccessCallees = struct
    type t = {
      if_called: PyreflyCallCallees.t;
      property_setters: PyreflyCallTarget.t list;
      property_getters: PyreflyCallTarget.t list;
      global_targets: PyreflyGlobalVariable.t list;
      is_attribute: bool;
    }
  end

  module PyreflyIdentifierCallees = struct
    type t = {
      if_called: PyreflyCallCallees.t;
      global_targets: PyreflyGlobalVariable.t list;
      captured_variables: CapturedVariable.t list;
    }
  end

  module PyreflyDefineCallees = struct
    type t = { define_targets: PyreflyCallTarget.t list }
  end

  module PyreflyFormatStringArtificialCallees = struct
    type t = { targets: PyreflyCallTarget.t list }
  end

  module PyreflyFormatStringStringifyCallees = struct
    type t = {
      targets: PyreflyCallTarget.t list;
      unresolved: CallGraph.Unresolved.t;
    }
  end

  module PyreflyReturnShimCallees = struct
    type t = {
      targets: PyreflyCallTarget.t list;
      arguments: CallGraph.ReturnShimCallees.argument_mapping list;
    }
  end

  module PyreflyExpressionCallees = struct
    type t =
      | Call of PyreflyCallCallees.t
      | Identifier of PyreflyIdentifierCallees.t
      | AttributeAccess of PyreflyAttributeAccessCallees.t
      | Define of PyreflyDefineCallees.t
      | FormatStringArtificial of PyreflyFormatStringArtificialCallees.t
      | FormatStringStringify of PyreflyFormatStringStringifyCallees.t
      | Return of PyreflyReturnShimCallees.t
  end

  module CallGraphEdge = struct
    type t = {
      expression_identifier: ExpressionIdentifier.t;
      callees: PyreflyExpressionCallees.t;
    }
  end

  module PyreflyCallGraph = struct
    type t = CallGraphEdge.t list
  end

  type t = {
    module_id: ModuleId.t;
    call_graphs: PyreflyCallGraph.t LocalFunctionId.Map.t;
  }
end

(* Set of type errors parsed from pyrefly. This represents the
   `pyrefly::report::pysa::PysaTypeErrorsFile` rust type. *)
module TypeErrors = struct
  module PyreflyError = struct
    type t = {
      module_name: string;
      module_path: ModulePath.t;
      location: Location.t;
      kind: string;
      message: string;
    }
  end

  type t = { errors: PyreflyError.t list }
end
