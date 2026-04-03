(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Shared types and format-agnostic utilities for pyrefly report files. *)

open Core
module PyreflyType = Analysis.PyrePysaEnvironment.PyreflyType
module PysaType = Analysis.PyrePysaEnvironment.PysaType
module ScalarTypeProperties = Analysis.PyrePysaEnvironment.ScalarTypeProperties

module FormatError : sig
  type t =
    | UnexpectedJsonType of {
        json: Yojson.Safe.t;
        message: string;
      }
    | UnsupportedVersion of { version: int }
    | UnparsableString of string
  [@@deriving show]
end

module Error : sig
  type t =
    | InvalidJsonError of string
    | InvalidCapnpError of string
    | IOError of string
    | FormatError of FormatError.t
  [@@deriving show]
end

val fixup_location : Ast.Location.t -> Ast.Location.t

val parse_location : string -> (Ast.Location.t, FormatError.t) result

exception
  PyreflyFileFormatError of {
    path: PyrePath.t;
    error: Error.t;
  }

module ModulePath : sig
  type t =
    | Filesystem of ArtifactPath.t
    | Namespace of PyrePath.t
    | Memory of PyrePath.t
    | BundledTypeshed of PyrePath.t
    | BundledTypeshedThirdParty of PyrePath.t
  [@@deriving compare, equal, show]

  val artifact_file_path : pyrefly_directory:PyrePath.t -> t -> ArtifactPath.t option
end

module ModuleId : sig
  type t [@@deriving compare, equal, sexp, hash, show]

  val from_int : int -> t

  val to_int : t -> int

  val max : t -> t -> t

  val increment : t -> t
end

module LocalClassId : sig
  type t [@@deriving compare, equal, sexp, hash, show]

  val from_int : int -> t

  val to_int : t -> int

  val of_string : string -> t
end

module GlobalClassId : sig
  type t = {
    module_id: ModuleId.t;
    local_class_id: LocalClassId.t;
  }
  [@@deriving compare, equal, show]
end

module GlobalClassIdSharedMemoryKey : sig
  type t = GlobalClassId.t [@@deriving compare]

  val to_string : t -> string
end

module LocalFunctionId : sig
  type t =
    | Function of Ast.Location.t
    | ModuleTopLevel
    | ClassTopLevel of LocalClassId.t
    | ClassField of {
        class_id: LocalClassId.t;
        name: string;
      }
    | FunctionDecoratedTarget of Ast.Location.t
  [@@deriving compare, equal, show, sexp]

  val from_string : string -> (t, FormatError.t) result

  val create_function : Ast.Location.t -> t

  val is_class_field : t -> bool

  module Map : Map.S with type Key.t = t
end

module GlobalCallableId : sig
  type t = {
    module_id: ModuleId.t;
    local_function_id: LocalFunctionId.t;
  }
  [@@deriving compare, equal, show]
end

module PyreflyTarget : sig
  type t =
    | Function of GlobalCallableId.t
    | Overrides of GlobalCallableId.t
    | FormatString
  [@@deriving compare, equal, show]
end

module ModuleIdSharedMemoryKey : sig
  type t = ModuleId.t [@@deriving compare]

  val to_string : t -> string
end

module GlobalCallableIdSharedMemoryKey : sig
  type t = GlobalCallableId.t [@@deriving compare]

  val to_string : t -> string
end

module ModuleQualifier : sig
  type t [@@deriving compare, equal, sexp, hash, show]

  val create : path:string option -> Ast.Reference.t -> t

  val to_reference : t -> Ast.Reference.t

  val from_reference_unchecked : Ast.Reference.t -> t

  module Map : Map.S with type Key.t = t
end

module ModuleQualifierSharedMemoryKey : sig
  type t = ModuleQualifier.t [@@deriving compare]

  val to_string : t -> string
end

module ModuleInfoFilename : sig
  type t [@@deriving compare, equal, sexp, hash, show]

  val create : string -> t

  val raw : t -> string
end

module ProjectFile : sig
  module Module : sig
    type t = {
      module_id: ModuleId.t;
      module_name: Ast.Reference.t;
      absolute_source_path: ModulePath.t;
      relative_source_path: string option;
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

module ClassFieldDeclarationKind : sig
  type t =
    | DeclaredByAnnotation
    | DeclaredWithoutAnnotation
    | AssignedInBody
    | DefinedWithoutAssign
    | DefinedInMethod
  [@@deriving equal, compare, show]

  val from_string : string -> (t, FormatError.t) result
end

module CapturedVariable : sig
  type t = {
    name: string;
    outer_function: GlobalCallableId.t;
  }
  [@@deriving equal, show]
end

module ModuleDefinitionsFile : sig
  module ParentScope : sig
    type t =
      | TopLevel
      | Class of Ast.Location.t
      | Function of Ast.Location.t
    [@@deriving equal, show]
  end

  module FunctionParameter : sig
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

  module FunctionParameters : sig
    type t =
      | List of FunctionParameter.t list
      | Ellipsis
      | ParamSpec
    [@@deriving equal, show]
  end

  module FunctionSignature : sig
    type t = {
      parameters: FunctionParameters.t;
      return_annotation: PyreflyType.t;
    }
    [@@deriving equal, show]
  end

  module FunctionDefinition : sig
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
      decorator_callees: GlobalCallableId.t list Ast.Location.SerializableMap.t;
    }
    [@@deriving equal, show]

    val create_module_toplevel : unit -> t

    val create_class_toplevel : name_location:Ast.Location.t -> local_class_id:LocalClassId.t -> t
  end

  module ClassMro : sig
    type t =
      | Resolved of GlobalClassId.t list
      | Cyclic
    [@@deriving equal, show]
  end

  module PyreflyClassField : sig
    type t = {
      name: string;
      type_: PyreflyType.t;
      explicit_annotation: string option;
      location: Ast.Location.t option;
      declaration_kind: ClassFieldDeclarationKind.t option;
    }
    [@@deriving equal, show]
  end

  module ClassDefinition : sig
    type t = {
      name: string;
      local_class_id: LocalClassId.t;
      name_location: Ast.Location.t;
      parent: ParentScope.t;
      bases: GlobalClassId.t list;
      mro: ClassMro.t;
      is_synthesized: bool;
      is_dataclass: bool;
      is_named_tuple: bool;
      is_typed_dict: bool;
      fields: PyreflyClassField.t list;
      decorator_callees: GlobalCallableId.t list Ast.Location.SerializableMap.t;
    }
    [@@deriving equal, show]
  end

  module PyreflyGlobalVariable : sig
    type t = {
      name: string;
      type_: PyreflyType.t option;
      location: Ast.Location.t;
    }
  end

  type t = {
    module_id: ModuleId.t;
    function_definitions: FunctionDefinition.t LocalFunctionId.Map.t;
    class_definitions: ClassDefinition.t Ast.Location.Map.t;
    global_variables: PyreflyGlobalVariable.t list;
  }
end

module ModuleTypeOfExpressions : sig
  module LocalTypeId : sig
    type t [@@deriving equal]

    val of_int : int -> t

    val to_index : t -> int
  end

  module TypeAtLocation : sig
    type t = {
      location: Ast.Location.t;
      type_: LocalTypeId.t;
    }
  end

  module FunctionTypeOfExpressions : sig
    type t = {
      function_id: LocalFunctionId.t;
      types: PyreflyType.t array;
      locations: TypeAtLocation.t list;
    }
  end

  type t = {
    module_id: ModuleId.t;
    functions: FunctionTypeOfExpressions.t list;
  }
end

module ModuleCallGraphs : sig
  module PyreflyImplicitReceiver : sig
    type t =
      | TrueWithClassReceiver
      | TrueWithObjectReceiver
      | False

    val is_true : t -> bool
  end

  module PyreflyCallTarget : sig
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

  module PyreflyHigherOrderParameter : sig
    type t = {
      index: int;
      call_targets: PyreflyCallTarget.t list;
      unresolved: CallGraph.Unresolved.t;
    }
  end

  module PyreflyHigherOrderParameterMap : sig
    type t = PyreflyHigherOrderParameter.t list

    val empty : t

    val data : t -> PyreflyHigherOrderParameter.t list
  end

  module PyreflyGlobalVariable : sig
    type t = {
      module_id: ModuleId.t;
      name: string;
    }
  end

  module PyreflyCallCallees : sig
    type t = {
      call_targets: PyreflyCallTarget.t list;
      init_targets: PyreflyCallTarget.t list;
      new_targets: PyreflyCallTarget.t list;
      higher_order_parameters: PyreflyHigherOrderParameterMap.t;
      unresolved: CallGraph.Unresolved.t;
    }
  end

  module PyreflyAttributeAccessCallees : sig
    type t = {
      if_called: PyreflyCallCallees.t;
      property_setters: PyreflyCallTarget.t list;
      property_getters: PyreflyCallTarget.t list;
      global_targets: PyreflyGlobalVariable.t list;
      is_attribute: bool;
    }
  end

  module PyreflyIdentifierCallees : sig
    type t = {
      if_called: PyreflyCallCallees.t;
      global_targets: PyreflyGlobalVariable.t list;
      captured_variables: CapturedVariable.t list;
    }
  end

  module PyreflyDefineCallees : sig
    type t = { define_targets: PyreflyCallTarget.t list }
  end

  module PyreflyFormatStringArtificialCallees : sig
    type t = { targets: PyreflyCallTarget.t list }
  end

  module PyreflyFormatStringStringifyCallees : sig
    type t = {
      targets: PyreflyCallTarget.t list;
      unresolved: CallGraph.Unresolved.t;
    }
  end

  module PyreflyReturnShimCallees : sig
    type t = {
      targets: PyreflyCallTarget.t list;
      arguments: CallGraph.ReturnShimCallees.argument_mapping list;
    }
  end

  module PyreflyExpressionCallees : sig
    type t =
      | Call of PyreflyCallCallees.t
      | Identifier of PyreflyIdentifierCallees.t
      | AttributeAccess of PyreflyAttributeAccessCallees.t
      | Define of PyreflyDefineCallees.t
      | FormatStringArtificial of PyreflyFormatStringArtificialCallees.t
      | FormatStringStringify of PyreflyFormatStringStringifyCallees.t
      | Return of PyreflyReturnShimCallees.t
  end

  module CallGraphEdge : sig
    type t = {
      expression_identifier: ExpressionIdentifier.t;
      callees: PyreflyExpressionCallees.t;
    }
  end

  module PyreflyCallGraph : sig
    type t = CallGraphEdge.t list
  end

  type t = {
    module_id: ModuleId.t;
    call_graphs: PyreflyCallGraph.t LocalFunctionId.Map.t;
  }
end

module TypeErrors : sig
  module PyreflyError : sig
    type t = {
      module_name: string;
      module_path: ModulePath.t;
      location: Ast.Location.t;
      kind: string;
      message: string;
    }
  end

  type t = { errors: PyreflyError.t list }
end
