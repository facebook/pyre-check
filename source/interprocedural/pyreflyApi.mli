(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Module that implements the PyrePysaApi using the results from a pyrefly run with
   --report-pysa. *)

open Core
module PysaType = Analysis.PyrePysaEnvironment.PysaType
module AstResult = Analysis.PyrePysaEnvironment.AstResult

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
    | IOError of string
    | FormatError of FormatError.t
  [@@deriving show]
end

exception
  PyreflyFileFormatError of {
    path: PyrePath.t;
    error: Error.t;
  }

module NameLocation : sig
  type t =
    | DefineName of
        Ast.Location.t (* Location of the name AST node, i.e location of `foo` in `def foo():` *)
    | ModuleTopLevel
    | ClassName of
        Ast.Location.t (* Location of the class AST node, i.e location of `Foo` in `class Foo:` *)
    | UnknonwnForClassField
  [@@deriving show]
end

module CallableMetadata : sig
  type t = {
    module_qualifier: Ast.Reference.t;
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

module PysaClassSummary : sig
  type t
end

(* API handle stored in the main process. The type `t` should not be sent to workers, since it's
   expensive to copy. *)
module ReadWrite : sig
  type t

  val create_from_directory
    :  scheduler:Scheduler.t ->
    scheduler_policies:Configuration.SchedulerPolicies.t ->
    configuration:Configuration.Analysis.t ->
    PyrePath.t ->
    t

  val parse_type_of_expressions
    :  t ->
    scheduler:Scheduler.t ->
    scheduler_policies:Configuration.SchedulerPolicies.t ->
    t

  val cleanup : t -> scheduler:Scheduler.t -> unit
end

(* Read-only API that can be sent to workers. Cheap to copy. *)
module ReadOnly : sig
  type t

  val of_read_write_api : ReadWrite.t -> t

  (* Return all qualifiers with source code *)
  val explicit_qualifiers : t -> Ast.Reference.t list

  val artifact_path_of_qualifier : t -> Ast.Reference.t -> ArtifactPath.t option

  val absolute_source_path_of_qualifier : t -> Ast.Reference.t -> string option

  val relative_path_of_qualifier : t -> Ast.Reference.t -> string option

  val get_class_names_for_qualifier
    :  t ->
    exclude_test_modules:bool ->
    Ast.Reference.t ->
    Ast.Reference.t list

  val all_classes : t -> scheduler:Scheduler.t -> string list

  val all_global_variables : t -> scheduler:Scheduler.t -> Ast.Reference.t list

  val get_define_names_for_qualifier
    :  t ->
    exclude_test_modules:bool ->
    Ast.Reference.t ->
    Ast.Reference.t list

  val get_qualifier_top_level_define_name : t -> Ast.Reference.t -> Ast.Reference.t

  val class_immediate_parents : t -> string -> string list

  val class_mro : t -> string -> string list

  val get_callable_metadata : t -> Ast.Reference.t -> CallableMetadata.t

  val get_callable_return_annotations
    :  t ->
    define_name:Ast.Reference.t ->
    define:Ast.Statement.Define.t ->
    PysaType.t list

  val get_callable_parameter_annotations
    :  t ->
    define_name:Ast.Reference.t ->
    Analysis.TaintAccessPath.NormalizedParameter.t list ->
    (Analysis.TaintAccessPath.NormalizedParameter.t * PysaType.t list) list

  val get_overriden_base_method
    :  t ->
    Analysis.PyrePysaEnvironment.MethodReference.t ->
    Analysis.PyrePysaEnvironment.MethodReference.t option

  val get_callable_captures
    :  t ->
    Ast.Reference.t ->
    Analysis.TaintAccessPath.CapturedVariable.t list

  val get_callable_decorator_callees
    :  t ->
    Ast.Reference.t ->
    Ast.Location.t ->
    Ast.Reference.t list option

  val get_methods_for_qualifier
    :  t ->
    exclude_test_modules:bool ->
    Ast.Reference.t ->
    Analysis.PyrePysaEnvironment.MethodReference.t list

  (* Is this a test module (i.e, unit test code that we shouldn't analyze) *)
  val is_test_qualifier : t -> Ast.Reference.t -> bool

  (* Is this a stub module, i.e a `.pyi` file. *)
  val is_stub_qualifier : t -> Ast.Reference.t -> bool

  (* Return the AST for the given function *)
  val get_define_opt : t -> Ast.Reference.t -> Ast.Statement.Define.t Ast.Node.t AstResult.t

  val get_undecorated_signatures
    :  t ->
    Ast.Reference.t ->
    Analysis.PyrePysaEnvironment.ModelQueries.FunctionSignature.t list

  val get_class_summary : t -> string -> PysaClassSummary.t

  val get_class_decorators_opt : t -> string -> Ast.Expression.t list AstResult.t

  val get_class_attributes
    :  t ->
    include_generated_attributes:bool ->
    only_simple_assignments:bool ->
    string ->
    string list option

  val get_class_attribute_inferred_type : t -> class_name:string -> attribute:string -> PysaType.t

  val get_class_attribute_explicit_annotation
    :  t ->
    class_name:string ->
    attribute:string ->
    string option

  val get_global_inferred_type : t -> qualifier:Ast.Reference.t -> name:string -> PysaType.t option

  val target_from_define_name : t -> override:bool -> Ast.Reference.t -> Target.t

  val parse_call_graphs
    :  t ->
    scheduler:Scheduler.t ->
    scheduler_policies:Configuration.SchedulerPolicies.t ->
    method_has_overrides:(Target.Method.t -> bool) ->
    global_is_string_literal:(Ast.Reference.t -> bool) ->
    store_shared_memory:bool ->
    attribute_targets:Target.Set.t ->
    skip_analysis_targets:Target.HashSet.t ->
    definitions:Target.t list ->
    create_dependency_for:CallGraph.AllTargetsUseCase.t ->
    redirect_to_decorated:(Target.t -> Target.t option) ->
    transform_call_graph:
      (t -> Target.t -> CallGraph.DefineCallGraph.t -> CallGraph.DefineCallGraph.t) ->
    CallGraph.SharedMemory.call_graphs

  val parse_type_errors : t -> Analysis.AnalysisError.Instantiated.t list

  val get_type_of_expression
    :  t ->
    qualifier:Ast.Reference.t ->
    location:Ast.Location.t ->
    PysaType.t option

  module Type : sig
    val scalar_properties : t -> PysaType.t -> Analysis.PyrePysaEnvironment.ScalarTypeProperties.t

    val get_class_names : t -> PysaType.t -> Analysis.PyrePysaEnvironment.ClassNamesFromType.t

    val is_dictionary_or_mapping : t -> PysaType.t -> bool
  end

  module ClassSummary : sig
    val has_custom_new : t -> PysaClassSummary.t -> bool

    val is_dataclass : t -> PysaClassSummary.t -> bool

    val is_named_tuple : t -> PysaClassSummary.t -> bool

    val is_typed_dict : t -> PysaClassSummary.t -> bool

    val dataclass_ordered_attributes : t -> PysaClassSummary.t -> string list

    val typed_dictionary_attributes : t -> PysaClassSummary.t -> string list

    val named_tuple_attributes : t -> PysaClassSummary.t -> string list
  end

  val named_tuple_attributes : t -> string -> string list option
end

val add_builtins_prefix : Ast.Reference.t -> Ast.Reference.t

val strip_builtins_prefix : Ast.Reference.t -> Ast.Reference.t

val target_symbolic_name : Ast.Reference.t -> Ast.Reference.t

module ModelQueries : sig
  module Function = Analysis.PyrePysaEnvironment.ModelQueries.Function
  module Global = Analysis.PyrePysaEnvironment.ModelQueries.Global

  val resolve_qualified_name_to_global
    :  ReadOnly.t ->
    is_property_getter:bool ->
    is_property_setter:bool ->
    Ast.Reference.t ->
    Global.t option

  val class_method_signatures
    :  ReadOnly.t ->
    Ast.Reference.t ->
    (Ast.Reference.t * Ast.Statement.Define.Signature.t option) list option
end

module InContext : sig
  type t

  val create_at_function_scope
    :  ReadOnly.t ->
    module_qualifier:Ast.Reference.t ->
    define_name:Ast.Reference.t ->
    call_graph:CallGraph.DefineCallGraph.t ->
    t

  val create_at_statement_scope
    :  ReadOnly.t ->
    module_qualifier:Ast.Reference.t ->
    define_name:Ast.Reference.t ->
    call_graph:CallGraph.DefineCallGraph.t ->
    statement_key:int ->
    t

  val pyre_api : t -> ReadOnly.t

  val call_graph : t -> CallGraph.DefineCallGraph.t

  val is_global : t -> reference:Ast.Reference.t -> bool

  val resolve_reference : t -> Ast.Reference.t -> Type.t

  val resolve_assignment : t -> Ast.Statement.Assign.t -> t

  val resolve_expression_to_type : t -> Ast.Expression.t -> Type.t

  val resolve_attribute_access : t -> base_type:Type.t -> attribute:string -> Type.t

  val fallback_attribute
    :  t ->
    ?accessed_through_class:bool ->
    ?type_for_lookup:Type.t option ->
    name:string ->
    string ->
    Analysis.AnnotatedAttribute.instantiated option

  val resolve_generators : t -> Ast.Expression.Comprehension.Generator.t list -> t

  val module_qualifier : t -> Ast.Reference.t

  val define_name : t -> Ast.Reference.t

  val root_of_identifier
    :  t ->
    location:Ast.Location.t ->
    identifier:Ast.Identifier.t ->
    Analysis.TaintAccessPath.Root.t

  val propagate_captured_variable
    :  t ->
    defining_function:Ast.Reference.t ->
    name:Ast.Identifier.t ->
    Analysis.TaintAccessPath.Root.t
end

(* Exposed for testing purposes *)
module ModuleId : sig
  type t [@@deriving compare, equal, show]

  val from_int : int -> t
end

(* Exposed for testing purposes *)
module LocalClassId : sig
  type t [@@deriving compare, equal, show]

  val from_int : int -> t
end

(* Exposed for testing purposes *)
module GlobalCallableId : sig
  type t [@@deriving compare, equal, show]
end

(* Exposed for testing purposes *)
module ModulePath : sig
  type t =
    | Filesystem of ArtifactPath.t
    | Namespace of PyrePath.t
    | Memory of PyrePath.t
    | BundledTypeshed of PyrePath.t
    | BundledTypeshedThirdParty of PyrePath.t
  [@@deriving compare, equal, show]
end

(* Exposed for testing purposes *)
module ModuleInfoFilename : sig
  type t [@@deriving compare, equal, show]

  val create : string -> t
end

(* Exposed for testing purposes *)
module ProjectFile : sig
  module Module : sig
    type t = {
      module_id: ModuleId.t;
      module_name: Ast.Reference.t;
      absolute_source_path: ModulePath.t;
      relative_source_path: string option;
      info_filename: ModuleInfoFilename.t option;
      is_test: bool;
      is_interface: bool;
      is_init: bool;
    }
    [@@deriving equal, show]
  end
end

(* Exposed for testing purposes *)
module GlobalClassId : sig
  type t [@@deriving show]
end

(* Exposed for testing purposes *)
module LocalFunctionId : sig
  type t [@@deriving show]

  val create_function : Ast.Location.t -> t

  module Map : Map.S with type Key.t = t
end

(* Exposed for testing purposes *)
module ClassWithModifiers : sig
  type t = {
    class_name: GlobalClassId.t;
    modifiers: Analysis.PyrePysaEnvironment.TypeModifier.t list;
  }
  [@@deriving equal, show]
end

(* Exposed for testing purposes *)
module ClassNamesResult : sig
  type t = {
    classes: ClassWithModifiers.t list;
    is_exhaustive: bool;
  }
  [@@deriving equal, show]
end

(* Exposed for testing purposes *)
module JsonType : sig
  type t = {
    string: string;
    scalar_properties: Analysis.PyrePysaEnvironment.ScalarTypeProperties.t;
    class_names: ClassNamesResult.t option;
  }
  [@@deriving equal, show]
end

(* Exposed for testing purposes *)
module ClassFieldDeclarationKind : sig
  type t =
    | DeclaredByAnnotation
    | DeclaredWithoutAnnotation
    | AssignedInBody
    | DefinedWithoutAssign
    | DefinedInMethod
  [@@deriving equal, show]
end

(* Exposed for testing purposes *)
module CapturedVariable : sig
  type t = {
    name: string;
    outer_function: GlobalCallableId.t;
  }
  [@@deriving equal, show]
end

(* Exposed for testing purposes *)
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
      return_annotation: JsonType.t;
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
  end

  module ClassMro : sig
    type t =
      | Resolved of GlobalClassId.t list
      | Cyclic
    [@@deriving equal, show]
  end

  module JsonClassField : sig
    type t = {
      name: string;
      type_: JsonType.t;
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
      fields: JsonClassField.t list;
      decorator_callees: GlobalCallableId.t list Ast.Location.SerializableMap.t;
    }
    [@@deriving equal, show]
  end
end

(* Exposed for testing purposes *)
module ModuleQualifier : sig
  type t [@@deriving compare, equal, show]

  val create : path:string option -> Ast.Reference.t -> t

  val from_reference_unchecked : Ast.Reference.t -> t

  val to_reference : t -> Ast.Reference.t

  module Map : Map.S with type Key.t = t
end

(* Exposed for testing purposes *)
module FullyQualifiedName : sig
  type t [@@deriving compare, equal, show]

  val to_reference : t -> Ast.Reference.t
end

(* Exposed for testing purposes *)
module Testing : sig
  module Module : sig
    type t = {
      module_id: ModuleId.t;
      module_name: Ast.Reference.t;
      absolute_source_path: ArtifactPath.t option;
      relative_source_path: string option;
      pyrefly_info_filename: ModuleInfoFilename.t option;
      is_test: bool;
      is_stub: bool;
    }
    [@@deriving compare, equal, show]
  end

  (* Build a mapping from unique module qualifiers (module name + path prefix) to module. *)
  val create_module_qualifiers
    :  pyrefly_directory:PyrePath.t ->
    add_toplevel_modules:bool ->
    ProjectFile.Module.t list ->
    Module.t ModuleQualifier.Map.t

  module Definition : sig
    type t =
      | Function of ModuleDefinitionsFile.FunctionDefinition.t
      | Class of ModuleDefinitionsFile.ClassDefinition.t
    [@@deriving equal, show]
  end

  module QualifiedDefinition : sig
    type t = {
      qualified_name: FullyQualifiedName.t;
      local_name: Ast.Reference.t; (* a non-unique name, more user-friendly. *)
      definition: Definition.t; (* class or def *)
      name_location: NameLocation.t;
    }
  end

  val create_fully_qualified_names
    :  module_qualifier:ModuleQualifier.t ->
    module_exists:(ModuleQualifier.t -> bool) ->
    class_definitions:ModuleDefinitionsFile.ClassDefinition.t Ast.Location.Map.t ->
    function_definitions:ModuleDefinitionsFile.FunctionDefinition.t LocalFunctionId.Map.t ->
    QualifiedDefinition.t list
end
