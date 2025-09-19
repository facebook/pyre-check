(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

(* Scalar properties of a type (it is a bool/int/float/etc.) *)
module ScalarTypeProperties : sig
  type t [@@deriving compare, equal, sexp, hash, show]

  val none : t

  val unknown : t

  val bool : t

  val integer : t

  val enumeration : t

  val is_boolean : t -> bool

  val is_integer : t -> bool

  val is_float : t -> bool

  val is_enumeration : t -> bool

  val create : is_boolean:bool -> is_integer:bool -> is_float:bool -> is_enumeration:bool -> t
end

(* Result of extracting class names from a type. *)
module ClassNamesFromType : sig
  type t = {
    class_names: string list;
    stripped_coroutine: bool;
    stripped_optional: bool;
    stripped_readonly: bool;
    unbound_type_variable: bool;
    is_exhaustive: bool;
        (* Is there an element (after stripping) that isn't a class name? For instance:
           get_class_name(Union[A, Callable[...])) = { class_names = [A], is_exhaustive = false } *)
  }

  val not_a_class : t
end

module PyreflyType : sig
  module ClassNamesFromType : sig
    type t = {
      class_names: (int * int) list;
      stripped_coroutine: bool;
      stripped_optional: bool;
      stripped_readonly: bool;
      unbound_type_variable: bool;
      is_exhaustive: bool;
    }
    [@@deriving equal, compare, show]

    val from_class : int * int -> t
  end

  type t = {
    string: string;
    scalar_properties: ScalarTypeProperties.t;
    class_names: ClassNamesFromType.t option;
  }
  [@@deriving equal, compare, show]
end

(* Minimal abstraction for a type, provided from Pyre1 or Pyrefly and used by Pysa. *)
module PysaType : sig
  type t [@@deriving equal, compare, show]

  val from_pyre1_type : Type.t -> t

  val from_pyrefly_type : PyreflyType.t -> t

  val as_pyrefly_type : t -> PyreflyType.t option

  val as_pyre1_type : t -> Type.t option

  (* Pretty print the type, usually meant for the user *)
  val pp_concise : Format.formatter -> t -> unit

  val show_fully_qualified : t -> string
end

module ReadWrite : sig
  type t

  val load_from_cache : configuration:Configuration.Analysis.t -> t

  val create_with_cold_start
    :  scheduler:Scheduler.t ->
    scheduler_policies:Configuration.SchedulerPolicies.t ->
    configuration:Configuration.Analysis.t ->
    decorator_configuration:DecoratorPreprocessing.Configuration.t ->
    skip_type_checking_callables:Ast.Reference.SerializableSet.t ->
    callback_with_qualifiers_and_definitions:
      ((lookup_source:(ArtifactPath.t -> SourcePath.t option) -> Ast.Reference.t -> string option) ->
      Ast.Reference.t list ->
      Ast.Reference.t list ->
      unit) ->
    t

  val configuration : t -> Configuration.Analysis.t

  val module_paths : t -> Ast.ModulePath.t list

  val module_paths_from_disk : t -> Ast.ModulePath.t list

  val all_module_paths : t -> Ast.ModulePath.t list

  val artifact_path_of_module_path : t -> Ast.ModulePath.t -> ArtifactPath.t

  val save : t -> unit

  val purge_sources_from_shared_memory : t -> unit
end

module MethodInQualifier : sig
  type t = {
    class_name: Ast.Reference.t;
    method_name: string;
    is_property_setter: bool;
  }
end

module ReadOnly : sig
  type t

  val of_read_write_api : ReadWrite.t -> t

  val create
    :  type_environment:TypeEnvironment.TypeEnvironmentReadOnly.t ->
    global_module_paths_api:GlobalModulePathsApi.t ->
    t

  val absolute_source_path_of_qualifier
    :  lookup_source:(ArtifactPath.t -> SourcePath.t option) ->
    t ->
    Ast.Reference.t ->
    string option

  val explicit_qualifiers : t -> Ast.Reference.t list

  val parse_annotation
    :  t ->
    ?validation:AttributeResolution.type_validation_policy ->
    Ast.Expression.t ->
    Type.t

  val get_class_summary : t -> string -> ClassSummary.t Ast.Node.t option

  val get_class_attributes
    :  t ->
    include_generated_attributes:bool ->
    only_simple_assignments:bool ->
    string ->
    string list option

  val get_class_attribute_annotation
    :  t ->
    include_generated_attributes:bool ->
    class_name:string ->
    attribute:string ->
    Ast.Expression.t option

  val source_is_unit_test : t -> source:Ast.Source.t -> bool

  val class_immediate_parents : t -> string -> string list

  val class_mro : t -> string -> string list

  val get_define_names_for_qualifier : t -> Ast.Reference.t -> Ast.Reference.t list

  val parse_reference : t -> Ast.Reference.t -> Type.t

  val module_exists : t -> Ast.Reference.t -> bool

  val class_exists : t -> string -> bool

  val get_define_body : t -> Ast.Reference.t -> Ast.Statement.Define.t Ast.Node.t option

  val get_variable : t -> string -> Type.Variable.t option

  val resolve_define
    :  t ->
    callable_name:Ast.Reference.t option ->
    implementation:Ast.Statement.Define.Signature.t option ->
    overloads:Ast.Statement.Define.Signature.t list ->
    scoped_type_variables:Type.Variable.t Ast.Identifier.Map.t option ->
    AttributeResolution.resolved_define

  val resolve_define_undecorated
    :  t ->
    callable_name:Ast.Reference.t option ->
    implementation:Ast.Statement.Define.Signature.t option ->
    overloads:Ast.Statement.Define.Signature.t list ->
    scoped_type_variables:Type.Variable.t Ast.Identifier.Map.t option ->
    AnnotatedAttribute.decorated_method

  val global : t -> Ast.Reference.t -> AttributeResolution.Global.t option

  val get_overriden_base_method
    :  t ->
    class_name:Ast.Reference.t ->
    method_name:string ->
    Ast.Reference.t option

  val annotation_parser : t -> AnnotatedCallable.annotation_parser

  val typed_dictionary_field_names : t -> Type.t -> string list

  val less_or_equal : t -> left:Type.t -> right:Type.t -> bool

  val resolve_exports : t -> ?from:Ast.Reference.t -> Ast.Reference.t -> ResolvedReference.t option

  val location_of_global : t -> Ast.Reference.t -> Ast.Location.WithModule.t option

  val get_function_definition : t -> Ast.Reference.t -> FunctionDefinition.t option

  val attribute_from_class_name
    :  t ->
    ?transitive:bool ->
    ?accessed_through_class:bool ->
    ?accessed_through_readonly:bool ->
    ?special_method:bool ->
    string ->
    name:string ->
    type_for_lookup:Type.t ->
    AnnotatedAttribute.instantiated option

  val has_transitive_successor : t -> successor:string -> string -> bool

  val exists_matching_class_decorator
    :  t ->
    ?dependency:SharedMemoryKeys.DependencyKey.registered ->
    names:string list ->
    ClassSummary.t Ast.Node.t ->
    bool

  val generic_parameters_as_variables : t -> string -> Type.Variable.t list option

  val source_of_qualifier : t -> Ast.Reference.t -> Ast.Source.t option

  val get_class_names_for_qualifier
    :  t ->
    exclude_test_modules:bool ->
    Ast.Reference.t ->
    Ast.Reference.t list

  val relative_path_of_qualifier : t -> Ast.Reference.t -> string option

  val decorated_define : t -> Ast.Statement.Define.t Ast.Node.t -> Ast.Statement.Define.t Ast.Node.t

  val named_tuple_attributes : t -> string -> string list option

  val resolve_expression_to_type_info : t -> Ast.Expression.t -> TypeInfo.Unit.t

  val get_unannotated_global
    :  t ->
    ?dependency:SharedMemoryKeys.DependencyKey.registered ->
    Ast.Reference.t ->
    Module.UnannotatedGlobal.t option

  val all_classes : t -> scheduler:Scheduler.t -> string list

  val all_unannotated_globals : t -> scheduler:Scheduler.t -> Ast.Reference.t list

  module Type : sig
    (* Returns whether the type is an int, float, bool or enum, after stripping Optional and
       Awaitable. *)
    val scalar_properties : t -> PysaType.t -> ScalarTypeProperties.t

    (* Return a list of fully qualified class names that this type refers to, after
     * stripping Optional, ReadOnly and TypeVar.
     *
     * For instance:
     * Union[int, str] -> [int, str]
     * Optional[int] -> [int]
     * List[int] -> [List]
     * List[Dict[str, str]] -> [List]
     *)
    val get_class_names : t -> PysaType.t -> ClassNamesFromType.t
  end

  val get_methods_for_qualifier
    :  t ->
    exclude_test_modules:bool ->
    Ast.Reference.t ->
    MethodInQualifier.t list
end

module InContext : sig
  type t

  val create_at_global_scope : ReadOnly.t -> t

  val create_at_statement_key
    :  ReadOnly.t ->
    define_name:Ast.Reference.t ->
    define:Ast.Statement.Define.t Ast.Node.t ->
    statement_key:int ->
    t

  val pyre_api : t -> ReadOnly.t

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
    AnnotatedAttribute.instantiated option

  val resolve_generators : t -> Ast.Expression.Comprehension.Generator.t list -> t
end

module ModelQueries : sig
  val property_decorators : String.Set.t

  module FunctionParameter : sig
    type t =
      | PositionalOnly of {
          name: string option;
          position: int;
          annotation: PysaType.t;
          has_default: bool;
        }
      | Named of {
          name: string;
          position: int;
          annotation: PysaType.t;
          has_default: bool;
        }
      | KeywordOnly of {
          name: string;
          annotation: PysaType.t;
          has_default: bool;
        }
      | Variable of {
          name: string option;
          position: int;
        }
      | Keywords of {
          name: string option;
          annotation: PysaType.t;
          excluded: string list;
        }
    [@@deriving equal, compare, show]

    val annotation : t -> PysaType.t option

    val root : t -> TaintAccessPath.Root.t

    val name : t -> string option

    val has_default : t -> bool
  end

  module FunctionParameters : sig
    type t =
      | List of FunctionParameter.t list
      | Ellipsis
      | ParamSpec
    [@@deriving equal, compare, show]
  end

  module FunctionSignature : sig
    type t = {
      parameters: FunctionParameters.t;
      return_annotation: PysaType.t;
    }
    [@@deriving equal, compare, show]

    val from_callable_type : Type.Callable.t -> t list

    val from_pyre1_ast
      :  pyre_api:ReadOnly.t ->
      parameters:Ast.Expression.Parameter.t list ->
      return_annotation:Ast.Expression.t option ->
      t
  end

  module Function : sig
    type t = {
      define_name: Ast.Reference.t;
      (* If the user-provided name is a re-export, this is the original name. *)
      imported_name: Ast.Reference.t option;
      (* Signature of the function, ignoring all decorators. None when unknown. *)
      (* Note that functions with `@overload` have multiple signatures. *)
      undecorated_signatures: FunctionSignature.t list option;
      is_property_getter: bool;
      is_property_setter: bool;
      is_method: bool;
    }
    [@@deriving show]
  end

  module Global : sig
    type t =
      | Class of { class_name: string }
      | Module
      (* function or method *)
      | Function of Function.t
      (* non-callable class attribute. *)
      | ClassAttribute of { name: Ast.Reference.t }
      (* non-callable module global variable. *)
      | ModuleGlobal of { name: Ast.Reference.t }
      (* class attribute exists, but type is unknown. *)
      | UnknownClassAttribute of { name: Ast.Reference.t }
      (* module global exists, but type is unknown. *)
      | UnknownModuleGlobal of { name: Ast.Reference.t }
    [@@deriving show]
  end

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

  val invalidate_cache : unit -> unit
end
