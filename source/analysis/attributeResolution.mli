(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open SharedMemoryKeys
open Statement
open Core
open SignatureSelection

module Global : sig
  type t = {
    type_info: TypeInfo.Unit.t;
    undecorated_signature: Type.Callable.t option;
    problem: AnnotatedAttribute.problem option;
  }
  [@@deriving show, compare, sexp]
end

type resolved_define = (Type.t, AnnotatedAttribute.problem) Result.t

type generic_type_problems =
  | IncorrectNumberOfParameters of {
      actual: int;
      expected: int;
      can_accept_more_parameters: bool;
    }
  | ViolateConstraints of {
      actual: Type.t;
      expected: Type.Variable.TypeVar.t;
    }
  | UnexpectedKind of {
      actual: Type.Argument.t;
      expected: Type.Variable.t;
    }
[@@deriving compare, sexp, show, hash]

type type_parameters_mismatch = {
  name: string;
  kind: generic_type_problems;
}
[@@deriving compare, sexp, show, hash]

type type_validation_policy =
  | NoValidation
  | ValidatePrimitives
  | ValidatePrimitivesAndTypeParameters
[@@deriving compare, sexp, show, hash]

module AttributeReadOnly : sig
  include Environment.ReadOnly

  val class_metadata_environment : t -> ClassSuccessorMetadataEnvironment.ReadOnly.t

  val validate_and_sanitize_type_arguments
    :  t ->
    ?dependency:DependencyKey.registered ->
    Type.t ->
    type_parameters_mismatch list * Type.t

  val parse_annotation
    :  t ->
    ?dependency:DependencyKey.registered ->
    scoped_type_variables:Type.Variable.t Identifier.Map.t option ->
    ?validation:type_validation_policy ->
    Expression.expression Node.t ->
    Type.t

  val resolve_define_undecorated
    :  t ->
    ?dependency:DependencyKey.registered ->
    callable_name:Reference.t option ->
    implementation:Define.Signature.t option ->
    overloads:Define.Signature.t list ->
    scoped_type_variables:Type.Variable.t Identifier.Map.t option ->
    AnnotatedAttribute.decorated_method

  val metaclass : t -> ?dependency:DependencyKey.registered -> Type.Primitive.t -> Type.t option

  val uninstantiated_attributes
    :  t ->
    ?dependency:DependencyKey.registered ->
    transitive:bool ->
    accessed_through_class:bool ->
    include_generated_attributes:bool ->
    ?special_method:bool ->
    string ->
    AnnotatedAttribute.uninstantiated list option

  val instantiate_attribute
    :  t ->
    ?dependency:DependencyKey.registered ->
    accessed_through_class:bool ->
    accessed_through_readonly:bool ->
    ?type_for_lookup:Type.t ->
    AnnotatedAttribute.uninstantiated ->
    AnnotatedAttribute.instantiated

  val attribute
    :  t ->
    ?dependency:DependencyKey.registered ->
    transitive:bool ->
    accessed_through_class:bool ->
    accessed_through_readonly:bool ->
    include_generated_attributes:bool ->
    ?special_method:bool ->
    ?type_for_lookup:Type.t ->
    attribute_name:Identifier.t ->
    string ->
    AnnotatedAttribute.instantiated option

  val get_typed_dictionary
    :  t ->
    ?dependency:DependencyKey.registered ->
    Type.t ->
    Type.TypedDictionary.t option

  val full_order : ?dependency:DependencyKey.registered -> t -> TypeOrder.order

  val resolve_define
    :  t ->
    ?dependency:DependencyKey.registered ->
    callable_name:Reference.t option ->
    implementation:Define.Signature.t option ->
    overloads:Define.Signature.t list ->
    scoped_type_variables:Type.Variable.t Identifier.Map.t option ->
    resolved_define

  val signature_select
    :  t ->
    ?dependency:DependencyKey.registered ->
    resolve_with_locals:
      (locals:(Reference.t * TypeInfo.Unit.t) list -> Expression.expression Node.t -> Type.t) ->
    arguments:Type.t Argument.t list ->
    location:Location.t ->
    callable:Type.Callable.t ->
    self_argument:Type.t option ->
    SignatureSelectionTypes.instantiated_return_annotation

  val resolve_mutable_literals
    :  t ->
    ?dependency:DependencyKey.registered ->
    resolve:(Expression.expression Node.t -> Type.t) ->
    expression:Expression.expression Node.t option ->
    resolved:Type.t ->
    expected:Type.t ->
    WeakenMutableLiterals.weakened_type

  val constraints_solution_exists
    :  t ->
    ?dependency:DependencyKey.registered ->
    get_typed_dictionary_override:(Type.t -> Type.TypedDictionary.t option) ->
    left:Type.t ->
    right:Type.t ->
    bool

  val variance_map
    :  t ->
    class_name:string ->
    parameters:Type.GenericParameter.t list ->
    Type.Record.Variance.t Identifier.Map.t

  val global : t -> ?dependency:DependencyKey.registered -> Reference.t -> Global.t option

  module Testing : sig
    val constraints_for_instantiate
      :  t ->
      ?dependency:DependencyKey.registered ->
      source_type_name:Type.Primitive.t ->
      current_type:Type.t ->
      unit ->
      TypeConstraints.Solution.t
  end
end

include
  Environment.S
    with module ReadOnly = AttributeReadOnly
     and module PreviousEnvironment = ClassSuccessorMetadataEnvironment
