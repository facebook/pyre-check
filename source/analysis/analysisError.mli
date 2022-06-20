(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Core

type missing_annotation = {
  name: Reference.t;
  annotation: Type.t option;
  given_annotation: Type.t option;
  evidence_locations: Location.WithPath.t list;
  thrown_at_source: bool;
}
[@@deriving compare, sexp, show, hash]

type revealed_local = {
  name: Reference.t;
  annotation: Annotation.t;
}
[@@deriving compare, sexp, show, hash]

type class_kind =
  | Class
  | Enumeration
  | Protocol of Reference.t
  | Abstract of Reference.t
[@@deriving compare, sexp, show, hash]

type invalid_class_instantiation =
  | AbstractClassInstantiation of {
      class_name: Reference.t;
      abstract_methods: string list;
    }
  | ProtocolInstantiation of Reference.t
[@@deriving compare, sexp, show, hash]

type module_reference =
  | ExplicitModule of ModulePath.t
  | ImplicitModule of Reference.t
[@@deriving compare, sexp, show, hash]

type class_origin =
  | ClassType of Type.t
  | ClassInUnion of {
      unions: Type.t list;
      index: int;
    }
[@@deriving compare, sexp, show, hash]

type origin =
  | Class of {
      class_origin: class_origin;
      parent_module_path: ModulePath.t option;
    }
  | Module of module_reference

and analysis_failure =
  | UnexpectedUndefinedType of string
  | FixpointThresholdReached of { define: Reference.t }

and mismatch = {
  actual: Type.t;
  expected: Type.t;
  due_to_invariance: bool;
}

and annotation_and_parent = {
  parent: Identifier.t;
  annotation: Type.t;
}

and typed_dictionary_field_mismatch =
  | RequirednessMismatch of {
      required_field_class: Identifier.t;
      non_required_field_class: Identifier.t;
      field_name: Identifier.t;
    }
  | TypeMismatch of {
      field_name: Identifier.t;
      annotation_and_parent1: annotation_and_parent;
      annotation_and_parent2: annotation_and_parent;
    }

and typed_dictionary_initialization_mismatch =
  | MissingRequiredField of {
      field_name: Identifier.t;
      class_name: Identifier.t;
    }
  | FieldTypeMismatch of {
      field_name: Identifier.t;
      class_name: Identifier.t;
      expected_type: Type.t;
      actual_type: Type.t;
    }
  | UndefinedField of {
      field_name: Identifier.t;
      class_name: Identifier.t;
    }

and incompatible_type = {
  name: Reference.t;
  mismatch: mismatch;
}

and invalid_argument =
  | Keyword of {
      expression: Expression.t option;
      annotation: Type.t;
      require_string_keys: bool;
    }
  | ConcreteVariable of {
      expression: Expression.t option;
      annotation: Type.t;
    }
  | VariableArgumentsWithUnpackableType of {
      variable: Type.OrderedTypes.t;
      mismatch: SignatureSelectionTypes.mismatch_with_unpackable_type;
    }

and precondition_mismatch =
  | Found of mismatch
  | NotFound of Type.t Type.Callable.Parameter.t

and override =
  | StrengthenedPrecondition of precondition_mismatch
  | WeakenedPostcondition of mismatch

and unpack_problem =
  | UnacceptableType of Type.t
  | CountMismatch of int

and type_variable_origin =
  | ClassToplevel
  | Define
  | Toplevel

and type_variance_origin =
  | Parameter
  | Return
  | Inheritance of Type.t

and illegal_action_on_incomplete_type =
  | Naming
  | Calling
  | AttributeAccess of Identifier.t

and override_kind =
  | Method
  | Attribute

and invalid_inheritance =
  | ClassName of Identifier.t
  | NonMethodFunction of Identifier.t
  | UninheritableType of {
      annotation: Type.t;
      is_parent_class_typed_dictionary: bool;
    }
  | TypedDictionarySuperclassCollision of typed_dictionary_field_mismatch

and invalid_override_kind =
  | Final
  | StaticSuper
  | StaticOverride
  | NothingOverridden
  | IllegalOverrideDecorator

and invalid_assignment_kind =
  | FinalAttribute of Reference.t
  | ClassVariable of {
      class_variable: Identifier.t;
      class_name: Identifier.t;
    }
  | ReadOnly of Reference.t

and invalid_type_kind =
  | FinalNested of Type.t
  | FinalParameter of Identifier.t
  | InvalidType of {
      annotation: Type.t;
      expected: string;
    }
  | NestedAlias of Identifier.t
  | NestedTypeVariables of Type.Variable.t
  | SingleExplicit of Type.t
  | InvalidLiteral of Reference.t

and unawaited_awaitable = {
  references: Reference.t list;
  expression: Expression.t;
}

and undefined_import =
  | UndefinedModule of Reference.t
  | UndefinedName of {
      from: module_reference;
      name: Identifier.t;
    }

and incompatible_overload_kind =
  | ReturnType of {
      implementation_annotation: Type.t;
      name: Reference.t;
      overload_annotation: Type.t;
    }
  | Unmatchable of {
      name: Reference.t;
      matching_overload: Type.t Type.Callable.overload;
      unmatched_location: Location.t;
    }
  | Parameters of {
      name: Reference.t;
      location: Location.t;
    }
  | DifferingDecorators
  | MisplacedOverloadDecorator

and polymorphism_base_class =
  | GenericBase
  | ProtocolBase

and unsupported_operand_kind =
  | Binary of {
      operator_name: Identifier.t;
      left_operand: Type.t;
      right_operand: Type.t;
    }
  | Unary of {
      operator_name: Identifier.t;
      operand: Type.t;
    }

and illegal_annotation_target_kind =
  | InvalidExpression
  | Reassignment

and tuple_concatenation_problem =
  | MultipleVariadics of { variadic_expressions: Expression.t list }
  | UnpackingNonIterable of { annotation: Type.t }
[@@deriving compare, sexp, show, hash]

type invalid_decoration =
  | CouldNotResolve of Expression.t
  | CouldNotResolveArgument of {
      name: Reference.t;
      argument: Expression.t;
    }
  | NonCallableDecoratorFactory of {
      name: Reference.t;
      annotation: Type.t;
    }
  | NonCallableDecorator of {
      name: Reference.t;
      has_arguments: bool;
      annotation: Type.t;
    }
  | DecoratorFactoryFailedToApply of {
      name: Reference.t;
      reason: kind option;
    }
  | ApplicationFailed of {
      name: Reference.t;
      has_arguments: bool;
      reason: kind option;
    }
  | SetterNameMismatch of {
      name: Reference.t;
      actual: string;
      expected: string;
    }

and kind =
  | AnalysisFailure of analysis_failure
  | ParserFailure of string
  | IllegalAnnotationTarget of {
      target: Expression.t;
      kind: illegal_annotation_target_kind;
    }
  | IncompatibleAsyncGeneratorReturnType of Type.t
  | IncompatibleAttributeType of {
      parent: Type.t;
      incompatible_type: incompatible_type;
    }
  | IncompatibleAwaitableType of Type.t
  | IncompatibleConstructorAnnotation of Type.t
  | IncompatibleParameterType of {
      name: Identifier.t option;
      position: int;
      callee: Reference.t option;
      mismatch: mismatch;
    }
  | IncompatibleReturnType of {
      mismatch: mismatch;
      is_implicit: bool;
      is_unimplemented: bool;
      define_location: Location.t;
    }
  | IncompatibleVariableType of {
      incompatible_type: incompatible_type;
      declare_location: Location.WithPath.t;
    }
  | IncompatibleOverload of incompatible_overload_kind
  | IncompleteType of {
      target: Expression.t;
      annotation: Type.t;
      attempted_action: illegal_action_on_incomplete_type;
    }
  | InconsistentOverride of {
      overridden_method: Identifier.t;
      parent: Reference.t;
      override: override;
      override_kind: override_kind;
    }
  | InvalidArgument of invalid_argument
  | InvalidClassInstantiation of invalid_class_instantiation
  | InvalidDecoration of invalid_decoration
  | InvalidException of {
      expression: Expression.t;
      annotation: Type.t;
    }
  | InvalidMethodSignature of {
      annotation: Type.t option;
      name: Identifier.t;
    }
  | InvalidType of invalid_type_kind
  | InvalidTypeParameters of AttributeResolution.type_parameters_mismatch
  | InvalidTypeVariable of {
      annotation: Type.Variable.t;
      origin: type_variable_origin;
    }
  | InvalidTypeVariance of {
      annotation: Type.t;
      origin: type_variance_origin;
    }
  | InvalidInheritance of invalid_inheritance
  | InvalidOverride of {
      parent: Identifier.t;
      decorator: invalid_override_kind;
    }
  | InvalidAssignment of invalid_assignment_kind
  | MissingArgument of {
      callee: Reference.t option;
      parameter: SignatureSelectionTypes.missing_argument;
    }
  | MissingAttributeAnnotation of {
      parent: Type.t;
      missing_annotation: missing_annotation;
    }
  | MissingCaptureAnnotation of Identifier.t
  | MissingGlobalAnnotation of missing_annotation
  | MissingOverloadImplementation of Reference.t
  | MissingParameterAnnotation of missing_annotation
  | MissingReturnAnnotation of missing_annotation
  | MutuallyRecursiveTypeVariables of Reference.t option
  | NotCallable of Type.t
  | PrivateProtocolProperty of {
      name: Identifier.t;
      parent: Type.t;
    }
  | ProhibitedAny of {
      is_type_alias: bool;
      missing_annotation: missing_annotation;
    }
  | RedefinedClass of {
      current_class: Reference.t;
      shadowed_class: Reference.t;
      is_shadowed_class_imported: bool;
    }
  | RedundantCast of Type.t
  | RevealedLocals of revealed_local list
  | RevealedType of {
      expression: Expression.t;
      annotation: Annotation.t;
      qualify: bool;
    }
  | UnsafeCast of {
      expression: Expression.t;
      annotation: Type.t;
    }
  | TooManyArguments of {
      callee: Reference.t option;
      expected: int;
      provided: int;
    }
  | Top
  | TypedDictionaryAccessWithNonLiteral of Identifier.t list
  | TypedDictionaryKeyNotFound of {
      typed_dictionary_name: Identifier.t;
      missing_key: string;
    }
  | UnboundName of Identifier.t
  | UninitializedLocal of Identifier.t
  | UndefinedAttribute of {
      attribute: Identifier.t;
      origin: origin;
    }
  | UndefinedImport of undefined_import
  | UndefinedType of Type.t
  | UnexpectedKeyword of {
      name: Identifier.t;
      callee: Reference.t option;
    }
  | UninitializedAttribute of {
      name: Identifier.t;
      parent: Type.t;
      mismatch: mismatch;
      kind: class_kind;
    }
  | Unpack of {
      expected_count: int;
      unpack_problem: unpack_problem;
    }
  | UnsupportedOperand of unsupported_operand_kind
  | UnusedIgnore of int list
  | UnusedLocalMode of {
      unused_mode: Source.local_mode Node.t;
      actual_mode: Source.local_mode Node.t;
    }
  | TypedDictionaryInvalidOperation of {
      typed_dictionary_name: Identifier.t;
      field_name: Identifier.t;
      method_name: Identifier.t;
      mismatch: mismatch;
    }
  | TypedDictionaryInitializationError of typed_dictionary_initialization_mismatch
  | DuplicateTypeVariables of {
      variable: Type.Variable.t;
      base: polymorphism_base_class;
    }
  | TupleConcatenationError of tuple_concatenation_problem
  (* Additional errors. *)
  | DeadStore of Identifier.t
  | Deobfuscation of Source.t
  | UnawaitedAwaitable of unawaited_awaitable
  (* Errors from run-time edge cases *)
  | BroadcastError of {
      expression: Expression.t;
      left: Type.t;
      right: Type.t;
    }
[@@deriving compare, sexp, show, hash]

type t = {
  location: Location.WithModule.t;
  kind: kind;
  signature: Statement.Define.Signature.t Node.t;
}
[@@deriving compare, show, sexp, hash]

module Instantiated : sig
  type t [@@deriving sexp, compare, show, hash, yojson { strict = false }]

  val location : t -> Location.WithPath.t

  val path : t -> string

  val code : t -> int

  val description : t -> string

  val long_description : t -> string

  val concise_description : t -> string
end

include Hashable with type t := t

val create : location:Location.WithModule.t -> kind:kind -> define:Statement.Define.t Node.t -> t

val module_reference : t -> Reference.t

val code : t -> int

val instantiate
  :  show_error_traces:bool ->
  lookup:(Reference.t -> string option) ->
  t ->
  Instantiated.t

module Set : Set.S with type Elt.t = t

val weaken_literals : kind -> kind

val due_to_analysis_limitations : t -> bool

val less_or_equal : resolution:GlobalResolution.t -> t -> t -> bool

val join : resolution:GlobalResolution.t -> t -> t -> t

val meet : resolution:GlobalResolution.t -> t -> t -> t

val widen : resolution:GlobalResolution.t -> previous:t -> next:t -> iteration:int -> t

val join_at_define : resolution:GlobalResolution.t -> t list -> t list

val join_at_source : resolution:GlobalResolution.t -> t list -> t list

val deduplicate : t list -> t list

val filter : resolution:GlobalResolution.t -> t list -> t list

val suppress : mode:Source.mode -> ignore_codes:int list -> t -> bool

val dequalify : Reference.t Reference.Map.t -> resolution:GlobalResolution.t -> t -> t

val create_mismatch
  :  resolution:GlobalResolution.t ->
  actual:Type.t ->
  expected:Type.t ->
  covariant:bool ->
  mismatch

module SimplificationMap : sig
  type t = Reference.t Reference.Map.t

  val pp : Format.formatter -> t -> unit

  val show : t -> string

  val create : Reference.t list -> t
end

val simplify_mismatch : mismatch -> mismatch
