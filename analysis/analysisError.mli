(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Ast
open Core

type missing_annotation = {
  name: Reference.t;
  annotation: Type.t option;
  given_annotation: Type.t option;
  evidence_locations: Location.Instantiated.t list;
  thrown_at_source: bool;
}
[@@deriving compare, eq, sexp, show, hash]

type class_kind =
  | Class
  | Protocol of Reference.t
  | Abstract of Reference.t
[@@deriving compare, eq, sexp, show, hash]

type origin =
  | Class of {
      annotation: Type.t;
      class_attribute: bool;
    }
  | Module of Reference.t

and mismatch = {
  actual: Type.t;
  actual_expressions: Expression.t list;
  expected: Type.t;
  due_to_invariance: bool;
}

and incompatible_type = {
  name: Reference.t;
  mismatch: mismatch;
  declare_location: Location.Instantiated.t;
}

and invalid_argument =
  | Keyword of {
      expression: Expression.t;
      annotation: Type.t;
    }
  | ConcreteVariable of {
      expression: Expression.t;
      annotation: Type.t;
    }
  | ListVariadicVariable of {
      variable: Type.OrderedTypes.t;
      mismatch: AnnotatedSignature.mismatch_with_list_variadic_type_variable;
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
  | UninheritableType of Type.t

and invalid_override_kind =
  | Final
  | StaticSuper
  | StaticOverride

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
  | InvalidType of Type.t
  | NestedTypeVariables of Type.Variable.t

and unawaited_awaitable = {
  references: Reference.t list;
  expression: Expression.t;
}

and incompatible_overload_kind =
  | ReturnType of {
      implementation_annotation: Type.t;
      name: Reference.t;
      overload_annotation: Type.t;
    }
  | Unmatchable of {
      name: Reference.t;
      matched_location: Location.t;
      unmatched_location: Location.t;
    }
  | Parameters of {
      name: Reference.t;
      location: Location.t;
    }
[@@deriving compare, eq, sexp, show, hash]

type kind =
  | AbstractClass of {
      class_name: Reference.t;
      method_names: Identifier.t list;
    }
  | AnalysisFailure of Type.t
  | IllegalAnnotationTarget of Expression.t
  | ImpossibleIsinstance of {
      expression: Expression.t;
      mismatch: mismatch;
    }
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
  | IncompatibleVariableType of incompatible_type
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
  | InvalidClass of Reference.t
  | InvalidClassInstantiation of class_kind
  | InvalidException of {
      expression: Expression.t;
      annotation: Type.t;
    }
  | InvalidMethodSignature of {
      annotation: Type.t option;
      name: Identifier.t;
    }
  | InvalidType of invalid_type_kind
  | InvalidTypeParameters of GlobalResolution.type_parameters_mismatch
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
      parameter: AnnotatedSignature.missing_argument;
    }
  | MissingAttributeAnnotation of {
      parent: Type.t;
      missing_annotation: missing_annotation;
    }
  | MissingGlobalAnnotation of missing_annotation
  | MissingOverloadImplementation of Reference.t
  | MissingParameterAnnotation of missing_annotation
  | MissingReturnAnnotation of missing_annotation
  | MutuallyRecursiveTypeVariables of Reference.t option
  | NotCallable of Type.t
  | ProhibitedAny of missing_annotation
  | RedundantCast of Type.t
  | RevealedType of {
      expression: Expression.t;
      annotation: Annotation.t;
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
  | UndefinedAttribute of {
      attribute: Identifier.t;
      origin: origin;
    }
  | UndefinedImport of Reference.t
  | UndefinedName of Reference.t
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
  | UnusedIgnore of int list
  (* Additional errors. *)
  | Deobfuscation of Source.t
  | UnawaitedAwaitable of unawaited_awaitable
[@@deriving compare, eq, sexp, show, hash]

include BaseError.Error with type kind := kind

module Set : Set.S with type Elt.t = t

val weaken_literals : kind -> kind

val due_to_analysis_limitations : t -> bool

val due_to_mismatch_with_any : Resolution.t -> t -> bool

val less_or_equal : resolution:GlobalResolution.t -> t -> t -> bool

val join : resolution:GlobalResolution.t -> t -> t -> t

val meet : resolution:GlobalResolution.t -> t -> t -> t

val widen : resolution:GlobalResolution.t -> previous:t -> next:t -> iteration:int -> t

val join_at_define : resolution:GlobalResolution.t -> t list -> t list

val join_at_source : resolution:GlobalResolution.t -> t list -> t list

val deduplicate : t list -> t list

val filter
  :  configuration:Configuration.Analysis.t ->
  resolution:GlobalResolution.t ->
  t list ->
  t list

val suppress : mode:Source.mode -> resolution:Resolution.t -> t -> bool

val dequalify : Reference.t Reference.Map.t -> resolution:GlobalResolution.t -> t -> t

val create_mismatch
  :  resolution:GlobalResolution.t ->
  actual:Type.t ->
  actual_expression:Expression.t option ->
  expected:Type.t ->
  covariant:bool ->
  mismatch

val language_server_hint : t -> bool
