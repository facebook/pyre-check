(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Core
open Expression


type origin =
  | Class of { annotation: Type.t; class_attribute: bool }
  | Module of Access.t
[@@deriving compare, eq, show, hash]

type mismatch = {
  actual: Type.t;
  expected: Type.t;
  due_to_invariance: bool;
}
[@@deriving compare, eq, show, hash]

type missing_annotation = {
  name: Access.t;
  annotation: Type.t option;
  given_annotation: Type.t option;
  evidence_locations: Location.Instantiated.t list;
  thrown_at_source: bool;
}
[@@deriving compare, eq, sexp, hash]

type incompatible_type = {
  name: Access.t;
  mismatch: mismatch;
  declare_location: Location.Instantiated.t;
}
[@@deriving compare, eq, show, sexp, hash]

type invalid_argument =
  | Keyword of { expression: Expression.t; annotation: Type.t }
  | Variable of { expression: Expression.t; annotation: Type.t }
[@@deriving compare, eq, show, sexp, hash]

type precondition_mismatch =
  | Found of mismatch
  | NotFound of Access.t
[@@deriving compare, eq, show, sexp, hash]

type override =
  | StrengthenedPrecondition of precondition_mismatch
  | WeakenedPostcondition of mismatch
[@@deriving compare, eq, show, sexp, hash]

type unpack_problem =
  | UnacceptableType of Type.t
  | CountMismatch of int
[@@deriving compare, eq, sexp, show, hash]


type type_variable_origin =
  | ClassToplevel
  | Define
  | Toplevel
[@@deriving compare, eq, sexp, show, hash]


type kind =
  | AnalysisFailure of Type.t
  | IllegalAnnotationTarget of Expression.t
  | ImpossibleIsinstance of { expression: Expression.t; mismatch: mismatch }
  | IncompatibleAttributeType of { parent: Type.t; incompatible_type: incompatible_type }
  | IncompatibleAwaitableType of Type.t
  | IncompatibleConstructorAnnotation of Type.t
  | IncompatibleParameterType of {
      name: Identifier.t option;
      position: int;
      callee: Access.t option;
      mismatch: mismatch;
    }
  | IncompatibleReturnType of { mismatch: mismatch; is_implicit: bool }
  | IncompatibleVariableType of incompatible_type
  | InconsistentOverride of {
      overridden_method: Identifier.t;
      parent: Access.t;
      override: override;
    }
  | InvalidArgument of invalid_argument
  | InvalidType of Type.t
  | InvalidTypeVariable of { annotation: Type.t; origin: type_variable_origin }
  | MissingArgument of { callee: Access.t option; name: Access.t }
  | MissingAttributeAnnotation of { parent: Type.t; missing_annotation: missing_annotation }
  | MissingGlobalAnnotation of missing_annotation
  | MissingParameterAnnotation of missing_annotation
  | MissingReturnAnnotation of missing_annotation
  | MissingTypeParameters of { annotation: Type.t; number_of_parameters: int }
  | MutuallyRecursiveTypeVariables of Access.t option
  | NotCallable of Type.t
  | ProhibitedAny of missing_annotation
  | RedundantCast of Type.t
  | RevealedType of { expression: Expression.t; annotation: Type.t }
  | TooManyArguments of { callee: Access.t option; expected: int; provided: int }
  | Top
  | TypedDictionaryAccessWithNonLiteral of string list
  | TypedDictionaryKeyNotFound of { typed_dictionary_name: Identifier.t; missing_key: string }
  | UndefinedAttribute of { attribute: Access.t; origin: origin }
  | UndefinedImport of Access.t
  | UndefinedName of Access.t
  | UndefinedType of Type.t
  | UnexpectedKeyword of { name: Identifier.t; callee: Access.t option }
  | UninitializedAttribute of { name: Access.t; parent: Type.t; mismatch: mismatch }
  | Unpack of { expected_count: int; unpack_problem: unpack_problem }
  | UnusedIgnore of int list

  (* Additional errors. *)
  | Deobfuscation of Source.t
  | UnawaitedAwaitable of Access.t
[@@deriving compare, eq, show, hash]

include BaseError.Error with type kind := kind
module Set: Set.S with type Elt.t = t

val due_to_analysis_limitations: t -> bool
val due_to_mismatch_with_any: t -> bool

val less_or_equal: resolution: Resolution.t -> t -> t -> bool
val join: resolution: Resolution.t -> t -> t -> t
val meet: resolution: Resolution.t -> t -> t -> t
val widen: resolution: Resolution.t -> previous: t -> next: t -> iteration: int -> t

val join_at_define
  :  resolution: Resolution.t
  -> t list
  -> t list

val join_at_source: resolution: Resolution.t -> t list -> t list

val filter: configuration: Configuration.Analysis.t -> resolution: Resolution.t -> t list -> t list
val suppress: mode: Source.mode -> t -> bool

val dequalify: Access.t Access.Map.t -> resolution: Resolution.t -> t -> t

val create_mismatch
  :  resolution: Resolution.t
  -> actual: Type.t
  -> expected: Type.t
  -> covariant: bool
  -> mismatch
