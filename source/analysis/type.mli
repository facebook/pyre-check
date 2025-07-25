(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast

module Record : sig
  module PreInferenceVariance : sig
    type t =
      | P_Covariant
      | P_Contravariant
      | P_Invariant
      | P_Undefined
    [@@deriving compare, equal, sexp, show, hash]

    val show_lowercase : t -> string
  end

  module Variance : sig
    type t =
      | Covariant
      | Contravariant
      | Invariant
      | Bivariant
    [@@deriving compare, equal, sexp, show, hash]

    val show_lowercase : t -> string
  end

  module TypeVarConstraints : sig
    type 'annotation t =
      | Bound of 'annotation
      | Explicit of 'annotation list
      | Unconstrained
      | LiteralIntegers
    [@@deriving compare, equal, sexp, show, hash]
  end

  module Variable : sig
    type state [@@deriving compare, equal, sexp, show, hash]

    module Namespace : sig
      type t [@@deriving compare, equal, sexp, show, hash]
    end

    module TypeVar : sig
      type 'annotation record = {
        name: Identifier.t;
        constraints: 'annotation TypeVarConstraints.t;
        state: state;
        namespace: Namespace.t;
      }
      [@@deriving compare, equal, sexp, show, hash]
    end

    module ParamSpec : sig
      type 'annotation record [@@deriving compare, equal, sexp, show, hash]

      module Components : sig
        type t [@@deriving compare, equal, sexp, show, hash]
      end
    end

    module TypeVarTuple : sig
      type 'annotation record [@@deriving compare, equal, sexp, show, hash]
    end

    type 'a record =
      | TypeVarVariable of 'a TypeVar.record
      | ParamSpecVariable of 'a ParamSpec.record
      | TypeVarTupleVariable of 'a TypeVarTuple.record
    [@@deriving compare, equal, sexp, show, hash]
  end

  module OrderedTypes : sig
    module Concatenation : sig
      type 'annotation record_unpackable [@@deriving compare, equal, sexp, show, hash]

      type 'annotation t [@@deriving compare, equal, sexp, show, hash]

      val create_unpackable
        :  'annotation Variable.TypeVarTuple.record ->
        'annotation record_unpackable

      val create_unbounded_unpackable : 'annotation -> 'annotation record_unpackable

      val create_from_unpackable
        :  ?prefix:'annotation list ->
        ?suffix:'annotation list ->
        'annotation record_unpackable ->
        'annotation t

      val create
        :  ?prefix:'annotation list ->
        ?suffix:'annotation list ->
        'annotation Variable.TypeVarTuple.record ->
        'annotation t

      val create_from_unbounded_element
        :  ?prefix:'annotation list ->
        ?suffix:'annotation list ->
        'annotation ->
        'annotation t
    end

    type 'annotation record =
      | Concrete of 'annotation list
      | Concatenation of 'annotation Concatenation.t
    [@@deriving compare, equal, sexp, show, hash]

    val create_unbounded_concatenation : 'annotation -> 'annotation record
  end

  module Callable : sig
    module CallableParamType : sig
      type 'annotation named = {
        name: Identifier.t;
        annotation: 'annotation;
        default: bool;
      }

      and 'annotation variable =
        | Concrete of 'annotation
        | Concatenation of 'annotation OrderedTypes.Concatenation.t
      [@@deriving compare, equal, sexp, show, hash]

      and 'annotation t =
        | PositionalOnly of {
            index: int;
            annotation: 'annotation;
            default: bool;
          }
        | Named of 'annotation named
        | KeywordOnly of 'annotation named
        | Variable of 'annotation variable
        | Keywords of 'annotation
      [@@deriving compare, equal, sexp, show, hash]

      val annotation : 'annotation t -> 'annotation option

      val name : 'annotation t -> Identifier.t option
    end

    type kind =
      | Anonymous
      | Named of Reference.t

    and 'annotation params_from_param_spec = {
      head: 'annotation list;
      variable: 'annotation Variable.ParamSpec.record;
    }

    and 'annotation record_parameters =
      | Defined of 'annotation CallableParamType.t list
      | Undefined
      | FromParamSpec of 'annotation params_from_param_spec

    and 'annotation overload = {
      annotation: 'annotation;
      parameters: 'annotation record_parameters;
    }
    [@@deriving compare, equal, sexp, show, hash]

    and 'annotation record = {
      kind: kind;
      implementation: 'annotation overload;
      overloads: 'annotation overload list;
    }
    [@@deriving compare, equal, sexp, show, hash]
  end

  module Argument : sig
    type 'annotation record =
      | Single of 'annotation
      | CallableParameters of 'annotation Callable.record_parameters
      | Unpacked of 'annotation OrderedTypes.Concatenation.record_unpackable
  end

  module RecursiveType : sig
    type 'annotation record [@@deriving compare, equal, sexp, show, hash]

    val name : 'annotation record -> Identifier.t

    val body : 'annotation record -> 'annotation
  end

  module TypeOperation : sig
    module Compose : sig
      type 'annotation t = 'annotation OrderedTypes.record
      [@@deriving compare, equal, sexp, show, hash]
    end

    type 'annotation record = Compose of 'annotation Compose.t
    [@@deriving compare, equal, sexp, show, hash]
  end
end

module Primitive : sig
  type t = Identifier.t [@@deriving compare, equal, sexp, show, hash]

  include Hashable with type t := t

  module Set : Set.S with type Elt.t = t

  val is_unit_test : t -> bool
end

type literal_string =
  | LiteralValue of string
  | AnyLiteral

and literal =
  | Boolean of bool
  | Integer of int
  | String of literal_string
  | Bytes of string
  | EnumerationMember of {
      enumeration_type: t;
      member_name: Identifier.t;
    }

and t =
  | Bottom
  | Callable of t Record.Callable.record
  | Any
  | Literal of literal
  | NoneType
  | Parametric of {
      name: Identifier.t;
      arguments: t Record.Argument.record list;
    }
  | ParamSpecComponent of Record.Variable.ParamSpec.Components.t
  | Primitive of Primitive.t
  | PyreReadOnly of t
  | RecursiveType of t Record.RecursiveType.record
  | Top
  | Tuple of t Record.OrderedTypes.record
  | TypeOperation of t Record.TypeOperation.record
  | Union of t list
  | Variable of t Record.Variable.TypeVar.record
[@@deriving compare, equal, sexp, show, hash]

type type_t = t [@@deriving compare, equal, sexp, show]

module Map : Map.S with type Key.t = t

module Set : Set.S with type Elt.t = t

include Hashable with type t := t

module Argument : sig
  include module type of struct
    include Record.Argument
  end

  type t = type_t Record.Argument.record [@@deriving compare, equal, sexp, show, hash]

  val all_singles : t list -> type_t list option

  val to_variable : t -> type_t Record.Variable.record option

  val pp_list : Format.formatter -> t list -> unit
end

val pp_concise : Format.formatter -> t -> unit

val show_concise : t -> string

val parametric : string -> Argument.t list -> t

val awaitable : t -> t

val coroutine : Argument.t list -> t

val bool : t

val bytes : t

val complex : t

val dictionary : key:t -> value:t -> t

val enumeration : t

val float : t

val generator : ?yield_type:t -> ?send_type:t -> ?return_type:t -> unit -> t

val async_generator : ?yield_type:t -> ?send_type:t -> unit -> t

val generator_expression : t -> t

val generic_primitive : t

val integer : t

val literal_integer : int -> t

val iterable : t -> t

val iterator : t -> t

val async_iterator : t -> t

val lambda : parameters:(Identifier.t * t) list -> return_annotation:t -> t

val list : t -> t

val mapping_primitive : string

val class_type : t -> t

val extract_from_class_type : t -> t option

val named_tuple : t

val none : t

val number : t

val object_primitive : t

val optional : t -> t

val sequence : t -> t

val set : t -> t

val string : t

val literal_string : string -> t

val literal_bytes : string -> t

val literal_any_string : t

val tuple : t list -> t

val union : t list -> t

val yield : t -> t

val expression : t -> Expression.t

module VisitWithTransform : sig
  type 'state visit_result = {
    transformed_annotation: t;
    new_state: 'state;
  }

  module type Implementation = sig
    type state

    val visit : state -> t -> state visit_result

    val visit_children_before : state -> t -> bool

    val visit_children_after : bool
  end

  module Make (Implementation : Implementation) : sig
    val visit : Implementation.state -> t -> Implementation.state * t
  end
end

val exists : t -> predicate:(t -> bool) -> bool

val collect_primitive_types : t -> t list

val collect_names : t -> Primitive.t list

val collect_types : t -> predicate:(t -> bool) -> t list

val apply_type_map : ?visit_children_before:bool -> t -> type_map:(t -> t option) -> t

module Callable : sig
  include module type of struct
    include Record.Callable
  end

  module CallableParamType : sig
    include module type of struct
      include Record.Callable.CallableParamType
    end

    val show_concise : type_t t -> string

    type parameter = type_t t [@@deriving compare, equal, sexp, show, hash]

    module Map : Core.Map.S with type Key.t = parameter

    (** Used to indicate * when passing in a list of parameters and creating a callable *)
    val dummy_star_parameter : type_t Record.Callable.CallableParamType.named

    val create : 'annotation named list -> 'annotation t list

    val default : parameter -> bool

    val names_compatible : parameter -> parameter -> bool

    val zip
      :  'a t list ->
      'b t list ->
      [ `Both of 'a t * 'b t | `Left of 'a t | `Right of 'b t ] list
  end

  type t = type_t Record.Callable.record [@@deriving compare, equal, sexp, show, hash]

  type parameters = type_t Record.Callable.record_parameters
  [@@deriving compare, equal, sexp, show, hash]

  module Overload : sig
    val parameters : type_t overload -> CallableParamType.parameter list option

    val return_annotation : type_t overload -> type_t

    val is_undefined : type_t overload -> bool
  end

  val from_overloads : t list -> t option

  val map : t -> f:(type_t -> type_t) -> t option

  val map_implementation : type_t overload -> f:(type_t -> type_t) -> type_t overload

  val map_parameters : t -> f:(parameters -> parameters) -> t

  val map_parameters_with_result
    :  t ->
    f:(parameters -> (parameters, 'error) result) ->
    (t, 'error) result

  val map_annotation : t -> f:(type_t -> type_t) -> t

  val with_return_annotation : t -> annotation:type_t -> t

  val create
    :  ?name:Reference.t ->
    ?overloads:type_t overload list ->
    ?parameters:parameters ->
    annotation:type_t ->
    unit ->
    type_t

  val create_from_implementation : type_t overload -> type_t

  val prepend_anonymous_parameters
    :  head:type_t list ->
    tail:type_t CallableParamType.t list ->
    type_t CallableParamType.t list

  val name : t -> Reference.t option
end

module RecursiveType : sig
  include module type of struct
    include Record.RecursiveType
  end

  val create : name:Primitive.t -> body:t -> t

  val is_recursive_alias_reference : alias_name:Primitive.t -> t -> bool

  val unfold_recursive_type : t record -> t

  val body_with_replaced_name : new_name:Primitive.t -> t record -> t

  module Namespace : sig
    val reset : unit -> unit

    val create_fresh_name : unit -> string
  end
end

module TypeOperation : sig
  include module type of struct
    include Record.TypeOperation
  end

  module Compose : sig
    include module type of struct
      include Record.TypeOperation.Compose
    end

    type t = type_t Record.TypeOperation.Compose.t

    val create : t -> type_t option
  end

  type t = type_t Record.TypeOperation.record
end

val optional_value : t -> t option

val awaitable_value : t -> t option

val coroutine_value : t -> t option

val class_variable_value : t -> t option

val unpack_value : t -> t option

val literal_integer_value : t -> int option

val final_value : t -> [> `NoArgument | `NotFinal | `Ok of t ]

val is_any : t -> bool

val is_async_iterator : t -> bool

val is_callable : t -> bool

val is_dictionary : ?with_key:t option -> t -> bool

val is_dictionary_or_mapping : t -> bool

val is_ellipsis : t -> bool

val is_generic_primitive : t -> bool

val is_iterable : t -> bool

val is_iterator : t -> bool

val is_list : t -> bool

val is_class_type : t -> bool

val is_none : t -> bool

val is_noreturn_or_never : t -> bool

val is_object : t -> bool

val is_optional : t -> bool

val is_optional_primitive : t -> bool

val is_primitive : t -> bool

val is_primitive_string : t -> bool

val is_literal_string : t -> bool

val is_top : t -> bool

val is_tuple : t -> bool

val is_type_alias : t -> bool

val is_unbound : t -> bool

val is_union : t -> bool

val is_falsy : t -> bool

val is_truthy : t -> bool

val contains_callable : t -> bool

val contains_any : t -> bool

val contains_unknown : t -> bool

val contains_undefined : t -> bool

val expression_contains_any : Expression.t -> bool

(* Contains `Bottom` or variables. *)
val is_not_instantiated : t -> bool

val contains_literal : t -> bool

val contains_final : t -> bool

val primitive_name : t -> Identifier.t option

val create_literal : Expression.expression -> t option

val is_partially_typed : t -> bool

val is_unpack : t -> bool

val is_untyped : t -> bool

val contains_variable : t -> bool

type type_guard_kind =
  | NoGuard
  | TypeIs of t
  | TypeGuard of t

val type_guard_kind_if_any : t -> type_guard_kind

val arguments : t -> Argument.t list option

val type_arguments_for_bounded_tuple_union : t -> t list option

val single_argument : t -> t

val weaken_literals : t -> t

module OrderedTypes : sig
  include module type of struct
    include Record.OrderedTypes
  end

  module Concatenation : sig
    include module type of struct
      include Record.OrderedTypes.Concatenation
    end

    val pp_unpackable : Format.formatter -> type_t record_unpackable -> unit

    val extract_sole_variadic
      :  'annotation t ->
      'annotation Record.Variable.TypeVarTuple.record option

    val extract_sole_unbounded_annotation : 'annotation t -> 'annotation option

    val is_fully_unbounded : 'annotation t -> bool
  end

  type t = type_t record [@@deriving compare, equal, sexp, show, hash]

  type ordered_types_t = t

  val pp_concise : Format.formatter -> t -> unit

  val union_upper_bound : t -> type_t

  (* Concatenation is only defined for certain members *)
  val concatenate : left:t -> right:t -> t option

  val to_arguments : t -> Argument.t list

  val to_starred_annotation_expression
    :  expression:(type_t -> Expression.t) ->
    type_t Concatenation.t ->
    Expression.t

  val concatenation_from_unpack_expression
    :  parse_annotation:(Expression.t -> type_t) ->
    Expression.t ->
    type_t Concatenation.t option

  val coalesce_ordered_types : type_t record list -> type_t record option

  type 'annotation ordered_type_split = {
    prefix_pairs: ('annotation * 'annotation) list;
    middle_pair: 'annotation record * 'annotation record;
    suffix_pairs: ('annotation * 'annotation) list;
  }
  [@@deriving compare, equal, sexp, show, hash]

  val split_matching_elements_by_length
    :  'annotation record ->
    'annotation record ->
    'annotation ordered_type_split option

  val drop_prefix : length:int -> 'annotation record -> 'annotation record option

  val index : python_index:int -> 'annotation record -> 'annotation option
end

val split : t -> t * Argument.t list

val class_name : t -> Reference.t

val class_variable : t -> t

val is_class_variable : t -> bool

val assume_any : t -> t

(* Takes a map generated from Preprocessing.dequalify_map and a type and dequalifies the type *)
val dequalify : Reference.t Reference.Map.t -> t -> t

val preprocess_alias_value : Expression.t -> Expression.t

val dequalify_identifier : Reference.t Reference.Map.t -> Identifier.t -> Identifier.t

val dequalify_reference : Reference.t Reference.Map.t -> Reference.t -> Reference.t

module Variable : sig
  include module type of struct
    include Record.Variable
  end

  val name : 'a record -> string

  module Namespace : sig
    include module type of struct
      include Record.Variable.Namespace
    end

    val reset : unit -> unit

    val create_fresh : unit -> t
  end

  type unary_t = type_t Record.Variable.TypeVar.record [@@deriving compare, equal, sexp, show, hash]

  type unary_domain = type_t [@@deriving compare, equal, sexp, show, hash]

  type parameter_variadic_t = type_t Record.Variable.ParamSpec.record
  [@@deriving compare, equal, sexp, show, hash]

  type parameter_variadic_domain = Callable.parameters [@@deriving compare, equal, sexp, show, hash]

  type tuple_variadic_t = type_t Record.Variable.TypeVarTuple.record
  [@@deriving compare, equal, sexp, show, hash]

  type tuple_variadic_domain = type_t OrderedTypes.record
  [@@deriving compare, equal, sexp, show, hash]

  type pair =
    | TypeVarPair of unary_t * unary_domain
    | ParamSpecPair of parameter_variadic_t * parameter_variadic_domain
    | TypeVarTuplePair of tuple_variadic_t * tuple_variadic_domain
  [@@deriving compare, equal, sexp, show, hash]

  type t = type_t Record.Variable.record [@@deriving compare, equal, sexp, show, hash]

  module type VariableKind = sig
    type t [@@deriving compare, equal, sexp, show, hash]

    module Map : Core.Map.S with type Key.t = t

    val is_free : t -> bool

    val is_escaped_and_free : t -> bool

    val mark_as_bound : t -> t

    val mark_as_escaped : t -> t

    val mark_as_free : t -> t

    val namespace : t -> namespace:Namespace.t -> t

    val dequalify : t -> dequalify_map:Reference.t Reference.Map.t -> t

    type domain [@@deriving compare, equal, sexp, show, hash]

    val any : domain

    (* The value in the domain directly corresponding to the variable, i.e. the replacement that
       would leave a type unchanged *)
    val self_reference : t -> domain

    val pair : t -> domain -> pair
  end

  module TypeVar : sig
    include module type of struct
      include Record.Variable.TypeVar
    end

    include VariableKind with type t = unary_t and type domain = type_t

    val create : ?constraints:type_t Record.TypeVarConstraints.t -> string -> t

    val upper_bound : t -> type_t

    val is_escaped_and_free : t -> bool

    val contains_subvariable : t -> bool
  end

  module ParamSpec : sig
    include VariableKind with type t = parameter_variadic_t and type domain = Callable.parameters

    val name : t -> Identifier.t

    val create : string -> t

    val of_component_annotations
      :  get_param_spec:(Primitive.t -> t option) ->
      args_annotation:Expression.t ->
      kwargs_annotation:Expression.t ->
      t option

    module Components : sig
      include module type of struct
        include Record.Variable.ParamSpec.Components
      end

      type component =
        | KeywordArguments
        | PositionalArguments

      type decomposition = {
        positional_component: type_t;
        keyword_component: type_t;
      }

      val combine : decomposition -> parameter_variadic_t option

      val component : t -> component
    end

    val decompose : t -> Components.decomposition
  end

  module TypeVarTuple : sig
    include VariableKind with type t = tuple_variadic_t and type domain = tuple_variadic_domain

    val name : t -> Identifier.t

    val create : string -> t

    val synthetic_class_name_for_error : string
  end

  module GlobalTransforms : sig
    module type S = sig
      type t

      type domain

      val replace_all : (t -> domain option) -> type_t -> type_t

      val collect_all : type_t -> t list
    end

    module TypeVar : S with type t = unary_t and type domain = type_t

    module ParamSpec : S with type t = parameter_variadic_t and type domain = Callable.parameters

    module TypeVarTuple :
      S with type t = tuple_variadic_t and type domain = type_t OrderedTypes.record
  end

  module Declaration : sig
    type t =
      | DTypeVar of {
          name: Identifier.t;
          constraints: Expression.t Record.TypeVarConstraints.t;
          variance: Record.PreInferenceVariance.t;
          infer_variance: bool;
        }
      | DTypeVarTuple of { name: Identifier.t }
      | DParamSpec of { name: Identifier.t }
    [@@deriving compare, equal, sexp, show, hash]

    val parse : Expression.t -> target:Reference.t -> t option
  end

  type variable_zip_result = {
    variable_pair: pair;
    received_argument: Argument.t;
  }
  [@@deriving compare, equal, sexp, show, hash]

  module Set : Core.Set.S with type Elt.t = t

  val of_declaration : Declaration.t -> create_type:(Expression.t -> type_t) -> t

  val of_ast_type_param
    :  Ast.Expression.TypeParam.type_param Ast.Node.t ->
    create_type:(Ast.Expression.Expression.t -> 'a) ->
    'a record

  val constraints_of_bound
    :  Ast.Expression.Expression.expression Ast.Node.t option ->
    create_type:(Ast.Expression.Expression.t -> 'a) ->
    'a Record.TypeVarConstraints.t

  val pp_concise : Format.formatter -> t -> unit

  val dequalify : Reference.t Reference.Map.t -> t -> t

  val namespace : t -> namespace:Namespace.t -> t

  val mark_all_variables_as_bound : ?specific:t list -> type_t -> type_t

  val mark_all_variables_as_free : ?specific:t list -> type_t -> type_t

  val mark_as_bound : t -> t

  val namespace_all_free_variables : type_t -> namespace:Namespace.t -> type_t

  val all_free_variables : type_t -> t list

  val all_variables_are_resolved : type_t -> bool

  val mark_all_free_variables_as_escaped : ?specific:t list -> type_t -> type_t

  val collapse_all_escaped_variable_unions : type_t -> type_t

  val contains_escaped_free_variable : type_t -> bool

  val convert_all_escaped_free_variables_to_anys : type_t -> type_t

  val zip_variables_with_arguments : arguments:Argument.t list -> t list -> pair list option

  val zip_variables_with_arguments_including_mismatches
    :  arguments:Argument.t list ->
    t list ->
    variable_zip_result list option

  val zip_variables_with_two_argument_lists
    :  left_arguments:Argument.t list ->
    right_arguments:Argument.t list ->
    t list ->
    (pair * pair) list option

  val all_unary : t list -> TypeVar.t list option

  val to_argument : t -> Argument.t
end

module GenericParameter : sig
  type t =
    | GpTypeVar of {
        name: Identifier.t;
        variance: Record.PreInferenceVariance.t;
        constraints: type_t Record.TypeVarConstraints.t;
      }
    | GpTypeVarTuple of { name: Identifier.t }
    | GpParamSpec of { name: Identifier.t }
  [@@deriving compare, equal, sexp, show, hash]

  val of_declaration : Variable.Declaration.t -> create_type:(Expression.t -> type_t) -> t

  val to_variable : t -> type_t Record.Variable.record

  val parameter_name : t -> string

  module ZipTwoArgumentsLists : sig
    type result =
      | TypeVarZipResult of {
          name: Identifier.t;
          left: type_t;
          right: type_t;
        }
      | TypeVarTupleZipResult of {
          name: Identifier.t;
          left: type_t Record.OrderedTypes.record;
          right: type_t Record.OrderedTypes.record;
        }
      | ParamSpecZipResult of {
          name: Identifier.t;
          left: type_t Record.Callable.record_parameters;
          right: type_t Record.Callable.record_parameters;
        }
      | MismatchedKindsZipResult of {
          parameter: t;
          left: type_t Record.Argument.record;
          right: type_t Record.Argument.record;
        }
      | MismatchedLengthsZipResult of {
          remaining_parameters: t list;
          remaining_left: type_t Record.Argument.record list;
          remaining_right: type_t Record.Argument.record list;
        }
      | MismatchedVariadicZipResult of {
          parameter: t;
          left: type_t Record.Argument.record list;
          right: type_t Record.Argument.record list;
        }
    [@@deriving compare, sexp, show]

    val zip
      :  left_arguments:type_t Record.Argument.record list ->
      right_arguments:type_t Record.Argument.record list ->
      t list ->
      result list
  end
end

val namespace_insensitive_compare : t -> t -> int

val variable : ?constraints:type_t Record.TypeVarConstraints.t -> string -> t

val is_concrete : t -> bool

module TypedDictionary : sig
  type typed_dictionary_field = {
    name: string;
    annotation: type_t;
    required: bool;
    readonly: bool;
  }
  [@@deriving compare, equal, sexp, show, hash]

  type t = {
    name: Identifier.t;
    fields: typed_dictionary_field list;
  }
  [@@deriving compare, equal, sexp, show, hash]

  val base_typed_dictionary : type_t

  val anonymous : typed_dictionary_field list -> t

  val create_field
    :  annotation:type_t ->
    has_non_total_typed_dictionary_base_class:bool ->
    string ->
    typed_dictionary_field

  val are_fields_total : typed_dictionary_field list -> bool

  val same_name : typed_dictionary_field -> typed_dictionary_field -> bool

  val same_name_different_requiredness : typed_dictionary_field -> typed_dictionary_field -> bool

  val same_name_different_annotation : typed_dictionary_field -> typed_dictionary_field -> bool

  val fields_have_colliding_keys
    :  typed_dictionary_field list ->
    typed_dictionary_field list ->
    bool

  val constructor : name:Identifier.t -> fields:typed_dictionary_field list -> Callable.t

  val fields_from_constructor
    :  Callable.t ->
    (string, bool, Base.String.comparator_witness) Map_intf.Map.t ->
    typed_dictionary_field list option

  val special_overloads
    :  class_name:Primitive.t ->
    fields:typed_dictionary_field list ->
    method_name:string ->
    type_t Callable.overload list option

  val is_special_mismatch
    :  class_name:Primitive.t ->
    total:bool ->
    method_name:string ->
    position:int ->
    bool

  val defines : total:bool -> t_self_expression:Expression.t -> Statement.t list

  val class_name : total:bool -> Primitive.t

  val is_builtin_typed_dictionary_class : Primitive.t -> bool

  val is_update_method : Reference.t -> bool
end

module PyreReadOnly : sig
  val create : t -> t

  val unpack_readonly : t -> t option

  val is_readonly : t -> bool

  val strip_readonly : t -> t

  val contains_readonly : t -> bool

  val lift_readonly_if_possible : make_container:(t -> t) -> t -> t
end

val resolve_aliases
  :  aliases:(?replace_unbound_parameters_with_any:bool -> string -> t option) ->
  t ->
  t

val create
  :  variables:(string -> Variable.t option) ->
  aliases:(?replace_unbound_parameters_with_any:bool -> Primitive.t -> t option) ->
  Expression.t ->
  t

val resolved_empty_aliases : ?replace_unbound_parameters_with_any:bool -> Primitive.t -> t option

val resolved_empty_variables : string -> Variable.t option

val infer_transform : t -> t

val contains_prohibited_any : t -> bool

val to_yojson : t -> Yojson.Safe.t

type class_attribute_lookup_data = {
  class_name: Primitive.t;
  type_for_lookup: t;
  accessed_through_class: bool;
  accessed_through_readonly: bool;
}
[@@deriving sexp]

val class_attribute_lookups_for_type : t -> class_attribute_lookup_data list option

(* Gives the name of either a Callable or BoundMethod[Callable, X] type *)
val callable_name : t -> Reference.t option

val equivalent_for_assert_type : t -> t -> bool
