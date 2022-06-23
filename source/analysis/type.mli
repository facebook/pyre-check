(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast

module Record : sig
  module Variable : sig
    type state [@@deriving compare, eq, sexp, show, hash]

    module RecordNamespace : sig
      type t [@@deriving compare, eq, sexp, show, hash]
    end

    type 'annotation constraints =
      | Bound of 'annotation
      | Explicit of 'annotation list
      | Unconstrained
      | LiteralIntegers
    [@@deriving compare, eq, sexp, show, hash]

    type variance =
      | Covariant
      | Contravariant
      | Invariant
    [@@deriving compare, eq, sexp, show, hash]

    module RecordUnary : sig
      type 'annotation record = {
        variable: Identifier.t;
        constraints: 'annotation constraints;
        variance: variance;
        state: state;
        namespace: RecordNamespace.t;
      }
      [@@deriving compare, eq, sexp, show, hash]
    end

    module RecordVariadic : sig
      module RecordParameters : sig
        type 'annotation record [@@deriving compare, eq, sexp, show, hash]

        module RecordComponents : sig
          type t [@@deriving compare, eq, sexp, show, hash]
        end
      end

      module Tuple : sig
        type 'annotation record [@@deriving compare, eq, sexp, show, hash]
      end
    end

    type 'a record =
      | Unary of 'a RecordUnary.record
      | ParameterVariadic of 'a RecordVariadic.RecordParameters.record
      | TupleVariadic of 'a RecordVariadic.Tuple.record
    [@@deriving compare, eq, sexp, show, hash]
  end

  module OrderedTypes : sig
    module Concatenation : sig
      type 'annotation record_unpackable [@@deriving compare, eq, sexp, show, hash]

      type 'annotation t [@@deriving compare, eq, sexp, show, hash]

      type 'annotation record_broadcast [@@deriving compare, eq, sexp, show, hash]

      val pp_unpackable
        :  pp_type:(Format.formatter -> 'annotation -> unit) ->
        Format.formatter ->
        'annotation record_unpackable ->
        unit

      val create_unpackable
        :  'annotation Variable.RecordVariadic.Tuple.record ->
        'annotation record_unpackable

      val create_unbounded_unpackable : 'annotation -> 'annotation record_unpackable

      val extract_sole_variadic
        :  'annotation t ->
        'annotation Variable.RecordVariadic.Tuple.record option

      val extract_sole_unbounded_annotation : 'annotation t -> 'annotation option

      val is_fully_unbounded : 'annotation t -> bool

      val create_from_unpackable
        :  ?prefix:'annotation list ->
        ?suffix:'annotation list ->
        'annotation record_unpackable ->
        'annotation t

      val create
        :  ?prefix:'annotation list ->
        ?suffix:'annotation list ->
        'annotation Variable.RecordVariadic.Tuple.record ->
        'annotation t

      val create_from_unbounded_element
        :  ?prefix:'annotation list ->
        ?suffix:'annotation list ->
        'annotation ->
        'annotation t

      val create_unpackable_from_concrete_against_concatenation
        :  concrete:'annotation list ->
        concatenation:'annotation t ->
        'annotation record_unpackable

      val create_unpackable_from_concatenation_against_concatenation
        :  compare_t:('annotation -> 'annotation -> int) ->
        'annotation t ->
        'annotation t ->
        'annotation record_unpackable

      val create_from_concrete_against_concrete
        :  ?prefix:'annotation list ->
        ?suffix:'annotation list ->
        compare_t:('annotation -> 'annotation -> int) ->
        left:'annotation list ->
        right:'annotation list ->
        'annotation t

      val create_from_concrete_against_concatenation
        :  ?prefix:'annotation list ->
        ?suffix:'annotation list ->
        concrete:'annotation list ->
        concatenation:'annotation t ->
        'annotation t

      val create_from_concatenation_against_concatenation
        :  ?prefix:'annotation list ->
        ?suffix:'annotation list ->
        compare_t:('annotation -> 'annotation -> int) ->
        'annotation t ->
        'annotation t ->
        'annotation t
    end

    type 'annotation record =
      | Concrete of 'annotation list
      | Concatenation of 'annotation Concatenation.t
    [@@deriving compare, eq, sexp, show, hash]

    type 'annotation ordered_type_split = {
      prefix_pairs: ('annotation * 'annotation) list;
      middle_pair: 'annotation record * 'annotation record;
      suffix_pairs: ('annotation * 'annotation) list;
    }
    [@@deriving compare, eq, sexp, show, hash]

    val create_unbounded_concatenation : 'annotation -> 'annotation record

    val pp_concise
      :  Format.formatter ->
      'a record ->
      pp_type:(Format.formatter -> 'a -> unit) ->
      unit

    val split_matching_elements_by_length
      :  'annotation record ->
      'annotation record ->
      'annotation ordered_type_split option

    val drop_prefix : length:int -> 'annotation record -> 'annotation record option

    val index : python_index:int -> 'annotation record -> 'annotation option
  end

  module Callable : sig
    module RecordParameter : sig
      type 'annotation named = {
        name: Identifier.t;
        annotation: 'annotation;
        default: bool;
      }

      and 'annotation variable =
        | Concrete of 'annotation
        | Concatenation of 'annotation OrderedTypes.Concatenation.t
      [@@deriving compare, eq, sexp, show, hash]

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
      [@@deriving compare, eq, sexp, show, hash]

      val annotation : 'annotation t -> 'annotation option
    end

    type kind =
      | Anonymous
      | Named of Reference.t

    and 'annotation parameter_variadic_type_variable = {
      head: 'annotation list;
      variable: 'annotation Variable.RecordVariadic.RecordParameters.record;
    }

    and 'annotation record_parameters =
      | Defined of 'annotation RecordParameter.t list
      | Undefined
      | ParameterVariadicTypeVariable of 'annotation parameter_variadic_type_variable

    and 'annotation overload = {
      annotation: 'annotation;
      parameters: 'annotation record_parameters;
    }
    [@@deriving compare, eq, sexp, show, hash]

    and 'annotation record = {
      kind: kind;
      implementation: 'annotation overload;
      overloads: 'annotation overload list;
    }
    [@@deriving compare, eq, sexp, show, hash]
  end

  module Parameter : sig
    type 'annotation record =
      | Single of 'annotation
      | CallableParameters of 'annotation Callable.record_parameters
      | Unpacked of 'annotation OrderedTypes.Concatenation.record_unpackable
  end

  module TypedDictionary : sig
    type 'annotation typed_dictionary_field = {
      name: string;
      annotation: 'annotation;
      required: bool;
    }
    [@@deriving compare, eq, sexp, show, hash]

    type 'annotation record = {
      name: Identifier.t;
      fields: 'annotation typed_dictionary_field list;
    }
    [@@deriving compare, eq, sexp, show, hash]
  end

  module RecursiveType : sig
    type 'annotation record [@@deriving compare, eq, sexp, show, hash]

    val name : 'annotation record -> Identifier.t
  end

  module TypeOperation : sig
    module Compose : sig
      type 'annotation t = 'annotation OrderedTypes.record
      [@@deriving compare, eq, sexp, show, hash]
    end

    type 'annotation record = Compose of 'annotation Compose.t
    [@@deriving compare, eq, sexp, show, hash]
  end
end

module Monomial : sig
  module Operation : sig
    type 'a t
  end

  type 'a variable [@@deriving compare, eq, sexp, show, hash]

  type 'a t [@@deriving eq, sexp, compare, hash, show]

  val create_variable : 'a Record.Variable.RecordUnary.record -> 'a variable

  val create_product : 'a Record.OrderedTypes.Concatenation.record_unpackable -> 'a variable
end

module Polynomial : sig
  type 'a t [@@deriving compare, eq, sexp, hash, show]

  val is_base_case : 'a t -> bool

  val show_normal
    :  show_variable:('a Record.Variable.RecordUnary.record -> string) ->
    show_type:(Format.formatter -> 'a -> unit) ->
    'a t ->
    string

  val create_from_variable : 'a Record.Variable.RecordUnary.record -> 'a t

  val create_from_int : int -> 'a t

  val create_from_operation : 'a Monomial.Operation.t -> 'a t

  val create_from_variables_list
    :  compare_t:('a -> 'a -> int) ->
    (int * ('a Record.Variable.RecordUnary.record * int) list) list ->
    'a t

  val create_from_monomial_variables_list
    :  compare_t:('a -> 'a -> int) ->
    (int * ('a Monomial.variable * int) list) list ->
    'a t

  val add : compare_t:('a -> 'a -> int) -> 'a t -> 'a t -> 'a t

  val subtract : compare_t:('a -> 'a -> int) -> 'a t -> 'a t -> 'a t

  val multiply : compare_t:('a -> 'a -> int) -> 'a t -> 'a t -> 'a t

  val divide : compare_t:('a -> 'a -> int) -> 'a t -> 'a t -> 'a t

  val replace
    :  compare_t:('a -> 'a -> int) ->
    'a t ->
    by:'a t ->
    variable:'a Monomial.variable ->
    'a t
end

module RecordIntExpression : sig
  type 'a t = private Data of 'a Polynomial.t [@@deriving compare, eq, sexp, show, hash]
end

module Primitive : sig
  type t = Identifier.t [@@deriving compare, eq, sexp, show, hash]

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
  | Annotated of t
  | Bottom
  | Callable of t Record.Callable.record
  | Any
  | Literal of literal
  | NoneType
  | Parametric of {
      name: Identifier.t;
      parameters: t Record.Parameter.record list;
    }
  | ParameterVariadicComponent of Record.Variable.RecordVariadic.RecordParameters.RecordComponents.t
  | Primitive of Primitive.t
  | RecursiveType of t Record.RecursiveType.record
  | Top
  | Tuple of t Record.OrderedTypes.record
  | TypeOperation of t Record.TypeOperation.record
  | Union of t list
  | Variable of t Record.Variable.RecordUnary.record
  | IntExpression of t RecordIntExpression.t
[@@deriving compare, eq, sexp, show, hash]

module IntExpression : sig
  val create : t Polynomial.t -> t
end

type class_data = {
  instantiated: t;
  accessed_through_class: bool;
  class_name: Primitive.t;
}
[@@deriving sexp]

type type_t = t [@@deriving compare, eq, sexp, show]

val polynomial_to_type : t Polynomial.t -> t

val solve_less_or_equal_polynomial
  :  left:t ->
  right:t ->
  solve:(left:t -> right:t -> 'a) ->
  impossible:'a ->
  'a

module Map : Map.S with type Key.t = t

val default_to_bottom : t Map.t -> t list -> t Map.t

module Set : Set.S with type Elt.t = t

include Hashable with type t := t

val pp_typed_dictionary_field
  :  pp_type:(Format.formatter -> type_t -> unit) ->
  Format.formatter ->
  t Record.TypedDictionary.typed_dictionary_field ->
  unit

val polynomial_show_variable : type_t Record.Variable.RecordUnary.record -> string

val pp_concise : Format.formatter -> t -> unit

module Parameter : sig
  include module type of struct
    include Record.Parameter
  end

  type t = type_t Record.Parameter.record [@@deriving compare, eq, sexp, show, hash]

  val all_singles : t list -> type_t list option

  val to_variable : t -> type_t Record.Variable.record option
end

val pp_parameters
  :  pp_type:(Format.formatter -> type_t -> unit) ->
  Format.formatter ->
  Parameter.t sexp_list ->
  unit

val show_concise : t -> string

val show_for_hover : t -> string

val serialize : t -> string

val parametric : string -> Parameter.t list -> t

val annotated : t -> t

val awaitable : t -> t

val coroutine : Parameter.t list -> t

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

val meta : t -> t

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

val tuple : t list -> t

val union : t list -> t

val yield : t -> t

val expression : t -> Expression.t

module Transform : sig
  type 'state visit_result = {
    transformed_annotation: t;
    new_state: 'state;
  }

  module type Transformer = sig
    type state

    val visit : state -> t -> state visit_result

    val visit_children_before : state -> t -> bool

    val visit_children_after : bool
  end

  module Make (Transformer : Transformer) : sig
    val visit : Transformer.state -> t -> Transformer.state * t
  end
end

val exists : t -> predicate:(t -> bool) -> bool

module Callable : sig
  module Parameter : sig
    include module type of struct
      include Record.Callable.RecordParameter
    end

    val show_concise : type_t t -> string

    type parameter = type_t t [@@deriving compare, eq, sexp, show, hash]

    module Map : Core.Map.S with type Key.t = parameter

    val create : 'annotation named list -> 'annotation t list

    val default : parameter -> bool

    val names_compatible : parameter -> parameter -> bool

    val zip
      :  'a t list ->
      'b t list ->
      [ `Both of 'a t * 'b t | `Left of 'a t | `Right of 'b t ] list
  end

  include module type of struct
    include Record.Callable
  end

  type t = type_t Record.Callable.record [@@deriving compare, eq, sexp, show, hash]

  type parameters = type_t Record.Callable.record_parameters
  [@@deriving compare, eq, sexp, show, hash]

  module Overload : sig
    val parameters : type_t overload -> Parameter.parameter list option

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
    tail:type_t Parameter.t list ->
    type_t Parameter.t list

  val resolve_getitem_callee
    :  resolve_aliases:(type_t -> type_t) ->
    Expression.expression ->
    Expression.expression
end

type alias =
  | TypeAlias of t
  | VariableAlias of t Record.Variable.record
[@@deriving compare, eq, sexp, show, hash]

val resolve_aliases
  :  aliases:(?replace_unbound_parameters_with_any:bool -> string -> alias option) ->
  t ->
  t

val create
  :  aliases:(?replace_unbound_parameters_with_any:bool -> Primitive.t -> alias option) ->
  Expression.t ->
  t

val empty_aliases : ?replace_unbound_parameters_with_any:bool -> Primitive.t -> alias option

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

val contains_callable : t -> bool

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

val is_meta : t -> bool

val is_none : t -> bool

val is_noreturn : t -> bool

val is_object : t -> bool

val is_optional : t -> bool

val is_optional_primitive : t -> bool

val is_primitive : t -> bool

val is_top : t -> bool

val is_tuple : t -> bool

val is_type_alias : t -> bool

val is_unbound : t -> bool

val is_union : t -> bool

val is_falsy : t -> bool

val is_truthy : t -> bool

val contains_any : t -> bool

val contains_unknown : t -> bool

val contains_undefined : t -> bool

val expression_contains_any : Expression.t -> bool

(* Contains `Bottom` or variables. *)
val is_not_instantiated : t -> bool

val contains_literal : t -> bool

val collect : t -> predicate:(t -> bool) -> t list

val contains_final : t -> bool

val primitive_name : t -> Identifier.t option

val create_literal : Expression.expression -> t option

val primitives : t -> t list

val elements : t -> Primitive.t list

val is_partially_typed : t -> bool

val is_untyped : t -> bool

val contains_variable : t -> bool

val optional_value : t -> t option

val awaitable_value : t -> t option

val coroutine_value : t -> t option

val typeguard_annotation : t -> t option

val parameters : t -> Parameter.t list option

val type_parameters_for_bounded_tuple_union : t -> t list option

val single_parameter : t -> t

val instantiate
  :  ?widen:bool ->
  ?visit_children_before:bool ->
  t ->
  constraints:(t -> t option) ->
  t

val weaken_literals : t -> t

val weaken_to_arbitrary_literal_if_possible : t -> t

module OrderedTypes : sig
  include module type of struct
    include Record.OrderedTypes
  end

  type t = type_t record [@@deriving compare, eq, sexp, show, hash]

  type ordered_types_t = t

  val pp_concise : Format.formatter -> t -> unit

  val union_upper_bound : t -> type_t

  (* Concatenation is only defined for certain members *)
  val concatenate : left:t -> right:t -> t option

  val to_parameters : t -> Parameter.t list

  val to_starred_annotation_expression
    :  expression:(type_t -> Expression.t) ->
    type_t Concatenation.t ->
    Expression.t

  val concatenation_from_unpack_expression
    :  parse_annotation:(Expression.t -> type_t) ->
    Expression.t ->
    type_t Concatenation.t option

  val broadcast : type_t -> type_t -> type_t

  val coalesce_ordered_types : type_t record list -> type_t record option
end

val split : t -> t * Parameter.t list

val class_name : t -> Reference.t

val class_variable : t -> t

val class_variable_value : t -> t option

val final_value : t -> [> `NoParameter | `NotFinal | `Ok of t ]

val assume_any : t -> t

(* Takes a map generated from Preprocessing.dequalify_map and a type and dequalifies the type *)
val dequalify : Reference.t Reference.Map.t -> t -> t

val dequalify_identifier : Reference.t Reference.Map.t -> Identifier.t -> Identifier.t

val dequalify_reference : Reference.t Reference.Map.t -> Reference.t -> Reference.t

module Variable : sig
  module Namespace : sig
    include module type of struct
      include Record.Variable.RecordNamespace
    end

    val reset : unit -> unit

    val create_fresh : unit -> t
  end

  type unary_t = type_t Record.Variable.RecordUnary.record
  [@@deriving compare, eq, sexp, show, hash]

  type unary_domain = type_t [@@deriving compare, eq, sexp, show, hash]

  type parameter_variadic_t = type_t Record.Variable.RecordVariadic.RecordParameters.record
  [@@deriving compare, eq, sexp, show, hash]

  type parameter_variadic_domain = Callable.parameters [@@deriving compare, eq, sexp, show, hash]

  type tuple_variadic_t = type_t Record.Variable.RecordVariadic.Tuple.record
  [@@deriving compare, eq, sexp, show, hash]

  type tuple_variadic_domain = type_t OrderedTypes.record [@@deriving compare, eq, sexp, show, hash]

  type pair =
    | UnaryPair of unary_t * unary_domain
    | ParameterVariadicPair of parameter_variadic_t * parameter_variadic_domain
    | TupleVariadicPair of tuple_variadic_t * tuple_variadic_domain
  [@@deriving compare, eq, sexp, show, hash]

  type t = type_t Record.Variable.record [@@deriving compare, eq, sexp, show, hash]

  type variable_t = t

  module type VariableKind = sig
    type t [@@deriving compare, eq, sexp, show, hash]

    module Map : Core.Map.S with type Key.t = t

    val is_free : t -> bool

    val is_escaped_and_free : t -> bool

    val mark_as_bound : t -> t

    val mark_as_escaped : t -> t

    val mark_as_free : t -> t

    val namespace : t -> namespace:Namespace.t -> t

    val dequalify : t -> dequalify_map:Reference.t Reference.Map.t -> t

    type domain [@@deriving compare, eq, sexp, show, hash]

    val any : domain

    (* The value in the domain directly corresponding to the variable, i.e. the replacement that
       would leave a type unchanged *)
    val self_reference : t -> domain

    val pair : t -> domain -> pair
  end

  module Unary : sig
    include module type of struct
      include Record.Variable.RecordUnary
    end

    include VariableKind with type t = unary_t and type domain = type_t

    val create
      :  ?constraints:type_t Record.Variable.constraints ->
      ?variance:Record.Variable.variance ->
      string ->
      t

    val is_contravariant : t -> bool

    val is_covariant : t -> bool

    val upper_bound : t -> type_t

    val is_escaped_and_free : t -> bool

    val contains_subvariable : t -> bool
  end

  module Variadic : sig
    module Parameters : sig
      include VariableKind with type t = parameter_variadic_t and type domain = Callable.parameters

      val name : t -> Identifier.t

      val create : ?variance:Record.Variable.variance -> string -> t

      val parse_instance_annotation
        :  create_type:
             (aliases:(?replace_unbound_parameters_with_any:bool -> Primitive.t -> alias option) ->
             Expression.t ->
             type_t) ->
        variable_parameter_annotation:Expression.t ->
        keywords_parameter_annotation:Expression.t ->
        aliases:(?replace_unbound_parameters_with_any:bool -> Primitive.t -> alias option) ->
        t option

      module Components : sig
        include module type of struct
          include Record.Variable.RecordVariadic.RecordParameters.RecordComponents
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

    module Tuple : sig
      include VariableKind with type t = tuple_variadic_t and type domain = tuple_variadic_domain

      val name : t -> Identifier.t

      val create : string -> t

      val synthetic_class_name_for_error : string
    end
  end

  module GlobalTransforms : sig
    module type S = sig
      type t

      type domain

      val replace_all : (t -> domain option) -> type_t -> type_t

      val collect_all : type_t -> t list
    end

    module Unary : S with type t = unary_t and type domain = type_t

    module ParameterVariadic :
      S with type t = parameter_variadic_t and type domain = Callable.parameters

    module TupleVariadic :
      S with type t = tuple_variadic_t and type domain = type_t OrderedTypes.record
  end

  include module type of struct
    include Record.Variable
  end

  type variable_zip_result = {
    variable_pair: pair;
    received_parameter: Parameter.t;
  }
  [@@deriving compare, eq, sexp, show, hash]

  module Set : Core.Set.S with type Elt.t = t

  val pp_concise : Format.formatter -> t -> unit

  val parse_declaration : Expression.t -> target:Reference.t -> t option

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

  val zip_variables_with_parameters : parameters:Parameter.t list -> t list -> pair list option

  val zip_variables_with_parameters_including_mismatches
    :  parameters:Parameter.t list ->
    t list ->
    variable_zip_result list option

  val zip_variables_with_two_parameter_lists
    :  left_parameters:Parameter.t list ->
    right_parameters:Parameter.t list ->
    t list ->
    (pair * pair) list option

  val all_unary : t list -> Unary.t list option

  val to_parameter : t -> Parameter.t
end

val namespace_insensitive_compare : t -> t -> int

val variable
  :  ?constraints:type_t Variable.constraints ->
  ?variance:Variable.variance ->
  string ->
  t

val is_concrete : t -> bool

module TypedDictionary : sig
  open Record.TypedDictionary

  val anonymous : t typed_dictionary_field list -> t record

  val create_field
    :  annotation:t ->
    has_non_total_typed_dictionary_base_class:bool ->
    string ->
    t typed_dictionary_field

  val are_fields_total : t typed_dictionary_field list -> bool

  val same_name : t typed_dictionary_field -> t typed_dictionary_field -> bool

  val same_name_different_requiredness
    :  t typed_dictionary_field ->
    t typed_dictionary_field ->
    bool

  val same_name_different_annotation : t typed_dictionary_field -> t typed_dictionary_field -> bool

  val fields_have_colliding_keys
    :  t typed_dictionary_field list ->
    t typed_dictionary_field list ->
    bool

  val constructor : name:Identifier.t -> fields:t typed_dictionary_field list -> Callable.t

  val fields_from_constructor : Callable.t -> t typed_dictionary_field list option

  val special_overloads
    :  class_name:Primitive.t ->
    fields:t typed_dictionary_field list ->
    method_name:string ->
    t Callable.overload list option

  val is_special_mismatch
    :  class_name:Primitive.t ->
    total:bool ->
    method_name:string ->
    position:int ->
    bool

  val defines : total:bool -> t_self_expression:Expression.t -> Statement.t list

  val class_name : total:bool -> Primitive.t

  val is_builtin_typed_dictionary_class : Primitive.t -> bool
end

val infer_transform : t -> t

val contains_prohibited_any : t -> bool

val to_yojson : t -> Yojson.Safe.json

val resolve_class : t -> class_data list option

(* Gives the name of either a Callable or BoundMethod[Callable, X] type *)
val callable_name : t -> Reference.t option
