(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

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

      module RecordList : sig
        type 'annotation record [@@deriving compare, eq, sexp, show, hash]
      end
    end

    type 'annotation record =
      | Unary of 'annotation RecordUnary.record
      | ParameterVariadic of 'annotation RecordVariadic.RecordParameters.record
      | ListVariadic of 'annotation RecordVariadic.RecordList.record
    [@@deriving compare, eq, sexp, show, hash]
  end

  module OrderedTypes : sig
    module RecordConcatenate : sig
      module Middle : sig
        type 'annotation t [@@deriving compare, eq, sexp, show, hash]
      end

      type ('middle, 'outer) t [@@deriving compare, eq, sexp, show, hash]
    end

    type 'annotation record =
      | Concrete of 'annotation list
      | Any
      | Concatenation of ('annotation RecordConcatenate.Middle.t, 'annotation) RecordConcatenate.t
    [@@deriving compare, eq, sexp, show, hash]

    val pp_concise
      :  Format.formatter ->
      'a record ->
      pp_type:(Format.formatter -> 'a -> unit) ->
      unit
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
        | Concatenation of
            ( 'annotation OrderedTypes.RecordConcatenate.Middle.t,
              'annotation )
            OrderedTypes.RecordConcatenate.t

      and 'annotation t =
        | Anonymous of {
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

    and 'annotation implicit_record = {
      implicit_annotation: 'annotation;
      name: Identifier.t;
    }

    and 'annotation record_parameters =
      | Defined of 'annotation RecordParameter.t list
      | Undefined
      | ParameterVariadicTypeVariable of
          'annotation Variable.RecordVariadic.RecordParameters.record

    and 'annotation overload = {
      annotation: 'annotation;
      parameters: 'annotation record_parameters;
      define_location: Location.t option;
    }

    and 'annotation record = {
      kind: kind;
      implementation: 'annotation overload;
      overloads: 'annotation overload list;
      implicit: 'annotation implicit_record option;
    }
    [@@deriving compare, eq, sexp, show, hash]
  end
end

module Primitive : sig
  type t = Identifier.t [@@deriving compare, eq, sexp, show, hash]

  include Hashable with type t := t

  module Set : Set.S with type Elt.t = t
end

type literal =
  | Boolean of bool
  | Integer of int
  | String of string

and tuple =
  | Bounded of t Record.OrderedTypes.record
  | Unbounded of t

and typed_dictionary_field = {
  name: string;
  annotation: t;
}

and t =
  | Annotated of t
  | Bottom
  | Callable of t Record.Callable.record
  | Any
  | Literal of literal
  | Optional of t
  | Parametric of {
      name: Identifier.t;
      parameters: t Record.OrderedTypes.record;
    }
  | ParameterVariadicComponent of
      Record.Variable.RecordVariadic.RecordParameters.RecordComponents.t
  | Primitive of Primitive.t
  | Top
  | Tuple of tuple
  | TypedDictionary of {
      name: Identifier.t;
      fields: typed_dictionary_field list;
      total: bool;
    }
  | Union of t list
  | Variable of t Record.Variable.RecordUnary.record
[@@deriving compare, eq, sexp, show, hash]

type type_t = t [@@deriving compare, eq, sexp, show]

module Map : Map.S with type Key.t = t

val default_to_bottom : t Map.t -> t list -> t Map.t

module Set : Set.S with type Elt.t = t

include Hashable with type t := t

module Cache : sig
  val enable : unit -> unit

  val disable : unit -> unit
end

val pp_concise : Format.formatter -> t -> unit

val show_concise : t -> string

val show_for_hover : t -> string

val serialize : t -> string

val parametric : string -> t Record.OrderedTypes.record -> t

val annotated : t -> t

val awaitable : t -> t

val coroutine : t Record.OrderedTypes.record -> t

val bool : t

val bytes : t

val complex : t

val dictionary : key:t -> value:t -> t

val enumeration : t

val float : t

val generator : ?async:bool -> t -> t

val generic_primitive : t

val integer : t

val literal_integer : int -> t

val iterable : t -> t

val iterator : t -> t

val async_iterator : t -> t

val lambda : parameters:(Identifier.t * t) list -> return_annotation:t -> t

val list : t -> t

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

val tuple : t list -> t

val undeclared : t

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

val is_unknown : t -> bool

val is_undeclared : t -> bool

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
  end

  include module type of struct
    include Record.Callable
  end

  type implicit = type_t Record.Callable.implicit_record [@@deriving compare, eq, sexp, show, hash]

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

  val with_return_annotation : t -> annotation:type_t -> t

  val create
    :  ?name:Reference.t ->
    ?overloads:type_t overload list ->
    ?parameters:parameters ->
    ?implicit:implicit ->
    annotation:type_t ->
    unit ->
    type_t

  val create_from_implementation : type_t overload -> type_t
end

type alias =
  | TypeAlias of t
  | VariableAlias of t Record.Variable.record
[@@deriving compare, eq, sexp, show, hash]

val create : aliases:(Primitive.t -> alias option) -> Expression.t -> t

val contains_callable : t -> bool

val is_any : t -> bool

val is_async_iterator : t -> bool

val is_callable : t -> bool

val is_dictionary : ?with_key:t option -> t -> bool

val is_ellipsis : t -> bool

val is_final : t -> bool

val is_generator : t -> bool

val is_generic_primitive : t -> bool

val is_iterable : t -> bool

val is_iterator : t -> bool

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

val is_typed_dictionary : t -> bool

val is_unbound : t -> bool

val is_union : t -> bool

val contains_any : t -> bool

val contains_unknown : t -> bool

val expression_contains_any : Expression.t -> bool

(* Contains `Bottom` or variables. *)
val is_not_instantiated : t -> bool

val contains_literal : t -> bool

val contains_final : t -> bool

val primitive_name : t -> Identifier.t option

val primitives : t -> t list

val elements : t -> Primitive.t list

val is_partially_typed : t -> bool

val is_untyped : t -> bool

val contains_variable : t -> bool

val optional_value : t -> t

val async_generator_value : t -> t

val awaitable_value : t -> t

val coroutine_value : t -> t

val parameters : t -> t Record.OrderedTypes.record option

val single_parameter : t -> t

val instantiate
  :  ?widen:bool ->
  ?visit_children_before:bool ->
  t ->
  constraints:(t -> t option) ->
  t

val weaken_literals : t -> t

module OrderedTypes : sig
  include module type of struct
    include Record.OrderedTypes
  end

  type t = type_t record [@@deriving compare, eq, sexp, show, hash]

  type ordered_types_t = t

  val pp_concise : Format.formatter -> t -> unit

  module Concatenation : sig
    include module type of struct
      include Record.OrderedTypes.RecordConcatenate
    end

    module Middle : sig
      include module type of struct
        include Record.OrderedTypes.RecordConcatenate.Middle
      end

      val unwrap_if_bare
        :  type_t t ->
        type_t Record.Variable.RecordVariadic.RecordList.record option

      val create_bare : type_t Record.Variable.RecordVariadic.RecordList.record -> type_t t

      val create
        :  variable:type_t Record.Variable.RecordVariadic.RecordList.record ->
        mappers:Identifier.t list ->
        type_t t

      val singleton_replace_variable : type_t t -> replacement:type_t -> type_t
    end

    val map_head_and_tail
      :  ('middle, 'outer_a) t ->
      f:('outer_a -> 'outer_b) ->
      ('middle, 'outer_b) t

    val map_middle : ('middle_a, 'outer) t -> f:('middle_a -> 'middle_b) -> ('middle_b, 'outer) t

    val replace_variable
      :  (type_t Middle.t, type_t) t ->
      replacement:
        (type_t Record.Variable.RecordVariadic.RecordList.record -> ordered_types_t option) ->
      ordered_types_t option

    val head : ('middle, 'outer) t -> 'outer list

    val middle : ('middle, 'outer) t -> 'middle

    val tail : ('middle, 'outer) t -> 'outer list

    val unwrap_if_only_middle : ('middle, 'outer) t -> 'middle option

    val variable
      :  (type_t Middle.t, 'outer) t ->
      type_t Record.Variable.RecordVariadic.RecordList.record

    val expression : (type_t Middle.t, type_t) t -> Expression.t

    val parse
      :  Expression.t ->
      aliases:(Primitive.t -> alias option) ->
      (type_t Middle.t, type_t) t option

    val create : ?head:'outer list -> ?tail:'outer list -> 'middle -> ('middle, 'outer) t

    val zip : ('middle, 'outer) t -> against:'a list -> ('middle * 'a list, 'outer * 'a) t option
  end

  val union_upper_bound : t -> type_t

  (* Concatenation is only defined for certain members *)
  val concatenate : left:t -> right:t -> t option
end

val split : t -> t * OrderedTypes.t

val class_name : t -> Reference.t

val class_variable : t -> t

val class_variable_value : t -> t option

val final_value : t -> t option

val assume_any : t -> t

(* Takes a map generated from Preprocessing.dequalify_map and a type and dequalifies the type *)
val dequalify : Reference.t Reference.Map.t -> t -> t

val dequalify_identifier : Reference.t Reference.Map.t -> Identifier.t -> Identifier.t

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

  type unary_domain = type_t

  type parameter_variadic_t = type_t Record.Variable.RecordVariadic.RecordParameters.record
  [@@deriving compare, eq, sexp, show, hash]

  type parameter_variadic_domain = Callable.parameters

  type list_variadic_t = type_t Record.Variable.RecordVariadic.RecordList.record
  [@@deriving compare, eq, sexp, show, hash]

  type list_variadic_domain = OrderedTypes.t

  type pair =
    | UnaryPair of unary_t * unary_domain
    | ParameterVariadicPair of parameter_variadic_t * parameter_variadic_domain
    | ListVariadicPair of list_variadic_t * list_variadic_domain

  type t = type_t Record.Variable.record [@@deriving compare, eq, sexp, show, hash]

  type variable_t = t

  module type VariableKind = sig
    type t [@@deriving compare, eq, sexp, show, hash]

    module Map : Core.Map.S with type Key.t = t

    val is_free : t -> bool

    val is_escaped_and_free : t -> bool

    val mark_as_bound : t -> t

    val mark_as_escaped : t -> t

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
        :  variable_parameter_annotation:Expression.t ->
        keywords_parameter_annotation:Expression.t ->
        aliases:(Primitive.t -> alias option) ->
        t option

      module Components : sig
        include module type of struct
          include Record.Variable.RecordVariadic.RecordParameters.RecordComponents
        end

        type decomposition = {
          positional_component: type_t;
          keyword_component: type_t;
        }

        val combine : decomposition -> parameter_variadic_t option
      end

      val decompose : t -> Components.decomposition
    end

    module List : sig
      include VariableKind with type t = list_variadic_t and type domain = list_variadic_domain

      val name : t -> Identifier.t

      val create
        :  ?constraints:type_t Record.Variable.constraints ->
        ?variance:Record.Variable.variance ->
        string ->
        t
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

    module ListVariadic : S with type t = list_variadic_t and type domain = list_variadic_domain
  end

  include module type of struct
    include Record.Variable
  end

  module Set : Core.Set.S with type Elt.t = t

  val pp_concise : Format.formatter -> t -> unit

  val parse_declaration : Expression.t -> t option

  val dequalify : Reference.t Reference.Map.t -> t -> t

  val namespace : t -> namespace:Namespace.t -> t

  val mark_all_variables_as_bound : ?specific:t list -> type_t -> type_t

  val namespace_all_free_variables : type_t -> namespace:Namespace.t -> type_t

  val all_free_variables : type_t -> t list

  val all_variables_are_resolved : type_t -> bool

  val mark_all_free_variables_as_escaped : ?specific:t list -> type_t -> type_t

  val collapse_all_escaped_variable_unions : type_t -> type_t

  val contains_escaped_free_variable : type_t -> bool

  val convert_all_escaped_free_variables_to_anys : type_t -> type_t
end

val namespace_insensitive_compare : t -> t -> int

val variable
  :  ?constraints:type_t Variable.constraints ->
  ?variance:Variable.variance ->
  string ->
  t

val is_concrete : t -> bool

module TypedDictionary : sig
  val anonymous : total:bool -> typed_dictionary_field list -> t

  val fields_have_colliding_keys
    :  typed_dictionary_field list ->
    typed_dictionary_field list ->
    bool

  val constructor
    :  name:Identifier.t ->
    fields:typed_dictionary_field list ->
    total:bool ->
    Callable.t

  val special_overloads
    :  fields:typed_dictionary_field list ->
    method_name:string ->
    total:bool ->
    t Callable.overload list option

  val is_special_mismatch : method_name:string -> position:int -> total:bool -> bool

  val defines : t_self_expression:Expression.t -> total:bool -> Statement.t list
end

val remove_undeclared : t -> t

val to_yojson : t -> Yojson.Safe.json
