(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Statement
module PyrePysaEnvironment = Analysis.PyrePysaEnvironment

type kind =
  | Normal
  | PropertySetter
  | Decorated
[@@deriving show, sexp, compare, hash, eq]

type function_name = {
  name: string;
  kind: kind;
}
[@@deriving show, sexp, compare, hash, eq]

type method_name = {
  class_name: string;
  method_name: string;
  kind: kind;
}
[@@deriving show, sexp, compare, hash, eq]

module Regular : sig
  type t =
    | Function of function_name
    | Method of method_name
    | Override of method_name
    (* Represents a global variable or field of a class that we want to model, * e.g os.environ or
       HttpRequest.GET *)
    | Object of string
  [@@deriving sexp, compare, hash, eq, show]

  val override_to_method : t -> t

  val define_name_exn : t -> Reference.t

  val create_derived_override_exn : at_type:Reference.t -> t -> t

  val get_corresponding_method_exn : t -> t

  val get_corresponding_override_exn : t -> t

  val kind : t -> kind option

  val set_kind : kind -> t -> t
end

module ParameterMap : Data_structures.SerializableMap.S with type key = TaintAccessPath.Root.t

type t =
  | Regular of Regular.t
  | Parameterized of {
      regular: Regular.t;
      parameters: t ParameterMap.t;
    }
    (* This represents a regular callable with its function-typed parameters being instantited with
       `parameters`. *)
[@@deriving sexp, compare, hash, eq]

module T : sig
  type nonrec t = t [@@deriving sexp, compare, hash, eq]
end

module Map : sig
  include Data_structures.SerializableMap.S with type key = t

  module Tree : module type of struct
    include Core.Map.Make_tree (struct
      include T
      include Core.Comparator.Make (T)
    end)
  end
end

(* Pretty printers. *)

val pp_internal : Format.formatter -> t -> unit

val show_internal : t -> string

val pp_pretty : Format.formatter -> t -> unit

val show_pretty : t -> string

val pp_pretty_with_kind : Format.formatter -> t -> unit

val show_pretty_with_kind : t -> string

val pp_external : Format.formatter -> t -> unit

val external_name : t -> string

(* Equivalent to pp_internal. Required by @@deriving. *)
val pp : Format.formatter -> t -> unit

(* Constructors. *)

val create_function : ?kind:kind -> Reference.t -> t

val create_method : ?kind:kind -> Reference.t -> t

val create_property_setter : Reference.t -> t

val create_override : ?kind:kind -> Reference.t -> t

val create_property_setter_override : Reference.t -> t

val create_object : Reference.t -> t

val create : Ast.Reference.t -> Define.t -> t

val from_regular : Regular.t -> t

(* Return `Regular.t` when called on any `t`. *)
val get_regular : t -> Regular.t

(* Return `Regular.t`, but throw if called on `Parameterized`. *)
val as_regular_exn : t -> Regular.t

val strip_parameters : t -> t

val get_corresponding_method_exn : must_be_regular:bool -> t -> t

(* Accessors. *)

val class_name : t -> string option

val method_name : t -> string option

val function_name : t -> string option

val object_name : t -> Reference.t

val is_function_or_method : t -> bool

val is_method_or_override : t -> bool

val is_method : t -> bool

val is_function : t -> bool

val is_override : t -> bool

val is_object : t -> bool

val is_normal : t -> bool

val is_decorated : t -> bool

val is_parameterized : t -> bool

val is_regular : t -> bool

(* A parameterized target contains recursive targets if one of its `regular` part also appears in
   one of its `parameters` part. Such recursion may lead to non-termination in high-order call graph
   building. *)
val contain_recursive_target : t -> bool

(* Return the level of target nestedness within a given target. *)
val depth : t -> int

val for_issue_handle : t -> t

(** Return the define name of a Function or Method target. Note that multiple targets can match to
    the same define name (e.g, property getters and setters). Hence, use this at your own risk. *)
val define_name_exn : t -> Reference.t

val set_kind : kind -> t -> t

module Set : sig
  include Stdlib.Set.S with type elt = t

  val pp_pretty_with_kind : Format.formatter -> t -> unit

  val show_pretty_with_kind : t -> string
end

module HashMap : Core.Hashtbl.S with type key := t

module HashSet : Core.Hash_set.S with type elt := t

module List : sig
  type t = T.t list

  val pp_pretty_with_kind : Format.formatter -> t -> unit

  val show_pretty_with_kind : t -> string
end

type definitions_result = {
  qualifier: Reference.t;
  (* Mapping from a target to its selected definition. *)
  callables: Define.t Node.t Map.t;
}

(** This is the source of truth for the mapping of callables to definitions. All parts of the
    analysis should use this (or `get_module_and_definition`) rather than walking over source files. *)
val get_definitions
  :  pyre_api:PyrePysaEnvironment.ReadOnly.t ->
  warn_multiple_definitions:bool ->
  Reference.t ->
  definitions_result option

(* Exposed for testing purposes only. *)
val get_module_and_definition_for_test
  :  pyre_api:PyrePysaEnvironment.ReadOnly.t ->
  t ->
  (Reference.t * Define.t Node.t) option

val resolve_method
  :  pyre_api:PyrePysaEnvironment.ReadOnly.t ->
  class_type:Type.t ->
  method_name:string ->
  t option

(* Define the meaning of `skip_analysis_targets`. We assume `skip_analysis_targets` only contains
   regular callables. *)
val should_skip_analysis : skip_analysis_targets:HashSet.t -> t -> bool

module ArtificialTargets : sig
  val format_string : t

  val str_add : t

  val str_mod : t

  val str_format : t

  val str_literal : t

  val condition : t
end

module SharedMemoryKey : sig
  type nonrec t = t

  val compare : t -> t -> int

  val to_string : t -> string

  val from_string : string -> t
end

(* Represent a hashset of targets inside the shared memory *)
module HashsetSharedMemory : sig
  type target = t

  type t

  val cleanup : t -> unit

  val from_heap : target list -> t

  module ReadOnly : sig
    type t

    val mem : t -> target -> bool
  end

  val read_only : t -> ReadOnly.t
end

(** Whether a method is an instance method, or a class method, or a static method. *)
module MethodKind : sig
  type t =
    | Static
    | Class
    | Instance
end

module CallablesSharedMemory : sig
  type target = t

  type t

  module Signature : sig
    type t = {
      qualifier: Reference.t;
      location: Location.t;
      define_name: Reference.t;
      parameters: Expression.Parameter.t list;
      return_annotation: Expression.t option;
      decorators: Expression.t list;
      captures: Define.Capture.t list;
      method_kind: MethodKind.t option;
      is_stub: bool;
    }
  end

  module DefineAndQualifier : sig
    type t = {
      qualifier: Reference.t;
      define: Define.t Node.t;
    }
  end

  module ReadOnly : sig
    type t

    val get_define : t -> target -> DefineAndQualifier.t option

    val get_location : t -> target -> Ast.Location.WithModule.t option

    val get_signature : t -> target -> Signature.t option

    val get_qualifier : t -> target -> Reference.t option

    val get_method_kind : t -> target -> bool * bool

    val is_stub : t -> target -> bool option

    val get_captures : t -> target -> Define.Capture.t list option

    val mem : t -> target -> bool
  end

  val empty : unit -> t

  val from_callables
    :  scheduler:Scheduler.t ->
    scheduler_policy:Scheduler.Policy.t ->
    pyre_api:PyrePysaEnvironment.ReadOnly.t ->
    target list ->
    t

  val read_only : t -> ReadOnly.t

  val cleanup : t -> unit

  val add_alist_sequential : t -> (target * DefineAndQualifier.t) list -> t
end

val class_method_decorators : string list

val static_method_decorators : string list
