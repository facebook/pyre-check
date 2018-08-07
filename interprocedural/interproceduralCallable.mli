(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

(* A generalization of what can be a call target during the analysis.
   - RealTarget refers to an actual definition.
   - OverrideTarget is a set of target represented by an override tree.
*)

open Ast
open Statement


type real_target = [ `RealTarget of Access.t ]
[@@deriving show, sexp, compare]

type override_target = [ `OverrideTarget of Access.t ]
[@@deriving show, sexp, compare]

type t = [ real_target | override_target ]
[@@deriving show, sexp, compare]

val show: [< t ] -> string
val compare: ([< t ] as 'a) -> 'a -> int

type target_with_stored_result = real_target

val make_real: Access.t -> [> real_target ]
val make_override: Access.t -> [> override_target ]
val make: Define.t Node.t -> [> real_target ]

val get_real_access: [< real_target ] -> Access.t
val get_override_access: [< override_target ] -> Access.t

module Key : sig
  type nonrec t = t
  val to_string: t -> string
  val compare: t -> t -> int
end

module Set : Caml.Set.S with type elt = t

(* Shared heap access to top-level definitions. *)
val add_definition: real_target -> File.Handle.t -> unit
val get_definition: [< real_target ] -> Define.t Node.t option
