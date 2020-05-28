(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

module Export : sig
  type t =
    | NameAlias of {
        from: Reference.t;
        name: Identifier.t;
      }
    | Module of Reference.t
    | Class
    | Define
    | GlobalVariable
  [@@deriving sexp, compare, hash]
end

type t [@@deriving eq, sexp, show, compare]

val empty_stub : t -> bool

val legacy_aliased_export : t -> Reference.t -> Reference.t option

val create : Source.t -> t

val create_implicit : ?empty_stub:bool -> unit -> t

val get_export : t -> Identifier.t -> Export.t option

val get_all_exports : t -> (Identifier.t * Export.t) list

val is_implicit : t -> bool

(* Exposed for testing only *)
val create_for_testing : local_mode:Source.local_mode Node.t option -> stub:bool -> t

val local_mode : t -> Source.local_mode Node.t option
