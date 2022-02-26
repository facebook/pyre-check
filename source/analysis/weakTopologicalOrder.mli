(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

module Component : sig
  type t = {
    id: int;
    kind: kind;
  }
  [@@deriving compare, sexp]

  and kind =
    | Node of Cfg.Node.t
    | Cycle of {
        head: Cfg.Node.t;
        components: t list;
      }
  [@@deriving compare, sexp]
end

(* Weak topological order of a control flow graph. *)
type t = Component.t list [@@deriving compare, sexp]

val create : cfg:Cfg.t -> entry_index:int -> successors:(Cfg.Node.t -> Int.Set.t) -> t
