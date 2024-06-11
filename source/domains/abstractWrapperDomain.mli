(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type INNER = sig
  include AbstractDomainCore.S

  val name : string
end

(* Wraps the given abstract domain into a new abstract domain, with its own `Self` abstract part.
   This is useful when using the same domain multiple times in a complex abstract domain type. *)
module Make (Inner : INNER) : sig
  include AbstractDomainCore.S with type t = Inner.t
end
