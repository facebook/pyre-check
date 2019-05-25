(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

module type S = sig
  include Map.S

  val set : 'a t -> key:key -> data:'a -> 'a t
end

module Make (Ordered : Map.OrderedType) : S with type key = Ordered.t = struct
  include Map.Make (Ordered)

  let set map ~key ~data = add key data map
end
