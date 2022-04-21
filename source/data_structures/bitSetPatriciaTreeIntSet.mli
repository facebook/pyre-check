(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module type CONFIG = sig
  val common_integers : int array
end

module Make (Config : CONFIG) : sig
  include PatriciaTreeSet.SET with type element = int
end
