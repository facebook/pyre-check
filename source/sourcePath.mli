(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type t [@@deriving sexp, compare, hash, show]

val create : PyrePath.t -> t

val raw : t -> PyrePath.t

module Event : sig
  module Kind : sig
    type t =
      | CreatedOrChanged
      | Deleted
    [@@deriving sexp, compare]
  end

  type source_path = t

  type t = private {
    kind: Kind.t;
    path: source_path;
  }
  [@@deriving sexp, compare]

  val create : kind:Kind.t -> source_path -> t
end
