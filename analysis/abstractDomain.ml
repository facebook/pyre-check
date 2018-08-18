(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


module type S = sig
  type t
  [@@deriving sexp]
  val bottom: t
  val is_bottom: t -> bool
  val join: t -> t -> t
  val less_or_equal: left:t -> right:t -> bool
  val show : t -> string
  val widen: iteration:int -> previous:t -> next:t -> t
end
