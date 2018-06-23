(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core


module Make(Element : Set.Elt) = struct
  module ElementSet = Set.Make(Element)
  include ElementSet

  let bottom = ElementSet.empty
  let is_bottom = ElementSet.is_empty

  let join x y =
    union x y

  let widen ~iteration:_ ~previous ~next =
    join previous next

  let less_or_equal ~left ~right =
    is_subset left ~of_:right

  let show a =
    ElementSet.sexp_of_t a |> Sexp.to_string
end
