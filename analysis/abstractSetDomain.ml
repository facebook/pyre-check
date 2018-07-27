(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core


module Make(Element : Set.Elt) = struct
  module Set = struct
    module ElementSet = Set.Make(Element)
    include ElementSet.Tree
  end

  include Set


  let bottom =
    Set.empty


  let is_bottom =
    Set.is_empty


  let join left right =
    Set.union left right


  let widen ~iteration:_ ~previous ~next =
    join previous next


  let less_or_equal ~left ~right =
    Set.is_subset left ~of_:right


  let show set =
    Sexp.to_string [%message (set: Set.t)]
end
