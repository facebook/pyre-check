(** Copyright (c) 2018-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Ast
open Expression


type real_target = [ `RealTarget of Access.t ]
[@@deriving show, compare]

type override_target = [ `OverrideTarget of Access.t ]
[@@deriving show, compare]

type t = [ real_target | override_target ]
[@@deriving show, compare]

type target_with_stored_result = real_target


module Key = struct
  type nonrec t = t
  let to_string = show
  let compare = compare
end


let get_definition (callable : real_target) =
  None
