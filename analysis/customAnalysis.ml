(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Statement

module NestedDefines = struct
  type 'state nested = {
    nested_define: Define.t;
    state: 'state;
  }

  and 'state t = 'state nested Location.Reference.Map.t

  let initial = Location.Reference.Map.empty

  let update_nested_defines nested_defines ~statement ~state =
    match statement with
    | { Node.location; value = Define nested_define } ->
        Map.set nested_defines ~key:location ~data:{ nested_define; state }
    | _ -> nested_defines
end

let nested_defines_deep_to_shallow define =
  let shallow_nested_defines { Node.value = { Statement.Define.body; _ }; _ } =
    let find_nested = function
      | { Node.value = Define define; location } -> Some (Node.create ~location define)
      | _ -> None
    in
    List.filter_map ~f:find_nested body
  in
  let rec ordered_defines define =
    let nested = shallow_nested_defines define |> List.map ~f:ordered_defines |> List.concat in
    nested @ [define]
  in
  shallow_nested_defines define |> List.map ~f:ordered_defines |> List.concat
