(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre

module Access = AstExpression.Access
module Node = AstNode
module Statement = AstStatement


type t = {
  exports: (Access.t * Access.t) list
}
[@@deriving compare, eq, sexp]


let pp format { exports } =
  List.map exports
    ~f:(fun (source, target) -> Format.asprintf "%a -> %a" Access.pp source Access.pp target)
  |> String.concat ~sep:", "
  |> Format.fprintf format "[%s]"


let show =
  Format.asprintf "%a" pp


let create statements =
  let exports =
    let exports { Node.value; _ } =
      let open Statement in
      match value with
      | Import { Import.from = Some from; imports } ->
          let export { Import.name; alias } =
            let alias = Option.value ~default:name alias in
            alias, from @ name
          in
          List.map ~f:export imports
      | _ ->
          []
    in
    List.concat_map ~f:exports statements
  in
  { exports }


let export { exports } source =
  Access.Map.of_alist exports
  |> (function
      | `Ok exports -> Some exports
      | _ -> None)
  >>= (fun exports -> Map.find exports source)
