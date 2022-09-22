(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Expression
module Error = AnalysisError

module ReadOnlyness = struct
  type t =
    | Mutable
    | ReadOnly
  [@@deriving compare, sexp, show]

  let name = "ReadOnlyness"

  let bottom = Mutable

  let less_or_equal ~left ~right =
    match left, right with
    | _, ReadOnly -> true
    | _ -> [%compare.equal: t] left right


  let join left right =
    match left, right with
    | ReadOnly, _
    | _, ReadOnly ->
        ReadOnly
    | _ -> Mutable


  let meet left right =
    match left, right with
    | Mutable, _
    | _, Mutable ->
        Mutable
    | _ -> ReadOnly
end

module Resolution = struct
  include
    Abstract.MapDomain.Make
      (struct
        include Reference

        let name = "Reference"

        let absence_implicitly_maps_to_bottom = false
      end)
      (Abstract.SimpleDomain.Make (ReadOnlyness))
end

module Resolved = struct
  type t = {
    resolution: Resolution.t;
    resolved: ReadOnlyness.t;
    errors: Error.t list;
  }
  [@@deriving show]
end

module State = struct
  let forward_expression ~resolution { Node.value; _ } =
    let open ReadOnlyness in
    match value with
    | Expression.Constant _ ->
        { Resolved.resolution; errors = []; resolved = ReadOnlyness.ReadOnly }
    | Expression.Name (Name.Identifier identifier) ->
        {
          Resolved.resolution;
          errors = [];
          resolved =
            Resolution.get_opt (Reference.create identifier) resolution
            |> Option.value ~default:Mutable;
        }
    | _ -> failwith "TODO(T130377746)"
end
