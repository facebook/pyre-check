(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre
open Statement


type t = {
  aliased_exports: (Access.t * Access.t) list;
  empty_stub: bool;
  handle: File.Handle.t option;
  wildcard_exports: Access.t list;
}
[@@deriving compare, eq, sexp]


let pp format { aliased_exports; empty_stub; handle; wildcard_exports } =
  let aliased_exports =
    List.map aliased_exports
      ~f:(fun (source, target) -> Format.asprintf "%a -> %a" Access.pp source Access.pp target)
    |> String.concat ~sep:", "
  in
  let wildcard_exports =
    List.map wildcard_exports ~f:(Format.asprintf "%a" Access.pp)
    |> String.concat ~sep:", "
  in
  Format.fprintf format
    "%s: [%s, empty_stub = %b, __all__ = [%s]]"
    (Option.value ~default:"unknown path" (handle >>| File.Handle.show))
    aliased_exports
    empty_stub
    wildcard_exports


let show =
  Format.asprintf "%a" pp


let empty_stub { empty_stub; _ } =
  empty_stub


let from_empty_stub ~access ~module_definition =
  let rec is_empty_stub ~lead ~tail =
    match tail with
    | head :: tail ->
        begin
          let lead = lead @ [head] in
          match module_definition lead with
          | Some definition when empty_stub definition -> true
          | Some _ -> is_empty_stub ~lead ~tail
          | _ -> false
        end
    | _ ->
        false
  in
  is_empty_stub ~lead:[] ~tail:access


let handle { handle; _ } =
  handle


let wildcard_exports { wildcard_exports; _ } =
  wildcard_exports


let create ~qualifier ~local_mode ?handle ~stub statements =
  let aliased_exports =
    let aliased_exports { Node.value; _ } =
      match value with
      | Import { Import.from = Some from; imports } ->
          let from =
            Source.expand_relative_import
              ?handle
              ~qualifier
              ~from
          in
          let export { Import.name; alias } =
            let alias = Option.value ~default:name alias in
            let name = if Access.show alias = "*" then from else from @ name in
            (* The problem this bit solves is that we may generate an alias prefix <- prefix.rest
               after qualification, which would cause an infinite loop when folding over
               prefix.attribute. To avoid this, drop the prefix whenever we see that the
               qualified alias would cause a loop. *)
            if Access.is_strict_prefix ~prefix:(qualifier @ alias) name then
              alias, Access.drop_prefix ~prefix:qualifier name
            else
              alias, name
          in
          List.map imports ~f:export
      | Import { Import.from = None; imports } ->
          let export { Import.name; alias } =
            let alias = Option.value ~default:name alias in
            if Access.is_strict_prefix ~prefix:(qualifier @ alias) name then
              alias, Access.drop_prefix ~prefix:qualifier name
            else
              alias, name
          in
          List.map imports ~f:export
      | _ ->
          []
    in
    let filter_duplicates (sofar, visited) (alias, target) =
      if Set.mem visited alias then
        (sofar, visited)
      else
        (alias, target) :: sofar, Set.add visited alias
    in
    List.concat_map ~f:aliased_exports statements
    |> List.fold ~init:([], Access.Set.empty) ~f:filter_duplicates
    |> fst
  in
  let toplevel_public, dunder_all =
    let gather_toplevel (public_values, dunder_all) { Node.value; _ } =
      let filter_private =
        let is_public name =
          let dequalified =
            Access.drop_prefix ~prefix:qualifier name
            |> Access.delocalize_qualified
          in
          if not (String.is_prefix ~prefix:"_" (Access.show dequalified)) then
            Some dequalified
          else
            None
        in
        List.filter_map ~f:is_public
      in
      match value with
      | Assign {
          Assign.target = { Node.value = Expression.Access target; _ };
          value = { Node.value = (Expression.List names); _ };
          _;
        }
        when Access.equal (Access.sanitized target) (Access.create "__all__") ->
          let to_access = function
            | { Node.value = Expression.String { value = name; _ }; _ } ->
                Access.create name
                |> List.last
                >>| fun access -> [access]
            | _ -> None
          in
          public_values, Some (List.filter_map ~f:to_access names)
      | Assign { Assign.target = { Node.value = Expression.Access target; _ }; _ } ->
          public_values @ (filter_private [target]), dunder_all
      | Class { Record.Class.name; _ } ->
          public_values @ (filter_private [name]), dunder_all
      | Define { Define.name; _ } ->
          public_values @ (filter_private [name]), dunder_all
      | Import { Import.imports; _ } ->
          let get_import_name { Import.alias; name } = Option.value alias ~default:name in
          public_values @ (filter_private (List.map imports ~f:get_import_name)), dunder_all
      | _ ->
          public_values, dunder_all
    in
    List.fold ~f:gather_toplevel ~init:([], None) statements
  in
  {
    aliased_exports;
    empty_stub = stub && Source.equal_mode local_mode Source.PlaceholderStub;
    handle;
    wildcard_exports = (Option.value dunder_all ~default:toplevel_public);
  }


let aliased_export { aliased_exports; _ } access =
  Access.Map.of_alist aliased_exports
  |> (function
      | `Ok exports -> Some exports
      | _ -> None)
  >>= (fun exports -> Map.find exports access)


let in_wildcard_exports { wildcard_exports; _ } access =
  List.exists ~f:(Expression.Access.equal access) wildcard_exports
