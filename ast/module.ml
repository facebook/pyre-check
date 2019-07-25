(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
open Expression
open Statement

type t = {
  aliased_exports: Reference.t Reference.Map.Tree.t;
  empty_stub: bool;
  wildcard_exports: Reference.t list;
}
[@@deriving eq, sexp]

(* We cache results of `from_empty_stub` here since module definition lookup requires shared memory
   lookup, which can be expensive *)
module Cache = struct
  let cache = Reference.Table.create ()

  let clear () = Reference.Table.clear cache
end

let pp format { aliased_exports; empty_stub; wildcard_exports } =
  let aliased_exports =
    Map.Tree.to_alist aliased_exports
    |> List.map ~f:(fun (source, target) ->
           Format.asprintf "%a -> %a" Reference.pp source Reference.pp target)
    |> String.concat ~sep:", "
  in
  let wildcard_exports =
    List.map wildcard_exports ~f:(Format.asprintf "%a" Reference.pp) |> String.concat ~sep:", "
  in
  Format.fprintf
    format
    "MODULE[%s, empty_stub = %b, __all__ = [%s]]"
    aliased_exports
    empty_stub
    wildcard_exports


let show = Format.asprintf "%a" pp

let empty_stub { empty_stub; _ } = empty_stub

let from_empty_stub ~reference ~module_definition =
  let rec is_empty_stub ~lead ~tail =
    match tail with
    | head :: tail -> (
        let lead = lead @ [head] in
        let reference = Reference.create_from_list lead in
        match module_definition reference with
        | Some definition when empty_stub definition -> true
        | Some _ -> is_empty_stub ~lead ~tail
        | _ -> false )
    | _ -> false
  in
  Hashtbl.find_or_add Cache.cache reference ~default:(fun _ ->
      is_empty_stub ~lead:[] ~tail:(Reference.as_list reference))


let wildcard_exports { wildcard_exports; _ } = wildcard_exports

let create_for_testing ~local_mode ~stub =
  {
    aliased_exports = Reference.Map.empty |> Map.to_tree;
    empty_stub = stub && Source.equal_mode local_mode Source.PlaceholderStub;
    wildcard_exports = [];
  }


let create
    ( { Source.is_stub; qualifier; statements; metadata = { Source.Metadata.local_mode; _ }; _ } as
    source )
  =
  let aliased_exports =
    let aliased_exports aliases { Node.value; _ } =
      match value with
      | Assign
          {
            Assign.target = { Node.value = Name (Name.Identifier target); _ };
            value = { Node.value = Name value; _ };
            _;
          } -> (
        match Expression.name_to_reference value with
        | Some reference when Reference.is_strict_prefix ~prefix:qualifier reference ->
            Map.set aliases ~key:(Reference.sanitized (Reference.create target)) ~data:reference
        | _ -> aliases )
      | Import { Import.from = Some from; imports } ->
          let from = Source.expand_relative_import source ~from in
          let export aliases { Import.name; alias } =
            let alias = Option.value alias ~default:name in
            let name =
              if String.equal (Reference.show alias) "*" then from else Reference.combine from name
            in
            Map.set aliases ~key:alias ~data:name
          in
          List.fold imports ~f:export ~init:aliases
      | Import { Import.from = None; imports } ->
          let export aliases { Import.name; alias } =
            let alias = Option.value alias ~default:name in
            let source, target =
              if Reference.is_strict_prefix ~prefix:(Reference.combine qualifier alias) name then
                alias, Reference.drop_prefix ~prefix:qualifier name
              else
                alias, name
            in
            Map.set aliases ~key:source ~data:target
          in
          List.fold imports ~f:export ~init:aliases
      | _ -> aliases
    in
    List.fold statements ~f:aliased_exports ~init:Reference.Map.empty |> Map.to_tree
  in
  let toplevel_public, dunder_all =
    let gather_toplevel (public_values, dunder_all) { Node.value; _ } =
      let filter_private =
        let is_public name =
          let dequalified =
            Reference.drop_prefix ~prefix:qualifier name |> Reference.sanitize_qualified
          in
          if not (String.is_prefix ~prefix:"_" (Reference.show dequalified)) then
            Some dequalified
          else
            None
        in
        List.filter_map ~f:is_public
      in
      match value with
      | Assign
          {
            Assign.target = { Node.value = Name (Name.Identifier target); _ };
            value = { Node.value = Expression.List names; _ };
            _;
          }
        when String.equal (Identifier.sanitized target) "__all__" ->
          let to_reference = function
            | { Node.value = Expression.String { value = name; _ }; _ } ->
                Reference.create name
                |> Reference.last
                |> (fun last -> if String.is_empty last then None else Some last)
                >>| Reference.create
            | _ -> None
          in
          public_values, Some (List.filter_map ~f:to_reference names)
      | Assign { Assign.target = { Node.value = Name target; _ }; _ }
        when Expression.is_simple_name target ->
          public_values @ filter_private [target |> Expression.name_to_reference_exn], dunder_all
      | Class { Record.Class.name; _ } -> public_values @ filter_private [name], dunder_all
      | Define { Define.signature = { name; _ }; _ } ->
          public_values @ filter_private [name], dunder_all
      | Import { Import.imports; _ } ->
          let get_import_name { Import.alias; name } = Option.value alias ~default:name in
          public_values @ filter_private (List.map imports ~f:get_import_name), dunder_all
      | _ -> public_values, dunder_all
    in
    List.fold ~f:gather_toplevel ~init:([], None) statements
  in
  {
    aliased_exports;
    empty_stub = is_stub && Source.equal_mode local_mode Source.PlaceholderStub;
    wildcard_exports = Option.value dunder_all ~default:toplevel_public;
  }


let create_implicit ?(empty_stub = false) () =
  { aliased_exports = Reference.Map.empty |> Map.to_tree; empty_stub; wildcard_exports = [] }


let aliased_export { aliased_exports; _ } reference =
  let aliased_exports = Reference.Map.of_tree aliased_exports in
  Map.find aliased_exports reference
