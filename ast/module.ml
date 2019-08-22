(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Expression
open Statement

type aliased_exports = Reference.t Reference.Map.Tree.t [@@deriving eq, sexp]

let compare_aliased_exports = Reference.Map.Tree.compare_direct Reference.compare

type t =
  | Explicit of {
      aliased_exports: aliased_exports;
      empty_stub: bool;
    }
  | Implicit of { empty_stub: bool }
[@@deriving eq, sexp, compare]

(* We cache results of `from_empty_stub` here since module definition lookup requires shared memory
   lookup, which can be expensive *)
module Cache = struct
  let cache = Reference.Table.create ()

  let clear () = Reference.Table.clear cache
end

let pp format printed_module =
  let aliased_exports, empty_stub =
    match printed_module with
    | Explicit { aliased_exports; empty_stub } -> Map.Tree.to_alist aliased_exports, empty_stub
    | Implicit { empty_stub } -> [], empty_stub
  in
  let aliased_exports =
    aliased_exports
    |> List.map ~f:(fun (source, target) ->
           Format.asprintf "%a -> %a" Reference.pp source Reference.pp target)
    |> String.concat ~sep:", "
  in
  Format.fprintf format "MODULE[%s, empty_stub = %b]" aliased_exports empty_stub


let show = Format.asprintf "%a" pp

let empty_stub = function
  | Implicit { empty_stub }
  | Explicit { empty_stub; _ } ->
      empty_stub


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


let create_for_testing ~local_mode ~stub =
  Explicit
    {
      aliased_exports = Reference.Map.empty |> Map.to_tree;
      empty_stub = stub && Source.equal_mode local_mode Source.PlaceholderStub;
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
  Explicit
    {
      aliased_exports;
      empty_stub = is_stub && Source.equal_mode local_mode Source.PlaceholderStub;
    }


let create_implicit ?(empty_stub = false) () = Implicit { empty_stub }

let aliased_export considered_module reference =
  match considered_module with
  | Explicit { aliased_exports; _ } ->
      let aliased_exports = Reference.Map.of_tree aliased_exports in
      Map.find aliased_exports reference
  | Implicit _ -> None
