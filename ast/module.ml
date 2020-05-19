(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Statement

type aliased_exports = Reference.t Reference.Map.Tree.t [@@deriving eq, sexp]

let compare_aliased_exports = Reference.Map.Tree.compare_direct Reference.compare

type t =
  | Explicit of {
      aliased_exports: aliased_exports;
      empty_stub: bool;
      local_mode: Source.local_mode Node.t option;
    }
  | Implicit of { empty_stub: bool }
[@@deriving eq, sexp, compare]

let pp format printed_module =
  let aliased_exports, empty_stub =
    match printed_module with
    | Explicit { aliased_exports; empty_stub; _ } -> Map.Tree.to_alist aliased_exports, empty_stub
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


let create_for_testing ~local_mode ~stub =
  Explicit
    {
      aliased_exports = Reference.Map.empty |> Map.to_tree;
      empty_stub = stub && Source.Metadata.is_placeholder_stub local_mode;
      local_mode = None;
    }


let create
    {
      Source.source_path = { SourcePath.is_stub; qualifier; _ } as source_path;
      statements;
      metadata = { Source.Metadata.local_mode; _ };
      _;
    }
  =
  let aliased_exports =
    let aliased_exports aliases { Node.value; _ } =
      match value with
      | Statement.Import { Import.from = Some from; imports } ->
          let from = SourcePath.expand_relative_import source_path ~from in
          let export aliases { Import.name = { Node.value = name; _ }; alias } =
            let alias =
              match alias with
              | None -> name
              | Some { Node.value = alias; _ } -> Reference.create alias
            in
            let name =
              if String.equal (Reference.show alias) "*" then
                Node.value from
              else
                Reference.combine (Node.value from) name
            in
            Map.set aliases ~key:alias ~data:name
          in
          List.fold imports ~f:export ~init:aliases
      | Import { Import.from = None; imports } ->
          let export aliases { Import.name = { Node.value = name; _ }; alias } =
            let alias =
              match alias with
              | None -> name
              | Some { Node.value = alias; _ } -> Reference.create alias
            in
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
      empty_stub = is_stub && Source.Metadata.is_placeholder_stub local_mode;
      local_mode;
    }


let create_implicit ?(empty_stub = false) () = Implicit { empty_stub }

let aliased_export considered_module reference =
  match considered_module with
  | Explicit { aliased_exports; _ } ->
      let aliased_exports = Reference.Map.of_tree aliased_exports in
      Map.find aliased_exports reference
  | Implicit _ -> None


let local_mode = function
  | Explicit { local_mode; _ } -> local_mode
  | Implicit _ -> None
