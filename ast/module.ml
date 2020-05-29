(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Statement

module Export = struct
  type t =
    | NameAlias of {
        from: Reference.t;
        name: Identifier.t;
      }
    | Module of Reference.t
    | Class
    | Define of { is_getattr_any: bool }
    | GlobalVariable
  [@@deriving sexp, compare, hash]
end

module ExportMap = struct
  type t = Export.t Identifier.Map.Tree.t [@@deriving sexp]

  let empty = Identifier.Map.Tree.empty

  let lookup = Identifier.Map.Tree.find

  let to_alist = Identifier.Map.Tree.to_alist

  let compare = Identifier.Map.Tree.compare_direct Export.compare

  let equal = [%compare.equal: t]
end

type legacy_aliased_exports = Reference.t Reference.Map.Tree.t [@@deriving eq, sexp]

let compare_legacy_aliased_exports = Reference.Map.Tree.compare_direct Reference.compare

type t =
  | Explicit of {
      exports: ExportMap.t;
      legacy_aliased_exports: legacy_aliased_exports;
      empty_stub: bool;
      local_mode: Source.local_mode Node.t option;
    }
  | Implicit of { empty_stub: bool }
[@@deriving eq, sexp, compare]

let pp format printed_module =
  let aliased_exports, empty_stub =
    match printed_module with
    | Explicit { legacy_aliased_exports; empty_stub; _ } ->
        Map.Tree.to_alist legacy_aliased_exports, empty_stub
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
      exports = ExportMap.empty;
      legacy_aliased_exports = Reference.Map.empty |> Map.to_tree;
      empty_stub = stub && Source.Metadata.is_placeholder_stub local_mode;
      local_mode = None;
    }


let create
    ( {
        Source.source_path = { SourcePath.is_stub; qualifier; _ } as source_path;
        statements;
        metadata = { Source.Metadata.local_mode; _ };
        _;
      } as source )
  =
  let exports =
    let open UnannotatedGlobal in
    let is_getattr_any
        {
          UnannotatedDefine.define =
            { Define.Signature.name = { Node.value = name; _ }; parameters; return_annotation; _ };
          _;
        }
      =
      match Reference.last name with
      | "__getattr__" -> (
          match parameters with
          | [{ Node.value = { Expression.Parameter.annotation = parameter_annotation; _ }; _ }] -> (
              match parameter_annotation, return_annotation with
              | ( (Some { Node.value = Name (Identifier "str"); _ } | None),
                  Some
                    {
                      Node.value =
                        Name
                          ( Identifier "Any"
                          | Attribute
                              {
                                base = { Node.value = Name (Identifier "typing"); _ };
                                attribute = "Any";
                                special = false;
                              } );
                      _;
                    } ) ->
                  true
              | _ -> false )
          | _ -> false )
      | _ -> false
    in
    let collect_export sofar { Collector.Result.name; unannotated_global } =
      match unannotated_global with
      | Imported ImportEntry.(Module { implicit_alias; _ } | Name { implicit_alias; _ })
        when implicit_alias && is_stub ->
          (* Stub files do not re-export unaliased imports *)
          sofar
      | Imported (ImportEntry.Module { target; implicit_alias }) ->
          let exported_module_name =
            if implicit_alias then
              Option.value_exn (Reference.head target)
            else
              target
          in
          Identifier.Map.set sofar ~key:name ~data:(Export.Module exported_module_name)
      | Imported (ImportEntry.Name { from; target; _ }) ->
          Identifier.Map.set sofar ~key:name ~data:(Export.NameAlias { from; name = target })
      | Define defines ->
          Identifier.Map.set
            sofar
            ~key:name
            ~data:(Export.Define { is_getattr_any = List.exists defines ~f:is_getattr_any })
      | Class -> Identifier.Map.set sofar ~key:name ~data:Export.Class
      | SimpleAssign _
      | TupleAssign _ ->
          Identifier.Map.set sofar ~key:name ~data:Export.GlobalVariable
    in
    Collector.from_source source
    |> List.fold ~init:Identifier.Map.empty ~f:collect_export
    |> Map.to_tree
  in
  let legacy_aliased_exports =
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
      exports;
      legacy_aliased_exports;
      empty_stub = is_stub && Source.Metadata.is_placeholder_stub local_mode;
      local_mode;
    }


let create_implicit ?(empty_stub = false) () = Implicit { empty_stub }

let get_export considered_module name =
  match considered_module with
  | Explicit { exports; _ } -> ExportMap.lookup exports name
  | Implicit _ -> None


let get_all_exports considered_module =
  match considered_module with
  | Explicit { exports; _ } -> ExportMap.to_alist exports
  | Implicit _ -> []


let is_implicit = function
  | Explicit _ -> false
  | Implicit _ -> true


let legacy_aliased_export considered_module reference =
  match considered_module with
  | Explicit { legacy_aliased_exports; _ } ->
      let aliased_exports = Reference.Map.of_tree legacy_aliased_exports in
      Map.find aliased_exports reference
  | Implicit _ -> None


let local_mode = function
  | Explicit { local_mode; _ } -> local_mode
  | Implicit _ -> None
