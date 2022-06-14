(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Statement

module Export = struct
  module Name = struct
    type t =
      | Class
      | Define of { is_getattr_any: bool }
      | GlobalVariable
    [@@deriving sexp, compare, hash]
  end

  type t =
    | NameAlias of {
        from: Reference.t;
        name: Identifier.t;
      }
    | Module of Reference.t
    | Name of Name.t
  [@@deriving sexp, compare, hash]
end

module ExportMap = struct
  type t = Export.t Identifier.Map.Tree.t [@@deriving sexp]

  let empty = Identifier.Map.Tree.empty

  let lookup map name =
    match Identifier.Map.Tree.find map name with
    | Some _ as export -> export
    | None -> (
        match name with
        | "__doc__"
        | "__file__"
        | "__name__"
        | "__package__"
        | "__path__"
        | "__dict__" ->
            Some (Export.Name Export.Name.GlobalVariable)
        | _ -> None)


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


let create_for_testing ~stub =
  Explicit
    {
      exports = ExportMap.empty;
      legacy_aliased_exports = Reference.Map.empty |> Map.to_tree;
      empty_stub = stub;
    }


let create
    ({
       Source.module_path = { ModulePath.is_stub; qualifier; _ } as source_path;
       statements;
       typecheck_flags = { Source.TypecheckFlags.local_mode; _ };
       _;
     } as source)
  =
  let exports =
    let open UnannotatedGlobal in
    let is_getattr_any
        {
          UnannotatedDefine.define = { Define.Signature.name; parameters; return_annotation; _ };
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
              | _ -> false)
          | _ -> false)
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
            ~data:
              Export.(Name (Name.Define { is_getattr_any = List.exists defines ~f:is_getattr_any }))
      | Class -> Identifier.Map.set sofar ~key:name ~data:Export.(Name Class)
      | SimpleAssign _
      | TupleAssign _ ->
          Identifier.Map.set sofar ~key:name ~data:Export.(Name GlobalVariable)
    in
    Collector.from_source source
    |> List.fold ~init:Identifier.Map.empty ~f:collect_export
    |> Map.to_tree
  in
  let legacy_aliased_exports =
    let rec aliased_exports aliases { Node.value; _ } =
      match value with
      | Statement.Import { Import.from = Some from; imports } ->
          let from = ModulePath.expand_relative_import source_path ~from in
          let export aliases { Node.value = { Import.name; alias }; _ } =
            let alias =
              match alias with
              | None -> name
              | Some alias -> Reference.create alias
            in
            let name =
              if String.equal (Reference.show alias) "*" then
                from
              else
                Reference.combine from name
            in
            Map.set aliases ~key:alias ~data:name
          in
          List.fold imports ~f:export ~init:aliases
      | Import { Import.from = None; imports } ->
          let export aliases { Node.value = { Import.name; alias }; _ } =
            let alias =
              match alias with
              | None -> name
              | Some alias -> Reference.create alias
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
      | If { If.body; orelse; _ } ->
          let aliases = List.fold body ~init:aliases ~f:aliased_exports in
          List.fold orelse ~init:aliases ~f:aliased_exports
      | _ -> aliases
    in
    List.fold statements ~f:aliased_exports ~init:Reference.Map.empty |> Map.to_tree
  in
  Explicit
    {
      exports;
      legacy_aliased_exports;
      empty_stub = is_stub && Source.TypecheckFlags.is_placeholder_stub local_mode;
    }


let create_implicit ?(empty_stub = false) () = Implicit { empty_stub }

let get_export considered_module name =
  let exports =
    match considered_module with
    | Explicit { exports; _ } -> exports
    | Implicit _ -> ExportMap.empty
  in
  ExportMap.lookup exports name


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
