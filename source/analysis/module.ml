(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core
open Ast

module UnannotatedGlobal = struct
  type define_signature = {
    signature: Statement.Define.Signature.t;
    location: Location.WithModule.t;
  }
  [@@deriving sexp, compare]

  type import =
    | ImportModule of {
        target: Reference.t;
        implicit_alias: bool;
      }
    | ImportFrom of {
        from: Reference.t;
        target: Identifier.t;
        implicit_alias: bool;
      }
  [@@deriving sexp, compare]

  type t =
    | SimpleAssign of {
        explicit_annotation: Expression.t option;
        value: Expression.t option;
        target_location: Location.WithModule.t;
      }
    | TupleAssign of {
        value: Expression.t option;
        target_location: Location.WithModule.t;
        index: int;
        total_length: int;
      }
    | Imported of import
    | Define of define_signature list
    | Class
  [@@deriving sexp, compare]
end

module Collector = struct
  module Result = struct
    type t = {
      name: Identifier.t;
      unannotated_global: UnannotatedGlobal.t;
    }
    [@@deriving sexp, compare]
  end

  let from_source { Source.statements; module_path = { ModulePath.qualifier; _ }; _ } =
    let open Ast.Statement in
    let open Ast.Expression in
    let open UnannotatedGlobal in
    let rec visit_statement ~qualifier globals { Node.value; location } =
      match value with
      | Statement.Assign
          {
            Assign.target = { Node.value = Expression.Name (Name.Identifier identifier); location };
            annotation;
            value;
            _;
          } ->
          {
            Result.name = Identifier.sanitized identifier;
            unannotated_global =
              SimpleAssign
                {
                  explicit_annotation = annotation;
                  value;
                  target_location = Location.with_module ~module_reference:qualifier location;
                };
          }
          :: globals
      | Statement.Assign { Assign.target = { Node.value = Expression.Tuple elements; _ }; value; _ }
        ->
          let valid =
            let total_length = List.length elements in
            let is_simple_name index = function
              | { Node.value = Expression.Name (Name.Identifier identifier); location } ->
                  Some
                    {
                      Result.name = Identifier.sanitized identifier;
                      unannotated_global =
                        TupleAssign
                          {
                            value;
                            target_location =
                              Location.with_module ~module_reference:qualifier location;
                            index;
                            total_length;
                          };
                    }
              | _ -> None
            in
            List.mapi elements ~f:is_simple_name
          in
          List.rev_append (Option.all valid |> Option.value ~default:[]) globals
      | Statement.Import { Import.from = None; imports } ->
          let collect_module_import sofar { Node.value = { Import.name = target; alias }; _ } =
            let implicit_alias, name =
              match alias with
              | None ->
                  (* `import a.b` will bind name `a` in the current module *)
                  true, Reference.as_list target |> List.hd_exn
              | Some alias -> false, alias
            in
            { Result.name; unannotated_global = Imported (ImportModule { target; implicit_alias }) }
            :: sofar
          in
          List.fold imports ~init:globals ~f:collect_module_import
      | Statement.Import { Import.from = Some { Node.value = from; _ }; imports } ->
          let collect_name_import sofar { Node.value = { Import.name = target; alias }; _ } =
            (* `target` must be an unqualified identifier *)
            match Reference.show target with
            | "*" ->
                (* Don't register x.* as a global when a user writes `from x import *`. *)
                sofar
            | target ->
                let implicit_alias, name =
                  match alias with
                  | None -> true, target
                  | Some alias -> false, alias
                in
                {
                  Result.name;
                  unannotated_global = Imported (ImportFrom { from; target; implicit_alias });
                }
                :: sofar
          in
          List.fold imports ~init:globals ~f:collect_name_import
      | Statement.Class { Class.name; _ } ->
          { Result.name = name |> Reference.last; unannotated_global = Class } :: globals
      | Statement.Define { Define.signature = { Define.Signature.name; _ } as signature; _ } ->
          {
            Result.name = name |> Reference.last;
            unannotated_global =
              Define
                [
                  { signature; location = Location.with_module ~module_reference:qualifier location };
                ];
          }
          :: globals
      | Statement.If { If.body; orelse; _ } ->
          (* TODO(T28732125): Properly take an intersection here. *)
          List.fold ~init:globals ~f:(visit_statement ~qualifier) (body @ orelse)
      | Statement.Try { Try.body; handlers; orelse; finally; handles_exception_group = _ } ->
          let globals = List.fold ~init:globals ~f:(visit_statement ~qualifier) body in
          let globals =
            let handlers_statements =
              List.concat_map handlers ~f:(fun { Try.Handler.body; _ } -> body)
            in
            List.fold ~init:globals ~f:(visit_statement ~qualifier) handlers_statements
          in
          let globals = List.fold ~init:globals ~f:(visit_statement ~qualifier) orelse in
          List.fold ~init:globals ~f:(visit_statement ~qualifier) finally
      | Statement.With { With.body; _ } ->
          List.fold ~init:globals ~f:(visit_statement ~qualifier) body
      | _ -> globals
    in
    List.fold ~init:[] ~f:(visit_statement ~qualifier) statements |> List.rev
end

module Export = struct
  module Name = struct
    type t =
      | Class
      | Define of { is_getattr_any: bool }
      | GlobalVariable
    [@@deriving sexp, compare, hash, show]
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

module Metadata = struct
  type t =
    | Explicit of {
        exports: ExportMap.t;
        empty_stub: bool;
      }
    | Implicit of { empty_stub: bool }
  [@@deriving eq, sexp, compare]

  let empty_stub = function
    | Implicit { empty_stub }
    | Explicit { empty_stub; _ } ->
        empty_stub


  let pp format printed_module =
    Format.fprintf format "MODULE[empty_stub = %b]" (empty_stub printed_module)


  let show = Format.asprintf "%a" pp

  let create_for_testing ~stub = Explicit { exports = ExportMap.empty; empty_stub = stub }

  let create
      ({ Source.module_path; typecheck_flags = { Source.TypecheckFlags.local_mode; _ }; _ } as
      source)
    =
    let is_stub = ModulePath.is_stub module_path in
    let exports =
      let open UnannotatedGlobal in
      let is_getattr_any
          { signature = { Statement.Define.Signature.name; parameters; return_annotation; _ }; _ }
        =
        match Reference.last name with
        | "__getattr__" -> (
            match parameters with
            | [{ Node.value = { Expression.Parameter.annotation = parameter_annotation; _ }; _ }]
              -> (
                match parameter_annotation, return_annotation with
                | ( ( Some
                        {
                          Node.value = Expression.Expression.Name (Expression.Name.Identifier "str");
                          _;
                        }
                    | None ),
                    Some
                      {
                        Node.value =
                          Expression.Expression.Name
                            ( Expression.Name.Identifier "Any"
                            | Expression.Name.Attribute
                                {
                                  Expression.Name.Attribute.base =
                                    {
                                      Node.value =
                                        Expression.Expression.Name
                                          (Expression.Name.Identifier "typing");
                                      _;
                                    };
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
        | Imported (ImportModule { implicit_alias; _ } | ImportFrom { implicit_alias; _ })
          when implicit_alias && is_stub ->
            (* Stub files do not re-export unaliased imports *)
            sofar
        | Imported (ImportModule { target; implicit_alias }) ->
            let exported_module_name =
              if implicit_alias then
                Option.value_exn (Reference.head target)
              else
                target
            in
            Identifier.Map.Tree.set sofar ~key:name ~data:(Export.Module exported_module_name)
        | Imported (ImportFrom { from; target; _ }) ->
            Identifier.Map.Tree.set sofar ~key:name ~data:(Export.NameAlias { from; name = target })
        | Define defines ->
            Identifier.Map.Tree.set
              sofar
              ~key:name
              ~data:
                Export.(
                  Name (Name.Define { is_getattr_any = List.exists defines ~f:is_getattr_any }))
        | Class -> Identifier.Map.Tree.set sofar ~key:name ~data:Export.(Name Name.Class)
        | SimpleAssign _
        | TupleAssign _ ->
            Identifier.Map.Tree.set sofar ~key:name ~data:Export.(Name Name.GlobalVariable)
      in
      let init =
        let export_from_missing_classes { Node.value = { Statement.Class.name; _ }; _ } =
          Reference.last name, Export.(Name Name.Class)
        in
        let exports_from_missing_classes missing_classes =
          List.map missing_classes ~f:export_from_missing_classes
          |> Identifier.Map.Tree.of_alist_exn
        in
        (* TODO(stroxler): Deduplicate this logic from
           UnannotatedGlobalEnvironment.ModuleComponents.class_summaries_of_source *)
        match Reference.as_list (ModulePath.qualifier module_path) with
        | [] -> exports_from_missing_classes MissingFromStubs.missing_builtin_classes
        | ["typing"] -> exports_from_missing_classes MissingFromStubs.missing_typing_classes
        | ["typing_extensions"] ->
            exports_from_missing_classes MissingFromStubs.missing_typing_extensions_classes
        | _ -> Identifier.Map.Tree.empty
      in
      Collector.from_source source |> List.fold ~init ~f:collect_export
    in
    Explicit
      { exports; empty_stub = is_stub && Source.TypecheckFlags.is_placeholder_stub local_mode }


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
end
