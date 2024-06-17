(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core
open Ast

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

module Collector = struct
  module Result = struct
    type unannotated_global = t [@@deriving sexp, compare]

    type t = {
      name: Identifier.t;
      unannotated_global: unannotated_global;
    }
    [@@deriving sexp, compare]
  end

  let from_source { Source.statements; module_path = { ModulePath.qualifier; _ }; _ } =
    let open Ast.Statement in
    let open Ast.Expression in
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
