(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
open Expression
open Statement

module UnannotatedDefine = struct
  type t = {
    define: Define.Signature.t;
    location: Location.WithModule.t;
  }
  [@@deriving sexp, compare]
end

type t =
  | SimpleAssign of {
      explicit_annotation: Expression.t option;
      value: Expression.t;
      target_location: Location.WithModule.t;
    }
  | TupleAssign of {
      value: Expression.t;
      target_location: Location.WithModule.t;
      index: int;
      total_length: int;
    }
  | Imported of Reference.t
  | Define of UnannotatedDefine.t list
[@@deriving sexp, compare]

module Collector = struct
  module Result = struct
    type unannotated_global = t [@@deriving sexp, compare]

    type t = {
      name: Reference.t;
      unannotated_global: unannotated_global;
    }
    [@@deriving sexp, compare]
  end

  let from_source { Source.statements; source_path = { SourcePath.qualifier; _ }; _ } =
    let rec visit_statement ~qualifier globals { Node.value; location } =
      match value with
      | Statement.Assign
          {
            Assign.target = { Node.value = Name (Name.Identifier identifier); location };
            annotation;
            value;
            _;
          } ->
          let qualified =
            Identifier.sanitized identifier |> Reference.create |> Reference.combine qualifier
          in
          {
            Result.name = qualified;
            unannotated_global =
              SimpleAssign
                {
                  explicit_annotation = annotation;
                  value;
                  target_location = Location.with_module ~qualifier location;
                };
          }
          :: globals
      | Statement.Assign { Assign.target = { Node.value = Tuple elements; _ }; value; _ } ->
          let valid =
            let total_length = List.length elements in
            let is_simple_name index = function
              | { Node.value = Expression.Name (Name.Identifier identifier); location } ->
                  let name =
                    Identifier.sanitized identifier
                    |> Reference.create
                    |> Reference.combine qualifier
                  in
                  Some
                    {
                      Result.name;
                      unannotated_global =
                        TupleAssign
                          {
                            value;
                            target_location = Location.with_module ~qualifier location;
                            index;
                            total_length;
                          };
                    }
              | _ -> None
            in
            List.mapi elements ~f:is_simple_name
          in
          List.rev_append (Option.all valid |> Option.value ~default:[]) globals
      | Import { Import.from = Some _; imports = [{ Import.name = { Node.value = name; _ }; _ }] }
        when String.equal (Reference.show name) "*" ->
          (* Don't register x.* as a global when a user writes `from x import *`. *)
          globals
      | Import { Import.from; imports } ->
          let from =
            match from >>| Node.value >>| Reference.show with
            | None
            | Some "future.builtins"
            | Some "builtins" ->
                Reference.empty
            | Some from -> Reference.create from
          in
          let import_to_global { Import.name = { Node.value = name; _ }; alias } =
            let qualified_name =
              match alias with
              | None -> Reference.combine qualifier name
              | Some { Node.value = alias; _ } ->
                  Reference.combine qualifier (Reference.create alias)
            in
            let original_name = Reference.combine from name in
            { Result.name = qualified_name; unannotated_global = Imported original_name }
          in
          List.rev_append (List.map ~f:import_to_global imports) globals
      | Define { Define.signature = { Define.Signature.name; _ } as signature; _ } ->
          {
            Result.name = Node.value name;
            unannotated_global =
              Define [{ define = signature; location = Location.with_module ~qualifier location }];
          }
          :: globals
      | If { If.body; orelse; _ } ->
          (* TODO(T28732125): Properly take an intersection here. *)
          List.fold ~init:globals ~f:(visit_statement ~qualifier) (body @ orelse)
      | Try { Try.body; handlers; orelse; finally } ->
          let globals = List.fold ~init:globals ~f:(visit_statement ~qualifier) body in
          let globals =
            let handlers_statements =
              List.concat_map handlers ~f:(fun { Try.Handler.body; _ } -> body)
            in
            List.fold ~init:globals ~f:(visit_statement ~qualifier) handlers_statements
          in
          let globals = List.fold ~init:globals ~f:(visit_statement ~qualifier) orelse in
          List.fold ~init:globals ~f:(visit_statement ~qualifier) finally
      | _ -> globals
    in
    List.fold ~init:[] ~f:(visit_statement ~qualifier) statements |> List.rev
end
