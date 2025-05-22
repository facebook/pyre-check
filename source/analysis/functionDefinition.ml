(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core
open Ast
open Statement

module Sibling = struct
  module Kind = struct
    type t =
      | Overload
      | PropertySetter
    [@@deriving sexp, equal, compare]
  end

  type t = {
    kind: Kind.t;
    body: Define.t Node.t;
  }
  [@@deriving sexp, equal, compare]
end

type t = {
  qualifier: Reference.t;
  body: Define.t Node.t option;
  siblings: Sibling.t list;
}
[@@deriving sexp, equal, compare]

(* Get a non-mangled, qualified name of a define under the assumption that the NestingContext.t has
   correct names but the actual name of the define might already qualified and mangled.

   This function should return the same result whether run before or after qualification. *)
let qualified_name_of_signature
    ~module_name
    { Define.Signature.name = mangled_name; parent = nesting_context; _ }
  =
  let unqualified_name = mangled_name |> Reference.sanitize_qualified |> Reference.last in
  Reference.create
    ~prefix:(NestingContext.to_qualifier ~module_name nesting_context)
    unqualified_name


let qualified_name_of_define ~module_name { Define.signature; _ } =
  qualified_name_of_signature ~module_name signature


let all_bodies { body; siblings; _ } =
  let sibling_bodies = List.map siblings ~f:(fun { Sibling.body; _ } -> body) in
  match body with
  | None -> sibling_bodies
  | Some body -> body :: sibling_bodies


let body_for_location function_definition ~location =
  all_bodies function_definition
  |> List.find ~f:(fun { Node.location = body_location; _ } ->
         Location.equal body_location location)


let collect_typecheck_units { Source.statements; module_path = { ModulePath.qualifier; _ }; _ } =
  (* TODO (T57944324): Support checking classes that are nested inside function bodies *)
  let rec collect_from_statement ~ignore_class sofar { Node.value; location } =
    match value with
    | Statement.Class ({ Class.name; body; _ } as class_) ->
        if ignore_class then (
          Log.debug
            "Dropping the body of class %a as it is nested inside a function"
            Reference.pp
            name;
          sofar)
        else
          let sofar =
            let define = Class.toplevel_define ~qualifier class_ |> Node.create ~location in
            define :: sofar
          in
          List.fold body ~init:sofar ~f:(collect_from_statement ~ignore_class)
    | Define ({ Define.body; _ } as define) ->
        let sofar = { Node.location; Node.value = define } :: sofar in
        List.fold body ~init:sofar ~f:(collect_from_statement ~ignore_class:true)
    | Match { Match.cases; _ } ->
        let from_case sofar { Match.Case.body; _ } =
          List.fold body ~init:sofar ~f:(collect_from_statement ~ignore_class)
        in
        List.fold cases ~init:sofar ~f:from_case
    | For { For.body; orelse; _ }
    | If { If.body; orelse; _ }
    | While { While.body; orelse; _ } ->
        let sofar = List.fold body ~init:sofar ~f:(collect_from_statement ~ignore_class) in
        List.fold orelse ~init:sofar ~f:(collect_from_statement ~ignore_class)
    | Try { Try.body; handlers; orelse; finally; handles_exception_group = _ } ->
        let sofar = List.fold body ~init:sofar ~f:(collect_from_statement ~ignore_class) in
        let sofar =
          List.fold handlers ~init:sofar ~f:(fun sofar { Try.Handler.body; _ } ->
              List.fold body ~init:sofar ~f:(collect_from_statement ~ignore_class))
        in
        let sofar = List.fold orelse ~init:sofar ~f:(collect_from_statement ~ignore_class) in
        List.fold finally ~init:sofar ~f:(collect_from_statement ~ignore_class)
    | With { With.body; _ } -> List.fold body ~init:sofar ~f:(collect_from_statement ~ignore_class)
    | Assign _
    | Assert _
    | AugmentedAssign _
    | Break
    | Continue
    | Delete _
    | Expression _
    | Global _
    | Import _
    | Nonlocal _
    | Pass
    | Raise _
    | TypeAlias _
    | Return _ ->
        sofar
  in
  let drop_nested_body { Node.value = { Define.body; _ } as define; location } =
    let new_define =
      let rec drop_nested_body_in_statement = function
        | Statement.Class definition -> Statement.Class { definition with body = [] }
        | Define { Define.signature; _ } ->
            Statement.Define { Define.signature; captures = []; unbound_names = []; body = [] }
        | For ({ For.body; orelse; _ } as for_statement) ->
            Statement.For
              {
                for_statement with
                body = drop_nested_body_in_statements body;
                orelse = drop_nested_body_in_statements orelse;
              }
        | Match ({ Match.cases; _ } as match_statement) ->
            Statement.Match
              {
                match_statement with
                cases =
                  List.map cases ~f:(fun ({ Match.Case.body; _ } as case) ->
                      { case with Match.Case.body = drop_nested_body_in_statements body });
              }
        | If ({ If.body; orelse; _ } as if_statement) ->
            Statement.If
              {
                if_statement with
                body = drop_nested_body_in_statements body;
                orelse = drop_nested_body_in_statements orelse;
              }
        | While ({ While.body; orelse; _ } as while_statement) ->
            Statement.While
              {
                while_statement with
                body = drop_nested_body_in_statements body;
                orelse = drop_nested_body_in_statements orelse;
              }
        | Try { Try.body; handlers; orelse; finally; handles_exception_group } ->
            Statement.Try
              {
                Try.body = drop_nested_body_in_statements body;
                handlers =
                  List.map handlers ~f:(fun ({ Try.Handler.body; _ } as handler) ->
                      { handler with Try.Handler.body = drop_nested_body_in_statements body });
                orelse = drop_nested_body_in_statements orelse;
                finally = drop_nested_body_in_statements finally;
                handles_exception_group;
              }
        | With ({ With.body; _ } as with_statement) ->
            Statement.With { with_statement with body = drop_nested_body_in_statements body }
        | _ as statement -> statement
      and drop_nested_body_in_statements statements =
        List.map statements ~f:(Node.map ~f:drop_nested_body_in_statement)
      in
      { define with Define.body = drop_nested_body_in_statements body }
    in
    { Node.value = new_define; location }
  in
  List.fold statements ~init:[] ~f:(collect_from_statement ~ignore_class:false)
  |> List.map ~f:drop_nested_body


let collect_defines ({ Source.module_path = { ModulePath.qualifier; _ }; _ } as source) =
  let all_defines = collect_typecheck_units source in
  let table = Reference.Table.create () in
  let process_define ({ Node.value = define; _ } as define_node) =
    let qualified_name = qualified_name_of_define ~module_name:qualifier define in
    let sibling =
      let open Sibling in
      if Define.is_overloaded_function define then
        Some { kind = Kind.Overload; body = define_node }
      else if Define.is_property_setter define then
        Some { kind = Kind.PropertySetter; body = define_node }
      else
        None
    in
    let update = function
      | None -> (
          match sibling with
          | Some sibling -> None, [sibling]
          | None -> Some define_node, [])
      | Some (body, siblings) -> (
          match sibling with
          | Some sibling -> body, sibling :: siblings
          | None ->
              if Option.is_some body then (
                Log.debug
                  "Dropping the body of function %a as it has duplicated name with other functions"
                  Reference.pp
                  qualified_name;
                (* Last definition wins -- collector returns functions in reverse order *)
                body, siblings)
              else
                Some define_node, siblings)
    in
    Hashtbl.update table qualified_name ~f:update
  in
  let collect_definition ~key ~data:(body, overloads) collected =
    let siblings = List.sort overloads ~compare:Sibling.compare in
    (key, { qualifier; body; siblings }) :: collected
  in
  let all_defines =
    (* Take into account module toplevel *)
    Source.top_level_define_node source :: all_defines
  in
  List.iter all_defines ~f:process_define;
  Hashtbl.fold table ~init:[] ~f:collect_definition
