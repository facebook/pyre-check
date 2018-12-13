(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Analysis
open Expression
open Pyre


(** Roots representing parameters, locals, and special return value in models. *)
module Root = struct
  type t =
    | LocalResult (* Special root representing the return value location. *)
    | PositionalParameter of { position: int; name: Identifier.t; }
    | NamedParameter of { name: Identifier.t; }
    | StarParameter of { position: int }
    | StarStarParameter of { excluded: Identifier.t list }
    | Variable of Identifier.t
  [@@deriving compare, eq, sexp, show, hash]

  let chop_parameter_prefix name =
    match String.chop_prefix ~prefix:"$parameter$" name with
    | Some chopped -> chopped
    | None -> name

  let normalize_parameters parameters =
    let normalize_parameters
        position
        (seen_star, excluded, normalized)
        { Node.value = { Parameter.name = prefixed_name; annotation; _ }; _ }
      =
      let prefixed_name_string = Identifier.show prefixed_name in
      if String.is_prefix ~prefix:"**" prefixed_name_string then
        let prefixed_variable_name =
          String.chop_prefix_exn prefixed_name_string ~prefix:"**"
          |> Identifier.create
        in
        (
          true,
          excluded,
          (StarStarParameter { excluded }, prefixed_variable_name, annotation) :: normalized
        )
      else if String.is_prefix ~prefix:"*" prefixed_name_string then
        let prefixed_variable_name =
          String.chop_prefix_exn prefixed_name_string ~prefix:"*"
          |> Identifier.create
        in
        (
          true,
          excluded,
          (StarParameter { position }, prefixed_variable_name, annotation) :: normalized
        )
      else if seen_star then
        let normal_name = prefixed_name_string |> chop_parameter_prefix |> Identifier.create in
        (
          true,
          normal_name :: excluded,
          (NamedParameter { name = normal_name }, prefixed_name, annotation) :: normalized
        )
      else
        let normal_name = prefixed_name_string |> chop_parameter_prefix |> Identifier.create in
        (
          false,
          normal_name :: excluded,
          (PositionalParameter { position; name = normal_name }, prefixed_name, annotation)
          :: normalized
        )
    in
    List.foldi parameters ~f:normalize_parameters ~init:(false, [],  [])
    |> fun (_, _, parameters) -> parameters

  let parameter_name = function
    | PositionalParameter { name; _ }
    | NamedParameter { name } ->
        Some (Identifier.show name)
    | StarParameter _ ->
        Some "*"
    | StarStarParameter _ ->
        Some "**"
    | _ ->
        None

end


type argument_match = {
  root: Root.t;
  actual_path: AbstractTreeDomain.Label.path;
  formal_path: AbstractTreeDomain.Label.path;
}
[@@deriving show]


type argument_position = [
  | `Precise of int
  | `Approximate of int
]
[@@deriving show]


let match_actuals_to_formals arguments roots =
  let open Root in
  let increment = function
    | `Precise n -> `Precise (n + 1)
    | `Approximate n -> `Approximate (n + 1)
  in
  let approximate = function
    | `Precise n -> `Approximate n
    | `Approximate n -> `Approximate n
  in
  let filter_to_named name_or_star_star formal =
    match name_or_star_star, formal with
    | `Name actual_name, (NamedParameter { name; } | PositionalParameter { name; _ })
      when Identifier.equal actual_name name ->
        Some {
          root = formal;
          actual_path = [];
          formal_path = [];
        }
    | `Name actual_name, StarStarParameter { excluded; }
      when not (List.exists ~f:(Identifier.equal actual_name) excluded) ->
        let field_name = Root.chop_parameter_prefix (Identifier.show actual_name) in
        Some {
          root = formal;
          actual_path = [];
          formal_path = [ AbstractTreeDomain.Label.create_name_field field_name ];
        }
    | `StarStar, NamedParameter { name } ->
        Some {
          root = formal;
          actual_path = [ AbstractTreeDomain.Label.Field (Identifier.show name) ];
          formal_path = [];
        }
    | `StarStar, StarStarParameter _ ->
        (* assume entire structure is passed. We can't just pick all but X fields. *)
        Some {
          root = formal;
          actual_path = [];
          formal_path = [];
        }
    | (`Name _ | `StarStar), _ ->
        None
  in
  let filter_to_positional position formal =
    let position = (
      position :> [
        | `Star of argument_position
        | argument_position
      ]
    )
    in
    match position, formal with
    | `Precise actual_position, PositionalParameter { position; _ }
      when actual_position = position ->
        Some {
          root = formal;
          actual_path = [];
          formal_path = [];
        }
    | `Approximate minimal_position, PositionalParameter { position; _ }
      when minimal_position <= position ->
        Some {
          root = formal;
          actual_path = [];
          formal_path = [];
        }
    | `Star (`Precise actual_position), PositionalParameter { position; _ }
      when actual_position <= position ->
        Some {
          root = formal;
          actual_path = [AbstractTreeDomain.Label.create_int_field (position - actual_position)];
          formal_path = [];
        }
    | `Star (`Approximate minimal_position), PositionalParameter { position; _ }
      when minimal_position <= position ->
        Some {
          root = formal;
          actual_path = [AbstractTreeDomain.Label.Any];
          formal_path = [];
        }
    | `Precise actual_position, StarParameter { position }
      when actual_position >= position ->
        Some {
          root = formal;
          actual_path = [];
          formal_path = [AbstractTreeDomain.Label.create_int_field (actual_position - position)];
        }
    | `Approximate minimal_position, StarParameter { position; _ }
      when minimal_position <= position ->
        (* Approximate: can't match up range *)
        Some {
          root = formal;
          actual_path = [];
          formal_path = [AbstractTreeDomain.Label.Any];
        }
    | `Star _, StarParameter _ ->
        (* Approximate: can't match up ranges, so pass entire structure *)
        Some {
          root = formal;
          actual_path = [];
          formal_path = [];
        }
    | (`Star _ | `Precise _ | `Approximate _), _ ->
        None
  in
  let match_actual (position, matches) { Argument.name; value } =
    match name, value.Node.value with
    | None, Starred (Once _) ->
        let formals = List.filter_map roots ~f:(filter_to_positional (`Star position)) in
        (approximate position, (value, formals) :: matches)
    | None, Starred (Twice _) ->
        let formals = List.filter_map roots ~f:(filter_to_named `StarStar) in
        (position, (value, formals) :: matches)
    | None, _ ->
        let formals = List.filter_map roots ~f:(filter_to_positional position) in
        (increment position, (value, formals) :: matches)
    | Some { value = name; _ }, _ ->
        let normal_name = Identifier.show name |> chop_parameter_prefix |> Identifier.create in
        let formals = List.filter_map roots ~f:(filter_to_named (`Name normal_name)) in
        (position, (value, formals) :: matches)
  in
  let _, result = List.fold arguments ~f:match_actual ~init:(`Precise 0, []) in
  result


type t = {
  root: Root.t;
  path: AbstractTreeDomain.Label.path;
}
[@@deriving eq]


let create root path = { root; path; }


let of_access path access_element =
  match path, access_element with
  | Some path, Access.Identifier id ->
      Some (AbstractTreeDomain.Label.Field (Identifier.show id) :: path)
  | _ -> None


let of_accesses = function
  | Access.Identifier root :: rest ->
      List.fold ~init:(Some []) ~f:of_access rest
      >>| (fun path -> { root = Root.Variable root; path })
  | _ -> None


let of_expression = function
  | { Node.value = Access access; _ } -> of_accesses access
  | _ -> None


type normalized_expression =
  | Access of { expression: normalized_expression; member: Identifier.t }
  | Call of {
      callee: normalized_expression;
      arguments: Argument.t list Node.t;
    }
  | Index of {
      expression: normalized_expression;
      index: AbstractTreeDomain.Label.t;
      original: Identifier.t;
      arguments: ((Expression.t Argument.record) list) Node.t;
    }
  | Global of Access.t
  | Local of Identifier.t
  | Expression of Expression.t
[@@deriving eq, show]


let is_get_item member =
  Identifier.show member = "__getitem__"


let get_index { Node.value = expression; _ } =
  match expression with
  | String literal ->
      AbstractTreeDomain.Label.Field literal.value
  | Integer i ->
      AbstractTreeDomain.Label.Field (string_of_int i)
  | _ ->
      AbstractTreeDomain.Label.Any

let normalize_access_list left = function
  | Access.Identifier member ->
      Access { expression = left; member }
  | Access.Call ({ value = [argument]; _ } as arguments) -> begin
      match left with
      | Access { expression; member } when is_get_item member ->
          Index {
            expression;
            index = get_index argument.value;
            original = member;
            arguments;
          }
      | _ ->
          Call { callee = left; arguments }
    end
  | Access.Call arguments ->
      Call { callee = left; arguments }
  | Access.Expression _ ->
      failwith "invalid expr in access (not in root position)"


let global_prefix ~resolution access =
  let rec module_prefix ~lead ~tail =
    match tail with
    | (Access.Identifier _ as identifier) :: new_tail ->
        let new_lead = lead @ [identifier] in
        if Resolution.module_definition resolution lead |> Option.is_some then
          module_prefix ~lead:new_lead ~tail:new_tail
        else
          lead, tail
    | _ ->
        lead, tail
  in
  module_prefix ~lead:[] ~tail:access


let split_root ~resolution = function
  | Access.Identifier identifier :: rest
    when Interprocedural.CallResolution.is_local identifier ->
      Local identifier, rest
  | Access.Expression expression :: rest ->
      Expression expression, rest
  | (Access.Identifier _ :: _) as access ->
      let prefix, rest = global_prefix access ~resolution in
      Global prefix, rest
  | Access.Call _ :: _ ->
      failwith "invalid root (call) in access"
  | _ ->
      failwith "empty access"


let normalize_access ~resolution path =
  let (root, rest) = split_root path ~resolution in
  List.fold rest ~init:root ~f:normalize_access_list


let rec as_access = function
  | Global access ->
      access
  | Local identifier ->
      Access.create_from_identifiers [identifier]
  | Expression expression ->
      [Access.Expression expression]
  | Call { callee; arguments; } ->
      let callee_access = as_access callee in
      callee_access @ [Access.Call arguments]
  | Access { expression; member; } ->
      let left = as_access expression in
      left @ [Access.Identifier member]
  | Index { expression; original; arguments; _ } ->
      let left = as_access expression in
      left @ [ Access.Identifier original; Access.Call arguments ]


let to_json { root; path; } =
  let open Root in
  let root_name = function
    | LocalResult ->
        "result"
    | PositionalParameter { position=_; name } ->
        Format.sprintf "formal(%s)" (Identifier.show name)
    | NamedParameter { name } ->
        Format.sprintf "formal(%s)" (Identifier.show name)
    | StarParameter { position } ->
        Format.sprintf "formal(*rest%d)" position
    | StarStarParameter _ ->
        "formal(**kw)"
    | Variable name ->
        Format.sprintf "local(%s)" (Identifier.show name)
  in
  `String (root_name root ^ AbstractTreeDomain.Label.show_path path)
