(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Analysis
open Expression
open Pyre

(** Roots representing parameters, locals, and special return value in models. *)
module Root = struct
  type t =
    | LocalResult (* Special root representing the return value location. *)
    | PositionalParameter of { position: int; name: Identifier.t }
    | NamedParameter of { name: Identifier.t }
    | StarParameter of { position: int }
    | StarStarParameter of { excluded: Identifier.t list }
    | Variable of Identifier.t
  [@@deriving compare, eq, sexp, show { with_path = false }, hash]

  let chop_parameter_prefix name =
    match String.chop_prefix ~prefix:"$parameter$" name with
    | Some chopped -> chopped
    | None -> name


  let normalize_parameters parameters =
    let normalize_parameters
        position
        (seen_star, excluded, normalized)
        ({ Node.value = { Parameter.name = prefixed_name; _ }; _ } as original)
      =
      let prefixed_name_string = prefixed_name in
      if String.is_prefix ~prefix:"**" prefixed_name_string then
        let prefixed_variable_name = String.chop_prefix_exn prefixed_name_string ~prefix:"**" in
        ( true,
          excluded,
          (StarStarParameter { excluded }, prefixed_variable_name, original) :: normalized )
      else if String.is_prefix ~prefix:"*" prefixed_name_string then
        let prefixed_variable_name = String.chop_prefix_exn prefixed_name_string ~prefix:"*" in
        ( true,
          excluded,
          (StarParameter { position }, prefixed_variable_name, original) :: normalized )
      else if seen_star then
        let normal_name = prefixed_name_string |> chop_parameter_prefix in
        ( true,
          normal_name :: excluded,
          (NamedParameter { name = normal_name }, prefixed_name, original) :: normalized )
      else
        let normal_name = prefixed_name_string |> chop_parameter_prefix in
        ( false,
          normal_name :: excluded,
          (PositionalParameter { position; name = normal_name }, prefixed_name, original)
          :: normalized )
    in
    List.foldi parameters ~f:normalize_parameters ~init:(false, [], [])
    |> fun (_, _, parameters) -> parameters


  let parameter_name = function
    | PositionalParameter { name; _ }
    | NamedParameter { name } ->
        Some name
    | StarParameter _ -> Some "*"
    | StarStarParameter _ -> Some "**"
    | _ -> None
end

type argument_match = {
  root: Root.t;
  actual_path: AbstractTreeDomain.Label.path;
  formal_path: AbstractTreeDomain.Label.path
}
[@@deriving show { with_path = false }]

type argument_position =
  [ `Precise of int
  | `Approximate of int
  ]

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
    | `Name actual_name, (NamedParameter { name } | PositionalParameter { name; _ })
      when Identifier.equal actual_name name ->
        Some { root = formal; actual_path = []; formal_path = [] }
    | `Name actual_name, StarStarParameter { excluded }
      when not (List.exists ~f:(Identifier.equal actual_name) excluded) ->
        let field_name = Root.chop_parameter_prefix actual_name in
        Some
          { root = formal;
            actual_path = [];
            formal_path = [AbstractTreeDomain.Label.create_name_field field_name]
          }
    | `StarStar, NamedParameter { name } ->
        Some
          { root = formal; actual_path = [AbstractTreeDomain.Label.Field name]; formal_path = [] }
    | `StarStar, StarStarParameter _ ->
        (* assume entire structure is passed. We can't just pick all but X fields. *)
        Some { root = formal; actual_path = []; formal_path = [] }
    | (`Name _ | `StarStar), _ -> None
  in
  let filter_to_positional position formal =
    let position = (position :> [ `Star of argument_position | argument_position ]) in
    match position, formal with
    | `Precise actual_position, PositionalParameter { position; _ } when actual_position = position
      ->
        Some { root = formal; actual_path = []; formal_path = [] }
    | `Approximate minimal_position, PositionalParameter { position; _ }
      when minimal_position <= position ->
        Some { root = formal; actual_path = []; formal_path = [] }
    | `Star (`Precise actual_position), PositionalParameter { position; _ }
      when actual_position <= position ->
        Some
          { root = formal;
            actual_path = [AbstractTreeDomain.Label.create_int_field (position - actual_position)];
            formal_path = []
          }
    | `Star (`Approximate minimal_position), PositionalParameter { position; _ }
      when minimal_position <= position ->
        Some { root = formal; actual_path = [AbstractTreeDomain.Label.Any]; formal_path = [] }
    | `Precise actual_position, StarParameter { position } when actual_position >= position ->
        Some
          { root = formal;
            actual_path = [];
            formal_path = [AbstractTreeDomain.Label.create_int_field (actual_position - position)]
          }
    | `Approximate minimal_position, StarParameter { position; _ }
      when minimal_position <= position ->
        (* Approximate: can't match up range *)
        Some { root = formal; actual_path = []; formal_path = [AbstractTreeDomain.Label.Any] }
    | `Star _, StarParameter _ ->
        (* Approximate: can't match up ranges, so pass entire structure *)
        Some { root = formal; actual_path = []; formal_path = [] }
    | (`Star _ | `Precise _ | `Approximate _), _ -> None
  in
  let match_actual (position, matches) { Argument.name; value } =
    match name, value.Node.value with
    | None, Starred (Once _) ->
        let formals = List.filter_map roots ~f:(filter_to_positional (`Star position)) in
        approximate position, (value, formals) :: matches
    | None, Starred (Twice _) ->
        let formals = List.filter_map roots ~f:(filter_to_named `StarStar) in
        position, (value, formals) :: matches
    | None, _ ->
        let formals = List.filter_map roots ~f:(filter_to_positional position) in
        increment position, (value, formals) :: matches
    | Some { value = name; _ }, _ ->
        let normal_name = chop_parameter_prefix name in
        let formals = List.filter_map roots ~f:(filter_to_named (`Name normal_name)) in
        position, (value, formals) :: matches
  in
  let _, result = List.fold arguments ~f:match_actual ~init:(`Precise 0, []) in
  result


type t = {
  root: Root.t;
  path: AbstractTreeDomain.Label.path
}
[@@deriving show { with_path = false }]

let create root path = { root; path }

let of_access path access_element =
  match path, access_element with
  | Some path, Access.Identifier id -> Some (AbstractTreeDomain.Label.Field id :: path)
  | _ -> None


let of_accesses = function
  | Access.Identifier root :: rest ->
      List.fold ~init:(Some []) ~f:of_access rest
      >>| fun path -> { root = Root.Variable root; path }
  | _ -> None


let of_expression = function
  | { Node.value = Access (SimpleAccess access); _ } -> of_accesses access
  | _ -> None


type normalized_expression =
  | Access of { expression: normalized_expression; member: Identifier.t }
  | Call of { callee: normalized_expression; arguments: Argument.t list Node.t }
  | Index of
      { expression: normalized_expression;
        index: AbstractTreeDomain.Label.t;
        original: Identifier.t;
        arguments: Expression.t Argument.record list Node.t
      }
  | Global of Access.t
  | Local of Identifier.t
  | Expression of Expression.t
[@@deriving eq, show { with_path = false }]

let is_get_item member = member = "__getitem__"

let get_index { Node.value = expression; _ } =
  match expression with
  | String literal -> AbstractTreeDomain.Label.Field literal.value
  | Integer i -> AbstractTreeDomain.Label.Field (string_of_int i)
  | _ -> AbstractTreeDomain.Label.Any


let normalize_access_list left = function
  | Access.Identifier member -> Access { expression = left; member }
  | Access.Call ({ value = [argument]; _ } as arguments) -> (
    match left with
    | Access { expression; member } when is_get_item member ->
        Index { expression; index = get_index argument.value; original = member; arguments }
    | _ -> Call { callee = left; arguments } )
  | Access.Call arguments -> Call { callee = left; arguments }


let global_prefix ~resolution access =
  let rec module_prefix ~lead ~tail =
    match tail with
    | (Access.Identifier _ as identifier) :: new_tail ->
        let new_lead = lead @ [identifier] in
        let reference = Reference.from_access lead in
        if Resolution.module_definition resolution reference |> Option.is_some then
          module_prefix ~lead:new_lead ~tail:new_tail
        else
          lead, tail
    | _ -> lead, tail
  in
  module_prefix ~lead:[] ~tail:access


let split_root ~resolution = function
  | Access.Identifier identifier :: rest when Interprocedural.CallResolution.is_local identifier ->
      Local identifier, rest
  | Access.Identifier _ :: _ as access ->
      let prefix, rest = global_prefix access ~resolution in
      Global prefix, rest
  | Access.Call _ :: _ -> failwith "invalid root (call) in access"
  | _ -> failwith "empty access"


let resolve_exports ~resolution = function
  | Access.SimpleAccess access ->
      let simple_prefix, suffix =
        List.split_while
          ~f:(function
            | Access.Identifier _ -> true
            | _ -> false)
          access
      in
      let prefix =
        Reference.from_access simple_prefix
        |> TypeCheck.resolve_exports ~resolution
        |> Reference.access
      in
      Access.SimpleAccess (prefix @ suffix)
  | expression -> expression


let normalize_access ~resolution (access : Access.general_access) =
  (* TODO(T42218730): should we also handle redirects here? *)
  match resolve_exports ~resolution access with
  | Access.SimpleAccess access ->
      let root, rest = split_root access ~resolution in
      List.fold rest ~init:root ~f:normalize_access_list
  | Access.ExpressionAccess { expression; access; _ } ->
      List.fold access ~init:(Expression expression) ~f:normalize_access_list


let rec as_access = function
  | Global access -> Expression.Access.SimpleAccess access
  | Local identifier ->
      Expression.Access.SimpleAccess (Access.create_from_identifiers [identifier])
  | Expression expression -> Expression.Access.ExpressionAccess { expression; access = [] }
  | Call { callee; arguments } ->
      let callee = Expression.Access (as_access callee) in
      Expression.Access.combine (Node.create_with_default_location callee) [Access.Call arguments]
  | Access { expression; member } ->
      let left = Expression.Access (as_access expression) in
      Expression.Access.combine (Node.create_with_default_location left) [Access.Identifier member]
  | Index { expression; original; arguments; _ } ->
      let left = Expression.Access (as_access expression) in
      Expression.Access.combine
        (Node.create_with_default_location left)
        [Access.Identifier original; Access.Call arguments]


let to_json { root; path } =
  let open Root in
  let root_name = function
    | LocalResult -> "result"
    | PositionalParameter { position = _; name } -> Format.sprintf "formal(%s)" name
    | NamedParameter { name } -> Format.sprintf "formal(%s)" name
    | StarParameter { position } -> Format.sprintf "formal(*rest%d)" position
    | StarStarParameter _ -> "formal(**kw)"
    | Variable name -> Format.sprintf "local(%s)" name
  in
  `String (root_name root ^ AbstractTreeDomain.Label.show_path path)


let is_property_access ~resolution ~expression:{ Node.location; value = expression } =
  match expression with
  | Access { expression; member } ->
      let access = as_access expression in
      let annotation =
        Node.create ~location (Expression.Access access) |> Resolution.resolve resolution
      in
      let is_property define =
        String.Set.exists ~f:(Statement.Define.has_decorator define) Recognized.property_decorators
      in
      Type.access annotation @ [Identifier member]
      |> Reference.from_access
      |> Resolution.function_definitions resolution
      >>| (function
            | [{ Node.value = define; _ }] -> is_property define
            | _ -> false)
      |> Option.value ~default:false
  | _ -> false
