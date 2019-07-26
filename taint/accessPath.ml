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
    | PositionalParameter of {
        position: int;
        name: Identifier.t;
      }
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
      else if Identifier.equal (Identifier.sanitized prefixed_name_string) "*" then
        (* This is not a real starred argument, but instead is just the indicator for the beginning
         * of the keyword only arguments *)
        true, excluded, normalized
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
  formal_path: AbstractTreeDomain.Label.path;
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
          {
            root = formal;
            actual_path = [];
            formal_path = [AbstractTreeDomain.Label.create_name_field field_name];
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
          {
            root = formal;
            actual_path = [AbstractTreeDomain.Label.create_int_field (position - actual_position)];
            formal_path = [];
          }
    | `Star (`Approximate minimal_position), PositionalParameter { position; _ }
      when minimal_position <= position ->
        Some { root = formal; actual_path = [AbstractTreeDomain.Label.Any]; formal_path = [] }
    | `Precise actual_position, StarParameter { position } when actual_position >= position ->
        Some
          {
            root = formal;
            actual_path = [];
            formal_path = [AbstractTreeDomain.Label.create_int_field (actual_position - position)];
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
  let match_actual (position, matches) { Call.Argument.name; value } =
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
  path: AbstractTreeDomain.Label.path;
}
[@@deriving show { with_path = false }, eq]

let create root path = { root; path }

let extend { root; path = original_path } ~path = { root; path = original_path @ path }

let get_index { Node.value = expression; _ } =
  match expression with
  | String literal -> AbstractTreeDomain.Label.Field literal.value
  | Integer i -> AbstractTreeDomain.Label.Field (string_of_int i)
  | Name name -> (
      let name = Expression.name_to_reference name >>| Reference.delocalize >>| Reference.last in
      match name with
      (* Heuristic: All uppercase names tend to be enums, so only taint the field in those cases. *)
      | Some name
        when String.for_all name ~f:(fun character ->
                 (not (Char.is_alpha character)) || Char.is_uppercase character) ->
          AbstractTreeDomain.Label.Field name
      | _ -> AbstractTreeDomain.Label.Any )
  | _ -> AbstractTreeDomain.Label.Any


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


let get_global ~resolution name =
  let global_resolution = Resolution.global_resolution resolution in
  let global =
    match Node.value name with
    | Name (Name.Identifier identifier)
      when not (Interprocedural.CallResolution.is_local identifier) ->
        Some (Reference.create identifier)
    | Name (Name.Identifier identifier) ->
        let reference = Reference.delocalize (Reference.create identifier) in
        if Resolution.is_global resolution ~reference then
          Some reference
        else
          None
    | Name (Name.Attribute { base = { Node.value = Name base_name; _ }; _ } as name) ->
        let name = Expression.name_to_reference name in
        let base_name = Expression.name_to_reference base_name in
        let is_module name =
          name >>= GlobalResolution.module_definition global_resolution |> Option.is_some
        in
        name >>= Option.some_if (is_module base_name && not (is_module name))
    | _ -> None
  in
  global >>| fun reference -> GlobalResolution.resolve_exports global_resolution ~reference


let is_global ~resolution name = Option.is_some (get_global ~resolution name)

let of_expression ~resolution = function
  | { Node.value = Name _; _ } as expression ->
      let expression =
        if is_global ~resolution expression then
          Ast.Expression.delocalize expression
        else
          expression
      in
      let rec of_expression path = function
        | { Node.value = Name (Name.Identifier identifier); _ } ->
            Some { root = Root.Variable identifier; path }
        | { Node.value = Name (Name.Attribute { base; attribute; _ }); _ } ->
            let path = AbstractTreeDomain.Label.Field attribute :: path in
            of_expression path base
        | _ -> None
      in
      of_expression [] expression
  | _ -> None
