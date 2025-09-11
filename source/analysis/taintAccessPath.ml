(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* AccessPath:
 *
 * Defines an access path, which is a subset of python expressions.
 *
 * Access paths are used to represent the model of a callable, which describes
 * the set of sources produced by the callable, and the set of sinks reachable
 * from the parameters of the callable.
 *
 * This also implements the logic to match actual parameters at call sites to
 * the formal parameters of a callable.
 *)

open Core
open Ast
open Expression

(** Roots representing parameters, locals, and special return value in models. *)
module Root = struct
  module T = struct
    type t =
      | LocalResult (* Special root representing the return value location. *)
      | PositionalParameter of {
          position: int;
          name: Identifier.t;
          positional_only: bool;
        }
      | NamedParameter of { name: Identifier.t }
      | StarParameter of { position: int }
      | StarStarParameter of { excluded: Identifier.t list }
      | Variable of Identifier.t
      | CapturedVariable of { name: Identifier.t }
    [@@deriving compare, equal, hash, sexp]

    let parameter_prefix = "$parameter$"

    let chop_parameter_prefix name =
      match String.chop_prefix ~prefix:parameter_prefix name with
      | Some chopped -> chopped
      | None -> name


    let prepend_parameter_prefix = String.append parameter_prefix

    let is_parameter = function
      | PositionalParameter _
      | NamedParameter _
      | StarParameter _
      | StarStarParameter _ ->
          true
      | _ -> false


    let parameter_name = function
      | PositionalParameter { name; _ }
      | NamedParameter { name } ->
          Some name
      | StarParameter _ -> Some "*"
      | StarStarParameter _ -> Some "**"
      | _ -> None


    let pp formatter = function
      | LocalResult -> Format.fprintf formatter "result"
      | PositionalParameter { position; name; positional_only } ->
          Format.fprintf
            formatter
            "formal(%s, position=%d%s)"
            name
            position
            (if positional_only then ", positional_only" else "")
      | NamedParameter { name } -> Format.fprintf formatter "formal(%s)" name
      | StarParameter { position } -> Format.fprintf formatter "formal(*args, position=%d)" position
      | StarStarParameter { excluded = [] } -> Format.fprintf formatter "formal(**kwargs)"
      | StarStarParameter { excluded } ->
          Format.fprintf
            formatter
            "formal(**kwargs, excluded=[%s])"
            (String.concat ~sep:"," excluded)
      | Variable name -> Format.fprintf formatter "local(%s)" name
      | CapturedVariable { name } -> Format.fprintf formatter "captured_variable(%s)" name


    let show = Format.asprintf "%a" pp

    (* Export for issue handles. This must be stable so handles are consistent between runs. *)
    let pp_for_issue_handle formatter = function
      | LocalResult -> Format.fprintf formatter "result"
      | PositionalParameter { name; _ } -> Format.fprintf formatter "formal(%s)" name
      | NamedParameter { name } -> Format.fprintf formatter "formal(%s)" name
      | StarParameter { position } -> Format.fprintf formatter "formal(*rest%d)" position
      | StarStarParameter _ -> Format.fprintf formatter "formal(**kw)"
      | Variable name -> Format.fprintf formatter "local(%s)" name
      | CapturedVariable { name } -> Format.fprintf formatter "captured_variable(%s)" name


    let show_for_issue_handle = Format.asprintf "%a" pp_for_issue_handle

    (* For backward compatibility, use the issue handle ports *)
    let pp_for_via_breadcrumb = pp_for_issue_handle

    let show_for_via_breadcrumb = Format.asprintf "%a" pp_for_via_breadcrumb

    let variable_to_captured_variable = function
      | Variable name -> CapturedVariable { name }
      | root -> root


    let captured_variable_to_variable = function
      | CapturedVariable { name; _ } -> Variable name
      | root -> root


    let is_captured_variable = function
      | CapturedVariable _ -> true
      | _ -> false


    (* Represent the origins of triggered sinks for string combine functions. *)
    let sink_port_in_string_combine_functions = StarStarParameter { excluded = [] }
  end

  include T
  module Set = Stdlib.Set.Make (T)

  module List = struct
    type t = T.t list [@@deriving show]
  end
end

module NormalizedParameter = struct
  type t = {
    root: Root.t;
    (* Qualified name (prefixed with `$parameter$`), ignoring stars. *)
    qualified_name: Identifier.t;
    original: Parameter.t;
  }
end

let normalize_parameters parameters =
  let has_pep570_syntax =
    List.find parameters ~f:(fun { Node.value = { Parameter.name = qualified_name; _ }; _ } ->
        Identifier.equal (Identifier.sanitized qualified_name) "/"
        || Identifier.equal (Identifier.sanitized qualified_name) "*")
    |> Option.is_some
  in
  let normalize_parameters
      (position, seen_star, excluded, normalized)
      ({ Node.value = { Parameter.name = qualified_name; _ }; _ } as original)
    =
    if Identifier.equal (Identifier.sanitized qualified_name) "/" then
      let mark_as_positional_only ({ NormalizedParameter.root; _ } as parameter) =
        let root =
          match root with
          | Root.PositionalParameter { position; name; _ } ->
              Root.PositionalParameter { position; name; positional_only = true }
          | _ -> root
        in
        { parameter with root }
      in
      position, seen_star, excluded, List.map ~f:mark_as_positional_only normalized
    else if String.is_prefix ~prefix:"**" qualified_name then
      let qualified_name = String.chop_prefix_exn qualified_name ~prefix:"**" in
      ( position + 1,
        true,
        excluded,
        { NormalizedParameter.root = Root.StarStarParameter { excluded }; qualified_name; original }
        :: normalized )
    else if Identifier.equal (Identifier.sanitized qualified_name) "*" then
      (* This is not a real starred argument, but instead is just the indicator for the beginning
       * of the keyword only arguments *)
      position, true, excluded, normalized
    else if String.is_prefix ~prefix:"*" qualified_name then
      let qualified_name = String.chop_prefix_exn qualified_name ~prefix:"*" in
      ( position + 1,
        true,
        excluded,
        { NormalizedParameter.root = Root.StarParameter { position }; qualified_name; original }
        :: normalized )
    else if seen_star then
      let unqualified_name = Root.chop_parameter_prefix qualified_name in
      ( position + 1,
        true,
        unqualified_name :: excluded,
        {
          NormalizedParameter.root = Root.NamedParameter { name = unqualified_name };
          qualified_name;
          original;
        }
        :: normalized )
    else
      let unqualified_name = Root.chop_parameter_prefix qualified_name in
      let positional_only =
        (not seen_star) && (not has_pep570_syntax) && Identifier.is_private_name unqualified_name
      in
      ( position + 1,
        false,
        unqualified_name :: excluded,
        {
          NormalizedParameter.root =
            Root.PositionalParameter { position; name = unqualified_name; positional_only };
          qualified_name;
          original;
        }
        :: normalized )
  in
  let _, _, _, parameters = List.fold parameters ~f:normalize_parameters ~init:(0, false, [], []) in
  List.rev parameters


module Path = struct
  type t = Abstract.TreeDomain.Label.t list [@@deriving compare, sexp, hash]

  let pp = Abstract.TreeDomain.Label.pp_path

  let show = Abstract.TreeDomain.Label.show_path

  let compare = Abstract.TreeDomain.Label.compare_path ?cmp:None

  let equal = Abstract.TreeDomain.Label.equal_path

  let empty = []

  let is_empty = List.is_empty

  let is_prefix = Abstract.TreeDomain.Label.is_prefix
end

type argument_match = {
  root: Root.t;
  actual_path: Path.t;
  formal_path: Path.t;
}
[@@deriving compare, show { with_path = false }]

type argument_position =
  [ `Precise of int
  | `Approximate of int
  ]

(* Will preserve the order in which the arguments were matched to formals. *)
let match_actuals_to_formals arguments roots =
  let increment = function
    | `Precise n -> `Precise (n + 1)
    | `Approximate n -> `Approximate (n + 1)
  in
  let approximate = function
    | `Precise n -> `Approximate n
    | `Approximate n -> `Approximate n
  in
  let filter_to_named matched_names name_or_star_star formal =
    match name_or_star_star, formal with
    | `Name actual_name, (Root.NamedParameter { name } | Root.PositionalParameter { name; _ })
      when Identifier.equal actual_name name ->
        Some { root = formal; actual_path = []; formal_path = [] }
    | `Name actual_name, Root.StarStarParameter { excluded }
      when not (List.exists ~f:(Identifier.equal actual_name) excluded) ->
        let index = Root.chop_parameter_prefix actual_name in
        Some
          {
            root = formal;
            actual_path = [];
            formal_path = [Abstract.TreeDomain.Label.create_name_index index];
          }
    | `StarStar, Root.NamedParameter { name }
    | `StarStar, Root.PositionalParameter { name; _ }
      when not (Base.Set.mem matched_names name) ->
        Some
          { root = formal; actual_path = [Abstract.TreeDomain.Label.Index name]; formal_path = [] }
    | `StarStar, Root.StarStarParameter _ ->
        (* assume entire structure is passed. We can't just pick all but X fields. *)
        Some { root = formal; actual_path = []; formal_path = [] }
    | (`Name _ | `StarStar), _ -> None
  in
  let filter_to_positional position formal =
    let position = (position :> [ `Star of argument_position | argument_position ]) in
    match position, formal with
    | `Precise actual_position, Root.PositionalParameter { position; _ }
      when actual_position = position ->
        Some { root = formal; actual_path = []; formal_path = [] }
    | `Approximate minimal_position, Root.PositionalParameter { position; _ }
      when minimal_position <= position ->
        Some { root = formal; actual_path = []; formal_path = [] }
    | `Star (`Precise actual_position), Root.PositionalParameter { position; _ }
      when actual_position <= position ->
        Some
          {
            root = formal;
            actual_path = [Abstract.TreeDomain.Label.create_int_index (position - actual_position)];
            formal_path = [];
          }
    | `Star (`Approximate minimal_position), Root.PositionalParameter { position; _ }
      when minimal_position <= position ->
        Some { root = formal; actual_path = [Abstract.TreeDomain.Label.AnyIndex]; formal_path = [] }
    | `Precise actual_position, Root.StarParameter { position } when actual_position >= position ->
        Some
          {
            root = formal;
            actual_path = [];
            formal_path = [Abstract.TreeDomain.Label.create_int_index (actual_position - position)];
          }
    | `Approximate _, Root.StarParameter _ ->
        (* Approximate: We can't filter by minimal position in either direction here. Think about
           the two following cases:

           1. ```def f(x, y, *z): ... f( *[1, 2], approx) ``` In this case, even though the minimal
           position for approx is < the starred parameter, it will match to z.

           2. ```def f( *z): ... f( *[], 1, approx) ``` In this case, we'll have approx, which has a
           minimal position > the starred parameter, flow to z. *)
        Some { root = formal; actual_path = []; formal_path = [Abstract.TreeDomain.Label.AnyIndex] }
    | `Star _, Root.StarParameter _ ->
        (* Approximate: can't match up ranges, so pass entire structure *)
        Some { root = formal; actual_path = []; formal_path = [] }
    | (`Star _ | `Precise _ | `Approximate _), _ -> None
  in
  let match_actual matched_names (position, matches) ({ Call.Argument.name; value } as argument) =
    match name, value.Node.value with
    | None, Starred (Once _) ->
        let formals = List.filter_map roots ~f:(filter_to_positional (`Star position)) in
        approximate position, (argument, formals) :: matches
    | None, Starred (Twice _) ->
        let formals = List.filter_map roots ~f:(filter_to_named matched_names `StarStar) in
        position, (argument, formals) :: matches
    | None, _ ->
        let formals = List.filter_map roots ~f:(filter_to_positional position) in
        increment position, (argument, formals) :: matches
    | Some { value = name; _ }, _ ->
        let unqualified_name = Root.chop_parameter_prefix name in
        let formals =
          List.filter_map roots ~f:(filter_to_named matched_names (`Name unqualified_name))
        in
        position, (argument, formals) :: matches
  in
  let matched_names =
    let matched_named_arguments =
      List.map arguments ~f:(fun { Call.Argument.name; _ } -> name)
      |> List.filter_opt
      |> List.map ~f:Node.value
      |> List.map ~f:Identifier.sanitized
    in
    let matched_positional_arguments =
      let matched_positions =
        let unstarred_positional = function
          | { Call.Argument.name = Some _; _ }
          (* We deliberately ignore starred arguments here, and pessimistically assume they match 0
             parameters. *)
          | { Call.Argument.name = None; value = { Node.value = Starred _; _ } } ->
              false
          | { Call.Argument.name = None; _ } -> true
        in
        List.count arguments ~f:unstarred_positional
      in
      let get_matched_root = function
        (* Positions are 0 indexed, hence the < instead of <=. *)
        | Root.PositionalParameter { position; name; _ } when position < matched_positions ->
            Some name
        | _ -> None
      in
      List.filter_map roots ~f:get_matched_root
    in
    String.Set.of_list (matched_named_arguments @ matched_positional_arguments)
  in

  let _, result = List.fold arguments ~f:(match_actual matched_names) ~init:(`Precise 0, []) in
  List.rev result


let match_actuals_to_one_formal arguments root =
  match_actuals_to_formals arguments [root]
  |> List.filter_map ~f:(function
         | argument, [argument_match] -> Some (argument, argument_match)
         | _ -> None)


type t = {
  root: Root.t;
  path: Path.t;
}
[@@deriving compare, equal, sexp, hash]

let pp formatter { root; path } = Format.fprintf formatter "%a%a" Root.pp root Path.pp path

let show access_path = Format.asprintf "%a" pp access_path

let create root path = { root; path }

let extend { root; path = original_path } ~path = { root; path = original_path @ path }

(* Evaluates to the representation of literal strings, integers and enums. *)
let extract_constant_name { Node.value = expression; _ } =
  let open Option in
  match expression with
  | Expression.Constant (Constant.String literal) -> Some literal.value
  | Expression.Constant (Constant.Integer i) -> Some (string_of_int i)
  | Expression.Constant Constant.False -> Some "False"
  | Expression.Constant Constant.True -> Some "True"
  | Expression.Name name -> (
      let name = name_to_reference name >>| Reference.delocalize >>| Reference.last in
      match name with
      (* Heuristic: All uppercase names tend to be enums, so only taint the field in those cases. *)
      | Some name
        when String.for_all name ~f:(fun character ->
                 (not (Char.is_alpha character)) || Char.is_uppercase character) ->
          Some name
      | _ -> None)
  | _ -> None


let get_index expression =
  match extract_constant_name expression with
  | Some "True" -> Abstract.TreeDomain.Label.Index "1"
  | Some "False" -> Abstract.TreeDomain.Label.Index "0"
  | Some name -> Abstract.TreeDomain.Label.Index name
  | None -> Abstract.TreeDomain.Label.AnyIndex


let of_expression ~self_variable expression =
  let open Option.Monad_infix in
  let rec of_expression path = function
    | { Node.value = Expression.Name (Name.Identifier identifier); _ } ->
        Some { root = Root.Variable identifier; path }
    | { Node.value = Name (Name.Attribute { base; attribute; _ }); _ } ->
        let path = Abstract.TreeDomain.Label.Index attribute :: path in
        of_expression path base
    | {
        Node.value =
          Subscript
            {
              base;
              index = { Node.value = Expression.Constant (Constant.String { value; _ }); _ };
              origin = _;
            };
        _;
      } ->
        let path = Abstract.TreeDomain.Label.Index value :: path in
        of_expression path base
    | {
        Node.value =
          Call
            {
              Call.callee =
                {
                  Node.value =
                    Name
                      (Name.Attribute
                        { Name.Attribute.base; attribute = "__getitem__"; origin = _ });
                  _;
                };
              arguments =
                [
                  {
                    Call.Argument.name = None;
                    value = { Node.value = Expression.Constant (Constant.String { value; _ }); _ };
                  };
                ];
              origin = _;
            };
        _;
      } ->
        let path = Abstract.TreeDomain.Label.Index value :: path in
        of_expression path base
    | {
        Node.value =
          Call
            {
              Call.callee = { Node.value = Name (Name.Identifier "super"); _ };
              arguments = [_; { Call.Argument.name = None; value = argument }];
              origin = _;
            };
        _;
      } ->
        (* super(Foo, self) *)
        of_expression path argument
    | {
        Node.value =
          Call
            {
              Call.callee = { Node.value = Name (Name.Identifier "super"); _ };
              arguments = [];
              origin = _;
            };
        _;
      } ->
        self_variable >>| fun self_variable -> { root = self_variable; path }
    | _ -> None
  in
  of_expression [] expression


let dictionary_keys = Abstract.TreeDomain.Label.Field "**keys"
