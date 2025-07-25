(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module allows storing the type of an expression and its attribute chains after refinement.
 *
 * For example, say we have an expression `foo` of type `Foo`, having attribute `bar:
 * Optional[Bar]`, with `Bar` having attribute `baz: Optional[Baz]`.
 *
 * If we see an if-statement that refines the type, such as:
 *
 * `if foo.bar is not None and foo.bar.baz is not None:`
 *
 * then, within that block, we would store the information that `foo.bar` is of type `Bar` (not
 * `Optional[Bar]`) and `foo.bar.baz` is of type `Baz` (not `Optional[Baz]`).
 *
 * We store the above for arbitrary chains of attributes starting from the base expression `foo`,
 * leading to a tree of attributes (`foo -> bar -> baz`, `foo -> hello -> world`, etc.).
 *
 * The submodules handle this at different granularity:
 * - TypeInfo.Unit: This represents the smallest unit of type info, one "name" which might
 *   represent a global or local variable, or might be a chain of attribute accesses.
 * - TypeInfo.LocalOrGlobal: This represents a given local or global variable, together
 *   with any attribute access chains, in a rose tree format.
 * - TypeInfo.Store: This represents the entire set of live type information as of analyzing
 *   one specific expression. We store this type info in two separate trees:
 *   - permanent type information representing either "primary" types coming
 *     from type inference / explicit annotations and refinements of final
 *     variables and attributes.
 *   - temporary type information representing "temporary refinements" which are an
 *     unsound attempt to allow "mostly" safe refinements of mutable attributes. We wipe
 *     temporary anntoations on the first function call or `await`, which is an attempt
 *     to minimize the likelihood of the narrowed type being invalidated by mutation.
 *     (This mostly works in the absence of free threading).
 * - TypeInfo.AroundStatement: This represents what we need to deal with type information in
 *   a statement, given the way Pyre currently handles statements using a fixpoint. It contains
 *   two `Store.t` values, one with the info *before* resolving the statement and another with
 *   the info *after*.
 * - TypeInfo.ForFunctionBody: This represent a map of the `AroundStatement.t` values for
 *   every state in a function body. The representation is a map whose keys are statement ids
 *   coming from the `Cfg`.
 *
 * For example, if we know that an attribute `bar` is final (e.g., it has type `Final[...]` or is
 * part of a frozen dataclass), then it is safe to permanently refine `foo.bar` as non-`None`, since
 * that attribute cannot be set to `None` again.
 *
 * However, if the attribute is not final, then we only mark it temporarily as non-`None`. Any
 * intervening function call on that expression will end up removing these temporary refinements,
 * since that function *could* have set the attribute to `None`.
 *)

open Core
open Ast
open Pyre
open LatticeOfMaps

module Mutability = struct
  type t =
    | Mutable
    | Immutable of {
        original: Type.t;
        final: bool;
      }
  [@@deriving compare, equal, hash, sexp]

  let pp format = function
    | Mutable -> Format.fprintf format "m"
    | Immutable { original; final } ->
        let final =
          match final with
          | true -> " (final)"
          | _ -> ""
        in
        Format.fprintf format " (%a)%s" Type.pp original final


  let transform_types ~f = function
    | Mutable -> Mutable
    | Immutable { original; final } -> Immutable { original = f original; final }


  let less_or_equal ~left ~right =
    match left, right with
    | Mutable, _
    (* we don't have to look at original or final because they will be the same *)
    | Immutable _, Immutable _ ->
        true
    | Immutable _, Mutable -> false


  let join ~type_join left right =
    match left, right with
    | Immutable left, Immutable right ->
        Immutable
          { original = type_join left.original right.original; final = left.final || right.final }
    | (Immutable _ as immutable), Mutable
    | Mutable, (Immutable _ as immutable) ->
        immutable
    | Mutable, Mutable -> Mutable


  let meet ~type_meet left right =
    match left, right with
    | Mutable, _
    | _, Mutable ->
        Mutable
    | Immutable left, Immutable right ->
        Immutable
          { original = type_meet left.original right.original; final = left.final && right.final }
end

module Unit = struct
  type t = {
    annotation: Type.t;
    mutability: Mutability.t;
  }
  [@@deriving compare, equal, hash, sexp]

  let pp format { annotation; mutability } =
    Format.fprintf format "(%a: %a)" Type.pp annotation Mutability.pp mutability


  let show = Format.asprintf "%a" pp

  let display_as_revealed_type { annotation; mutability } =
    match mutability with
    | Mutable -> Format.asprintf "`%a`" Type.pp annotation
    | Immutable { original; final; _ } ->
        let if_final display = if final then display else "" in
        if Type.contains_unknown original then
          Format.asprintf "`%a`%s" Type.pp annotation (if_final " (final)")
        else if Type.equal annotation original then
          Format.asprintf "`%a`%s" Type.pp original (if_final " (final)")
        else
          Format.asprintf
            "`%a` (inferred: `%a`%s)"
            Type.pp
            original
            Type.pp
            annotation
            (if_final ", final")


  let create_mutable annotation = { annotation; mutability = Mutable }

  let create_immutable ?(original = None) ?(final = false) annotation =
    let original = Option.value ~default:annotation original in
    { annotation; mutability = Immutable { original; final } }


  let annotation { annotation; _ } = annotation

  let original { annotation; mutability } =
    match mutability with
    | Immutable { original; _ } -> original
    | Mutable -> annotation


  let is_immutable { mutability; _ } = not (Mutability.equal mutability Mutable)

  let is_final { mutability; _ } =
    match mutability with
    | Immutable { final; _ } -> final
    | Mutable -> false


  let transform_types ~f { annotation; mutability } =
    { annotation = f annotation; mutability = Mutability.transform_types ~f mutability }


  let dequalify dequalify_map annotation =
    let dequalify = Type.dequalify dequalify_map in
    transform_types ~f:dequalify annotation


  let less_or_equal ~type_less_or_equal ~left ~right =
    Mutability.less_or_equal ~left:left.mutability ~right:right.mutability
    && type_less_or_equal ~left:left.annotation ~right:right.annotation


  let join ~type_join left right =
    {
      annotation = type_join left.annotation right.annotation;
      mutability = Mutability.join ~type_join left.mutability right.mutability;
    }


  let meet ~type_meet left right =
    {
      annotation = type_meet left.annotation right.annotation;
      mutability = Mutability.meet ~type_meet left.mutability right.mutability;
    }


  let refine ~type_less_or_equal ~solve_less_or_equal ~refined_type { annotation; mutability } =
    let accept_refinement_of_immutable original =
      (not (Type.is_unbound refined_type)) && type_less_or_equal ~left:refined_type ~right:original
    in
    match mutability with
    | Mutable -> { annotation = refined_type; mutability }
    | Immutable { original; _ } -> (
        match refined_type with
        | Type.Top -> { annotation = Type.Top; mutability }
        | Type.Bottom -> { annotation; mutability }
        | _ when accept_refinement_of_immutable original ->
            { annotation = refined_type; mutability }
        | _ ->
            {
              annotation =
                solve_less_or_equal ~left:refined_type ~right:original
                |> Option.value ~default:annotation;
              mutability;
            })


  let join_forcing_union ~type_join left right =
    let refined = join ~type_join left right in
    { refined with annotation = Type.union [left.annotation; right.annotation] }
end

module LocalOrGlobal = struct
  type t = {
    base: Unit.t option;
    attributes: t Identifier.Map.Tree.t;
  }
  [@@deriving equal]

  let empty = { base = None; attributes = IdentifierMap.empty }

  let create base = { empty with base = Some base }

  let create_mutable type_ = create (Unit.create_mutable type_)

  let top = create (Unit.create_mutable Type.Top)

  let rec pp format { base; attributes } =
    let attribute_map_entry (identifier, refinement_unit) =
      Format.asprintf "%a -> %a" Identifier.pp identifier pp refinement_unit
    in
    (match base with
    | Some base -> Format.fprintf format "[Base: %a; " Unit.pp base
    | None -> Format.fprintf format "[Base: (); ");
    Map.Tree.to_alist attributes
    |> List.map ~f:attribute_map_entry
    |> String.concat ~sep:", "
    |> Format.fprintf format "Attributes: [%s]]"


  let show = Format.asprintf "%a" pp

  let find = Identifier.Map.Tree.find

  let base { base; _ } = base

  let set_base refinement_unit ~base = { refinement_unit with base = Some base }

  let set_base_if_none refinement_unit ~base =
    { refinement_unit with base = Option.first_some refinement_unit.base base }


  (** If `attribute_path` is empty, set the base type_info. Otherwise, find the appropriate
      attribute (traversing intermediate units and constructing new ones as needed) and set the base
      there. *)
  let set_type_info ?(wipe_subtree = false) ~attribute_path ~type_info refinement_unit =
    let rec recurse ~type_info ~identifiers ({ attributes; _ } as refinement_unit) =
      match identifiers with
      | [] ->
          if wipe_subtree then
            { empty with base = Some type_info }
          else
            { refinement_unit with base = Some type_info }
      | identifier :: identifiers ->
          {
            refinement_unit with
            attributes =
              attributes
              |> IdentifierMap.set
                   ~key:identifier
                   ~data:
                     (find attributes identifier
                     |> Option.value ~default:empty
                     |> recurse ~type_info ~identifiers);
          }
    in
    recurse ~type_info ~identifiers:(attribute_path |> Reference.as_list) refinement_unit


  (** If `attribute_path` is empty, get the base type_info. Otherwise, find the appropriate
      attribute (traversing intermediate units until we finish or hit a dead end) and return the
      base found there, if any *)
  let get_type_info ~attribute_path refinement_unit =
    let rec recurse { base; attributes } ~identifiers =
      match identifiers with
      | [] -> base
      | identifier :: identifiers -> (
          match find attributes identifier with
          | Some refinement_unit -> recurse refinement_unit ~identifiers
          | None -> None)
    in
    recurse refinement_unit ~identifiers:(attribute_path |> Reference.as_list)


  let rec less_or_equal ~type_less_or_equal ~left ~right =
    let base_less_or_equal left_base right_base =
      match left_base, right_base with
      | Some left, Some right -> Unit.less_or_equal ~type_less_or_equal ~left ~right
      | None, None -> true (* intermediate refinement units don't require computation *)
      | _ -> false
    in
    let less_or_equal_one = less_or_equal ~type_less_or_equal in
    base_less_or_equal left.base right.base
    && IdentifierMap.less_or_equal ~less_or_equal_one ~left:left.attributes ~right:right.attributes


  let rec join ~type_join left right =
    let base =
      match left.base, right.base with
      | Some left_base, Some right_base -> Some (Unit.join ~type_join left_base right_base)
      | _ -> None
    in
    {
      base;
      attributes = IdentifierMap.join ~join_one:(join ~type_join) left.attributes right.attributes;
    }


  let rec meet ~type_meet left right =
    let base =
      match left.base, right.base with
      | Some left_base, Some right_base -> Some (Unit.meet ~type_meet left_base right_base)
      | Some base, None
      | None, Some base ->
          Some base
      | None, None -> None
    in
    {
      base;
      attributes = IdentifierMap.meet ~meet_one:(meet ~type_meet) left.attributes right.attributes;
    }


  let widen ~type_join ~widening_threshold ~iteration left right =
    if iteration + 1 >= widening_threshold then
      create (Unit.create_mutable Type.Top)
    else
      join ~type_join left right

  (* TODO() Explore whether this is really necessary - why would we want to force a union? *)
end

module Store = struct
  type t = {
    type_info: LocalOrGlobal.t Reference.Map.Tree.t;
    temporary_type_info: LocalOrGlobal.t Reference.Map.Tree.t;
  }
  [@@deriving equal]

  let empty = { type_info = ReferenceMap.empty; temporary_type_info = ReferenceMap.empty }

  let pp format { type_info; temporary_type_info } =
    let show_type_info (reference, unit) =
      Format.asprintf "%a -> %a" Reference.pp reference LocalOrGlobal.pp unit
    in
    Reference.Map.Tree.to_alist type_info
    |> List.map ~f:show_type_info
    |> String.concat ~sep:", "
    |> Format.fprintf format "type_info: [%s]\n";
    Reference.Map.Tree.to_alist temporary_type_info
    |> List.map ~f:show_type_info
    |> String.concat ~sep:", "
    |> Format.fprintf format "temporary_type_info: [%s]"


  let show = Format.asprintf "%a" pp

  let print_as_json formatter { type_info; temporary_type_info } =
    let pp_element ~temporary ~key ~data =
      let temporary_suffix = if temporary then "(temp)" else "" in
      Format.fprintf
        formatter
        "\"%a\": \"%a\"%s, "
        Reference.pp
        key
        LocalOrGlobal.pp
        data
        temporary_suffix
    in
    Format.fprintf formatter "{";
    Reference.Map.Tree.iteri type_info ~f:(pp_element ~temporary:false);
    Reference.Map.Tree.iteri temporary_type_info ~f:(pp_element ~temporary:true);
    Format.fprintf formatter "}"


  let has_nontemporary_type_info ~name { type_info; _ } = ReferenceMap.mem type_info name

  (** Map an operation over what's at a given name. If there's nothing already existing, use
      `empty`.

      The way we handle temporary vs non-temporary is very particular:

      - If `temporary` is true we only apply this to `temporary_type_info`
      - Otherwise, we apply it to `type_info` and also apply it to any *existing* data in
        `temporary_type_info`, but we don't create any new `temporary_type_info`.
      - The idea here is to minimize the amount of duplicated data, but ensure that `type_info` and
        `temporary_type_info` always have a consistent view of (non-temporary) refinements. *)
  let map_over_name ~temporary ~name ~f { type_info; temporary_type_info } =
    let map_over_reference_map ~fallback reference_map =
      match Option.first_some (ReferenceMap.find reference_map name) fallback with
      | Some unit -> ReferenceMap.set ~key:name ~data:(f unit) reference_map
      | None -> reference_map
    in
    if temporary then
      {
        type_info;
        temporary_type_info =
          map_over_reference_map ~fallback:(Some LocalOrGlobal.empty) temporary_type_info;
      }
    else
      {
        type_info = map_over_reference_map ~fallback:(Some LocalOrGlobal.empty) type_info;
        temporary_type_info = map_over_reference_map ~fallback:None temporary_type_info;
      }


  let select_nontemporary_or_temporary (nontemporary, temporary) =
    match nontemporary, temporary with
    | None, info -> info
    | info, None -> info
    | Some info, Some _ ->
        (* Nontemporary refinements can get mirrored into temporary refinements. Prefer the
           nontemporary if we find both. *)
        Some info


  let get_base ~name { type_info; temporary_type_info } =
    let get_type_info_from type_info_map =
      Reference.Map.Tree.find type_info_map name >>= LocalOrGlobal.base
    in
    (get_type_info_from type_info, get_type_info_from temporary_type_info)
    |> select_nontemporary_or_temporary


  let get_type_info ~name ~attribute_path { type_info; temporary_type_info } =
    let get_type_info_from type_info_map =
      Reference.Map.Tree.find type_info_map name >>= LocalOrGlobal.get_type_info ~attribute_path
    in
    (get_type_info_from type_info, get_type_info_from temporary_type_info)
    |> select_nontemporary_or_temporary


  let set_base ?(temporary = false) ~name ~base store =
    map_over_name ~temporary ~name ~f:(LocalOrGlobal.set_base ~base) store


  let new_as_base ?(temporary = false) ~name ~base { type_info; temporary_type_info } =
    if temporary then
      {
        type_info;
        temporary_type_info =
          ReferenceMap.set temporary_type_info ~key:name ~data:(LocalOrGlobal.create base);
      }
    else
      {
        type_info = ReferenceMap.set type_info ~key:name ~data:(LocalOrGlobal.create base);
        temporary_type_info = ReferenceMap.remove temporary_type_info name;
      }


  let set_type_info
      ?(temporary = false)
      ?(wipe_subtree = false)
      ~name
      ~attribute_path
      ~base_type_info
      ~type_info
      store
    =
    let set_type_info_unit unit =
      unit
      |> LocalOrGlobal.set_type_info ~wipe_subtree ~attribute_path ~type_info
      |> LocalOrGlobal.set_base_if_none ~base:base_type_info
    in
    map_over_name ~temporary ~name ~f:set_type_info_unit store


  let less_or_equal ~type_less_or_equal ~left ~right =
    let less_or_equal_one = LocalOrGlobal.less_or_equal ~type_less_or_equal in
    ReferenceMap.less_or_equal ~less_or_equal_one ~left:left.type_info ~right:right.type_info
    && ReferenceMap.less_or_equal
         ~less_or_equal_one
         ~left:left.temporary_type_info
         ~right:right.temporary_type_info


  (** Whenever we know for sure that right is pointwise less_or_equal to left, then we can save
      computation by only checking for equality pointwise, which doesn't require type ordering
      operations *)
  let less_or_equal_monotone ~left ~right =
    let less_or_equal_one ~left ~right = LocalOrGlobal.equal left right in
    ReferenceMap.less_or_equal ~less_or_equal_one ~left:left.type_info ~right:right.type_info
    && ReferenceMap.less_or_equal
         ~less_or_equal_one
         ~left:left.temporary_type_info
         ~right:right.temporary_type_info


  let meet ~type_meet left right =
    let meet_one = LocalOrGlobal.meet ~type_meet in
    {
      type_info = ReferenceMap.meet ~meet_one left.type_info right.type_info;
      temporary_type_info =
        ReferenceMap.meet ~meet_one left.temporary_type_info right.temporary_type_info;
    }


  (** Use an "outer" join to join or widen stores, which means we are strict about types (a proper
      join) but permissive about variables that might only be instantiated on one side.

      This can be done as either a join or a widen depending whether we set `widening_threshod`,
      which is applied at the `TypeInfo.LocalOrGlobal` level. *)
  let widen_or_join ~merge_one left right =
    {
      (* Newly-instantiated locals live in `type_info`, so we merge with join *)
      type_info = ReferenceMap.merge_with ~merge_one left.type_info right.type_info;
      (* `temporary_type_info` only has type info, so we do a proper join *)
      temporary_type_info =
        ReferenceMap.join ~join_one:merge_one left.temporary_type_info right.temporary_type_info;
    }


  let outer_join ~type_join =
    let merge_one = LocalOrGlobal.join ~type_join in
    widen_or_join ~merge_one


  let outer_widen ~type_join ~iteration ~widening_threshold =
    let merge_one = LocalOrGlobal.widen ~type_join ~iteration ~widening_threshold in
    widen_or_join ~merge_one


  let update_existing ~old_store ~new_store =
    {
      type_info =
        ReferenceMap.update_existing_entries
          ~map_to_update:old_store.type_info
          ~new_map:new_store.type_info;
      temporary_type_info =
        ReferenceMap.update_existing_entries
          ~map_to_update:old_store.temporary_type_info
          ~new_map:new_store.temporary_type_info;
    }


  let update_with_filter ~old_store ~new_store ~filter =
    let update_map old_map new_map =
      let f ~key ~data sofar =
        if LocalOrGlobal.base data |> Option.map ~f:(filter key) |> Option.value ~default:false then
          sofar |> ReferenceMap.set ~key ~data
        else
          sofar
      in
      ReferenceMap.fold ~init:old_map ~f new_map
    in
    {
      type_info = update_map old_store.type_info new_store.type_info;
      temporary_type_info = update_map old_store.temporary_type_info new_store.temporary_type_info;
    }
end

module AroundStatement = struct
  type t = {
    precondition: Store.t;
    postcondition: Store.t;
  }
  [@@deriving equal]

  let pp formatter { precondition; postcondition } =
    Format.fprintf
      formatter
      "{ \"Precondition\": %a, \"Postcondition\": %a}"
      Store.print_as_json
      precondition
      Store.print_as_json
      postcondition
end

module ForFunctionBody = struct
  (* Maps a key, unique to each statement for a function CFG, to type info. The statement key is
     computed from a tuple CFG node ID and and statement index (see Fixpoint.forward) *)
  type t = AroundStatement.t Int.Table.t

  let equal left right = Core.Hashtbl.equal AroundStatement.equal left right

  let empty () = Int.Table.create ()

  let pp formatter statements =
    let pp_map formatter iterator pp_key map =
      Format.fprintf formatter "{ ";
      let pp_map_entry ~key ~data =
        Format.fprintf formatter "%a: %a" pp_key key AroundStatement.pp data
      in
      iterator map ~f:pp_map_entry;
      Format.fprintf formatter " }"
    in
    pp_map formatter Hashtbl.iteri Int.pp statements


  let show map = Format.asprintf "%a" pp map

  let set
      ?(precondition =
        Store.
          { type_info = Reference.Map.Tree.empty; temporary_type_info = Reference.Map.Tree.empty })
      ?(postcondition =
        Store.
          { type_info = Reference.Map.Tree.empty; temporary_type_info = Reference.Map.Tree.empty })
      ~statement_key
      type_info_for_function
    =
    Hashtbl.set
      type_info_for_function
      ~key:statement_key
      ~data:{ AroundStatement.precondition; postcondition }


  module StatementIdMap = struct
    module StatementId = struct
      type t = int [@@deriving compare, sexp, hash, to_yojson]
    end

    include Map.Make_tree (struct
      include StatementId
      include Comparator.Make (StatementId)
    end)
  end

  module ReadOnly = struct
    type t = AroundStatement.t StatementIdMap.t [@@deriving equal]

    let get_precondition type_info_for_function ~statement_key =
      StatementIdMap.find type_info_for_function statement_key
      >>| fun { AroundStatement.precondition; _ } -> precondition


    let get_postcondition type_info_for_function ~statement_key =
      StatementIdMap.find type_info_for_function statement_key
      >>| fun { AroundStatement.postcondition; _ } -> postcondition
  end

  let read_only type_info_for_function =
    Hashtbl.to_alist type_info_for_function |> StatementIdMap.of_alist_exn
end
