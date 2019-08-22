(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Expression
open Statement
open Pyre
open CustomAnalysis
module Error = AnalysisError

module ErrorMap = struct
  type key = {
    location: Location.t;
    identifier: string;
  }
  [@@deriving compare, eq, sexp, show, hash]

  include Hashable.Make (struct
    type nonrec t = key

    let compare = compare_key

    let hash = Hashtbl.hash

    let hash_fold_t = hash_fold_key

    let sexp_of_t = sexp_of_key

    let t_of_sexp = key_of_sexp
  end)

  type t = Error.t Table.t
end

module type Context = sig
  val global_resolution : GlobalResolution.t

  val errors : ErrorMap.t
end

module NestedDefineLookup = struct
  type key = Define.t [@@deriving compare, eq, sexp, show, hash]

  include Hashable.Make (struct
    type nonrec t = key

    let compare = compare_key

    let hash = Hashtbl.hash

    let hash_fold_t = hash_fold_key

    let sexp_of_t = sexp_of_key

    let t_of_sexp = key_of_sexp
  end)

  type 'data t = 'data Table.t
end

module State (Context : Context) = struct
  type t = {
    used: Identifier.Set.t;
    define: Define.t Node.t;
    nested_define_lookup: t NestedDefineLookup.t;
  }

  let show { used; _ } = Set.to_list used |> String.concat ~sep:", "

  let pp format state = Format.fprintf format "%s" (show state)

  let initial ~lookup ~define =
    { used = Identifier.Set.empty; define; nested_define_lookup = lookup }


  let less_or_equal ~left:{ used = left; _ } ~right:{ used = right; _ } =
    Set.is_subset left ~of_:right


  let join left right = { left with used = Set.union left.used right.used }

  let widen ~previous ~next ~iteration:_ = join previous next

  let errors { used; define; _ } =
    let { Node.value = { Define.signature = { Define.parameters; _ }; _ }; _ } = define in
    let check_parameter { Node.value = { Parameter.name; _ }; location } =
      match Set.find used ~f:(Identifier.equal name) with
      | Some _ -> ()
      | None ->
          let error = Error.create ~location ~kind:(Error.DeadStore name) ~define in
          ErrorMap.Table.set
            Context.errors
            ~key:{ ErrorMap.location; identifier = name }
            ~data:error
    in
    List.iter ~f:check_parameter parameters;
    ErrorMap.Table.data Context.errors |> List.sort ~compare:Error.compare


  let forward ?key:_ state ~statement:_ = state

  let backward
      ?key
      ({ used; define; _ } as state)
      ~statement:({ Node.location; value } as statement)
    =
    let resolution =
      let { Node.value = { Define.signature = { name; parent; _ }; _ }; _ } = define in
      TypeCheck.resolution_with_key ~global_resolution:Context.global_resolution ~parent ~name ~key
    in
    (* Check for bottomed out state. *)
    let bottom =
      match value with
      | Assert { Assert.test = { Node.value = False; _ }; _ } -> true
      | Expression expression -> Type.is_noreturn (Resolution.resolve resolution expression)
      | Return _ -> true
      | _ -> false
    in
    let used = if bottom then Identifier.Set.empty else used in
    (* Remove assignments from used. *)
    let used =
      let remove_from_used ~used ~location identifier =
        match Set.find used ~f:(Identifier.equal identifier) with
        | Some _ -> Set.remove used identifier
        | None ->
            let error = Error.create ~location ~kind:(Error.DeadStore identifier) ~define in
            ErrorMap.Table.set Context.errors ~key:{ ErrorMap.location; identifier } ~data:error;
            used
      in
      match value with
      | Assign { target; _ } ->
          let rec update_target used = function
            | { Node.value = Name (Name.Identifier identifier); _ } ->
                remove_from_used ~used ~location identifier
            | { Node.value = List elements; _ } -> List.fold ~init:used ~f:update_target elements
            | { Node.value = Starred (Starred.Once target); _ } -> update_target used target
            | { Node.value = Tuple elements; _ } -> List.fold ~init:used ~f:update_target elements
            | _ -> used
          in
          update_target used target
      | _ -> used
    in
    (* Add used identifiers. *)
    let used =
      let used_names =
        match value with
        | Assign { annotation; value; _ } ->
            (* Don't count LHS of assignments as used. *)
            annotation
            >>| (fun annotation ->
                  Visit.collect_base_identifiers
                    (Node.create ~location (Statement.Expression annotation)))
            |> Option.value ~default:[]
            |> List.append
                 (Visit.collect_base_identifiers
                    (Node.create ~location (Statement.Expression value)))
            |> List.map ~f:Node.value
        | _ -> Visit.collect_base_identifiers statement |> List.map ~f:Node.value
      in
      List.fold used_names ~f:Set.add ~init:used
    in
    { state with used }
end

let name = "Liveness"

let run ~configuration:_ ~global_resolution ~source =
  let module Context = struct
    let global_resolution = global_resolution

    let errors = ErrorMap.Table.create ()
  end
  in
  let module State = State (Context) in
  let module Fixpoint = Fixpoint.Make (State) in
  let lookup = NestedDefineLookup.Table.create () in
  let define = Source.top_level_define_node source in
  let check define =
    let cfg = Cfg.create (Node.value define) in
    Fixpoint.backward ~cfg ~initial:(State.initial ~lookup ~define)
    |> Fixpoint.entry
    >>| (fun state ->
          NestedDefineLookup.Table.set lookup ~key:(Node.value define) ~data:state;
          State.errors state)
    |> Option.value ~default:[]
  in
  List.map ~f:check (nested_defines_deep_to_shallow define) |> ignore;
  check define
