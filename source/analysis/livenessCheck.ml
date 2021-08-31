(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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

module NestedDefineLookup = struct
  type key = Reference.t [@@deriving compare, eq, sexp, show, hash]

  include Hashable.Make (struct
    type nonrec t = key

    let compare = compare_key

    let hash = Hashtbl.hash

    let hash_fold_t = hash_fold_key

    let sexp_of_t = sexp_of_key

    let t_of_sexp = key_of_sexp
  end)

  type t = Identifier.Set.t Table.t
end

module type Context = sig
  val qualifier : Reference.t

  val environment : TypeEnvironment.ReadOnly.t

  val errors : ErrorMap.t

  val nested_define_lookup : NestedDefineLookup.t
end

module State (Context : Context) = struct
  type t = {
    used: Identifier.Set.t;
    define: Define.t Node.t;
    local_annotations: LocalAnnotationMap.ReadOnly.t option;
  }

  let show { used; _ } = Set.to_list used |> String.concat ~sep:", "

  let pp format state = Format.fprintf format "%s" (show state)

  let initial ~define =
    let local_annotations =
      TypeCheck.get_or_recompute_local_annotations
        ~environment:Context.environment
        (Node.value define |> Define.name |> Node.value)
    in
    { used = Identifier.Set.empty; define; local_annotations }


  let less_or_equal ~left:{ used = left; _ } ~right:{ used = right; _ } =
    Set.is_subset left ~of_:right


  let join left right = { left with used = Set.union left.used right.used }

  let widen ~previous ~next ~iteration:_ = join previous next

  let errors { used; define; _ } =
    let { Node.value = { Define.signature = { Define.Signature.parameters; _ }; _ }; _ } = define in
    let check_parameter { Node.value = { Parameter.name; _ }; location } =
      match Set.find used ~f:(Identifier.equal name) with
      | Some _ -> ()
      | None ->
          let error =
            Error.create
              ~location:(Location.with_module ~qualifier:Context.qualifier location)
              ~kind:(Error.DeadStore name)
              ~define
          in
          ErrorMap.Table.set
            Context.errors
            ~key:{ ErrorMap.location; identifier = name }
            ~data:error
    in
    List.iter ~f:check_parameter parameters;
    ErrorMap.Table.data Context.errors |> List.sort ~compare:Error.compare


  let forward ~key:_ state ~statement:_ = state

  let backward
      ~key
      ({ used; define; local_annotations; _ } as state)
      ~statement:({ Node.location; value } as statement)
    =
    let resolution =
      let { Node.value = { Define.signature = { Define.Signature.parent; _ }; _ }; _ } = define in
      let global_resolution = TypeEnvironment.ReadOnly.global_resolution Context.environment in
      TypeCheck.resolution_with_key
        ~global_resolution
        ~local_annotations
        ~parent
        ~key
        (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
        (module TypeCheck.DummyContext)
    in

    (* Check for bottomed out state. *)
    let bottom =
      match value with
      | Statement.Assert { Assert.test = { Node.value = False; _ }; _ } -> true
      | Expression expression ->
          Type.is_noreturn (Resolution.resolve_expression_to_type resolution expression)
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
            let error =
              Error.create
                ~location:(Location.with_module ~qualifier:Context.qualifier location)
                ~kind:(Error.DeadStore identifier)
                ~define
            in
            ErrorMap.Table.set Context.errors ~key:{ ErrorMap.location; identifier } ~data:error;
            used
      in
      match value with
      | Assign { target; _ } ->
          let rec update_target used = function
            | { Node.value = Expression.Name (Name.Identifier identifier); _ } ->
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
      let used_names_in_calls =
        let lookup_used_in_defines { Node.value; _ } =
          let name =
            match value with
            | Expression.Call { callee = { Node.value = Name callee; _ }; _ }
              when is_simple_name callee ->
                Some (name_to_reference_exn callee)
            | _ -> None
          in
          match name >>= NestedDefineLookup.Table.find Context.nested_define_lookup with
          | Some used -> Set.to_list used
          | None -> []
        in
        Visit.collect_calls statement
        |> List.map ~f:(fun { Node.value = { Call.callee; _ }; _ } -> callee)
        |> List.map ~f:lookup_used_in_defines
        |> List.concat
      in
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
      List.fold (used_names @ used_names_in_calls) ~f:Set.add ~init:used
    in
    { state with used }
end

let name = "Liveness"

let run
    ~configuration:_
    ~environment
    ~source:({ Source.source_path = { SourcePath.qualifier; _ }; _ } as source)
  =
  let module Context = struct
    let qualifier = qualifier

    let environment = environment

    let errors = ErrorMap.Table.create ()

    let nested_define_lookup = NestedDefineLookup.Table.create ()
  end
  in
  let module State = State (Context) in
  let module Fixpoint = Fixpoint.Make (State) in
  let define = Source.top_level_define_node source in
  let check
      ({
         Node.value =
           { Define.signature = { Define.Signature.name = { Node.value = name; _ }; _ }; _ };
         _;
       } as define)
    =
    let cfg = Cfg.create (Node.value define) in
    Fixpoint.backward ~cfg ~initial:(State.initial ~define)
    |> Fixpoint.entry
    >>| (fun ({ used; _ } as state) ->
          NestedDefineLookup.Table.set Context.nested_define_lookup ~key:name ~data:used;
          State.errors state)
    |> Option.value ~default:[]
  in
  List.map ~f:check (nested_defines_deep_to_shallow define) |> ignore;
  check define
