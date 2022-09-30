(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Pyre
open Expression
open Statement
module Error = AnalysisError

module Resolution = struct
  include
    Abstract.MapDomain.Make
      (struct
        include Reference

        let name = "Reference"

        let absence_implicitly_maps_to_bottom = false
      end)
      (Abstract.SimpleDomain.Make (ReadOnlyness))
end

module Resolved = struct
  type t = {
    resolution: Resolution.t;
    resolved: ReadOnlyness.t;
    errors: Error.t list;
  }
  [@@deriving show]
end

module LocalErrorMap = struct
  type t = Error.t list Int.Table.t

  let empty () = Int.Table.create ()

  let set ~statement_key ~errors error_map = Int.Table.set error_map ~key:statement_key ~data:errors

  let append ~statement_key ~error error_map =
    Int.Table.add_multi error_map ~key:statement_key ~data:error


  let all_errors error_map = Int.Table.data error_map |> List.concat
end

module type Context = sig
  val qualifier : Reference.t

  val define : Define.t Node.t

  val global_resolution : GlobalResolution.t

  (* Where to store errors found during the fixpoint. `None` discards them. *)
  val error_map : LocalErrorMap.t option
end

module State (Context : Context) = struct
  include Resolution

  let add_error ~location ~kind errors =
    Error.create
      ~location:(Location.with_module ~module_reference:Context.qualifier location)
      ~kind
      ~define:Context.define
    :: errors


  let instantiate_path ~global_resolution location =
    let lookup =
      GlobalResolution.module_tracker global_resolution
      |> ModuleTracker.ReadOnly.lookup_relative_path
    in
    let location = Location.with_module ~module_reference:Context.qualifier location in
    Location.WithModule.instantiate ~lookup location


  let widen ~previous ~next ~iteration = widen ~prev:previous ~next ~iteration

  let forward_expression ~resolution { Node.value; _ } =
    let open ReadOnlyness in
    match value with
    | Expression.Constant _ ->
        { Resolved.resolution; errors = []; resolved = ReadOnlyness.ReadOnly }
    | Expression.Name (Name.Identifier identifier) ->
        {
          Resolved.resolution;
          errors = [];
          resolved =
            Resolution.get_opt (Reference.create identifier) resolution
            |> Option.value ~default:Mutable;
        }
    | _ -> failwith "TODO(T130377746)"


  let forward_assignment
      ~resolution
      ~location
      ~target:{ Node.value = target; location = target_location }
      ~annotation
      ~value
    =
    (* TODO(T130377746): Handle other Assign targets. *)
    match target with
    | Expression.Name target -> (
        match name_to_reference target with
        | Some name ->
            let { Resolved.resolved = actual_readonlyness; errors; resolution } =
              forward_expression ~resolution value
            in
            let expected_readonlyness =
              annotation
              >>| GlobalResolution.parse_annotation Context.global_resolution
              >>| ReadOnlyness.of_type
            in
            let resolution =
              Resolution.set
                resolution
                ~key:name
                ~data:(Option.value ~default:actual_readonlyness expected_readonlyness)
            in
            let errors =
              match expected_readonlyness with
              | Some expected_readonlyness
                when not
                       (ReadOnlyness.less_or_equal
                          ~left:actual_readonlyness
                          ~right:expected_readonlyness) ->
                  add_error
                    ~location
                    ~kind:
                      (Error.ReadOnlynessMismatch
                         (IncompatibleVariableType
                            {
                              incompatible_type =
                                {
                                  name;
                                  mismatch =
                                    {
                                      actual = actual_readonlyness;
                                      expected = expected_readonlyness;
                                    };
                                };
                              declare_location =
                                instantiate_path
                                  ~global_resolution:Context.global_resolution
                                  target_location;
                            }))
                    errors
              | _ -> errors
            in
            resolution, errors
        | None -> resolution, [])
    | _ -> resolution, []


  let forward_statement ~state ~statement:{ Node.value; location } =
    let resolution = state in
    match value with
    | Statement.Assign { Assign.target; annotation; value } ->
        forward_assignment ~resolution ~location ~target ~annotation ~value
    | _ -> state, []


  let initial = Resolution.of_list []

  let forward ~statement_key state ~statement =
    let new_state, errors = forward_statement ~state ~statement in
    let () =
      let _ = Context.error_map >>| LocalErrorMap.set ~statement_key ~errors in
      ()
    in
    new_state


  let backward ~statement_key:_ _state ~statement:_ =
    failwith "Not implementing this for readonly analysis"
end

let readonly_errors_for_define ~type_environment ~qualifier define =
  let module Context = struct
    let qualifier = qualifier

    let define = define

    let global_resolution = TypeEnvironment.ReadOnly.global_resolution type_environment

    let error_map = Some (LocalErrorMap.empty ())
  end
  in
  let module State = State (Context) in
  let module Fixpoint = Fixpoint.Make (State) in
  let cfg = Node.value define |> Cfg.create in
  let _state = Fixpoint.forward ~cfg ~initial:State.initial |> Fixpoint.exit in
  Option.value_exn
    ~message:"no error map found in the analysis context"
    (Context.error_map >>| LocalErrorMap.all_errors >>| Error.deduplicate)
