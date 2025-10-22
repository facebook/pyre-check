(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Statement
open Expression
module PyrePysaLogic = Analysis.PyrePysaLogic
module AstResult = PyrePysaApi.AstResult

module Heap = struct
  type t = StringLiteral.t Reference.Map.t [@@deriving show, equal]

  let of_alist_exn alist =
    alist
    |> List.map ~f:(fun (key, value) -> key, StringLiteral.create value)
    |> Reference.Map.of_alist_exn


  let empty = Reference.Map.empty

  let from_qualifier ~pyre_api ~callables_to_definitions_map qualifier =
    let extract_string = function
      (* __module__ affects name resolution, due to __module__ specifying the module something was
         defined in, so a solution is just to skip __module__ assignments *)
      | {
          Node.value =
            {
              Assign.target =
                { Node.value = Expression.Name (Attribute { attribute = "__module__"; _ }); _ };
              _;
            };
          _;
        } ->
          None
      | {
          Node.value =
            {
              Assign.target = { Node.value = Expression.Name (_ as name); _ };
              Assign.value =
                Some { Node.value = Expression.Constant (Constant.String (_ as value)); _ };
              _;
            };
          _;
        }
        when Option.is_some (Ast.Expression.name_to_reference name) ->
          (* TODO(T225700656): Migrate for pyrefly *)
          let as_local = Ast.Expression.name_to_reference_exn name in
          let delocalized = Ast.Reference.delocalize as_local in
          if Ast.Reference.is_prefix ~prefix:qualifier delocalized then
            Some (delocalized, value)
          else (* Should not collect global variables imported from a different module. *)
            None
      | _ -> None
    in
    let split_for_map = function
      | name, value -> name, value
    in
    let open Option.Monad_infix in
    qualifier
    |> PyrePysaApi.ReadOnly.get_qualifier_top_level_define_name pyre_api
    |> Target.create_function
    |> CallablesSharedMemory.ReadOnly.get_define callables_to_definitions_map
    |> AstResult.to_option
    >>| (fun { CallablesSharedMemory.DefineAndQualifier.define; _ } -> define)
    >>| Ast.Node.value
    >>| (fun { Ast.Statement.Define.body; _ } -> body)
    |> Option.value ~default:[]
    |> Source.create_from_module_path
         ~typecheck_flags:(Source.TypecheckFlags.create_for_testing ())
         ~module_path:{ ModulePath.raw = ModulePath.Raw.empty; qualifier; should_type_check = true }
    |> PyrePysaApi.ReadOnly.ensures_qualified pyre_api
    |> Preprocessing.toplevel_assigns
    |> List.concat_map ~f:Preprocessing.toplevel_expand_tuple_assign
    |> List.filter_map ~f:extract_string
    |> List.map ~f:split_for_map
    (* Overwrite with the newer expression for duplicate global assigns *)
    |> Ast.Reference.Map.of_alist_reduce ~f:(fun _old updated -> updated)


  let from_qualifiers ~pyre_api ~callables_to_definitions_map ~qualifiers =
    let build_per_qualifier qualifier =
      from_qualifier ~pyre_api ~callables_to_definitions_map qualifier
    in
    let reduce =
      let merge ~key = function
        | `Both (left, right) when not (StringLiteral.equal left right) ->
            failwith
              (Format.asprintf
                 "Two different globals in different modules with the same unqualified name `%s` \
                  and different values `%s` and `%s`."
                 (StringLiteral.show left)
                 (StringLiteral.show right)
                 (Reference.show key))
        | `Both (value, _)
        | `Left value
        | `Right value ->
            Some value
      in
      Map.merge ~f:merge
    in
    qualifiers |> List.map ~f:build_per_qualifier |> Algorithms.fold_balanced ~init:empty ~f:reduce
end

module SharedMemory = struct
  module T =
    SaveLoadSharedMemory.MakeKeyValue
      (PyrePysaLogic.SharedMemoryKeys.ReferenceKey)
      (struct
        type t = StringLiteral.t

        let prefix = Hack_parallel.Std.Prefix.make ()

        let handle_prefix = Hack_parallel.Std.Prefix.make ()

        let description = "Mapping from fully qualified global name to expression"
      end)

  include T

  let from_heap heap = heap |> Map.to_alist |> T.of_alist_sequential

  let from_qualifiers
      ~scheduler
      ~scheduler_policies
      ~pyre_api
      ~callables_to_definitions_map
      ~qualifiers
    =
    let scheduler_policy =
      Scheduler.Policy.from_configuration_or_default
        scheduler_policies
        Configuration.ScheduleIdentifier.GlobalConstants
        ~default:
          (Scheduler.Policy.fixed_chunk_count
             ~minimum_chunks_per_worker:1
             ~minimum_chunk_size:1
             ~preferred_chunks_per_worker:1
             ())
    in
    let handle = T.create () |> T.add_only in
    let empty_handle = T.AddOnly.create_empty handle in
    let add_heap handle heap =
      Map.fold heap ~init:handle ~f:(fun ~key ~data handle -> T.AddOnly.add handle key data)
    in
    Scheduler.map_reduce
      scheduler
      ~policy:scheduler_policy
      ~initial:handle
      ~map:(fun qualifiers ->
        add_heap
          empty_handle
          (Heap.from_qualifiers ~pyre_api ~callables_to_definitions_map ~qualifiers))
      ~reduce:(fun smaller larger -> T.AddOnly.merge_same_handle_disjoint_keys ~smaller ~larger)
      ~inputs:qualifiers
      ()
    |> T.from_add_only
end
