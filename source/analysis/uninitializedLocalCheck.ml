(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Pyre
open Statement
module Error = AnalysisError

let name = "UninitializedLocal"

let extract_reads_expression expression =
  let name_access_to_identifier_node { Define.NameAccess.name; location } =
    { Node.value = name; location }
  in
  Preprocessing.AccessCollector.from_expression Preprocessing.NameAccessSet.empty expression
  |> Preprocessing.NameAccessSet.to_list
  |> List.map ~f:name_access_to_identifier_node


let extract_reads_statement { Node.value; _ } =
  let expressions =
    match value with
    | Statement.Assign { Assign.value = expression; _ }
    | Delete expression
    | Expression expression
    | Yield expression
    | YieldFrom expression
    | If { If.test = expression; _ }
    | While { While.test = expression; _ } ->
        [expression]
    | Assert { Assert.test; message; _ } -> [test] @ Option.to_list message
    | For { For.target; iterator; _ } -> [target; iterator]
    | Raise { Raise.expression; from } -> Option.to_list expression @ Option.to_list from
    | Return { Return.expression; _ } -> Option.to_list expression
    | With { With.items; _ } -> items |> List.map ~f:(fun (value, _) -> value)
    | Break
    | Class _
    | Continue
    | Define _
    | Global _
    | Import _
    | Nonlocal _
    | Pass
    | Try _ ->
        []
  in
  expressions |> List.concat_map ~f:extract_reads_expression


module InitializedVariables = Identifier.Set

module type Context = sig
  val uninitialized_usage : Identifier.t Node.t list Int.Table.t
end

module State (Context : Context) = struct
  type t = InitializedVariables.t

  let show state =
    InitializedVariables.elements state |> String.concat ~sep:", " |> Format.sprintf "[%s]"


  let pp format state = Format.fprintf format "%s" (show state)

  let initial ~define:{ Node.value = { Define.signature; _ }; _ } =
    signature.parameters
    |> Scope.Binding.of_parameters []
    |> List.map ~f:Scope.Binding.name
    |> List.map ~f:Identifier.sanitized
    |> InitializedVariables.of_list


  let errors ~qualifier ~define _ =
    let emit_error { Node.value; location } =
      Error.create
        ~location:(Location.with_module ~qualifier location)
        ~kind:(Error.UnboundName value)
        ~define
    in
    let all_locals =
      let { Scope.Scope.bindings; globals; nonlocals; _ } =
        Scope.Scope.of_define_exn define.value
      in
      (* Santitization is needed to remove (some) scope information that is (sometimes, but not
         consistently) added into the identifiers themselves (e.g. $local_test?f$y). *)
      let locals =
        Identifier.Map.keys bindings |> List.map ~f:Identifier.sanitized |> Identifier.Set.of_list
      in
      (* This operation needs to be repeated as Scope doesn't know about qualification, and hence
         doesn't remove all globals and nonlocals from bindings *)
      let globals = Identifier.Set.map ~f:Identifier.sanitized globals in
      let nonlocals = Identifier.Set.map ~f:Identifier.sanitized nonlocals in
      Identifier.Set.diff (Identifier.Set.diff locals globals) nonlocals
    in
    let in_local_scope { Node.value = identifier; _ } =
      identifier |> Identifier.sanitized |> Identifier.Set.mem all_locals
    in
    Int.Table.data Context.uninitialized_usage
    |> List.concat
    |> List.filter ~f:in_local_scope
    |> List.map ~f:emit_error


  let less_or_equal ~left ~right = InitializedVariables.is_subset right ~of_:left

  let join left right = InitializedVariables.inter left right

  let widen ~previous ~next ~iteration:_ = join previous next

  let forward ~key state ~statement =
    let is_uninitialized { Node.value = identifier; _ } =
      not (InitializedVariables.mem state (Identifier.sanitized identifier))
    in
    let uninitialized_usage =
      extract_reads_statement statement |> List.filter ~f:is_uninitialized
    in
    Hashtbl.set Context.uninitialized_usage ~key ~data:uninitialized_usage;
    Scope.Binding.of_statement [] statement
    |> List.map ~f:Scope.Binding.name
    |> List.map ~f:Identifier.sanitized
    |> InitializedVariables.of_list
    |> InitializedVariables.union state


  let backward ~key:_ _ ~statement:_ = failwith "Not implemented"
end

let run_on_define ~qualifier define =
  let module Context = struct
    let uninitialized_usage = Int.Table.create ()
  end
  in
  let module State = State (Context) in
  let module Fixpoint = Fixpoint.Make (State) in
  let cfg = Cfg.create (Node.value define) in
  let fixpoint = Fixpoint.forward ~cfg ~initial:(State.initial ~define) in
  Fixpoint.exit fixpoint >>| State.errors ~qualifier ~define |> Option.value ~default:[]


let run
    ~configuration:_
    ~environment:_
    ~source:({ Source.source_path = { SourcePath.qualifier; _ }; _ } as source)
  =
  source
  |> Preprocessing.defines ~include_toplevels:false
  |> List.map ~f:(run_on_define ~qualifier)
  |> List.concat
