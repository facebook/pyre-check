(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Per-function analysis that determines whether a global's been written to. *)

open Core
open Ast
open Expression
open Statement
open Pyre
module Error = AnalysisError

module LocalErrorMap = struct
  type t = Error.t list Int.Table.t

  let empty () = Int.Table.create ()

  let append error_map ~statement_key ~error =
    Int.Table.add_multi error_map ~key:statement_key ~data:error


  let all_errors error_map = Int.Table.data error_map |> List.concat
end

module type Context = sig
  val qualifier : Reference.t

  val define : Define.t Node.t

  val global_resolution : GlobalResolution.t

  val local_annotations : LocalAnnotationMap.ReadOnly.t option

  val error_map : LocalErrorMap.t
end

module State (Context : Context) = struct
  type t = unit [@@deriving show]

  let less_or_equal ~left:_ ~right:_ = true

  let join _ _ = ()

  let widen ~previous ~next ~iteration:_ = join previous next

  let errors () = Context.error_map |> LocalErrorMap.all_errors

  let forward ~statement_key state ~statement:{ Node.value; _ } =
    let { Node.value = { Define.signature = { Define.Signature.parent; _ }; _ }; _ } =
      Context.define
    in
    let resolution =
      TypeCheck.resolution_with_key
        ~global_resolution:Context.global_resolution
        ~local_annotations:Context.local_annotations
        ~parent
        ~statement_key
        (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
        (module TypeCheck.DummyContext)
    in
    ignore resolution;
    match value with
    | Statement.Assert _ -> ()
    | Assign { target = { Node.value = Expression.Name (Name.Identifier target); location }; _ } ->
        let error =
          Error.create
            ~location:(Location.with_module ~module_reference:Context.qualifier location)
            ~kind:(Error.GlobalLeak { global_name = Ast.Reference.create target })
            ~define:Context.define
        in
        LocalErrorMap.append Context.error_map ~statement_key ~error
    | Assign _ -> ()
    | Delete _ -> ()
    | Expression _ -> ()
    | Raise _ -> ()
    | Return _ -> ()
    (* Control flow and nested functions/classes doesn't need to be analyzed explicitly. *)
    | If _
    | Class _
    | Define _
    | For _
    | Match _
    | While _
    | With _
    | Try _ ->
        state
    (* Trivial cases. *)
    | Break
    | Continue
    | Global _
    | Import _
    | Nonlocal _
    | Pass ->
        state


  let backward ~statement_key:_ _ ~statement:_ = ()

  let bottom = ()

  let initial ~global_resolution:_ _ = ()
end

let global_leak_errors ~type_environment ~qualifier define =
  let module Context = struct
    let qualifier = qualifier

    let define = define

    let global_resolution = TypeEnvironment.ReadOnly.global_resolution type_environment

    let local_annotations =
      TypeEnvironment.TypeEnvironmentReadOnly.get_or_recompute_local_annotations
        type_environment
        (Node.value define |> Define.name)


    let error_map = LocalErrorMap.empty ()
  end
  in
  let module State = State (Context) in
  let module Fixpoint = Fixpoint.Make (State) in
  let cfg = Cfg.create (Node.value define) in
  let global_resolution =
    TypeEnvironment.ReadOnly.global_environment type_environment |> GlobalResolution.create
  in
  Fixpoint.forward ~cfg ~initial:(State.initial ~global_resolution (Node.value define))
  |> Fixpoint.exit
  >>| State.errors
  |> Option.value ~default:[]


(* TODO: Use this function to limit by entrypoints in the future. *)
let should_run_analysis ~type_environment:_ _ = true

let check_define ~type_environment ~qualifier define =
  if should_run_analysis ~type_environment define then
    global_leak_errors ~type_environment ~qualifier define
  else
    []


let check_module_TESTING_ONLY
    ~type_environment
    ({ Source.module_path = { ModulePath.qualifier; _ }; _ } as source)
  =
  source
  |> Preprocessing.defines ~include_toplevels:true
  |> List.map ~f:(check_define ~type_environment ~qualifier)
  |> List.concat
