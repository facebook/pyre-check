(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Analysis
open Ast

let resolve_callee ~resolution ~callee =
  match Resolution.resolve_expression_to_type resolution callee with
  | Type.Callable { Type.Callable.kind = Type.Callable.Named name; _ } ->
      [Callable.create_function name]
  | _ -> []


(* This is a bit of a trick. The only place that knows where the local annotation map keys is the
   fixpoint (shared across the type check and additional static analysis modules). By having a
   fixpoint that always terminates (by having a state = unit), we re-use the fixpoint id's without
   having to hackily recompute them. *)
module DefineCallGraph (Context : sig
  val global_resolution : GlobalResolution.t

  val local_annotations : LocalAnnotationMap.ReadOnly.t option

  val parent : Reference.t option

  val callees_at_location : Callable.t list Location.Table.t
end) =
Fixpoint.Make (struct
  module CalleeVisitor = Visit.Make (struct
    type t = Resolution.t

    let expression resolution { Node.value; location } =
      match value with
      | Expression.Expression.Call { Expression.Call.callee; _ } ->
          Location.Table.set
            Context.callees_at_location
            ~key:location
            ~data:(resolve_callee ~resolution ~callee);
          resolution
      | _ -> resolution


    let statement resolution _ = resolution
  end)

  type t = unit [@@deriving show]

  let less_or_equal ~left:_ ~right:_ = true

  let widen ~previous:_ ~next:_ ~iteration:_ = ()

  let forward_statement ~resolution ~statement =
    CalleeVisitor.visit resolution (Ast.Source.create [statement]) |> ignore;
    ()


  let forward ~key _ ~statement =
    let resolution =
      TypeCheck.resolution_with_key
        ~global_resolution:Context.global_resolution
        ~local_annotations:Context.local_annotations
        ~parent:Context.parent
        ~key
        (module TypeCheck.DummyContext)
    in
    forward_statement ~resolution ~statement


  let backward ~key:_ _ ~statement:_ = ()
end)

let call_graph_of_define
    ~environment
    ~define:
      ({ Statement.Define.signature = { Statement.Define.Signature.name; parent; _ }; _ } as define)
  =
  let callees_at_location = Location.Table.create () in
  let module DefineFixpoint = DefineCallGraph (struct
    let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment

    let local_annotations =
      TypeEnvironment.ReadOnly.get_local_annotations environment (Node.value name)


    let parent = parent

    let callees_at_location = callees_at_location
  end)
  in
  DefineFixpoint.forward ~cfg:(Cfg.create define) ~initial:() |> ignore;
  Location.Table.to_alist callees_at_location |> Location.Map.of_alist_exn
