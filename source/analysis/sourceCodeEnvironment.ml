(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This is the bottom layer of the stack of environment functor instances, it sits between
   UnannotatedGlobalEnvironment and SourceCodeIncrementalApi.

   SourceCodeIncrementalApi serves as the logical basis for the environment. But to functorize the
   stack, we need a slightly different interface to directy wrap. And there are some "optional" APIs
   pyre environments can expose that require direct access to an underlying AstEnvironment.

   We need a separate module for this interface in part to avoid a cycle with AstEnvironment. *)

open Base

module ReadOnly = struct
  include SourceCodeIncrementalApi.ReadOnly

  let source_code_read_only = Fn.id
end

module UpdateResult = struct
  include SourceCodeIncrementalApi.UpdateResult

  let locally_triggered_dependencies = triggered_dependencies

  let all_triggered_dependencies update_result = [triggered_dependencies update_result]

  let source_code_update_result = Fn.id

  (* This is just needed because Environment.UpdateResult requires the same type for the previous
     environment and current environment for simplicitly; this implementation is never actually
     used. *)
  let modules_with_invalidated_type_check _ = Ast.Reference.Set.empty
end

module Overlay = struct
  include SourceCodeIncrementalApi.Overlay

  let source_code_overlay = Fn.id

  let filter_update source_code_overlay update_result =
    let filtered_invalidated_modules =
      SourceCodeIncrementalApi.UpdateResult.invalidated_modules update_result
      |> List.filter ~f:(SourceCodeIncrementalApi.Overlay.owns_qualifier source_code_overlay)
    in
    {
      update_result with
      SourceCodeIncrementalApi.UpdateResult.invalidated_modules = filtered_invalidated_modules;
    }


  let propagate_parent_update = filter_update
end

type t = {
  source_code_base: SourceCodeIncrementalApi.Base.t;
  maybe_ast_environment: AstEnvironment.t option;
}

let of_ast_environment ast_environment =
  let source_code_base = AstEnvironment.as_source_code_incremental ast_environment in
  { source_code_base; maybe_ast_environment = Some ast_environment }


let of_source_code_base source_code_base = { source_code_base; maybe_ast_environment = None }

let create = Fn.id

let source_code_base { source_code_base; _ } = source_code_base

let read_only { source_code_base; _ } = SourceCodeIncrementalApi.Base.read_only source_code_base

let overlay { source_code_base; _ } = SourceCodeIncrementalApi.Base.overlay source_code_base

let update_this_and_all_preceding_environments { source_code_base; _ } =
  SourceCodeIncrementalApi.Base.update source_code_base


module AssumeAstEnvironment = struct
  (* Some operations like saved state, shared memory clearing in tests, and Pysa caching require
     direct access to the AstEnvironment. This will only work in cases where the
     SourceCodeEnvironment is actually backed by an AstEnvironment. *)
  let ast_environment { maybe_ast_environment; _ } =
    Option.value_exn
      maybe_ast_environment
      ~message:"This environment is not backed by an AstEnvironment"


  (* All SharedMemory tables are populated and stored in separate, imperative steps that must be run
     before loading / after storing. These functions only handle serializing and deserializing the
     non-SharedMemory data *)
  let load controls = AstEnvironment.load controls |> of_ast_environment |> create

  let store { maybe_ast_environment; _ } =
    match maybe_ast_environment with
    | Some ast_environment -> AstEnvironment.store ast_environment
    | None -> failwith "Cannot store environment not backed by AstEnvironment"
end
