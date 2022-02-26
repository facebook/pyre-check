(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val type_to_reference : Type.t -> Ast.Reference.t

module Inference : sig
  type target =
    | Return
    | Parameter of { name: Ast.Reference.t }
    | Global of {
        name: Ast.Reference.t;
        location: Ast.Location.WithModule.t;
      }
    | Attribute of {
        parent: Ast.Reference.t;
        name: Ast.Reference.t;
        location: Ast.Location.WithModule.t;
      }
  [@@deriving show]

  type raw = {
    type_: Type.t;
    target: target;
  }
  [@@deriving show]

  type t [@@deriving show]

  val create : raw -> t
end

module LocalResult : sig
  type t [@@deriving show, to_yojson]

  val define_name : t -> Ast.Reference.t

  val from_signature
    :  global_resolution:Analysis.GlobalResolution.t ->
    lookup:(Ast.Reference.t -> string option) ->
    qualifier:Ast.Reference.t ->
    Ast.Statement.Define.t Ast.Node.t ->
    t

  val add_inference
    :  global_resolution:Analysis.GlobalResolution.t ->
    lookup:(Ast.Reference.t -> string option) ->
    t ->
    Inference.t ->
    t
end

module GlobalResult : sig
  type t [@@deriving show, to_yojson]

  val inference_count : t -> int

  val empty : t

  val suppress_unhelpful_types : t -> t

  val from_local_results : global_resolution:Analysis.GlobalResolution.t -> LocalResult.t list -> t
end
