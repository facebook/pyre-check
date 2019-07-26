(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core

module IntKey : Memory.KeyType with type t = int and type out = int

module Sources : sig
  module SourceValue : Memory.ValueType with type t = Source.t

  module Sources : module type of Memory.NoCache (Reference.Key) (SourceValue)

  val get : Reference.t -> Source.t option

  val add : Source.t -> unit

  val remove : Reference.t list -> unit

  (* Exposed for testing. *)
  val hash_of_qualifier : Reference.t -> string

  val serialize_qualifier : Reference.t -> string

  val compute_hashes_to_keys : keys:Reference.t list -> string String.Map.t
end

module Modules : sig
  module ModuleValue : Memory.ValueType with type t = Module.t

  module Modules : module type of Memory.NoCache (Reference.Key) (ModuleValue)

  val get : qualifier:Reference.t -> Module.t option

  val get_exports : qualifier:Reference.t -> Reference.t list option

  val add : qualifier:Reference.t -> ast_module:Module.t -> unit

  val remove : qualifiers:Reference.t list -> unit

  val exists : qualifier:Reference.t -> bool

  (* Exposed for testing. *)
  val hash_of_key : Reference.t -> string

  val serialize_key : Reference.t -> string

  val compute_hashes_to_keys : keys:Reference.t list -> string String.Map.t
end

module Handles : sig
  module PathValue : Memory.ValueType with type t = string

  module Paths : module type of Memory.WithCache (Reference.Key) (PathValue)

  val get : Reference.t -> string option

  val add : Reference.t -> handle:string -> unit

  (* Exposed for testing. *)
  val hash_of_key : Reference.t -> string

  val serialize_key : Reference.t -> string

  val compute_hashes_to_keys : keys:Reference.t list -> string String.Map.t
end

val heap_size : unit -> int
