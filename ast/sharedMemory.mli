(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core

module HandleKey : Memory.KeyType with type t = File.Handle.t and type out = File.Handle.t

module ReferenceKey : Memory.KeyType with type t = Reference.t and type out = Reference.t

module IntKey : Memory.KeyType with type t = int and type out = int

module SymlinksToPaths : sig
  module SymlinkTarget : Memory.KeyType with type t = string and type out = string

  module SymlinkSource : Value.Type with type t = PyrePath.t

  module SymlinksToPaths : module type of Memory.NoCache (SymlinkTarget) (SymlinkSource)

  val get : string -> PyrePath.t option

  val add : string -> PyrePath.t -> unit

  val remove : targets:string list -> unit

  (* Exposed for testing. *)
  val hash_of_key : string -> string

  val serialize_key : string -> string

  val compute_hashes_to_keys : keys:string list -> string String.Map.t
end

module Sources : sig
  module SourceValue : Value.Type with type t = Source.t

  module Sources : module type of Memory.NoCache (ReferenceKey) (SourceValue)

  val get : Reference.t -> Source.t option

  val add : Source.t -> unit

  val remove : Reference.t list -> unit

  (* Exposed for testing. *)
  val hash_of_qualifier : Reference.t -> string

  val serialize_qualifier : Reference.t -> string

  val compute_hashes_to_keys : keys:Reference.t list -> string String.Map.t
end

module HandleKeys : sig
  module HandleKeysValue : Value.Type with type t = File.Handle.Set.Tree.t

  module HandleKeys : module type of Memory.NoCache (Memory.SingletonKey) (HandleKeysValue)

  val get : unit -> File.Handle.Set.Tree.t

  val add : handles:File.Handle.Set.Tree.t -> unit

  val remove : handles:File.Handle.t list -> unit

  (* Can only be called from the master process. *)
  val clear : unit -> unit

  val normalize : unit -> unit

  val compute_hashes_to_keys : unit -> string String.Map.t
end

module Modules : sig
  module ModuleValue : Value.Type with type t = Module.t

  module Modules : module type of Memory.NoCache (ReferenceKey) (ModuleValue)

  val get : qualifier:Reference.t -> Module.t option

  val get_exports : qualifier:Reference.t -> Reference.t list option

  val add : qualifier:Reference.t -> ast_module:Module.t -> unit

  val remove : qualifiers:Reference.t list -> unit

  val exists : qualifier:Reference.t -> bool

  (* Exposed for testing. *)
  val hash_of_key : Reference.t -> string

  val serialize_key : Reference.t -> string

  val compute_hashes_to_keys : keys:Reference.t list -> string String.Map.t

  (* Instead of writing values to shared memory, changes to shared memory are cached locally in a
     begin_transaction/end_transaction block. *)
  val begin_transaction : unit -> unit

  val end_transaction : unit -> unit
end

module Handles : sig
  module PathValue : Value.Type with type t = string

  module Paths : module type of Memory.WithCache (IntKey) (PathValue)

  val get : hash:int -> string option

  val add_handle_hash : handle:string -> unit

  (* Exposed for testing. *)
  val hash_of_key : int -> string

  val serialize_key : int -> string

  val compute_hashes_to_keys : keys:string list -> string String.Map.t
end
