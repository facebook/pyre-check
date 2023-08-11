(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

val exception_to_error
  :  error:'a ->
  message:string ->
  f:(unit -> ('b, 'a) result) ->
  ('b, 'a) result

module Usage : sig
  type error = LoadError [@@deriving show]

  type t =
    | Used
    | Unused of error
  [@@deriving show]
end

module type SingleValueValueType = sig
  type t

  val name : string
end

(* Support storing / loading a single OCaml value into / from the shared memory, for caching
   purposes. *)
module MakeSingleValue (Value : SingleValueValueType) : sig
  val load : unit -> (Value.t, Usage.t) result

  val save : Value.t -> unit
end

module type KeyValueValueType = sig
  type t

  val prefix : Hack_parallel.Std.Prefix.t

  val description : string
end

(* Support storing / loading key-value pairs into / from the shared memory. *)
module MakeKeyValue (Key : Hack_parallel.Std.SharedMemory.KeyType) (Value : KeyValueValueType) : sig
  module KeySet : Set.S with type t = Set.Make(Key).t and type elt = Key.t

  type t

  val create : unit -> t

  val get : t -> Key.t -> Value.t option

  val mem : t -> Key.t -> bool

  val of_alist : (Key.t * Value.t) list -> t

  val cleanup : t -> unit
end
