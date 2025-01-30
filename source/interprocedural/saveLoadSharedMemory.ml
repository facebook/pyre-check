(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

let exception_to_error ~error ~message ~f =
  try f () with
  | exception_ ->
      Log.error "Error %s:\n%s" message (Exception.exn_to_string exception_);
      Error error


module Usage = struct
  type error =
    | LoadError
    | Stale
  [@@deriving compare, show { with_path = false }]

  type t =
    | Used
    | Unused of error
  [@@deriving compare, show { with_path = false }]
end

module type SingleValueValueType = sig
  type t

  val prefix : Hack_parallel.Std.Prefix.t

  val name : string
end

(* Support storing / loading a single OCaml value into / from the shared memory, for caching
   purposes. *)
module MakeSingleValue (Value : SingleValueValueType) = struct
  module T = Memory.Serializer (struct
    type t = Value.t

    module Serialized = struct
      type t = Value.t

      let prefix = Value.prefix

      let description = Value.name
    end

    let serialize = Fn.id

    let deserialize = Fn.id
  end)

  let load_from_cache () =
    exception_to_error
      ~error:(Usage.Unused Usage.LoadError)
      ~message:(Format.asprintf "Loading %s from cache" Value.name)
      ~f:(fun () ->
        Log.info "Loading %s from cache..." Value.name;
        let value = T.load () in
        Log.info "Loaded %s from cache." Value.name;
        Ok value)


  let save_to_cache value =
    exception_to_error
      ~error:()
      ~message:(Format.asprintf "Saving %s to cache" Value.name)
      ~f:(fun () ->
        Memory.SharedMemory.collect `aggressive;
        T.store value;
        Log.info "Saved %s to cache." Value.name;
        Ok ())
    |> ignore
end

module type KeyValueValueType = sig
  type t

  val prefix : Hack_parallel.Std.Prefix.t

  val handle_prefix : Hack_parallel.Std.Prefix.t

  val description : string
end

(* Support storing / loading key-value pairs into / from the shared memory. *)
module MakeKeyValue (Key : Hack_parallel.Std.SharedMemory.KeyType) (Value : KeyValueValueType) =
struct
  module T =
    Hack_parallel.Std.SharedMemory.MakeFirstClassWithKeys
      (Key)
      (struct
        type t = Value.t

        let prefix = Value.prefix

        let description = Value.description
      end)

  include T

  module HandleSharedMemory = MakeSingleValue (struct
    type t = T.t

    let prefix = Value.handle_prefix

    let name = Format.asprintf "Handle of %s" Value.description
  end)

  let save_to_cache = HandleSharedMemory.save_to_cache

  let load_from_cache = HandleSharedMemory.load_from_cache
end
