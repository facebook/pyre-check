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
      Log.error "Error %s:\n%s" message (Exn.to_string exception_);
      Error error


module Usage = struct
  type error = LoadError [@@deriving show { with_path = false }]

  type t =
    | Used
    | Unused of error
  [@@deriving show { with_path = false }]
end

module type SingleValueValueType = sig
  type t

  val name : string
end

(* Support storing / loading a single OCaml value into / from the shared memory, for caching
   purposes. *)
module MakeSingleValue (Value : SingleValueValueType) = struct
  let shared_memory_prefix = Hack_parallel.Std.Prefix.make ()

  module T = Memory.Serializer (struct
    type t = Value.t

    module Serialized = struct
      type t = Value.t

      let prefix = shared_memory_prefix

      let description = Value.name
    end

    let serialize = Fn.id

    let deserialize = Fn.id
  end)

  let load () =
    exception_to_error
      ~error:(Usage.Unused Usage.LoadError)
      ~message:(Format.asprintf "Loading %s from cache" Value.name)
      ~f:(fun () ->
        Log.info "Loading %s from cache..." Value.name;
        let value = T.load () in
        Log.info "Loaded %s from cache." Value.name;
        Ok value)


  let save value =
    exception_to_error
      ~error:()
      ~message:(Format.asprintf "Saving %s to cache" Value.name)
      ~f:(fun () ->
        Memory.SharedMemory.collect `aggressive;
        T.store value;
        Log.info "Saved %s to cache." Value.name;
        Ok ())
    |> ignore


  let load_or_compute ~should_save f =
    let value, usage =
      match load () with
      | Ok value -> Some value, Usage.Used
      | Error error -> None, error
    in
    match value with
    | Some value -> value, usage
    | None ->
        let value = f () in
        if should_save then save value;
        value, usage
end
