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
end

module type KeyValueValueType = sig
  type t

  val prefix : Hack_parallel.Std.Prefix.t

  val description : string
end

(* Support storing / loading key-value pairs into / from the shared memory. *)
module MakeKeyValue (Key : Hack_parallel.Std.SharedMemory.KeyType) (Value : KeyValueValueType) =
struct
  module FirstClass =
    Hack_parallel.Std.SharedMemory.FirstClass.WithCache.Make
      (Key)
      (struct
        type t = Value.t

        let prefix = Value.prefix

        let description = Value.description
      end)

  (* The table handle and the keys, whose combination locates all entries in the shared memory that
     belong to this table. *)
  module Handle = struct
    type t = {
      first_class_handle: FirstClass.t;
      keys: FirstClass.KeySet.t;
    }
  end

  module KeySet = FirstClass.KeySet

  type t = Handle.t

  let create () =
    { Handle.first_class_handle = FirstClass.create (); keys = FirstClass.KeySet.empty }


  let get { Handle.first_class_handle; _ } = FirstClass.get first_class_handle

  let mem { Handle.first_class_handle; _ } = FirstClass.mem first_class_handle

  (* Partially invalidate the shared memory. *)
  let cleanup { Handle.first_class_handle; keys } = FirstClass.remove_batch first_class_handle keys

  let of_alist list =
    let save_entry ~first_class_handle (key, value) = FirstClass.add first_class_handle key value in
    let first_class_handle = FirstClass.create () in
    List.iter list ~f:(save_entry ~first_class_handle);
    { Handle.first_class_handle; keys = list |> List.map ~f:fst |> FirstClass.KeySet.of_list }
end
