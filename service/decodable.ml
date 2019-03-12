(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

type decodable = ..


type decoding_error = [
  | `Malformed_key
  | `Unknown_type
  | `Decoder_failure of exn
]


let registry = Hashtbl.create 13


let register prefix decoder =
  let prefix = Prefix.make_key prefix "" in
  assert (not (Hashtbl.mem registry prefix));
  Hashtbl.add registry prefix decoder


let decode ~key ~value =
  match String.index key '$' with
  | exception Not_found ->
      Result.Error `Malformed_key
  | dollar ->
      let prefix_size = dollar + 1 in
      let prefix = String.sub key 0 prefix_size in
      match Hashtbl.find registry prefix with
      | exception Not_found -> Result.Error `Unknown_type
      | decoder ->
          let key =
            String.sub key prefix_size (String.length key - prefix_size)
          in
          match decoder key value with
          | result -> Result.Ok result
          | exception exn -> Result.Error (`Decoder_failure exn)


module type KeyType = sig
  include SharedMem.UserKeyType
  type out
  val from_string: string -> out
end


module Register (Key: KeyType) (Value: Value.Type) (): sig
  type decodable += Decoded of Key.out * Value.t
end = struct
  (* Register decoder *)
  type decodable += Decoded of Key.out * Value.t

  let () =
    register
      Value.prefix
      (fun key value -> Decoded (Key.from_string key, Marshal.from_string value 0))
end


module NoCache (Key: KeyType) (Value: Value.Type):
sig
  type decodable += Decoded of Key.out * Value.t
  include Memory.NoCache with
    type t = Value.t
    and type key = Key.t
    and module KeySet = Set.Make (Key)
    and module KeyMap = MyMap.Make (Key)
end = struct
  include Register (Key) (Value) ()
  include Memory.NoCache (Key) (Value)
end


module WithCache (Key: KeyType) (Value: Value.Type):
sig
  type decodable += Decoded of Key.out * Value.t
  include Memory.WithCache with
    type t = Value.t
    and type key = Key.t
    and module KeySet = Set.Make (Key)
    and module KeyMap = MyMap.Make (Key)
end = struct
  include Register (Key) (Value) ()
  include Memory.WithCache (Key) (Value)
end
