(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core

type t = int [@@deriving compare, show, sexp, hash]

module IndexKey = struct
  type nonrec t = t

  let to_string = show

  let compare = compare

  let from_string = Int.of_string
end

module Set = Set.Make (struct
  type nonrec t = t

  let compare = compare

  let sexp_of_t = sexp_of_t

  let t_of_sexp = t_of_sexp
end)

include Hashable.Make (struct
  type nonrec t = t

  let compare = compare

  let hash = Hashtbl.hash

  let hash_fold_t = hash_fold_t

  let sexp_of_t = sexp_of_t

  let t_of_sexp = t_of_sexp
end)

module OrderIndexValue = struct
  type t = int

  let prefix = Prefix.make ()

  let description = "Order indices"
end

module OrderAnnotationValue = struct
  type t = string

  let prefix = Prefix.make ()

  let description = "Order annotations"
end

module OrderIndices = Memory.WithCache.Make (SharedMemoryKeys.StringKey) (OrderIndexValue)
module OrderAnnotations = Memory.WithCache.Make (SharedMemoryKeys.IntKey) (OrderAnnotationValue)

let index annotation =
  match OrderIndices.get annotation with
  | Some index -> index
  | None -> (
      let rec claim_free_index current =
        OrderAnnotations.write_around current annotation;
        match OrderAnnotations.get_no_cache current with
        (* Successfully claimed the id *)
        | Some decoded when String.equal decoded annotation -> current
        (* Someone else claimed the id first *)
        | Some _non_equal_annotation -> claim_free_index (current + 1)
        | None -> failwith "read-your-own-write consistency was violated"
      in
      let encoded = claim_free_index (Type.Primitive.hash annotation) in
      (* There may be a race between two identical calls to index, which would result in
       * Annotations: { hash(a) -> a; hash(a) + 1 -> a; ... }
       * By writing through, and then returning the canonical value, we ensure we only
       * ever return one value for index(a) *)
      OrderIndices.write_around annotation encoded;
      match OrderIndices.get_no_cache annotation with
      | Some index -> index
      | None -> failwith "read-your-own-write consistency was violated")


let indices annotations =
  let replace_if_new annotation = function
    | Some got_in_batch -> got_in_batch
    | None -> index annotation
  in
  Core.Set.to_list annotations
  |> OrderIndices.KeySet.of_list
  |> OrderIndices.get_batch
  |> OrderIndices.KeyMap.mapi replace_if_new
  |> OrderIndices.KeyMap.values
  |> Set.of_list


let annotation index =
  (* Because we seal the type, we can be sure this will always succeed *)
  OrderAnnotations.get_exn index
