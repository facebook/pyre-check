(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core
open Analysis
open EnvironmentSharedMemory

open Pyre

let compute_hashes_to_keys ~indices ~annotations =
  let add_index_mappings map key =
    Map.add_exn
      map
      ~key:(OrderAnnotations.hash_of_key key)
      ~data:(OrderAnnotations.serialize_key key)
    |> Map.add_exn
      ~key:(OrderEdges.hash_of_key key)
      ~data:(OrderEdges.serialize_key key)
    |> Map.add_exn
      ~key:(OrderBackedges.hash_of_key key)
      ~data:(OrderBackedges.serialize_key key)
  in
  let add_annotation_mappings map annotation =
    Map.add_exn
      map
      ~key:(OrderIndices.hash_of_key annotation)
      ~data:(OrderIndices.serialize_key annotation)
  in
  List.fold indices ~init:String.Map.empty ~f:add_index_mappings
  |> fun map -> List.fold annotations ~init:map ~f:add_annotation_mappings


module Handler = struct
  type ('key, 'value) lookup = {
    get: 'key -> 'value option;
    set: 'key -> 'value -> unit;
  }

  let edges () = {
    get = OrderEdges.get;
    set = (fun key value ->
        OrderEdges.remove_batch (OrderEdges.KeySet.singleton key);
        OrderEdges.add key value);
  }

  let backedges () = {
    get = OrderBackedges.get;
    set =
      (fun key value ->
         OrderBackedges.remove_batch (OrderBackedges.KeySet.singleton key);
         OrderBackedges.add key value);
  }

  let indices () = {
    get = OrderIndices.get;
    set =
      (fun key value ->
         OrderIndices.remove_batch (OrderIndices.KeySet.singleton key);
         OrderIndices.add key value);
  }

  let annotations () = {
    get = OrderAnnotations.get;
    set =
      (fun key value ->
         OrderAnnotations.remove_batch (OrderAnnotations.KeySet.singleton key);
         OrderAnnotations.add key value);
  }

  let find { get; _ } key = get key

  let find_unsafe { get; _ } key = Option.value_exn (get key)

  let contains { get; _ } key = Option.is_some (get key)

  let set { set; _ } ~key ~data =
    set key data

  let length _ =
    (OrderKeys.get "Order"
     >>| List.length)
    |> Option.value ~default:0

  let add_key key =
    match OrderKeys.get "Order" with
    | None -> OrderKeys.add "Order" [key]
    | Some keys ->
        OrderKeys.remove_batch (OrderKeys.KeySet.singleton "Order");
        OrderKeys.add "Order" (key :: keys)

  let keys () =
    Option.value ~default:[] (OrderKeys.get "Order")

  let show () =
    let keys =
      keys ()
      |> List.sort ~compare:Int.compare
    in
    let serialized_keys = List.to_string ~f:Int.to_string keys in
    let serialized_annotations =
      let serialize_annotation key =
        find (annotations ()) key
        >>| (fun annotation -> Format.asprintf "%d->%a\n" key Type.pp annotation)
      in
      List.filter_map ~f:serialize_annotation keys
      |> String.concat
    in
    let serialized_edges edges =
      let edges_of_key key =
        let show_successor { TypeOrder.Target.target = successor; _ } =
          Option.value_exn (find (annotations ()) successor)
          |> Type.show
        in
        Option.value ~default:[] (find edges key)
        |> List.to_string ~f:show_successor
      in
      List.to_string ~f:(fun key -> Format.asprintf "%d -> %s\n" key (edges_of_key key)) keys
    in
    Format.asprintf "Keys:\n%s\nAnnotations:\n%s\nEdges:\n%s\nBackedges:\n%s\n"
      serialized_keys
      serialized_annotations
      (serialized_edges (edges ()))
      (serialized_edges (backedges ()))
end
