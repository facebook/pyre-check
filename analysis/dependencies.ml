(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Pyre
module SharedMemory = Memory

type index = {
  function_keys: Reference.t Hash_set.t Reference.Table.t;
  class_keys: Identifier.t Hash_set.t Reference.Table.t;
  alias_keys: Identifier.t Hash_set.t Reference.Table.t;
  global_keys: Reference.t Hash_set.t Reference.Table.t;
  dependent_keys: Reference.t Hash_set.t Reference.Table.t;
}

type t = {
  index: index;
  dependents: Reference.Set.t Reference.Table.t;
}

module type Handler = sig
  val add_function_key : qualifier:Reference.t -> Reference.t -> unit

  val add_class_key : qualifier:Reference.t -> Identifier.t -> unit

  val add_alias_key : qualifier:Reference.t -> Identifier.t -> unit

  val add_global_key : qualifier:Reference.t -> Reference.t -> unit

  val add_dependent_key : qualifier:Reference.t -> Reference.t -> unit

  val add_dependent : qualifier:Reference.t -> Reference.t -> unit

  val dependents : Reference.t -> Reference.Set.Tree.t option

  val get_function_keys : qualifier:Reference.t -> Reference.t list

  val get_class_keys : qualifier:Reference.t -> Identifier.t list

  val get_alias_keys : qualifier:Reference.t -> Identifier.t list

  val get_global_keys : qualifier:Reference.t -> Reference.t list

  val get_dependent_keys : qualifier:Reference.t -> Reference.t list

  val clear_keys_batch : Reference.t list -> unit

  val normalize : Reference.t list -> unit
end

let handler
    { index = { function_keys; class_keys; alias_keys; global_keys; dependent_keys }; dependents }
  =
  ( module struct
    let add_function_key ~qualifier name =
      match Hashtbl.find function_keys qualifier with
      | None -> Hashtbl.set function_keys ~key:qualifier ~data:(Reference.Hash_set.of_list [name])
      | Some hash_set -> Hash_set.add hash_set name


    let add_class_key ~qualifier class_type =
      match Hashtbl.find class_keys qualifier with
      | None ->
          Hashtbl.set class_keys ~key:qualifier ~data:(Identifier.Hash_set.of_list [class_type])
      | Some hash_set -> Hash_set.add hash_set class_type


    let add_alias_key ~qualifier alias =
      match Hashtbl.find alias_keys qualifier with
      | None -> Hashtbl.set alias_keys ~key:qualifier ~data:(Identifier.Hash_set.of_list [alias])
      | Some hash_set -> Hash_set.add hash_set alias


    let add_global_key ~qualifier global =
      match Hashtbl.find global_keys qualifier with
      | None -> Hashtbl.set global_keys ~key:qualifier ~data:(Reference.Hash_set.of_list [global])
      | Some hash_set -> Hash_set.add hash_set global


    let add_dependent_key ~qualifier dependent =
      match Hashtbl.find dependent_keys qualifier with
      | None ->
          Hashtbl.set dependent_keys ~key:qualifier ~data:(Reference.Hash_set.of_list [dependent])
      | Some hash_set -> Hash_set.add hash_set dependent


    let add_dependent ~qualifier dependent =
      add_dependent_key ~qualifier dependent;
      let update entry =
        match entry with
        | None -> Reference.Set.singleton qualifier
        | Some set -> Set.add set qualifier
      in
      Hashtbl.update dependents dependent ~f:update


    let dependents_table = dependents

    let dependents reference = Hashtbl.find dependents reference >>| Set.to_tree

    let get_function_keys ~qualifier =
      Hashtbl.find function_keys qualifier >>| Hash_set.to_list |> Option.value ~default:[]


    let get_class_keys ~qualifier =
      Hashtbl.find class_keys qualifier >>| Hash_set.to_list |> Option.value ~default:[]


    let get_alias_keys ~qualifier =
      Hashtbl.find alias_keys qualifier >>| Hash_set.to_list |> Option.value ~default:[]


    let get_global_keys ~qualifier =
      Hashtbl.find global_keys qualifier >>| Hash_set.to_list |> Option.value ~default:[]


    let get_dependent_keys ~qualifier =
      Hashtbl.find dependent_keys qualifier >>| Hash_set.to_list |> Option.value ~default:[]


    let clear_keys_batch qualifiers =
      List.iter ~f:(Hashtbl.remove function_keys) qualifiers;
      List.iter ~f:(Hashtbl.remove class_keys) qualifiers;
      List.iter ~f:(Hashtbl.remove alias_keys) qualifiers;
      List.iter ~f:(Hashtbl.remove global_keys) qualifiers;
      List.iter ~f:(Hashtbl.remove dependent_keys) qualifiers


    let normalize qualifiers =
      let normalize qualifier =
        match Hashtbl.find dependents_table qualifier with
        | Some unnormalized ->
            Reference.Set.to_list unnormalized
            |> List.sort ~compare:Reference.compare
            |> Reference.Set.of_list
            |> fun normalized -> Hashtbl.set dependents_table ~key:qualifier ~data:normalized
        | None -> ()
      in
      List.concat_map qualifiers ~f:(fun qualifier -> get_dependent_keys ~qualifier)
      |> List.dedup_and_sort ~compare:Reference.compare
      |> List.iter ~f:normalize
  end : Handler )


let create () =
  let index =
    {
      function_keys = Reference.Table.create ();
      class_keys = Reference.Table.create ();
      alias_keys = Reference.Table.create ();
      global_keys = Reference.Table.create ();
      dependent_keys = Reference.Table.create ();
    }
  in
  { index; dependents = Reference.Table.create () }


let copy
    { index = { function_keys; class_keys; alias_keys; global_keys; dependent_keys }; dependents }
  =
  {
    index =
      {
        function_keys = Hashtbl.copy function_keys;
        class_keys = Hashtbl.copy class_keys;
        alias_keys = Hashtbl.copy alias_keys;
        global_keys = Hashtbl.copy global_keys;
        dependent_keys = Hashtbl.copy dependent_keys;
      };
    dependents = Hashtbl.copy dependents;
  }


let transitive_of_list ~get_dependencies ~modules =
  let rec transitive ~visited ~frontier =
    if Reference.Set.Tree.is_empty frontier then
      visited
    else
      let visited = Reference.Set.Tree.union visited frontier in
      let frontier =
        let add_dependencies current node =
          match get_dependencies node with
          | Some dependencies -> Reference.Set.Tree.union current dependencies
          | None -> current
        in
        Reference.Set.Tree.fold frontier ~init:Reference.Set.Tree.empty ~f:add_dependencies
        |> fun new_frontier -> Reference.Set.Tree.diff new_frontier visited
      in
      transitive ~visited ~frontier
  in
  let modules = Reference.Set.Tree.of_list modules in
  transitive ~visited:Reference.Set.Tree.empty ~frontier:modules
  |> (fun dependents -> Reference.Set.Tree.diff dependents modules)
  |> Reference.Set.of_tree


let of_list ~get_dependencies ~modules =
  let fold_dependents dependents handle =
    get_dependencies handle
    >>| Reference.Set.of_tree
    >>| Set.union dependents
    |> Option.value ~default:dependents
  in
  List.fold ~init:Reference.Set.empty ~f:fold_dependents modules
  |> fun dependents -> Set.diff dependents (Reference.Set.of_list modules)


let to_dot ~get_dependencies ~qualifier =
  let nodes, edges =
    let rec iterate ~worklist ~visited ~result:((nodes, edges) as result) =
      match Queue.dequeue worklist with
      | Some reference ->
          let visited, nodes, edges =
            if not (Set.mem visited reference) then
              let visited = Set.add visited reference in
              let nodes = reference :: nodes in
              let dependencies =
                get_dependencies reference |> Option.value ~default:Reference.Set.Tree.empty
              in
              let enqueue edges dependency =
                if not (Set.mem visited dependency) then
                  Queue.enqueue worklist dependency;
                (reference, dependency) :: edges
              in
              let edges = Reference.Set.Tree.fold dependencies ~init:edges ~f:enqueue in
              visited, nodes, edges
            else
              visited, nodes, edges
          in
          iterate ~worklist ~visited ~result:(nodes, edges)
      | _ -> result
    in
    let worklist = Queue.create () in
    Queue.enqueue worklist qualifier;
    let nodes, edges = iterate ~worklist ~visited:Reference.Set.empty ~result:([], []) in
    List.rev nodes, List.rev edges
  in
  let buffer = Buffer.create 10000 in
  Buffer.add_string buffer "digraph {\n";
  let print_node reference =
    let label =
      Printf.sprintf
        "  %d[label=\"%s\"%s]\n"
        (Reference.hash reference)
        (Reference.show reference)
        ( if Reference.equal reference qualifier then
            " color=\"red\""
        else
          "" )
    in
    Buffer.add_string buffer label
  in
  let print_edge (source, dependency) =
    let edge =
      Printf.sprintf "  %d -> %d [dir=back]\n" (Reference.hash source) (Reference.hash dependency)
    in
    Buffer.add_string buffer edge
  in
  List.iter nodes ~f:print_node;
  List.iter edges ~f:print_edge;
  Buffer.add_string buffer "}";
  Buffer.contents buffer


module Callgraph = struct
  type dispatch =
    | Dynamic
    | Static

  and callee =
    | Function of Reference.t
    | Method of {
        direct_target: Reference.t;
        static_target: Reference.t;
        dispatch: dispatch;
      }
  [@@deriving compare, eq, show, to_yojson]

  let callee_to_yojson = function
    | Function name -> `Assoc ["kind", `String "function"; "target", `String (Reference.show name)]
    | Method { direct_target; static_target; dispatch } ->
        `Assoc
          [ "kind", `String "method";
            "direct_target", `String (Reference.show direct_target);
            "static_target", `String (Reference.show static_target);
            ( "dispatch",
              `String
                ( match dispatch with
                | Dynamic -> "dynamic"
                | Static -> "static" ) ) ]


  module CalleeValue = struct
    type t = callee list

    let prefix = Prefix.make ()

    let description = "Reference List"

    let unmarshall value = Marshal.from_string value 0
  end

  module SharedMemory = SharedMemory.WithCache (Reference.Key) (CalleeValue)

  let set ~caller ~callees = SharedMemory.add caller callees

  let get ~caller = SharedMemory.get caller |> Option.value ~default:[]
end
