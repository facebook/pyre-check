(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Pyre

module Type = AnalysisType

type index = {
  function_keys: (Access.t Hash_set.t) String.Table.t;
  class_keys: (Type.t Hash_set.t) String.Table.t;
  alias_keys: (Type.t Hash_set.t) String.Table.t;
  global_keys: (Access.t Hash_set.t) String.Table.t;
  dependent_keys: (string Hash_set.t) String.Table.t;
}


type t = {
  index: index;
  dependents: (string list) String.Table.t;
}


module type Handler = sig
  val add_function_key: path: string -> Access.t -> unit
  val add_class_key: path: string -> Type.t -> unit
  val add_alias_key: path: string -> Type.t -> unit
  val add_global_key: path: string -> Access.t -> unit
  val add_dependent_key: path: string -> string -> unit

  val add_dependent: path: string -> string -> unit
  val dependents: string -> (string list) option

  val get_function_keys: path: string -> Access.t list
  val get_class_keys: path: string -> Type.t list
  val get_alias_keys: path: string -> Type.t list
  val get_global_keys: path: string -> Access.t list
  val get_dependent_keys: path: string -> string list

  val clear_keys_batch: string list -> unit
end


let handler {
    index = { function_keys; class_keys; alias_keys; global_keys; dependent_keys };
    dependents;
  } =
  (module struct
    let add_function_key ~path name =
      match Hashtbl.find function_keys path with
      | None ->
          Hashtbl.set
            function_keys
            ~key:path
            ~data:(Access.Hash_set.of_list [name])
      | Some hash_set ->
          Hash_set.add hash_set name


    let add_class_key ~path class_type =
      match Hashtbl.find class_keys path with
      | None ->
          Hashtbl.set
            class_keys
            ~key:path
            ~data:(Type.Hash_set.of_list [class_type])
      | Some hash_set ->
          Hash_set.add hash_set class_type


    let add_alias_key ~path alias =
      match Hashtbl.find alias_keys path with
      | None ->
          Hashtbl.set
            alias_keys
            ~key:path
            ~data:(Type.Hash_set.of_list [alias])
      | Some hash_set ->
          Hash_set.add hash_set alias


    let add_global_key ~path global =
      match Hashtbl.find global_keys path with
      | None ->
          Hashtbl.set
            global_keys
            ~key:path
            ~data:(Access.Hash_set.of_list [global])
      | Some hash_set ->
          Hash_set.add hash_set global


    let add_dependent_key ~path dependent =
      match Hashtbl.find dependent_keys path with
      | None ->
          Hashtbl.set
            dependent_keys
            ~key:path
            ~data:(String.Hash_set.of_list [dependent])
      | Some hash_set ->
          Hash_set.add hash_set dependent


    let add_dependent ~path dependent =
      add_dependent_key ~path dependent;
      Hashtbl.add_multi ~key:dependent ~data:path dependents


    let dependents = Hashtbl.find dependents


    let get_function_keys ~path =
      Hashtbl.find function_keys path
      >>| Hash_set.to_list
      |> Option.value ~default:[]


    let get_class_keys ~path =
      Hashtbl.find class_keys path
      >>| Hash_set.to_list
      |> Option.value ~default:[]


    let get_alias_keys ~path =
      Hashtbl.find alias_keys path
      >>| Hash_set.to_list
      |> Option.value ~default:[]


    let get_global_keys ~path =
      Hashtbl.find global_keys path
      >>| Hash_set.to_list
      |> Option.value ~default:[]


    let get_dependent_keys ~path =
      Hashtbl.find dependent_keys path
      >>| Hash_set.to_list
      |> Option.value ~default:[]


    let clear_keys_batch paths =
      List.iter ~f:(Hashtbl.remove function_keys) paths;
      List.iter ~f:(Hashtbl.remove class_keys) paths;
      List.iter ~f:(Hashtbl.remove alias_keys) paths;
      List.iter ~f:(Hashtbl.remove global_keys) paths;
      List.iter ~f:(Hashtbl.remove dependent_keys) paths
  end: Handler)


let create () =
  let index = {
    function_keys = String.Table.create ();
    class_keys = String.Table.create ();
    alias_keys = String.Table.create ();
    global_keys = String.Table.create ();
    dependent_keys = String.Table.create ();
  }
  in
  { index = index; dependents = String.Table.create (); }


let copy {
    index = { function_keys; class_keys; alias_keys; global_keys; dependent_keys };
    dependents } =
  {
    index = {
      function_keys = Hashtbl.copy function_keys;
      class_keys = Hashtbl.copy class_keys;
      alias_keys = Hashtbl.copy alias_keys;
      global_keys = Hashtbl.copy global_keys;
      dependent_keys = Hashtbl.copy dependent_keys;
    };
    dependents = Hashtbl.copy dependents;
  }


let transitive ~get_dependencies ~path =
  let transitive_closure path =
    let rec closure ~visited node =
      if Set.mem visited node then
        visited
      else
        let visited = Set.add visited node in
        match get_dependencies node with
        | None -> visited
        | Some neighbors ->
            List.fold
              ~init:visited
              ~f:(fun visited neighbor ->
                  closure ~visited neighbor)
              neighbors
    in
    closure ~visited:String.Set.empty path
    |> fun paths -> Set.remove paths path
  in
  transitive_closure path


let transitive_of_list ~get_dependencies ~paths =
  paths
  |> List.map ~f:(fun path -> transitive ~get_dependencies ~path)
  |> List.fold ~init:String.Set.empty ~f:Set.union
  (* Ensure no file gets double-checked. *)
  |> (fun dependents -> Set.diff dependents (String.Set.of_list paths))


let of_list ~get_dependencies ~paths =
  let fold_dependents dependents path =
    get_dependencies path
    >>| String.Set.of_list
    >>| Set.union dependents
    |> Option.value ~default:dependents
  in
  List.fold
    ~init:String.Set.empty
    ~f:fold_dependents
    paths
  |> (fun dependents -> Set.diff dependents (String.Set.of_list paths))
