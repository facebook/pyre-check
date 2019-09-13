(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Expression
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

  val add_alias_key : qualifier:Reference.t -> Identifier.t -> unit

  val add_global_key : qualifier:Reference.t -> Reference.t -> unit

  val add_dependent_key : qualifier:Reference.t -> Reference.t -> unit

  val add_dependent : qualifier:Reference.t -> Reference.t -> unit

  val dependents : Reference.t -> Reference.Set.Tree.t option

  val get_function_keys : qualifier:Reference.t -> Reference.t list

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
        class_name: Reference.t;
        direct_target: Reference.t;
        dispatch: dispatch;
        is_optional_class_attribute: bool;
      }
  [@@deriving compare, hash, sexp, eq, show, to_yojson]

  type callee_with_locations = {
    callee: callee;
    locations: Location.Reference.t list;
  }

  include Hashable.Make (struct
    type t = callee [@@deriving compare, hash, sexp]
  end)

  let callee_to_yojson ?locations callee =
    let locations =
      match locations with
      | None -> []
      | Some locations ->
          ["locations", `List (List.map locations ~f:Location.Instantiated.to_yojson)]
    in
    match callee with
    | Function name ->
        `Assoc
          (List.rev_append
             locations
             ["kind", `String "function"; "target", `String (Reference.show name)])
    | Method { direct_target; class_name; dispatch; is_optional_class_attribute } ->
        `Assoc
          (List.rev_append
             locations
             [
               "kind", `String "method";
               "is_optional_class_attribute", `Bool is_optional_class_attribute;
               "direct_target", `String (Reference.show direct_target);
               "class_name", `String (Reference.show class_name);
               ( "dispatch",
                 `String
                   ( match dispatch with
                   | Dynamic -> "dynamic"
                   | Static -> "static" ) );
             ])


  module CalleeValue = struct
    type t = callee_with_locations list

    let prefix = Prefix.make ()

    let description = "Reference List"

    let unmarshall value = Marshal.from_string value 0
  end

  module SharedMemory = SharedMemory.WithCache.Make (SharedMemoryKeys.ReferenceKey) (CalleeValue)

  let set ~caller ~callees = SharedMemory.add caller callees

  let get ~caller = SharedMemory.get caller |> Option.value ~default:[]

  module type Builder = sig
    val initialize : unit -> unit

    val add_callee
      :  global_resolution:GlobalResolution.t ->
      target:Type.t option ->
      callables:Type.Callable.t list option ->
      dynamic:bool ->
      callee:Expression.t ->
      unit

    val add_property_callees
      :  global_resolution:GlobalResolution.t ->
      resolved_base:Type.t ->
      attributes:(AnnotatedAttribute.t * Reference.t) list ->
      name:string ->
      location:Location.t ->
      unit

    val get_all_callees : unit -> callee_with_locations list
  end

  module DefaultBuilder : Builder = struct
    let table = Location.Reference.Table.create ()

    let initialize () = Hashtbl.clear table

    let add_callee ~global_resolution ~target ~callables ~dynamic ~callee =
      (* Store callees. *)
      let callees =
        let method_callee ?(is_optional_class_attribute = false) annotation callable =
          match callable with
          | { Type.Callable.kind = Named direct_target; _ } ->
              let class_name =
                ( if Type.is_meta annotation then
                    Type.single_parameter annotation
                else
                  annotation )
                |> Type.class_name
              in
              [
                Method
                  {
                    direct_target;
                    class_name;
                    dispatch = (if dynamic then Dynamic else Static);
                    is_optional_class_attribute;
                  };
              ]
          | _ -> []
        in
        match target, callables with
        | Some (Type.Union elements), Some callables
          when List.length elements = List.length callables ->
            List.map2_exn elements callables ~f:method_callee |> List.concat
        | Some annotation, Some [callable] -> method_callee annotation callable
        | Some (Type.Optional annotation), _ -> (
          match Node.value callee with
          | Name (Name.Attribute { attribute; _ }) -> (
              GlobalResolution.attribute global_resolution ~parent:annotation ~name:attribute
              >>| AnnotatedAttribute.annotation
              >>| Annotation.annotation
              >>= (function
                    | Type.Callable callable -> Some callable
                    | _ -> None)
              >>| method_callee ~is_optional_class_attribute:true annotation
              |> function
              | None -> []
              | Some list -> list )
          | _ -> [] )
        | None, Some [{ Type.Callable.kind = Named define; _ }] -> [Function define]
        | _ -> []
      in
      Hashtbl.set table ~key:(Node.location callee) ~data:callees


    let add_property_callees ~global_resolution ~resolved_base ~attributes ~name ~location =
      let property_callables = ref [] in
      let register_attribute_callable ?(is_optional_class_attribute = false) class_name attribute =
        let direct_target_name =
          Annotated.Attribute.parent attribute
          |> Type.primitive_name
          >>| fun parent -> Reference.create ~prefix:(Reference.create parent) name
        in
        match direct_target_name with
        | Some direct_target ->
            property_callables :=
              Method { direct_target; class_name; dispatch = Dynamic; is_optional_class_attribute }
              :: !property_callables
        | None -> ()
      in
      let register (attribute, class_name) =
        if Option.is_some (Annotated.Attribute.property attribute) then
          register_attribute_callable class_name attribute
        (* As the callgraph is an overapproximation, we also have to consider property calls from
           optional attributes.*)
        else
          match resolved_base with
          | Type.Optional base -> (
              Annotated.Class.resolve_class ~resolution:global_resolution base
              |> function
              | Some [{ Annotated.Class.instantiated; class_attributes; class_definition }] ->
                  let attribute =
                    Annotated.Class.attribute
                      class_definition
                      ~transitive:true
                      ~class_attributes
                      ~special_method:false
                      ~resolution:global_resolution
                      ~name
                      ~instantiated
                  in
                  if Option.is_some (Annotated.Attribute.property attribute) then
                    register_attribute_callable
                      ~is_optional_class_attribute:true
                      (Annotated.Class.name class_definition)
                      attribute
              | Some _
              | None ->
                  () )
          | _ -> ()
      in
      List.iter attributes ~f:register;
      if not (List.is_empty !property_callables) then
        Hashtbl.set table ~key:location ~data:!property_callables


    let get_all_callees () =
      (* Sort the callees as a map from callee -> list of locations. *)
      let callees = Table.create () in
      let add_binding ~key ~data =
        List.iter data ~f:(fun callee -> Hashtbl.add_multi callees ~key:callee ~data:key)
      in
      Hashtbl.iteri table ~f:add_binding;
      Hashtbl.clear table;
      Hashtbl.to_alist callees |> List.map ~f:(fun (callee, locations) -> { callee; locations })
  end
end
