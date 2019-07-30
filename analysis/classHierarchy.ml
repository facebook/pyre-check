(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

(* `edges` mapping from type index to a set of targets. `indices` mapping from annotation to its
   vertex index. `annotations` inverse of `indices`. *)
open Core
open Ast
open Pyre
module Callable = Type.Callable

exception Cyclic

exception Incomplete

exception InconsistentMethodResolutionOrder of Type.Primitive.t

exception Untracked of Type.t

module Target = struct
  type t = {
    target: int;
    parameters: Type.OrderedTypes.t;
  }
  [@@deriving compare, eq, sexp, show]

  module type ListOrSet = sig
    type record

    val filter : record -> f:(t -> bool) -> record

    val is_empty : record -> bool

    val exists : record -> f:(t -> bool) -> bool

    val iter : record -> f:(t -> unit) -> unit

    val equal : record -> record -> bool

    val mem : record -> t -> bool

    val to_string : f:(t -> string) -> record -> string

    val fold : record -> init:'accum -> f:('accum -> t -> 'accum) -> 'accum

    val empty : record

    val add : record -> t -> record
  end

  module Set = struct
    include Set.Make (struct
      type nonrec t = t

      let compare = compare

      let sexp_of_t = sexp_of_t

      let t_of_sexp = t_of_sexp
    end)

    type record = t

    let to_string ~f set = to_list set |> List.to_string ~f
  end

  let target { target; _ } = target

  let target_equal = equal

  module List = struct
    type record = t list

    include List

    let mem = List.mem ~equal:target_equal

    let equal = List.equal target_equal

    let add list element = element :: list

    let empty = []
  end
end

let generic_primitive = "typing.Generic"

let object_primitive = "object"

let integer = "int"

let float = "float"

let complex = "complex"

type t = {
  edges: Target.t list Int.Table.t;
  backedges: Target.Set.t Int.Table.t;
  indices: int Type.Primitive.Table.t;
  annotations: Type.Primitive.t Int.Table.t;
}

module type Handler = sig
  type ('key, 'table) lookup

  val edges : unit -> (int, Target.t list) lookup

  val backedges : unit -> (int, Target.Set.t) lookup

  val indices : unit -> (Type.Primitive.t, int) lookup

  val annotations : unit -> (int, Type.Primitive.t) lookup

  val find : ('key, 'value) lookup -> 'key -> 'value option

  val find_unsafe : ('key, 'value) lookup -> 'key -> 'value

  val contains : ('key, 'value) lookup -> 'key -> bool

  val set : ('key, 'value) lookup -> key:'key -> data:'value -> unit

  val add_key : int -> unit

  val keys : unit -> int list

  val length : ('key, 'value) lookup -> int

  val show : unit -> string
end

let pp format { edges; backedges; annotations; _ } =
  let print_edge (source, targets) =
    let annotation index = Hashtbl.find_exn annotations index in
    let targets =
      let target { Target.target; parameters } =
        Format.asprintf "%s [%a]" (annotation target) Type.OrderedTypes.pp_concise parameters
      in
      targets |> List.map ~f:target |> String.concat ~sep:", "
    in
    Format.fprintf format "  %s -> %s\n" (annotation source) targets
  in
  Format.fprintf format "Edges:\n";
  List.iter ~f:print_edge (Hashtbl.to_alist edges);
  Format.fprintf format "Back-edges:\n";
  Hashtbl.to_alist backedges |> List.Assoc.map ~f:Set.to_list |> List.iter ~f:print_edge


let show order = Format.asprintf "%a" pp order

let handler order =
  ( module struct
    type ('key, 'value) lookup = ('key, 'value) Hashtbl.t

    let edges () = order.edges

    let backedges () = order.backedges

    let indices () = order.indices

    let annotations () = order.annotations

    let find table key = Hashtbl.find table key

    let find_unsafe table key = Hashtbl.find_exn table key

    let contains table key = Hashtbl.mem table key

    let set table ~key ~data = Hashtbl.set table ~key ~data

    let add_key _ = ()

    let keys () = Hashtbl.keys order.annotations

    let length table = Hashtbl.length table

    let show () = show order
  end : Handler )


let index_of (module Handler : Handler) annotation =
  Handler.find_unsafe (Handler.indices ()) annotation


let insert (module Handler : Handler) annotation =
  if not (Handler.contains (Handler.indices ()) annotation) then (
    let annotations = Handler.annotations () in
    let index =
      let initial = Type.Primitive.hash annotation in
      let rec pick_index index =
        if Handler.contains annotations index then
          pick_index (index + 1)
        else
          index
      in
      pick_index initial
    in
    Handler.add_key index;
    Handler.set (Handler.indices ()) ~key:annotation ~data:index;
    Handler.set annotations ~key:index ~data:annotation;
    Handler.set (Handler.edges ()) ~key:index ~data:[];
    Handler.set (Handler.backedges ()) ~key:index ~data:Target.Set.empty )


let connect
    ?(parameters = Type.OrderedTypes.Concrete [])
    ((module Handler : Handler) as order)
    ~predecessor
    ~successor
  =
  if
    (not (Handler.contains (Handler.indices ()) predecessor))
    || not (Handler.contains (Handler.indices ()) successor)
  then
    Statistics.event
      ~name:"invalid type order connection"
      ~integers:[]
      ~normals:["Predecessor", predecessor; "Successor", successor]
      ()
  else
    let predecessor = index_of order predecessor in
    let successor = index_of order successor in
    let edges = Handler.edges () in
    let backedges = Handler.backedges () in
    (* Add edges. *)
    let successors = Handler.find edges predecessor |> Option.value ~default:[] in
    Handler.set
      edges
      ~key:predecessor
      ~data:({ Target.target = successor; parameters } :: successors);

    (* Add backedges. *)
    let predecessors =
      Handler.find backedges successor |> Option.value ~default:Target.Set.empty
    in
    Handler.set
      backedges
      ~key:successor
      ~data:(Set.add predecessors { Target.target = predecessor; parameters })


let disconnect_successors (module Handler : Handler) annotations =
  let edges = Handler.edges () in
  let backedges = Handler.backedges () in
  let keys_to_remove =
    List.filter_map annotations ~f:(Handler.find (Handler.indices ())) |> Int.Hash_set.of_list
  in
  let all_successors =
    let all_successors = Int.Hash_set.create () in
    let add_successors key =
      match Handler.find edges key with
      | Some successors ->
          List.iter successors ~f:(fun { Target.target; _ } -> Hash_set.add all_successors target)
      | None -> ()
    in
    Hash_set.iter keys_to_remove ~f:add_successors;
    all_successors
  in
  let remove_backedges successor =
    Handler.find backedges successor
    >>| (fun current_predecessors ->
          let new_predecessors =
            Set.filter
              ~f:(fun { Target.target; _ } -> not (Hash_set.mem keys_to_remove target))
              current_predecessors
          in
          Handler.set backedges ~key:successor ~data:new_predecessors)
    |> ignore
  in
  Hash_set.iter all_successors ~f:remove_backedges;
  let clear_edges key =
    match Handler.find edges key with
    | Some _ -> Handler.set edges ~key ~data:[]
    | None -> ()
  in
  Hash_set.iter keys_to_remove ~f:clear_edges


let contains (module Handler : Handler) annotation =
  Handler.contains (Handler.indices ()) annotation


let is_instantiated (module Handler : Handler) annotation =
  let is_invalid = function
    | Type.Variable { constraints = Type.Variable.Unary.Unconstrained; _ } -> true
    | Type.Primitive name
    | Type.Parametric { name; _ } ->
        not (Handler.contains (Handler.indices ()) name)
    | _ -> false
  in
  not (Type.exists ~predicate:is_invalid annotation)


let raise_if_untracked order annotation =
  if not (contains order annotation) then
    raise (Untracked (Type.Primitive annotation))


let method_resolution_order_linearize
    ((module Handler : Handler) as order)
    ~get_successors
    class_name
  =
  let rec merge = function
    | [] -> []
    | [single_linearized_parent] -> single_linearized_parent
    | linearized_successors ->
        let find_valid_head linearizations =
          let is_valid_head head =
            let not_in_tail target = function
              | [] -> true
              | _ :: tail -> not (List.exists ~f:(Identifier.equal target) tail)
            in
            List.for_all ~f:(not_in_tail head) linearizations
          in
          linearizations
          |> List.filter_map ~f:List.hd
          |> List.find ~f:is_valid_head
          |> function
          | Some head -> head
          | None -> raise (InconsistentMethodResolutionOrder class_name)
        in
        let strip_head head = function
          | [] -> None
          | [successor_head] when Identifier.equal successor_head head -> None
          | successor_head :: tail when Identifier.equal successor_head head -> Some tail
          | successor -> Some successor
        in
        let head = find_valid_head linearized_successors in
        let linearized_successors = List.filter_map ~f:(strip_head head) linearized_successors in
        head :: merge linearized_successors
  in
  let rec linearize class_name =
    let linearized_successors =
      let create_annotation { Target.target = index; _ } =
        index |> Handler.find_unsafe (Handler.annotations ())
      in
      index_of order class_name
      |> get_successors
      |> Option.value ~default:[]
      |> List.map ~f:create_annotation
      |> List.map ~f:linearize
    in
    class_name :: merge linearized_successors
  in
  linearize class_name


let successors ((module Handler : Handler) as order) annotation =
  let linearization =
    method_resolution_order_linearize
      ~get_successors:(Handler.find (Handler.edges ()))
      order
      annotation
  in
  match linearization with
  | _ :: successors -> successors
  | [] -> []


type variables =
  | Unaries of Type.Variable.Unary.t list
  | ListVariadic of Type.Variable.Variadic.List.t
[@@deriving compare, eq, sexp, show]

let clean = function
  | Type.OrderedTypes.Concrete parameters ->
      List.map parameters ~f:(function
          | Type.Variable variable -> Some variable
          | _ -> None)
      |> Option.all
      >>| fun unaries -> Unaries unaries
  | Variable variable -> Some (ListVariadic variable)
  | Map _
  | Any ->
      None


let variables ?(default = None) (module Handler : Handler) = function
  | "type" ->
      (* Despite what typeshed says, typing.Type is covariant:
         https://www.python.org/dev/peps/pep-0484/#the-type-of-class-objects *)
      Some (Unaries [Type.Variable.Unary.create ~variance:Covariant "_T_meta"])
  | "typing.Callable" ->
      (* This is not the "real" typing.Callable. We are just proxying to the Callable instance in
         the type order here. *)
      Some (Unaries [Type.Variable.Unary.create ~variance:Covariant "_T_meta"])
  | node -> (
      let edges =
        Handler.find (Handler.indices ()) generic_primitive
        >>= fun generic_index ->
        Handler.find (Handler.indices ()) node
        >>= fun primitive_index ->
        Handler.find (Handler.edges ()) primitive_index
        >>= List.find ~f:(fun { Target.target; _ } -> target = generic_index)
        >>| fun { Target.parameters; _ } -> parameters
      in
      match edges with
      | None -> default
      | Some edges -> clean edges )


let get_generic_parameters ~generic_index edges =
  let generic_parameters { Target.target; parameters } =
    match generic_index with
    | Some index when index = target -> Some parameters
    | _ -> None
  in
  List.find_map ~f:generic_parameters edges


let least_common_successor ((module Handler : Handler) as order) ~successors left right =
  raise_if_untracked order left;
  raise_if_untracked order right;
  if Type.Primitive.compare left right = 0 then
    [left]
  else
    (let rec iterate left right =
       let successors sources =
         Set.fold
           ~init:Int.Set.empty
           ~f:(fun sofar index -> Set.union sofar (successors index))
           sources
       in
       let left_successors = successors (List.hd_exn left) in
       let right_successors = successors (List.hd_exn right) in
       if Set.is_empty left_successors && Set.is_empty right_successors then
         []
       else
         let intersect left right =
           let collect = List.fold ~init:Int.Set.empty ~f:Set.union in
           Set.inter (collect left) (collect right)
         in
         let left = left_successors :: left in
         let right = right_successors :: right in
         let left_tail_right = intersect (List.tl_exn left) right in
         let left_right_tail = intersect left (List.tl_exn right) in
         if (not (Set.is_empty left_tail_right)) || not (Set.is_empty left_right_tail) then
           Set.union left_tail_right left_right_tail |> Set.to_list
         else
           let left_right = intersect left right in
           if not (Set.is_empty left_right) then
             Set.to_list left_right
           else
             iterate left right
     in
     iterate [Int.Set.of_list [index_of order left]] [Int.Set.of_list [index_of order right]])
    |> List.map ~f:(Handler.find_unsafe (Handler.annotations ()))


let least_upper_bound ((module Handler : Handler) as order) =
  let successors index =
    match Handler.find (Handler.edges ()) index with
    | Some targets -> targets |> List.map ~f:Target.target |> Int.Set.of_list
    | None -> Int.Set.empty
  in
  least_common_successor order ~successors


let greatest_lower_bound ((module Handler : Handler) as order) =
  let predecessors index =
    match Handler.find (Handler.backedges ()) index with
    | Some targets -> Set.to_list targets |> List.map ~f:Target.target |> Int.Set.of_list
    | None -> Int.Set.empty
  in
  least_common_successor order ~successors:predecessors


let deduplicate (module Handler : Handler) ~annotations =
  let edges = Handler.edges () in
  let backedges = Handler.backedges () in
  let deduplicate_annotation index =
    let module Deduplicator (ListOrSet : Target.ListOrSet) = struct
      let deduplicate edges =
        let keep_first (visited, edges) ({ Target.target; _ } as edge) =
          if Set.mem visited target then
            visited, edges
          else
            Set.add visited target, ListOrSet.add edges edge
        in
        let deduplicate found =
          ListOrSet.fold found ~f:keep_first ~init:(Int.Set.empty, ListOrSet.empty) |> snd
        in
        match Handler.find edges index with
        | Some found -> Handler.set edges ~key:index ~data:(deduplicate found)
        | None -> ()
    end
    in
    let module EdgeDeduplicator = Deduplicator (Target.List) in
    let module BackedgeDeduplicator = Deduplicator (Target.Set) in
    EdgeDeduplicator.deduplicate edges;
    BackedgeDeduplicator.deduplicate backedges
  in
  annotations
  |> List.map ~f:(Handler.find_unsafe (Handler.indices ()))
  |> List.iter ~f:deduplicate_annotation


let remove_extra_edges_to_object (module Handler : Handler) annotations =
  let edges = Handler.edges () in
  let index_of annotation = Handler.find_unsafe (Handler.indices ()) annotation in
  let keys = List.map annotations ~f:index_of in
  let backedges = Handler.backedges () in
  let object_index = index_of object_primitive in
  let remove_extra_references key =
    Handler.find edges key
    >>| (fun connected ->
          let disconnected =
            Target.List.filter connected ~f:(fun { Target.target; _ } -> target <> object_index)
          in
          if Target.List.is_empty disconnected then
            []
          else (
            Handler.set edges ~key ~data:disconnected;
            [key] ))
    |> Option.value ~default:[]
  in
  let removed_indices = List.concat_map ~f:remove_extra_references keys |> Int.Set.of_list in
  Handler.find backedges object_index
  >>| (fun edges ->
        let edges =
          Target.Set.filter edges ~f:(fun { Target.target; _ } ->
              not (Set.mem removed_indices target))
        in
        Handler.set backedges ~key:object_index ~data:edges)
  |> Option.value ~default:()


let is_transitive_successor ((module Handler : Handler) as handler) ~source ~target =
  raise_if_untracked handler source;
  raise_if_untracked handler target;
  let worklist = Queue.create () in
  Queue.enqueue worklist { Target.target = index_of handler source; parameters = Concrete [] };
  let rec iterate worklist =
    match Queue.dequeue worklist with
    | Some { Target.target = current; _ } ->
        if current = index_of handler target then
          true
        else
          let enqueue_all targets = List.iter targets ~f:(Queue.enqueue worklist) in
          Option.iter (Handler.find (Handler.edges ()) current) ~f:enqueue_all;
          iterate worklist
    | None -> false
  in
  iterate worklist


let instantiate_successors_parameters ((module Handler : Handler) as handler) ~source ~target =
  raise_if_untracked handler target;
  let generic_index = Handler.find (Handler.indices ()) generic_primitive in
  match source with
  | Type.Bottom ->
      let set_to_anys = function
        | Type.OrderedTypes.Concrete concrete ->
            List.map concrete ~f:(fun _ -> Type.Any) |> fun anys -> Type.OrderedTypes.Concrete anys
        | Map _
        | Variable _
        | Any ->
            Type.OrderedTypes.Any
      in
      index_of handler target
      |> Handler.find (Handler.edges ())
      >>= get_generic_parameters ~generic_index
      >>| set_to_anys
  | _ ->
      let split =
        match Type.split source with
        | Primitive primitive, _ when not (contains handler primitive) -> None
        | Primitive "tuple", parameters ->
            let union = Type.OrderedTypes.union_upper_bound parameters |> Type.weaken_literals in
            Some ("tuple", Type.OrderedTypes.Concrete [union])
        | Primitive primitive, parameters -> Some (primitive, parameters)
        | _ ->
            (* We can only propagate from those that actually split off a primitive *)
            None
      in
      let handle_split (primitive, parameters) =
        let worklist = Queue.create () in
        Queue.enqueue worklist { Target.target = index_of handler primitive; parameters };
        let rec iterate worklist =
          match Queue.dequeue worklist with
          | Some { Target.target = target_index; parameters } ->
              let instantiated_successors =
                (* If a node on the graph has Generic[_T1, _T2, ...] as a supertype and has
                   concrete parameters, all occurrences of _T1, _T2, etc. in other supertypes need
                   to be replaced with the concrete parameter corresponding to the type variable.
                   This function takes a target with concrete parameters and its supertypes, and
                   instantiates the supertypes accordingly. *)
                let get_instantiated_successors ~generic_index ~parameters successors =
                  let variables =
                    get_generic_parameters successors ~generic_index
                    >>= clean
                    |> Option.value ~default:(Unaries [])
                  in
                  let replacement =
                    match variables with
                    | Unaries variables -> (
                        let zipped =
                          match parameters with
                          | Type.OrderedTypes.Concrete parameters -> (
                            match List.zip variables parameters with
                            | Ok zipped -> Some zipped
                            | _ -> None )
                          | Variable _
                          | Any
                          | Map _ ->
                              None
                        in
                        match zipped with
                        | Some pairs ->
                            List.map pairs ~f:(fun (variable, parameter) ->
                                Type.Variable.UnaryPair (variable, parameter))
                        | None ->
                            (* This is the specified behavior for empty parameters, and other
                               mismatched lengths should have an error at the declaration site, and
                               this behavior seems reasonable *)
                            List.map variables ~f:(fun variable ->
                                Type.Variable.UnaryPair (variable, Type.Any)) )
                    | ListVariadic variable ->
                        [Type.Variable.ListVariadicPair (variable, parameters)]
                  in
                  let replacement = TypeConstraints.Solution.create replacement in
                  let instantiate_parameters { Target.target; parameters } =
                    {
                      Target.target;
                      parameters =
                        TypeConstraints.Solution.instantiate_ordered_types replacement parameters;
                    }
                  in
                  List.map successors ~f:instantiate_parameters
                in
                Handler.find (Handler.edges ()) target_index
                >>| get_instantiated_successors ~generic_index ~parameters
              in
              if target_index = index_of handler target then
                match target with
                | "typing.Callable" -> Some parameters
                | _ -> instantiated_successors >>= get_generic_parameters ~generic_index
              else (
                instantiated_successors >>| List.iter ~f:(Queue.enqueue worklist) |> ignore;
                iterate worklist )
          | None -> None
        in
        iterate worklist
      in
      split >>= handle_split


let instantiate_predecessors_parameters
    ((module Handler : Handler) as handler)
    ~source
    ~target
    ~step
  =
  match Type.split source with
  | Type.Primitive primitive, parameters ->
      raise_if_untracked handler primitive;
      raise_if_untracked handler target;
      let generic_index = Handler.find (Handler.indices ()) generic_primitive in
      let worklist = Queue.create () in
      Queue.enqueue worklist { Target.target = index_of handler primitive; parameters };
      let rec iterate worklist =
        match Queue.dequeue worklist with
        | Some { Target.target = target_index; parameters } ->
            let get_instantiated_predecessors
                (module Handler : Handler)
                ~generic_index
                ~parameters
                ~step
                predecessors
              =
              let instantiate { Target.target; parameters = predecessor_variables } =
                let generic_parameters =
                  Handler.find (Handler.edges ()) target
                  >>= get_generic_parameters ~generic_index
                  |> Option.value ~default:(Type.OrderedTypes.Concrete [])
                in
                (* Mappings from the generic variables, as they appear in the predecessor, to the
                   instantiated parameter in the current annotation. For example, given:

                   Derived(Base[T1, int, T2], Generic[ ...irrelevant... ])

                   and an instantiated: Base[str, int, float] This mapping would include: { T1 =>
                   str; T2 => float } *)
                let handle_substitutions substitutions =
                  {
                    Target.target;
                    parameters =
                      TypeConstraints.Solution.instantiate_ordered_types
                        substitutions
                        generic_parameters;
                  }
                in
                step ~predecessor_variables ~parameters >>| handle_substitutions
              in
              List.filter_map predecessors ~f:instantiate
            in
            if target_index = index_of handler target then
              Some parameters
            else (
              Handler.find (Handler.backedges ()) target_index
              >>| Set.to_list
              >>| get_instantiated_predecessors handler ~generic_index ~parameters ~step
              >>| List.iter ~f:(Queue.enqueue worklist)
              |> ignore;
              iterate worklist )
        | None -> None
      in
      iterate worklist
  | _ ->
      (* We can't propagate from something that does not split off a primitive *)
      None


let connect_annotations_to_object ((module Handler : Handler) as handler) annotations =
  let indices = Handler.indices () in
  let connect_to_top annotation =
    let index = Handler.find_unsafe indices annotation in
    let annotation = Handler.find_unsafe (Handler.annotations ()) index in
    if not (is_transitive_successor handler ~source:object_primitive ~target:annotation) then
      match Handler.find (Handler.edges ()) index with
      | Some targets when List.length targets > 0 -> ()
      | _ -> connect handler ~predecessor:annotation ~successor:object_primitive
  in
  List.iter ~f:connect_to_top annotations


let check_integrity (module Handler : Handler) =
  (* Ensure keys are consistent. *)
  let key_consistent key =
    let raise_if_none value =
      if Option.is_none value then (
        Log.error "Inconsistency in type order: No value for key %d" key;
        raise Incomplete )
    in
    raise_if_none (Handler.find (Handler.edges ()) key);
    raise_if_none (Handler.find (Handler.backedges ()) key);
    raise_if_none (Handler.find (Handler.annotations ()) key);
    let annotation = Option.value_exn (Handler.find (Handler.annotations ()) key) in
    raise_if_none (Handler.find (Handler.indices ()) annotation)
  in
  List.iter ~f:key_consistent (Handler.keys ());

  (* Check for cycles. *)
  let started_from = ref Int.Set.empty in
  let find_cycle start =
    if not (Set.mem !started_from start) then
      let rec visit reverse_visited index =
        if List.mem ~equal:Int.equal reverse_visited index then (
          let trace =
            List.rev_map
              ~f:(Handler.find_unsafe (Handler.annotations ()))
              (index :: reverse_visited)
            |> String.concat ~sep:" -> "
          in
          Log.error "Order is cyclic:\nTrace: %s" (* (Handler.show ()) *) trace;
          raise Cyclic )
        else if not (Set.mem !started_from index) then (
          started_from := Set.add !started_from index;
          match Handler.find (Handler.edges ()) index with
          | Some successors ->
              successors
              |> List.map ~f:Target.target
              |> List.iter ~f:(visit (index :: reverse_visited))
          | None -> () )
      in
      visit [] start
  in
  Handler.keys () |> List.iter ~f:find_cycle;

  (* Check that backedges are complete. *)
  let module InverseChecker (Edges : Target.ListOrSet) (Backedges : Target.ListOrSet) = struct
    let check_inverse ~get_keys ~edges ~backedges =
      let check_backedge index =
        let check_backedge { Target.target; _ } =
          let has_backedge =
            match Handler.find backedges target with
            | Some targets ->
                Backedges.exists ~f:(fun { Target.target; _ } -> target = index) targets
            | None -> false
          in
          if not has_backedge then (
            Log.error
              "No back-edge found for %s -> %s"
              (Handler.find_unsafe (Handler.annotations ()) index)
              (Handler.find_unsafe (Handler.annotations ()) target);
            raise Incomplete )
        in
        Edges.iter ~f:check_backedge (Handler.find_unsafe edges index)
      in
      get_keys () |> List.iter ~f:check_backedge
  end
  in
  let module ForwardCheckInverse = InverseChecker (Target.List) (Target.Set) in
  ForwardCheckInverse.check_inverse
    ~get_keys:Handler.keys
    ~edges:(Handler.edges ())
    ~backedges:(Handler.backedges ());
  let module ReverseCheckInverse = InverseChecker (Target.Set) (Target.List) in
  ReverseCheckInverse.check_inverse
    ~get_keys:Handler.keys
    ~edges:(Handler.backedges ())
    ~backedges:(Handler.edges ())


let to_dot (module Handler : Handler) =
  let indices = List.sort ~compare (Handler.keys ()) in
  let nodes =
    List.map indices ~f:(fun index -> index, Handler.find_unsafe (Handler.annotations ()) index)
  in
  let buffer = Buffer.create 10000 in
  Buffer.add_string buffer "digraph {\n";
  List.iter
    ~f:(fun (index, annotation) ->
      Format.asprintf "  %d[label=\"%s\"]\n" index annotation |> Buffer.add_string buffer)
    nodes;
  let add_edges index =
    Handler.find (Handler.edges ()) index
    >>| List.sort ~compare
    >>| List.iter ~f:(fun { Target.target = successor; parameters } ->
            Format.asprintf "  %d -> %d" index successor |> Buffer.add_string buffer;
            if not (Type.OrderedTypes.equal parameters (Concrete [])) then
              Format.asprintf "[label=\"(%a)\"]" Type.OrderedTypes.pp_concise parameters
              |> Buffer.add_string buffer;
            Buffer.add_string buffer "\n")
    |> ignore
  in
  List.iter ~f:add_edges indices;
  Buffer.add_string buffer "}";
  Buffer.contents buffer


module Builder = struct
  let create () =
    {
      edges = Int.Table.create ();
      backedges = Int.Table.create ();
      indices = Type.Primitive.Table.create ();
      annotations = Int.Table.create ();
    }


  let copy { edges; backedges; indices; annotations } =
    {
      edges = Hashtbl.copy edges;
      backedges = Hashtbl.copy backedges;
      indices = Hashtbl.copy indices;
      annotations = Hashtbl.copy annotations;
    }


  let default_annotations =
    let singleton annotation = [annotation; object_primitive] in
    [ [object_primitive];
      (* Special forms *)
      singleton "typing.Annotated";
      singleton "typing.Tuple";
      singleton "typing.NamedTuple";
      singleton generic_primitive;
      singleton "typing.GenericMeta";
      singleton "typing.Protocol";
      singleton "typing.Callable";
      singleton "typing.FrozenSet";
      singleton "typing.Optional";
      singleton "typing.TypeVar";
      singleton "typing.Undeclared";
      singleton "typing.Union";
      singleton "typing.NoReturn";
      (* Ensure unittest.mock.Base is there because we check against it. *)
      singleton "unittest.mock.Base";
      singleton "unittest.mock.NonCallableMock";
      singleton "typing.ClassVar";
      singleton "typing.Final";
      singleton "typing_extensions.Final";
      singleton "typing_extensions.Literal";
      ["dict"; "typing.Dict"; object_primitive];
      singleton "None";
      (* Numerical hierarchy. *)
      [integer; float; complex; "numbers.Complex"; "numbers.Number"; object_primitive];
      [integer; "numbers.Integral"; object_primitive];
      [float; "numbers.Rational"; object_primitive];
      [float; "numbers.Real"; object_primitive] ]


  let builtin_types = List.concat default_annotations |> Type.Primitive.Set.of_list

  let add_default_order handler =
    Set.iter builtin_types ~f:(insert handler);
    let rec connect_primitive_chain annotations =
      match annotations with
      | predecessor :: successor :: rest ->
          connect handler ~predecessor ~successor;
          connect_primitive_chain (successor :: rest)
      | _ -> ()
    in
    List.iter ~f:connect_primitive_chain default_annotations;

    (* Since the builtin type hierarchy is not primitive, it's special cased. *)
    let type_builtin = "type" in
    let type_variable = Type.Variable (Type.Variable.Unary.create "_T") in
    insert handler type_builtin;
    connect
      handler
      ~predecessor:type_builtin
      ~parameters:(Concrete [type_variable])
      ~successor:generic_primitive;
    let typed_dictionary = "TypedDictionary" in
    let non_total_typed_dictionary = "NonTotalTypedDictionary" in
    let typing_mapping = "typing.Mapping" in
    insert handler non_total_typed_dictionary;
    insert handler typed_dictionary;
    insert handler typing_mapping;
    connect handler ~predecessor:non_total_typed_dictionary ~successor:typed_dictionary;
    connect
      handler
      ~predecessor:typed_dictionary
      ~parameters:(Concrete [Type.string; Type.Any])
      ~successor:typing_mapping


  let default () =
    let order = create () in
    let handler = handler order in
    add_default_order handler;
    order
end
