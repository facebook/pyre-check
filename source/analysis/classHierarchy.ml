(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* `edges` mapping from type index to a set of targets. `indices` mapping from annotation to its
   vertex index. `annotations` inverse of `indices`. *)
open Core
open Ast
open Pyre

exception Cyclic of String.Hash_set.t

exception Incomplete

exception InconsistentMethodResolutionOrder of Type.Primitive.t

exception Untracked of string

module Target = struct
  type t = {
    target: IndexTracker.t;
    parameters: Type.Parameter.t list;
  }
  [@@deriving compare, sexp, show]

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

  let target_equal = [%compare.equal: t]

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

module type Handler = sig
  val edges : IndexTracker.t -> Target.t list option

  val extends_placeholder_stub : IndexTracker.t -> bool

  val contains : Type.Primitive.t -> bool
end

let index_of annotation = IndexTracker.index annotation

let contains (module Handler : Handler) = Handler.contains

let is_instantiated (module Handler : Handler) annotation =
  let is_invalid = function
    | Type.Variable { constraints = Type.Variable.Unconstrained; _ } -> true
    | Type.Primitive name
    | Type.Parametric { name; _ } ->
        not (contains (module Handler) name)
    | _ -> false
  in
  not (Type.exists ~predicate:is_invalid annotation)


let raise_if_untracked order annotation =
  if not (contains order annotation) then
    raise (Untracked annotation)


let method_resolution_order_linearize ~get_successors class_name =
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
  let rec linearize ~visited class_name =
    if Hash_set.mem visited class_name then (
      Log.error "Order is cyclic:\nTrace: {%s}" (Hash_set.to_list visited |> String.concat ~sep:", ");
      raise (Cyclic visited));
    let visited = Hash_set.copy visited in
    Hash_set.add visited class_name;
    let linearized_successors =
      let create_annotation { Target.target = index; _ } = IndexTracker.annotation index in
      index_of class_name
      |> get_successors
      |> Option.value ~default:[]
      |> List.map ~f:create_annotation
      |> List.map ~f:(linearize ~visited)
    in
    class_name :: merge linearized_successors
  in
  linearize ~visited:(String.Hash_set.create ()) class_name


let successors (module Handler : Handler) annotation =
  let linearization = method_resolution_order_linearize ~get_successors:Handler.edges annotation in
  match linearization with
  | _ :: successors -> successors
  | [] -> []


let immediate_parents (module Handler : Handler) class_name =
  index_of class_name
  |> Handler.edges
  >>| List.map ~f:(fun target -> Target.target target |> IndexTracker.annotation)
  |> Option.value ~default:[]


let parameters_to_variables parameters =
  List.map parameters ~f:Type.Parameter.to_variable |> Option.all


let variables ?(default = None) (module Handler : Handler) = function
  | "type" ->
      (* Despite what typeshed says, typing.Type is covariant:
         https://www.python.org/dev/peps/pep-0484/#the-type-of-class-objects *)
      Some [Type.Variable.Unary (Type.Variable.Unary.create ~variance:Covariant "_T_meta")]
  | "typing.Callable" ->
      (* This is not the "real" typing.Callable. We are just proxying to the Callable instance in
         the type order here. *)
      Some [Unary (Type.Variable.Unary.create ~variance:Covariant "_T_meta")]
  | primitive_name ->
      let generic_index = index_of generic_primitive in
      let primitive_index = index_of primitive_name in
      Handler.edges primitive_index
      >>= List.find ~f:(fun { Target.target; _ } ->
              [%compare.equal: IndexTracker.t] target generic_index)
      >>| (fun { Target.parameters; _ } -> parameters_to_variables parameters)
      |> Option.value ~default


let get_generic_parameters ~generic_index edges =
  let generic_parameters { Target.target; parameters } =
    Option.some_if ([%compare.equal: IndexTracker.t] generic_index target) parameters
  in
  List.find_map ~f:generic_parameters edges


let least_common_successor order ~successors left right =
  raise_if_untracked order left;
  raise_if_untracked order right;
  if Type.Primitive.compare left right = 0 then
    [left]
  else
    (let rec iterate left right =
       let successors sources =
         Set.fold
           ~init:IndexTracker.Set.empty
           ~f:(fun sofar index -> Set.union sofar (successors index))
           sources
       in
       let left_successors = successors (List.hd_exn left) in
       let right_successors = successors (List.hd_exn right) in
       if Set.is_empty left_successors && Set.is_empty right_successors then
         []
       else
         let intersect left right =
           let collect = List.fold ~init:IndexTracker.Set.empty ~f:Set.union in
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
     iterate [IndexTracker.Set.of_list [index_of left]] [IndexTracker.Set.of_list [index_of right]])
    |> List.map ~f:IndexTracker.annotation


let least_upper_bound ((module Handler : Handler) as order) =
  let successors index =
    match Handler.edges index with
    | Some targets -> targets |> List.map ~f:Target.target |> IndexTracker.Set.of_list
    | None -> IndexTracker.Set.empty
  in
  least_common_successor order ~successors


let is_transitive_successor
    ?(placeholder_subclass_extends_all = true)
    ((module Handler : Handler) as handler)
    ~source
    ~target
  =
  raise_if_untracked handler source;
  raise_if_untracked handler target;
  let worklist = Queue.create () in
  let visited = IndexTracker.Hash_set.create () in
  Queue.enqueue worklist { Target.target = index_of source; parameters = [] };
  let rec iterate worklist =
    match Queue.dequeue worklist with
    | None -> false
    | Some { Target.target = current; _ } -> (
        match Hash_set.strict_add visited current with
        | Error _ -> iterate worklist
        | Ok () ->
            if
              [%compare.equal: IndexTracker.t] current (index_of target)
              || (placeholder_subclass_extends_all && Handler.extends_placeholder_stub current)
            then
              true
            else (
              Option.iter (Handler.edges current) ~f:(Queue.enqueue_all worklist);
              iterate worklist))
  in
  iterate worklist


let instantiate_successors_parameters ((module Handler : Handler) as handler) ~source ~target =
  raise_if_untracked handler target;
  let generic_index = IndexTracker.index generic_primitive in
  match source with
  | Type.Bottom ->
      let to_any = function
        | Type.Variable.Unary _ -> [Type.Parameter.Single Type.Any]
        | ParameterVariadic _ -> [CallableParameters Undefined]
        | TupleVariadic _ -> Type.OrderedTypes.to_parameters Type.Variable.Variadic.Tuple.any
      in
      index_of target
      |> Handler.edges
      >>= get_generic_parameters ~generic_index
      >>= parameters_to_variables
      >>| List.concat_map ~f:to_any
  | _ ->
      let split =
        match Type.split source with
        | Primitive primitive, _ when not (contains handler primitive) -> None
        | Primitive "tuple", [Type.Parameter.Single parameter] ->
            Some ("tuple", [Type.Parameter.Single (Type.weaken_literals parameter)])
        | Primitive primitive, parameters -> Some (primitive, parameters)
        | _ ->
            (* We can only propagate from those that actually split off a primitive *)
            None
      in
      let handle_split (primitive, parameters) =
        let worklist = Queue.create () in
        Queue.enqueue worklist { Target.target = index_of primitive; parameters };
        let rec iterate worklist =
          match Queue.dequeue worklist with
          | Some { Target.target = target_index; parameters } ->
              let instantiated_successors =
                (* If a node on the graph has Generic[_T1, _T2, ...] as a supertype and has concrete
                   parameters, all occurrences of _T1, _T2, etc. in other supertypes need to be
                   replaced with the concrete parameter corresponding to the type variable. This
                   function takes a target with concrete parameters and its supertypes, and
                   instantiates the supertypes accordingly. *)
                let get_instantiated_successors ~generic_index ~parameters successors =
                  let variables =
                    get_generic_parameters successors ~generic_index
                    >>= parameters_to_variables
                    |> Option.value ~default:[]
                  in
                  let replacement =
                    let to_any = function
                      | Type.Variable.Unary variable -> Type.Variable.UnaryPair (variable, Type.Any)
                      | ParameterVariadic variable ->
                          Type.Variable.ParameterVariadicPair (variable, Undefined)
                      | TupleVariadic variadic ->
                          Type.Variable.TupleVariadicPair
                            (variadic, Type.Variable.Variadic.Tuple.any)
                    in
                    Type.Variable.zip_variables_with_parameters ~parameters variables
                    |> Option.value ~default:(List.map ~f:to_any variables)
                    |> TypeConstraints.Solution.create
                  in
                  let instantiate_parameters { Target.target; parameters } =
                    let instantiate = function
                      | Type.Parameter.Single single ->
                          [
                            Type.Parameter.Single
                              (TypeConstraints.Solution.instantiate replacement single);
                          ]
                      | CallableParameters parameters ->
                          [
                            CallableParameters
                              (TypeConstraints.Solution.instantiate_callable_parameters
                                 replacement
                                 parameters);
                          ]
                      | Unpacked unpackable ->
                          Type.OrderedTypes.to_parameters
                            (TypeConstraints.Solution.instantiate_ordered_types
                               replacement
                               (Concatenation
                                  (Type.OrderedTypes.Concatenation.create_from_unpackable
                                     unpackable)))
                    in
                    { Target.target; parameters = List.concat_map parameters ~f:instantiate }
                  in
                  List.map successors ~f:instantiate_parameters
                in
                Handler.edges target_index
                >>| get_instantiated_successors ~generic_index ~parameters
              in
              if [%compare.equal: IndexTracker.t] target_index (index_of target) then
                match target with
                | "typing.Callable" -> Some parameters
                | _ -> instantiated_successors >>= get_generic_parameters ~generic_index
              else (
                instantiated_successors >>| List.iter ~f:(Queue.enqueue worklist) |> ignore;
                iterate worklist)
          | None -> None
        in
        iterate worklist
      in
      split >>= handle_split


let check_integrity (module Handler : Handler) ~(indices : IndexTracker.t list) =
  (* Ensure keys are consistent. *)
  let key_consistent key =
    let raise_if_none value =
      if Option.is_none value then (
        Log.error "Inconsistency in type order: No value for key %s" (IndexTracker.show key);
        raise Incomplete)
    in
    raise_if_none (Handler.edges key)
  in
  List.iter ~f:key_consistent indices;

  (* Check for cycles. *)
  let started_from = ref IndexTracker.Set.empty in
  let find_cycle start =
    if not (Set.mem !started_from start) then
      let rec visit reverse_visited index =
        if List.mem ~equal:[%compare.equal: IndexTracker.t] reverse_visited index then (
          let trace = List.rev_map ~f:IndexTracker.annotation (index :: reverse_visited) in
          Log.error "Order is cyclic:\nTrace: %s" (String.concat ~sep:" -> " trace);
          raise (Cyclic (String.Hash_set.of_list trace)))
        else if not (Set.mem !started_from index) then (
          started_from := Set.add !started_from index;
          match Handler.edges index with
          | Some successors ->
              successors
              |> List.map ~f:Target.target
              |> List.iter ~f:(visit (index :: reverse_visited))
          | None -> ())
      in
      visit [] start
  in
  indices |> List.iter ~f:find_cycle


let to_dot (module Handler : Handler) ~indices =
  let indices = List.sort ~compare:IndexTracker.compare indices in
  let nodes = List.map indices ~f:(fun index -> index, IndexTracker.annotation index) in
  let buffer = Buffer.create 10000 in
  Buffer.add_string buffer "digraph {\n";
  List.iter
    ~f:(fun (index, annotation) ->
      Format.asprintf "  %s[label=\"%s\"]\n" (IndexTracker.show index) annotation
      |> Buffer.add_string buffer)
    nodes;
  let add_edges index =
    Handler.edges index
    >>| List.sort ~compare:Target.compare
    >>| List.iter ~f:(fun { Target.target = successor; parameters } ->
            Format.asprintf "  %s -> %s" (IndexTracker.show index) (IndexTracker.show successor)
            |> Buffer.add_string buffer;
            if not (List.is_empty parameters) then
              Format.asprintf
                "[label=\"(%a)\"]"
                (Type.pp_parameters ~pp_type:Type.pp_concise)
                parameters
              |> Buffer.add_string buffer;
            Buffer.add_string buffer "\n")
    |> ignore
  in
  List.iter ~f:add_edges indices;
  Buffer.add_string buffer "}";
  Buffer.contents buffer


let is_typed_dictionary_subclass ~class_hierarchy name =
  let (module TypeOrderHandler : Handler) = class_hierarchy in
  TypeOrderHandler.contains name
  && TypeOrderHandler.contains (Type.TypedDictionary.class_name ~total:true)
  && is_transitive_successor
       ~placeholder_subclass_extends_all:false
       class_hierarchy
       ~source:name
       ~target:(Type.TypedDictionary.class_name ~total:true)
  && not (String.equal name (Type.TypedDictionary.class_name ~total:true))


let is_total_typed_dictionary ~class_hierarchy name =
  not
    (is_transitive_successor
       ~placeholder_subclass_extends_all:false
       class_hierarchy
       ~source:name
       ~target:(Type.TypedDictionary.class_name ~total:false))
