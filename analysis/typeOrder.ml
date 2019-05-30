(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Pyre
module Callable = Type.Callable

exception Cyclic

exception Incomplete

exception Untracked of Type.t

exception InconsistentMethodResolutionOrder of Type.primitive

module Target = struct
  type t = {
    target: int;
    parameters: Type.t list
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

  let enqueue worklist actual_parameters targets =
    let enqueue { target; parameters } =
      let parameters =
        (* We currently ignore the actual type variable mapping. *)
        if List.length parameters = List.length actual_parameters then
          actual_parameters
        else
          []
      in
      Queue.enqueue worklist { target; parameters }
    in
    List.iter targets ~f:enqueue


  let target_equal = equal

  module List = struct
    type record = t list

    include List

    let mem = List.mem ~equal:target_equal

    let equal = List.equal ~equal:target_equal

    let add list element = element :: list

    let empty = []
  end
end

(* `edges` mapping from type index to a set of targets. `indices` mapping from annotation to its
   vertex index. `annotations` inverse of `indices`. *)
type t = {
  edges: Target.t list Int.Table.t;
  backedges: Target.Set.t Int.Table.t;
  indices: int Type.Table.t;
  annotations: Type.t Int.Table.t
}

module type Handler = sig
  type ('key, 'table) lookup

  val edges : unit -> (int, Target.t list) lookup

  val backedges : unit -> (int, Target.Set.t) lookup

  val indices : unit -> (Type.t, int) lookup

  val annotations : unit -> (int, Type.t) lookup

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
    let annotation index = Hashtbl.find_exn annotations index |> Format.asprintf "%a" Type.pp in
    let targets =
      let target { Target.target; parameters } =
        Format.sprintf
          "%s [%s]"
          (annotation target)
          (List.map ~f:(Format.asprintf "%a" Type.pp) parameters |> String.concat ~sep:", ")
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
      let initial = Type.hash annotation in
      let rec pick_index index =
        if Handler.contains annotations index then
          pick_index (initial + 1)
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


let connect ?(parameters = [])
            ((module Handler : Handler) as order)
            ~predecessor
            ~successor =
  if
    (not (Handler.contains (Handler.indices ()) predecessor))
    || not (Handler.contains (Handler.indices ()) successor)
  then
    Statistics.event
      ~name:"invalid type order connection"
      ~integers:[]
      ~normals:["Predecessor", Type.show predecessor; "Successor", Type.show successor]
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
        not (Handler.contains (Handler.indices ()) (Type.Primitive name))
    | _ -> false
  in
  not (Type.exists ~predicate:is_invalid annotation)


let raise_if_untracked order annotation =
  if not (contains order annotation) then
    raise (Untracked annotation)


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
        index |> Handler.find_unsafe (Handler.annotations ()) |> Type.primitive_name
      in
      index_of order (Type.Primitive class_name)
      |> get_successors
      |> Option.value ~default:[]
      |> List.filter_map ~f:create_annotation
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


let variables (module Handler : Handler) annotation =
  match Type.split annotation with
  | left, _ when String.equal (Type.show left) "type" ->
      (* Despite what typeshed says, typing.Type is covariant:
         https://www.python.org/dev/peps/pep-0484/#the-type-of-class-objects *)
      Some [Type.Variable (Type.Variable.Unary.create ~variance:Covariant "_T_meta")]
  | left, _ when String.equal (Type.show left) "typing.Callable" ->
      (* This is not the "real" typing.Callable. We are just proxying to the Callable instance in
         the type order here. *)
      Some [Type.Variable (Type.Variable.Unary.create ~variance:Covariant "_T_meta")]
  | primitive, _ ->
      Handler.find (Handler.indices ()) Type.generic_primitive
      >>= fun generic_index ->
      Handler.find (Handler.indices ()) primitive
      >>= fun primitive_index ->
      Handler.find (Handler.edges ()) primitive_index
      >>= List.find ~f:(fun { Target.target; _ } -> target = generic_index)
      >>| fun { Target.parameters; _ } -> parameters


let get_generic_parameters ~generic_index edges =
  let generic_parameters { Target.target; parameters } =
    match generic_index with
    | Some index when index = target -> Some parameters
    | _ -> None
  in
  List.find_map ~f:generic_parameters edges


module ProtocolAssumptions : sig
  type t

  val find_assumed_protocol_parameters
    :  candidate:Type.t ->
    protocol:Identifier.t ->
    t ->
    Type.t list option

  val add : candidate:Type.t -> protocol:Identifier.t -> protocol_parameters:Type.t list -> t -> t

  val empty : t
end = struct
  type protocol_parameters = Type.t list

  type assumption = {
    candidate: Type.t;
    protocol: Identifier.t
  }
  [@@deriving eq]

  type t = (assumption, protocol_parameters) List.Assoc.t

  let find_assumed_protocol_parameters ~candidate ~protocol assumptions =
    List.Assoc.find assumptions { candidate; protocol } ~equal:equal_assumption


  let add ~candidate ~protocol ~protocol_parameters existing_assumptions =
    List.Assoc.add
      existing_assumptions
      { candidate; protocol }
      protocol_parameters
      ~equal:equal_assumption


  let empty = []
end

type order = {
  handler: (module Handler);
  constructor: Type.t -> Type.t option;
  attributes: Type.t -> AnnotatedAttribute.t list option;
  is_protocol: Type.t -> bool;
  any_is_bottom: bool;
  protocol_assumptions: ProtocolAssumptions.t
}

module type FullOrderTypeWithoutT = sig
  val solve_less_or_equal
    :  order ->
    constraints:TypeConstraints.t ->
    left:Type.t ->
    right:Type.t ->
    TypeConstraints.t list

  val less_or_equal : order -> left:Type.t -> right:Type.t -> bool

  val least_upper_bound : (module Handler) -> Type.t -> Type.t -> Type.t list

  val greatest_lower_bound : (module Handler) -> Type.t -> Type.t -> Type.t list

  val meet : order -> Type.t -> Type.t -> Type.t

  val join : order -> Type.t -> Type.t -> Type.t

  val diff_variables : Type.t Type.Map.t -> Type.t -> Type.t -> Type.t Type.Map.t

  val instantiate_successors_parameters
    :  order ->
    source:Type.t ->
    target:Type.t ->
    Type.t List.t Option.t

  val instantiate_protocol_parameters
    :  order ->
    candidate:Type.t ->
    protocol:Ast.Identifier.t ->
    Type.t list option
end

module type FullOrderType = sig
  type t = order

  include FullOrderTypeWithoutT
end

module type OrderedConstraintsType = TypeConstraints.OrderedConstraintsType with type order = order

module OrderImplementation = struct
  module Make (OrderedConstraints : OrderedConstraintsType) = struct
    type t = order

    (* TODO(T40105833): merge this with actual signature select *)
    let rec simulate_signature_select
        order ~callable:{ Type.Callable.implementation; overloads; _ } ~called_as ~constraints
      =
      let open Callable in
      let solve implementation ~initial_constraints =
        try
          let rec solve_parameters left right constraints =
            match left, right with
            | Parameter.Anonymous _ :: _, Parameter.Named _ :: _ -> []
            | ( Parameter.Anonymous { annotation = left_annotation; _ } :: left_parameters,
                Parameter.Anonymous { annotation = right_annotation; _ } :: right_parameters )
            | ( Parameter.Named { annotation = left_annotation; _ } :: left_parameters,
                Parameter.Anonymous { annotation = right_annotation; _ } :: right_parameters ) ->
                solve_less_or_equal_safe
                  order
                  ~constraints
                  ~left:right_annotation
                  ~right:left_annotation
                |> List.concat_map ~f:(solve_parameters left_parameters right_parameters)
            | ( Parameter.Named ({ Parameter.annotation = left_annotation; _ } as left)
                :: left_parameters,
                Parameter.Named ({ Parameter.annotation = right_annotation; _ } as right)
                :: right_parameters )
            | ( Parameter.Keywords ({ Parameter.annotation = left_annotation; _ } as left)
                :: left_parameters,
                Parameter.Keywords ({ Parameter.annotation = right_annotation; _ } as right)
                :: right_parameters )
            | ( Parameter.Variable ({ Parameter.annotation = left_annotation; _ } as left)
                :: left_parameters,
                Parameter.Variable ({ Parameter.annotation = right_annotation; _ } as right)
                :: right_parameters ) ->
                if Parameter.names_compatible (Parameter.Named left) (Parameter.Named right) then
                  solve_less_or_equal_safe
                    order
                    ~constraints
                    ~left:right_annotation
                    ~right:left_annotation
                  |> List.concat_map ~f:(solve_parameters left_parameters right_parameters)
                else
                  []
            | ( Parameter.Variable { Parameter.annotation = left_annotation; _ } :: _,
                Parameter.Anonymous { annotation = right_annotation; _ } :: right_parameters ) ->
                solve_less_or_equal_safe
                  order
                  ~constraints
                  ~left:right_annotation
                  ~right:left_annotation
                |> List.concat_map ~f:(solve_parameters left right_parameters)
            | Parameter.Variable _ :: left_parameters, []
            | Parameter.Keywords _ :: left_parameters, [] ->
                solve_parameters left_parameters [] constraints
            | ( (Parameter.Variable _ as variable) :: (Parameter.Keywords _ as keywords) :: _,
                (Parameter.Named _ as named) :: right ) ->
                (* SOLVE *)
                let is_compatible =
                  Type.equal (Parameter.annotation variable) (Parameter.annotation keywords)
                  && less_or_equal
                       order
                       ~left:(Parameter.annotation named)
                       ~right:(Parameter.annotation keywords)
                in
                if is_compatible then
                  solve_parameters left right constraints
                else
                  []
            | left :: left_parameters, [] ->
                if Parameter.default left then
                  solve_parameters left_parameters [] constraints
                else
                  []
            | [], [] -> [constraints]
            | _ -> []
          in
          match implementation.parameters, called_as.parameters with
          | Undefined, Undefined -> [initial_constraints]
          | Defined implementation, Defined called_as ->
              solve_parameters implementation called_as initial_constraints
          (* We exhibit unsound behavior when a concrete callable is passed into one expecting a
             Callable[..., T] - Callable[..., T] admits any parameters, which is not true when a
             callable that doesn't admit kwargs and varargs is passed in. We need this since there
             is no good way of representing "leave the parameters alone and change the return type"
             in the Python type system at the moment. *)
          | Defined _, Undefined -> [initial_constraints]
          | Undefined, Defined _ -> [initial_constraints]
          | bound, ParameterVariadicTypeVariable variable
            when Type.Variable.Variadic.Parameters.is_free variable ->
              let pair = Type.Variable.ParameterVariadicPair (variable, bound) in
              OrderedConstraints.add_upper_bound initial_constraints ~order ~pair |> Option.to_list
          | _, _ -> []
        with
        | _ -> []
      in
      let overload_to_instantiated_return_and_altered_constraints overload =
        let namespace = Type.Variable.Namespace.create_fresh () in
        let namespaced_variables =
          Type.Callable
            { Type.Callable.kind = Anonymous;
              implementation = overload;
              overloads = [];
              implicit = None
            }
          |> Type.Variable.all_free_variables
          |> List.map ~f:(Type.Variable.namespace ~namespace)
        in
        let overload =
          map_implementation overload ~f:(Type.Variable.namespace_all_free_variables ~namespace)
        in
        let does_not_leak_namespaced_variables (external_constraints, _) =
          not
            (TypeConstraints.exists_in_bounds external_constraints ~variables:namespaced_variables)
        in
        let instantiate_return (external_constraints, partial_solution) =
          let instantiated_return =
            TypeConstraints.Solution.instantiate partial_solution overload.annotation
            |> Type.Variable.mark_all_free_variables_as_escaped ~specific:namespaced_variables
          in
          instantiated_return, external_constraints
        in
        solve overload ~initial_constraints:constraints
        |> List.map
             ~f:
               (OrderedConstraints.extract_partial_solution ~order ~variables:namespaced_variables)
        |> List.concat_map ~f:Option.to_list
        |> List.filter ~f:does_not_leak_namespaced_variables
        |> List.map ~f:instantiate_return
      in
      let overloads =
        if List.is_empty overloads then
          [implementation]
        else if Type.Callable.Overload.is_undefined implementation then
          overloads
        else
          (* TODO(T41195241) always ignore implementation when has overloads. Currently put
             implementation as last resort *)
          overloads @ [implementation]
      in
      List.concat_map overloads ~f:overload_to_instantiated_return_and_altered_constraints


    (* ## solve_less_or_equal, a brief explanation: ##

       At the heart of our handling of generics is this function, solve_less_or_equal.

       This function takes:

       * a statement of the form F(T1, T2, ... Tn) =<= G(T1, T2, ... Tn), where F and G are types
       which may contain any number of free type variables. In this context a free type variable is
       one whose value we are trying to determine. This could be a function level generic, an
       "escaped" type variable from some sort of incomplete initialization, or even some sort of
       synthetic type variable we're using to answer a question like, "if this is an iterable, what
       kind of iterable is it?" for correctly handling *args parameters.

       * a precondition set of constraints (as defined in TypeConstraints.mli) from a previous call
       to solve_less_or_equal (or from somewhere else). This is how you're able to define
       conjunctions of =<= statements, as when you are trying to satisfy a number of argument <->
       parameter pairs in signature selection

       and returns:

       * an arbitrarily ordered list of constraints (again as defined in Typeconstraints.mli) that
       each are sufficient to satisfy the given statement and the precondition constraints. If this
       list is empty, there is no way to satify those requirements (at least as well as we can know
       that).

       The general strategy undertaken to achieve this behavior is to pairwise recursively break up
       the types in the same way, e.g. we expect to get a tuple on the right if we have a tuple on
       the left, and to handle that by enforcing that each of the contained types be less than
       their pair on the other side (as tuples are covariant). Certain pairs, such as X =<=
       Union[...] or comparing callables with overloads naturally create multiple disjoint
       possibilities which give rise to the list of constraints that we end up returning.

       Once you have enforced all of the statements you would like to ensure are true, you can
       extract possible solutions to the constraints set you have built up with List.filter_map
       ~f:OrderedConstraints.solve *)
    and solve_less_or_equal_safe
        ({ handler; constructor; any_is_bottom; is_protocol; _ } as order)
        ~constraints
        ~left
        ~right
      =
      let rec solve_less_or_equal_throws order ~constraints ~left ~right =
        if
          Type.Variable.all_variables_are_resolved left
          && Type.Variable.all_variables_are_resolved right
        then
          if less_or_equal order ~left ~right then [constraints] else []
        else
          let solve_all constraints ~lefts ~rights =
            let folded_constraints =
              let solve_pair constraints left right =
                constraints
                |> List.concat_map ~f:(fun constraints ->
                       solve_less_or_equal_throws order ~constraints ~left ~right)
              in
              List.fold2 ~init:[constraints] ~f:solve_pair lefts rights
            in
            match folded_constraints with
            | List.Or_unequal_lengths.Ok constraints -> constraints
            | List.Or_unequal_lengths.Unequal_lengths -> []
          in
          match left, right with
          | _, _ when Type.equal left right -> [constraints]
          | _, Type.Primitive "object"
          | _, Type.Any
          | _, Type.Top ->
              [constraints]
          | Type.Any, _ when any_is_bottom -> [constraints]
          | Type.Union lefts, right ->
              solve_all constraints ~lefts ~rights:(List.map lefts ~f:(fun _ -> right))
          | Type.Variable left_variable, Type.Variable right_variable
            when Type.Variable.Unary.is_free left_variable
                 && Type.Variable.Unary.is_free right_variable ->
              (* Either works because constraining V1 to be less or equal to V2 implies that V2 is
                 greater than or equal to V1. Therefore either constraint is sufficient, and we
                 should consider both. This approach simplifies things downstream for the
                 constraint solver *)
              let right_greater_than_left, left_less_than_right =
                ( OrderedConstraints.add_lower_bound
                    constraints
                    ~order
                    ~pair:(Type.Variable.UnaryPair (right_variable, left))
                  |> Option.to_list,
                  OrderedConstraints.add_upper_bound
                    constraints
                    ~order
                    ~pair:(Type.Variable.UnaryPair (left_variable, right))
                  |> Option.to_list )
              in
              right_greater_than_left @ left_less_than_right
          | Type.Variable variable, bound when Type.Variable.Unary.is_free variable ->
              let pair = Type.Variable.UnaryPair (variable, bound) in
              OrderedConstraints.add_upper_bound constraints ~order ~pair |> Option.to_list
          | bound, Type.Variable variable when Type.Variable.Unary.is_free variable ->
              let pair = Type.Variable.UnaryPair (variable, bound) in
              OrderedConstraints.add_lower_bound constraints ~order ~pair |> Option.to_list
          | Type.Callable _, Type.Primitive protocol when is_protocol right ->
              if instantiate_protocol_parameters order ~protocol ~candidate:left = Some [] then
                [constraints]
              else
                []
          | Type.Callable _, Type.Parametric { name; _ } when is_protocol right ->
              instantiate_protocol_parameters order ~protocol:name ~candidate:left
              >>| Type.parametric name
              >>| (fun left -> solve_less_or_equal_throws order ~constraints ~left ~right)
              |> Option.value ~default:[]
          | _, Type.Parametric { name = right_name; parameters = right_parameters } ->
              let solve_parameters left_parameters =
                let solve_parameter_pair constraints (variable, (left, right)) =
                  match variable with
                  | Type.Variable { variance = Covariant; _ } ->
                      constraints
                      |> List.concat_map ~f:(fun constraints ->
                             solve_less_or_equal_throws order ~constraints ~left ~right)
                  | Type.Variable { variance = Contravariant; _ } ->
                      constraints
                      |> List.concat_map ~f:(fun constraints ->
                             solve_less_or_equal_throws order ~constraints ~left:right ~right:left)
                  | Type.Variable { variance = Invariant; _ } ->
                      constraints
                      |> List.concat_map ~f:(fun constraints ->
                             solve_less_or_equal_throws order ~constraints ~left ~right)
                      |> List.concat_map ~f:(fun constraints ->
                             solve_less_or_equal_throws order ~constraints ~left:right ~right:left)
                  | _ -> []
                in
                let zip_on_parameters variables =
                  List.zip left_parameters right_parameters >>= List.zip variables
                in
                variables handler right
                >>= zip_on_parameters
                >>| List.fold ~f:solve_parameter_pair ~init:[constraints]
              in
              let parameters =
                let parameters =
                  instantiate_successors_parameters
                    order
                    ~source:left
                    ~target:(Type.Primitive right_name)
                in
                match parameters with
                | None when is_protocol right ->
                    instantiate_protocol_parameters order ~protocol:right_name ~candidate:left
                | _ -> parameters
              in
              parameters >>= solve_parameters |> Option.value ~default:[]
          | Type.Parametric _, Type.Primitive _ ->
              let left = Type.Variable.mark_all_variables_as_bound left in
              if less_or_equal order ~left ~right then
                [constraints]
              else
                []
          | Optional left, Optional right
          | left, Optional right
          | Type.Tuple (Type.Unbounded left), Type.Tuple (Type.Unbounded right) ->
              solve_less_or_equal_throws order ~constraints ~left ~right
          | Type.Tuple (Type.Bounded lefts), Type.Tuple (Type.Bounded rights) ->
              solve_all constraints ~lefts ~rights
          | Type.Tuple (Type.Unbounded left), Type.Tuple (Type.Bounded rights) ->
              let lefts = List.init (List.length rights) ~f:(fun _ -> left) in
              solve_all constraints ~lefts ~rights
          | Type.Tuple (Type.Bounded lefts), Type.Tuple (Type.Unbounded right) ->
              let left = Type.union lefts in
              solve_less_or_equal_throws order ~constraints ~left ~right
          | _, Type.Union rights ->
              List.concat_map rights ~f:(fun right ->
                  solve_less_or_equal_throws order ~constraints ~left ~right)
          | Type.Callable callable, Type.Callable { implementation; overloads; _ } ->
              let fold_overload sofar called_as =
                let call_as_overload constraints =
                  simulate_signature_select order ~callable ~called_as ~constraints
                  |> List.concat_map ~f:(fun (left, constraints) ->
                         solve_less_or_equal_throws
                           order
                           ~constraints
                           ~left
                           ~right:called_as.annotation)
                in
                List.concat_map sofar ~f:call_as_overload
              in
              List.fold (implementation :: overloads) ~f:fold_overload ~init:[constraints]
          | _, Type.Callable _ when Type.is_meta left ->
              Type.single_parameter left
              |> constructor
              >>| (fun left -> solve_less_or_equal_throws order ~constraints ~left ~right)
              |> Option.value ~default:[]
          | _ -> []
      in
      (* TODO(T39612118): unwrap this when attributes are safe *)
      try solve_less_or_equal_throws order ~constraints ~left ~right with
      | Untracked _ -> []


    and less_or_equal
        ( { handler = (module Handler : Handler) as handler;
            constructor;
            any_is_bottom;
            is_protocol
          ; _
          } as order )
        ~left
        ~right
      =
      let nominally_less_or_equal ~left ~right =
        Type.equal left right
        ||
        match left, right with
        | _, Type.Any -> true
        | other, Type.Top ->
            not
              (Type.exists other ~predicate:(fun annotation ->
                   Type.equal annotation Type.undeclared))
        | Type.Top, _ -> false
        | Type.Any, _ -> any_is_bottom
        | Type.Bottom, _ -> true
        | _, Type.Bottom -> false
        | _, Type.Primitive "object" -> true
        | _, Type.Variable _ -> false
        | Type.Parametric { name = left_name; _ }, Type.Parametric _ ->
            let compare_parameter left right variable =
              match left, right, variable with
              | Type.Bottom, _, _ ->
                  (* T[Bottom] is a subtype of T[_T2], for any _T2 and regardless of its variance. *)
                  true
              | _, Type.Top, _ ->
                  (* T[_T2] is a subtype of T[Top], for any _T2 and regardless of its variance. *)
                  true
              | Type.Top, _, _ -> false
              | _, _, Type.Variable { variance = Covariant; _ } -> less_or_equal order ~left ~right
              | _, _, Type.Variable { variance = Contravariant; _ } ->
                  less_or_equal order ~left:right ~right:left
              | _, _, Type.Variable { variance = Invariant; _ } ->
                  less_or_equal order ~left ~right && less_or_equal order ~left:right ~right:left
              | _ ->
                  Log.warning
                    "Cannot compare %a and %a, not a variable: %a"
                    Type.pp
                    left
                    Type.pp
                    right
                    Type.pp
                    variable;
                  false
            in
            let left_primitive, left_parameters = Type.split left in
            let right_primitive, right_parameters = Type.split right in
            raise_if_untracked handler left_primitive;
            raise_if_untracked handler right_primitive;
            let generic_index = Handler.find (Handler.indices ()) Type.generic_primitive in
            let left_variables = variables handler left in
            if Type.equal left_primitive right_primitive then
              (* Left and right primitives coincide, a simple parameter comparison is enough. *)
              let compare_parameters ~left ~right variables =
                List.length variables = List.length left
                && List.length variables = List.length right
                && List.map3_exn ~f:compare_parameter left right variables |> List.for_all ~f:Fn.id
              in
              left_variables
              >>| compare_parameters ~left:left_parameters ~right:right_parameters
              |> Option.value ~default:false
            else (* Perform one step in all appropriate directions. *)
              let parametric ~primitive ~parameters =
                match primitive with
                | Type.Primitive name -> Some (Type.Parametric { name; parameters })
                | _ -> None
              in
              (* 1. Go one level down in the class hierarchy and try from there. *)
              let step_into_subclasses () =
                let target_to_parametric { Target.target; parameters } =
                  let parameters =
                    Handler.find (Handler.edges ()) target
                    >>| get_instantiated_successors ~generic_index ~parameters
                    >>= get_generic_parameters ~generic_index
                    |> Option.value ~default:[]
                  in
                  Handler.find (Handler.annotations ()) target
                  >>| Type.split
                  >>| fst
                  >>= fun primitive -> parametric ~primitive ~parameters
                in
                let successors =
                  let left_index = index_of handler left_primitive in
                  Handler.find (Handler.edges ()) left_index |> Option.value ~default:[]
                in
                get_instantiated_successors ~generic_index ~parameters:left_parameters successors
                |> List.filter_map ~f:target_to_parametric
                |> List.exists ~f:(fun left -> less_or_equal order ~left ~right)
              in
              (* 2. Try and replace all parameters, one at a time, to get closer to the
                 destination. *)
              let replace_parameters_with_destination left_variables =
                (* Mapping from a variable in `left` to the target parameter (via subclass
                   substitutions) in `right`. *)
                let variable_substitutions =
                  let right_propagated =
                    (* Create a "fake" primitive+variables type that we can propagate to the
                       target. *)
                    parametric ~primitive:left_primitive ~parameters:left_variables
                    >>= (fun source ->
                          instantiate_successors_parameters order ~source ~target:right_primitive)
                    |> Option.value ~default:[]
                  in
                  match
                    List.fold2
                      right_propagated
                      right_parameters
                      ~init:Type.Map.empty
                      ~f:diff_variables
                  with
                  | Ok result -> result
                  | Unequal_lengths -> Type.Map.empty
                in
                let propagate_with_substitutions index variable =
                  let replace_one_parameter replacement =
                    let head, tail = List.split_n left_parameters index in
                    match List.hd tail with
                    | Some original
                      when (* If the original and replacement do not differ, no recursion is
                              needed. *)
                           Type.equal original replacement
                           or (* Cannot perform the replacement if variance does not allow it. *)
                              not (compare_parameter original replacement variable) ->
                        None
                    | _ ->
                        let tail = List.tl tail |> Option.value ~default:[] in
                        let parameters = head @ [replacement] @ tail in
                        Some (Type.Parametric { name = left_name; parameters })
                  in
                  Map.find variable_substitutions variable
                  >>= replace_one_parameter
                  >>| (fun left -> less_or_equal order ~left ~right)
                  |> Option.value ~default:false
                in
                List.existsi left_variables ~f:propagate_with_substitutions
              in
              let step_sideways () =
                left_variables
                >>| (fun left_variables ->
                      List.length left_variables == List.length left_parameters
                      && replace_parameters_with_destination left_variables)
                |> Option.value ~default:false
              in
              step_into_subclasses () or step_sideways ()
        (* \forall i \in Union[...]. A_i <= B -> Union[...] <= B. *)
        | Type.Union left, right ->
            List.fold
              ~init:true
              ~f:(fun current left -> current && less_or_equal order ~left ~right)
              left
        (* We have to consider both the variables' constraint and its full value against the union. *)
        | Type.Variable variable, Type.Union union ->
            List.exists ~f:(fun right -> less_or_equal order ~left ~right) union
            || less_or_equal order ~left:(Type.Variable.Unary.upper_bound variable) ~right
        (* \exists i \in Union[...]. A <= B_i -> A <= Union[...] *)
        | left, Type.Union right ->
            List.exists ~f:(fun right -> less_or_equal order ~left ~right) right
        (* We have to consider both the variables' constraint and its full value against the
           optional. *)
        | Type.Variable variable, Type.Optional optional ->
            less_or_equal order ~left ~right:optional
            || less_or_equal order ~left:(Type.Variable.Unary.upper_bound variable) ~right
        (* A <= B -> A <= Optional[B].*)
        | Type.Optional left, Type.Optional right -> less_or_equal order ~left ~right
        | _, Type.Optional parameter -> less_or_equal order ~left ~right:parameter
        | Type.Optional _, _ -> false
        | Type.Variable variable, right ->
            less_or_equal order ~left:(Type.Variable.Unary.upper_bound variable) ~right
        (* Tuple variables are covariant. *)
        | Type.Tuple (Type.Bounded left), Type.Tuple (Type.Bounded right)
          when List.length left = List.length right ->
            List.for_all2_exn left right ~f:(fun left right -> less_or_equal order ~left ~right)
        | Type.Tuple (Type.Unbounded left), Type.Tuple (Type.Unbounded right) ->
            less_or_equal order ~left ~right
        | Type.Tuple (Type.Bounded []), Type.Tuple (Type.Unbounded _) -> true
        | Type.Tuple (Type.Bounded (left :: tail)), Type.Tuple (Type.Unbounded right) ->
            let left = List.fold tail ~init:left ~f:(join order) in
            less_or_equal order ~left ~right
        | Type.Tuple _, Type.Parametric _ ->
            (* Join parameters to handle cases like `Tuple[int, int]` <= `Iterator[int]`. *)
            let parametric =
              let primitive, parameters = Type.split left in
              let parameter = List.fold ~init:Type.Bottom ~f:(join order) parameters in
              let name =
                match primitive with
                | Type.Primitive name -> name
                | _ -> "?"
              in
              Type.Parametric { name; parameters = [parameter] }
            in
            less_or_equal order ~left:parametric ~right
        | Type.Tuple (Type.Unbounded parameter), Type.Primitive _ ->
            less_or_equal order ~left:(Type.parametric "tuple" [parameter]) ~right
        | Type.Tuple (Type.Bounded (left :: tail)), Type.Primitive _ ->
            let parameter = List.fold ~f:(join order) ~init:left tail in
            less_or_equal order ~left:(Type.parametric "tuple" [parameter]) ~right
        | Type.Primitive name, Type.Tuple _ -> String.equal name "tuple"
        | Type.Tuple _, _
        | _, Type.Tuple _ ->
            false
        | ( Type.Callable { Callable.kind = Callable.Named left; _ },
            Type.Callable { Callable.kind = Callable.Named right; _ } )
          when Reference.equal left right ->
            true
        | Type.Callable callable, Type.Callable { Callable.implementation; overloads; _ } ->
            let validate_overload called_as =
              simulate_signature_select
                order
                ~callable
                ~called_as
                ~constraints:TypeConstraints.empty
              |> List.exists ~f:(fun (left, _) ->
                     less_or_equal order ~left ~right:called_as.annotation)
            in
            List.for_all (implementation :: overloads) ~f:validate_overload
        | _, Type.Callable _ when Type.is_meta left ->
            Type.single_parameter left
            |> constructor
            >>| (fun left -> less_or_equal order ~left ~right)
            |> Option.value ~default:false
        (* A[...] <= B iff A <= B. *)
        | Type.Parametric _, Type.Primitive _ ->
            let parametric_primitive, _ = Type.split left in
            less_or_equal order ~left:parametric_primitive ~right
        | Type.Primitive name, Type.Parametric _ ->
            let left = Type.Parametric { name; parameters = [] } in
            less_or_equal order ~left ~right
        | left, Type.Callable _ -> (
            let joined = join order (Type.parametric "typing.Callable" [Type.Bottom]) left in
            match joined with
            | Type.Parametric { name; parameters = [left] }
              when Identifier.equal name "typing.Callable" ->
                less_or_equal order ~left ~right
            | _ -> false )
        | Type.Callable _, _ -> false
        | Type.TypedDictionary left, Type.TypedDictionary right ->
            let field_not_found field =
              not (List.exists left.fields ~f:(Type.equal_typed_dictionary_field field))
            in
            left.total = right.total && not (List.exists right.fields ~f:field_not_found)
        | Type.TypedDictionary _, _ ->
            less_or_equal order ~left:(Type.Primitive "TypedDictionary") ~right
        | _, Type.TypedDictionary _ ->
            less_or_equal order ~left ~right:(Type.Primitive "TypedDictionary")
        | _, Type.Literal _ -> false
        | Type.Literal _, _ -> less_or_equal order ~left:(Type.weaken_literals left) ~right
        | _ ->
            raise_if_untracked handler left;
            raise_if_untracked handler right;
            let worklist = Queue.create () in
            Queue.enqueue worklist { Target.target = index_of handler left; parameters = [] };
            let rec iterate worklist =
              match Queue.dequeue worklist with
              | Some { Target.target; _ } ->
                  if target = index_of handler right then
                    true
                  else (
                    Option.iter
                      (Handler.find (Handler.edges ()) target)
                      ~f:(Target.enqueue worklist []);
                    iterate worklist )
              | None -> false
            in
            iterate worklist
      in
      let is_nominally_less_or_equal = nominally_less_or_equal ~left ~right in
      match right with
      | Type.Primitive name when (not is_nominally_less_or_equal) && is_protocol right ->
          instantiate_protocol_parameters order ~candidate:left ~protocol:name = Some []
      | Type.Parametric { name; _ } when (not is_nominally_less_or_equal) && is_protocol right ->
          instantiate_protocol_parameters order ~candidate:left ~protocol:name
          >>| Type.parametric name
          >>| (fun left -> nominally_less_or_equal ~left ~right)
          |> Option.value ~default:false
      | _ -> is_nominally_less_or_equal


    and least_common_successor ((module Handler : Handler) as order) ~successors left right =
      raise_if_untracked order left;
      raise_if_untracked order right;
      if Type.compare left right = 0 then
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


    and least_upper_bound ((module Handler : Handler) as order) =
      let successors index =
        match Handler.find (Handler.edges ()) index with
        | Some targets -> targets |> List.map ~f:Target.target |> Int.Set.of_list
        | None -> Int.Set.empty
      in
      least_common_successor order ~successors


    and greatest_lower_bound ((module Handler : Handler) as order) =
      let predecessors index =
        match Handler.find (Handler.backedges ()) index with
        | Some targets -> Set.to_list targets |> List.map ~f:Target.target |> Int.Set.of_list
        | None -> Int.Set.empty
      in
      least_common_successor order ~successors:predecessors


    and join_implementations ~parameter_join ~return_join order left right =
      let open Callable in
      let parameters =
        match left.parameters, right.parameters with
        | Undefined, Undefined -> Some Undefined
        | Defined left, Defined right -> (
          try
            let join_parameter sofar left right =
              match sofar with
              | Some sofar ->
                  let joined =
                    if Type.Callable.Parameter.names_compatible left right then
                      match left, right with
                      | Parameter.Anonymous left, Parameter.Anonymous right
                        when left.default = right.default ->
                          Some
                            (Parameter.Anonymous
                               { left with
                                 annotation = parameter_join order left.annotation right.annotation
                               })
                      | Parameter.Anonymous anonymous, Parameter.Named named
                      | Parameter.Named named, Parameter.Anonymous anonymous
                        when named.default = anonymous.default ->
                          Some
                            (Parameter.Anonymous
                               { anonymous with
                                 annotation =
                                   parameter_join order named.annotation anonymous.annotation
                               })
                      | Parameter.Named left, Parameter.Named right
                        when left.Parameter.default = right.Parameter.default ->
                          Some
                            (Parameter.Named
                               { left with
                                 Parameter.annotation =
                                   parameter_join
                                     order
                                     left.Parameter.annotation
                                     right.Parameter.annotation
                               })
                      | Parameter.Variable left, Parameter.Variable right ->
                          Some
                            (Parameter.Variable
                               { left with
                                 Parameter.annotation =
                                   parameter_join
                                     order
                                     left.Parameter.annotation
                                     right.Parameter.annotation
                               })
                      | Parameter.Keywords left, Parameter.Keywords right ->
                          Some
                            (Parameter.Keywords
                               { left with
                                 Parameter.annotation =
                                   parameter_join
                                     order
                                     left.Parameter.annotation
                                     right.Parameter.annotation
                               })
                      | _ -> None
                    else
                      None
                  in
                  joined >>| fun joined -> joined :: sofar
              | None -> None
            in
            List.fold2_exn ~init:(Some []) ~f:join_parameter left right
            >>| List.rev
            >>| fun parameters -> Defined parameters
          with
          | _ -> None )
        | _ -> None
      in
      parameters
      >>| fun parameters ->
      { annotation = return_join order left.annotation right.annotation; parameters }


    and join
        ({ handler = (module Handler : Handler) as handler; constructor; is_protocol; _ } as order)
        left
        right
      =
      let union = Type.union [left; right] in
      if Type.equal left right then
        left
      else if
        Type.Variable.contains_escaped_free_variable left
        || Type.Variable.contains_escaped_free_variable right
      then
        union
      else
        match left, right with
        | Type.Bottom, other
        | other, Type.Bottom ->
            other
        | undeclared, _ when Type.equal undeclared Type.undeclared -> Type.union [left; right]
        | _, undeclared when Type.equal undeclared Type.undeclared -> Type.union [left; right]
        | Type.Top, _
        | _, Type.Top ->
            Type.Top
        | Type.Any, _
        | _, Type.Any ->
            Type.Any
        (* n: A_n = B_n -> Union[A_i] <= Union[B_i]. *)
        | Type.Union left, Type.Union right -> Type.union (left @ right)
        | (Type.Union elements as union), other
        | other, (Type.Union elements as union) -> (
            if less_or_equal order ~left:other ~right:union then
              union
            else
              match other with
              | Type.Optional Type.Bottom -> Type.Optional union
              | Type.Optional element -> Type.Optional (Type.union (element :: elements))
              | _ ->
                  List.map elements ~f:(join order other)
                  |> List.fold ~f:(join order) ~init:Type.Bottom )
        | _, Type.Variable _
        | Type.Variable _, _ ->
            union
        | Type.Parametric _, Type.Parametric _
        | Type.Parametric _, Type.Primitive _
        | Type.Primitive _, Type.Parametric _ ->
            if less_or_equal order ~left ~right then
              right
            else if less_or_equal order ~left:right ~right:left then
              left
            else
              let left_primitive, _ = Type.split left in
              let right_primitive, _ = Type.split right in
              let target =
                try
                  if less_or_equal ~left:left_primitive ~right:right_primitive order then
                    right_primitive
                  else if less_or_equal ~left:right_primitive ~right:left_primitive order then
                    left_primitive
                  else
                    join order left_primitive right_primitive
                with
                | Untracked _ -> Type.Any
              in
              if Handler.contains (Handler.indices ()) target then
                let left_parameters =
                  instantiate_successors_parameters order ~source:left ~target
                in
                let right_parameters =
                  instantiate_successors_parameters order ~source:right ~target
                in
                let variables = variables handler target in
                let parameters =
                  let join_parameters left right variable =
                    match left, right, variable with
                    | Type.Bottom, other, _
                    | other, Type.Bottom, _ ->
                        other
                    | Type.Top, _, _
                    | _, Type.Top, _ ->
                        Type.Top
                    | _, _, Type.Variable { variance = Covariant; _ } -> join order left right
                    | _, _, Type.Variable { variance = Contravariant; _ } -> meet order left right
                    | _, _, Type.Variable { variance = Invariant; _ } ->
                        if
                          less_or_equal order ~left ~right
                          && less_or_equal order ~left:right ~right:left
                        then
                          left
                        else
                          (* We fallback to Type.Any if type equality fails to help display
                             meaningful error messages. *)
                          Type.Any
                    | _ ->
                        Log.warning
                          "Cannot join %a and %a, not a variable: %a"
                          Type.pp
                          left
                          Type.pp
                          right
                          Type.pp
                          variable;
                        Type.Any
                  in
                  match left_parameters, right_parameters, variables with
                  | Some left, Some right, Some variables
                    when List.length left = List.length right
                         && List.length left = List.length variables ->
                      let join_parameters left right variable =
                        let replace_free_unary_variables_with_top =
                          let replace_if_free variable =
                            Option.some_if (Type.Variable.Unary.is_free variable) Type.Top
                          in
                          Type.Variable.UnaryGlobalTransforms.replace_all replace_if_free
                        in
                        join_parameters left right variable
                        |> replace_free_unary_variables_with_top
                      in
                      Some (List.map3_exn ~f:join_parameters left right variables)
                  | _ -> None
                in
                match target, parameters with
                | Type.Primitive name, Some parameters -> Type.Parametric { name; parameters }
                | Type.Primitive _, None -> target
                | _ ->
                    (* TODO(T41082573) throw here instead of unioning *)
                    union
              else (* TODO(T41082573) throw here instead of unioning *)
                union
        (* Special case joins of optional collections with their uninstantated counterparts. *)
        | ( Type.Parametric ({ parameters = [Type.Bottom]; _ } as other),
            Type.Optional (Type.Parametric ({ parameters = [parameter]; _ } as collection)) )
        | ( Type.Optional (Type.Parametric ({ parameters = [parameter]; _ } as collection)),
            Type.Parametric ({ parameters = [Type.Bottom]; _ } as other) )
          when Identifier.equal other.name collection.name ->
            Type.Parametric { other with parameters = [parameter] }
        (* A <= B -> lub(A, Optional[B]) = Optional[B]. *)
        | other, Type.Optional parameter
        | Type.Optional parameter, other ->
            Type.optional (join order other parameter)
        (* Tuple variables are covariant. *)
        | Type.Tuple (Type.Bounded left), Type.Tuple (Type.Bounded right)
          when List.length left = List.length right ->
            List.map2_exn left right ~f:(join order) |> Type.tuple
        | Type.Tuple (Type.Unbounded left), Type.Tuple (Type.Unbounded right) ->
            Type.Tuple (Type.Unbounded (join order left right))
        | Type.Tuple (Type.Bounded (left :: tail)), Type.Tuple (Type.Unbounded right)
        | Type.Tuple (Type.Unbounded right), Type.Tuple (Type.Bounded (left :: tail))
          when List.for_all ~f:(fun element -> Type.equal element left) tail
               && less_or_equal order ~left ~right ->
            Type.Tuple (Type.Unbounded right)
        | Type.Tuple (Type.Unbounded parameter), (Type.Parametric _ as annotation)
        | Type.Tuple (Type.Unbounded parameter), (Type.Primitive _ as annotation)
        | (Type.Parametric _ as annotation), Type.Tuple (Type.Unbounded parameter)
        | (Type.Primitive _ as annotation), Type.Tuple (Type.Unbounded parameter) ->
            join order (Type.parametric "tuple" [parameter]) annotation
        | Type.Tuple (Type.Bounded parameters), (Type.Parametric _ as annotation) ->
            (* Handle cases like `Tuple[int, int]` <= `Iterator[int]`. *)
            let parameter = List.fold ~init:Type.Bottom ~f:(join order) parameters in
            join order (Type.parametric "tuple" [parameter]) annotation
        | Type.Tuple _, _
        | _, Type.Tuple _ ->
            Type.union [left; right]
        | ( (Type.Callable { Callable.kind = Callable.Named left; _ } as callable),
            Type.Callable { Callable.kind = Callable.Named right; _ } )
          when Reference.equal left right ->
            callable
        | ( Type.TypedDictionary { fields = left_fields; total = left_total; _ },
            Type.TypedDictionary { fields = right_fields; total = right_total; _ } ) ->
            if
              Type.TypedDictionary.fields_have_colliding_keys left_fields right_fields
              || left_total <> right_total
            then
              Type.Parametric { name = "typing.Mapping"; parameters = [Type.string; Type.Any] }
            else
              let join_fields =
                if less_or_equal order ~left ~right then
                  right_fields
                else if less_or_equal order ~left:right ~right:left then
                  left_fields
                else
                  let found_match field =
                    List.exists left_fields ~f:(Type.equal_typed_dictionary_field field)
                  in
                  List.filter right_fields ~f:found_match
              in
              Type.TypedDictionary.anonymous ~total:left_total join_fields
        | Type.TypedDictionary _, other
        | other, Type.TypedDictionary _ ->
            let class_join = join order (Type.Primitive "TypedDictionary") other in
            let failed =
              Type.exists class_join ~predicate:(function
                  | Type.Primitive "TypedDictionary" -> true
                  | _ -> false)
            in
            if failed then union else class_join
        | Type.Callable left, Type.Callable right ->
            if List.is_empty left.Callable.overloads && List.is_empty right.Callable.overloads then
              let kind =
                if Type.Callable.equal_kind left.kind right.kind then
                  left.kind
                else
                  Type.Callable.Anonymous
              in
              join_implementations
                ~parameter_join:meet
                ~return_join:join
                order
                left.Callable.implementation
                right.Callable.implementation
              >>| (fun implementation -> Type.Callable { left with Callable.kind; implementation })
              |> Option.value ~default:union
            else
              union
        | Type.Callable callable, other
        | other, Type.Callable callable ->
            let default =
              let other = join order (Type.parametric "typing.Callable" [Type.Bottom]) other in
              match other with
              | Type.Parametric { name; parameters = [other_callable] }
                when Identifier.equal name "typing.Callable" ->
                  join order (Type.Callable callable) other_callable
              | _ -> Type.union [left; right]
            in
            Option.some_if (Type.is_meta other) other
            >>= constructor
            >>| join order (Type.Callable callable)
            |> Option.value ~default
        | (Type.Literal _ as literal), other
        | other, (Type.Literal _ as literal) ->
            join order other (Type.weaken_literals literal)
        | _ when is_protocol right && less_or_equal order ~left ~right -> right
        | _ when is_protocol left && less_or_equal order ~left:right ~right:left -> left
        | _ -> (
          match List.hd (least_upper_bound handler left right) with
          | Some joined ->
              if Type.equal joined left || Type.equal joined right then
                joined
              else
                union
          | None ->
              Log.debug "Couldn't find a upper bound for %a and %a" Type.pp left Type.pp right;
              union )


    and meet
        ({ handler = (module Handler : Handler) as handler; constructor; is_protocol; _ } as order)
        left
        right
      =
      if Type.equal left right then
        left
      else
        match left, right with
        | Type.Top, other
        | other, Type.Top ->
            other
        | Type.Any, other
        | other, Type.Any
          when not (Type.is_unknown other) ->
            other
        | Type.Bottom, _
        | _, Type.Bottom ->
            Type.Bottom
        | (Type.Variable _ as variable), other
        | other, (Type.Variable _ as variable) ->
            if less_or_equal order ~left:variable ~right:other then
              variable
            else
              Type.Bottom
        | Type.Union left, Type.Union right ->
            let union =
              Set.inter (Type.Set.of_list left) (Type.Set.of_list right) |> Set.to_list
            in
            Type.union union
        | (Type.Union elements as union), other
        | other, (Type.Union elements as union) ->
            if less_or_equal order ~left:other ~right:union then
              other
            else
              List.map elements ~f:(meet order other) |> List.fold ~f:(meet order) ~init:Type.Top
        | Type.Parametric _, Type.Parametric _ ->
            if less_or_equal order ~left ~right then
              left
            else if less_or_equal order ~left:right ~right:left then
              right
            else
              let left_primitive, _ = Type.split left in
              let right_primitive, _ = Type.split right in
              let target = meet order left_primitive right_primitive in
              if Handler.contains (Handler.indices ()) target then
                let left_parameters =
                  instantiate_predecessors_parameters order ~source:left ~target
                in
                let right_parameters =
                  instantiate_predecessors_parameters order ~source:right ~target
                in
                let variables = variables handler target in
                let parameters =
                  let meet_parameters left right variable =
                    match left, right, variable with
                    | Type.Bottom, _, _
                    | _, Type.Bottom, _ ->
                        Type.Bottom
                    | Type.Top, other, _
                    | other, Type.Top, _ ->
                        other
                    | Type.Any, _, _
                    | _, Type.Any, _ ->
                        Type.Bottom
                    | _, _, Type.Variable { variance = Covariant; _ } -> meet order left right
                    | _, _, Type.Variable { variance = Contravariant; _ } -> join order left right
                    | _, _, Type.Variable { variance = Invariant; _ } ->
                        if
                          less_or_equal order ~left ~right
                          && less_or_equal order ~left:right ~right:left
                        then
                          left
                        else
                          (* We fallback to Type.Bottom if type equality fails to help display
                             meaningful error messages. *)
                          Type.Bottom
                    | _ ->
                        Log.warning
                          "Cannot meet %a and %a, not a variable: %a"
                          Type.pp
                          left
                          Type.pp
                          right
                          Type.pp
                          variable;
                        Type.Bottom
                  in
                  match left_parameters, right_parameters, variables with
                  | Some left, Some right, Some variables
                    when List.length left = List.length right
                         && List.length left = List.length variables ->
                      Some (List.map3_exn ~f:meet_parameters left right variables)
                  | _ -> None
                in
                match target, parameters with
                | Type.Primitive name, Some parameters -> Type.Parametric { name; parameters }
                | _ -> Type.Bottom
              else
                Type.Bottom
        (* A <= B -> glb(A, Optional[B]) = A. *)
        | other, Type.Optional parameter
        | Type.Optional parameter, other ->
            if less_or_equal order ~left:other ~right:parameter then
              other
            else
              Type.Bottom
        (* Tuple variables are covariant. *)
        | Type.Tuple (Type.Bounded left), Type.Tuple (Type.Bounded right)
          when List.length left = List.length right ->
            List.map2_exn left right ~f:(meet order) |> Type.tuple
        | Type.Tuple (Type.Unbounded left), Type.Tuple (Type.Unbounded right) ->
            Type.Tuple (Type.Unbounded (meet order left right))
        | Type.Tuple (Type.Bounded (left :: tail)), Type.Tuple (Type.Unbounded right)
        | Type.Tuple (Type.Unbounded right), Type.Tuple (Type.Bounded (left :: tail))
          when List.for_all ~f:(fun element -> Type.equal element left) tail
               && less_or_equal order ~left ~right ->
            Type.Tuple (Type.Unbounded left) (* My brain hurts... *)
        | (Type.Tuple _ as tuple), (Type.Parametric _ as parametric)
        | (Type.Parametric _ as parametric), (Type.Tuple _ as tuple) ->
            if less_or_equal order ~left:tuple ~right:parametric then
              tuple
            else
              Type.Bottom
        | Type.Tuple _, _
        | _, Type.Tuple _ ->
            Type.Bottom
        | Type.Parametric _, Type.Primitive _
        | Type.Primitive _, Type.Parametric _ ->
            if less_or_equal order ~left ~right then
              left
            else if less_or_equal order ~left:right ~right:left then
              right
            else
              Type.Bottom
        | ( Type.Callable ({ Callable.kind = Callable.Anonymous; _ } as left),
            Type.Callable ({ Callable.kind = Callable.Anonymous; _ } as right) ) ->
            join_implementations
              ~parameter_join:join
              ~return_join:meet
              order
              left.Callable.implementation
              right.Callable.implementation
            >>| (fun implementation -> Type.Callable { left with Callable.implementation })
            |> Option.value ~default:Type.Bottom
        | ( (Type.Callable { Callable.kind = Callable.Named left; _ } as callable),
            Type.Callable { Callable.kind = Callable.Named right; _ } )
          when Reference.equal left right ->
            callable
        | Type.Callable callable, other
        | other, Type.Callable callable ->
            Option.some_if (Type.is_meta other) other
            >>= constructor
            >>| meet order (Type.Callable callable)
            |> Option.value ~default:Type.Bottom
        | ( Type.TypedDictionary { fields = left_fields; total = left_total; _ },
            Type.TypedDictionary { fields = right_fields; total = right_total; _ } ) ->
            if
              Type.TypedDictionary.fields_have_colliding_keys left_fields right_fields
              || left_total <> right_total
            then
              Type.Bottom
            else
              let meet_fields =
                if less_or_equal order ~left ~right then
                  left_fields
                else if less_or_equal order ~left:right ~right:left then
                  right_fields
                else
                  List.dedup_and_sort
                    (left_fields @ right_fields)
                    ~compare:Type.compare_typed_dictionary_field
              in
              Type.TypedDictionary.anonymous ~total:left_total meet_fields
        | Type.TypedDictionary _, _
        | _, Type.TypedDictionary _ ->
            Type.Bottom
        | Type.Literal _, _
        | _, Type.Literal _ ->
            Type.Bottom
        | _ when is_protocol right && less_or_equal order ~left ~right -> left
        | _ when is_protocol left && less_or_equal order ~left:right ~right:left -> right
        | _ -> (
          match List.hd (greatest_lower_bound handler left right) with
          | Some bound -> bound
          | None ->
              Log.debug "No lower bound found for %a and %a" Type.pp left Type.pp right;
              Type.Bottom )


    (* If a node on the graph has Generic[_T1, _T2, ...] as a supertype and has concrete
       parameters, all occurrences of _T1, _T2, etc. in other supertypes need to be replaced with
       the concrete parameter corresponding to the type variable. This function takes a target with
       concrete parameters and its supertypes, and instantiates the supertypes accordingly. *)
    and get_instantiated_successors ~generic_index ~parameters successors =
      let variables =
        get_generic_parameters successors ~generic_index |> Option.value ~default:[]
      in
      let parameters =
        if List.length variables = List.length parameters then
          parameters
        else
          (* This is the specified behavior for empty parameters, and other mismatched lengths
             should have an error at the declaration site, and this behavior seems reasonable *)
          List.init (List.length variables) ~f:(fun _ -> Type.Any)
      in
      let constraints =
        List.zip_exn variables parameters
        |> Type.Map.of_alist_reduce ~f:(fun first _ -> first)
        |> Map.find
      in
      let instantiate_parameters { Target.target; parameters } =
        { Target.target; parameters = List.map parameters ~f:(Type.instantiate ~constraints) }
      in
      List.map successors ~f:instantiate_parameters


    and get_instantiated_predecessors
        (module Handler : Handler)
        ~generic_index
        ~parameters
        predecessors
      =
      let instantiate { Target.target; parameters = predecessor_variables } =
        let generic_parameters =
          Handler.find (Handler.edges ()) target
          >>= get_generic_parameters ~generic_index
          |> Option.value ~default:[]
        in
        (* Mappings from the generic variables, as they appear in the predecessor, to the
           instantiated parameter in the current annotation. For example, given:

           Derived(Base[T1, int, T2], Generic[ ...irrelevant... ])

           and an instantiated: Base[str, int, float] This mapping would include: { T1 => str; T2
           => float } *)
        let substitutions =
          match
            List.fold2 predecessor_variables parameters ~init:Type.Map.empty ~f:diff_variables
          with
          | Ok result -> result
          | Unequal_lengths -> Type.Map.empty
        in
        let propagated =
          let replace parameter =
            Map.find substitutions parameter
            (* Use Bottom if we could not determine the value of the generic because the
               predecessor did not propagate it to the base class. *)
            |> Option.value ~default:Type.Bottom
          in
          List.map generic_parameters ~f:replace
        in
        { Target.target; parameters = propagated }
      in
      List.map predecessors ~f:instantiate


    and instantiate_successors_parameters
        ({ handler = (module Handler : Handler) as handler; _ } as order)
        ~source
        ~target
      =
      raise_if_untracked handler target;
      let generic_index = Handler.find (Handler.indices ()) Type.generic_primitive in
      match source with
      | Type.Bottom ->
          index_of handler target
          |> Handler.find (Handler.edges ())
          >>= get_generic_parameters ~generic_index
          >>| List.map ~f:(fun _ -> Type.Any)
      | _ ->
          let primitive, parameters = Type.split source in
          let parameters =
            match primitive with
            | Type.Primitive "tuple" ->
                (* Handle cases like `Tuple[int, int]` <= `Iterator[int]`. *)
                [List.fold ~init:Type.Bottom ~f:(join order) parameters]
                |> List.map ~f:Type.weaken_literals
            | _ -> parameters
          in
          raise_if_untracked handler primitive;
          let worklist = Queue.create () in
          Queue.enqueue worklist { Target.target = index_of handler primitive; parameters };
          let rec iterate worklist =
            match Queue.dequeue worklist with
            | Some { Target.target = target_index; parameters } ->
                let instantiated_successors =
                  Handler.find (Handler.edges ()) target_index
                  >>| get_instantiated_successors ~generic_index ~parameters
                in
                if target_index = index_of handler target then
                  match target with
                  | Type.Primitive "typing.Callable" -> Some parameters
                  | _ -> instantiated_successors >>= get_generic_parameters ~generic_index
                else (
                  instantiated_successors >>| List.iter ~f:(Queue.enqueue worklist) |> ignore;
                  iterate worklist )
            | None -> None
          in
          iterate worklist


    and instantiate_predecessors_parameters
        { handler = (module Handler : Handler) as handler; _ }
        ~source
        ~target
      =
      let primitive, parameters = Type.split source in
      raise_if_untracked handler primitive;
      raise_if_untracked handler target;
      let generic_index = Handler.find (Handler.indices ()) Type.generic_primitive in
      let worklist = Queue.create () in
      Queue.enqueue worklist { Target.target = index_of handler primitive; parameters };
      let rec iterate worklist =
        match Queue.dequeue worklist with
        | Some { Target.target = target_index; parameters } ->
            if target_index = index_of handler target then
              Some parameters
            else (
              Handler.find (Handler.backedges ()) target_index
              >>| Set.to_list
              >>| get_instantiated_predecessors handler ~generic_index ~parameters
              >>| List.iter ~f:(Queue.enqueue worklist)
              |> ignore;
              iterate worklist )
        | None -> None
      in
      iterate worklist


    and diff_variables substitutions left right =
      match left, right with
      | ( Callable { implementation = left_implementation; overloads = left_overloads; _ },
          Callable { implementation = right_implementation; overloads = right_overloads; _ } ) -> (
          let open Type.Callable in
          let diff_overloads
              substitutions
              { annotation = left_annotation; parameters = left_parameters }
              { annotation = right_annotation; parameters = right_parameters }
            =
            let substitutions = diff_variables substitutions left_annotation right_annotation in
            match left_parameters, right_parameters with
            | Defined left, Defined right -> (
                let diff_parameters substitutions left right =
                  diff_variables
                    substitutions
                    (Parameter.annotation left)
                    (Parameter.annotation right)
                in
                match List.fold2 ~init:substitutions ~f:diff_parameters left right with
                | Ok substitutions -> substitutions
                | Unequal_lengths -> substitutions )
            | _ -> substitutions
          in
          let substitutions =
            diff_overloads substitutions left_implementation right_implementation
          in
          match
            List.fold2 ~init:substitutions ~f:diff_overloads left_overloads right_overloads
          with
          | Ok substitutions -> substitutions
          | Unequal_lengths -> substitutions )
      | Optional left, Optional right -> diff_variables substitutions left right
      | Parametric { parameters = left; _ }, Parametric { parameters = right; _ } ->
          diff_variables_list substitutions left right
      | Tuple (Bounded left), Tuple (Bounded right) -> diff_variables_list substitutions left right
      | Tuple (Unbounded left), Tuple (Unbounded right) -> diff_variables substitutions left right
      | ( TypedDictionary { fields = left_fields; total = left_total; _ },
          TypedDictionary { fields = right_fields; total = right_total; _ } ) ->
          if left_total = right_total then
            let diff_fields
                substitutions { Type.annotation = left; _ } { Type.annotation = right; _ }
              =
              diff_variables substitutions left right
            in
            match List.fold2 ~init:substitutions ~f:diff_fields left_fields right_fields with
            | Ok substitutions -> substitutions
            | Unequal_lengths -> substitutions
          else
            substitutions
      | Union left, Union right -> diff_variables_list substitutions left right
      | Variable _, _ -> Map.set substitutions ~key:left ~data:right
      | _ -> substitutions


    and diff_variables_list substitutions left right =
      match List.fold2 left right ~init:substitutions ~f:diff_variables with
      | Ok substitutions -> substitutions
      | Unequal_lengths -> substitutions


    and instantiate_protocol_parameters
        ( { attributes; handler = (module Handler : Handler) as handler; protocol_assumptions; _ }
        as order )
        ~candidate
        ~protocol
      =
      let find_generic_parameters name =
        let generic_index = Handler.find (Handler.indices ()) Type.generic_primitive in
        let index = index_of handler (Type.Primitive name) in
        Handler.find (Handler.edges ()) index >>= get_generic_parameters ~generic_index
      in
      match candidate with
      | Type.Primitive candidate_name when Option.is_some (find_generic_parameters candidate_name)
        ->
          (* If we are given a "stripped" generic, we decline to do structural analysis, as these
             kinds of comparisons only exists for legacy reasons to do nominal comparisons *)
          None
      | Type.Primitive candidate_name when Identifier.equal candidate_name protocol -> Some []
      | Type.Parametric { name; parameters } when Identifier.equal name protocol -> Some parameters
      | _ -> (
          let assumed_protocol_parameters =
            ProtocolAssumptions.find_assumed_protocol_parameters
              protocol_assumptions
              ~candidate
              ~protocol
          in
          match assumed_protocol_parameters with
          | Some result -> Some result
          | None -> (
              let protocol_attributes =
                let is_not_object_or_generic_method
                    { Node.value = { AnnotatedAttribute.parent; _ }; _ }
                  =
                  (not (Type.is_object parent)) && not (Type.is_generic_primitive parent)
                in
                attributes (Type.Primitive protocol)
                >>| List.filter ~f:is_not_object_or_generic_method
              in
              let candidate_attributes, transformations =
                match candidate with
                | Type.Callable _ as callable ->
                    let attributes =
                      [ Node.create_with_default_location
                          { AnnotatedAttribute.name = "__call__";
                            parent = callable;
                            annotation = Annotation.create callable;
                            value = Node.create_with_default_location Expression.Ellipsis;
                            defined = true;
                            class_attribute = false;
                            async = false;
                            initialized = true;
                            property = false;
                            final = false;
                            static = false;
                            frozen = false
                          } ]
                      |> Option.some
                    in
                    attributes, []
                | _ ->
                    (* We don't return constraints for the candidate's free variables, so we must
                       underapproximate and determine conformance in the worst case *)
                    let transformations, sanitized_candidate =
                      let namespace = Type.Variable.Namespace.create_fresh () in
                      let module SanitizeTransform = Type.Transform.Make (struct
                        type state = (Type.Variable.Unary.t * Type.Variable.Unary.t) list

                        let visit_children_before _ _ = true

                        let visit_children_after = false

                        let visit sofar = function
                          | Type.Variable variable when Type.Variable.Unary.is_free variable ->
                              let transformed_variable =
                                Type.Variable.Unary.namespace variable ~namespace
                                |> Type.Variable.Unary.mark_as_bound
                              in
                              { Type.Transform.transformed_annotation =
                                  Type.Variable transformed_variable;
                                new_state = (variable, transformed_variable) :: sofar
                              }
                          | transformed_annotation ->
                              { Type.Transform.transformed_annotation; new_state = sofar }
                      end)
                      in
                      SanitizeTransform.visit [] candidate
                    in
                    attributes sanitized_candidate, transformations
              in
              match candidate_attributes, protocol_attributes with
              | Some all_candidate_attributes, Some all_protocol_attributes ->
                  let build_attribute_map =
                    let add_to_map map data =
                      match Identifier.Map.add map ~key:(AnnotatedAttribute.name data) ~data with
                      | `Ok x -> x
                      (* Attributes are listed in resolution order *)
                      | `Duplicate -> map
                    in
                    List.fold ~f:add_to_map ~init:Identifier.Map.empty
                  in
                  let merge_attributes ~key:_ = function
                    | `Both pair -> Some (`Found pair)
                    (* In candidate but not protocol *)
                    | `Left _ -> None
                    (* In protocol but not candidate *)
                    | `Right _ -> Some `Missing
                  in
                  let protocol_generics =
                    find_generic_parameters protocol
                    >>| List.map ~f:(function
                            | Type.Variable variable -> Some variable
                            | _ -> None)
                    >>= Option.all
                    >>| List.map ~f:(fun variable -> Type.Variable variable)
                  in
                  let order_with_new_assumption =
                    { order with
                      protocol_assumptions =
                        ProtocolAssumptions.add
                          protocol_assumptions
                          ~candidate
                          ~protocol
                          ~protocol_parameters:(Option.value protocol_generics ~default:[])
                    }
                  in
                  let attribute_implements ~key:_ ~data constraints_set =
                    match data with
                    | `Found (candidate_attribute, protocol_attribute) ->
                        let attribute_annotation attribute =
                          AnnotatedAttribute.annotation attribute |> Annotation.annotation
                        in
                        List.concat_map constraints_set ~f:(fun constraints ->
                            solve_less_or_equal_safe
                              order_with_new_assumption
                              ~left:(attribute_annotation candidate_attribute)
                              ~right:(attribute_annotation protocol_attribute)
                              ~constraints)
                    | `Missing -> []
                  in
                  let instantiate_protocol_generics solution =
                    let desanitize_map = List.Assoc.inverse transformations in
                    let desanitize =
                      let constraints = function
                        | Type.Variable variable ->
                            List.Assoc.find
                              desanitize_map
                              variable
                              ~equal:Type.Variable.Unary.equal
                            >>| fun variable -> Type.Variable variable
                        | _ -> None
                      in
                      Type.instantiate ~constraints
                    in
                    protocol_generics
                    >>| List.map ~f:(TypeConstraints.Solution.instantiate solution)
                    >>| List.map ~f:desanitize
                    |> Option.value ~default:[]
                  in
                  Identifier.Map.merge
                    (build_attribute_map all_candidate_attributes)
                    (build_attribute_map all_protocol_attributes)
                    ~f:merge_attributes
                  |> Identifier.Map.fold ~init:[TypeConstraints.empty] ~f:attribute_implements
                  |> List.filter_map ~f:(OrderedConstraints.solve ~order:order_with_new_assumption)
                  |> List.hd
                  >>| instantiate_protocol_generics
              | _ -> None ) )


    let solve_less_or_equal order ~constraints ~left ~right =
      solve_less_or_equal_safe order ~constraints ~left ~right
  end
end

module rec Constraints : OrderedConstraintsType =
  TypeConstraints.OrderedConstraints (Implementation)

and Implementation : FullOrderType = OrderImplementation.Make (Constraints)

module OrderedConstraints = Constraints

module IncludableImplementation : FullOrderTypeWithoutT = Implementation

include IncludableImplementation

let rec is_consistent_with order left right =
  less_or_equal { order with any_is_bottom = true } ~left ~right


let rec is_compatible_with order ~left ~right =
  match left, right with
  (* Any *)
  | _, Type.Any
  | Type.Any, _ ->
      true
  (* Top *)
  | _, Type.Top -> true
  | Type.Top, _ -> false
  (* Optional *)
  | Type.Optional left, Type.Optional right -> is_compatible_with order ~left ~right
  | _, Type.Optional parameter -> is_compatible_with order ~left ~right:parameter
  | Type.Optional _, _ -> false
  (* Tuple *)
  | Type.Tuple (Type.Bounded left), Type.Tuple (Type.Bounded right)
    when List.length left = List.length right ->
      List.for_all2_exn left right ~f:(fun left right -> is_compatible_with order ~left ~right)
  | Type.Tuple (Type.Bounded bounded), Type.Tuple (Type.Unbounded right) ->
      List.for_all bounded ~f:(fun bounded_type ->
          is_compatible_with order ~left:bounded_type ~right)
  (* Union *)
  | Type.Union left, right ->
      List.fold
        ~init:true
        ~f:(fun current left -> current && is_compatible_with order ~left ~right)
        left
  | left, Type.Union right ->
      List.exists ~f:(fun right -> is_compatible_with order ~left ~right) right
  (* Parametric *)
  | ( Parametric { name = left_name; parameters = left_parameters },
      Parametric { name = right_name; parameters = right_parameters } )
    when String.equal left_name right_name
         && Int.equal (List.length left_parameters) (List.length right_parameters) ->
      List.for_all2_exn left_parameters right_parameters ~f:(fun left right ->
          is_compatible_with order ~left ~right)
  (* Fallback *)
  | _, _ -> is_consistent_with order left right


let rec consistent_solution_exists order left right =
  let order = { order with any_is_bottom = true } in
  solve_less_or_equal order ~left ~right ~constraints:TypeConstraints.empty
  |> List.filter_map ~f:(OrderedConstraints.solve ~order)
  |> List.is_empty
  |> not


let widen order ~widening_threshold ~previous ~next ~iteration =
  if iteration > widening_threshold then
    Type.Top
  else
    join order previous next


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


let remove_extra_edges (module Handler : Handler) ~bottom ~top annotations =
  let module Disconnector (Edges : Target.ListOrSet) (Backedges : Target.ListOrSet) = struct
    let disconnect keys ~edges ~backedges special_index =
      let remove_extra_references key =
        Handler.find edges key
        >>| (fun connected ->
              let disconnected =
                Edges.filter connected ~f:(fun { Target.target; _ } -> target <> special_index)
              in
              if Edges.is_empty disconnected then
                []
              else (
                Handler.set edges ~key ~data:disconnected;
                [key] ))
        |> Option.value ~default:[]
      in
      let removed_indices = List.concat_map ~f:remove_extra_references keys |> Int.Set.of_list in
      Handler.find backedges special_index
      >>| (fun edges ->
            let edges =
              Backedges.filter edges ~f:(fun { Target.target; _ } ->
                  not (Set.mem removed_indices target))
            in
            Handler.set backedges ~key:special_index ~data:edges)
      |> Option.value ~default:()
  end
  in
  let edges = Handler.edges () in
  let backedges = Handler.backedges () in
  let index_of annotation = Handler.find_unsafe (Handler.indices ()) annotation in
  let keys = List.map annotations ~f:index_of in
  let module ForwardDisconnector = Disconnector (Target.List) (Target.Set) in
  ForwardDisconnector.disconnect keys ~edges ~backedges (index_of top);
  let module ReverseDisconnector = Disconnector (Target.Set) (Target.List) in
  ReverseDisconnector.disconnect keys ~edges:backedges ~backedges:edges (index_of bottom)


let connect_annotations_to_top ((module Handler : Handler) as handler)
                               ~top
                               annotations =
  let indices = Handler.indices () in
  let connect_to_top annotation =
    let index = Handler.find_unsafe indices annotation in
    let annotation = Handler.find_unsafe (Handler.annotations ()) index in
    let order =
      { handler;
        constructor = (fun _ -> None);
        attributes = (fun _ -> None);
        is_protocol = (fun _ -> false);
        any_is_bottom = false;
        protocol_assumptions = ProtocolAssumptions.empty
      }
    in
    if not (less_or_equal order ~left:top ~right:annotation) then
      match Handler.find (Handler.edges ()) index with
      | Some targets when List.length targets > 0 -> ()
      | _ -> connect handler ~predecessor:annotation ~successor:top
  in
  List.iter ~f:connect_to_top annotations


let sort_bottom_edges (module Handler : Handler) ~bottom =
  let bottom_index = Handler.find_unsafe (Handler.indices ()) bottom in
  match Handler.find (Handler.edges ()) bottom_index with
  | None -> ()
  | Some edges ->
      Handler.set
        (Handler.edges ())
        ~key:bottom_index
        ~data:(List.rev (List.sort edges ~compare:Target.compare))


let check_integrity (module Handler : Handler) =
  (* Check `Top` and `Bottom`. *)
  let contains annotation = Handler.contains (Handler.indices ()) annotation in
  if not (contains Type.Bottom && contains Type.Top) then (
    Log.error "Order is missing either Bottom or Top:\n%s" (Handler.show ());
    raise Incomplete );
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
            |> List.map ~f:(Format.asprintf "%a" Type.pp)
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
              "No back-edge found for %a -> %a"
              Type.pp
              (Handler.find_unsafe (Handler.annotations ()) index)
              Type.pp
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
      Format.asprintf "  %d[label=\"%s\"]\n" index (Type.show annotation)
      |> Buffer.add_string buffer)
    nodes;
  let add_edges index =
    Handler.find (Handler.edges ()) index
    >>| List.sort ~compare
    >>| List.iter ~f:(fun { Target.target = successor; parameters } ->
            Format.asprintf "  %d -> %d" index successor |> Buffer.add_string buffer;
            if List.length parameters > 0 then
              Format.asprintf "[label=\"%s\"]" (List.to_string ~f:Type.show parameters)
              |> Buffer.add_string buffer;
            Buffer.add_string buffer "\n")
    |> ignore
  in
  List.iter ~f:add_edges indices;
  Buffer.add_string buffer "}";
  Buffer.contents buffer


module Builder = struct
  let create () =
    { edges = Int.Table.create ();
      backedges = Int.Table.create ();
      indices = Type.Table.create ();
      annotations = Int.Table.create ()
    }


  let copy { edges; backedges; indices; annotations } =
    { edges = Hashtbl.copy edges;
      backedges = Hashtbl.copy backedges;
      indices = Hashtbl.copy indices;
      annotations = Hashtbl.copy annotations
    }


  let default_annotations =
    let singleton annotation = [Type.Bottom; annotation; Type.object_primitive] in
    [ [Type.Bottom; Type.object_primitive; Type.Any; Type.Top];
      (* Special forms *)
      singleton (Type.Primitive "typing.Tuple");
      singleton Type.named_tuple;
      singleton Type.generic_primitive;
      singleton (Type.Primitive "typing.GenericMeta");
      singleton (Type.Primitive "typing.Protocol");
      singleton (Type.Primitive "typing.Callable");
      singleton (Type.Primitive "typing.FrozenSet");
      singleton (Type.Primitive "typing.Optional");
      singleton (Type.Primitive "typing.TypeVar");
      singleton (Type.Primitive "typing.Undeclared");
      singleton (Type.Primitive "typing.Union");
      singleton (Type.Primitive "typing.NoReturn");
      (* Ensure unittest.mock.Base is there because we check against it. *)
      singleton (Type.Primitive "unittest.mock.Base");
      singleton (Type.Primitive "unittest.mock.NonCallableMock");
      singleton (Type.Primitive "typing.ClassVar");
      singleton (Type.Primitive "typing.Final");
      [Type.Bottom; Type.Primitive "dict"; Type.Primitive "typing.Dict"; Type.object_primitive];
      singleton (Type.Primitive "None");
      (* Numerical hierarchy. *)
      [ Type.Bottom;
        Type.integer;
        Type.float;
        Type.complex;
        Type.Primitive "numbers.Complex";
        Type.Primitive "numbers.Number";
        Type.object_primitive ];
      [Type.integer; Type.Primitive "numbers.Integral"; Type.object_primitive];
      [Type.float; Type.Primitive "numbers.Rational"; Type.object_primitive];
      [Type.float; Type.Primitive "numbers.Real"; Type.object_primitive] ]


  let builtin_types = List.concat default_annotations |> Type.Set.of_list

  let default () =
    let order = create () in
    let handler = handler order in
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
    let type_builtin = Type.Primitive "type" in
    let type_variable = Type.Variable (Type.Variable.Unary.create "_T") in
    insert handler type_builtin;
    connect handler ~predecessor:Type.Bottom ~successor:type_builtin;
    connect
      handler
      ~predecessor:type_builtin
      ~parameters:[type_variable]
      ~successor:Type.generic_primitive;
    let typed_dictionary = Type.Primitive "TypedDictionary" in
    let non_total_typed_dictionary = Type.Primitive "NonTotalTypedDictionary" in
    let typing_mapping = Type.Primitive "typing.Mapping" in
    insert handler non_total_typed_dictionary;
    insert handler typed_dictionary;
    insert handler typing_mapping;
    connect handler ~predecessor:Type.Bottom ~successor:non_total_typed_dictionary;
    connect handler ~predecessor:non_total_typed_dictionary ~successor:typed_dictionary;
    connect
      handler
      ~predecessor:typed_dictionary
      ~parameters:[Type.string; Type.Any]
      ~successor:typing_mapping;
    connect
      handler
      ~parameters:
        [ Type.Variable (Type.Variable.Unary.create "_T");
          Type.Variable (Type.Variable.Unary.create "_T2") ]
      ~predecessor:typing_mapping
      ~successor:Type.generic_primitive;
    order
end
