(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* The classHierarchy module contains an implementation of the C3 superclass linearization
   algorithm. This algorithm (atopted in Python 2.3) is a way to determine an MRO (method resolution
   order) which maintains certain properties. More can be found on
   [wikipedia](https://en.wikipedia.org/wiki/C3_linearization) *)
open Core
open Ast
open Pyre

exception Cyclic of Type.Primitive.t

exception InconsistentMethodResolutionOrder of Type.Primitive.t

exception Untracked of string

module MethodResolutionOrderError = struct
  type t =
    | Cyclic of Type.Primitive.t
    | Inconsistent of Type.Primitive.t
  [@@deriving sexp, compare]
end

module CheckIntegrityError = struct
  type t =
    | Cyclic of Type.Primitive.t
    | Incomplete of Type.Primitive.t
  [@@deriving sexp, compare]
end

module Target = struct
  type t = {
    target: Identifier.t;
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

module Edges = struct
  type t = {
    parents: Target.t list;
    (* The instantiation of `typing.Generic` that the class inherits from but is not necessarily
       listed explicitly as a parent. It needs to be stored separately because this class may not
       take part in MRO computation. *)
    generic_base: Target.t option;
    has_placeholder_stub_parent: bool;
  }
  [@@deriving sexp, compare]
end

module type Handler = sig
  val edges : Identifier.t -> Edges.t option

  val contains : Type.Primitive.t -> bool
end

let parents_of (module Handler : Handler) target =
  Handler.edges target >>| fun { parents; _ } -> parents


let parents_and_generic_of_target (module Handler : Handler) target =
  Handler.edges target
  >>= fun { parents; generic_base; _ } ->
  match generic_base with
  | None -> Some parents
  | Some base -> Some (List.append parents [base])


let extends_placeholder_stub_of_target (module Handler : Handler) target =
  match Handler.edges target with
  | None -> false
  | Some { has_placeholder_stub_parent; _ } -> has_placeholder_stub_parent


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


let method_resolution_order_linearize_exn ~get_successors class_name =
  (* The `merge` function takes a list of "constraints" as input and return a list `L` representing
     the computed MRO that satisfy those constraints. Each "constraint" is by itself another list of
     class names indicating what orderings must be perserved in `L`.

     For example, if we pass `[["A"; "B"; "C"]; ["C", "D"]]` to `merge`, then we know that in the
     returned list, class `A` must precede class `B`, class `B` must precede class `C` (from the
     first constraint), and class `C` must precede class `D` (from the second constraint). One
     possible return list that satisfies all of these ordering constraint would be `["A"; "B"; "C";
     "D"]`.

     The intuition behind the C3 algorithm for computing `L` is straightforward: it's an iterative
     process where each iteration would inspect all constraints we currently have, and pick a
     candidate class that does not break any of them (i.e. look at the first class from each
     "constraint" and see if that class gets preceded by any other classes in other constraints),
     add the candidate to the result list, and advance to the next iteration. If at any iteration we
     could not find a valid candidate, an inconsistent MRO would be detected and we fail the entire
     process. *)
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
  (* `linearize C` computes the MRO for class `C`. The additional `visited` parameter is used to
     detect when MRO computation would run into cycles (e.g. class A inherits from class B, which in
     turn inherits from class A).

     For a given class `C` that inherits from 2 parents `A` and `B`, the MRO for class `C` must obey
     2 kinds of constraints: (1) C's MRO must satisfy all constraints of `A`'s MRO and `B`'s MRO.
     (2) In C's MRO, `A` must precede `B` because `A` comes before `B` in the base class list.

     The implementation here simply translates the two points above into a list of constraints, and
     invoke `merge` to "solve" those constraints and get a satisfying MRO. *)
  let rec linearize ~visited class_name =
    if String.Set.mem visited class_name then (
      Log.error
        "Order is cyclic:\nTrace: {%s}"
        (String.Set.to_list visited |> String.concat ~sep:", ");
      raise (Cyclic class_name));
    let visited = String.Set.add visited class_name in
    let successors =
      let create_annotation { Target.target = index; _ } = index in
      class_name |> get_successors |> Option.value ~default:[] |> List.map ~f:create_annotation
    in
    let linearized_successors = List.map successors ~f:(linearize ~visited) in
    class_name :: merge (List.append linearized_successors [successors])
  in
  linearize ~visited:String.Set.empty class_name


let method_resolution_order_linearize ~get_successors class_name =
  match method_resolution_order_linearize_exn ~get_successors class_name with
  | result -> Result.Ok result
  | exception Cyclic name -> Result.Error (MethodResolutionOrderError.Cyclic name)
  | exception InconsistentMethodResolutionOrder name ->
      Result.Error (MethodResolutionOrderError.Inconsistent name)


let immediate_parents (module Handler : Handler) class_name =
  class_name
  |> parents_of (module Handler)
  >>| List.map ~f:Target.target
  |> Option.value ~default:[]


let extends_placeholder_stub (module Handler : Handler) class_name =
  class_name |> extends_placeholder_stub_of_target (module Handler)


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
      parents_and_generic_of_target (module Handler) primitive_name
      >>= List.find ~f:(fun { Target.target; _ } ->
              [%compare.equal: Identifier.t] target generic_primitive)
      >>| (fun { Target.parameters; _ } -> parameters_to_variables parameters)
      |> Option.value ~default


let get_generic_parameters ~generic_primitive edges =
  let generic_parameters { Target.target; parameters } =
    Option.some_if ([%compare.equal: Identifier.t] generic_primitive target) parameters
  in
  List.find_map ~f:generic_parameters edges


let instantiate_successors_parameters ((module Handler : Handler) as handler) ~source ~target =
  raise_if_untracked handler target;
  match source with
  | Type.Bottom ->
      let to_any = function
        | Type.Variable.Unary _ -> [Type.Parameter.Single Type.Any]
        | ParameterVariadic _ -> [CallableParameters Undefined]
        | TupleVariadic _ -> Type.OrderedTypes.to_parameters Type.Variable.Variadic.Tuple.any
      in
      target
      |> parents_and_generic_of_target (module Handler)
      >>= get_generic_parameters ~generic_primitive
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
        Queue.enqueue worklist { Target.target = primitive; parameters };
        let rec iterate worklist =
          match Queue.dequeue worklist with
          | Some { Target.target = target_index; parameters } ->
              let instantiated_successors =
                (* If a node on the graph has Generic[_T1, _T2, ...] as a supertype and has concrete
                   parameters, all occurrences of _T1, _T2, etc. in other supertypes need to be
                   replaced with the concrete parameter corresponding to the type variable. This
                   function takes a target with concrete parameters and its supertypes, and
                   instantiates the supertypes accordingly. *)
                let get_instantiated_successors ~generic_primitive ~parameters successors =
                  let variables =
                    get_generic_parameters successors ~generic_primitive
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
                parents_and_generic_of_target (module Handler) target_index
                >>| get_instantiated_successors ~generic_primitive ~parameters
              in
              if [%compare.equal: Identifier.t] target_index target then
                match target with
                | "typing.Callable" -> Some parameters
                | _ -> instantiated_successors >>= get_generic_parameters ~generic_primitive
              else (
                instantiated_successors >>| List.iter ~f:(Queue.enqueue worklist) |> ignore;
                iterate worklist)
          | None -> None
        in
        iterate worklist
      in
      split >>= handle_split


let check_for_cycles_exn (module Handler : Handler) ~(class_names : Identifier.t list) =
  let started_from = ref Identifier.Set.empty in
  let find_cycle start =
    if not (Set.mem !started_from start) then
      let rec visit reverse_visited class_name =
        if List.mem ~equal:[%compare.equal: Identifier.t] reverse_visited class_name then (
          let trace = List.rev (class_name :: reverse_visited) in
          Log.error "Order is cyclic:\nTrace: %s" (String.concat ~sep:" -> " trace);
          raise (Cyclic class_name))
        else if not (Set.mem !started_from class_name) then (
          started_from := Set.add !started_from class_name;
          match parents_of (module Handler) class_name with
          | Some successors ->
              successors
              |> List.map ~f:Target.target
              |> List.iter ~f:(visit (class_name :: reverse_visited))
          | None -> ())
      in
      visit [] start
  in
  List.iter class_names ~f:find_cycle


let check_integrity ~class_names (module Handler : Handler) =
  let no_edges name = Handler.edges name |> Option.is_none in
  match List.find class_names ~f:no_edges with
  | Some name ->
      Log.error "Inconsistency in type order: No edges for key %s" name;
      Result.Error (CheckIntegrityError.Incomplete name)
  | None -> (
      try Result.Ok (check_for_cycles_exn ~class_names (module Handler)) with
      | Cyclic name -> Result.Error (CheckIntegrityError.Cyclic name))


let to_dot (module Handler : Handler) ~class_names =
  let class_names = List.sort ~compare:Identifier.compare class_names in
  let nodes = List.map class_names ~f:(fun class_name -> class_name, class_name) in
  let buffer = Buffer.create 10000 in
  Buffer.add_string buffer "digraph {\n";
  List.iter
    ~f:(fun (class_name, annotation) ->
      Format.asprintf "  %s[label=\"%s\"]\n" class_name annotation |> Buffer.add_string buffer)
    nodes;
  let add_edges class_name =
    parents_of (module Handler) class_name
    >>| List.sort ~compare:Target.compare
    >>| List.iter ~f:(fun { Target.target = successor; parameters } ->
            Format.asprintf "  %s -> %s" class_name successor |> Buffer.add_string buffer;
            if not (List.is_empty parameters) then
              Format.asprintf
                "[label=\"(%a)\"]"
                (Type.pp_parameters ~pp_type:Type.pp_concise)
                parameters
              |> Buffer.add_string buffer;
            Buffer.add_string buffer "\n")
    |> ignore
  in
  List.iter ~f:add_edges class_names;
  Buffer.add_string buffer "}";
  Buffer.contents buffer
