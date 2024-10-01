(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* The classHierarchy module contains an implementation of the C3 superclass linearization
   algorithm. This algorithm (adopted in Python 2.3) is a way to determine a method resolution order
   (MRO) which maintains certain properties. More can be found on
   [wikipedia](https://en.wikipedia.org/wiki/C3_linearization)

   Note that there are two blocks of code in this module that convert type *arguments* to type
   *parameters*. This is because the pre-PEP-695 syntax for generic classes indicates the type
   *parameters* of a class as *arguments* to its generic base class. That is the only reason such
   coercion makes sense, in general the two concepts are not interchangeable (parameters are the
   placeholders on a class treated as a type constructor, arguments are the actual types provided in
   a concrete annotation that specializes the generic). *)
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
    arguments: Type.Argument.t list;
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

    let to_string ~f set = Core.Set.to_list set |> List.to_string ~f

    let filter = Set.filter

    let is_empty = Set.is_empty

    let exists = Set.exists

    let iter = Set.iter

    let mem = Set.mem

    let fold = Set.fold

    let add = Set.add
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

module GenericMetadata = struct
  (* This data stores information about whether the class is generic, and if so what the type
     parameters are.

     We explicitly track `InvalidGenericBase` in order to handle the case where code tries to
     explicitly mark a class as inheriting from `Generic[C]` or `Protocol[C]` for some concrete type
     `C`... this is needed in part because `missingFromStubs.ml` is using `Generic[Any]` as a base
     class of many of made-up classes for typing special forms, and Pyre is relying on the fact that
     when `generic_parameters_as_variables` gets called with a non-None default parameter we always
     return None in these cases.

     TODO(T199653412): we should clean this up, which may involve nontrivial downstream changes *)
  type t =
    | NotGeneric
    | GenericBase of Type.GenericParameter.t list
    | InvalidGenericBase
  [@@deriving sexp, show, compare]
end

module Edges = struct
  type t = {
    parents: Target.t list;
    generic_metadata: GenericMetadata.t;
  }
  [@@deriving sexp, compare]
end

let generic_primitive = "typing.Generic"

module type Handler = sig
  val edges : Identifier.t -> Edges.t option

  val contains : Type.Primitive.t -> bool
end

let parents_of (module Handler : Handler) target =
  Handler.edges target >>| fun { parents; _ } -> parents


let parents_and_generic_of_target (module Handler : Handler) target =
  Handler.edges target
  >>= fun { parents; generic_metadata; _ } ->
  match generic_metadata with
  | GenericMetadata.NotGeneric
  | GenericMetadata.InvalidGenericBase ->
      Some parents
  | GenericMetadata.GenericBase parameters ->
      Some
        (List.append
           parents
           [
             {
               target = generic_primitive;
               arguments =
                 (let parameter_to_argument parameter =
                    parameter |> Type.GenericParameter.to_variable |> Type.Variable.to_argument
                  in
                  List.map parameters ~f:parameter_to_argument);
             };
           ])


let is_instantiated (module Handler : Handler) annotation =
  let is_invalid = function
    | Type.Variable { constraints = Type.Record.TypeVarConstraints.Unconstrained; _ } -> true
    | Type.Primitive name
    | Type.Parametric { name; _ } ->
        not (Handler.contains name)
    | _ -> false
  in
  not (Type.exists ~predicate:is_invalid annotation)


let raise_if_untracked (module Handler : Handler) annotation =
  if not (Handler.contains annotation) then
    raise (Untracked annotation)


let method_resolution_order_linearize_exn ~get_parents class_name =
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
    if Set.mem visited class_name then (
      Log.error "Order is cyclic:\nTrace: {%s}" (Set.to_list visited |> String.concat ~sep:", ");
      raise (Cyclic class_name));
    let visited = Set.add visited class_name in
    let immediate_parents =
      let get_class_name { Target.target; _ } = target in
      class_name |> get_parents |> Option.value ~default:[] |> List.map ~f:get_class_name
    in
    let linearized_successors = List.map immediate_parents ~f:(linearize ~visited) in
    class_name :: merge (List.append linearized_successors [immediate_parents])
  in
  linearize ~visited:String.Set.empty class_name


let method_resolution_order_linearize ~get_parents class_name =
  match method_resolution_order_linearize_exn ~get_parents class_name with
  | result -> Result.Ok result
  | exception Cyclic name -> Result.Error (MethodResolutionOrderError.Cyclic name)
  | exception InconsistentMethodResolutionOrder name ->
      Result.Error (MethodResolutionOrderError.Inconsistent name)


let immediate_parents (module Handler : Handler) class_name =
  class_name
  |> parents_of (module Handler)
  >>| List.map ~f:Target.target
  |> Option.value ~default:[]


let parameters_to_variables parameters =
  List.map parameters ~f:Type.Argument.to_variable |> Option.all


let generic_parameters ?(empty_for_nongeneric = false) (module Handler : Handler) = function
  | "type"
    (* Despite what typeshed says, typing.Type is covariant:
       https://www.python.org/dev/peps/pep-0484/#the-type-of-class-objects *)
  | "typing.Callable" ->
      (* This is not the "real" typing.Callable. We are just proxying to the Callable instance in
         the type order here. *)
      Some
        [
          Type.GenericParameter.GpTypeVar
            {
              name = "_T_meta";
              variance = Type.Record.PreInferenceVariance.P_Covariant;
              constraints = Type.Record.TypeVarConstraints.Unconstrained;
            };
        ]
  | primitive_name -> (
      Handler.edges primitive_name
      >>| (fun { generic_metadata; _ } -> generic_metadata)
      |> function
      | None
      | Some GenericMetadata.NotGeneric ->
          (* Fall back to the default both for failed lookups and for non-generic classes*)
          if empty_for_nongeneric then
            Some []
          else
            None
      | Some GenericMetadata.InvalidGenericBase ->
          (* Don't fall back if there's an invalid generic base, return None igonoring `default` *)
          None
      | Some (GenericMetadata.GenericBase parameters) -> Some parameters)


let generic_parameters_as_variables
    ?(empty_for_nongeneric = false)
    (module Handler : Handler)
    type_name
  =
  match generic_parameters ~empty_for_nongeneric (module Handler) type_name with
  | Some parameters -> Some (List.map ~f:Type.GenericParameter.to_variable parameters)
  | None -> None


let get_generic_parameters ~generic_primitive edges =
  let generic_parameters { Target.target; arguments } =
    Option.some_if ([%compare.equal: Identifier.t] generic_primitive target) arguments
  in
  List.find_map ~f:generic_parameters edges


let instantiate_successors_parameters ((module Handler : Handler) as handler) ~source ~target =
  raise_if_untracked handler target;
  match source with
  | Type.Bottom ->
      let to_any = function
        | Type.Variable.TypeVarVariable _ -> [Type.Argument.Single Type.Any]
        | ParamSpecVariable _ -> [CallableParameters Undefined]
        | TypeVarTupleVariable _ -> Type.OrderedTypes.to_arguments Type.Variable.TypeVarTuple.any
      in
      target
      |> parents_and_generic_of_target (module Handler)
      >>= get_generic_parameters ~generic_primitive
      >>= parameters_to_variables
      >>| List.concat_map ~f:to_any
  | _ ->
      let split =
        match Type.split source with
        | Primitive primitive, _ when not (Handler.contains primitive) -> None
        | Primitive "tuple", [Type.Argument.Single argument] ->
            Some ("tuple", [Type.Argument.Single (Type.weaken_literals argument)])
        | Primitive primitive, arguments -> Some (primitive, arguments)
        | _ ->
            (* We can only propagate from those that actually split off a primitive *)
            None
      in
      let handle_split (primitive, arguments) =
        let worklist = Queue.create () in
        Queue.enqueue worklist { Target.target = primitive; arguments };
        let rec iterate worklist =
          match Queue.dequeue worklist with
          | Some { Target.target = target_index; arguments } ->
              let instantiated_successors =
                (* If a node on the graph has Generic[_T1, _T2, ...] as a supertype and has concrete
                   arguments, all occurrences of _T1, _T2, etc. in other supertypes need to be
                   replaced with the concrete argument corresponding to the type variable. This
                   function takes a target with concrete arguments and its supertypes, and
                   instantiates the supertypes accordingly. *)
                let get_instantiated_successors ~generic_primitive ~arguments successors =
                  let variables =
                    get_generic_parameters successors ~generic_primitive
                    >>= parameters_to_variables
                    |> Option.value ~default:[]
                  in
                  let replacement =
                    let to_any = function
                      | Type.Variable.TypeVarVariable variable ->
                          Type.Variable.TypeVarPair (variable, Type.Any)
                      | ParamSpecVariable variable ->
                          Type.Variable.ParamSpecPair (variable, Undefined)
                      | TypeVarTupleVariable variadic ->
                          Type.Variable.TypeVarTuplePair (variadic, Type.Variable.TypeVarTuple.any)
                    in
                    Type.Variable.zip_variables_with_arguments ~arguments variables
                    |> Option.value ~default:(List.map ~f:to_any variables)
                    |> TypeConstraints.Solution.create
                  in
                  let instantiate_parameters { Target.target; arguments } =
                    let instantiate = function
                      | Type.Argument.Single single ->
                          [
                            Type.Argument.Single
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
                          Type.OrderedTypes.to_arguments
                            (TypeConstraints.Solution.instantiate_ordered_types
                               replacement
                               (Concatenation
                                  (Type.OrderedTypes.Concatenation.create_from_unpackable
                                     unpackable)))
                    in
                    { Target.target; arguments = List.concat_map arguments ~f:instantiate }
                  in
                  List.map successors ~f:instantiate_parameters
                in
                parents_and_generic_of_target (module Handler) target_index
                >>| get_instantiated_successors ~generic_primitive ~arguments
              in
              if [%compare.equal: Identifier.t] target_index target then
                match target with
                | "typing.Callable" -> Some arguments
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
    >>| List.iter ~f:(fun { Target.target = successor; arguments } ->
            Format.asprintf "  %s -> %s" class_name successor |> Buffer.add_string buffer;
            if not (List.is_empty arguments) then
              Format.asprintf "[label=\"(%a)\"]" Type.Argument.pp_list arguments
              |> Buffer.add_string buffer;
            Buffer.add_string buffer "\n")
    |> ignore
  in
  List.iter ~f:add_edges class_names;
  Buffer.add_string buffer "}";
  Buffer.contents buffer
