(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Pyre

exception Cyclic
exception Incomplete
exception Undefined of Type.t


module Target = struct
  type t = {
    target: int;
    parameters: Type.t list
  }
  [@@deriving compare, eq, sexp, show]


  module Set = Set.Make(struct
      type nonrec t = t
      let compare = compare
      let sexp_of_t = sexp_of_t
      let t_of_sexp = t_of_sexp
    end)


  let target { target; _ } =
    target


  let enqueue worklist actual_parameters targets =
    let enqueue { target; parameters } =
      let parameters =
        (* We currently ignore the actual type variable mapping. *)
        if List.length parameters = List.length actual_parameters then
          actual_parameters
        else
          [] in
      Queue.enqueue worklist { target; parameters } in
    List.iter targets ~f:enqueue
end


module Edge = struct
  type t = {
    source: Type.t;
    target: Type.t;
  }
  [@@deriving compare, eq, sexp, show]


  module Set = Set.Make(struct
      type nonrec t = t
      let compare = compare
      let sexp_of_t = sexp_of_t
      let t_of_sexp = t_of_sexp
    end)
end


(* `edges` mapping from type index to a set of targets.
   `indices` mapping from annotation to its vertex index.
   `annotations` inverse of `indices`. *)
type t = {
  edges: Target.t list Int.Table.t;
  backedges: (Target.t list) Int.Table.t;
  indices: int Type.Table.t;
  annotations: Type.t Int.Table.t;
}

module type Reader = sig
  type ('key, 'table) lookup

  val edges: unit -> (int, Target.t list) lookup
  val backedges: unit -> (int, Target.t list) lookup
  val indices: unit -> (Type.t, int) lookup
  val annotations: unit -> (int, Type.t) lookup

  val find: ('key, 'value) lookup -> 'key -> 'value option
  val find_unsafe: ('key, 'value) lookup -> 'key -> 'value
  val contains: ('key, 'value) lookup -> 'key -> bool
  val set: ('key, 'value) lookup -> key:'key -> data:'value -> unit

  val fold
    :  ('key, 'value) lookup
    -> init:'accumulator
    -> f:(key:'key -> data:'value -> 'accumulator -> 'accumulator)
    -> 'accumulator

  val keys: ('key, 'value) lookup -> 'key list
  val length: ('key, 'value) lookup -> int

  val show: unit -> string
end


let pp format { edges; backedges; annotations; _ } =
  let print_edge (source, targets) =
    let annotation index =
      Hashtbl.find_exn annotations index
      |> (Format.asprintf "%a" Type.pp) in
    let targets =
      let target { Target.target; parameters } =
        Format.sprintf
          "%s [%s]"
          (annotation target)
          (List.map ~f:(Format.asprintf "%a" Type.pp) parameters |> String.concat ~sep:", ") in
      targets
      |> List.map ~f:target
      |> String.concat ~sep:", " in
    Format.fprintf
      format
      "  %s -> %s\n"
      (annotation source)
      targets in
  Format.fprintf format "Edges:\n";
  List.iter ~f:print_edge (Hashtbl.to_alist edges);
  Format.fprintf format "Back-edges:\n";
  List.iter ~f:print_edge (Hashtbl.to_alist backedges)


let show order =
  Format.asprintf "%a" pp order


let reader order =
  (module struct
    type ('key, 'value) lookup = ('key, 'value) Hashtbl.t

    let edges () =
      order.edges

    let backedges () =
      order.backedges

    let indices () =
      order.indices

    let annotations () =
      order.annotations

    let find table key =
      Hashtbl.find table key

    let find_unsafe table key =
      Hashtbl.find_exn table key

    let contains table key =
      Hashtbl.mem table key

    let set table ~key ~data =
      Hashtbl.set table ~key ~data

    let fold table ~init ~f =
      Hashtbl.fold table ~init ~f

    let keys table =
      Hashtbl.keys table

    let length table =
      Hashtbl.length table

    let show () = show order
  end : Reader)


let index_of (module Reader: Reader) annotation =
  Reader.find_unsafe (Reader.indices ()) annotation


let insert (module Reader: Reader) annotation =
  let indices = Reader.indices () in
  let index = Reader.length indices in
  Reader.set indices ~key:annotation ~data:index;
  Reader.set (Reader.annotations ()) ~key:index ~data:annotation


let connect
    ?(parameters = [])
    ((module Reader: Reader) as order)
    ~predecessor
    ~successor =
  let predecessor = index_of order predecessor in
  let successor = index_of order successor in
  let bottom = index_of order Type.Bottom in

  let edges = Reader.edges () in
  let backedges = Reader.backedges () in

  (* Add edges. *)
  let connect ~edges ~predecessor ~successor =
    let successors =
      Reader.find edges predecessor
      |> Option.value ~default:[]
    in
    let target = { Target.target = successor; parameters} in
    if not (List.mem ~equal:Target.equal successors target) then
      Reader.set
        edges
        ~key:predecessor
        ~data:(target :: successors);
  in
  connect ~edges ~predecessor ~successor;
  connect ~edges:backedges ~predecessor:successor ~successor:predecessor;

  (* Remove extra back-edges to Bottom. *)
  let predecessors =
    Reader.find_unsafe backedges successor
    |> List.filter ~f:(fun { Target.target; _ } -> target <> bottom)
  in
  if not (List.is_empty predecessors) then
    Reader.set backedges ~key:successor ~data:predecessors;

  (* Remove extra in-edges from Bottom. *)
  if predecessor <> bottom then
    Reader.find edges bottom
    >>| List.filter ~f:(fun { Target.target; _ } -> target <> successor)
    >>| (fun successors -> Reader.set edges ~key:bottom ~data:successors)
    |> ignore


let find (module Reader: Reader) annotation =
  Reader.find (Reader.indices ()) annotation
  >>| Reader.find_unsafe (Reader.annotations ())


let contains (module Reader: Reader) annotation =
  Reader.contains (Reader.indices ()) annotation


let raise_if_untracked order annotation =
  if not (contains order annotation) then
    raise (Undefined annotation)


let breadth_first_fold
    ((module Reader: Reader) as order)
    ~initial
    ~f
    ~successor_indices
    annotation =
  let rec iterate ~worklist ~visited ~accumulator =
    match Queue.dequeue worklist with
    | Some ({ Target.target = index; parameters } as target) ->
        let accumulator, visited =
          if not (Set.mem visited target) then
            let successor =
              let annotation = Reader.find_unsafe (Reader.annotations ()) index in
              match annotation, parameters with
              | _, [] ->
                  annotation
              | Type.Primitive name, _ ->
                  Type.Parametric { Type.name; parameters }
              | _ ->
                  failwith (Format.asprintf "Unexpected type %a" Type.pp annotation)
            in
            Option.iter
              (successor_indices index)
              ~f:(Target.enqueue worklist parameters);
            let visited = Set.add visited target in
            f accumulator successor visited
          else
            accumulator, visited
        in
        iterate ~worklist ~visited ~accumulator
    | None ->
        accumulator in

  let worklist, visited =
    let worklist = Queue.create () in
    let primitive, parameters = Type.split annotation in
    let index = index_of order primitive in
    Option.iter (successor_indices index) ~f:(Target.enqueue worklist parameters);
    worklist, Target.Set.of_list [{ Target.target = index; parameters }]
  in

  iterate ~worklist ~visited ~accumulator:initial


let successors_fold ((module Reader: Reader) as order) ~initial ~f annotation =
  breadth_first_fold
    order
    ~initial
    ~f:(fun sofar annotation visited -> f sofar annotation, visited)
    ~successor_indices:(Reader.find (Reader.edges ()))
    annotation


let successors ((module Reader: Reader) as order) annotation =
  successors_fold
    order
    ~initial:[]
    ~f:(fun successors successor -> (successor :: successors))
    annotation
  |> List.rev


let predecessors ((module Reader: Reader) as order) annotation =
  breadth_first_fold
    order
    ~initial:[]
    ~f:(fun successors successor visited -> (successor :: successors), visited)
    ~successor_indices:(Reader.find (Reader.backedges ()))
    annotation
  |> List.rev


let greatest ((module Reader: Reader) as order) ~matches =
  let collect_matching annotations annotation visited =
    if matches annotation then
      let visited =
        (* Mark all predecessors as visited. *)
        breadth_first_fold
          order
          ~initial:Target.Set.empty
          ~f:(fun sofar _ visited -> (Set.union sofar visited), visited)
          ~successor_indices:(Reader.find (Reader.backedges ()))
          annotation
        |> Set.union visited
      in
      (annotation :: annotations), visited
    else
      annotations, visited
  in
  breadth_first_fold
    order
    ~initial:[]
    ~f:collect_matching
    ~successor_indices:(Reader.find (Reader.backedges ()))
    Type.Top


let rec less_or_equal ((module Reader: Reader) as order) ~left ~right =
  Type.equal left right ||
  match left, right with
  | _, Type.Top ->
      true
  | Type.Top, _ ->
      false

  | _, Type.Object ->
      true
  | Type.Object, _ ->
      false

  | Type.Bottom, _ ->
      true
  | _, Type.Bottom ->
      false

  | Type.Parametric _,
    Type.Parametric _ ->
      let right_primitive, right_parameters = Type.split right in
      instantiate_parameters order ~source:left ~target:right_primitive
      >>| (fun parameters ->
          List.length parameters = List.length right_parameters &&
          List.for_all2_exn
            ~f:(fun left right -> less_or_equal order ~left ~right)
            parameters
            right_parameters)
      |> Option.value ~default:false

  (* \forall i \in Union[...]. A_i <= B -> Union[...] <= B. *)
  | Type.Union left, right ->
      List.fold
        ~init:true
        ~f:(fun current left -> current && less_or_equal order ~left ~right)
        left
  (* \exists i \in Union[...]. A <= B_i ->  A <= Union[...] *)
  | left, Type.Union right ->
      List.exists ~f:(fun right -> less_or_equal order ~left ~right) right

  (* A <= B -> A <= Optional[B].*)
  | Type.Optional left, Type.Optional right ->
      less_or_equal order ~left ~right
  | _, Type.Optional parameter ->
      less_or_equal order ~left ~right:parameter
  | Type.Optional _, _ ->
      false

  (* Tuple variables are covariant. *)
  | Type.Tuple (Type.Bounded left), Type.Tuple (Type.Bounded right)
    when List.length left = List.length right ->
      List.for_all2_exn left right ~f:(fun left right -> less_or_equal order ~left ~right)
  | Type.Tuple (Type.Unbounded left), Type.Tuple (Type.Unbounded right) ->
      less_or_equal order ~left ~right
  | Type.Tuple (Type.Bounded (left :: tail)), Type.Tuple (Type.Unbounded right) ->
      List.for_all ~f:(fun element -> Type.equal element left) tail &&
      less_or_equal order ~left ~right
  | Type.Tuple _, Type.Parametric _ ->
      (* Join parameters to handle cases like `Tuple[int, int]` <= `Iterator[int]`. *)
      let parametric =
        let primitive, parameters = Type.split left in
        let parameter = List.fold ~init:Type.Bottom ~f:(join order) parameters in
        let name =
          match primitive with
          | Type.Primitive name -> name
          | _ -> Identifier.create "?"
        in
        Type.Parametric {
          Type.name;
          parameters = [parameter];
        }
      in
      less_or_equal order ~left:parametric ~right
  | Type.Tuple (Type.Unbounded parameter), Type.Primitive _ ->
      less_or_equal
        order
        ~left:(Type.Parametric { Type.name = Identifier.create "tuple"; parameters = [parameter] })
        ~right
  | Type.Tuple (Type.Bounded (left :: tail)), Type.Primitive _ ->
      let tuple_parameter = List.fold ~f:(join order) ~init:left tail in
      less_or_equal
        order
        ~left:(Type.Parametric {
            Type.name = Identifier.create "tuple";
            parameters = [tuple_parameter];
          })
        ~right
  | Type.Primitive name, Type.Tuple _ ->
      Identifier.show name = "tuple"
  | Type.Tuple _, _
  | _, Type.Tuple _ ->
      false

  (* A[...] <= B iff A <= B. *)
  | Type.Parametric _, Type.Primitive _  ->
      let parametric_primitive, _ = Type.split left in
      less_or_equal order ~left:parametric_primitive ~right

  | Type.Primitive name, Type.Parametric _ ->
      let left = Type.Parametric { Type.name; parameters = [] } in
      less_or_equal order ~left ~right

  | _ ->
      raise_if_untracked order left;
      raise_if_untracked order right;

      let worklist = Queue.create () in
      Queue.enqueue
        worklist
        { Target.target = index_of order left; parameters = [] };

      let rec iterate worklist =
        match Queue.dequeue worklist with
        | Some { Target.target; _ } ->
            if target = (index_of order right) then
              true
            else
              begin
                Option.iter
                  (Reader.find (Reader.edges ()) target)
                  ~f:(Target.enqueue worklist []);
                iterate worklist
              end
        | None ->
            false in

      iterate worklist


and least_common_successor ((module Reader: Reader) as order) ~successors left right =
  raise_if_untracked order left;
  raise_if_untracked order right;

  if Type.compare left right = 0 then
    [left]
  else
    begin
      let rec iterate left right =
        let successors sources =
          Set.fold
            ~init:Int.Set.empty
            ~f:(fun sofar index -> Set.union sofar (successors index))
            sources
        in

        let left_successors = successors (List.hd_exn left) in
        let right_successors = successors (List.hd_exn right) in

        if Set.is_empty left_successors &&
           Set.is_empty right_successors then
          []
        else
          begin
            let intersect left right =
              let collect =
                List.fold ~init:Int.Set.empty ~f:Set.union
              in
              Set.inter (collect left) (collect right)
            in

            let left = left_successors::left in
            let right = right_successors::right in

            let left_tail_right = intersect (List.tl_exn left) right in
            let left_right_tail = intersect left (List.tl_exn right) in

            if not (Set.is_empty left_tail_right) ||
               not (Set.is_empty left_right_tail) then
              Set.union left_tail_right left_right_tail
              |> Set.to_list
            else
              let left_right = intersect left right in
              if not (Set.is_empty left_right) then
                Set.to_list left_right
              else
                iterate left right
          end
      in
      iterate
        [Int.Set.of_list [index_of order left]]
        [Int.Set.of_list [index_of order right]];
      |> List.map ~f:(Reader.find_unsafe (Reader.annotations ()))
    end


and least_upper_bound ((module Reader: Reader) as order) =
  let successors index =
    match Reader.find (Reader.edges ()) index with
    | Some targets ->
        targets
        |> List.map ~f:Target.target
        |> Int.Set.of_list
    | None -> Int.Set.empty
  in
  least_common_successor order ~successors


and greatest_lower_bound ((module Reader: Reader) as order) =
  let predecessors index =
    match Reader.find (Reader.backedges ()) index with
    | Some targets ->
        targets
        |> List.map ~f:Target.target
        |> Int.Set.of_list
    | None -> Int.Set.empty
  in
  least_common_successor order ~successors:predecessors


and join ((module Reader: Reader) as order) left right =
  if Type.equal left right then
    left
  else
    match left, right with
    | Type.Top, _
    | _, Type.Top ->
        Type.Top

    | Type.Object, _
    | _, Type.Object ->
        Type.Object

    | Type.Bottom, other
    | other, Type.Bottom ->
        other

    (* n: A_n = B_n -> Union[A_i] <= Union[B_i]. *)
    | (Type.Union left), (Type.Union right) ->
        Type.union (left @ right)
    | (Type.Union elements as union), other
    | other, (Type.Union elements as union) ->
        if less_or_equal order ~left:other ~right:union then
          union
        else
          (match other with
           | Type.Optional element ->
               Type.Optional (Type.union (element :: elements))
           | _ ->
               List.map ~f:(join order other) elements
               |> List.fold ~f:(join order) ~init:Type.Bottom)

    | Type.Parametric _, Type.Parametric _ ->
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
          with Undefined _ ->
            Type.Object
        in
        if Reader.contains (Reader.indices ()) target then
          let left_parameters = instantiate_parameters order ~source:left ~target in
          let right_parameters = instantiate_parameters order ~source:right ~target in
          let parameters =
            match left_parameters, right_parameters with
            | Some left, Some right when List.length left = List.length right ->
                Some (List.map2_exn ~f:(join order) left right)
            | _ ->
                None
          in
          (match target, parameters with
           | Type.Primitive name, Some parameters ->
               Type.Parametric { Type.name; parameters }
           | _ ->
               Type.Object)

        else
          Type.Object

    (* Special case joins of optional collections with their uninstantated counterparts. *)
    | Type.Parametric ({ Type.parameters = [Type.Bottom]; _ } as other),
      Type.Optional (Type.Parametric ({ Type.parameters = [parameter]; _ } as collection))
    | Type.Optional (Type.Parametric ({ Type.parameters = [parameter]; _ } as collection)),
      Type.Parametric ({ Type.parameters = [Type.Bottom]; _ } as other)
      when Identifier.equal other.Type.name collection.Type.name ->
        Type.Parametric { other with Type.parameters = [parameter] }

    (* A <= B -> lub(A, Optional[B]) = Optional[B]. *)
    | other, Type.Optional parameter
    | Type.Optional parameter, other ->
        Type.optional (join order other parameter)

    (* Tuple variables are covariant. *)
    | Type.Tuple (Type.Bounded left), Type.Tuple (Type.Bounded right)
      when List.length left = List.length right ->
        List.map2_exn left right ~f:(join order)
        |> Type.tuple
    | Type.Tuple (Type.Unbounded left), Type.Tuple (Type.Unbounded right) ->
        Type.Tuple (Type.Unbounded (join order left right))
    | Type.Tuple (Type.Bounded (left :: tail)), Type.Tuple (Type.Unbounded right)
    | Type.Tuple (Type.Unbounded right), Type.Tuple (Type.Bounded (left :: tail))
      when List.for_all ~f:(fun element -> Type.equal element left) tail &&
           less_or_equal order ~left ~right ->
        Type.Tuple (Type.Unbounded right)
    | (Type.Tuple _ as tuple), (Type.Parametric _ as parametric)
    | (Type.Parametric _ as parametric), (Type.Tuple _ as tuple) ->
        if less_or_equal order ~left:tuple ~right:parametric then
          parametric
        else
          Type.Object
    | Type.Tuple _, _
    | _, Type.Tuple _ ->
        Type.Object

    | Type.Parametric _ , Type.Primitive _
    | Type.Primitive _, Type.Parametric _ ->
        if less_or_equal order ~left ~right then
          right
        else if less_or_equal order ~left:right ~right:left then
          left
        else
          Type.Object
    | _ ->
        let joined =
          least_upper_bound order left right
          |> List.hd_exn
        in
        if Type.equal joined left || Type.equal joined right then
          joined
        else
          Type.union [left; right]


and meet order left right =
  if Type.equal left right then
    left
  else
    match left, right with
    | Type.Top, other
    | other, Type.Top ->
        other

    | Type.Bottom, _
    | _, Type.Bottom ->
        Type.Bottom

    | (Type.Union left), (Type.Union right) ->
        let union =
          Set.inter (Type.Set.of_list left) (Type.Set.of_list right)
          |> Set.to_list
        in
        Type.union union
    | (Type.Union elements as union), other
    | other, (Type.Union elements as union) ->
        if less_or_equal order ~left:other ~right:union then
          other
        else
          List.map ~f:(meet order other) elements
          |> List.fold ~f:(meet order) ~init:Type.Top

    | Type.Parametric _, Type.Parametric _ ->
        let left_primitive, left_parameters = Type.split left in
        let right_primitive, right_parameters = Type.split right in
        let primitive = meet order left_primitive right_primitive in
        let parameters =
          (* TODO(T22785171): take type variables into account. *)
          if List.length left_parameters = List.length right_parameters then
            Some (List.map2_exn ~f:(meet order) left_parameters right_parameters)
          else
            None
        in
        (match primitive, parameters with
         | Type.Primitive name, Some parameters ->
             Type.Parametric { Type.name; parameters }
         | _ ->
             Type.Bottom)

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
        List.map2_exn left right ~f:(meet order)
        |> Type.tuple
    | Type.Tuple (Type.Unbounded left), Type.Tuple (Type.Unbounded right) ->
        Type.Tuple (Type.Unbounded (meet order left right))
    | Type.Tuple (Type.Bounded (left :: tail)), Type.Tuple (Type.Unbounded right)
    | Type.Tuple (Type.Unbounded right), Type.Tuple (Type.Bounded (left :: tail))
      when List.for_all ~f:(fun element -> Type.equal element left) tail &&
           less_or_equal order ~left ~right ->
        Type.Tuple (Type.Unbounded left)  (* My brain hurts... *)
    | (Type.Tuple _ as tuple), (Type.Parametric _ as parametric)
    | (Type.Parametric _ as parametric), (Type.Tuple _ as tuple) ->
        if less_or_equal order ~left:tuple ~right:parametric then
          tuple
        else
          Type.Bottom
    | Type.Tuple _, _
    | _, Type.Tuple _ ->
        Type.Bottom

    | Type.Parametric _ , Type.Primitive _
    | Type.Primitive _, Type.Parametric _ ->
        if less_or_equal order ~left ~right then
          left
        else if less_or_equal order ~left:right ~right:left then
          right
        else
          Type.Bottom

    | _ ->
        greatest_lower_bound order left right
        |> List.hd_exn


and instantiate_parameters
    ((module Reader: Reader) as order)
    ~source
    ~target =
  let primitive, parameters = Type.split source in

  raise_if_untracked order primitive;
  raise_if_untracked order target;

  let generic_index = Reader.find (Reader.indices ()) Type.generic in

  (* If a node on the graph has Generic[_T1, _T2, ...] as a supertype and has
     concrete parameters, all occurences of _T1, _T2, etc. in other supertypes
     need to be replaced with the concrete parameter corresponding to the type
     variable. This function takes a target with concrete parameters and its supertypes,
     and instantiates the supertypes accordingly.
  *)
  let get_instantiated_successors { Target.parameters; _ } successors =
    let generic_parameters =
      let generic_parameters { Target.target; parameters } =
        if Some target = generic_index then
          Some parameters
        else
          None
      in
      List.find_map ~f:generic_parameters successors
    in
    generic_parameters
    >>| (fun type_variables ->
        if List.length type_variables = List.length parameters then
          let type_variables =
            List.zip_exn type_variables parameters
            |> Type.Map.of_alist_reduce ~f:(fun first _ -> first)
          in
          let instantiate_parameters {Target.target; parameters } =
            let rec instantiate_type_variables annotation =
              match annotation with
              | Type.Bottom | Type.Object | Type.Top ->
                  annotation

              | Type.Variable _ | Type.Primitive _ ->
                  (match Map.find type_variables annotation with
                   | Some instantiated -> instantiated
                   | None -> annotation)

              | Type.Parametric { Type.name; parameters } ->
                  Type.Parametric {
                    Type.name;
                    parameters = List.map ~f:instantiate_type_variables parameters
                  }

              | Type.Tuple (Type.Bounded list) ->
                  Type.Tuple (Type.Bounded (List.map ~f:instantiate_type_variables list))

              | Type.Tuple (Type.Unbounded annotation) ->
                  Type.Tuple (Type.Unbounded (instantiate_type_variables annotation))

              | Type.Union union ->
                  Type.Union (List.map ~f:instantiate_type_variables union)

              | Type.Optional optional ->
                  Type.Optional (instantiate_type_variables optional)
            in
            { Target.target; parameters = List.map ~f:instantiate_type_variables parameters }
          in
          List.map ~f:instantiate_parameters successors
        else
          successors)
    |> Option.value ~default:successors
  in

  let worklist = Queue.create () in
  Queue.enqueue
    worklist
    { Target.target = index_of order primitive; parameters };
  let rec iterate worklist =
    match Queue.dequeue worklist with
    | Some { Target.target = target_index; parameters } ->
        if target_index = index_of order target then
          Some parameters
        else
          begin
            Reader.find (Reader.edges ()) target_index
            >>| get_instantiated_successors { Target.target = target_index; parameters }
            >>| List.iter ~f:(Queue.enqueue worklist)
            |> ignore;
            iterate worklist
          end
    | None ->
        None
  in
  iterate worklist


let widen order ~widening_threshold ~previous ~next ~iteration =
  if iteration > widening_threshold then
    Type.Top
  else
    join order previous next


let complete ((module Reader: Reader) as order) ~bottom ~top =
  let index_of annotation = Reader.find_unsafe (Reader.indices ()) annotation in
  let top_index = index_of top in
  let visited = ref (Int.Set.of_list [top_index]) in

  let rec visit index =
    let annotation = Reader.find_unsafe (Reader.annotations ()) index in
    if not (Set.mem !visited index) &&
       not (less_or_equal order ~left:top ~right:annotation) then
      begin
        visited := Set.add !visited index;
        match Reader.find (Reader.edges ()) index with
        | Some targets when List.length targets > 0 ->
            targets
            |> List.map ~f:Target.target
            |> List.iter ~f:visit
        | _ ->
            connect order ~predecessor:annotation ~successor:top
      end in
  visit (index_of  bottom)


let check_integrity (module Reader: Reader) =
  (* Check `Top` and `Bottom`. *)
  let contains annotation = Reader.contains (Reader.indices ()) annotation in
  if not (contains Type.Bottom && contains Type.Top) then
    begin
      Log.error "Order is missing either Bottom or Top:\n%s" (Reader.show ());
      raise Incomplete
    end;

  (* Check for cycles. *)
  let started_from = ref Int.Set.empty in
  let find_cycle start =
    if not (Set.mem !started_from start) then
      begin
        let rec visit reverse_visited index =
          if List.mem ~equal:Int.equal reverse_visited index then
            begin
              let trace =
                List.rev_map
                  ~f:(Reader.find_unsafe (Reader.annotations ()))
                  (index :: reverse_visited)
                |> List.map ~f:(Format.asprintf "%a" Type.pp)
                |> String.concat ~sep:" -> " in
              Log.error
                "Order is cyclic:\n%s\nTrace: %s"
                (Reader.show ())
                trace;
              raise Cyclic
            end
          else if not (Set.mem !started_from index) then
            begin
              started_from := Set.add !started_from index;
              match Reader.find (Reader.edges ()) index with
              | Some successors ->
                  successors
                  |> List.map ~f:Target.target
                  |> List.iter ~f:(visit (index :: reverse_visited))
              | None ->
                  ()
            end in
        visit [] start
      end in
  Reader.keys (Reader.edges ())
  |> List.iter ~f:find_cycle;

  (* Check that backedges are complete. *)
  let check_inverse ~edges ~backedges =
    let check_backedge index =
      let check_backedge { Target.target; _ } =
        let has_backedge =
          match Reader.find backedges target with
          | Some targets ->
              List.exists ~f:(fun { Target.target; _ } -> target = index) targets
          | None ->
              false
        in
        if not has_backedge then
          begin
            Log.error
              "No back-edge found for %a -> %a"
              Type.pp (Reader.find_unsafe (Reader.annotations ()) index)
              Type.pp (Reader.find_unsafe (Reader.annotations ()) target);
            raise Incomplete
          end
      in
      List.iter ~f:check_backedge (Reader.find_unsafe edges index)
    in
    Reader.keys edges
    |> List.iter ~f:check_backedge
  in
  check_inverse ~edges:(Reader.edges ()) ~backedges:(Reader.backedges ());
  check_inverse ~edges:(Reader.backedges ()) ~backedges:(Reader.edges ())


module Builder = struct
  let create () =
    {
      edges = Int.Table.create ();
      backedges = Int.Table.create ();
      indices = Type.Table.create ();
      annotations = Int.Table.create ();
    }


  let copy { edges; backedges; indices; annotations } =
    {
      edges = Hashtbl.copy edges;
      backedges = Hashtbl.copy backedges;
      indices = Hashtbl.copy indices;
      annotations = Hashtbl.copy annotations;
    }


  let default () =
    let order = create () in
    let reader = reader order in

    insert reader Type.Bottom;
    insert reader Type.Top;

    (* Object and Void. *)
    insert reader Type.Object;
    insert reader Type.void;
    connect reader ~predecessor:Type.Bottom ~successor:Type.void;
    connect reader ~predecessor:Type.void ~successor:Type.Object;
    connect reader ~predecessor:Type.Object ~successor:Type.Top;

    (* Numerical hierarchy. *)
    insert reader Type.integer;
    insert reader Type.float;
    insert reader Type.complex;
    connect reader ~predecessor:Type.Bottom ~successor:Type.integer;
    connect reader ~predecessor:Type.integer ~successor:Type.float;
    connect reader ~predecessor:Type.float ~successor:Type.complex;
    connect reader ~predecessor:Type.complex ~successor:Type.Object;

    order
end
