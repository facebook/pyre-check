(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Pyre

module Callable = Type.Callable

exception Cyclic
exception Incomplete
exception Untracked of Type.t
exception InconsistentMethodResolutionOrder of Type.t


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

module type Handler = sig
  type ('key, 'table) lookup

  val edges: unit -> (int, Target.t list) lookup
  val backedges: unit -> (int, Target.t list) lookup
  val indices: unit -> (Type.t, int) lookup
  val annotations: unit -> (int, Type.t) lookup

  val find: ('key, 'value) lookup -> 'key -> 'value option
  val find_unsafe: ('key, 'value) lookup -> 'key -> 'value
  val contains: ('key, 'value) lookup -> 'key -> bool
  val set: ('key, 'value) lookup -> key:'key -> data:'value -> unit

  val add_key: int -> unit
  val keys: unit -> int list

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


let handler order =
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

    let add_key _ = ()

    let keys () =
      Hashtbl.keys order.annotations

    let length table =
      Hashtbl.length table

    let show () = show order

  end : Handler)


let index_of (module Handler: Handler) annotation =
  Handler.find_unsafe (Handler.indices ()) annotation


let insert (module Handler: Handler) annotation =
  if not (Handler.contains (Handler.indices ()) annotation) then
    begin
      let indices = Handler.indices () in
      let index = Handler.length indices in
      Handler.add_key index;
      Handler.set indices ~key:annotation ~data:index;
      Handler.set (Handler.annotations ()) ~key:index ~data:annotation;
      Handler.set (Handler.edges ()) ~key:index ~data:[];
      Handler.set (Handler.backedges ()) ~key:index ~data:[]
    end


let connect
    ?(parameters = [])
    ?(add_backedge = true)
    ((module Handler: Handler) as order)
    ~predecessor
    ~successor =
  if not (Handler.contains (Handler.indices ()) predecessor) ||
     not (Handler.contains (Handler.indices ()) successor) then
    Statistics.event
      ~name:"invalid type order connection"
      ~integers:[]
      ~normals:[
        "Predecessor", Type.show predecessor;
        "Successor", Type.show successor;
      ]
      ()
  else
    begin
      let predecessor = index_of order predecessor in
      let successor = index_of order successor in

      let edges = Handler.edges () in
      let backedges = Handler.backedges () in

      let connect ~edges ~predecessor ~successor =
        (* Add edges. *)
        let successors =
          Handler.find edges predecessor
          |> Option.value ~default:[]
        in
        let target = { Target.target = successor; parameters} in
        Handler.set
          edges
          ~key:predecessor
          ~data:(target :: successors)
      in
      connect ~edges ~predecessor ~successor;
      if add_backedge then
        connect ~edges:backedges ~predecessor:successor ~successor:predecessor
    end


let disconnect_successors ((module Handler: Handler) as order) annotation =
  if not (Handler.contains (Handler.indices ()) annotation) then
    Statistics.event
      ~name:"invalid type order disconnection"
      ~integers:[]
      ~normals:[
        "Annotation", Type.show annotation;
      ]
      ()
  else
    begin
      let key = index_of order annotation in

      let edges = Handler.edges () in
      let backedges = Handler.backedges () in
      let remove_backedge ~predecessor ~successor =
        Handler.find backedges successor
        >>| (fun current_predecessors ->
            let new_predecessors =
              List.filter
                ~f:(fun { Target.target; _ } -> target <> predecessor)
                current_predecessors
            in
            Handler.set backedges ~key:successor ~data:new_predecessors)
        |> ignore
      in

      Handler.find edges key
      >>| (fun successors ->
          Handler.set edges ~key ~data:[];
          (* Remove corresponding backedges. *)
          List.iter
            ~f:(fun { Target.target = successor; _ } -> remove_backedge ~predecessor:key ~successor)
            successors
        )
      |> ignore
    end


let contains (module Handler: Handler) annotation =
  Handler.contains (Handler.indices ()) annotation


let is_instantiated (module Handler: Handler) annotation =
  let is_invalid = function
    | Type.Variable { constraints = Type.Unconstrained; _ } -> true
    | Type.Primitive name ->
        not (Handler.contains (Handler.indices ()) (Type.Primitive name))
    | _ ->
        false
  in
  not (Type.exists ~predicate:is_invalid annotation)


let raise_if_untracked order annotation =
  if not (contains order annotation) then
    raise (Untracked annotation)


let method_resolution_order_linearize
    ((module Handler: Handler) as order)
    ~get_successors
    annotation =
  let rec merge = function
    | [] ->
        []
    | single_linearized_parent :: [] ->
        single_linearized_parent
    | linearized_successors ->
        let find_valid_head linearizations =
          let is_valid_head head =
            let not_in_tail target = function
              | [] -> true
              | _ :: tail -> not (List.exists ~f:(fun element -> element = target) tail)
            in
            List.for_all ~f:(not_in_tail head) linearizations
          in
          linearizations
          |> List.filter_map ~f:List.hd
          |> List.find ~f:(is_valid_head)
          |> (function
              | Some head -> head
              | None -> raise (InconsistentMethodResolutionOrder annotation))
        in
        let strip_head head = function
          | [] -> None
          | successor_head :: [] when successor_head = head -> None
          | successor_head :: tail when successor_head = head -> Some tail
          | successor -> Some successor
        in
        let head = find_valid_head linearized_successors in
        let linearized_successors = List.filter_map ~f:(strip_head head) linearized_successors in
        head :: merge linearized_successors
  in
  let rec linearize annotation =
    let primitive, actual_parameters = Type.split annotation in
    let linearized_successors =
      let create_annotation { Target.target = index; parameters } =
        let annotation = Handler.find_unsafe (Handler.annotations ()) index in
        let parameters =
          (* We currently ignore the actual type variable mapping. *)
          if List.length parameters = List.length actual_parameters then
            actual_parameters
          else
            []
        in
        match annotation, parameters with
        | _, [] ->
            annotation
        | Type.Primitive name, _ ->
            Type.Parametric { name; parameters }
        | _ ->
            failwith (Format.asprintf "Unexpected type %a" Type.pp annotation)
      in
      index_of order primitive
      |> get_successors
      |> Option.value ~default:[]
      |> List.map ~f:create_annotation
      |> List.map ~f:linearize
    in
    annotation :: merge linearized_successors
  in
  linearize annotation


let successors ((module Handler: Handler) as order) annotation =
  let linearization =
    method_resolution_order_linearize
      ~get_successors:(Handler.find (Handler.edges ()))
      order
      annotation
  in
  match linearization with
  | _ :: successors -> successors
  | [] -> []


let breadth_first_fold
    ((module Handler: Handler) as order)
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
              let annotation = Handler.find_unsafe (Handler.annotations ()) index in
              match annotation, parameters with
              | _, [] ->
                  annotation
              | Type.Primitive name, _ ->
                  Type.Parametric { name; parameters }
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


let predecessors ((module Handler: Handler) as order) annotation =
  breadth_first_fold
    order
    ~initial:[]
    ~f:(fun successors successor visited -> (successor :: successors), visited)
    ~successor_indices:(Handler.find (Handler.backedges ()))
    annotation
  |> List.rev


let greatest ((module Handler: Handler) as order) ~matches =
  let collect_matching annotations annotation visited =
    if matches annotation then
      let visited =
        (* Mark all predecessors as visited. *)
        breadth_first_fold
          order
          ~initial:Target.Set.empty
          ~f:(fun sofar _ visited -> (Set.union sofar visited), visited)
          ~successor_indices:(Handler.find (Handler.backedges ()))
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
    ~successor_indices:(Handler.find (Handler.backedges ()))
    Type.Top


let variables (module Handler: Handler) annotation =
  match Type.split annotation with
  | left, _ when String.equal (Type.show left) "type" ->
      (* Despite what typeshed says, typing.Type is covariant:
         https://www.python.org/dev/peps/pep-0484/#the-type-of-class-objects *)
      Some [Type.variable ~variance:Type.Covariant "_T_meta"]
  | left, _ when String.equal (Type.show left) "typing.Callable" ->
      (* This is not the "real" typing.Callable. We are just
         proxying to the Callable instance in the type order here. *)
      Some [Type.variable ~variance:Type.Covariant "_T_meta"]
  | _ ->
      let primitive = Type.split annotation |> fst in
      Handler.find (Handler.indices ()) Type.generic
      >>= fun generic_index ->
      Handler.find (Handler.indices ()) primitive
      >>= fun primitive_index ->
      Handler.find (Handler.edges ()) primitive_index
      >>= List.find ~f:(fun { Target.target; _ } -> target = generic_index)
      >>| fun { Target.parameters; _ } -> parameters


let rec less_or_equal ((module Handler: Handler) as order) ~left ~right =
  Type.equal left right ||
  match left, right with
  | other, Type.Top ->
      not (Type.exists other ~predicate:(fun annotation -> Type.equal annotation Type.undeclared))
  | Type.Top, _ ->
      false

  | _, Type.Deleted ->
      true
  | Type.Deleted, _ ->
      false

  | _, Type.Variable { constraints = Type.Unconstrained; _ } ->
      true
  | Type.Variable { constraints = Type.Unconstrained; _ },
    (Type.Primitive _ | Type.Parametric _) ->
      false
  | _, Type.Object ->
      true
  | Type.Object, _ ->
      false

  | Type.Bottom, _ ->
      true
  | _, Type.Bottom ->
      false

  | Type.Variable { constraints = Type.Bound left; _ }, right ->
      less_or_equal order ~left ~right
  | _, Type.Variable { constraints = Type.Bound _; _ } ->
      false
  | Type.Variable { constraints = Type.Explicit left; _ }, right ->
      less_or_equal order ~left:(Type.union left) ~right
  | left, Type.Variable { constraints = Type.Explicit right; _ } ->
      less_or_equal order ~left ~right:(Type.union right)


  | Type.Parametric { name = left_name; _ },
    Type.Parametric _ ->
      let compare_parameter left right variable =
        match left, right, variable with
        | Type.Bottom, _, _ ->
            (* T[Bottom] is a subtype of T[_T2], for any _T2 and regardless of its variance. *)
            true
        | _ , Type.Object, _ ->
            (* T[_T2] is a subtype of T[Any], for any _T2 and regardless of its variance. *)
            true
        | _ , _ , Type.Variable { variance = Covariant; _ } ->
            less_or_equal order ~left ~right
        | _ , _ , Type.Variable { variance = Contravariant; _ } ->
            less_or_equal order ~left:right ~right:left
        | _, _, Type.Variable { variance = Invariant; _ } ->
            less_or_equal order ~left ~right &&
            less_or_equal order ~left:right ~right:left
        | _ ->
            Log.warning "Cannot compare %a and %a, not a variable: %a"
              Type.pp left Type.pp right Type.pp variable;
            false
      in

      let left_primitive, left_parameters = Type.split left in
      let right_primitive, right_parameters = Type.split right in
      raise_if_untracked order left_primitive;
      raise_if_untracked order right_primitive;
      let generic_index = Handler.find (Handler.indices ()) Type.generic in
      let left_variables = variables order left in

      if Type.equal left_primitive right_primitive then
        (* Left and right primitives coincide, a simple parameter comparison is enough. *)
        let compare_parameters ~left ~right variables =
          List.length variables = List.length left &&
          List.length variables = List.length right &&
          List.map3_exn ~f:compare_parameter left right variables
          |> List.for_all ~f:Fn.id
        in
        left_variables
        >>| compare_parameters ~left:left_parameters ~right:right_parameters
        |> Option.value ~default:false
      else
        (* Perform one step in all appropriate directions. *)
        let parametric ~primitive ~parameters =
          match primitive with
          | Type.Primitive name ->
              Some (Type.Parametric { name; parameters })
          | _ ->
              None
        in

        (* 1. Go one level down in the class hierarchy and try from there. *)
        let step_into_subclasses () =
          let target_to_parametric { Target.target; parameters } =
            Handler.find (Handler.annotations ()) target
            >>= fun annotation ->
            Type.split annotation
            |> fst
            |> fun primitive -> parametric ~primitive ~parameters
          in
          let successors =
            let left_index = index_of order left_primitive in
            Handler.find (Handler.edges ()) left_index
            |> Option.value ~default:[]
          in
          get_instantiated_successors ~generic_index ~parameters:left_parameters successors
          |> List.filter_map ~f:target_to_parametric
          |> List.exists ~f:(fun left -> less_or_equal order ~left ~right)
        in

        (* 2. Try and replace all parameters, one at a time, to get closer to the destination. *)
        let replace_parameters_with_destination left_variables =
          (* Mapping from a variable in `left` to the target parameter (via subclass
             substitutions) in `right`. *)
          let variable_substitutions =
            let right_propagated =
              (* Create a "fake" primitive+variables type that we can propagate to the target. *)
              parametric ~primitive:left_primitive ~parameters:left_variables
              >>= (fun source ->
                  instantiate_successors_parameters order ~source ~target:right_primitive)
              |> Option.value ~default:[]
            in
            match
              List.fold2 right_propagated right_parameters ~init:Type.Map.empty ~f:diff_variables
            with
            | Ok result -> result
            | Unequal_lengths -> Type.Map.empty
          in
          let propagate_with_substitutions index variable =
            let replace_one_parameter replacement =
              let head, tail = List.split_n left_parameters index in
              match List.hd tail with
              | Some original when
                  (* If the original and replacement do not differ, no recursion is needed. *)
                  Type.equal original replacement or
                  (* Cannot perform the replacement if variance does not allow it. *)
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
              (List.length left_variables == List.length left_parameters) &&
              replace_parameters_with_destination left_variables)
          |> Option.value ~default:false
        in

        step_into_subclasses () or
        step_sideways ()

  (* \forall i \in Union[...]. A_i <= B -> Union[...] <= B. *)
  | Type.Union left, right ->
      List.fold
        ~init:true
        ~f:(fun current left ->
            current && less_or_equal order ~left ~right)
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
          | _ -> Identifier.create "?"
        in
        Type.Parametric { name; parameters = [parameter] }
      in
      less_or_equal order ~left:parametric ~right
  | Type.Tuple (Type.Unbounded parameter), Type.Primitive _ ->
      less_or_equal order ~left:(Type.parametric "tuple" [parameter]) ~right
  | Type.Tuple (Type.Bounded (left :: tail)), Type.Primitive _ ->
      let parameter = List.fold ~f:(join order) ~init:left tail in
      less_or_equal order ~left:(Type.parametric "tuple" [parameter]) ~right
  | Type.Primitive name, Type.Tuple _ ->
      Identifier.show name = "tuple"
  | Type.Tuple _, _
  | _, Type.Tuple _ ->
      false

  | Type.Callable { Callable.kind = Callable.Named left; _ },
    Type.Callable { Callable.kind = Callable.Named right; _ }
    when Expression.Access.equal left right ->
      true
  | Type.Callable { Callable.implementation = left; _ },
    Type.Callable { Callable.implementation = right; _ } ->
      let open Callable in
      let parameters_less_or_equal () =
        match left.parameters, right.parameters with
        | Undefined, Undefined ->
            true
        | Defined left, Defined right ->
            begin
              try
                let rec parameters_less_or_equal left right =
                  match left, right with
                  | Parameter.Named ({ Parameter.annotation = left_annotation; _ } as left)
                    :: left_parameters,
                    Parameter.Named ({ Parameter.annotation = right_annotation; _ } as right)
                    :: right_parameters
                  | Parameter.Keywords ({ Parameter.annotation = left_annotation; _ } as left)
                    :: left_parameters,
                    Parameter.Keywords ({ Parameter.annotation = right_annotation; _ } as right)
                    :: right_parameters
                  | Parameter.Variable ({ Parameter.annotation = left_annotation; _ } as left)
                    :: left_parameters,
                    Parameter.Variable ({ Parameter.annotation = right_annotation; _ } as right)
                    :: right_parameters ->
                      Parameter.names_compatible (Parameter.Named left) (Parameter.Named right) &&
                      less_or_equal order ~left:right_annotation ~right:left_annotation &&
                      parameters_less_or_equal left_parameters right_parameters


                  | Parameter.Variable { Parameter.annotation = left_annotation; _ }
                    :: _,
                    (Parameter.Named { Parameter.annotation = right_annotation; _ } as right)
                    :: right_parameters
                    when Parameter.is_anonymous right ->
                      less_or_equal order ~left:right_annotation ~right:left_annotation &&
                      parameters_less_or_equal left right_parameters
                  | Parameter.Variable _ :: left_parameters, []
                  | Parameter.Keywords _ :: left_parameters, [] ->
                      parameters_less_or_equal left_parameters []

                  | (Parameter.Variable _ as variable) :: (Parameter.Keywords _ as keywords) :: _,
                    (Parameter.Named _ as named) :: right ->
                      let is_compatible =
                        Type.equal (Parameter.annotation variable) (Parameter.annotation keywords) &&
                        less_or_equal
                          order
                          ~left:(Parameter.annotation named)
                          ~right:(Parameter.annotation keywords)
                      in
                      if is_compatible then
                        parameters_less_or_equal left right
                      else
                        false

                  | left :: left_parameters, [] ->
                      if Parameter.default left then
                        parameters_less_or_equal left_parameters []
                      else
                        false

                  | [], [] ->
                      true

                  | _ ->
                      false
                in
                parameters_less_or_equal left right
              with _ ->
                false
            end
        | Defined _, Undefined ->
            true
        | Undefined, Defined _ ->
            false
      in
      less_or_equal order ~left:left.annotation ~right:right.annotation &&
      parameters_less_or_equal ()

  (* A[...] <= B iff A <= B. *)
  | Type.Parametric _, Type.Primitive _  ->
      let parametric_primitive, _ = Type.split left in
      less_or_equal order ~left:parametric_primitive ~right

  | Type.Primitive name, Type.Parametric _ ->
      let left = Type.Parametric { name; parameters = [] } in
      less_or_equal order ~left ~right

  | left, Type.Callable _ ->
      let joined = join (module Handler) (Type.parametric "typing.Callable" [Type.Bottom]) left in
      begin
        match joined with
        | Type.Parametric { name; parameters = [left] }
          when Identifier.equal name (Identifier.create "typing.Callable") ->
            less_or_equal (module Handler) ~left ~right
        | _ ->
            false
      end

  | Type.Callable _, _ ->
      false

  | Type.TypedDictionary left, Type.TypedDictionary right ->
      let field_not_found field =
        not (List.exists left.fields ~f:(Type.equal_typed_dictionary_field field))
      in
      not (List.exists right.fields ~f:field_not_found)

  | Type.TypedDictionary _, Type.Parametric { name = mapping; parameters = [ key; value ] }
    when Identifier.show mapping = "typing.Mapping" &&
         Type.equal Type.string key &&
         Type.equal Type.Object value ->
      true

  | Type.TypedDictionary _, _
  | _, Type.TypedDictionary _ ->
      false

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
                  (Handler.find (Handler.edges ()) target)
                  ~f:(Target.enqueue worklist []);
                iterate worklist
              end
        | None ->
            false in

      iterate worklist


and least_common_successor ((module Handler: Handler) as order) ~successors left right =
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
      |> List.map ~f:(Handler.find_unsafe (Handler.annotations ()))
    end


and least_upper_bound ((module Handler: Handler) as order) =
  let successors index =
    match Handler.find (Handler.edges ()) index with
    | Some targets ->
        targets
        |> List.map ~f:Target.target
        |> Int.Set.of_list
    | None -> Int.Set.empty
  in
  least_common_successor order ~successors


and greatest_lower_bound ((module Handler: Handler) as order) =
  let predecessors index =
    match Handler.find (Handler.backedges ()) index with
    | Some targets ->
        targets
        |> List.map ~f:Target.target
        |> Int.Set.of_list
    | None -> Int.Set.empty
  in
  least_common_successor order ~successors:predecessors


and join_implementations ~parameter_join ~return_join order left right =
  let open Callable in
  let parameters =
    match left.parameters, right.parameters with
    | Undefined, Undefined ->
        Some Undefined
    | Defined left, Defined right ->
        begin
          try
            let join_parameter sofar left right =
              match sofar with
              | Some sofar ->
                  let joined =
                    let join_names left right =
                      if String.is_prefix ~prefix:"$" (Expression.Access.show left) then
                        left
                      else if String.is_prefix ~prefix:"$" (Expression.Access.show right) then
                        right
                      else
                        left
                    in
                    if Type.Callable.Parameter.names_compatible left right then
                      match left, right with
                      | Parameter.Named left, Parameter.Named right
                        when left.Parameter.default = right.Parameter.default ->
                          Some
                            (Parameter.Named {
                                left with
                                Parameter.annotation =
                                  parameter_join
                                    order
                                    left.Parameter.annotation
                                    right.Parameter.annotation;
                                name = join_names left.Parameter.name right.Parameter.name;
                              })
                      | Parameter.Variable left, Parameter.Variable right ->
                          Some
                            (Parameter.Variable {
                                left with
                                Parameter.annotation =
                                  parameter_join
                                    order
                                    left.Parameter.annotation
                                    right.Parameter.annotation;
                                name = join_names left.Parameter.name right.Parameter.name;
                              })
                      | Parameter.Keywords left, Parameter.Keywords right ->
                          Some
                            (Parameter.Keywords {
                                left with
                                Parameter.annotation =
                                  parameter_join
                                    order
                                    left.Parameter.annotation
                                    right.Parameter.annotation;
                                name = join_names left.Parameter.name right.Parameter.name;
                              })
                      | _ ->
                          None
                    else
                      None
                  in
                  joined
                  >>| (fun joined -> joined :: sofar)
              | None ->
                  None
            in
            List.fold2_exn ~init:(Some []) ~f:join_parameter left right
            >>| List.rev
            >>| fun parameters -> Defined parameters
          with _ ->
            None
        end
    | _ ->
        None
  in
  parameters
  >>| fun parameters ->
  { annotation = return_join order left.annotation right.annotation; parameters = parameters }


and join ((module Handler: Handler) as order) left right =
  if Type.equal left right then
    left
  else
    match left, right with
    | undeclared, _ when Type.equal undeclared Type.undeclared ->
        Type.union [left; right]
    | _, undeclared when Type.equal undeclared Type.undeclared ->
        Type.union [left; right]

    | Type.Top, _
    | _, Type.Top ->
        Type.Top

    | Type.Deleted, _
    | _, Type.Deleted ->
        Type.Deleted

    | Type.Object, _
    | _, Type.Object ->
        Type.Object

    | Type.Bottom, other
    | other, Type.Bottom ->
        other

    | (Type.Variable { constraints = Type.Unconstrained; _ } as variable), other
    | other, (Type.Variable { constraints = Type.Unconstrained; _ } as variable) ->
        Type.union [variable; other]
    | Type.Variable { constraints = Type.Bound left; _ }, right ->
        join order left right
    | left, Type.Variable { constraints = Type.Bound right; _ } ->
        join order left right
    | Type.Variable { constraints = Type.Explicit left; _ }, right ->
        join order (Type.union left) right
    | left, Type.Variable { constraints = Type.Explicit right; _ } ->
        join order left (Type.union right)

    (* n: A_n = B_n -> Union[A_i] <= Union[B_i]. *)
    | (Type.Union left), (Type.Union right) ->
        Type.union (left @ right)
    | (Type.Union elements as union), other
    | other, (Type.Union elements as union) ->
        if less_or_equal order ~left:other ~right:union then
          union
        else
          begin
            match other with
            | Type.Optional Type.Bottom ->
                Type.Optional union
            | Type.Optional element ->
                Type.Optional (Type.union (element :: elements))
            | _ ->
                List.map elements ~f:(join order other)
                |> List.fold ~f:(join order) ~init:Type.Bottom
          end

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
            with Untracked _ ->
              Type.Object
          in
          if Handler.contains (Handler.indices ()) target then
            let left_parameters = instantiate_successors_parameters order ~source:left ~target in
            let right_parameters = instantiate_successors_parameters order ~source:right ~target in
            let variables = variables order target in
            let parameters =
              let join_parameters left right variable =
                match left, right, variable with
                | Type.Bottom, other, _
                | other, Type.Bottom, _ ->
                    other
                | Type.Object, _, _
                | _, Type.Object, _ ->
                    Type.Object
                | _, _, Type.Variable { variance = Covariant; _ } ->
                    join order left right
                | _, _, Type.Variable { variance = Contravariant; _ } ->
                    meet order left right
                | _, _, Type.Variable { variance = Invariant; _ } ->
                    if less_or_equal order ~left ~right &&
                       less_or_equal order ~left:right ~right:left then
                      left
                    else
                      (* We fallback to Type.Object if type equality fails to help display
                         meaningful error messages. *)
                      Type.Object
                | _ ->
                    Log.warning "Cannot join %a and %a, not a variable: %a"
                      Type.pp left Type.pp right Type.pp variable;
                    Type.Object
              in
              match left_parameters, right_parameters, variables with
              | Some left, Some right, Some variables
                when List.length left = List.length right &&
                     List.length left = List.length variables ->
                  let join_parameters left right variable =
                    join_parameters left right variable
                    |> Type.instantiate_variables ~replacement:Type.Top
                  in
                  Some (List.map3_exn ~f:join_parameters left right variables)
              | _ ->
                  None
            in
            begin
              match target, parameters with
              | Type.Primitive name, Some parameters ->
                  Type.Parametric { name; parameters }
              | Type.Primitive _, None ->
                  target
              | _ ->
                  Type.Object
            end
          else
            Type.Object

    (* Special case joins of optional collections with their uninstantated counterparts. *)
    | Type.Parametric ({ parameters = [Type.Bottom]; _ } as other),
      Type.Optional (Type.Parametric ({ parameters = [parameter]; _ } as collection))
    | Type.Optional (Type.Parametric ({ parameters = [parameter]; _ } as collection)),
      Type.Parametric ({ parameters = [Type.Bottom]; _ } as other)
      when Identifier.equal other.name collection.name ->
        Type.Parametric { other with parameters = [parameter] }

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
    | (Type.Tuple (Type.Unbounded parameter)), (Type.Parametric _ as annotation)
    | (Type.Tuple (Type.Unbounded parameter)), (Type.Primitive _ as annotation)
    | (Type.Parametric _ as annotation), (Type.Tuple (Type.Unbounded parameter))
    | (Type.Primitive _ as annotation), (Type.Tuple (Type.Unbounded parameter)) ->
        join order (Type.parametric "tuple" [parameter]) annotation
    | Type.Tuple _, _
    | _, Type.Tuple _ ->
        Type.union [left; right]

    | (Type.Callable { Callable.kind = Callable.Named left; _ } as callable),
      Type.Callable { Callable.kind = Callable.Named right; _ }
      when Expression.Access.equal left right ->
        callable
    | Type.TypedDictionary { fields = left_fields; _ },
      Type.TypedDictionary { fields = right_fields; _ } ->
        if Type.TypedDictionary.fields_have_colliding_keys left_fields right_fields then
          Type.Parametric {
            name = Identifier.create "typing.Mapping";
            parameters = [ Type.string; Type.Object ]
          }
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
          Type.TypedDictionary.anonymous join_fields

    | Type.TypedDictionary _, Type.Parametric { name = mapping; parameters = [ key_annotation; _ ] }
    | Type.Parametric { name = mapping; parameters = [ key_annotation; _ ] }, Type.TypedDictionary _
      when Identifier.show mapping = "typing.Mapping" &&
           (* Mappings are only covariant in their value annotations. *)
           Type.equal key_annotation Type.string ->
        Type.Parametric {
          name = Identifier.create "typing.Mapping";
          parameters = [ Type.string; Type.Object ]
        }

    | Type.TypedDictionary _, _
    | _, Type.TypedDictionary _ ->
        Type.Object

    | Type.Callable left,
      Type.Callable right ->
        if List.is_empty left.Callable.overloads
        && List.is_empty right.Callable.overloads then
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
          |> Option.value ~default:Type.Object
        else
          Type.Object

    | Type.Callable callable, other
    | other, Type.Callable callable ->
        let other = join (module Handler) (Type.parametric "typing.Callable" [Type.Bottom]) other in
        begin
          match other with
          | Type.Parametric { name; parameters = [other_callable] }
            when Identifier.equal name (Identifier.create "typing.Callable") ->
              join (module Handler) (Type.Callable callable) other_callable
          | _ ->
              Type.union [left; right]
        end
    | _ ->
        match List.hd (least_upper_bound order left right) with
        | Some joined ->
            if Type.equal joined left || Type.equal joined right then
              joined
            else
              Type.union [left; right]
        | None ->
            Log.debug "Couldn't find a upper bound for %a and %a" Type.pp left Type.pp right;
            Type.Object


and meet ((module Handler: Handler) as order) left right =
  if Type.equal left right then
    left
  else
    match left, right with
    | Type.Top, other
    | other, Type.Top ->
        other

    | Type.Deleted, other
    | other, Type.Deleted when not (Type.equal other Type.Top) ->
        other

    | Type.Object, other
    | other, Type.Object when not (Type.is_unknown other) ->
        other

    | Type.Bottom, _
    | _, Type.Bottom ->
        Type.Bottom

    | Type.Variable { constraints = Type.Unconstrained; _ }, other
    | other, Type.Variable { constraints = Type.Unconstrained; _ } ->
        other
    | Type.Variable ({ constraints = Type.Bound left; _ } as variable), right ->
        Type.Variable { variable with constraints = Type.Bound (meet order left right) }
    | left, Type.Variable ({ constraints = Type.Bound right; _ } as variable) ->
        Type.Variable { variable with constraints = Type.Bound (meet order left right) }
    | Type.Variable { constraints = Type.Explicit left; _ }, right ->
        meet order (Type.union left) right
    | left, Type.Variable { constraints = Type.Explicit right; _ } ->
        meet order left (Type.union right)

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
          List.map elements ~f:(meet order other)
          |> List.fold ~f:(meet order) ~init:Type.Top

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
            let left_parameters = instantiate_predecessors_parameters order ~source:left ~target in
            let right_parameters =
              instantiate_predecessors_parameters
                order
                ~source:right
                ~target
            in
            let variables = variables order target in

            let parameters =
              let meet_parameters left right variable =
                match left, right, variable with
                | Type.Object, other, _
                | other, Type.Object, _ when not (Type.is_unknown other) ->
                    other
                | Type.Bottom, _, _
                | _, Type.Bottom, _ ->
                    Type.Bottom
                | _, _, Type.Variable { variance = Covariant; _ } ->
                    meet order left right
                | _, _, Type.Variable { variance = Contravariant; _ } ->
                    join order left right
                | _, _, Type.Variable { variance = Invariant; _ } ->
                    if less_or_equal order ~left ~right &&
                       less_or_equal order ~left:right ~right:left then
                      left
                    else
                      (* We fallback to Type.Bottom if type equality fails to help display
                         meaningful error messages. *)
                      Type.Bottom
                | _ ->
                    Log.warning "Cannot meet %a and %a, not a variable: %a"
                      Type.pp left Type.pp right Type.pp variable;
                    Type.Bottom
              in
              match left_parameters, right_parameters, variables with
              | Some left, Some right, Some variables
                when List.length left = List.length right &&
                     List.length left = List.length variables ->
                  Some (List.map3_exn ~f:meet_parameters left right variables)
              | _ ->
                  None
            in
            begin
              match target, parameters with
              | Type.Primitive name, Some parameters ->
                  Type.Parametric { name; parameters }
              | _ ->
                  Type.Bottom
            end
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

    | Type.Callable ({ Callable.kind = Callable.Anonymous; _ } as left),
      Type.Callable ({ Callable.kind = Callable.Anonymous; _ } as right) ->
        join_implementations
          ~parameter_join:join
          ~return_join:meet
          order
          left.Callable.implementation
          right.Callable.implementation
        >>| (fun implementation -> Type.Callable { left with Callable.implementation })
        |> Option.value ~default:Type.Bottom
    | (Type.Callable { Callable.kind = Callable.Named left; _ } as callable),
      Type.Callable { Callable.kind = Callable.Named right; _ }
      when Expression.Access.equal left right ->
        callable
    | Type.Callable _, _
    | _, Type.Callable _ ->
        Type.Bottom
    | Type.TypedDictionary { fields = left_fields; _ },
      Type.TypedDictionary { fields = right_fields; _ } ->
        if Type.TypedDictionary.fields_have_colliding_keys left_fields right_fields then
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
          Type.TypedDictionary.anonymous meet_fields
    | Type.TypedDictionary _, _
    | _, Type.TypedDictionary _ ->
        Type.Bottom

    | _ ->
        match List.hd (greatest_lower_bound order left right) with
        | Some bound -> bound
        | None ->
            Log.debug "No lower bound found for %a and %a" Type.pp left Type.pp right;
            Type.Bottom


(* If a node on the graph has Generic[_T1, _T2, ...] as a supertype and has
   concrete parameters, all occurrences of _T1, _T2, etc. in other supertypes
   need to be replaced with the concrete parameter corresponding to the type
   variable. This function takes a target with concrete parameters and its supertypes,
   and instantiates the supertypes accordingly. *)
and get_instantiated_successors ~generic_index ~parameters successors =
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
  >>| (fun variables ->
      if List.length variables = List.length parameters then
        let constraints =
          List.zip_exn variables parameters
          |> Type.Map.of_alist_reduce ~f:(fun first _ -> first)
          |> Map.find
        in
        let instantiate_parameters { Target.target; parameters } =
          {
            Target.target;
            parameters = List.map parameters ~f:(Type.instantiate ~constraints);
          }
        in
        List.map successors ~f:instantiate_parameters
      else
        successors)
  |> Option.value ~default:successors


and get_instantiated_predecessors
    (module Handler: Handler)
    ~generic_index
    ~parameters
    predecessors =
  let instantiate { Target.target; parameters = predecessor_variables } =
    let generic_parameters =
      let generic_parameters { Target.target; parameters } =
        if Some target = generic_index then
          Some parameters
        else
          None
      in
      Handler.find (Handler.edges ()) target
      >>= List.find_map ~f:generic_parameters
      |> Option.value ~default:[]
    in

    (* Mappings from the generic variables, as they appear in the predecessor, to the
       instantiated parameter in the current annotation. For example, given:

       Derived(Base[T1, int, T2], Generic[ ...irrelevant... ])

       and an instantiated: Base[str, int, float]
       This mapping would include: { T1 => str; T2 => float }
    *)
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
    ((module Handler: Handler) as order)
    ~source
    ~target =
  let primitive, parameters = Type.split source in

  raise_if_untracked order primitive;
  raise_if_untracked order target;

  let generic_index = Handler.find (Handler.indices ()) Type.generic in

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
            Handler.find (Handler.edges ()) target_index
            >>| get_instantiated_successors ~generic_index ~parameters
            >>| List.iter ~f:(Queue.enqueue worklist)
            |> ignore;
            iterate worklist
          end
    | None ->
        None
  in
  iterate worklist


and instantiate_predecessors_parameters
    ((module Handler: Handler) as order)
    ~source
    ~target =
  let primitive, parameters = Type.split source in

  raise_if_untracked order primitive;
  raise_if_untracked order target;

  let generic_index = Handler.find (Handler.indices ()) Type.generic in

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
            Handler.find (Handler.backedges ()) target_index
            >>| get_instantiated_predecessors order ~generic_index ~parameters
            >>| List.iter ~f:(Queue.enqueue worklist)
            |> ignore;
            iterate worklist
          end
    | None ->
        None
  in
  iterate worklist


and diff_variables substitutions left right =
  match left, right with
  | Callable { implementation = left_implementation; overloads = left_overloads; _ },
    Callable { implementation = right_implementation; overloads = right_overloads; _ } ->
      begin
        let open Type.Callable in
        let diff_overloads
            substitutions
            { annotation = left_annotation; parameters = left_parameters }
            { annotation = right_annotation; parameters = right_parameters } =
          let substitutions = diff_variables substitutions left_annotation right_annotation in
          match left_parameters, right_parameters with
          | Defined left, Defined right ->
              begin
                let diff_parameters substitutions left right =
                  diff_variables substitutions (Parameter.annotation left) (Parameter.annotation right)
                in
                match List.fold2 ~init:substitutions ~f:diff_parameters left right with
                | Ok substitutions -> substitutions
                | Unequal_lengths -> substitutions
              end
          | _ ->
              substitutions
        in
        let substitutions = diff_overloads substitutions left_implementation right_implementation in
        match List.fold2 ~init:substitutions ~f:diff_overloads left_overloads right_overloads with
        | Ok substitutions -> substitutions
        | Unequal_lengths -> substitutions
      end
  | Optional left, Optional right ->
      diff_variables substitutions left right
  | Parametric { parameters = left; _ }, Parametric { parameters = right; _ } ->
      diff_variables_list substitutions left right
  | Tuple (Bounded left), Tuple (Bounded right) ->
      diff_variables_list substitutions left right
  | Tuple (Unbounded left), Tuple (Unbounded right) ->
      diff_variables substitutions left right
  | TypedDictionary { fields = left_fields; _ }, TypedDictionary { fields = right_fields; _ } ->
      begin
        let diff_fields substitutions { Type.annotation = left; _ } { Type.annotation = right; _ } =
          diff_variables substitutions left right
        in
        match List.fold2 ~init:substitutions ~f:diff_fields left_fields right_fields with
        | Ok substitutions -> substitutions
        | Unequal_lengths -> substitutions
      end
  | Union left, Union right ->
      diff_variables_list substitutions left right
  | Variable _, _ ->
      Map.set substitutions ~key:left ~data:right
  | _ ->
      substitutions


and diff_variables_list substitutions left right =
  match List.fold2 left right ~init:substitutions ~f:diff_variables with
  | Ok substitutions -> substitutions
  | Unequal_lengths -> substitutions


let widen order ~widening_threshold ~previous ~next ~iteration =
  if iteration > widening_threshold then
    Type.Top
  else
    join order previous next


let add_backedges (module Handler: Handler) ~bottom =
  let backedges = Handler.backedges () in
  let edge_keys = Handler.keys () in
  let bottom = Handler.find_unsafe (Handler.indices ()) bottom in
  let add_backedges predecessor =
    let successors = Handler.find (Handler.edges ()) predecessor in
    let add_backedge { Target.target = successor; parameters } =
      let node = { Target.target = predecessor; parameters } in
      match (Handler.find (Handler.backedges ()) successor) with
      | None ->
          Handler.set backedges ~key:successor ~data:[node]
      | Some nodes ->
          Handler.set backedges ~key:successor ~data:(node :: nodes)
    in
    match successors with
    | Some successors ->
        List.iter ~f:add_backedge successors
    | None ->
        ()
  in
  let clear_backedge key =
    Handler.set backedges ~key ~data:[]
  in
  let add_bottom key =
    match key <> bottom, Handler.find backedges key with
    | true, None
    | true, Some [] ->
        Handler.set backedges ~key ~data:[{ Target.target = bottom; parameters = [] }]
    | _ ->
        ()
  in
  List.iter ~f:clear_backedge edge_keys;
  List.iter ~f:add_backedges edge_keys;
  List.iter ~f:add_bottom edge_keys


let deduplicate (module Handler: Handler) ~annotations =
  let edges = Handler.edges () in
  let backedges = Handler.backedges () in
  let deduplicate_annotation index =
    let deduplicate_edges edges =
      let keep_first (visited, edges) ({ Target.target; _ } as edge) =
        if Set.mem visited target then
          visited, edges
        else
          Set.add visited target, edge :: edges
      in
      let deduplicate found =
        List.fold ~f:keep_first ~init:(Int.Set.empty, []) found
        |> snd
      in
      match Handler.find edges index with
      | Some found ->
          Handler.set edges ~key:index ~data:(deduplicate found)
      | None ->
          ()
    in
    deduplicate_edges edges;
    deduplicate_edges backedges
  in
  annotations
  |> List.map ~f:(Handler.find_unsafe (Handler.indices ()))
  |> List.iter ~f:deduplicate_annotation


let remove_extra_edges (module Handler: Handler) ~bottom ~top annotations =
  let disconnect keys ~edges ~backedges special_index =
    let remove_extra_references key =
      (Handler.find edges key
       >>|
       (fun connected ->
          let disconnected =
            List.filter ~f:(fun { Target.target; _ } -> target <> special_index) connected
          in
          if List.is_empty disconnected then
            []
          else
            begin
              Handler.set edges ~key ~data:disconnected;
              [key]
            end))
      |> Option.value ~default:[]
    in
    let removed_indices = List.concat_map ~f:remove_extra_references keys |> Int.Set.of_list in
    Handler.find backedges special_index
    >>|
    (fun edges ->
       let edges =
         List.filter ~f:(fun { Target.target; _ } -> not (Set.mem removed_indices target)) edges
       in
       Handler.set backedges ~key:special_index ~data:edges)
    |> Option.value ~default:()
  in
  let edges = Handler.edges () in
  let backedges = Handler.backedges () in
  let index_of annotation = Handler.find_unsafe (Handler.indices ()) annotation in
  let keys = List.map annotations ~f:index_of in
  disconnect keys ~edges ~backedges (index_of top);
  disconnect
    keys
    ~edges:backedges
    ~backedges:edges
    (index_of bottom)


let connect_annotations_to_top
    ((module Handler: Handler) as order)
    ~top
    annotations =
  let indices =
    let index_of annotation = Handler.find_unsafe (Handler.indices ()) annotation in
    List.map annotations ~f:index_of
  in
  let connect_to_top index =
    let annotation = Handler.find_unsafe (Handler.annotations ()) index in
    if not (less_or_equal order ~left:top ~right:annotation) then
      begin
        match Handler.find (Handler.edges ()) index with
        | Some targets when List.length targets > 0 ->
            ()
        | _ ->
            connect order ~predecessor:annotation ~successor:top
      end in
  List.iter ~f:connect_to_top indices


let check_integrity (module Handler: Handler) =
  (* Check `Top` and `Bottom`. *)
  let contains annotation = Handler.contains (Handler.indices ()) annotation in
  if not (contains Type.Bottom && contains Type.Top) then
    begin
      Log.error "Order is missing either Bottom or Top:\n%s" (Handler.show ());
      raise Incomplete
    end;

  (* Ensure keys are consistent. *)
  let key_consistent key =
    let raise_if_none value =
      if Option.is_none value then
        begin
          Log.error "Inconsistency in type order: No value for key %d" key;
          raise Incomplete
        end
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
      begin
        let rec visit reverse_visited index =
          if List.mem ~equal:Int.equal reverse_visited index then
            begin
              let trace =
                List.rev_map
                  ~f:(Handler.find_unsafe (Handler.annotations ()))
                  (index :: reverse_visited)
                |> List.map ~f:(Format.asprintf "%a" Type.pp)
                |> String.concat ~sep:" -> " in
              Log.error
                "Order is cyclic:\nTrace: %s"
                (* (Handler.show ()) *)
                trace;
              raise Cyclic
            end
          else if not (Set.mem !started_from index) then
            begin
              started_from := Set.add !started_from index;
              match Handler.find (Handler.edges ()) index with
              | Some successors ->
                  successors
                  |> List.map ~f:Target.target
                  |> List.iter ~f:(visit (index :: reverse_visited))
              | None ->
                  ()
            end in
        visit [] start
      end in
  Handler.keys ()
  |> List.iter ~f:find_cycle;

  (* Check that backedges are complete. *)
  let check_inverse ~get_keys ~edges ~backedges =
    let check_backedge index =
      let check_backedge { Target.target; _ } =
        let has_backedge =
          match Handler.find backedges target with
          | Some targets ->
              List.exists ~f:(fun { Target.target; _ } -> target = index) targets
          | None ->
              false
        in
        if not has_backedge then
          begin
            Log.error
              "No back-edge found for %a -> %a"
              Type.pp (Handler.find_unsafe (Handler.annotations ()) index)
              Type.pp (Handler.find_unsafe (Handler.annotations ()) target);
            raise Incomplete
          end
      in
      List.iter ~f:check_backedge (Handler.find_unsafe edges index)
    in
    get_keys ()
    |> List.iter ~f:check_backedge
  in
  check_inverse
    ~get_keys:Handler.keys
    ~edges:(Handler.edges ())
    ~backedges:(Handler.backedges ());
  check_inverse
    ~get_keys:Handler.keys
    ~edges:(Handler.backedges ())
    ~backedges:(Handler.edges ())


let to_dot (module Handler: Handler) =
  let indices = List.sort ~compare (Handler.keys ()) in
  let nodes =
    List.map indices ~f:(fun index -> (index, Handler.find_unsafe (Handler.annotations ()) index))
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
    >>| List.iter ~f:(
      fun { Target.target = successor; parameters } ->
        Format.asprintf "  %d -> %d" index successor
        |> Buffer.add_string buffer;
        if List.length parameters > 0 then
          Format.asprintf "[label=\"%s\"]" (List.to_string ~f:Type.show parameters)
          |> Buffer.add_string buffer;
        Buffer.add_string buffer "\n"
    )
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


  let default_annotations =
    let singleton annotation = [Type.Bottom; annotation; Type.Object] in
    [
      [Type.Bottom; Type.Object; Type.Deleted; Type.Top];
      (* Special forms *)
      singleton (Type.primitive "typing.Tuple");
      singleton Type.named_tuple;
      singleton Type.generic;
      singleton (Type.primitive "typing.Protocol");
      singleton (Type.primitive "typing.Callable");
      singleton (Type.primitive "typing.FrozenSet");
      singleton (Type.primitive "typing.Optional");
      singleton (Type.primitive "typing.TypeVar");
      singleton (Type.primitive "typing.Undeclared");
      singleton (Type.primitive "typing.Union");
      singleton (Type.primitive "typing.NoReturn");
      (* Ensure unittest.mock.Base is there because we check against it. *)
      singleton (Type.primitive "unittest.mock.Base");
      singleton (Type.primitive "unittest.mock.NonCallableMock");
      singleton (Type.primitive "typing.ClassVar");
      [Type.Bottom; Type.primitive "dict"; Type.primitive "typing.Dict"; Type.Object];
      singleton (Type.primitive "None");
      (* Numerical hierarchy. *)
      [
        Type.Bottom;
        Type.integer;
        Type.float;
        Type.complex;
        Type.primitive "numbers.Complex";
        Type.primitive "numbers.Number";
        Type.Object;
      ];
      [Type.integer; Type.primitive "numbers.Integral"; Type.Object];
      [Type.float; Type.primitive "numbers.Rational"; Type.Object];
      [Type.float; Type.primitive "numbers.Real"; Type.Object];
    ]


  let builtin_types =
    List.concat default_annotations
    |> Type.Set.of_list


  let default () =
    let order = create () in
    let handler = handler order in

    Set.iter builtin_types ~f:(insert handler);
    let rec connect_primitive_chain annotations =
      match annotations with
      | predecessor :: successor :: rest ->
          connect handler ~predecessor ~successor;
          connect_primitive_chain (successor :: rest)
      | _ ->
          ()
    in
    List.iter ~f:connect_primitive_chain default_annotations;

    (* Since the builtin type hierarchy is not primitive, it's special cased. *)
    let type_builtin = Type.Primitive (Identifier.create "type") in
    let type_variable = Type.variable "_T" in
    insert handler type_builtin;
    connect handler ~predecessor:Type.Bottom ~successor:type_builtin;
    connect handler ~predecessor:type_builtin ~parameters:[type_variable] ~successor:Type.generic;

    order
end
