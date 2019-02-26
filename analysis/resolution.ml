(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre

open Ast
open Statement


type global = Annotation.t Node.t
[@@deriving eq, show]


type class_representation = {
  class_definition: Class.t Node.t;
  successors: Type.t list;
  explicit_attributes: Attribute.t Access.SerializableMap.t;
  implicit_attributes: Attribute.t Access.SerializableMap.t;
  is_test: bool;
  methods: Type.t list;
}


type t = {
  annotations: Annotation.t Access.Map.t;
  type_variables: Type.Set.t;
  order: (module TypeOrder.Handler);

  resolve: resolution: t -> Expression.t -> Type.t;
  parse_annotation: Expression.t -> Type.t;

  global: Access.t -> global option;
  module_definition: Access.t -> Module.t option;
  class_definition: Type.t -> (Class.t Node.t) option;
  class_representation: Type.t -> class_representation option;
  constructor: instantiated: Type.t -> resolution: t -> Class.t Node.t -> Type.t;

  parent: Access.t option;
}


let create
    ~annotations
    ~order
    ~resolve
    ~parse_annotation
    ~global
    ~module_definition
    ~class_definition
    ~class_representation
    ~constructor
    ?parent
    () =
  {
    annotations;
    type_variables = Type.Set.empty;
    order;
    resolve;
    parse_annotation;
    global;
    module_definition;
    class_definition;
    class_representation;
    constructor;
    parent;
  }


let pp format { annotations; type_variables; _ } =
  let annotation_map_entry (access, annotation) =
    Format.asprintf
      "%a -> %a"
      Access.pp access
      Annotation.pp annotation;
  in
  Type.Set.to_list type_variables
  |> List.map ~f:Type.show
  |> String.concat ~sep:", "
  |> Format.fprintf format "Type variables: [%s]\n";
  Map.to_alist annotations
  |> List.map ~f:annotation_map_entry
  |> String.concat ~sep:", "
  |> Format.fprintf format "Annotations: [%s]"


let show resolution =
  Format.asprintf "%a" pp resolution


let set_local ({ annotations; _ } as resolution) ~access ~annotation =
  { resolution with annotations = Map.set annotations ~key:access ~data:annotation }


let get_local ?(global_fallback=true) ~access { annotations; global; _ } =
  match Map.find annotations access with
  | Some result ->
      Some result
  | _ when global_fallback ->
      Access.delocalize access
      |> global
      >>| Node.value
  | _ ->
      None


let unset_local ({ annotations; _ } as resolution) ~access =
  { resolution with annotations = Map.remove annotations access }


let is_global { annotations; global; _ } ~access =
  match Map.find annotations access with
  | Some annotation ->
      Annotation.is_global annotation
  | _ ->
      Access.delocalize access
      |> global
      |> Option.is_some


let add_type_variable ({ type_variables; _ } as resolution) ~variable =
  { resolution with type_variables = Type.Set.add type_variables variable }


let type_variable_exists { type_variables; _ } ~variable =
  Type.Set.mem type_variables variable


let annotations { annotations; _ } =
  annotations


let with_annotations resolution ~annotations =
  { resolution with annotations }


let parent { parent; _ } =
  parent


let with_parent resolution ~parent =
  { resolution with parent }


let order { order; _ } =
  order


let resolve ({ resolve; _  } as resolution) =
  resolve ~resolution


let global { global; _ } =
  global


let module_definition { module_definition; _ } =
  module_definition


let class_definition { class_definition; _ } =
  class_definition


let class_representation { class_representation; _ } =
  class_representation


let constructor ({ constructor; _ } as resolution) =
  constructor ~resolution


let function_definitions resolution access =
  let qualifier =
    let rec qualifier ~lead ~tail =
      match tail with
      | head :: tail ->
          let new_lead = lead @ [head] in
          if Option.is_none (module_definition resolution new_lead) then
            lead
          else
            qualifier ~lead:new_lead ~tail
      | _ ->
          lead
    in
    qualifier ~lead:[] ~tail:access
  in
  Ast.SharedMemory.Sources.get_for_qualifier qualifier
  >>| Preprocessing.defines ~include_stubs:true ~include_nested:true
  >>| List.filter ~f:(fun { Node.value = { Define.name; _ }; _ } -> Access.equal access name)


let order_and_constructor ({ order; _ } as resolution) =
  let constructor instantiated =
    class_definition resolution instantiated
    >>| constructor resolution ~instantiated
  in
  { TypeOrder.handler = order; constructor }

let solve_constraints resolution =
  order_and_constructor resolution
  |> TypeOrder.solve_constraints

let constraints_solution_exists ~source ~target resolution =
  solve_constraints resolution ~constraints:Type.Map.empty ~source ~target
  |> Option.is_some

let less_or_equal resolution =
  order_and_constructor resolution
  |> TypeOrder.less_or_equal


let join resolution =
  order_and_constructor resolution
  |> TypeOrder.join


let meet resolution =
  order_and_constructor resolution
  |> TypeOrder.meet


let widen resolution =
  order_and_constructor resolution
  |> TypeOrder.widen


let is_instantiated { order; _ } =
  TypeOrder.is_instantiated order


let is_tracked { order; _ } annotation =
  TypeOrder.contains order annotation


let contains_untracked resolution annotation =
  List.exists
    ~f:(fun annotation -> not (is_tracked resolution annotation))
    (Type.elements annotation)


let is_string_to_any_mapping resolution annotation =
  (* TODO(T40377122): Remove special-casing of Dict[str, Any] in strict. *)
  less_or_equal
    resolution
    ~left:annotation
    ~right:(Type.parametric "typing.Mapping" [Type.string; Type.Any])


let parse_annotation
    ?(allow_untracked=false)
    ({ parse_annotation; module_definition; _ } as resolution)
    expression =
  let expression =
    let is_local_access =
      Expression.show expression
      |> String.is_substring ~substring:"$local_"
    in
    if is_local_access then
      Expression.delocalize expression
    else
      expression
  in
  let parsed = parse_annotation expression in
  let constraints = function
    | Type.Primitive name ->
        let originates_from_empty_stub =
          name
          |> Access.create
          |> fun access -> Module.from_empty_stub ~access ~module_definition
        in
        if originates_from_empty_stub then
          Some Type.Any
        else
          None
    | _ ->
        None
  in
  let annotation = Type.instantiate parsed ~constraints in
  if contains_untracked resolution annotation && not allow_untracked then
    Type.Top
  else
    annotation


let is_invariance_mismatch resolution ~left ~right =
  match left, right with
  | Type.Parametric { name = left_name; parameters = left_parameters },
    Type.Parametric { name = right_name; parameters = right_parameters }
    when Identifier.equal left_name right_name ->
      let zipped =
        TypeOrder.variables (order resolution) left
        >>= fun variables ->
        (List.map3
           variables
           left_parameters
           right_parameters
           ~f:(fun variable left right -> (variable, left, right))
         |> function
         | List.Or_unequal_lengths.Ok list -> Some list
         | _ -> None)
      in
      let due_to_invariant_variable (variable, left, right) =
        match variable with
        | Type.Variable { variance = Type.Invariant; _ } ->
            less_or_equal resolution ~left ~right
        | _ ->
            false
      in
      zipped
      >>| List.exists ~f:due_to_invariant_variable
      |> Option.value ~default:false
  | _ ->
      false


(* In general, python expressions can be self-referential. This resolution only checks
   literals and annotations found in the resolution map, without resolving expressions. *)
let rec resolve_literal resolution expression =
  let open Ast.Expression in
  match Node.value expression with
  | Access (SimpleAccess access) ->
      begin
        let is_defined class_name =
          class_definition resolution class_name
          |> Option.is_some
        in
        match Expression.Access.name_and_arguments ~call:access with
        | Some { Expression.Access.callee; _ } ->
            let class_name =
              Expression.Access.create callee
              |> Expression.Access.expression
              |> parse_annotation resolution
            in
            if is_defined class_name then
              class_name
            else
              Type.Top
        | None ->
            let class_name = parse_annotation resolution expression in
            (* None is a special type that doesn't have a constructor. *)
            if Type.equal class_name Type.none then
              Type.none
            else if is_defined class_name then
              Type.meta class_name
            else
              Type.Top
      end
  | Await expression ->
      resolve_literal resolution expression
      |> Type.awaitable_value

  | BooleanOperator { BooleanOperator.left; right; _ } ->
      let annotation =
        join
          resolution
          (resolve_literal resolution left)
          (resolve_literal resolution right)
      in
      if Type.is_concrete annotation then annotation else Type.Any

  | Complex _ ->
      Type.complex

  | Dictionary { Dictionary.entries; keywords = [] } ->
      let key_annotation, value_annotation =
        let join_entry (key_annotation, value_annotation) { Dictionary.key; value } =
          (
            join resolution key_annotation (resolve_literal resolution key),
            join resolution value_annotation (resolve_literal resolution value)
          )
        in
        List.fold ~init:(Type.Bottom, Type.Bottom) ~f:join_entry entries
      in
      if Type.is_concrete key_annotation && Type.is_concrete value_annotation then
        Type.dictionary ~key:key_annotation ~value:value_annotation
      else
        Type.Any

  | False ->
      Type.bool

  | Float _ ->
      Type.float

  | Integer _ ->
      Type.integer

  | List elements ->
      let parameter =
        let join sofar element =
          join resolution sofar (resolve_literal resolution element)
        in
        List.fold ~init:Type.Bottom ~f:join elements
      in
      if Type.is_concrete parameter then Type.list parameter else Type.Any

  | Set elements ->
      let parameter =
        let join sofar element =
          join resolution sofar (resolve_literal resolution element)
        in
        List.fold ~init:Type.Bottom ~f:join elements
      in
      if Type.is_concrete parameter then Type.set parameter else Type.Any

  | String { StringLiteral.kind; _ } ->
      begin
        match kind with
        | StringLiteral.Bytes -> Type.bytes
        | _ -> Type.string
      end

  | Ternary { Ternary.target; alternative; _ } ->
      let annotation =
        join
          resolution
          (resolve_literal resolution target)
          (resolve_literal resolution alternative)
      in
      if Type.is_concrete annotation then annotation else Type.Any

  | True ->
      Type.bool

  | Tuple elements ->
      Type.tuple (List.map elements ~f:(resolve_literal resolution))

  | Expression.Yield _ ->
      Type.yield Type.Any

  | _ ->
      Type.Any

let resolve_mutable_literals resolution ~expression ~resolved ~expected =
  match expression with
  | Some { Node.value = Expression.List _; _ }
  | Some { Node.value = Expression.ListComprehension _; _ } ->
      begin
        match resolved, expected with
        | Type.Parametric { name = actual_name; parameters = [actual] },
          Type.Parametric { name = expected_name; parameters = [expected_parameter] }
          when Identifier.equal actual_name "list" &&
               Identifier.equal expected_name "list" &&
               less_or_equal resolution ~left:actual ~right:expected_parameter ->
            expected
        | _ ->
            resolved
      end

  | Some { Node.value = Expression.Set _; _ }
  | Some { Node.value = Expression.SetComprehension _; _ } ->
      begin
        match resolved, expected with
        | Type.Parametric { name = actual_name; parameters = [actual] },
          Type.Parametric { name = expected_name; parameters = [expected_parameter] }
          when Identifier.equal actual_name "set" &&
               Identifier.equal expected_name "set" &&
               less_or_equal resolution ~left:actual ~right:expected_parameter ->
            expected
        | _ ->
            resolved
      end

  | Some { Node.value = Expression.Dictionary _; _ }
  | Some { Node.value = Expression.DictionaryComprehension _; _ } ->
      begin
        match resolved, expected with
        | Type.Parametric { name = actual_name; parameters = [actual_key; actual_value] },
          Type.Parametric {
            name = expected_name;
            parameters = [expected_key; expected_value];
          }
          when Identifier.equal actual_name "dict" &&
               Identifier.equal expected_name "dict" &&
               less_or_equal resolution ~left:actual_key ~right:expected_key &&
               less_or_equal
                 resolution
                 ~left:actual_value
                 ~right:expected_value ->
            expected
        | _ ->
            resolved
      end

  | _ ->
      resolved
