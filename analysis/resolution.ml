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
  order: (module TypeOrder.Handler);

  resolve: resolution: t -> Expression.t -> Type.t;
  parse_annotation: Expression.t -> Type.t;

  global: Access.t -> global option;
  module_definition: Access.t -> Module.t option;
  class_definition: Type.t -> (Class.t Node.t) option;
  class_representation: Type.t -> class_representation option;

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
    ?parent
    () =
  {
    annotations;
    order;
    resolve;
    parse_annotation;
    global;
    module_definition;
    class_definition;
    class_representation;
    parent;
  }


let pp format { annotations; _ } =
  let annotation_map_entry (access, annotation) =
    Format.asprintf
      "%a -> %a"
      Access.pp access
      Annotation.pp annotation;
  in
  Map.to_alist annotations
  |> List.map ~f:annotation_map_entry
  |> String.concat ~sep:", "
  |> Format.fprintf format "[%s]"


let show resolution =
  Format.asprintf "%a" pp resolution


let set_local ({ annotations; _ } as resolution) ~access ~annotation =
  { resolution with annotations = Map.set annotations ~key:access ~data:annotation }


let get_local ?(global_fallback=true) ~access { annotations; global; _ } =
  match Map.find annotations access with
  | Some ({ Annotation.annotation; _ } as result) when not (Type.equal annotation Type.Deleted) ->
      Some result
  | _ when global_fallback ->
      Access.delocalize access
      |> global
      >>| Node.value
  | _ ->
      None


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


let parse_annotation { parse_annotation; module_definition; _ } expression =
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
          Identifier.show name
          |> Access.create
          |> fun access -> Module.from_empty_stub ~access ~module_definition
        in
        if originates_from_empty_stub then
          Some Type.Object
        else
          None
    | _ ->
        None
  in
  Type.instantiate parsed ~constraints


let parse_meta_annotation resolution expression =
  match parse_annotation resolution expression with
  | Type.Top ->
      (* Try to resolve meta-types. *)
      let annotation = resolve resolution expression in
      if Type.is_meta annotation then
        Some (Type.single_parameter annotation)
      else
        None
  | annotation ->
      Some annotation


let global { global; _ } =
  global


let module_definition { module_definition; _ } =
  module_definition


let class_definition { class_definition; _ } =
  class_definition


let class_representation { class_representation; _ } =
  class_representation


let less_or_equal { order; _ } =
  TypeOrder.less_or_equal order


let join { order; _ } =
  TypeOrder.join order


let meet { order; _ } =
  TypeOrder.meet order


let widen { order; _ } =
  TypeOrder.widen order


let is_instantiated { order; _ } =
  TypeOrder.is_instantiated order


let is_tracked { order; _ } annotation =
  TypeOrder.contains order annotation


let is_invariance_mismatch { order; _ } ~left ~right =
  match left, right with
  | Type.Parametric { name = left_name; parameters = left_parameters },
    Type.Parametric { name = right_name; parameters = right_parameters }
    when Identifier.equal left_name right_name ->
      let zipped =
        TypeOrder.variables order left
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
            TypeOrder.less_or_equal order ~left ~right
        | _ ->
            false
      in
      zipped
      >>| List.exists ~f:due_to_invariant_variable
      |> Option.value ~default:false
  | _ ->
      false


(* In general, python expressions can be self-referential. This non-recursive resolution only checks
   literals and annotations found in the resolution map, without any resolutions/joins. *)
let rec resolve_literal resolution expression =
  let open Ast.Expression in
  match Node.value expression with
  | Access access ->
      begin
        match Expression.Access.name_and_arguments ~call:access with
        | Some { Expression.Access.callee; _ } ->
            let class_name =
              Expression.Access.create callee
              |> Expression.Access.expression
              |> parse_annotation resolution
            in
            let is_defined =
              class_definition resolution class_name
              |> Option.is_some
            in
            if is_defined then
              class_name
            else
              Type.Top
        | None ->
            Type.Top
      end
  | Await expression ->
      resolve_literal resolution expression
      |> Type.awaitable_value

  | Complex _ ->
      Type.complex

  | False ->
      Type.bool

  | Float _ ->
      Type.float

  | Integer _ ->
      Type.integer

  | String { StringLiteral.kind; _ } ->
      begin
        match kind with
        | StringLiteral.Bytes -> Type.bytes
        | _ -> Type.string
      end

  | True ->
      Type.bool

  | Tuple elements ->
      Type.tuple (List.map elements ~f:(resolve_literal resolution))

  | Expression.Yield _ ->
      Type.yield Type.Top

  | _ ->
      Type.Top
