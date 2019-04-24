(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre

open Ast
open Statement


type global = Annotation.t Node.t
[@@deriving eq, show]


type class_metadata = {
  successors: Type.primitive list;
  is_test: bool;
}


type type_parameters_mismatch = {
  name: string;
  expected_number_of_parameters: int;
  given_number_of_parameters: int;
}


module Cache = struct
  module Generics = struct
    type t = (Type.t list option) Identifier.Table.t

    let cache =
      Identifier.Table.create ()

    let clear () = Hashtbl.clear cache
  end

  let clear () =
    Generics.clear ()
end


type t = {
  annotations: Annotation.t Reference.Map.t;
  type_variables: Type.Set.t;
  order: (module TypeOrder.Handler);

  resolve: resolution: t -> Expression.t -> Type.t;
  aliases: Type.t -> Type.t option;

  global: Reference.t -> global option;
  undecorated_signature: Reference.t -> Type.t Type.Callable.overload option;
  module_definition: Reference.t -> Module.t option;
  class_definition: Type.primitive -> (Class.t Node.t) option;
  class_metadata: Type.primitive -> class_metadata option;
  constructor: (resolution: t -> Type.primitive -> Type.t option);
  generics: resolution: t -> Class.t Node.t -> Type.t list;
  attributes: resolution: t -> Type.t -> AnnotatedAttribute.t list option;
  is_protocol: Type.t -> bool;

  parent: Reference.t option;
}


let create
    ~annotations
    ~order
    ~resolve
    ~aliases
    ~global
    ~module_definition
    ~class_definition
    ~class_metadata
    ~constructor
    ~generics
    ~undecorated_signature
    ~attributes
    ~is_protocol
    ?parent
    () =
  {
    annotations;
    type_variables = Type.Set.empty;
    order;
    resolve;
    aliases;
    global;
    module_definition;
    class_definition;
    class_metadata;
    constructor;
    generics;
    undecorated_signature;
    attributes;
    is_protocol;
    parent;
  }


let pp format { annotations; type_variables; _ } =
  let annotation_map_entry (reference, annotation) =
    Format.asprintf
      "%a -> %a"
      Reference.pp reference
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


let set_local ({ annotations; _ } as resolution) ~reference ~annotation =
  { resolution with annotations = Map.set annotations ~key:reference ~data:annotation }


let get_local ?(global_fallback=true) ~reference { annotations; global; _ } =
  match Map.find annotations reference with
  | Some result ->
      Some result
  | _ when global_fallback ->
      Reference.delocalize reference
      |> global
      >>| Node.value
  | _ ->
      None


let unset_local ({ annotations; _ } as resolution) ~reference =
  { resolution with annotations = Map.remove annotations reference }


let is_global { annotations; global; _ } ~reference =
  match Map.find annotations reference with
  | Some annotation ->
      Annotation.is_global annotation
  | _ ->
      Reference.delocalize reference
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


let primitive_name annotation =
  let primitive, _ = Type.split annotation in
  Type.primitive_name primitive


let class_definition { class_definition; _ } annotation =
  primitive_name annotation
  >>= class_definition


let class_metadata { class_metadata; _ } annotation =
  primitive_name annotation
  >>= class_metadata


let constructor ({ constructor; _ } as resolution) =
  constructor ~resolution


let generics ({ generics; class_definition; _ } as resolution) annotation =
  primitive_name annotation
  >>= fun key ->
  Hashtbl.find_or_add Cache.Generics.cache key ~default:(fun _ ->
      class_definition key >>| generics ~resolution
    )


let attributes ({ attributes; _ } as resolution) =
  attributes ~resolution


let is_protocol { is_protocol; _ } =
  is_protocol


module FunctionDefinitionsCache = struct
  let cache =
    Reference.Table.create ()

  let enabled =
    (* Only enable this in nonincremental mode for now. *)
    ref false

  let enable () =
    enabled := true

  let set key value =
    Hashtbl.set cache ~key ~data:value

  let get key =
    if !enabled then
      Hashtbl.find cache key
    else
      None

  let invalidate () =
    Hashtbl.clear cache
end


let function_definitions resolution reference =
  match FunctionDefinitionsCache.get reference with
  | Some result ->
      result
  | None ->
      let qualifier =
        let rec qualifier ~lead ~tail =
          match tail with
          | head :: tail ->
              let new_lead = Reference.create ~prefix:lead head in
              if Option.is_none (module_definition resolution new_lead) then
                lead
              else
                qualifier ~lead:new_lead ~tail
          | _ ->
              lead
        in
        qualifier ~lead:Reference.empty ~tail:(Reference.as_list reference)
      in
      let result =
        Ast.SharedMemory.Sources.get_for_qualifier qualifier
        >>| Preprocessing.convert
        >>| Preprocessing.defines ~include_stubs:true ~include_nested:true
        >>| List.filter
          ~f:(fun { Node.value = { Define.signature = { name; _ }; _ }; _ } ->
              Reference.equal reference name)
      in
      FunctionDefinitionsCache.set reference result;
      result


let undecorated_signature { undecorated_signature; _ }  =
  undecorated_signature


let full_order ({ order; _ } as resolution) =
  let constructor instantiated =
    instantiated
    |> Type.primitive_name
    >>= constructor resolution
  in
  {
    TypeOrder.handler = order;
    constructor;
    attributes = attributes resolution;
    is_protocol = is_protocol resolution;
    protocol_assumptions = TypeOrder.ProtocolAssumptions.empty;
    any_is_bottom = false;
  }


let solve_less_or_equal resolution ~constraints ~left ~right =
  full_order resolution
  |> TypeOrder.solve_less_or_equal ~constraints ~left ~right


let constraints_solution_exists ~left ~right resolution =
  not (
    solve_less_or_equal resolution ~left ~right ~constraints:TypeConstraints.empty
    |> List.filter_map ~f:(TypeOrder.OrderedConstraints.solve ~order:(full_order resolution))
    |> List.is_empty)


let consistent_solution_exists resolution =
  TypeOrder.consistent_solution_exists (full_order resolution)


let solve_constraints resolution =
  TypeOrder.OrderedConstraints.solve ~order:(full_order resolution)


let partial_solve_constraints resolution =
  TypeOrder.OrderedConstraints.extract_partial_solution ~order:(full_order resolution)


let less_or_equal resolution =
  full_order resolution
  |> TypeOrder.less_or_equal


let is_compatible_with resolution =
  full_order resolution
  |> TypeOrder.is_compatible_with


let join resolution =
  full_order resolution
  |> TypeOrder.join


let meet resolution =
  full_order resolution
  |> TypeOrder.meet


let widen resolution =
  full_order resolution
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
    ~right:(Type.optional (Type.parametric "typing.Mapping" [Type.string; Type.Any]))


let check_invalid_type_parameters resolution annotation =
  let module InvalidTypeParametersTransform = Type.Transform.Make(struct
      type state = type_parameters_mismatch list

      let visit_children_before _ _ = false
      let visit_children_after = true
      let visit sofar annotation =
        let transformed_annotation, new_state =
          let generics_for_name name =
            match name with
            | "type"
            | "typing.Type"
            | "typing.ClassVar"
            | "typing.Iterator"
            | "Optional"
            | "typing.Optional" ->
                [Type.Variable (Type.Variable.create "T")]
            | _ ->
                generics resolution (Type.Primitive name)
                |> Option.value ~default:[]
          in
          let invalid_type_parameters ~name ~given =
            let generics = generics_for_name name in
            if List.length generics = given then
              annotation, sofar
            else
              let mismatch =
                {
                  name;
                  expected_number_of_parameters = List.length generics;
                  given_number_of_parameters = given;
                }
              in
              Type.parametric name (List.map generics ~f:(fun _ -> Type.Any)), mismatch :: sofar
          in
          match annotation with
          | Type.Primitive name ->
              invalid_type_parameters ~name ~given:0
          (* natural variadics *)
          | Type.Parametric { name = "typing.Protocol"; _ }
          | Type.Parametric { name = "typing.Generic"; _ } ->
              annotation, sofar
          | Type.Parametric { name; parameters } ->
              invalid_type_parameters ~name ~given:(List.length parameters)
          | _ ->
              annotation, sofar
        in
        { Type.Transform.transformed_annotation; new_state }
    end)
  in
  InvalidTypeParametersTransform.visit [] annotation


let parse_annotation
    ?(allow_untracked=false)
    ?(allow_invalid_type_parameters=false)
    ({ aliases; module_definition; _ } as resolution)
    expression =
  let expression =
    Expression.delocalize expression
    |> Expression.convert
  in
  let aliases annotation =
    if allow_invalid_type_parameters then
      aliases annotation
    else
      aliases annotation
      >>| check_invalid_type_parameters resolution
      >>| snd
  in
  let parsed = Type.create ~convert:true ~aliases expression in
  let constraints = function
    | Type.Primitive name ->
        let originates_from_empty_stub =
          Reference.create name
          |> fun reference -> Module.from_empty_stub ~reference ~module_definition
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
  else if not allow_invalid_type_parameters then
    check_invalid_type_parameters resolution annotation
    |> snd
  else
    annotation


let parse_reference ?(allow_untracked=false) resolution reference =
  Reference.expression ~convert:true reference
  |> parse_annotation ~allow_untracked ~allow_invalid_type_parameters:true resolution


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
        | Type.Variable { variance = Type.Variable.Invariant; _ } ->
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
  let expression = Expression.convert expression in
  match Node.value expression with
  | Access (SimpleAccess access) ->
      begin
        let is_concrete_constructable class_type =
          generics resolution class_type
          >>| List.is_empty
          |> Option.value ~default:false
        in
        match Expression.Access.name_and_arguments ~call:access with
        | Some { Expression.Access.callee; _ } ->
            let class_type =
              Expression.Access.create callee
              |> Expression.Access.expression
              |> parse_annotation resolution
            in
            if is_concrete_constructable class_type then
              class_type
            else
              Type.Top
        | None ->
            let class_type = parse_annotation resolution expression in
            (* None is a special type that doesn't have a constructor. *)
            if Type.is_none class_type then
              Type.none
            else if is_concrete_constructable class_type then
              Type.meta class_type
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


let weaken_mutable_literals resolution ~expression ~resolved ~expected ~comparator =
  match expression, expected with
  | Some { Node.value = Expression.List _; _ }, _
  | Some { Node.value = Expression.ListComprehension _; _ }, _ ->
      begin
        match resolved, expected with
        | Type.Parametric { name = actual_name; parameters = [actual] },
          Type.Parametric { name = expected_name; parameters = [expected_parameter] }
          when Identifier.equal actual_name "list" &&
               Identifier.equal expected_name "list" &&
               comparator ~left:actual ~right:expected_parameter ->
            expected
        | _ ->
            resolved
      end

  | Some { Node.value = Expression.Set _; _ }, _
  | Some { Node.value = Expression.SetComprehension _; _ }, _ ->
      begin
        match resolved, expected with
        | Type.Parametric { name = actual_name; parameters = [actual] },
          Type.Parametric { name = expected_name; parameters = [expected_parameter] }
          when Identifier.equal actual_name "set" &&
               Identifier.equal expected_name "set" &&
               comparator ~left:actual ~right:expected_parameter ->
            expected
        | _ ->
            resolved
      end

  | Some { Node.value = Expression.Dictionary { entries; keywords = [] }; _},
    Type.TypedDictionary { total; fields; _ } ->
      let resolve_entry { Expression.Dictionary.key; value } =
        let key = resolve resolution key in
        match key with
        | Type.Literal (Type.String name) ->
            let annotation =
              let resolved = resolve resolution value in
              let matching_name { Type.name = expected_name; _ } =
                String.equal name expected_name
              in
              let relax { Type.annotation; _ } =
                if comparator ~left:resolved ~right:annotation then
                  annotation
                else
                  resolved
              in
              List.find fields ~f:matching_name
              >>| relax
              |> Option.value ~default:resolved
            in
            Some { Type.name; annotation }
        | _ ->
            None
      in
      List.map entries ~f:resolve_entry
      |> Option.all
      >>| Type.TypedDictionary.anonymous ~total
      |> Option.value ~default:resolved

  | Some { Node.value = Expression.Dictionary _; _ }, _
  | Some { Node.value = Expression.DictionaryComprehension _; _ }, _ ->
      begin
        match resolved, expected with
        | Type.Parametric { name = actual_name; parameters = [actual_key; actual_value] },
          Type.Parametric {
            name = expected_name;
            parameters = [expected_key; expected_value];
          }
          when Identifier.equal actual_name "dict" &&
               Identifier.equal expected_name "dict" &&
               comparator ~left:actual_key ~right:expected_key &&
               comparator
                 ~left:actual_value
                 ~right:expected_value ->
            expected
        | _ ->
            resolved
      end

  | _ ->
      resolved


let resolve_mutable_literals resolution =
  weaken_mutable_literals
    resolution
    ~comparator:(constraints_solution_exists resolution)


let is_consistent_with resolution left right ~expression =
  let comparator ~left ~right =
    consistent_solution_exists resolution left right
  in
  let left =
    weaken_mutable_literals
      resolution
      ~expression
      ~resolved:left
      ~expected:right
      ~comparator
  in
  comparator ~left ~right
