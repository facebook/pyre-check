(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Analysis
open Expression
open Pyre

let defining_attribute ~resolution parent_type attribute =
  let global_resolution = Resolution.global_resolution resolution in
  Type.split parent_type
  |> fst
  |> Type.primitive_name
  >>= fun class_name ->
  GlobalResolution.attribute_from_class_name
    ~transitive:true
    ~resolution:global_resolution
    ~name:attribute
    ~instantiated:parent_type
    class_name
  >>= fun instantiated_attribute ->
  if Annotated.Attribute.defined instantiated_attribute then
    Some instantiated_attribute
  else
    Resolution.fallback_attribute ~resolution ~name:attribute class_name


let strip_optional annotation = Type.optional_value annotation |> Option.value ~default:annotation

let rec resolve_ignoring_optional ~resolution expression =
  let annotation =
    match Node.value expression with
    | Expression.Name (Name.Attribute { base; attribute; _ }) -> (
        let base_type = resolve_ignoring_optional ~resolution base |> strip_optional in
        match defining_attribute ~resolution base_type attribute with
        | Some _ -> Resolution.resolve_attribute_access resolution ~base_type ~attribute
        | None -> Resolution.resolve_expression_to_type resolution expression
        (* Lookup the base_type for the attribute you were interested in *) )
    | _ -> Resolution.resolve_expression_to_type resolution expression
  in
  strip_optional annotation


let strip_optional_and_meta t =
  t |> (fun t -> if Type.is_meta t then Type.single_parameter t else t) |> strip_optional


let get_property_defining_parent ~resolution ~base ~attribute =
  let annotation =
    Resolution.resolve_expression_to_type resolution base |> strip_optional_and_meta
  in
  match defining_attribute ~resolution annotation attribute with
  | Some property when Annotated.Attribute.property property ->
      Annotated.Attribute.parent property |> Reference.create |> Option.some
  | _ -> None


let is_local identifier = String.is_prefix ~prefix:"$" identifier

let extract_constant_name { Node.value = expression; _ } =
  match expression with
  | Expression.String literal -> Some literal.value
  | Integer i -> Some (string_of_int i)
  | Name name -> (
      let name = name_to_reference name >>| Reference.delocalize >>| Reference.last in
      match name with
      (* Heuristic: All uppercase names tend to be enums, so only taint the field in those cases. *)
      | Some name
        when String.for_all name ~f:(fun character ->
                 (not (Char.is_alpha character)) || Char.is_uppercase character) ->
          Some name
      | _ -> None )
  | _ -> None


(* Figure out what target to pick for an indirect call that resolves to implementation_target.
   E.g., if the receiver type is A, and A derives from Base, and the target is Base.method, then
   targetting the override tree of Base.method is wrong, as it would include all siblings for A.

 * Instead, we have the following cases:
 * a) receiver type matches implementation_target's declaring type -> override implementation_target
 * b) no implementation_target override entries are subclasses of A -> real implementation_target
 * c) some override entries are subclasses of A -> search upwards for actual implementation,
 *    and override all those where the override name is
 *  1) the override target if it exists in the override shared mem
 *  2) the real target otherwise
 *)
let compute_indirect_targets ~resolution ~receiver_type implementation_target =
  (* Target name must be the resolved implementation target *)
  let global_resolution = Resolution.global_resolution resolution in
  let get_class_type = GlobalResolution.parse_reference global_resolution in
  let get_actual_target method_name =
    if DependencyGraphSharedMemory.overrides_exist method_name then
      Callable.create_override method_name
    else
      Callable.create_method method_name
  in
  let receiver_type = strip_optional_and_meta receiver_type |> Type.weaken_literals in
  let declaring_type = Reference.prefix implementation_target in
  if
    declaring_type
    >>| Reference.equal (Type.class_name receiver_type)
    |> Option.value ~default:false
  then (* case a *)
    [get_actual_target implementation_target]
  else
    let target_callable = Callable.create_method implementation_target in
    match DependencyGraphSharedMemory.get_overriding_types ~member:implementation_target with
    | None ->
        (* case b *)
        [target_callable]
    | Some overriding_types ->
        (* case c *)
        let keep_subtypes candidate =
          let candidate_type = get_class_type candidate in
          GlobalResolution.less_or_equal global_resolution ~left:candidate_type ~right:receiver_type
        in
        let override_targets =
          let create_override_target class_name =
            let method_name = Reference.last implementation_target in
            Reference.create ~prefix:class_name method_name |> get_actual_target
          in
          List.filter overriding_types ~f:keep_subtypes
          |> fun subtypes -> List.map subtypes ~f:create_override_target
        in
        target_callable :: override_targets


let rec is_all_names = function
  | Expression.Name (Name.Identifier identifier) when not (is_local identifier) -> true
  | Name (Name.Attribute { base; attribute; _ }) when not (is_local attribute) ->
      is_all_names (Node.value base)
  | _ -> false


let resolve_target ~resolution ?receiver_type callee =
  let callable_type = resolve_ignoring_optional ~resolution callee in
  let global =
    match get_identifier_base callee, Node.value callee with
    | Some "super", _
    | Some "type", _ ->
        None
    | Some base, Name name when is_simple_name name && not (is_local base) ->
        let reference = name_to_reference_exn name in
        let global =
          reference
          |> GlobalResolution.global (Resolution.global_resolution resolution)
          |> Option.is_some
        in
        let is_class =
          match Node.value callee with
          | Name (Name.Attribute { base; _ }) ->
              resolve_ignoring_optional ~resolution base
              |> GlobalResolution.class_definition (Resolution.global_resolution resolution)
              |> Option.is_some
          | _ -> false
        in
        Option.some_if (global && not is_class) reference
    | _ -> None
  in
  let is_super_call =
    let rec is_super callee =
      match Node.value callee with
      | Expression.Call { callee = { Node.value = Name (Name.Identifier "super"); _ }; _ } -> true
      | Call { callee; _ } -> is_super callee
      | Name (Name.Attribute { base; _ }) -> is_super base
      | _ -> false
    in
    is_super callee
  in
  let is_all_names = is_all_names (Node.value callee) in
  let is_local_variable =
    match Node.value callee with
    | Expression.Name (Name.Identifier name) -> is_local name
    | _ -> false
  in
  let rec resolve_type callable_type =
    let underlying_callable, self_argument =
      match callable_type with
      | Type.Callable underlying_callable -> Some underlying_callable, None
      | Parametric
          {
            name = "BoundMethod";
            parameters = [Single (Callable underlying_callable); Single self_argument];
          } ->
          Some underlying_callable, Some self_argument
      | _ -> None, None
    in
    match underlying_callable, self_argument, callable_type, receiver_type, global with
    | Some { kind = Named name; _ }, self_argument, _, _, Some _ ->
        [Callable.create_function name, self_argument]
    | Some { kind = Named name; _ }, self_argument, _, _, _ when is_super_call ->
        [Callable.create_method name, self_argument]
    | Some { kind = Named name; _ }, None, _, None, _ when is_all_names ->
        [Callable.create_function name, None]
    | Some { kind = Named name; _ }, self_argument, _, _, _ when is_all_names ->
        [Callable.create_method name, self_argument]
    | Some { kind = Named name; _ }, self_argument, _, Some type_or_class, _ ->
        compute_indirect_targets ~resolution ~receiver_type:type_or_class name
        |> List.map ~f:(fun target -> target, self_argument)
    | _, _, Type.Union annotations, _, _ -> List.concat_map ~f:resolve_type annotations
    | Some { kind = Named name; _ }, _, _, _, _ when is_local_variable -> (
        match self_argument with
        | Some _ -> [Callable.create_method name, self_argument]
        | None -> [Callable.create_function name, None] )
    | _
      when is_all_names
           && Set.mem SpecialCallResolution.recognized_callable_target_types callable_type -> (
        let name =
          Node.value callee
          |> (function
               | Name name -> Some name
               | _ -> None)
          >>= Ast.Expression.name_to_reference
        in
        match name with
        | Some name -> [Callable.create_function name, None]
        | _ -> [] )
    | _ -> []
  in
  resolve_type callable_type


let get_indirect_targets ~resolution ~receiver ~method_name =
  let receiver_type = resolve_ignoring_optional ~resolution receiver in
  let callee =
    Expression.Name (Name.Attribute { base = receiver; attribute = method_name; special = false })
    |> Node.create_with_default_location
  in
  let indirect_targets = resolve_target ~resolution ~receiver_type callee in
  match indirect_targets with
  | (_, None) :: _ -> indirect_targets, None
  | _ -> indirect_targets, Some { Call.Argument.name = None; value = receiver }


let resolve_property_targets ~resolution ~base ~attribute ~setter =
  match get_property_defining_parent ~resolution ~base ~attribute with
  | None -> None
  | Some defining_parent ->
      let targets =
        let receiver_type = resolve_ignoring_optional ~resolution base in
        if Type.is_meta receiver_type then
          [Callable.create_method (Reference.create ~prefix:defining_parent attribute), None]
        else
          let callee = Reference.create ~prefix:defining_parent attribute in
          compute_indirect_targets ~resolution ~receiver_type callee
          |> List.map ~f:(fun target -> target, None)
      in
      if setter then
        let to_setter (target, implicit) =
          let target =
            match target with
            | `OverrideTarget { Callable.class_name; method_name } ->
                `OverrideTarget { Callable.class_name; method_name = method_name ^ "$setter" }
            | `Method { Callable.class_name; method_name } ->
                `Method { Callable.class_name; method_name = method_name ^ "$setter" }
            | _ -> target
          in
          target, implicit
        in
        List.map targets ~f:to_setter |> Option.some
      else
        Some targets


let resolve_call_targets ~resolution call =
  let { Call.callee; _ } = Analysis.Annotated.Call.redirect_special_calls ~resolution call in
  match Node.value callee with
  | Name (Name.Attribute { base; _ }) ->
      let receiver_type = resolve_ignoring_optional ~resolution base in
      resolve_target ~resolution ~receiver_type callee
  | Name (Name.Identifier name) when not (String.equal name "super") ->
      let receiver_type = resolve_ignoring_optional ~resolution callee in
      if Type.is_meta receiver_type then
        let callee =
          Expression.Name
            (Name.Attribute { base = callee; attribute = "__init__"; special = false })
          |> Node.create_with_default_location
        in
        resolve_target ~resolution ~receiver_type callee
      else
        resolve_target ~resolution callee
  | _ -> resolve_target ~resolution callee


type target = Callable.t * Type.t option

type constructor_targets = {
  new_targets: target list;
  init_targets: target list;
}

let get_constructor_targets ~resolution ~receiver =
  let new_targets =
    let targets, _ = get_indirect_targets ~resolution ~receiver ~method_name:"__new__" in
    let keep = function
      | `Method { Callable.class_name = "object"; _ }, _ -> false
      | _ -> true
    in
    List.filter targets ~f:keep
  in
  let init_targets, _ = get_indirect_targets ~resolution ~receiver ~method_name:"__init__" in
  { new_targets; init_targets }


type global_targets =
  | ConstructorTargets of {
      constructor_targets: constructor_targets;
      callee: Expression.t;
    }
  | GlobalTargets of target list

let get_global_targets ~resolution reference =
  let callee =
    Expression.Name (create_name_from_reference ~location:Location.any reference)
    |> Node.create_with_default_location
  in
  (* Determine if this is a constructor call, as we need to add the uninitialized object argument
     for self. *)
  let global_type = Resolution.resolve_reference resolution reference in
  if Type.is_meta global_type then
    ConstructorTargets
      { constructor_targets = get_constructor_targets ~resolution ~receiver:callee; callee }
  else
    GlobalTargets (resolve_target ~resolution callee)


let transform_special_calls ~resolution { Call.callee; arguments } =
  match callee, arguments with
  | ( {
        Node.value =
          Expression.Name
            (Name.Attribute
              {
                base = { Node.value = Expression.Name (Name.Identifier "functools"); _ };
                attribute = "partial";
                _;
              });
        _;
      },
      { Call.Argument.value = actual_callable; _ } :: actual_arguments ) ->
      Some { Call.callee = actual_callable; arguments = actual_arguments }
  | ( {
        Node.value =
          Name
            (Name.Attribute
              {
                base = { Node.value = Expression.Name (Name.Identifier "multiprocessing"); _ };
                attribute = "Process";
                _;
              });
        _;
      },
      [
        { Call.Argument.value = process_callee; name = Some { Node.value = "$parameter$target"; _ } };
        {
          Call.Argument.value = { Node.value = Expression.Tuple process_arguments; _ };
          name = Some { Node.value = "$parameter$args"; _ };
        };
      ] ) ->
      Some
        {
          Call.callee = process_callee;
          arguments =
            List.map process_arguments ~f:(fun value -> { Call.Argument.value; name = None });
        }
  | _ -> SpecialCallResolution.redirect ~resolution { Call.callee; arguments }


let redirect_special_calls ~resolution call =
  match transform_special_calls ~resolution call with
  | Some call -> call
  | None -> Annotated.Call.redirect_special_calls ~resolution call
