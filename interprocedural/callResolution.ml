(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Analysis
open Expression
open Pyre

let normalize_global ~resolution reference =
  (* Determine if this is a constructor call, as we need to add the uninitialized object argument
     for self. *)
  let global_type = Resolution.resolve_reference resolution reference in
  if Type.is_meta global_type then
    let dummy_self =
      {
        Expression.Call.Argument.name = None;
        value = Node.create_with_default_location Expression.False;
      }
    in
    let full_reference = Reference.create ~prefix:reference "__init__" in
    full_reference, [dummy_self]
  else
    reference, []


let defining_attribute ~resolution parent_type attribute =
  let global_resolution = Resolution.global_resolution resolution in
  GlobalResolution.class_definition global_resolution parent_type
  >>| Annotated.Class.create
  >>| (fun definition ->
        Annotated.Class.attribute
          ~transitive:true
          definition
          ~resolution:global_resolution
          ~name:attribute
          ~instantiated:parent_type)
  >>= fun attribute -> if Annotated.Attribute.defined attribute then Some attribute else None


let strip_optional t = if Type.is_optional t then Type.optional_value t else t

let rec resolve_ignoring_optional ~resolution expression =
  match Node.value expression with
  | Name (Name.Attribute { base; attribute; _ }) -> (
      let base_type = resolve_ignoring_optional ~resolution base |> strip_optional in
      match defining_attribute ~resolution base_type attribute with
      | Some attribute ->
          Annotated.Attribute.annotation attribute |> Annotation.annotation |> strip_optional
      | None -> Resolution.resolve resolution expression |> strip_optional )
  (* Lookup the base_type for the attribute you were interested in *)
  | _ -> Resolution.resolve resolution expression |> strip_optional


let strip_optional_and_meta t =
  t |> (fun t -> if Type.is_meta t then Type.single_parameter t else t) |> strip_optional


let get_property_defining_parent ~resolution ~base ~attribute =
  let annotation = Resolution.resolve resolution base |> strip_optional_and_meta in
  match defining_attribute ~resolution annotation attribute with
  | Some property when Option.is_some (Annotated.Attribute.property property) ->
      Annotated.Attribute.parent property |> Type.primitive_name >>| Reference.create
  | _ -> None


let is_local identifier = String.is_prefix ~prefix:"$" identifier

let extract_constant_name { Node.value = expression; _ } =
  match expression with
  | String literal -> Some literal.value
  | Integer i -> Some (string_of_int i)
  | Name name -> (
      let name = Expression.name_to_reference name >>| Reference.delocalize >>| Reference.last in
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
  let receiver_type = strip_optional_and_meta receiver_type in
  let declaring_type = Reference.prefix implementation_target >>| get_class_type in
  if declaring_type >>| Type.equal receiver_type |> Option.value ~default:false then (* case a *)
    [get_actual_target implementation_target]
  else
    let target_callable = Callable.create_method implementation_target in
    match DependencyGraphSharedMemory.get_overriding_types ~member:implementation_target with
    | None ->
        (* case b *)
        [target_callable]
    | Some overriding_types -> (
        let keep_subtypes candidate =
          let candidate_type = get_class_type candidate in
          GlobalResolution.less_or_equal
            global_resolution
            ~left:candidate_type
            ~right:receiver_type
        in
        match List.filter overriding_types ~f:keep_subtypes with
        | [] ->
            (* case b *)
            [target_callable]
        | subtypes ->
            (* case c *)
            let method_name = Reference.last implementation_target in
            let create_override_target class_name =
              Reference.create ~prefix:class_name method_name |> get_actual_target
            in
            target_callable :: List.map subtypes ~f:create_override_target )


let rec is_all_names = function
  | Name (Name.Identifier identifier) when not (is_local identifier) -> true
  | Name (Name.Attribute { base; attribute; _ }) when not (is_local attribute) ->
      is_all_names (Node.value base)
  | _ -> false


let resolve_target ~resolution ?receiver_type callee =
  let callable_type = Resolution.resolve resolution callee in
  let global =
    match Expression.get_identifier_base callee, Node.value callee with
    | Some "super", _
    | Some "type", _ ->
        None
    | Some base, Name name when Expression.is_simple_name name && not (is_local base) ->
        let reference = Expression.name_to_reference_exn name in
        let global =
          reference
          |> GlobalResolution.global (Resolution.global_resolution resolution)
          |> Option.is_some
        in
        let is_class =
          match Node.value callee with
          | Name (Name.Attribute { base; _ }) ->
              Resolution.resolve resolution base
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
      | Call { callee = { Node.value = Name (Name.Identifier "super"); _ }; _ } -> true
      | Call { callee; _ } -> is_super callee
      | Name (Name.Attribute { base; _ }) -> is_super base
      | _ -> false
    in
    is_super callee
  in
  let is_all_names = is_all_names (Node.value callee) in
  let rec resolve_type callable_type =
    match callable_type, receiver_type, global with
    | Type.Callable { implicit; kind = Named name; _ }, _, Some _ ->
        [Callable.create_function name, implicit]
    | Type.Callable { implicit; kind = Named name; _ }, _, _ when is_super_call ->
        [Callable.create_method name, implicit]
    | Type.Callable { implicit = None; kind = Named name; _ }, None, _ when is_all_names ->
        [Callable.create_function name, None]
    | Type.Callable { implicit; kind = Named name; _ }, _, _ when is_all_names ->
        [Callable.create_method name, implicit]
    | Type.Callable { implicit; kind = Named name; _ }, Some type_or_class, _ ->
        compute_indirect_targets ~resolution ~receiver_type:type_or_class name
        |> List.map ~f:(fun target -> target, implicit)
    | Type.Union annotations, _, _ -> List.concat_map ~f:resolve_type annotations
    | Type.Optional annotation, _, _ -> resolve_type annotation
    | _, _, _ when Type.is_meta callable_type -> (
        let class_type = Type.single_parameter callable_type in
        match Type.primitive_name class_type with
        | Some class_name when class_name <> "super" ->
            let resolution = Resolution.global_resolution resolution in
            Callable.resolve_method ~resolution ~class_type ~method_name:"__init__"
            >>| (fun callable ->
                  [ ( callable,
                      Some { Type.Callable.implicit_annotation = callable_type; name = "self" } )
                  ])
            |> Option.value ~default:[]
        | _ -> [] )
    | _ -> []
  in
  resolve_type callable_type


let get_indirect_targets ~resolution ~receiver ~method_name =
  let receiver_type = Resolution.resolve resolution receiver in
  let callee =
    Name (Name.Attribute { base = receiver; attribute = method_name; special = false })
    |> Node.create_with_default_location
  in
  resolve_target ~resolution ~receiver_type callee


let resolve_property_targets ~resolution ~base ~attribute =
  let receiver_type = Resolution.resolve resolution base in
  match get_property_defining_parent ~resolution ~base ~attribute with
  | None -> None
  | Some defining_parent when Type.is_meta receiver_type ->
      Some [Callable.create_method (Reference.create ~prefix:defining_parent attribute), None]
  | Some defining_parent ->
      let callee = Reference.create ~prefix:defining_parent attribute in
      compute_indirect_targets ~resolution ~receiver_type callee
      |> List.map ~f:(fun target -> target, None)
      |> Option.some


let get_global_targets ~resolution ~global =
  Name (Expression.create_name_from_reference ~location:Location.Reference.any global)
  |> Node.create_with_default_location
  |> resolve_target ~resolution


let resolve_call_targets ~resolution call =
  let { Expression.Call.callee; _ } =
    Analysis.Annotated.Call.redirect_special_calls ~resolution call
  in
  match Node.value callee with
  | Name (Name.Attribute { base; _ }) ->
      let receiver_type = Resolution.resolve resolution base in
      resolve_target ~resolution ~receiver_type callee
  | Name (Name.Identifier name) when name <> "super" ->
      let receiver_type = Resolution.resolve resolution callee in
      if Type.is_meta receiver_type then
        let callee =
          Name (Name.Attribute { base = callee; attribute = "__init__"; special = false })
          |> Node.create_with_default_location
        in
        resolve_target ~resolution ~receiver_type callee
      else
        resolve_target ~resolution callee
  | _ -> resolve_target ~resolution callee
