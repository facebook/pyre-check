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

let strip_optional annotation = Type.optional_value annotation |> Option.value ~default:annotation

let strip_optional_and_meta t =
  t |> (fun t -> if Type.is_meta t then Type.single_parameter t else t) |> strip_optional


let get_property_defining_parent ~resolution ~base ~attribute =
  let annotation =
    Resolution.resolve_expression_to_type resolution base |> strip_optional_and_meta
  in
  match CallGraph.defining_attribute ~resolution annotation attribute with
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


let rec is_all_names = function
  | Expression.Name (Name.Identifier identifier) when not (is_local identifier) -> true
  | Name (Name.Attribute { base; attribute; _ }) when not (is_local attribute) ->
      is_all_names (Node.value base)
  | _ -> false


let resolve_target ~resolution ?receiver_type callee =
  let callable_type = CallGraph.resolve_ignoring_optional ~resolution callee in
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
              CallGraph.resolve_ignoring_optional ~resolution base
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
  let resolve_lru_cache ~implementing_class =
    match implementing_class, Node.value callee with
    | Type.Top, Name name when is_all_names ->
        (* If implementing_class is unknown, this must be a function rather than a method. We can
           use global resolution on the callee. *)
        GlobalResolution.global
          (Resolution.global_resolution resolution)
          (Ast.Expression.name_to_reference_exn name)
        >>= (fun { AttributeResolution.Global.undecorated_signature; _ } ->
              Some (undecorated_signature, None))
        |> Option.value ~default:(None, None)
    | _ -> (
        let implementing_class_name =
          if Type.is_meta implementing_class then
            Type.parameters implementing_class
            >>= fun parameters ->
            List.nth parameters 0
            >>= function
            | Single implementing_class -> Some implementing_class
            | _ -> None
          else
            Some implementing_class
        in
        match implementing_class_name with
        | Some implementing_class_name ->
            let class_primitive =
              match implementing_class_name with
              | Parametric { name; _ } -> Some name
              | Primitive name -> Some name
              | _ -> None
            in
            let method_name =
              match Node.value callee with
              | Expression.Name (Name.Attribute { attribute; _ }) -> Some attribute
              | _ -> None
            in
            method_name
            >>= (fun method_name ->
                  class_primitive
                  >>| fun class_name -> Format.sprintf "%s.%s" class_name method_name)
            >>| Reference.create
            (* Here, we blindly reconstruct the callable instead of going through the global
               resolution, as Pyre doesn't have an API to get the undecorated signature of methods. *)
            >>| (fun name ->
                  ( Some
                      {
                        Type.Callable.kind = Named name;
                        implementation =
                          { annotation = Type.Any; parameters = Type.Callable.Defined [] };
                        overloads = [];
                      },
                    Some implementing_class ))
            |> Option.value ~default:(None, None)
        | _ -> None, None )
  in
  let rec resolve_type ?(callable_class_type = None) callable_type =
    let underlying_callable, self_argument =
      match callable_type with
      | Type.Callable underlying_callable -> Some underlying_callable, None
      (* For the case of the LRU cache decorator, the type system loses the callable information as
         it creates a wrapper class. We reconstruct the underlying callable using the implicit. *)
      | Parametric
          {
            name = "BoundMethod";
            parameters =
              [
                Single (Parametric { name = "functools._lru_cache_wrapper"; _ });
                Single implementing_class;
              ];
          } ->
          resolve_lru_cache ~implementing_class
      | Parametric { name = "functools._lru_cache_wrapper"; _ } -> (
          (* Because of the special class, we don't get a bound method & lose the self argument for
             non-classmethod LRU cache wrappers. Reconstruct self in this case. *)
          match Node.value callee with
          | Expression.Name (Name.Attribute { base; _ }) ->
              CallGraph.resolve_ignoring_optional ~resolution base
              |> fun implementing_class -> resolve_lru_cache ~implementing_class
          | _ -> None, None )
      | Parametric
          {
            name = "BoundMethod";
            parameters = [Single (Callable underlying_callable); Single self_argument];
          } ->
          Some underlying_callable, Some self_argument
      | _ -> None, None
    in
    match underlying_callable, self_argument, callable_type, receiver_type, global with
    | Some { kind = Named name; _ }, _, _, _, _ when Option.is_some callable_class_type ->
        [Callable.create_method name, callable_class_type]
    | Some { kind = Anonymous; _ }, _, _, _, _ when Option.is_some callable_class_type -> (
        (* TODO(T66895305): Callable protocols don't retain the name of the callable, so we special
           case them here. *)
        match callable_class_type >>= Type.primitive_name with
        | Some class_name ->
            [`Method { Callable.class_name; method_name = "__call__" }, callable_class_type]
        | None -> [] )
    | Some { kind = Named name; _ }, self_argument, _, _, Some _ ->
        [Callable.create_function name, self_argument]
    | Some { kind = Named name; _ }, self_argument, _, _, _ when is_super_call ->
        [Callable.create_method name, self_argument]
    | Some { kind = Named name; _ }, None, _, None, _ when is_all_names ->
        [Callable.create_function name, None]
    | Some { kind = Named name; _ }, self_argument, _, _, _ when is_all_names ->
        [Callable.create_method name, self_argument]
    | Some { kind = Named name; _ }, self_argument, _, Some type_or_class, _ ->
        CallGraph.compute_indirect_targets ~resolution ~receiver_type:type_or_class name
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
    (* Handle callable classes. `typing.Type` interacts specially with __call__, so we choose to
       ignore it for now to make sure our constructor logic via `cls()` still works. *)
    | _, _, (Type.Primitive _ | Type.Parametric _), _, _ when not (Type.is_meta callable_type) -> (
        let callable_class_type = callable_type in
        match
          Resolution.resolve_attribute_access
            resolution
            ~base_type:callable_type
            ~attribute:"__call__"
        with
        | Type.Any
        | Type.Top ->
            []
        | callable_type ->
            resolve_type ~callable_class_type:(Some callable_class_type) callable_type )
    | _ -> []
  in
  resolve_type callable_type


let get_indirect_targets ~resolution ~receiver ~method_name =
  let receiver_type = CallGraph.resolve_ignoring_optional ~resolution receiver in
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
        let receiver_type = CallGraph.resolve_ignoring_optional ~resolution base in
        if Type.is_meta receiver_type then
          [Callable.create_method (Reference.create ~prefix:defining_parent attribute), None]
        else
          let callee = Reference.create ~prefix:defining_parent attribute in
          CallGraph.compute_indirect_targets ~resolution ~receiver_type callee
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


type target = Callable.t * Type.t option [@@deriving show, eq]

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
