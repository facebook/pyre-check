(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open Ast
open Statement
open ClassSummary

type options = {
  init: bool;
  repr: bool;
  eq: bool;
  order: bool;
  match_args: bool;
  field_specifiers: Ast.Expression.t list;
  keyword_only: bool;
  has_slots: bool;
  frozen: bool;
  unsafe_hash: bool;
}

(** This is necessary as an abstraction over AnnotatedAttribute to determine which attributes are
    keyword_only. *)
type 'annotation dataclass_constructor_parameter = {
  name: Identifier.t;
  annotation: 'annotation;
  default: bool;
  keyword_only: bool;
}

let extract_options
    ~default
    ~init
    ~repr
    ~eq
    ~order
    ~keyword_only
    ~has_slots
    ~frozen
    ~unsafe_hash
    decorator
  =
  let open Expression in
  let extract_options_from_arguments =
    let apply_arguments default argument =
      let recognize_value ~default = function
        | Expression.Constant Constant.False -> false
        | Expression.Constant Constant.True -> true
        | _ -> default
      in
      match argument with
      | { Call.Argument.name = Some { Node.value = argument_name; _ }; value = { Node.value; _ } }
        ->
          let argument_name = Identifier.sanitized argument_name in
          (* We need to check each keyword sequentially because different keywords may correspond to
             the same string. *)
          let default =
            if String.equal argument_name init then
              { default with init = recognize_value value ~default:default.init }
            else
              default
          in
          let default =
            if String.equal argument_name repr then
              { default with repr = recognize_value value ~default:default.repr }
            else
              default
          in
          let default =
            if String.equal argument_name eq then
              { default with eq = recognize_value value ~default:default.eq }
            else
              default
          in
          let default =
            if String.equal argument_name order then
              { default with order = recognize_value value ~default:default.order }
            else
              default
          in
          let default =
            if String.equal argument_name "match_args" then
              { default with match_args = recognize_value value ~default:default.match_args }
            else
              default
          in
          let default =
            if String.equal argument_name "field_specifiers" then
              match value with
              | Expression.Tuple field_specifiers -> { default with field_specifiers }
              | _ -> default
            else
              default
          in
          let default =
            if String.equal argument_name keyword_only then
              { default with keyword_only = recognize_value value ~default:default.keyword_only }
            else
              default
          in
          let default =
            if String.equal argument_name has_slots then
              { default with has_slots = recognize_value value ~default:default.has_slots }
            else
              default
          in
          let default =
            if String.equal argument_name frozen then
              { default with frozen = recognize_value value ~default:default.frozen }
            else
              default
          in
          let default =
            if String.equal argument_name unsafe_hash then
              { default with unsafe_hash = recognize_value value ~default:default.unsafe_hash }
            else
              default
          in

          default
      | _ -> default
    in
    List.fold ~init:default ~f:apply_arguments
  in
  match decorator with
  | { Decorator.arguments = Some arguments; _ } -> extract_options_from_arguments arguments
  | _ -> default


let dataclass_options ~first_matching_class_decorator class_summary =
  let field_specifiers =
    [
      Reference.create "dataclasses.field"
      |> Ast.Expression.from_reference ~location:Location.any ~create_origin:(fun _ -> None);
    ]
  in
  first_matching_class_decorator ~names:["dataclasses.dataclass"; "dataclass"] class_summary
  >>| extract_options
        ~default:
          {
            init = true;
            repr = true;
            eq = true;
            order = false;
            match_args = true;
            field_specifiers;
            keyword_only = false;
            has_slots = false;
            frozen = false;
            unsafe_hash = false;
          }
        ~init:"init"
        ~repr:"repr"
        ~eq:"eq"
        ~order:"order"
        ~keyword_only:"kw_only"
        ~has_slots:"slots"
        ~frozen:"frozen"
        ~unsafe_hash:"unsafe_hash"


let attrs_attributes ~first_matching_class_decorator class_summary =
  first_matching_class_decorator ~names:["attr.s"; "attr.attrs"] class_summary
  >>| extract_options
        ~default:
          {
            init = true;
            repr = true;
            eq = true;
            order = true;
            match_args = false;
            field_specifiers = [];
            keyword_only = false;
            has_slots = false;
            frozen = false;
            unsafe_hash = false;
          }
        ~init:"init"
        ~repr:"repr"
        ~eq:"cmp"
        ~order:"cmp"
        ~keyword_only:"kw_only"
        ~has_slots:"slots"
        ~frozen:"frozen"
        ~unsafe_hash:"unsafe_hash"


(* Is a decorator one of the spec-defined dataclass transform decorators.

   This does *not* answer whether a decorator is a custom dataclass transform, it rather asks
   whether a decorator (which will be applied either to a decorator or a class, for either
   decorator- or base-class style dataclass transforms) is marking a custom dataclass transform
   definition. *)
let is_dataclass_transform decorator_expression =
  match Decorator.from_expression decorator_expression with
  | None -> false
  | Some { Decorator.name = { Node.value = decorator_reference; _ }; _ } -> (
      match Reference.show decorator_reference with
      | "typing.dataclass_transform"
      | "typing_extensions.dataclass_transform" ->
          true
      | _ -> String.equal (Reference.last decorator_reference) "__dataclass_transform__")


(* Determine based on the use of one of the spec-defined dataclass transform decorators what the
   default options are for the custom transform being defined.

   Here we are looking at the use of `@dataclass_transform` itself when *defining* a dataclass
   transform, not to the use of the custom transform thus defined; that's handled downstream. *)
let dataclass_transform_default_options decorator =
  let default_options =
    {
      init = true;
      repr = false;
      eq = true;
      order = false;
      match_args = false;
      field_specifiers = [];
      keyword_only = false;
      has_slots = false;
      frozen = false;
      unsafe_hash = false;
    }
  in
  extract_options
    ~default:default_options
    ~init:""
    ~repr:""
    ~eq:"eq_default"
    ~order:"order_default"
    ~keyword_only:"kw_only_default"
    ~has_slots:"slots"
    ~frozen:"frozen_default"
    ~unsafe_hash:""
    decorator


(* Given a particular use of a custom dataclass transform decorator, determine the use-specific
   options. The defaults come from where the custom transform was defined, and were produced by
   running `extract_dataclass_transform_default_options` above.

   This requires knowing the default options for the custom dataclass transform being used, which
   will be determined by `extract_dataclass_transform_default_options` above.

   It determines the options based the use of the custom dataclass. It accepts that use as a
   `Decorator.t` which is suitable for decorator-style dataclass transforms; we use an adapter for
   base-class style transforms. *)
let dataclass_transform_options_from_decorator decorator default_options_for_custom_transform =
  extract_options
    ~default:default_options_for_custom_transform
    ~init:"init"
    ~repr:"repr"
    ~eq:"eq"
    ~order:"order"
    ~keyword_only:"kw_only"
    ~has_slots:"slots"
    ~frozen:"frozen"
    ~unsafe_hash:"unsafe_hash"
    decorator


(* Check all decorators of a class to see if any of them is a custom dataclass transform. If so,
   determine the options specified by that decorator (which will depend on both the default options
   defined for the custom transform and any options defined in this specific use).

   This function only handles "decorator-style" dataclass transforms that work like the `@dataclass`
   decorator itself. Transforms defined as base classes are handled separately in
   `options_from_custom_dataclass_transform_base_class_or_metaclass`. *)
let options_from_custom_dataclass_transform_decorator
    ~get_unannotated_global
    { Node.value = { ClassSummary.decorators; _ }; _ }
  =
  let get_dataclass_transform_decorator_with_default_options decorator =
    let { Decorator.name = { Node.value = decorator_reference; _ }; _ } = decorator in
    get_unannotated_global decorator_reference
    >>= function
    | Module.UnannotatedGlobal.Define signatures ->
        (* Grab the implementation signature, which will be the last definition if there are
           overloads. *)
        let { Module.UnannotatedGlobal.signature = { Define.Signature.decorators; _ }; _ } =
          List.last_exn signatures
        in

        (* Determine whether any decorators are marking this function as a dataclass transform *)
        List.find decorators ~f:is_dataclass_transform
        >>= Decorator.from_expression
        >>| dataclass_transform_default_options
        >>| fun default_options_for_custom_transform ->
        dataclass_transform_options_from_decorator decorator default_options_for_custom_transform
    | _ -> None
  in
  decorators
  |> List.filter_map ~f:Decorator.from_expression
  |> List.find_map ~f:get_dataclass_transform_decorator_with_default_options


let get_dataclass_options_from_metaclass
    name
    get_dataclass_transform_default
    successors
    get_class_summary
  =
  let get_metaclass get_class_summary name =
    let class_summary_option = get_class_summary name in
    Option.bind
      ~f:(fun node -> node |> Node.value |> fun class_summary -> class_summary.bases.metaclass)
      class_summary_option
  in
  let metaclass_and_successor_metaclasses = name :: successors name in
  List.find_map
    ~f:(fun name ->
      get_metaclass get_class_summary name
      |> Option.map ~f:Expression.show
      |> Option.bind ~f:get_dataclass_transform_default)
    metaclass_and_successor_metaclasses


let options_from_custom_dataclass_transform_base_class_or_metaclass
    ~get_class_summary
    ~successors
    { Node.value = { ClassSummary.name; bases = { init_subclass_arguments; _ }; _ }; _ }
  =
  let get_dataclass_transform_default name =
    let class_decorators { ClassSummary.decorators; _ } = decorators in
    get_class_summary name
    >>| Node.value
    >>| class_decorators
    >>= List.find ~f:is_dataclass_transform
    >>= Decorator.from_expression
    >>| dataclass_transform_default_options
  in
  let metaclass_option =
    get_dataclass_options_from_metaclass
      (Reference.show name)
      get_dataclass_transform_default
      successors
      get_class_summary
  in
  let successor_option =
    Reference.show name |> successors |> List.find_map ~f:get_dataclass_transform_default
  in
  let default_options_for_custom_transform =
    match metaclass_option, successor_option with
    | _, Some successor_default_options -> Some successor_default_options
    | Some metaclass_default_options, _ -> Some metaclass_default_options
    | None, None -> None
  in
  Option.map
    ~f:(fun default_options_for_custom_transform ->
      (* For historical reasons, extracting options is done by processing decorator callsites. But
         base class options are instead defined via keyword arguments to the class definition
         itself, which become `init_subclass_options`. Here we construct a "decorator" so we can use
         the preexisting option extraction. *)
      let synthetic_decorator =
        {
          Decorator.name = Node.create_with_default_location name;
          arguments = Some init_subclass_arguments;
          original_expression =
            Decorator.create_original_expression
              ~create_origin_for_reference:(fun _ -> None)
              ~call_origin:None
              ~name:(Node.create_with_default_location name)
              ~arguments:(Some init_subclass_arguments);
        }
      in
      dataclass_transform_options_from_decorator
        synthetic_decorator
        default_options_for_custom_transform)
    default_options_for_custom_transform
