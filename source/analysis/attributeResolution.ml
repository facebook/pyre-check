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
open Assumptions
open ClassSummary

module Global = struct
  type t = {
    annotation: Annotation.t;
    undecorated_signature: Type.Callable.t option;
    problem: AnnotatedAttribute.problem option;
  }
  [@@deriving show, compare, sexp]
end

module UninstantiatedAnnotation = struct
  type property_annotation = {
    self: Type.t option;
    value: Type.t option;
  }
  [@@deriving compare]

  type kind =
    | Attribute of Type.t
    | Property of {
        getter: property_annotation;
        setter: property_annotation option;
      }
  [@@deriving compare]

  type t = {
    accessed_via_metaclass: bool;
    kind: kind;
  }
  [@@deriving compare]
end

type uninstantiated = UninstantiatedAnnotation.t

type uninstantiated_attribute = uninstantiated AnnotatedAttribute.t

type resolved_define = {
  undecorated_signature: Type.Callable.t;
  decorated: (Type.t, AnnotatedAttribute.problem) Result.t;
}

module Argument = struct
  type t = {
    expression: Expression.t option;
    kind: Ast.Expression.Call.Argument.kind;
    resolved: Type.t;
  }

  module WithPosition = struct
    type t = {
      position: int;
      expression: Expression.t option;
      kind: Ast.Expression.Call.Argument.kind;
      resolved: Type.t;
    }
    [@@deriving compare, show]
  end
end

type matched_argument =
  | MatchedArgument of {
      argument: Argument.WithPosition.t;
      index_into_starred_tuple: int option;
    }
  | Default
[@@deriving compare, show]

let make_matched_argument ?index_into_starred_tuple argument =
  MatchedArgument { argument; index_into_starred_tuple }


type ranks = {
  arity: int;
  annotation: int;
  position: int;
}
[@@deriving compare, show]

type reasons = {
  arity: SignatureSelectionTypes.reason list;
  annotation: SignatureSelectionTypes.reason list;
}
[@@deriving compare, show]

type extracted_ordered_type = {
  ordered_type: Type.OrderedTypes.t;
  argument: Argument.WithPosition.t;
  item_type_for_error: Type.t;
}

let location_insensitive_compare_reasons
    { arity = left_arity; annotation = left_annotation }
    { arity = right_arity; annotation = right_annotation }
  =
  match
    List.compare SignatureSelectionTypes.location_insensitive_compare_reason left_arity right_arity
  with
  | x when not (Int.equal x 0) -> x
  | _ ->
      List.compare
        SignatureSelectionTypes.location_insensitive_compare_reason
        left_annotation
        right_annotation


let empty_reasons = { arity = []; annotation = [] }

module ParameterArgumentMapping = struct
  type t = {
    parameter_argument_mapping: matched_argument list Type.Callable.Parameter.Map.t;
    reasons: reasons;
  }
  [@@deriving compare]

  let pp format { parameter_argument_mapping; reasons } =
    Format.fprintf
      format
      "ParameterArgumentMapping { parameter_argument_mapping: %s; reasons: %a }"
      ([%show: (Type.Callable.Parameter.parameter * matched_argument list) list]
         (Map.to_alist parameter_argument_mapping))
      pp_reasons
      reasons
end

type signature_match = {
  callable: Type.Callable.t;
  parameter_argument_mapping: matched_argument list Type.Callable.Parameter.Map.t;
  constraints_set: TypeConstraints.t list;
  ranks: ranks;
  reasons: reasons;
}
[@@deriving compare]

let pp_signature_match
    format
    { callable; parameter_argument_mapping; constraints_set; ranks; reasons }
  =
  Format.fprintf
    format
    "{ callable = %a; parameter_argument_mapping = %s; constraints_set = %s; ranks = %a; reasons = \
     %a }"
    Type.Callable.pp
    callable
    ([%show: (Type.Callable.Parameter.parameter * matched_argument list) list]
       (Map.to_alist parameter_argument_mapping))
    ([%show: TypeConstraints.t list] constraints_set)
    pp_ranks
    ranks
    pp_reasons
    reasons


let show_signature_match = Format.asprintf "%a" pp_signature_match

let create_uninstantiated_method ?(accessed_via_metaclass = false) callable =
  { UninstantiatedAnnotation.accessed_via_metaclass; kind = Attribute (Callable callable) }


module UninstantiatedAttributeTable = struct
  type element = UninstantiatedAnnotation.t AnnotatedAttribute.t [@@deriving compare]

  type table = (string, element) Caml.Hashtbl.t

  type t = {
    attributes: table;
    names: string list ref;
  }

  let create () = { attributes = Caml.Hashtbl.create 15; names = ref [] }

  let add { attributes; names } attribute =
    let name = AnnotatedAttribute.name attribute in
    if Caml.Hashtbl.mem attributes name then
      ()
    else (
      Caml.Hashtbl.add attributes name attribute;
      names := name :: !names)


  let mark_as_implicitly_initialized_if_uninitialized { attributes; _ } name =
    let is_uninitialized attribute =
      match AnnotatedAttribute.initialized attribute with
      | NotInitialized -> true
      | _ -> false
    in
    match Caml.Hashtbl.find_opt attributes name with
    | Some attribute when is_uninitialized attribute ->
        AnnotatedAttribute.with_initialized ~initialized:OnlyOnInstance attribute
        |> Caml.Hashtbl.replace attributes name
    | _ -> ()


  let lookup_name { attributes; _ } = Caml.Hashtbl.find_opt attributes

  let to_list { attributes; names } = List.rev_map !names ~f:(Caml.Hashtbl.find attributes)

  let names { names; _ } = !names

  let compare ({ names = left_names; _ } as left) ({ names = right_names; _ } as right) =
    let left_names = !left_names in
    let right_names = !right_names in
    match List.compare String.compare left_names right_names with
    | 0 ->
        let rec compare_elements = function
          | [] -> 0
          | name :: names -> (
              match
                Option.compare compare_element (lookup_name left name) (lookup_name right name)
              with
              | 0 -> compare_elements names
              | nonzero -> nonzero)
        in
        compare_elements left_names
    | nonzero -> nonzero
end

(* These modules get included at the bottom of this file, they're just here for aesthetic purposes *)
module TypeParameterValidationTypes = struct
  type generic_type_problems =
    | IncorrectNumberOfParameters of {
        actual: int;
        expected: int;
        can_accept_more_parameters: bool;
      }
    | ViolateConstraints of {
        actual: Type.t;
        expected: Type.Variable.Unary.t;
      }
    | UnexpectedKind of {
        actual: Type.Parameter.t;
        expected: Type.Variable.t;
      }
  [@@deriving compare, sexp, show, hash]

  type type_parameters_mismatch = {
    name: string;
    kind: generic_type_problems;
  }
  [@@deriving compare, sexp, show, hash]
end

let class_hierarchy_environment class_metadata_environment =
  ClassMetadataEnvironment.ReadOnly.class_hierarchy_environment class_metadata_environment


let alias_environment class_metadata_environment =
  ClassHierarchyEnvironment.ReadOnly.alias_environment
    (class_hierarchy_environment class_metadata_environment)


let empty_stub_environment class_metadata_environment =
  alias_environment class_metadata_environment |> AliasEnvironment.ReadOnly.empty_stub_environment


let unannotated_global_environment class_metadata_environment =
  alias_environment class_metadata_environment
  |> AliasEnvironment.ReadOnly.unannotated_global_environment


let class_summary class_metadata_environment annotation ~dependency =
  Type.split annotation
  |> fst
  |> Type.primitive_name
  >>= UnannotatedGlobalEnvironment.ReadOnly.get_class_summary
        (unannotated_global_environment class_metadata_environment)
        ?dependency


let aliases class_metadata_environment ~dependency =
  AliasEnvironment.ReadOnly.get_alias ?dependency (alias_environment class_metadata_environment)


let is_suppressed_module class_metadata_environment ~dependency reference =
  EmptyStubEnvironment.ReadOnly.from_empty_stub
    (empty_stub_environment class_metadata_environment)
    ?dependency
    reference


let is_final_class class_metadata_environment ~dependency class_name =
  match
    ClassMetadataEnvironment.ReadOnly.get_class_metadata
      ?dependency
      class_metadata_environment
      class_name
  with
  | Some { ClassMetadataEnvironment.is_final; _ } -> is_final
  | _ -> false


let class_name { Node.value = { ClassSummary.name; _ }; _ } = name

module ClassDecorators = struct
  type options = {
    init: bool;
    repr: bool;
    eq: bool;
    order: bool;
    match_args: bool;
    field_descriptors: Ast.Expression.t list;
  }

  let find_decorator ~class_metadata_environment ~names ?dependency class_summary =
    UnannotatedGlobalEnvironment.ReadOnly.first_matching_class_decorator
      (unannotated_global_environment class_metadata_environment)
      ?dependency
      ~names
      class_summary


  let extract_options ~default ~init ~repr ~eq ~order decorator =
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
            (* We need to check each keyword sequentially because different keywords may correspond
               to the same string. *)
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
              if String.equal argument_name "field_descriptors" then
                match value with
                | Expression.Tuple field_descriptors -> { default with field_descriptors }
                | _ -> default
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


  let dataclass_options ~class_metadata_environment ?dependency class_summary =
    let field_descriptors =
      [Reference.create "dataclasses.field" |> Ast.Expression.from_reference ~location:Location.any]
    in
    find_decorator
      ~names:["dataclasses.dataclass"; "dataclass"]
      ~class_metadata_environment
      ?dependency
      class_summary
    >>| extract_options
          ~default:
            {
              init = true;
              repr = true;
              eq = true;
              order = false;
              match_args = true;
              field_descriptors;
            }
          ~init:"init"
          ~repr:"repr"
          ~eq:"eq"
          ~order:"order"


  let attrs_attributes ~class_metadata_environment ?dependency class_summary =
    find_decorator
      ~names:["attr.s"; "attr.attrs"]
      ~class_metadata_environment
      ?dependency
      class_summary
    >>| extract_options
          ~default:
            {
              init = true;
              repr = true;
              eq = true;
              order = true;
              match_args = false;
              field_descriptors = [];
            }
          ~init:"init"
          ~repr:"repr"
          ~eq:"cmp"
          ~order:"cmp"


  let is_dataclass_transform decorator =
    let decorator_reference { Decorator.name = { Node.value; _ }; _ } = value in
    Decorator.from_expression decorator
    >>| decorator_reference
    >>| Reference.last
    >>| String.equal "__dataclass_transform__"
    |> Option.value ~default:false


  let dataclass_transform_default =
    {
      init = true;
      repr = false;
      eq = true;
      order = false;
      match_args = false;
      field_descriptors = [];
    }


  let find_dataclass_transform_decorator_with_default
      ~class_metadata_environment
      ?dependency
      { Node.value = { ClassSummary.decorators; _ }; _ }
    =
    let get_dataclass_transform_decorator_with_default decorator =
      let decorator_reference { Decorator.name = { Node.value; _ }; _ } = value in
      let lookup_function reference =
        UnannotatedGlobalEnvironment.ReadOnly.get_function_definition
          (unannotated_global_environment class_metadata_environment)
          ?dependency
          reference
        >>= fun { FunctionDefinition.body; _ } -> body >>| Node.value
      in
      let function_decorators { Define.signature = { Define.Signature.decorators; _ }; _ } =
        decorators
      in
      decorator_reference decorator
      |> lookup_function
      >>| function_decorators
      >>= List.find ~f:is_dataclass_transform
      >>= Decorator.from_expression
      >>| extract_options
            ~default:dataclass_transform_default
            ~init:""
            ~repr:""
            ~eq:"eq_default"
            ~order:"order_default"
      >>| fun default -> decorator, default
    in
    decorators
    |> List.filter_map ~f:Decorator.from_expression
    |> List.find_map ~f:get_dataclass_transform_decorator_with_default


  let dataclass_transform_options ~class_metadata_environment ?dependency class_summary =
    find_dataclass_transform_decorator_with_default
      ~class_metadata_environment
      ?dependency
      class_summary
    >>| fun (decorator, default) ->
    extract_options ~default ~init:"init" ~repr:"repr" ~eq:"eq" ~order:"order" decorator


  let find_dataclass_transform_class_as_decorator_with_default
      ~class_metadata_environment
      ?dependency
      { Node.value = { ClassSummary.name; bases = { init_subclass_arguments; _ }; _ }; _ }
    =
    let get_dataclass_transform_default name =
      let class_decorators { ClassSummary.decorators; _ } = decorators in
      name
      |> UnannotatedGlobalEnvironment.ReadOnly.get_class_summary
           (ClassMetadataEnvironment.ReadOnly.unannotated_global_environment
              class_metadata_environment)
           ?dependency
      >>| Node.value
      >>| class_decorators
      >>= List.find ~f:is_dataclass_transform
      >>= Decorator.from_expression
      >>| extract_options
            ~default:dataclass_transform_default
            ~init:""
            ~repr:""
            ~eq:"eq_default"
            ~order:"order_default"
    in
    ClassMetadataEnvironment.ReadOnly.successors
      class_metadata_environment
      ?dependency
      (Reference.show name)
    |> List.find_map ~f:get_dataclass_transform_default
    >>| fun default ->
    ( {
        Decorator.name = Node.create_with_default_location name;
        arguments = Some init_subclass_arguments;
      },
      default )


  let dataclass_transform_class_options ~class_metadata_environment ?dependency class_summary =
    find_dataclass_transform_class_as_decorator_with_default
      ~class_metadata_environment
      ?dependency
      class_summary
    >>| fun (decorator, default) ->
    extract_options ~default ~init:"init" ~repr:"repr" ~eq:"eq" ~order:"order" decorator


  let apply
      ~definition
      ~class_metadata_environment
      ~create_attribute
      ~instantiate_attribute
      ?dependency
      table
    =
    let open Expression in
    let { Node.value = { ClassSummary.name; _ }; _ } = definition in
    let parent_dataclasses =
      let class_summary =
        UnannotatedGlobalEnvironment.ReadOnly.get_class_summary
          (ClassMetadataEnvironment.ReadOnly.unannotated_global_environment
             class_metadata_environment)
          ?dependency
      in
      ClassMetadataEnvironment.ReadOnly.successors
        class_metadata_environment
        ?dependency
        (Reference.show name)
      |> List.filter_map ~f:class_summary
    in
    let generate_attributes ~options =
      let already_in_table name =
        UninstantiatedAttributeTable.lookup_name table name |> Option.is_some
      in
      let make_attribute ~annotation ~attribute_name =
        AnnotatedAttribute.create_uninstantiated
          ~uninstantiated_annotation:
            { UninstantiatedAnnotation.accessed_via_metaclass = false; kind = Attribute annotation }
          ~abstract:false
          ~async_property:false
          ~class_variable:false
          ~defined:true
          ~initialized:OnClass
          ~name:attribute_name
          ~parent:(Reference.show name)
          ~visibility:ReadWrite
          ~property:false
          ~undecorated_signature:None
          ~problem:None
      in
      let make_method ~parameters ~annotation ~attribute_name =
        let parameters =
          {
            Type.Callable.Parameter.name = "$parameter$self";
            annotation = Type.Primitive (Reference.show name);
            default = false;
          }
          :: parameters
        in
        let callable =
          {
            Type.Callable.kind = Named (Reference.combine name (Reference.create attribute_name));
            overloads = [];
            implementation =
              { annotation; parameters = Defined (Type.Callable.Parameter.create parameters) };
          }
        in

        AnnotatedAttribute.create_uninstantiated
          ~uninstantiated_annotation:
            {
              UninstantiatedAnnotation.accessed_via_metaclass = false;
              kind = Attribute (Callable callable);
            }
          ~abstract:false
          ~async_property:false
          ~class_variable:false
          ~defined:true
          ~initialized:OnClass
          ~name:attribute_name
          ~parent:(Reference.show name)
          ~visibility:ReadWrite
          ~property:false
          ~undecorated_signature:(Some callable)
          ~problem:None
      in
      match options definition with
      | None -> []
      | Some { init; repr; eq; order; match_args; field_descriptors } ->
          let init_parameters ~implicitly_initialize =
            let extract_dataclass_field_arguments (_, value) =
              match value with
              | { Node.value = Expression.Call { callee; arguments; _ }; _ } ->
                  Option.some_if
                    (List.exists field_descriptors ~f:(fun field_descriptor ->
                         Int.equal
                           (Ast.Expression.location_insensitive_compare callee field_descriptor)
                           0))
                    arguments
              | _ -> None
            in
            let init_not_disabled attribute =
              let is_disable_init { Call.Argument.name; value = { Node.value; _ } } =
                match name, value with
                | Some { Node.value = parameter_name; _ }, Expression.Constant Constant.False
                  when String.equal "init" (Identifier.sanitized parameter_name) ->
                    true
                | _ -> false
              in
              match extract_dataclass_field_arguments attribute with
              | Some arguments -> not (List.exists arguments ~f:is_disable_init)
              | _ -> true
            in
            let extract_init_value (attribute, value) =
              let initialized = AnnotatedAttribute.initialized attribute in
              let get_default_value { Call.Argument.name; value } =
                name
                >>| Node.value
                >>| Identifier.sanitized
                >>= function
                | "default" -> Some value
                | "default_factory"
                | "factory" ->
                    let { Node.location; _ } = value in
                    Some
                      {
                        Node.value = Expression.Call { Call.callee = value; arguments = [] };
                        location;
                      }
                | _ -> None
              in
              match initialized with
              | NotInitialized -> None
              | _ -> (
                  match extract_dataclass_field_arguments (attribute, value) with
                  | Some arguments -> List.find_map arguments ~f:get_default_value
                  | _ -> Some value)
            in
            let collect_parameters parameters (attribute, value) =
              (* Parameters must be annotated attributes *)
              let annotation =
                instantiate_attribute attribute
                |> AnnotatedAttribute.annotation
                |> Annotation.original
                |> function
                | Type.Parametric
                    { name = "dataclasses.InitVar"; parameters = [Single single_parameter] } ->
                    single_parameter
                | annotation -> annotation
              in
              match AnnotatedAttribute.name attribute with
              | name when not (Type.contains_unknown annotation) ->
                  if implicitly_initialize then
                    UninstantiatedAttributeTable.mark_as_implicitly_initialized_if_uninitialized
                      table
                      name;
                  let name = "$parameter$" ^ name in
                  let value = extract_init_value (attribute, value) in
                  let rec override_existing_parameters unchecked_parameters =
                    match unchecked_parameters with
                    | [] ->
                        [
                          {
                            Type.Callable.Parameter.name;
                            annotation;
                            default = Option.is_some value;
                          };
                        ]
                    | { Type.Callable.Parameter.name = old_name; default = old_default; _ } :: tail
                      when Identifier.equal old_name name ->
                        { name; annotation; default = Option.is_some value || old_default } :: tail
                    | head :: tail -> head :: override_existing_parameters tail
                  in
                  override_existing_parameters parameters
              | _ -> parameters
            in
            let get_table ({ Node.value = class_summary; _ } as parent) =
              let create attribute : uninstantiated_attribute * Expression.t =
                let value =
                  match attribute with
                  | {
                   Node.value = { Attribute.kind = Simple { values = { value; _ } :: _; _ }; _ };
                   _;
                  } ->
                      value
                  | { Node.location; _ } ->
                      Node.create (Expression.Constant Constant.Ellipsis) ~location
                in
                ( create_attribute
                    ~parent
                    ?defined:None
                    ~accessed_via_metaclass:false
                    (Node.value attribute),
                  value )
              in
              let compare_by_location left right =
                Ast.Location.compare (Node.location left) (Node.location right)
              in
              ClassSummary.attributes
                ~include_generated_attributes:false
                ~in_test:false
                class_summary
              |> Identifier.SerializableMap.bindings
              |> List.unzip
              |> snd
              |> List.filter ~f:(fun attribute ->
                     (* ClassVar should be excluded from considerations as field. *)
                     match Node.value attribute with
                     | {
                      Attribute.kind =
                        Attribute.Simple
                          {
                            annotation =
                              Some
                                {
                                  Node.value =
                                    Expression.Call
                                      {
                                        callee =
                                          {
                                            value =
                                              Name
                                                (Name.Attribute
                                                  {
                                                    attribute = "__getitem__";
                                                    base =
                                                      {
                                                        Node.value =
                                                          Name
                                                            (Name.Attribute
                                                              {
                                                                attribute = "ClassVar";
                                                                base =
                                                                  {
                                                                    Node.value =
                                                                      Name
                                                                        (Name.Identifier "typing");
                                                                    _;
                                                                  };
                                                                _;
                                                              });
                                                        _;
                                                      };
                                                    _;
                                                  });
                                            _;
                                          };
                                        arguments = [_];
                                      };
                                  _;
                                };
                            _;
                          };
                      _;
                     } ->
                         false
                     | _ -> true)
              |> List.sort ~compare:compare_by_location
              |> List.map ~f:create
            in

            let parent_attribute_tables =
              parent_dataclasses
              |> List.filter ~f:(fun definition -> options definition |> Option.is_some)
              |> List.rev
              |> List.map ~f:get_table
            in
            parent_attribute_tables @ [get_table definition]
            |> List.map ~f:(List.filter ~f:init_not_disabled)
            |> List.fold ~init:[] ~f:(fun parameters ->
                   List.fold ~init:parameters ~f:collect_parameters)
          in
          let methods =
            if init && not (already_in_table "__init__") then
              [
                make_method
                  ~parameters:(init_parameters ~implicitly_initialize:true)
                  ~annotation:Type.none
                  ~attribute_name:"__init__";
              ]
            else
              []
          in
          let methods =
            if repr && not (already_in_table "__repr__") then
              let new_method =
                make_method ~parameters:[] ~annotation:Type.string ~attribute_name:"__repr__"
              in
              new_method :: methods
            else
              methods
          in
          let add_order_method methods name =
            let annotation = Type.object_primitive in
            if not (already_in_table name) then
              make_method
                ~parameters:[{ name = "$parameter$o"; annotation; default = false }]
                ~annotation:Type.bool
                ~attribute_name:name
              :: methods
            else
              methods
          in
          let methods =
            if eq then
              add_order_method methods "__eq__"
            else
              methods
          in
          let methods =
            if order then
              ["__lt__"; "__le__"; "__gt__"; "__ge__"]
              |> List.fold ~init:methods ~f:add_order_method
            else
              methods
          in
          let methods =
            if match_args && not (already_in_table "__match_args__") then
              let parameter_name { Callable.RecordParameter.name; _ } = Identifier.sanitized name in
              let init_parameter_names =
                List.map ~f:parameter_name (init_parameters ~implicitly_initialize:false)
              in
              let literal_string_value_type name = Type.Literal (String (LiteralValue name)) in
              let annotation =
                Type.tuple (List.map ~f:literal_string_value_type init_parameter_names)
              in
              make_attribute ~annotation ~attribute_name:"__match_args__" :: methods
            else
              methods
          in
          methods
    in
    let dataclass_attributes () =
      (* TODO (T43210531): Warn about inconsistent annotations *)
      generate_attributes ~options:(dataclass_options ~class_metadata_environment ?dependency)
    in
    let attrs_attributes () =
      (* TODO (T41039225): Add support for other methods *)
      generate_attributes ~options:(attrs_attributes ~class_metadata_environment ?dependency)
    in
    let dataclass_transform_attributes () =
      generate_attributes
        ~options:(dataclass_transform_options ~class_metadata_environment ?dependency)
    in
    let dataclass_transform_class_attributes () =
      generate_attributes
        ~options:(dataclass_transform_class_options ~class_metadata_environment ?dependency)
    in
    dataclass_attributes ()
    @ attrs_attributes ()
    @ dataclass_transform_attributes ()
    @ dataclass_transform_class_attributes ()
    |> List.iter ~f:(UninstantiatedAttributeTable.add table)
end

let partial_apply_self { Type.Callable.implementation; overloads; _ } ~order ~self_type =
  let open Type.Callable in
  let implementation, overloads =
    match implementation, overloads with
    | { Type.Callable.parameters = Defined (Named { annotation; _ } :: _); _ }, _ -> (
        let solution =
          try
            TypeOrder.OrderedConstraintsSet.add
              ConstraintsSet.empty
              ~new_constraint:(LessOrEqual { left = self_type; right = annotation })
              ~order
            |> TypeOrder.OrderedConstraintsSet.solve ~order
            |> Option.value ~default:ConstraintsSet.Solution.empty
          with
          | ClassHierarchy.Untracked _ -> ConstraintsSet.Solution.empty
        in
        let instantiated =
          ConstraintsSet.Solution.instantiate
            solution
            (Type.Callable { kind = Anonymous; implementation; overloads })
        in
        match instantiated with
        | Type.Callable { implementation; overloads; _ } -> implementation, overloads
        | _ -> implementation, overloads)
    | _ -> implementation, overloads
  in
  let drop_self { Type.Callable.annotation; parameters } =
    let parameters =
      match parameters with
      | Type.Callable.Defined (_ :: parameters) -> Type.Callable.Defined parameters
      | ParameterVariadicTypeVariable { head = _ :: head; variable } ->
          ParameterVariadicTypeVariable { head; variable }
      | _ -> parameters
    in
    { Type.Callable.annotation; parameters }
  in
  {
    Type.Callable.kind = Anonymous;
    implementation = drop_self implementation;
    overloads = List.map overloads ~f:drop_self;
  }


let callable_call_special_cases
    ~instantiated
    ~class_name
    ~attribute_name
    ~order
    ~accessed_through_class
  =
  match instantiated, class_name, attribute_name, accessed_through_class with
  | Some (Type.Callable _), "typing.Callable", "__call__", false -> instantiated
  | ( Some
        (Parametric
          { name = "BoundMethod"; parameters = [Single (Callable callable); Single self_type] }),
      "typing.Callable",
      "__call__",
      false ) ->
      let order = order () in
      partial_apply_self callable ~order ~self_type
      |> fun callable -> Type.Callable callable |> Option.some
  | _ -> None


module SignatureSelection = struct
  (** Return a mapping from each parameter to the arguments that may be assigned to it. Also include
      any error reasons when there are too many or too few arguments.

      Parameters such as `*args: int` and `**kwargs: str` may have any number of arguments assigned
      to them.

      Other parameters such as named parameters (`x: int`), positional-only, or keyword-only
      parameters will have zero or one argument mapped to them. *)
  let get_parameter_argument_mapping ~all_parameters ~parameters ~self_argument arguments =
    let open Type.Callable in
    let all_arguments = arguments in
    let rec consume
        ({ ParameterArgumentMapping.parameter_argument_mapping; reasons = { arity; _ } as reasons }
        as parameter_argument_mapping_with_reasons)
        ~arguments
        ~parameters
      =
      let update_mapping parameter argument =
        Map.add_multi parameter_argument_mapping ~key:parameter ~data:argument
      in
      let arity_mismatch ?(unreachable_parameters = []) ~arguments reasons =
        match all_parameters with
        | Defined all_parameters ->
            let matched_keyword_arguments =
              let is_keyword_argument = function
                | { Argument.WithPosition.kind = Named _; _ } -> true
                | _ -> false
              in
              List.filter ~f:is_keyword_argument all_arguments
            in
            let positional_parameter_count =
              List.length all_parameters
              - List.length unreachable_parameters
              - List.length matched_keyword_arguments
            in
            let self_argument_adjustment =
              if Option.is_some self_argument then
                1
              else
                0
            in
            let error =
              SignatureSelectionTypes.TooManyArguments
                {
                  expected = positional_parameter_count - self_argument_adjustment;
                  provided =
                    positional_parameter_count + List.length arguments - self_argument_adjustment;
                }
            in
            { reasons with arity = error :: arity }
        | _ -> reasons
      in
      match arguments, parameters with
      | [], [] ->
          (* Both empty *)
          parameter_argument_mapping_with_reasons
      | { Argument.WithPosition.kind = SingleStar; _ } :: arguments_tail, []
      | { kind = DoubleStar; _ } :: arguments_tail, [] ->
          (* Starred or double starred arguments; parameters empty *)
          consume ~arguments:arguments_tail ~parameters parameter_argument_mapping_with_reasons
      | { kind = Named name; _ } :: _, [] ->
          (* Named argument; parameters empty *)
          let reasons = { reasons with arity = UnexpectedKeyword name.value :: arity } in
          { parameter_argument_mapping_with_reasons with reasons }
      | _, [] ->
          (* Positional argument; parameters empty *)
          {
            parameter_argument_mapping_with_reasons with
            reasons = arity_mismatch ~arguments reasons;
          }
      | [], (Parameter.KeywordOnly { default = true; _ } as parameter) :: parameters_tail
      | [], (Parameter.PositionalOnly { default = true; _ } as parameter) :: parameters_tail
      | [], (Parameter.Named { default = true; _ } as parameter) :: parameters_tail ->
          (* Arguments empty, default parameter *)
          let parameter_argument_mapping = update_mapping parameter Default in
          consume
            ~arguments
            ~parameters:parameters_tail
            { parameter_argument_mapping_with_reasons with parameter_argument_mapping }
      | [], parameter :: parameters_tail ->
          (* Arguments empty, parameter *)
          let parameter_argument_mapping =
            match Map.find parameter_argument_mapping parameter with
            | Some _ -> parameter_argument_mapping
            | None -> Map.set ~key:parameter ~data:[] parameter_argument_mapping
          in
          consume
            ~arguments
            ~parameters:parameters_tail
            { parameter_argument_mapping_with_reasons with parameter_argument_mapping }
      | ( ({ kind = Named _; _ } as argument) :: arguments_tail,
          (Parameter.Keywords _ as parameter) :: _ ) ->
          (* Labeled argument, keywords parameter *)
          let parameter_argument_mapping =
            update_mapping parameter (make_matched_argument argument)
          in
          consume
            ~arguments:arguments_tail
            ~parameters
            { parameter_argument_mapping_with_reasons with parameter_argument_mapping }
      | ({ kind = Named name; _ } as argument) :: arguments_tail, parameters ->
          (* Labeled argument *)
          let rec extract_matching_name searched to_search =
            match to_search with
            | [] -> None, List.rev searched
            | (Parameter.KeywordOnly { name = parameter_name; _ } as head) :: tail
            | (Parameter.Named { name = parameter_name; _ } as head) :: tail
              when Identifier.equal_sanitized parameter_name name.value ->
                Some head, List.rev searched @ tail
            | (Parameter.Keywords _ as head) :: tail ->
                let matching, parameters = extract_matching_name (head :: searched) tail in
                let matching = Some (Option.value matching ~default:head) in
                matching, parameters
            | head :: tail -> extract_matching_name (head :: searched) tail
          in
          let matching_parameter, remaining_parameters = extract_matching_name [] parameters in
          let parameter_argument_mapping, reasons =
            match matching_parameter with
            | Some matching_parameter ->
                update_mapping matching_parameter (make_matched_argument argument), reasons
            | None ->
                ( parameter_argument_mapping,
                  { reasons with arity = UnexpectedKeyword name.value :: arity } )
          in
          consume
            ~arguments:arguments_tail
            ~parameters:remaining_parameters
            { parameter_argument_mapping_with_reasons with parameter_argument_mapping; reasons }
      | ( ({ kind = DoubleStar; _ } as argument) :: arguments_tail,
          (Parameter.Keywords _ as parameter) :: _ )
      | ( ({ kind = SingleStar; _ } as argument) :: arguments_tail,
          (Parameter.Variable _ as parameter) :: _ ) ->
          (* (Double) starred argument, (double) starred parameter *)
          let parameter_argument_mapping =
            update_mapping parameter (make_matched_argument argument)
          in
          consume
            ~arguments:arguments_tail
            ~parameters
            { parameter_argument_mapping_with_reasons with parameter_argument_mapping }
      | { kind = SingleStar; _ } :: _, Parameter.Keywords _ :: parameters_tail ->
          (* Starred argument, double starred parameter *)
          consume
            ~arguments
            ~parameters:parameters_tail
            { parameter_argument_mapping_with_reasons with parameter_argument_mapping }
      | { kind = Positional; _ } :: _, Parameter.Keywords _ :: parameters_tail ->
          (* Unlabeled argument, double starred parameter *)
          consume
            ~arguments
            ~parameters:parameters_tail
            { parameter_argument_mapping_with_reasons with parameter_argument_mapping }
      | { kind = DoubleStar; _ } :: _, Parameter.Variable _ :: parameters_tail ->
          (* Double starred argument, starred parameter *)
          consume
            ~arguments
            ~parameters:parameters_tail
            { parameter_argument_mapping_with_reasons with parameter_argument_mapping }
      | ( ({ kind = Positional; _ } as argument) :: arguments_tail,
          (Parameter.Variable _ as parameter) :: _ ) ->
          (* Unlabeled argument, starred parameter *)
          let parameter_argument_mapping_with_reasons =
            let parameter_argument_mapping =
              update_mapping parameter (make_matched_argument argument)
            in
            { parameter_argument_mapping_with_reasons with parameter_argument_mapping }
          in
          consume ~arguments:arguments_tail ~parameters parameter_argument_mapping_with_reasons
      | { kind = SingleStar; _ } :: arguments_tail, Type.Callable.Parameter.KeywordOnly _ :: _ ->
          (* Starred argument, keyword only parameter *)
          consume ~arguments:arguments_tail ~parameters parameter_argument_mapping_with_reasons
      | ({ kind = DoubleStar; _ } as argument) :: _, parameter :: parameters_tail
      | ({ kind = SingleStar; _ } as argument) :: _, parameter :: parameters_tail ->
          (* Double starred or starred argument, parameter *)
          let parameter_argument_mapping =
            update_mapping parameter (make_matched_argument argument)
          in
          consume
            ~arguments
            ~parameters:parameters_tail
            { parameter_argument_mapping_with_reasons with parameter_argument_mapping }
      | { kind = Positional; _ } :: _, (Parameter.KeywordOnly _ as parameter) :: parameters_tail ->
          (* Unlabeled argument, keyword only parameter *)
          let reasons =
            arity_mismatch reasons ~unreachable_parameters:(parameter :: parameters_tail) ~arguments
          in
          { parameter_argument_mapping_with_reasons with reasons }
      | ({ kind = Positional; _ } as argument) :: arguments_tail, parameter :: parameters_tail ->
          (* Unlabeled argument, parameter *)
          let parameter_argument_mapping =
            update_mapping parameter (make_matched_argument argument)
          in
          consume
            ~arguments:arguments_tail
            ~parameters:parameters_tail
            { parameter_argument_mapping_with_reasons with parameter_argument_mapping }
    in
    {
      ParameterArgumentMapping.parameter_argument_mapping = Parameter.Map.empty;
      reasons = empty_reasons;
    }
    |> consume ~arguments ~parameters


  (** Check all arguments against the respective parameter types. Return a signature match
      containing constraints from the above compatibility checks and any mismatch errors. *)
  let check_arguments_against_parameters
      ~order
      ~resolve_mutable_literals
      ~resolve_with_locals
      ~callable
      { ParameterArgumentMapping.parameter_argument_mapping; reasons }
    =
    let open SignatureSelectionTypes in
    let open Type.Callable in
    (* Check whether the parameter annotation is `Callable[[ParamVar], ReturnVar]`
     * and the argument is `lambda parameter: body` *)
    let is_generic_lambda parameter arguments =
      match parameter, arguments with
      | ( Parameter.PositionalOnly
            {
              annotation =
                Type.Callable
                  {
                    kind = Anonymous;
                    implementation =
                      {
                        annotation = Type.Variable return_variable;
                        parameters =
                          Defined
                            [
                              Parameter.PositionalOnly
                                {
                                  index = 0;
                                  annotation = Type.Variable parameter_variable;
                                  default = false;
                                };
                            ];
                      };
                    overloads = [];
                  } as annotation;
              _;
            },
          [
            MatchedArgument
              {
                argument =
                  {
                    expression =
                      Some
                        {
                          value =
                            Lambda
                              {
                                body = lambda_body;
                                parameters =
                                  [
                                    {
                                      value =
                                        { name = lambda_parameter; value = None; annotation = None };
                                      _;
                                    };
                                  ];
                              };
                          _;
                        };
                    _;
                  };
                _;
              };
          ] )
        when Type.Variable.Unary.is_free parameter_variable
             && Type.Variable.Unary.is_free return_variable ->
          Some (annotation, parameter_variable, return_variable, lambda_parameter, lambda_body)
      | _ -> None
    in
    let check_arguments_and_update_signature_match
        ~parameter
        ~arguments
        ({ reasons = { arity; _ } as reasons; _ } as signature_match)
      =
      let check_argument_and_set_constraints_and_reasons
          ~position
          ~argument_location
          ~name
          ~argument_annotation
          ~parameter_annotation
          ({ constraints_set; reasons = { annotation; _ } as reasons; _ } as signature_match)
        =
        let reasons_with_mismatch =
          let mismatch =
            let location = name >>| Node.location |> Option.value ~default:argument_location in
            {
              actual = argument_annotation;
              expected = parameter_annotation;
              name = Option.map name ~f:Node.value;
              position;
            }
            |> Node.create ~location
            |> fun mismatch -> Mismatches [Mismatch mismatch]
          in
          { reasons with annotation = mismatch :: annotation }
        in
        let updated_constraints_set =
          TypeOrder.OrderedConstraintsSet.add
            constraints_set
            ~new_constraint:
              (LessOrEqual { left = argument_annotation; right = parameter_annotation })
            ~order
        in
        if ConstraintsSet.potentially_satisfiable updated_constraints_set then
          { signature_match with constraints_set = updated_constraints_set }
        else
          { signature_match with constraints_set; reasons = reasons_with_mismatch }
      in
      let extract_iterable_item_type ~synthetic_variable ~generic_iterable_type resolved =
        let iterable_constraints =
          if Type.is_unbound resolved then
            ConstraintsSet.impossible
          else
            TypeOrder.OrderedConstraintsSet.add
              ConstraintsSet.empty
              ~new_constraint:(LessOrEqual { left = resolved; right = generic_iterable_type })
              ~order
        in
        TypeOrder.OrderedConstraintsSet.solve iterable_constraints ~order
        >>| fun solution ->
        ConstraintsSet.Solution.instantiate_single_variable solution synthetic_variable
        |> Option.value ~default:Type.Any
      in
      let bind_arguments_to_variadic ~expected ~arguments =
        let extract_ordered_types arguments =
          let extracted, errors =
            let extract
                ( ({ Argument.WithPosition.kind; resolved; expression; _ } as argument),
                  index_into_starred_tuple )
              =
              match kind with
              | SingleStar -> (
                  match resolved, index_into_starred_tuple with
                  | Type.Tuple ordered_type, Some index_into_starred_tuple ->
                      Type.OrderedTypes.drop_prefix ~length:index_into_starred_tuple ordered_type
                      >>| (fun ordered_type ->
                            Either.First
                              {
                                ordered_type;
                                argument;
                                item_type_for_error =
                                  Type.OrderedTypes.union_upper_bound ordered_type;
                              })
                      |> Option.value ~default:(Either.Second { expression; annotation = resolved })
                  | Type.Tuple ordered_type, None ->
                      Either.First
                        {
                          ordered_type;
                          argument;
                          item_type_for_error = Type.OrderedTypes.union_upper_bound ordered_type;
                        }
                  | _, _ -> (
                      let synthetic_variable = Type.Variable.Unary.create "$_T" in
                      let generic_iterable_type =
                        Type.iterable (Type.Variable synthetic_variable)
                      in
                      match
                        extract_iterable_item_type
                          ~synthetic_variable
                          ~generic_iterable_type
                          resolved
                      with
                      | Some item_type ->
                          Either.First
                            {
                              ordered_type =
                                Type.OrderedTypes.create_unbounded_concatenation item_type;
                              argument;
                              item_type_for_error = item_type;
                            }
                      | _ -> Either.Second { expression; annotation = resolved }))
              | _ ->
                  Either.First
                    {
                      ordered_type = Type.OrderedTypes.Concrete [resolved];
                      argument;
                      item_type_for_error = resolved;
                    }
            in
            List.rev arguments |> List.partition_map ~f:extract
          in
          match errors with
          | [] -> Ok extracted
          | not_bounded_tuple :: _ ->
              Error
                (Mismatches
                   [
                     MismatchWithUnpackableType
                       { variable = expected; mismatch = NotUnpackableType not_bounded_tuple };
                   ])
        in
        let concatenate extracted =
          let ordered_types = List.map extracted ~f:(fun { ordered_type; _ } -> ordered_type) in
          match Type.OrderedTypes.coalesce_ordered_types ordered_types with
          | Some concatenated -> Ok (concatenated, extracted)
          | None ->
              Error
                (Mismatches
                   [
                     MismatchWithUnpackableType
                       { variable = expected; mismatch = CannotConcatenate ordered_types };
                   ])
        in
        let solve (concatenated, extracted_ordered_types) =
          let updated_constraints_set =
            TypeOrder.OrderedConstraintsSet.add
              signature_match.constraints_set
              ~new_constraint:(OrderedTypesLessOrEqual { left = concatenated; right = expected })
              ~order
          in
          if ConstraintsSet.potentially_satisfiable updated_constraints_set then
            Ok updated_constraints_set
          else
            let expected_concatenation_type =
              match expected with
              | Concatenation concatenation -> Some concatenation
              | _ -> None
            in
            match
              expected_concatenation_type
              >>= Type.OrderedTypes.Concatenation.extract_sole_unbounded_annotation
            with
            | Some expected_item_type ->
                (* The expected type is `*args: *Tuple[X, ...]`. Raise an individual error for each
                   argument that was passed. *)
                let make_mismatch
                    {
                      argument = { Argument.WithPosition.position; expression; kind; _ };
                      item_type_for_error;
                      _;
                    }
                  =
                  let name =
                    match kind with
                    | Named name -> Some name
                    | _ -> None
                  in
                  let location =
                    Option.first_some (name >>| Node.location) (expression >>| Node.location)
                    |> Option.value ~default:Location.any
                  in
                  let is_mismatch =
                    TypeOrder.OrderedConstraintsSet.add
                      signature_match.constraints_set
                      ~new_constraint:
                        (LessOrEqual { left = item_type_for_error; right = expected_item_type })
                      ~order
                    |> ConstraintsSet.potentially_satisfiable
                    |> not
                  in
                  {
                    actual = item_type_for_error;
                    expected = expected_item_type;
                    name = name >>| Node.value;
                    position;
                  }
                  |> Node.create ~location
                  |> fun mismatch -> Mismatch mismatch |> Option.some_if is_mismatch
                in
                Error (Mismatches (List.filter_map extracted_ordered_types ~f:make_mismatch))
            | None ->
                (* The expected type is different from `*args: *Tuple[X, ...]`, such as `*Ts` or
                   more complicated unbounded tuples. It may require a prefix or suffix of
                   arguments. Since we cannot express that clearly by raising individual errors, we
                   raise a combined error about the arguments. *)
                Error
                  (Mismatches
                     [
                       MismatchWithUnpackableType
                         { variable = expected; mismatch = ConstraintFailure concatenated };
                     ])
        in
        let make_signature_match = function
          | Ok constraints_set -> { signature_match with constraints_set }
          | Error error ->
              { signature_match with reasons = { reasons with arity = error :: arity } }
        in
        let arguments =
          List.map arguments ~f:(function
              | MatchedArgument { argument; index_into_starred_tuple } ->
                  argument, index_into_starred_tuple
              | Default -> failwith "Variable parameters do not have defaults")
        in
        let open Result in
        extract_ordered_types arguments >>= concatenate >>= solve |> make_signature_match
      in
      match parameter, arguments with
      | Parameter.Variable (Concatenation concatenation), arguments ->
          bind_arguments_to_variadic
            ~expected:(Type.OrderedTypes.Concatenation concatenation)
            ~arguments
      | Parameter.Variable (Concrete parameter_annotation), arguments ->
          bind_arguments_to_variadic
            ~expected:(Type.OrderedTypes.create_unbounded_concatenation parameter_annotation)
            ~arguments
      | Parameter.Keywords _, [] ->
          (* Parameter was not matched, but empty is acceptable for variable arguments and keyword
             arguments. *)
          signature_match
      | Parameter.KeywordOnly { name; _ }, []
      | Parameter.Named { name; _ }, [] ->
          (* Parameter was not matched *)
          let reasons = { reasons with arity = MissingArgument (Named name) :: arity } in
          { signature_match with reasons }
      | Parameter.PositionalOnly { index; _ }, [] ->
          (* Parameter was not matched *)
          let reasons = { reasons with arity = MissingArgument (PositionalOnly index) :: arity } in
          { signature_match with reasons }
      | PositionalOnly { annotation = parameter_annotation; _ }, arguments
      | KeywordOnly { annotation = parameter_annotation; _ }, arguments
      | Named { annotation = parameter_annotation; _ }, arguments
      | Keywords parameter_annotation, arguments -> (
          let rec check ~arguments signature_match =
            match arguments with
            | [] -> signature_match
            | Default :: tail ->
                (* Parameter default value was used. Assume it is correct. *)
                check signature_match ~arguments:tail
            | MatchedArgument
                { argument = { expression; position; kind; resolved }; index_into_starred_tuple }
              :: tail -> (
                let argument_location =
                  expression >>| Node.location |> Option.value ~default:Location.any
                in
                let name =
                  match kind with
                  | Named name -> Some name
                  | _ -> None
                in
                let check_argument argument_annotation =
                  check_argument_and_set_constraints_and_reasons
                    ~position
                    ~argument_location
                    ~argument_annotation
                    ~parameter_annotation
                    ~name
                    signature_match
                in
                let add_annotation_error
                    ({ reasons = { annotation; _ }; _ } as signature_match)
                    error
                  =
                  {
                    signature_match with
                    reasons = { reasons with annotation = error :: annotation };
                  }
                in
                let update_signature_match_for_iterable ~create_error ~resolved iterable_item_type =
                  let argument_location =
                    expression >>| Node.location |> Option.value ~default:Location.any
                  in
                  match iterable_item_type with
                  | Some iterable_item_type ->
                      check_argument_and_set_constraints_and_reasons
                        ~position
                        ~argument_location
                        ~argument_annotation:iterable_item_type
                        ~parameter_annotation
                        ~name
                        signature_match
                      |> check ~arguments:tail
                  | None ->
                      let argument_location =
                        expression >>| Node.location |> Option.value ~default:Location.any
                      in
                      { expression; annotation = resolved }
                      |> Node.create ~location:argument_location
                      |> create_error
                      |> add_annotation_error signature_match
                in
                match kind with
                | DoubleStar ->
                    let create_error error = InvalidKeywordArgument error in
                    let synthetic_variable = Type.Variable.Unary.create "$_T" in
                    let generic_iterable_type =
                      Type.parametric
                        "typing.Mapping"
                        [Single Type.string; Single (Type.Variable synthetic_variable)]
                    in
                    extract_iterable_item_type ~synthetic_variable ~generic_iterable_type resolved
                    |> update_signature_match_for_iterable ~create_error ~resolved
                | SingleStar -> (
                    let signature_match_for_single_element =
                      match parameter, index_into_starred_tuple, resolved with
                      | ( (PositionalOnly _ | Named _),
                          Some index_into_starred_tuple,
                          Type.Tuple ordered_type ) ->
                          Type.OrderedTypes.index
                            ~python_index:index_into_starred_tuple
                            ordered_type
                          >>| check_argument
                          >>| check ~arguments:tail
                      | _ -> None
                    in
                    match signature_match_for_single_element with
                    | Some signature_match_for_single_element -> signature_match_for_single_element
                    | None ->
                        let create_error error = InvalidVariableArgument error in
                        let synthetic_variable = Type.Variable.Unary.create "$_T" in
                        let generic_iterable_type =
                          Type.iterable (Type.Variable synthetic_variable)
                        in
                        extract_iterable_item_type
                          ~synthetic_variable
                          ~generic_iterable_type
                          resolved
                        |> update_signature_match_for_iterable ~create_error ~resolved)
                | Named _
                | Positional -> (
                    let argument_annotation, weakening_error =
                      if Type.Variable.all_variables_are_resolved parameter_annotation then
                        let { WeakenMutableLiterals.resolved; typed_dictionary_errors } =
                          resolve_mutable_literals
                            ~resolve:(resolve_with_locals ~locals:[])
                            ~expression
                            ~resolved
                            ~expected:parameter_annotation
                        in
                        let weakening_error =
                          if List.is_empty typed_dictionary_errors then
                            None
                          else
                            Some (TypedDictionaryInitializationError typed_dictionary_errors)
                        in
                        resolved, weakening_error
                      else
                        resolved, None
                    in
                    match weakening_error with
                    | Some weakening_error -> add_annotation_error signature_match weakening_error
                    | None -> argument_annotation |> check_argument |> check ~arguments:tail))
          in
          match is_generic_lambda parameter arguments with
          | Some _ -> signature_match (* Handle this later in `special_case_lambda_parameter` *)
          | None -> check ~arguments:(List.rev arguments) signature_match)
    in
    let check_if_solution_exists
        ({ constraints_set; reasons = { annotation; _ } as reasons; callable; _ } as
        signature_match)
      =
      let solution =
        TypeOrder.OrderedConstraintsSet.solve
          constraints_set
          ~order
          ~only_solve_for:(Type.Variable.all_free_variables (Type.Callable callable))
      in
      if Option.is_some solution then
        signature_match
      else
        (* All other cases should have been able to been blamed on a specefic argument, this is the
           only global failure. *)
        {
          signature_match with
          reasons = { reasons with annotation = MutuallyRecursiveTypeVariables :: annotation };
        }
    in
    let special_case_dictionary_constructor
        ({ parameter_argument_mapping; callable; constraints_set; _ } as signature_match)
      =
      let open Type.Record.Callable in
      let has_matched_keyword_parameter parameters =
        List.find parameters ~f:(function
            | RecordParameter.Keywords _ -> true
            | _ -> false)
        >>= Type.Callable.Parameter.Map.find parameter_argument_mapping
        >>| List.is_empty
        >>| not
        |> Option.value ~default:false
      in
      match callable with
      | {
       kind = Named name;
       implementation =
         {
           parameters = Defined parameters;
           annotation = Type.Parametric { parameters = [Single key_type; _]; _ };
           _;
         };
       _;
      }
        when String.equal (Reference.show name) "dict.__init__"
             && has_matched_keyword_parameter parameters ->
          let updated_constraints =
            TypeOrder.OrderedConstraintsSet.add
              constraints_set
              ~new_constraint:(LessOrEqual { left = Type.string; right = key_type })
              ~order
          in
          if ConstraintsSet.potentially_satisfiable updated_constraints then
            { signature_match with constraints_set = updated_constraints }
          else (* TODO(T41074174): Error here *)
            signature_match
      | _ -> signature_match
    in
    let special_case_lambda_parameter ({ parameter_argument_mapping; _ } as signature_match) =
      (* Special case: `Callable[[ParamVar], ReturnVar]` with `lambda parameter: body` *)
      let check_lambda_argument_and_update_signature_match
          ~parameter
          ~arguments
          ({ constraints_set; _ } as signature_match)
        =
        match is_generic_lambda parameter arguments with
        | None -> signature_match
        | Some (annotation, parameter_variable, _, lambda_parameter, lambda_body) -> (
            (* Infer the parameter type using existing constraints. *)
            let solution =
              TypeOrder.OrderedConstraintsSet.solve
                constraints_set
                ~order
                ~only_solve_for:[Type.Record.Variable.Unary parameter_variable]
              >>= fun solution ->
              ConstraintsSet.Solution.instantiate_single_variable solution parameter_variable
            in
            match solution with
            | None -> signature_match
            | Some parameter_type ->
                (* Infer the return type by resolving the lambda body with the parameter type *)
                let updated_constraints =
                  let resolved =
                    let return_type =
                      resolve_with_locals
                        ~locals:
                          [
                            ( Reference.create lambda_parameter,
                              Annotation.create_mutable parameter_type );
                          ]
                        lambda_body
                      |> Type.weaken_literals
                    in
                    let parameters =
                      Type.Callable.Parameter.create
                        [
                          {
                            Type.Callable.Parameter.name = lambda_parameter;
                            annotation = parameter_type;
                            default = false;
                          };
                        ]
                    in
                    Type.Callable.create ~parameters:(Defined parameters) ~annotation:return_type ()
                  in
                  TypeOrder.OrderedConstraintsSet.add
                    constraints_set
                    ~new_constraint:(LessOrEqual { left = resolved; right = annotation })
                    ~order
                  (* Once we've used this solution, we have to commit to it *)
                  |> TypeOrder.OrderedConstraintsSet.add
                       ~new_constraint:
                         (VariableIsExactly (UnaryPair (parameter_variable, parameter_type)))
                       ~order
                in
                { signature_match with constraints_set = updated_constraints })
      in
      Map.fold
        ~init:signature_match
        ~f:(fun ~key ~data ->
          check_lambda_argument_and_update_signature_match ~parameter:key ~arguments:data)
        parameter_argument_mapping
    in
    let signature_match =
      {
        callable;
        parameter_argument_mapping;
        constraints_set = [TypeConstraints.empty];
        ranks = { arity = 0; annotation = 0; position = 0 };
        reasons;
      }
    in
    Map.fold
      ~init:signature_match
      ~f:(fun ~key ~data ->
        check_arguments_and_update_signature_match ~parameter:key ~arguments:data)
      parameter_argument_mapping
    |> special_case_dictionary_constructor
    |> special_case_lambda_parameter
    |> check_if_solution_exists


  (** Check arguments against the given callable signature and returning possible signature matches. *)
  let rec check_arguments_against_signature
      ~order
      ~resolve_mutable_literals
      ~resolve_with_locals
      ~callable
      ~self_argument
      ~(arguments : Argument.WithPosition.t list)
      implementation
    =
    let open SignatureSelectionTypes in
    let open Type.Callable in
    let callable = { callable with Type.Callable.implementation; overloads = [] } in
    let base_signature_match =
      {
        callable;
        parameter_argument_mapping = Parameter.Map.empty;
        constraints_set = [TypeConstraints.empty];
        ranks = { arity = 0; annotation = 0; position = 0 };
        reasons = empty_reasons;
      }
    in
    let { parameters = all_parameters; _ } = implementation in
    let check_arguments_against_parameters =
      check_arguments_against_parameters ~order ~resolve_mutable_literals ~resolve_with_locals
    in
    match all_parameters with
    | Defined parameters ->
        get_parameter_argument_mapping ~parameters ~all_parameters ~self_argument arguments
        |> check_arguments_against_parameters ~callable
        |> fun signature_match -> [signature_match]
    | Undefined -> [base_signature_match]
    | ParameterVariadicTypeVariable { head; variable }
      when Type.Variable.Variadic.Parameters.is_free variable -> (
        (* Handle callables where an early parameter binds a ParamSpec and later parameters expect
           the corresponding arguments.

           For example, when a function like `def foo(f: Callable[P, R], *args: P.args, **kwargs:
           P.kwargs) -> None` is called as `foo(add, 1, 2)`, first solve for the free variable `P`
           using the callable argument `add` and then use the solution to get concrete types for
           `P.args` and `P.kwargs`. *)
        let front, back =
          let is_labeled = function
            | { Argument.WithPosition.kind = Named _; _ } -> true
            | _ -> false
          in
          let labeled, unlabeled = List.partition_tf arguments ~f:is_labeled in
          let first_unlabeled, remainder = List.split_n unlabeled (List.length head) in
          first_unlabeled, labeled @ remainder
        in
        let ({ constraints_set; reasons = { arity = head_arity; annotation = head_annotation }; _ }
            as head_signature)
          =
          get_parameter_argument_mapping
            ~all_parameters
            ~parameters:(Type.Callable.prepend_anonymous_parameters ~head ~tail:[])
            ~self_argument
            front
          |> check_arguments_against_parameters ~callable
        in
        let solve_back parameters =
          let constraints_set =
            (* If we use this option, we have to commit to it as to not move away from it later *)
            TypeOrder.OrderedConstraintsSet.add
              constraints_set
              ~new_constraint:(VariableIsExactly (ParameterVariadicPair (variable, parameters)))
              ~order
          in
          check_arguments_against_signature
            ~order
            ~resolve_mutable_literals
            ~resolve_with_locals
            ~callable
            ~self_argument
            ~arguments:back
            { implementation with parameters }
          |> List.map
               ~f:(fun { reasons = { arity = tail_arity; annotation = tail_annotation }; _ } ->
                 {
                   base_signature_match with
                   constraints_set;
                   reasons =
                     {
                       arity = head_arity @ tail_arity;
                       annotation = head_annotation @ tail_annotation;
                     };
                 })
        in
        TypeOrder.OrderedConstraintsSet.get_parameter_specification_possibilities
          constraints_set
          ~parameter_specification:variable
          ~order
        |> List.concat_map ~f:solve_back
        |> function
        | [] -> [head_signature]
        | nonempty -> nonempty)
    | ParameterVariadicTypeVariable { head; variable } -> (
        (* The ParamSpec variable `P` is in scope, so the only valid arguments are `*args` and
           `**kwargs` that have "type" `P.args` and `P.kwargs` respectively. If the ParamSpec has a
           `head` prefix of parameters, check for any prefix arguments. *)
        let combines_into_variable ~positional_component ~keyword_component =
          Type.Variable.Variadic.Parameters.Components.combine
            { positional_component; keyword_component }
          >>| Type.Variable.Variadic.Parameters.equal variable
          |> Option.value ~default:false
        in
        match List.rev arguments with
        | { kind = DoubleStar; resolved = keyword_component; _ }
          :: { kind = SingleStar; resolved = positional_component; _ } :: reversed_arguments_head
          when combines_into_variable ~positional_component ~keyword_component ->
            let arguments = List.rev reversed_arguments_head in
            get_parameter_argument_mapping
              ~parameters:(Type.Callable.prepend_anonymous_parameters ~head ~tail:[])
              ~all_parameters
              ~self_argument
              arguments
            |> check_arguments_against_parameters ~callable
            |> fun signature_match -> [signature_match]
        | _ ->
            [
              {
                base_signature_match with
                reasons = { arity = [CallingParameterVariadicTypeVariable]; annotation = [] };
              };
            ])


  (** Given a signature match for a callable, solve for any type variables and instantiate the
      return annotation. *)
  let instantiate_return_annotation
      ?(skip_marking_escapees = false)
      ~order
      {
        callable =
          { implementation = { annotation = uninstantiated_return_annotation; _ }; _ } as callable;
        constraints_set;
        reasons = { arity; annotation; _ };
        _;
      }
    =
    let open SignatureSelectionTypes in
    let instantiated_return_annotation =
      let local_free_variables = Type.Variable.all_free_variables (Type.Callable callable) in
      let solution =
        TypeOrder.OrderedConstraintsSet.solve
          constraints_set
          ~only_solve_for:local_free_variables
          ~order
        |> Option.value ~default:ConstraintsSet.Solution.empty
      in
      let instantiated =
        ConstraintsSet.Solution.instantiate solution uninstantiated_return_annotation
      in
      if skip_marking_escapees then
        instantiated
      else
        Type.Variable.mark_all_free_variables_as_escaped ~specific:local_free_variables instantiated
        (* We need to do transformations of the form Union[T_escaped, int] => int in order to
           properly handle some typeshed stubs which only sometimes bind type variables and expect
           them to fall out in this way (see Mapping.get) *)
        |> Type.Variable.collapse_all_escaped_variable_unions
    in
    let rev_filter_out_self_argument_errors reasons =
      let filter_too_many_arguments = function
        (* These would come from methods lacking a self argument called on an instance *)
        | TooManyArguments { expected; _ } -> not (Int.equal expected (-1))
        | _ -> true
      in
      let filter_mismatches reason =
        match reason with
        | Mismatches mismatches ->
            Mismatches
              (List.filter mismatches ~f:(function
                  | Mismatch { Node.value = { position; _ }; _ } -> not (Int.equal position 0)
                  | _ -> true))
        | _ -> reason
      in
      List.map (List.rev_filter ~f:filter_too_many_arguments reasons) ~f:filter_mismatches
    in
    match
      rev_filter_out_self_argument_errors arity, rev_filter_out_self_argument_errors annotation
    with
    | [], [] -> Found { selected_return_annotation = instantiated_return_annotation }
    | reason :: reasons, _
    | [], reason :: reasons ->
        let importance = function
          | AbstractClassInstantiation _ -> 1
          | CallingParameterVariadicTypeVariable -> 1
          | InvalidKeywordArgument _ -> 0
          | InvalidVariableArgument _ -> 0
          | Mismatches _ -> -1
          | MissingArgument _ -> 1
          | MutuallyRecursiveTypeVariables -> 1
          | ProtocolInstantiation _ -> 1
          | TooManyArguments _ -> 1
          | TypedDictionaryInitializationError _ -> 1
          | UnexpectedKeyword _ -> 1
        in
        let get_most_important best_reason reason =
          if importance reason > importance best_reason then
            reason
          else
            match best_reason, reason with
            | Mismatches mismatches, Mismatches other_mismatches ->
                Mismatches (List.append mismatches other_mismatches)
            | _, _ -> best_reason
        in
        let sort_mismatches reason =
          match reason with
          | Mismatches mismatches ->
              let compare_mismatches mismatch other_mismatch =
                match mismatch, other_mismatch with
                | ( Mismatch { Node.value = { position; _ }; _ },
                    Mismatch { Node.value = { position = other_position; _ }; _ } ) ->
                    position - other_position
                | _, _ -> 0
              in
              Mismatches (List.sort mismatches ~compare:compare_mismatches)
          | _ -> reason
        in
        let reason =
          Some (List.fold ~init:reason ~f:get_most_important reasons |> sort_mismatches)
        in
        NotFound { closest_return_annotation = instantiated_return_annotation; reason }


  let default_signature
      { Type.Callable.implementation = { annotation = default_return_annotation; _ }; _ }
    =
    let open SignatureSelectionTypes in
    NotFound { closest_return_annotation = default_return_annotation; reason = None }


  let calculate_rank ({ reasons = { arity; annotation; _ }; _ } as signature_match) =
    let open SignatureSelectionTypes in
    let arity_rank = List.length arity in
    let positions, annotation_rank =
      let count_unique (positions, count) = function
        | Mismatches mismatches ->
            let count_unique_mismatches (positions, count) mismatch =
              match mismatch with
              | Mismatch { Node.value = { position; _ }; _ } when not (Set.mem positions position)
                ->
                  Set.add positions position, count + 1
              | Mismatch _ -> positions, count
              | _ -> positions, count + 1
            in
            List.fold ~init:(positions, count) mismatches ~f:count_unique_mismatches
        | _ -> positions, count + 1
      in
      List.fold ~init:(Int.Set.empty, 0) ~f:count_unique annotation
    in
    let position_rank =
      Int.Set.min_elt positions >>| Int.neg |> Option.value ~default:Int.min_value
    in
    {
      signature_match with
      ranks = { arity = arity_rank; annotation = annotation_rank; position = position_rank };
    }


  (** Find the signature that is "closest" to what the user intended. Essentially, sort signatures
      on the number of arity mismatches, number of annotation mismatches, and the earliest mismatch
      position.

      TODO(T109092235): Clean up the rank calculation to more clearly reflect that we want to do
      `maximum_by (arity, annotation, position)`. *)
  let find_closest_signature signature_matches =
    let get_arity_rank { ranks = { arity; _ }; _ } = arity in
    let get_annotation_rank { ranks = { annotation; _ }; _ } = annotation in
    let get_position_rank { ranks = { position; _ }; _ } = position in
    let rec get_best_rank ~best_matches ~best_rank ~getter = function
      | [] -> best_matches
      | head :: tail ->
          let rank = getter head in
          if rank < best_rank then
            get_best_rank ~best_matches:[head] ~best_rank:rank ~getter tail
          else if rank = best_rank then
            get_best_rank ~best_matches:(head :: best_matches) ~best_rank ~getter tail
          else
            get_best_rank ~best_matches ~best_rank ~getter tail
    in
    signature_matches
    |> List.map ~f:calculate_rank
    |> get_best_rank ~best_matches:[] ~best_rank:Int.max_value ~getter:get_arity_rank
    |> get_best_rank ~best_matches:[] ~best_rank:Int.max_value ~getter:get_annotation_rank
    |> get_best_rank ~best_matches:[] ~best_rank:Int.max_value ~getter:get_position_rank
    (* Each get_best_rank reverses the list, because we have an odd number, we need an extra reverse
       in order to prefer the first defined overload *)
    |> List.rev
    |> List.hd


  let prepare_arguments_for_signature_selection ~self_argument arguments =
    let add_positions arguments =
      let add_index index { Argument.expression; kind; resolved } =
        { Argument.WithPosition.position = index + 1; expression; kind; resolved }
      in
      List.mapi ~f:add_index arguments
    in
    let unpack_starred_arguments arguments =
      let unpack sofar argument =
        match argument with
        | { Argument.resolved = Tuple (Concrete tuple_parameters); kind = SingleStar; expression }
          ->
            let unpacked_arguments =
              List.map tuple_parameters ~f:(fun resolved ->
                  { Argument.expression; kind = Positional; resolved })
            in
            List.concat [List.rev unpacked_arguments; sofar]
        | _ -> argument :: sofar
      in
      List.fold ~f:unpack ~init:[] arguments |> List.rev
    in
    let separate_labeled_unlabeled_arguments arguments =
      let is_labeled = function
        | { Argument.WithPosition.kind = Named _; _ } -> true
        | _ -> false
      in
      let labeled_arguments, unlabeled_arguments = arguments |> List.partition_tf ~f:is_labeled in
      let self_argument =
        self_argument
        >>| (fun resolved ->
              { Argument.WithPosition.position = 0; expression = None; kind = Positional; resolved })
        |> Option.to_list
      in
      self_argument @ labeled_arguments @ unlabeled_arguments
    in
    arguments |> unpack_starred_arguments |> add_positions |> separate_labeled_unlabeled_arguments
end

class base class_metadata_environment dependency =
  object (self)
    method get_typed_dictionary ~assumptions annotation =
      match annotation with
      | Type.Primitive class_name
        when ClassMetadataEnvironment.ReadOnly.is_typed_dictionary
               class_metadata_environment
               ?dependency
               class_name ->
          let fields =
            self#attribute
              ~assumptions
              ~transitive:false
              ~accessed_through_class:true
              ~include_generated_attributes:true
              ~instantiated:(Type.meta annotation)
              ~special_method:false
              ~attribute_name:"__init__"
              class_name
            >>| AnnotatedAttribute.annotation
            >>| Annotation.annotation
            >>= function
            | Type.Callable callable -> Type.TypedDictionary.fields_from_constructor callable
            | _ -> None
          in
          fields >>| fun fields -> { Type.Record.TypedDictionary.fields; name = class_name }
      | _ -> None

    method full_order ~assumptions =
      let resolve class_type =
        match Type.resolve_class class_type with
        | None -> None
        | Some [] -> None
        | Some [resolved] -> Some resolved
        | Some (_ :: _) ->
            (* These come from calling attributes on Unions, which are handled by
               solve_less_or_equal indirectly by breaking apart the union before doing the
               instantiate_protocol_parameters. Therefore, there is no reason to deal with joining
               the attributes together here *)
            None
      in
      let attribute class_type ~assumptions ~name =
        resolve class_type
        >>= fun { instantiated; accessed_through_class; class_name } ->
        self#attribute
          ~assumptions
          ~transitive:true
          ~accessed_through_class
          ~include_generated_attributes:true
          ?special_method:None
          ~attribute_name:name
          ~instantiated
          class_name
      in
      let all_attributes class_type ~assumptions =
        resolve class_type
        >>= fun { instantiated; accessed_through_class; class_name } ->
        self#all_attributes
          ~assumptions
          ~transitive:true
          ~accessed_through_class
          ~include_generated_attributes:true
          ?special_method:None
          class_name
        >>| List.map
              ~f:
                (self#instantiate_attribute
                   ~assumptions
                   ~instantiated
                   ~accessed_through_class
                   ?apply_descriptors:None)
      in

      let is_protocol annotation ~protocol_assumptions:_ =
        UnannotatedGlobalEnvironment.ReadOnly.is_protocol
          (unannotated_global_environment class_metadata_environment)
          ?dependency
          annotation
      in
      let class_hierarchy_handler =
        ClassHierarchyEnvironment.ReadOnly.class_hierarchy
          ?dependency
          (class_hierarchy_environment class_metadata_environment)
      in
      let metaclass class_name ~assumptions = self#metaclass class_name ~assumptions in
      {
        ConstraintsSet.class_hierarchy =
          {
            instantiate_successors_parameters =
              ClassHierarchy.instantiate_successors_parameters class_hierarchy_handler;
            is_transitive_successor = ClassHierarchy.is_transitive_successor class_hierarchy_handler;
            variables = ClassHierarchy.variables class_hierarchy_handler;
            least_upper_bound = ClassHierarchy.least_upper_bound class_hierarchy_handler;
          };
        attribute;
        all_attributes;
        is_protocol;
        assumptions;
        get_typed_dictionary = self#get_typed_dictionary ~assumptions;
        metaclass;
      }

    method check_invalid_type_parameters
        ?(replace_unbound_parameters_with_any = true)
        ~assumptions
        annotation =
      let open TypeParameterValidationTypes in
      let module InvalidTypeParametersTransform = Type.Transform.Make (struct
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
              | "typing.Final"
              | "typing_extensions.Final"
              | "typing.Optional" ->
                  [Type.Variable.Unary (Type.Variable.Unary.create "T")]
              | "typing.Callable" ->
                  [
                    Type.Variable.ParameterVariadic (Type.Variable.Variadic.Parameters.create "Ps");
                    Type.Variable.Unary (Type.Variable.Unary.create "R");
                  ]
              | _ ->
                  ClassHierarchyEnvironment.ReadOnly.variables
                    (class_hierarchy_environment class_metadata_environment)
                    ?dependency
                    name
                  |> Option.value ~default:[]
            in
            let invalid_type_parameters ~name ~given =
              let generics = generics_for_name name in
              match
                Type.Variable.zip_variables_with_parameters_including_mismatches
                  ~parameters:given
                  generics
              with
              | Some [] -> Type.Primitive name, sofar
              | Some paired ->
                  let check_parameter { Type.Variable.variable_pair; received_parameter } =
                    match variable_pair, received_parameter with
                    | Type.Variable.UnaryPair (unary, given), Type.Parameter.Single _ ->
                        let invalid =
                          let order = self#full_order ~assumptions in
                          TypeOrder.OrderedConstraints.add_lower_bound
                            TypeConstraints.empty
                            ~order
                            ~pair:variable_pair
                          >>| TypeOrder.OrderedConstraints.add_upper_bound
                                ~order
                                ~pair:variable_pair
                          |> Option.is_none
                        in
                        if invalid then
                          ( [Type.Parameter.Single Type.Any],
                            Some
                              {
                                name;
                                kind = ViolateConstraints { expected = unary; actual = given };
                              } )
                        else
                          [Type.Parameter.Single given], None
                    | ParameterVariadicPair (_, given), CallableParameters _ ->
                        (* TODO(T47346673): accept w/ new kind of validation *)
                        [CallableParameters given], None
                    | TupleVariadicPair (_, given), Single (Tuple _) ->
                        Type.OrderedTypes.to_parameters given, None
                    | Type.Variable.UnaryPair (unary, given), _ ->
                        ( [Single given],
                          Some
                            {
                              name;
                              kind =
                                UnexpectedKind
                                  {
                                    expected = Type.Variable.Unary unary;
                                    actual = received_parameter;
                                  };
                            } )
                    | ParameterVariadicPair (parameter_variadic, given), _ ->
                        ( [CallableParameters given],
                          Some
                            {
                              name;
                              kind =
                                UnexpectedKind
                                  {
                                    expected = ParameterVariadic parameter_variadic;
                                    actual = received_parameter;
                                  };
                            } )
                    | TupleVariadicPair (tuple_variadic, given), _ ->
                        ( Type.OrderedTypes.to_parameters given,
                          Some
                            {
                              name;
                              kind =
                                UnexpectedKind
                                  {
                                    expected = TupleVariadic tuple_variadic;
                                    actual = received_parameter;
                                  };
                            } )
                  in
                  List.map paired ~f:check_parameter
                  |> List.unzip
                  |> fun (list_of_parameters, errors) ->
                  ( Type.parametric name (List.concat list_of_parameters),
                    List.filter_map errors ~f:Fn.id @ sofar )
              | None when not replace_unbound_parameters_with_any ->
                  Type.parametric name (List.map generics ~f:Type.Variable.to_parameter), sofar
              | None ->
                  let annotation, expected_parameter_count, can_accept_more_parameters =
                    match name with
                    | "typing.Callable" ->
                        Type.Callable.create ~annotation:Type.Any (), List.length generics, false
                    | "tuple" ->
                        ( Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.Any),
                          List.length generics,
                          true )
                    | _ ->
                        let is_tuple_variadic = function
                          | Type.Variable.TupleVariadic _ -> true
                          | _ -> false
                        in
                        let annotation =
                          Type.parametric
                            name
                            (List.concat_map generics ~f:(function
                                | Type.Variable.Unary _ -> [Type.Parameter.Single Type.Any]
                                | ParameterVariadic _ -> [CallableParameters Undefined]
                                | TupleVariadic _ ->
                                    Type.OrderedTypes.to_parameters Type.Variable.Variadic.Tuple.any))
                        in
                        ( annotation,
                          List.filter generics ~f:(fun x -> not (is_tuple_variadic x))
                          |> List.length,
                          List.exists generics ~f:is_tuple_variadic )
                  in
                  let mismatch =
                    {
                      name;
                      kind =
                        IncorrectNumberOfParameters
                          {
                            actual = List.length given;
                            expected = expected_parameter_count;
                            can_accept_more_parameters;
                          };
                    }
                  in
                  annotation, mismatch :: sofar
            in
            match annotation with
            | Type.Primitive ("typing.Final" | "typing_extensions.Final") -> annotation, sofar
            | Type.Primitive name -> invalid_type_parameters ~name ~given:[]
            (* natural variadics *)
            | Type.Parametric { name = "typing.Protocol"; _ }
            | Type.Parametric { name = "typing.Generic"; _ } ->
                annotation, sofar
            | Type.Parametric { name; parameters } ->
                invalid_type_parameters ~name ~given:parameters
            | Type.IntExpression _ ->
                let invalid_generic_int given =
                  let generic_int =
                    Type.Variable.Unary.create
                      "T"
                      ~constraints:(Type.Record.Variable.Bound (Primitive "int"))
                  in
                  let invalid =
                    let order = self#full_order ~assumptions in
                    let pair = Type.Variable.UnaryPair (generic_int, given) in
                    TypeOrder.OrderedConstraints.add_lower_bound TypeConstraints.empty ~order ~pair
                    >>= TypeOrder.OrderedConstraints.add_upper_bound ~order ~pair
                    |> Option.is_none
                  in
                  if invalid then
                    Some
                      {
                        name = "IntExpression";
                        kind = ViolateConstraints { actual = given; expected = generic_int };
                      }
                  else
                    None
                in
                let errors =
                  Type.Variable.GlobalTransforms.Unary.collect_all annotation
                  |> List.filter_map ~f:(fun variable ->
                         invalid_generic_int (Type.Variable variable))
                in
                if List.length errors > 0 then
                  Any, errors @ sofar
                else
                  annotation, sofar
            | _ -> annotation, sofar
          in
          { Type.Transform.transformed_annotation; new_state }
      end)
      in
      InvalidTypeParametersTransform.visit [] annotation

    method parse_annotation
        ~assumptions
        ?(validation = SharedMemoryKeys.ParseAnnotationKey.ValidatePrimitivesAndTypeParameters)
        expression =
      let modify_aliases ?replace_unbound_parameters_with_any = function
        | Type.TypeAlias alias ->
            self#check_invalid_type_parameters
              ?replace_unbound_parameters_with_any
              alias
              ~assumptions
            |> snd
            |> fun alias -> Type.TypeAlias alias
        | result -> result
      in
      let allow_untracked =
        match validation with
        | NoValidation -> true
        | ValidatePrimitives
        | ValidatePrimitivesAndTypeParameters ->
            false
      in
      let annotation =
        AliasEnvironment.ReadOnly.parse_annotation_without_validating_type_parameters
          (alias_environment class_metadata_environment)
          ~modify_aliases
          ?dependency
          ~allow_untracked
          expression
      in
      let result =
        match validation with
        | ValidatePrimitivesAndTypeParameters ->
            self#check_invalid_type_parameters annotation ~assumptions |> snd
        | NoValidation
        | ValidatePrimitives ->
            annotation
      in
      result

    method sqlalchemy_attribute_table
        ~assumptions
        ~include_generated_attributes
        ~in_test
        ~accessed_via_metaclass
        ({ Node.value = { ClassSummary.name = parent_name; _ }; _ } as parent) =
      let class_name = Reference.show parent_name in
      let unannotated_attributes
          ~include_generated_attributes
          ~in_test
          ({ Node.value = class_summary; _ } as parent)
        =
        let attributes =
          ClassSummary.attributes ~include_generated_attributes ~in_test class_summary
          |> Identifier.SerializableMap.bindings
          |> List.map ~f:(fun (_, attribute) -> attribute)
        in
        let unannotated_attribute { Node.value = attribute; _ } =
          self#create_attribute
            ~assumptions
            ~parent
            ?defined:(Some true)
            ~accessed_via_metaclass
            attribute
        in
        List.map attributes ~f:unannotated_attribute
      in
      let add_constructor table =
        let successor_definitions =
          let class_summary =
            UnannotatedGlobalEnvironment.ReadOnly.get_class_summary
              (ClassMetadataEnvironment.ReadOnly.unannotated_global_environment
                 class_metadata_environment)
              ?dependency
          in
          ClassMetadataEnvironment.ReadOnly.successors
            class_metadata_environment
            ?dependency
            class_name
          |> List.filter_map ~f:class_summary
        in
        let name_annotation_pairs =
          let name_annotation_pair attribute =
            let name = AnnotatedAttribute.name attribute in
            if Expression.is_dunder_attribute name || AnnotatedAttribute.is_private attribute then
              None
            else
              let annotation =
                self#instantiate_attribute
                  ~assumptions
                  ~accessed_through_class:false
                  ?instantiated:None
                  ?apply_descriptors:None
                  attribute
                |> AnnotatedAttribute.annotation
                |> Annotation.annotation
              in
              Some (name, annotation)
          in
          parent :: successor_definitions
          |> List.concat_map
               ~f:(unannotated_attributes ~include_generated_attributes:false ~in_test:false)
          |> List.filter_map ~f:name_annotation_pair
          (* Pick the overriding attribute. *)
          |> Identifier.Map.of_alist_reduce ~f:(fun first _ -> first)
          |> Identifier.Map.to_alist
        in
        let parameters =
          let keyword_only_parameter (name, annotation) =
            Type.Record.Callable.RecordParameter.KeywordOnly
              { name = Format.asprintf "$parameter$%s" name; annotation; default = true }
          in
          let self_parameter =
            Type.Callable.Parameter.Named
              { name = "$parameter$self"; annotation = Type.Primitive class_name; default = false }
          in
          List.map ~f:keyword_only_parameter name_annotation_pairs
          |> fun parameters -> Type.Record.Callable.Defined (self_parameter :: parameters)
        in
        let constructor =
          {
            Type.Callable.kind = Named (Reference.create ~prefix:parent_name "__init__");
            implementation = { annotation = Type.none; parameters };
            overloads = [];
          }
        in
        AnnotatedAttribute.create_uninstantiated
          ~abstract:false
          ~uninstantiated_annotation:(create_uninstantiated_method constructor)
          ~async_property:false
          ~class_variable:false
          ~defined:true
          ~initialized:OnClass
          ~name:"__init__"
          ~parent:class_name
          ~visibility:ReadWrite
          ~property:false
          ~undecorated_signature:(Some constructor)
          ~problem:None
        |> UninstantiatedAttributeTable.add table
      in
      let add_special_attribute ~name ~annotation table =
        AnnotatedAttribute.create_uninstantiated
          ~abstract:false
          ~uninstantiated_annotation:
            {
              UninstantiatedAnnotation.accessed_via_metaclass = false;
              kind = UninstantiatedAnnotation.Attribute annotation;
            }
          ~async_property:false
          ~class_variable:false
          ~defined:true
          ~initialized:OnClass
          ~name
          ~parent:class_name
          ~visibility:ReadWrite
          ~property:false
          ~undecorated_signature:None
          ~problem:None
        |> UninstantiatedAttributeTable.add table
      in
      let table = UninstantiatedAttributeTable.create () in
      unannotated_attributes ~include_generated_attributes ~in_test parent
      |> List.iter ~f:(UninstantiatedAttributeTable.add table);
      if include_generated_attributes then
        add_constructor table;
      add_special_attribute
        ~name:"metadata"
        ~annotation:(Type.Primitive "sqlalchemy.sql.schema.MetaData")
        table;
      add_special_attribute
        ~name:"__table__"
        ~annotation:(Type.Primitive "sqlalchemy.sql.schema.Table")
        table;
      table

    method typed_dictionary_special_methods_table
        ~assumptions
        ~include_generated_attributes
        ~in_test
        ~accessed_via_metaclass
        ?dependency
        ~class_metadata_environment
        ~class_name
        ({ Node.value = { ClassSummary.name; _ }; _ } as parent_definition) =
      let table = UninstantiatedAttributeTable.create () in
      let add_special_methods () =
        let class_summary =
          UnannotatedGlobalEnvironment.ReadOnly.get_class_summary
            (ClassMetadataEnvironment.ReadOnly.unannotated_global_environment
               class_metadata_environment)
            ?dependency
        in
        let successor_definitions =
          ClassMetadataEnvironment.ReadOnly.successors
            class_metadata_environment
            ?dependency
            (Reference.show name)
          |> List.filter_map ~f:class_summary
        in
        let base_typed_dictionary_definition fields =
          let total = Type.TypedDictionary.are_fields_total fields in
          match class_summary (Type.TypedDictionary.class_name ~total) with
          | Some definition -> definition
          | None -> failwith "Expected to find TypedDictionary"
        in
        let typed_dictionary_definitions =
          List.filter
            (parent_definition :: successor_definitions)
            ~f:(fun { Node.value = { ClassSummary.name; _ }; _ } ->
              ClassMetadataEnvironment.ReadOnly.is_typed_dictionary
                class_metadata_environment
                ?dependency
                (Reference.show name))
        in
        let get_field_attributes
            ~include_generated_attributes
            { Node.value = { bases = { ClassSummary.base_classes; _ }; _ } as class_summary; _ }
          =
          let has_non_total_typed_dictionary_base_class =
            List.exists base_classes ~f:(fun base_expression ->
                String.equal
                  (Expression.show base_expression)
                  (Type.TypedDictionary.class_name ~total:false))
          in
          ClassSummary.attributes ~include_generated_attributes ~in_test class_summary
          |> Identifier.SerializableMap.bindings
          |> List.map ~f:(fun (_, field_attribute) ->
                 ( self#create_attribute
                     ~assumptions
                     ~parent:parent_definition
                     ?defined:(Some true)
                     ~accessed_via_metaclass
                     (Node.value field_attribute),
                   has_non_total_typed_dictionary_base_class ))
        in
        let attribute_to_typed_dictionary_field
            (attribute, has_non_total_typed_dictionary_base_class)
          =
          match AnnotatedAttribute.uninstantiated_annotation attribute with
          | { UninstantiatedAnnotation.kind = Attribute annotation; _ } ->
              Some
                (Type.TypedDictionary.create_field
                   ~annotation
                   ~has_non_total_typed_dictionary_base_class
                   (AnnotatedAttribute.name attribute))
          | _ -> None
        in
        let keep_last_declarations fields =
          List.map
            fields
            ~f:(fun (field : Type.t Type.Record.TypedDictionary.typed_dictionary_field) ->
              field.name, field)
          |> Map.of_alist_multi (module String)
          |> Map.to_alist
          |> List.map ~f:(fun (_, fields) -> List.last_exn fields)
        in
        let fields =
          List.rev typed_dictionary_definitions
          |> List.concat_map ~f:(get_field_attributes ~include_generated_attributes:false)
          |> List.filter_map ~f:attribute_to_typed_dictionary_field
          |> keep_last_declarations
        in
        let overload_method (attribute, _) =
          match AnnotatedAttribute.uninstantiated_annotation attribute with
          | { UninstantiatedAnnotation.kind = Attribute (Callable callable); _ } as
            uninstantiated_annotation ->
              let overloaded_callable overloads =
                {
                  callable with
                  Type.Callable.implementation = { annotation = Type.Top; parameters = Undefined };
                  overloads;
                }
              in
              Type.TypedDictionary.special_overloads
                ~class_name
                ~fields
                ~method_name:(AnnotatedAttribute.name attribute)
              >>| overloaded_callable
              >>| fun callable ->
              AnnotatedAttribute.with_uninstantiated_annotation
                ~uninstantiated_annotation:
                  {
                    uninstantiated_annotation with
                    UninstantiatedAnnotation.kind = Attribute (Callable callable);
                  }
                attribute
              |> AnnotatedAttribute.with_undecorated_signature
                   ~undecorated_signature:(Some callable)
          | _ -> None
        in
        let constructor =
          let constructor = Type.TypedDictionary.constructor ~name:class_name ~fields in
          constructor
          |> create_uninstantiated_method
          |> fun uninstantiated_annotation ->
          AnnotatedAttribute.create_uninstantiated
            ~uninstantiated_annotation
            ~abstract:false
            ~async_property:false
            ~class_variable:false
            ~defined:true
            ~initialized:OnClass
            ~name:"__init__"
            ~parent:class_name
            ~visibility:ReadWrite
            ~property:false
            ~undecorated_signature:(Some constructor)
            ~problem:None
        in
        let all_special_methods =
          constructor
          ::
          (base_typed_dictionary_definition fields
          |> get_field_attributes ~include_generated_attributes:true
          |> List.filter_map ~f:overload_method)
        in
        List.iter ~f:(UninstantiatedAttributeTable.add table) all_special_methods
      in
      if include_generated_attributes then add_special_methods ();
      table

    method single_uninstantiated_attribute_table
        ~assumptions
        ~include_generated_attributes
        ~accessed_via_metaclass
        class_name =
      let handle ({ Node.value = class_summary; _ } as parent) ~in_test =
        let table = UninstantiatedAttributeTable.create () in
        let add_actual () =
          let collect_attributes attribute =
            self#create_attribute
              (Node.value attribute)
              ~assumptions
              ~parent
              ~accessed_via_metaclass
            |> UninstantiatedAttributeTable.add table
          in
          ClassSummary.attributes ~include_generated_attributes ~in_test class_summary
          |> fun attribute_map ->
          Identifier.SerializableMap.iter (fun _ data -> collect_attributes data) attribute_map
        in
        let add_placeholder_stub_inheritances () =
          let add_if_missing ~attribute_name ~annotation =
            if Option.is_none (UninstantiatedAttributeTable.lookup_name table attribute_name) then
              let callable =
                {
                  Type.Callable.kind = Anonymous;
                  implementation = { annotation; parameters = Undefined };
                  overloads = [];
                }
              in
              UninstantiatedAttributeTable.add
                table
                (AnnotatedAttribute.create_uninstantiated
                   ~uninstantiated_annotation:
                     {
                       UninstantiatedAnnotation.accessed_via_metaclass;
                       kind = Attribute (Callable callable);
                     }
                   ~abstract:false
                   ~async_property:false
                   ~class_variable:false
                   ~defined:true
                   ~initialized:OnClass
                   ~name:attribute_name
                   ~parent:class_name
                   ~visibility:ReadWrite
                   ~property:false
                   ~undecorated_signature:(Some callable)
                   ~problem:None)
            else
              ()
          in
          add_if_missing ~attribute_name:"__init__" ~annotation:Type.none;
          add_if_missing ~attribute_name:"__getattr__" ~annotation:Type.Any
        in
        add_actual ();
        if
          include_generated_attributes
          && AnnotatedBases.extends_placeholder_stub_class
               parent
               ~aliases:(aliases class_metadata_environment ~dependency)
               ~from_empty_stub:(is_suppressed_module class_metadata_environment ~dependency)
        then
          add_placeholder_stub_inheritances ();
        let () =
          if include_generated_attributes then
            ClassDecorators.apply
              ~definition:parent
              ~class_metadata_environment
              ~create_attribute:(self#create_attribute ~assumptions)
              ~instantiate_attribute:
                (self#instantiate_attribute
                   ~assumptions
                   ?instantiated:None
                   ~accessed_through_class:
                     false
                     (* TODO(T65806273): Right now we're just ignoring `__set__`s on dataclass
                        attributes. This avoids needing to explicitly break the loop that would
                        otherwise result or to somehow separate these results from the main set of
                        attributes *)
                   ~apply_descriptors:false)
              ?dependency
              table
        in
        table
      in
      match
        ( UnannotatedGlobalEnvironment.ReadOnly.get_class_summary
            (ClassMetadataEnvironment.ReadOnly.unannotated_global_environment
               class_metadata_environment)
            ?dependency
            class_name,
          ClassMetadataEnvironment.ReadOnly.get_class_metadata
            class_metadata_environment
            ?dependency
            class_name )
      with
      | Some definition, Some { is_typed_dictionary; is_test = in_test; _ } ->
          let is_declarative_sqlalchemy_class () =
            Option.equal
              Type.equal
              (self#metaclass ~assumptions class_name)
              (Some (Type.Primitive "sqlalchemy.ext.declarative.api.DeclarativeMeta"))
          in
          let table =
            if is_typed_dictionary then
              self#typed_dictionary_special_methods_table
                ~assumptions
                ~include_generated_attributes
                ~in_test
                ~accessed_via_metaclass
                ~class_metadata_environment
                ?dependency
                ~class_name
                definition
            else if is_declarative_sqlalchemy_class () then
              self#sqlalchemy_attribute_table
                ~assumptions
                ~include_generated_attributes
                ~in_test
                ~accessed_via_metaclass
                definition
            else
              handle definition ~in_test
          in
          Some table
      | _ -> None

    method uninstantiated_attribute_tables
        ~assumptions
        ~transitive
        ~accessed_through_class
        ~include_generated_attributes
        ~special_method
        class_name =
      let handle { ClassMetadataEnvironment.successors; _ } =
        let get_table ~accessed_via_metaclass =
          self#single_uninstantiated_attribute_table
            ~assumptions
            ~include_generated_attributes
            ~accessed_via_metaclass
        in
        let normal_tables =
          let normal_hierarchy =
            (* Pass over normal class hierarchy. *)
            if accessed_through_class && special_method then
              []
            else if transitive then
              class_name :: successors
            else
              [class_name]
          in
          Sequence.of_list normal_hierarchy
          |> Sequence.filter_map ~f:(get_table ~accessed_via_metaclass:false)
        in
        let metaclass_tables =
          (* We don't want to have to find our metaclass/it's parents if we successfully find the
             attribute in one of our actual parents *)
          lazy
            begin
              let metaclass_hierarchy =
                (* Class over meta hierarchy if necessary. *)
                if accessed_through_class then
                  let successors_of class_name =
                    ClassMetadataEnvironment.ReadOnly.successors
                      class_metadata_environment
                      ?dependency
                      class_name
                  in
                  self#metaclass ~assumptions class_name
                  >>| Type.split
                  >>| fst
                  >>= Type.primitive_name
                  >>| (fun metaclass -> metaclass :: successors_of metaclass)
                  |> Option.value ~default:[]
                else
                  []
              in
              metaclass_hierarchy
              |> Sequence.of_list
              |> Sequence.filter_map ~f:(get_table ~accessed_via_metaclass:true)
            end
        in
        Sequence.append normal_tables (Sequence.of_lazy metaclass_tables)
      in
      ClassMetadataEnvironment.ReadOnly.get_class_metadata
        class_metadata_environment
        ?dependency
        class_name
      >>| handle

    method attribute
        ~assumptions
        ~transitive
        ~accessed_through_class
        ~include_generated_attributes
        ?(special_method = false)
        ?instantiated
        ?apply_descriptors
        ~attribute_name
        class_name =
      let order () = self#full_order ~assumptions in
      match
        callable_call_special_cases
          ~instantiated
          ~class_name
          ~attribute_name
          ~accessed_through_class
          ~order
      with
      | Some callable ->
          AnnotatedAttribute.create
            ~annotation:callable
            ~original_annotation:callable
            ~uninstantiated_annotation:None
            ~visibility:ReadWrite
            ~abstract:false
            ~async_property:false
            ~class_variable:false
            ~defined:true
            ~initialized:OnClass
            ~name:"__call__"
            ~parent:"typing.Callable"
            ~property:false
            ~undecorated_signature:None
            ~problem:None
          |> Option.some
      | None ->
          self#uninstantiated_attribute_tables
            ~assumptions
            ~transitive
            ~accessed_through_class
            ~include_generated_attributes
            ~special_method
            class_name
          >>= Sequence.find_map ~f:(fun table ->
                  UninstantiatedAttributeTable.lookup_name table attribute_name)
          >>| self#instantiate_attribute
                ~assumptions
                ~accessed_through_class
                ?instantiated
                ?apply_descriptors

    method all_attributes
        ~assumptions
        ~transitive
        ~accessed_through_class
        ~include_generated_attributes
        ?(special_method = false)
        class_name =
      let collect sofar table =
        let add ((sofar_list, sofar_set) as sofar) attribute =
          let name = AnnotatedAttribute.name attribute in
          if Set.mem sofar_set name then
            sofar
          else
            attribute :: sofar_list, Set.add sofar_set name
        in
        UninstantiatedAttributeTable.to_list table |> List.fold ~f:add ~init:sofar
      in
      self#uninstantiated_attribute_tables
        ~assumptions
        ~transitive
        ~accessed_through_class
        ~include_generated_attributes
        ~special_method
        class_name
      >>| Sequence.fold ~f:collect ~init:([], Identifier.Set.empty)
      >>| fst
      >>| List.rev

    method attribute_names
        ~assumptions
        ~transitive
        ~accessed_through_class
        ~include_generated_attributes
        ?(special_method = false)
        class_name =
      let collect sofar table =
        let add ((sofar_list, sofar_set) as sofar) name =
          if Set.mem sofar_set name then
            sofar
          else
            name :: sofar_list, Set.add sofar_set name
        in
        UninstantiatedAttributeTable.names table |> List.fold ~f:add ~init:sofar
      in
      self#uninstantiated_attribute_tables
        ~assumptions
        ~transitive
        ~accessed_through_class
        ~include_generated_attributes
        ~special_method
        class_name
      >>| Sequence.fold ~f:collect ~init:([], Identifier.Set.empty)
      >>| fst
      >>| List.rev

    method instantiate_attribute
        ~assumptions
        ~accessed_through_class
        ?instantiated
        ?(apply_descriptors = true)
        attribute =
      let get_attribute = self#attribute in
      let class_name = AnnotatedAttribute.parent attribute in
      let attribute_name = AnnotatedAttribute.name attribute in
      let { UninstantiatedAnnotation.accessed_via_metaclass; kind = annotation } =
        AnnotatedAttribute.uninstantiated_annotation attribute
      in

      let accessed_through_class = accessed_through_class && not accessed_via_metaclass in

      let uninstantiated_annotation =
        match annotation with
        | Attribute annotation -> Some annotation
        | Property _ -> None
      in

      let annotation =
        match instantiated with
        | None -> annotation
        | Some instantiated -> (
            let solution = self#constraints ~target:class_name ~instantiated ~assumptions () in
            let instantiate annotation = ConstraintsSet.Solution.instantiate solution annotation in
            match annotation with
            | Attribute annotation -> UninstantiatedAnnotation.Attribute (instantiate annotation)
            | Property { getter; setter } ->
                let instantiate_property_annotation { UninstantiatedAnnotation.self; value } =
                  {
                    UninstantiatedAnnotation.self = self >>| instantiate;
                    value = value >>| instantiate;
                  }
                in
                Property
                  {
                    getter = instantiate_property_annotation getter;
                    setter = setter >>| instantiate_property_annotation;
                  })
      in

      let annotation, original =
        let instantiated =
          match instantiated with
          | Some instantiated -> instantiated
          | None -> Type.Primitive class_name
        in
        let instantiated =
          if accessed_via_metaclass then Type.meta instantiated else instantiated
        in
        let special_case_methods callable =
          (* Certain callables' types can't be expressed directly and need to be special cased *)
          let self_parameter =
            Type.Callable.Parameter.Named { name = "self"; annotation = Type.Top; default = false }
          in
          match instantiated, attribute_name, class_name with
          | Type.Tuple (Concrete members), "__getitem__", _ ->
              let { Type.Callable.overloads; _ } = callable in
              let overload index member =
                {
                  Type.Callable.annotation = member;
                  parameters =
                    Defined
                      [
                        self_parameter;
                        Named
                          { name = "x"; annotation = Type.literal_integer index; default = false };
                      ];
                }
              in
              let overloads =
                List.mapi ~f:overload members
                @ List.map2_exn
                    ~f:overload
                    (List.init ~f:(fun x -> -x - 1) (List.length members))
                    (List.rev members)
                @ overloads
              in
              Type.Callable { callable with overloads }
          | ( Parametric { name = "type"; parameters = [Single (Type.Primitive name)] },
              "__getitem__",
              "typing.GenericMeta" ) ->
              let implementation, overloads =
                let generics =
                  ClassHierarchyEnvironment.ReadOnly.variables
                    (ClassMetadataEnvironment.ReadOnly.class_hierarchy_environment
                       class_metadata_environment)
                    ?dependency
                    name
                  |> Option.value ~default:[]
                in
                let create_parameter annotation =
                  Type.Callable.Parameter.PositionalOnly { index = 0; annotation; default = false }
                in
                let synthetic =
                  Type.Variable
                    (Type.Variable.Unary.create "$synthetic_attribute_resolution_variable")
                in
                match name with
                (* This can't be expressed without IntVars, StrVars, and corresponding TypeVarTuple
                   variants of them *)
                | "typing_extensions.Literal"
                | "typing.Literal"
                (* TODO:(T60535947) We can't do the Map[Ts, type] -> X[Ts] trick here because we
                   don't yet support Union[Ts] *)
                | "typing.Union" ->
                    { Type.Callable.annotation = Type.meta Type.Any; parameters = Undefined }, []
                | "typing.Callable" ->
                    ( {
                        Type.Callable.annotation =
                          Type.meta (Type.Callable.create ~annotation:synthetic ());
                        parameters =
                          Defined
                            [
                              self_parameter;
                              create_parameter
                                (Type.Tuple (Concrete [Type.Any; Type.meta synthetic]));
                            ];
                      },
                      [] )
                | _ -> (
                    let overload parameter =
                      let generics = List.map generics ~f:Type.Variable.to_parameter in
                      match name, generics with
                      | "typing.Optional", [Single generic] ->
                          {
                            Type.Callable.annotation = Type.meta (Type.optional generic);
                            parameters = Defined [self_parameter; parameter];
                          }
                      | _ ->
                          {
                            Type.Callable.annotation = Type.meta (Type.parametric name generics);
                            parameters = Defined [self_parameter; parameter];
                          }
                    in
                    match generics with
                    | [Unary generic] ->
                        overload (create_parameter (Type.meta (Variable generic))), []
                    | _ ->
                        (* To support the value `GenericFoo[int, str]`, we need `class
                           GenericFoo[T1, T2]` to have:

                           `def __getitem__(cls, __x: Tuple[Type[T1], Type[T2]] ) -> GenericFoo[T1,
                           T2]`. *)
                        let meta_type_and_return_type = function
                          | Type.Variable.Unary single ->
                              ( Type.meta (Variable single),
                                Type.Parameter.Single (Type.Variable single) )
                          | ParameterVariadic _ ->
                              (* TODO:(T60536033) We'd really like to take FiniteList[Ts], but
                                 without that we can't actually return the correct metatype, which
                                 is a bummer *)
                              Type.Any, Type.Parameter.CallableParameters Undefined
                          | TupleVariadic _ -> Type.Any, Single Any
                        in
                        let meta_types, return_parameters =
                          List.map generics ~f:meta_type_and_return_type |> List.unzip
                        in
                        ( {
                            Type.Callable.annotation =
                              Type.meta (Type.parametric name return_parameters);
                            parameters =
                              Defined [self_parameter; create_parameter (Type.tuple meta_types)];
                          },
                          [] ))
              in
              Type.Callable { callable with implementation; overloads }
          | Parametric { name = "type"; parameters = [Single meta_parameter] }, "__call__", "type"
            when accessed_via_metaclass ->
              let get_constructor { Type.instantiated; accessed_through_class; class_name } =
                if accessed_through_class then (* Type[Type[X]] is invalid *)
                  None
                else
                  Some (self#constructor ~assumptions class_name ~instantiated)
              in
              Type.resolve_class meta_parameter
              >>| List.map ~f:get_constructor
              >>= Option.all
              >>| Type.union
              |> Option.value ~default:(Type.Callable callable)
          | _ -> Type.Callable callable
        in

        match annotation with
        | Property { getter = getter_annotation; setter = setter_annotation } -> (
            (* Special case properties with type variables. *)
            let solve_property
                { UninstantiatedAnnotation.self = self_annotation; value = value_annotation }
              =
              match value_annotation with
              | None -> Type.Top
              | Some annotation -> (
                  let order = self#full_order ~assumptions in
                  let constraints =
                    match self_annotation with
                    | Some annotation ->
                        TypeOrder.OrderedConstraintsSet.add
                          ConstraintsSet.empty
                          ~new_constraint:(LessOrEqual { left = instantiated; right = annotation })
                          ~order
                    | None -> ConstraintsSet.empty
                  in
                  match TypeOrder.OrderedConstraintsSet.solve ~order constraints with
                  | Some solution -> ConstraintsSet.Solution.instantiate solution annotation
                  | None -> Type.Top)
            in
            match setter_annotation with
            | Some setter_annotation ->
                solve_property getter_annotation, solve_property setter_annotation
            | None ->
                let annotation = solve_property getter_annotation in
                annotation, annotation)
        | Attribute annotation -> (
            let annotation =
              match annotation with
              | Type.Callable callable -> special_case_methods callable
              | other -> other
            in
            let order () = self#full_order ~assumptions in
            let special =
              callable_call_special_cases
                ~instantiated:(Some instantiated)
                ~class_name
                ~attribute_name
                ~order
                ~accessed_through_class
              >>| fun callable -> callable, callable
            in
            match special with
            | Some special -> special
            | None
              when [%compare.equal: AnnotatedAttribute.initialized]
                     (AnnotatedAttribute.initialized attribute)
                     OnClass
                   && apply_descriptors -> (
                let call_dunder_get (descriptor, callable) =
                  let selection_result =
                    self#signature_select
                      ~assumptions
                      ~arguments:
                        [
                          { Argument.kind = Positional; expression = None; resolved = descriptor };
                          {
                            Argument.kind = Positional;
                            expression = None;
                            resolved = (if accessed_through_class then Type.none else instantiated);
                          };
                          {
                            Argument.kind = Positional;
                            expression = None;
                            resolved = Type.meta instantiated;
                          };
                        ]
                      ~resolve_with_locals:(fun ~locals:_ _ -> Type.object_primitive)
                      ~callable
                      ~self_argument:None
                      ~skip_marking_escapees:true
                  in
                  match selection_result with
                  | SignatureSelectionTypes.NotFound _ -> None
                  | Found { selected_return_annotation = return } -> Some return
                in
                let invert_dunder_set (descriptor, callable) ~order =
                  let synthetic = Type.Variable.Unary.create "$synthetic_dunder_set_variable" in
                  let right =
                    Type.Callable.create
                      ~annotation:Type.none
                      ~parameters:
                        (Defined
                           [
                             PositionalOnly { index = 0; annotation = descriptor; default = false };
                             PositionalOnly
                               { index = 1; annotation = instantiated; default = false };
                             PositionalOnly
                               { index = 2; annotation = Variable synthetic; default = false };
                           ])
                      ()
                  in
                  TypeOrder.OrderedConstraintsSet.add
                    ConstraintsSet.empty
                    ~new_constraint:(LessOrEqual { left = Type.Callable callable; right })
                    ~order
                  |> TypeOrder.OrderedConstraintsSet.solve ~order
                  >>= fun solution ->
                  ConstraintsSet.Solution.instantiate_single_variable solution synthetic
                in
                let function_dunder_get callable =
                  if accessed_through_class then
                    Type.Callable callable
                  else
                    Type.parametric "BoundMethod" [Single (Callable callable); Single instantiated]
                in

                let get_descriptor_method
                    { Type.instantiated; accessed_through_class; class_name }
                    ~kind
                  =
                  if accessed_through_class then
                    (* descriptor methods are statically looked up on the class (in this case
                       `type`), not on the instance. `type` is not a descriptor. *)
                    `NotDescriptor (Type.meta instantiated)
                  else
                    match instantiated with
                    | Callable callable -> (
                        match kind with
                        | `DunderGet ->
                            (* We unsoundly assume all callables are callables with the `function`
                               `__get__` *)
                            `HadDescriptor (function_dunder_get callable)
                        | `DunderSet -> `NotDescriptor instantiated)
                    | _ -> (
                        let attribute =
                          let attribute_name =
                            match kind with
                            | `DunderGet -> "__get__"
                            | `DunderSet -> "__set__"
                          in
                          (* descriptor methods are statically looked up on the class, and are not
                             themselves subject to description *)
                          get_attribute
                            ~assumptions
                            ~transitive:true
                            ~accessed_through_class:true
                            ~include_generated_attributes:true
                            ?special_method:None
                            ?instantiated:(Some instantiated)
                            ?apply_descriptors:(Some false)
                            ~attribute_name
                            class_name
                          >>| AnnotatedAttribute.annotation
                          >>| Annotation.annotation
                        in
                        match attribute with
                        | None -> `NotDescriptor instantiated
                        | Some (Type.Callable callable) ->
                            let extracted =
                              match kind with
                              | `DunderGet -> call_dunder_get (instantiated, callable)
                              | `DunderSet ->
                                  invert_dunder_set ~order:(order ()) (instantiated, callable)
                            in
                            extracted
                            >>| (fun extracted -> `HadDescriptor extracted)
                            |> Option.value ~default:`FailedToExtract
                        | Some _ ->
                            (* In theory we could support `__get__`s or `__set__`s that are not just
                               Callables, but for now lets just ignore that *)
                            `DescriptorNotACallable)
                in

                match Type.resolve_class annotation with
                | None ->
                    (* This means we have a type that can't be `Type.split`, (most of) which aren't
                       descriptors, so we should be usually safe to just ignore. In general we
                       should fix resolve_class to always return something. *)
                    annotation, annotation
                | Some elements ->
                    let collect x =
                      let partitioner = function
                        | `NotDescriptor element -> `Fst element
                        | `HadDescriptor element -> `Snd element
                        (* Every descriptor should accept all hosts (and all host types) as a matter
                           of Liskov substitutibility with `object`. This means we need to error on
                           these invalid definitions (T65807232), and not on usages *)
                        | `FailedToExtract
                        | `DescriptorNotACallable ->
                            `Trd ()
                      in
                      match List.partition3_map x ~f:partitioner with
                      | _, _, _ :: _ ->
                          (* If we have broken descriptor methods we should error on them, not their
                             usages *)
                          Type.Any
                      | _, [], _ ->
                          (* If none of the components are descriptors, we don't need to worry about
                             re-unioning together the components we split apart, we can just give
                             back the original type *)
                          annotation
                      | normal, had_descriptors, _ -> Type.union (normal @ had_descriptors)
                    in

                    let elements_and_get_results =
                      List.map elements ~f:(fun element ->
                          element, get_descriptor_method element ~kind:`DunderGet)
                    in

                    let get_type = List.unzip elements_and_get_results |> snd |> collect in
                    let set_type =
                      if accessed_through_class then
                        annotation
                      else
                        let process (element, get_result) =
                          match get_descriptor_method element ~kind:`DunderSet, get_result with
                          | `NotDescriptor _, `HadDescriptor element ->
                              (* non-data descriptors set type should be their get type *)
                              `HadDescriptor element
                          | other, _ -> other
                        in
                        List.map elements_and_get_results ~f:process |> collect
                    in
                    get_type, set_type)
            | None -> annotation, annotation)
      in
      AnnotatedAttribute.instantiate
        attribute
        ~annotation
        ~original_annotation:original
        ~uninstantiated_annotation

    method create_attribute
        ~assumptions
        ~parent
        ?(defined = true)
        ~accessed_via_metaclass
        { Attribute.name = attribute_name; kind } =
      let { Node.value = { ClassSummary.name = parent_name; _ }; _ } = parent in
      let parent_name = Reference.show parent_name in
      let class_annotation = Type.Primitive parent_name in
      let annotation, class_variable, visibility, undecorated_signature, problem =
        match kind with
        | Simple { annotation; values; frozen; toplevel; implicit; primitive; _ } ->
            let value = List.hd values >>| fun { value; _ } -> value in
            let parsed_annotation = annotation >>| self#parse_annotation ~assumptions in
            (* Account for class attributes. *)
            let annotation, final, class_variable =
              parsed_annotation
              >>| (fun annotation ->
                    let process_class_variable annotation =
                      match Type.class_variable_value annotation with
                      | Some annotation -> true, annotation
                      | None -> false, annotation
                    in
                    match Type.final_value annotation with
                    | `NoParameter -> None, true, false
                    | `NotFinal ->
                        let is_class_variable, annotation = process_class_variable annotation in
                        Some annotation, false, is_class_variable
                    | `Ok annotation ->
                        let is_class_variable, annotation = process_class_variable annotation in
                        Some annotation, true, is_class_variable)
              |> Option.value ~default:(None, false, false)
            in
            (* Handle enumeration attributes. *)
            let annotation, visibility =
              let superclasses =
                ClassMetadataEnvironment.ReadOnly.successors
                  class_metadata_environment
                  ?dependency
                  parent_name
                |> String.Set.of_list
              in
              if
                (not (Set.mem Recognized.enumeration_classes (Type.show class_annotation)))
                && (not (Set.is_empty (Set.inter Recognized.enumeration_classes superclasses)))
                && primitive
                && defined
                && not implicit
              then
                ( Some
                    (Type.Literal
                       (Type.EnumerationMember
                          { enumeration_type = class_annotation; member_name = attribute_name })),
                  AnnotatedAttribute.ReadOnly (Refinable { overridable = true }) )
              else
                let visibility =
                  if final then
                    AnnotatedAttribute.ReadOnly (Refinable { overridable = false })
                  else if frozen then
                    ReadOnly (Refinable { overridable = true })
                  else
                    ReadWrite
                in
                annotation, visibility
            in
            (* Try resolve to tuple of string literal types for __match_args__ *)
            let annotation =
              let open Expression in
              match attribute_name, annotation, value with
              | "__match_args__", None, Some { Node.value = Expression.Tuple elements; _ } ->
                  let string_literal_value_to_type = function
                    | {
                        Node.value =
                          Expression.Constant
                            (Constant.String { StringLiteral.kind = String; value });
                        _;
                      } ->
                        Some (Type.Literal (String (LiteralValue value)))
                    | _ -> None
                  in
                  List.map elements ~f:string_literal_value_to_type |> Option.all >>| Type.tuple
              | _ -> annotation
            in
            let annotation =
              match annotation, value with
              | Some annotation, _ -> annotation
              | None, Some value ->
                  let literal_value_annotation = self#resolve_literal ~assumptions value in
                  let is_dataclass_attribute =
                    UnannotatedGlobalEnvironment.ReadOnly.exists_matching_class_decorator
                      (unannotated_global_environment class_metadata_environment)
                      ?dependency
                      ~names:["dataclasses.dataclass"; "dataclass"]
                      parent
                  in
                  if
                    (not (Type.is_partially_typed literal_value_annotation))
                    && (not is_dataclass_attribute)
                    && toplevel
                  then (* Treat literal attributes as having been explicitly annotated. *)
                    literal_value_annotation
                  else
                    Type.Top
              | _ -> Type.Top
            in
            UninstantiatedAnnotation.Attribute annotation, class_variable, visibility, None, None
        | Method { signatures; final; _ } ->
            (* Handle Callables *)
            let visibility =
              if final then
                AnnotatedAttribute.ReadOnly (Refinable { overridable = false })
              else
                ReadWrite
            in
            let callable, undecorated_signature, problem =
              let overloads =
                let create_overload define =
                  Define.Signature.is_overloaded_function define, define
                in
                List.map signatures ~f:create_overload
              in
              let implementation, overloads =
                let to_signature (implementation, overloads) (is_overload, signature) =
                  if is_overload then
                    implementation, signature :: overloads
                  else
                    Some signature, overloads
                in
                List.fold ~init:(None, []) ~f:to_signature overloads
              in
              let { decorated; undecorated_signature } =
                self#resolve_define ~implementation ~overloads ~assumptions
              in
              let annotation =
                match decorated with
                | Ok resolved -> (
                    match attribute_name, resolved with
                    (* these names are only magic-ed into being ClassMethods/StaticMethods if
                       they're "plain functions". We can't capture that in the type system, so we
                       approximate with Callable *)
                    | "__new__", Callable _ ->
                        Type.parametric "typing.StaticMethod" [Single resolved]
                    | "__init_subclass__", Callable _
                    | "__class_getitem__", Callable _ ->
                        Type.parametric "typing.ClassMethod" [Single resolved]
                    | _ -> resolved)
                | Error _ -> Any
              in
              ( UninstantiatedAnnotation.Attribute annotation,
                undecorated_signature,
                Result.error decorated )
            in
            callable, false, visibility, Some undecorated_signature, problem
        | Property { kind; _ } -> (
            let parse_annotation_option annotation =
              annotation >>| self#parse_annotation ~assumptions
            in
            match kind with
            | ReadWrite
                {
                  getter = { self = getter_self_annotation; return = getter_annotation; _ };
                  setter = { self = setter_self_annotation; value = setter_annotation; _ };
                } ->
                let getter_annotation = parse_annotation_option getter_annotation in
                let setter_annotation = parse_annotation_option setter_annotation in
                ( UninstantiatedAnnotation.Property
                    {
                      getter =
                        {
                          self = parse_annotation_option getter_self_annotation;
                          value = getter_annotation;
                        };
                      setter =
                        Some
                          {
                            self = parse_annotation_option setter_self_annotation;
                            value = setter_annotation;
                          };
                    },
                  false,
                  ReadWrite,
                  None,
                  None )
            | ReadOnly { getter = { self = self_annotation; return = getter_annotation; _ } } ->
                let annotation = parse_annotation_option getter_annotation in
                ( UninstantiatedAnnotation.Property
                    {
                      getter =
                        { self = parse_annotation_option self_annotation; value = annotation };
                      setter = None;
                    },
                  false,
                  ReadOnly Unrefinable,
                  None,
                  None ))
      in
      let initialized =
        match kind with
        | Simple { nested_class = true; _ } -> AnnotatedAttribute.OnClass
        | Simple { values; _ } ->
            let is_not_ellipsis = function
              | { Attribute.value = { Node.value = Constant Expression.Constant.Ellipsis; _ }; _ }
                ->
                  false
              | _ -> true
            in
            List.find values ~f:is_not_ellipsis
            >>| (function
                  | { Attribute.origin = Explicit; _ } -> AnnotatedAttribute.OnClass
                  | { origin = Implicit; _ } -> OnlyOnInstance)
            |> Option.value ~default:AnnotatedAttribute.NotInitialized
        | Method _
        | Property { class_property = true; _ } ->
            OnClass
        | Property { class_property = false; _ } -> OnlyOnInstance
      in
      AnnotatedAttribute.create_uninstantiated
        ~uninstantiated_annotation:
          { UninstantiatedAnnotation.accessed_via_metaclass; kind = annotation }
        ~visibility
        ~abstract:
          (match kind with
          | Method { signatures; _ } ->
              List.exists signatures ~f:Define.Signature.is_abstract_method
          | _ -> false)
        ~async_property:
          (match kind with
          | Property { async; _ } -> async
          | _ -> false)
        ~class_variable
        ~defined
        ~initialized
        ~name:attribute_name
        ~parent:parent_name
        ~property:
          (match kind with
          | Property _ -> true
          | _ -> false)
        ~undecorated_signature
        ~problem

    method metaclass ~assumptions target =
      (* See
         https://docs.python.org/3/reference/datamodel.html#determining-the-appropriate-metaclass
         for why we need to consider all metaclasses. *)
      let rec handle
          ({ Node.value = { ClassSummary.bases = { base_classes; metaclass; _ }; _ }; _ } as
          original)
        =
        let open Expression in
        let parse_annotation = self#parse_annotation ~assumptions ?validation:None in
        let metaclass_candidates =
          let explicit_metaclass = metaclass >>| parse_annotation in
          let metaclass_of_bases =
            let explicit_bases =
              let base_to_class base_expression =
                delocalize base_expression |> parse_annotation |> Type.split |> fst
              in
              base_classes
              |> List.map ~f:base_to_class
              |> List.filter_map ~f:(class_summary class_metadata_environment ~dependency)
              |> List.filter ~f:(fun base_class ->
                     not ([%compare.equal: ClassSummary.t Node.t] base_class original))
            in
            let filter_generic_meta base_metaclasses =
              (* We only want a class directly inheriting from Generic to have a metaclass of
                 GenericMeta. *)
              if
                List.exists
                  ~f:(fun base ->
                    Reference.equal (Reference.create "typing.Generic") (class_name base))
                  explicit_bases
              then
                base_metaclasses
              else
                List.filter
                  ~f:(fun metaclass ->
                    not (Type.equal (Type.Primitive "typing.GenericMeta") metaclass))
                  base_metaclasses
            in
            explicit_bases |> List.map ~f:handle |> filter_generic_meta
          in
          match explicit_metaclass with
          | Some metaclass -> metaclass :: metaclass_of_bases
          | None -> metaclass_of_bases
        in
        match metaclass_candidates with
        | [] -> Type.Primitive "type"
        | first :: candidates -> (
            let order = self#full_order ~assumptions in
            let candidate = List.fold candidates ~init:first ~f:(TypeOrder.meet order) in
            match candidate with
            | Type.Bottom ->
                (* If we get Bottom here, we don't have a "most derived metaclass", so default to
                   one. *)
                first
            | _ -> candidate)
      in
      UnannotatedGlobalEnvironment.ReadOnly.get_class_summary
        (ClassMetadataEnvironment.ReadOnly.unannotated_global_environment
           class_metadata_environment)
        ?dependency
        target
      >>| handle

    method constraints ~assumptions ~target ?parameters ~instantiated () =
      let parameters =
        match parameters with
        | None ->
            ClassHierarchyEnvironment.ReadOnly.variables
              (ClassMetadataEnvironment.ReadOnly.class_hierarchy_environment
                 class_metadata_environment)
              ?dependency
              target
            >>| List.map ~f:Type.Variable.to_parameter
            |> Option.value ~default:[]
        | Some parameters -> parameters
      in
      if List.is_empty parameters then
        ConstraintsSet.Solution.empty
      else
        let right = Type.parametric target parameters in
        match instantiated, right with
        | Type.Primitive name, Parametric { name = right_name; _ } when String.equal name right_name
          ->
            (* TODO(T42259381) This special case is only necessary because constructor calls
               attributes with an "instantiated" type of a bare parametric, which will fill with
               Anys *)
            ConstraintsSet.Solution.empty
        | _ ->
            let order = self#full_order ~assumptions in
            TypeOrder.OrderedConstraintsSet.add
              ConstraintsSet.empty
              ~new_constraint:(LessOrEqual { left = instantiated; right })
              ~order
            |> TypeOrder.OrderedConstraintsSet.solve ~order
            (* TODO(T39598018): error in this case somehow, something must be wrong *)
            |> Option.value ~default:ConstraintsSet.Solution.empty

    (* In general, python expressions can be self-referential. This resolution only checks literals
       and annotations found in the resolution map, without resolving expressions. *)
    method resolve_literal ~assumptions expression =
      let open Ast.Expression in
      let is_concrete_class class_type =
        class_type
        |> class_summary class_metadata_environment ~dependency
        >>| (fun { Node.value = { name; _ }; _ } -> Reference.show name)
        >>= ClassHierarchyEnvironment.ReadOnly.variables
              (class_hierarchy_environment class_metadata_environment)
              ?dependency
              ~default:(Some [])
        >>| List.is_empty
      in
      let fully_specified_type = function
        | { Node.value = Expression.Name name; _ } as annotation when is_simple_name name ->
            let class_type = self#parse_annotation ~assumptions annotation in
            if
              Type.is_none class_type || is_concrete_class class_type |> Option.value ~default:false
            then
              Some class_type
            else
              None
        | {
            Node.value =
              Call
                {
                  callee =
                    {
                      value =
                        Name
                          (Name.Attribute
                            {
                              base = { Node.value = Name generic_name; _ };
                              attribute = "__getitem__";
                              special = true;
                            });
                      _;
                    };
                  _;
                };
            _;
          } as annotation
          when is_simple_name generic_name ->
            let class_type = self#parse_annotation ~assumptions annotation in
            if is_concrete_class class_type >>| not |> Option.value ~default:false then
              Some class_type
            else
              None
        | _ -> None
      in
      let resolve_name expression =
        if has_identifier_base expression then
          match fully_specified_type expression with
          | Some annotation ->
              if Type.is_none annotation then
                Type.none
              else
                Type.meta annotation
          | None -> Type.Any
        else
          Type.Any
      in

      let order = self#full_order ~assumptions in
      match Node.value expression with
      | Expression.Await expression ->
          self#resolve_literal ~assumptions expression
          |> Type.awaitable_value
          |> Option.value ~default:Type.Any
      | BooleanOperator { BooleanOperator.left; right; _ } ->
          let annotation =
            TypeOrder.join
              order
              (self#resolve_literal ~assumptions left)
              (self#resolve_literal ~assumptions right)
          in
          if Type.is_concrete annotation then annotation else Type.Any
      | Call { callee; _ } -> (
          match fully_specified_type expression with
          | Some annotation ->
              (* Literal generic type, e.g. global = List[int] *)
              Type.meta annotation
          | None ->
              (* Constructor on concrete class or fully specified generic,
               * e.g. global = GenericClass[int](x, y) or global = ConcreteClass(x) *)
              Option.value (fully_specified_type callee) ~default:Type.Any)
      | Constant Constant.NoneLiteral -> Type.Any
      | Constant (Constant.Complex _) -> Type.complex
      | Constant (Constant.False | Constant.True) -> Type.bool
      | Constant (Constant.Float _) -> Type.float
      | Constant (Constant.Integer _) -> Type.integer
      | Constant (Constant.String { StringLiteral.kind; _ }) -> (
          match kind with
          | StringLiteral.Bytes -> Type.bytes
          | _ -> Type.string)
      | FormatString _ -> Type.string
      | Name name when is_simple_name name -> (
          let reference = name_to_reference_exn name in
          let unannotated_global_environment =
            unannotated_global_environment class_metadata_environment
          in
          match
            UnannotatedGlobalEnvironment.ReadOnly.get_unannotated_global
              ?dependency
              unannotated_global_environment
              reference
          with
          | Some (UnannotatedGlobal.Define defines) ->
              let { decorated; _ } =
                List.map defines ~f:(fun { define; _ } -> define)
                |> List.partition_tf ~f:Define.Signature.is_overloaded_function
                |> fun (overloads, implementations) ->
                self#resolve_define
                  ~assumptions
                  ~implementation:(List.last implementations)
                  ~overloads
              in
              Result.ok decorated |> Option.value ~default:Type.Any
          | _ -> resolve_name expression)
      | Name _ -> resolve_name expression
      | Dictionary { Dictionary.entries; keywords = [] } ->
          let key_annotation, value_annotation =
            let join_entry (key_annotation, value_annotation) { Dictionary.Entry.key; value } =
              ( TypeOrder.join order key_annotation (self#resolve_literal ~assumptions key),
                TypeOrder.join order value_annotation (self#resolve_literal ~assumptions value) )
            in
            List.fold ~init:(Type.Bottom, Type.Bottom) ~f:join_entry entries
          in
          let key = if Type.is_concrete key_annotation then key_annotation else Type.Any in
          let value = if Type.is_concrete value_annotation then value_annotation else Type.Any in
          Type.dictionary ~key ~value
      | Dictionary _ -> Type.dictionary ~key:Type.Any ~value:Type.Any
      | List elements ->
          let parameter =
            let join sofar element =
              TypeOrder.join order sofar (self#resolve_literal ~assumptions element)
            in
            List.fold ~init:Type.Bottom ~f:join elements
          in
          if Type.is_concrete parameter then Type.list parameter else Type.list Type.Any
      | Set elements ->
          let parameter =
            let join sofar element =
              TypeOrder.join order sofar (self#resolve_literal ~assumptions element)
            in
            List.fold ~init:Type.Bottom ~f:join elements
          in
          if Type.is_concrete parameter then Type.set parameter else Type.set Type.Any
      | Ternary { Ternary.target; alternative; _ } ->
          let annotation =
            TypeOrder.join
              order
              (self#resolve_literal ~assumptions target)
              (self#resolve_literal ~assumptions alternative)
          in
          if Type.is_concrete annotation then annotation else Type.Any
      | Tuple elements -> Type.tuple (List.map elements ~f:(self#resolve_literal ~assumptions))
      | Expression.Yield _ -> Type.yield Type.Any
      | _ -> Type.Any

    method resolve_define ~assumptions ~implementation ~overloads =
      let apply_decorator (index, decorator) argument =
        let make_error reason =
          Result.Error (AnnotatedAttribute.InvalidDecorator { index; reason })
        in
        match Decorator.from_expression decorator with
        | None -> make_error CouldNotResolve
        | Some { Decorator.name; arguments } -> (
            let name = Node.value name |> Reference.delocalize in
            let decorator =
              UnannotatedGlobalEnvironment.ReadOnly.resolve_exports
                (unannotated_global_environment class_metadata_environment)
                ?dependency
                name
            in
            let simple_decorator_name =
              match decorator with
              | Some (ModuleAttribute { from; name; remaining; _ }) ->
                  Reference.create_from_list (Reference.as_list from @ name :: remaining)
                  |> Reference.show
              | _ -> Reference.show name
            in
            match simple_decorator_name, argument with
            | ( ("click.decorators.pass_context" | "click.decorators.pass_obj"),
                Type.Callable callable ) ->
                (* Suppress caller/callee parameter matching by altering the click entry point to
                   have a generic parameter list. *)
                let parameters =
                  Type.Callable.Defined
                    [
                      Type.Callable.Parameter.Variable (Concrete Type.Any);
                      Type.Callable.Parameter.Keywords Type.Any;
                    ]
                in
                Type.Callable (Type.Callable.map_parameters callable ~f:(fun _ -> parameters))
                |> Result.return
            | name, Callable callable
              when String.equal name "contextlib.asynccontextmanager"
                   || Set.mem Recognized.asyncio_contextmanager_decorators name ->
                let process_overload ({ Type.Callable.annotation; _ } as overload) =
                  let joined =
                    let order = self#full_order ~assumptions in
                    try TypeOrder.join order annotation (Type.async_iterator Type.Bottom) with
                    | ClassHierarchy.Untracked _ ->
                        (* create_overload gets called when building the environment, which is
                           unsound and can raise. *)
                        Type.Any
                  in
                  if Type.is_async_iterator joined then
                    {
                      overload with
                      Type.Callable.annotation =
                        Type.parametric
                          "typing.AsyncContextManager"
                          [Single (Type.single_parameter joined)];
                    }
                  else
                    overload
                in
                let {
                  Type.Callable.implementation = old_implementation;
                  overloads = old_overloads;
                  _;
                }
                  =
                  callable
                in
                Type.Callable
                  {
                    callable with
                    implementation = process_overload old_implementation;
                    overloads = List.map old_overloads ~f:process_overload;
                  }
                |> Result.return
            | name, callable when String.is_suffix name ~suffix:".validator" ->
                (* TODO(T70606997): We should be type checking attr validators properly. *)
                Result.return callable
            | "contextlib.contextmanager", Callable callable ->
                let process_overload ({ Type.Callable.annotation; _ } as overload) =
                  let joined =
                    let order = self#full_order ~assumptions in
                    try TypeOrder.join order annotation (Type.iterator Type.Bottom) with
                    | ClassHierarchy.Untracked _ ->
                        (* create_overload gets called when building the environment, which is
                           unsound and can raise. *)
                        Type.Any
                  in
                  if Type.is_iterator joined then
                    {
                      overload with
                      Type.Callable.annotation =
                        Type.parametric
                          "contextlib._GeneratorContextManager"
                          [Single (Type.single_parameter joined)];
                    }
                  else
                    overload
                in
                let {
                  Type.Callable.implementation = old_implementation;
                  overloads = old_overloads;
                  _;
                }
                  =
                  callable
                in
                Type.Callable
                  {
                    callable with
                    implementation = process_overload old_implementation;
                    overloads = List.map old_overloads ~f:process_overload;
                  }
                |> Result.return
            | name, argument when Set.mem Decorators.special_decorators name ->
                Decorators.apply ~argument ~name |> Result.return
            | name, _ when Set.mem Recognized.classmethod_decorators name ->
                (* TODO (T67024249): convert these to just normal stubs *)
                Type.parametric "typing.ClassMethod" [Single argument] |> Result.return
            | "staticmethod", _ ->
                Type.parametric "typing.StaticMethod" [Single argument] |> Result.return
            | _ -> (
                let { decorator_assumptions; _ } = assumptions in
                if
                  Assumptions.DecoratorAssumptions.not_a_decorator
                    decorator_assumptions
                    ~candidate:name
                then
                  make_error CouldNotResolve
                else
                  let assumptions =
                    {
                      assumptions with
                      decorator_assumptions =
                        Assumptions.DecoratorAssumptions.add
                          decorator_assumptions
                          ~assume_is_not_a_decorator:name;
                    }
                  in
                  let resolve_attribute_access ?special_method base ~attribute_name =
                    let access { Type.instantiated; accessed_through_class; class_name } =
                      self#attribute
                        ~assumptions
                        ~transitive:true
                        ~accessed_through_class
                        ~include_generated_attributes:true
                        ?special_method
                        ~attribute_name
                        ~instantiated
                        class_name
                    in
                    let join_all = function
                      | head :: tail ->
                          let order = self#full_order ~assumptions in
                          List.fold tail ~init:head ~f:(TypeOrder.join order) |> Option.some
                      | [] -> None
                    in
                    Type.resolve_class base
                    >>| List.map ~f:access
                    >>= Option.all
                    >>| List.map ~f:AnnotatedAttribute.annotation
                    >>| List.map ~f:Annotation.annotation
                    >>= join_all
                  in
                  let resolver = function
                    | UnannotatedGlobalEnvironment.ResolvedReference.Module _ -> None
                    | PlaceholderStub _ -> Some Type.Any
                    | ModuleAttribute { from; name; remaining; _ } ->
                        let rec resolve_remaining base ~remaining =
                          match remaining with
                          | [] -> Some base
                          | attribute_name :: remaining ->
                              resolve_attribute_access base ~attribute_name
                              >>= resolve_remaining ~remaining
                        in
                        Reference.create_from_list [name]
                        |> Reference.combine from
                        |> self#global_annotation ~assumptions
                        >>| (fun { Global.annotation = { annotation; _ }; _ } -> annotation)
                        >>= resolve_remaining ~remaining
                  in
                  let extract_callable = function
                    | Type.Callable callable -> Some callable
                    | other -> (
                        match
                          resolve_attribute_access
                            other
                            ~attribute_name:"__call__"
                            ~special_method:true
                        with
                        | None -> None
                        | Some (Type.Callable callable) -> Some callable
                        | Some other -> (
                            (* We potentially need to go specifically two layers in order to support
                               when name resolves to Type[X], which has a __call__ of its
                               constructor that is itself a BoundMethod, which has a Callable
                               __call__ *)
                            match
                              resolve_attribute_access
                                other
                                ~attribute_name:"__call__"
                                ~special_method:true
                            with
                            | Some (Callable callable) -> Some callable
                            | _ -> None))
                  in
                  let apply_arguments_to_decorator_factory ~factory_callable ~arguments =
                    let arguments =
                      let resolve argument_index argument =
                        let expression, kind = Ast.Expression.Call.Argument.unpack argument in
                        let make_matched_argument resolved =
                          { Argument.kind; expression = Some expression; resolved }
                        in
                        let error = AnnotatedAttribute.CouldNotResolveArgument { argument_index } in
                        match expression with
                        | {
                         Node.value = Expression.Expression.Constant Expression.Constant.NoneLiteral;
                         _;
                        } ->
                            Ok (make_matched_argument Type.NoneType)
                        | { Node.value = Expression.Expression.Name name; _ } ->
                            Expression.name_to_reference name
                            >>| Reference.delocalize
                            >>= UnannotatedGlobalEnvironment.ReadOnly.resolve_exports
                                  (unannotated_global_environment class_metadata_environment)
                                  ?dependency
                            >>= resolver
                            >>| make_matched_argument
                            |> Result.of_option
                                 ~error:
                                   (AnnotatedAttribute.InvalidDecorator { index; reason = error })
                        | expression ->
                            let resolved = self#resolve_literal ~assumptions expression in
                            if Type.is_untyped resolved || Type.contains_unknown resolved then
                              make_error error
                            else
                              Ok (make_matched_argument resolved)
                      in
                      List.mapi arguments ~f:resolve |> Result.all
                    in
                    let select arguments =
                      self#signature_select
                        ~assumptions
                        ~resolve_with_locals:(fun ~locals:_ _ -> Type.Top)
                        ~arguments
                        ~callable:factory_callable
                        ~self_argument:None
                        ~skip_marking_escapees:false
                    in
                    let extract = function
                      | SignatureSelectionTypes.Found { selected_return_annotation; _ } ->
                          Result.Ok selected_return_annotation
                      | NotFound { reason; _ } ->
                          make_error
                            (FactorySignatureSelectionFailed { reason; callable = factory_callable })
                    in
                    Result.map arguments ~f:select |> Result.bind ~f:extract
                  in
                  let resolved_decorator =
                    match decorator >>= resolver with
                    | None -> make_error CouldNotResolve
                    | Some Any -> Ok Type.Any
                    | Some fetched -> (
                        match arguments with
                        | None -> Ok fetched
                        | Some arguments -> (
                            match extract_callable fetched with
                            | None -> make_error (NonCallableDecoratorFactory fetched)
                            | Some factory_callable ->
                                apply_arguments_to_decorator_factory ~factory_callable ~arguments))
                  in
                  match resolved_decorator with
                  | Error error -> Result.Error error
                  | Ok Any -> Ok Any
                  | Ok resolved_decorator -> (
                      match extract_callable resolved_decorator with
                      | None -> make_error (NonCallableDecorator resolved_decorator)
                      | Some callable -> (
                          match
                            self#signature_select
                              ~assumptions
                              ~resolve_with_locals:(fun ~locals:_ _ -> Type.object_primitive)
                              ~arguments:
                                [{ kind = Positional; expression = None; resolved = argument }]
                              ~callable
                              ~self_argument:None
                              ~skip_marking_escapees:false
                          with
                          | SignatureSelectionTypes.Found { selected_return_annotation; _ } ->
                              Ok selected_return_annotation
                          | NotFound { reason; _ } ->
                              make_error (ApplicationFailed { reason; callable })))))
      in
      let parse =
        let parser =
          {
            AnnotatedCallable.parse_annotation = self#parse_annotation ~assumptions;
            parse_as_parameter_specification_instance_annotation =
              AliasEnvironment.ReadOnly.parse_as_parameter_specification_instance_annotation
                (alias_environment class_metadata_environment)
                ?dependency;
          }
        in
        let variables =
          ClassHierarchyEnvironment.ReadOnly.variables
            (ClassMetadataEnvironment.ReadOnly.class_hierarchy_environment
               class_metadata_environment)
            ?dependency
        in
        AnnotatedCallable.create_overload_without_applying_decorators ~parser ~variables
      in
      (*parsed, decorators |> List.rev |> List.fold ~init:parsed ~f:apply_decorator*)
      let kind =
        match implementation, overloads with
        | Some { Define.Signature.name; _ }, _
        | _, { Define.Signature.name; _ } :: _ ->
            Type.Callable.Named name
        | None, [] ->
            (* Should never happen, but not worth crashing over *)
            Type.Callable.Anonymous
      in
      let undefined_overload =
        { Type.Callable.annotation = Type.Top; parameters = Type.Callable.Undefined }
      in
      let parsed_overloads, parsed_implementation, decorators =
        match overloads, implementation with
        | ({ decorators = head_decorators; _ } as overload) :: tail, _ ->
            let purify =
              let is_not_overload_decorator decorator =
                not
                  (Ast.Statement.Define.Signature.is_overloaded_function
                     { overload with decorators = [decorator] })
              in
              List.filter ~f:is_not_overload_decorator
            in
            let enforce_equality ~parsed ~current sofar =
              let equal left right =
                Int.equal (Ast.Expression.location_insensitive_compare left right) 0
              in
              if List.equal equal sofar (purify current) then
                Ok sofar
              else
                Error (AnnotatedAttribute.DifferingDecorators { offender = parsed })
            in
            let reversed_parsed_overloads, decorators =
              let collect
                  (reversed_parsed_overloads, decorators_sofar)
                  ({ Define.Signature.decorators = current; _ } as overload)
                =
                let parsed = parse overload in
                ( parsed :: reversed_parsed_overloads,
                  Result.bind decorators_sofar ~f:(enforce_equality ~parsed ~current) )
              in
              List.fold tail ~f:collect ~init:([parse overload], Result.Ok (purify head_decorators))
            in
            let parsed_implementation, decorators =
              match implementation with
              | Some ({ Define.Signature.decorators = current; _ } as implementation) ->
                  let parsed = parse implementation in
                  Some parsed, Result.bind decorators ~f:(enforce_equality ~parsed ~current)
              | None -> None, decorators
            in
            List.rev reversed_parsed_overloads, parsed_implementation, decorators
        | [], Some { decorators; _ } -> [], implementation >>| parse, Result.Ok decorators
        | [], None -> [], None, Ok []
      in
      let undecorated_signature =
        {
          Type.Callable.implementation =
            parsed_implementation |> Option.value ~default:undefined_overload;
          overloads = parsed_overloads;
          kind;
        }
      in
      let decorated =
        let apply_decorators decorators =
          let applied =
            let apply_decorator sofar current = Result.bind sofar ~f:(apply_decorator current) in
            List.mapi decorators ~f:(fun index decorator -> index, decorator)
            |> List.rev
            |> List.fold ~init:(Ok (Type.Callable undecorated_signature)) ~f:apply_decorator
          in
          match applied with
          | Result.Ok (Type.Callable callable)
            when Type.Callable.equal { callable with kind } undecorated_signature ->
              (* Do some amateur taint analysis and assume that this is calling the original
                 function under the hood *)
              Ok (Type.Callable { callable with kind })
          | other -> other
        in
        Result.bind decorators ~f:apply_decorators
      in
      { undecorated_signature; decorated }

    method signature_select
        ~assumptions
        ~resolve_with_locals
        ~arguments
        ~callable:({ Type.Callable.implementation; overloads; _ } as callable)
        ~self_argument
        ~skip_marking_escapees =
      let order = self#full_order ~assumptions in
      let get_match signatures =
        let check_arguments_against_signature =
          SignatureSelection.check_arguments_against_signature
            ~order
            ~resolve_mutable_literals:(self#resolve_mutable_literals ~assumptions)
            ~resolve_with_locals
            ~callable
            ~self_argument
            ~arguments:
              (SignatureSelection.prepare_arguments_for_signature_selection
                 ~self_argument
                 arguments)
        in
        signatures
        |> List.concat_map ~f:check_arguments_against_signature
        |> SignatureSelection.find_closest_signature
        >>| SignatureSelection.instantiate_return_annotation ~skip_marking_escapees ~order
        |> Option.value ~default:(SignatureSelection.default_signature callable)
      in
      if List.is_empty overloads then
        get_match [implementation]
      else
        get_match overloads

    method resolve_mutable_literals ~assumptions ~resolve =
      WeakenMutableLiterals.weaken_mutable_literals
        resolve
        ~get_typed_dictionary:(self#get_typed_dictionary ~assumptions)
        ~comparator:(self#constraints_solution_exists ~assumptions)

    method constraints_solution_exists ~assumptions ~get_typed_dictionary_override ~left ~right =
      let ({ ConstraintsSet.get_typed_dictionary; _ } as order) = self#full_order ~assumptions in
      let order =
        {
          order with
          get_typed_dictionary =
            (fun annotation ->
              Option.first_some
                (get_typed_dictionary_override annotation)
                (get_typed_dictionary annotation));
        }
      in
      TypeOrder.OrderedConstraintsSet.add
        ConstraintsSet.empty
        ~new_constraint:(LessOrEqual { left; right })
        ~order
      |> TypeOrder.OrderedConstraintsSet.solve ~order
      |> Option.is_some

    method constructor ~assumptions class_name ~instantiated =
      let return_annotation =
        let generics =
          ClassHierarchyEnvironment.ReadOnly.variables
            (ClassMetadataEnvironment.ReadOnly.class_hierarchy_environment
               class_metadata_environment)
            ?dependency
            class_name
          >>| List.map ~f:Type.Variable.to_parameter
          |> Option.value ~default:[]
        in
        (* Tuples are special. *)
        if String.equal class_name "tuple" then
          match generics with
          | [Single tuple_variable] ->
              Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation tuple_variable)
          | _ -> Type.Tuple (Type.OrderedTypes.create_unbounded_concatenation Type.Any)
        else
          let backup = Type.parametric class_name generics in
          match instantiated, generics with
          | _, [] -> instantiated
          | Type.Primitive instantiated_name, _ when String.equal instantiated_name class_name ->
              backup
          | Type.Parametric { parameters; name = instantiated_name }, generics
            when String.equal instantiated_name class_name
                 && List.length parameters <> List.length generics ->
              backup
          | _ -> instantiated
      in
      let definitions =
        class_name
        ::
        ClassMetadataEnvironment.ReadOnly.successors
          class_metadata_environment
          ?dependency
          class_name
      in
      let definition_index parent =
        parent
        |> (fun class_annotation ->
             List.findi definitions ~f:(fun _ annotation ->
                 Type.equal (Primitive annotation) class_annotation))
        >>| fst
        |> Option.value ~default:Int.max_value
      in
      let signature_index_and_parent ~name =
        let signature, parent_name =
          match
            self#attribute
              ~assumptions
              ~transitive:true
              ~accessed_through_class:false
              ~include_generated_attributes:true
              ?special_method:None
              ?instantiated:(Some return_annotation)
              ~attribute_name:name
              class_name
          with
          | Some attribute ->
              ( AnnotatedAttribute.annotation attribute |> Annotation.annotation,
                AnnotatedAttribute.parent attribute )
          | None -> Type.Top, class_name
        in
        signature, definition_index (Type.Primitive parent_name), parent_name
      in
      let constructor_signature, constructor_index, _ =
        signature_index_and_parent ~name:"__init__"
      in
      let new_signature, new_index, new_parent_name =
        let new_signature, new_index, new_parent_name =
          signature_index_and_parent ~name:"__new__"
        in
        ( Type.parametric "BoundMethod" [Single new_signature; Single (Type.meta instantiated)],
          new_index,
          new_parent_name )
      in
      let signature, with_return =
        if new_index < constructor_index then
          (* If it is a Final class, we respect the return type of `__new__` and its overloads. *)
          if is_final_class class_metadata_environment ~dependency new_parent_name then
            new_signature, Fn.id
          else
            new_signature, Type.Callable.with_return_annotation ~annotation:return_annotation
        else
          constructor_signature, Type.Callable.with_return_annotation ~annotation:return_annotation
      in
      match signature with
      | Type.Callable callable -> Type.Callable (with_return callable)
      | Parametric
          { name = "BoundMethod"; parameters = [Single (Callable callable); Single self_type] } ->
          Parametric
            {
              name = "BoundMethod";
              parameters = [Single (Callable (with_return callable)); Single self_type];
            }
      | _ -> signature

    method global_annotation ~assumptions name =
      let process_unannotated_global global =
        let produce_assignment_global ~is_explicit ~is_final annotation =
          let original =
            if is_explicit then
              None
            else if
              (* Treat literal globals as having been explicitly annotated. *)
              Type.is_partially_typed annotation
            then
              Some Type.Top
            else
              None
          in
          Annotation.create_immutable ~final:is_final ~original annotation
        in
        match global with
        | UnannotatedGlobal.Define defines ->
            let { undecorated_signature; decorated } =
              List.map defines ~f:(fun { define; _ } -> define)
              |> List.partition_tf ~f:Define.Signature.is_overloaded_function
              |> fun (overloads, implementations) ->
              self#resolve_define
                ~implementation:(List.last implementations)
                ~overloads
                ~assumptions
            in
            let annotation =
              Result.ok decorated |> Option.value ~default:Type.Any |> Annotation.create_immutable
            in
            Some
              {
                Global.annotation;
                undecorated_signature = Some undecorated_signature;
                problem = Result.error decorated;
              }
        | SimpleAssign
            {
              explicit_annotation = None;
              value =
                {
                  Node.value =
                    Call
                      {
                        callee =
                          {
                            value =
                              Name
                                (Attribute
                                  {
                                    base = { Node.value = Name (Identifier "typing"); _ };
                                    attribute = "TypeAlias";
                                    _;
                                  });
                            _;
                          };
                        _;
                      };
                  _;
                };
              target_location = location;
            } ->
            let location = Location.strip_module location in
            Ast.Expression.Expression.Name (Expression.create_name_from_reference ~location name)
            |> Node.create ~location
            |> self#parse_annotation ~validation:ValidatePrimitives ~assumptions
            |> Type.meta
            |> Annotation.create_immutable
            |> fun annotation ->
            Some { Global.annotation; undecorated_signature = None; problem = None }
        | SimpleAssign { explicit_annotation; value; _ } ->
            let explicit_annotation =
              explicit_annotation
              >>| self#parse_annotation ~assumptions
              >>= fun annotation -> Option.some_if (not (Type.is_type_alias annotation)) annotation
            in
            let annotation, is_explicit, is_final =
              match explicit_annotation with
              | None -> self#resolve_literal value ~assumptions, false, false
              | Some explicit -> (
                  match Type.final_value explicit with
                  | `Ok final_value -> final_value, true, true
                  | `NoParameter -> self#resolve_literal value ~assumptions, false, true
                  | `NotFinal -> explicit, true, false)
            in
            produce_assignment_global ~is_explicit ~is_final annotation
            |> fun annotation ->
            Some { Global.annotation; undecorated_signature = None; problem = None }
        | TupleAssign { value; index; total_length; _ } ->
            let extracted =
              match self#resolve_literal ~assumptions value with
              | Type.Tuple (Concrete parameters) when List.length parameters = total_length ->
                  List.nth parameters index
                  (* This should always be Some, but I don't think its worth being fragile here *)
                  |> Option.value ~default:Type.Top
              | Type.Tuple (Concatenation concatenation) ->
                  Type.OrderedTypes.Concatenation.extract_sole_unbounded_annotation concatenation
                  |> Option.value ~default:Type.Top
              | _ -> Type.Top
            in
            produce_assignment_global ~is_explicit:false ~is_final:false extracted
            |> fun annotation ->
            Some { Global.annotation; undecorated_signature = None; problem = None }
        | _ -> None
      in
      let class_lookup =
        Reference.show name
        |> UnannotatedGlobalEnvironment.ReadOnly.class_exists
             (unannotated_global_environment class_metadata_environment)
             ?dependency
      in
      if class_lookup then
        let primitive = Type.Primitive (Reference.show name) in
        Annotation.create_immutable (Type.meta primitive)
        |> fun annotation ->
        Some { Global.annotation; undecorated_signature = None; problem = None }
      else
        UnannotatedGlobalEnvironment.ReadOnly.get_unannotated_global
          (unannotated_global_environment class_metadata_environment)
          ?dependency
          name
        >>= fun global ->
        let timer = Timer.start () in
        let result = process_unannotated_global global in
        Statistics.performance
          ~flush:false
          ~randomly_log_every:500
          ~section:`Check
          ~name:"SingleGlobalTypeCheck"
          ~timer
          ~normals:["name", Reference.show name; "request kind", "SingleGlobalTypeCheck"]
          ();
        result
  end

let empty_assumptions =
  {
    protocol_assumptions = ProtocolAssumptions.empty;
    callable_assumptions = CallableAssumptions.empty;
    decorator_assumptions = DecoratorAssumptions.empty;
  }


module ParseAnnotationCache = struct
  module Cache = ManagedCache.Make (struct
    module PreviousEnvironment = ClassMetadataEnvironment
    module Key = SharedMemoryKeys.ParseAnnotationKey

    module Value = struct
      type t = Type.t [@@deriving eq]

      let prefix = Prefix.make ()

      let description = "parse annotation"
    end

    module KeySet = SharedMemoryKeys.ParseAnnotationKey.Set
    module HashableKey = SharedMemoryKeys.ParseAnnotationKey

    let lazy_incremental = false

    let produce_value
        class_metadata_environment
        { SharedMemoryKeys.ParseAnnotationKey.validation; expression }
        ~dependency
      =
      let implementation = new base class_metadata_environment dependency in
      implementation#parse_annotation ~assumptions:empty_assumptions ~validation expression


    let filter_upstream_dependency = function
      | SharedMemoryKeys.ParseAnnotation key -> Some key
      | _ -> None


    let trigger_to_dependency key = SharedMemoryKeys.ParseAnnotation key

    (* It is difficult to set fine-grained ownership for fine-grained annotations, so for now the
       overlay will own all annotations. Our hope is that lazy evaluation plus the limited fanout on
       other environments will prevent this from becoming too big a problem *)
    let overlay_owns_key _ _ = true
  end)

  include Cache

  module ReadOnly = struct
    include Cache.ReadOnly

    class with_cached_parse_annotation dependency read_only =
      object
        inherit base (upstream_environment read_only) dependency

        method! parse_annotation
            ~assumptions:_
            ?(validation = SharedMemoryKeys.ParseAnnotationKey.ValidatePrimitivesAndTypeParameters)
            expression =
          get read_only ?dependency { SharedMemoryKeys.ParseAnnotationKey.validation; expression }
      end
  end
end

module MetaclassCache = struct
  module Cache = ManagedCache.Make (struct
    module PreviousEnvironment = ParseAnnotationCache

    module Key = struct
      type t = string [@@deriving compare, show, sexp, hash]

      let to_string = show

      let from_string = Fn.id
    end

    module Value = struct
      type t = Type.t option [@@deriving equal]

      let prefix = Prefix.make ()

      let description = "metaclasses"
    end

    module KeySet = String.Set
    module HashableKey = String

    let lazy_incremental = false

    let produce_value parse_annotation_cache key ~dependency =
      let implementation_with_cached_parse_annotation =
        new ParseAnnotationCache.ReadOnly.with_cached_parse_annotation
          dependency
          parse_annotation_cache
      in
      implementation_with_cached_parse_annotation#metaclass key ~assumptions:empty_assumptions


    let filter_upstream_dependency = function
      | SharedMemoryKeys.Metaclass key -> Some key
      | _ -> None


    let trigger_to_dependency key = SharedMemoryKeys.Metaclass key

    let overlay_owns_key module_tracker_overlay =
      ModuleTracker.Overlay.owns_identifier module_tracker_overlay
  end)

  include Cache

  module ReadOnly = struct
    include Cache.ReadOnly

    class with_parse_annotation_and_metaclass_caches dependency read_only =
      object
        inherit
          ParseAnnotationCache.ReadOnly.with_cached_parse_annotation
            dependency
            (upstream_environment read_only)

        method! metaclass ~assumptions:_ key = get read_only ?dependency key
      end
  end
end

module AttributeCache = struct
  module Cache = ManagedCache.Make (struct
    module PreviousEnvironment = MetaclassCache
    module Key = SharedMemoryKeys.AttributeTableKey

    module Value = struct
      type t = UninstantiatedAttributeTable.t option [@@deriving compare]

      let prefix = Prefix.make ()

      let description = "attributes"

      let equal = Memory.equal_from_compare compare
    end

    module KeySet = SharedMemoryKeys.AttributeTableKey.Set
    module HashableKey = SharedMemoryKeys.AttributeTableKey

    let lazy_incremental = true

    let produce_value
        metaclass_cache
        {
          SharedMemoryKeys.AttributeTableKey.include_generated_attributes;
          accessed_via_metaclass;
          name;
        }
        ~dependency
      =
      let implementation_with_cached_parse_annotation =
        new MetaclassCache.ReadOnly.with_parse_annotation_and_metaclass_caches
          dependency
          metaclass_cache
      in
      implementation_with_cached_parse_annotation#single_uninstantiated_attribute_table
        ~include_generated_attributes
        ~accessed_via_metaclass
        ~assumptions:empty_assumptions
        name


    let filter_upstream_dependency = function
      | SharedMemoryKeys.AttributeTable key -> Some key
      | _ -> None


    let trigger_to_dependency key = SharedMemoryKeys.AttributeTable key

    let overlay_owns_key module_tracker_overlay { SharedMemoryKeys.AttributeTableKey.name; _ } =
      ModuleTracker.Overlay.owns_identifier module_tracker_overlay name
  end)

  include Cache

  module ReadOnly = struct
    include Cache.ReadOnly

    let metaclass_cache = upstream_environment

    let cached_single_uninstantiated_attribute_table
        read_only
        dependency
        ~include_generated_attributes
        ~accessed_via_metaclass
        name
      =
      get
        read_only
        ?dependency
        {
          SharedMemoryKeys.AttributeTableKey.include_generated_attributes;
          accessed_via_metaclass;
          name;
        }


    class with_parse_annotation_metaclass_and_attribute_caches dependency read_only =
      object
        inherit
          MetaclassCache.ReadOnly.with_parse_annotation_and_metaclass_caches
            dependency
            (metaclass_cache read_only)

        method! single_uninstantiated_attribute_table ~assumptions:_ =
          cached_single_uninstantiated_attribute_table read_only dependency
      end
  end
end

module GlobalAnnotationCache = struct
  module Cache = Environment.EnvironmentTable.WithCache (struct
    let show_key = Reference.show

    let overlay_owns_key module_tracker_overlay =
      ModuleTracker.Overlay.owns_reference module_tracker_overlay


    module PreviousEnvironment = AttributeCache
    module Key = SharedMemoryKeys.ReferenceKey

    module Value = struct
      type t = Global.t option

      let prefix = Prefix.make ()

      let description = "Global"

      let equal = Memory.equal_from_compare (Option.compare Global.compare)
    end

    type trigger = Reference.t [@@deriving sexp, compare]

    let convert_trigger = Fn.id

    let key_to_trigger = Fn.id

    module TriggerSet = Reference.Set

    let lazy_incremental = false

    let produce_value attribute_cache key ~dependency =
      let implementation_with_preceding_caches =
        new AttributeCache.ReadOnly.with_parse_annotation_metaclass_and_attribute_caches
          dependency
          attribute_cache
      in
      implementation_with_preceding_caches#global_annotation ~assumptions:empty_assumptions key


    let filter_upstream_dependency = function
      | SharedMemoryKeys.AnnotateGlobal name -> Some name
      | _ -> None


    let trigger_to_dependency name = SharedMemoryKeys.AnnotateGlobal name

    let equal_value = Option.equal [%compare.equal: Global.t]
  end)

  include Cache

  module ReadOnly = struct
    include Cache.ReadOnly

    let attribute_cache = upstream_environment

    class with_all_caches dependency read_only =
      object
        inherit
          AttributeCache.ReadOnly.with_parse_annotation_metaclass_and_attribute_caches
            dependency
            (attribute_cache read_only)

        method! global_annotation ~assumptions:_ = get read_only ?dependency
      end
  end
end

module PreviousEnvironment = ClassMetadataEnvironment
include GlobalAnnotationCache

module ReadOnly = struct
  include GlobalAnnotationCache.ReadOnly

  let attribute_cache = upstream_environment

  let metaclass_cache read_only =
    attribute_cache read_only |> AttributeCache.ReadOnly.upstream_environment


  let parse_annotation_cache read_only =
    metaclass_cache read_only |> MetaclassCache.ReadOnly.upstream_environment


  let class_metadata_environment read_only =
    ParseAnnotationCache.ReadOnly.upstream_environment (parse_annotation_cache read_only)


  class with_uninstantiated_attributes_cache dependency read_only =
    object
      inherit base (class_metadata_environment read_only) dependency

      method! single_uninstantiated_attribute_table ~assumptions:_ =
        AttributeCache.ReadOnly.cached_single_uninstantiated_attribute_table
          (attribute_cache read_only)
          dependency
    end

  let add_all_caches_and_empty_assumptions f read_only ?dependency =
    new GlobalAnnotationCache.ReadOnly.with_all_caches dependency read_only
    |> f
    |> fun method_ -> method_ ~assumptions:empty_assumptions


  let instantiate_attribute =
    add_all_caches_and_empty_assumptions (fun o -> o#instantiate_attribute ?apply_descriptors:None)


  let attribute =
    add_all_caches_and_empty_assumptions (fun o -> o#attribute ?apply_descriptors:None)


  let all_attributes = add_all_caches_and_empty_assumptions (fun o -> o#all_attributes)

  let attribute_names = add_all_caches_and_empty_assumptions (fun o -> o#attribute_names)

  let check_invalid_type_parameters =
    add_all_caches_and_empty_assumptions (fun o ->
        o#check_invalid_type_parameters ~replace_unbound_parameters_with_any:true)


  let parse_annotation read_only ?dependency =
    let attributes_cached_but_not_annotations =
      new with_uninstantiated_attributes_cache dependency read_only
    in
    attributes_cached_but_not_annotations#parse_annotation ~assumptions:empty_assumptions


  let metaclass = add_all_caches_and_empty_assumptions (fun o -> o#metaclass)

  let constraints = add_all_caches_and_empty_assumptions (fun o -> o#constraints)

  let resolve_literal = add_all_caches_and_empty_assumptions (fun o -> o#resolve_literal)

  let resolve_define = add_all_caches_and_empty_assumptions (fun o -> o#resolve_define)

  let resolve_mutable_literals =
    add_all_caches_and_empty_assumptions (fun o -> o#resolve_mutable_literals)


  let constraints_solution_exists =
    add_all_caches_and_empty_assumptions (fun o -> o#constraints_solution_exists)


  let full_order ?dependency read_only =
    let implementation = new with_all_caches dependency read_only in
    implementation#full_order ~assumptions:empty_assumptions


  let get_typed_dictionary = add_all_caches_and_empty_assumptions (fun o -> o#get_typed_dictionary)

  let signature_select =
    add_all_caches_and_empty_assumptions (fun o -> o#signature_select ~skip_marking_escapees:false)


  let get_global = add_all_caches_and_empty_assumptions (fun o -> o#global_annotation)
end

module AttributeReadOnly = ReadOnly
include TypeParameterValidationTypes

module Testing = struct
  module ReadOnly = struct
    let upstream environment =
      GlobalAnnotationCache.Testing.ReadOnly.upstream environment
      |> AttributeCache.Testing.ReadOnly.upstream
      |> MetaclassCache.Testing.ReadOnly.upstream
      |> ParseAnnotationCache.Testing.ReadOnly.upstream
  end

  module UpdateResult = struct
    let upstream update_result =
      GlobalAnnotationCache.Testing.UpdateResult.upstream update_result
      |> AttributeCache.Testing.UpdateResult.upstream
      |> MetaclassCache.Testing.UpdateResult.upstream
      |> ParseAnnotationCache.Testing.UpdateResult.upstream
  end
end
