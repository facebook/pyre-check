(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
open Ast
open Statement
open Assumptions

module UninstantiatedAnnotation = struct
  type kind =
    | Method of {
        callable: Type.Callable.t;
        is_class_method: bool;
      }
    | Attribute of {
        annotation: Type.t;
        original_annotation: Type.t;
        is_property: bool;
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
      names := name :: !names )


  let lookup_name { attributes; _ } = Caml.Hashtbl.find_opt attributes

  let to_list { attributes; names } = List.rev_map !names ~f:(Caml.Hashtbl.find attributes)

  let names { names; _ } = !names

  let compare left right = List.compare compare_element (to_list left) (to_list right)
end

(* These modules get included at the bottom of this file, they're just here for aesthetic purposes *)
module TypeParameterValidationTypes = struct
  type generic_type_problems =
    | IncorrectNumberOfParameters of {
        actual: int;
        expected: int;
      }
    | ViolateConstraints of {
        actual: Type.t;
        expected: Type.Variable.Unary.t;
      }
    | UnexpectedGroup of {
        actual: Type.OrderedTypes.t;
        expected: Type.Variable.Unary.t;
      }
    | UnexpectedSingle of {
        actual: Type.t;
        expected: Type.Variable.Variadic.List.t;
      }
  [@@deriving compare, eq, sexp, show, hash]

  type type_parameters_mismatch = {
    name: string;
    kind: generic_type_problems;
  }
  [@@deriving compare, eq, sexp, show, hash]
end

module SignatureSelectionTypes = struct
  type mismatch = {
    actual: Type.t;
    expected: Type.t;
    name: Identifier.t option;
    position: int;
  }
  [@@deriving eq, show, compare]

  type invalid_argument = {
    expression: Expression.t;
    annotation: Type.t;
  }
  [@@deriving compare, eq, show, sexp, hash]

  type missing_argument =
    | Named of Identifier.t
    | Anonymous of int
  [@@deriving eq, show, compare, sexp, hash]

  type mismatch_with_list_variadic_type_variable =
    | NotDefiniteTuple of invalid_argument
    | CantConcatenate of Type.OrderedTypes.t list
    | ConstraintFailure of Type.OrderedTypes.t
  [@@deriving compare, eq, show, sexp, hash]

  type reason =
    | AbstractClassInstantiation of {
        class_name: Reference.t;
        abstract_methods: string list;
      }
    | CallingParameterVariadicTypeVariable
    | InvalidKeywordArgument of invalid_argument Node.t
    | InvalidVariableArgument of invalid_argument Node.t
    | Mismatch of mismatch Node.t
    | MismatchWithListVariadicTypeVariable of {
        variable: Type.OrderedTypes.t;
        mismatch: mismatch_with_list_variadic_type_variable;
      }
    | MissingArgument of missing_argument
    | MutuallyRecursiveTypeVariables
    | ProtocolInstantiation of Reference.t
    | TooManyArguments of {
        expected: int;
        provided: int;
      }
    | UnexpectedKeyword of Identifier.t
  [@@deriving eq, show, compare]

  type closest = {
    callable: Type.Callable.t;
    reason: reason option;
  }
  [@@deriving show]

  let equal_closest (left : closest) (right : closest) =
    (* Ignore rank. *)
    Type.Callable.equal left.callable right.callable
    && Option.equal equal_reason left.reason right.reason


  type sig_t =
    | Found of Type.Callable.t
    | NotFound of closest
  [@@deriving eq, show]

  module Argument = struct
    type kind =
      | SingleStar
      | DoubleStar
      | Named of string Node.t
      | Positional

    type t = {
      expression: Expression.t;
      full_expression: Expression.t;
      position: int;
      kind: kind;
      resolved: Type.t;
    }
  end

  type argument =
    | Argument of Argument.t
    | Default

  type ranks = {
    arity: int;
    annotation: int;
    position: int;
  }

  type reasons = {
    arity: reason list;
    annotation: reason list;
  }

  type signature_match = {
    callable: Type.Callable.t;
    argument_mapping: argument list Type.Callable.Parameter.Map.t;
    constraints_set: TypeConstraints.t list;
    ranks: ranks;
    reasons: reasons;
  }
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


let ast_environment class_metadata_environment =
  unannotated_global_environment class_metadata_environment
  |> UnannotatedGlobalEnvironment.ReadOnly.ast_environment


let class_definition class_metadata_environment annotation ~dependency =
  Type.split annotation
  |> fst
  |> Type.primitive_name
  >>= UnannotatedGlobalEnvironment.ReadOnly.get_class_definition
        (unannotated_global_environment class_metadata_environment)
        ?dependency


let aliases class_metadata_environment ~dependency =
  AliasEnvironment.ReadOnly.get_alias ?dependency (alias_environment class_metadata_environment)


let is_suppressed_module class_metadata_environment ~dependency reference =
  EmptyStubEnvironment.ReadOnly.from_empty_stub
    (empty_stub_environment class_metadata_environment)
    ?dependency
    reference


let undecorated_signature class_metadata_environment ~dependency =
  UndecoratedFunctionEnvironment.ReadOnly.get_undecorated_function
    ?dependency
    (ClassMetadataEnvironment.ReadOnly.undecorated_function_environment class_metadata_environment)


let class_name { Node.value = { ClassSummary.name; _ }; _ } = name

let class_annotation { Node.value = { ClassSummary.name; _ }; _ } =
  Type.Primitive (Reference.show name)


let rec weaken_mutable_literals resolve ~expression ~resolved ~expected ~comparator =
  let open Expression in
  match expression, resolved, expected with
  | _, _, Type.Union expected_types -> (
      let resolved_types =
        List.map
          ~f:(fun expected_type ->
            weaken_mutable_literals
              resolve
              ~expression
              ~resolved
              ~expected:expected_type
              ~comparator)
          expected_types
      in
      match
        List.exists2 ~f:(fun left right -> comparator ~left ~right) resolved_types expected_types
      with
      | Ok true -> expected
      | _ -> resolved )
  | _, _, Type.Optional expected_type ->
      let resolved_type =
        weaken_mutable_literals resolve ~expression ~resolved ~expected:expected_type ~comparator
      in
      if comparator ~left:resolved_type ~right:expected_type then
        expected
      else
        resolved_type
  | ( Some { Node.value = Expression.List items; _ },
      Type.Parametric { name = "list"; parameters = [Single actual_item_type] },
      Type.Parametric { name = "list"; parameters = [Single expected_item_type] } ) ->
      let weakened_item_type =
        Type.union
          (List.map
             ~f:(fun item ->
               weaken_mutable_literals
                 resolve
                 ~expression:(Some item)
                 ~resolved:actual_item_type
                 ~expected:expected_item_type
                 ~comparator)
             items)
      in
      if comparator ~left:weakened_item_type ~right:expected_item_type then
        expected
      else
        Type.list weakened_item_type
  | ( Some { Node.value = Expression.ListComprehension _; _ },
      Type.Parametric { name = "list"; parameters = [Single actual] },
      Type.Parametric { name = "list"; parameters = [Single expected_parameter] } )
    when comparator ~left:actual ~right:expected_parameter ->
      expected
  | ( Some { Node.value = Expression.Set items; _ },
      Type.Parametric { name = "set"; parameters = [Single actual_item_type] },
      Type.Parametric { name = "set"; parameters = [Single expected_item_type] } ) ->
      let weakened_item_type =
        Type.union
          (List.map
             ~f:(fun item ->
               weaken_mutable_literals
                 resolve
                 ~expression:(Some item)
                 ~resolved:actual_item_type
                 ~expected:expected_item_type
                 ~comparator)
             items)
      in
      if comparator ~left:weakened_item_type ~right:expected_item_type then
        expected
      else
        Type.set weakened_item_type
  | ( Some { Node.value = Expression.SetComprehension _; _ },
      Type.Parametric { name = "set"; parameters = [Single actual] },
      Type.Parametric { name = "set"; parameters = [Single expected_parameter] } )
    when comparator ~left:actual ~right:expected_parameter ->
      expected
  | ( Some { Node.value = Expression.Dictionary { entries; keywords = [] }; _ },
      _,
      Type.TypedDictionary { total; fields; _ } ) ->
      let find_matching_field ~name =
        let matching_name { Type.name = expected_name; _ } = String.equal name expected_name in
        List.find ~f:matching_name
      in
      let resolve_entry { Dictionary.Entry.key; value } =
        let key = resolve key in
        match key with
        | Type.Literal (Type.String name) ->
            let annotation =
              let resolved = resolve value in
              let relax { Type.annotation; _ } =
                if Type.is_dictionary resolved || Type.is_typed_dictionary resolved then
                  weaken_mutable_literals
                    resolve
                    ~expression:(Some value)
                    ~resolved
                    ~expected:annotation
                    ~comparator
                else if comparator ~left:resolved ~right:annotation then
                  annotation
                else
                  resolved
              in
              find_matching_field fields ~name >>| relax |> Option.value ~default:resolved
            in
            Some { Type.name; annotation }
        | _ -> None
      in
      let add_missing_fields sofar =
        let is_missing { Type.name; _ } = Option.is_none (find_matching_field sofar ~name) in
        sofar @ List.filter fields ~f:is_missing
      in
      List.map entries ~f:resolve_entry
      |> Option.all
      >>| (if total then Fn.id else add_missing_fields)
      >>| Type.TypedDictionary.anonymous ~total
      |> Option.value_map ~default:resolved ~f:(fun typed_dictionary ->
             if comparator ~left:typed_dictionary ~right:expected then
               expected
             else
               typed_dictionary)
  | Some { Node.value = Expression.Dictionary { entries; _ }; _ }, _, _ ->
      weaken_dictionary_entries resolve ~resolved ~expected ~comparator ~entries
  | ( Some { Node.value = Expression.DictionaryComprehension _; _ },
      Type.Parametric { name = "dict"; parameters = [Single actual_key; Single actual_value] },
      Type.Parametric { name = "dict"; parameters = [Single expected_key; Single expected_value] } )
    when comparator ~left:actual_key ~right:expected_key
         && comparator ~left:actual_value ~right:expected_value ->
      expected
  | _ -> resolved


and weaken_dictionary_entries resolve ~resolved ~expected ~comparator ~entries =
  match entries with
  | _ -> (
      match resolved, expected with
      | ( Type.Parametric
            { name = "dict"; parameters = [Single actual_key_type; Single actual_value_type] },
          Type.Parametric
            { name = "dict"; parameters = [Single expected_key_type; Single expected_value_type] } )
        ->
          let weakened_key_type =
            Type.union
              (List.map
                 ~f:(fun { key; _ } ->
                   weaken_mutable_literals
                     resolve
                     ~expression:(Some key)
                     ~resolved:actual_key_type
                     ~expected:expected_key_type
                     ~comparator)
                 entries)
          in
          let weakened_value_type =
            Type.union
              (List.map
                 ~f:(fun { value; _ } ->
                   weaken_mutable_literals
                     resolve
                     ~expression:(Some value)
                     ~resolved:actual_value_type
                     ~expected:expected_value_type
                     ~comparator)
                 entries)
          in

          if
            comparator ~left:weakened_key_type ~right:expected_key_type
            && comparator ~left:weakened_value_type ~right:expected_value_type
          then
            expected
          else
            Type.dictionary ~key:weakened_key_type ~value:weakened_value_type
      | _ -> resolved )


module Implementation = struct
  module ClassDecorators = struct
    type options = {
      init: bool;
      repr: bool;
      eq: bool;
      order: bool;
    }

    let extract_options
        ~class_metadata_environment
        ~names
        ~default
        ~init
        ~repr
        ~eq
        ~order
        ?dependency
        { Node.value = { ClassSummary.decorators; _ }; _ }
      =
      let open Expression in
      let get_decorators ~names =
        let get_decorator decorator =
          List.filter_map
            ~f:
              (AstEnvironment.ReadOnly.matches_decorator
                 (ast_environment class_metadata_environment)
                 ?dependency
                 ~target:decorator)
            decorators
        in
        names |> List.map ~f:get_decorator |> List.concat
      in
      let extract_options_from_arguments =
        let apply_arguments default argument =
          let recognize_value ~default = function
            | Expression.False -> false
            | True -> true
            | _ -> default
          in
          match argument with
          | {
           Call.Argument.name = Some { Node.value = argument_name; _ };
           value = { Node.value; _ };
          } ->
              let argument_name = Identifier.sanitized argument_name in
              (* We need to check each keyword sequentially because different keywords may
                 correspond to the same string. *)
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
              default
          | _ -> default
        in
        List.fold ~init:default ~f:apply_arguments
      in
      match get_decorators ~names with
      | [] -> None
      | { arguments = Some arguments; _ } :: _ -> Some (extract_options_from_arguments arguments)
      | _ -> Some default


    let dataclass_options =
      extract_options
        ~names:["dataclasses.dataclass"; "dataclass"]
        ~default:{ init = true; repr = true; eq = true; order = false }
        ~init:"init"
        ~repr:"repr"
        ~eq:"eq"
        ~order:"order"


    let attrs_attributes =
      extract_options
        ~names:["attr.s"; "attr.attrs"]
        ~default:{ init = true; repr = true; eq = true; order = true }
        ~init:"init"
        ~repr:"repr"
        ~eq:"cmp"
        ~order:"cmp"


    let apply
        ~definition
        ~class_metadata_environment
        ~class_attributes
        ~create_attribute
        ~instantiate_attribute
        ?dependency
        table
      =
      let open Expression in
      let parent_dataclasses =
        ClassMetadataEnvironment.ReadOnly.superclasses
          class_metadata_environment
          ?dependency
          definition
      in
      let { Node.value = { ClassSummary.name; _ }; _ } = definition in
      let generate_attributes ~options =
        let already_in_table name =
          UninstantiatedAttributeTable.lookup_name table name |> Option.is_some
        in
        let make_callable ~parameters ~annotation ~attribute_name =
          let parameters =
            if class_attributes then
              { Type.Callable.Parameter.name = "self"; annotation = Type.Top; default = false }
              :: parameters
            else
              parameters
          in
          ( attribute_name,
            Type.Callable.create
              ~implicit:
                { implicit_annotation = Primitive (Reference.show name); name = "$parameter$self" }
              ~name:(Reference.combine name (Reference.create attribute_name))
              ~parameters:(Defined (Type.Callable.Parameter.create parameters))
              ~annotation
              () )
        in
        match options definition with
        | None -> []
        | Some { init; repr; eq; order } ->
            let generated_methods =
              let methods =
                if init && not (already_in_table "__init__") then
                  let parameters =
                    let extract_dataclass_field_arguments (_, value) =
                      match value with
                      | {
                       Node.value =
                         Expression.Call
                           {
                             callee =
                               {
                                 Node.value =
                                   Expression.Name
                                     (Name.Attribute
                                       {
                                         base =
                                           {
                                             Node.value =
                                               Expression.Name (Name.Identifier "dataclasses");
                                             _;
                                           };
                                         attribute = "field";
                                         _;
                                       });
                                 _;
                               };
                             arguments;
                             _;
                           };
                       _;
                      } ->
                          Some arguments
                      | _ -> None
                    in
                    let init_not_disabled attribute =
                      let is_disable_init { Call.Argument.name; value = { Node.value; _ } } =
                        match name, value with
                        | Some { Node.value = parameter_name; _ }, Expression.False
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
                        match name with
                        | Some { Node.value = parameter_name; _ } ->
                            if String.equal "default" (Identifier.sanitized parameter_name) then
                              Some value
                            else if
                              String.equal "default_factory" (Identifier.sanitized parameter_name)
                            then
                              let { Node.location; _ } = value in
                              Some
                                {
                                  Node.value =
                                    Expression.Call { Call.callee = value; arguments = [] };
                                  location;
                                }
                            else
                              None
                        | _ -> None
                      in
                      match initialized with
                      | false -> None
                      | true -> (
                          match extract_dataclass_field_arguments (attribute, value) with
                          | Some arguments -> List.find_map arguments ~f:get_default_value
                          | _ -> Some value )
                    in
                    let collect_parameters parameters (attribute, value) =
                      (* Parameters must be annotated attributes *)
                      let annotation =
                        instantiate_attribute attribute
                        |> AnnotatedAttribute.annotation
                        |> Annotation.original
                        |> function
                        | Type.Parametric
                            { name = "dataclasses.InitVar"; parameters = [Single single_parameter] }
                          ->
                            single_parameter
                        | annotation -> annotation
                      in
                      match AnnotatedAttribute.name attribute with
                      | name when not (Type.is_unknown annotation) ->
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
                            | { Type.Callable.Parameter.name = old_name; default = old_default; _ }
                              :: tail
                              when Identifier.equal old_name name ->
                                { name; annotation; default = Option.is_some value || old_default }
                                :: tail
                            | head :: tail -> head :: override_existing_parameters tail
                          in
                          override_existing_parameters parameters
                      | _ -> parameters
                    in
                    let get_table
                        ({ Node.value = { ClassSummary.attribute_components; _ }; _ } as parent)
                      =
                      let create attribute : uninstantiated_attribute * Expression.t =
                        let value =
                          match attribute with
                          | {
                           Node.value = { Attribute.kind = Simple { value = Some value; _ }; _ };
                           _;
                          } ->
                              value
                          | { Node.location; _ } -> Node.create Expression.Ellipsis ~location
                        in
                        ( create_attribute
                            ~class_metadata_environment
                            ?dependency
                            ~parent
                            ?defined:None
                            ?inherited:None
                            ?default_class_attribute:None
                            ~accessed_via_metaclass:false
                            attribute,
                          value )
                      in
                      let compare_by_location left right =
                        Ast.Location.compare (Node.location left) (Node.location right)
                      in
                      Class.attributes
                        ~include_generated_attributes:false
                        ~in_test:false
                        attribute_components
                      |> Identifier.SerializableMap.bindings
                      |> List.unzip
                      |> snd
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
                  [make_callable ~parameters ~annotation:Type.none ~attribute_name:"__init__"]
                else
                  []
              in
              let methods =
                if repr && not (already_in_table "__repr__") then
                  let new_method =
                    make_callable ~parameters:[] ~annotation:Type.string ~attribute_name:"__repr__"
                  in
                  new_method :: methods
                else
                  methods
              in
              let add_order_method methods name =
                let annotation = Type.object_primitive in
                if not (already_in_table name) then
                  make_callable
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
              methods
            in
            let make_attribute (attribute_name, annotation) =
              AnnotatedAttribute.create_uninstantiated
                ~uninstantiated_annotation:
                  {
                    UninstantiatedAnnotation.accessed_via_metaclass = false;
                    kind =
                      Attribute
                        { annotation; original_annotation = annotation; is_property = false };
                  }
                ~abstract:false
                ~async:false
                ~class_attribute:false
                ~defined:true
                ~initialized:true
                ~name:attribute_name
                ~parent:(Reference.show name)
                ~visibility:ReadWrite
                ~static:false
                ~property:false
                ~has_ellipsis_value:true
            in
            List.map generated_methods ~f:make_attribute
      in
      let dataclass_attributes () =
        (* TODO (T43210531): Warn about inconsistent annotations *)
        generate_attributes ~options:(dataclass_options ~class_metadata_environment ?dependency)
      in
      let attrs_attributes () =
        (* TODO (T41039225): Add support for other methods *)
        generate_attributes ~options:(attrs_attributes ~class_metadata_environment ?dependency)
      in
      dataclass_attributes () @ attrs_attributes ()
      |> List.iter ~f:(UninstantiatedAttributeTable.add table)
  end

  type dependency = SharedMemoryKeys.dependency

  type open_recurser = {
    full_order:
      assumptions:Assumptions.t ->
      ?dependency:dependency ->
      ClassMetadataEnvironment.ReadOnly.t ->
      TypeOrder.order;
    uninstantiated_attribute_table:
      assumptions:Assumptions.t ->
      transitive:sexp_bool ->
      class_attributes:sexp_bool ->
      include_generated_attributes:sexp_bool ->
      ?special_method:sexp_bool ->
      ?dependency:dependency ->
      ClassSummary.t Node.t ->
      class_metadata_environment:ClassMetadataEnvironment.MetadataReadOnly.t ->
      UninstantiatedAttributeTable.t;
    check_invalid_type_parameters:
      assumptions:Assumptions.t ->
      class_metadata_environment:ClassMetadataEnvironment.ReadOnly.t ->
      ?dependency:SharedMemoryKeys.dependency ->
      Type.t ->
      TypeParameterValidationTypes.type_parameters_mismatch list * Type.t;
    parse_annotation:
      assumptions:Assumptions.t ->
      class_metadata_environment:ClassMetadataEnvironment.ReadOnly.t ->
      ?allow_untracked:bool ->
      ?allow_invalid_type_parameters:bool ->
      ?allow_primitives_from_empty_stubs:bool ->
      ?dependency:SharedMemoryKeys.dependency ->
      Expression.expression Node.t ->
      Type.t;
    create_attribute:
      assumptions:Assumptions.t ->
      class_metadata_environment:ClassMetadataEnvironment.ReadOnly.t ->
      ?dependency:SharedMemoryKeys.dependency ->
      parent:ClassSummary.t Node.t ->
      ?defined:bool ->
      ?inherited:bool ->
      ?default_class_attribute:bool ->
      accessed_via_metaclass:bool ->
      Attribute.attribute Node.t ->
      UninstantiatedAnnotation.t AnnotatedAttribute.t;
    metaclass:
      assumptions:Assumptions.t ->
      class_metadata_environment:ClassMetadataEnvironment.ReadOnly.t ->
      ?dependency:SharedMemoryKeys.dependency ->
      ClassSummary.t Node.t ->
      Type.t;
    constraints:
      assumptions:Assumptions.t ->
      class_metadata_environment:ClassMetadataEnvironment.ReadOnly.t ->
      target:Type.Primitive.t ->
      ?parameters:Type.Parameter.t list ->
      ?dependency:SharedMemoryKeys.dependency ->
      instantiated:Type.t ->
      unit ->
      TypeConstraints.Solution.t;
    resolve_literal:
      assumptions:Assumptions.t ->
      class_metadata_environment:ClassMetadataEnvironment.ReadOnly.t ->
      ?dependency:SharedMemoryKeys.dependency ->
      Expression.expression Node.t ->
      Type.t;
    create_overload:
      assumptions:Assumptions.t ->
      class_metadata_environment:ClassMetadataEnvironment.ReadOnly.t ->
      ?dependency:SharedMemoryKeys.dependency ->
      Define.Signature.t Node.t ->
      Type.t Type.Callable.overload;
    signature_select:
      assumptions:Assumptions.t ->
      class_metadata_environment:ClassMetadataEnvironment.ReadOnly.t ->
      ?dependency:SharedMemoryKeys.dependency ->
      resolve:(Expression.expression Node.t -> Type.t) ->
      arguments:Expression.Call.Argument.t list ->
      callable:Type.t Type.Callable.record ->
      SignatureSelectionTypes.sig_t;
    resolve_mutable_literals:
      assumptions:Assumptions.t ->
      class_metadata_environment:ClassMetadataEnvironment.ReadOnly.t ->
      ?dependency:SharedMemoryKeys.dependency ->
      resolve:(Expression.expression Node.t -> Type.t) ->
      expression:Expression.expression Node.t option ->
      resolved:Type.t ->
      expected:Type.t ->
      Type.t;
    constraints_solution_exists:
      assumptions:Assumptions.t ->
      class_metadata_environment:ClassMetadataEnvironment.ReadOnly.t ->
      ?dependency:SharedMemoryKeys.dependency ->
      left:Type.t ->
      right:Type.t ->
      bool;
    constructor:
      assumptions:Assumptions.t ->
      class_metadata_environment:ClassMetadataEnvironment.ReadOnly.t ->
      ?dependency:SharedMemoryKeys.dependency ->
      ClassSummary.t Node.t ->
      instantiated:Type.t ->
      Type.t;
    instantiate_attribute:
      assumptions:Assumptions.t ->
      class_metadata_environment:ClassMetadataEnvironment.ReadOnly.t ->
      ?dependency:SharedMemoryKeys.dependency ->
      ?instantiated:Type.t ->
      UninstantiatedAnnotation.t AnnotatedAttribute.t ->
      AnnotatedAttribute.instantiated;
  }

  let full_order
      { constructor; uninstantiated_attribute_table; instantiate_attribute; _ }
      ~assumptions
      ?dependency
      class_metadata_environment
    =
    let constructor instantiated ~protocol_assumptions =
      let constructor assumptions class_name =
        let instantiated = Type.Primitive class_name in
        class_definition class_metadata_environment ~dependency instantiated
        >>| fun definition ->
        constructor ~assumptions ~class_metadata_environment ?dependency definition ~instantiated
      in

      instantiated |> Type.primitive_name >>= constructor { assumptions with protocol_assumptions }
    in
    let attributes class_type ~assumptions =
      match Type.resolve_class class_type with
      | None -> None
      | Some [] -> None
      | Some [{ instantiated; class_attributes; class_name }] ->
          UnannotatedGlobalEnvironment.ReadOnly.get_class_definition
            (unannotated_global_environment class_metadata_environment)
            ?dependency
            class_name
          >>| (fun definition ->
                uninstantiated_attribute_table
                  ~assumptions
                  ~transitive:true
                  ~class_attributes
                  ~include_generated_attributes:true
                  ?special_method:None
                  ?dependency
                  definition
                  ~class_metadata_environment)
          >>| UninstantiatedAttributeTable.to_list
          >>| List.map
                ~f:(instantiate_attribute ~assumptions ~class_metadata_environment ~instantiated)
      | Some (_ :: _) ->
          (* These come from calling attributes on Unions, which are handled by solve_less_or_equal
             indirectly by breaking apart the union before doing the
             instantiate_protocol_parameters. Therefore, there is no reason to deal with joining the
             attributes together here *)
          None
    in
    let is_protocol annotation ~protocol_assumptions:_ =
      UnannotatedGlobalEnvironment.ReadOnly.is_protocol
        (unannotated_global_environment class_metadata_environment)
        ?dependency
        annotation
    in
    {
      TypeOrder.handler =
        ClassHierarchyEnvironment.ReadOnly.class_hierarchy
          ?dependency
          (class_hierarchy_environment class_metadata_environment);
      constructor;
      attributes;
      is_protocol;
      assumptions;
    }


  let check_invalid_type_parameters
      { full_order; _ }
      ~assumptions
      ~class_metadata_environment
      ?dependency
      annotation
    =
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
                [ClassHierarchy.Variable.Unary (Type.Variable.Unary.create "T")]
            | _ ->
                ClassHierarchyEnvironment.ReadOnly.variables
                  (class_hierarchy_environment class_metadata_environment)
                  ?dependency
                  name
                |> Option.value ~default:[]
          in
          let invalid_type_parameters ~name ~given =
            let generics = generics_for_name name in
            match ClassHierarchy.Variable.zip_on_parameters ~parameters:given generics with
            | Some [] -> Type.Primitive name, sofar
            | Some paired ->
                let check_parameter (given, generic) =
                  match generic, given with
                  | ClassHierarchy.Variable.Unary generic, Type.Parameter.Single given ->
                      let invalid =
                        let order =
                          full_order ?dependency class_metadata_environment ~assumptions
                        in
                        let pair = Type.Variable.UnaryPair (generic, given) in
                        TypeOrder.OrderedConstraints.add_lower_bound
                          TypeConstraints.empty
                          ~order
                          ~pair
                        >>| TypeOrder.OrderedConstraints.add_upper_bound ~order ~pair
                        |> Option.is_none
                      in
                      if invalid then
                        ( Type.Parameter.Single Type.Any,
                          Some
                            {
                              name;
                              kind = ViolateConstraints { actual = given; expected = generic };
                            } )
                      else
                        Type.Parameter.Single given, None
                  | Unary expected, Type.Parameter.Group actual ->
                      Single Any, Some { name; kind = UnexpectedGroup { expected; actual } }
                  | ListVariadic expected, Single actual ->
                      Group Any, Some { name; kind = UnexpectedSingle { expected; actual } }
                  | ListVariadic _, Group _ ->
                      (* TODO(T47346673): accept w/ new kind of validation *)
                      given, None
                in
                List.map paired ~f:check_parameter
                |> List.unzip
                |> fun (parameters, errors) ->
                Type.parametric name parameters, List.filter_map errors ~f:Fn.id @ sofar
            | None ->
                let mismatch =
                  {
                    name;
                    kind =
                      IncorrectNumberOfParameters
                        { actual = List.length given; expected = List.length generics };
                  }
                in
                ( Type.parametric
                    name
                    (List.map generics ~f:(function
                        | ClassHierarchy.Variable.Unary _ -> Type.Parameter.Single Type.Any
                        | ListVariadic _ -> Group Any)),
                  mismatch :: sofar )
          in
          match annotation with
          | Type.Primitive ("typing.Final" | "typing_extensions.Final") -> annotation, sofar
          | Type.Primitive name -> invalid_type_parameters ~name ~given:[]
          (* natural variadics *)
          | Type.Parametric { name = "typing.Protocol"; _ }
          | Type.Parametric { name = "typing.Generic"; _ } ->
              annotation, sofar
          | Type.Parametric { name; parameters } -> invalid_type_parameters ~name ~given:parameters
          | _ -> annotation, sofar
        in
        { Type.Transform.transformed_annotation; new_state }
    end)
    in
    InvalidTypeParametersTransform.visit [] annotation


  let parse_annotation
      { check_invalid_type_parameters; _ }
      ~assumptions
      ~class_metadata_environment
      ?(allow_untracked = false)
      ?(allow_invalid_type_parameters = false)
      ?(allow_primitives_from_empty_stubs = false)
      ?dependency
      expression
    =
    let modify_aliases = function
      | Type.TypeAlias alias ->
          check_invalid_type_parameters ~class_metadata_environment ?dependency alias ~assumptions
          |> snd
          |> fun alias -> Type.TypeAlias alias
      | result -> result
    in
    let annotation =
      AliasEnvironment.ReadOnly.parse_annotation_without_validating_type_parameters
        (alias_environment class_metadata_environment)
        ~modify_aliases
        ?dependency
        ~allow_untracked
        ~allow_primitives_from_empty_stubs
        expression
    in
    let result =
      if not allow_invalid_type_parameters then
        check_invalid_type_parameters
          ~class_metadata_environment
          ?dependency
          annotation
          ~assumptions
        |> snd
      else
        annotation
    in
    result


  let uninstantiated_attribute_table
      { create_attribute; metaclass; instantiate_attribute; _ }
      ~assumptions
      ~transitive
      ~class_attributes
      ~include_generated_attributes
      ?(special_method = false)
      ?dependency
      ({ Node.value = { ClassSummary.name; _ }; _ } as definition)
      ~class_metadata_environment
    =
    let definition_attributes
        ~in_test
        ~class_attributes
        ~table
        ~accessed_via_metaclass
        ({ Node.value = { ClassSummary.name = parent_name; attribute_components; _ }; _ } as parent)
      =
      let add_actual () =
        let collect_attributes attribute =
          create_attribute
            attribute
            ?dependency
            ~class_metadata_environment
            ~assumptions
            ~parent
            ~inherited:(not (Reference.equal name parent_name))
            ~default_class_attribute:class_attributes
            ~accessed_via_metaclass
          |> UninstantiatedAttributeTable.add table
        in
        Class.attributes ~include_generated_attributes ~in_test attribute_components
        |> fun attribute_map ->
        Identifier.SerializableMap.iter (fun _ data -> collect_attributes data) attribute_map
      in
      let add_placeholder_stub_inheritances () =
        let add_if_missing ~attribute_name ~annotation =
          if Option.is_none (UninstantiatedAttributeTable.lookup_name table attribute_name) then
            UninstantiatedAttributeTable.add
              table
              (AnnotatedAttribute.create_uninstantiated
                 ~uninstantiated_annotation:
                   {
                     UninstantiatedAnnotation.accessed_via_metaclass;
                     kind =
                       Attribute
                         { annotation; original_annotation = annotation; is_property = false };
                   }
                 ~abstract:false
                 ~async:false
                 ~class_attribute:false
                 ~defined:true
                 ~initialized:true
                 ~name:attribute_name
                 ~parent:(Reference.show name)
                 ~visibility:ReadWrite
                 ~static:true
                 ~property:false
                 ~has_ellipsis_value:true)
          else
            ()
        in
        add_if_missing
          ~attribute_name:"__init__"
          ~annotation:(Type.Callable.create ~annotation:Type.none ());
        add_if_missing
          ~attribute_name:"__getattr__"
          ~annotation:(Type.Callable.create ~annotation:Type.Any ())
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
      if include_generated_attributes then
        ClassDecorators.apply
          ~definition:parent
          ~class_metadata_environment
          ~class_attributes
          ~create_attribute:(create_attribute ~assumptions)
          ~instantiate_attribute:
            (instantiate_attribute
               ?dependency
               ~assumptions
               ~class_metadata_environment
               ?instantiated:None)
          ?dependency
          table
    in
    let superclasses =
      ClassMetadataEnvironment.ReadOnly.superclasses class_metadata_environment ?dependency
    in
    let superclass_definitions = superclasses definition in
    let in_test =
      List.exists (definition :: superclass_definitions) ~f:(fun { Node.value; _ } ->
          ClassSummary.is_unit_test value)
    in
    let table = UninstantiatedAttributeTable.create () in
    (* Pass over normal class hierarchy. *)
    let definitions =
      if class_attributes && special_method then
        []
      else if transitive then
        definition :: superclass_definitions
      else
        [definition]
    in
    List.iter
      definitions
      ~f:(definition_attributes ~in_test ~class_attributes ~table ~accessed_via_metaclass:false);

    (* Class over meta hierarchy if necessary. *)
    let meta_definitions =
      if class_attributes then
        metaclass ?dependency ~class_metadata_environment ~assumptions definition
        |> class_definition class_metadata_environment ~dependency
        >>| (fun definition -> definition :: superclasses definition)
        |> Option.value ~default:[]
      else
        []
    in
    List.iter
      meta_definitions
      ~f:
        (definition_attributes ~in_test ~class_attributes:false ~table ~accessed_via_metaclass:true);
    table


  let instantiate_attribute
      { constraints; full_order; _ }
      ~assumptions
      ~class_metadata_environment
      ?dependency
      ?instantiated
      attribute
    =
    let default_class_attribute = AnnotatedAttribute.class_attribute attribute in
    let class_annotation = AnnotatedAttribute.parent attribute in
    let attribute_name = AnnotatedAttribute.name attribute in
    let { UninstantiatedAnnotation.accessed_via_metaclass; kind = annotation } =
      AnnotatedAttribute.uninstantiated_annotation attribute
    in
    let annotation, original =
      let instantiated =
        match instantiated with
        | Some instantiated -> instantiated
        | None -> Type.Primitive class_annotation
      in
      let instantiated = if accessed_via_metaclass then Type.meta instantiated else instantiated in
      match annotation with
      | Method
          { callable = { Type.Callable.implementation; overloads; _ } as callable; is_class_method }
        ->
          let callable =
            let partial_apply_self ~self_type =
              let open Type.Callable in
              let { Type.Callable.kind; implementation; overloads; implicit } =
                match implementation, overloads with
                | { Type.Callable.parameters = Defined (Named { name; annotation; _ } :: _); _ }, _
                  -> (
                    let callable =
                      let implicit = { implicit_annotation = self_type; name } in
                      { callable with implicit = Some implicit }
                    in
                    let order = full_order ?dependency class_metadata_environment ~assumptions in
                    let solution =
                      try
                        TypeOrder.solve_less_or_equal
                          order
                          ~left:self_type
                          ~right:annotation
                          ~constraints:TypeConstraints.empty
                        |> List.filter_map ~f:(TypeOrder.OrderedConstraints.solve ~order)
                        |> List.hd
                        |> Option.value ~default:TypeConstraints.Solution.empty
                      with
                      | ClassHierarchy.Untracked _ -> TypeConstraints.Solution.empty
                    in
                    let instantiated =
                      TypeConstraints.Solution.instantiate solution (Type.Callable callable)
                    in
                    match instantiated with
                    | Type.Callable callable -> callable
                    | _ -> callable )
                (* We also need to set the implicit up correctly for overload-only methods. *)
                | _, { parameters = Defined (Named { name; _ } :: _); _ } :: _ ->
                    let implicit = { implicit_annotation = self_type; name } in
                    { callable with implicit = Some implicit }
                | _ -> callable
              in
              let drop_self { Type.Callable.annotation; parameters; define_location } =
                let parameters =
                  match parameters with
                  | Type.Callable.Defined (_ :: parameters) -> Type.Callable.Defined parameters
                  | _ -> parameters
                in
                { Type.Callable.annotation; parameters; define_location }
              in
              {
                Type.Callable.kind;
                implementation = drop_self implementation;
                overloads = List.map overloads ~f:drop_self;
                implicit;
              }
            in
            if String.equal attribute_name "__new__" then
              (* Special case __new__ because it is the only static method with one of its
                 parameters implicitly annotated. *)
              let add_class_annotation { Type.Callable.annotation; parameters; define_location } =
                let parameters =
                  match parameters with
                  | Defined
                      (Named { Type.Callable.Parameter.name; annotation = Type.Top; default }
                      :: parameters) ->
                      Type.Callable.Defined
                        (Named { name; annotation = Type.meta instantiated; default } :: parameters)
                  | _ -> parameters
                in
                { Type.Callable.annotation; parameters; define_location }
              in
              {
                callable with
                implementation = add_class_annotation implementation;
                overloads = List.map overloads ~f:add_class_annotation;
              }
            else if is_class_method then
              partial_apply_self ~self_type:(Type.meta instantiated)
            else if AnnotatedAttribute.static attribute then
              callable
            else if default_class_attribute then
              (* Keep first argument around when calling instance methods from class attributes. *)
              callable
            else
              partial_apply_self ~self_type:instantiated
          in
          (* Special cases *)
          let callable =
            match instantiated, attribute_name, callable with
            | Type.TypedDictionary { fields; total; _ }, method_name, callable ->
                Type.TypedDictionary.special_overloads ~fields ~method_name ~total
                >>| (fun overloads ->
                      {
                        callable with
                        implementation =
                          { annotation = Type.Top; parameters = Undefined; define_location = None };
                        overloads;
                      })
                |> Option.value ~default:callable
            | Type.Tuple (Bounded (Concrete members)), "__getitem__", ({ overloads; _ } as callable)
              ->
                let overload index member =
                  {
                    Type.Callable.annotation = member;
                    parameters =
                      Defined
                        [
                          Named
                            { name = "x"; annotation = Type.literal_integer index; default = false };
                        ];
                    define_location = None;
                  }
                in
                let overloads = List.mapi ~f:overload members @ overloads in
                { callable with overloads }
            | ( Parametric { name = "type"; parameters = [Single (Type.Primitive name)] },
                "__getitem__",
                ({ kind = Named callable_name; _ } as callable) )
              when String.equal (Reference.show callable_name) "typing.GenericMeta.__getitem__" ->
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
                    Type.Callable.Parameter.Anonymous { index = 0; annotation; default = false }
                  in
                  let synthetic =
                    Type.Variable
                      (Type.Variable.Unary.create "$synthetic_attribute_resolution_variable")
                  in
                  match name with
                  (* This can't be expressed without IntVars, StrVars, and corresponding
                     ListVariadic variants of them *)
                  | "typing_extensions.Literal"
                  (* TODO:(T60535947) We can't do the Map[Ts, type] -> X[Ts] trick here because we
                     don't yet support Union[Ts] *)
                  | "typing.Union" ->
                      ( {
                          Type.Callable.annotation = Type.meta Type.Any;
                          parameters = Undefined;
                          define_location = None;
                        },
                        [] )
                  | "typing.Optional" ->
                      ( {
                          Type.Callable.annotation = Type.meta (Type.Optional synthetic);
                          parameters = Defined [create_parameter (Type.meta synthetic)];
                          define_location = None;
                        },
                        [] )
                  | "typing.Callable" ->
                      ( {
                          Type.Callable.annotation =
                            Type.meta (Type.Callable.create ~annotation:synthetic ());
                          parameters =
                            Defined
                              [
                                create_parameter
                                  (Type.Tuple (Bounded (Concrete [Type.Any; Type.meta synthetic])));
                              ];
                          define_location = None;
                        },
                        [] )
                  | _ -> (
                      let overload parameter =
                        let generics = List.map generics ~f:ClassHierarchy.Variable.to_parameter in
                        {
                          Type.Callable.annotation =
                            Type.meta (Type.Parametric { name; parameters = generics });
                          parameters = Defined [parameter];
                          define_location = None;
                        }
                      in
                      match generics with
                      | [ListVariadic variable] ->
                          let meta_generics =
                            Type.OrderedTypes.Concatenation.Middle.create
                              ~variable
                              ~mappers:["type"]
                            |> Type.OrderedTypes.Concatenation.create
                          in
                          let single_type_case =
                            (* In the case of VariadicClass[int], it's being called with a
                               Type[int], not a Tuple[Type[int]].*)
                            overload (Variable (Concatenation meta_generics))
                          in
                          let multiple_type_case =
                            overload
                              (create_parameter
                                 (Type.Tuple (Bounded (Concatenation meta_generics))))
                          in
                          single_type_case, [multiple_type_case; single_type_case]
                      | [Unary generic] ->
                          overload (create_parameter (Type.meta (Variable generic))), []
                      | _ ->
                          let handle_variadics = function
                            | ClassHierarchy.Variable.Unary single ->
                                ( Type.Parameter.Single (Type.Variable single),
                                  Type.meta (Variable single) )
                            | ListVariadic _ ->
                                (* TODO:(T60536033) We'd really like to take FiniteList[Ts], but
                                   without that we can't actually return the correct metatype, which
                                   is a bummer *)
                                Type.Parameter.Group Any, Type.Any
                          in
                          let return_parameters, parameter_parameters =
                            List.map generics ~f:handle_variadics |> List.unzip
                          in
                          ( {
                              Type.Callable.annotation =
                                Type.meta (Type.Parametric { name; parameters = return_parameters });
                              parameters =
                                Defined [create_parameter (Type.tuple parameter_parameters)];
                              define_location = None;
                            },
                            [] ) )
                in
                { callable with implementation; overloads }
            | _ -> callable
          in
          Type.Callable callable, Type.Callable callable
      | Attribute { annotation; original_annotation; is_property = true } ->
          (* Special case properties with type variables. *)
          (* TODO(T44676629): handle this correctly *)
          let free_variables =
            let variables =
              annotation
              |> Type.Variable.all_free_variables
              |> List.filter_map ~f:(function
                     | Type.Variable.Unary variable -> Some (Type.Variable variable)
                     | _ -> None)
              |> Type.Set.of_list
            in
            let generics =
              ClassHierarchyEnvironment.ReadOnly.variables
                (ClassMetadataEnvironment.ReadOnly.class_hierarchy_environment
                   class_metadata_environment)
                ?dependency
                class_annotation
              >>= ClassHierarchy.Variable.all_unary
              >>| List.map ~f:Type.Variable.Unary.self_reference
              >>| Type.Set.of_list
              (* TODO(T44676629): This case should be handled when we re-do this handling *)
              |> Option.value ~default:Type.Set.empty
            in

            Set.diff variables generics |> Set.to_list
          in
          if not (List.is_empty free_variables) then
            let constraints =
              List.fold free_variables ~init:Type.Map.empty ~f:(fun map variable ->
                  Map.set map ~key:variable ~data:instantiated)
              |> Map.find
            in
            let annotation = Type.instantiate ~constraints annotation in
            annotation, annotation
          else
            annotation, original_annotation
      | Attribute { annotation; original_annotation; is_property = false } ->
          annotation, original_annotation
    in
    let annotation, original =
      match instantiated with
      | Some instantiated ->
          let solution =
            constraints
              ?dependency
              ~target:class_annotation
              ~instantiated
              ~class_metadata_environment
              ~assumptions
              ()
          in
          let instantiate annotation = TypeConstraints.Solution.instantiate solution annotation in
          instantiate annotation, instantiate original
      | None -> annotation, original
    in
    AnnotatedAttribute.instantiate attribute ~annotation ~original_annotation:original


  let create_attribute
      { parse_annotation; resolve_literal; create_overload; _ }
      ~assumptions
      ~class_metadata_environment
      ?dependency
      ~parent
      ?(defined = true)
      ?(inherited = false)
      ?(default_class_attribute = false)
      ~accessed_via_metaclass
      { Node.location; value = { Attribute.name = attribute_name; kind } }
    =
    let class_annotation = class_annotation parent in
    let annotation, value, class_attribute, visibility =
      match kind with
      | Simple { annotation; value; frozen; toplevel; implicit; primitive } ->
          let parsed_annotation =
            annotation >>| parse_annotation ?dependency ~assumptions ~class_metadata_environment
          in
          (* Account for class attributes. *)
          let annotation, final, class_attribute =
            parsed_annotation
            >>| (fun annotation ->
                  let is_final, annotation =
                    match Type.final_value annotation with
                    | Some annotation -> true, annotation
                    | None -> false, annotation
                  in
                  let is_class_variable, annotation =
                    match Type.class_variable_value annotation with
                    | Some annotation -> true, annotation
                    | None -> false, annotation
                  in
                  Some annotation, is_final, is_class_variable)
            |> Option.value ~default:(None, false, default_class_attribute)
          in
          (* Handle enumeration attributes. *)
          let annotation, value, class_attribute =
            let superclasses =
              ClassMetadataEnvironment.ReadOnly.superclasses
                class_metadata_environment
                ?dependency
                parent
              |> List.map ~f:(fun definition -> class_name definition |> Reference.show)
              |> String.Set.of_list
            in
            if
              (not (Set.mem Recognized.enumeration_classes (Type.show class_annotation)))
              && (not (Set.is_empty (Set.inter Recognized.enumeration_classes superclasses)))
              && (not inherited)
              && primitive
              && defined
              && not implicit
            then
              Some class_annotation, None, true (* Enums override values. *)
            else
              annotation, value, class_attribute
          in
          let annotation, original =
            match annotation, value with
            | Some annotation, Some _ -> annotation, annotation
            | Some annotation, None -> annotation, annotation
            | None, Some value ->
                let literal_value_annotation =
                  resolve_literal ?dependency ~class_metadata_environment ~assumptions value
                in
                let is_dataclass_attribute =
                  let get_decorator =
                    AstEnvironment.ReadOnly.get_decorator
                      (ast_environment class_metadata_environment)
                      ?dependency
                  in
                  let get_dataclass_decorator annotated =
                    get_decorator annotated ~decorator:"dataclasses.dataclass"
                    @ get_decorator annotated ~decorator:"dataclass"
                  in
                  not (List.is_empty (get_dataclass_decorator parent))
                in
                if
                  (not (Type.is_partially_typed literal_value_annotation))
                  && (not is_dataclass_attribute)
                  && toplevel
                then (* Treat literal attributes as having been explicitly annotated. *)
                  literal_value_annotation, literal_value_annotation
                else
                  ( parse_annotation ?dependency ~class_metadata_environment ~assumptions value,
                    Type.Top )
            | _ -> Type.Top, Type.Top
          in
          let visibility =
            if final then
              AnnotatedAttribute.ReadOnly (Refinable { overridable = false })
            else if frozen then
              ReadOnly (Refinable { overridable = true })
            else
              ReadWrite
          in
          ( UninstantiatedAnnotation.Attribute
              { annotation; original_annotation = original; is_property = false },
            value,
            class_attribute,
            visibility )
      | Method { signatures; final; _ } ->
          (* Handle Callables *)
          let visibility =
            if final then
              AnnotatedAttribute.ReadOnly (Refinable { overridable = false })
            else
              ReadWrite
          in
          let callable =
            match signatures with
            | ({ Define.Signature.name = { Node.value = name; _ }; _ } as define) :: _ as defines ->
                let open Type.Callable in
                let overloads =
                  let create_overload define =
                    ( Define.Signature.is_overloaded_function define,
                      create_overload
                        ~class_metadata_environment
                        ~assumptions
                        ?dependency
                        (Node.create define ~location) )
                  in
                  List.map defines ~f:create_overload
                in
                let implementation, overloads =
                  let to_signature (implementation, overloads) (is_overload, signature) =
                    if is_overload then
                      implementation, signature :: overloads
                    else
                      signature, overloads
                  in
                  List.fold
                    ~init:
                      ( {
                          annotation = Type.Top;
                          parameters = Type.Callable.Undefined;
                          define_location = None;
                        },
                        [] )
                    ~f:to_signature
                    overloads
                in
                let callable = { kind = Named name; implementation; overloads; implicit = None } in
                let is_class_method = Define.Signature.is_class_method define in
                UninstantiatedAnnotation.Method { callable; is_class_method }
            | [] -> failwith "impossible"
          in
          callable, None, default_class_attribute, visibility
      | Property { kind; class_property; _ } ->
          let annotation, original, visibility =
            match kind with
            | ReadWrite { setter_annotation; getter_annotation } ->
                let current =
                  getter_annotation
                  >>| parse_annotation ?dependency ~class_metadata_environment ~assumptions
                  |> Option.value ~default:Type.Top
                in
                let original =
                  setter_annotation
                  >>| parse_annotation ?dependency ~class_metadata_environment ~assumptions
                  |> Option.value ~default:Type.Top
                in
                current, original, AnnotatedAttribute.ReadWrite
            | ReadOnly { getter_annotation } ->
                let annotation =
                  getter_annotation
                  >>| parse_annotation ?dependency ~class_metadata_environment ~assumptions
                  |> Option.value ~default:Type.Top
                in
                annotation, annotation, AnnotatedAttribute.ReadOnly Unrefinable
          in
          ( UninstantiatedAnnotation.Attribute
              { annotation; original_annotation = original; is_property = true },
            None,
            class_property,
            visibility )
    in
    AnnotatedAttribute.create_uninstantiated
      ~uninstantiated_annotation:
        { UninstantiatedAnnotation.accessed_via_metaclass; kind = annotation }
      ~visibility
      ~abstract:
        ( match kind with
        | Method { signatures; _ } -> List.exists signatures ~f:Define.Signature.is_abstract_method
        | _ -> false )
      ~async:
        ( match kind with
        | Property { async; _ } -> async
        | _ -> false )
      ~class_attribute
      ~defined
      ~initialized:
        ( match kind with
        | Simple { value = Some { Node.value = Ellipsis; _ }; _ }
        | Simple { value = None; _ } ->
            false
        | Simple { value = Some _; _ }
        | Method _
        | Property _ ->
            true )
      ~name:attribute_name
      ~parent:(Reference.show (class_name parent))
      ~static:
        ( match kind with
        | Method { static; _ } -> static
        | _ -> false )
      ~property:
        ( match kind with
        | Property _ -> true
        | _ -> false )
      ~has_ellipsis_value:
        ( match value >>| Node.value with
        | None
        | Some Expression.Expression.Ellipsis ->
            true
        | Some _ -> false )


  let metaclass
      { parse_annotation; metaclass; full_order; _ }
      ~assumptions
      ~class_metadata_environment
      ?dependency
      ({ Node.value = { ClassSummary.bases; _ }; _ } as original)
    =
    (* See https://docs.python.org/3/reference/datamodel.html#determining-the-appropriate-metaclass
       for why we need to consider all metaclasses. *)
    let open Expression in
    let parse_annotation = parse_annotation ?dependency ~class_metadata_environment ~assumptions in
    let metaclass_candidates =
      let explicit_metaclass =
        let find_explicit_metaclass = function
          | { Call.Argument.name = Some { Node.value = "metaclass"; _ }; value } ->
              Some (parse_annotation value)
          | _ -> None
        in
        List.find_map ~f:find_explicit_metaclass bases
      in
      let metaclass_of_bases =
        let explicit_bases =
          let base_to_class { Call.Argument.value; _ } =
            delocalize value |> parse_annotation |> Type.split |> fst
          in
          List.filter
            ~f:(function
              | { Call.Argument.name = None; _ } -> true
              | _ -> false)
            bases
          |> List.map ~f:base_to_class
          |> List.filter_map ~f:(class_definition class_metadata_environment ~dependency)
          |> List.filter ~f:(fun base_class ->
                 not (Node.equal ClassSummary.equal base_class original))
        in
        let filter_generic_meta base_metaclasses =
          (* We only want a class directly inheriting from Generic to have a metaclass of
             GenericMeta. *)
          if
            List.exists
              ~f:(fun base -> Reference.equal (Reference.create "typing.Generic") (class_name base))
              explicit_bases
          then
            base_metaclasses
          else
            List.filter
              ~f:(fun metaclass -> not (Type.equal (Type.Primitive "typing.GenericMeta") metaclass))
              base_metaclasses
        in
        explicit_bases
        |> List.map ~f:(metaclass ?dependency ~class_metadata_environment ~assumptions)
        |> filter_generic_meta
      in
      match explicit_metaclass with
      | Some metaclass -> metaclass :: metaclass_of_bases
      | None -> metaclass_of_bases
    in
    match metaclass_candidates with
    | [] -> Type.Primitive "type"
    | first :: candidates -> (
        let order = full_order ?dependency class_metadata_environment ~assumptions in
        let candidate = List.fold candidates ~init:first ~f:(TypeOrder.meet order) in
        match candidate with
        | Type.Bottom ->
            (* If we get Bottom here, we don't have a "most derived metaclass", so default to one. *)
            first
        | _ -> candidate )


  let constraints
      { full_order; _ }
      ~assumptions
      ~class_metadata_environment
      ~target
      ?parameters
      ?dependency
      ~instantiated
      ()
    =
    let parameters =
      match parameters with
      | None ->
          ClassHierarchyEnvironment.ReadOnly.variables
            (ClassMetadataEnvironment.ReadOnly.class_hierarchy_environment
               class_metadata_environment)
            ?dependency
            target
          >>| List.map ~f:ClassHierarchy.Variable.to_parameter
          |> Option.value ~default:[]
      | Some parameters -> parameters
    in
    if List.is_empty parameters then
      TypeConstraints.Solution.empty
    else
      let right = Type.parametric target parameters in
      match instantiated, right with
      | Type.Primitive name, Parametric { name = right_name; _ } when String.equal name right_name
        ->
          (* TODO(T42259381) This special case is only necessary because constructor calls
             attributes with an "instantiated" type of a bare parametric, which will fill with Anys *)
          TypeConstraints.Solution.empty
      | _ ->
          let order = full_order ?dependency class_metadata_environment ~assumptions in
          TypeOrder.solve_less_or_equal
            order
            ~constraints:TypeConstraints.empty
            ~left:instantiated
            ~right
          |> List.filter_map ~f:(TypeOrder.OrderedConstraints.solve ~order)
          |> List.hd
          (* TODO(T39598018): error in this case somehow, something must be wrong *)
          |> Option.value ~default:TypeConstraints.Solution.empty


  (* In general, python expressions can be self-referential. This resolution only checks literals
     and annotations found in the resolution map, without resolving expressions. *)
  let resolve_literal
      { full_order; resolve_literal; parse_annotation; _ }
      ~assumptions
      ~class_metadata_environment
      ?dependency
      expression
    =
    let open Ast.Expression in
    let is_concrete_class class_type =
      class_type
      |> class_definition class_metadata_environment ~dependency
      >>| (fun { Node.value = { name; _ }; _ } -> Reference.show name)
      >>= ClassHierarchyEnvironment.ReadOnly.variables
            (class_hierarchy_environment class_metadata_environment)
            ?dependency
            ~default:(Some [])
      >>| List.is_empty
    in
    let fully_specified_type = function
      | { Node.value = Expression.Name name; _ } as annotation when is_simple_name name ->
          let class_type =
            parse_annotation ?dependency ~assumptions ~class_metadata_environment annotation
          in
          if Type.is_none class_type || is_concrete_class class_type |> Option.value ~default:false
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
          let class_type =
            parse_annotation ?dependency ~assumptions ~class_metadata_environment annotation
          in
          if is_concrete_class class_type >>| not |> Option.value ~default:false then
            Some class_type
          else
            None
      | _ -> None
    in

    let order = full_order ?dependency class_metadata_environment ~assumptions in
    match Node.value expression with
    | Expression.Await expression ->
        resolve_literal ?dependency ~class_metadata_environment ~assumptions expression
        |> Type.awaitable_value
        |> Option.value ~default:Type.Top
    | BooleanOperator { BooleanOperator.left; right; _ } ->
        let annotation =
          TypeOrder.join
            order
            (resolve_literal ?dependency ~class_metadata_environment ~assumptions left)
            (resolve_literal ?dependency ~class_metadata_environment ~assumptions right)
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
            Option.value (fully_specified_type callee) ~default:Top )
    | Name _ when has_identifier_base expression -> (
        match fully_specified_type expression with
        | Some annotation ->
            if Type.is_none annotation then
              Type.none
            else
              Type.meta annotation
        | None -> Type.Top )
    | Complex _ -> Type.complex
    | Dictionary { Dictionary.entries; keywords = [] } ->
        let key_annotation, value_annotation =
          let join_entry (key_annotation, value_annotation) { Dictionary.Entry.key; value } =
            ( TypeOrder.join
                order
                key_annotation
                (resolve_literal ?dependency ~class_metadata_environment ~assumptions key),
              TypeOrder.join
                order
                value_annotation
                (resolve_literal ?dependency ~class_metadata_environment ~assumptions value) )
          in
          List.fold ~init:(Type.Bottom, Type.Bottom) ~f:join_entry entries
        in
        if Type.is_concrete key_annotation && Type.is_concrete value_annotation then
          Type.dictionary ~key:key_annotation ~value:value_annotation
        else
          Type.Any
    | False -> Type.bool
    | Float _ -> Type.float
    | Integer _ -> Type.integer
    | List elements ->
        let parameter =
          let join sofar element =
            TypeOrder.join
              order
              sofar
              (resolve_literal ?dependency ~class_metadata_environment ~assumptions element)
          in
          List.fold ~init:Type.Bottom ~f:join elements
        in
        if Type.is_concrete parameter then Type.list parameter else Type.Any
    | Set elements ->
        let parameter =
          let join sofar element =
            TypeOrder.join
              order
              sofar
              (resolve_literal ?dependency ~class_metadata_environment ~assumptions element)
          in
          List.fold ~init:Type.Bottom ~f:join elements
        in
        if Type.is_concrete parameter then Type.set parameter else Type.Any
    | String { StringLiteral.kind; _ } -> (
        match kind with
        | StringLiteral.Bytes -> Type.bytes
        | _ -> Type.string )
    | Ternary { Ternary.target; alternative; _ } ->
        let annotation =
          TypeOrder.join
            order
            (resolve_literal ?dependency ~class_metadata_environment ~assumptions target)
            (resolve_literal ?dependency ~class_metadata_environment ~assumptions alternative)
        in
        if Type.is_concrete annotation then annotation else Type.Any
    | True -> Type.bool
    | Tuple elements ->
        Type.tuple
          (List.map
             elements
             ~f:(resolve_literal ?dependency ~class_metadata_environment ~assumptions))
    | Expression.Yield _ -> Type.yield Type.Any
    | _ -> Type.Any


  let create_overload
      { full_order; parse_annotation; resolve_literal; signature_select; _ }
      ~assumptions
      ~class_metadata_environment
      ?dependency
      { Node.value = { Define.Signature.decorators; _ } as signature; location }
    =
    let apply_decorator
        ({ Type.Callable.annotation; parameters; _ } as overload)
        { Node.value = decorator; _ }
      =
      let resolve_decorators name ~arguments =
        let handle = function
          | "click.command"
          | "click.group"
          | "click.pass_context"
          | "click.pass_obj" ->
              (* Suppress caller/callee parameter matching by altering the click entry point to have
                 a generic parameter list. *)
              let parameters =
                Type.Callable.Defined
                  [
                    Type.Callable.Parameter.Variable (Concrete Type.Any);
                    Type.Callable.Parameter.Keywords Type.Any;
                  ]
              in
              { overload with Type.Callable.parameters }
          | name when Set.mem Recognized.asyncio_contextmanager_decorators name ->
              let joined =
                let order = full_order ?dependency class_metadata_environment ~assumptions in
                try TypeOrder.join order annotation (Type.async_iterator Type.Bottom) with
                | ClassHierarchy.Untracked _ ->
                    (* create_overload gets called when building the environment, which is unsound
                       and can raise. *)
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
          | name when Set.mem Decorators.special_decorators name ->
              Decorators.apply ~overload ~resolution:() ~name
          | name -> (
              let resolved_decorator =
                match
                  ( undecorated_signature
                      class_metadata_environment
                      (Reference.create name)
                      ~dependency,
                    arguments )
                with
                | Some signature, Some arguments -> (
                    let resolve expression =
                      let resolved =
                        resolve_literal
                          ?dependency
                          ~class_metadata_environment
                          ~assumptions
                          expression
                      in
                      if Type.is_partially_typed resolved then
                        Type.Top
                      else
                        resolved
                    in
                    let callable =
                      {
                        Type.Callable.kind = Anonymous;
                        implementation = signature;
                        overloads = [];
                        implicit = None;
                      }
                    in
                    match
                      signature_select
                        ?dependency
                        ~class_metadata_environment
                        ~assumptions
                        ~resolve
                        ~arguments
                        ~callable
                    with
                    | SignatureSelectionTypes.Found
                        {
                          implementation = { annotation = Type.Callable { implementation; _ }; _ };
                          _;
                        } ->
                        Some implementation
                    | _ -> None )
                | Some signature, None -> Some signature
                | None, _ -> None
              in
              match resolved_decorator with
              | Some
                  {
                    Type.Callable.annotation = return_annotation;
                    parameters =
                      Type.Callable.Defined
                        [Type.Callable.Parameter.Anonymous { annotation = parameter_annotation; _ }];
                    _;
                  }
              | Some
                  {
                    Type.Callable.annotation = return_annotation;
                    parameters =
                      Type.Callable.Defined
                        [Type.Callable.Parameter.Named { annotation = parameter_annotation; _ }];
                    _;
                  } -> (
                  let order = full_order ?dependency class_metadata_environment ~assumptions in
                  let decorated_annotation =
                    TypeOrder.solve_less_or_equal
                      order
                      ~constraints:TypeConstraints.empty
                      ~left:(Type.Callable.create ~parameters ~annotation ())
                      ~right:parameter_annotation
                    |> List.filter_map ~f:(TypeOrder.OrderedConstraints.solve ~order)
                    |> List.hd
                    >>| fun solution ->
                    TypeConstraints.Solution.instantiate solution return_annotation
                    (* If we failed, just default to the old annotation. *)
                  in
                  let decorated_annotation =
                    decorated_annotation |> Option.value ~default:annotation
                  in
                  match decorated_annotation with
                  (* Note that @property decorators can't properly be handled in this fashion. The
                     problem stems from the need to use `apply_decorators` to individual overloaded
                     defines - if an overloaded define could become Not An Overload, it's not clear
                     what we should do. Defer the problem by now by only inferring a limited set of
                     decorators. *)
                  | Type.Callable
                      {
                        Type.Callable.implementation =
                          {
                            Type.Callable.parameters = decorated_parameters;
                            annotation = decorated_annotation;
                            define_location;
                          };
                        _;
                      } ->
                      {
                        Type.Callable.annotation = decorated_annotation;
                        parameters = decorated_parameters;
                        define_location;
                      }
                  | _ -> overload )
              | _ -> overload )
        in
        Expression.name_to_identifiers name
        >>| String.concat ~sep:"."
        >>| handle
        |> Option.value ~default:overload
      in
      let open Expression in
      match decorator with
      | Expression.Call { callee = { Node.value = Expression.Name name; _ }; arguments } ->
          resolve_decorators name ~arguments:(Some arguments)
      | Expression.Name name -> resolve_decorators name ~arguments:None
      | _ -> overload
    in
    let parser =
      {
        AnnotatedCallable.parse_annotation =
          parse_annotation ?dependency ~class_metadata_environment ~assumptions;
        parse_as_concatenation =
          AliasEnvironment.ReadOnly.parse_as_concatenation
            (alias_environment class_metadata_environment)
            ?dependency;
        parse_as_parameter_specification_instance_annotation =
          AliasEnvironment.ReadOnly.parse_as_parameter_specification_instance_annotation
            (alias_environment class_metadata_environment)
            ?dependency;
      }
    in
    let init =
      Node.create signature ~location
      |> AnnotatedCallable.create_overload_without_applying_decorators ~parser
    in
    decorators |> List.rev |> List.fold ~init ~f:apply_decorator


  let signature_select
      { full_order; resolve_mutable_literals; parse_annotation; _ }
      ~assumptions
      ~class_metadata_environment
      ?dependency
      ~resolve
      ~arguments
      ~callable:({ Type.Callable.implementation; overloads; _ } as callable)
    =
    let open SignatureSelectionTypes in
    let order = full_order ~assumptions ?dependency class_metadata_environment in
    let open Expression in
    let open Type.Callable in
    let match_arity ({ parameters = all_parameters; _ } as implementation) =
      let all_arguments = arguments in
      let base_signature_match =
        {
          callable = { callable with Type.Callable.implementation; overloads = [] };
          argument_mapping = Parameter.Map.empty;
          constraints_set = [TypeConstraints.empty];
          ranks = { arity = 0; annotation = 0; position = 0 };
          reasons = { arity = []; annotation = [] };
        }
      in
      let rec consume
          ({ argument_mapping; reasons = { arity; _ } as reasons; _ } as signature_match)
          ~arguments
          ~parameters
        =
        let update_mapping parameter argument =
          Map.add_multi argument_mapping ~key:parameter ~data:argument
        in
        let arity_mismatch ?(unreachable_parameters = []) ~arguments reasons =
          match all_parameters with
          | Defined all_parameters ->
              let matched_keyword_arguments =
                let is_keyword_argument = function
                  | { Call.Argument.name = Some _; _ } -> true
                  | _ -> false
                in
                List.filter ~f:is_keyword_argument all_arguments
              in
              let positional_parameter_count =
                List.length all_parameters
                - List.length unreachable_parameters
                - List.length matched_keyword_arguments
              in
              let error =
                TooManyArguments
                  {
                    expected = positional_parameter_count;
                    provided = positional_parameter_count + List.length arguments;
                  }
              in
              { reasons with arity = error :: arity }
          | _ -> reasons
        in
        match arguments, parameters with
        | [], [] ->
            (* Both empty *)
            signature_match
        | { Argument.kind = Argument.SingleStar; _ } :: arguments_tail, []
        | { kind = DoubleStar; _ } :: arguments_tail, [] ->
            (* Starred or double starred arguments; parameters empty *)
            consume ~arguments:arguments_tail ~parameters signature_match
        | { kind = Named name; _ } :: _, [] ->
            (* Named argument; parameters empty *)
            let reasons = { reasons with arity = UnexpectedKeyword name.value :: arity } in
            { signature_match with reasons }
        | _, [] ->
            (* Positional argument; parameters empty *)
            { signature_match with reasons = arity_mismatch ~arguments reasons }
        | [], (Parameter.KeywordOnly { default = true; _ } as parameter) :: parameters_tail
        | [], (Parameter.Anonymous { default = true; _ } as parameter) :: parameters_tail
        | [], (Parameter.Named { default = true; _ } as parameter) :: parameters_tail ->
            (* Arguments empty, default parameter *)
            let argument_mapping = update_mapping parameter Default in
            consume ~arguments ~parameters:parameters_tail { signature_match with argument_mapping }
        | [], parameter :: parameters_tail ->
            (* Arguments empty, parameter *)
            let argument_mapping =
              match Map.find argument_mapping parameter with
              | Some _ -> argument_mapping
              | None -> Map.set ~key:parameter ~data:[] argument_mapping
            in
            consume ~arguments ~parameters:parameters_tail { signature_match with argument_mapping }
        | ( ({ kind = Named _; _ } as argument) :: arguments_tail,
            (Parameter.Keywords _ as parameter) :: _ ) ->
            (* Labeled argument, keywords parameter *)
            let argument_mapping = update_mapping parameter (Argument argument) in
            consume ~arguments:arguments_tail ~parameters { signature_match with argument_mapping }
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
            let argument_mapping, reasons =
              match matching_parameter with
              | Some matching_parameter ->
                  update_mapping matching_parameter (Argument argument), reasons
              | None ->
                  argument_mapping, { reasons with arity = UnexpectedKeyword name.value :: arity }
            in
            consume
              ~arguments:arguments_tail
              ~parameters:remaining_parameters
              { signature_match with argument_mapping; reasons }
        | ( ({ kind = DoubleStar; _ } as argument) :: arguments_tail,
            (Parameter.Keywords _ as parameter) :: _ )
        | ( ({ kind = SingleStar; _ } as argument) :: arguments_tail,
            (Parameter.Variable _ as parameter) :: _ ) ->
            (* (Double) starred argument, (double) starred parameter *)
            let argument_mapping = update_mapping parameter (Argument argument) in
            consume ~arguments:arguments_tail ~parameters { signature_match with argument_mapping }
        | { kind = SingleStar; _ } :: _, Parameter.Keywords _ :: parameters_tail ->
            (* Starred argument, double starred parameter *)
            consume ~arguments ~parameters:parameters_tail { signature_match with argument_mapping }
        | { kind = Positional; _ } :: _, Parameter.Keywords _ :: parameters_tail ->
            (* Unlabeled argument, double starred parameter *)
            consume ~arguments ~parameters:parameters_tail { signature_match with argument_mapping }
        | { kind = DoubleStar; _ } :: _, Parameter.Variable _ :: parameters_tail ->
            (* Double starred argument, starred parameter *)
            consume ~arguments ~parameters:parameters_tail { signature_match with argument_mapping }
        | ( ({ kind = Positional; _ } as argument) :: arguments_tail,
            (Parameter.Variable _ as parameter) :: _ ) ->
            (* Unlabeled argument, starred parameter *)
            let signature_match =
              let argument_mapping = update_mapping parameter (Argument argument) in
              { signature_match with argument_mapping }
            in
            consume ~arguments:arguments_tail ~parameters signature_match
        | { kind = SingleStar; _ } :: arguments_tail, Type.Callable.Parameter.KeywordOnly _ :: _ ->
            (* Starred argument, keyword only parameter *)
            consume ~arguments:arguments_tail ~parameters signature_match
        | ({ kind = DoubleStar; _ } as argument) :: _, parameter :: parameters_tail
        | ({ kind = SingleStar; _ } as argument) :: _, parameter :: parameters_tail ->
            (* Double starred or starred argument, parameter *)
            let argument_mapping = update_mapping parameter (Argument argument) in
            consume ~arguments ~parameters:parameters_tail { signature_match with argument_mapping }
        | { kind = Positional; _ } :: _, (Parameter.KeywordOnly _ as parameter) :: parameters_tail
          ->
            (* Unlabeled argument, keyword only parameter *)
            let reasons =
              arity_mismatch
                reasons
                ~unreachable_parameters:(parameter :: parameters_tail)
                ~arguments
            in
            { signature_match with reasons }
        | ({ kind = Positional; _ } as argument) :: arguments_tail, parameter :: parameters_tail ->
            (* Unlabeled argument, parameter *)
            let argument_mapping = update_mapping parameter (Argument argument) in
            consume
              ~arguments:arguments_tail
              ~parameters:parameters_tail
              { signature_match with argument_mapping }
      in
      let ordered_arguments () =
        let create_argument index { Call.Argument.name; value } =
          let expression, kind =
            match value, name with
            | { Node.value = Starred (Starred.Once expression); _ }, _ ->
                expression, Argument.SingleStar
            | { Node.value = Starred (Starred.Twice expression); _ }, _ -> expression, DoubleStar
            | expression, Some name -> expression, Named name
            | expression, None -> expression, Positional
          in
          let resolved = resolve expression in
          { Argument.position = index + 1; expression; full_expression = value; kind; resolved }
        in
        let is_labeled = function
          | { Argument.kind = Named _; _ } -> true
          | _ -> false
        in
        let labeled_arguments, unlabeled_arguments =
          arguments |> List.mapi ~f:create_argument |> List.partition_tf ~f:is_labeled
        in
        labeled_arguments @ unlabeled_arguments
      in
      match all_parameters with
      | Defined parameters ->
          consume base_signature_match ~arguments:(ordered_arguments ()) ~parameters
      | Undefined -> base_signature_match
      | ParameterVariadicTypeVariable variable -> (
          let combines_into_variable ~positional_component ~keyword_component =
            Type.Variable.Variadic.Parameters.Components.combine
              { positional_component; keyword_component }
            >>| Type.Variable.Variadic.Parameters.equal variable
            |> Option.value ~default:false
          in
          match ordered_arguments () with
          | [
           { kind = SingleStar; resolved = positional_component; _ };
           { kind = DoubleStar; resolved = keyword_component; _ };
          ]
            when combines_into_variable ~positional_component ~keyword_component ->
              base_signature_match
          | _ ->
              {
                base_signature_match with
                reasons = { arity = [CallingParameterVariadicTypeVariable]; annotation = [] };
              } )
    in
    let check_annotations ({ argument_mapping; _ } as signature_match) =
      let update ~key ~data ({ reasons = { arity; _ } as reasons; _ } as signature_match) =
        let bind_arguments_to_variadic ~expected ~arguments =
          let extract arguments =
            let extracted, errors =
              let arguments =
                List.map arguments ~f:(function
                    | Argument argument -> argument
                    | Default -> failwith "Variable parameters do not have defaults")
              in
              let extract { Argument.kind; resolved; expression; _ } =
                match kind with
                | SingleStar -> (
                    match resolved with
                    | Type.Tuple (Bounded ordered_types) -> `Fst ordered_types
                    (* We don't support expanding indefinite containers into ListVariadics *)
                    | annotation -> `Snd { expression; annotation } )
                | _ -> `Fst (Type.OrderedTypes.Concrete [resolved])
              in
              List.rev arguments |> List.partition_map ~f:extract
            in
            match errors with
            | [] -> Ok extracted
            | not_definite_tuple :: _ ->
                Error
                  (MismatchWithListVariadicTypeVariable
                     { variable = expected; mismatch = NotDefiniteTuple not_definite_tuple })
          in
          let concatenate extracted =
            let concatenated =
              match extracted with
              | [] -> Some (Type.OrderedTypes.Concrete [])
              | head :: tail ->
                  let concatenate sofar next =
                    sofar >>= fun left -> Type.OrderedTypes.concatenate ~left ~right:next
                  in
                  List.fold tail ~f:concatenate ~init:(Some head)
            in
            match concatenated with
            | Some concatenated -> Ok concatenated
            | None ->
                Error
                  (MismatchWithListVariadicTypeVariable
                     { variable = expected; mismatch = CantConcatenate extracted })
          in
          let solve concatenated =
            match
              List.concat_map signature_match.constraints_set ~f:(fun constraints ->
                  TypeOrder.solve_ordered_types_less_or_equal
                    order
                    ~left:concatenated
                    ~right:expected
                    ~constraints)
            with
            | [] ->
                Error
                  (MismatchWithListVariadicTypeVariable
                     { variable = expected; mismatch = ConstraintFailure concatenated })
            | updated_constraints_set -> Ok updated_constraints_set
          in
          let make_signature_match = function
            | Ok constraints_set -> { signature_match with constraints_set }
            | Error error ->
                { signature_match with reasons = { reasons with arity = error :: arity } }
          in
          let open Result in
          extract arguments >>= concatenate >>= solve |> make_signature_match
        in
        match key, data with
        | Parameter.Variable (Concatenation concatenation), arguments ->
            bind_arguments_to_variadic
              ~expected:(Type.OrderedTypes.Concatenation concatenation)
              ~arguments
        | Parameter.Variable _, []
        | Parameter.Keywords _, [] ->
            (* Parameter was not matched, but empty is acceptable for variable arguments and keyword
               arguments. *)
            signature_match
        | Parameter.KeywordOnly { name; _ }, []
        | Parameter.Named { name; _ }, [] ->
            (* Parameter was not matched *)
            let reasons = { reasons with arity = MissingArgument (Named name) :: arity } in
            { signature_match with reasons }
        | Parameter.Anonymous { index; _ }, [] ->
            (* Parameter was not matched *)
            let reasons = { reasons with arity = MissingArgument (Anonymous index) :: arity } in
            { signature_match with reasons }
        | Anonymous { annotation = parameter_annotation; _ }, arguments
        | KeywordOnly { annotation = parameter_annotation; _ }, arguments
        | Named { annotation = parameter_annotation; _ }, arguments
        | Variable (Concrete parameter_annotation), arguments
        | Keywords parameter_annotation, arguments ->
            let set_constraints_and_reasons
                ~position
                ~argument
                ~name
                ~argument_annotation
                ({ constraints_set; reasons = { annotation; _ }; _ } as signature_match)
              =
              let reasons_with_mismatch =
                let mismatch =
                  let location =
                    name >>| Node.location |> Option.value ~default:argument.Node.location
                  in
                  {
                    actual = argument_annotation;
                    expected = parameter_annotation;
                    name = Option.map name ~f:Node.value;
                    position;
                  }
                  |> Node.create ~location
                  |> fun mismatch -> Mismatch mismatch
                in
                { reasons with annotation = mismatch :: annotation }
              in
              match
                List.concat_map constraints_set ~f:(fun constraints ->
                    TypeOrder.solve_less_or_equal
                      order
                      ~constraints
                      ~left:argument_annotation
                      ~right:parameter_annotation)
              with
              | [] -> { signature_match with constraints_set; reasons = reasons_with_mismatch }
              | updated_constraints_set ->
                  { signature_match with constraints_set = updated_constraints_set }
            in
            let rec check signature_match = function
              | [] -> signature_match
              | Default :: tail ->
                  (* Parameter default value was used. Assume it is correct. *)
                  check signature_match tail
              | Argument { expression; full_expression; position; kind; resolved } :: tail -> (
                  let set_constraints_and_reasons argument_annotation =
                    let name =
                      match kind with
                      | Named name -> Some name
                      | _ -> None
                    in
                    set_constraints_and_reasons
                      ~position
                      ~argument:full_expression
                      ~argument_annotation
                      ~name
                      signature_match
                    |> fun signature_match -> check signature_match tail
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
                  let solution_based_extraction ~create_error ~synthetic_variable ~solve_against =
                    let signature_with_error =
                      { expression; annotation = resolved }
                      |> Node.create ~location:expression.location
                      |> create_error
                      |> add_annotation_error signature_match
                    in
                    let iterable_constraints =
                      if Type.is_unbound resolved then
                        []
                      else
                        TypeOrder.solve_less_or_equal
                          order
                          ~constraints:TypeConstraints.empty
                          ~left:resolved
                          ~right:solve_against
                    in
                    match iterable_constraints with
                    | [] -> signature_with_error
                    | iterable_constraint :: _ ->
                        TypeOrder.OrderedConstraints.solve ~order iterable_constraint
                        >>| (fun solution ->
                              TypeConstraints.Solution.instantiate_single_variable
                                solution
                                synthetic_variable
                              |> Option.value ~default:Type.Any)
                        >>| set_constraints_and_reasons
                        |> Option.value ~default:signature_with_error
                  in
                  match kind with
                  | DoubleStar ->
                      let create_error error = InvalidKeywordArgument error in
                      let synthetic_variable = Type.Variable.Unary.create "$_T" in
                      let solve_against =
                        Type.parametric
                          "typing.Mapping"
                          [Single Type.string; Single (Type.Variable synthetic_variable)]
                      in
                      solution_based_extraction ~create_error ~synthetic_variable ~solve_against
                  | SingleStar ->
                      let create_error error = InvalidVariableArgument error in
                      let synthetic_variable = Type.Variable.Unary.create "$_T" in
                      let solve_against = Type.iterable (Type.Variable synthetic_variable) in
                      solution_based_extraction ~create_error ~synthetic_variable ~solve_against
                  | Named _
                  | Positional ->
                      let argument_annotation =
                        if Type.Variable.all_variables_are_resolved parameter_annotation then
                          resolve_mutable_literals
                            ~assumptions
                            ~class_metadata_environment
                            ?dependency
                            ~resolve
                            ~expression:(Some expression)
                            ~resolved
                            ~expected:parameter_annotation
                        else
                          resolved
                      in
                      if Type.is_meta parameter_annotation && Type.is_top argument_annotation then
                        parse_annotation
                          ~assumptions
                          ~class_metadata_environment
                          ?allow_untracked:None
                          ?allow_invalid_type_parameters:None
                          ?allow_primitives_from_empty_stubs:None
                          ?dependency
                          expression
                        |> Type.meta
                        |> set_constraints_and_reasons
                      else
                        argument_annotation |> set_constraints_and_reasons )
            in
            List.rev arguments |> check signature_match
      in
      let check_if_solution_exists
          ( { constraints_set; reasons = { annotation; _ } as reasons; callable; _ } as
          signature_match )
        =
        let solutions =
          let variables = Type.Variable.all_free_variables (Type.Callable callable) in
          List.filter_map
            constraints_set
            ~f:(TypeOrder.OrderedConstraints.extract_partial_solution ~order ~variables)
        in
        if not (List.is_empty solutions) then
          signature_match
        else
          (* All other cases should have been able to been blamed on a specefic argument, this is
             the only global failure. *)
          {
            signature_match with
            reasons = { reasons with annotation = MutuallyRecursiveTypeVariables :: annotation };
          }
      in
      let special_case_dictionary_constructor
          ({ argument_mapping; callable; constraints_set; _ } as signature_match)
        =
        let open Type.Record.Callable in
        let has_matched_keyword_parameter parameters =
          List.find parameters ~f:(function
              | RecordParameter.Keywords _ -> true
              | _ -> false)
          >>= Type.Callable.Parameter.Map.find argument_mapping
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
              List.concat_map constraints_set ~f:(fun constraints ->
                  TypeOrder.solve_less_or_equal order ~constraints ~left:Type.string ~right:key_type)
            in
            if List.is_empty updated_constraints then (* TODO(T41074174): Error here *)
              signature_match
            else
              { signature_match with constraints_set = updated_constraints }
        | _ -> signature_match
      in
      Map.fold ~init:signature_match ~f:update argument_mapping
      |> special_case_dictionary_constructor
      |> check_if_solution_exists
    in
    let calculate_rank ({ reasons = { arity; annotation; _ }; _ } as signature_match) =
      let arity_rank = List.length arity in
      let positions, annotation_rank =
        let count_unique (positions, count) = function
          | Mismatch { Node.value = { position; _ }; _ } when not (Set.mem positions position) ->
              Set.add positions position, count + 1
          | Mismatch _ -> positions, count
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
    in
    let find_closest signature_matches =
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
      let determine_reason { callable; constraints_set; reasons = { arity; annotation; _ }; _ } =
        let callable =
          let instantiate annotation =
            let solution =
              let variables = Type.Variable.all_free_variables (Type.Callable callable) in
              List.filter_map
                constraints_set
                ~f:(TypeOrder.OrderedConstraints.extract_partial_solution ~order ~variables)
              |> List.map ~f:snd
              |> List.hd
              |> Option.value ~default:TypeConstraints.Solution.empty
            in
            TypeConstraints.Solution.instantiate solution annotation
            |> Type.Variable.mark_all_free_variables_as_escaped
            (* We need to do transformations of the form Union[T_escaped, int] => int in order to
               properly handle some typeshed stubs which only sometimes bind type variables and
               expect them to fall out in this way (see Mapping.get) *)
            |> Type.Variable.collapse_all_escaped_variable_unions
          in
          Type.Callable.map ~f:instantiate callable
          |> function
          | Some callable -> callable
          | _ -> failwith "Instantiate did not return a callable"
        in
        match List.rev arity, List.rev annotation with
        | [], [] -> Found callable
        | reason :: reasons, _
        | [], reason :: reasons ->
            let importance = function
              | AbstractClassInstantiation _ -> 1
              | CallingParameterVariadicTypeVariable -> 1
              | InvalidKeywordArgument _ -> 0
              | InvalidVariableArgument _ -> 0
              | Mismatch { Node.value = { position; _ }; _ } -> 0 - position
              | MissingArgument _ -> 1
              | MismatchWithListVariadicTypeVariable _ -> 1
              | MutuallyRecursiveTypeVariables -> 1
              | ProtocolInstantiation _ -> 1
              | TooManyArguments _ -> 1
              | UnexpectedKeyword _ -> 1
            in
            let get_most_important best_reason reason =
              if importance reason > importance best_reason then
                reason
              else
                best_reason
            in
            let reason = Some (List.fold ~init:reason ~f:get_most_important reasons) in
            NotFound { callable; reason }
      in
      signature_matches
      |> get_best_rank ~best_matches:[] ~best_rank:Int.max_value ~getter:get_arity_rank
      |> get_best_rank ~best_matches:[] ~best_rank:Int.max_value ~getter:get_annotation_rank
      |> get_best_rank ~best_matches:[] ~best_rank:Int.max_value ~getter:get_position_rank
      (* Each get_best_rank reverses the list, because we have an odd number, we need an extra
         reverse in order to prefer the first defined overload *)
      |> List.rev
      |> List.hd
      >>| determine_reason
      |> Option.value ~default:(NotFound { callable; reason = None })
    in
    let get_match signatures =
      signatures
      |> List.map ~f:match_arity
      |> List.map ~f:check_annotations
      |> List.map ~f:calculate_rank
      |> find_closest
    in
    if List.is_empty overloads then
      get_match [implementation]
    else
      get_match overloads


  let resolve_mutable_literals
      { constraints_solution_exists; _ }
      ~assumptions
      ~class_metadata_environment
      ?dependency
      ~resolve
    =
    weaken_mutable_literals
      resolve
      ~comparator:(constraints_solution_exists ?dependency ~class_metadata_environment ~assumptions)


  let constraints_solution_exists
      { full_order; _ }
      ~assumptions
      ~class_metadata_environment
      ?dependency
      ~left
      ~right
    =
    let order = full_order ?dependency class_metadata_environment ~assumptions in
    not
      ( TypeOrder.solve_less_or_equal order ~left ~right ~constraints:TypeConstraints.empty
      |> List.filter_map ~f:(TypeOrder.OrderedConstraints.solve ~order)
      |> List.is_empty )


  let constructor
      { uninstantiated_attribute_table; instantiate_attribute; _ }
      ~assumptions
      ~class_metadata_environment
      ?dependency
      (definition : ClassSummary.t Node.t)
      ~instantiated
    =
    let return_annotation =
      let class_annotation = class_annotation definition in
      match class_annotation with
      | Type.Primitive name
      | Type.Parametric { name; _ } -> (
          let generics =
            ClassHierarchyEnvironment.ReadOnly.variables
              (ClassMetadataEnvironment.ReadOnly.class_hierarchy_environment
                 class_metadata_environment)
              ?dependency
              (Reference.show (class_name definition))
            >>| List.map ~f:ClassHierarchy.Variable.to_parameter
            |> Option.value ~default:[]
          in
          (* Tuples are special. *)
          if String.equal name "tuple" then
            match generics with
            | [Single tuple_variable] -> Type.Tuple (Type.Unbounded tuple_variable)
            | _ -> Type.Tuple (Type.Unbounded Type.Any)
          else
            let backup = Type.Parametric { name; parameters = generics } in
            match instantiated, generics with
            | _, [] -> instantiated
            | Type.Primitive instantiated_name, _ when String.equal instantiated_name name -> backup
            | Type.Parametric { parameters; name = instantiated_name }, generics
              when String.equal instantiated_name name
                   && List.length parameters <> List.length generics ->
                backup
            | _ -> instantiated )
      | _ -> instantiated
    in
    let definitions =
      definition
      :: ClassMetadataEnvironment.ReadOnly.superclasses
           class_metadata_environment
           ?dependency
           definition
      |> List.map ~f:(fun definition -> class_annotation definition)
    in
    let definition_index parent =
      parent
      |> (fun class_annotation ->
           List.findi definitions ~f:(fun _ annotation -> Type.equal annotation class_annotation))
      >>| fst
      |> Option.value ~default:Int.max_value
    in
    let uninstantiated_attribute_table =
      uninstantiated_attribute_table
        ~assumptions
        ~transitive:true
        ~class_attributes:false
        ~include_generated_attributes:true
        ?special_method:None
        ?dependency
        definition
        ~class_metadata_environment
    in
    let signature_and_index ~name =
      let signature, parent =
        match UninstantiatedAttributeTable.lookup_name uninstantiated_attribute_table name with
        | Some attribute ->
            ( instantiate_attribute
                ~assumptions
                ~class_metadata_environment
                ?instantiated:(Some return_annotation)
                attribute
              |> AnnotatedAttribute.annotation
              |> Annotation.annotation,
              Type.Primitive (AnnotatedAttribute.parent attribute) )
        | None -> Type.Top, class_annotation definition
      in
      signature, definition_index parent
    in
    let constructor_signature, constructor_index = signature_and_index ~name:"__init__" in
    let new_signature, new_index =
      let new_signature, new_index = signature_and_index ~name:"__new__" in
      let drop_class_parameter = function
        | Type.Callable { Type.Callable.kind; implementation; overloads; implicit } ->
            let drop_parameter { Type.Callable.annotation; parameters; define_location } =
              let parameters =
                match parameters with
                | Type.Callable.Defined (_ :: parameters) -> Type.Callable.Defined parameters
                | _ -> parameters
              in
              { Type.Callable.annotation; parameters; define_location }
            in
            Type.Callable
              {
                kind;
                implementation = drop_parameter implementation;
                overloads = List.map overloads ~f:drop_parameter;
                implicit;
              }
        | annotation -> annotation
      in
      drop_class_parameter new_signature, new_index
    in
    let signature =
      if new_index < constructor_index then
        new_signature
      else
        constructor_signature
    in
    match signature with
    | Type.Callable callable ->
        Type.Callable (Type.Callable.with_return_annotation ~annotation:return_annotation callable)
    | _ -> signature
end

let make_open_recurser ~given_uninstantiated_attribute_table ~given_parse_annotation =
  let rec open_recurser =
    {
      Implementation.full_order;
      uninstantiated_attribute_table;
      check_invalid_type_parameters;
      parse_annotation;
      create_attribute;
      metaclass;
      constraints;
      resolve_literal;
      create_overload;
      signature_select;
      resolve_mutable_literals;
      constraints_solution_exists;
      constructor;
      instantiate_attribute;
    }
  and uninstantiated_attribute_table ~assumptions =
    given_uninstantiated_attribute_table open_recurser ~assumptions
  and parse_annotation ~assumptions = given_parse_annotation open_recurser ~assumptions
  and full_order ~assumptions = Implementation.full_order open_recurser ~assumptions
  and check_invalid_type_parameters ~assumptions =
    Implementation.check_invalid_type_parameters open_recurser ~assumptions
  and create_attribute ~assumptions = Implementation.create_attribute open_recurser ~assumptions
  and metaclass ~assumptions = Implementation.metaclass open_recurser ~assumptions
  and constraints ~assumptions = Implementation.constraints open_recurser ~assumptions
  and resolve_literal ~assumptions = Implementation.resolve_literal open_recurser ~assumptions
  and create_overload ~assumptions = Implementation.create_overload open_recurser ~assumptions
  and signature_select ~assumptions = Implementation.signature_select open_recurser ~assumptions
  and resolve_mutable_literals ~assumptions =
    Implementation.resolve_mutable_literals open_recurser ~assumptions
  and constraints_solution_exists ~assumptions =
    Implementation.constraints_solution_exists open_recurser ~assumptions
  and constructor ~assumptions = Implementation.constructor open_recurser ~assumptions
  and instantiate_attribute ~assumptions =
    Implementation.instantiate_attribute open_recurser ~assumptions
  in
  open_recurser


let empty_assumptions =
  {
    protocol_assumptions = ProtocolAssumptions.empty;
    callable_assumptions = CallableAssumptions.empty;
  }


module ParseAnnotationCache = struct
  module Cache = ManagedCache.Make (struct
    module PreviousEnvironment = ClassMetadataEnvironment
    module Key = SharedMemoryKeys.ParseAnnotationKey

    module Value = struct
      type t = Type.t [@@deriving compare]

      let prefix = Prefix.make ()

      let description = "parse annotation"

      let unmarshall value = Marshal.from_string value 0
    end

    module KeySet = SharedMemoryKeys.ParseAnnotationKey.Set
    module HashableKey = SharedMemoryKeys.ParseAnnotationKey

    let lazy_incremental = false

    let produce_value
        class_metadata_environment
        ( {
            SharedMemoryKeys.ParseAnnotationKey.assumptions;
            allow_untracked;
            allow_invalid_type_parameters;
            allow_primitives_from_empty_stubs;
            expression;
          } as key )
        ~track_dependencies
      =
      let uncached_open_recurser =
        make_open_recurser
          ~given_uninstantiated_attribute_table:Implementation.uninstantiated_attribute_table
          ~given_parse_annotation:Implementation.parse_annotation
      in
      let dependency =
        if track_dependencies then Some (SharedMemoryKeys.ParseAnnotation key) else None
      in
      Implementation.parse_annotation
        uncached_open_recurser
        ~assumptions
        ~class_metadata_environment
        ~allow_untracked
        ~allow_invalid_type_parameters
        ~allow_primitives_from_empty_stubs
        ?dependency
        expression


    let filter_upstream_dependency = function
      | SharedMemoryKeys.ParseAnnotation key -> Some key
      | _ -> None
  end)

  include Cache

  module ReadOnly = struct
    include Cache.ReadOnly

    let cached_parse_annotation
        read_only
        _open_recurser
        ~assumptions
        ~class_metadata_environment:_
        ?(allow_untracked = false)
        ?(allow_invalid_type_parameters = false)
        ?(allow_primitives_from_empty_stubs = false)
        ?dependency
        expression
      =
      get
        read_only
        ?dependency
        {
          SharedMemoryKeys.ParseAnnotationKey.assumptions;
          allow_untracked;
          allow_invalid_type_parameters;
          allow_primitives_from_empty_stubs;
          expression;
        }
  end
end

module Cache = ManagedCache.Make (struct
  module PreviousEnvironment = ParseAnnotationCache
  module Key = SharedMemoryKeys.AttributeTableKey

  module Value = struct
    type t = UninstantiatedAttributeTable.t option [@@deriving compare]

    let prefix = Prefix.make ()

    let description = "attributes"

    let unmarshall value = Marshal.from_string value 0
  end

  module KeySet = SharedMemoryKeys.AttributeTableKey.Set
  module HashableKey = SharedMemoryKeys.AttributeTableKey

  let lazy_incremental = true

  let produce_value
      parse_annotation_cache
      ( {
          SharedMemoryKeys.AttributeTableKey.transitive;
          class_attributes;
          include_generated_attributes;
          special_method;
          name;
          assumptions;
        } as key )
      ~track_dependencies
    =
    let dependency =
      if track_dependencies then Some (SharedMemoryKeys.AttributeTable key) else None
    in
    let class_metadata_environment =
      ParseAnnotationCache.ReadOnly.upstream_environment parse_annotation_cache
    in
    let unannotated_global_environment =
      unannotated_global_environment class_metadata_environment
    in
    let open_recurser_with_parse_annotation_cache =
      make_open_recurser
        ~given_uninstantiated_attribute_table:Implementation.uninstantiated_attribute_table
        ~given_parse_annotation:
          (ParseAnnotationCache.ReadOnly.cached_parse_annotation parse_annotation_cache)
    in
    UnannotatedGlobalEnvironment.ReadOnly.get_class_definition
      unannotated_global_environment
      ?dependency
      name
    >>| Implementation.uninstantiated_attribute_table
          open_recurser_with_parse_annotation_cache
          ~transitive
          ~class_attributes
          ~include_generated_attributes
          ~special_method
          ?dependency
          ~class_metadata_environment
          ~assumptions


  let filter_upstream_dependency = function
    | SharedMemoryKeys.AttributeTable key -> Some key
    | _ -> None
end)

module PreviousEnvironment = ClassMetadataEnvironment
include Cache

module ReadOnly = struct
  include Cache.ReadOnly

  let parse_annotation_cache = upstream_environment

  let class_metadata_environment read_only =
    ParseAnnotationCache.ReadOnly.upstream_environment (upstream_environment read_only)


  let cached_attribute_table
      read_only
      (* There's no need to use the given open recurser since we're already using the uncached
         version inside of the implementation of produce_value *)
        _open_recurser
      ~assumptions
      ~transitive
      ~class_attributes
      ~include_generated_attributes
      ?(special_method = false)
      ?dependency
      { Node.value = { ClassSummary.name; _ }; _ }
      ~class_metadata_environment:
        (* Similarly the class_metadata_environment is already baked into the get *)
        _
    =
    get
      read_only
      ?dependency
      {
        SharedMemoryKeys.AttributeTableKey.transitive;
        class_attributes;
        include_generated_attributes;
        special_method;
        name = Reference.show name;
        assumptions;
      }
    |> Option.value ~default:(UninstantiatedAttributeTable.create ())


  let open_recurser_with_both_caches read_only =
    make_open_recurser
      ~given_uninstantiated_attribute_table:(cached_attribute_table read_only)
      ~given_parse_annotation:
        (ParseAnnotationCache.ReadOnly.cached_parse_annotation (parse_annotation_cache read_only))


  let add_both_caches_and_empty_assumptions f read_only =
    f
      (open_recurser_with_both_caches read_only)
      ~assumptions:empty_assumptions
      ~class_metadata_environment:(class_metadata_environment read_only)


  let instantiate_attribute =
    add_both_caches_and_empty_assumptions Implementation.instantiate_attribute


  let attribute
      read_only
      ~transitive
      ~class_attributes
      ~include_generated_attributes
      ?(special_method = false)
      ?instantiated
      ?dependency
      ~attribute_name
      class_name
    =
    get
      read_only
      ?dependency
      {
        SharedMemoryKeys.AttributeTableKey.transitive;
        class_attributes;
        include_generated_attributes;
        special_method;
        name = class_name;
        assumptions = empty_assumptions;
      }
    >>= fun table ->
    UninstantiatedAttributeTable.lookup_name table attribute_name
    >>| instantiate_attribute read_only ?instantiated


  let attribute_names
      read_only
      ~transitive
      ~class_attributes
      ~include_generated_attributes
      ?(special_method = false)
      ?dependency
      class_name
    =
    get
      read_only
      ?dependency
      {
        SharedMemoryKeys.AttributeTableKey.transitive;
        class_attributes;
        include_generated_attributes;
        special_method;
        name = class_name;
        assumptions = empty_assumptions;
      }
    >>| UninstantiatedAttributeTable.names


  let all_attributes
      read_only
      ~transitive
      ~class_attributes
      ~include_generated_attributes
      ?(special_method = false)
      ?dependency
      class_name
    =
    get
      read_only
      ?dependency
      {
        SharedMemoryKeys.AttributeTableKey.transitive;
        class_attributes;
        include_generated_attributes;
        special_method;
        name = class_name;
        assumptions = empty_assumptions;
      }
    >>| UninstantiatedAttributeTable.to_list


  let check_invalid_type_parameters =
    add_both_caches_and_empty_assumptions Implementation.check_invalid_type_parameters


  let parse_annotation = add_both_caches_and_empty_assumptions Implementation.parse_annotation

  let metaclass = add_both_caches_and_empty_assumptions Implementation.metaclass

  let constraints = add_both_caches_and_empty_assumptions Implementation.constraints

  let resolve_literal = add_both_caches_and_empty_assumptions Implementation.resolve_literal

  let create_overload = add_both_caches_and_empty_assumptions Implementation.create_overload

  let resolve_mutable_literals =
    add_both_caches_and_empty_assumptions Implementation.resolve_mutable_literals


  let constraints_solution_exists =
    add_both_caches_and_empty_assumptions Implementation.constraints_solution_exists


  let constructor = add_both_caches_and_empty_assumptions Implementation.constructor

  let full_order ?dependency read_only =
    Implementation.full_order
      ?dependency
      (open_recurser_with_both_caches read_only)
      ~assumptions:empty_assumptions
      (class_metadata_environment read_only)


  let signature_select = add_both_caches_and_empty_assumptions Implementation.signature_select
end

module AttributeReadOnly = ReadOnly
include TypeParameterValidationTypes
include SignatureSelectionTypes
