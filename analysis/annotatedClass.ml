(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Pyre
open Statement

module Callable = AnnotatedCallable


type t = Class.t Node.t
[@@deriving compare, eq, sexp, show, hash]


type decorator = {
  access: string;
  arguments: (Argument.t list) option
}
[@@deriving compare, eq, sexp, show, hash]


let name_equal
    { Node.value = { Class.name = left; _ }; _ }
    { Node.value = { Class.name = right; _ }; _ } =
  Access.equal left right


type class_t = t
[@@deriving compare, eq, sexp, show, hash]


let create definition =
  definition


let name { Node.value = { Class.name; _ }; _ } =
  name


let bases { Node.value = { Class.bases; _ }; _ } =
  bases


let get_decorator { Node.value = { Class.decorators; _ }; _ } ~decorator =
  let matches target decorator =
    match decorator with
    | { Node.value = Access access; _ } ->
        begin
          match Expression.Access.name_and_arguments ~call:access with
          | Some { callee = name; arguments } when name = target ->
              Some { access = name; arguments = Some arguments }
          | None when Access.show access = target ->
              Some { access = Access.show access; arguments = None }
          | _ ->
              None
        end
    | _ ->
        None
  in
  List.filter_map ~f:(matches decorator) decorators


let annotation { Node.value = { Class.name; _ }; location } ~resolution =
  Resolution.parse_annotation resolution (Node.create ~location (Access name))


let successors class_node ~resolution =
  annotation class_node ~resolution
  |> Type.split
  |> (fun (primitive, _ ) -> primitive)
  |> Resolution.class_representation resolution
  >>| (fun { Resolution.successors; _ } -> successors)
  |> Option.value ~default:[]


let successors_fold class_node ~resolution ~f ~initial =
  successors class_node ~resolution
  |> List.fold ~init:initial ~f


module Method = struct
  type t = {
    define: Define.t;
    parent: Type.t;
  }
  [@@deriving compare, eq, sexp, show, hash]


  let create ~define ~parent =
    { define; parent }


  let name { define = { Define.name; _ }; _ } =
    name


  let define { define; _ } =
    define


  let parent { parent; _ } =
    parent


  let parameter_annotations { define = { Define.parameters; _ }; _ } ~resolution =
    let element { Node.value = { Parameter.name; annotation; _ }; _ } =
      let annotation =
        (annotation
         >>| fun annotation -> Resolution.parse_annotation resolution annotation)
        |> Option.value ~default:Type.Top
      in
      name, annotation
    in
    List.map parameters ~f:element
    |> Identifier.Map.of_alist_exn


  let parameter_annotations_positional { define = { Define.parameters; _ }; _ } ~resolution =
    let element index { Node.value = { Parameter.annotation; _ }; _ } =
      let annotation =
        (annotation
         >>| fun annotation -> Resolution.parse_annotation resolution annotation)
        |> Option.value ~default:Type.Top
      in
      index, annotation
    in
    List.mapi ~f:element parameters
    |> Int.Map.of_alist_exn


  let return_annotation
      { define = { Define.return_annotation; async; _ } as define; _ }
      ~resolution =
    let annotation =
      Option.value_map
        return_annotation
        ~f:(Resolution.parse_annotation resolution)
        ~default:Type.Top
    in
    if async then
      Type.awaitable annotation
    else
    if Define.is_coroutine define then
      begin
        match annotation with
        | Type.Parametric { name; parameters = [_; _; return_annotation] }
          when Identifier.show name = "typing.Generator" ->
            Type.awaitable return_annotation
        | _ ->
            Type.Top
      end
    else
      annotation
end


let generics { Node.value = { Class.bases; _ }; _ } ~resolution =
  let generic { Argument.value; _ } =
    let annotation = Resolution.parse_annotation resolution value in
    match annotation with
    | Type.Parametric { parameters; _ }
      when Type.is_generic annotation ->
        Some parameters
    | Type.Parametric { parameters; _ }
      when Type.is_protocol annotation ->
        Some parameters
    | _ ->
        None
  in
  let find_single_type_variable { Argument.value; _ } =
    match Resolution.parse_annotation resolution value with
    | Type.Parametric { parameters = [Type.Variable variable]; _ } ->
        Some [Type.Variable variable]
    | _ ->
        None
  in
  begin
    match List.find_map ~f:generic bases with
    | None -> List.find_map ~f:find_single_type_variable bases
    | Some parameters -> Some parameters
  end
  |> Option.value ~default:[]


let inferred_generic_base { Node.value = { Class.bases; _ }; _ } ~resolution =
  let is_generic { Argument.value; _ } =
    let primitive, _ =
      Resolution.parse_annotation resolution value
      |> Type.split
    in
    Type.equal primitive Type.generic
  in
  let find_single_type_variable { Argument.value; _ } =
    let _, parameters =
      Resolution.parse_annotation resolution value
      |> Type.split
    in
    match parameters with
    | [Type.Variable variable] ->
        Some (Type.Variable variable)
    | _ ->
        None
  in
  if List.exists ~f:is_generic bases then
    []
  else
    begin
      List.find_map ~f:find_single_type_variable bases
      >>| fun annotation -> [{
          Argument.name = None;
          value =
            Type.parametric "typing.Generic" [annotation]
            |> Type.expression;
        }]
    end
    |> Option.value ~default:[]


let constraints ?target ?parameters definition ~instantiated ~resolution =
  let target = Option.value ~default:definition target in
  let parameters =
    match parameters with
    | None ->
        generics ~resolution target
    | Some parameters ->
        parameters
  in
  let resolved_parameters =
    let target =
      annotation ~resolution target
      |> Type.split
      |> fst
    in
    TypeOrder.instantiate_successors_parameters
      (Resolution.order resolution)
      ~source:instantiated
      ~target
    |> Option.value ~default:[]
  in
  if List.length parameters = List.length resolved_parameters then
    let rec compute_constraints map expected instantiated =
      match expected, instantiated with
      | Type.Parametric { name = left_name; parameters = left_parameters },
        Type.Parametric { name = right_name; parameters = right_parameters }
        when Identifier.equal left_name right_name ->
          List.fold2 ~init:map ~f:compute_constraints left_parameters right_parameters
          |>
          (function
            | List.Or_unequal_lengths.Ok map ->
                map
            | _ ->
                None)
      | Type.Variable { constraints = Type.Unconstrained; _ }, _
        when not (Type.equal instantiated Type.Bottom) ->
          map
          >>| Map.set ~key:expected ~data:instantiated
      | Type.Variable { constraints = Type.Explicit constraints; _ }, _ ->
          let matches_constraint variable_constraint =
            Resolution.less_or_equal
              resolution
              ~left:instantiated
              ~right:variable_constraint
          in
          if List.exists ~f:matches_constraint constraints then
            map
            >>| Map.set ~key:expected ~data:instantiated
          else
            None
      | _ ->
          map
    in
    List.fold2_exn
      ~init:(Some Type.Map.empty)
      ~f:compute_constraints
      parameters
      resolved_parameters
    |> Option.value ~default:Type.Map.empty
  else
    Type.Map.empty


let superclasses definition ~resolution =
  successors ~resolution definition
  |> List.filter_map ~f:(Resolution.class_definition resolution)
  |> List.map ~f:create


let immediate_superclasses definition ~resolution =
  let (module Handler: TypeOrder.Handler) = Resolution.order resolution in
  let annotation = annotation definition ~resolution in

  let has_definition { TypeOrder.Target.target; _ } =
    Handler.find (Handler.annotations ()) target
    >>= Resolution.class_definition resolution
    >>| create
  in
  Handler.find (Handler.indices ()) annotation
  >>= Handler.find (Handler.edges ())
  |> Option.value ~default:[]
  |> List.find_map ~f:has_definition


let metaclass definition ~resolution =
  let get_metaclass { Node.value = { Class.bases; _ }; _ } =
    let get_metaclass = function
      | { Argument.name = Some { Node.value = metaclass; _ }; value }
        when Identifier.equal metaclass (Identifier.create "metaclass") ->
          Some (Resolution.parse_annotation resolution value)
      | _ ->
          None
    in
    List.find_map ~f:get_metaclass bases
  in
  definition :: superclasses ~resolution definition
  |> List.find_map ~f:get_metaclass
  |> Option.value ~default:(Type.primitive "type")


let methods ({ Node.value = { Class.body; _ }; _ } as definition) ~resolution =
  let extract_define = function
    | { Node.value = Define define; _ } ->
        Some (Method.create ~define ~parent:(annotation definition ~resolution))
    | _ ->
        None
  in
  List.filter_map ~f:extract_define body


let is_protocol { Node.value = { Class.bases; _ }; _ } =
  let is_protocol { Argument.name; value } =
    match name, Expression.show value with
    | None, "typing.Protocol"
    | None, "typing_extensions.Protocol" ->
        true
    | _ ->
        false
  in
  List.exists ~f:is_protocol bases


module Attribute = struct
  type attribute = {
    name: Expression.expression;
    parent: Type.t;
    annotation: Annotation.t;
    value: Expression.t;
    defined: bool;
    class_attribute: bool;
    async: bool;
  }
  [@@deriving eq, show]

  type t = attribute Node.t
  [@@deriving eq, show]


  let create
      ~resolution
      ~parent
      ?instantiated
      ?(defined = true)
      ?(default_class_attribute = false)
      {
        Node.location;
        value = {
          Attribute.target = { Node.value = target; _ };
          annotation = attribute_annotation;
          defines;
          value;
          async;
          setter;
          property;
          primitive;
        };
      } =
    let class_annotation = annotation parent ~resolution in

    (* Account for class attributes. *)
    let annotation, class_attribute =
      (attribute_annotation
       >>| Resolution.parse_annotation resolution
       >>| (fun annotation ->
           match Type.class_variable_value annotation with
           | Some annotation -> Some annotation, true
           | _ -> Some annotation, false))
      |> Option.value ~default:(None, default_class_attribute)
    in

    (* Handle enumeration attributes. *)
    let annotation, value, class_attribute =
      let superclasses =
        superclasses ~resolution parent
        |> List.map ~f:(fun definition -> name definition |> Access.show)
        |> String.Set.of_list
      in
      if not (Set.is_empty (Set.inter Recognized.enumeration_classes superclasses)) &&
         primitive then
        Some class_annotation, None, true  (* Enums override values. *)
      else
        annotation, value, class_attribute
    in

    (* Handle Callables *)
    let annotation =
      let instantiated =
        match instantiated with
        | Some instantiated ->
            instantiated
        | None ->
            class_annotation
      in
      match defines with
      | Some ((define :: _) as defines) ->
          let parent =
            if Define.is_static_method define then
              None
            else if Define.is_class_method define then
              Some (Type.meta instantiated)
            else if class_attribute then
              (* Keep first argument around when calling instance methods from class attributes. *)
              None
            else
              Some instantiated
          in
          List.map defines ~f:(fun define -> Callable.apply_decorators ~define ~resolution)
          |> Callable.create ~resolution ~parent
          |> (fun callable -> Some (Type.Callable callable))
      | _ ->
          annotation
    in

    let annotation =
      match annotation, value with
      | Some annotation, Some value ->
          Annotation.create_immutable
            ~global:true
            ~original:(Some annotation)
            (if setter then
               (Resolution.parse_annotation resolution value)
             else
               annotation)
      | Some annotation, None ->
          Annotation.create_immutable ~global:true annotation
      | None, Some value ->
          Annotation.create_immutable
            ~global:true
            ~original:(Some Type.Top)
            (if setter then
               (Resolution.parse_annotation resolution value)
             else
               (Resolution.resolve_literal resolution value))
      | _ ->
          Annotation.create Type.Top
    in

    (* Special case properties with type variables. *)
    let annotation =
      let free_variables =
        let variables =
          Annotation.annotation annotation
          |> Type.variables
          |> Type.Set.of_list
        in
        let generics =
          generics parent ~resolution
          |> Type.Set.of_list
        in
        Set.diff variables generics
        |> Set.to_list
      in
      if property && not (List.is_empty free_variables) then
        let constraints =
          List.fold
            free_variables
            ~init:Type.Map.empty
            ~f:(fun map variable -> Map.set map ~key:variable ~data:class_annotation)
          |> Map.find
        in
        Annotation.annotation annotation
        |> Type.instantiate ~constraints
        |> Annotation.create_immutable ~global:true ~original:(Some Type.Top)
      else
        annotation
    in

    let value = Option.value value ~default:(Node.create Ellipses ~location) in

    {
      Node.location;
      value = {
        name = target;
        parent = class_annotation;
        annotation;
        value;
        defined;
        class_attribute;
        async;
      };

    }


  let name { Node.value = { name; _ }; _ } =
    name


  let access { Node.value = { name; _ }; _ } =
    match name with
    | Access access -> access
    | _ -> []


  let annotation { Node.value = { annotation; async; _ }; _ } =
    if async then
      Annotation.annotation annotation
      |> Type.awaitable
      |> Annotation.create
    else
      annotation


  let parent { Node.value = { parent; _ }; _ } =
    parent


  let value { Node.value = { value; _ }; _ } =
    value


  let initialized { Node.value = { value = { Node.value; _ }; _ }; _ } =
    match value with
    | Ellipses -> false
    | _ -> true


  let location { Node.location; _ } =
    location


  let defined { Node.value = { defined; _ }; _ } =
    defined


  let class_attribute { Node.value = { class_attribute; _ }; _ } =
    class_attribute


  let async { Node.value = { async; _ }; _ } =
    async


  let instantiate
      ({ Node.value = ({ annotation; _ } as attribute); _ } as attribute_node)
      ~constraints =
    {
      attribute_node with
      Node.value = { attribute with annotation = Annotation.instantiate annotation ~constraints }
    }

  module Cache = struct
    type t = {
      transitive: bool;
      class_attributes: bool;
      include_generated_attributes: bool;
      name: Access.t;
    }
    [@@deriving compare, sexp, hash]


    include Hashable.Make(struct
        type nonrec t = t
        let compare = compare
        let hash = Hashtbl.hash
        let hash_fold_t = hash_fold_t
        let sexp_of_t = sexp_of_t
        let t_of_sexp = t_of_sexp
      end)


    let cache: attribute Node.t list Table.t =
      Table.create ~size:1023 ()


    let clear () =
      Table.clear cache
  end



end


let attributes
    ?(transitive = false)
    ?(class_attributes = false)
    ?(include_generated_attributes = true)
    ({ Node.value = { Class.name; _ }; _ } as definition)
    ~resolution =
  let key =
    {
      Attribute.Cache.transitive;
      class_attributes;
      include_generated_attributes;
      name;
    }
  in
  match Hashtbl.find Attribute.Cache.cache key with
  | Some result ->
      result
  | None ->
      let class_annotation = annotation definition ~resolution in
      let definition_attributes
          ~in_test
          attributes
          ({ Node.value = definition; _ } as parent) =
        let collect_attributes attributes attribute =
          let reverse_define_order
              { Node.value = { Statement.Attribute.defines; _ } as attribute_value; location } =
            {
              Node.location;
              value = {
                attribute_value with Statement.Attribute.defines = defines >>| List.rev
              };
            }
          in
          let attribute_node =
            (* Map fold returns the reverse order; order matters when constructing callables *)
            attribute
            |> reverse_define_order
            |> Attribute.create
              ~resolution
              ~parent
              ~instantiated:class_annotation
              ~default_class_attribute:class_attributes
          in
          let existing_attribute =
            let compare_names existing_attribute =
              Expression.equal_expression
                (Attribute.name attribute_node)
                (Attribute.name existing_attribute)
            in
            List.find ~f:compare_names attributes
          in
          (* We allow instance variables to be accessed as class variables. *)
          match existing_attribute, attribute_node with
          | Some { Node.value = { Attribute.annotation = existing_annotation; _ }; _ },
            { Node.value = ({ Attribute.annotation = new_annotation; _ } as attribute); _ } ->
              let open Type.Callable in
              let merged_annotation =
                match
                  Annotation.annotation existing_annotation,
                  Annotation.annotation new_annotation with
                | Type.Callable ({ overloads; _ } as existing_callable),
                  Type.Callable
                    { implementation; overloads = new_overloads; _ } ->
                    Annotation.create (Type.Callable {
                        existing_callable with
                        implementation;
                        overloads = new_overloads @ overloads })
                | _ ->
                    existing_annotation
              in
              {
                attribute_node with
                Node.value = { attribute with Attribute.annotation = merged_annotation }
              } :: attributes
          | _ ->
              attribute_node :: attributes
        in
        Statement.Class.attributes ~include_generated_attributes ~in_test definition
        |> fun attribute_map ->
        Access.SerializableMap.fold
          (fun _ data attributes -> collect_attributes attributes data)
          attribute_map
          attributes
      in
      let superclass_definitions = superclasses ~resolution definition in
      let in_test =
        let is_unit_test { Node.value = { Record.Class.name; _ }; _ } =
          Access.show name
          |> String.equal "unittest.TestCase"
        in
        List.exists ~f:is_unit_test (definition :: superclass_definitions)
      in
      (* Pass over normal class hierarchy. *)
      let accumulator =
        let definitions =
          if transitive then
            definition :: superclass_definitions
          else
            [definition]
        in
        List.fold
          ~f:(definition_attributes ~in_test)
          ~init:[]
          definitions
      in
      (* Class over meta hierarchy if necessary. *)
      let meta_definitions =
        if class_attributes then
          metaclass ~resolution definition
          |> Resolution.class_definition resolution
          >>| (fun definition -> definition :: superclasses ~resolution definition)
          |> Option.value ~default:[]
        else
          []
      in
      let result =
        List.fold
          ~f:(definition_attributes ~in_test)
          ~init:accumulator
          meta_definitions
        |> List.rev
      in
      Hashtbl.set ~key ~data:result Attribute.Cache.cache;
      result

let implements ~resolution definition ~protocol =
  let overload_implements (name, overload) (protocol_name, protocol_overload) =
    let open Type.Callable in
    Access.equal name protocol_name &&
    Type.equal overload.annotation protocol_overload.annotation &&
    equal_parameters Type.equal overload.parameters protocol_overload.parameters
  in
  let rec implements instance_methods protocol_methods =
    match instance_methods, protocol_methods with
    | _, [] ->
        true
    | [], _ :: _ ->
        false
    | instance_method :: instance_methods,
      ((protocol_method :: protocol_methods) as old_protocol_methods) ->
        if overload_implements instance_method protocol_method then
          implements instance_methods protocol_methods
        else
          implements instance_methods old_protocol_methods
  in
  let callables_of_attribute =
    function
    | { Node.value =
          { Attribute.annotation = {
                Annotation.annotation = Type.Callable {
                    kind = Type.Record.Callable.Named callable_name;
                    implementation;
                    overloads;
                    _ };
                _ };
            parent;
            _ }; _ } ->
        let local_name =
          Access.drop_prefix
            callable_name
            ~prefix:(Expression.Access.create (Type.show parent))
        in
        List.map ~f:(fun overload -> (local_name, overload)) (implementation :: overloads)
    | _ -> []
  in
  let definition_attributes = attributes ~resolution definition in
  let protocol_attributes = attributes ~resolution protocol in
  implements
    (List.concat_map ~f:callables_of_attribute definition_attributes)
    (List.concat_map ~f:callables_of_attribute protocol_attributes)

let attribute_fold
    ?(transitive = false)
    ?(class_attributes = false)
    ?(include_generated_attributes = true)
    definition
    ~initial
    ~f
    ~resolution =
  attributes ~transitive ~class_attributes ~include_generated_attributes ~resolution definition
  |> List.fold ~init:initial ~f


let attribute
    ?(transitive = false)
    ?(class_attributes = false)
    ({ Node.location; _ } as definition)
    ~resolution
    ~name
    ~instantiated =
  let undefined =
    Attribute.create
      ~resolution
      ~parent:definition
      ~defined:false
      ~default_class_attribute:class_attributes
      {
        Node.location;
        value = {
          Statement.Attribute.target = Node.create_with_default_location (Access name);
          annotation = None;
          defines = None;
          value = None;
          async = false;
          setter = false;
          property = false;
          primitive = true;
        }
      }
  in
  attributes
    ~transitive
    ~class_attributes
    ~include_generated_attributes:true
    ~resolution
    definition
  |> List.find
    ~f:(fun attribute -> Expression.equal_expression (Access name) (Attribute.name attribute))
  |> Option.value ~default:undefined
  |> (fun attribute ->
      Attribute.parent attribute
      |> Resolution.class_definition resolution
      >>| (fun target ->
          let constraints =
            constraints
              ~target
              ~instantiated
              ~resolution
              definition
          in
          Attribute.instantiate ~constraints attribute)
      |> Option.value ~default:undefined)


let fallback_attribute ~resolution ~access definition =
  let fallback =
    attribute
      definition
      ~class_attributes:true
      ~transitive:true
      ~resolution
      ~name:(Access.create "__getattr__")
      ~instantiated:(annotation definition ~resolution)
  in
  if Attribute.defined fallback then
    let annotation =
      fallback
      |> Attribute.annotation
      |> Annotation.annotation
    in
    begin
      match annotation with
      | Type.Callable { Type.Callable.implementation; _ } ->
          let return_annotation = Type.Callable.Overload.return_annotation implementation in
          let location = Attribute.location fallback in
          Some
            (Attribute.create
               ~resolution
               ~parent:definition
               {
                 Node.location;
                 value = {
                   Statement.Attribute.target = Node.create ~location (Access access);
                   annotation = Some (Type.expression return_annotation);
                   defines = None;
                   value = None;
                   async = false;
                   setter = false;
                   property = false;
                   primitive = true;
                 };
               })
      | _ ->
          None
    end
  else
    None


let constructor definition ~resolution =
  let class_annotation = annotation definition ~resolution in
  let return_annotation =
    match class_annotation with
    | Type.Primitive name
    | Type.Parametric { name; _ } ->
        let generics = generics definition ~resolution in
        (* Tuples are special. *)
        if Identifier.show name = "tuple" then
          match generics with
          | [tuple_variable] ->
              Type.Tuple (Type.Unbounded tuple_variable)
          | _ ->
              Type.Tuple (Type.Unbounded Type.Object)
        else if List.is_empty generics then
          class_annotation
        else
          Type.Parametric { name; parameters = generics }
    | _ ->
        class_annotation
  in
  let class_annotation, _ = Type.split class_annotation in
  let definitions =
    definition :: superclasses ~resolution definition
    |> List.map ~f:name
  in
  let definition_index attribute =
    attribute
    |> Attribute.parent
    |> Type.show
    |> Access.create
    |> (fun class_name -> List.findi definitions ~f:(fun _ name -> Access.equal name class_name))
    >>| fst
    |> Option.value ~default:Int.max_value
  in
  let constructor_signature, constructor_index =
    let attribute =
      attribute
        definition
        ~transitive:true
        ~resolution
        ~name:(Access.create "__init__")
        ~instantiated:class_annotation
    in
    let signature =
      attribute
      |> Attribute.annotation
      |> Annotation.annotation
    in
    signature, definition_index attribute
  in
  let new_signature, new_index =
    let attribute =
      attribute
        definition
        ~transitive:true
        ~resolution
        ~name:(Access.create "__new__")
        ~instantiated:class_annotation
    in
    let signature =
      attribute
      |> Attribute.annotation
      |> Annotation.annotation
    in
    signature, definition_index attribute
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
  | _ ->
      signature


let overrides definition ~resolution ~name =
  let find_override parent =
    let potential_override =
      attribute
        ~transitive:false
        ~class_attributes:true
        parent
        ~resolution
        ~name
        ~instantiated:(annotation parent ~resolution)
    in
    if Attribute.defined potential_override then
      annotation ~resolution definition
      |> (fun instantiated -> constraints ~target:parent definition ~resolution ~instantiated)
      |> (fun constraints -> Attribute.instantiate ~constraints potential_override)
      |> Option.some
    else
      None
  in
  superclasses definition ~resolution
  |> List.find_map ~f:find_override


let has_method ?transitive definition ~resolution ~name =
  attribute
    ?transitive
    definition
    ~resolution
    ~name
    ~instantiated:(annotation definition ~resolution)
  |> Attribute.annotation
  |> Annotation.annotation
  |> Type.is_callable


let inferred_callable_type definition ~resolution =
  let explicit_callables =
    let extract_callable { Method.define = ({ Define.name; _ } as define); _ } =
      match List.last name with
      | Some (Access.Identifier call) when Identifier.equal call (Identifier.create "__call__") ->
          Some define
      | _ ->
          None
    in
    methods definition ~resolution
    |> List.filter_map ~f:extract_callable
  in
  if List.is_empty explicit_callables then
    None
  else
    let parent = annotation definition ~resolution in
    let callable = Callable.create ~parent:(Some parent) explicit_callables ~resolution in
    Some (Type.Callable callable)
