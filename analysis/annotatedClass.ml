(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Expression
open Pyre
open Statement
module Callable = AnnotatedCallable
module Attribute = AnnotatedAttribute

type t = Class.t Node.t [@@deriving compare, eq, sexp, show, hash]

type decorator = {
  name: string;
  arguments: Expression.Call.Argument.t list option;
}
[@@deriving compare, eq, sexp, show, hash]

module AttributeCache = struct
  type t = {
    transitive: bool;
    class_attributes: bool;
    include_generated_attributes: bool;
    special_method: bool;
    name: Reference.t;
    instantiated: Type.t;
  }
  [@@deriving compare, sexp, hash]

  include Hashable.Make (struct
    type nonrec t = t

    let compare = compare

    let hash = Hashtbl.hash

    let hash_fold_t = hash_fold_t

    let sexp_of_t = sexp_of_t

    let t_of_sexp = t_of_sexp
  end)

  let cache : Attribute.Table.t Table.t = Table.create ~size:1023 ()

  let clear () = Table.clear cache
end

type class_data = {
  instantiated: Type.t;
  class_attributes: bool;
  class_definition: t;
}

let name_equal
    { Node.value = { Class.name = left; _ }; _ }
    { Node.value = { Class.name = right; _ }; _ }
  =
  Reference.equal left right


let create definition = definition

let name { Node.value = { Class.name; _ }; _ } = name

let bases { Node.value = { Class.bases; _ }; _ } = bases

let get_decorator { Node.value = { Class.decorators; _ }; _ } ~resolution ~decorator =
  let matches target decorator =
    let name_resolves_to_target ~name =
      let name =
        GlobalResolution.resolve_exports resolution ~reference:(Reference.create name)
        |> Reference.show
      in
      String.equal name target
    in
    match decorator with
    | { Node.value = Call { callee; arguments }; _ }
      when name_resolves_to_target ~name:(Expression.show callee) ->
        Some { name = target; arguments = Some arguments }
    | { Node.value = Name _; _ } when name_resolves_to_target ~name:(Expression.show decorator) ->
        Some { name = target; arguments = None }
    | _ -> None
  in
  List.filter_map ~f:(matches decorator) decorators


let annotation { Node.value = { Class.name; _ }; _ } = Type.Primitive (Reference.show name)

let successors { Node.value = { Class.name; _ }; _ } ~resolution =
  Type.Primitive (Reference.show name)
  |> GlobalResolution.class_metadata resolution
  >>| (fun { GlobalResolution.successors; _ } -> successors)
  |> Option.value ~default:[]


let successors_fold class_node ~resolution ~f ~initial =
  successors class_node ~resolution |> List.fold ~init:initial ~f


let is_unit_test { Node.value; _ } = Class.is_unit_test value

let is_abstract { Node.value; _ } = Statement.Class.is_abstract value

let resolve_class ~resolution annotation =
  let rec extract ~is_meta original_annotation =
    let annotation =
      match original_annotation with
      | Type.Variable variable -> Type.Variable.Unary.upper_bound variable
      | _ -> original_annotation
    in
    match annotation with
    | Type.Top
    | Type.Bottom
    | Type.Any ->
        Some []
    | Type.Union annotations ->
        let flatten_optional sofar optional =
          match sofar, optional with
          | Some sofar, Some optional -> Some (optional :: sofar)
          | _ -> None
        in
        List.map ~f:(extract ~is_meta) annotations
        |> List.fold ~init:(Some []) ~f:flatten_optional
        >>| List.concat
        >>| List.rev
    | annotation when Type.is_meta annotation ->
        Type.single_parameter annotation |> extract ~is_meta:true
    | _ -> (
      match GlobalResolution.class_definition resolution annotation with
      | Some class_definition ->
          Some
            [ {
                instantiated = original_annotation;
                class_attributes = is_meta;
                class_definition = create class_definition;
              } ]
      | None -> None )
  in
  extract ~is_meta:false annotation


module Method = struct
  type t = {
    define: Define.t;
    parent: Type.t;
  }
  [@@deriving compare, eq, sexp, show, hash]

  let create ~define ~parent = { define; parent }

  let name { define; _ } = Define.unqualified_name define

  let define { define; _ } = define

  let parent { parent; _ } = parent

  let parameter_annotations { define = { Define.signature = { parameters; _ }; _ }; _ } ~resolution
    =
    let element { Node.value = { Parameter.name; annotation; _ }; _ } =
      let annotation =
        annotation
        >>| (fun annotation -> GlobalResolution.parse_annotation resolution annotation)
        |> Option.value ~default:Type.Top
      in
      name, annotation
    in
    List.map parameters ~f:element


  let return_annotation
      { define = { Define.signature = { return_annotation; async; _ }; _ } as define; _ }
      ~resolution
    =
    let annotation =
      Option.value_map
        return_annotation
        ~f:(GlobalResolution.parse_annotation resolution)
        ~default:Type.Top
    in
    if async then
      Type.awaitable annotation
    else if Define.is_coroutine define then
      match annotation with
      | Type.Parametric { name; parameters = Concrete [_; _; return_annotation] }
        when String.equal name "typing.Generator" ->
          Type.awaitable return_annotation
      | _ -> Type.Top
    else
      annotation
end

let find_propagated_type_variables bases ~resolution =
  let find_type_variables { Expression.Call.Argument.value; _ } =
    GlobalResolution.parse_annotation ~allow_invalid_type_parameters:true resolution value
    |> Type.Variable.all_free_variables
  in
  let handle_deduplicated = function
    | [Type.Variable.ListVariadic variable] -> Type.Variable.Variadic.List.self_reference variable
    | deduplicated ->
        let to_unary = function
          | Type.Variable.Unary variable -> Some (Type.Variable variable)
          | _ -> None
        in
        List.map deduplicated ~f:to_unary
        |> Option.all
        |> Option.value ~default:[]
        |> fun concrete -> Type.OrderedTypes.Concrete concrete
  in
  List.concat_map ~f:find_type_variables bases
  |> List.dedup ~compare:Type.Variable.compare
  |> handle_deduplicated


let generics { Node.value = { Class.bases; _ }; _ } ~resolution =
  let generic { Expression.Call.Argument.value; _ } =
    let annotation =
      GlobalResolution.parse_annotation ~allow_invalid_type_parameters:true resolution value
    in
    match annotation with
    | Type.Parametric { name = "typing.Generic"; parameters } -> Some parameters
    | Type.Parametric { name = "typing.Protocol"; parameters } -> Some parameters
    | _ -> None
  in
  match List.find_map ~f:generic bases with
  | None -> find_propagated_type_variables bases ~resolution
  | Some parameters -> parameters


let inferred_generic_base { Node.value = { Class.bases; _ }; _ } ~resolution =
  let is_generic { Expression.Call.Argument.value; _ } =
    let primitive, _ =
      GlobalResolution.parse_annotation ~allow_invalid_type_parameters:true resolution value
      |> Type.split
    in
    Type.is_generic_primitive primitive
  in
  if List.exists ~f:is_generic bases then
    []
  else
    let variables = find_propagated_type_variables bases ~resolution in
    if Type.OrderedTypes.equal variables (Concrete []) then
      []
    else
      [ {
          Expression.Call.Argument.name = None;
          value = Type.parametric "typing.Generic" variables |> Type.expression;
        } ]


let constraints ?target ?parameters definition ~instantiated ~resolution =
  let target = Option.value ~default:definition target in
  let parameters =
    match parameters with
    | None -> generics ~resolution target
    | Some parameters -> parameters
  in
  let right =
    let target = annotation target in
    match target with
    | Primitive name -> Type.parametric name parameters
    | _ -> target
  in
  match instantiated, right with
  | Type.Primitive name, Parametric { name = right_name; _ } when String.equal name right_name ->
      (* TODO(T42259381) This special case is only necessary because constructor calls attributes
         with an "instantiated" type of a bare parametric, which will fill with Anys *)
      TypeConstraints.Solution.empty
  | _ ->
      GlobalResolution.solve_less_or_equal
        resolution
        ~constraints:TypeConstraints.empty
        ~left:instantiated
        ~right
      |> List.filter_map ~f:(GlobalResolution.solve_constraints resolution)
      |> List.hd
      (* TODO(T39598018): error in this case somehow, something must be wrong *)
      |> Option.value ~default:TypeConstraints.Solution.empty


let superclasses definition ~resolution =
  successors ~resolution definition
  |> List.filter_map ~f:(fun name ->
         GlobalResolution.class_definition resolution (Type.Primitive name))
  |> List.map ~f:create


let unimplemented_abstract_methods definition ~resolution =
  let gather_abstract_methods { Node.value = class_definition; _ } sofar =
    let abstract_methods, base_methods =
      class_definition
      |> Statement.Class.defines
      |> List.partition_tf ~f:Statement.Define.is_abstract_method
    in
    let add_to_map sofar definition =
      let name = Statement.Define.unqualified_name definition in
      match String.Map.add sofar ~key:name ~data:definition with
      | `Ok map -> map
      | `Duplicate -> sofar
    in
    let sofar =
      if Statement.Class.is_abstract class_definition then
        abstract_methods
        |> List.filter ~f:(fun method_definition ->
               not (Statement.Define.is_property method_definition))
        |> List.fold ~init:sofar ~f:add_to_map
      else
        sofar
    in
    base_methods
    |> List.map ~f:Statement.Define.unqualified_name
    |> List.fold ~init:sofar ~f:String.Map.remove
  in
  let successors = definition |> superclasses ~resolution in
  if List.exists successors ~f:(fun { Node.value; _ } -> Statement.Class.is_abstract value) then
    successors
    |> List.cons definition
    |> List.fold_right ~init:String.Map.empty ~f:gather_abstract_methods
    |> Map.data
  else
    []


let rec metaclass ({ Node.value = { Class.bases; _ }; _ } as original) ~resolution =
  (* See https://docs.python.org/3/reference/datamodel.html#determining-the-appropriate-metaclass
     for why we need to consider all metaclasses. *)
  let metaclass_candidates =
    let explicit_metaclass =
      let find_explicit_metaclass = function
        | { Expression.Call.Argument.name = Some { Node.value = "metaclass"; _ }; value } ->
            Some (GlobalResolution.parse_annotation resolution value)
        | _ -> None
      in
      List.find_map ~f:find_explicit_metaclass bases
    in
    let metaclass_of_bases =
      let explicit_bases =
        let base_to_class { Call.Argument.value; _ } =
          Expression.delocalize value
          |> GlobalResolution.parse_annotation resolution
          |> Type.split
          |> fst
        in
        List.filter
          ~f:(function
            | { Expression.Call.Argument.name = None; _ } -> true
            | _ -> false)
          bases
        |> List.map ~f:base_to_class
        |> List.filter_map ~f:(GlobalResolution.class_definition resolution)
        |> List.filter ~f:(fun base_class -> not (equal base_class original))
      in
      let filter_generic_meta base_metaclasses =
        (* We only want a class directly inheriting from Generic to have a metaclass of
           GenericMeta. *)
        if
          List.exists
            ~f:(fun base -> Reference.equal (Reference.create "typing.Generic") (name base))
            explicit_bases
        then
          base_metaclasses
        else
          List.filter
            ~f:(fun metaclass -> not (Type.equal (Type.Primitive "typing.GenericMeta") metaclass))
            base_metaclasses
      in
      explicit_bases |> List.map ~f:(metaclass ~resolution) |> filter_generic_meta
    in
    match explicit_metaclass with
    | Some metaclass -> metaclass :: metaclass_of_bases
    | None -> metaclass_of_bases
  in
  match metaclass_candidates with
  | [] -> Type.Primitive "type"
  | first :: candidates -> (
      let candidate = List.fold candidates ~init:first ~f:(GlobalResolution.meet resolution) in
      match candidate with
      | Type.Bottom ->
          (* If we get Bottom here, we don't have a "most derived metaclass", so default to one. *)
          first
      | _ -> candidate )


let methods ({ Node.value = { Class.body; _ }; _ } as definition) =
  let extract_define = function
    | { Node.value = Define define; _ } ->
        Some (Method.create ~define ~parent:(annotation definition))
    | _ -> None
  in
  List.filter_map ~f:extract_define body


let has_abstract_methods { Node.value; _ } =
  not
    ( value
    |> Statement.Class.defines
    |> List.filter ~f:Statement.Define.is_abstract_method
    |> List.is_empty )


let is_protocol { Node.value; _ } = Statement.Class.is_protocol value

let create_attribute
    ~resolution
    ~parent
    ?instantiated
    ?(defined = true)
    ?(inherited = false)
    ?(default_class_attribute = false)
    {
      Node.location;
      value =
        {
          Statement.Attribute.name = attribute_name;
          annotation = attribute_annotation;
          defines;
          value;
          async;
          setter;
          property;
          primitive;
          toplevel;
          final;
          static;
          frozen;
          implicit;
        };
    }
  =
  let class_annotation = annotation parent in
  let initialized =
    match value with
    | Some { Node.value = Ellipsis; _ }
    | None ->
        false
    | _ -> true
  in
  let parsed_annotation = attribute_annotation >>| GlobalResolution.parse_annotation resolution in
  (* Account for class attributes. *)
  let annotation, class_attribute =
    parsed_annotation
    >>| (fun annotation ->
          let annotation_value =
            if Type.is_final annotation then
              Type.final_value annotation
            else
              Type.class_variable_value annotation
          in
          match annotation_value with
          | Some annotation -> Some annotation, true
          | _ -> Some annotation, false)
    |> Option.value ~default:(None, default_class_attribute)
  in
  let final =
    let is_final_annotation = parsed_annotation >>| Type.is_final |> Option.value ~default:false in
    final || is_final_annotation
  in
  (* Handle enumeration attributes. *)
  let annotation, value, class_attribute =
    let superclasses =
      superclasses ~resolution parent
      |> List.map ~f:(fun definition -> name definition |> Reference.show)
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
  (* Handle Callables *)
  let annotation =
    let instantiated =
      match instantiated with
      | Some instantiated -> instantiated
      | None -> class_annotation
    in
    match defines with
    | Some (({ Define.signature = { Define.name; _ }; _ } as define) :: _ as defines) ->
        let parent =
          (* TODO(T45029821): __new__ is special cased to be a static method. It doesn't play well
             with our logic here - we should clean up the call logic to handle passing the extra
             argument, and eliminate the special fields from here. *)
          if
            Define.is_static_method define
            && not (String.equal (Define.unqualified_name define) "__new__")
          then
            None
          else if Define.is_class_method define then
            Some (Type.meta instantiated)
          else if class_attribute then
            (* Keep first argument around when calling instance methods from class attributes. *)
            None
          else
            Some instantiated
        in
        let apply_decorators define =
          ( Define.is_overloaded_method define,
            Callable.apply_decorators ~resolution ~location define )
        in
        List.map defines ~f:apply_decorators
        |> Callable.create ~resolution ~parent ~name:(Reference.show name)
        |> fun callable -> Some (Type.Callable callable)
    | _ -> annotation
  in
  (* Special cases *)
  let annotation =
    match instantiated, attribute_name, annotation with
    | Some (Type.TypedDictionary { fields; total; _ }), method_name, Some (Type.Callable callable)
      ->
        Type.TypedDictionary.special_overloads ~fields ~method_name ~total
        >>| (fun overloads ->
              Some
                (Type.Callable
                   {
                     callable with
                     implementation =
                       { annotation = Type.Top; parameters = Undefined; define_location = None };
                     overloads;
                   }))
        |> Option.value ~default:annotation
    | ( Some (Type.Tuple (Bounded (Concrete members))),
        "__getitem__",
        Some (Type.Callable ({ overloads; _ } as callable)) ) ->
        let overload index member =
          {
            Type.Callable.annotation = member;
            parameters =
              Defined
                [Named { name = "x"; annotation = Type.literal_integer index; default = false }];
            define_location = None;
          }
        in
        let overloads = List.mapi ~f:overload members @ overloads in
        Some (Type.Callable { callable with overloads })
    | ( Some (Parametric { name = "type"; parameters = Concrete [Type.Primitive name] }),
        "__getitem__",
        Some (Type.Callable ({ kind = Named callable_name; _ } as callable)) )
      when String.equal (Reference.show callable_name) "typing.GenericMeta.__getitem__" ->
        let implementation =
          let generics =
            GlobalResolution.class_definition resolution (Type.Primitive name)
            >>| create
            >>| generics ~resolution
            |> Option.value ~default:(Type.OrderedTypes.Concrete [])
          in
          match generics with
          | Concrete generics ->
              let parameters =
                let create_parameter annotation =
                  Type.Callable.Parameter.Anonymous { index = 0; annotation; default = false }
                in
                match generics with
                | [] -> []
                | [generic] -> [create_parameter (Type.meta generic)]
                | generics -> [create_parameter (Type.tuple (List.map ~f:Type.meta generics))]
              in
              {
                Type.Callable.annotation =
                  Type.meta (Type.Parametric { name; parameters = Concrete generics });
                parameters = Defined parameters;
                define_location = None;
              }
          | _ ->
              (* TODO(T47347970): make this a *args: Ts -> X[Ts] for that case, and ignore the
                 others *)
              {
                Type.Callable.annotation =
                  Type.meta (Type.Parametric { name; parameters = generics });
                parameters = Undefined;
                define_location = None;
              }
        in
        Some (Type.Callable { callable with implementation; overloads = [] })
    | _ -> annotation
  in
  let annotation =
    match annotation, value with
    | Some annotation, Some value ->
        Annotation.create_immutable
          ~global:true
          ~final
          ~original:(Some annotation)
          ( if setter then
              GlobalResolution.parse_annotation resolution value
          else
            annotation )
    | Some annotation, None -> Annotation.create_immutable ~global:true ~final annotation
    | None, Some value ->
        let literal_value_annotation =
          if setter then
            GlobalResolution.parse_annotation resolution value
          else
            GlobalResolution.resolve_literal resolution value
        in
        let is_dataclass_attribute =
          let get_dataclass_decorator annotated =
            get_decorator annotated ~resolution ~decorator:"dataclasses.dataclass"
            @ get_decorator annotated ~resolution ~decorator:"dataclass"
          in
          not (List.is_empty (get_dataclass_decorator parent))
        in
        if
          (not (Type.is_partially_typed literal_value_annotation))
          && (not is_dataclass_attribute)
          && toplevel
        then (* Treat literal attributes as having been explicitly annotated. *)
          Annotation.create_immutable ~global:true ~final literal_value_annotation
        else
          Annotation.create_immutable
            ~global:true
            ~final
            ~original:(Some Type.Top)
            (GlobalResolution.parse_annotation resolution value)
    | _ -> Annotation.create_immutable ~global:true ~final Type.Top
  in
  (* Special case properties with type variables. *)
  (* TODO(T44676629): handle this correctly *)
  let annotation =
    let free_variables =
      let variables =
        Annotation.annotation annotation
        |> Type.Variable.all_free_variables
        |> List.filter_map ~f:(function
               | Type.Variable.Unary variable -> Some (Type.Variable variable)
               | _ -> None)
        |> Type.Set.of_list
      in
      let generics =
        match generics parent ~resolution with
        | Concrete generics -> Type.Set.of_list generics
        | _ ->
            (* TODO(T44676629): This case should be handled when we re-do this handling *)
            Type.Set.empty
      in
      Set.diff variables generics |> Set.to_list
    in
    if property && not (List.is_empty free_variables) then
      let constraints =
        let instantiated = Option.value instantiated ~default:class_annotation in
        List.fold free_variables ~init:Type.Map.empty ~f:(fun map variable ->
            Map.set map ~key:variable ~data:instantiated)
        |> Map.find
      in
      Annotation.annotation annotation
      |> Type.instantiate ~constraints
      |> Annotation.create_immutable ~global:true ~final
    else
      annotation
  in
  (* We need to distinguish between unannotated attributes and non-existent ones - ensure that the
     annotation is viewed as mutable to distinguish from user-defined globals. *)
  let annotation =
    if not defined then
      { annotation with Annotation.mutability = Annotation.Mutable }
    else
      annotation
  in
  let value = Option.value value ~default:(Node.create Ellipsis ~location) in
  let property =
    match property, setter with
    | true, true when not frozen -> Some Attribute.ReadWrite
    | true, false -> Some Attribute.ReadOnly
    | _, _ when frozen -> Some Attribute.ReadOnly
    | _, _ -> None
  in
  {
    Node.location;
    value =
      {
        Attribute.annotation;
        async;
        class_attribute;
        defined;
        initialized;
        final;
        name = attribute_name;
        parent = class_annotation;
        property;
        static;
        value;
      };
  }


let extends_placeholder_stub_class
    { Node.value = { Class.bases; _ }; _ }
    ~aliases
    ~module_definition
  =
  let is_from_placeholder_stub { Expression.Call.Argument.value; _ } =
    let value = Expression.delocalize value in
    let parsed = Type.create ~aliases value in
    match parsed with
    | Type.Primitive primitive
    | Parametric { name = primitive; _ } ->
        Reference.create primitive
        |> fun reference -> Module.from_empty_stub ~reference ~module_definition
    | _ -> false
  in
  List.exists bases ~f:is_from_placeholder_stub


let implicit_attributes { Node.value; _ } = Statement.Class.implicit_attributes value

let attribute_table
    ~transitive
    ~class_attributes
    ~include_generated_attributes
    ?(special_method = false)
    ?instantiated
    ({ Node.value = { Class.name; _ }; _ } as definition)
    ~resolution
  =
  let original_instantiated = instantiated in
  let instantiated = Option.value instantiated ~default:(annotation definition) in
  let key =
    {
      AttributeCache.transitive;
      class_attributes;
      special_method;
      include_generated_attributes;
      name;
      instantiated;
    }
  in
  match Hashtbl.find AttributeCache.cache key with
  | Some result -> result
  | None ->
      let definition_attributes
          ~in_test
          ~instantiated
          ~class_attributes
          ~table
          ({ Node.value = { Class.name = parent_name; _ } as definition; _ } as parent)
        =
        let add_actual () =
          let collect_attributes attribute =
            create_attribute
              attribute
              ~resolution
              ~parent
              ~instantiated
              ~inherited:(not (Reference.equal name parent_name))
              ~default_class_attribute:class_attributes
            |> Attribute.Table.add table
          in
          Statement.Class.attributes ~include_generated_attributes ~in_test definition
          |> fun attribute_map ->
          Identifier.SerializableMap.iter (fun _ data -> collect_attributes data) attribute_map
        in
        let add_placeholder_stub_inheritances () =
          if Option.is_none (Attribute.Table.lookup_name table "__init__") then
            Attribute.Table.add
              table
              (Node.create_with_default_location
                 {
                   Attribute.annotation =
                     Annotation.create (Type.Callable.create ~annotation:Type.none ());
                   async = false;
                   class_attribute = false;
                   defined = true;
                   final = false;
                   initialized = true;
                   name = "__init__";
                   parent = Primitive (Reference.show name);
                   property = None;
                   static = true;
                   value = Node.create_with_default_location Ellipsis;
                 });
          if Option.is_none (Attribute.Table.lookup_name table "__getattr__") then
            Attribute.Table.add
              table
              (Node.create_with_default_location
                 {
                   Attribute.annotation =
                     Annotation.create (Type.Callable.create ~annotation:Type.Any ());
                   async = false;
                   class_attribute = false;
                   defined = true;
                   final = false;
                   initialized = true;
                   name = "__getattr__";
                   parent = Primitive (Reference.show name);
                   property = None;
                   static = true;
                   value = Node.create_with_default_location Ellipsis;
                 })
        in
        add_actual ();
        if
          extends_placeholder_stub_class
            parent
            ~aliases:(GlobalResolution.aliases resolution)
            ~module_definition:(GlobalResolution.module_definition resolution)
        then
          add_placeholder_stub_inheritances ()
      in
      let superclass_definitions = superclasses ~resolution definition in
      let in_test =
        List.exists (definition :: superclass_definitions) ~f:(fun { Node.value; _ } ->
            Class.is_unit_test value)
      in
      let table = Attribute.Table.create () in
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
        ~f:(definition_attributes ~in_test ~instantiated ~class_attributes ~table);

      (* Class over meta hierarchy if necessary. *)
      let meta_definitions =
        if class_attributes then
          metaclass ~resolution definition
          |> GlobalResolution.class_definition resolution
          >>| (fun definition -> definition :: superclasses ~resolution definition)
          |> Option.value ~default:[]
        else
          []
      in
      List.iter
        meta_definitions
        ~f:
          (definition_attributes
             ~in_test
             ~instantiated:(Type.meta instantiated)
             ~class_attributes:false
             ~table);
      let instantiate ~instantiated attribute =
        Attribute.parent attribute
        |> GlobalResolution.class_definition resolution
        >>| fun target ->
        let solution = constraints ~target ~instantiated ~resolution definition in
        Attribute.instantiate
          ~constraints:(fun annotation ->
            Some (TypeConstraints.Solution.instantiate solution annotation))
          attribute
      in
      Option.iter original_instantiated ~f:(fun instantiated ->
          Attribute.Table.filter_map table ~f:(instantiate ~instantiated));
      Hashtbl.set ~key ~data:table AttributeCache.cache;
      table


let attributes
    ?(transitive = false)
    ?(class_attributes = false)
    ?(include_generated_attributes = true)
    ?instantiated
    definition
    ~resolution
  =
  attribute_table
    ~transitive
    ~class_attributes
    ~include_generated_attributes
    ?instantiated
    definition
    ~resolution
  |> Attribute.Table.to_list


let attribute_fold
    ?(transitive = false)
    ?(class_attributes = false)
    ?(include_generated_attributes = true)
    definition
    ~initial
    ~f
    ~resolution
  =
  attributes ~transitive ~class_attributes ~include_generated_attributes ~resolution definition
  |> List.fold ~init:initial ~f


let attribute
    ?(transitive = false)
    ?(class_attributes = false)
    ?(special_method = false)
    ({ Node.location; _ } as definition)
    ~resolution
    ~name
    ~instantiated
  =
  let table =
    attribute_table
      ~instantiated
      ~transitive
      ~class_attributes
      ~special_method
      ~include_generated_attributes:true
      ~resolution
      definition
  in
  match Attribute.Table.lookup_name table name with
  | Some attribute -> attribute
  | None ->
      create_attribute
        ~resolution
        ~parent:definition
        ~defined:false
        ~default_class_attribute:class_attributes
        {
          Node.location;
          value =
            {
              Statement.Attribute.name;
              annotation = None;
              defines = None;
              value = None;
              async = false;
              setter = false;
              property = false;
              primitive = true;
              toplevel = true;
              final = false;
              static = false;
              frozen = false;
              implicit = false;
            };
        }


let rec fallback_attribute
    ~(resolution : Resolution.t)
    ~name
    ({ Node.value = { Class.name = class_name; _ }; _ } as definition)
  =
  let compound_backup =
    let name =
      match name with
      | "__iadd__" -> Some "__add__"
      | "__isub__" -> Some "__sub__"
      | "__imul__" -> Some "__mul__"
      | "__imatmul__" -> Some "__matmul__"
      | "__itruediv__" -> Some "__truediv__"
      | "__ifloordiv__" -> Some "__floordiv__"
      | "__imod__" -> Some "__mod__"
      | "__idivmod__" -> Some "__divmod__"
      | "__ipow__" -> Some "__pow__"
      | "__ilshift__" -> Some "__lshift__"
      | "__irshift__" -> Some "__rshift__"
      | "__iand__" -> Some "__and__"
      | "__ixor__" -> Some "__xor__"
      | "__ior__" -> Some "__or__"
      | _ -> None
    in
    match name with
    | Some name ->
        attribute
          definition
          ~class_attributes:false
          ~transitive:true
          ~resolution:(Resolution.global_resolution resolution)
          ~name
          ~instantiated:(annotation definition)
        |> Option.some
    | _ -> None
  in
  let getitem_backup () =
    let fallback =
      attribute
        definition
        ~class_attributes:true
        ~transitive:true
        ~resolution:(Resolution.global_resolution resolution)
        ~name:"__getattr__"
        ~instantiated:(annotation definition)
    in
    if Attribute.defined fallback then
      let annotation = fallback |> Attribute.annotation |> Annotation.annotation in
      match annotation with
      | Type.Callable ({ implementation; _ } as callable) ->
          let location = Attribute.location fallback in
          let arguments =
            let self_argument =
              { Call.Argument.name = None; value = Expression.from_reference ~location class_name }
            in
            let name_argument =
              {
                Call.Argument.name = None;
                value = { Node.location; value = Expression.String (StringLiteral.create name) };
              }
            in
            [self_argument; name_argument]
          in
          let implementation =
            match AnnotatedSignature.select ~resolution ~arguments ~callable with
            | AnnotatedSignature.Found { Type.Callable.implementation; _ } -> implementation
            | AnnotatedSignature.NotFound _ -> implementation
          in
          let return_annotation = Type.Callable.Overload.return_annotation implementation in
          Some
            (create_attribute
               ~resolution:(Resolution.global_resolution resolution)
               ~parent:definition
               {
                 Node.location;
                 value =
                   {
                     Statement.Attribute.name;
                     annotation = Some (Type.expression return_annotation);
                     defines = None;
                     value = None;
                     async = false;
                     setter = false;
                     property = false;
                     primitive = true;
                     toplevel = true;
                     final = false;
                     static = false;
                     frozen = false;
                     implicit = false;
                   };
               })
      | _ -> None
    else
      None
  in
  match compound_backup with
  | Some backup when Attribute.defined backup -> Some backup
  | _ -> getitem_backup ()


let constructor definition ~instantiated ~resolution =
  let return_annotation =
    let class_annotation = annotation definition in
    match class_annotation with
    | Type.Primitive name
    | Type.Parametric { name; _ } -> (
        let generics = generics definition ~resolution in
        (* Tuples are special. *)
        if String.equal name "tuple" then
          match generics with
          | Concrete [tuple_variable] -> Type.Tuple (Type.Unbounded tuple_variable)
          | _ -> Type.Tuple (Type.Unbounded Type.Any)
        else
          let backup = Type.Parametric { name; parameters = generics } in
          match instantiated, generics with
          | _, Concrete [] -> instantiated
          | Type.Primitive instantiated_name, _ when String.equal instantiated_name name -> backup
          | ( Type.Parametric { parameters = Concrete parameters; name = instantiated_name },
              Concrete generics )
            when String.equal instantiated_name name
                 && List.length parameters <> List.length generics ->
              backup
          | _ -> instantiated )
    | _ -> instantiated
  in
  let definitions =
    definition :: superclasses ~resolution definition
    |> List.map ~f:(fun definition -> annotation definition)
  in
  let definition_index attribute =
    attribute
    |> Attribute.parent
    |> (fun class_annotation ->
         List.findi definitions ~f:(fun _ annotation -> Type.equal annotation class_annotation))
    >>| fst
    |> Option.value ~default:Int.max_value
  in
  let constructor_signature, constructor_index =
    let attribute =
      attribute definition ~transitive:true ~resolution ~name:"__init__" ~instantiated
    in
    let signature = attribute |> Attribute.annotation |> Annotation.annotation in
    signature, definition_index attribute
  in
  let new_signature, new_index =
    let attribute =
      attribute definition ~transitive:true ~resolution ~name:"__new__" ~instantiated
    in
    let signature = attribute |> Attribute.annotation |> Annotation.annotation in
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
  | _ -> signature


let constructors definition ~resolution =
  let in_test =
    let superclasses = superclasses ~resolution definition in
    List.exists ~f:is_unit_test (definition :: superclasses)
  in
  Class.constructors ~in_test (Node.value definition)


let overrides definition ~resolution ~name =
  let find_override parent =
    let potential_override =
      attribute
        ~transitive:false
        ~class_attributes:true
        parent
        ~resolution
        ~name
        ~instantiated:(annotation parent)
    in
    if Attribute.defined potential_override then
      annotation definition
      |> (fun instantiated -> constraints ~target:parent definition ~resolution ~instantiated)
      |> (fun solution ->
           Attribute.instantiate
             ~constraints:(fun annotation ->
               Some (TypeConstraints.Solution.instantiate solution annotation))
             potential_override)
      |> Option.some
    else
      None
  in
  superclasses definition ~resolution |> List.find_map ~f:find_override


let has_method ?transitive definition ~resolution ~name =
  attribute ?transitive definition ~resolution ~name ~instantiated:(annotation definition)
  |> Attribute.annotation
  |> Annotation.annotation
  |> Type.is_callable


let inferred_callable_type definition ~resolution =
  let explicit_callables =
    let extract_callable { Method.define = { Define.signature = { name; _ }; _ } as define; _ } =
      Option.some_if (Reference.is_suffix ~suffix:(Reference.create "__call__") name) define
      >>| fun define ->
      ( Reference.show name,
        Define.is_overloaded_method define,
        Callable.create_overload define ~resolution )
    in
    methods definition |> List.filter_map ~f:extract_callable
  in
  if List.is_empty explicit_callables then
    None
  else
    let parent = annotation definition in
    let name, _, _ = List.hd_exn explicit_callables in
    let explicit_callables =
      List.map explicit_callables ~f:(fun (_, is_overload, callable) -> is_overload, callable)
    in
    let callable = Callable.create ~resolution ~parent:(Some parent) ~name explicit_callables in
    Some callable
