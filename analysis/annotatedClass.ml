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
  Reference.equal left right


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
    | { Node.value = Access (SimpleAccess access); _ } ->
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


let annotation { Node.value = { Class.name; _ }; _ } ~resolution =
  Resolution.parse_reference resolution name


let successors class_node ~resolution =
  annotation class_node ~resolution
  |> Type.split
  |> (fun (primitive, _ ) -> primitive)
  |> Resolution.class_metadata resolution
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


  let name { define; _ } =
    Define.unqualified_name define


  let define { define; _ } =
    define


  let parent { parent; _ } =
    parent


  let parameter_annotations
      { define = { Define.signature = { parameters; _ }; _ }; _ }
      ~resolution =
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


  let parameter_annotations_positional
      { define = { Define.signature = { parameters; _ }; _ }; _ }
      ~resolution =
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
      { define = { Define.signature = { return_annotation; async; _ }; _ } as define; _ }
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
          when name = "typing.Generator" ->
            Type.awaitable return_annotation
        | _ ->
            Type.Top
      end
    else
      annotation
end


let find_propagated_type_variables bases ~resolution =
  let find_type_variables { Argument.value; _ } =
    Resolution.parse_annotation ~allow_invalid_type_parameters:true resolution value
    |> Type.free_variables
    |> List.map ~f:(fun variable -> Type.Variable variable)
  in
  List.concat_map ~f:find_type_variables bases
  |> List.dedup ~compare:Type.compare


let generics { Node.value = { Class.bases; _ }; _ } ~resolution =
  let generic { Argument.value; _ } =
    let annotation =
      Resolution.parse_annotation ~allow_invalid_type_parameters:true resolution value
    in
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
  begin
    match List.find_map ~f:generic bases with
    | None -> find_propagated_type_variables bases ~resolution
    | Some parameters -> parameters
  end


let inferred_generic_base { Node.value = { Class.bases; _ }; _ } ~resolution =
  let is_generic { Argument.value; _ } =
    let primitive, _ =
      Resolution.parse_annotation ~allow_invalid_type_parameters:true resolution value
      |> Type.split
    in
    Type.equal primitive Type.generic
  in
  if List.exists ~f:is_generic bases then
    []
  else
    let variables = find_propagated_type_variables bases ~resolution in
    if List.is_empty variables then
      []
    else
      [{
        Argument.name = None;
        value =
          Type.parametric "typing.Generic" variables
          |> Type.expression;
      }]


let constraints ?target ?parameters definition ~instantiated ~resolution =
  let target = Option.value ~default:definition target in
  let parameters =
    match parameters with
    | None ->
        generics ~resolution target
    | Some parameters ->
        parameters
  in
  let right =
    let target =
      annotation ~resolution target
      |> Type.split
      |> fst
    in
    match target with
    | Primitive name ->
        Type.parametric name parameters
    | _ ->
        target
  in
  match instantiated, right with
  | Type.Primitive name, Parametric { name = right_name; _ } when name = right_name ->
      (* TODO(T42259381) This special case is only necessary because constructor calls attributes
         with an "instantiated" type of a bare parametric, which will fill with Anys *)
      Type.Map.empty
  | _ ->
      Resolution.solve_less_or_equal
        resolution
        ~constraints:TypeConstraints.empty
        ~left:instantiated
        ~right
      |> List.filter_map ~f:(Resolution.solve_constraints resolution)
      |> List.hd
      (* TODO(T39598018): error in this case somehow, something must be wrong *)
      |> Option.value ~default:Type.Map.empty


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
      | { Argument.name = Some { Node.value = "metaclass"; _ }; value } ->
          Some (Resolution.parse_annotation resolution value)
      | _ ->
          None
    in
    List.find_map ~f:get_metaclass bases
  in
  definition :: superclasses ~resolution definition
  |> List.find_map ~f:get_metaclass
  |> Option.value ~default:(Type.Primitive "type")


let methods ({ Node.value = { Class.body; _ }; _ } as definition) ~resolution =
  let extract_define = function
    | { Node.value = Define define; _ } ->
        Some (Method.create ~define ~parent:(annotation definition ~resolution))
    | _ ->
        None
  in
  List.filter_map ~f:extract_define body


let is_protocol { Node.value = { Class.bases; _ }; _ } =
  let is_protocol { Argument.name; value = { Node.value; _ } } =
    match name, value with
    | None, Access (SimpleAccess ((Identifier "typing") :: (Identifier "Protocol") :: _))
    | None,
      Access (SimpleAccess ((Identifier "typing_extensions") :: (Identifier "Protocol") :: _)) ->
        true
    | _ ->
        false
  in
  List.exists ~f:is_protocol bases


module Attribute = struct
  type attribute = {
    name: Identifier.t;
    parent: Type.t;
    annotation: Annotation.t;
    value: Expression.t;
    defined: bool;
    class_attribute: bool;
    async: bool;
    initialized: bool;
  }
  [@@deriving eq, show]

  type t = attribute Node.t
  [@@deriving eq, show]


  let create
      ~resolution
      ~parent
      ?instantiated
      ?(defined = true)
      ?(inherited = false)
      ?(default_class_attribute = false)
      {
        Node.location;
        value = {
          Attribute.name = attribute_name;
          annotation = attribute_annotation;
          defines;
          value;
          async;
          setter;
          property;
          primitive;
          toplevel;
        };
      } =
    let class_annotation = annotation parent ~resolution in
    let initialized =
      match value with
      | Some { Node.value = Ellipsis; _ }
      | None ->
          false
      | _ ->
          true
    in

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
        |> List.map ~f:(fun definition -> name definition |> Reference.show)
        |> String.Set.of_list
      in
      if not (Set.mem Recognized.enumeration_classes (Type.show class_annotation)) &&
         not (Set.is_empty (Set.inter Recognized.enumeration_classes superclasses)) &&
         not inherited &&
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
          let literal_value_annotation =
            if setter then
              Resolution.parse_annotation resolution value
            else
              Resolution.resolve_literal resolution value
          in
          let is_dataclass_attribute =
            let get_dataclass_decorator annotated =
              get_decorator annotated ~decorator:"dataclasses.dataclass"
              @ get_decorator annotated ~decorator:"dataclass"
            in
            not (List.is_empty (get_dataclass_decorator parent))
          in
          if not (Type.is_partially_typed literal_value_annotation) &&
             not is_dataclass_attribute &&
             toplevel
          then
            (* Treat literal attributes as having been explicitly annotated. *)
            Annotation.create_immutable
              ~global:true
              literal_value_annotation
          else
            Annotation.create_immutable
              ~global:true
              ~original:(Some Type.Top)
              (Resolution.parse_annotation resolution value)
      | _ ->
          Annotation.create Type.Top
    in

    (* Special case properties with type variables. *)
    let annotation =
      let free_variables =
        let variables =
          Annotation.annotation annotation
          |> Type.free_variables
          |> List.map ~f:(fun variable -> Type.Variable variable)
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
          let instantiated = Option.value instantiated ~default:class_annotation in
          List.fold
            free_variables
            ~init:Type.Map.empty
            ~f:(fun map variable -> Map.set map ~key:variable ~data:instantiated)
          |> Map.find
        in
        Annotation.annotation annotation
        |> Type.instantiate ~constraints
        |> Annotation.create_immutable ~global:true ~original:(Some Type.Top)
      else
        annotation
    in
    let target = Access ( SimpleAccess(Access.create attribute_name)) in

    (* Special cases *)
    let annotation =
      let open Expression in
      let open Record.Access in
      match instantiated, target, annotation with
      | Some (Type.TypedDictionary { fields; total; _ }),
        Access (SimpleAccess [Identifier method_name]),
        { annotation = Type.Callable callable; _ } ->
          Type.TypedDictionary.special_overloads ~fields ~method_name ~total
          >>| (fun overloads ->
              {
                annotation with
                annotation =
                  Type.Callable {
                    callable with
                    implementation = { annotation = Type.Top; parameters = Undefined };
                    overloads;
                  };
              })
          |> Option.value ~default:annotation
      | Some (Type.Tuple (Bounded members)),
        Access (SimpleAccess [Identifier "__getitem__"]),
        { annotation = Type.Callable ({ overloads; _ } as callable); _ } ->
          let overload index member =
            {
              Type.Callable.annotation = member;
              parameters = Defined [
                  Named { name = "x"; annotation = Type.literal_integer index; default = false };
                ];
            }
          in
          let overloads =  (List.mapi ~f:overload members) @ overloads in
          { annotation with annotation = Type.Callable { callable with overloads } }
      | Some (Type.Primitive name),
        Access (SimpleAccess [Identifier "__getitem__"]),
        { annotation = Type.Callable ({ kind = Named callable_name; _ } as callable); _ }
        when Reference.show callable_name = "typing.Generic.__getitem__" ->
          let implementation =
            let generics =
              Resolution.class_definition resolution (Type.Primitive name)
              >>| create
              >>| generics ~resolution
              |> Option.value ~default:[]
            in
            let parameters =
              let parameter generic =
                Type.Callable.Parameter.Named {
                  name = "$";
                  annotation = Type.meta generic;
                  default = false;
                }
              in
              List.map generics ~f:parameter
            in
            {
              Type.Callable.annotation =
                Type.meta (Type.Parametric { name; parameters = generics });
              parameters = Defined parameters;
            }
          in
          {
            annotation with
            annotation = Type.Callable { callable with implementation; overloads = [] }
          }
      | _ ->
          annotation
    in

    let value = Option.value value ~default:(Node.create Ellipsis ~location) in

    {
      Node.location;
      value = {
        name = attribute_name;
        parent = class_annotation;
        annotation;
        value;
        defined;
        class_attribute;
        async;
        initialized;
      };

    }


  let name { Node.value = { name; _ }; _ } =
    name


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


  let initialized { Node.value = { initialized; _ }; _ } =
    initialized


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
      name: Reference.t;
      instantiated: Type.t option;
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
    ?(instantiated = None)
    ({ Node.value = { Class.name; _ }; _ } as definition)
    ~resolution =
  let key =
    {
      Attribute.Cache.transitive;
      class_attributes;
      include_generated_attributes;
      name;
      instantiated;
    }
  in
  match Hashtbl.find Attribute.Cache.cache key with
  | Some result ->
      result
  | None ->
      let instantiated = Option.value instantiated ~default:(annotation definition ~resolution) in
      let definition_attributes
          ~in_test
          ~instantiated
          ~class_attributes
          attributes
          ({ Node.value = ({ Class.name = parent_name; _ } as definition); _ } as parent) =
        let collect_attributes attributes attribute =
          let attribute_node =
            Attribute.create
              attribute
              ~resolution
              ~parent
              ~instantiated
              ~inherited:(name <> parent_name)
              ~default_class_attribute:class_attributes
          in
          let existing_attribute =
            let compare_names existing_attribute =
              String.equal (Attribute.name attribute_node) (Attribute.name existing_attribute)
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
        Identifier.SerializableMap.fold
          (fun _ data attributes -> collect_attributes attributes data)
          attribute_map
          attributes
      in
      let superclass_definitions = superclasses ~resolution definition in
      let in_test =
        List.exists
          (definition :: superclass_definitions)
          ~f:(fun { Node.value; _ } -> Class.is_unit_test value)
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
          ~f:(definition_attributes ~in_test ~instantiated ~class_attributes)
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
          ~f:(definition_attributes
                ~in_test
                ~instantiated:(Type.meta instantiated)
                ~class_attributes:false)
          ~init:accumulator
          meta_definitions
        |> List.rev
      in
      Hashtbl.set ~key ~data:result Attribute.Cache.cache;
      result

let callables_of_attributes =
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
        (* We have to split the type here due to our built-in aliasing. Namely, the "list" and
           "dict" classes get expanded into parametric types of List[Any] and Dict[Any, Any]. *)
        let parent = fst (Type.split parent) in
        let local_name =
          Reference.drop_prefix callable_name ~prefix:(Reference.create (Type.show parent))
        in
        List.map ~f:(fun overload -> (local_name, overload)) (implementation :: overloads)
    | _ -> []
  in
  List.concat_map ~f:callables_of_attribute

let map_of_name_to_annotation_implements ~resolution all_instance_methods ~protocol =
  let overload_implements ~constraints (name, overload) (protocol_name, protocol_overload) =
    if Reference.equal name protocol_name then
      let left =
        Type.Callable.create_from_implementation overload
        |> Type.mark_variables_as_bound ~simulated:true
      in
      Resolution.solve_less_or_equal
        resolution
        ~left
        ~right:(Type.Callable.create_from_implementation protocol_overload)
        ~constraints
    else
      []
  in
  (* TODO(T40727281): This needs to be transitive once we're actually checking based on
       transitive *)
  let all_protocol_methods =
    attributes ~resolution ~transitive:true protocol
    |> List.filter ~f:(fun { Node.value = {Attribute.parent; _}; _} ->
        parent <> Type.object_primitive && parent <> Type.generic)
    |> callables_of_attributes
  in
  let rec implements ~constraints instance_methods protocol_methods =
    match instance_methods, protocol_methods with
    | _, [] ->
        [constraints]
    | [], _ :: _ ->
        []
    | instance_method :: instance_methods,
      ((protocol_method :: protocol_methods) as old_protocol_methods) ->
        match overload_implements ~constraints instance_method protocol_method with
        | [] ->
            implements ~constraints instance_methods old_protocol_methods
        | constraints_set ->
            List.concat_map constraints_set ~f:(fun constraints ->
                implements ~constraints all_instance_methods protocol_methods)
  in
  let instantiate_protocol_generics solution =
    let generics = generics ~resolution protocol in
    let solution = Type.default_to_bottom solution generics in
    List.map generics ~f:(Type.instantiate ~constraints:(Type.Map.find solution))
  in
  implements ~constraints:TypeConstraints.empty all_instance_methods all_protocol_methods
  |> List.filter_map ~f:(Resolution.solve_constraints resolution)
  |> List.hd
  >>| Type.Map.map ~f:Type.free_simulated_bound_variables
  >>| instantiate_protocol_generics
  >>| (fun parameters -> TypeOrder.Implements { parameters })
  |> Option.value ~default:TypeOrder.DoesNotImplement


let callable_implements ~resolution { Type.Callable.implementation; overloads; _ } ~protocol =
  List.map (implementation :: overloads) ~f:(fun overload -> (Reference.create "__call__", overload))
  |> map_of_name_to_annotation_implements ~resolution ~protocol


let implements ~resolution definition ~protocol =
  callables_of_attributes (attributes ~resolution definition)
  |> map_of_name_to_annotation_implements ~resolution ~protocol


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
          Statement.Attribute.name;
          annotation = None;
          defines = None;
          value = None;
          async = false;
          setter = false;
          property = false;
          primitive = true;
          toplevel = true;
        }
      }
  in
  attributes
    ~instantiated:(Some instantiated)
    ~transitive
    ~class_attributes
    ~include_generated_attributes:true
    ~resolution
    definition
  |> List.find
    ~f:(fun attribute -> String.equal name (Attribute.name attribute))
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


let rec fallback_attribute ~resolution ~name definition =
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
          ~resolution
          ~name
          ~instantiated:(annotation definition ~resolution)
        |> Option.some
    | _ ->
        None
  in
  let getitem_backup () =
    let fallback =
      attribute
        definition
        ~class_attributes:true
        ~transitive:true
        ~resolution
        ~name:"__getattr__"
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
                     Statement.Attribute.name;
                     annotation = Some (Type.expression return_annotation);
                     defines = None;
                     value = None;
                     async = false;
                     setter = false;
                     property = false;
                     primitive = true;
                     toplevel = true;
                   };
                 })
        | _ ->
            None
      end
    else
      None
  in
  match compound_backup with
  | Some backup when Attribute.defined backup -> Some backup
  | _ -> getitem_backup ()


let constructor definition ~instantiated ~resolution =
  let return_annotation =
    let class_annotation = annotation definition ~resolution in
    match class_annotation with
    | Type.Primitive name
    | Type.Parametric { name; _ } ->
        let generics = generics definition ~resolution in
        (* Tuples are special. *)
        if name = "tuple" then
          match generics with
          | [tuple_variable] ->
              Type.Tuple (Type.Unbounded tuple_variable)
          | _ ->
              Type.Tuple (Type.Unbounded Type.Any)
        else
          begin
            let backup = Type.Parametric { name; parameters = generics } in
            match instantiated, generics with
            | _, [] ->
                instantiated
            | Type.Primitive instantiated_name, _ when instantiated_name = name ->
                backup
            | Type.Parametric { parameters; name = instantiated_name }, _
              when instantiated_name = name && List.length parameters <> List.length generics ->
                backup
            | _ ->
                instantiated
          end
    | _ ->
        instantiated
  in
  let definitions =
    definition :: superclasses ~resolution definition
    |> List.map ~f:(fun definition -> annotation ~resolution definition)
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
      attribute
        definition
        ~transitive:true
        ~resolution
        ~name:"__init__"
        ~instantiated
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
        ~name:"__new__"
        ~instantiated
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
    let extract_callable { Method.define = ({ Define.signature = { name; _ }; _ } as define); _ } =
      Option.some_if (Reference.is_suffix ~suffix:(Reference.create "__call__") name) define
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
