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

module Attribute : sig
  type getter_property = {
    self: Expression.t option;
    return: Expression.t option;
  }
  [@@deriving compare, sexp, show, hash]

  type setter_property = {
    self: Expression.t option;
    value: Expression.t option;
  }
  [@@deriving compare, sexp, show, hash]

  type property_kind =
    | ReadOnly of { getter: getter_property }
    | ReadWrite of {
        getter: getter_property;
        setter: setter_property;
      }
  [@@deriving compare, sexp, show, hash]

  type origin =
    | Explicit
    | Implicit
  [@@deriving compare, sexp, show, hash]

  type value_and_origin = {
    value: Expression.t;
    origin: origin;
  }
  [@@deriving compare, sexp, show, hash]

  type simple = {
    annotation: Expression.t option;
    values: value_and_origin list;
    primitive: bool;
    frozen: bool;
    toplevel: bool;
    implicit: bool;
    nested_class: bool;
  }
  [@@deriving compare, sexp, show, hash]

  type method_ = {
    signatures: Define.Signature.t list;
    static: bool;
    final: bool;
  }
  [@@deriving compare, sexp, show, hash]

  type property = {
    async: bool;
    class_property: bool;
    kind: property_kind;
  }
  [@@deriving compare, sexp, show, hash]

  type kind =
    | Simple of simple
    | Method of method_
    | Property of property
  [@@deriving compare, sexp, show, hash]

  type attribute = {
    kind: kind;
    name: Identifier.t;
  }
  [@@deriving compare, sexp, show, hash]

  type t = attribute Node.t [@@deriving compare, sexp, show, hash]

  val create_simple
    :  location:Location.t ->
    ?annotation:Expression.t ->
    ?value_and_origin:value_and_origin ->
    ?primitive:bool ->
    ?frozen:bool ->
    ?toplevel:bool ->
    ?implicit:bool ->
    ?nested_class:bool ->
    name:string ->
    unit ->
    t

  val name : parent:Reference.t -> Expression.t -> string option

  val location_insensitive_compare : t -> t -> int

  val location_insensitive_compare_kind : kind -> kind -> int
end = struct
  type getter_property = {
    self: Expression.t option;
    return: Expression.t option;
  }
  [@@deriving compare, sexp, show, hash]

  type setter_property = {
    self: Expression.t option;
    value: Expression.t option;
  }
  [@@deriving compare, sexp, show, hash]

  type property_kind =
    | ReadOnly of { getter: getter_property }
    | ReadWrite of {
        getter: getter_property;
        setter: setter_property;
      }
  [@@deriving compare, sexp, show, hash]

  type origin =
    | Explicit
    | Implicit
  [@@deriving compare, sexp, show, hash]

  type value_and_origin = {
    value: Expression.t;
    origin: origin;
  }
  [@@deriving compare, sexp, show, hash]

  type simple = {
    annotation: Expression.t option;
    values: value_and_origin list;
    primitive: bool;
    frozen: bool;
    toplevel: bool;
    implicit: bool;
    nested_class: bool;
  }
  [@@deriving compare, sexp, show, hash]

  type method_ = {
    signatures: Define.Signature.t list;
    static: bool;
    final: bool;
  }
  [@@deriving compare, sexp, show, hash]

  type property = {
    async: bool;
    class_property: bool;
    kind: property_kind;
  }
  [@@deriving compare, sexp, show, hash]

  type kind =
    | Simple of simple
    | Method of method_
    | Property of property
  [@@deriving compare, sexp, show, hash]

  type attribute = {
    kind: kind;
    name: Identifier.t;
  }
  [@@deriving compare, sexp, show, hash]

  type t = attribute Node.t [@@deriving compare, sexp, show, hash]

  let location_insensitive_compare_property_kind left right =
    match left, right with
    | ( ReadOnly { getter = { self = left_self; return = left_return } },
        ReadOnly { getter = { self = right_self; return = right_return } } ) -> (
        match Option.compare Expression.location_insensitive_compare left_self right_self with
        | x when not (Int.equal x 0) -> x
        | _ -> Option.compare Expression.location_insensitive_compare left_return right_return)
    | ( ReadWrite
          {
            getter = { self = left_getter_self; return = left_getter_return };
            setter = { self = left_setter_self; value = left_setter_value };
          },
        ReadWrite
          {
            getter = { self = right_getter_self; return = right_getter_return };
            setter = { self = right_setter_self; value = right_setter_value };
          } ) -> (
        match
          Option.compare Expression.location_insensitive_compare left_getter_self right_getter_self
        with
        | x when not (Int.equal x 0) -> x
        | _ -> (
            match
              Option.compare
                Expression.location_insensitive_compare
                left_getter_return
                right_getter_return
            with
            | x when not (Int.equal x 0) -> x
            | _ -> (
                match
                  Option.compare
                    Expression.location_insensitive_compare
                    left_setter_self
                    right_setter_self
                with
                | x when not (Int.equal x 0) -> x
                | _ ->
                    Option.compare
                      Expression.location_insensitive_compare
                      left_setter_value
                      right_setter_value)))
    | _ -> -1


  let location_insensitive_compare_property left right =
    match Bool.compare left.async right.async with
    | x when not (Int.equal x 0) -> x
    | _ -> (
        match Bool.compare left.class_property right.class_property with
        | x when not (Int.equal x 0) -> x
        | _ -> location_insensitive_compare_property_kind left.kind right.kind)


  let location_insensitive_compare_method left right =
    match compare_method_ { left with signatures = [] } { right with signatures = [] } with
    | x when not (Int.equal x 0) -> x
    | _ ->
        List.compare Define.Signature.location_insensitive_compare left.signatures right.signatures


  let location_insensitive_compare_simple left right =
    match
      compare_simple
        { left with annotation = None; values = [] }
        { right with annotation = None; values = [] }
    with
    | x when not (Int.equal x 0) -> x
    | _ -> (
        match
          Option.compare Expression.location_insensitive_compare left.annotation right.annotation
        with
        | x when not (Int.equal x 0) -> x
        | _ ->
            let compare_expression_and_origin
                { value = left_expression; origin = left_origin }
                { value = right_expression; origin = right_origin }
              =
              match compare_origin left_origin right_origin with
              | 0 -> Expression.location_insensitive_compare left_expression right_expression
              | nonzero -> nonzero
            in
            List.compare compare_expression_and_origin left.values right.values)


  let location_insensitive_compare_kind left right =
    match left, right with
    | Simple left, Simple right -> location_insensitive_compare_simple left right
    | Method left, Method right -> location_insensitive_compare_method left right
    | Property left, Property right -> location_insensitive_compare_property left right
    | _ -> -1


  let location_insensitive_compare_attribute left right =
    match [%compare: Identifier.t] left.name right.name with
    | x when not (Int.equal x 0) -> x
    | _ -> location_insensitive_compare_kind left.kind right.kind


  let location_insensitive_compare =
    Node.location_insensitive_compare location_insensitive_compare_attribute


  let create_simple
      ~location
      ?annotation
      ?value_and_origin
      ?(primitive = false)
      ?(frozen = false)
      ?(toplevel = true)
      ?(implicit = false)
      ?(nested_class = false)
      ~name
      ()
    =
    let values = Option.to_list value_and_origin in
    {
      name;
      kind = Simple { annotation; values; primitive; frozen; toplevel; implicit; nested_class };
    }
    |> Node.create ~location


  let name ~parent target =
    let open Expression in
    match Node.value target with
    | Expression.Name
        (Name.Attribute { base = { Node.value = Expression.Name name; _ }; attribute; _ })
      when Option.equal Reference.equal (Some parent) (name_to_reference name) ->
        Some attribute
    | _ -> None
end

module ClassAttributes = struct
  type attribute_map = Attribute.attribute Node.t Identifier.SerializableMap.t
  [@@deriving compare, sexp, show, hash]

  type t = {
    explicitly_assigned_attributes: attribute_map;
    constructor_attributes: attribute_map;
    test_setup_attributes: attribute_map;
    additional_attributes: attribute_map;
  }
  [@@deriving compare, sexp, show, hash]

  let assigned_by_define
      ({ Define.body; signature = { parameters; _ }; _ } as define)
      ~definition:{ Class.body = definition_body; _ }
      : Attribute.t Identifier.SerializableMap.t
    =
    let open Expression in
    let parameter_annotations =
      let add_parameter map = function
        | { Node.value = { Parameter.name; annotation = Some annotation; _ }; _ } ->
            Identifier.SerializableMap.set map ~key:name ~data:annotation
        | _ -> map
      in
      List.fold ~init:Identifier.SerializableMap.empty ~f:add_parameter parameters
    in
    let attribute ~toplevel map { Node.value; _ } =
      match value with
      | Statement.Assign { Assign.target; annotation; value; _ } -> (
          let simple_attribute ~map ~target:({ Node.location; _ } as target) ~annotation =
            match target with
            | {
             Node.value =
               Expression.Name
                 (Name.Attribute
                   { base = { Node.value = Name (Name.Identifier self); _ }; attribute = name; _ });
             _;
            }
              when Identifier.equal self (Define.self_identifier define) ->
                let simple =
                  {
                    Attribute.annotation;
                    values = [{ value; origin = Implicit }];
                    primitive = true;
                    frozen = false;
                    toplevel;
                    implicit = true;
                    nested_class = false;
                  }
                  |> Node.create ~location
                in
                let update = function
                  | Some (head, tail) -> Some (simple, head :: tail)
                  | None -> Some (simple, [])
                in
                Identifier.SerializableMap.update name update map
            | _ -> map
          in
          match target with
          | { Node.value = Name _; _ } ->
              let annotation =
                match toplevel, annotation, value with
                | true, None, { Node.value = Name (Name.Identifier value); _ } ->
                    Identifier.SerializableMap.find_opt value parameter_annotations
                | _ -> annotation
              in
              simple_attribute ~map ~target ~annotation
          | { Node.value = Tuple targets; _ } ->
              List.fold
                ~init:map
                ~f:(fun map target -> simple_attribute ~map ~target ~annotation)
                targets
          | _ -> map)
      | _ -> map
    in
    let merge_attributes name = function
      | { Node.location; value = simple }, [] ->
          { Attribute.kind = Simple simple; name } |> Node.create ~location
      | ({ Node.location; value = simple } as head), tail ->
          let annotation =
            let annotation = function
              | { Node.value = { Attribute.annotation = Some annotation; _ }; _ } -> Some annotation
              | _ -> None
            in
            match List.filter_map ~f:annotation (head :: tail) with
            | [] -> None
            | ({ Node.location; _ } as annotation) :: annotations ->
                let argument_value =
                  Node.create_with_default_location (Expression.Tuple (annotation :: annotations))
                in
                if List.for_all ~f:([%compare.equal: Expression.t] annotation) annotations then
                  Some annotation
                else
                  Some
                    {
                      Node.location;
                      value =
                        Call
                          {
                            callee =
                              {
                                Node.location;
                                value =
                                  Name
                                    (Name.Attribute
                                       {
                                         base =
                                           {
                                             Node.location;
                                             value =
                                               Name
                                                 (Name.Attribute
                                                    {
                                                      base =
                                                        {
                                                          Node.location;
                                                          value = Name (Name.Identifier "typing");
                                                        };
                                                      attribute = "Union";
                                                      special = false;
                                                    });
                                           };
                                         attribute = "__getitem__";
                                         special = true;
                                       });
                              };
                            arguments = [{ Call.Argument.name = None; value = argument_value }];
                          };
                    }
          in
          {
            Node.location;
            value = { Attribute.name; Attribute.kind = Simple { simple with annotation } };
          }
    in
    let rec gather_nested_statements ~toplevel body =
      (* Can't use `Visit` module due to circularity :( *)
      let expand_statement ({ Node.value; _ } as statement) =
        match value with
        | Statement.If { If.body; orelse; _ }
        | For { For.body; orelse; _ }
        | While { While.body; orelse; _ } ->
            gather_nested_statements ~toplevel:false body
            @ gather_nested_statements ~toplevel:false orelse
        | Try { Try.body; orelse; finally; _ } ->
            gather_nested_statements ~toplevel:false body
            @ gather_nested_statements ~toplevel:false orelse
            @ gather_nested_statements ~toplevel:false finally
        | With { With.body; _ } -> gather_nested_statements ~toplevel:false body
        | Expression
            {
              Node.value =
                Call
                  {
                    callee =
                      {
                        Node.value =
                          Name
                            (Name.Attribute
                              {
                                base = { Node.value = Name (Name.Identifier self); _ };
                                attribute = name;
                                _;
                              });
                        _;
                      };
                    _;
                  };
              _;
            }
          when Identifier.equal self (Define.self_identifier define) ->
            (* Look for method in class definition. *)
            let inline = function
              | {
                  Node.value =
                    Statement.Define
                      { signature = { name = callee; parent = Some parent; _ }; body; _ };
                  _;
                }
                when Reference.equal callee (Reference.create ~prefix:parent name) ->
                  Some body
              | _ -> None
            in
            List.find_map ~f:inline definition_body |> Option.value ~default:[statement]
        | _ ->
            if toplevel then
              []
            else
              [statement]
      in
      List.concat_map ~f:expand_statement body
    in
    let toplevel_attributes =
      body |> List.fold ~init:Identifier.SerializableMap.empty ~f:(attribute ~toplevel:true)
    in
    gather_nested_statements ~toplevel:true body
    |> List.fold ~init:toplevel_attributes ~f:(attribute ~toplevel:false)
    |> Identifier.SerializableMap.mapi merge_attributes


  module PropertyDefine = struct
    type getter = {
      name: string;
      self_annotation: Expression.t option;
      return_annotation: Expression.t option;
      location: Location.t;
      async: bool;
      is_class_property: bool;
    }

    type setter = {
      name: string;
      self_annotation: Expression.t option;
      value_annotation: Expression.t option;
    }

    type t =
      | Getter of getter
      | Setter of setter

    let create
        ~location
        ({ Define.signature = { name; return_annotation; parameters; parent; _ }; _ } as define)
      =
      let inspect_decorators name =
        let async = Define.is_async define in
        let is_instance_property () =
          String.Set.exists Recognized.property_decorators ~f:(Define.has_decorator define)
        in
        let is_class_property () =
          String.Set.exists Recognized.classproperty_decorators ~f:(Define.has_decorator define)
        in
        let self_annotation =
          match parameters with
          | { Node.value = { Expression.Parameter.annotation; _ }; _ } :: _ -> annotation
          | _ -> None
        in
        let getter ~is_class_property =
          Some
            (Getter { name; self_annotation; return_annotation; is_class_property; async; location })
        in
        if is_instance_property () then
          getter ~is_class_property:false
        else if is_class_property () then
          getter ~is_class_property:true
        else
          match Define.is_property_setter define, parameters with
          | ( true,
              _
              :: { Node.value = { Expression.Parameter.annotation = value_annotation; _ }; _ } :: _
            ) ->
              Some (Setter { name; self_annotation; value_annotation })
          | _ -> None
      in
      parent
      >>= fun parent ->
      Attribute.name ~parent (Expression.from_reference ~location name) >>= inspect_decorators
  end

  (* Bias towards the right (previously occuring map in the `|> merge other_map` flow), but
     accumulate values *)
  let merge_attribute_maps _ left right =
    match left, right with
    | Some _, None -> left
    | None, Some _ -> right
    | ( Some { Node.value = { Attribute.kind = Simple { values = left_values; _ }; _ }; _ },
        Some
          {
            Node.value =
              { Attribute.kind = Simple ({ values = right_values; _ } as simple); _ } as right;
            location;
          } ) ->
        Some
          {
            Node.value =
              { right with kind = Simple { simple with values = right_values @ left_values } };
            location;
          }
    | _ -> right


  let create ({ Class.name = parent_name; body; _ } as definition) =
    let explicitly_assigned_attributes =
      let assigned_attributes map { Node.location; value } =
        let open Expression in
        match value with
        (* Handle multiple assignments on same line *)
        | Statement.Assign
            {
              Assign.target = { Node.value = Tuple targets; _ };
              value = { Node.value = Tuple values; _ };
              _;
            } ->
            let add_attribute map ({ Node.location; _ } as target) value =
              Attribute.name ~parent:parent_name target
              |> function
              | Some name ->
                  let attribute =
                    Attribute.create_simple
                      ~location
                      ~name
                      ~value_and_origin:{ value; origin = Explicit }
                      ~primitive:true
                      ()
                  in
                  Identifier.SerializableMap.set map ~key:name ~data:attribute
              | _ -> map
            in
            if List.length targets = List.length values then
              List.fold2_exn ~init:map ~f:add_attribute targets values
            else
              map
        | Assign { Assign.target = { Node.value = Tuple targets; _ }; value; _ } ->
            let add_attribute index map ({ Node.location; _ } as target) =
              Attribute.name ~parent:parent_name target
              |> function
              | Some name ->
                  let value =
                    let index =
                      Node.create ~location (Expression.Constant (Constant.Integer index))
                    in
                    match value with
                    | { Node.value = Call _; _ }
                    | { Node.value = Name _; _ } ->
                        Some
                          {
                            value with
                            Node.value =
                              Expression.Call
                                {
                                  callee =
                                    {
                                      Node.location;
                                      value =
                                        Name
                                          (Name.Attribute
                                             {
                                               base = value;
                                               attribute = "__getitem__";
                                               special = true;
                                             });
                                    };
                                  arguments = [{ Call.Argument.name = None; value = index }];
                                };
                          }
                    | _ -> None
                  in
                  value
                  >>| (fun value ->
                        Attribute.create_simple
                          ~location
                          ~name
                          ~value_and_origin:{ value; origin = Explicit }
                          ~primitive:true
                          ())
                  >>| (fun data -> Identifier.SerializableMap.set map ~key:name ~data)
                  |> Option.value ~default:map
              | _ -> map
            in
            List.foldi ~init:map ~f:add_attribute targets
        | Assign { Assign.target; annotation; value; _ } -> (
            Attribute.name ~parent:parent_name target
            |> function
            | Some name ->
                let frozen = Class.is_frozen definition in
                let attribute =
                  Attribute.create_simple
                    ~location
                    ~name
                    ~value_and_origin:{ value; origin = Explicit }
                    ?annotation
                    ~primitive:true
                    ~frozen
                    ()
                in
                Identifier.SerializableMap.set map ~key:name ~data:attribute
            | _ -> map)
        | _ -> map
      in
      List.fold ~init:Identifier.SerializableMap.empty ~f:assigned_attributes body
    in
    let get_implicits defines =
      List.map defines ~f:(assigned_by_define ~definition)
      |> List.fold
           ~init:Identifier.SerializableMap.empty
           ~f:(Identifier.SerializableMap.merge merge_attribute_maps)
    in
    let constructor_attributes = Class.constructors ~in_test:false definition |> get_implicits in
    let test_setup_attributes =
      let test_setups { Class.body; _ } =
        let constructor = function
          | { Node.value = Statement.Define define; _ } when Define.is_test_setup define ->
              Some define
          | _ -> None
        in
        List.filter_map ~f:constructor body
      in
      test_setups definition |> get_implicits
    in
    let additional_attributes =
      let property_attributes =
        let property_attributes map = function
          | { Node.location; value = Statement.Define define } -> (
              match PropertyDefine.create ~location define with
              | Some (Setter { name; _ } as kind)
              | Some (Getter { name; _ } as kind) ->
                  let data =
                    Identifier.SerializableMap.find_opt name map
                    |> Option.value ~default:(None, None)
                    |> fun (existing_getter, existing_setter) ->
                    match kind with
                    | Setter setter -> existing_getter, Some setter
                    | Getter getter -> Some getter, existing_setter
                  in
                  Identifier.SerializableMap.set map ~key:name ~data
              | None -> map)
          | _ -> map
        in
        let consolidate = function
          | _, (None, None)
          | _, (None, Some _) ->
              None (* not allowed *)
          | ( _,
              ( Some
                  {
                    PropertyDefine.name;
                    self_annotation;
                    return_annotation;
                    async;
                    location;
                    is_class_property;
                  },
                None ) ) ->
              ( name,
                {
                  Attribute.name;
                  kind =
                    Property
                      {
                        kind =
                          ReadOnly
                            { getter = { self = self_annotation; return = return_annotation } };
                        async;
                        class_property = is_class_property;
                      };
                }
                |> Node.create ~location )
              |> Option.some
          | ( _,
              ( Some
                  {
                    PropertyDefine.name;
                    self_annotation = getter_self_annotation;
                    return_annotation = getter_return_annotation;
                    async;
                    location;
                    is_class_property;
                  },
                Some
                  {
                    PropertyDefine.self_annotation = setter_self_annotation;
                    value_annotation = setter_value_annotation;
                    _;
                  } ) ) ->
              ( name,
                {
                  Attribute.name;
                  kind =
                    Property
                      {
                        kind =
                          ReadWrite
                            {
                              getter =
                                { self = getter_self_annotation; return = getter_return_annotation };
                              setter =
                                { self = setter_self_annotation; value = setter_value_annotation };
                            };
                        async;
                        class_property = is_class_property;
                      };
                }
                |> Node.create ~location )
              |> Option.some
        in
        List.fold ~init:Identifier.SerializableMap.empty ~f:property_attributes body
        |> Identifier.SerializableMap.to_seq
        |> Seq.filter_map consolidate
        |> Identifier.SerializableMap.of_seq
      in
      let callable_attributes =
        let callable_attributes map { Node.location; value } =
          match value with
          | Statement.Define ({ Define.signature = { name = target; _ } as signature; _ } as define)
            ->
              Attribute.name (Expression.from_reference ~location target) ~parent:parent_name
              >>| (fun name ->
                    let attribute =
                      match Identifier.SerializableMap.find_opt name map with
                      | Some { Node.value = { Attribute.kind = Method { signatures; _ }; _ }; _ } ->
                          {
                            Attribute.name;
                            kind =
                              Method
                                {
                                  signatures = signature :: signatures;
                                  static = Define.is_static_method define;
                                  final = Define.is_final_method define;
                                };
                          }
                          |> Node.create ~location
                      | _ ->
                          {
                            Attribute.name;
                            kind =
                              Method
                                {
                                  signatures = [signature];
                                  static = Define.is_static_method define;
                                  final = Define.is_final_method define;
                                };
                          }
                          |> Node.create ~location
                    in
                    Identifier.SerializableMap.set map ~key:name ~data:attribute)
              |> Option.value ~default:map
          | _ -> map
        in
        List.fold ~init:Identifier.SerializableMap.empty ~f:callable_attributes body
      in
      let class_attributes =
        let callable_attributes map { Node.location; value } =
          match value with
          | Statement.Class { name; _ } ->
              let open Expression in
              let annotation =
                let meta_annotation =
                  {
                    Node.location;
                    value =
                      Expression.Call
                        {
                          callee =
                            {
                              Node.location;
                              value =
                                Name
                                  (Name.Attribute
                                     {
                                       base =
                                         {
                                           Node.location;
                                           value =
                                             Name
                                               (Name.Attribute
                                                  {
                                                    base =
                                                      {
                                                        Node.location;
                                                        value = Name (Name.Identifier "typing");
                                                      };
                                                    attribute = "Type";
                                                    special = false;
                                                  });
                                         };
                                       attribute = "__getitem__";
                                       special = true;
                                     });
                            };
                          arguments =
                            [
                              {
                                Call.Argument.name = None;
                                value = from_reference ~location:Location.any name;
                              };
                            ];
                        };
                  }
                in
                {
                  Node.location;
                  value =
                    Expression.Call
                      {
                        callee =
                          {
                            Node.location;
                            value =
                              Name
                                (Name.Attribute
                                   {
                                     base =
                                       {
                                         Node.location;
                                         value =
                                           Name
                                             (Name.Attribute
                                                {
                                                  base =
                                                    {
                                                      Node.location;
                                                      value = Name (Name.Identifier "typing");
                                                    };
                                                  attribute = "ClassVar";
                                                  special = false;
                                                });
                                       };
                                     attribute = "__getitem__";
                                     special = true;
                                   });
                          };
                        arguments = [{ Call.Argument.name = None; value = meta_annotation }];
                      };
                }
              in
              let attribute_name = Reference.last name in
              Identifier.SerializableMap.set
                map
                ~key:attribute_name
                ~data:
                  (Attribute.create_simple
                     ~location
                     ~name:attribute_name
                     ~annotation
                     ~nested_class:true
                     ())
          | _ -> map
        in
        List.fold ~init:Identifier.SerializableMap.empty ~f:callable_attributes body
      in
      let slots_attributes =
        let slots_attributes map { Node.value; _ } =
          let open Expression in
          let is_slots = function
            | Expression.Name (Name.Identifier "__slots__")
            | Expression.Name (Name.Attribute { attribute = "__slots__"; _ }) ->
                true
            | _ -> false
          in
          match value with
          | Statement.Assign
              {
                Assign.target = { Node.value = target_value; _ };
                value = { Node.value = List attributes; location };
                _;
              }
            when is_slots target_value ->
              let add_attribute map { Node.value; _ } =
                match value with
                | Expression.Constant (Constant.String { StringLiteral.value; _ }) ->
                    Attribute.create_simple ~location ~name:value ()
                    |> fun attribute ->
                    Identifier.SerializableMap.set map ~key:value ~data:attribute
                | _ -> map
              in
              List.fold ~init:map ~f:add_attribute attributes
          | _ -> map
        in
        List.fold ~init:Identifier.SerializableMap.empty ~f:slots_attributes body
      in
      property_attributes
      |> Identifier.SerializableMap.merge merge_attribute_maps callable_attributes
      |> Identifier.SerializableMap.merge merge_attribute_maps class_attributes
      |> Identifier.SerializableMap.merge merge_attribute_maps slots_attributes
    in
    {
      explicitly_assigned_attributes;
      constructor_attributes;
      test_setup_attributes;
      additional_attributes;
    }


  let empty () =
    {
      explicitly_assigned_attributes = Identifier.SerializableMap.empty;
      constructor_attributes = Identifier.SerializableMap.empty;
      test_setup_attributes = Identifier.SerializableMap.empty;
      additional_attributes = Identifier.SerializableMap.empty;
    }


  let implicit_attributes
      ~in_test
      { constructor_attributes; test_setup_attributes; additional_attributes; _ }
    =
    let implicitly_assigned_attributes =
      if in_test then
        Identifier.SerializableMap.merge
          merge_attribute_maps
          test_setup_attributes
          constructor_attributes
      else
        constructor_attributes
    in
    (* Merge with decreasing priority. *)
    additional_attributes
    |> Identifier.SerializableMap.merge merge_attribute_maps implicitly_assigned_attributes


  let attributes
      ~include_generated_attributes
      ~in_test
      ({ explicitly_assigned_attributes; _ } as components)
    =
    if not include_generated_attributes then
      explicitly_assigned_attributes
    else
      explicitly_assigned_attributes
      |> Identifier.SerializableMap.merge
           merge_attribute_maps
           (implicit_attributes ~in_test components)


  (* Exposed for testing only *)
  module Private = struct
    let assigned_by_define = assigned_by_define
  end
end

module ClassSummary = struct
  type bases = {
    base_classes: Expression.t list;
    metaclass: Expression.t option;
    init_subclass_arguments: Expression.Call.Argument.t list;
  }
  [@@deriving compare, sexp, show, hash, to_yojson]

  type t = {
    name: Reference.t;
    qualifier: Reference.t;
    bases: bases;
    decorators: Expression.t list;
    class_attributes: ClassAttributes.t;
  }
  [@@deriving compare, sexp, show, hash]

  let create ~qualifier ({ Ast.Statement.Class.name; decorators; _ } as class_definition) =
    let bases =
      {
        base_classes = Ast.Statement.Class.base_classes class_definition;
        metaclass = Ast.Statement.Class.metaclass class_definition;
        init_subclass_arguments = Ast.Statement.Class.init_subclass_arguments class_definition;
      }
    in
    {
      name;
      qualifier;
      bases;
      decorators;
      class_attributes = ClassAttributes.create class_definition;
    }


  let is_protocol { bases = { base_classes; _ }; _ } =
    let is_protocol { Node.value; _ } =
      let open Expression in
      match value with
      | Expression.Call
          {
            callee =
              {
                Node.value =
                  Name
                    (Attribute
                      {
                        base =
                          {
                            Node.value =
                              Name
                                (Attribute
                                  {
                                    base = { Node.value = Name (Identifier typing); _ };
                                    attribute = "Protocol";
                                    _;
                                  });
                            _;
                          };
                        attribute = "__getitem__";
                        _;
                      });
                _;
              };
            _;
          }
      | Name
          (Attribute
            { base = { Node.value = Name (Identifier typing); _ }; attribute = "Protocol"; _ })
        when String.equal typing "typing" || String.equal typing "typing_extensions" ->
          true
      | _ -> false
    in
    List.exists ~f:is_protocol base_classes


  let has_decorator { decorators; _ } decorator =
    Expression.exists_in_list ~expression_list:decorators decorator


  let is_final definition =
    has_decorator definition "typing.final" || has_decorator definition "typing_extensions.final"


  let is_abstract { bases = { base_classes; metaclass; _ }; _ } =
    let open Expression in
    let is_abstract_base_class { Node.value; _ } =
      match value with
      | Expression.Name
          (Attribute { base = { Node.value = Name (Identifier "abc"); _ }; attribute = "ABC"; _ })
        ->
          true
      | _ -> false
    in
    let is_abstract_metaclass = function
      | Some
          {
            Node.value =
              Expression.Name
                (Attribute
                  { base = { Node.value = Name (Identifier "abc"); _ }; attribute = "ABCMeta"; _ });
            _;
          } ->
          true
      | _ -> false
    in
    List.exists base_classes ~f:is_abstract_base_class || is_abstract_metaclass metaclass


  let fields_tuple_value { class_attributes; _ } =
    let attributes =
      ClassAttributes.attributes ~include_generated_attributes:false ~in_test:false class_attributes
    in
    match Identifier.SerializableMap.find_opt "_fields" attributes with
    | Some
        {
          Node.value =
            {
              kind =
                Simple
                  { values = [{ origin = Explicit; value = { Node.value = Tuple fields; _ } }]; _ };
              _;
            };
          _;
        } ->
        let name = function
          | {
              Node.value =
                Ast.Expression.(Expression.Constant (Constant.String { StringLiteral.value; _ }));
              _;
            } ->
              Some value
          | _ -> None
        in
        Some (List.filter_map fields ~f:name)
    | _ -> None


  let name { name; _ } = name

  let bases { bases; _ } = bases

  let base_classes { bases = { base_classes; _ }; _ } = base_classes

  let constructor_attributes { class_attributes = { ClassAttributes.constructor_attributes; _ }; _ }
    =
    constructor_attributes


  let attributes ?(include_generated_attributes = true) ?(in_test = false) { class_attributes; _ } =
    ClassAttributes.attributes ~include_generated_attributes ~in_test class_attributes
end

include ClassSummary
