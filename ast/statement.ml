(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
module Name = Expression.Name

module Record = struct
  module Define = struct
    type signature = {
      name: Reference.t;
      parameters: Expression.t Parameter.t list;
      decorators: Expression.t list;
      docstring: string option;
      return_annotation: Expression.t option;
      async: bool;
      parent: Reference.t option; (* The class owning the method. *)
    }
    [@@deriving compare, eq, sexp, show, hash, to_yojson]

    type 'statement record = {
      signature: signature;
      body: 'statement list;
    }
    [@@deriving compare, eq, sexp, show, hash, to_yojson]
  end

  module Class = struct
    type 'statement record = {
      name: Reference.t;
      bases: Expression.Call.Argument.t list;
      body: 'statement list;
      decorators: Expression.t list;
      docstring: string option;
    }
    [@@deriving compare, eq, sexp, show, hash, to_yojson]
  end

  module For = struct
    type 'statement record = {
      target: Expression.t;
      iterator: Expression.t;
      body: 'statement list;
      orelse: 'statement list;
      async: bool;
    }
    [@@deriving compare, eq, sexp, show, hash, to_yojson]
  end

  module With = struct
    type 'statement record = {
      items: (Expression.t * Expression.t option) list;
      body: 'statement list;
      async: bool;
    }
    [@@deriving compare, eq, sexp, show, hash, to_yojson]
  end

  module Try = struct
    type 'statement handler = {
      kind: Expression.t option;
      name: Identifier.t option;
      handler_body: 'statement list;
    }
    [@@deriving compare, eq, sexp, show, hash, to_yojson]

    type 'statement record = {
      body: 'statement list;
      handlers: 'statement handler list;
      orelse: 'statement list;
      finally: 'statement list;
    }
    [@@deriving compare, eq, sexp, show, hash, to_yojson]
  end
end

(* Not sure why the OCaml compiler hates me... *)
module RecordWith = Record.With
module RecordFor = Record.For
module RecordTry = Record.Try

module While = struct
  type 'statement t = {
    test: Expression.t;
    body: 'statement list;
    orelse: 'statement list;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]
end

module If = struct
  type 'statement t = {
    test: Expression.t;
    body: 'statement list;
    orelse: 'statement list;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]
end

module Assert = struct
  type 'statement origin =
    | Assertion
    | If of {
        statement: 'statement;
        true_branch: bool;
      }
    | While

  and 'statement t = {
    test: Expression.t;
    message: Expression.t option;
    origin: 'statement origin;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]
end

module Import = struct
  type import = {
    name: Reference.t;
    alias: Reference.t option;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  type t = {
    from: Reference.t option;
    imports: import list;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]
end

module Assign = struct
  type t = {
    target: Expression.t;
    annotation: Expression.t option;
    value: Expression.t;
    parent: Reference.t option;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  let is_static_attribute_initialization { parent; _ } = Option.is_some parent
end

module Raise = struct
  type t = {
    expression: Expression.t option;
    from: Expression.t option;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]
end

module Return = struct
  type t = {
    is_implicit: bool;
    expression: Expression.t option;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]
end

type statement =
  | Assign of Assign.t
  | Assert of t Assert.t
  | Break
  | Class of t Record.Class.record
  | Continue
  | Define of t Record.Define.record
  | Delete of Expression.t
  | Expression of Expression.t
  | For of t Record.For.record
  | Global of Identifier.t list
  | If of t If.t
  | Import of Import.t
  | Nonlocal of Identifier.t list
  | Pass
  | Raise of Raise.t
  | Return of Return.t
  | Try of t Record.Try.record
  | With of t Record.With.record
  | While of t While.t
  | Yield of Expression.t
  | YieldFrom of Expression.t

and t = statement Node.t [@@deriving compare, eq, sexp, show, hash, to_yojson]

let _ = show (* shadowed below *)

type statement_t = t [@@deriving compare, eq, sexp, show, hash, to_yojson]

module Attribute = struct
  type attribute = {
    annotation: Expression.t option;
    async: bool;
    defines: statement_t Record.Define.record list option;
    final: bool;
    implicit: bool;
    frozen: bool;
    name: Identifier.t;
    primitive: bool;
    property: bool;
    setter: bool;
    static: bool;
    toplevel: bool;
    value: Expression.t option;
  }
  [@@deriving compare, eq, sexp, show, hash]

  type t = attribute Node.t [@@deriving compare, eq, sexp, show, hash]

  let create
      ~location
      ?(async = false)
      ?(setter = false)
      ?(primitive = false)
      ?(property = false)
      ?(toplevel = true)
      ?value
      ?annotation
      ?defines
      ?(final = false)
      ?(static = false)
      ?(frozen = false)
      ?(implicit = false)
      ~name
      ()
    =
    {
      name;
      annotation;
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
    }
    |> Node.create ~location


  let name ~parent target =
    let open Expression in
    match Node.value target with
    | Name (Name.Attribute { base; attribute; _ })
      when Expression.equal
             base
             (Expression.from_reference ~location:Location.Reference.any parent) ->
        Some attribute
    | _ -> None
end

module Define = struct
  include Record.Define

  type t = statement_t Record.Define.record [@@deriving compare, eq, sexp, show, hash, to_yojson]

  module Signature = struct
    type t = Record.Define.signature [@@deriving compare, eq, sexp, show, hash]

    let create_toplevel ~qualifier =
      {
        name = Reference.create ?prefix:qualifier "$toplevel";
        parameters = [];
        decorators = [];
        docstring = None;
        return_annotation = None;
        async = false;
        parent = None;
      }


    let create_class_toplevel ~parent =
      {
        name = Reference.create ~prefix:parent "$class_toplevel";
        parameters = [];
        decorators = [];
        docstring = None;
        return_annotation = None;
        async = false;
        parent = Some parent;
      }


    let unqualified_name { name; _ } = Reference.last name

    let self_identifier { parameters; _ } =
      match parameters with
      | { Node.value = { Parameter.name; _ }; _ } :: _ -> name
      | _ -> "self"


    let is_method { parent; _ } = Option.is_some parent

    let has_decorator ?(match_prefix = false) { decorators; _ } decorator =
      Expression.exists_in_list ~match_prefix ~expression_list:decorators decorator


    let has_return_annotation { return_annotation; _ } = Option.is_some return_annotation

    let is_coroutine signature = has_decorator signature "asyncio.coroutines.coroutine"

    let is_abstract_method signature =
      has_decorator signature "abstractmethod"
      || has_decorator signature "abc.abstractmethod"
      || has_decorator signature "abstractproperty"
      || has_decorator signature "abc.abstractproperty"


    let is_overloaded_method signature =
      has_decorator signature "overload" || has_decorator signature "typing.overload"


    let is_static_method signature =
      (* `__new__` is always a static method. See
         `https://docs.python.org/3/reference/datamodel.html#object.__new__`. *)
      String.equal (unqualified_name signature) "__new__" || has_decorator signature "staticmethod"


    let is_final_method signature = has_decorator signature "typing.final"

    let is_dunder_method signature =
      let name = unqualified_name signature in
      String.is_prefix ~prefix:"__" name && String.is_suffix ~suffix:"__" name


    let is_class_method ({ parent; _ } as signature) =
      let valid_names = ["__init_subclass__"; "__new__"; "__class_getitem__"] in
      Option.is_some parent
      && ( Set.exists Recognized.classmethod_decorators ~f:(has_decorator signature)
         || List.mem valid_names (unqualified_name signature) ~equal:String.equal )


    let is_class_property ({ parent; _ } as signature) =
      Option.is_some parent
      && Set.exists Recognized.classproperty_decorators ~f:(has_decorator signature)


    let is_constructor ?(in_test = false) ({ parent; _ } as signature) =
      let name = unqualified_name signature in
      if Option.is_none parent then
        false
      else
        String.equal name "__init__"
        || String.equal name "__new__"
        || in_test
           && List.mem
                ~equal:String.equal
                ["async_setUp"; "setUp"; "_setup"; "_async_setup"; "with_context"]
                name


    let is_property_setter signature =
      has_decorator signature (unqualified_name signature ^ ".setter")


    let is_property signature = has_decorator signature "property"

    let is_untyped { return_annotation; _ } = Option.is_none return_annotation

    let is_async { async; _ } = async

    let is_toplevel signature = String.equal (unqualified_name signature) "$toplevel"

    let is_class_toplevel signature = String.equal (unqualified_name signature) "$class_toplevel"
  end

  let create_toplevel ~qualifier ~statements =
    { signature = Signature.create_toplevel ~qualifier; body = statements }


  let create_class_toplevel ~parent ~statements =
    { signature = Signature.create_class_toplevel ~parent; body = statements }


  let unqualified_name { signature; _ } = Signature.unqualified_name signature

  let self_identifier { signature; _ } = Signature.self_identifier signature

  let is_method { signature; _ } = Signature.is_method signature

  let has_decorator ?(match_prefix = false) { signature; _ } decorator =
    Signature.has_decorator ~match_prefix signature decorator


  let has_return_annotation { signature; _ } = Signature.has_return_annotation signature

  let is_coroutine { signature; _ } = Signature.is_coroutine signature

  let is_abstract_method { signature; _ } = Signature.is_abstract_method signature

  let is_overloaded_method { signature; _ } = Signature.is_overloaded_method signature

  let is_static_method { signature; _ } = Signature.is_static_method signature

  let is_final_method { signature; _ } = Signature.is_final_method signature

  let is_dunder_method { signature; _ } = Signature.is_dunder_method signature

  let is_class_method { signature; _ } = Signature.is_class_method signature

  let is_class_property { signature; _ } = Signature.is_class_property signature

  let is_constructor ?(in_test = false) { signature; _ } =
    Signature.is_constructor ~in_test signature


  let is_property_setter { signature; _ } = Signature.is_property_setter signature

  let is_property { signature; _ } = Signature.is_property signature

  let is_untyped { signature; _ } = Signature.is_untyped signature

  let is_async { signature; _ } = Signature.is_async signature

  let is_toplevel { signature; _ } = Signature.is_toplevel signature

  let is_class_toplevel { signature; _ } = Signature.is_class_toplevel signature

  let contains_call { body; _ } name =
    let matches = function
      | {
          Node.value =
            Expression
              {
                Node.value =
                  Expression.Call
                    {
                      callee =
                        { Node.value = Expression.Name (Expression.Name.Identifier identifier); _ };
                      _;
                    };
                _;
              };
          _;
        }
        when String.equal identifier name ->
          true
      | _ -> false
    in
    List.exists ~f:matches body


  let is_stub { body; _ } =
    match List.rev body with
    | { Node.value = Expression { Node.value = Expression.Ellipsis; _ }; _ } :: _
    | _ :: { Node.value = Expression { Node.value = Expression.Ellipsis; _ }; _ } :: _ ->
        true
    | _ -> false


  let dump define = contains_call define "pyre_dump"

  let dump_cfg define = contains_call define "pyre_dump_cfg"

  let dump_locations define = contains_call define "pyre_dump_locations"

  let show_json define = define |> to_yojson |> Yojson.Safe.pretty_to_string

  let implicit_attributes
      ({ body; signature = { parameters; _ } } as define)
      ~definition:{ Record.Class.body = definition_body; _ }
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
      | Assign { Assign.target; annotation; value; _ } -> (
          let attribute ~map ~target:({ Node.location; _ } as target) ~annotation =
            match target with
            | {
             Node.value =
               Name
                 (Name.Attribute
                   { base = { Node.value = Name (Name.Identifier self); _ }; attribute = name; _ });
             _;
            }
              when Identifier.equal self (self_identifier define) ->
                let attribute =
                  Attribute.create
                    ~primitive:true
                    ~toplevel
                    ~location
                    ~name
                    ?annotation
                    ~value
                    ~implicit:true
                    ()
                in
                let update = function
                  | Some attributes -> Some (attribute :: attributes)
                  | None -> Some [attribute]
                in
                Identifier.SerializableMap.update name update map
            | _ -> map
          in
          match target with
          | { Node.value = Name _; _ } ->
              let annotation =
                let is_reassignment target value =
                  let target = Identifier.sanitized target in
                  let value = Identifier.sanitized value in
                  String.equal target value || String.equal target ("_" ^ value)
                in
                match toplevel, annotation, target, value with
                | ( true,
                    None,
                    {
                      Node.value =
                        Name
                          (Name.Attribute
                            {
                              base = { Node.value = Name (Name.Identifier _); _ };
                              attribute = target;
                              _;
                            });
                      _;
                    },
                    { Node.value = Name (Name.Identifier value); _ } )
                  when is_reassignment target value ->
                    Identifier.SerializableMap.find_opt value parameter_annotations
                | _ -> annotation
              in
              attribute ~map ~target ~annotation
          | { Node.value = Tuple targets; _ } ->
              List.fold ~init:map ~f:(fun map target -> attribute ~map ~target ~annotation) targets
          | _ -> map )
      | _ -> map
    in
    let merge_attributes = function
      | [attribute] -> attribute
      | { Node.location; value = attribute } :: _ as attributes ->
          let annotation =
            let annotation = function
              | { Node.value = { Attribute.annotation = Some annotation; _ }; _ } ->
                  Some annotation
              | _ -> None
            in
            match List.filter_map ~f:annotation attributes with
            | [] -> None
            | ({ Node.location; _ } as annotation) :: annotations ->
                let argument_value =
                  Node.create_with_default_location (Tuple (annotation :: annotations))
                in
                if List.for_all ~f:(Expression.equal annotation) annotations then
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
          { Node.location; value = { attribute with Attribute.annotation } }
      | [] -> failwith "Unpossible!"
    in
    let rec gather_nested_statements ~toplevel body =
      (* Can't use `Visit` module due to circularity :( *)
      let expand_statement ({ Node.value; _ } as statement) =
        match value with
        | If { If.body; orelse; _ }
        | For { RecordFor.body; orelse; _ }
        | While { While.body; orelse; _ } ->
            gather_nested_statements ~toplevel:false body
            @ gather_nested_statements ~toplevel:false orelse
        | Try { RecordTry.body; orelse; finally; _ } ->
            gather_nested_statements ~toplevel:false body
            @ gather_nested_statements ~toplevel:false orelse
            @ gather_nested_statements ~toplevel:false finally
        | With { RecordWith.body; _ } -> gather_nested_statements ~toplevel:false body
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
          when Identifier.equal self (self_identifier define) ->
            (* Look for method in class definition. *)
            let inline = function
              | {
                  Node.value =
                    Define { signature = { name = callee; parent = Some parent; _ }; body };
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
    |> Identifier.SerializableMap.map merge_attributes


  let property_attribute
      ~location
      ({ signature = { name; return_annotation; parameters; parent; _ }; _ } as define)
    =
    let attribute ?(setter = false) annotation =
      parent
      >>= (fun parent -> Attribute.name ~parent (Expression.from_reference ~location name))
      >>| fun name ->
      Attribute.create
        ~location
        ~setter
        ~name
        ~property:true
        ?annotation
        ~async:(is_async define)
        ()
    in
    match String.Set.find ~f:(has_decorator define) Recognized.property_decorators with
    | Some decorator when Set.mem Recognized.classproperty_decorators decorator ->
        let return_annotation =
          let open Expression in
          match return_annotation with
          | Some ({ Node.location; value = Name _ } as name) ->
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
                                                  attribute = "ClassVar";
                                                  special = false;
                                                });
                                       };
                                     attribute = "__getitem__";
                                     special = true;
                                   });
                          };
                        arguments = [{ Call.Argument.name = None; value = name }];
                      };
                }
          | _ -> None
        in
        attribute return_annotation
    | Some _ -> attribute return_annotation
    | None -> (
      match is_property_setter define, parameters with
      | true, _ :: { Node.value = { Parameter.annotation; _ }; _ } :: _ ->
          attribute ~setter:true annotation
      | _ -> None )
end

let assume ?(origin = Assert.Assertion) ({ Node.location; _ } as test) =
  { Node.location; value = Assert { Assert.test; message = None; origin } }


(* Naive assumptions *)
let terminates body =
  let find_terminator = function
    | { Node.value = Return _; _ }
    | { Node.value = Raise _; _ }
    | { Node.value = Continue; _ } ->
        true
    | _ -> false
  in
  Option.is_some (List.find ~f:find_terminator body)


module Class = struct
  include Record.Class

  type t = statement_t Record.Class.record [@@deriving compare, eq, sexp, show, hash]

  let constructors ?(in_test = false) { Record.Class.body; _ } =
    let constructor = function
      | { Node.value = Define define; _ } when Define.is_constructor ~in_test define -> Some define
      | _ -> None
    in
    List.filter_map ~f:constructor body


  let defines { Record.Class.body; _ } =
    let define = function
      | { Node.value = Define define; _ } -> Some define
      | _ -> None
    in
    List.filter_map ~f:define body


  let find_define { Record.Class.body; _ } ~method_name =
    let is_define = function
      | { Node.value = Define define; location }
        when String.equal (Define.unqualified_name define) method_name ->
          Some { Node.value = define; location }
      | _ -> None
    in
    List.filter_map ~f:is_define body |> List.hd


  let is_frozen { decorators; _ } =
    let is_frozen_dataclass decorator =
      match decorator with
      | {
       Node.value =
         Expression.Call
           {
             callee =
               {
                 Node.value =
                   Name
                     (Name.Attribute
                       {
                         base = { value = Name (Name.Identifier "dataclasses"); _ };
                         attribute = "dataclass";
                         _;
                       });
                 _;
               };
             arguments;
           };
       _;
      } ->
          let has_frozen_argument Expression.Call.Argument.{ name; value } =
            match name, value with
            | Some { Node.value; _ }, { Node.value = Expression.True; _ } ->
                String.equal "frozen" (Identifier.sanitized value)
            | _, _ -> false
          in
          List.exists arguments ~f:has_frozen_argument
      | _ -> false
    in
    List.exists decorators ~f:is_frozen_dataclass


  let explicitly_assigned_attributes ({ Record.Class.name; body; _ } as definition) =
    let assigned_attributes map { Node.location; value } =
      let open Expression in
      match value with
      (* Handle multiple assignments on same line *)
      | Assign
          {
            Assign.target = { Node.value = Tuple targets; _ };
            value = { Node.value = Tuple values; _ };
            _;
          } ->
          let add_attribute map ({ Node.location; _ } as target) value =
            Attribute.name ~parent:name target
            |> function
            | Some name ->
                let attribute = Attribute.create ~primitive:true ~location ~name ~value () in
                Identifier.SerializableMap.set map ~key:name ~data:attribute
            | _ -> map
          in
          if List.length targets = List.length values then
            List.fold2_exn ~init:map ~f:add_attribute targets values
          else
            map
      | Assign { Assign.target = { Node.value = Tuple targets; _ }; value; _ } ->
          let add_attribute index map ({ Node.location; _ } as target) =
            Attribute.name ~parent:name target
            |> function
            | Some name ->
                let value =
                  let index = Node.create ~location (Integer index) in
                  match value with
                  | { Node.value = Call _; _ }
                  | { Node.value = Name _; _ } ->
                      Some
                        {
                          value with
                          Node.value =
                            Call
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
                >>| (fun value -> Attribute.create ~primitive:true ~location ~name ~value ())
                >>| (fun data -> Identifier.SerializableMap.set map ~key:name ~data)
                |> Option.value ~default:map
            | _ -> map
          in
          List.foldi ~init:map ~f:add_attribute targets
      | Assign { Assign.target; annotation; value; _ } -> (
          Attribute.name ~parent:name target
          |> function
          | Some name ->
              let frozen = is_frozen definition in
              let attribute =
                Attribute.create ~primitive:true ~location ~name ?annotation ~frozen ~value ()
              in
              Identifier.SerializableMap.set map ~key:name ~data:attribute
          | _ -> map )
      | _ -> map
    in
    List.fold ~init:Identifier.SerializableMap.empty ~f:assigned_attributes body


  let implicit_attributes ?(in_test = false) ({ Record.Class.name; body; _ } as definition) =
    (* Bias towards the right (previously occuring map in the `|> merge other_map` flow). *)
    let merge _ left right =
      match right with
      | None -> left
      | Some _ -> right
    in
    let implicitly_assigned_attributes =
      constructors ~in_test definition
      |> List.map ~f:(Define.implicit_attributes ~definition)
      |> List.fold
           ~init:Identifier.SerializableMap.empty
           ~f:(Identifier.SerializableMap.merge merge)
    in
    let property_attributes =
      let property_attributes map = function
        | { Node.location; value = Define define } -> (
          match Define.property_attribute ~location define with
          | Some
              ( {
                  Node.value =
                    { Attribute.name; setter = new_setter; annotation = new_annotation; _ } as
                    attribute;
                  _;
                } as attribute_node ) ->
              let merged_attribute =
                let existing_attribute = Identifier.SerializableMap.find_opt name map in
                match existing_attribute, new_setter with
                | Some { Node.value = { Attribute.setter = true; annotation; _ }; _ }, false ->
                    { attribute with Attribute.annotation; value = new_annotation; setter = true }
                    |> fun edited -> { attribute_node with Node.value = edited }
                | Some { Node.value = { Attribute.setter = false; annotation; _ }; _ }, true ->
                    {
                      attribute with
                      Attribute.annotation = new_annotation;
                      value = annotation;
                      setter = true;
                    }
                    |> fun edited -> { attribute_node with Node.value = edited }
                | _ -> attribute_node
              in
              Identifier.SerializableMap.set map ~key:name ~data:merged_attribute
          | _ -> map )
        | _ -> map
      in
      List.fold ~init:Identifier.SerializableMap.empty ~f:property_attributes body
    in
    let callable_attributes =
      let callable_attributes map { Node.location; value } =
        match value with
        | Define ({ Define.signature = { name = target; _ }; _ } as define) ->
            Attribute.name (Expression.from_reference ~location target) ~parent:name
            >>| (fun name ->
                  let attribute =
                    match Identifier.SerializableMap.find_opt name map with
                    | Some { Node.value = { Attribute.defines = Some defines; _ }; _ } ->
                        Attribute.create
                          ~location
                          ~name
                          ~defines:(define :: defines)
                          ~final:(Define.is_final_method define)
                          ~static:(Define.is_static_method define)
                          ()
                    | _ ->
                        Attribute.create
                          ~location
                          ~name
                          ~defines:[define]
                          ~final:(Define.is_final_method define)
                          ~static:(Define.is_static_method define)
                          ()
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
        | Class { Record.Class.name; _ } ->
            let open Expression in
            let annotation =
              let meta_annotation =
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
                                                  attribute = "Type";
                                                  special = false;
                                                });
                                       };
                                     attribute = "__getitem__";
                                     special = true;
                                   });
                          };
                        arguments =
                          [ {
                              Call.Argument.name = None;
                              value =
                                Expression.from_reference ~location:Location.Reference.any name;
                            } ];
                      };
                }
              in
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
              ~data:(Attribute.create ~location ~name:attribute_name ~annotation ())
        | _ -> map
      in
      List.fold ~init:Identifier.SerializableMap.empty ~f:callable_attributes body
    in
    let slots_attributes =
      let slots_attributes map { Node.value; _ } =
        let open Expression in
        let is_slots = function
          | Name (Name.Identifier "__slots__")
          | Name (Name.Attribute { attribute = "__slots__"; _ }) ->
              true
          | _ -> false
        in
        match value with
        | Assign
            {
              Assign.target = { Node.value = target_value; _ };
              value = { Node.value = List attributes; location };
              _;
            }
          when is_slots target_value ->
            let add_attribute map { Node.value; _ } =
              match value with
              | String { StringLiteral.value; _ } ->
                  Attribute.create ~location ~name:value ()
                  |> fun attribute -> Identifier.SerializableMap.set map ~key:value ~data:attribute
              | _ -> map
            in
            List.fold ~init:map ~f:add_attribute attributes
        | _ -> map
      in
      List.fold ~init:Identifier.SerializableMap.empty ~f:slots_attributes body
    in
    (* Merge with decreasing priority. *)
    implicitly_assigned_attributes
    |> Identifier.SerializableMap.merge merge property_attributes
    |> Identifier.SerializableMap.merge merge callable_attributes
    |> Identifier.SerializableMap.merge merge class_attributes
    |> Identifier.SerializableMap.merge merge slots_attributes


  let attributes ?(include_generated_attributes = true) ?(in_test = false) definition =
    let explicit_attributes = explicitly_assigned_attributes definition in
    if not include_generated_attributes then
      explicit_attributes
    else
      let merge _ left right =
        match right with
        | None -> left
        | Some _ -> right
      in
      explicit_attributes
      |> Identifier.SerializableMap.merge merge (implicit_attributes ~in_test definition)


  let update { Record.Class.body = stub; _ } ~definition:({ Record.Class.body; _ } as definition) =
    let updated, undefined =
      let update (updated, undefined) statement =
        match statement with
        | { Node.location; value = Assign ({ Assign.target; _ } as assign) } -> (
            let is_stub = function
              | { Node.value = Assign { Assign.target = stub_target; _ }; _ }
                when Expression.equal target stub_target ->
                  true
              | _ -> false
            in
            match List.find ~f:is_stub stub with
            | Some
                {
                  Node.value =
                    Assign
                      { Assign.annotation; value = { Node.value = Expression.Ellipsis; _ }; _ };
                  _;
                } ->
                let updated_assign =
                  { Node.location; value = Assign { assign with Assign.annotation } }
                in
                ( updated_assign :: updated,
                  List.filter ~f:(fun statement -> not (is_stub statement)) undefined )
            | _ -> statement :: updated, undefined )
        | {
         Node.location;
         value = Define ({ Record.Define.signature = { name; parameters; _ }; _ } as define);
        } -> (
            let is_stub = function
              | {
                  Node.value =
                    Define
                      {
                        signature =
                          { Record.Define.name = stub_name; parameters = stub_parameters; _ };
                        _;
                      };
                  _;
                }
                when Reference.equal name stub_name
                     && List.length parameters = List.length stub_parameters ->
                  true
              | _ -> false
            in
            match List.find ~f:is_stub stub with
            | Some
                {
                  Node.value =
                    Define ({ signature = { parameters; return_annotation; _ }; _ } as stub);
                  _;
                }
              when Define.is_stub stub ->
                let updated_signature = { define.signature with parameters; return_annotation } in
                let updated_define =
                  { Node.location; value = Define { define with signature = updated_signature } }
                in
                ( updated_define :: updated,
                  List.filter ~f:(fun statement -> not (is_stub statement)) undefined )
            | _ -> statement :: updated, undefined )
        | _ -> statement :: updated, undefined
      in
      let stub =
        match stub with
        | [{ Node.value = Expression { Node.value = Ellipsis; _ }; _ }] -> []
        | _ -> stub
      in
      List.fold ~init:([], stub) ~f:update (List.rev body)
    in
    { definition with Record.Class.body = undefined @ updated }


  let has_decorator { decorators; _ } decorator =
    Expression.exists_in_list ~expression_list:decorators decorator


  let is_unit_test { name; _ } =
    let name = Reference.show name in
    String.equal name "unittest.TestCase" || String.equal name "unittest.case.TestCase"


  let is_final definition = has_decorator definition "typing.final"

  let is_abstract { bases; _ } =
    let abstract_metaclass { Expression.Call.Argument.value; _ } =
      match value with
      | {
       Node.value =
         Expression.Name
           (Name.Attribute
             {
               base = { Node.value = Name (Name.Identifier "abc"); _ };
               attribute = "ABCMeta" | "ABC";
               _;
             });
       _;
      } ->
          true
      | _ -> false
    in
    List.exists bases ~f:abstract_metaclass


  let is_protocol { bases; _ } =
    let is_protocol { Expression.Call.Argument.name; value = { Node.value; _ } } =
      match name, value with
      | ( None,
          Expression.Call
            {
              callee =
                {
                  Node.value =
                    Name
                      (Name.Attribute
                        {
                          base =
                            {
                              Node.value =
                                Name
                                  (Name.Attribute
                                    {
                                      base = { Node.value = Name (Name.Identifier typing); _ };
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
            } )
      | ( None,
          Name
            (Name.Attribute
              {
                base = { Node.value = Name (Name.Identifier typing); _ };
                attribute = "Protocol";
                _;
              }) )
        when String.equal typing "typing" || String.equal typing "typing_extensions" ->
          true
      | _ -> false
    in
    List.exists ~f:is_protocol bases
end

module For = struct
  include Record.For

  type t = statement_t Record.For.record [@@deriving compare, eq, sexp, show, hash]

  let preamble { target = { Node.location; _ } as target; iterator; async; _ } =
    let open Expression in
    let value =
      let value =
        let create_call base iterator next =
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
                                 Call
                                   {
                                     callee =
                                       {
                                         Node.location;
                                         value =
                                           Name
                                             (Name.Attribute
                                                { base; attribute = iterator; special = true });
                                       };
                                     arguments = [];
                                   };
                             };
                           attribute = next;
                           special = true;
                         });
                };
              arguments = [];
            }
        in
        if async then
          create_call iterator "__aiter__" "__anext__"
        else
          create_call iterator "__iter__" "__next__"
      in
      if async then
        { Node.location; value = Await (Node.create value ~location) }
      else
        { Node.location; value }
    in
    { Node.location; value = Assign { Assign.target; annotation = None; value; parent = None } }
end

module With = struct
  include Record.With

  type t = statement_t Record.With.record [@@deriving compare, eq, sexp, show, hash]

  let preamble { items; async; _ } =
    let preamble (({ Node.location; _ } as expression), target) =
      target
      >>| (fun target ->
            let open Expression in
            let enter_call =
              let create_call call_name =
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
                                   { base = expression; attribute = call_name; special = true });
                          };
                        arguments = [];
                      };
                }
              in
              if async then
                Node.create ~location (Await (create_call "__aenter__"))
              else
                create_call "__enter__"
            in
            let assign = { Assign.target; annotation = None; value = enter_call; parent = None } in
            Node.create ~location (Assign assign))
      |> Option.value ~default:(Node.create ~location (Expression expression))
    in
    List.map items ~f:preamble
end

module Try = struct
  include Record.Try

  type t = statement_t Record.Try.record [@@deriving compare, eq, sexp, show, hash]

  let preamble { kind; name; _ } =
    let open Expression in
    let assume ~location ~target ~annotation =
      [ {
          Node.location;
          value =
            Assign
              {
                Assign.target;
                annotation = None;
                value = Node.create ~location Ellipsis;
                parent = None;
              };
        };
        {
          Node.location;
          value =
            Assert
              {
                Assert.test =
                  {
                    Node.location;
                    value =
                      Call
                        {
                          callee = { Node.location; value = Name (Name.Identifier "isinstance") };
                          arguments =
                            [ { Call.Argument.name = None; value = target };
                              { Call.Argument.name = None; value = annotation } ];
                        };
                  };
                message = None;
                origin = Assert.Assertion;
              };
        } ]
    in
    match kind, name with
    | Some ({ Node.location; value = Name _; _ } as annotation), Some name ->
        assume ~location ~target:{ Node.location; value = Name (Name.Identifier name) } ~annotation
    | Some { Node.location; value = Tuple values; _ }, Some name ->
        let annotation =
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
                  arguments =
                    [{ Call.Argument.name = None; value = { Node.location; value = Tuple values } }];
                };
          }
        in
        assume ~location ~target:{ Node.location; value = Name (Name.Identifier name) } ~annotation
    | Some ({ Node.location; _ } as expression), _ ->
        (* Insert raw `kind` so that we type check the expression. *)
        [Node.create ~location (Expression expression)]
    | _ -> []
end

let extract_docstring statements =
  (* See PEP 257 for Docstring formatting. The main idea is that we want to get the shortest
   * indentation from line 2 onwards as the indentation of the docstring. *)
  let unindent docstring =
    let indentation line =
      let line_without_indentation = String.lstrip line in
      String.length line - String.length line_without_indentation
    in
    match String.split ~on:'\n' docstring with
    | [] -> docstring
    | first :: rest ->
        let difference =
          List.map rest ~f:indentation |> List.fold ~init:Int.max_value ~f:Int.min
        in
        let rest = List.map rest ~f:(fun s -> String.drop_prefix s difference) in
        String.concat ~sep:"\n" (first :: rest)
  in
  match statements with
  | {
      Node.value =
        Expression { Node.value = Expression.String { Expression.StringLiteral.value; _ }; _ };
      _;
    }
    :: _ ->
      Some (unindent value)
  | _ -> None


module PrettyPrinter = struct
  let pp_decorators formatter = function
    | [] -> ()
    | decorators ->
        Format.fprintf formatter "@[<v>@@(%a)@;@]" Expression.pp_expression_list decorators


  let pp_reference_option formatter = function
    | None -> ()
    | Some reference -> Format.fprintf formatter "%a" Reference.pp reference


  let pp_list formatter pp sep list =
    let rec pp' formatter = function
      | [] -> ()
      | [x] -> Format.fprintf formatter "%a" pp x
      | x :: xs -> Format.fprintf formatter ("%a" ^^ sep ^^ "%a") pp x pp' xs
    in
    pp' formatter list


  let pp_option ?(prefix = "") ?(suffix = "") formatter option pp =
    Option.value_map option ~default:() ~f:(fun value ->
        Format.fprintf formatter "%s%a%s" prefix pp value suffix)


  let pp_expression_option formatter (prefix, option) =
    pp_option ~prefix formatter option Expression.pp


  let pp_async formatter = function
    | true -> Format.fprintf formatter "async@;"
    | false -> ()


  let rec pp_statement_t formatter { Node.value = statement; _ } =
    Format.fprintf formatter "%a" pp_statement statement


  and pp_statement_list formatter = function
    | [] -> ()
    | [statement] -> Format.fprintf formatter "%a" pp_statement_t statement
    | statement :: statement_list ->
        Format.fprintf formatter "%a@;%a" pp_statement_t statement pp_statement_list statement_list


  and pp_assign formatter { Assign.target; annotation; value; parent } =
    Format.fprintf
      formatter
      "%a%a%a = %a"
      pp_reference_option
      parent
      Expression.pp
      target
      pp_expression_option
      (": ", annotation)
      Expression.pp
      value


  and pp_class formatter { Record.Class.name; bases; body; decorators; _ } =
    Format.fprintf
      formatter
      "%a@[<v 2>class %a(%a):@;@[<v>%a@]@;@]"
      pp_decorators
      decorators
      Reference.pp
      name
      Expression.pp_expression_argument_list
      bases
      pp_statement_list
      body


  and pp_define
      formatter
      {
        Define.signature = { name; parameters; decorators; return_annotation; async; parent; _ };
        body;
      }
    =
    let return_annotation =
      match return_annotation with
      | Some annotation -> Format.asprintf " -> %a" Expression.pp annotation
      | _ -> ""
    in
    Format.fprintf
      formatter
      "%a@[<v 2>%adef %a%s%a(%a)%s:@;%a@]@."
      pp_decorators
      decorators
      pp_async
      async
      pp_reference_option
      parent
      (if Option.is_some parent then "#" else "")
      Reference.pp
      name
      Expression.pp_expression_parameter_list
      parameters
      return_annotation
      pp_statement_list
      body


  and pp_statement formatter statement =
    match statement with
    | Assign assign -> Format.fprintf formatter "%a" pp_assign assign
    | Assert { Assert.test; Assert.message; _ } ->
        Format.fprintf
          formatter
          "assert %a, %a"
          Expression.pp
          test
          pp_expression_option
          ("", message)
    | Break -> Format.fprintf formatter "break"
    | Class definition -> Format.fprintf formatter "%a" pp_class definition
    | Continue -> Format.fprintf formatter "continue"
    | Define define -> Format.fprintf formatter "%a" pp_define define
    | Delete expression -> Format.fprintf formatter "del %a" Expression.pp expression
    | Expression expression -> Expression.pp formatter expression
    | For { For.target; iterator; body; orelse; async } ->
        Format.fprintf
          formatter
          "@[<v 2>%afor %a in %a:@;%a@]%a"
          pp_async
          async
          Expression.pp
          target
          Expression.pp
          iterator
          pp_statement_list
          body
          pp_statement_list
          orelse
    | Global globals -> Format.fprintf formatter "global %s" (String.concat globals ~sep:", ")
    | If { If.test; body; orelse } ->
        if List.is_empty orelse then
          Format.fprintf
            formatter
            "@[<v>@[<v 2>if %a:@;%a@]@]@;"
            Expression.pp
            test
            pp_statement_list
            body
        else
          Format.fprintf
            formatter
            "@[<v>@[<v 2>if %a:@;%a@]@]@;@[<v 2>else:@;%a@]"
            Expression.pp
            test
            pp_statement_list
            body
            pp_statement_list
            orelse
    | Import { Import.from; imports } ->
        let pp_import formatter { Import.name; alias } =
          Format.fprintf formatter "%a%a" Reference.pp name pp_reference_option alias
        in
        let pp_imports formatter import_list = pp_list formatter pp_import ", " import_list in
        Format.fprintf formatter "@[<v>%aimport %a@]" pp_reference_option from pp_imports imports
    | Nonlocal nonlocal_list -> pp_list formatter String.pp "," nonlocal_list
    | Pass -> Format.fprintf formatter "%s" "pass"
    | Raise { Raise.expression; _ } ->
        Format.fprintf formatter "raise %a" pp_expression_option ("", expression)
    | Return { Return.expression; _ } ->
        Format.fprintf formatter "return %a" pp_expression_option ("", expression)
    | Try { Record.Try.body; handlers; orelse; finally } ->
        let pp_try_block formatter body =
          Format.fprintf formatter "@[<v 2>try:@;%a@]" pp_statement_list body
        in
        let pp_except_block formatter handlers =
          let pp_as formatter name = pp_option ~prefix:" as " formatter name String.pp in
          let pp_handler formatter { Record.Try.kind; name; handler_body } =
            Format.fprintf
              formatter
              "@[<v 2>except%a%a:@;%a@]"
              pp_expression_option
              (" ", kind)
              pp_as
              name
              pp_statement_list
              handler_body
          in
          let pp_handler_list formatter handler_list =
            pp_list formatter pp_handler "@;" handler_list
          in
          Format.fprintf formatter "%a" pp_handler_list handlers
        in
        let pp_else_block formatter = function
          | [] -> ()
          | orelse -> Format.fprintf formatter "@[<v 2>else:@;%a@]" pp_statement_list orelse
        in
        let pp_finally_block formatter = function
          | [] -> ()
          | finally ->
              Format.fprintf formatter "@[<v 2>finally:@;@[<v>%a@]@]" pp_statement_list finally
        in
        Format.fprintf
          formatter
          "@[<v>%a@;%a@;%a@;%a@]"
          pp_try_block
          body
          pp_except_block
          handlers
          pp_else_block
          orelse
          pp_finally_block
          finally
    | With { Record.With.items; body; async } ->
        let pp_item formatter (expression, expression_option) =
          Format.fprintf
            formatter
            "%a%a"
            Expression.pp
            expression
            pp_expression_option
            (" as ", expression_option)
        in
        let rec pp_item_list formatter = function
          | [] -> ()
          | [item] -> Format.fprintf formatter "%a" pp_item item
          | item :: item_list ->
              Format.fprintf formatter "%a,%a" pp_item item pp_item_list item_list
        in
        Format.fprintf
          formatter
          "@[<v 2>%a with %a:@;%a@]"
          pp_async
          async
          pp_item_list
          items
          pp_statement_list
          body
    | While { While.test; body; orelse } ->
        Format.fprintf
          formatter
          "@[<v 2>while %a:@;%a@]@[<v>%a@]"
          Expression.pp
          test
          pp_statement_list
          body
          pp_statement_list
          orelse
    | Yield expression -> Format.fprintf formatter "yield %a" Expression.pp expression
    | YieldFrom expression -> Format.fprintf formatter "yield from %a" Expression.pp expression


  let pp = pp_statement_t
end

let pp formatter statement = Format.fprintf formatter "%a" PrettyPrinter.pp statement

let show statement = Format.asprintf "%a" pp statement
