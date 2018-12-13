(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre

module Access = Expression.Access
module Argument = Expression.Argument


module Record = struct
  module Define = struct
    type 'statement record = {
      name: Access.t;
      parameters: (Expression.t Parameter.t) list;
      body: 'statement list;
      decorators: Expression.t list;
      docstring: string option;
      return_annotation: Expression.t option;
      async: bool;
      parent: Access.t option; (* The class owning the method. *)
    }
    [@@deriving compare, eq, sexp, show, hash]
  end

  module Class = struct
    type 'statement record = {
      name: Access.t;
      bases: Argument.t list;
      body: 'statement list;
      decorators: Expression.t list;
      docstring: string option;
    }
    [@@deriving compare, eq, sexp, show, hash]
  end

  module For = struct
    type 'statement record = {
      target: Expression.t;
      iterator: Expression.t;
      body: 'statement list;
      orelse: 'statement list;
      async: bool;
    }
    [@@deriving compare, eq, sexp, show, hash]
  end

  module With = struct
    type 'statement record = {
      items: (Expression.t * Expression.t option) list;
      body: 'statement list;
      async: bool;
    }
    [@@deriving compare, eq, sexp, show, hash]
  end

  module Try = struct
    type 'statement handler = {
      kind: Expression.t option;
      name: Identifier.t option;
      handler_body: 'statement list;
    }
    [@@deriving compare, eq, sexp, show, hash]


    type 'statement record = {
      body: 'statement list;
      handlers: 'statement handler list;
      orelse: 'statement list;
      finally: 'statement list;
    }
    [@@deriving compare, eq, sexp, show, hash]
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
  [@@deriving compare, eq, sexp, show, hash]
end



module If = struct
  type 'statement t = {
    test: Expression.t;
    body: 'statement list;
    orelse: 'statement list;
  }
  [@@deriving compare, eq, sexp, show, hash]
end


module Assert = struct
  type t = {
    test: Expression.t;
    message: Expression.t option;
  }
  [@@deriving compare, eq, sexp, show, hash]
end


module Import = struct
  type import = {
    name: Access.t;
    alias: Access.t option;
  }
  [@@deriving compare, eq, sexp, show, hash]


  type t = {
    from: Access.t option;
    imports: import list;
  }
  [@@deriving compare, eq, sexp, show, hash]
end


module Assign = struct
  type t = {
    target: Expression.t;
    annotation: Expression.t option;
    value: Expression.t;
    parent: Access.t option;
  }
  [@@deriving compare, eq, sexp, show, hash]


  let is_static_attribute_initialization { parent; _ } =
    Option.is_some parent
end


module Return = struct
  type t = {
    is_implicit: bool;
    expression: Expression.t option;
  }
  [@@deriving compare, eq, sexp, show, hash]
end


type statement =
  | Assign of Assign.t
  | Assert of Assert.t
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
  | Raise of Expression.t option
  | Return of Return.t
  | Try of t Record.Try.record
  | With of t Record.With.record
  | While of t While.t
  | Yield of Expression.t
  | YieldFrom of Expression.t


and t = statement Node.t
[@@deriving compare, eq, sexp, show, hash]


type statement_t = t
[@@deriving compare, eq, sexp, show, hash]


module Attribute = struct
  type attribute = {
    target: Expression.t;
    annotation: Expression.t option;
    defines: ((statement_t Record.Define.record) list) option;
    value: Expression.t option;
    async: bool;
    setter: bool;
    property: bool;
    primitive: bool;
  }
  [@@deriving compare, eq, sexp, show, hash]


  type t = attribute Node.t
  [@@deriving compare, eq, sexp, show, hash]


  let create
      ~location
      ?(async = false)
      ?(setter = false)
      ?(primitive = false)
      ?(property = false)
      ?value
      ?annotation
      ?defines
      ~target
      () =
    { target; annotation; defines; value; async; setter; property; primitive }
    |> Node.create ~location


  let target ~parent target =
    let open Expression in
    let access = Expression.access target in
    match List.rev access with
    | (Access.Call _) :: ((Access.Identifier _) as access) :: class_name
    | ((Access.Identifier _) as access) :: class_name
      when Access.equal parent (List.rev class_name) ->
        Some { target with Node.value = Access [access] }
    | _ ->
        None
end


module Define = struct
  include Record.Define


  type t = statement_t Record.Define.record
  [@@deriving compare, eq, sexp, show, hash]


  let create_toplevel ~qualifier ~statements =
    {
      name = qualifier @ (Access.create "$toplevel");
      parameters = [];
      body = statements;
      decorators = [];
      docstring = None;
      return_annotation = None;
      async = false;
      parent = None;
    }


  let create_class_toplevel ~qualifier ~statements =
    {
      name = qualifier @ (Access.create "$class_toplevel");
      parameters = [];
      body = statements;
      decorators = [];
      docstring = None;
      return_annotation = None;
      async = false;
      parent = Some qualifier;
    }


  let unqualified_name { name; parent; _ } =
    match parent with
    | Some parent ->
        let is_qualified =
          List.is_prefix
            name
            ~prefix:parent
            ~equal:(Access.equal_access Expression.equal)
        in
        if is_qualified then List.drop name (List.length parent) else name
    | _ ->
        name


  let self_identifier { parameters; _ } =
    match parameters with
    | { Node.value = { Parameter.name; _ }; _ } :: _ -> name
    | _ -> Identifier.create "self"


  let is_method { parent; _ } =
    Option.is_some parent


  let has_decorator { decorators; _ } decorator =
    Expression.exists_in_list ~expression_list:decorators decorator


  let is_coroutine define =
    has_decorator define "asyncio.coroutines.coroutine"


  let is_abstract_method define =
    has_decorator define "abstractmethod" ||
    has_decorator define "abc.abstractmethod" ||
    has_decorator define "abstractproperty" ||
    has_decorator define "abc.abstractproperty"


  let is_overloaded_method define =
    has_decorator define "overload" ||
    has_decorator define "typing.overload"


  let is_static_method define =
    has_decorator define "staticmethod"


  let is_dunder_method { name; _ } =
    match List.last name with
    | Some dunder ->
        let name = Access.show [dunder] in
        String.is_prefix ~prefix:"__" name &&
        String.is_suffix ~suffix:"__" name
    | _ ->
        false


  let is_class_method ({ parent; _ } as define) =
    Option.is_some parent &&
    (Set.exists Recognized.classmethod_decorators ~f:(has_decorator define) ||
     List.mem
       ["__init_subclass__"; "__new__"; "__class_getitem__"]
       (Access.show (unqualified_name define))
       ~equal:String.equal)


  let is_class_property ({ parent; _ } as define) =
    Option.is_some parent &&
    Set.exists Recognized.classproperty_decorators ~f:(has_decorator define)


  let is_constructor ?(in_test = false) { name; parent; _ } =
    let name =
      match List.last name with
      | Some (Access.Identifier name) -> Identifier.show name
      | _ -> "$unknown"
    in
    if Option.is_none parent then
      false
    else
      name = "__init__" ||
      name = "__enter__" ||
      (in_test &&
       List.mem
         ~equal:String.equal
         ["async_setUp"; "setUp"; "_setup"; "_async_setup"; "with_context"]
         name)


  let is_property_setter ({ name; _ } as define) =
    let last =
      List.last name
      >>| (fun last -> [last])
      |> Option.value ~default:[]
    in
    has_decorator define ((Access.show last) ^ ".setter")


  let is_untyped { return_annotation; _ } =
    Option.is_none return_annotation


  let is_async { async; _ } =
    async


  let is_toplevel { name; _ } =
    match List.last name with
    | Some (Access.Identifier toplevel) when Identifier.show toplevel = "$toplevel" -> true
    | _ -> false


  let is_class_toplevel { name; _ } =
    match List.last name with
    | Some (Access.Identifier toplevel) when Identifier.show toplevel = "$class_toplevel" -> true
    | _ -> false


  let contains_call { body; _ } name =
    let matches = function
      | {
        Node.value = Expression {
            Node.value = Expression.Access [
                Access.Identifier identifier;
                Access.Call _;
              ];
            _;
          };
        _;
      } when Identifier.show identifier = name ->
          true
      | _ ->
          false
    in
    List.exists ~f:matches body


  let is_stub { body; _ } =
    match List.rev body with
    | { Node.value = Expression { Node.value = Expression.Ellipses; _ }; _ } :: _
    | _ :: { Node.value = Expression { Node.value = Expression.Ellipses; _ }; _ } :: _ ->
        true
    | _ ->
        false


  let dump define =
    contains_call define "pyre_dump"


  let dump_cfg define =
    contains_call define "pyre_dump_cfg"


  let implicit_attributes
      ({ body; parameters; _ } as define)
      ~definition:{ Record.Class.body = definition_body; _ }: Attribute.t Access.SerializableMap.t =
    let open Expression in
    let parameter_annotations =
      let add_parameter map = function
        | { Node.value = { Parameter.name; annotation = Some annotation; _ }; _ } ->
            Access.SerializableMap.set
              map
              ~key:(Access.create_from_identifiers [name])
              ~data:annotation
        | _ ->
            map
      in
      List.fold ~init:Access.SerializableMap.empty ~f:add_parameter parameters
    in
    let attribute map { Node.value; _ } =
      match value with
      | Assign { Assign.target; annotation; value; _ } ->
          let attribute ~map ~target:({ Node.location; _ } as target) ~annotation =
            match target with
            | ({
                Node.value = Access ((Access.Identifier self) :: ([_] as access));
                _;
              } as target) when Identifier.equal self (self_identifier define) ->
                let attribute =
                  let target = { target with Node.value = Access access } in
                  Attribute.create ~primitive:true ~location ~target ?annotation ~value ()
                in
                let update = function
                  | Some attributes -> Some (attribute :: attributes)
                  | None -> Some [attribute]
                in
                Access.SerializableMap.update access update map
            | _ ->
                map
          in
          begin
            match target with
            | { Node.value = Access _; _ } as target ->
                let annotation =
                  match annotation, value with
                  | None, { Node.value = Access access; _ } ->
                      Access.SerializableMap.find_opt access parameter_annotations
                  | _ ->
                      annotation
                in
                attribute ~map ~target ~annotation
            | { Node.value = Tuple targets; _ } ->
                List.fold
                  ~init:map
                  ~f:(fun map target -> attribute ~map ~target ~annotation)
                  targets
            | _ ->
                map
          end
      | _ ->
          map
    in
    let merge_attributes = function
      | [attribute] ->
          attribute
      | ({ Node.location; value = attribute } :: _) as attributes ->
          let annotation =
            let annotation = function
              | { Node.value = { Attribute.annotation = Some annotation; _ }; _ } -> Some annotation
              | _ -> None
            in
            match List.filter_map ~f:annotation attributes with
            | [] ->
                None
            | ({ Node.location; _ } as annotation) :: annotations ->
                if List.for_all ~f:(Expression.equal annotation) annotations then
                  Some annotation
                else
                  let argument =
                    {
                      Argument.name = None;
                      value = Node.create_with_default_location (Tuple (annotation :: annotations));
                    }
                  in
                  Some {
                    Node.location;
                    value = Access [
                        Access.Identifier (Identifier.create "typing");
                        Access.Identifier (Identifier.create "Union");
                        Access.Identifier (Identifier.create "__getitem__");
                        Access.Call (Node.create_with_default_location [argument]);
                      ];
                  }
          in
          { Node.location; value = { attribute with Attribute.annotation }}
      | [] ->
          failwith "Unpossible!"
    in
    let rec expand_statements body =
      (* Can't use `Visit` module due to circularity :( *)
      let expand_statement ({ Node.value; _ } as statement) =
        match value with
        | If { If.body; orelse; _ }
        | For { RecordFor.body; orelse; _ }
        | While { While.body; orelse; _ } ->
            (expand_statements body) @ (expand_statements orelse)
        | Try { RecordTry.body; orelse; finally; _ } ->
            (expand_statements body) @ (expand_statements orelse) @ (expand_statements finally)
        | With { RecordWith.body; _ } ->
            expand_statements body
        | Expression {
            Node.value =
              Access [
                Access.Identifier self;
                (Access.Identifier _) as name;
                Access.Call _;
              ];
            _;
          } when Identifier.equal self (self_identifier define) ->
            (* Look for method in class definition. *)
            let inline = function
              | { Node.value = Define { name = callee; body; parent = Some parent; _ }; _ }
                when Access.equal callee (parent @ [name]) ->
                  Some body
              | _ ->
                  None
            in
            List.find_map ~f:inline definition_body
            |> Option.value ~default:[statement]
        | _ ->
            [statement]
      in
      List.concat_map ~f:expand_statement body
    in
    expand_statements body
    |> List.fold ~init:Access.SerializableMap.empty ~f:attribute
    |> Access.SerializableMap.map merge_attributes


  let property_attribute ~location ({ name; return_annotation; parameters; parent; _ } as define) =
    let open Expression in
    let attribute ?(setter = false) annotation =
      parent
      >>= (fun parent -> Attribute.target ~parent (Node.create ~location (Access name)))
      >>| fun target ->
      Attribute.create
        ~location
        ~setter
        ~target
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
          | Some ({ Node.location; value = Access _ } as access) ->
              let argument =
                {
                  Argument.name = None;
                  value = access;
                }
              in
              Some {
                Node.location;
                value = Access [
                    Access.Identifier (Identifier.create "typing");
                    Access.Identifier (Identifier.create "ClassVar");
                    Access.Identifier (Identifier.create "__getitem__");
                    Access.Call (Node.create_with_default_location [argument]);
                  ];
              }
          | _ ->
              None
        in
        attribute return_annotation
    | Some _ ->
        attribute return_annotation
    | None ->
        begin
          match is_property_setter define, parameters with
          | true, _ :: { Node.value = { Parameter.annotation; _ }; _ } :: _ ->
              attribute ~setter:true annotation
          | _ ->
              None
        end
end


let assume ({ Node.location; _ } as test) =
  {
    Node.location;
    value = Assert { Assert.test; message = None };
  }


(* Naive assumptions *)
let terminates body =
  let find_terminator = function
    | { Node.value = Return _; _ }
    | { Node.value = Raise _; _ }
    | { Node.value = Continue; _ } -> true
    | _ -> false
  in
  Option.is_some (List.find ~f:find_terminator body)


module Class = struct
  include Record.Class


  type t = statement_t Record.Class.record
  [@@deriving compare, eq, sexp, show, hash]


  let constructors ?(in_test = false) { Record.Class.body; _ } =
    let constructor = function
      | { Node.value = Define define; _ } when Define.is_constructor ~in_test define ->
          Some define
      | _ ->
          None
    in
    List.filter_map ~f:constructor body


  let defines { Record.Class.body; _ } =
    let define = function
      | { Node.value = Define define; _ } ->
          Some define
      | _ ->
          None
    in
    List.filter_map ~f:define body


  let find_define { Record.Class.body; _ } ~method_name =
    let is_define = function
      | { Node.value = Define ({ name; _ } as define); location} ->
          begin
            match List.last name with
            | Some (Access.Identifier name)
              when Identifier.equal name method_name ->
                Some { Node.value = define; location }
            | _ ->
                None
          end
      | _ ->
          None
    in
    List.filter_map ~f:is_define body |> List.hd


  let explicitly_assigned_attributes { Record.Class.name; body; _ } =
    let assigned_attributes map { Node.location; value } =
      let open Expression in
      match value with
      (* Handle multiple assignments on same line *)
      | Assign {
          Assign.target = { Node.value = Tuple targets; _ };
          value = { Node.value = Tuple values; _ };
          _;
        } ->
          let add_attribute map target value =
            Attribute.target ~parent:name target
            >>| (fun target ->
                let access = Expression.access target in
                let attribute =
                  Attribute.create
                    ~primitive:true
                    ~location
                    ~target
                    ~value
                    ()
                in
                Access.SerializableMap.set map ~key:access ~data:attribute)
            |> Option.value ~default:map
          in
          if List.length targets = List.length values then
            List.fold2_exn ~init:map ~f:add_attribute targets values
          else
            map
      | Assign {
          Assign.target = { Node.value = Tuple targets; _ };
          value = { Node.value = Access values; location } as value;
          _;
        } ->
          let add_attribute index map target =
            Attribute.target ~parent:name target
            >>| (fun target ->
                let access = Expression.access target in
                let value =
                  let get_item =
                    let index = Node.create ~location (Integer index) in
                    [
                      Access.Identifier (Identifier.create "__getitem__");
                      Access.Call
                        (Node.create ~location [{ Argument.name = None; value = index }]);
                    ]
                  in
                  { value with Node.value = Access (values @ get_item) }
                in
                let attribute =
                  Attribute.create
                    ~primitive:true
                    ~location
                    ~target
                    ~value
                    ()
                in
                Access.SerializableMap.set map ~key:access ~data:attribute)
            |> Option.value ~default:map
          in
          List.foldi ~init:map ~f:add_attribute targets
      | Assign { Assign.target; annotation; value; _ } ->
          Attribute.target ~parent:name target
          >>| (fun target ->
              let access = Expression.access target in
              let attribute =
                Attribute.create
                  ~primitive:true
                  ~location
                  ~target
                  ?annotation
                  ~value
                  ()
              in
              Access.SerializableMap.set
                map
                ~key:access
                ~data:attribute)
          |> Option.value ~default:map
      | _ ->
          map
    in
    List.fold ~init:Access.SerializableMap.empty ~f:assigned_attributes body

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
      |> List.fold ~init:Access.SerializableMap.empty ~f:(Access.SerializableMap.merge merge)
    in
    let property_attributes =
      let property_attributes map = function
        | { Node.location; value = Define define } ->
            begin
              match Define.property_attribute ~location define with
              | Some ({
                  Node.value =
                    ({
                      Attribute.target = { Node.value = Expression.Access ([_] as access); _ };
                      setter = new_setter;
                      annotation = new_annotation;
                      _;
                    } as attribute);
                  _;
                } as attribute_node) ->
                  let merged_attribute =
                    match Access.SerializableMap.find_opt access map, new_setter with
                    | Some { Node.value = { Attribute.setter = true; annotation; _ }; _ },
                      false ->
                        {
                          attribute with
                          Attribute.annotation;
                          value = new_annotation;
                          setter = true
                        }
                        |> (fun edited -> { attribute_node with Node.value = edited })
                    | Some { Node.value = { Attribute.setter = false; annotation; _ }; _ },
                      true ->
                        {
                          attribute with
                          Attribute.annotation = new_annotation;
                          value = annotation;
                          setter = true
                        }
                        |> (fun edited -> { attribute_node with Node.value = edited })
                    | _ ->
                        attribute_node
                  in
                  Access.SerializableMap.set map ~key:access ~data:merged_attribute
              | _ ->
                  map
            end
        | _ ->
            map
      in
      List.fold ~init:Access.SerializableMap.empty ~f:property_attributes body
    in
    let callable_attributes =
      let callable_attributes map { Node.location; value } =
        match value with
        | Define ({ Define.name = target; _ } as define) ->
            Attribute.target ~parent:name (Node.create ~location (Expression.Access target))
            >>| (fun target ->
                let name = Expression.access target in
                let attribute =
                  match Access.SerializableMap.find_opt name map with
                  | Some { Node.value = { Attribute.defines = Some defines; _ }; _ } ->
                      Attribute.create
                        ~location
                        ~target
                        ~defines:({ define with Define.body = [] } :: defines)
                        ()
                  | _ ->
                      Attribute.create
                        ~location
                        ~target
                        ~defines:[{ define with Define.body = [] }]
                        ()
                in
                Access.SerializableMap.set map ~key:name ~data:attribute)
            |> Option.value ~default:map
        | _ ->
            map
      in
      List.fold ~init:Access.SerializableMap.empty ~f:callable_attributes body
    in
    let class_attributes =
      let callable_attributes map { Node.location; value } =
        match value with
        | Class { Record.Class.name; _ } when not (List.is_empty name) ->
            let open Expression in
            let annotation =
              let meta_annotation =
                let argument = { Argument.name = None; value = Access.expression name } in
                Node.create
                  ~location
                  (Access [
                      Access.Identifier (Identifier.create "typing");
                      Access.Identifier (Identifier.create "Type");
                      Access.Identifier (Identifier.create "__getitem__");
                      Access.Call (Node.create_with_default_location [argument]);
                    ])
              in
              let argument = { Argument.name = None; value = meta_annotation } in
              Node.create
                ~location
                (Access [
                    Access.Identifier (Identifier.create "typing");
                    Access.Identifier (Identifier.create "ClassVar");
                    Access.Identifier (Identifier.create "__getitem__");
                    Access.Call (Node.create_with_default_location [argument]);
                  ])
            in
            Access.SerializableMap.set
              map
              ~key:name
              ~data:(
                Attribute.create
                  ~location
                  ~target:(Node.create ~location (Expression.Access [List.last_exn name]))
                  ~annotation
                  ())
        | _ ->
            map
      in
      List.fold ~init:Access.SerializableMap.empty ~f:callable_attributes body
    in
    let slots_attributes =
      let slots_attributes map { Node.value; _ } =
        let open Expression in
        let is_slots access =
          match List.last access with
          | Some (Access.Identifier identifier)
            when (Identifier.show identifier) = "__slots__" ->
              true
          | _ ->
              false
        in
        match value with
        | Assign {
            Assign.target = { Node.value = Access access; _ };
            value = { Node.value = List attributes; location };
            _;
          } when is_slots access ->
            let add_attribute map { Node.value; _ } =
              match value with
              | String { StringLiteral.value; _ } ->
                  let access = Access.create value in
                  Attribute.create
                    ~location
                    ~target:
                      (Node.create ~location (Expression.Access access))
                    ()
                  |> fun attribute -> Access.SerializableMap.set map ~key:access ~data:attribute
              | _ ->
                  map
            in
            List.fold ~init:map ~f:add_attribute attributes
        | _ ->
            map
      in
      List.fold ~init:Access.SerializableMap.empty ~f:slots_attributes body
    in
    (* Merge with decreasing priority. *)
    implicitly_assigned_attributes
    |> Access.SerializableMap.merge merge property_attributes
    |> Access.SerializableMap.merge merge callable_attributes
    |> Access.SerializableMap.merge merge class_attributes
    |> Access.SerializableMap.merge merge slots_attributes


  let attributes
      ?(include_generated_attributes = true)
      ?(in_test = false)
      definition =

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
      |> Access.SerializableMap.merge merge (implicit_attributes ~in_test definition)


  let update
      { Record.Class.body = stub; _ }
      ~definition:({ Record.Class.body; _ } as definition) =
    let updated, undefined =
      let update (updated, undefined) statement =
        match statement with
        | { Node.location; value = Assign ({ Assign.target; _ } as assign)} ->
            begin
              let is_stub = function
                | { Node.value = Assign { Assign.target = stub_target; _; }; _; }
                  when Expression.equal target stub_target ->
                    true
                | _ ->
                    false
              in
              match List.find ~f:is_stub stub with
              | Some {
                  Node.value = Assign {
                      Assign.annotation;
                      value = { Node.value = Expression.Ellipses; _ };
                      _;
                    };
                  _;
                } ->
                  let updated_assign =
                    {
                      Node.location;
                      value = Assign { assign with Assign.annotation }
                    }
                  in
                  updated_assign :: updated,
                  (List.filter ~f:(fun statement -> not (is_stub statement)) undefined)
              | _ ->
                  statement :: updated, undefined
            end
        | { Node.location; value = Define ({ Record.Define.name; parameters; _ } as define)} ->
            begin
              let is_stub = function
                | {
                  Node.value = Define {
                      Record.Define.name = stub_name;
                      parameters = stub_parameters;
                      _;
                    };
                  _;
                } when Access.equal name stub_name &&
                       List.length parameters = List.length stub_parameters ->
                    true
                | _ ->
                    false
              in
              match List.find ~f:is_stub stub with
              | Some {
                  Node.value = Define ({
                      Define.parameters;
                      return_annotation;
                      _;
                    } as stub);
                  _;
                } when Define.is_stub stub ->
                  let updated_define =
                    {
                      Node.location;
                      value = Define { define with Define.parameters; return_annotation }
                    }
                  in
                  updated_define :: updated,
                  (List.filter ~f:(fun statement -> not (is_stub statement)) undefined)
              | _ ->
                  statement :: updated, undefined
            end
        | _ ->
            statement :: updated, undefined
      in
      let stub = match stub with
        | [{ Node.value = Expression { Node.value = Ellipses; _ }; _ }] -> []
        | _ -> stub
      in
      List.fold ~init:([], stub) ~f:update (List.rev body)
    in
    { definition with Record.Class.body = undefined @ updated }


  let has_decorator { decorators; _ } decorator =
    Expression.exists_in_list ~expression_list:decorators decorator
end


module For = struct
  include Record.For


  type t = statement_t Record.For.record
  [@@deriving compare, eq, sexp, show, hash]


  let preamble
      {
        target = { Node.location; _ } as target;
        iterator = { Node.value; _ };
        async;
        _;
      } =
    let open Expression in
    let value =
      let next =
        if async then
          (Access.call ~name:"__aiter__" ~location ()) @
          (Access.call ~name: "__anext__" ~location ())
        else
          (Access.call ~name:"__iter__" ~location ()) @
          (Access.call ~name: "__next__" ~location ())
      in
      begin
        match value with
        | Access access ->
            access @ next
        | expression ->
            [Access.Expression (Node.create_with_default_location expression)] @ next
      end
    in
    let value =
      if async then
        { Node.location; value = Await (Node.create (Access value) ~location) }
      else
        { Node.location; value = Access value }
    in
    {
      Node.location;
      value = Assign {
          Assign.target;
          annotation = None;
          value;
          parent = None;
        }
    }
end


module With = struct
  include Record.With


  type t = statement_t Record.With.record
  [@@deriving compare, eq, sexp, show, hash]


  let preamble { items; async; _ } =
    let preamble ({ Node.location; _ } as expression, target) =
      (target
       >>| fun target ->
       let open Expression in
       let enter_call =
         let base_call =
           let enter_call_name =
             if async then
               "__aenter__"
             else
               "__enter__"
           in
           Access
             ((Expression.access expression) @ (Access.call ~name:enter_call_name ~location ()))
           |> Node.create ~location
         in
         if async then
           Node.create ~location (Await base_call)
         else
           base_call
       in
       let assign =
         {
           Assign.target;
           annotation = None;
           value = enter_call;
           parent = None;
         }
       in
       Node.create ~location (Assign assign))
      |> Option.value ~default:(Node.create ~location (Expression expression))
    in
    List.map items ~f:preamble
end


module Try = struct
  include Record.Try


  type t = statement_t Record.Try.record
  [@@deriving compare, eq, sexp, show, hash]


  let preamble { kind; name; _ } =
    let open Expression in
    let name =
      name
      >>| Identifier.show
      >>| Access.create
    in
    let assume ~location ~target ~annotation =
      {
        Node.location;
        value = Assign {
            Assign.target;
            annotation = Some annotation;
            value = Node.create Ellipses ~location;
            parent = None;
          }
      }
    in
    match kind, name with
    | Some ({ Node.location; value = Access _; _ } as annotation), Some name ->
        [assume ~location ~target:{ Node.location; value = Access name } ~annotation]
    | Some { Node.location; value = Tuple values; _ }, Some name ->
        let annotation =
          let get_item =
            let tuple =
              Tuple values
              |> Node.create ~location
            in
            Access.call
              ~arguments:[{ Argument.name = None; value = tuple }]
              ~location
              ~name:"__getitem__"
              ()
          in
          {
            Node.location;
            value = Access ((Access.create "typing.Union") @ get_item);
          }
        in
        [assume ~location ~target:{ Node.location; value = Access name } ~annotation]
    | Some ({ Node.location; _ } as expression), _ ->
        (* Insert raw `kind` so that we type check the expression. *)
        [Node.create ~location (Expression expression)]
    | _ ->
        []
end


let extract_docstring statements =
  (* See PEP 257 for Docstring formatting. The main idea is that we want to get the shortest
   * indentation from line 2 onwards as the indentation of the docstring. *)
  let unindent docstring =
    let indentation line =
      let line_without_indentation = String.lstrip line in
      (String.length line) - (String.length line_without_indentation) in
    match String.split ~on:'\n' docstring with
    | [] -> docstring
    | first :: rest ->
        let difference =
          List.map rest ~f:indentation
          |> List.fold ~init:Int.max_value ~f:Int.min
        in
        let rest = List.map rest ~f:(fun s -> String.drop_prefix s difference) in
        String.concat ~sep:"\n" (first::rest)
  in
  match statements with
  | {
    Node.value = Expression {
        Node.value = Expression.String { Expression.StringLiteral.value; _ };
        _;
      };
    _;
  } :: _ ->
      Some (unindent value)
  | _ ->
      None


module PrettyPrinter = struct
  let pp_decorators formatter =
    function
    | [] -> ()
    | decorators ->
        Format.fprintf
          formatter
          "@[<v>@@(%a)@;@]"
          Expression.pp_expression_list decorators


  let pp_access_list_option formatter =
    function
    | None -> ()
    | Some access_list ->
        Format.fprintf
          formatter
          "@[%a@]"
          Expression.pp_expression_access_list access_list


  let pp_access_list formatter =
    function
    | [] -> ()
    | access_list ->
        Format.fprintf
          formatter
          "@[%a@]"
          Expression.pp_expression_access_list access_list


  let pp_list formatter pp sep list =
    let rec pp' formatter =
      function
      | [] -> ()
      | x :: [] -> Format.fprintf formatter "%a" pp x
      | x :: xs -> Format.fprintf formatter ("%a"^^sep^^"%a") pp x pp' xs
    in
    pp' formatter list


  let pp_option formatter option pp =
    Option.value_map option ~default:() ~f:(Format.fprintf formatter "%a" pp)


  let pp_option_with_prefix formatter (prefix,option) pp =
    Option.value_map
      option
      ~default:()
      ~f:(Format.fprintf formatter (prefix^^"%a") pp)


  let pp_expression_option formatter (prefix,option) =
    pp_option_with_prefix formatter (prefix,option) Expression.pp


  let pp_async formatter =
    function
    | true -> Format.fprintf formatter "async@;"
    | false -> ()


  let rec pp_statement_t formatter { Node.value = statement; _ } =
    Format.fprintf formatter "%a" pp_statement statement


  and pp_statement_list formatter =
    function
    | [] -> ()
    | statement :: [] -> Format.fprintf formatter "%a" pp_statement_t statement
    | statement :: statement_list ->
        Format.fprintf
          formatter "%a@;%a"
          pp_statement_t statement
          pp_statement_list statement_list


  and pp_assign formatter { Assign.target; annotation; value; parent } =
    Format.fprintf
      formatter
      "%a%a%a = %a"
      pp_access_list_option parent
      Expression.pp target
      pp_expression_option (": ", annotation)
      Expression.pp value


  and pp_class formatter { Record.Class.name; bases; body; decorators; _ } =
    Format.fprintf
      formatter
      "%a@[<v 2>class %a(%a):@;@[<v>%a@]@;@]"
      pp_decorators decorators
      pp_access_list name
      Expression.pp_expression_argument_list bases
      pp_statement_list body


  and pp_define
      formatter
      { Define.name; parameters; body; decorators; return_annotation; async; parent; _ } =
    let return_annotation =
      match return_annotation with
      | Some annotation -> Format.asprintf " -> %a" Expression.pp annotation
      | _ -> ""
    in
    Format.fprintf
      formatter
      "%a@[<v 2>%adef %a#%a(%a)%s:@;%a@]@."
      pp_decorators decorators
      pp_async async
      pp_access_list_option parent
      pp_access_list name
      Expression.pp_expression_parameter_list parameters
      return_annotation
      pp_statement_list body


  and pp_statement formatter statement =
    match statement with
    | Assign assign ->
        Format.fprintf
          formatter
          "%a"
          pp_assign assign

    | Assert { Assert.test; Assert.message } ->
        Format.fprintf
          formatter
          "assert %a, %a"
          Expression.pp test
          pp_expression_option ("", message)

    | Break ->
        Format.fprintf formatter "break"

    | Class definition ->
        Format.fprintf formatter "%a" pp_class definition

    | Continue ->
        Format.fprintf formatter "continue"

    | Define define ->
        Format.fprintf formatter "%a" pp_define define

    | Delete expression ->
        Format.fprintf formatter "del %a" Expression.pp expression

    | Expression expression ->
        Expression.pp formatter expression

    | For { For.target; iterator; body; orelse; async } ->
        Format.fprintf
          formatter
          "@[<v 2>%afor %a in %a:@;%a@]%a"
          pp_async async
          Expression.pp target
          Expression.pp iterator
          pp_statement_list body
          pp_statement_list orelse

    | Global global_list ->
        pp_list formatter Identifier.pp "," global_list

    | If { If.test; body; orelse } ->
        Format.fprintf
          formatter
          "@[<v>@[<v 2>if %a:@;%a@]@;@[<v 2>else:@;%a@]@]"
          Expression.pp test
          pp_statement_list body
          pp_statement_list orelse

    | Import { Import.from; imports } ->
        let pp_from formatter access_list =
          pp_option_with_prefix formatter ("from ", access_list) pp_access_list
        in
        let pp_import formatter { Import.name; alias } =
          let pp_alias_option formatter access_list =
            pp_option_with_prefix formatter (" as ", access_list) pp_access_list
          in
          Format.fprintf
            formatter
            "%a%a"
            pp_access_list name
            pp_alias_option alias
        in
        let pp_imports formatter import_list =
          pp_list formatter pp_import ", " import_list
        in
        Format.fprintf
          formatter
          "@[<v>%a import %a@]"
          pp_from from
          pp_imports imports

    | Nonlocal nonlocal_list ->
        pp_list formatter Identifier.pp "," nonlocal_list

    | Pass ->
        Format.fprintf formatter "%s" "pass"

    | Raise expression ->
        Format.fprintf
          formatter
          "raise %a"
          pp_expression_option ("", expression)

    | Return { Return.expression; _ } ->
        Format.fprintf
          formatter
          "return %a"
          pp_expression_option ("", expression)

    | Try { Record.Try.body; handlers; orelse; finally } ->
        let pp_try_block formatter body =
          Format.fprintf
            formatter
            "@[<v 2>try:@;%a@]"
            pp_statement_list body
        in
        let pp_except_block formatter handlers =
          let pp_as formatter name =
            pp_option_with_prefix formatter (" as ", name) Identifier.pp
          in
          let pp_handler formatter { Record.Try.kind; name; handler_body } =
            Format.fprintf
              formatter
              "@[<v 2>except%a%a:@;%a@]"
              pp_expression_option (" ", kind)
              pp_as name
              pp_statement_list handler_body
          in
          let pp_handler_list formatter handler_list =
            pp_list formatter pp_handler "@;" handler_list
          in
          Format.fprintf
            formatter
            "%a"
            pp_handler_list handlers
        in
        let pp_else_block formatter =
          function
          | [] -> ()
          | orelse ->
              Format.fprintf
                formatter
                "@[<v 2>else:@;%a@]"
                pp_statement_list orelse
        in
        let pp_finally_block formatter =
          function
          | [] -> ()
          | finally ->
              Format.fprintf
                formatter
                "@[<v 2>finally:@;@[<v>%a@]@]"
                pp_statement_list finally
        in
        Format.fprintf
          formatter
          "@[<v>%a@;%a@;%a@;%a@]"
          pp_try_block body
          pp_except_block handlers
          pp_else_block orelse
          pp_finally_block finally

    | With { Record.With.items; body; async } ->
        let pp_item formatter (expression, expression_option) =
          Format.fprintf
            formatter
            "%a%a"
            Expression.pp expression
            pp_expression_option (" as ", expression_option)
        in
        let rec pp_item_list formatter =
          function
          | [] -> ()
          | item :: [] -> Format.fprintf formatter "%a" pp_item item
          | item :: item_list ->
              Format.fprintf formatter "%a,%a" pp_item item pp_item_list item_list
        in
        Format.fprintf
          formatter
          "@[<v 2>%a with %a:@;%a@]"
          pp_async async
          pp_item_list items
          pp_statement_list body

    | While { While.test; body; orelse } ->
        Format.fprintf
          formatter
          "@[<v 2>while %a:@;%a@]@[<v>%a@]"
          Expression.pp test
          pp_statement_list body
          pp_statement_list orelse

    | Yield expression -> Format.fprintf formatter "yield %a" Expression.pp expression
    | YieldFrom expression -> Format.fprintf formatter "yield from %a" Expression.pp expression


  let pp = pp_statement_t
end


let pp formatter statement =
  Format.fprintf
    formatter
    "%a"
    PrettyPrinter.pp statement


let show statement =
  Format.asprintf "%a" pp statement
