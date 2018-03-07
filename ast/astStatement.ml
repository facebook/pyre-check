(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre

module Expression = AstExpression
module Parameter = AstParameter
module Argument = AstArgument
module Identifier = AstIdentifier
module Location = AstLocation
module Node = AstNode


module Record = struct
  module Define = struct
    type 'statement record = {
      name: Expression.Access.t;
      parameters: (Expression.t Parameter.t) list;
      body: 'statement list;
      decorators: Expression.t list;
      docstring: string option;
      return_annotation: Expression.t option;
      async: bool;
      generated: bool;
      parent: Expression.Access.t option; (* The class owning the method. *)
    }
    [@@deriving compare, eq, sexp, show, hash]
  end

  module Class = struct
    type 'statement record = {
      name: Expression.Access.t;
      bases: (Expression.t Argument.t) list;
      body: 'statement list;
      decorators: Expression.t list;
      docstring: string option;
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
end


(* Not sure why the OCaml compiler hates me... *)
module RecordWith = Record.With


module For = struct
  type 'statement t = {
    target: Expression.t;
    iterator: Expression.t;
    body: 'statement list;
    orelse: 'statement list;
    async: bool;
  }
  [@@deriving compare, eq, sexp, show, hash]
end


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


module Try = struct
  type 'statement handler = {
    kind: Expression.t option;
    name: Identifier.t option;
    handler_body: 'statement list;
  }
  [@@deriving compare, eq, sexp, show, hash]


  type 'statement t = {
    body: 'statement list;
    handlers: 'statement handler list;
    orelse: 'statement list;
    finally: 'statement list;
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
    name: Expression.Access.t;
    alias: Expression.Access.t option;
  }
  [@@deriving compare, eq, sexp, show, hash]


  type t = {
    from: Expression.Access.t option;
    imports: import list;
  }
  [@@deriving compare, eq, sexp, show, hash]
end


module Assign = struct
  type t = {
    target: Expression.t;
    annotation: Expression.t option;
    value: Expression.t option;
    compound: Expression.BinaryOperator.operator option;
    parent: Expression.Access.t option;
  }
  [@@deriving compare, eq, sexp, show, hash]


  let is_static_attribute_initialization { parent; _ } =
    Option.is_some parent
end

module Attribute = struct
  type attribute = {
    async: bool;
    assign: Assign.t;
  }
  [@@deriving compare, eq, sexp, show, hash]

  type t = attribute Node.t
  [@@deriving compare, eq, sexp, show, hash]


  let create ~location ~async ~assign =
    Node.create ~location { async; assign }

  let create_from_node { Node.location; value = assign } =
    create ~location ~async:false ~assign
end

module Stub = struct
  type 'statement t =
    | Assign of Assign.t
    | Class of 'statement Record.Class.record
    | Define of 'statement Record.Define.record
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
  | For of t For.t
  | Global of Identifier.t list
  | If of t If.t
  | Import of Import.t
  | Nonlocal of Identifier.t list
  | Pass
  | Raise of Expression.t option
  | Return of Expression.t option
  | Stub of t Stub.t
  | Try of t Try.t
  | With of t Record.With.record
  | While of t While.t
  | Yield of Expression.t
  | YieldFrom of Expression.t


and t = statement Node.t
[@@deriving compare, eq, sexp, show, hash]


type statement_node = t
[@@deriving compare, eq, sexp, show, hash]


module Define = struct
  include Record.Define


  type t = statement_node Record.Define.record
  [@@deriving compare, eq, sexp, show, hash]


  let create_toplevel statements =
    {
      name = Expression.Access.create "$toplevel";
      parameters = [];
      body = statements;
      decorators = [];
      docstring = None;
      return_annotation = None;
      async = false;
      generated = false;
      parent = None;
    }


  let is_method { name; parent; _ } =
    Option.is_some parent && List.length name = 1


  let has_decorator { decorators; _ } decorator =
    let open Expression in
    let rec is_decorator expected actual =
      match expected, actual with
      | (expected_decorator :: expected_decorators),
        { Node.location; value = Access ((Access.Identifier identifier) :: identifiers) }
        when Identifier.show identifier = expected_decorator ->
          if List.is_empty expected_decorators && List.is_empty identifiers then
            true
          else
            is_decorator expected_decorators { Node.location; value = Access identifiers }
      | _ ->
          false
    in
    List.exists ~f:(is_decorator (String.split ~on:'.' decorator)) decorators


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


  let is_class_method define =
    has_decorator define "classmethod"


  let is_constructor ?(in_test = false) { name; parent; _ } =
    let string_name = Expression.Access.show name in
    match parent with
    | None ->
        false
    | Some parent ->
        Expression.Access.show parent = string_name ||
        (string_name = "__init__" ||
         (in_test &&
          List.mem
            ~equal:String.equal
            ["setUp"; "_setup"; "_async_setup"; "with_context"]
            string_name))


  let is_generated_constructor { generated; _ } = generated


  let is_untyped { return_annotation; _ } =
    Option.is_none return_annotation


  let is_async { async; _ } =
    async


  let create_generated_constructor { Record.Class.name; docstring; _ } =
    {
      name;
      parameters = [Parameter.create ~name:(Identifier.create "self") ()];
      body = [Node.create_with_default_location Pass];
      decorators = [];
      return_annotation = None;
      async = false;
      generated = true;
      parent = Some name;
      docstring;
    }

  let contains_call { body; _ } name =
    let matches = function
      | {
        Node.value = Expression {
            Node.value = Expression.Access [
                Expression.Access.Call {
                  Node.value = {
                    Expression.Call.name = {
                      Node.value = Expression.Access access;
                      _;
                    };
                    _;
                  };
                  _;
                };
              ];
            _;
          };
        _;
      } when Expression.Access.show access = name ->
          true
      | _ ->
          false
    in
    List.exists ~f:matches body


  let dump define =
    contains_call define "pyre_dump"


  let dump_cfg define =
    contains_call define "pyre_dump_cfg"


  let implicit_attribute_assigns
      { body; _ }
      ~definition:{ Record.Class.body = definition_body; _ } =
    let open Expression in
    let attribute_assign map { Node.location; value } =
      match value with
      | Assign ({ Assign.target; _; } as assign) ->
          let attribute_assign map target =
            match target with
            | ({
                Node.value = Access ((Access.Identifier self) :: ([_] as access));
                _;
              } as target) when Identifier.show self = "self" ->
                let assign =
                  Node.create
                    ~location
                    {
                      assign with
                      Assign.target = {
                        target with
                        Node.value = Access access;
                      };
                      value = None;
                    }
                in
                let update = function
                  | Some assigns -> Some (assign :: assigns)
                  | None -> Some [assign]
                in
                Map.change ~f:update map access
            | _ ->
                map
          in
          let targets =
            match target with
            | { Node.value = Access _; _ } as target ->
                [target]
            | { Node.value = Tuple targets; _ } ->
                targets
            | _ ->
                []
          in
          List.fold ~init:map ~f:attribute_assign targets
      | _ ->
          map
    in
    let merge_assigns = function
      | [assign] -> assign
      | ({ Node.location; value = assign } :: _) as assigns ->
          let annotation =
            let annotation = function
              | { Node.value = { Assign.annotation = Some annotation; _ }; _ } -> Some annotation
              | _ -> None
            in
            match List.filter_map ~f:annotation assigns with
            | [] -> None
            | ({ Node.location; _ } as annotation) :: annotations ->
                if List.for_all ~f:(Expression.equal annotation) annotations then
                  Some annotation
                else
                  Some {
                    Node.location;
                    value = Access [
                        Access.Identifier (Identifier.create "typing");
                        Access.Identifier (Identifier.create "Union");
                        Access.Subscript
                          (List.map
                             ~f:(fun index -> Access.Index index)
                             (annotation :: annotations));
                      ];
                  }
          in
          { Node.location; value = { assign with Assign.annotation }}
      | [] -> failwith "Unpossible!"
    in
    let rec expand_statements body =
      (* Can't use `Visit` module due to circularity :( *)
      let expand_statement ({ Node.value; _ } as statement) =
        match value with
        | If { If.body; orelse; _ }
        | For { For.body; orelse; _ }
        | While { While.body; orelse; _ } ->
            (expand_statements body) @ (expand_statements orelse)
        | Try { Try.body; orelse; finally; _ } ->
            (expand_statements body) @ (expand_statements orelse) @ (expand_statements finally)
        | With { RecordWith.body; _ } ->
            expand_statements body
        | Expression {
            Node.value =
              Expression.Access [
                Expression.Access.Identifier self;
                Expression.Access.Call {
                  Node.value = {
                    Call.name = {
                      Node.value = Expression.Access [Access.Identifier name];
                      _;
                    };
                    _;
                  };
                  _;
                }
              ];
            _;
          } when Identifier.show self = "self" ->
            (* Look for method in class definition. *)
            let inline = function
              | { Node.value = Define { name = callee; body; _ }; _ }
                when Expression.Access.show callee = Identifier.show name ->
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
    |> List.fold ~init:Expression.Access.Map.empty ~f:attribute_assign
    |> Map.map ~f:merge_assigns


  let property_attribute_assign ~location ({ name; return_annotation; _ } as define) =
    let assign return_annotation =
      Node.create
        ~location
        {
          Assign.target = Node.create ~location (Expression.Access name);
          annotation = return_annotation;
          value = None;
          compound = None;
          parent = None;
        }
    in
    match String.Set.find ~f:(has_decorator define) Recognized.property_decorators with
    | Some "util.classproperty"
    | Some "util.etc.cached_classproperty"
    | Some "util.etc.class_property" ->
        let return_annotation =
          let open Expression in
          match return_annotation with
          | Some ({ Node.location; value = Access _ } as access) ->
              Some {
                Node.location;
                value = Access [
                    Access.Identifier (Identifier.create "typing");
                    Access.Identifier (Identifier.create "ClassVar");
                    Access.Subscript [Access.Index access];
                  ];
              }
          | _ ->
              None
        in
        Some (assign return_annotation)
    | Some _ ->
        Some (assign return_annotation)
    | None ->
        None
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


  type t = statement_node Record.Class.record
  [@@deriving compare, eq, sexp, show, hash]


  let constructors ?(in_test = false) { Record.Class.body; _ } =
    let constructor = function
      | { Node.value = Define define; _ } when Define.is_constructor ~in_test define ->
          Some define
      | _ ->
          None
    in
    List.filter_map ~f:constructor body


  let attribute_assigns
      ?(include_generated_attributes = true)
      ?(in_test = false)
      ({ Record.Class.body; bases; _ } as definition) =
    let explicit_attribute_assigns =
      let attribute_assigns map { Node.location; value } =
        match value with
        | Assign ({
            Assign.target = { Node.value = Expression.Access ([_] as access); _ };
            _;
          } as assign)
        | Stub
            (Stub.Assign
               ({
                 Assign.target = { Node.value = Expression.Access ([_] as access); _ };
                 _;
               } as assign)) ->
            Map.set ~key:access ~data:(Attribute.create ~location ~async:false ~assign) map
        | _ ->
            map
      in
      List.fold ~init:Expression.Access.Map.empty ~f:attribute_assigns body
    in

    if not include_generated_attributes then
      explicit_attribute_assigns
    else
      let merge ~key:_ = function
        | `Both (_, right) ->
            Some right
        | `Left value
        | `Right value ->
            Some value
      in
      let implicit_attribute_assigns =
        constructors ~in_test definition
        |> List.map ~f:(Define.implicit_attribute_assigns ~definition)
        |> List.fold ~init:Expression.Access.Map.empty ~f:(Map.merge ~f:merge)
        |> Map.map ~f:Attribute.create_from_node
      in
      let named_tuple_assigns =
        let open Expression in
        let named_tuple_assigns sofar { Argument.value; _ } =
          match Node.value value with
          | Access [
              Access.Identifier typing;
              Access.Call {
                Node.value = {
                  Call.name = {
                    Node.value = Access [Access.Identifier named_tuple];
                    _;
                  };
                  arguments = [
                    _;
                    { Argument.value = { Node.value = List attributes; _; }; _ };
                  ];
                };
                _;
              }
            ] when (Identifier.show typing = "typing" &&
                    Identifier.show named_tuple = "NamedTuple") ||
                   (Identifier.show typing = "collections" &&
                    Identifier.show named_tuple = "namedtuple")->
              let named_tuple_assigns sofar { Node.location; value } =
                match value with
                | String name ->
                    let access = Access.create name in
                    let assign =
                      {
                        Assign.target = { Node.location; value = Access access};
                        annotation = None;
                        value = None;
                        compound = None;
                        parent = None;
                      }
                    in
                    Map.set
                      ~key:access
                      ~data:(Attribute.create ~location ~async:false ~assign)
                      sofar
                | Tuple [{ Node.location; value = String name}; annotation] ->
                    let access = Access.create name in
                    let assign =
                      {
                        Assign.target = { Node.location; value = Access access};
                        annotation = Some annotation;
                        value = None;
                        compound = None;
                        parent = None;
                      }
                    in
                    Map.set
                      ~key:access
                      ~data:(Attribute.create ~location ~async:false ~assign)
                      sofar
                | _ ->
                    sofar
              in
              List.fold ~f:named_tuple_assigns ~init:sofar attributes
          | _ ->
              sofar
        in
        List.fold ~f:named_tuple_assigns ~init:Expression.Access.Map.empty bases
      in
      let property_assigns =
        let property_assigns map = function
          | { Node.location; value = Stub (Stub.Define define) }
          | { Node.location; value = Define define } ->
              (Define.property_attribute_assign ~location define
               >>= fun ({ Node.value = { Assign.target; _ } as assign; _ }) ->
               match target with
               | { Node.value = Expression.Access ([_] as access); _ } ->
                   Some
                     (Map.set
                        ~key:access
                        ~data:
                          (Attribute.create
                             ~location
                             ~async:(Define.is_async define)
                             ~assign)
                        map)
               | _ ->
                   None)
              |> Option.value ~default:map
          | _ ->
              map
        in
        List.fold ~init:Expression.Access.Map.empty ~f:property_assigns body
      in
      let callable_assigns =
        let callable_assigns map { Node.location; value } =
          match value with
          | Stub (Stub.Define { Define.name; _ })
          | Define { Define.name; _ } ->
              let assign =
                {
                  Assign.target = Node.create ~location (Expression.Access name);
                  annotation = None;  (* This should be a `Callable`. Ignoring for now... *)
                  value = None;
                  compound = None;
                  parent = None;
                }
              in
              Map.set ~key:name ~data:(Attribute.create ~location ~async:false ~assign) map
          | _ ->
              map
        in
        List.fold ~init:Expression.Access.Map.empty ~f:callable_assigns body
      in
      let class_assigns =
        let callable_assigns map { Node.location; value } =
          match value with
          | Stub (Stub.Class { Record.Class.name; _ })
          | Class { Record.Class.name; _ } when not (List.is_empty name) ->
              let open Expression in
              let annotation =
                let meta_annotation =
                  Node.create
                    ~location
                    (Access [
                        Access.Identifier (Identifier.create "typing");
                        Access.Identifier (Identifier.create "Type");
                        Access.Subscript [Access.Index (Node.create ~location (Access name))];
                      ])
                in
                Node.create
                  ~location
                  (Access [
                      Access.Identifier (Identifier.create "typing");
                      Access.Identifier (Identifier.create "ClassVar");
                      Access.Subscript [Access.Index meta_annotation];
                    ])
              in
              let assign =
                {
                  Assign.target = Node.create ~location (Expression.Access [List.last_exn name]);
                  annotation = Some annotation;
                  value = None;
                  compound = None;
                  parent = None;
                }
              in
              Map.set ~key:name ~data:(Attribute.create ~location ~async:false ~assign) map
          | _ ->
              map
        in
        List.fold ~init:Expression.Access.Map.empty ~f:callable_assigns body
      in
      (* Merge with decreasing priority. Explicit attributes override all. *)
      explicit_attribute_assigns
      |> Map.merge ~f:merge property_assigns
      |> Map.merge ~f:merge named_tuple_assigns
      |> Map.merge ~f:merge callable_assigns
      |> Map.merge ~f:merge class_assigns
      |> Map.merge ~f:merge implicit_attribute_assigns


  let update
      { Record.Class.body = stub; _ }
      ~definition:({ Record.Class.body; _ } as definition) =
    let updated, undefined =
      let update (updated, undefined) statement =
        match statement with
        | { Node.location; value = Assign ({ Assign.target; _ } as assign)} ->
            begin
              let is_stub = function
                | { Node.value = Stub (Stub.Assign { Assign.target = stub_target; _ }); _ }
                | { Node.value = Assign { Assign.target = stub_target; _; }; _; }
                  when Expression.equal target stub_target ->
                    true
                | _ ->
                    false
              in
              match List.find ~f:is_stub stub with
              | Some { Node.value = Stub (Stub.Assign { Assign.annotation; _ }); _ } ->
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
                  Node.value = Stub (Stub.Define {
                      Record.Define.name = stub_name;
                      parameters = stub_parameters;
                      _;
                    });
                  _;
                }
                | {
                  Node.value = Define {
                      Record.Define.name = stub_name;
                      parameters = stub_parameters;
                      _;
                    };
                  _;
                } when Expression.Access.equal name stub_name &&
                       List.length parameters = List.length stub_parameters ->
                    true
                | _ ->
                    false
              in
              match List.find ~f:is_stub stub with
              | Some {
                  Node.value = Stub (Stub.Define { Define.parameters; return_annotation; _ });
                  _;
                } ->
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
      List.fold ~init:([], stub) ~f:update body
    in
    { definition with Record.Class.body = undefined @ updated }
end


module With = struct
  include Record.With


  type t = statement_node Record.With.record
  [@@deriving compare, eq, sexp, show, hash]


  let preamble { items; _ } =
    let preamble ({ Node.location; _ } as expression, target) =
      (target
       >>| fun target ->
       let open Expression in
       let enter_call = Node.create ~location (Access [
           Access.Expression expression;
           Access.Call (Node.create ~location {
               Call.name = Access (Access.create "__enter__")
                           |> Node.create ~location;
               arguments = [];
             })
         ])
       in
       let assign =
         {
           Assign.target;
           annotation = None;
           value = Some enter_call;
           compound = None;
           parent = None;
         }
       in
       Node.create ~location (Assign assign))
      |> Option.value
        ~default:(Node.create ~location (Expression expression))
    in
    List.map ~f:preamble items
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
        let indentations = List.map ~f:indentation rest in
        let difference = List.fold ~init:Int.max_value ~f:Int.min indentations in
        let rest = List.map ~f:(fun s -> String.drop_prefix s difference) rest in
        String.concat ~sep:"\n" (first::rest)
  in
  match statements with
  | { Node.value = Expression { Node.value = Expression.String s; _ }; _ } :: _ -> Some (unindent s)
  | _ -> None


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
          "@[%a.@]"
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


  let pp_binary_operator_option formatter option =
    pp_option formatter option Expression.BinaryOperator.pp_binary_operator


  let pp_async formatter =
    function
    | true -> Format.fprintf formatter "async@;"
    | false -> ()


  let rec pp_statement_node formatter { Node.value = statement ; _ } =
    Format.fprintf formatter "%a" pp_statement statement


  and pp_statement_list formatter =
    function
    | [] -> ()
    | statement :: [] -> Format.fprintf formatter "%a" pp_statement_node statement
    | statement :: statement_list ->
        Format.fprintf
          formatter "%a@;%a"
          pp_statement_node statement
          pp_statement_list statement_list


  and pp_assign formatter { Assign.target; annotation; value; compound; parent } =
    Format.fprintf
      formatter
      "%a%a %a= %a%a"
      pp_access_list_option parent
      Expression.pp target
      pp_binary_operator_option compound
      pp_expression_option ("", value)
      pp_expression_option (" # ", annotation)


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
      "%a@[<v 2>%adef %a%a(%a)%s:@;%a@]@."
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
            pp_option_with_prefix formatter ("as ", access_list) pp_access_list
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

    | Return expression ->
        Format.fprintf
          formatter
          "return %a"
          pp_expression_option ("", expression)

    | Stub (Stub.Assign assign) ->
        Format.fprintf
          formatter
          "%a"
          pp_assign assign

    | Stub (Stub.Class definition) ->
        Format.fprintf formatter "%a" pp_class definition

    | Stub (Stub.Define define) ->
        Format.fprintf formatter "%a" pp_define define

    | Try { Try.body; handlers; orelse; finally } ->
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
          let pp_handler formatter {Try.kind; Try.name; Try.handler_body } =
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

    | Yield expression -> Format.fprintf formatter "%a" Expression.pp expression
    | YieldFrom expression -> Format.fprintf formatter "%a" Expression.pp expression


  let pp = pp_statement_node
end


let pp formatter statement =
  Format.fprintf
    formatter
    "%a"
    PrettyPrinter.pp statement


let show statement =
  Format.asprintf "%a" pp statement
