(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

%{
  open Core

  open Ast
  open Expression
  open Statement
  open Pyre

  let with_decorators decorators = function
    | { Node.location; value = Class value } ->
        let decorated = { value with Class.decorators = decorators; } in
        { Node.location; value = Class decorated }
    | { Node.location; value = Define value } ->
        let decorated = { value with Define.decorators = decorators; } in
        { Node.location; value = Define decorated }
    | _ -> raise (ParserError "Cannot decorate statement")

  type entry =
    | Entry of Expression.t Dictionary.entry
    | Item of Expression.t
    | Keywords of Expression.t

  let extract_entries entries =
    let extract_entries (entries, keywords, items) = function
      | Entry entry ->
          entry :: entries, keywords, items
      | Item item ->
          entries, keywords, item :: items
      | Keywords keyword ->
          entries, keyword :: keywords, items
    in
    let entries, keywords, items = List.fold ~init:([], [], []) ~f:extract_entries entries in
    List.rev entries, List.rev keywords, List.rev items

  (* Helper function to combine a start position of type Lexing.position and
   * stop position of type Location.position. *)
  let location_create_with_stop ~start ~stop =
    let position = Location.create ~start ~stop:start in
    { position with Location.stop = stop }

  type binary_operator =
    | Add
    | At
    | BitAnd
    | BitOr
    | BitXor
    | Divide
    | FloorDivide
    | LeftShift
    | Modulo
    | Multiply
    | Power
    | RightShift
    | Subtract

  let binary_operator ~left:({ Node.location; _ } as left) ~operator ~right =
    let access =
      let name =
        match operator with
        | Add -> "__add__"
        | At -> "__matmul__"
        | BitAnd -> "__and__"
        | BitOr -> "__or__"
        | BitXor -> "__xor__"
        | Divide -> "__truediv__"
        | FloorDivide -> "__floordiv__"
        | LeftShift -> "__lshift__"
        | Modulo -> "__mod__"
        | Multiply -> "__mul__"
        | Power -> "__pow__"
        | RightShift -> "__rshift__"
        | Subtract -> "__sub__"
      in
      let arguments = [{ Argument.name = None; value = right }] in
      ((access left) @ (Access.call ~arguments ~location ~name ()))
    in
    {
      Node.location = left.Node.location;
      value = Access access
    }

  let slice ~lower ~upper ~step =
    let location =
      let expression =
        match lower with
        | Some lower -> Some lower
        | None ->
            begin
              match upper with
              | Some upper -> Some upper
              | None ->
                  begin
                    match step with
                    | Some step -> Some step
                    | None -> None
                  end
            end
      in
      expression >>| Node.location |> Option.value ~default:Location.Reference.any
    in
    let arguments =
      let argument argument =
        let none =
          Access [Access.Identifier (Identifier.create "None")]
          |> Node.create ~location
        in
        Option.value argument ~default:none
      in
      [
        { Argument.name = None; value = argument lower };
        { Argument.name = None; value = argument upper };
        { Argument.name = None; value = argument step };
      ]
    in
    Access (Access.call ~arguments ~location ~name:"slice" ())
    |> Node.create ~location

  let create_ellipses (start, stop) =
    let location = Location.create ~start ~stop in
    Node.create Ellipses ~location

  let subscript_argument ~subscripts ~location =
    let value =
      match subscripts with
      | [subscript] -> subscript
      | subscripts -> { Node.location; value = Tuple subscripts }
    in
    { Argument.name = None; value }

  let subscript_access subscript =
    let head, subscripts = subscript in
    let location = Node.location head in
    let get_item =
      let arguments = [subscript_argument ~subscripts ~location] in
      Access.call ~arguments ~location ~name:"__getitem__" ()
    in
    { Node.location; value = Access ((Expression.access head) @ get_item) }

  let subscript_mutation ~subscript ~value ~annotation:_ =
    let head, subscripts = subscript in
    let location =
      { head.Node.location with Location.stop = value.Node.location.Location.stop }
    in
    let set_item =
      let arguments =
        [subscript_argument ~subscripts ~location; { name = None; value }]
      in
      Access.call ~arguments ~location ~name:"__setitem__" ()
    in
    Access ((Expression.access head) @ set_item)
    |> Node.create ~location
    |> (fun expression -> Expression expression)
    |> Node.create ~location

  let with_annotation ~parameter ~annotation =
    let value =
      let { Node.value = { Parameter.annotation = existing; _ } as value; _ } = parameter in
      let annotation =
        match existing, annotation with
        | None, Some annotation -> Some annotation
        | _ -> existing
      in
      { value with Parameter.annotation }
    in
    { parameter with Node.value }
%}

(* The syntactic junkyard. *)
%token <Lexing.position * Lexing.position> ASTERIKS
%token <Lexing.position> AWAIT
%token <Lexing.position> COLON
%token <Lexing.position> DEDENT
%token <Lexing.position> DOT
%token <Lexing.position> LEFTBRACKET
%token <Lexing.position> LEFTCURLY
%token <Lexing.position> LEFTPARENS
%token <Lexing.position> MINUS
%token <Lexing.position> NEWLINE
%token <Lexing.position> NOT
%token <Lexing.position> PLUS
(* the RIGHT* lexemes only contain the end position. *)
%token <Lexing.position> RIGHTBRACKET
%token <Lexing.position> RIGHTCURLY
%token <Lexing.position> RIGHTPARENS
%token <Lexing.position> TILDE

%token <string list * string> SIGNATURE_COMMENT
%token <(Lexing.position * Lexing.position) * string> ANNOTATION_COMMENT

%token AMPERSAND
%token AMPERSANDEQUALS
%token AND
%token ASTERIKSASTERIKSEQUALS
%token ASTERIKSEQUALS
%token AT
%token ATEQUALS
%token BAR
%token BAREQUALS
%token COMMA
%token DOUBLEEQUALS
%token EOF
%token EQUALS
%token EXCLAMATIONMARK
%token HAT
%token HATEQUALS
%token INDENT
%token IS
%token ISNOT
%token LEFTANGLE
%token LEFTANGLEEQUALS
%token LEFTANGLELEFTANGLE
%token LEFTANGLELEFTANGLEEQUALS
%token MINUSEQUALS
%token OR
%token PERCENT
%token PERCENTEQUALS
%token PLUSEQUALS
%token RIGHTANGLE
%token RIGHTANGLEEQUALS
%token RIGHTANGLERIGHTANGLE
%token RIGHTANGLERIGHTANGLEEQUALS
%token SEMICOLON
%token SLASH
%token SLASHEQUALS
%token SLASHSLASHEQUALS

(* Declarations. *)
%token <Lexing.position * Lexing.position> ASYNC
%token <Lexing.position> CLASS
%token <Lexing.position> DEFINE
%token <Lexing.position> LAMBDA

(* Values. *)
%token <(Lexing.position * Lexing.position) * float> FLOAT
%token <(Lexing.position * Lexing.position) * float> COMPLEX
%token <(Lexing.position * Lexing.position) * int> INTEGER
%token <(Lexing.position * Lexing.position) * string> BYTES
%token <(Lexing.position * Lexing.position) * string> FORMAT
%token <(Lexing.position * Lexing.position) * string> IDENTIFIER
%token <(Lexing.position * Lexing.position) * string> STRING
%token <(Lexing.position * Lexing.position)> ELLIPSES
%token <(Lexing.position * Lexing.position)> FALSE
%token <(Lexing.position * Lexing.position)> TRUE

(* Control. *)
%token <Lexing.position> ASSERT
%token <Lexing.position * Lexing.position> BREAK
%token <Lexing.position * Lexing.position> CONTINUE
%token <Lexing.position> DELETE
%token <Lexing.position> ELSEIF
%token <Lexing.position> FOR
%token <Lexing.position> FROM
%token <Lexing.position> GLOBAL
%token <Lexing.position> IF
%token <Lexing.position> IMPORT
%token <Lexing.position> NONLOCAL
%token <Lexing.position * Lexing.position> PASS
%token <Lexing.position * Lexing.position> RAISE
%token <Lexing.position * Lexing.position> RETURN
%token <Lexing.position> TRY
%token <Lexing.position> WITH
%token <Lexing.position> WHILE
%token <Lexing.position * Lexing.position> YIELD
%token AS
%token ELSE
%token <Lexing.position> EXCEPT
%token FINALLY
%token IN

%right OR
%right AND
%left NOT
%left IS
%left BAR
%left HAT
%left AMPERSAND
%left PLUS MINUS
%left ASTERIKS PERCENT SLASH
%left TILDE
%left AT
%left DOT

%nonassoc LEFTPARENS


%start <Ast.Statement.t list> parse

%%

parse:
  | statements = statements; EOF { snd statements }
  ;

(* Statements. *)

statements:
  | { Location.Reference.any, [] }
  | NEWLINE; statements = statements { statements }
  | statement = statement; statements = statements {
      (* The recursion always terminates in the empty statement case. This logic avoids
       * propagating the end location information from there. *)
      let location =
        match (snd statements) with
        | [] -> fst statement
        | _ -> {(fst statement) with Location.stop = (fst statements).Location.stop;}
       in
      location, (snd statement)@(snd statements)
    }
  ;

statement:
  | statements = simple_statement { statements }
  | statement = compound_statement { statement.Node.location, [statement] }
  | statement = decorated_statement { statement.Node.location, [statement] }
  | statement = async_statement { statement.Node.location, [statement] }

simple_statement:
  | statements = separated_nonempty_list_of_lists(SEMICOLON, small_statement);
    NEWLINE {
      let flattened_statements = List.concat statements in
      let head = List.hd_exn flattened_statements in
      let last = List.last_exn flattened_statements in
      let location = {head.Node.location with Location.stop = Node.stop last} in
      location, flattened_statements
    }
  ;

small_statement:
  | subscript = subscript; compound = compound_operator; value = test {
      let value =
        binary_operator
          ~left:(subscript_access subscript)
          ~operator:compound
          ~right:value
      in
      [subscript_mutation ~subscript ~value ~annotation:None]
  }
  | target = test_list;
    compound = compound_operator;
    value = test_list {
      let value = binary_operator ~left:target ~operator:compound ~right:value in
      [{
        Node.location = {
          target.Node.location with Location.stop =
            value.Node.location.Location.stop;
        };
        value = Assign {
          Assign.target;
          annotation = None;
          value;
          parent = None;
        };
      }]
    }
  | target = test_list;
    annotation = annotation {
      [{
        Node.location = target.Node.location;
        value = Assign {
          Assign.target;
          annotation = Some annotation;
          value = Node.create_with_default_location Ellipses;
          parent = None;
        };
      }]
    }
  | target = test_list;
    annotation = annotation;
    EQUALS;
    value = test_list {
      [{
        Node.location = target.Node.location;
        value = Assign {
          Assign.target;
          annotation = Some annotation;
          value;
          parent = None;
        };
      }]
    }
  | targets = targets; value = value; annotation = comment_annotation? {
      List.map ~f:(fun target -> target ~value ~annotation) targets
  }
  | targets = targets; ellipses = ELLIPSES {
      let value = create_ellipses ellipses in
      List.map ~f:(fun target -> target ~value ~annotation:None) targets
    }
  | target = test_list;
    annotation = annotation;
    EQUALS;
    ellipses = ELLIPSES {
      [{
        Node.location = target.Node.location;
        value = Assign {
          Assign.target;
          annotation = Some annotation;
          value = create_ellipses ellipses;
          parent = None;
        };
      }]
    }

  | start = ASSERT; test = test {
      [{
        Node.location = location_create_with_stop ~start ~stop:(Node.stop test);
        value = Assert { Assert.test; message = None }
      }]
    }
  | start = ASSERT; test = test;
    COMMA; message = test {
      [{
        Node.location = location_create_with_stop ~start ~stop:(Node.stop test);
        value = Assert { Assert.test; message = Some message }
      }]
    }

  | position = BREAK {
      let start, stop = position in
      [{ Node.location = Location.create ~start ~stop; value = Break }]
    }

  | position = CONTINUE {
      let start, stop = position in
      [{ Node.location = Location.create ~start ~stop; value = Continue }]
    }

  | test = test_list {
      [{ Node.location = test.Node.location; value = Expression test }]
    }

  | start = GLOBAL; globals = separated_nonempty_list(COMMA, identifier) {
      let last = List.last_exn globals in
      let stop = (fst last).Location.stop in
      [{
        Node.location = location_create_with_stop ~start ~stop;
        value = Global (List.map globals ~f:snd);
      }]
    }

  | start = IMPORT; imports = imports; {
      [{
        Node.location = location_create_with_stop ~start ~stop:((fst imports).Location.stop);
        value = Import { Import.from = None; imports = snd imports };
      }]
    }
  | start = FROM; from = from; IMPORT; imports = imports {
      [{
        Node.location = location_create_with_stop ~start ~stop:((fst imports).Location.stop);
        value = Import {
          Import.from;
          imports = snd imports;
        };
      }]
    }

  | start = NONLOCAL; nonlocals = separated_nonempty_list(COMMA, identifier) {
      let stop = (fst (List.last_exn nonlocals)).Location.stop in
      [{
        Node.location = location_create_with_stop ~start ~stop;
        value = Nonlocal (List.map nonlocals ~f:snd);
      }]
    }

  | position = PASS {
      let start, stop = position in
      [{ Node.location = Location.create ~start ~stop; value = Pass }]
    }

  | position = RAISE; test = test_list?; raise_from = raise_from? {
      let start, stop = position in
      let location =
        match (test, raise_from) with
        | None, None -> Location.create ~start ~stop
        | Some node, None ->
          location_create_with_stop ~start ~stop:(Node.stop node)
        | _, Some location ->
          location_create_with_stop ~start ~stop:(location.Location.stop)
      in
      [{
        Node.location;
        value = Raise test;
      }]
    }

  | return = RETURN; test = test_list? {
      let start, stop = return in
      let location =
        match test with
        | None -> Location.create ~start ~stop
        | Some node -> location_create_with_stop ~start ~stop:(Node.stop node)
      in
      [{
        Node.location;
        value = Return { Return.expression = test; is_implicit = false };
      }]
    }

  | delete = DELETE;
    expression = expression_list {
      let stop = Node.stop expression in
      [{
        Node.location = location_create_with_stop ~start:delete ~stop;
        value = Delete expression;
      }]
    }

  | yield = yield {
      let has_from, yield = yield in
      let location = Node.location yield in
      if has_from then
        let yield =
          match yield with
          | { Node.value = Yield (Some yield); _ } ->
              (Expression.access yield) @ (Access.call ~name:"__iter__" ~location ())
              |> (fun access -> Access access)
              |> Node.create ~location
          | _ ->
              yield
        in
        [
          {
            Node.location;
            value = Statement.YieldFrom { Node.location; value = Expression.Yield (Some yield) }
          };
        ]
      else
        [{ Node.location; value = Statement.Yield yield }]
    }
  ;

raise_from:
  | FROM; test_list = test_list { test_list.Node.location }
  ;

compound_statement:
  | definition = CLASS; name = simple_access;
    bases = bases; colon_position = COLON;
    body = block_or_stub_body {
      let location = Location.create ~start:definition ~stop:colon_position in
      let body_location, body = body in
      let location = { location with Location.stop = body_location.Location.stop } in
      let _, name = name in
      let body =
        let rec transform_toplevel_statements = function
          | { Node.location; value = Assign assign } ->
              {
                Node.location;
                value = Assign { assign with Assign.parent = Some name };
              }
          | { Node.location; value = Define define } ->
              {
                Node.location;
                value = Define { define with Define.parent = Some name };
              }
          | {
              Node.location;
              value = If {
                If.test;
                body;
                orelse;
              };
            } ->
              {
                Node.location;
                value = If {
                  If.test;
                  body = List.map ~f:transform_toplevel_statements body;
                  orelse = List.map ~f:transform_toplevel_statements orelse;
                };
              }
          | statement ->
              statement
        in
        List.map ~f:transform_toplevel_statements body in
      {
        Node.location;
        value = Class {
          Class.name;
          bases;
          body;
          decorators = [];
          docstring = Statement.extract_docstring body;
        };
      }
    }

  | definition = DEFINE; name = simple_access;
    LEFTPARENS;
    parameters = define_parameters;
    RIGHTPARENS;
    return_annotation = return_annotation?;
    COLON;
    signature_comment = SIGNATURE_COMMENT?;
    body = block_or_stub_body {
      let body_location, body = body in
      let docstring = Statement.extract_docstring body in
      let location =
        location_create_with_stop ~start:definition ~stop:body_location.Location.stop
      in
      let annotation =
        match return_annotation with
        | Some return_annotation -> Some return_annotation
        | None ->
          signature_comment
          >>= (fun (_, return_annotation) ->
              Some {
                Node.location;
                value = String (StringLiteral.create return_annotation);
              }
            )
      in
      let parameters =
        match signature_comment with
        | Some (parameter_annotations, _)
          when not (List.is_empty parameter_annotations) ->
            let add_annotation
              ({ Node.value = parameter; _ } as parameter_node)
              annotation =
                {
                  parameter_node with
                  Node.value = {
                    parameter with
                      Parameter.annotation = Some {
                        Node.location = Location.Reference.any;
                        value = String (StringLiteral.create annotation);
                      }
                  }
                }
            in
            if List.length parameters = List.length parameter_annotations then
              List.map2_exn
                ~f:add_annotation
                parameters
                parameter_annotations
            else
              parameters
        | _ ->
            parameters
      in
      {
        Node.location;
        value = Define {
          Define.name = snd name;
          parameters;
          body;
          decorators = [];
          return_annotation = annotation;
          async = false;
          parent = None;
          docstring = docstring;
        };
      }
    }

  | start = FOR; target = expression_list; IN; iterator = test_list; COLON;
    body = block; orelse = named_optional_block(ELSE) {
      let stop = begin match orelse with
      | _, [] -> (fst body).Location.stop
      | location, _ -> location.Location.stop
      end in
      {
        Node.location = location_create_with_stop ~start ~stop;
        value = For {
          For.target;
          iterator;
          body = snd body;
          orelse = snd orelse;
          async = false
        };
      }
    }

  | start = IF; value = conditional {
      let value_location, value = value in
      {
        Node.location = location_create_with_stop ~start ~stop:value_location.Location.stop;
        value
      }
    }

  | start = TRY; COLON;
    body = block;
    handlers = list(handler);
    orelse = named_optional_block(ELSE);
    finally = named_optional_block(FINALLY) {

      let stop =
        begin
          match handlers, snd orelse, snd finally with
          | _, _, (_::_) -> fst finally
          | _, (_::_), [] -> fst orelse
          | (_::_), [], [] -> (fst (List.last_exn handlers))
          | _ -> (fst body)
        end.Location.stop in
      {
        Node.location = location_create_with_stop ~start ~stop;
        value = Try {
          Try.body = snd body;
          handlers = List.map ~f:snd handlers;
          orelse = snd orelse;
          finally = snd finally
        };
      }
    }

  | start = WITH;
    items = separated_nonempty_list(COMMA, with_item); COLON;
    body = block {
      {
        Node.location = location_create_with_stop ~start ~stop:(fst body).Location.stop;
        value = With { With.items; body = snd body; async = false };
      }
    }

  | start = WHILE; test = test_list; COLON;
    body = block; orelse = named_optional_block(ELSE) {
      let stop =
        match orelse with
        | _, [] -> (fst body).Location.stop
        | location, _ -> location.Location.stop in
      {
        Node.location = location_create_with_stop ~start ~stop;
        value = While { While.test; body = snd body; orelse = snd orelse };
      }
    }
  ;

decorated_statement:
  | decorators = decorator+; statement = compound_statement {
      with_decorators decorators statement
    }
  | decorators = decorator+; statement = async_statement {
      with_decorators decorators statement
    }
  ;

async_statement:
  | position = ASYNC; statement = compound_statement {
      let location = location_create_with_stop ~start:(fst position) ~stop:(Node.stop statement) in
      match statement with
      | { Node.value = Define value; _ } ->
          let decorated = { value with Define.async = true } in
          {
            Node.location;
            value = Define decorated;
          }
      | { Node.value = For value; _ } ->
          let with_async = { value with For.async = true } in
          {
            Node.location;
            value = For with_async;
          }
      | { Node.value = With value; _ } ->
          let with_async = { value with With.async = true } in
          {
            Node.location;
            value = With with_async;
          }
      | _ -> raise (ParserError "Async not supported on statement.")
    }
  ;

block_or_stub_body:
  | ellipses = ELLIPSES; NEWLINE
  | NEWLINE+; INDENT; ellipses = ELLIPSES; NEWLINE; DEDENT; NEWLINE* {
    let location = Location.create ~start:(fst ellipses) ~stop:(snd ellipses) in
    let body = [Node.create (Expression (Node.create Ellipses ~location)) ~location] in
    location, body
   }
  | statements = block { statements }
  ;

block:
  | simple_statement = simple_statement; { simple_statement }
  | NEWLINE+; INDENT; statements = statements; DEDENT; NEWLINE* {
      statements
    }
  ;

named_optional_block(NAME):
  | { Location.Reference.any, [] }
  | NAME; COLON; block = block { block }
  ;

conditional:
  | test = test_list; COLON;
    body = block; orelse = named_optional_block(ELSE) {
      {
        test.Node.location with
        Location.stop =
          match orelse with
          | _, [] -> (fst body).Location.stop
          | location, _ -> location.Location.stop;
      },
      If { If.test; body = snd body; orelse = snd orelse }
    }
  | test = test_list; COLON;
    body = block;
    else_start = ELSEIF; value = conditional {
      { test.Node.location with Location.stop = (fst value).Location.stop },
      If {
        If.test;
        body = (snd body);
        orelse = [{
          Node.location =
            location_create_with_stop ~start:else_start ~stop:(fst value).Location.stop;
          value = snd value
        }];
      }
    }
 ;

bases:
  | { [] }
  | LEFTPARENS; bases = separated_list(COMMA, argument); RIGHTPARENS {
      bases
    }
  ;

decorator:
  | AT; expression = expression; NEWLINE+ { expression }
  ;

identifier:
  | identifier = IDENTIFIER {
      let start, stop = fst identifier in
      Location.create ~start ~stop, Identifier.create (snd identifier)
    }
  | position = ASYNC {
      Location.create ~start:(fst position) ~stop:(snd position),
      Identifier.create "async"
    }
  ;

simple_access:
  | identifiers = separated_nonempty_list(DOT, identifier) {
      let location =
        let (start, _) = List.hd_exn identifiers in
        let (stop, _) = List.last_exn identifiers in
        { start with Location.stop = stop.Location.stop } in
      let identifiers =
        List.map ~f:snd identifiers
        |> List.map ~f:(fun identifier -> Access.Identifier identifier) in
      location, identifiers
    }
  ;

define_parameters:
  | parameter = define_parameter;
    COMMA;
    annotation = comment_annotation?;
    parameters = define_parameters { (with_annotation ~parameter ~annotation) :: parameters }
  | parameter = define_parameter;
    annotation = comment_annotation? { [with_annotation ~parameter ~annotation] }
  | { [] }

%inline define_parameter:
  (* `*` itself is a valid parameter... *)
  | asteriks = ASTERIKS {
      {
        Node.location = Location.create ~start:(fst asteriks) ~stop:(snd asteriks);
        value = {
            Parameter.name = Identifier.create "*";
            value = None;
            annotation = None;
        };
      }
    }
  | name = name; annotation = annotation? {
      {
        Node.location = fst name;
        value = { Parameter.name = snd name; value = None; annotation };
      }
    }
  | name = name; annotation = annotation?; EQUALS; value = test {
      {
        Node.location = fst name;
        value = { Parameter.name = snd name; value = Some value; annotation };
      }
    }
  ;

%inline lambda_parameter:
  | name = name {
      {
        Node.location = fst name;
        value = { Parameter.name = snd name; value = None; annotation = None }
      }
    }
  | name = name; EQUALS; value = test {
      {
        Node.location = fst name;
        value = { Parameter.name = snd name; value = Some value; annotation = None};
      }
    }
  ;

%inline name:
  | expression = expression {
      let rec identifier expression =
        match expression with
        | { Node.location; value = Access [Access.Identifier identifier] } ->
            (location, identifier)
        | { Node.location; value = Starred (Starred.Once expression) } ->
            location,
            identifier expression
            |> snd
            |> Identifier.map ~f:(fun identifier -> "*" ^ identifier)
        | { Node.location; value = Starred (Starred.Twice expression) } ->
            location,
            identifier expression
            |> snd
            |> Identifier.map ~f:(fun identifier -> "**" ^ identifier)
        | _ ->
            raise (ParserError "Unexpected parameters") in
      identifier expression
    }
  ;

%inline annotation:
  | COLON; expression = expression { expression }
  ;

%inline comment_annotation:
  | annotation = ANNOTATION_COMMENT {
      let (start, stop), annotation = annotation in
      String (StringLiteral.create annotation)
      |> Node.create ~location:(Location.create ~start ~stop)
    }

%inline return_annotation:
  | MINUS; RIGHTANGLE; expression = expression { expression }
  ;

%inline subscript:
  | head = expression;
    LEFTBRACKET; subscripts = separated_nonempty_list(COMMA, subscript_key); RIGHTBRACKET {
      head, subscripts
    }
  ;

with_item:
  | resource = test { resource, None }
  | resource = test; AS; target = expression { resource, Some target }
  ;

handler:
  | start = EXCEPT; COLON; handler_body = block {
      location_create_with_stop ~start ~stop:(fst handler_body).Location.stop,
      { Try.kind = None; name = None; handler_body = snd handler_body }
    }
  | start = EXCEPT; kind = expression; COLON; handler_body = block {
      location_create_with_stop ~start ~stop:(fst handler_body).Location.stop,
      { Try.kind = Some kind; name = None; handler_body = snd handler_body }
    }
  | start = EXCEPT;
    kind = expression; AS; name = identifier;
    COLON; handler_body = block
  | start = EXCEPT;
    kind = expression; COMMA; name = identifier;
    COLON; handler_body = block {
      location_create_with_stop ~start ~stop:(fst handler_body).Location.stop,
      { Try.kind = Some kind; name = Some (snd name); handler_body = snd handler_body }
    }
  ;

from:
  | from = from_string {
      Access.create from
      |> Option.some
    }
  ;

from_string:
  | { "" }
  | identifier = identifier; from_string = from_string {
      (Identifier.show (snd identifier)) ^ from_string
    }
  | relative = nonempty_list(ellipses_or_dot);
    from_string = from_string {
      (String.concat relative) ^ from_string
    }
  ;

ellipses_or_dot:
  | DOT { "." }
  | ELLIPSES { "..." }
  ;

imports:
  | imports = separated_nonempty_list(COMMA, import) {
      let location =
        let (start, _) = List.hd_exn imports in
        let (stop, _) = List.last_exn imports in
        { start with Location.stop = stop.Location.stop }
      in
      location, List.map ~f:snd imports
    }
  | start = LEFTPARENS;
    imports = separated_nonempty_list(COMMA, import);
    stop = RIGHTPARENS {
      (Location.create ~start ~stop),
      List.map ~f:snd imports
    }
  ;

import:
  | position = ASTERIKS {
      let start, stop = position in
      Location.create ~start ~stop,
      {
        Import.name = Access.create "*";
        alias = None;
      }
    }
  | name = simple_access {
      fst name,
      {
        Import.name = snd name;
        alias = None;
      }
    }
  | name = simple_access;
    AS; alias = identifier {
      {(fst name) with Location.stop = (fst alias).Location.stop},
      {
        Import.name = snd name;
        alias = Some [Access.Identifier (snd alias)];
      }
    }
  ;

%inline target:
  | target = test_list {
      let assignment_with_annotation ~value ~annotation =
        {
          Node.location = target.Node.location;
          value = Assign {
            Assign.target;
            annotation;
            value;
            parent = None;
          };
        }
      in
      assignment_with_annotation
    }
  | subscript = subscript { subscript_mutation ~subscript }

targets:
  | target = target; EQUALS { [target] }
  | targets = targets; target = target; EQUALS { targets @ [target] }
  ;

value:
  | test = test_list { test }
  | yield = yield { snd yield }
  ;

(* Expressions. *)

atom:
  | identifier = identifier {
      {
        Node.location = fst identifier;
        value = Access [Access.Identifier (snd identifier)];
      }
    }

  | ellipses = ELLIPSES {
      let location = Location.create ~start:(fst ellipses) ~stop:(snd ellipses) in
      Node.create Ellipses ~location
    }

  | left = expression;
    operator = binary_operator;
    right = expression; {
      binary_operator ~left ~operator ~right
    }

  | bytes = BYTES+ {
      let start, stop = fst (List.hd_exn bytes) in
      {
        Node.location = Location.create ~start ~stop;
        value = String (StringLiteral.create ~bytes:true (String.concat (List.map bytes ~f:snd)));
      }
    }

  | format = FORMAT; mixed_string = mixed_string {
      let all_strings = format::mixed_string in
      let (head, _), (last, _) = List.hd_exn all_strings, List.last_exn all_strings in
      let (start, _) = head in
      let (_, stop) = last in
      {
        Node.location = Location.create ~start ~stop;
        value = String
          (StringLiteral.create
            ~expressions:[]
            ((snd format) ^ String.concat (List.map mixed_string ~f:snd)));
      }
    }

  | name = expression;
    start = LEFTPARENS;
    arguments = arguments;
    stop = RIGHTPARENS {
      let call_location = Location.create ~start ~stop in
      {
        Node.location = name.Node.location;
        value =
          Access
            (Expression.access name @
             [Access.Call (Node.create ~location:call_location arguments)]);
      }
    }

  | start = LEFTCURLY;
    entries = separated_list(COMMA, set_or_dictionary_entry);
    stop = RIGHTCURLY {
      let value =
        match extract_entries entries with
        | entries, keywords, [] -> Dictionary { Dictionary.entries; keywords }
        | [], [], items -> Set items
        | _ -> failwith "Invalid set or dictionary"
      in
      { Node.location = Location.create ~start ~stop; value }
    }

  | start = LEFTCURLY;
    key = expression; COLON; value = test;
    generators = comprehension+;
    stop = RIGHTCURLY {
      {
        Node.location = Location.create ~start ~stop;
        value = DictionaryComprehension {
          Comprehension.element = { Dictionary.key; value };
          generators;
        };
      }
    }
  | start = LEFTCURLY;
    key = test; COLON; value = test;
    generators = comprehension+;
    stop = RIGHTCURLY {
      {
        Node.location = Location.create ~start ~stop;
        value = DictionaryComprehension {
          Comprehension.element = { Dictionary.key; value };
          generators;
        };
      }
    }

  | position = FALSE {
      let start, stop = position in
      {
        Node.location = Location.create ~start ~stop;
        value = False;
      }
    }

  | number = COMPLEX {
      let start, stop = fst number in
      {
        Node.location = Location.create ~start ~stop;
        value = Complex (snd number);
      }
    }

  | number = FLOAT {
      let start, stop = fst number in
      {
        Node.location = Location.create ~start ~stop;
        value = Float (snd number);
      }
    }

  | number = INTEGER {
      let start, stop = fst number in
      {
        Node.location = Location.create ~start ~stop;
        value = Integer (snd number);
      }
    }

  | start = LEFTBRACKET;
    items = separated_list(COMMA, test);
    stop = RIGHTBRACKET {
      {
        Node.location = Location.create ~start ~stop;
        value = List items;
      }
    }
  | start = LEFTBRACKET;
    element = test;
    generators = comprehension+;
    stop = RIGHTBRACKET {
      {
        Node.location = Location.create ~start ~stop;
        value = ListComprehension { Comprehension.element; generators };
      }
    }

  | start = LEFTCURLY;
    element = test;
    generators = comprehension+;
    stop = RIGHTCURLY {
      {
        Node.location = Location.create ~start ~stop;
        value = SetComprehension { Comprehension.element; generators };
      }
    }

  | position = ASTERIKS; test = test {
      let start, _ = position in
      let location = location_create_with_stop ~start ~stop:(Node.stop test) in
      match test with
      | {
          Node.value = Starred (Starred.Once test);
          _;
        } -> {
          Node.location;
          value = Starred (Starred.Twice test);
        }
      | _ -> {
          Node.location;
          value = Starred (Starred.Once test);
        }
    }

  | string = STRING; mixed_string = mixed_string {
      let all_strings = string::mixed_string in
      let (head, _), (last, _) = List.hd_exn all_strings, List.last_exn all_strings in
      let (start, _) = head in
      let (_, stop) = last in
      {
        Node.location = Location.create ~start ~stop;
        value = String
          (StringLiteral.create
            ((snd string) ^ String.concat ~sep:"" (List.map mixed_string ~f:snd)));
      }
    }

  | position = TRUE {
      let start, stop = position in
      { Node.location = Location.create ~start ~stop; value = True }
    }

  | operator = unary_operator; operand = expression {
      let start, operator = operator in
      {
        Node.location = location_create_with_stop ~start ~stop:(Node.stop operand);
        value = UnaryOperator {
          UnaryOperator.operator = operator;
          operand;
        };
      }
    }
  ;

expression:
  | atom = atom { atom }

  | start = LEFTPARENS; stop = RIGHTPARENS {
      {
        Node.location = Location.create ~start ~stop;
        value = Tuple [];
      }
    }

  | LEFTPARENS; test = test_list; RIGHTPARENS { test }

  | access = expression; DOT; expression = expression {
      {
        Node.location = { access.Node.location with Location.stop = Node.stop expression };
        value = Access ((Expression.access access) @ (Expression.access expression));
      }
    }

  | subscript = subscript { subscript_access subscript }

  | start = AWAIT; expression = expression {
      {
        Node.location = location_create_with_stop ~start ~stop:(Node.stop expression);
        value = Await expression;
      }
    }

  | LEFTPARENS; generator = generator; RIGHTPARENS { generator }

  | LEFTPARENS; yield = yield RIGHTPARENS { snd yield }
  ;

expression_list:
  | items = separated_nonempty_list_indicator(COMMA, expression) {
      match items with
      | head::[], has_trailing_comma ->
          if has_trailing_comma then
            {
              Node.location = head.Node.location;
              value = Tuple [head];
            }
          else head
      | (head :: _) as items, _ ->
          let last = List.last_exn items in
          {
            Node.location = { head.Node.location with Location.stop = Node.stop last };
            value = Tuple items;
          }
      | _ -> raise (ParserError "invalid atom")
    }
  ;

mixed_string:
  | { [] }
  | first_string = FORMAT; rest = mixed_string {
      first_string :: rest
    }
  | first_string = STRING; rest = mixed_string {
      first_string :: rest
    }
  ;

test_without_ternary:
  | left = test_without_ternary; operator = boolean_operator; right = test_without_ternary {
    {
      Node.location = left.Node.location;
      value = BooleanOperator { BooleanOperator.left; operator; right };
    }
  }

  | left = expression; comparisons = nonempty_list(comparison) {
      let rec comparison ({ Node.location; _ } as left) comparisons =
        match comparisons with
        | (operator, right) :: comparisons when List.length comparisons > 0 ->
            let left =
              ComparisonOperator { ComparisonOperator.left; operator; right }
              |> Node.create ~location
            in
            let right = comparison right comparisons in
            BooleanOperator {
              BooleanOperator.left;
              operator = BooleanOperator.And;
              right;
            }
            |> Node.create ~location;
        | [operator, right] ->
            ComparisonOperator { ComparisonOperator.left; operator; right }
            |> Node.create ~location
        | _ ->
            failwith "The parser is a lie! Did not get a non-empty comparison list."
      in
      comparison left comparisons
    }

  | start = LAMBDA;
    parameters = separated_list(COMMA, lambda_parameter);
    COLON;
    body = test {
      {
        Node.location =  location_create_with_stop ~start ~stop:(Node.stop body);
        value = Lambda { Lambda.parameters; body }
      }
    }

  | expression = expression { expression }
  ;

test_with_generator:
  | generator = generator { generator }
  | test = test { test }
  ;

test:
  | test_without_ternary = test_without_ternary { test_without_ternary }

  | target = test_without_ternary;
    IF;
    test = test_list;
    ELSE;
    alternative = test {
      {
        Node.location = target.Node.location;
        value = Ternary { Ternary.target; test; alternative };
      }
    }
  ;

test_list:
  | items = separated_nonempty_list_indicator(COMMA, test) {
      match items with
      | head :: [], has_trailing_comma ->
        if has_trailing_comma then
          {
            Node.location = head.Node.location;
            value = Tuple [head];
          }
        else
          head
      | head::_, _ ->
          {
            Node.location = head.Node.location;
            value = Tuple (fst items);
          }
      | _ -> raise (ParserError "invalid atom")
    }
  ;

yield:
  | position = YIELD; has_from = FROM?; test = test_list?; {
      let start, stop = position in
      let location =
        Option.map
         ~f:(fun test -> location_create_with_stop ~start ~stop:(Node.stop test))
         test
        |> Option.value ~default:(Location.create ~start ~stop)
      in
      has_from <> None,
      {
        Node.location;
        value = Expression.Yield test;
      }
    }
  ;

%inline boolean_operator:
  | AND { BooleanOperator.And }
  | OR { BooleanOperator.Or }
  ;

%inline binary_operator:
  | PLUS { Add }
  | AT { At }
  | AMPERSAND { BitAnd }
  | BAR { BitOr }
  | HAT { BitXor }
  | SLASH; SLASH { FloorDivide }
  | SLASH { Divide }
  | LEFTANGLELEFTANGLE { LeftShift }
  | PERCENT { Modulo }
  | ASTERIKS; ASTERIKS { Power }
  | ASTERIKS { Multiply }
  | RIGHTANGLERIGHTANGLE { RightShift }
  | MINUS { Subtract }
  ;

%inline compound_operator:
  | PLUSEQUALS { Add }
  | ATEQUALS { At }
  | AMPERSANDEQUALS { BitAnd }
  | BAREQUALS { BitOr }
  | HATEQUALS { BitXor }
  | SLASHSLASHEQUALS { FloorDivide }
  | SLASHEQUALS { Divide }
  | LEFTANGLELEFTANGLEEQUALS { LeftShift }
  | PERCENTEQUALS { Modulo }
  | ASTERIKSASTERIKSEQUALS { Power }
  | ASTERIKSEQUALS { Multiply }
  | RIGHTANGLERIGHTANGLEEQUALS { RightShift }
  | MINUSEQUALS { Subtract }
  ;

%inline unary_operator:
  | position = TILDE { position, UnaryOperator.Invert }
  | position = MINUS { position, UnaryOperator.Negative }
  | position = NOT { position, UnaryOperator.Not }
  | position = PLUS { position, UnaryOperator.Positive }
  ;

comparison:
  | DOUBLEEQUALS; operand = expression { ComparisonOperator.Equals, operand }
  | RIGHTANGLE; operand = expression { ComparisonOperator.GreaterThan, operand }
  | RIGHTANGLEEQUALS; operand = expression {
      ComparisonOperator.GreaterThanOrEquals, operand
    }
  | IN; operand = expression { ComparisonOperator.In, operand }
  | IS; operand = expression { ComparisonOperator.Is, operand }
  | ISNOT; operand = expression { ComparisonOperator.IsNot, operand }
  | LEFTANGLE; operand = expression { ComparisonOperator.LessThan, operand }
  | LEFTANGLEEQUALS; operand = expression {
      ComparisonOperator.LessThanOrEquals, operand
    }
  | EXCLAMATIONMARK; EQUALS; operand = expression {
      ComparisonOperator.NotEquals, operand
    }
  | NOT; IN; operand = expression { ComparisonOperator.NotIn, operand }
  ;

arguments:
  | arguments = separated_list(COMMA, argument) { arguments }
  | test = test_with_generator { [{ Argument.name = None; value = test }] }
  | test = generator; COMMA { [{ Argument.name = None; value = test }] }
  ;

argument:
  | identifier = identifier; EQUALS; value = test {
      {
        Argument.name = Some { Node.location = fst identifier; value = snd identifier };
        value;
      }
    }
  | value = test { { Argument.name = None; value } }
  ;

subscript_key:
  | index = test { index }
  | lower = test?; COLON; upper = test? {
      slice ~lower ~upper ~step:None
    }
  | lower = test?; COLON; upper = test?; COLON; step = test? {
      slice ~lower ~upper ~step
    }
  ;

(* Collections. *)
set_or_dictionary_entry:
  | test = test {
      match test with
      | { Node.value = Starred (Starred.Twice keywords); _ } ->
          Keywords keywords
      | _ ->
          Item test
    }
  | key = expression; COLON; value = test {
      Entry { Dictionary.key = key; value = value; }
    }
  ;

generator:
  | element = test; generators = comprehension+ {
      {
        Node.location = element.Node.location;
        value = Generator { Comprehension.element; generators };
      }
    }
  ;

comprehension:
  | async = ASYNC?; FOR; target = expression_list; IN; iterator = test_without_ternary;
    conditions = list(condition) {
      { Comprehension.target; iterator; conditions; async = Option.is_some async }
    }
  ;

condition:
  | IF; test = test_without_ternary { test }
  ;

(* Helper rule dumping ground. *)

separated_list(SEPARATOR, item):
  | { [] }
  | item = item { [item] }
  | item = item; SEPARATOR; rest = separated_list(SEPARATOR, item) {
      item::rest
    }
  ;

separated_nonempty_list_indicator_tail(SEPARATOR, item):
  | { [], false }
  | SEPARATOR { [], true }
  | SEPARATOR; item = item; rest = separated_nonempty_list_indicator_tail(SEPARATOR, item) {
      let rest, has_trailing = rest in
      item :: rest, has_trailing
    }
  ;

separated_nonempty_list_indicator(SEPARATOR, item):
  | item = item; rest = separated_nonempty_list_indicator_tail(SEPARATOR, item) {
      let rest, has_trailing = rest in
      item :: rest, has_trailing
    }
  ;


separated_nonempty_list(SEPARATOR, item):
  | item = item { [item] }
  | item = item; SEPARATOR; rest = separated_list(SEPARATOR, item) {
      item::rest
    }
  ;

separated_list_of_lists(SEPARATOR, list_item):
  | { [] }
  | list_item = list_item { [list_item] }
  | list_item = list_item; SEPARATOR;
    rest = separated_list_of_lists(SEPARATOR, list_item) {
      list_item::rest
    }
  ;

separated_nonempty_list_of_lists(SEPARATOR, list_item):
  | list_item = list_item { [list_item] }
  | list_item = list_item; SEPARATOR;
    rest = separated_list_of_lists(SEPARATOR, list_item) {
      list_item::rest
    }
  ;
