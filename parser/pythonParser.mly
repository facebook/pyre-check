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
    | { Node.location; value = Stub (Stub.Class definition) } ->
        let decorated = { definition with Class.decorators = decorators; } in
        { Node.location; value = Stub (Stub.Class decorated) }
    | { Node.location; value = Stub (Stub.Define definition) } ->
        let decorated = { definition with Define.decorators = decorators; } in
        { Node.location; value = Stub (Stub.Define decorated) }
    | _ -> raise (ParserError "Cannot decorate statement")

  let extract_access access =
    match access with
    | { Node.value = Access access; _ } -> access
    | _ -> [Access.Expression access]

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
    (List.rev entries), List.hd keywords, (List.rev items)

  (* Helper function to combine a start position of type Lexing.position and
   * stop position of type Location.position. *)
  let location_create_with_stop ~start ~stop =
    let position = Location.create ~start ~stop:start in
    { position with Location.stop = stop }

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
%token STUB
%token <string> SIGNATURE_COMMENT
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
%token ELLIPSES
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

%left OR
%left AND
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
  | { Location.any, [] }
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
  | target = test_list;
    compound = compound_operator;
    value = test_list {
      [{
        Node.location = {
          target.Node.location with Location.stop =
            value.Node.location.Location.stop;
        };
        value = Assign {
          Assign.target;
          annotation = None;
          value = Some value;
          compound = Some compound;
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
          value = None;
          compound = None;
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
          value = Some value;
          compound = None;
          parent = None;
        };
      }]
    }
  | targets = targets; value = value {
      let assign target =
      {
        Node.location = target.Node.location;
        value = Assign {
          Assign.target;
          annotation = None;
          value = Some value;
          compound = None;
          parent = None;
        };
      } in
      List.map ~f:assign targets
  }
  | targets = targets; ELLIPSES {
      let assign target =
      {
        Node.location = target.Node.location;
        value = Stub
          (Stub.Assign {
            Stub.value = target;
            annotation = None;
            parent = None;
          });
      } in
      List.map ~f:assign targets
    }
  | targets = targets; STUB; annotation = value {
      let assign target =
      {
        Node.location = target.Node.location;
        value = Stub
          (Stub.Assign {
            Stub.value = target;
            annotation = Some annotation;
            parent = None;
          });
      } in
      List.map ~f:assign targets
    }
  | target = test_list;
    annotation = annotation;
    EQUALS;
    ELLIPSES {
      [{
        Node.location = target.Node.location;
        value = Stub
          (Stub.Assign {
            Stub.value = target;
            annotation = Some annotation;
            parent = None;
          });
      }]
    }

  | start = ASSERT; test = test_list {
      [{
        Node.location = location_create_with_stop ~start ~stop:(Node.stop test);
        value = Assert { Assert.test; message = None }
      }]
    }
  | start = ASSERT; test = expression;
    COMMA; message = expression {
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
        value = Return test;
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
      if has_from then
        [{
          Node.location = yield.Node.location;
          value = Statement.YieldFrom yield;
        }]
      else
        [{
          Node.location = yield.Node.location;
          value = Statement.Yield yield;
        }]
    }
  ;

raise_from:
  | FROM; test_list = test_list { test_list.Node.location }
  ;

compound_statement:
  | definition = CLASS; name = simple_access;
    bases = bases; colon_position = COLON;
    body = block_or_stub {
      let location = Location.create ~start:definition ~stop:colon_position in
      match body with
      | Some (body_location, body) ->
          let location = { location with Location.stop = body_location.Location.stop } in
          let _, name = name in
          let body =
            let rec transform_toplevel_statements = function
              | { Node.location; value = Assign assign } ->
                  [{
                    Node.location;
                    value = Assign { assign with Assign.parent = Some name };
                  }]
              | { Node.location; value = Stub (Stub.Assign assign) } ->
                  [{
                    Node.location;
                    value = Stub
                      (Stub.Assign { assign with Stub.parent = Some name });
                  }]
              | { Node.location; value = Define define } ->
                  [{
                    Node.location;
                    value = Define { define with Define.parent = Some name };
                  }]
              | { Node.location; value = Stub (Stub.Define define) } ->
                  [{
                    Node.location;
                    value = Stub
                      (Stub.Define { define with Define.parent = Some name });
                  }]
              | {
                  Node.value = If {
                    If.test = {
                      Node.value = ComparisonOperator {
                          ComparisonOperator.left;
                          right = [
                            ComparisonOperator.GreaterThanOrEquals,
                            { Node.value = Tuple ({ Node.value = Integer major; _ } :: _ ); _ }
                          ];
                        };
                      _;
                    };
                    body;
                    _
                  };
                  _
                } when (Expression.show left) = "sys.version_info" && major = 3 ->
                  List.concat_map ~f:transform_toplevel_statements body
              | statement ->
                  [statement]
            in
            List.concat_map ~f:transform_toplevel_statements body in
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
      | None ->
          {
            Node.location;
            value = Stub
              (Stub.Class {
                Class.name = snd name;
                bases; body = [];
                decorators = [];
                docstring = None
              });
          }
    }

  | definition = DEFINE; name = simple_access;
    LEFTPARENS;
    parameters = separated_list(COMMA, define_parameter);
    RIGHTPARENS;
    return_annotation = return_annotation?;
    colon_position = COLON;
    signature_comment = SIGNATURE_COMMENT?;
    body = block_or_stub {
      match body with
      | Some body ->
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
              >>= (fun signature_comment ->
                  Some {
                    Node.location;
                    value = Access (Instantiated.Access.create signature_comment)
                  }
                )
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
      | None ->
          {
            Node.location = Location.create ~start:definition ~stop:colon_position;
            value = Stub
              (Stub.Define {
                Define.name = snd name;
                parameters;
                body = [];
                decorators = [];
                return_annotation;
                async = false;
                parent = None;
                docstring = None;
              });
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
        (match handlers, snd orelse, snd finally with
        | _, _, (_::_) -> fst finally
        | _, (_::_), [] -> fst orelse
        | (_::_), [], [] -> (fst (List.last_exn handlers))
        | _ -> (fst body)).Location.stop in
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

block_or_stub:
  | ELLIPSES; NEWLINE
  | STUB; value; NEWLINE
  | NEWLINE+; INDENT; ELLIPSES; NEWLINE; DEDENT; NEWLINE* { None }
  | statements = block { Some statements }
  ;

block:
  | simple_statement = simple_statement; { simple_statement }
  | NEWLINE+; INDENT; statements = statements; DEDENT; NEWLINE* {
      statements
    }
  ;

named_optional_block(NAME):
  | { Location.any, [] }
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
  | name = name; annotation = annotation?; EQUALS; ELLIPSES {
      {
        Node.location = fst name;
        value = {
            Parameter.name = snd name;
            value = Some (Node.create (Access (Instantiated.Access.create "...")));
            annotation;
        };
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
           (location,
             Identifier.append
              ~separator:""
              (Identifier.create "*")
              (identifier expression |> snd))
        | { Node.location; value = Starred (Starred.Twice expression) } ->
           (location,
            Identifier.append
              ~separator:""
              (Identifier.create "**")
              (identifier expression |> snd))
        | _ ->
            raise (ParserError "Unexpected parameters") in
      identifier expression
    }
  ;

%inline annotation:
  | COLON; expression = expression { expression }
  ;

%inline return_annotation:
  | MINUS; RIGHTANGLE; expression = expression { expression }
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
      Instantiated.Access.create from
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
        Import.name = Instantiated.Access.create "*";
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

targets:
  | test = test_list; EQUALS { [test] }
  | targets = targets; test = test_list; EQUALS { targets @ [test] }
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

  | left = expression;
    operator = binary_operator;
    right = expression; {
      {
        Node.location = left.Node.location;
        value = BinaryOperator { BinaryOperator.left; operator; right };
      }
    }

  | bytes = BYTES+ {
      let start, stop = fst (List.hd_exn bytes) in
      {
        Node.location = Location.create ~start ~stop;
        value = Bytes (String.concat (List.map bytes ~f:snd));
      }
    }

  | format = FORMAT; mixed_string = mixed_string {
      let all_strings = format::mixed_string in
      let (head, _), (last, _) = List.hd_exn all_strings, List.last_exn all_strings in
      let (start, _) = head in
      let (_, stop) = last in
      {
        Node.location = Location.create ~start ~stop;
        value = Format ((snd format) ^ String.concat (List.map mixed_string ~f:snd));
      }
    }

  | name = expression;
    LEFTPARENS;
    arguments = arguments;
    RIGHTPARENS {
      {
        Node.location = name.Node.location;
        value = Access [
          Access.Call {
            Node.location = name.Node.location;
            value = { Call.name; arguments };
          };
        ];
      }
    }

  | start = LEFTCURLY;
    entries = separated_list(COMMA, set_or_dictionary_entry);
    stop = RIGHTCURLY {
      let value =
        match extract_entries entries with
        | entries, keywords, [] -> Dictionary { Dictionary.entries; keywords }
        | [], None, items -> Set items
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
          ((snd string) ^ String.concat ~sep:"" (List.map mixed_string ~f:snd));
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
        value = Access ((extract_access access) @ (extract_access expression));
      }
    }

  | head = expression;
    LEFTBRACKET; subscripts = separated_nonempty_list(COMMA, subscript);
    RIGHTBRACKET {
      {
        Node.location = head.Node.location;
        value = Access ((extract_access head) @ [Access.Subscript subscripts]);
      }
    }

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

  | left = expression; right = nonempty_list(comparison) {
      {
        Node.location = left.Node.location;
        value = ComparisonOperator { ComparisonOperator.left; right };
      }
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
  | PLUS { BinaryOperator.Add }
  | AT { BinaryOperator.At }
  | AMPERSAND { BinaryOperator.BitAnd }
  | BAR { BinaryOperator.BitOr }
  | HAT { BinaryOperator.BitXor }
  | SLASH; SLASH { BinaryOperator.FloorDivide }
  | SLASH { BinaryOperator.Divide }
  | LEFTANGLELEFTANGLE { BinaryOperator.LeftShift }
  | PERCENT { BinaryOperator.Modulo }
  | ASTERIKS; ASTERIKS { BinaryOperator.Power }
  | ASTERIKS { BinaryOperator.Multiply }
  | RIGHTANGLERIGHTANGLE { BinaryOperator.RightShift }
  | MINUS { BinaryOperator.Subtract }
  ;

%inline compound_operator:
  | PLUSEQUALS { BinaryOperator.Add }
  | ATEQUALS { BinaryOperator.At }
  | AMPERSANDEQUALS { BinaryOperator.BitAnd }
  | BAREQUALS { BinaryOperator.BitOr }
  | HATEQUALS { BinaryOperator.BitXor }
  | SLASHSLASHEQUALS { BinaryOperator.FloorDivide }
  | SLASHEQUALS { BinaryOperator.Divide }
  | LEFTANGLELEFTANGLEEQUALS { BinaryOperator.LeftShift }
  | PERCENTEQUALS { BinaryOperator.Modulo }
  | ASTERIKSASTERIKSEQUALS { BinaryOperator.Power }
  | ASTERIKSEQUALS { BinaryOperator.Multiply }
  | RIGHTANGLERIGHTANGLEEQUALS { BinaryOperator.RightShift }
  | MINUSEQUALS { BinaryOperator.Subtract }
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
      { Argument.name = Some (snd identifier); value; }
    }
  | value = test { { Argument.name = None; value } }
  ;

subscript:
  | index = test { Access.Index index }
  | ELLIPSES {
      Access.Index (Node.create (Access (Instantiated.Access.create "...")))
    }
  | lower = test?; COLON; upper = test? {
      Access.Slice { Access.lower; upper; step = None }
    }
  | lower = test?; COLON; upper = test?; COLON; step = test? {
      Access.Slice { Access.lower; upper; step }
    }
  ;

(* Collections. *)
set_or_dictionary_entry:
  | test = test {
      match test with
      | { Node.value = Starred (Starred.Twice _); _ } ->
          Keywords test
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
