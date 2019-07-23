(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

%{
  open Core

  open Ast
  open Expression
  open Statement
  open Pyre

  type parser_expression =
    | Await of parser_expression_node
    | BooleanOperator of parser_expression_node BooleanOperator.t
    | Call of parser_expression_node Call.t
    | ComparisonOperator of parser_expression_node Expression.Record.ComparisonOperator.record
    | Complex of float
    | Dictionary of parser_expression_node Dictionary.t
    | DictionaryComprehension of
        (parser_expression_node Dictionary.entry, parser_expression_node) Comprehension.t
    | Ellipsis
    | False
    | Float of float
    | Generator of (parser_expression_node, parser_expression_node) Comprehension.t
    | Integer of int
    | Lambda of parser_expression_node Lambda.t
    | List of parser_expression_node list
    | ListComprehension of (parser_expression_node, parser_expression_node) Comprehension.t
    | Name of parser_expression_node Name.t
    | Parenthesis of parser_expression_node
    | Set of parser_expression_node list
    | SetComprehension of (parser_expression_node, parser_expression_node) Comprehension.t
    | Starred of parser_expression_node Starred.t
    | String of parser_expression_node StringLiteral.t
    | Ternary of parser_expression_node Ternary.t
    | True
    | Tuple of parser_expression_node list
    | UnaryOperator of parser_expression_node Expression.Record.UnaryOperator.record
    | Yield of parser_expression_node option

  and parser_expression_node = parser_expression Node.t

  let rec convert { Node.location; value } =
    let convert_entry { Dictionary.key; value } =
      { Dictionary.key = convert key; value = convert value}
    in
    let convert_generator { Comprehension.target; iterator; conditions; async } =
      {
        Comprehension.target = convert target;
        iterator = convert iterator;
        conditions = List.map ~f:convert conditions;
        async;
      }
    in
    match value with
    | Await expression -> Expression.Await (convert expression) |> Node.create ~location
    | BooleanOperator { left; operator; right } ->
        Expression.BooleanOperator { left = convert left; operator; right = convert right }
        |> Node.create ~location
    | Call { callee; arguments } ->
        Expression.Call {
          callee = convert callee;
          arguments = List.map ~f:convert_argument arguments;
        }
        |> Node.create ~location
    | ComparisonOperator { left; operator; right } ->
        Expression.ComparisonOperator { left = convert left; operator; right = convert right }
        |> Node.create ~location
    | Complex value -> Expression.Complex value |> Node.create ~location
    | Dictionary { Dictionary.entries; keywords } ->
        Expression.Dictionary {
          entries = List.map ~f:convert_entry entries;
          keywords = List.map ~f:convert keywords;
        }
        |> Node.create ~location
    | DictionaryComprehension { Comprehension.element; generators } ->
        Expression.DictionaryComprehension {
          element = convert_entry element;
          generators = List.map ~f:convert_generator generators;
        }
        |> Node.create ~location
    | Ellipsis -> Expression.Ellipsis |> Node.create ~location
    | False -> Expression.False |> Node.create ~location
    | Float value -> Expression.Float value |> Node.create ~location
    | Generator { Comprehension.element; generators } ->
        Expression.Generator {
          element = convert element;
          generators = List.map ~f:convert_generator generators;
        }
        |> Node.create ~location
    | Integer value -> Expression.Integer value |> Node.create ~location
    | Lambda { Lambda.parameters; body } ->
        Expression.Lambda {
          parameters = List.map ~f:convert_parameter parameters;
          body = convert body;
        }
        |> Node.create ~location
    | List expression_list ->
        Expression.List (List.map ~f:convert expression_list) |> Node.create ~location
    | ListComprehension { Comprehension.element; generators } ->
        Expression.ListComprehension {
          element = convert element;
          generators = List.map ~f:convert_generator generators;
        }
        |> Node.create ~location
    | Name (Name.Attribute { base; attribute; special }) ->
        Expression.Name (Name.Attribute { base = convert base; attribute; special })
        |> Node.create ~location
    | Name (Name.Identifier name) ->
        Expression.Name (Expression.Name.Identifier name) |> Node.create ~location
    | Parenthesis expression -> convert expression
    | Set expression_list ->
        Expression.Set (List.map ~f:convert expression_list) |> Node.create ~location
    | SetComprehension { Comprehension.element; generators } ->
        Expression.SetComprehension {
          element = convert element;
          generators = List.map ~f:convert_generator generators;
        }
        |> Node.create ~location
    | Starred (Once expression) ->
        Expression.Starred (Once (convert expression)) |> Node.create ~location
    | Starred (Twice expression) ->
        Expression.Starred (Twice (convert expression)) |> Node.create ~location
    | String { value; kind } ->
        let value =
          match kind with
          | Format expression_list ->
              Expression.String { value; kind = Format (List.map ~f:convert expression_list) }
          | Mixed mixed -> Expression.String { value; kind = Mixed mixed }
          | String -> Expression.String { value; kind = String }
          | Bytes -> Expression.String { value; kind = Bytes }
        in
        { Node.location; value }
    | Ternary { target; test; alternative } ->
        Expression.Ternary {
          target = convert target;
          test = convert test;
          alternative = convert alternative;
        }
        |> Node.create ~location
    | True -> Expression.True |> Node.create ~location
    | Tuple expression_list ->
        Expression.Tuple (List.map ~f:convert expression_list) |> Node.create ~location
    | UnaryOperator { UnaryOperator.operator; operand } ->
        Expression.UnaryOperator { operator; operand = convert operand } |> Node.create ~location
    | Yield expression -> Expression.Yield (expression >>| convert) |> Node.create ~location

  and convert_argument { Call.Argument.name; value } =
    { Call.Argument.name; value = convert value }

  and convert_parameter { Node.location; value = { Parameter.name; value; annotation } } =
    { Parameter.name; value = value >>| convert; annotation = annotation >>| convert }
    |> Node.create ~location

  let with_decorators decorators = function
    | { Node.location; value = Class value } ->
        let decorated = { value with Class.decorators = List.map ~f:convert decorators; } in
        { Node.location; value = Class decorated }
    | { Node.location; value = Define value } ->
        let signature =
          { value.signature with Define.decorators = List.map ~f:convert decorators }
        in
        let decorated = { value with signature } in
        { Node.location; value = Define decorated }
    | _ -> raise (ParserError "Cannot decorate statement")

  type entry =
    | Entry of parser_expression_node Dictionary.entry
    | Item of parser_expression_node
    | Keywords of parser_expression_node
    | Comprehension of parser_expression_node Comprehension.generator

  type entries = {
      entries: parser_expression_node Dictionary.entry list;
      items: parser_expression_node list;
      keywords: parser_expression_node list;
      comprehensions: parser_expression_node Comprehension.generator list;
    }


  let add_entry so_far = function
    | Entry entry ->
        { so_far with entries = entry :: so_far.entries }
    | Item item ->
        { so_far with items = item :: so_far.items }
    | Keywords keyword ->
        { so_far with keywords = keyword :: so_far.keywords }
    | Comprehension comprehension ->
        { so_far with comprehensions = comprehension :: so_far.comprehensions }

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

  let binary_operator
    ~compound
    ~left:({ Node.location; _ } as left)
    ~operator
    ~right:({ Node.location = { Location.stop; _ }; _ } as right) =
    let name =
      let name =
        match operator with
        | Add -> "add"
        | At -> "matmul"
        | BitAnd -> "and"
        | BitOr -> "or"
        | BitXor -> "xor"
        | Divide -> "truediv"
        | FloorDivide -> "floordiv"
        | LeftShift -> "lshift"
        | Modulo -> "mod"
        | Multiply -> "mul"
        | Power -> "pow"
        | RightShift -> "rshift"
        | Subtract -> "sub"
      in
      Format.asprintf "__%s%s__" (if compound then "i" else "") name
    in
    let callee =
      Name (Name.Attribute { base = left; attribute = name; special = true })
      |> Node.create ~location
    in
    Call { callee; arguments = [{ Call.Argument.name = None; value = right }] }
    |> Node.create ~location:{ location with stop }

  let slice ~lower ~upper ~step ~bound_colon ~step_colon =
    let increment ({ Location.start; stop; _ } as location) =
      let increment ({ Location.column; _ } as position) =
        { position with Location.column = column + 1 }
      in
      { location with Location.start = increment start; stop = increment stop }
    in
    let lower_location =
      match lower with
      | Some lower -> lower.Node.location
      | None -> Location.create ~start:bound_colon ~stop:bound_colon
    in
    let upper_location =
      match upper with
      | Some upper -> upper.Node.location
      | None -> Location.create ~start:bound_colon ~stop:bound_colon |> increment
    in
    let step_location =
      match step with
      | Some step -> step.Node.location
      | None ->
          begin
            match step_colon with
            | Some colon -> Location.create ~start:colon ~stop:colon |> increment
            | None ->
                begin
                  match upper with
                  | Some { Node.location = ({ stop; _ } as location); _ } ->
                      { location with start = stop }
                  | None -> Location.create ~start:bound_colon ~stop:bound_colon |> increment
                end
          end
    in
    let slice_location =
      { lower_location with Location.stop = step_location.Location.stop  }
    in
    let arguments =
      let argument argument location =
        let none =
          Name (Name.Identifier "None")
          |> Node.create ~location
        in
        Option.value argument ~default:none
      in
      [
        { Call.Argument.name = None; value = argument lower lower_location };
        { Call.Argument.name = None; value = argument upper upper_location };
        { Call.Argument.name = None; value = argument step step_location };
      ]
    in
    let callee =
      Name (Name.Identifier "slice")
      |> Node.create ~location:slice_location
    in
    Call { callee; arguments }
    |> Node.create ~location:slice_location


  let create_ellipsis (start, stop) =
    let location = Location.create ~start ~stop in
    Node.create Ellipsis ~location

  let create_ellipsis_after { Node.location; _ } =
    Node.create Ellipsis ~location:{ location with start = location.stop }

  let subscript_argument ~subscripts ~location =
    let value =
      match subscripts with
      | [subscript] -> subscript
      | subscripts -> { Node.location; value = Tuple subscripts }
    in
    { Call.Argument.name = None; value }

  let subscript_access subscript =
    let head, subscripts, subscript_location = subscript in
    let location = Node.location head in
    let callee =
      Name (Name.Attribute { base = head; attribute = "__getitem__"; special = true })
      |> Node.create ~location
    in
    Call { callee; arguments = [subscript_argument ~subscripts ~location] }
    |> Node.create ~location:{ subscript_location with start = location.start }

  let subscript_mutation ~subscript ~value ~annotation:_ =
    let head, subscripts, subscript_location = subscript in
    let callee =
      let location =
        { head.Node.location with Location.stop = subscript_location.Location.stop }
      in
      Name (Name.Attribute { base = head; attribute = "__setitem__"; special = true })
      |> Node.create ~location
    in
    let location =
      { head.Node.location with Location.stop = value.Node.location.Location.stop }
    in
    Call {
      callee;
      arguments = [subscript_argument ~subscripts ~location; { Call.Argument.name = None; value }];
    }
    |> Node.create ~location
    |> fun expression -> Expression (convert expression)
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

  let create_substring kind (string_position, (start, stop), value) =
    string_position,
    {
      Node.location = Location.create ~start ~stop;
      value = { StringLiteral.Substring.kind; value };
    }
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

%token <(Lexing.position * Lexing.position) * string list * string> SIGNATURE_COMMENT
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
%token <(Lexing.position * Lexing.position) * (Lexing.position * Lexing.position) * string> BYTES
%token <(Lexing.position * Lexing.position) * (Lexing.position * Lexing.position) * string> FORMAT
%token <(Lexing.position * Lexing.position) * string> IDENTIFIER
%token <(Lexing.position * Lexing.position) * (Lexing.position * Lexing.position) * string> STRING
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

%left NOT
%left BAR
%left HAT
%left AMPERSAND
%left PLUS MINUS
%left ASTERIKS PERCENT SLASH
%left TILDE
%left AT
%left DOT
%left LEFTANGLELEFTANGLE RIGHTANGLERIGHTANGLE

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
  | statements = parser_generator_separated_nonempty_list_of_lists(SEMICOLON, small_statement);
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
          ~compound:true
          ~left:(subscript_access subscript)
          ~operator:compound
          ~right:value
      in
      [subscript_mutation ~subscript ~value ~annotation:None]
  }
  | target = test_list;
    compound = compound_operator;
    value = value {
      let value = binary_operator ~compound:true ~left:target ~operator:compound ~right:value in
      [{
        Node.location = {
          target.Node.location with Location.stop =
            value.Node.location.Location.stop;
        };
        value = Assign {
          Assign.target = convert target;
          annotation = None;
          value = convert value;
          parent = None;
        };
      }]
    }
  | target = test_list;
    annotation = annotation {
      [{
        Node.location = {
          target.Node.location with Location.stop =
            annotation.Node.location.Location.stop;
        };
        value = Assign {
          Assign.target = convert target;
          annotation = Some annotation >>| convert;
          value = create_ellipsis_after annotation |> convert;
          parent = None;
        };
      }]
    }
  | target = test_list;
    annotation = comment_annotation {
      [{
        Node.location = {
          target.Node.location with Location.stop =
            annotation.Node.location.Location.stop;
        };
        value = Assign {
          Assign.target = convert target;
          annotation = Some annotation >>| convert;
          value = create_ellipsis_after annotation |> convert;
          parent = None;
        };
      }]
    }
  | target = test_list;
    annotation = annotation;
    EQUALS;
    value = test_list {
      [{
        Node.location = {
          target.Node.location with Location.stop =
            value.Node.location.Location.stop;
        };
        value = Assign {
          Assign.target = convert target;
          annotation = Some annotation >>| convert;
          value = convert value;
          parent = None;
        };
      }]
    }
  | targets = targets; value = value; annotation = comment_annotation? {
      List.map ~f:(fun target -> target ~value ~annotation) targets
  }
  | targets = targets; ellipsis = ELLIPSES {
      let value = create_ellipsis ellipsis in
      List.map ~f:(fun target -> target ~value ~annotation:None) targets
    }
  | target = test_list;
    annotation = annotation;
    EQUALS;
    ellipsis = ELLIPSES {
      let ellipsis = create_ellipsis ellipsis in
      [{
        Node.location = {
          target.Node.location with Location.stop =
            ellipsis.Node.location.Location.stop;
        };
        value = Assign {
          Assign.target = convert target;
          annotation = Some annotation >>| convert;
          value = convert ellipsis;
          parent = None;
        };
      }]
    }

  | start = ASSERT; test = test {
      [{
        Node.location = location_create_with_stop ~start ~stop:(Node.stop test);
        value = Assert { Assert.test = convert test; message = None; origin = Assert.Assertion }
      }]
    }
  | start = ASSERT; test = test;
    COMMA; message = test {
      [{
        Node.location = location_create_with_stop ~start ~stop:(Node.stop test);
        value = Assert {
          Assert.test = convert test;
          message = Some message >>| convert;
          origin = Assert.Assertion
        }
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
      [{ Node.location = test.Node.location; value = Expression (convert test) }]
    }

  | start = GLOBAL; globals = parser_generator_separated_nonempty_list(COMMA, identifier) {
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

  | start = NONLOCAL; nonlocals = parser_generator_separated_nonempty_list(COMMA, identifier) {
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
        | _, Some { Node.location; _ } ->
          location_create_with_stop ~start ~stop:(location.Location.stop)
      in
      [{
        Node.location;
        value = Raise { Raise.expression = test >>| convert; from = raise_from >>| convert };
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
        value = Return { Return.expression = test >>| convert; is_implicit = false };
      }]
    }

  | delete = DELETE;
    expression = expression_list {
      let stop = Node.stop expression in
      [{
        Node.location = location_create_with_stop ~start:delete ~stop;
        value = Delete (convert expression);
      }]
    }

  | yield = yield {
      let has_from, yield = yield in
      let location = Node.location yield in
      if has_from then
        let yield =
          match yield with
          | { Node.value = Yield (Some yield); _ } ->
              let callee =
                Name (
                  Name.Attribute {
                    base = yield;
                    attribute = "__iter__";
                    special = true
                  }
                ) |> Node.create ~location
              in
              Call { callee; arguments = [] }
              |> Node.create ~location
          | _ ->
              yield
        in
        [
          {
            Node.location;
            value = YieldFrom { Node.location; value = Yield (Some (convert yield)) }
          };
        ]
      else
        [{ Node.location; value = Yield (convert yield) }]
    }
  ;

raise_from:
  | FROM; test_list = test_list { test_list }
  ;

compound_statement:
  | definition = CLASS; name = reference;
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
              let signature = { define.signature with Define.parent = Some name } in
              {
                Node.location;
                value = Define { define with signature };
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
      let convert_argument { Expression.Call.Argument.name; value } =
        { Expression.Call.Argument.name; value = convert value}
      in
      {
        Node.location;
        value = Class {
          Class.name;
          bases = List.map ~f:convert_argument bases;
          body;
          decorators = [];
          docstring = Statement.extract_docstring body;
        };
      }
    }

  | definition = DEFINE; name = reference;
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
          >>= (fun ((start, stop), _, return_annotation) ->
              Some {
                Node.location = Location.create ~start ~stop;
                value = String (StringLiteral.create return_annotation);
              }
            )
      in
      let parameters =
        match signature_comment with
        | Some ((start, stop), parameter_annotations, _)
          when not (List.is_empty parameter_annotations) ->
            let add_annotation ({ Node.value = parameter; _ } as parameter_node) annotation =
                match annotation with
                | None ->
                    parameter_node
                | Some annotation -> {
                    parameter_node with
                    Node.value = {
                      parameter with
                        Parameter.annotation = Some {
                          Node.location = Location.create ~start ~stop;
                          value = String (StringLiteral.create annotation);
                        };
                      }
                  }
            in
            (* We don't know whether a define is a method at this point, and mypy's documentation
               specifies that a method's self should NOT be annotated:
               `https://mypy.readthedocs.io/en/latest/python2.html`.

                Because we don't know whether we are parsing a method at this point or whether
                there's any decorators that mean a function doesn't have a self parameter, we make
                the angelic assumption that annotations lacking a single annotation knowingly elided
                the self annotation. *)
            let unannotated_parameter_count =
               List.length parameters - List.length parameter_annotations
            in
            if unannotated_parameter_count = 0 || unannotated_parameter_count = 1 then
              let parameter_annotations =
                List.init ~f:(fun _ -> None) unannotated_parameter_count @
                List.map ~f:Option.some parameter_annotations
              in
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
          signature = {
            name = snd name;
            parameters = List.map ~f:convert_parameter parameters;
            decorators = [];
            return_annotation = annotation >>| convert;
            async = false;
            parent = None;
            docstring = docstring;
          };
          body;
        };
      }
    }

  | start = FOR; target = expression_list; IN; iterator = test_list; COLON;
    ANNOTATION_COMMENT?; body = block; orelse = named_optional_block(ELSE) {
      let stop = begin match orelse with
      | _, [] -> (fst body).Location.stop
      | location, _ -> location.Location.stop
      end in
      {
        Node.location = location_create_with_stop ~start ~stop;
        value = For {
          For.target = convert target;
          iterator = convert iterator;
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
        end.Location.stop
      in
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
    items = parser_generator_separated_nonempty_list(COMMA, with_item); COLON;
    body = block {
      let convert_item (expression, expression_option) =
        (convert expression, expression_option >>| convert)
      in
      {
        Node.location = location_create_with_stop ~start ~stop:(fst body).Location.stop;
        value = With {
          With.items = List.map ~f:convert_item items;
          body = snd body;
          async = false;
        };
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
        value = While { While.test = convert test; body = snd body; orelse = snd orelse };
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
          let signature = { value.signature with Define.async = true } in
          let decorated = { value with signature } in
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
  | ellipsis = ELLIPSES; NEWLINE
  | NEWLINE+; INDENT; ellipsis = ELLIPSES; NEWLINE; DEDENT; NEWLINE* {
    let location = Location.create ~start:(fst ellipsis) ~stop:(snd ellipsis) in
    let body = [Node.create (Expression (Node.create Expression.Ellipsis ~location)) ~location] in
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
      If { If.test = convert test; body = snd body; orelse = snd orelse }
    }
  | test = test_list; COLON;
    body = block;
    else_start = ELSEIF; value = conditional {
      let stop = (fst value).Location.stop in
      { test.Node.location with Location.stop },
      If {
        If.test = convert test;
        body = (snd body);
        orelse = [{
          Node.location =
            location_create_with_stop ~start:else_start ~stop;
          value = snd value
        }];
      }
    }
 ;

bases:
  | { [] }
  | LEFTPARENS; bases = parser_generator_separated_list(COMMA, argument); RIGHTPARENS {
      bases
    }
  ;

decorator:
  | AT; expression = expression; NEWLINE+ { expression }
  ;

identifier:
  | identifier = IDENTIFIER {
      let start, stop = fst identifier in
      Location.create ~start ~stop, snd identifier
    }
  | position = ASYNC {
      Location.create ~start:(fst position) ~stop:(snd position),
      "async"
    }
  ;

reference:
  | identifiers = parser_generator_separated_nonempty_list(DOT, identifier) {
      let location =
        let (start, _) = List.hd_exn identifiers in
        let (stop, _) = List.last_exn identifiers in
        { start with Location.stop = stop.Location.stop }
      in
      let reference =
        List.map ~f:snd identifiers
        |> Reference.create_from_list
      in
      location, reference
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
            Parameter.name = "*";
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
        Node.location = { (fst name) with Location.stop = value.Node.location.Location.stop };
        value = { Parameter.name = snd name; value = Some value; annotation = None};
      }
    }
  ;

%inline name:
  | expression = expression {
      let rec identifier expression =
        match expression with
        | { Node.location; value = Name (Name.Identifier identifier) } ->
            (location, identifier)
        | { Node.location; value = Starred (Starred.Once expression) } ->
            location,
            identifier expression
            |> snd
            |> fun identifier -> "*" ^ identifier
        | { Node.location; value = Starred (Starred.Twice expression) } ->
            location,
            identifier expression
            |> snd
            |> fun identifier -> "**" ^ identifier
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
      annotation
      |> String.strip ~drop:(function | '\'' | '"' -> true | _ -> false)
      |> StringLiteral.create
      |> fun string -> String string
      |> Node.create ~location:(Location.create ~start ~stop)
    }

%inline return_annotation:
  | MINUS; RIGHTANGLE; expression = expression { expression }
  ;

%inline subscript:
  | head = expression;
    left = LEFTBRACKET;
    subscripts = parser_generator_separated_nonempty_list(COMMA, subscript_key);
    right = RIGHTBRACKET {
      head, subscripts, Location.create ~start:left ~stop:right
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
      { Try.kind = Some kind >>| convert; name = None; handler_body = snd handler_body }
    }
  | start = EXCEPT;
    kind = expression; AS; name = identifier;
    COLON; handler_body = block
  | start = EXCEPT;
    kind = expression; COMMA; name = identifier;
    COLON; handler_body = block {
      location_create_with_stop ~start ~stop:(fst handler_body).Location.stop,
      { Try.kind = Some kind >>| convert; name = Some (snd name); handler_body = snd handler_body }
    }
  | start = EXCEPT;
    kind = or_test; COLON; handler_body = block {
      location_create_with_stop ~start ~stop:(fst handler_body).Location.stop,
      { Try.kind = Some kind >>| convert; name = None; handler_body = snd handler_body }
    }
  | start = EXCEPT;
    kind = or_test; AS; name = identifier;
    COLON; handler_body = block {
      location_create_with_stop ~start ~stop:(fst handler_body).Location.stop,
      { Try.kind = Some kind >>| convert; name = Some (snd name); handler_body = snd handler_body }
    }
  ;

from:
  | from = from_string {
      Reference.create from
      |> Option.some
    }
  ;

from_string:
  | { "" }
  | identifier = identifier; from_string = from_string {
      (snd identifier) ^ from_string
    }
  | relative = nonempty_list(ellipsis_or_dot);
    from_string = from_string {
      (String.concat relative) ^ from_string
    }
  ;

ellipsis_or_dot:
  | DOT { "." }
  | ELLIPSES { "..." }
  ;

imports:
  | imports = parser_generator_separated_nonempty_list(COMMA, import) {
      let location =
        let (start, _) = List.hd_exn imports in
        let (stop, _) = List.last_exn imports in
        { start with Location.stop = stop.Location.stop }
      in
      location, List.map ~f:snd imports
    }
  | start = LEFTPARENS;
    imports = parser_generator_separated_nonempty_list(COMMA, import);
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
        Import.name = Reference.create "*";
        alias = None;
      }
    }
  | name = reference {
      fst name,
      {
        Import.name = snd name;
        alias = None;
      }
    }
  | name = reference;
    AS; alias = identifier {
      {(fst name) with Location.stop = (fst alias).Location.stop},
      {
        Import.name = snd name;
        alias = Some (Reference.create (snd alias));
      }
    }
  ;

%inline target:
  | target = test_list {
      let assignment_with_annotation ~value ~annotation =
        {
          Node.location = {
            target.Node.location with Location.stop =
              value.Node.location.Location.stop;
          };
          value = Assign {
            Assign.target = convert target;
            annotation = annotation >>| convert;
            value = convert value;
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
        value = Name (Name.Identifier (snd identifier));
      }
    }

  | ellipsis = ELLIPSES {
      let location = Location.create ~start:(fst ellipsis) ~stop:(snd ellipsis) in
      Node.create Ellipsis ~location
    }

  | left = expression;
    operator = binary_operator;
    right = expression; {
      binary_operator ~compound:false ~left ~operator ~right
    }

  | bytes = BYTES+ {
      let (start, stop), _, _ = List.hd_exn bytes in
      {
        Node.location = Location.create ~start ~stop;
        value = String (
          StringLiteral.create
            ~bytes:true
            (String.concat (List.map bytes ~f:(fun (_, _, value) -> value)))
        );
      }
    }

  | format = FORMAT; mixed_string = mixed_string {
      let all_strings = create_substring StringLiteral.Substring.Format format :: mixed_string in
      let all_pieces = List.map all_strings ~f:snd in
      let (head, _), (last, _) = List.hd_exn all_strings, List.last_exn all_strings in
      let (start, _) = head in
      let (_, stop) = last in
      {
        Node.location = Location.create ~start ~stop;
        value = String (StringLiteral.create_mixed all_pieces);
      }
    }

  | name = expression;
    start = LEFTPARENS;
    arguments = arguments;
    stop = RIGHTPARENS {
      let call_location = Location.create ~start ~stop in
      Call { callee = name; arguments }
      |> Node.create
        ~location:({ name.Node.location with Location.stop = call_location.Location.stop })
    }

  | set_or_dictionary = set_or_dictionary {
      set_or_dictionary
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
    items = parser_generator_separated_list(COMMA, test);
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

  | position = ASTERIKS; test = expression {
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
      let all_strings = create_substring StringLiteral.Substring.Literal string :: mixed_string in
      let all_pieces = List.map all_strings ~f:snd in
      let (head, _), (last, _) = List.hd_exn all_strings, List.last_exn all_strings in
      let (start, _) = head in
      let (_, stop) = last in
      {
        Node.location = Location.create ~start ~stop;
        value = String (StringLiteral.create_mixed all_pieces);
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

  | start = LEFTPARENS; test = test_list; stop = RIGHTPARENS {
      {
        Node.location = Location.create ~start ~stop;
        value = Parenthesis test;
      }
    }

  | expression = expression; DOT; identifier = identifier {
      let location =
        { expression.Node.location with Location.stop = Location.Reference.stop (fst identifier) }
      in
      {
        Node.location;
        value = Name (
          Name.Attribute { base = expression; attribute = snd identifier; special = false }
        )
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
      create_substring StringLiteral.Substring.Format first_string :: rest
    }
  | first_string = STRING; rest = mixed_string {
      create_substring StringLiteral.Substring.Literal first_string :: rest
    }
  ;

comparison:
  | expression = expression { expression }

  | left = expression; comparisons = nonempty_list(comparison_operator) {
      let rec comparison ({ Node.location; _ } as left) comparisons =
        match comparisons with
        | (operator, right) :: comparisons when List.length comparisons > 0 ->
            let left =
              ComparisonOperator { ComparisonOperator.left; operator; right }
              |> Node.create ~location:({ location with Location.stop = Node.stop right })
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
            |> Node.create ~location:({ location with Location.stop = Node.stop right })
        | _ ->
            failwith "The parser is a lie! Did not get a non-empty comparison list."
      in
      comparison left comparisons
    }
  ;

not_test:
  | comparison = comparison { comparison }
  | start = NOT; not_test = not_test {
      let location = location_create_with_stop ~start ~stop:(Node.stop not_test) in
      {
        Node.location;
        value = UnaryOperator {
          UnaryOperator.operator = UnaryOperator.Not;
          operand = not_test;
        }
      }
  }
  ;

and_test:
  | not_test = not_test { not_test }
  | left = not_test; AND; right = and_test {
      let location = { (Node.location left) with Location.stop = Node.stop right } in
      {
        Node.location;
        value = BooleanOperator {
          BooleanOperator.left;
          operator = BooleanOperator.And;
          right;
        }
      }
   }
  ;

or_test:
  | and_test = and_test { and_test }
  | left = and_test; OR; right = or_test {
      let location = { (Node.location left) with Location.stop = Node.stop right } in
      {
        Node.location;
        value = BooleanOperator {
          BooleanOperator.left;
          operator = BooleanOperator.Or;
          right;
        }
      }
   }
  ;

test_with_generator:
  | generator = generator { generator }
  | test = test { test }
  ;

test:
  | or_test = or_test { or_test }

  | target = or_test;
    IF;
    test = test_list;
    ELSE;
    alternative = test {
      {
        Node.location = { target.Node.location with Location.stop = Node.stop alternative };
        value = Ternary { Ternary.target; test; alternative };
      }
    }
  | start = LAMBDA;
    parameters = parser_generator_separated_list(COMMA, lambda_parameter);
    COLON;
    body = test {
      {
        Node.location =  location_create_with_stop ~start ~stop:(Node.stop body);
        value = Lambda { Lambda.parameters; body }
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
      | (head :: _ as items), _ ->
          let last = List.last_exn items in
          {
            Node.location = { head.Node.location with Location.stop = Node.stop last };
            value = Tuple items;
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
        value = Yield test;
      }
    }
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

comparison_operator:
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

arguments:
  | arguments = parser_generator_separated_list(COMMA, argument) { arguments }
  | test = test_with_generator { [{ Call.Argument.name = None; value = test }] }
  | test = generator; COMMA { [{ Call.Argument.name = None; value = test }] }
  ;

argument:
  | identifier = identifier; EQUALS; value = test {
      {
        Call.Argument.name = Some { Node.location = fst identifier; value = snd identifier };
        value;
      }
    }
  | value = test { { Call.Argument.name = None; value } }
  ;

subscript_key:
  | index = test { index }
  | lower = test?; bound_colon = COLON; upper = test? {
      slice ~lower ~upper ~step:None ~bound_colon ~step_colon:None
    }
  | lower = test?; bound_colon = COLON; upper = test?; step_colon = COLON; step = test? {
      slice ~lower ~upper ~step ~bound_colon ~step_colon:(Some step_colon)
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
  | key = test; COLON; value = test {
      Entry { Dictionary.key = key; value = value; }
    }
  ;

set_or_dictionary_maker:
  | entry = set_or_dictionary_entry {
      add_entry { entries = []; comprehensions = []; items = []; keywords = [] } entry
    }
  | items = set_or_dictionary_maker; COMMA; entry = set_or_dictionary_entry {
      add_entry items entry
    }
  | items = set_or_dictionary_maker; comprehension = comprehension {
      add_entry items (Comprehension comprehension)
    }
  ;

set_or_dictionary:
  | start = LEFTCURLY; stop = RIGHTCURLY {
      {
        Node.location = Location.create ~start ~stop;
        value = Dictionary { Dictionary.entries = []; keywords = [] };
      }
    }
  | start = LEFTCURLY; items = set_or_dictionary_maker; COMMA?; stop = RIGHTCURLY {
      let value =
        match items with
        | { entries; keywords; comprehensions = []; items = [] } ->
             Dictionary { Dictionary.entries = List.rev entries; keywords = List.rev keywords }
        | { entries = [entry]; keywords = []; items = []; comprehensions  } ->
              DictionaryComprehension {
                Comprehension.element = entry;
                generators = List.rev comprehensions;
              }
        | { items; entries = []; comprehensions = []; keywords = [] } ->
               Set (List.rev items)
        | { items = [item]; comprehensions; keywords = []; entries = [] } ->
             SetComprehension {
               Comprehension.element = item;
               generators = List.rev comprehensions;
             }
        | _ -> failwith "Invalid dictionary or set"
      in
      { Node.location = Location.create ~start ~stop; value }
    }

generator:
  | element = test; generators = comprehension+ {
      let stop =
        let { Comprehension.iterator; conditions; _ } = List.last_exn generators in
        match List.rev conditions with
        | [] -> Node.stop iterator
        | condition :: _ -> Node.stop condition
      in
      {
        Node.location = { element.Node.location with Location.stop };
        value = Generator { Comprehension.element; generators };
      }
    }
  ;

comprehension:
  | ASYNC; FOR; target = expression_list; IN; iterator = or_test;
    conditions = list(condition) {
      { Comprehension.target; iterator; conditions; async = true }
    }
  | FOR; target = expression_list; IN; iterator = or_test;
    conditions = list(condition) {
      { Comprehension.target; iterator; conditions; async = false }
    }

  ;

condition:
  | IF; test = or_test { test }
  ;

(* Helper rule dumping ground. *)

parser_generator_separated_list(SEPARATOR, item):
  | { [] }
  | item = item { [item] }
  | item = item; SEPARATOR; rest = parser_generator_separated_list(SEPARATOR, item) {
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


parser_generator_separated_nonempty_list(SEPARATOR, item):
  | item = item { [item] }
  | item = item; SEPARATOR; rest = parser_generator_separated_list(SEPARATOR, item) {
      item::rest
    }
  ;

parser_generator_separated_list_of_lists(SEPARATOR, list_item):
  | { [] }
  | list_item = list_item { [list_item] }
  | list_item = list_item; SEPARATOR;
    rest = parser_generator_separated_list_of_lists(SEPARATOR, list_item) {
      list_item::rest
    }
  ;

parser_generator_separated_nonempty_list_of_lists(SEPARATOR, list_item):
  | list_item = list_item { [list_item] }
  | list_item = list_item; SEPARATOR;
    rest = parser_generator_separated_list_of_lists(SEPARATOR, list_item) {
      list_item::rest
    }
  ;
