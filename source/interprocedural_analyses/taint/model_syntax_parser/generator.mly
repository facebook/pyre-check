/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

%{
  open Core

  module Location = Ast.Location
  open ParserExpression
  open ParserStatement

  (* This weird-looking empty module definition is to work around a nasty issue when *)
  (* using menhir infer mode with dune: https://github.com/ocaml/dune/issues/2450 *)
  [@@@warning "-60"]
  module PysaModelSyntaxParser = struct end
  [@@@warning "+60"]

  let with_decorators decorators decoratee =
    match decoratee with
    | { Node.location; value = Statement.Class value } ->
        let decorated = { value with Class.decorators; } in
        { Node.location; value = Statement.Class decorated }
    | { Node.location; value = Statement.Define ({ Define.signature; _} as value) } ->
        let signature =
          { signature with Define.Signature.decorators }
        in
        let decorated = { value with Define.signature } in
        { Node.location; value = Statement.Define decorated }
    | _ -> raise (Failure "Cannot decorate statement")

  (* Helper function to combine a start position of type Lexing.position and
   * stop position of type Location.position. *)
  let location_create_with_stop ~start ~stop =
    let position = Location.create ~start ~stop:start in
    { position with Location.stop = stop }

  let binary_operator
    ~left:({ Node.location; _ } as left)
    ~operator
    ~right:({ Node.location = { Location.stop; _ }; _ } as right) =
    Expression.BinaryOperator { BinaryOperator.left; operator; right }
    |> Node.create ~location:{ location with Location.stop }

  let slice ~lower ~upper ~step ~bound_colon ~step_colon =
    let increment { Location.start; stop; _ } =
      let increment ({ Location.column; _ } as position) =
        { position with Location.column = column + 1 }
      in
      { Location.start = increment start; stop = increment stop }
    in
    let lower_location =
      match lower with
      | Some lower -> lower.Node.location
      | None -> Location.create ~start:bound_colon ~stop:bound_colon
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
                  | Some { Node.location = ({ Location.stop; _ } as location); _ } ->
                      { location with Location.start = stop }
                  | None -> Location.create ~start:bound_colon ~stop:bound_colon |> increment
                end
          end
    in
    let slice_location =
      { lower_location with Location.stop = step_location.Location.stop  }
    in
    Expression.Slice { Slice.start = lower; stop = upper; step }
    |> Node.create ~location:slice_location

  let create_ellipsis (start, stop) =
    let location = Location.create ~start ~stop in
    Node.create (Expression.Constant AstExpression.Constant.Ellipsis) ~location


  let subscript_access subscript =
    let head, subscripts, subscript_location = subscript in
    let location = Node.location head in
    let index =
      match subscripts with
      | [] -> failwith "subscript can never be empty"
      | [subscript] -> subscript
      | subscripts ->
         let { Node.location = { Location.start; _ }; _ } = List.hd_exn subscripts  in
         let { Node.location = { Location.stop; _ }; _ } = List.last_exn subscripts in
         { Node.location = { Location.start; stop }; value = Expression.Tuple subscripts }
    in
    Expression.Subscript { Subscript.base = head; index }
    |> Node.create ~location:{ subscript_location with Location.start = location.Location.start }

  let create_literal_substring (string_position, (start, stop), value) =
    string_position,
    {
      Substring.kind = Substring.Kind.Literal;
      location = Location.create ~start ~stop;
      value;
    }

  let create_raw_format_substring (string_position, (start, stop), value) =
    string_position,
    {
      Substring.kind = Substring.Kind.RawFormat;
      location = Location.create ~start ~stop;
      value;
    }

  let create_mixed_string = function
    | [] -> Expression.Constant
              (AstExpression.Constant.String
                (AstExpression.StringLiteral.create ""))
    | [ { Substring.kind = Substring.Kind.Literal; value; _ } ] ->
       Expression.Constant
         (AstExpression.Constant.String
            (AstExpression.StringLiteral.create value))
    | _ as pieces ->
       let is_all_literal = List.for_all ~f:(fun { Substring.kind; _ } ->
          match kind with
          | Substring.Kind.Literal -> true
          | Substring.Kind.RawFormat -> false
        )
        in
        if is_all_literal pieces then
          let value =
            List.map pieces ~f:(fun { Substring.value; _ } -> value)
            |> String.concat ~sep:""
          in
          Expression.Constant
            (AstExpression.Constant.String
              (AstExpression.StringLiteral.create value))
        else
          Expression.FormatString pieces

%}

(* Tokens. *)
%token <Lexing.position * Lexing.position> ASTERIKS
%token <Lexing.position> COLON
%token <Lexing.position> DEDENT
%token <Lexing.position * Lexing.position> DOT
%token <Lexing.position> LEFTBRACKET
%token <Lexing.position> LEFTPARENS
%token <Lexing.position> MINUS
%token <Lexing.position> NEWLINE
%token <Lexing.position> NOT
%token <Lexing.position> PLUS
%token <Lexing.position> SLASH
(* the RIGHT* lexemes only contain the end position. *)
%token <Lexing.position> RIGHTBRACKET
%token <Lexing.position> RIGHTPARENS
%token <Lexing.position> TILDE

%token AND
%token AT
%token COMMA
%token DOUBLEEQUALS
%token EOF
%token EQUALS
%token EXCLAMATIONMARK
%token INDENT
%token IS
%token ISNOT
%token LEFTANGLE
%token LEFTANGLEEQUALS
%token OR
%token RIGHTANGLE
%token RIGHTANGLEEQUALS

(* Declarations. *)
%token <Lexing.position * Lexing.position> ASYNC
%token <Lexing.position> CLASS
%token <Lexing.position> DEFINE

(* Values. *)
%token <(Lexing.position * Lexing.position) * float> FLOAT
%token <(Lexing.position * Lexing.position) * float> COMPLEX
%token <(Lexing.position * Lexing.position) * int> INTEGER
%token <(Lexing.position * Lexing.position) * string> IDENTIFIER
%token <(Lexing.position * Lexing.position) * (Lexing.position * Lexing.position) * string> FORMAT
%token <(Lexing.position * Lexing.position) * (Lexing.position * Lexing.position) * string> STRING
%token <(Lexing.position * Lexing.position)> ELLIPSES
%token <(Lexing.position * Lexing.position)> FALSE
%token <(Lexing.position * Lexing.position)> TRUE
%token <(Lexing.position * Lexing.position)> NONE

(* Control. *)
%token <Lexing.position> ELSEIF
%token <Lexing.position> IF
%token ELSE
%token IN

%left NOT
%left PLUS MINUS
%left ASTERIKS SLASH
%left TILDE
%left AT
%left DOT

%nonassoc LEFTPARENS


%start <ParserStatement.Statement.statement Ast.Node.t list> parse_module
%start <ParserStatement.Expression.expression Ast.Node.t> parse_expression

%type <ParserStatement.Expression.expression Ast.Node.t> and_test
%type <ParserExpression.Call.Argument.t> argument
%type <ParserExpression.Call.Argument.t list> arguments
%type <ParserStatement.Statement.statement Ast.Node.t> async_statement
%type <ParserStatement.Expression.expression Ast.Node.t> atom
%type <ParserExpression.Call.Argument.t list> bases
%type <Location.t * ParserStatement.Statement.statement Ast.Node.t list> block
%type <ParserStatement.Expression.expression Ast.Node.t> comparison
%type <Ast.Expression.ComparisonOperator.operator * ParserStatement.Expression.expression Ast.Node.t> comparison_operator
%type <ParserStatement.Statement.statement Ast.Node.t> compound_statement
%type <Location.t * ParserStatement.Statement.statement> conditional
%type <ParserStatement.Statement.statement Ast.Node.t> decorated_statement
%type <ParserStatement.Expression.expression Ast.Node.t> decorator
%type <ParserExpression.Parameter.t list> define_parameters
%type <Location.t * ParserStatement.Statement.statement Ast.Node.t list> ellipsis_body
%type <ParserStatement.Expression.expression Ast.Node.t> expression
%type <Location.t * string> identifier
%type <Lexing.position list> list(NEWLINE)
%type <((Lexing.position * Lexing.position) * ParserExpression.Substring.t) list> mixed_string
%type <Location.t * ParserStatement.Statement.statement Ast.Node.t list> named_optional_block(ELSE)
%type <Lexing.position list> nonempty_list(NEWLINE)
%type <(Ast.Expression.ComparisonOperator.operator * ParserStatement.Expression.expression Ast.Node.t) list> nonempty_list(comparison_operator)
%type <ParserStatement.Expression.t list> nonempty_list(decorator)
%type <ParserStatement.Expression.expression Ast.Node.t> not_test
%type <ParserExpression.Expression.t option> option(annotation)
%type <ParserStatement.Expression.expression Ast.Node.t option> option(return_annotation)
%type <ParserStatement.Expression.expression Ast.Node.t option> option(value)
%type <ParserStatement.Expression.expression Ast.Node.t> or_test
%type <ParserExpression.Call.Argument.t list> parser_generator_separated_list(COMMA,argument)
%type <ParserStatement.Expression.t list> parser_generator_separated_list(COMMA,subscript_key)
%type <ParserStatement.Expression.t list> parser_generator_separated_list(COMMA,value)
%type <(Location.t * string) list> parser_generator_separated_list(DOT,identifier)
%type <ParserStatement.Expression.t list> parser_generator_separated_nonempty_list(COMMA,subscript_key)
%type <(Location.t * string) list> parser_generator_separated_nonempty_list(DOT,identifier)
%type <Location.t * Location.AstReference.t> reference
%type <ParserStatement.Expression.expression Ast.Node.t list * bool> separated_nonempty_list_indicator(COMMA,value)
%type <ParserStatement.Expression.expression Ast.Node.t list * bool> separated_nonempty_list_indicator_tail(COMMA,value)
%type <Location.t * ParserStatement.Statement.statement Ast.Node.t list> simple_statement
%type <ParserStatement.Statement.statement Ast.Node.t list> small_statement
%type <Location.t * ParserStatement.Statement.statement Ast.Node.t list> statement
%type <Location.t * ParserStatement.Statement.statement Ast.Node.t list> statements
%type <ParserStatement.Expression.expression Ast.Node.t> subscript_key
%type <ParserStatement.Expression.expression Ast.Node.t> value
%type <ParserStatement.Expression.expression Ast.Node.t> value_list

%%

parse_module:
  | statements = statements; EOF { snd statements }
  ;

parse_expression:
  | expression = expression; EOF { expression }
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
  | statements = small_statement; NEWLINE {
      let head = List.hd_exn statements in
      let last = List.last_exn statements in
      let location = {head.Node.location with Location.stop = Node.stop last} in
      location, statements
    }
  ;

small_statement:
  | target = expression;
    annotation = annotation {
      [{
        Node.location = {
          target.Node.location with Location.stop =
            annotation.Node.location.Location.stop;
        };
        value = Statement.Assign {
          Assign.target = target;
          annotation = Some annotation;
          value = None;
          index_in_chain = None;
        };
      }]
    }
  | target = expression;
    annotation = annotation;
    EQUALS;
    ellipsis = ELLIPSES {
      let ellipsis = create_ellipsis ellipsis in
      [{
        Node.location = {
          target.Node.location with Location.stop =
            ellipsis.Node.location.Location.stop;
        };
        value = Statement.Assign {
          Assign.target = target;
          annotation = Some annotation;
          value = Some ellipsis;
          index_in_chain = None;
        };
      }]
    }
  | expression = value {
      [{ Node.location = expression.Node.location; value = Statement.Expression expression }]
    }
  ;

compound_statement:
  | definition = CLASS; name = reference;
    bases = bases; colon_position = COLON;
    body = ellipsis_body {
      let location = Location.create ~start:definition ~stop:colon_position in
      let body_location, body = body in
      let location = { location with Location.stop = body_location.Location.stop } in
      let _, name = name in
      {
        Node.location;
        value = Statement.Class {
          Class.name = name;
          base_arguments = bases;
          body;
          decorators = [];
        };
      }
    }

  | definition = DEFINE; name = reference;
    LEFTPARENS;
    parameters = define_parameters;
    RIGHTPARENS;
    return_annotation = return_annotation?;
    COLON;
    body = ellipsis_body {
      let body_location, body = body in
      let location =
        location_create_with_stop ~start:definition ~stop:body_location.Location.stop
      in
      let _, name = name in
      {
        Node.location;
        value = Statement.Define {
          Define.signature = {
            Define.Signature.name = name;
            parameters = parameters;
            decorators = [];
            return_annotation = return_annotation;
            async = false;
          };
          body
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
      | { Node.value = Statement.Define ({ Define.signature; _ } as value); _ } ->
          let signature = { signature with Define.Signature.async = true } in
          let decorated = { value with Define.signature } in
          {
            Node.location;
            value = Statement.Define decorated;
          }
      | _ -> raise (Failure "Async not supported on statement.")
    }
  ;

ellipsis_body:
  | ellipsis = ELLIPSES; NEWLINE
  | NEWLINE+; INDENT; ellipsis = ELLIPSES; NEWLINE; DEDENT; NEWLINE* {
    let location = Location.create ~start:(fst ellipsis) ~stop:(snd ellipsis) in
    let body = [
      Node.create
        ~location
        (Statement.Expression
          (Node.create
            ~location
            (Expression.Constant AstExpression.Constant.Ellipsis)
          )
        )
    ] in
    location, body
   }
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
  | condition = value_list; COLON;
    body = block; orelse = named_optional_block(ELSE) {
      {
        condition.Node.location with
        Location.stop =
          match orelse with
          | _, [] -> (fst body).Location.stop
          | location, _ -> location.Location.stop;
      },
      Statement.If { If.test = condition; body = snd body; orelse = snd orelse }
    }
  | condition = value_list; COLON;
    body = block;
    else_start = ELSEIF; value = conditional {
      let stop = (fst value).Location.stop in
      { condition.Node.location with Location.stop },
      Statement.If {
        If.test = condition;
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
  | AT; expression = expression; NEWLINE+ {
      expression
    }
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
  | parameter = define_parameter; COMMA; parameters = define_parameters { parameter :: parameters }
  | parameter = define_parameter { [parameter] }
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
  | slash = SLASH {
    {
      Node.location = Location.create ~start:slash ~stop:slash;
      value = {
          Parameter.name = "/";
          value = None;
          annotation = None;
      };
    }
  }
  | name = name; annotation = annotation? {
      let location =
        let name_location = fst name in
        match annotation with
        | None -> name_location
        | Some { Node.location = { Location.stop; _ }; _ } -> { name_location with Location.stop }
      in
      {
        Node.location;
        value = { Parameter.name = snd name; value = None; annotation };
      }
    }
  | name = name; annotation = annotation?; EQUALS; value = value {
      let location =
        let name_location = fst name in
        match annotation with
        | None -> name_location
        | Some { Node.location = { Location.stop; _ }; _ } -> { name_location with Location.stop }
      in
      {
        Node.location;
        value = { Parameter.name = snd name; value = Some value; annotation };
      }
    }
  ;

%inline name:
  | expression = expression {
      let rec identifier expression =
        match expression with
        | { Node.location; value = Expression.Name (Name.Identifier identifier) } ->
            (location, identifier)
        | { Node.location; value = Expression.Starred (Starred.Once expression) } ->
            location,
            identifier expression
            |> snd
            |> fun identifier -> "*" ^ identifier
        | { Node.location; value = Expression.Starred (Starred.Twice expression) } ->
            location,
            identifier expression
            |> snd
            |> fun identifier -> "**" ^ identifier
        | _ ->
            raise (Failure "Unexpected parameters") in
      identifier expression
    }
  ;

%inline annotation:
  | COLON; expression = expression { expression }
  ;

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

(* Expressions. *)

atom:
  | identifier = identifier {
      {
        Node.location = fst identifier;
        value = Expression.Name (Name.Identifier (snd identifier));
      }
    }

  | ellipsis = ELLIPSES {
      let location = Location.create ~start:(fst ellipsis) ~stop:(snd ellipsis) in
      Node.create (Expression.Constant AstExpression.Constant.Ellipsis) ~location
    }

  | name = expression;
    start = LEFTPARENS;
    arguments = arguments;
    stop = RIGHTPARENS {
      let call_location = Location.create ~start ~stop in
      Expression.Call { Call.callee = name; arguments }
      |> Node.create
        ~location:({ name.Node.location with Location.stop = call_location.Location.stop })
    }

  | position = FALSE {
      let start, stop = position in
      {
        Node.location = Location.create ~start ~stop;
        value = Expression.Constant AstExpression.Constant.False;
      }
    }

  | number = COMPLEX {
      let start, stop = fst number in
      {
        Node.location = Location.create ~start ~stop;
        value = Expression.Constant (AstExpression.Constant.Complex (snd number));
      }
    }

  | number = FLOAT {
      let start, stop = fst number in
      {
        Node.location = Location.create ~start ~stop;
        value = Expression.Constant (AstExpression.Constant.Float (snd number));
      }
    }

  | number = INTEGER {
      let start, stop = fst number in
      {
        Node.location = Location.create ~start ~stop;
        value = Expression.Constant (AstExpression.Constant.Integer (snd number));
      }
    }

  | position = NONE {
      let start, stop = position in
      {
        Node.location = Location.create ~start ~stop;
        value = Expression.Constant AstExpression.Constant.NoneLiteral;
      }
    }

  | start = LEFTBRACKET;
    items = parser_generator_separated_list(COMMA, value);
    stop = RIGHTBRACKET {
      {
        Node.location = Location.create ~start ~stop;
        value = Expression.List items;
      }
    }

  | position = ASTERIKS; inner = expression {
    let start, _ = position in
    let location = location_create_with_stop ~start ~stop:(Node.stop inner) in
    match inner with
    | {
        Node.value = Expression.Starred (Starred.Once inner);
        _;
      } -> {
        Node.location;
        value = Expression.Starred (Starred.Twice inner);
      }
    | _ -> {
        Node.location;
        value = Expression.Starred (Starred.Once inner);
      }
    }

  | string = STRING; mixed_string = mixed_string {
      let all_strings = create_literal_substring string :: mixed_string in
      let all_pieces = List.map all_strings ~f:snd in
      let (head, _), (last, _) = List.hd_exn all_strings, List.last_exn all_strings in
      let (start, _) = head in
      let (_, stop) = last in
      {
        Node.location = Location.create ~start ~stop;
        value = create_mixed_string all_pieces;
      }
    }

  | format = FORMAT; mixed_string = mixed_string {
      let all_strings = create_raw_format_substring format :: mixed_string in
      let all_pieces = List.map all_strings ~f:snd in
      let (head, _), (last, _) = List.hd_exn all_strings, List.last_exn all_strings in
      let (start, _) = head in
      let (_, stop) = last in
      {
        Node.location = Location.create ~start ~stop;
        value = create_mixed_string all_pieces;
      }
    }

  | position = TRUE {
      let start, stop = position in
      {
        Node.location = Location.create ~start ~stop;
        value = Expression.Constant AstExpression.Constant.True
      }
    }

  | operator = unary_operator; operand = expression {
      let start, operator = operator in
      let { Node.value; _ } = operand in
      let location = location_create_with_stop ~start ~stop:(Node.stop operand)
      in
      match operator, value with
      | AstExpression.UnaryOperator.Negative,
        Expression.Constant (AstExpression.Constant.Integer literal) -> {
        Node.location;
        value = Expression.Constant (AstExpression.Constant.Integer (-1 * literal));
      }
      | _, _ -> {
        Node.location;
        value = Expression.UnaryOperator {
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
        value = Expression.Tuple [];
      }
    }

  | start = LEFTPARENS; inner = value_list; stop = RIGHTPARENS {
      {
        Node.location = Location.create ~start ~stop;
        value = Expression.Parenthesis inner;
      }
    }

  | expression = expression; DOT; identifier = identifier {
      let location =
        { expression.Node.location with Location.stop = Location.stop (fst identifier) }
      in
      {
        Node.location;
        value = Expression.Name (
          Name.Attribute { Name.Attribute.base = expression; attribute = snd identifier }
        )
      }
    }

  | subscript = subscript { subscript_access subscript }

  | left = expression;
    operator = binary_operator;
    right = expression {
      binary_operator ~left ~operator ~right
    }
  ;

mixed_string:
  | { [] }
  | first_string = FORMAT; rest = mixed_string {
      create_raw_format_substring first_string :: rest
    }
  | first_string = STRING; rest = mixed_string {
      create_literal_substring first_string :: rest
    }
  ;

comparison:
  | expression = expression { expression }

  | left = expression; comparisons = nonempty_list(comparison_operator) {
      let rec comparison ({ Node.location; _ } as left) comparisons =
        match comparisons with
        | (operator, right) :: comparisons when List.length comparisons > 0 ->
            let left =
              Expression.ComparisonOperator { ComparisonOperator.left; operator; right }
              |> Node.create ~location:({ location with Location.stop = Node.stop right })
            in
            let right = comparison right comparisons in
            Expression.BooleanOperator {
              BooleanOperator.left;
              operator = AstExpression.BooleanOperator.And;
              right;
            }
            |> Node.create ~location;
        | [operator, right] ->
            Expression.ComparisonOperator { ComparisonOperator.left; operator; right }
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
        value = Expression.UnaryOperator {
          UnaryOperator.operator = AstExpression.UnaryOperator.Not;
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
        value = Expression.BooleanOperator {
          BooleanOperator.left;
          operator = AstExpression.BooleanOperator.And;
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
        value = Expression.BooleanOperator {
          BooleanOperator.left;
          operator = AstExpression.BooleanOperator.Or;
          right;
        }
      }
   }
  ;

value:
  | or_test = or_test { or_test }
  ;

value_list:
  | items = separated_nonempty_list_indicator(COMMA, value) {
      match items with
      | head :: [], has_trailing_comma ->
        if has_trailing_comma then
          {
            Node.location = head.Node.location;
            value = Expression.Tuple [head];
          }
        else
          head
      | (head :: _ as items), _ ->
          let last = List.last_exn items in
          {
            Node.location = { head.Node.location with Location.stop = Node.stop last };
            value = Expression.Tuple items;
          }
      | _ -> raise (Failure "invalid atom")
    }
  ;

%inline binary_operator:
  | PLUS { AstExpression.BinaryOperator.Add }
  | MINUS { AstExpression.BinaryOperator.Sub }
  | ASTERIKS { AstExpression.BinaryOperator.Mult }
  | SLASH { AstExpression.BinaryOperator.Div }
  ;

comparison_operator:
  | DOUBLEEQUALS; operand = expression { AstExpression.ComparisonOperator.Equals, operand }
  | RIGHTANGLE; operand = expression { AstExpression.ComparisonOperator.GreaterThan, operand }
  | RIGHTANGLEEQUALS; operand = expression {
      AstExpression.ComparisonOperator.GreaterThanOrEquals, operand
    }
  | IN; operand = expression { AstExpression.ComparisonOperator.In, operand }
  | IS; operand = expression { AstExpression.ComparisonOperator.Is, operand }
  | ISNOT; operand = expression { AstExpression.ComparisonOperator.IsNot, operand }
  | LEFTANGLE; operand = expression { AstExpression.ComparisonOperator.LessThan, operand }
  | LEFTANGLEEQUALS; operand = expression {
      AstExpression.ComparisonOperator.LessThanOrEquals, operand
    }
  | EXCLAMATIONMARK; EQUALS; operand = expression {
      AstExpression.ComparisonOperator.NotEquals, operand
    }
  | NOT; IN; operand = expression { AstExpression.ComparisonOperator.NotIn, operand }
  ;

%inline unary_operator:
  | position = TILDE { position, AstExpression.UnaryOperator.Invert }
  | position = MINUS { position, AstExpression.UnaryOperator.Negative }
  | position = NOT { position, AstExpression.UnaryOperator.Not }
  | position = PLUS { position, AstExpression.UnaryOperator.Positive }
  ;

arguments:
  | arguments = parser_generator_separated_list(COMMA, argument) { arguments }
  ;

argument:
  | identifier = identifier; EQUALS; value = value {
     {
        Call.Argument.name = Some { Node.location = fst identifier; value = snd identifier };
        value;
      }
    }
  | value = value { { Call.Argument.name = None; value } }
  ;

subscript_key:
  | index = value { index }
  | lower = value?; bound_colon = COLON; upper = value? {
      slice ~lower ~upper ~step:None ~bound_colon ~step_colon:None
    }
  | lower = value?; bound_colon = COLON; upper = value?; step_colon = COLON; step = value? {
      slice ~lower ~upper ~step ~bound_colon ~step_colon:(Some step_colon)
    }
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
