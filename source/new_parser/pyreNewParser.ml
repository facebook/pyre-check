(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base
module Context = PyreAst.Parser.Context
module Error = PyreAst.Parser.Error

exception Exception of Error.t

exception InternalError of Error.t

let position ~line ~column = { Ast.Location.line; column }

let location ~start ~stop = { Ast.Location.start; stop }

let identifier x = x

let expression_context = PyreAst.TaglessFinal.ExpressionContext.make ~load:() ~store:() ~del:() ()

let constant =
  let open Ast.Expression in
  let integer i = Constant.Integer i in
  let big_integer i =
    (* We should wrap the raw string into a real bignum type (e.g., zarith Z) to support
       arithmetics, *)
    Constant.BigInteger i
  in
  let float_ f = Constant.Float f in
  let complex f = Constant.Complex f in
  let string_ s = Constant.String (StringLiteral.create s) in
  let byte_string s = Constant.String (StringLiteral.create ~bytes:true s) in
  PyreAst.TaglessFinal.Constant.make
    ~none:Constant.NoneLiteral
    ~false_:Constant.False
    ~true_:Constant.True
    ~ellipsis:Constant.Ellipsis
    ~integer
    ~big_integer
    ~float_
    ~complex
    ~string_
    ~byte_string
    ()


let boolean_operator =
  let open Ast.Expression in
  PyreAst.TaglessFinal.BooleanOperator.make ~and_:BooleanOperator.And ~or_:BooleanOperator.Or ()


let binary_operator =
  PyreAst.TaglessFinal.BinaryOperator.make
    ~add:"add"
    ~sub:"sub"
    ~mult:"mul"
    ~matmult:"matmul"
    ~div:"truediv"
    ~mod_:"mod"
    ~pow:"pow"
    ~lshift:"lshift"
    ~rshift:"rshift"
    ~bitor:"or"
    ~bitxor:"xor"
    ~bitand:"and"
    ~floordiv:"floordiv"
    ()


let unary_operator =
  let open Ast.Expression in
  PyreAst.TaglessFinal.UnaryOperator.make
    ~invert:UnaryOperator.Invert
    ~not_:UnaryOperator.Not
    ~uadd:UnaryOperator.Positive
    ~usub:UnaryOperator.Negative
    ()


let comparison_operator =
  let open Ast.Expression in
  PyreAst.TaglessFinal.ComparisonOperator.make
    ~eq:ComparisonOperator.Equals
    ~noteq:ComparisonOperator.NotEquals
    ~lt:ComparisonOperator.LessThan
    ~lte:ComparisonOperator.LessThanOrEquals
    ~gt:ComparisonOperator.GreaterThan
    ~gte:ComparisonOperator.GreaterThanOrEquals
    ~is:ComparisonOperator.Is
    ~isnot:ComparisonOperator.IsNot
    ~in_:ComparisonOperator.In
    ~notin:ComparisonOperator.NotIn
    ()


let comprehension ~target ~iter ~ifs ~is_async =
  let open Ast.Expression in
  (* PEP-572 disallows assignment expressions in comprehension iterables. `pyre-ast` does not check
     for that. *)
  let check_assignment_expression expression =
    let mapper =
      let open Ast.Location in
      let map_walrus_operator
          ~mapper:_
          ~location:{ start = { line; column }; stop = { line = end_line; column = end_column } }
          _
        =
        raise
          (InternalError
             {
               Error.line;
               column;
               end_line;
               end_column;
               message =
                 "assignment expression cannot be used in a comprehension iterable expression";
             })
      in
      (* The following 4 items are introduced for optimization purpose: avoid recursing into
         comprehension sub-expressions as they are already checked for assignment expressions upon
         construction. *)
      let map_dictionary_comprehension ~mapper:_ ~location comprehension =
        Ast.Node.create ~location (Expression.DictionaryComprehension comprehension)
      in
      let map_generator ~mapper:_ ~location comprehension =
        Ast.Node.create ~location (Expression.Generator comprehension)
      in
      let map_list_comprehension ~mapper:_ ~location comprehension =
        Ast.Node.create ~location (Expression.ListComprehension comprehension)
      in
      let map_set_comprehension ~mapper:_ ~location comprehension =
        Ast.Node.create ~location (Expression.SetComprehension comprehension)
      in
      Mapper.create_default
        ~map_walrus_operator
        ~map_dictionary_comprehension
        ~map_generator
        ~map_list_comprehension
        ~map_set_comprehension
        ()
    in
    Mapper.map ~mapper expression
  in
  {
    Comprehension.Generator.target;
    iterator = check_assignment_expression iter;
    conditions = ifs;
    async = is_async;
  }


module KeywordArgument = struct
  type t = {
    location: Ast.Location.t;
    name: Ast.Identifier.t option;
    value: Ast.Expression.t;
  }
end

let keyword ~location ~arg ~value = { KeywordArgument.location; name = arg; value }

let convert_positional_argument value = { Ast.Expression.Call.Argument.name = None; value }

let convert_keyword_argument { KeywordArgument.location; name; value } =
  let open Ast.Expression in
  let module Node = Ast.Node in
  match name with
  | None ->
      (* CPython quirk: **arg is represented as keyword arg without a name. *)
      {
        Call.Argument.name = None;
        value = Expression.Starred (Starred.Twice value) |> Node.create ~location;
      }
  | Some keyword_name -> { Call.Argument.name = Some (Node.create ~location keyword_name); value }


module SingleParameter = struct
  type t = {
    location: Ast.Location.t;
    identifier: Ast.Identifier.t;
    annotation: Ast.Expression.t option;
  }
end

let argument ~location ~identifier ~annotation ~type_comment =
  let annotation =
    match annotation with
    | Some _ -> annotation
    | None -> (
        match type_comment with
        | None -> None
        | Some comment ->
            let comment_annotation =
              Ast.Expression.(
                Expression.Constant
                  (Constant.String { StringLiteral.kind = String; value = comment }))
            in
            Some (Ast.Node.create ~location comment_annotation))
  in
  { SingleParameter.location; identifier; annotation }


let arguments ~posonlyargs ~args ~vararg ~kwonlyargs ~kw_defaults ~kwarg ~defaults =
  let open Ast.Expression in
  let module Node = Ast.Node in
  let to_parameter ({ SingleParameter.location; identifier; annotation }, default_value) =
    { Parameter.name = identifier; value = default_value; annotation } |> Node.create ~location
  in
  let to_parameters parameter_list default_list =
    List.zip_exn parameter_list default_list |> List.map ~f:to_parameter
  in
  let positional_only_defaults, regular_defaults =
    let positional_only_count = List.length posonlyargs in
    let regular_count = List.length args in
    let expanded_defaults =
      let total_counts = positional_only_count + regular_count in
      let fill_counts = total_counts - List.length defaults in
      List.map defaults ~f:Option.some |> List.append (List.init fill_counts ~f:(fun _ -> None))
    in
    List.split_n expanded_defaults positional_only_count
  in
  let positional_only_parameters = to_parameters posonlyargs positional_only_defaults in
  let regular_parameters = to_parameters args regular_defaults in
  let keyword_only_parameters = to_parameters kwonlyargs kw_defaults in
  let vararg_parameter =
    let handle_vararg { SingleParameter.location; identifier; annotation } =
      let name = Caml.Format.sprintf "*%s" identifier in
      { Parameter.name; value = None; annotation } |> Node.create ~location
    in
    Option.map vararg ~f:handle_vararg
  in
  let kwarg_parameter =
    let handle_kwarg { SingleParameter.location; identifier; annotation } =
      let name = Caml.Format.sprintf "**%s" identifier in
      { Parameter.name; value = None; annotation } |> Node.create ~location
    in
    Option.map kwarg ~f:handle_kwarg
  in
  let delimiter_parameter ~should_insert name =
    (* TODO(T101307161): This is just an ugly temporary hack that helps preserve backward
       compatibility. *)
    if should_insert then
      [Node.create_with_default_location { Parameter.name; value = None; annotation = None }]
    else
      []
  in
  List.concat
    [
      positional_only_parameters;
      delimiter_parameter ~should_insert:(not (List.is_empty positional_only_parameters)) "/";
      regular_parameters;
      Option.to_list vararg_parameter;
      delimiter_parameter
        ~should_insert:
          ((not (List.is_empty keyword_only_parameters)) && Option.is_none vararg_parameter)
        "*";
      keyword_only_parameters;
      Option.to_list kwarg_parameter;
    ]


let expression =
  let open Ast.Expression in
  let module Node = Ast.Node in
  let bool_op ~location ~op ~values =
    match values with
    | [] ->
        (* NOTE(grievejia): I don't think the CPython parser will give us empty boolean operands.
           Doing this just to be safe. *)
        let default_value =
          match op with
          | BooleanOperator.And -> Constant.True
          | BooleanOperator.Or -> Constant.False
        in
        Expression.Constant default_value |> Node.create ~location
    | [value] -> value
    | first :: second :: rest ->
        (* Boolean operators are left-associative *)
        let init =
          Expression.BooleanOperator { BooleanOperator.left = first; operator = op; right = second }
          |> Node.create ~location:{ location with stop = second.location.stop }
        in
        let f sofar next =
          let { Node.location = { Ast.Location.stop = next_stop; _ }; _ } = next in
          Expression.BooleanOperator { BooleanOperator.left = sofar; operator = op; right = next }
          |> Node.create ~location:{ location with stop = next_stop }
        in
        List.fold rest ~init ~f
  in
  let named_expr ~location ~target ~value =
    (* TODO(T47589601): `target` can be strenghthened into `Identifier.t` if qualification is
       removed. *)
    Expression.WalrusOperator { WalrusOperator.target; value } |> Node.create ~location
  in
  let bin_op ~location ~left ~op ~right =
    (* TODO(T101299882): Avoid lowering binary operators in parsing phase. *)
    let callee =
      let dunder_name = Caml.Format.sprintf "__%s__" op in
      Expression.Name
        (Name.Attribute { base = left; attribute = identifier dunder_name; special = true })
      |> Node.create ~location:(Node.location left)
    in
    Expression.Call { callee; arguments = [{ Call.Argument.name = None; value = right }] }
    |> Node.create ~location
  in
  let unary_op ~location ~op ~operand =
    match op, operand with
    | UnaryOperator.Positive, { Node.value = Expression.Constant (Constant.Integer literal); _ } ->
        Expression.Constant (Constant.Integer literal) |> Node.create ~location
    | UnaryOperator.Negative, { Node.value = Expression.Constant (Constant.Integer literal); _ } ->
        Expression.Constant (Constant.Integer (-literal)) |> Node.create ~location
    | _ ->
        Expression.UnaryOperator { UnaryOperator.operator = op; operand } |> Node.create ~location
  in
  let lambda ~location ~args ~body =
    Expression.Lambda { Lambda.parameters = args; body } |> Node.create ~location
  in
  let if_exp ~location ~test ~body ~orelse =
    Expression.Ternary { Ternary.target = body; test; alternative = orelse }
    |> Node.create ~location
  in
  let dict ~location ~keys ~values =
    let entries, keywords =
      (* `keys` and `values` are guaranteed by CPython parser to be of the same length. *)
      List.zip_exn keys values
      |> List.partition_map ~f:(fun (key, value) ->
             match key with
             | None -> Either.Second value
             | Some key -> Either.First { Dictionary.Entry.key; value })
    in
    Expression.Dictionary { Dictionary.entries; keywords } |> Node.create ~location
  in
  let set ~location ~elts = Expression.Set elts |> Node.create ~location in
  let list_comp ~location ~elt ~generators =
    Expression.ListComprehension { Comprehension.element = elt; generators }
    |> Node.create ~location
  in
  let set_comp ~location ~elt ~generators =
    Expression.SetComprehension { Comprehension.element = elt; generators } |> Node.create ~location
  in
  let dict_comp ~location ~key ~value ~generators =
    Expression.DictionaryComprehension
      { Comprehension.element = { Dictionary.Entry.key; value }; generators }
    |> Node.create ~location
  in
  let generator_exp ~location ~elt ~generators =
    Expression.Generator { Comprehension.element = elt; generators } |> Node.create ~location
  in
  let await ~location ~value = Expression.Await value |> Node.create ~location in
  let yield ~location ~value = Expression.Yield value |> Node.create ~location in
  let yield_from ~location ~value = Expression.YieldFrom value |> Node.create ~location in
  let compare ~location ~left ~ops ~comparators =
    let f (sofar, last) (operator, next) =
      (* NOTE(grievejia): This is not 100% accurate since `last` is never evaluated more than once
         at runtime. But it's a fairly close approximation. *)
      let right =
        let { Node.location = { Ast.Location.start = last_start; _ }; _ } = last in
        let { Node.location = { Ast.Location.stop = next_stop; _ }; _ } = next in
        Expression.ComparisonOperator { ComparisonOperator.left = last; operator; right = next }
        |> Node.create ~location:{ Ast.Location.start = last_start; stop = next_stop }
      in
      let sofar =
        Expression.BooleanOperator
          { BooleanOperator.left = sofar; operator = BooleanOperator.And; right }
        |> Node.create ~location:{ location with stop = right.location.stop }
      in
      sofar, next
    in
    (* `ops` and `comparators` are guaranteed by CPython parser to be of the same length. *)
    List.zip_exn ops comparators
    |> function
    | [] -> left
    | (operator, right) :: rest ->
        let { Node.location = { Ast.Location.stop = right_stop; _ }; _ } = right in
        let first_operand =
          Expression.ComparisonOperator { ComparisonOperator.left; operator; right }
          |> Node.create ~location:{ location with stop = right_stop }
        in
        let result, _ = List.fold ~init:(first_operand, right) ~f rest in
        result
  in
  let call ~location ~func ~args ~keywords =
    let arguments =
      (* NOTE(T101305324): Ordering between positional and keyword args is artificial. *)
      List.append
        (List.map args ~f:convert_positional_argument)
        (List.map keywords ~f:convert_keyword_argument)
    in
    Expression.Call { callee = func; arguments } |> Node.create ~location
  in
  let formatted_value ~location ~value ~conversion:_ ~format_spec:_ =
    Expression.FormatString [Substring.Format value] |> Node.create ~location
  in
  let joined_str ~location ~values =
    let collapse_formatted_value ({ Node.value; location } as expression) =
      match value with
      | Expression.Constant (Constant.String { StringLiteral.kind = String; value }) ->
          Substring.Literal (Node.create ~location value)
      | Expression.FormatString [substring] -> substring
      | _ ->
          (* NOTE (grievejia): It may be impossible for CPython parser to reach this branch *)
          Substring.Format expression
    in
    Expression.FormatString (List.map values ~f:collapse_formatted_value) |> Node.create ~location
  in
  let constant ~location ~value ~kind:_ = Expression.Constant value |> Node.create ~location in
  let attribute ~location ~value ~attr ~ctx:() =
    Expression.Name
      (Name.Attribute { Name.Attribute.base = value; attribute = attr; special = false })
    |> Node.create ~location
  in
  let subscript ~location ~value ~slice ~ctx:() =
    (* TODO(T101303314): We should avoid lowering subscript expressions at parser phase. *)
    let callee =
      let { Node.location = value_location; _ } = value in
      Expression.Name
        (Name.Attribute { Name.Attribute.base = value; attribute = "__getitem__"; special = true })
      |> Node.create ~location:value_location
    in
    let arguments = [{ Call.Argument.name = None; value = slice }] in
    Expression.Call { callee; arguments } |> Node.create ~location
  in
  let starred ~location ~value ~ctx:() =
    Expression.Starred (Starred.Once value) |> Node.create ~location
  in
  let name ~location ~id ~ctx:() = Expression.Name (Name.Identifier id) |> Node.create ~location in
  let list ~location ~elts ~ctx:() = Expression.List elts |> Node.create ~location in
  let tuple ~location ~elts ~ctx:() = Expression.Tuple elts |> Node.create ~location in
  let slice ~location ~lower ~upper ~step =
    (* TODO(T101302994): We should avoid lowering slice expressions at parser phase. *)
    let callee = Expression.Name (Name.Identifier "slice") |> Node.create ~location in
    let arguments =
      let to_argument = function
        | None -> Expression.Constant Constant.NoneLiteral |> Node.create ~location:Ast.Location.any
        | Some expression -> expression
      in
      [
        { Call.Argument.name = None; value = to_argument lower };
        { Call.Argument.name = None; value = to_argument upper };
        { Call.Argument.name = None; value = to_argument step };
      ]
    in
    Expression.Call { callee; arguments } |> Node.create ~location
  in
  PyreAst.TaglessFinal.Expression.make
    ~bool_op
    ~named_expr
    ~bin_op
    ~unary_op
    ~lambda
    ~if_exp
    ~dict
    ~set
    ~list_comp
    ~set_comp
    ~dict_comp
    ~generator_exp
    ~await
    ~yield
    ~yield_from
    ~compare
    ~call
    ~formatted_value
    ~joined_str
    ~constant
    ~attribute
    ~subscript
    ~starred
    ~name
    ~list
    ~tuple
    ~slice
    ()


module FunctionSignature = struct
  type t = {
    parameter_annotations: Ast.Expression.t list;
    return_annotation: Ast.Expression.t;
  }
end

module StatementContext = struct
  type t = {
    (* [parse_function_signature] takes function type comment as string and parse it into a
       [FunctionSignature.t]. *)
    parse_function_signature: string -> (FunctionSignature.t, Error.t) Result.t;
    (* [parent] holds the name of the immediate containing class of a statement. *)
    parent: Ast.Identifier.t option;
  }
end

let build_statements ~context statement_builders =
  let build_statement builder = builder ~context in
  List.concat (List.map statement_builders ~f:build_statement)


let with_item ~context_expr ~optional_vars = context_expr, optional_vars

let import_alias ~location ~name ~asname =
  let open Ast in
  Node.create ~location { Statement.Import.name = Reference.create name; alias = asname }


let exception_handler ~location:_ ~type_ ~name ~body ~context =
  { Ast.Statement.Try.Handler.kind = type_; name; body = build_statements ~context body }


let build_exception_handlers ~context exception_handler_builders =
  let build_exception_handler builder = builder ~context in
  List.map exception_handler_builders ~f:build_exception_handler


let match_case ~pattern ~guard ~body ~context =
  { Ast.Statement.Match.Case.pattern; guard; body = build_statements ~context body }


let build_match_cases ~context match_cases =
  let build_match_case builder = builder ~context in
  List.map match_cases ~f:build_match_case


let pattern =
  let open Ast.Expression in
  let open Ast.Statement in
  let module Node = Ast.Node in
  let match_value ~location ~value = Match.Pattern.MatchValue value |> Node.create ~location in
  let match_singleton ~location ~value =
    Match.Pattern.MatchSingleton value |> Node.create ~location
  in
  let match_sequence ~location ~patterns =
    Match.Pattern.MatchSequence patterns |> Node.create ~location
  in
  let match_mapping ~location ~keys ~patterns ~rest =
    Match.Pattern.MatchMapping { keys; patterns; rest } |> Node.create ~location
  in
  let match_class ~location ~cls ~patterns ~kwd_attrs ~kwd_patterns =
    let class_name =
      match Node.value cls with
      | Expression.Name name -> Node.create ~location:(Node.location cls) name
      | _ ->
          let {
            Ast.Location.start = { line; column };
            stop = { line = end_line; column = end_column };
          }
            =
            location
          in
          raise
            (InternalError
               {
                 Error.line;
                 column;
                 end_line;
                 end_column;
                 message = "class pattern expects simple identifier or attribute accesses only";
               })
    in
    Match.Pattern.MatchClass
      { class_name; patterns; keyword_attributes = kwd_attrs; keyword_patterns = kwd_patterns }
    |> Node.create ~location
  in
  let match_star ~location ~name = Match.Pattern.MatchStar name |> Node.create ~location in
  let match_as ~location ~pattern ~name =
    match name, pattern with
    | None, Some _ ->
        let {
          Ast.Location.start = { line; column };
          stop = { line = end_line; column = end_column };
        }
          =
          location
        in
        raise
          (InternalError
             {
               Error.line;
               column;
               end_line;
               end_column;
               message = "as pattern expects non-wildcard pattern when name is `_`";
             })
    | None, None -> Node.create ~location Match.Pattern.MatchWildcard
    | Some name, pattern -> Match.Pattern.MatchAs { name; pattern } |> Node.create ~location
  in
  let match_or ~location ~patterns = Match.Pattern.MatchOr patterns |> Node.create ~location in
  PyreAst.TaglessFinal.Pattern.make
    ~match_value
    ~match_singleton
    ~match_sequence
    ~match_mapping
    ~match_class
    ~match_star
    ~match_as
    ~match_or
    ()


let create_assign ~location ~target ~annotation ~value () =
  let open Ast.Expression in
  let open Ast.Statement in
  let module Node = Ast.Node in
  let value =
    (* TODO(T101298692): Make `value` optional in assign statement and stop auto-filling `...` *)
    let location =
      let open Ast.Location in
      { location with start = location.stop }
    in
    Option.value value ~default:(Expression.Constant Constant.Ellipsis |> Node.create ~location)
  in
  match Node.value target with
  | Expression.Call
      {
        callee =
          {
            Node.value =
              Expression.Name
                (Name.Attribute { Name.Attribute.base; attribute = "__getitem__"; special = true });
            location = callee_location;
          };
        arguments;
      } ->
      let setitem =
        Expression.Call
          {
            callee =
              Expression.Name
                (Name.Attribute { Name.Attribute.base; attribute = "__setitem__"; special = true })
              |> Node.create ~location:callee_location;
            arguments = List.append arguments [{ Call.Argument.name = None; value }];
          }
        |> Node.create ~location
      in
      Statement.Expression setitem |> Node.create ~location
  | _ ->
      (* TODO(T101303314): This does not take into account things like `a[0], b = ...`, where we'll
         need to turn `a[0]` into `__setitem__` call. *)
      Statement.Assign { target; annotation; value } |> Node.create ~location


let process_function_type_comment
    ~context:{ StatementContext.parse_function_signature; parent }
    ~parameters
    ~returns
    ~comment_location
  = function
  | None -> Result.Ok (parameters, returns)
  | Some type_comment -> (
      match parse_function_signature type_comment with
      | Result.Error _ -> Result.Error "Syntax error in function signature type comment"
      | Result.Ok { FunctionSignature.parameter_annotations; return_annotation } -> (
          let open Ast.Expression in
          let module Node = Ast.Node in
          let parameter_annotations =
            let parameter_count = List.length parameters in
            match parameter_annotations with
            | [{ Node.value = Expression.Constant Constant.Ellipsis; _ }] ->
                List.init parameter_count ~f:(fun _ -> None)
            | _ ->
                let annotations = List.map parameter_annotations ~f:Option.some in
                let annotation_count = List.length annotations in
                (* For methods, it is allowed to have one extra `self` and `cls` parameter without
                   annotation. *)
                if Option.is_some parent && annotation_count == parameter_count - 1 then
                  None :: annotations
                else
                  annotations
          in
          match List.zip parameters parameter_annotations with
          | List.Or_unequal_lengths.Unequal_lengths ->
              let message =
                Format.sprintf
                  "Function signature type comment has %d parameter types, while the corresponding \
                   function contains %d parameters"
                  (List.length parameter_annotations)
                  (List.length parameters)
              in
              Result.Error message
          | List.Or_unequal_lengths.Ok pairs ->
              let location_patcher =
                (* NOTE(grievejia): Locations in both `parameter_annotations` and
                   `return_annotation` are all off since they are counted from the start of
                   `type_comment`, not from the start of the entire file. Therefore, we need to
                   replace them with something more sensible. *)
                Mapper.create_transformer ~map_location:(fun _ -> comment_location) ()
              in
              let override_annotation old_annotation new_annotation =
                (* NOTE(grievejia): Currently we let inline annotations take precedence over comment
                   annotations. *)
                match old_annotation with
                | Some _ -> old_annotation
                | None -> (
                    match new_annotation with
                    | None -> None
                    | Some new_annotation ->
                        Some (Mapper.map ~mapper:location_patcher new_annotation))
              in
              let override_parameter ({ Node.value = parameter; location }, new_annotation) =
                let { Parameter.annotation; _ } = parameter in
                {
                  Node.location;
                  value =
                    { parameter with annotation = override_annotation annotation new_annotation };
                }
              in
              Result.Ok
                ( List.map pairs ~f:override_parameter,
                  override_annotation returns (Some return_annotation) )))


let statement =
  let open Ast.Expression in
  let open Ast.Statement in
  let module Node = Ast.Node in
  let create_function_definition
      ~location
      ~async
      ~name
      ~args
      ~body
      ~decorator_list
      ~returns
      ~type_comment
      ~context:({ StatementContext.parent; _ } as context)
    =
    let body = build_statements ~context:{ context with parent = None } body in
    let comment_location =
      (* NOTE(grievejia): This is just a rough estimation on where type comment is. We don't know
         for sure since CPython does not preserve the positions of those comments. *)
      let open Ast.Location in
      let estimated_stop =
        match body with
        | [] -> location.stop
        | { Node.location; _ } :: _ -> location.start
      in
      { start = location.start; stop = estimated_stop }
    in
    match
      process_function_type_comment
        ~context
        ~parameters:args
        ~returns
        ~comment_location
        type_comment
    with
    | Result.Error message ->
        let {
          Ast.Location.start = { line; column };
          stop = { line = end_line; column = end_column };
        }
          =
          location
        in
        raise (InternalError { Error.line; column; end_line; end_column; message })
    | Result.Ok (parameters, return_annotation) ->
        let signature =
          {
            Define.Signature.name = Ast.Reference.create name;
            parameters;
            decorators = decorator_list;
            return_annotation;
            async;
            generator = is_generator body;
            parent = Option.map parent ~f:Ast.Reference.create;
            nesting_define = None;
          }
        in
        [
          Statement.Define { Define.signature; captures = []; unbound_names = []; body }
          |> Node.create ~location;
        ]
  in
  let function_def ~location ~name ~args ~body ~decorator_list ~returns ~type_comment ~context =
    create_function_definition
      ~location
      ~async:false
      ~name
      ~args
      ~body
      ~decorator_list
      ~returns
      ~type_comment
      ~context
  in
  let async_function_def ~location ~name ~args ~body ~decorator_list ~returns ~type_comment ~context
    =
    create_function_definition
      ~location
      ~async:true
      ~name
      ~args
      ~body
      ~decorator_list
      ~returns
      ~type_comment
      ~context
  in
  let class_def ~location ~name ~bases ~keywords ~body ~decorator_list ~context =
    let base_arguments =
      List.append
        (List.map bases ~f:convert_positional_argument)
        (List.map keywords ~f:convert_keyword_argument)
    in
    [
      Statement.Class
        {
          Class.name = Ast.Reference.create name;
          base_arguments;
          body = build_statements ~context:{ context with StatementContext.parent = Some name } body;
          decorators = decorator_list;
          top_level_unbound_names = [];
        }
      |> Node.create ~location;
    ]
  in
  let return ~location ~value ~context:_ =
    [Statement.Return { Return.expression = value; is_implicit = false } |> Node.create ~location]
  in
  let delete ~location ~targets ~context:_ = [Statement.Delete targets |> Node.create ~location] in
  let assign ~location ~targets ~value ~type_comment ~context:_ =
    (* Eagerly turn chained assignments `a = b = c` into `a = c; b = c`. *)
    let create_assign_for_target target =
      let location =
        let open Ast.Location in
        let { start; _ } = Node.location target in
        { location with start }
      in
      let annotation =
        match type_comment, target with
        | Some comment, { Node.value = Expression.Name _; _ } ->
            let annotation = Expression.Constant (Constant.String (StringLiteral.create comment)) in
            let location =
              (* Type comments do not have locations attached in CPython. This is just a rough
                 guess.*)
              let open Ast.Location in
              let { stop = { line = start_line; column = start_column }; _ } =
                Node.location value
              in
              let { stop; _ } = location in
              { start = { line = start_line; column = start_column + 1 }; stop }
            in
            Some (Node.create ~location annotation)
        | _ ->
            (* TODO (T104971233): Support type comments when the LHS of assign is a list/tuple. *)
            None
      in
      create_assign ~location ~target ~annotation ~value:(Some value) ()
    in
    List.map targets ~f:create_assign_for_target
  in
  let aug_assign ~location ~target ~op ~value ~context:_ =
    let callee =
      let dunder_name = Caml.Format.sprintf "__i%s__" op in
      Expression.Name
        (Name.Attribute { base = target; attribute = identifier dunder_name; special = true })
      |> Node.create ~location:(Node.location target)
    in
    let value =
      Expression.Call { callee; arguments = [{ Call.Argument.name = None; value }] }
      |> Node.create ~location
    in
    [create_assign ~location ~target ~annotation:None ~value:(Some value) ()]
  in
  let ann_assign ~location ~target ~annotation ~value ~simple:_ ~context:_ =
    [create_assign ~location ~target ~annotation:(Some annotation) ~value ()]
  in
  let for_ ~location ~target ~iter ~body ~orelse ~type_comment:_ ~context =
    [
      Statement.For
        {
          For.target;
          iterator = iter;
          body = build_statements ~context body;
          orelse = build_statements ~context orelse;
          async = false;
        }
      |> Node.create ~location;
    ]
  in
  let async_for ~location ~target ~iter ~body ~orelse ~type_comment:_ ~context =
    [
      Statement.For
        {
          For.target;
          iterator = iter;
          body = build_statements ~context body;
          orelse = build_statements ~context orelse;
          async = true;
        }
      |> Node.create ~location;
    ]
  in
  let while_ ~location ~test ~body ~orelse ~context =
    [
      Statement.While
        {
          While.test;
          body = build_statements ~context body;
          orelse = build_statements ~context orelse;
        }
      |> Node.create ~location;
    ]
  in
  let if_ ~location ~test ~body ~orelse ~context =
    [
      Statement.If
        {
          If.test;
          body = build_statements ~context body;
          orelse = build_statements ~context orelse;
        }
      |> Node.create ~location;
    ]
  in
  let with_ ~location ~items ~body ~type_comment:_ ~context =
    [
      Statement.With { With.items; body = build_statements ~context body; async = false }
      |> Node.create ~location;
    ]
  in
  let async_with ~location ~items ~body ~type_comment:_ ~context =
    [
      Statement.With { With.items; body = build_statements ~context body; async = true }
      |> Node.create ~location;
    ]
  in
  let match_ ~location ~subject ~cases ~context =
    let check_cases_refutability cases =
      let is_case_irrefutable case = not (Ast.Statement.Match.Case.is_refutable case) in
      let raise_remanining_patterns_unreachable { Match.Case.pattern = { Ast.Node.location; _ }; _ }
        =
        let {
          Ast.Location.start = { line; column };
          stop = { line = end_line; column = end_column };
        }
          =
          location
        in
        raise
          (InternalError
             {
               Error.line;
               column;
               end_line;
               end_column;
               message = "This pattern makes remaining patterns unreachable.";
             })
      in
      List.iter (List.drop_last_exn cases) ~f:(fun case ->
          if is_case_irrefutable case then raise_remanining_patterns_unreachable case)
    in
    let cases = build_match_cases ~context cases in
    check_cases_refutability cases;
    [Statement.Match { Match.subject; cases } |> Node.create ~location]
  in
  let raise_ ~location ~exc ~cause ~context:_ =
    [Statement.Raise { Raise.expression = exc; from = cause } |> Node.create ~location]
  in
  let try_ ~location ~body ~handlers ~orelse ~finalbody ~context =
    [
      Statement.Try
        {
          Try.body = build_statements ~context body;
          orelse = build_statements ~context orelse;
          finally = build_statements ~context finalbody;
          handlers = build_exception_handlers ~context handlers;
        }
      |> Node.create ~location;
    ]
  in
  let assert_ ~location ~test ~msg ~context:_ =
    [
      Statement.Assert { Assert.test; message = msg; origin = Assert.Origin.Assertion }
      |> Node.create ~location;
    ]
  in
  let import ~location ~names ~context:_ =
    [Statement.Import { Import.imports = names; from = None } |> Node.create ~location]
  in
  let import_from ~location ~module_ ~names ~level ~context:_ =
    let dots = List.init level ~f:(fun _ -> ".") |> String.concat ~sep:"" in
    let from_module_name = Option.value module_ ~default:"" in
    let from = Caml.Format.sprintf "%s%s" dots from_module_name |> Ast.Reference.create in
    [Statement.Import { Import.imports = names; from = Some from } |> Node.create ~location]
  in
  let global ~location ~names ~context:_ = [Statement.Global names |> Node.create ~location] in
  let nonlocal ~location ~names ~context:_ = [Statement.Nonlocal names |> Node.create ~location] in
  let expr ~location ~value ~context:_ = [Statement.Expression value |> Node.create ~location] in
  let pass ~location ~context:_ = [Statement.Pass |> Node.create ~location] in
  let break ~location ~context:_ = [Statement.Break |> Node.create ~location] in
  let continue ~location ~context:_ = [Statement.Continue |> Node.create ~location] in
  PyreAst.TaglessFinal.Statement.make
    ~function_def
    ~async_function_def
    ~class_def
    ~return
    ~delete
    ~assign
    ~aug_assign
    ~ann_assign
    ~for_
    ~async_for
    ~while_
    ~if_
    ~with_
    ~async_with
    ~match_
    ~raise_
    ~try_
    ~assert_
    ~import
    ~import_from
    ~global
    ~nonlocal
    ~expr
    ~pass
    ~break
    ~continue
    ()


let type_ignore ~lineno:_ ~tag:_ = ()

let module_ ~body ~type_ignores:_ ~context = build_statements ~context body

let function_type ~argtypes ~returns =
  { FunctionSignature.parameter_annotations = argtypes; return_annotation = returns }


let specification =
  PyreAst.TaglessFinal.make
    ~argument
    ~arguments
    ~binary_operator
    ~boolean_operator
    ~comparison_operator
    ~comprehension
    ~constant
    ~exception_handler
    ~expression
    ~expression_context
    ~function_type
    ~identifier
    ~import_alias
    ~keyword
    ~location
    ~match_case
    ~module_
    ~pattern
    ~position
    ~statement
    ~type_ignore
    ~unary_operator
    ~with_item
    ()


let with_context ?on_failure = PyreAst.Parser.with_context ?on_init_failure:on_failure

let parse_module ?enable_type_comment ~context text =
  try
    let open Result in
    let parse_function_signature text =
      PyreAst.Parser.TaglessFinal.parse_function_type ~context ~spec:specification text
    in
    PyreAst.Parser.TaglessFinal.parse_module ?enable_type_comment ~context ~spec:specification text
    >>= fun module_builder ->
    Ok (module_builder ~context:{ StatementContext.parse_function_signature; parent = None })
  with
  | InternalError error -> Result.Error error


let parse_module_exn ?enable_type_comment ~context text =
  match parse_module ?enable_type_comment ~context text with
  | Result.Ok statements -> statements
  | Result.Error error -> raise (Exception error)


let parse_expression ~context text =
  try PyreAst.Parser.TaglessFinal.parse_expression ~context ~spec:specification text with
  | InternalError error -> Result.Error error
