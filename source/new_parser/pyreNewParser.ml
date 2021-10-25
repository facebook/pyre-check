(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base
module Context = PyreAst.Parser.Context
module Error = PyreAst.Parser.Error

exception Exception of Error.t

let position ~line ~column = { Ast.Location.line; column }

let location ~start ~stop = { Ast.Location.start; stop }

let identifier x = x

let expression_context = PyreAst.TaglessFinal.ExpressionContext.make ~load:() ~store:() ~del:() ()

let constant =
  let open Ast.Expression in
  let integer i = Constant.Integer i in
  let big_integer _ =
    (* TODO (T102723192): We should probably mark this case properly. *)
    Constant.Integer Int.max_value
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
  {
    Ast.Expression.Comprehension.Generator.target;
    iterator = iter;
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
  let delimiter_parameter ~delimited name =
    (* TODO(T101307161): This is just an ugly temporary hack that helps preserve backward
       compatibility. *)
    if List.is_empty delimited then
      []
    else
      [Node.create_with_default_location { Parameter.name; value = None; annotation = None }]
  in
  List.concat
    [
      positional_only_parameters;
      delimiter_parameter ~delimited:positional_only_parameters "/";
      regular_parameters;
      Option.to_list vararg_parameter;
      delimiter_parameter ~delimited:keyword_only_parameters "*";
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
          Expression.BooleanOperator { BooleanOperator.left = sofar; operator = op; right = next }
          |> Node.create ~location:{ location with stop = next.location.stop }
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
      |> Node.create ~location
    in
    Expression.Call { callee; arguments = [{ Call.Argument.name = None; value = right }] }
    |> Node.create ~location
  in
  let unary_op ~location ~op ~operand =
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
        Expression.ComparisonOperator { ComparisonOperator.left = last; operator; right = next }
        |> Node.create
             ~location:{ Ast.Location.start = last.location.start; stop = next.location.stop }
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
        let first_operand =
          Expression.ComparisonOperator { ComparisonOperator.left; operator; right }
          |> Node.create ~location:{ location with stop = right.location.stop }
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


let with_item ~context_expr ~optional_vars = context_expr, optional_vars

let import_alias ~location ~name ~asname =
  let open Ast in
  Node.create ~location { Statement.Import.name = Reference.create name; alias = asname }


let exception_handler ~location:_ ~type_ ~name ~body =
  { Ast.Statement.Try.Handler.kind = type_; name; body }


(* TODO(T102720335): Support pattern matching *)
let match_case ~pattern:_ ~guard:_ ~body:_ = failwith "not implemented yet"

(* TODO(T102720335): Support pattern matching *)
let pattern =
  let match_value ~location:_ ~value:_ = () in
  let match_singleton ~location:_ ~value:_ = () in
  let match_sequence ~location:_ ~patterns:_ = () in
  let match_mapping ~location:_ ~keys:_ ~patterns:_ ~rest:_ = () in
  let match_class ~location:_ ~cls:_ ~patterns:_ ~kwd_attrs:_ ~kwd_patterns:_ = () in
  let match_star ~location:_ ~name:_ = () in
  let match_as ~location:_ ~pattern:_ ~name:_ = () in
  let match_or ~location:_ ~patterns:_ = () in
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


let statement =
  let open Ast.Statement in
  let module Node = Ast.Node in
  let function_def ~location ~name ~args ~body ~decorator_list ~returns ~type_comment:_ =
    let signature =
      {
        Define.Signature.name = Ast.Reference.create name;
        parameters = args;
        decorators = decorator_list;
        return_annotation = returns;
        async = false;
        generator = is_generator body;
        parent = None;
        nesting_define = None;
      }
    in
    Statement.Define { Define.signature; captures = []; unbound_names = []; body }
    |> Node.create ~location
  in
  let async_function_def ~location ~name ~args ~body ~decorator_list ~returns ~type_comment:_ =
    let signature =
      {
        Define.Signature.name = Ast.Reference.create name;
        parameters = args;
        decorators = decorator_list;
        return_annotation = returns;
        async = true;
        generator = is_generator body;
        parent = None;
        nesting_define = None;
      }
    in
    Statement.Define { Define.signature; captures = []; unbound_names = []; body }
    |> Node.create ~location
  in
  let class_def ~location ~name ~bases ~keywords ~body ~decorator_list =
    let base_arguments =
      List.append
        (List.map bases ~f:convert_positional_argument)
        (List.map keywords ~f:convert_keyword_argument)
    in
    Statement.Class
      {
        Class.name = Ast.Reference.create name;
        base_arguments;
        body;
        decorators = decorator_list;
        top_level_unbound_names = [];
      }
    |> Node.create ~location
  in
  let return ~location ~value =
    Statement.Return { Return.expression = value; is_implicit = false } |> Node.create ~location
  in
  let delete ~location ~targets = Statement.Delete targets |> Node.create ~location in
  let assign ~location:_ ~targets:_ ~value:_ ~type_comment:_ = failwith "not implemented yet" in
  let aug_assign ~location:_ ~target:_ ~op:_ ~value:_ = failwith "not implemented yet" in
  let ann_assign ~location:_ ~target:_ ~annotation:_ ~value:_ ~simple:_ =
    failwith "not implemented yet"
  in
  let for_ ~location ~target ~iter ~body ~orelse ~type_comment:_ =
    Statement.For { For.target; iterator = iter; body; orelse; async = false }
    |> Node.create ~location
  in
  let async_for ~location ~target ~iter ~body ~orelse ~type_comment:_ =
    Statement.For { For.target; iterator = iter; body; orelse; async = true }
    |> Node.create ~location
  in
  let while_ ~location ~test ~body ~orelse =
    Statement.While { While.test; body; orelse } |> Node.create ~location
  in
  let if_ ~location ~test ~body ~orelse =
    Statement.If { If.test; body; orelse } |> Node.create ~location
  in
  let with_ ~location ~items ~body ~type_comment:_ =
    Statement.With { With.items; body; async = false } |> Node.create ~location
  in
  let async_with ~location ~items ~body ~type_comment:_ =
    Statement.With { With.items; body; async = true } |> Node.create ~location
  in
  let match_ ~location:_ ~subject:_ ~cases:_ =
    (* TODO(T102720335): Support pattern matching *)
    failwith "not implemented yet"
  in
  let raise_ ~location ~exc ~cause =
    Statement.Raise { Raise.expression = exc; from = cause } |> Node.create ~location
  in
  let try_ ~location ~body ~handlers ~orelse ~finalbody =
    Statement.Try { Try.body; orelse; finally = finalbody; handlers } |> Node.create ~location
  in
  let assert_ ~location ~test ~msg =
    Statement.Assert { Assert.test; message = msg; origin = Assert.Origin.Assertion }
    |> Node.create ~location
  in
  let import ~location ~names =
    Statement.Import { Import.imports = names; from = None } |> Node.create ~location
  in
  let import_from ~location ~module_ ~names ~level =
    let dots = List.init level ~f:(fun _ -> ".") |> String.concat ~sep:"" in
    let from_module_name = Option.value module_ ~default:"" in
    let from = Caml.Format.sprintf "%s%s" dots from_module_name |> Ast.Reference.create in
    Statement.Import { Import.imports = names; from = Some from } |> Node.create ~location
  in
  let global ~location ~names = Statement.Global names |> Node.create ~location in
  let nonlocal ~location ~names = Statement.Nonlocal names |> Node.create ~location in
  let expr ~location ~value = Statement.Expression value |> Node.create ~location in
  let pass ~location = Statement.Pass |> Node.create ~location in
  let break ~location = Statement.Break |> Node.create ~location in
  let continue ~location = Statement.Continue |> Node.create ~location in
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

let module_ ~body ~type_ignores:_ = body

let function_type ~argtypes:_ ~returns:_ = ()

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

let parse_module ?filename ?enable_type_comment ~context =
  PyreAst.Parser.TaglessFinal.parse_module
    ?filename
    ?enable_type_comment
    ~context
    ~spec:specification


let parse_module_exn ?filename ?enable_type_comment ~context text =
  match parse_module ?filename ?enable_type_comment ~context text with
  | Result.Ok statements -> statements
  | Result.Error error -> raise (Exception error)


let parse_expression ~context =
  PyreAst.Parser.TaglessFinal.parse_expression ~context ~spec:specification
