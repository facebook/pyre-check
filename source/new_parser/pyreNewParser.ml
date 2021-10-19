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
  let and_ () = failwith "not implemented yet" in
  let or_ () = failwith "not implemented yet" in
  PyreAst.TaglessFinal.BooleanOperator.make ~and_ ~or_ ()


let binary_operator =
  let add () = failwith "not implemented yet" in
  let sub () = failwith "not implemented yet" in
  let mult () = failwith "not implemented yet" in
  let matmult () = failwith "not implemented yet" in
  let div () = failwith "not implemented yet" in
  let mod_ () = failwith "not implemented yet" in
  let pow () = failwith "not implemented yet" in
  let lshift () = failwith "not implemented yet" in
  let rshift () = failwith "not implemented yet" in
  let bitor () = failwith "not implemented yet" in
  let bitxor () = failwith "not implemented yet" in
  let bitand () = failwith "not implemented yet" in
  let floordiv () = failwith "not implemented yet" in
  PyreAst.TaglessFinal.BinaryOperator.make
    ~add
    ~sub
    ~mult
    ~matmult
    ~div
    ~mod_
    ~pow
    ~lshift
    ~rshift
    ~bitor
    ~bitxor
    ~bitand
    ~floordiv
    ()


let unary_operator =
  let invert () = failwith "not implemented yet" in
  let not_ () = failwith "not implemented yet" in
  let uadd () = failwith "not implemented yet" in
  let usub () = failwith "not implemented yet" in
  PyreAst.TaglessFinal.UnaryOperator.make ~invert ~not_ ~uadd ~usub ()


let comparison_operator =
  let eq () = failwith "not implemented yet" in
  let noteq () = failwith "not implemented yet" in
  let lt () = failwith "not implemented yet" in
  let lte () = failwith "not implemented yet" in
  let gt () = failwith "not implemented yet" in
  let gte () = failwith "not implemented yet" in
  let is () = failwith "not implemented yet" in
  let isnot () = failwith "not implemented yet" in
  let in_ () = failwith "not implemented yet" in
  let notin () = failwith "not implemented yet" in
  PyreAst.TaglessFinal.ComparisonOperator.make
    ~eq
    ~noteq
    ~lt
    ~lte
    ~gt
    ~gte
    ~is
    ~isnot
    ~in_
    ~notin
    ()


let comprehension ~target ~iter ~ifs ~is_async =
  {
    Ast.Expression.Comprehension.Generator.target;
    iterator = iter;
    conditions = ifs;
    async = is_async;
  }


let keyword ~location:_ ~arg:_ ~value:_ = failwith "not implemented yet"

let argument ~location:_ ~identifier:_ ~annotation:_ ~type_comment:_ =
  failwith "not implemented yet"


let arguments ~posonlyargs:_ ~args:_ ~vararg:_ ~kwonlyargs:_ ~kw_defaults:_ ~kwarg:_ ~defaults:_ =
  failwith "not implemented yet"


let expression =
  let open Ast.Expression in
  let module Node = Ast.Node in
  let bool_op ~location:_ ~op:_ ~values:_ = failwith "not implemented yet" in
  let named_expr ~location ~target ~value =
    (* TODO(T47589601): `target` can be strenghthened into `Identifier.t` if qualification is
       removed. *)
    Expression.WalrusOperator { WalrusOperator.target; value } |> Node.create ~location
  in
  let bin_op ~location:_ ~left:_ ~op:_ ~right:_ = failwith "not implemented yet" in
  let unary_op ~location:_ ~op:_ ~operand:_ = failwith "not implemented yet" in
  let lambda ~location:_ ~args:_ ~body:_ = failwith "not implemented yet" in
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
  let compare ~location:_ ~left:_ ~ops:_ ~comparators:_ = failwith "not implemented yet" in
  let call ~location:_ ~func:_ ~args:_ ~keywords:_ = failwith "not implemented yet" in
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
  let attribute ~location:_ ~value:_ ~attr:_ ~ctx:() = failwith "not implemented yet" in
  let subscript ~location:_ ~value:_ ~slice:_ ~ctx:() = failwith "not implemented yet" in
  let starred ~location ~value ~ctx:() =
    Expression.Starred (Starred.Once value) |> Node.create ~location
  in
  let name ~location ~id ~ctx:() = Expression.Name (Name.Identifier id) |> Node.create ~location in
  let list ~location ~elts ~ctx:() = Expression.List elts |> Node.create ~location in
  let tuple ~location ~elts ~ctx:() = Expression.Tuple elts |> Node.create ~location in
  let slice ~location:_ ~lower:_ ~upper:_ ~step:_ = failwith "not implemented yet" in
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


let with_item ~context_expr:_ ~optional_vars:_ = failwith "not implemented yet"

let import_alias ~location:_ ~name:_ ~asname:_ = failwith "not implemented yet"

let exception_handler ~location:_ ~type_:_ ~name:_ ~body:_ = failwith "not implemented yet"

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
  let function_def ~location:_ ~name:_ ~args:_ ~body:_ ~decorator_list:_ ~returns:_ ~type_comment:_ =
    failwith "not implemented yet"
  in
  let async_function_def
      ~location:_
      ~name:_
      ~args:_
      ~body:_
      ~decorator_list:_
      ~returns:_
      ~type_comment:_
    =
    failwith "not implemented yet"
  in
  let class_def ~location:_ ~name:_ ~bases:_ ~keywords:_ ~body:_ ~decorator_list:_ =
    failwith "not implemented yet"
  in
  let return ~location:_ ~value:_ = failwith "not implemented yet" in
  let delete ~location:_ ~targets:_ = failwith "not implemented yet" in
  let assign ~location:_ ~targets:_ ~value:_ ~type_comment:_ = failwith "not implemented yet" in
  let aug_assign ~location:_ ~target:_ ~op:_ ~value:_ = failwith "not implemented yet" in
  let ann_assign ~location:_ ~target:_ ~annotation:_ ~value:_ ~simple:_ =
    failwith "not implemented yet"
  in
  let for_ ~location:_ ~target:_ ~iter:_ ~body:_ ~orelse:_ ~type_comment:_ =
    failwith "not implemented yet"
  in
  let async_for ~location:_ ~target:_ ~iter:_ ~body:_ ~orelse:_ ~type_comment:_ =
    failwith "not implemented yet"
  in
  let while_ ~location:_ ~test:_ ~body:_ ~orelse:_ = failwith "not implemented yet" in
  let if_ ~location:_ ~test:_ ~body:_ ~orelse:_ = failwith "not implemented yet" in
  let with_ ~location:_ ~items:_ ~body:_ ~type_comment:_ = failwith "not implemented yet" in
  let async_with ~location:_ ~items:_ ~body:_ ~type_comment:_ = failwith "not implemented yet" in
  let match_ ~location:_ ~subject:_ ~cases:_ =
    (* TODO(T102720335): Support pattern matching *)
    failwith "not implemented yet"
  in
  let raise_ ~location:_ ~exc:_ ~cause:_ = failwith "not implemented yet" in
  let try_ ~location:_ ~body:_ ~handlers:_ ~orelse:_ ~finalbody:_ =
    failwith "not implemented yet"
  in
  let assert_ ~location:_ ~test:_ ~msg:_ = failwith "not implemented yet" in
  let import ~location:_ ~names:_ = failwith "not implemented yet" in
  let import_from ~location:_ ~module_:_ ~names:_ ~level:_ = failwith "not implemented yet" in
  let global ~location:_ ~names:_ = failwith "not implemented yet" in
  let nonlocal ~location:_ ~names:_ = failwith "not implemented yet" in
  let expr ~location:_ ~value:_ = failwith "not implemented yet" in
  let pass ~location:_ = failwith "not implemented yet" in
  let break ~location:_ = failwith "not implemented yet" in
  let continue ~location:_ = failwith "not implemented yet" in
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
