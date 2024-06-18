(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type num =
  | Int of int
  | Float of float
  | Complex of float
  | Big_int of string
      (** pyast uses Py.Object.t for Big_int, but this is presumably not what we want (if it's an
          opaque wrapper for a CPython object). Just have the source text representing the integer
          digits for now instead. *)
[@@deriving show]

type object_ = num [@@deriving show]

type constant_desc =
  | Ellipsis
  | Bool of bool
  | Num of num
  | Str of string
  | ByteStr of string
[@@deriving show]

type constant = constant_desc option [@@deriving show]
type singleton = bool option [@@deriving show]

type withitem = {
  context_expr: expr;
  optional_vars: expr option;
}

and recoverableerrorwithlocation = {
  error: string;
  lineno: int;
  col_offset: int;
  end_lineno: int;
  end_col_offset: int;
}

and unaryop =
  | Invert
  | Not
  | UAdd
  | USub

and type_ignore =
  | TypeIgnore of {
      lineno: int;
      tag: string;
    }

and stmt = {
  desc: stmt_desc;
  lineno: int;
  col_offset: int;
  end_lineno: int option;
  end_col_offset: int option;
}

and stmt_desc =
  | FunctionDef of {
      name: string;
      args: arguments;
      body: stmt list;
      decorator_list: expr list;
      type_params: expr list;
      returns: expr option;
      type_comment: string option;
    }
  | AsyncFunctionDef of {
      name: string;
      args: arguments;
      body: stmt list;
      decorator_list: expr list;
      type_params: expr list;
      returns: expr option;
      type_comment: string option;
    }
  | ClassDef of {
      name: string;
      bases: expr list;
      keywords: keyword list;
      body: stmt list;
      decorator_list: expr list;
      type_params: expr list;
    }
  | Return of expr option
  | Delete of expr list
  | Assign of {
      targets: expr list;
      value: expr;
      type_comment: string option;
    }
  | AugAssign of {
      target: expr;
      op: operator;
      value: expr;
    }
  | AnnAssign of {
      target: expr;
      annotation: expr;
      value: expr option;
      simple: int;
    }
  | For of {
      target: expr;
      iter: expr;
      body: stmt list;
      orelse: stmt list;
      type_comment: string option;
    }
  | AsyncFor of {
      target: expr;
      iter: expr;
      body: stmt list;
      orelse: stmt list;
      type_comment: string option;
    }
  | While of {
      test: expr;
      body: stmt list;
      orelse: stmt list;
    }
  | If of {
      test: expr;
      body: stmt list;
      orelse: stmt list;
    }
  | With of {
      items: withitem list;
      body: stmt list;
      type_comment: string option;
    }
  | AsyncWith of {
      items: withitem list;
      body: stmt list;
      type_comment: string option;
    }
  | Match of {
      subject: expr;
      cases: match_case list;
    }
  | Raise of {
      exc: expr option;
      cause: expr option;
    }
  | Try of {
      body: stmt list;
      handlers: excepthandler list;
      orelse: stmt list;
      finalbody: stmt list;
    }
  | Assert of {
      test: expr;
      msg: expr option;
    }
  | Import of alias list
  | ImportFrom of {
      module_: string option;
      names: alias list;
      level: int option;
    }
  | Global of string list
  | Nonlocal of string list
  | Expr of expr
  | Pass
  | Break
  | Continue

and slice = expr

and pattern = {
  desc: pattern_desc;
  lineno: int;
  col_offset: int;
  end_lineno: int;
  end_col_offset: int;
}

and pattern_desc =
  | MatchValue of expr
  | MatchSingleton of constant
  | MatchSequence of pattern list
  | MatchMapping of {
      keys: expr list;
      patterns: pattern list;
      rest: string option;
    }
  | MatchClass of {
      cls: expr;
      patterns: pattern list;
      kwd_attrs: string list;
      kwd_patterns: pattern list;
    }
  | MatchStar of string option
  | MatchAs of {
      pattern: pattern option;
      name: string option;
    }
  | MatchOr of pattern list

and operator =
  | Add
  | Sub
  | Mult
  | MatMult
  | Div
  | Mod
  | Pow
  | LShift
  | RShift
  | BitOr
  | BitXor
  | BitAnd
  | FloorDiv

and mod_ =
  | Module of {
      body: stmt list;
      type_ignores: type_ignore list;
    }
  | Interactive of stmt list
  | Expression of expr
  | FunctionType of {
      argtypes: expr list;
      returns: expr;
    }

and match_case = {
  pattern: pattern;
  guard: expr option;
  body: stmt list;
}

and keyword = {
  arg: string option;
  value: expr;
  lineno: int;
  col_offset: int;
  end_lineno: int option;
  end_col_offset: int option;
}

and expr_context =
  | Load
  | Store
  | Del

and expr = {
  desc: expr_desc;
  lineno: int;
  col_offset: int;
  end_lineno: int option;
  end_col_offset: int option;
}

and expr_desc =
  | BoolOp of {
      op: boolop;
      values: expr list;
    }
  | NamedExpr of {
      target: expr;
      value: expr;
    }
  | BinOp of {
      left: expr;
      op: operator;
      right: expr;
    }
  | UnaryOp of {
      op: unaryop;
      operand: expr;
    }
  | Lambda of {
      args: arguments;
      body: expr;
    }
  | IfExp of {
      test: expr;
      body: expr;
      orelse: expr;
    }
  | Dict of {
      keys: expr option list;
      values: expr list;
    }
  | Set of expr list
  | ListComp of {
      elt: expr;
      generators: comprehension list;
    }
  | SetComp of {
      elt: expr;
      generators: comprehension list;
    }
  | DictComp of {
      key: expr;
      value: expr;
      generators: comprehension list;
    }
  | GeneratorExp of {
      elt: expr;
      generators: comprehension list;
    }
  | Await of expr
  | Yield of expr option
  | YieldFrom of expr
  | Compare of {
      left: expr;
      ops: cmpop list;
      comparators: expr list;
    }
  | Call of {
      func: expr;
      args: expr list;
      keywords: keyword list;
    }
  | FormattedValue of {
      value: expr;
      conversion: int option;
      format_spec: expr option;
    }
  | JoinedStr of expr list
  | Constant of {
      value: constant;
      kind: string option;
    }
  | Attribute of {
      value: expr;
      attr: string;
      ctx: expr_context;
    }
  | Subscript of {
      value: expr;
      slice: expr;
      ctx: expr_context;
    }
  | Starred of {
      value: expr;
      ctx: expr_context;
    }
  | Name of {
      id: string;
      ctx: expr_context;
    }
  | List of {
      elts: expr list;
      ctx: expr_context;
    }
  | Tuple of {
      elts: expr list;
      ctx: expr_context;
    }
  | Slice of {
      lower: expr option;
      upper: expr option;
      step: expr option;
    }

and excepthandler = {
  desc: excepthandler_desc;
  lineno: int;
  col_offset: int;
  end_lineno: int option;
  end_col_offset: int option;
}

and excepthandler_desc =
  | ExceptHandler of {
      type_: expr option;
      name: string option;
      body: stmt list;
    }

and comprehension = {
  target: expr;
  iter: expr;
  ifs: expr list;
  is_async: bool;
}

and cmpop =
  | Eq
  | NotEq
  | Lt
  | LtE
  | Gt
  | GtE
  | Is
  | IsNot
  | In
  | NotIn

and boolop =
  | And
  | Or

and arguments = {
  posonlyargs: arg list;
  args: arg list;
  vararg: arg option;
  kwonlyargs: arg list;
  kw_defaults: expr option list;
  kwarg: arg option;
  defaults: expr list;
}

and arg = {
  arg: string;
  annotation: expr option;
  type_comment: string option;
  lineno: int;
  col_offset: int;
  end_lineno: int option;
  end_col_offset: int option;
}

and alias = {
  name: string;
  asname: string option;
  lineno: int;
  col_offset: int;
  end_lineno: int option;
  end_col_offset: int option;
}
[@@deriving show]

type arguments_args =
  | Expr_list of expr list
  | Arg_list of arg list
[@@deriving show]

type arguments_kwarg =
  | Identifier_opt of string option
  | Arg_opt of arg option
[@@deriving show]

type arguments_vararg =
  | Identifier_opt of string option
  | Arg_opt of arg option
[@@deriving show]

type excepthandler_excepthandler_name =
  | Identifier_opt of string option
  | Expr_opt of expr option
[@@deriving show]

type expr_bytes_s =
  | String of string
  | Bytes of string
[@@deriving show]

type expr_subscript_slice =
  | Slice of slice
  | Expr of expr
[@@deriving show]

type expr_yieldfrom_value =
  | Expr_opt of expr option
  | Expr of expr
[@@deriving show]

type keyword_arg =
  | Identifier_opt of string option
  | Identifier of string
[@@deriving show]

type stmt_importfrom_module =
  | Identifier_opt of string option
  | Identifier of string
[@@deriving show]
