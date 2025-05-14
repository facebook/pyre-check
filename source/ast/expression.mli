(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module StringLiteral : sig
  type kind =
    | String
    | Bytes
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  type t = {
    value: string;
    kind: kind;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int

  val create : ?bytes:bool -> string -> t
end

module Constant : sig
  type t =
    | NoneLiteral
    | Ellipsis
    | False
    | True
    | Integer of int
    | BigInteger of string
    | Float of float
    | Complex of float
    | String of StringLiteral.t
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end

module rec BooleanOperator : sig
  type operator =
    | And
    | Or
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  type t = {
    left: Expression.t;
    operator: operator;
    right: Expression.t;
    origin: Origin.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val pp_boolean_operator : Format.formatter -> operator -> unit

  val inverse : operator -> operator

  val location_insensitive_compare : t -> t -> int
end

and Call : sig
  module Argument : sig
    type t = {
      name: Identifier.t Node.t option;
      value: Expression.t;
    }
    [@@deriving equal, compare, sexp, show, hash, to_yojson]

    val equal : t -> t -> bool

    type kind =
      | SingleStar
      | DoubleStar
      | Named of {
          name: string Node.t;
          (* During signature selection when typed dictionaries are unpacked as kwargs, their fields
             are expanded into named arguments. Fields in non-total dictionaries or those marked
             with `NotRequired` may or may not be present.

             When this flag is set, we want to check that the types are correct if the arguments are
             present, but also require that they are only matched to parameters which do not require
             an argument (such as named parameters with defaults, or kwargs).

             Regular named arguments from other sources will have this value set to false. *)
          requires_default: bool;
        }
      | Positional
    [@@deriving equal, compare, show]

    val location_insensitive_compare : t -> t -> int

    val unpack : t -> Expression.t * kind
  end

  type t = {
    callee: Expression.t;
    arguments: Argument.t list;
    origin: Origin.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end

and Await : sig
  type t = {
    operand: Expression.t;
    origin: Origin.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end

and ComparisonOperator : sig
  type operator =
    | Equals
    | GreaterThan
    | GreaterThanOrEquals
    | In
    | Is
    | IsNot
    | LessThan
    | LessThanOrEquals
    | NotEquals
    | NotIn
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  type t = {
    left: Expression.t;
    operator: operator;
    right: Expression.t;
    origin: Origin.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val inverse : operator -> operator

  val pp_comparison_operator : Format.formatter -> operator -> unit

  val lower_to_expression
    :  location:Location.t ->
    callee_location:Location.t ->
    t ->
    Expression.t option

  val location_insensitive_compare : t -> t -> int
end

and BinaryOperator : sig
  type operator =
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
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  type t = {
    left: Expression.t;
    operator: operator;
    right: Expression.t;
    origin: Origin.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val pp_binary_operator : Format.formatter -> operator -> unit

  val location_insensitive_compare : t -> t -> int

  val lower_to_call : location:Location.t -> callee_location:Location.t -> t -> Call.t

  val lower_to_expression : location:Location.t -> callee_location:Location.t -> t -> Expression.t

  val binary_operator_method : operator -> string
end

and Comprehension : sig
  module Generator : sig
    type t = {
      target: Expression.t;
      iterator: Expression.t;
      conditions: Expression.t list;
      async: bool;
    }
    [@@deriving equal, compare, sexp, show, hash, to_yojson]

    val location_insensitive_compare : t -> t -> int
  end

  type 'element t = {
    element: 'element;
    generators: Generator.t list;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare
    :  ('element -> 'element -> int) ->
    'element t ->
    'element t ->
    int
end

and Dictionary : sig
  module Entry : sig
    module KeyValue : sig
      type t = {
        key: Expression.t;
        value: Expression.t;
      }
      [@@deriving equal, compare, sexp, show, hash, to_yojson]

      val location_insensitive_compare : t -> t -> int
    end

    type t =
      | KeyValue of KeyValue.t
      | Splat of Expression.t
    [@@deriving equal, compare, sexp, show, hash, to_yojson]

    val location_insensitive_compare : t -> t -> int
  end

  type t = Entry.t list [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int

  val string_literal_keys : Entry.t list -> (string * Expression.t) list option

  val has_no_keywords : t -> bool
end

and Lambda : sig
  type t = {
    parameters: Parameter.t list;
    body: Expression.t;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]
end

and Name : sig
  module Attribute : sig
    type t = {
      base: Expression.t;
      attribute: Identifier.t;
      origin: Origin.t option;
    }
    [@@deriving equal, compare, sexp, show, hash, to_yojson]

    val location_insensitive_compare : t -> t -> int
  end

  type t =
    | Attribute of Attribute.t
    | Identifier of Identifier.t
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int

  val last : t -> string
end

and Parameter : sig
  type parameter = {
    name: Identifier.t;
    value: Expression.t option;
    annotation: Expression.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  type t = parameter Node.t [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val create
    :  location:Location.t ->
    ?value:Expression.t ->
    ?annotation:Expression.t ->
    name:Identifier.t ->
    unit ->
    t

  val name : t -> Identifier.t

  val location_insensitive_compare : t -> t -> int
end

and Starred : sig
  type t =
    | Once of Expression.t
    | Twice of Expression.t
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end

and Slice : sig
  type t = {
    start: Expression.t option;
    stop: Expression.t option;
    step: Expression.t option;
    origin: Origin.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int

  val lowered : location:Location.t -> t -> Expression.t
end

and Subscript : sig
  type t = {
    base: Expression.t;
    index: Expression.t;
    origin: Origin.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end

and Substring : sig
  type t =
    | Literal of string Node.t
    | Format of {
        value: Expression.t;
        format_spec: Expression.t option;
      }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end

and Ternary : sig
  type t = {
    target: Expression.t;
    test: Expression.t;
    alternative: Expression.t;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end

and UnaryOperator : sig
  type operator =
    | Invert
    | Negative
    | Not
    | Positive
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  type t = {
    operator: operator;
    operand: Expression.t;
    origin: Origin.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int

  val pp_unary_operator : Format.formatter -> operator -> unit

  val lower_to_expression
    :  location:Location.t ->
    callee_location:Location.t ->
    t ->
    Expression.t option
end

and WalrusOperator : sig
  type t = {
    target: Expression.t;
    value: Expression.t;
    origin: Origin.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end

and TypeParam : sig
  type type_var = {
    name: Identifier.t;
    bound: Expression.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  type type_param =
    | TypeVar of type_var
    | ParamSpec of Identifier.t
    | TypeVarTuple of Identifier.t
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  type t = type_param Node.t [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end

and Origin : sig
  (* During the analysis, we create artificial nodes that were not present
   * in the original code. This type is used to describe the original node
   * that originated the artificial node. *)
  type kind =
    | ComparisonOperator (* `a == b` is turned into `a.__eq__(b)` *)
    | BinaryOperator (* `a + b` is turned into `a.__add__(b)` *)
    | UnaryOperator (* `-a` is turned into `a.__neg__()` *)
    | AugmentedAssign (* `a += b` is turned into `a = a.__add__(b)` *)
    | Qualification of string list (* all symbols are turned into their fully qualified version *)
    | SubscriptSetItem (* `d[a] = b` is turned into `d.__setitem__(a, b)` *)
    | SubscriptGetItem (* `d[a]` is turned into `d.__getitem__(a)` *)
    | ForIter (* `for e in l:` is turned into `l.__iter__().__next__()` *)
    | ForNext (* `for e in l:` is turned into `l.__iter__().__next__()` *)
    | ForAwait (* `for e in l:` might be turned into `await l.__iter__().__next__()` *)
    | GeneratorIter (* `(e for e in l)` is turned into `l.__iter__().__next__()` *)
    | GeneratorNext (* `(e for e in l)` is turned into `l.__iter__().__next__()` *)
    | GeneratorAwait (* `(e for e in l)` might be turned into `await l.__iter__().__next__()` *)
    | With (* `with e1 as e2` is turned into `e2 = e1.__enter__()` *)
    | InContains (* `e in l` can be turned into `l.__contains__(e)` *)
    | InIter (* `e in l` can be turned into `l.__iter__().__next__().__eq__(e)` *)
    | InGetItem (* `e in l` can be turned into `l.__getitem__(0).__eq__(e)` *)
    | InGetItemEq (* `e in l` can be turned into `l.__getitem__(0).__eq__(e)` *)
    | Slice (* `1:2` is turned into `slice(1,2,None)` *)
    | UnionShorthand (* `a | b` is turned into `typing.Union[a, b]` when in typing context *)
    | Negate (* `if cond:` is turned into `assert(cond)` and `assert(not cond)` *)
    | NegateIs (* `not(a is not b)` is turned into `a is b` *)
    | NegateIsNot (* `not(a is b)` is turned into `a is not b` *)
    | Normalize (* we turn boolean expressions into negation normal form *)
    | NormalizeNotComparison (* `not(a < b)` is turned into `a >= b` *)
    | NormalizeNotBoolOperator (* `not(a == b)` is turned into `a != b` *)
    | TryHandlerIsInstance (* `try..except X as e` is turned into `assert(isinstance(X, e))` *)
    | NamedTupleConstructorAssignment of string
      (* `collections.namedtuple('T', 'a b')` is turned into `def __init__(self, a, b): self.a = a;
         self.b = b` *)
    | DataclassImplicitField
      (* `@dataclass` on `class A: x: int` is turned into `x: int = dataclasses.field()` *)
    | DataclassImplicitDefault
    (* `x: int = dataclasses.field(default_factory=f)` is turned into `x = f()` in the implicit
       constructor *)
    (* All the origins below are used to translate `match` statements *)
    | MatchTypingSequence
      (* `match x: case [..]:` is turned into `isinstance(x, typing.Sequence)` *)
    | MatchTypingMapping (* `match x: case {...}:` is turned into `isinstance(x, typing.Mapping)` *)
    | MatchAsComparisonEquals
    | MatchAsWithCondition
    | MatchClassArgs of int
    | MatchClassGetAttr of int
    | MatchClassArgsSubscript of int
    | MatchClassKeywordAttribute of string
    | MatchClassIsInstance
    | MatchClassJoinConditions
    | MatchMappingKeySubscript
    | MatchMappingRestDict of string
    | MatchMappingRestComparisonEquals of string
    | MatchMappingIsInstance
    | MatchMappingJoinConditions
    | MatchOrJoinConditions
    | MatchSingletonComparisonIs
    | MatchSequenceRestList of string
    | MatchSequenceRestSubscript of string
    | MatchSequenceRestSlice of string
    | MatchSequenceRestComparisonEquals of string
    | MatchSequencePrefix of int
    | MatchSequenceSuffix of int
    | MatchSequenceIsInstance
    | MatchSequenceJoinConditions
    | MatchValueComparisonEquals
    | MatchConditionWithGuard
    | StrCall (* str(x) is turned into x.__str__() or x.__repr__() *)
    | ReprCall (* repr(x) is turned into x.__repr__() *)
    | AbsCall (* abs(x) is turned into x.__abs__() *)
    | IterCall (* iter(x) is turned into x.__iter__() *)
    | NextCall (* next(x) is turned into x.__next__() *)
    | ImplicitInitCall (* A(x) is turned into A.__init__(..., x) *)
    | SelfImplicitTypeVar of string
    | SelfImplicitTypeVarQualification of string * string list
      (* `def f(self):` is turned into `def f(self: TSelf):` with `TSelf = TypeVar["self",
         bound=MyClass])` *)
    | FunctionalEnumImplicitAuto of string list
      (* `Enum("Color", ("RED", "GREEN", "BLUE"))` is turned into `class Color: RED = enum.auto();
         ...` *)
    | DecoratorInlining (* Pysa inlines decorator during preprocessing *)
    | ForDecoratedTarget (* `@foo def f(): ...` is turned into `def f@decorated(): return foo(f)` *)
    | ForDecoratedTargetCallee of
        string list (* `@foo def f(): ...` is turned into `def f@decorated(): return foo(f)` *)
    | FormatStringImplicitStr (* f"{x}" is turned into f"{x.__str__()}" or f"{x.__repr__}" *)
    | GetAttrConstantLiteral (* getattr(x, "foo") is turned into x.foo *)
    | SetAttrConstantLiteral (* object.__setattr__(x, "foo", value) is turned into x.foo = value *)
    | PysaCallRedirect of string (* hardcoded AST rewrite made for Pysa analysis *)
    | PysaHigherOrderParameter of
        int (* `f(g)` might be turned into `f(g())` for the taint analysis *)
    | ForTestPurpose (* AST node created when running tests *)
    | ForTypeChecking (* AST node created internally during a type check of an expression *)
    | Nested of {
        head: kind;
        tail: kind;
      }
      (* In some rare cases, AST lowering happens in multiple steps.
       * For instance: `match x: case 0: pass` turns into `if x == 0:` which then turns into `if x.__equals__(0):`.
       * We keep a trace of all AST transforms. *)
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  type t = {
    kind: kind;
    location: Location.t;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val create : ?base:t -> location:Location.t -> kind -> t

  val is_dunder_method : t -> bool
end

and Expression : sig
  type expression =
    | Await of Await.t
    | BinaryOperator of BinaryOperator.t
    | BooleanOperator of BooleanOperator.t
    | Call of Call.t
    | ComparisonOperator of ComparisonOperator.t
    | Constant of Constant.t
    | Dictionary of Dictionary.t
    | DictionaryComprehension of Dictionary.Entry.KeyValue.t Comprehension.t
    | Generator of t Comprehension.t
    | FormatString of Substring.t list
    | Lambda of Lambda.t
    | List of t list
    | ListComprehension of t Comprehension.t
    | Name of Name.t
    | Set of t list
    | SetComprehension of t Comprehension.t
    | Slice of Slice.t
    | Starred of Starred.t
    | Subscript of Subscript.t
    | Ternary of Ternary.t
    | Tuple of t list
    | UnaryOperator of UnaryOperator.t
    | WalrusOperator of WalrusOperator.t
    | Yield of t option
    | YieldFrom of t

  and t = expression Node.t [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end

module Mapper : sig
  type 'a t

  val map : mapper:'a t -> Expression.t -> 'a

  val map_list : mapper:'a t -> Expression.t list -> 'a list

  val map_option : mapper:'a t -> Expression.t option -> 'a option

  val create
    :  map_await:(mapper:'a t -> location:Location.t -> Await.t -> 'a) ->
    map_binary_operator:(mapper:'a t -> location:Location.t -> BinaryOperator.t -> 'a) ->
    map_boolean_operator:(mapper:'a t -> location:Location.t -> BooleanOperator.t -> 'a) ->
    map_call:(mapper:'a t -> location:Location.t -> Call.t -> 'a) ->
    map_comparison_operator:(mapper:'a t -> location:Location.t -> ComparisonOperator.t -> 'a) ->
    map_constant:(mapper:'a t -> location:Location.t -> Constant.t -> 'a) ->
    map_dictionary:(mapper:'a t -> location:Location.t -> Dictionary.t -> 'a) ->
    map_dictionary_comprehension:
      (mapper:'a t -> location:Location.t -> Dictionary.Entry.KeyValue.t Comprehension.t -> 'a) ->
    map_generator:(mapper:'a t -> location:Location.t -> Expression.t Comprehension.t -> 'a) ->
    map_format_string:(mapper:'a t -> location:Location.t -> Substring.t list -> 'a) ->
    map_lambda:(mapper:'a t -> location:Location.t -> Lambda.t -> 'a) ->
    map_list:(mapper:'a t -> location:Location.t -> Expression.t list -> 'a) ->
    map_list_comprehension:
      (mapper:'a t -> location:Location.t -> Expression.t Comprehension.t -> 'a) ->
    map_name:(mapper:'a t -> location:Location.t -> Name.t -> 'a) ->
    map_set:(mapper:'a t -> location:Location.t -> Expression.t list -> 'a) ->
    map_set_comprehension:(mapper:'a t -> location:Location.t -> Expression.t Comprehension.t -> 'a) ->
    map_slice:(mapper:'a t -> location:Location.t -> Slice.t -> 'a) ->
    map_starred:(mapper:'a t -> location:Location.t -> Starred.t -> 'a) ->
    map_subscript:(mapper:'a t -> location:Location.t -> Subscript.t -> 'a) ->
    map_ternary:(mapper:'a t -> location:Location.t -> Ternary.t -> 'a) ->
    map_tuple:(mapper:'a t -> location:Location.t -> Expression.t list -> 'a) ->
    map_unary_operator:(mapper:'a t -> location:Location.t -> UnaryOperator.t -> 'a) ->
    map_walrus_operator:(mapper:'a t -> location:Location.t -> WalrusOperator.t -> 'a) ->
    map_yield:(mapper:'a t -> location:Location.t -> Expression.t option -> 'a) ->
    map_yield_from:(mapper:'a t -> location:Location.t -> Expression.t -> 'a) ->
    unit ->
    'a t

  val create_default
    :  ?map_await:(mapper:Expression.t t -> location:Location.t -> Await.t -> Expression.t) ->
    ?map_binary_operator:
      (mapper:Expression.t t -> location:Location.t -> BinaryOperator.t -> Expression.t) ->
    ?map_boolean_operator:
      (mapper:Expression.t t -> location:Location.t -> BooleanOperator.t -> Expression.t) ->
    ?map_call:(mapper:Expression.t t -> location:Location.t -> Call.t -> Expression.t) ->
    ?map_comparison_operator:
      (mapper:Expression.t t -> location:Location.t -> ComparisonOperator.t -> Expression.t) ->
    ?map_constant:(mapper:Expression.t t -> location:Location.t -> Constant.t -> Expression.t) ->
    ?map_dictionary:(mapper:Expression.t t -> location:Location.t -> Dictionary.t -> Expression.t) ->
    ?map_dictionary_comprehension:
      (mapper:Expression.t t ->
      location:Location.t ->
      Dictionary.Entry.KeyValue.t Comprehension.t ->
      Expression.t) ->
    ?map_generator:
      (mapper:Expression.t t -> location:Location.t -> Expression.t Comprehension.t -> Expression.t) ->
    ?map_format_string:
      (mapper:Expression.t t -> location:Location.t -> Substring.t list -> Expression.t) ->
    ?map_lambda:(mapper:Expression.t t -> location:Location.t -> Lambda.t -> Expression.t) ->
    ?map_list:(mapper:Expression.t t -> location:Location.t -> Expression.t list -> Expression.t) ->
    ?map_list_comprehension:
      (mapper:Expression.t t -> location:Location.t -> Expression.t Comprehension.t -> Expression.t) ->
    ?map_name:(mapper:Expression.t t -> location:Location.t -> Name.t -> Expression.t) ->
    ?map_set:(mapper:Expression.t t -> location:Location.t -> Expression.t list -> Expression.t) ->
    ?map_set_comprehension:
      (mapper:Expression.t t -> location:Location.t -> Expression.t Comprehension.t -> Expression.t) ->
    ?map_slice:(mapper:Expression.t t -> location:Location.t -> Slice.t -> Expression.t) ->
    ?map_starred:(mapper:Expression.t t -> location:Location.t -> Starred.t -> Expression.t) ->
    ?map_subscript:(mapper:Expression.t t -> location:Location.t -> Subscript.t -> Expression.t) ->
    ?map_ternary:(mapper:Expression.t t -> location:Location.t -> Ternary.t -> Expression.t) ->
    ?map_tuple:(mapper:Expression.t t -> location:Location.t -> Expression.t list -> Expression.t) ->
    ?map_unary_operator:
      (mapper:Expression.t t -> location:Location.t -> UnaryOperator.t -> Expression.t) ->
    ?map_walrus_operator:
      (mapper:Expression.t t -> location:Location.t -> WalrusOperator.t -> Expression.t) ->
    ?map_yield:(mapper:Expression.t t -> location:Location.t -> Expression.t option -> Expression.t) ->
    ?map_yield_from:(mapper:Expression.t t -> location:Location.t -> Expression.t -> Expression.t) ->
    unit ->
    Expression.t t

  val create_transformer
    :  ?map_await:(mapper:Expression.t t -> Await.t -> Await.t) ->
    ?map_binary_operator:(mapper:Expression.t t -> BinaryOperator.t -> BinaryOperator.t) ->
    ?map_boolean_operator:(mapper:Expression.t t -> BooleanOperator.t -> BooleanOperator.t) ->
    ?map_call:(mapper:Expression.t t -> Call.t -> Call.t) ->
    ?map_comparison_operator:(mapper:Expression.t t -> ComparisonOperator.t -> ComparisonOperator.t) ->
    ?map_constant:(mapper:Expression.t t -> Constant.t -> Constant.t) ->
    ?map_dictionary:(mapper:Expression.t t -> Dictionary.t -> Dictionary.t) ->
    ?map_dictionary_comprehension:
      (mapper:Expression.t t ->
      Dictionary.Entry.KeyValue.t Comprehension.t ->
      Dictionary.Entry.KeyValue.t Comprehension.t) ->
    ?map_generator:
      (mapper:Expression.t t -> Expression.t Comprehension.t -> Expression.t Comprehension.t) ->
    ?map_format_string:(mapper:Expression.t t -> Substring.t list -> Substring.t list) ->
    ?map_lambda:(mapper:Expression.t t -> Lambda.t -> Lambda.t) ->
    ?map_list:(mapper:Expression.t t -> Expression.t list -> Expression.t list) ->
    ?map_list_comprehension:
      (mapper:Expression.t t -> Expression.t Comprehension.t -> Expression.t Comprehension.t) ->
    ?map_name:(mapper:Expression.t t -> Name.t -> Name.t) ->
    ?map_set:(mapper:Expression.t t -> Expression.t list -> Expression.t list) ->
    ?map_set_comprehension:
      (mapper:Expression.t t -> Expression.t Comprehension.t -> Expression.t Comprehension.t) ->
    ?map_slice:(mapper:Expression.t t -> Slice.t -> Slice.t) ->
    ?map_starred:(mapper:Expression.t t -> Starred.t -> Starred.t) ->
    ?map_subscript:(mapper:Expression.t t -> Subscript.t -> Subscript.t) ->
    ?map_ternary:(mapper:Expression.t t -> Ternary.t -> Ternary.t) ->
    ?map_tuple:(mapper:Expression.t t -> Expression.t list -> Expression.t list) ->
    ?map_unary_operator:(mapper:Expression.t t -> UnaryOperator.t -> UnaryOperator.t) ->
    ?map_walrus_operator:(mapper:Expression.t t -> WalrusOperator.t -> WalrusOperator.t) ->
    ?map_yield:(mapper:Expression.t t -> Expression.t option -> Expression.t option) ->
    ?map_yield_from:(mapper:Expression.t t -> Expression.t -> Expression.t) ->
    ?map_location:(Location.t -> Location.t) ->
    unit ->
    Expression.t t
end

module Folder : sig
  type 'a t

  val fold : folder:'a t -> state:'a -> Expression.t -> 'a

  val fold_list : folder:'a t -> state:'a -> Expression.t list -> 'a

  val fold_option : folder:'a t -> state:'a -> Expression.t option -> 'a

  val create
    :  ?fold_await:(folder:'a t -> state:'a -> location:Location.t -> Await.t -> 'a) ->
    ?fold_binary_operator:(folder:'a t -> state:'a -> location:Location.t -> BinaryOperator.t -> 'a) ->
    ?fold_boolean_operator:
      (folder:'a t -> state:'a -> location:Location.t -> BooleanOperator.t -> 'a) ->
    ?fold_call:(folder:'a t -> state:'a -> location:Location.t -> Call.t -> 'a) ->
    ?fold_comparison_operator:
      (folder:'a t -> state:'a -> location:Location.t -> ComparisonOperator.t -> 'a) ->
    ?fold_constant:(folder:'a t -> state:'a -> location:Location.t -> Constant.t -> 'a) ->
    ?fold_dictionary:(folder:'a t -> state:'a -> location:Location.t -> Dictionary.t -> 'a) ->
    ?fold_dictionary_comprehension:
      (folder:'a t ->
      state:'a ->
      location:Location.t ->
      Dictionary.Entry.KeyValue.t Comprehension.t ->
      'a) ->
    ?fold_generator:
      (folder:'a t -> state:'a -> location:Location.t -> Expression.t Comprehension.t -> 'a) ->
    ?fold_format_string:(folder:'a t -> state:'a -> location:Location.t -> Substring.t list -> 'a) ->
    ?fold_lambda:(folder:'a t -> state:'a -> location:Location.t -> Lambda.t -> 'a) ->
    ?fold_list:(folder:'a t -> state:'a -> location:Location.t -> Expression.t list -> 'a) ->
    ?fold_list_comprehension:
      (folder:'a t -> state:'a -> location:Location.t -> Expression.t Comprehension.t -> 'a) ->
    ?fold_name:(folder:'a t -> state:'a -> location:Location.t -> Name.t -> 'a) ->
    ?fold_set:(folder:'a t -> state:'a -> location:Location.t -> Expression.t list -> 'a) ->
    ?fold_set_comprehension:
      (folder:'a t -> state:'a -> location:Location.t -> Expression.t Comprehension.t -> 'a) ->
    ?fold_slice:(folder:'a t -> state:'a -> location:Location.t -> Slice.t -> 'a) ->
    ?fold_starred:(folder:'a t -> state:'a -> location:Location.t -> Starred.t -> 'a) ->
    ?fold_subscript:(folder:'a t -> state:'a -> location:Location.t -> Subscript.t -> 'a) ->
    ?fold_ternary:(folder:'a t -> state:'a -> location:Location.t -> Ternary.t -> 'a) ->
    ?fold_tuple:(folder:'a t -> state:'a -> location:Location.t -> Expression.t list -> 'a) ->
    ?fold_unary_operator:(folder:'a t -> state:'a -> location:Location.t -> UnaryOperator.t -> 'a) ->
    ?fold_walrus_operator:(folder:'a t -> state:'a -> location:Location.t -> WalrusOperator.t -> 'a) ->
    ?fold_yield:(folder:'a t -> state:'a -> location:Location.t -> Expression.t option -> 'a) ->
    ?fold_yield_from:(folder:'a t -> state:'a -> location:Location.t -> Expression.t -> 'a) ->
    unit ->
    'a t

  val create_with_uniform_location_fold
    :  ?fold_await:(folder:'a t -> state:'a -> Await.t -> 'a) ->
    ?fold_binary_operator:(folder:'a t -> state:'a -> BinaryOperator.t -> 'a) ->
    ?fold_boolean_operator:(folder:'a t -> state:'a -> BooleanOperator.t -> 'a) ->
    ?fold_call:(folder:'a t -> state:'a -> Call.t -> 'a) ->
    ?fold_comparison_operator:(folder:'a t -> state:'a -> ComparisonOperator.t -> 'a) ->
    ?fold_constant:(folder:'a t -> state:'a -> Constant.t -> 'a) ->
    ?fold_dictionary:(folder:'a t -> state:'a -> Dictionary.t -> 'a) ->
    ?fold_dictionary_comprehension:
      (folder:'a t -> state:'a -> Dictionary.Entry.KeyValue.t Comprehension.t -> 'a) ->
    ?fold_generator:(folder:'a t -> state:'a -> Expression.t Comprehension.t -> 'a) ->
    ?fold_format_string:(folder:'a t -> state:'a -> Substring.t list -> 'a) ->
    ?fold_lambda:(folder:'a t -> state:'a -> Lambda.t -> 'a) ->
    ?fold_list:(folder:'a t -> state:'a -> Expression.t list -> 'a) ->
    ?fold_list_comprehension:(folder:'a t -> state:'a -> Expression.t Comprehension.t -> 'a) ->
    ?fold_name:(folder:'a t -> state:'a -> Name.t -> 'a) ->
    ?fold_set:(folder:'a t -> state:'a -> Expression.t list -> 'a) ->
    ?fold_set_comprehension:(folder:'a t -> state:'a -> Expression.t Comprehension.t -> 'a) ->
    ?fold_slice:(folder:'a t -> state:'a -> Slice.t -> 'a) ->
    ?fold_starred:(folder:'a t -> state:'a -> Starred.t -> 'a) ->
    ?fold_subscript:(folder:'a t -> state:'a -> Subscript.t -> 'a) ->
    ?fold_ternary:(folder:'a t -> state:'a -> Ternary.t -> 'a) ->
    ?fold_tuple:(folder:'a t -> state:'a -> Expression.t list -> 'a) ->
    ?fold_unary_operator:(folder:'a t -> state:'a -> UnaryOperator.t -> 'a) ->
    ?fold_walrus_operator:(folder:'a t -> state:'a -> WalrusOperator.t -> 'a) ->
    ?fold_yield:(folder:'a t -> state:'a -> Expression.t option -> 'a) ->
    ?fold_yield_from:(folder:'a t -> state:'a -> Expression.t -> 'a) ->
    ?fold_location:(state:'a -> Location.t -> 'a) ->
    unit ->
    'a t
end

type t = Expression.t [@@deriving equal, compare, sexp, show, hash, to_yojson]

type expression = Expression.expression [@@deriving equal, compare, sexp, show, hash, to_yojson]

val location_insensitive_compare : t -> t -> int

val negate : t -> t

val normalize : t -> t

val is_false : t -> bool

val is_none : t -> bool

val create_name_from_identifiers
  :  location:Location.t ->
  create_origin:(string list -> Origin.t option) ->
  Identifier.t list ->
  Name.t

val create_name
  :  location:Location.t ->
  create_origin:(string list -> Origin.t option) ->
  string ->
  Name.t

val create_name_from_reference
  :  location:Location.t ->
  create_origin:(string list -> Origin.t option) ->
  Reference.t ->
  Name.t

val from_reference
  :  location:Location.t ->
  create_origin:(string list -> Origin.t option) ->
  Reference.t ->
  t

val name_to_identifiers : Name.t -> Identifier.t list option

val name_to_reference : Name.t -> Reference.t option

val name_to_reference_exn : Name.t -> Reference.t

val is_simple_name : Name.t -> bool

val get_identifier_base : t -> Identifier.t option

val has_identifier_base : t -> bool

val name_is : name:string -> t -> bool

val sanitized : t -> t

val delocalize : create_origin:(expression:t -> string list -> Origin.t option) -> t -> t

val delocalize_qualified : t -> t

val is_self_call : callee:t -> bool

val is_cls_call : callee:t -> bool

val exists_in_list : ?match_prefix:bool -> expression_list:t list -> string -> bool

val arguments_location : Call.t -> Location.t

val subscript
  :  string ->
  expression Node.t list ->
  location:Location.t ->
  create_origin_for_base:(string list -> Origin.t option) ->
  origin:Origin.t option ->
  expression

val subscript_for_annotation : string -> expression Node.t list -> location:Location.t -> expression

val is_dunder_attribute : string -> bool

val pp_expression_list : Format.formatter -> t list -> unit

val pp_expression_argument_list : Format.formatter -> Call.Argument.t list -> unit

val pp_expression_parameter_list : Format.formatter -> Parameter.t list -> unit

val pp_type_param_list : Format.formatter -> TypeParam.t list -> unit

val inverse_operator : string -> string option

val is_operator : string -> bool

val operator_name_to_symbol : string -> string option

val origin : t -> Origin.t option

val remove_origins : t -> t
