Require Import ZArith List String.

(** * Operators *)
(** Boolean operators *)
Inductive boolean_operator :=
  | And
  | Or
.

(** Comparison operators
    Currently <<Is>> and <<IsNot>> are not supported
*)
Inductive comparison_operator :=
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
.

(** Unary operators *)
Inductive unary_operator :=
  | Invert
  | Negative
  | Not
  | Positive.

(** * Expressions
    More will be added later. See
    https://github.com/facebook/pyre-check/blob/main/ast/expression.mli#L216
*)
Inductive t: Set :=
  | BooleanOperator: forall (left: t) (op: boolean_operator) (right: t), t
  | ComparisonOperator: forall (left: t) (op: comparison_operator) (right: t), t
  | False_: t
  | Integer: Z -> t
  | List: tlist -> t
  | Id: string -> t
  | None: t
  | String: string -> t
  | Ternary: forall (target: t) (test: t) (alternative: t), t
  | True_: t
  | Tuple: tlist -> t
  | UnaryOperator: unary_operator -> t -> t
with tlist: Set :=
  | Nil: tlist
  | Cons: t -> tlist -> tlist
.

(* begin hide *)
Hint Constructors t tlist : core.

Scheme expr_ind' := Induction for t Sort Prop
    with expr_list_ind' := Induction for tlist Sort Prop.

Combined Scheme expression_ind from expr_ind', expr_list_ind'.

Module ListHelpers.
Fixpoint to_list (l: tlist) : list t :=
    match l with
    | Nil => nil
    | Cons hd tl => hd :: to_list tl
    end.

Fixpoint from_list (l: list t) : tlist :=
    match l with
    | nil => Nil
    | hd :: tl => Cons hd (from_list tl)
    end.

Lemma to_list_cancel: forall (l: tlist), from_list (to_list l) = l.
Proof.
induction l as [ | hd tl hi]; simpl in *; [ now idtac | ].
now rewrite hi.
Qed.

Lemma from_list_cancel: forall (l: list t), to_list (from_list l) = l.
Proof.
induction l as [ | hd tl hi]; simpl in *; [ now idtac | ].
now rewrite hi.
Qed.

Coercion to_list: tlist >-> list.
End ListHelpers.
(* end hide *)
