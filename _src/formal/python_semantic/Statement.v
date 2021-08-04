Require Import List String.
Require Pyre.Expression.
Require Pyre.Lvalue.
Require Pyre.Typ.

(** * Statements

  More will be added later on. See
  https://github.com/facebook/pyre-check/blob/main/ast/statement.mli#L331
*)
Inductive t : Set :=
  | Assign: forall (target: Lvalue.t)
                   (annotation: option Typ.t)
                   (value: Expression.t),
                   t
  | Call: forall (target: option (Lvalue.t * Typ.t))
                 (callee: Expression.t)
                 (arguments: list ((option string) * Expression.t)),
                 t
  | Expression: Expression.t -> t
  | Delete: Expression.t -> t
  | For: forall (target: Lvalue.t) (iterator: Expression.t) (body orelse: t), t
  | Global: list string -> t
  | If: forall (test: Expression.t) (body orelse: t), t
  | Nonlocal: list string -> t
  | Pass: t
  | While: forall (test: Expression.t) (body orelse: t), t
  | Seq : forall (st next : t), t
  | Return: forall (ret: Expression.t), t
.

Fixpoint to_seq (statements: list t) : t :=
  match statements with
  | nil => Pass
  | hd :: nil => hd
  | hd :: tl => Seq hd (to_seq tl)
  end.
