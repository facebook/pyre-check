Require Import String.

(** Lvalues
    Currently we only support simple variables.
*)
Inductive t : Set :=
  | Id : string -> t
.
