(** Type syntax:

 Simple types with a Unit type, the functional arrow and a reference type
 to distinguish pure terms from terms with references.

*)

Inductive Ty: Set :=
  | Unit : Ty
  | Arr  : Ty -> Ty -> Ty
  | Ref : Ty -> Ty
.

Notation "A ⇒  B" := (Arr A B) (at level 17, right associativity).
