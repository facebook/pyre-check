Require Import List Sorted.
Import ListNotations.

(** Facts about HdRel and Forall *)
Section HdRelFacts.
Variable A: Type.
Variable R: A -> A -> Prop.
Hypothesis Rtrans : forall x y z, R x y -> R y z -> R x z.

Lemma HdRel_Forall: forall l x,
  Sorted R l ->
  HdRel R x l -> Forall (R x) l.
Proof.
induction l as [ | hd tl hi]; intros x hs h; simpl in *; [ now constructor | ].
apply HdRel_inv in h.
constructor; [ assumption | ].
apply Sorted_inv in hs as [ h0 h1].
apply hi; [assumption |].
destruct tl; [ now constructor | constructor].
apply HdRel_inv in h1.
now apply (Rtrans x hd a).
Qed.
End HdRelFacts.
