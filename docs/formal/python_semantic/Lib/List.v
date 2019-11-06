Require Import List Sorted.
Import ListNotations.

(** This file is an extension of the <<List>> library by adding some
    boolean predicates to reflect existing propositional ones. *)

(** * Boolean version of Sorted *)
Section BooleanSorted.

Variable A: Type.
(* Generic relation. Will usually be instantiated with an order like <<lt>>.
   If the following file, I'll refer to <<R x y>> as "x smaller than y". *)
Variable R : A -> A -> bool.

(* <<HdRel a l>> means that either <<l>> is empty, or <<a>> is "smaller" than
   the head of <<l>> *)
Fixpoint HdRelb (a: A) (l: list A) : bool :=
  match l with
  | nil => true 
  | b :: _ => R a b
end.

(* A list is (weakly) sorted if any element is "smaller" than the next.
   A list is strongly sorted if any element is "smaller" than any of the 
   following ones in the list. *)
Fixpoint Sortedb (l : list A) : bool :=
  match l with
  | nil => true
  | hd :: tl => andb (Sortedb tl) (HdRelb hd tl)
end.

Lemma reflect_HdRel: forall l a,
  @HdRel A (fun x y => R x y = true) a l <-> HdRelb a l = true.
Proof.
induction l as [ | hd tl hi]; intros a; split; intro h; simpl in *.
- reflexivity.
- now constructor.
- now apply HdRel_inv in h.
- now constructor.
Qed.

Lemma reflect_Sorted: forall l,
  @Sorted A (fun x y => R x y = true) l <-> Sortedb l = true.
Proof.
induction l as [ | hd tl hi]; split; intro h; simpl in *.
- reflexivity.
- now constructor.
- apply Sorted_inv in h as [h0 h1].
  apply andb_true_intro.
  split; [ now apply hi | ].
  now apply reflect_HdRel.
- apply andb_prop in h as [h0 h1].
  constructor; [ now apply hi | ].
  now apply reflect_HdRel.
Qed.

Lemma reflect_Sorted_false: forall l,
  (~ @Sorted A (fun x y => R x y = true) l) <-> Sortedb l = false.
Proof.
intros l; split; intro h.
- case_eq (Sortedb l); intro hs; [| reflexivity].
  apply reflect_Sorted in hs.
  now elim hs.
- intro hs.
  apply reflect_Sorted in hs.
  rewrite hs in h; discriminate.
Qed.

End BooleanSorted.

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

(* In this section we define a boolean equality on lists (as long as the
   underlying type has one), by pair-wise comparing the elements of the lists.
*)
Section ListEq.
Variable A: Type.
Variable Aeqb : A -> A -> bool.
(* We need some hypothesis on the underlying equaliy, like reflexivity, ... *)
Hypothesis Aeqb_eq: forall x y, Aeqb x y = true -> x = y.
Hypothesis Aeqb_neq: forall x y, Aeqb x y = false -> x <> y.
Hypothesis Aeqb_refl: forall x, Aeqb x x = true.
Hypothesis Aeqb_trans: forall x y z,
  Aeqb x y = true -> Aeqb y z = true -> Aeqb x z = true.

Fixpoint eqb (l1 l2: list A) : bool :=
  match (l1, l2) with
  | (nil, nil) => true
  | (hd1 :: tl1, hd2 :: tl2) => (Aeqb hd1 hd2 && eqb tl1 tl2)%bool
  | (_, _) => false
end.

Lemma eqb_eq: forall l1 l2, eqb l1 l2 = true -> l1 = l2.
Proof.
induction l1 as [ | hd1 tl1 hi]; intros [ | hd2 tl2] heq; simpl in *;
    try discriminate; [reflexivity | ].
apply andb_prop in heq as [h1 h2].
apply Aeqb_eq in h1.
apply hi in h2.
now subst.
Qed.

Lemma eqb_neq: forall l1 l2, eqb l1 l2 = false -> l1 <> l2.
Proof.
induction l1 as [ | hd1 tl1 hi]; intros [ | hd2 tl2] heq h; simpl in *;
    try discriminate.
injection h; intros hhd htl; clear h; subst.
apply Bool.andb_false_iff in heq as [hb | hb].
- rewrite Aeqb_refl in hb; discriminate.
- apply hi in hb.
  now elim hb.
Qed.

Lemma eqb_refl: forall l, eqb l l = true.
Proof.
induction l as [ | hd tl hi]; simpl in *; [ reflexivity | ].
now rewrite Aeqb_refl, hi.
Qed.

Lemma eqb_trans: forall x y z,
  eqb x y = true -> eqb y z = true -> eqb x z = true.
Proof.
induction x as [ | x xs hi]; intros [ | y ys] [ | z zs] hxy hyz;
    simpl in *; try discriminate; [reflexivity | ].
apply andb_prop in hxy as [hxy hxys].
apply andb_prop in hyz as [hyz hyzs].
apply andb_true_intro; split.
- now apply (Aeqb_trans x y z).
- now apply (hi ys zs).
Qed.

End ListEq.

Require Eqdep_dec.

Section ListProofUnicity.
Import Eqdep_dec.

Variable A: Type.
Variable R: A -> A -> bool.

(* The unicity of proofs comes in handy when manipulating dependently typed
   structures which embeds some proofs. This lemma means that any two
   proofs of the fact that a list is sorted or not are equal. Since
   we can write one, this means there is a single proof of that fact,
   and so we can replace any proof by this witness, and avoid difficult
   dependent rewrites *)
Lemma sorted_proof_unicity: forall l b (P Q: @Sortedb _ R l = b), P = Q.
Proof.
intros l b P Q.
apply eq_proofs_unicity.
now intros [] []; intuition.
Qed.

End ListProofUnicity.
