Require Import String.
Require Import OrderedType OrderedTypeEx.

(* Wrapper around String_as_OT *)
Module String_facts := OrderedTypeFacts(String_as_OT).

Definition lt (s1 s2: string) : Prop := String_as_OT.lt s1 s2.

Definition ltb (s1 s2: string) : bool :=
  match String_facts.lt_dec s1 s2 with 
  | left _ => true 
  | right _ => false
end.

Lemma ltb_lt: forall s1 s2, lt s1 s2 <-> ltb s1 s2 = true.
Proof.
unfold lt, ltb.
intros s1 s2; split; intro h.
- destruct String_facts.lt_dec as [ hdec | hdec ]; [ reflexivity | ].
  now elim hdec.
- destruct String_facts.lt_dec as [ hdec | hdec ]; [ | discriminate ].
  assumption.
Qed.

Lemma ltb_nlt: forall s1 s2, (~ lt s1 s2) <-> ltb s1 s2 = false.
Proof.
intros s1 s2; split; intro h.
- destruct (ltb_lt s1 s2) as [h1 h2].
  destruct (ltb s1 s2); [ | reflexivity ].
  now elim (h (h2 refl_equal)).
- destruct (ltb_lt s1 s2) as [h1 h2].
  intro hlt.
  apply h1 in hlt.
  rewrite h in hlt.
  discriminate.
Qed.

Definition lt_trans := String_as_OT.lt_trans.

Lemma lt_ge: forall x y:string, ltb x y = false -> ltb y x = true \/ x = y.
Proof.
intros x y hltb.
destruct (String_facts.lt_total x y) as [h | [h | h]].
- apply ltb_lt in h.
  now rewrite h in hltb; discriminate.
- now subst; right.
- apply ltb_lt in h.
  now left.
Qed.

Definition lt_not_eq := String_as_OT.lt_not_eq.
