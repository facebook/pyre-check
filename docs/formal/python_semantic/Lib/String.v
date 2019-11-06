Require Import Arith Ascii String OrderedTypeEx.

(* In this module we define a <<less than>> boolean relation
   on strings using a lexicographic ordering *)
Fixpoint ltb (s0 s1: string) : bool :=
  match (s0, s1) with
  | (EmptyString, EmptyString) => false
  | (EmptyString, String _ _) => true
  | (String _ _, EmptyString) => false
  | (String u us, String v vs) =>
      if Nat.eqb (nat_of_ascii u) (nat_of_ascii v)
      then ltb us vs
      else Nat.ltb (nat_of_ascii u) (nat_of_ascii v)
end.

(* <<nat_of_ascii>> is injective *)
Definition nat_of_ascii_inverse := String_as_OT.nat_of_ascii_inverse.

Lemma ltb_tail_unique a s1 s2 :
  ltb (String a s1) (String a s2) = true -> ltb s1 s2 = true.
Proof.
now simpl; rewrite Nat.eqb_refl.
Qed.

Lemma ltb_irrefl: forall s, ltb s s = false.
Proof.
induction s as [ | hd tl hi]; simpl in *; [reflexivity | ].
now rewrite Nat.eqb_refl.
Qed.

Definition leb s1 s2 := orb (ltb s1 s2) (s1 =? s2)%string.

Remark helper: forall n m, n <> m -> n <= m -> n < m.
Proof.
intros n m h hle; revert h; induction hle.
- intro h; now elim h.
- auto with arith.
Qed.

Lemma ltb_antisym: forall s1 s2, ltb s1 s2 = negb (leb s2 s1).  
Proof.
unfold leb.
induction s1 as [ | hd1 tl1 hi]; intros [ | hd2 tl2]; simpl in *;
    try reflexivity.
case_eq (Nat.eqb (nat_of_ascii hd1) (nat_of_ascii hd2)); intro h.
- rewrite Nat.eqb_sym, h.
  rewrite (hi tl2); f_equal.
  apply Nat.eqb_eq in h.
  apply nat_of_ascii_inverse in h.
  now rewrite h, Ascii.eqb_refl.
- rewrite Nat.eqb_sym, h.
  assert (hhd: (hd2 =? hd1)%char = false).
  + apply Nat.eqb_neq in h.
    now apply Ascii.eqb_neq; intro; subst; apply h.
  + rewrite hhd, Bool.orb_false_r, Nat.ltb_antisym; f_equal.
    apply Nat.eqb_neq in h.
    case_eq (nat_of_ascii hd2 <=? nat_of_ascii hd1); intro h1.
    * rewrite Nat.leb_antisym in h1.
      apply Bool.negb_true_iff in h1.
      apply Nat.ltb_ge in h1.
      symmetry.
      apply Nat.ltb_lt.
      apply helper; [ | assumption ].
      intro hx; now apply h; subst.
    * symmetry.
      apply Nat.ltb_ge.
      rewrite Nat.leb_antisym in h1.
      apply Bool.negb_false_iff in h1.
      apply Nat.ltb_lt in h1.
      now auto with arith.
Qed.

Lemma ltb_trans : forall x y z : string,
  ltb x y = true -> ltb y z = true -> ltb x z = true.
Proof.
induction x as [ | u us hi]; intros [ | v vs] [ | w ws];
    simpl; try discriminate; intros hxy hyz; [ reflexivity | ].
case_eq (Nat.eqb (nat_of_ascii u) (nat_of_ascii w)); intro huw.
- case_eq (Nat.eqb (nat_of_ascii u) (nat_of_ascii v)); intro huv;
    rewrite huv in hxy.
  + case_eq (Nat.eqb (nat_of_ascii v) (nat_of_ascii w)); intro hvw;
    rewrite hvw in hyz.
    * now apply (hi vs ws).
    * apply Nat.eqb_eq in huw; rewrite huw in huv.
      apply Nat.eqb_eq in huv; rewrite huv in hvw.
      rewrite Nat.eqb_refl in hvw; discriminate hvw.
  + apply Nat.eqb_eq in huw; rewrite huw in hxy, huv.
    rewrite Nat.eqb_sym in hyz.
    rewrite huv in hyz.
    apply Nat.eqb_neq in huv.
    apply Nat.ltb_lt in hxy.
    apply Nat.ltb_lt in hyz.
    elim (lt_irrefl (nat_of_ascii w)).
    eapply lt_trans; [ now apply hxy | now apply hyz ].
- case_eq (Nat.eqb (nat_of_ascii u) (nat_of_ascii v)); intro huv;
    rewrite huv in hxy.
  + case_eq (Nat.eqb (nat_of_ascii v) (nat_of_ascii w)); intro hvw;
    rewrite hvw in hyz.
    * apply Nat.eqb_eq in huv; rewrite huv in huw.
      apply Nat.eqb_eq in hvw; rewrite hvw in huw.
      rewrite Nat.eqb_refl in huw; discriminate huw.
    * now apply Nat.eqb_eq in huv; rewrite huv.
  + case_eq (Nat.eqb (nat_of_ascii v) (nat_of_ascii w)); intro hvw;
      rewrite hvw in hyz.
    * now apply Nat.eqb_eq in hvw; rewrite hvw in hxy.
    * apply Nat.ltb_lt.
      apply Nat.ltb_lt in hxy.
      apply Nat.ltb_lt in hyz.
      eapply lt_trans; [ now apply hxy | now apply hyz ].
Qed.

Lemma lt_not_eq : forall x y : string,
  ltb x y = true -> (x =? y)%string = false.
Proof.
induction x as [ | u us hi]; intros [ | v vs]; simpl in *; try discriminate;
    intro hlt; [reflexivity | ].
case_eq (Nat.eqb (nat_of_ascii u) (nat_of_ascii v)); intro huv.
- apply Nat.eqb_eq in huv.
  apply nat_of_ascii_inverse in huv; subst.
  rewrite Ascii.eqb_refl.
  rewrite Nat.eqb_refl in hlt.
  now apply hi.
- apply Nat.eqb_neq in huv. 
  case_eq (Ascii.eqb u v); intro huv2.
  + apply Ascii.eqb_eq in huv2; subst.
    now elim huv.
  + reflexivity.
Qed.

Lemma lt_ge: forall x y:string, ltb x y = false -> ltb y x = true \/ x = y.
Proof.
induction x as [ | u us hi]; intros [ | v vs] hne; simpl in *.
- now right.
- discriminate.
- now left.
- rewrite Nat.eqb_sym.
  case_eq (Nat.eqb (nat_of_ascii u) (nat_of_ascii v)); intro huv;
    rewrite huv in hne.
  + apply hi in hne as [ h | h]; [left; assumption|right ].
    now apply Nat.eqb_eq in huv; apply nat_of_ascii_inverse in huv; subst.
  + apply Nat.ltb_ge in hne.
    apply Nat.eqb_neq in huv.
    now left; apply Nat.ltb_lt; intuition.
Qed.
