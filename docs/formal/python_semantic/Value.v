Require Import String Bool ZArith List.
Require Import Pyre.Prelude.

(** * Values *)
Inductive t : Set :=
  | Boolean: bool -> t
  | Integer: Z -> t 
  | String: string -> t 
  | Sequence: tlist -> t 
with tlist  : Set :=
  | Nil: tlist
  | Cons: t -> tlist -> tlist
.

Hint Constructors t tlist : Pyre.

Scheme value_ind' := Induction for t Sort Prop
  with value_list_ind' := Induction for tlist Sort Prop.

Combined Scheme value_ind from value_ind', value_list_ind'.

(** Boolean equality for values and list of values *)
Fixpoint eqb (l r: t) { struct l } : bool :=
    match l, r with 
    | Boolean b0, Boolean b1 => Bool.eqb b0 b1
    | Integer z0, Integer z1 => Z.eqb z0 z1
    | String s0, String s1 => String.eqb s0 s1
    | Sequence l1, Sequence l2 => eqb_list l1 l2
    | _ ,_ => false
    end
with eqb_list (l1 l2: tlist) : bool :=
    match l1, l2 with
    | Nil, Nil => true
    | (Cons hd1 tl1), (Cons  hd2 tl2) =>
            andb (eqb hd1 hd2) (eqb_list tl1 tl2)
    | _, _ => false
    end.

Lemma eqb_true_ : (forall v0 v1, eqb v0 v1 = true -> v0 = v1)
  /\ (forall l0 l1, eqb_list l0 l1 = true -> l0 = l1).
Proof.
apply value_ind.
- intros b [] h; simpl in *; try discriminate.
  now apply Bool.eqb_prop in h; subst.
- intros z []  h; simpl in *; try discriminate.
  now apply Z.eqb_eq in h; subst.
- intros str [] h; simpl in *; try discriminate.
  now apply String.eqb_eq in h; subst.
- intros l0 hi [ | | | l1 ] h; simpl in *; try discriminate.
   now apply hi in h; subst.
- now intros [ ]; try discriminate.
- intros v hiv l0 hi [ | hd tl ] h; simpl in *; try discriminate.
  apply andb_prop in h as [h0 h1].
  apply hiv in h0 as ->.
  now apply hi in h1 as ->.
Qed.

Lemma eqb_prop: forall v0 v1, eqb v0 v1 = true -> v0 = v1.
Proof.
    now apply eqb_true_.
Qed.

Lemma eqb_list_prop:  forall l0 l1, eqb_list l0 l1 = true -> l0 = l1.
Proof.
    now apply eqb_true_.
Qed.

Require Import List.

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

Lemma to_list_cancel: forall l, from_list (to_list l) = l.
Proof.
induction l as [ | hd tl hi]; simpl in *; [ now idtac | ].
now rewrite hi.
Qed.

Lemma from_list_cancel: forall l, to_list (from_list l) = l.
Proof.
induction l as [ | hd tl hi]; simpl in *; [ now idtac | ].
now rewrite hi.
Qed.

Coercion to_list: tlist >-> list.
End ListHelpers.
