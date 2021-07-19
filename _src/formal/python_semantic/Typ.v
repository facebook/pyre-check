Require Import List ZArith String.

(** Types *)
Inductive t: Set :=
 | Integer: t
 | Boolean: t
 | String: t
 | List: t -> t
 | Tuple: tlist -> t
 | None: t
with tlist: Set :=
 | Nil: tlist
 | Cons: t -> tlist -> tlist
.

Hint Constructors t tlist : core.

Scheme typ_ind' := Induction for t Sort Prop
  with typ_list_ind' := Induction for tlist Sort Prop.

Combined Scheme typ_ind from typ_ind', typ_list_ind'.

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
