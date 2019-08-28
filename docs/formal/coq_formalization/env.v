Require Import List Arith Omega.
Require Import STLC_Ref.term STLC_Ref.ty.

(** * Typing Environment for annotated terms .
  As for Terms, we define contexts of "Annotated" terms, with the very 
  safe function and tools as for the usual one.*)

(** Very naive definition of environment : list of term 

 be carefull, the usual written env Γ(x:A) is encoded in 
  A::Γ
**)

Definition Env := list Ty.


(** Some manipulation functions (mostly from Bruno Barras' PTS contrib): 
 - how to find an item in the environment
 - how to truncate an environment 
 - how to insert a new element (with correct de Bruijn update)
 - how to substitute something in the environment
 *)
Set Implicit Arguments.

Inductive item (A:Type) (x:A): list A ->nat->Prop :=
| item_hd: forall Γ :list A, (item x (cons x Γ) O)
| item_tl: forall (Γ:list A)(n:nat)(y:A), item x Γ n -> item x (cons y Γ) (S n).

Hint Constructors item.

(** In the list [Γ], the [n]th item is syntacticaly [x]. *)
Notation " x ↓ n ∈ Γ " := (item x Γ n) (at level 80, no associativity).

Lemma fun_item: forall T (A B:T)(Γ:list T)(n:nat), 
  A ↓ n ∈ Γ -> B ↓ n ∈ Γ -> A = B.
Proof.
intros T A B  Γ n;revert T A B Γ. 
induction n as [ | n hi]; intros T A B Γ h1 h2.
- inversion h1; subst; clear h1.
  now inversion h2; subst; clear h2.
- inversion h1; subst; clear h1.
  inversion h2; subst; clear h2.
  now apply (hi _ _ _ _ H1 H0).
Qed.

Inductive trunc (A:Type) : nat->list A ->list A->Prop :=
  | trunc_O: forall (Γ:list A) , (trunc O Γ Γ)
  | trunc_S: forall (k:nat)(Γ Γ':list A)(x:A), trunc k Γ Γ' 
                -> trunc (S k) (cons x Γ) Γ'.

Hint Constructors trunc.

Lemma item_trunc: forall (T:Type) (n:nat) (Γ:list T) (t:T), 
  t ↓ n ∈ Γ -> exists Γ' , trunc (S n) Γ Γ'.
Proof.
intros T; induction n as [ | n hi]; intros Γ t hin.
- inversion hin; subst; clear hin.
  exists Γ0.
  now repeat constructor.
- inversion hin; subst; clear hin.
  destruct (hi Γ0 t H1) as [Γ' hΓ].
  exists Γ'.
  now repeat constructor.
Qed.

(** This type describe how do we add an element in an environment: no type
checking is done, this is just the mecanic way to do it. *)
Inductive ins_in_env (Γ:Env ) (d1:Ty): nat->Env -> Env  ->Prop :=
  | ins_O: ins_in_env Γ d1 O Γ (d1::Γ)
  | ins_S: forall (n:nat)(Δ Δ':Env )(d:Ty), (ins_in_env Γ d1 n Δ Δ')
    -> ins_in_env Γ d1 (S n) (d::Δ) ( d::Δ' ).

Hint Constructors ins_in_env.


(** Some lemmas about inserting a new element. They explain how
 terms in the environment are lifted according to their original position
 and the position of insertion. *)
Lemma ins_item_ge: forall (d':Ty) (n:nat) (Γ Δ Δ':Env), 
  ins_in_env Γ d' n Δ Δ' -> 
  forall (v:nat), n<=v -> 
 forall (d:Ty),  d ↓ v ∈ Δ  -> d ↓ (S v) ∈ Δ'.
Proof.
intros d'; induction n as [ | n hi]; intros Γ Δ Δ' hins v hle d hd.
- inversion hd; subst; clear hd.
  + inversion hins; subst; clear hins.
    now repeat constructor.
  + inversion hins; subst; clear hins.
    now repeat constructor.
- inversion hd; subst; clear hd; [ now omega | ].
  inversion hins; subst; clear hins.
  constructor.
  eapply hi.
  + now apply H4.
  + now omega.
  + assumption.
Qed.

Lemma ins_item_lt: forall (d':Ty)(n:nat)(Γ Δ Δ':Env),
 ins_in_env Γ d' n Δ Δ' ->
 forall (v:nat), n > v ->
 forall (d:Ty), d ↓ v ∈ Δ -> d  ↓ v ∈ Δ' .
Proof.
intros d'; induction n as [ | n hi]; intros Γ Δ Δ' hins v hlt d hd; [ now omega | ].
inversion hins; subst; clear hins.
destruct v as [ | v].
- now inversion hd; subst; constructor.
- inversion hd; subst; clear hd.
  constructor.
  eapply hi.
  + now apply H0.
  + now omega.
  + now assumption.
Qed.


(** This type describe how do we do substitution inside a context.
As previously, no type checking is done at this point.*)

Inductive sub_in_env (Γ : Env) (T:Ty):
  nat -> Env -> Env -> Prop :=
    | sub_O :  sub_in_env Γ T 0 (T :: Γ) Γ
    | sub_S :
        forall Δ Δ' n  B,
        sub_in_env Γ T n Δ Δ' ->
        sub_in_env Γ T (S n) (B :: Δ) ( B :: Δ').

Hint Constructors sub_in_env.


(** Some ins / subst related facts: what happens to term when we do
 a substitution in a context.*)
Lemma nth_sub_sup :
   forall n Γ Δ Δ' T,
   sub_in_env Γ T n Δ Δ' ->
   forall v : nat, n <= v -> 
   forall d , d ↓ (S v) ∈ Δ -> d ↓ v ∈ Δ'.
Proof.
induction 1 as [ | Δ  Δ' n b hΔ hi]; intros v hle d hd.
- now inversion hd; subst; clear hd.
- inversion hd; subst; clear hd.
  destruct v as [ | v]; [ now omega | ].
  constructor.
  now apply hi; [ omega |].
Qed.

Lemma nth_sub_eq :
   forall T n Γ Δ Δ',
   sub_in_env Γ T n Δ Δ' -> 
  forall d , d↓ n ∈ Δ -> T = d.
Proof.
induction 1 as [ | Δ  Δ' n b hΔ hi]; intros d hd.
- now inversion hd; subst; clear hd.
- inversion hd; subst; clear hd.
  now apply hi.
Qed.

Lemma nth_sub_inf :
   forall T n Γ Δ Δ',
   sub_in_env Γ T n Δ Δ' ->
   forall v : nat,
   n > v ->
   forall d , d ↓ v ∈ Δ -> d ↓ v ∈ Δ' .
Proof.
induction 1 as [ | Δ  Δ' n b hΔ hi]; intros v hlt d hd; [ now omega | ].
inversion hd; subst; clear hd; [ now constructor | ].
constructor.
apply hi; [ now omega |assumption].
Qed.
