(** Simply Typed Lambda Calculus with References with de Bruijn indexes *)
Require Import Arith Omega.
Require Import STLC_Ref.ty.

(** Var syntax: *)
Definition Vars := nat.
(** Addresses syntax:

 we don't really need them to be natural number, we just need a decidable
 equality and a way to build a 'fresh' one. Using nats make that easy
*)
Definition Addrs := nat.

(** Term syntax:
A term can be:
 - a variable
 - a function application
 - a function abstraction
 - the unit singleton
 - a reference over a term
 - a dereferenced term
 - an address
 - an assignation to a reference
*)
Inductive Term : Set:=
 | Var : Vars -> Term
 | App : Term -> Term -> Term
 | La  : Ty -> Term -> Term
 | unit : Term
 | ref : Term -> Term
 | deref : Term -> Term
 | Addr : Addrs -> Term
 | Assign : Term -> Term -> Term
.

Notation "x • y" := (App x y) (at level 15, left associativity).
Notation "# v" := (Var v) (at level 1).
Notation "'λ' [ T ] , v " := (La T v) (at level 20, T , v at level 30).
Notation "M :=: N" := (Assign M N) (at level 25).
Reserved Notation " t ↑ x # n " (at level 5, x at level 0, left associativity).

(* Disclaimner:
 *
 *
 * The rest of the file is about DeBruijn indexes support,
 * feel free to skip it
 *)

(** In order to deal with variable bindings and captures, we need a lift
function to deal with free and bounded variables.


   [M ↑ n # m] recursivly add [n] to all variables that are
   above [m] in [M]. *)
Fixpoint lift_rec (n:nat) (k:nat) (T:Term) {struct T} := match T with
   | # x =>  if le_gt_dec k x then # (n+x) else # x
   | M • N =>  (M ↑ n # k) • (N ↑ n # k)
   | λ[A], M => λ[A], (M ↑ n # (S k)) 
   | unit => unit 
   | ref M => ref (M ↑ n # k)
   | deref M => deref (M ↑ n # k)
   | Addr u => Addr u
   | M :=: N => (M ↑ n # k) :=: (N ↑ n # k)
 end  
   where "t ↑ n # k" := (lift_rec n k t).

Notation " t ↑ n " := (lift_rec n 0 t) (at level 5, n at level 0, left associativity).

(* begin hide *)
(* Some basic properties of the lift function. That is everything we will
ever need to handle de Bruijn indexes *)

Lemma inv_lift : forall M N n m , M ↑ n # m = N ↑ n # m -> M = N.
Proof.
induction M as [ v | ? hi1 ? hi2 | ? ? hi | | ? hi | ? hi | ? | ? hi1 ? hi2];
        destruct N as [ w | ? ? | ? ? | | ? | ? | ? | ? ?];
        simpl in *; intros n m;
        try solve [ destruct le_gt_dec; intros; now discriminate
                  | intros; now discriminate 
                  | intro heq; injection heq; intros h1 h2;
                    apply hi1 in h2; apply hi2 in h1; now subst
                  | intro heq; injection heq; intros h1 h2;
                    apply hi in h1; now subst 
                  | intro heq; injection heq; intros h1;
                    apply hi in h1; now subst ].
- destruct (le_gt_dec m v) as [ h | h];
        destruct (le_gt_dec m w) as [ h' | h']; intro heq;
                repeat destruct le_gt_dec; now injection heq; clear heq;
                intro heq; f_equal; omega.
- intro; reflexivity.
- intro h; injection h; clear h; intros h1.
  now rewrite h1.
Qed.

Lemma lift_rec0 : forall M n, M ↑ 0 # n = M.
Proof.
induction M as [ v | ? hi1 ? hi2 | ? ? hi | | ? hi | ? hi | ? | ? hi1 ? hi2];
        intros; simpl; try solve [
        now rewrite hi |
        now rewrite hi1, hi2 |
        reflexivity ].
now destruct (le_gt_dec n v).
Qed.

Lemma lift0 : forall M, M ↑ 0 = M.
Proof.
intros; apply lift_rec0.
Qed.

Lemma liftP1 : forall M i j k, (M ↑ j # i) ↑ k # (j+i) = M ↑ (j+k) # i.
Proof.
induction M as [ v | ? hi1 ? hi2 | ? ? hi | | ? hi | ? hi | ? | ? hi1 ? hi2 ];
        intros i j k; simpl in *; try solve [
            now rewrite hi1, hi2 |
            now rewrite <- hi |
            reflexivity ].
- destruct (le_gt_dec i v) as [ h | h]; simpl.
  + destruct (le_gt_dec (j+i) (j+v)) as [ h' | h']; simpl; now apply f_equal; omega.
  + destruct (le_gt_dec (j+i)) as [ h' | h']; [ | reflexivity].
    now apply f_equal; omega.
- rewrite <- hi.
  now replace (j + S i) with (S(j + i)).
Qed.

Lemma liftP2: forall M i j k n, i <= n ->
  (M ↑ j # i) ↑ k # (j+n) = (M ↑ k # n) ↑ j # i.
Proof.
induction M as [ v | ? hi1 ? hi2 | ? ? hi | | ? hi | ? hi | ? | ? hi1 ? hi2];
        intros i j k n hle; simpl in *; try solve [
            now rewrite hi1, hi2 |
            now rewrite hi |
            reflexivity ].
- destruct (le_gt_dec i v) as [ h | h]; destruct (le_gt_dec n v) as [ h' | h']; simpl in *;
  destruct le_gt_dec; destruct le_gt_dec; now apply f_equal; omega.
- replace (S (j + n)) with (j + S n) by intuition. 
  rewrite hi; [ reflexivity | now omega].
Qed.

Lemma liftP3 : forall M i k j n , i <= k -> k <= (i+n) ->
  (M ↑ n # i) ↑ j # k = M ↑ (j+n) # i.
Proof.
induction M as [ ? | ? hi1 ? hi2 | ? ? hi | | ? hi | ? hi | ? | ? hi1 ? hi2];
        intros i k j n h1 h2; simpl in *; try solve [
            reflexivity |
            now rewrite hi |
            now rewrite hi1, hi2 ].
- destruct (le_gt_dec i v) as [ h | h]; simpl in *; destruct le_gt_dec; apply f_equal; omega.
- rewrite hi; [ reflexivity | now omega | now omega].
Qed.

Lemma lift_lift : forall M n m, (M ↑ m) ↑ n  = M↑ (n+m).
Proof.
intros.
apply liftP3; intuition.
Qed.

(* end hide *)

(** We will consider the usual implicit substitution without variable capture
(this is where the lift operator comes in handy).
  [ M [ n ← N ] ] replace the variable [n] in [M] by the term [N].
  *)
Reserved Notation "t [ x ← u ]" (at level 5, x at level 0, left associativity).

Fixpoint subst_rec U T n {struct T} :=
 match T with
  | # x => match (lt_eq_lt_dec x n) with
      | inleft (left _) => # x (* v < n *)
      | inleft (right _) => U ↑ n  (* v = n *)
      | inright _ => # (x - 1) (* v > n *)
      end
  | M • N => (M [ n ← U ]) • ( N [ n ← U ]) 
  | λ[A] , M => λ[A] , (M [ S n ← U ]) 
  | unit => unit 
  | ref M => ref (M [ n ← U ])
  | deref M => deref (M [ n ← U ])
  | Addr u => Addr u
  | M :=: N => (M [ n ← U ]) :=: (N [ n ← U ])
end
    where " t [ n ← w ] " := (subst_rec w t n).

Notation " t [ ← w ] " := (subst_rec w t 0) (at level 5).

(* begin hide *)
(* Some basic properties of the substitution function. Again, we will only need
a few functions to deal with indexes. *)

Lemma substP1: forall M N i j k , 
  ( M [ j ← N] ) ↑ k # (j+i) = (M ↑ k # (S (j+i))) [ j ← (N ↑ k # i ) ].
Proof.
induction M as [ v | ? hi1 ? hi2 | ? ? hi | | ? hi | ? hi | ? | ? hi1 ? hi2 ];
        intros N i j k; try solve [
        reflexivity |
        now simpl; rewrite hi |
        now simpl; rewrite hi1, hi2 ].
- simpl (#v [j ← N] ↑ k # (j+i)).
  change (#v ↑ k # (S (j+i))) with  (if le_gt_dec (S (j+i)) v then #(k+v) else #v).
  destruct (lt_eq_lt_dec v j) as [[ | ] | ].
  + destruct (le_gt_dec (S (j+i)) v); [ now omega| ].
    simpl.
    destruct (le_gt_dec (j+i) v) as [  | ]; [ now omega | ].
    destruct (lt_eq_lt_dec v j) as [[ | ] | ]; [ reflexivity | now omega| now omega].
  + destruct (le_gt_dec (S(j+i)) v) as [ | ]; [ now omega| ].
    simpl.
    destruct (lt_eq_lt_dec v j) as [[] | ]; [now omega | | now omega].
    subst; apply liftP2; now omega.
  + destruct (le_gt_dec (S (j+i)) v).
    * simpl.
      destruct (le_gt_dec (j+i) v) as [  | ].
      -- destruct (lt_eq_lt_dec) as [ [] | ].
         ++ destruct le_gt_dec;now omega.
         ++ destruct le_gt_dec; now omega.
         ++ destruct le_gt_dec; [ f_equal; now omega| now omega].
      -- destruct le_gt_dec; destruct lt_eq_lt_dec as [ [] | ]; now omega.
    * simpl.
      destruct le_gt_dec; destruct lt_eq_lt_dec as [ [] | ]; try now omega.
      reflexivity.
- simpl.
  replace (S(S(j+i))) with (S((S j)+i)) by intuition.
  now rewrite <- hi.
Qed.

Lemma substP2: forall M N i j n, i <= n ->
  (M ↑ j # i ) [ j+n ← N ] = ( M [ n ← N]) ↑ j # i .
Proof.
induction M as [ v | ? hi1 ? hi2 | ? ? hi | | ? hi | ? hi | ? | ? hi1 ? hi2];
        intros N i j n hle; simpl in *; try solve [
            reflexivity |
            now rewrite hi |
            now rewrite hi1, hi2 ].
- destruct (le_gt_dec i v); destruct (lt_eq_lt_dec v n) as [[] | ]; simpl.
  + destruct lt_eq_lt_dec as [ [] | ]; destruct le_gt_dec; try now omega.
    reflexivity.
  + destruct lt_eq_lt_dec as [ [] | ]; try now omega.
    now rewrite liftP3; [ | omega | omega].
  + destruct lt_eq_lt_dec as [ [] | ]; destruct le_gt_dec; try now omega.
    now f_equal; omega.
  + destruct lt_eq_lt_dec as [ [] | ]; destruct le_gt_dec; try now omega.
    reflexivity.
  + now omega.
  + now omega.
- rewrite <- hi; [ | now omega].
  now replace (S (j + n)) with (j + S n).
Qed.

Lemma substP3: forall M N i  k n, i <= k -> k <= i+n ->
  (M↑ (S n) # i) [ k← N] = M ↑ n # i.
Proof.
induction M as [ v | ? hi1 ? hi2 | ? ? hi | | ? hi | ? hi | ? | ? hi1 ? hi2];
        intros N i k n h1 h2; simpl in *; try solve [
            reflexivity |
            now rewrite hi |
            now rewrite hi1, hi2 ].
- destruct (le_gt_dec i v).
  + unfold subst_rec.
   destruct (lt_eq_lt_dec (S(n+v)) k) as [[] | ]; [ now omega | now omega| now f_equal; omega].
  + simpl. destruct (lt_eq_lt_dec v k) as [[] | ]; [ reflexivity | now omega | now omega].
- rewrite hi; [ reflexivity | now omega | now omega].
Qed.

Lemma substP4: forall M N P i j, 
   (M [ i← N]) [i+j ← P] = (M [S(i+j) ← P]) [i← N[j← P]].
Proof.
induction M as [ v | ? hi1 ? hi2 | ? ? hi | | ? hi | ? hi | ? | ? hi1 ? hi2];
        intros N P i j; simpl in *; try solve [
            reflexivity |
            now rewrite hi |
            now rewrite hi1, hi2 ].
- destruct lt_eq_lt_dec as [ [] | ]; destruct lt_eq_lt_dec as [ [] | ]; simpl.
  + destruct lt_eq_lt_dec as [ [] | ]; destruct lt_eq_lt_dec as [ [] | ]; [ reflexivity | | | | | | | | ]; now omega.
  + now omega.
  + now omega.
  + destruct lt_eq_lt_dec as [ [] | ]; [ now omega | | now omega ].
    now rewrite substP2; [ reflexivity | omega ].
  + now omega.
  + now omega.
  + destruct lt_eq_lt_dec as [ [] | ]; destruct lt_eq_lt_dec as [ [] | ]; [ | | reflexivity | | | | | | ]; now omega.
  + destruct lt_eq_lt_dec as [ [] | ]; [ now omega | | now omega].
    now rewrite substP3; [ reflexivity | omega | omega ].
  + destruct lt_eq_lt_dec as [ [] | ]; destruct lt_eq_lt_dec as [ [] | ]; [ reflexivity | | | | | | | | reflexivity]; now omega.
- replace (S(S(i+j))) with (S((S i)+ j)) by intuition.
  now rewrite <- hi.
Qed.

Lemma subst_travers : 
 forall  M N P n, (M [← N]) [n ← P] = (M [n+1 ← P])[← N[n← P]].
Proof.
intros.
rewrite plus_comm. change n with (O+n). apply substP4.
Qed.
(* end hide *)
