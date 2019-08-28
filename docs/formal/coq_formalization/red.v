Require Import STLC_Ref.term STLC_Ref.ty.
Require Import Arith Omega List.

(** 'value' definition.

  Note that we are going to work with a call-by-value reduction, so we
  could remove the variable which don't make sense in an empty context,
  but in practice, it doesn't harm so keeping the usual definition is
  not a problem.
*)
Definition is_value (t: Term) : Prop :=
  match t with
  | # v => True
  | λ[A] , M => True
  | Addr u => True 
  | unit => True
  | _ => False
  end.

(* begin hide *)
Lemma is_value_lift: forall T, is_value T -> forall n k, is_value (lift_rec n k T).
Proof.
destruct T as [ v | ? ? | ? ? | | ? | ? | ? | ? ? ]; intros hT n k; simpl;
        try now idtac.
now destruct le_gt_dec.
Qed.
 
Lemma is_value_subst: forall T, is_value T -> forall n V, is_value V ->
    is_value (subst_rec V T n).
Proof.
destruct T as [ v | ? ? | ? ? | | ? | ? | ? | ? ? ]; intros hT n V hV; simpl;
        try now idtac.
destruct lt_eq_lt_dec as [ [] | ]; try now idtac.
now apply is_value_lift.
Qed.
(* end hide *)

(**
 A memory is a partial map from addresses to untyped terms.
 The typing part will be done by an external map in sr.v.
 All terms in a memory should be regarded as well-typed term _in the empty
 context_.
*)
Definition Mem := list (Addrs * Term).

(* begin hide *)
(* Operators to apply lift/subst to memories *)
Definition lift_mem n k mem :=
    map (fun (aM: Addrs * Term) =>
    let '(a, M) := aM in (a, lift_rec n k M)) mem.

Definition subst_mem V mem n :=
    map (fun (aM: Addrs * Term) =>
    let '(a,  M) := aM in (a, subst_rec V M n))
    mem.
(* end hide *)

(** Check if an address is bound by some generic map (will be used 
   for memories and type stores)
*)
(* TODO: utiliser existsb ? *)
Fixpoint InAddr {T: Type} u (l: list (Addrs * T)) := 
    match l with 
    | nil => False 
    | (a, _) :: tl => if eq_nat_dec a u then True else InAddr u tl 
    end.

(* begin hide *)
Lemma InAddr_lift_inv : forall u mem n k,
    InAddr u (lift_mem n k mem) -> InAddr u mem.
Proof.
intros u; induction mem as [ | [a W] tl hi]; intros n k h; simpl in *; 
        [ now idtac | ].
destruct Nat.eq_dec as [heq | hne ]; [now idtac| ].
now eapply hi; apply h.
Qed.

Lemma InAddr_subst_inv: forall u mem V n,
    InAddr u (subst_mem V mem n) -> InAddr u mem.
Proof.
intros u; induction mem as [ | [a W] tl hi]; intros n k h; simpl in *; 
        [ now idtac | ].
destruct Nat.eq_dec as [heq | hne ]; [now idtac| ].
now eapply hi; apply h.
Qed.
(* end hide *)

(** Bind a new address in a map with some value *)
Definition bindAddr {T : Type} (a: Addrs) (t: T) (l : list (Addrs * T)) :=
    (a, t) :: l.       

Lemma Inbind : forall {T : Type} l a u (t: T),
    InAddr a (bindAddr u t l) <-> (a = u \/ InAddr a l).
Proof.
intro t; induction l as [ | [x y] tl hi]; intros a u T; simpl in *.
- split.
  + destruct Nat.eq_dec; intros; [ subst; now left | now right ].
  + intros [ h | h]; [now subst; destruct Nat.eq_dec | now elim h].
- split; intro h.
  + destruct Nat.eq_dec; [now subst; left | ].
    now right.
  + destruct Nat.eq_dec; [now destruct Nat.eq_dec | ].
    destruct h; subst; now destruct Nat.eq_dec.
Qed.

(* begin hide *)
Lemma bindMem_lift : forall a t m n k, lift_mem n k (bindAddr a t m) =
     bindAddr a (lift_rec n k t) (lift_mem n k m).
Proof.
    now intros.
Qed.

Lemma bindMem_subst: forall a t V m n, subst_mem V (bindAddr a t m) n =
    bindAddr a (subst_rec V t n) (subst_mem V m n).
Proof.
    now intros.
Qed.
(* end hide *)

(** Tries to find the associated value to an address in a map.
  If the value is not bound, it will return None.
*)
Fixpoint readAddr {T: Type} (u: Addrs) (l: list (Addrs * T)) :=
    match l with
    | nil => None
    | (a, V) :: tl => if eq_nat_dec a u then Some V else
            readAddr u tl
    end.

(* begin hide *)
Lemma readMem_lift_none: forall u m, readAddr u m = None ->
    forall n k, readAddr u (lift_mem n k m) = None.
Proof.
intro u; induction m as [ | [a V] tl hi]; intros heq n k; simpl in *;
        [ reflexivity | ].
destruct Nat.eq_dec; [ discriminate | now apply hi].
Qed.
 
Lemma readMem_lift_some : forall u m V, readAddr u m = Some V ->
   forall n k, readAddr u (lift_mem n k m) = Some (lift_rec n k V).
Proof.
intro u; induction m as [ | [a V] tl hi]; intros W heq n k; simpl in *;
        [ discriminate | ].
destruct Nat.eq_dec; [ injection heq; intros ; now subst | ].
now apply hi.
Qed.

Lemma readMem_subst_none: forall u m, readAddr u m = None ->
    forall V n, readAddr u (subst_mem V m n) = None.
intro u; induction m as [ | [a V] tl hi]; intros heq W n; simpl in *;
        [ reflexivity | ].
destruct Nat.eq_dec; [ discriminate | now apply hi].
Qed.

Lemma readMem_subst_some : forall u m V, readAddr u m = Some V ->
   forall W n, readAddr u (subst_mem W m n) = Some (subst_rec W V n).
Proof.
intro u; induction m as [ | [a U] tl hi]; intros V heq W n; simpl in *;
        [ discriminate | ].
destruct Nat.eq_dec; [ injection heq; intros ; now subst | ].
now apply hi.
Qed.
(* end hide *)

Lemma read2In: forall (T: Type) l u (t : T), readAddr u l = Some t ->
    InAddr u l.
Proof.
intro T; induction l as [ | (a, s) tl hi]; intros u t h; simpl in *;
        [ discriminate | ].
now destruct Nat.eq_dec; [ | apply hi with t].
Qed.

Lemma In2read: forall (T: Type) l u, InAddr u l -> exists t:T,
    readAddr u l = Some t.
Proof.
intro T; induction l as [ | (a, s) tl hi]; intros u h; simpl in *; 
        [ now idtac | ].
destruct Nat.eq_dec as [ heq | hne].
- subst.
  now exists s.
- now apply hi in h.
Qed.

Lemma readAddr_app {T: Type}: forall D1 u (t: T),
    readAddr u D1 = Some t -> forall D2, readAddr u (D1 ++ D2) = Some t.
Proof.
induction D1 as [ | [a v] tl hi]; intros u t heq D2.
- simpl in heq; now discriminate.
- revert heq; simpl.
  destruct Nat.eq_dec; [ now idtac | intros heq].
  now apply hi.
Qed.

(** Call by Value based on traditional $\beta$ reduction extended
to deal with references:
- (λ[x:A], M)•N → M[[x/N]]
- (mem, ref M) → (mem + (u, M), u) for any u not bound in mem 
- (mem, deref u) → (mem, mem(u))
- (mem, u := M) → (mem + (u, M), unit)
- usual contextual extension to make it a congruence

Since we need a memory to interprete the addresses, the reduction relation
associate a term and a memory to a new term and a new memory.
*)
Inductive red : Term -> Mem -> Term -> Mem -> Prop :=
(* Head reduction *)
  | rBeta : forall A M V mem, is_value V ->
          red ((λ[A], M) • V) mem (M [← V]) mem
  | rRef : forall u mem V,  ~ InAddr u mem -> is_value V ->
          red (ref V) mem (Addr u) (bindAddr u V mem)
  | rDeref: forall u mem V, readAddr u mem = Some V ->
         red (deref (Addr u)) mem V mem
  | rStore: forall u V mem, is_value V ->
          red ((Addr u) :=: V) mem unit (bindAddr u V mem)
(* Context rules *)
  | rApp1: forall m1 m2 M M' N , red M m1 M' m2 -> red (M • N) m1 (M' • N) m2
  | rApp2: forall m1 m2 V N  N', is_value V -> red N m1 N' m2 ->
          red  (V • N) m1 ( V  • N') m2
  | rRef1: forall m1 m2 M M', red M m1 M' m2 -> red (ref M) m1 (ref M') m2
  | rDeref1: forall m1 m2 M M', red M m1 M' m2 ->
          red (deref M) m1 (deref M') m2
  | rAssign1 : forall m1 m2 M M' N, red M m1 M' m2 ->
          red (M :=: N) m1 (M' :=: N) m2
  | rAssign2 : forall m1 m2 V N N', is_value V -> red N m1 N' m2 ->
          red (V :=: N) m1 (V :=: N') m2
.

(* begin hide *)
Lemma red_lift: forall m1 m2 M N, red M m1 N m2 -> forall n m,
    red (M ↑ n # m) (lift_mem n m m1) (N ↑ n # m) (lift_mem n m m2).
Proof.
induction 1 as [ ? ? ? ? ? | ? ? ? h ? | ? ? ? ? | ? ? ? ? |
        ? ? ? ? ? h hi | ? ? ? ? ? ? h hi | ? ? ? ? h hi |
        ? ? ? ? h hi | ? ? ? ? ? h hi | ? ? ? ? ? ? h hi ]; intros n m; simpl in *;
                try solve [
            now constructor |
            now constructor; [ apply is_value_lift | apply hi ]].
- change m with (0 + m).
  rewrite (substP1 _ _ m 0 n); simpl.
  constructor.
  now apply is_value_lift.
- simpl.
  econstructor; [ | now apply is_value_lift ].
  intro hh; apply h; now apply InAddr_lift_inv in hh.
- constructor.
  now apply readMem_lift_some.
- constructor.
  now apply is_value_lift.
Qed.

Lemma red_subst : forall m1 m2 M N, red M m1 N m2 ->
    forall n P, is_value P ->
    red (M [ n ← P]) (subst_mem P m1 n) (N [ n ← P]) (subst_mem P m2 n).
Proof.
induction 1 as [ ? ? ? ? ? | ? ? ? h ? | ? ? ? ? | ? ? ? ? |
        ? ? ? ? ? h hi | ? ? ? ? ? ? h hi | ? ? ? ? h hi |
        ? ? ? ? h hi | ? ? ? ? ? h hi | ? ? ? ? ? ? h hi ]; intros n P hP;
             simpl in *; try solve [
               constructor; now apply hi 
               | now constructor; [ apply is_value_subst | apply hi]].
- rewrite subst_travers.
  replace (n + 1) with (S n) by intuition.
  econstructor.
  now apply is_value_subst.
- constructor; [ | now apply is_value_subst ].
  intro hh; apply h; now apply InAddr_subst_inv in hh.
- constructor.
  now apply readMem_subst_some.
- constructor.
  now apply is_value_subst.
Qed.

(* Helper function to generate fresh addresses for maps *)
Fixpoint fresh {T: Type} (l : list (Addrs * T)) : Addrs :=
    match l with 
    | nil => 0
    | (hd, _) :: tl => S (hd + fresh tl)
    end.

Lemma fresh_is_greater {T: Type} : forall (l: list (Addrs * T)) u,
    InAddr u l -> u < fresh l.
Proof.
induction l as [ | [hd ?] tl hi]; intros u hIn; simpl in *;
        [ now elim hIn | ].
revert hIn.
destruct Nat.eq_dec as [heq | hne]; [ intros _; now subst; omega | intro h].
apply hi in h; now omega.
Qed.

Lemma fresh_is_fresh {T: Type} : forall (l: list (Addrs * T)),
    ~ InAddr (fresh l) l.
Proof.
intros l hin.
apply fresh_is_greater in hin.
now apply lt_irrefl in hin.
Qed.
(* end hide *)
