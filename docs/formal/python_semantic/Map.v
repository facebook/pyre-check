Require Import Arith Ascii String List Sorted.
Import ListNotations.
Require Import Pyre.Lib.List Pyre.Lib.String.

(** In this module, we define the main <<State>> structure of our judgements.
The underlying structure is a <<Map>> indexed by <<string>>s. They are
implemented using lists of "key/value" pairs, strictly (no duplicated entries)
sorted by keys. *)

(* Some helper definitions to deal encapsulate/project the order on string to
order of pairs of (string/anything). *)
Section SortedListHelpers.
Variable A: Type.

(* Pair comparison by only using the first member *)
Definition pair_lt (elt0 elt1: string * A) : Prop :=
  String.lt (fst elt0) (fst elt1)
.

(* begin hide *)
Lemma HdRel_pair_lt_right: forall l key value0 value1,
  HdRel pair_lt (key, value0) l -> HdRel pair_lt (key, value1) l.
Proof.
intros [ | hd tl] key v0 v1 h; simpl in *; [ now constructor | ].
apply HdRel_inv in h; constructor.
now unfold pair_lt in *.
Qed.

Lemma pair_lt_trans: forall (x y z: string * A),
  pair_lt x y -> pair_lt y z -> pair_lt x z.
Proof.
intros [x0 x1] [y0 y1] [z0 z1]; intros h0 h1; simpl in *.
now apply String.lt_trans with (y := y0).
Qed.

Lemma Forall_pair_lt: forall key1 value1 key2 value2,
  pair_lt (key1, value1) (key2, value2) ->
  forall l, Forall (pair_lt (key2, value2)) l ->
  Forall (pair_lt (key1, value1)) l.
Proof.
intros key1 value1 key2 value2 hp.
induction l as [ | [k v] l hi]; intros hf; simpl in *; [ now constructor | ].
constructor.
- eapply pair_lt_trans; [now apply hp| ].
  now apply Forall_inv in hf.
- apply hi.
  now apply Forall_inv_tail in hf.
Qed.
(* end hide *)
End SortedListHelpers.

(** * Map

A <<Map>> is a stricly sorted list of key/value pairs which helps keeping
track of types / values in the rest of the developlment. *)
Section Map.

(* Generic type of values *)
Context {A: Set}.

(** Generic definition of partial maps from identifiers to some type A
    encoded as sorted lists of pairs *)
Record Map : Set := mkMap {
  data :> list (string * A); (* Maps can be coerced to their data field *)
  correct : @Sorted _ (pair_lt _) data
}.

(** The empty map *)
Definition empty: Map := mkMap nil (Sorted_nil _).

(** Reading from a map, might return None if the key is not present *)
Fixpoint get_map_ (map: list (string * A)) id : option A :=
  match map with 
  | nil => None
  | (key, value) :: map =>
      if (id =? key)%string
      then Some value
      else get_map_ map id
end.

(* wrapper around get_map_ *)
Definition get_map (map: Map) key : option A := get_map_ map key.

(** Updating a binding in a map: since we always keep our lists sorted,
    we proceed by insertion sort, overriding the existing value if the 
    key was already in use. *)
Fixpoint insert_sorted (key: string) (value: A)
  (l: list (string * A)) : list (string * A) :=
  match l with
  | [] => [(key, value)]
  | (key', value') :: l' =>
      if (key =? key')%string
      then (key, value) :: l'
      else if String.ltb key key'
           then (key, value) :: l
           else (key', value') :: insert_sorted key value l'
  end.

(* begin hide *)
Lemma insert_sorted_hdrel: forall l key value,
  HdRel (pair_lt _) (key, value) l ->
  forall key' value',
  String.lt key key' ->
  HdRel (pair_lt _) (key, value) (insert_sorted key' value' l).
Proof.
induction l as [ | [k v] data hi]; intros key value hhd key' value' hlt;
    simpl in *; [now constructor | ].
apply HdRel_inv in hhd; unfold pair_lt in hhd; simpl in hhd.
case_eq (String.eqb key' k); intro heq.
- now constructor.
- case_eq (ltb key' k); intro hk;  now constructor.
Qed.

Lemma insert_sorted_sorts: forall l, Sorted (pair_lt _) l ->
  forall key value, Sorted (pair_lt _) (insert_sorted key value l).
Proof.
induction l as [ | [key value] data hi]; intros hsort key' value';
    simpl in *; [ now repeat constructor | ].
case_eq (String.eqb key' key); intro heq.
- apply Sorted_inv in hsort as [ h0 h1].
  constructor; [ assumption | ].
  apply String.eqb_eq in heq; subst.
  now apply HdRel_pair_lt_right with (value0 := value).
- case_eq (ltb key' key); intro hkeys.
  + constructor; [ assumption | ].
    apply String.ltb_lt in hkeys.
    now constructor; unfold pair_lt; simpl.
  + apply Sorted_inv in hsort as [ h0 h1].
    constructor; [ now apply hi | ].
    apply insert_sorted_hdrel;[ assumption | ].
    apply String.eqb_neq in heq.
    apply String.lt_ge in hkeys as [ h | h ]; [ now apply String.ltb_lt | ].
    now elim heq.
Qed.
(* end hide *)

(** Set a <<key>> to some <<value>> in a <<Map>>, overriding the existing
    value if the <<key>> was already in use. *)
Definition set_map (map: Map) key (value: A) : Map := 
  let new_data := insert_sorted key value map in
  mkMap new_data (insert_sorted_sorts map (correct map) key value).

(** Removing a key/value pair in a sorted list, based on the key.
    Since we are stricly sorted, the first one (if any) is the only one *)
Fixpoint remove_sorted (l: list (string * A)) id : list (string * A) :=
  match l with 
  | nil => nil
  | (key, value) :: l =>
      if (id =? key)%string
      then l
      else (key, value) :: remove_sorted l id
end.

(* begin hide *)
Lemma remove_sorted_forall: forall l key value,
  Forall (pair_lt _ (key, value)) l ->
  forall key',
  Forall (pair_lt _ (key, value)) (remove_sorted l key').
Proof.
induction l as [ | [key value] l hi]; intros k v hf key'; simpl in *;
    [now constructor | ].
case_eq (String.eqb key' key); intro heq; [ now apply Forall_inv_tail in hf | ].
constructor; [now apply Forall_inv in hf | ].
apply hi.
now apply Forall_inv_tail in hf.
Qed.

Lemma remove_sorted_strongly_sorted: forall l,
  StronglySorted (pair_lt _) l -> forall key,
  StronglySorted (pair_lt _) (remove_sorted l key).
Proof.
induction 1 as [ | [key value] data hdata hi hforall ]; intros key';
    simpl in *; [ now constructor | ].
case_eq (String.eqb key' key); intro hk; [ assumption | ].
constructor; [now apply hi | ].
now apply remove_sorted_forall.
Qed.

Lemma remove_sorted_sorts: forall l, Sorted (pair_lt _) l ->
  forall key, Sorted (pair_lt _) (remove_sorted l key).
Proof.
intros l hs k.
apply StronglySorted_Sorted.
apply remove_sorted_strongly_sorted.
apply Sorted_StronglySorted; [| assumption].
intros x y z.
now apply pair_lt_trans.
Qed.
(* end hide *)

(** Remove an entry from a map. Once this function terminates, the <<key>>
  is no longer mapped to anything in the map. *)
Definition unset_map (map: Map) key : Map :=
  let new_data := remove_sorted (data map) key in 
  mkMap new_data (remove_sorted_sorts (data map) (correct map) key).

(* If a <<key>> is less that all the keys in a list, then getting <<key>>
   from the list will return nothing. *)
Lemma get_lt_none: forall l key value, Forall (pair_lt _ (key, value)) l ->
  get_map_ l key = None.
Proof.
induction l as [ | [k v] l hi]; intros key value hf; simpl in *; [reflexivity | ].
case_eq (String.eqb key k); intro hk.
- apply Forall_inv in hf.
  unfold pair_lt in hf; simpl in hf.
  apply String.lt_not_eq in hf.
  elim hf.
  now apply String.eqb_eq.
- apply Forall_inv_tail in hf.
  now apply hi in hf.
Qed.

Lemma get_set_map: forall map x y a,
  get_map (set_map map x a) y =
  if  (y =? x)%string then Some a else get_map map y.
Proof.
intros [l hs] x y a; unfold set_map, get_map; simpl.
induction l as [ | [k v] l hi]; simpl in *; [reflexivity | ].
case_eq (String.eqb x k); intro hk; simpl.
- case_eq (String.eqb y x); intro hyx; rewrite hyx in *; [reflexivity | ].
  apply String.eqb_eq in hk as ->.
  now rewrite hyx.
- apply Sorted_inv in hs as [hs hhd].
  case_eq (ltb x k); intro hlt; simpl.
  + now case_eq (String.eqb y x); intro hyx; rewrite hyx in *.
  + case_eq (String.eqb y x); intro hyx; rewrite hyx in *.
    * apply String.eqb_eq in hyx; subst.
      rewrite hk.
      now apply hi.
    * case_eq (String.eqb y k); intro hyk; [reflexivity | ].
      now apply hi.
Qed.

Lemma get_unset_map:
  forall map x z,
  get_map (unset_map map x) z =
  if (z =? x)%string then None else get_map map z.
Proof.
unfold unset_map, get_map; simpl.
intros [l hs] x z.
induction l as [ | [k v] l hi]; simpl in *; [ now destruct String.eqb | ].
apply Sorted_inv in hs as [hs hhd].
case_eq (z =? x)%string; intro hzx; rewrite hzx in hi.
- case_eq (x =? k)%string; intro hxk; simpl.
  + apply String.eqb_eq in hzx.
    apply String.eqb_eq in hxk; subst.
    apply get_lt_none with (value := v).
    now apply HdRel_Forall; [ now apply pair_lt_trans | | ].
  + apply String.eqb_eq in hzx; subst.
    rewrite hxk.
    now apply hi.
- case_eq (x =? k)%string; intro hxk; simpl.
  + apply String.eqb_eq in hxk; subst.
    now rewrite hzx.
  + case_eq (z =? k)%string; intro hzk; simpl; [ reflexivity | ].
    now apply hi.
Qed.

(** Intermediate "state", which will be used to track types or values
associated with identifiers *)
Record RawState0: Set := mkRawState0 {
    local: Map;
}.

(* Wrappers around their _map counter-part *)
Definition Empty0 : RawState0 := mkRawState0 empty.

Definition get0 s k := get_map (local s) k.

Definition set0 st k (v: A) :=
    mkRawState0 (set_map (local st) k v).

Definition unset0 st k :=
    mkRawState0 (unset_map (local st) k).

Lemma get0_set0: forall s x y a,
  get0 (set0 s x a) y =
  if (y =? x)%string then Some a else get0 s y.
Proof.
intros s x y a; unfold set0, get0; simpl.
now rewrite get_set_map.
Qed.

Lemma get0_unset0:
  forall s x z,
  get0 (unset0 s x) z =
  if (z =? x)%string then None else get0 s z.
Proof.
intros s x z; unfold unset0, get0; simpl.
now rewrite get_unset_map.
Qed.

End Map.

Section RawState.
Context {A B: Set}.

(** Main "state", which stores a <<RawState0>> plus additional information
  (like a list of the already checked/defined functions.
  We implicitely coerce <<RawState>> to their underlying <<RawState0>>. *)
Record RawState : Set := mkRawState {
    state :> @RawState0 A;
    info : @Map B;
}.

Definition Empty (info: @Map B) : RawState := mkRawState Empty0 info.

Definition get (s: RawState) id := get0 (state s) id.
Definition set (s: RawState) id v :=
    mkRawState (set0 s id v) (info s).
Definition unset (s: RawState) id :=
    mkRawState (unset0 s id) (info s).

Definition get_info (s: RawState) f := get_map (info s) f.

Lemma get_set: forall s x y a,
  get (set s x a) y =
  if  (y =? x)%string then Some a else get s y.
Proof.
intros [s ?] x y a; simpl.
unfold set, get; simpl state.
now rewrite get0_set0.
Qed.

Lemma get_unset:
  forall s x z,
  get (unset s x) z =
  if (z =? x)%string then None else get s z.
Proof.
intros s x z; unfold unset, get; simpl.
now rewrite get0_unset0.
Qed.

End RawState.
