Require Import Arith Ascii String List Sorted.
Import ListNotations.
Require Import Pyre.Lib.String Pyre.Lib.List.

(** In this module, we define the main <<State>> structure of our judgements.
The underlying structure is a <<Map>> indexed by <<string>>s. They are
implemented using lists of "key/value" pairs, strictly (no duplicated entries)
sorted by keys. *)

(* Some helper definitions to deal encapsulate/project the order on string to
order of pairs of (string/anything). *)
Section SortedListHelpers.
Variable A: Type.

(* Pair comparison by only using the first member *)
Definition pair_ltb (elt0 elt1: string * A) : bool :=
  ltb (fst elt0) (fst elt1)
.

(* Propositional equivalent of the boolean relation pair_ltb *)
Definition pair_lt (elt0 elt1: string * A) : Prop :=
  pair_ltb elt0 elt1 = true
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
  pair_ltb x y = true -> pair_ltb y z = true -> pair_ltb x z = true.
Proof.
intros [x0 x1] [y0 y1] [z0 z1]; unfold pair_ltb; intros h0 h1; simpl in *.
now apply ltb_trans with (y := y0).
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
  correct : Sortedb _ (pair_ltb _) data = true;
}.

(* Note to the curious reader: <<Map>> is a dependently typed struct as
   its <<correct>> field depends on its <<data>> field. So comparing two
   instances of <<Map>> would require comparing proofs, which is notably
   hard/impossible to do. However in this particular case, the proof is
   simply an equality on booleans, which are equipped with a decidable
   equality. In this case, it is known that we have `uniqueness of identify
   proofs`, meaning that we won't have to compare anything, as there is a
   single inhabitant of these types. This allows us to compare maps
   just as if they were only simply typed lists *)


(* A comes with a boolean equality *)
Variable Aeqb : A -> A -> bool.
Hypothesis Aeqb_eq: forall x y, Aeqb x y = true -> x = y.
Hypothesis Aeqb_neq: forall x y, Aeqb x y = false -> x <> y.
Hypothesis Aeqb_refl: forall x, Aeqb x x = true.

(* Equality over pairs of string * A *)
Definition pair_eq elt1 elt2 : bool :=
  (((fst elt1) =? (fst elt2))%string &&
   Aeqb (snd elt1) (snd elt2))%bool.

(* begin hide *)
Lemma pair_eq_eq: forall x y, pair_eq x y = true -> x = y.
Proof.
intros [k1 v1] [k2 v2] heq.
apply andb_prop in heq as [h1 h2]; simpl in *.
apply String.eqb_eq in h1 as ->.
now apply Aeqb_eq in h2 as ->.
Qed.

Lemma pair_eq_neq: forall x y, pair_eq x y = false -> x <> y.
Proof.
intros [k1 v1] [k2 v2] hneq h; subst.
injection h; intros; subst; clear h.
apply Bool.andb_false_iff in hneq; simpl in *.
destruct hneq as [h | h].
- now apply String.eqb_neq in h.
- now apply Aeqb_neq in h.
Qed.

Lemma pair_eq_refl: forall x, pair_eq x x = true.
Proof.
intros [k v]; unfold pair_eq; simpl.
now rewrite Aeqb_refl, String.eqb_refl.
Qed.
(* begin hide *)

(** Boolean equality on <<Map>>s *)
(* As stated in the previous note, we only compare the underlying list,
   because of the uniqueness of identity proofs.
   Note that we also rely on the coercion from map to (data map) provided
   by the system. The "full" statement should be
   ``eqb _ pair_eq (data map1) (data map2)`` but this version is a bit 
   more readable *)
Definition eqb (map1 map2: Map) : bool :=
  (eqb _ pair_eq map1 map2).

Lemma eqb_eq: forall map1 map2, eqb map1 map2 = true -> map1 = map2.
Proof.
intros [l1 h1] [l2 h2] h; unfold eqb in h; simpl in *.
apply List.eqb_eq in h as ->; [ | now apply pair_eq_eq].
now rewrite (sorted_proof_unicity _ _ l2 _ h1 h2).
Qed.

Lemma eqb_neq: forall map1 map2, eqb map1 map2 = false -> map1 <> map2.
Proof.
intros [l1 h1] [l2 h2] hn h; unfold eqb in hn; simpl in *.
injection h; clear h; intros; subst.
rewrite List.eqb_refl in hn; [ discriminate | now apply pair_eq_refl].
Qed.

(** The empty map *)
Definition empty: Map := mkMap nil (refl_equal _).

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
      else if ltb key key'
           then (key, value) :: l
           else (key', value') :: insert_sorted key value l'
  end.

(* begin hide *)
Lemma insert_sorted_hdrel: forall l key value,
  HdRel (pair_lt _) (key, value) l ->
  forall key' value',
  ltb key key' = true ->
  HdRel (pair_lt _) (key, value) (insert_sorted key' value' l).
Proof.
induction l as [ | [k v] data hi]; intros key value hhd key' value' hlt;
    simpl in *; [now constructor | ].
apply HdRel_inv in hhd; unfold pair_lt, pair_ltb in hhd; simpl in hhd.
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
    now constructor; unfold pair_lt; simpl.
  + apply Sorted_inv in hsort as [ h0 h1].
    constructor; [ now apply hi | ].
    apply insert_sorted_hdrel;[ assumption | ].
    apply String.eqb_neq in heq.
    apply lt_ge in hkeys as [ h | h ]; [assumption | ].
    now elim heq.
Qed.

Lemma insert_sorted_sorts_bool: forall l, Sortedb _ (pair_ltb _) l = true ->
  forall key value, Sortedb _ (pair_ltb _) (insert_sorted key value l) = true.
Proof.
intros l h key value.
apply reflect_Sorted in h; apply reflect_Sorted.
now apply insert_sorted_sorts.
Qed.
(* begin hide *)

(** Set a <<key>> to some <<value>> in a <<Map>>, overriding the existing
    value if the <<key>> was already in use. *)
Definition set_map (map: Map) key (value: A) : Map := 
  let new_data := insert_sorted key value map in
  mkMap new_data (insert_sorted_sorts_bool map (correct map) key value).

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

Lemma remove_sorted_sorts_bool: forall l, Sortedb _ (pair_ltb _) l = true ->
  forall key, Sortedb _ (pair_ltb _) (remove_sorted l key) = true.
Proof.
intros l hs key.
apply reflect_Sorted in hs; apply reflect_Sorted.
now apply remove_sorted_sorts.
Qed.
(* end hide *)

(** Remove an entry from a map. Once this function terminates, the <<key>>
  is no longer mapped to anything in the map. *)
Definition unset_map (map: Map) key : Map :=
  let new_data := remove_sorted (data map) key in 
  mkMap new_data (remove_sorted_sorts_bool (data map) (correct map) key).

(** In this section, we proove that our maps enjoy the extensionality
    property: it is enough to compare all keys to test the equality 
    between maps.
    This part reallies on the fact that our <<lt>> relation is transitive. *)
Section Extensionality.

(* If a <<key>> is less that all the keys in a list, then getting <<key>>
   from the list will return nothing. *)
Lemma get_lt_none: forall l key value, Forall (pair_lt _ (key, value)) l ->
  get_map_ l key = None.
Proof.
induction l as [ | [k v] l hi]; intros key value hf; simpl in *; [reflexivity | ].
case_eq (String.eqb key k); intro hk.
- apply Forall_inv in hf.
  unfold pair_lt, pair_ltb in hf; simpl in hf.
  apply lt_not_eq in hf; rewrite hf in hk; discriminate.
- apply Forall_inv_tail in hf.
  now apply hi in hf.
Qed.

(* If two lists are strongly sorted, and all keys map to the same value (or 
   absence of value, then they are equal. *)
Lemma map_extensionality_: forall l1 l2,
  StronglySorted (pair_lt _) l1 ->
  StronglySorted (pair_lt _) l2 ->
  (forall key, get_map_ l1 key = get_map_ l2 key) ->
  l1 = l2.
Proof.
induction l1 as [ | [k1 v1] l1 hi]; intros [ | [k2 v2] l2] hs1 hs2 hk;
    simpl in *.
- reflexivity.
- specialize hk with k2.
  rewrite String.eqb_refl in hk.
  discriminate hk.
- specialize hk with k1.
  rewrite String.eqb_refl in hk.
  discriminate hk.
- apply StronglySorted_inv in hs1 as [hs1 hf1].
  apply StronglySorted_inv in hs2 as [hs2 hf2].
  case_eq (String.eqb k1 k2); intro hk12.
  + replace k2 with k1 in * by now apply String.eqb_eq in hk12.
    rewrite (hi l2); intuition.
    * replace v2 with v1; [ reflexivity | ].
      specialize hk with k1.
      rewrite String.eqb_refl in hk.
      now injection hk.
    * specialize hk with key.
      case_eq (String.eqb key k1); intro heq; rewrite heq in hk; [| assumption].
      apply String.eqb_eq in heq; subst.
      now rewrite (get_lt_none l1 k1 v1), (get_lt_none l2 k1 v2).
  + case_eq (ltb k1 k2); intro hcomp.
    * specialize hk with k1.
      rewrite String.eqb_refl, hk12 in hk.
      rewrite (get_lt_none l2 k1 v1) in hk; [discriminate | ].
      eapply Forall_pair_lt; [ | apply hf2].
      now apply hcomp.
    * specialize hk with k2.
      rewrite String.eqb_sym in hk12.
      rewrite String.eqb_refl, hk12 in hk.
      rewrite (get_lt_none l1 k2 v2) in hk; [discriminate | ].
      eapply Forall_pair_lt; [ | apply hf1].
      unfold pair_lt, pair_ltb; simpl.
      apply lt_ge in hcomp as [ ? | heq]; [assumption | ].
      subst; rewrite String.eqb_refl in hk12; discriminate.
Qed.
 
(** If two maps have the exact same mappings, they are equal. *)
Lemma map_extensionality: forall map1 map2,
  (forall key, get_map map1 key = get_map map2 key) -> map1 = map2.
Proof.
intros map1 map2 hk.
apply eqb_eq.
destruct map1 as [l1 hl1]; destruct map2 as [l2 hl2];
   unfold eqb, get_map in *; simpl in *.
replace l2 with l1; [ now apply List.eqb_refl; apply pair_eq_refl | ].
apply map_extensionality_; intuition.
- apply Sorted_StronglySorted; [now intros x y z; apply pair_lt_trans | ].
  now apply reflect_Sorted.
- apply Sorted_StronglySorted; [now intros x y z; apply pair_lt_trans | ].
  now apply reflect_Sorted.
Qed.

End Extensionality.

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
- apply andb_prop in hs as [hs hhd].
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
apply andb_prop in hs as [hs hhd].
case_eq (z =? x)%string; intro hzx; rewrite hzx in hi.
- case_eq (x =? k)%string; intro hxk; simpl.
  + apply String.eqb_eq in hzx.
    apply String.eqb_eq in hxk; subst.
    apply get_lt_none with (value := v).
    apply HdRel_Forall; [ now apply pair_lt_trans | now apply reflect_Sorted in hs |].
    now apply reflect_HdRel in hhd.
  + apply String.eqb_eq in hzx; subst.
    rewrite hxk.
    now apply hi.
- case_eq (x =? k)%string; intro hxk; simpl.
  + apply String.eqb_eq in hxk; subst.
    now rewrite hzx.
  + case_eq (z =? k)%string; intro hzk; simpl; [ reflexivity | ].
    now apply hi.
Qed.

(* Sorted insertion in a sorted list commutes if both keys are different *)
Lemma set_set_diff_: forall map x y a b z,
  (x =? y)%string = false ->
  get_map_ (insert_sorted y b (insert_sorted x a map)) z =
  get_map_ (insert_sorted x a (insert_sorted y b map)) z.
Proof.
induction map as [ | [key value] tl hi]; intros x y a b z hne; simpl in *.
- rewrite hne, String.eqb_sym, hne.
  case_eq (ltb y x); intro hlt.
  + rewrite ltb_antisym in hlt.
    unfold leb in hlt.
    rewrite Bool.negb_orb in hlt.
    apply andb_prop in hlt as [ h1 h2 ].
    now apply Bool.negb_true_iff in h1 as ->.
  + rewrite ltb_antisym in hlt.
    unfold leb in hlt.
    rewrite Bool.negb_orb in hlt.
    apply Bool.andb_false_iff in hlt as [ h | h ].
    * now apply Bool.negb_false_iff in h as ->.
    * apply Bool.negb_false_iff in h. 
      rewrite h in hne; discriminate.
- case_eq (String.eqb x key); intro hxkey; simpl.
  + apply String.eqb_eq in hxkey; subst.
    rewrite String.eqb_sym, hne; simpl.
    case_eq (ltb y key); intro hlt; simpl insert_sorted.
    * rewrite hne, ltb_antisym; unfold leb.
      now rewrite hlt, String.eqb_refl.
    * now rewrite String.eqb_refl.
  + case_eq (String.eqb y key); intro hykey.
    * apply String.eqb_eq in hykey; subst.
      case_eq (ltb x key); intro hlt; simpl insert_sorted.
      -- rewrite String.eqb_sym, hne, String.eqb_refl, ltb_antisym.
         now unfold leb; rewrite hlt; simpl.
      -- now rewrite String.eqb_refl, hne, hlt.
    * case_eq (ltb x key); intro hltx.
      -- case_eq (ltb y key); intro hlty; simpl insert_sorted.
         ++ rewrite String.eqb_sym, hne, hykey, hlty, hxkey, hltx.
            case_eq (ltb y x); intro h.
            ** now rewrite ltb_antisym; unfold leb; rewrite h.
            ** now rewrite ltb_antisym; unfold leb; rewrite h,
                 String.eqb_sym, hne.
         ++ rewrite String.eqb_sym, hne, hykey, hlty, hxkey, hltx.
            case_eq (ltb y x); intro h; [ | reflexivity ].
            assert (hw: ltb y y = true);
                  [ | now rewrite ltb_irrefl in hw; discriminate ].
            apply (ltb_trans y x y); [ assumption | ].
            apply (ltb_trans x key y); [ assumption | ].
            now rewrite ltb_antisym; unfold leb; rewrite hlty, hykey.
     -- case_eq (ltb y key); intro hlty; simpl insert_sorted.
        ++ rewrite hykey, hne, hlty, hltx, hxkey.
           case_eq (ltb x y); intro h; [| reflexivity ].
            assert (hw: ltb x x = true);
                  [ | now rewrite ltb_irrefl in hw; discriminate ].
            apply (ltb_trans x y x); [ assumption | ].
            apply (ltb_trans y key x); [ assumption | ].
            now rewrite ltb_antisym; unfold leb; rewrite hltx, hxkey.
        ++ rewrite hykey, hxkey, hlty, hltx.
           simpl.
           case_eq (String.eqb z key); [ reflexivity | intro h].
           now apply hi.
Qed.

Lemma set_set_diff_map: forall map x y a b,
  String.eqb x y = false ->
  set_map (set_map map x a) y b =
  set_map (set_map map y b) x a.
Proof.
intros map x y a b hne; apply map_extensionality; intros z.
unfold set_map, get_map; simpl.
now apply set_set_diff_.
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

Lemma set0_set0_diff: forall s x y a b,
  (x =? y)%string = false ->
  set0 (set0 s x a) y b =
  set0 (set0 s y b) x a.
Proof.
intros s x y a b hne; unfold set0, get0; simpl.
now rewrite set_set_diff_map.
Qed.

End Map.

Section RawState.
Context {A B: Set}.

Variable Aeqb : A -> A -> bool.
Hypothesis Aeqb_eq: forall x y, Aeqb x y = true -> x = y.
Hypothesis Aeqb_refl: forall x, Aeqb x x = true.

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

Lemma set_set_diff: forall s x y a b,
  (x =? y)%string = false ->
  set (set s x a) y b =
  set (set s y b) x a.
Proof.
intros [s ?] x y a b h; simpl.
unfold set, get.
simpl state.
now rewrite (set0_set0_diff _ Aeqb_eq Aeqb_refl).
Qed.

End RawState.
