Require Import String List.

(** * Map *)

Section Map.
Variable A: Set.

(** Generic definition of partial maps from identifiers to some type A *)
Definition Map : Set := string -> option A.

(** The empty map *)
Definition empty: Map := fun _ => None.

(** Reading from a map, might return None *)
Definition get_map (map: Map) id : option A := map id.

(** Updating a binding in a map *)
Definition set_map (map: Map) id (data: A) : Map := 
    fun x => if String.eqb id x then Some data else get_map map x.

(** Removing a binding in a map *)
Definition unset_map (map: Map) id : Map :=
    fun x => if String.eqb id x then None else get_map map x.

Lemma get_set_map: forall map x y a,
  get_map (set_map map x a) y =
  if  String.eqb x y then Some a else get_map map y.
Proof.
now  unfold set_map, get_map; simpl.
Qed.

(** Intermediate "state", which will be used to track types or values
associated with identifiers *)
Record RawState0: Set := mkRawState0 {
    local: Map;
}.

Definition Empty0 : RawState0 := mkRawState0 empty.

Definition get0 s k := get_map (local s) k.

Definition set0 st k (v: A) :=
    mkRawState0 (set_map (local st) k v).

Definition unset0 st k :=
    mkRawState0 (unset_map (local st) k).

Lemma get0_set0: forall s x y a,
  get0 (set0 s x a) y =
  if  String.eqb x y then Some a else get0 s y.
Proof.
intros s x y a; unfold set0, get0; simpl.
now rewrite get_set_map.
Qed.

End Map.

Section RawState.
Variable A: Set.
Variable B: Set.

(** Main "state", which stores a <<RawState0>> plus additional information
(like a list of the already checked/defined functions *)
Record RawState : Set := mkRawState {
    state : RawState0 A;
    info : Map B;
}.

Definition Empty (info: Map B) : RawState := mkRawState (Empty0 _) info.

Definition get (s: RawState) id := get0 _ (state s) id.
Definition set (s: RawState) id v :=
    mkRawState (set0 _ (state s) id v) (info s).
Definition unset (s: RawState) id :=
    mkRawState (unset0 _ (state s) id) (info s).

Definition get_info (s: RawState) f := get_map _ (info s) f.

Lemma get_set: forall s x y a,
  get (set s x a) y =
  if  String.eqb x y then Some a else get s y.
intros [s ?] x y a; simpl.
unfold set, get, set0, get0; simpl.
now rewrite get_set_map.
Qed.
End RawState.
