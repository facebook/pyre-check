From Coq Require Import Structures.OrderedTypeEx.
From Coq Require Import FMapList.
From Coq Require Import String.
From Pyre Require Typ Statement.

Module SMap := FMapList.Make(String_as_OT).

(** * Information about function definitions

We store in here all relevant information about known functions:
- their <<name>>.
- their signature, as a list of <<parameters>> and a <<return annotation>>.
- the actual <<body>> statement of the function.
*)
Record func : Set := mkFun {
    (* TODO(T53097965): support dotted access for methods *)
    name: string;
    (* TODO(T53097965): support more parameter kinds *)
    parameters: list (string * Typ.t);
    (* decorators: Expression.t list; *)
    return_annotation: Typ.t; (* no option, set to TNone if need be *)
    (* async: bool; *)
    (* parent: Reference.t option; (1* The class owning the method. *1) *)
    body: Statement.t;
}.

(** Global state for the statement operational semantic. It is made of
the state for expression evaluation, along with information about know
functions. *)
Record State {A: Set} := mkState {
  RawState :> SMap.t A;
  FuncState : SMap.t func 
}.

Definition set {A: Set} (state: @State A) (id: string) (v: A) :=
  mkState _ (SMap.add id v state) (FuncState state).

Definition remove {A: Set} (state : @State A) (id: string) : @State A :=
  mkState _ (SMap.remove id state) (FuncState state).


Definition get_info {A: Set} (state: @State A) (fun_name: string) : option func :=
  SMap.find fun_name (FuncState state).

From Coq Require Import FMapFacts.

Module SMapFacts := WFacts (SMap).

Lemma find_add_smap: forall {A: Set} (state : SMap.t A) (x y: string) (v : A),
  SMap.find y (SMap.add x v state) =
  if (y =? x)%string then Some v else SMap.find y state.
Proof.
intros A state x y v.
case_eq (String.eqb y x); intro hyx.
- apply SMapFacts.add_eq_o.
  apply String.eqb_eq in hyx; rewrite hyx; reflexivity.
- apply SMapFacts.add_neq_o.
  apply String.eqb_neq in hyx.
  firstorder.
Qed.

Lemma find_add: forall {A: Set} (state : @State A) (x y: string) (v : A),
  SMap.find y (set state x v) =
  if (y =? x)%string then Some v else SMap.find y state.
Proof.
intros A state x y v.
unfold set; simpl.
rewrite find_add_smap.
reflexivity.
Qed.

Lemma find_remove_smap: forall {A : Set} (state: @SMap.t A) (x z: string),
  SMap.find z (SMap.remove x state) =
  if (z =? x)%string then None else SMap.find z state.
Proof.
intros A state x z.
case_eq (String.eqb z x); intro hzx.
- apply SMapFacts.remove_eq_o.
  apply String.eqb_eq in hzx; rewrite hzx; reflexivity.
- apply SMapFacts.remove_neq_o.
  apply String.eqb_neq in hzx.
  firstorder.
Qed.
