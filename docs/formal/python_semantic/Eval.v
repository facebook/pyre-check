Require Import String Bool ZArith List.
Require Pyre.Value.
Require Pyre.Lvalue.
Require Pyre.Typ.
Require Pyre.Expression.
Require Pyre.Statement.
Require Import Pyre.Map.

Import Pyre.Value.ListHelpers.
Import Pyre.Typ.ListHelpers.
Import Pyre.Expression.ListHelpers.

(* begin hide *)
Definition evaluate_boolean_operator lexpr op rexpr :=
    match op with 
    | Expression.And => andb lexpr rexpr
    | Expression.Or => orb lexpr rexpr
end.

(* Helper to evaluate value membership *)
Fixpoint value_in (x lst:Value.t) : bool :=
    match lst with 
    | Value.Sequence lst => List.existsb (fun y => Value.eqb x y) lst
    | _ => false 
    end.

Definition evaluate_int_comparison_operator lexpr op rexpr: bool :=
    match lexpr, rexpr with 
    | Value.Integer lexpr, Value.Integer rexpr =>
            match op with 
            | Expression.GreaterThan => Z.gtb lexpr rexpr
            | Expression.GreaterThanOrEquals => Z.geb lexpr rexpr
            | Expression.LessThan => Z.ltb lexpr rexpr
            | Expression.LessThanOrEquals => Z.leb lexpr rexpr
            | _ => false 
            end 
    | _ , _ => false 
end.

Definition evaluate_comparison_operator lexpr op rexpr :=
    match op with 
    | Expression.Equals => Value.eqb lexpr rexpr
    | Expression.NotEquals => negb (Value.eqb lexpr rexpr)
    | Expression.GreaterThan 
    | Expression.GreaterThanOrEquals
    | Expression.LessThan
    | Expression.LessThanOrEquals =>
      evaluate_int_comparison_operator lexpr op rexpr
    | Expression.In => value_in lexpr rexpr
    | Expression.NotIn => negb (value_in lexpr rexpr)
    | _ => (* TODO(T53097965): implement Is/IsNot *) false
    end
.

Definition evaluate_unary_operator op expr : option Value.t :=
    match op, expr with 
    | Expression.Not, Value.Boolean b => Some (Value.Boolean (negb b))
    | Expression.Invert, Value.Integer z => Some (Value.Integer (-z -1)%Z) 
    | Expression.Negative, Value.Integer z => Some (Value.Integer (- z)%Z)
    | Expression.Positive, Value.Integer z => Some (Value.Integer z)
    | _, _ => None
    end.

Lemma evaluate_unary_int_operator : forall op z,
  evaluate_unary_operator op (Value.Integer z) =
    match op with 
    | Expression.Negative => Some (Value.Integer (-z)%Z)
    | Expression.Positive => Some (Value.Integer z)
    | Expression.Invert => Some (Value.Integer (-z -1)%Z)
    | _ => None 
    end.
Proof.
now destruct op; intros z; simpl.
Qed.
(* end hide *)

(** Map from identifiers to values. Used during <<Expression.t>> evaluation *)
Definition EState := SMap.t Value.t.
Definition Empty : EState := SMap.empty _.

(** * Evaluation of expressions
  Evaluates the expression <<expr>> using the state <<now>>. Returns some
  <<Value.t>> if everything went ok, or <<None>> otherwise.
*)
Fixpoint eval (now: EState) (expr : Expression.t) : option Value.t :=
    match expr with
    | Expression.BooleanOperator lexpr op rexpr => 
            match (eval now lexpr, eval now rexpr) with
            | (Some (Value.Boolean b0), Some (Value.Boolean b1)) =>
                    Some (Value.Boolean (evaluate_boolean_operator b0 op b1))
            | (_, _) => None
            end
    | Expression.ComparisonOperator lexpr op rexpr =>
            match (eval now lexpr, eval now rexpr) with
            | (Some vl, Some vr) =>
               Some (Value.Boolean (evaluate_comparison_operator vl op vr))
            | (_, _) => None
            end
    | Expression.False_ => Some (Value.Boolean false)
    | Expression.Integer z => Some (Value.Integer z)
    | Expression.List el =>
            match eval_list now el with
            | Some vl => Some (Value.Sequence vl)
            | None => None
            end
    | Expression.Id id => SMap.find id now
    | Expression.None => Some Value.None
    | Expression.String s => Some (Value.String s)
    | Expression.Ternary target test alternative =>
            match eval now test with
            | Some (Value.Boolean true) => eval now target
            | Some (Value.Boolean false) => eval now alternative
            | Some _ => None
            | None => None
            end
    | Expression.True_ => Some (Value.Boolean true)
    | Expression.Tuple el =>
            match eval_list now el with
            | Some vl => Some (Value.Sequence vl)
            | None => None
            end
    | Expression.UnaryOperator op e =>
            match eval now e with
            | Some v => evaluate_unary_operator op v
            | None => None
            end
    end
with eval_list (now: EState) (elist : Expression.tlist) :
  option Value.tlist :=
  match elist with
  | Expression.Nil => Some (Value.Nil)
  | Expression.Cons hd tl =>
          match (eval now hd, eval_list now tl) with
          | (Some hd, Some tl) => Some (Value.Cons hd tl)
          | (_, _) => None
          end
  end
.

Lemma eval_list_length: forall (state: EState)
  (l: Expression.tlist) (vl: Value.tlist),
  eval_list state l = Some vl -> List.length l = List.length vl.
Proof.
intro now; induction l as [ | e es hi]; intros vs' h; simpl in *.
- now injection h; clear h; intro h; subst.
- case_eq (eval now e); [ intros v | ]; intros hv; rewrite hv in h.
  + case_eq (eval_list now es); [ intros vs | ]; intros hvs; rewrite hvs in h.
    * injection h; clear h; intro h; subst; simpl.
      now rewrite (hi _ hvs).
    * discriminate h.
  + discriminate h.
Qed.

(** Global state for the statement operational semantic. It is made of
the state for expression evaluation, along with information about know
functions. *)
Definition State := @Map.State Value.t.
Definition EState_of_State (state : State) : EState := RawState state.
Coercion EState_of_State : State >-> EState.

(* Creates an empty state, keeping the function information around. It is
used to create the initial state for a sub-function call. *)
Definition reset_local (now: State) : State := 
  mkState _ Empty (FuncState now).

Fixpoint prepare_call_state (now: EState)
    (arguments: list Value.t)
    (parameters: list (string * Typ.t)) : option EState :=
    match (arguments, parameters) with
    | (nil, nil) => Some now 
    | (v :: arguments, ((name, _) :: parameters)) =>
        prepare_call_state (SMap.add name v now) arguments parameters
    | (_, _) => None
end.

Lemma prepare_call_state_length: forall now arguments parameters after,
  prepare_call_state now arguments parameters = Some after ->
  List.length arguments = List.length parameters.
Proof.
intros now arguments; revert now.
induction arguments as [ | arg arguments hi];
    intros now [ | [? name] parameters]
    after h; simpl in *; try discriminate; [ reflexivity | ].
now rewrite (hi _ _ _ h).
Qed.

Lemma prepare_call_state_same_length: forall now arguments parameters,
  List.length arguments = List.length parameters ->
  exists after, prepare_call_state now arguments parameters = Some after.
Proof.
intros now arguments; revert now.
induction arguments as [ | arg arguments hi]; intros now [ | [name ?] params]
    hlen; simpl in *; try discriminate.
- now exists now.
- injection hlen; intros; subst; clear hlen.
  eapply hi in H as [after h].
  now exists after; exact h.
Qed.

(** * Continuations

Type of continuation, to deal with the execution flow.
- <<KStop>> means that there is nothing left to execute.
- <<KSeq statement k>> is the sequence operator. When the current statement is
  evaluated, continue the evaluation with <<statement>> and then with <<k>>.
- <<KWhile test body orelse k>> means that we are currently evaluating
  a while loop. The payload will drive what happens when we reach the end
  of the loop or a control operator. Once the loop is done, continue with
    <<k>>.
- <<KCall state target k>> means that we are executing a
  sub-routine/function. Before the call, the current state was <<state>>.
  Once the call returns, the output value (if any) will be stored in
  the <<target>> identifier (if any).
  execution then resumes using the <<k>> continuation.
*)
Inductive Cont :=
  | KStop: Cont
  | KSeq: Statement.t -> Cont -> Cont
  | KWhile: forall (test: Expression.t) (body orelse: Statement.t) (k: Cont),
          Cont
  | KCall: forall (call_state: State)
                  (target: option (Lvalue.t * Typ.t))
                  (k: Cont),
                  Cont
.

(** *  Single Step Semantic

    <<sss s st k s' st' k'>> means that in the <<State>> <<s>>, the
    statement <<st>> with contiuation <<k>> will
    evaluation in the statement <<st'>> with continuation <<k'>>. As a
    result the new evaluation <<State>> is <<s'>>.
*)
Inductive sss: State -> Statement.t -> Cont ->
               State -> Statement.t -> Cont -> Prop  :=
    | sssAssign: forall (state: State) id annotation value k v,
        eval state value = Some v ->
        sss state (Statement.Assign (Lvalue.Id id) annotation value) k
            (set state id v) Statement.Pass k
    (* executing a function call yield a fresh new state (and the current
       one is stored in the <<KCall>> continuation *)
    | sssCallFun: forall (state: State) id annotation callee arguments func k
        (vargs: Value.tlist) call_state,
        get_info state callee = Some func ->
        eval_list state (from_list (List.map snd arguments)) = Some vargs ->
        prepare_call_state Empty vargs (parameters func) = Some call_state ->
        sss
          state
          (Statement.Call (Some (id, annotation)) (Expression.Id callee) arguments)
          k
          (mkState _ call_state (FuncState state))
          (body func)
          (KCall state (Some (id, return_annotation func)) k)
    (* executing a procedure call yield a fresh new state (and the current
       one is stored in the <<KCall>> continuation *)
    | sssCallProc: forall (state: State) callee arguments func k
        (vargs: Value.tlist) call_state,
        get_info state callee = Some func ->
        eval_list state (from_list (List.map snd arguments)) = Some vargs ->
        prepare_call_state Empty vargs (parameters func) = Some call_state ->
        sss
          state
          (Statement.Call None (Expression.Id callee) arguments)
          k
          (mkState _ call_state (FuncState state))
          (body func)
          (KCall state None k)
    | sssReturnFun: forall (state call_state: State) ret id annotation k vret,
        eval state ret = Some vret ->
        sss state (Statement.Return ret) (KCall call_state (Some (Lvalue.Id id, annotation)) k)
          (set call_state id vret) Statement.Pass k
    | sssReturnProc: forall (state call_state: State)ret k,
        sss state (Statement.Return ret) (KCall call_state None k)
            call_state Statement.Pass k
    | sssReturnWhile: forall state test body orelse k ret,
        sss state (Statement.Return ret) (KWhile test body orelse k)
            state (Statement.Return ret) k
    | sssReturnSeq: forall state st k ret,
        sss state (Statement.Return ret) (KSeq st k)
            state (Statement.Return ret) k
    (* if some `Return` is spotted at toplevel (outside any function),
       we transition to the standard "exit" state: Pass/KStop *)
    | sssReturnStop: forall state ret,
        sss state (Statement.Return ret) KStop
            state Statement.Pass KStop
    | sssPassProc: forall (state call_state: State) k,
        sss state Statement.Pass (KCall call_state None k)
            call_state Statement.Pass k
    | sssExpression : forall (state: State) expr v k, 
            eval state expr = Some v ->
            sss state (Statement.Expression expr) k state Statement.Pass k
    | sssSeq: forall state st0 st1 k,
            sss state (Statement.Seq st0 st1) k state st0 (KSeq st1 k)
    | sssPassSeq: forall state st k,
            sss state Statement.Pass (KSeq st k) state st k
    | sssWhileTrue: forall (state: State) test body orelse k,
            eval state test = Some (Value.Boolean true) ->
            sss state (Statement.While test body orelse) k
                state body (KWhile test body orelse k)
    | sssWhileFalse: forall (state: State) test body orelse k,
            eval state test = Some (Value.Boolean false) ->
            sss state (Statement.While test body orelse) k
                state orelse k
    | sssPassWhileTrue: forall (state: State) test body orelse k,
            eval state test = Some (Value.Boolean true) ->
            sss state Statement.Pass (KWhile test body orelse k)
                state body (KWhile test body orelse k)
    | sssPassWhileFalse: forall (state: State) test body orelse k,
            eval state test = Some (Value.Boolean false) ->
            sss state Statement.Pass (KWhile test body orelse k)
                state orelse k
    | sssIfTrue: forall test body orelse (state: State) k,
            eval state test = Some (Value.Boolean true) ->
            sss state (Statement.If test body orelse) k
                state body k
    | sssIfFalse: forall test body orelse (state: State) k,
            eval state test = Some (Value.Boolean false) ->
            sss state (Statement.If test body orelse) k
                state orelse k
.

(** Multi step version of sss *)
Inductive sssn: State -> Statement.t -> Cont ->
               State -> Statement.t -> Cont -> Prop  :=
 | sss_refl: forall state st k, sssn state st k state st k
 | sss_trans: forall state st k state' st' k' state'' st'' k'',
         sss state st k state' st' k' ->
         sssn state' st' k' state'' st'' k'' ->
         sssn state st k state'' st'' k''
.

Hint Constructors sss sssn : core.

Lemma sssn_step: forall state st k state' st' k',
  sss state st k state' st' k' ->
  sssn state st k state' st' k'.
Proof.
intros ? ? ? ? ? ?  h.
eapply sss_trans; [now apply h | now apply sss_refl ].
Qed.

Lemma sssn_trans: forall state st k state' st' k' state'' st'' k'',
         sssn state st k state' st' k' ->
         sssn state' st' k' state'' st'' k'' ->
         sssn state st k state'' st'' k''.
Proof.
intros  s st k s' st' k' s'' st'' k'' h; revert s'' st'' k''.
induction h as [ s st k | s st k s' st' k' s'' st'' k'' h hs hi];
        intros z zt l;[  now idtac | ]; intro h''.
eapply sss_trans; [ now apply h | ].
now apply hi.
Qed.

(* begin hide *)
Module Test.
(* Toy examples to convince myself that everything is correct *)
Definition XID : Lvalue.t  := Lvalue.Id "x"%string.
Definition YID : Lvalue.t  := Lvalue.Id "y"%string.
Definition X : Expression.t := Expression.Id "x"%string.
Definition Ret x := Statement.Return x.
(* Identity function, named "int_id", from int to int *)

Definition int_id_parameters := ((("x"%string), Typ.Integer) :: nil).

Definition int_id : func := mkFun "int_id"%string 
  int_id_parameters
  Typ.Integer
  (Ret X).

Definition Int z : Expression.t := Expression.Integer z.

(* Empty value state, with a single function defined: int_id *)
Definition test_state : State :=
  mkState _ Empty (SMap.add "int_id"%string int_id (SMap.empty _)).
(* Value state has a single binding: y |-> 42. Function information
   is the same as test_state *)
Definition final_state : State := set test_state "y"%string (Value.Integer 42).

(* Function call `y := int_id(42)` *)
Definition Prog :=
    Statement.Call (Some (YID, Typ.Integer))
      (Expression.Id "int_id"%string)
      ((None, Int 42) :: nil).

(* Proof that the program `Prog` executes correctly and updates the 
   initial environment `test_state` into `final_state`. *)
Lemma sss_int_id_42:
  sssn test_state Prog KStop final_state Statement.Pass KStop.
Proof.
unfold Prog.
eapply sss_trans.
  apply sssCallFun with (vargs := (Value.Cons (Value.Integer 42) Value.Nil)). 
  unfold get_info, test_state; simpl.
  apply SMap.find_1.
  apply SMap.add_1; reflexivity.

  simpl; now reflexivity.

  simpl; now reflexivity.

simpl.
eapply sss_trans.
  constructor.
  simpl.
  apply SMap.find_1.
  apply SMap.add_1; reflexivity.
now apply sss_refl.
Qed.

(* Function "expect_int" : int -> void *)
Definition expect_int : func := mkFun "expect_int"%string 
  int_id_parameters (* we use the same list of parameters *)
  Typ.None
  Statement.Pass.

(* New test state: Empty value state and a single function definition:
 expect_int *)
Definition test_state2 : State :=
  mkState _ Empty (SMap.add "expect_int"%string expect_int (SMap.empty _)).

(* Function call `expect_int(1664)` *)
Definition Prog2 :=
    Statement.Call None
      (Expression.Id "expect_int"%string)
      ((None, Int 1664) :: nil).

(* Proof that the program `Prog2` executes correctly and do not modify its
   initial environment `test_state2`. *)
Lemma sss_expect_int:
  sssn test_state2 Prog2 KStop test_state2 Statement.Pass KStop.
Proof.
unfold Prog2.
eapply sss_trans.
  apply sssCallProc with (vargs := Value.Cons (Value.Integer 1664) Value.Nil).
  apply SMap.find_1.
  apply SMap.add_1; reflexivity.

  simpl; now reflexivity.

  simpl; now reflexivity.
simpl.
eapply sss_trans.
  now constructor.
now apply sss_refl.
Qed.

Lemma WhileFalse: forall s body orelse k,
    sssn s (Statement.While Expression.False_ body orelse) k s orelse k.
Proof.
intros s body orelse k.
eapply sss_trans.
now apply sssWhileFalse.
now apply sss_refl.
Qed.

End Test.
(* end hide *)

(** Single Step Semantic is a deterministic relation *)
Lemma sss_determinist: forall s0 st0 k0 s1 st1 k1 s2 st2 k2,
    sss s0 st0 k0 s1 st1 k1 ->
    sss s0 st0 k0 s2 st2 k2 ->
    (s1 = s2 /\ st1 = st2 /\ k1 = k2).
Proof.
intros s0 st0 k0 s1 st1 k1 s2 st2 k2 h; revert s2 st2 k2.
destruct h as [
      s id annotation value k v hvalue
    | s id annotation callee arguments func k vargs cstate hf heval hstate
    | s callee arguments func k vargs cstate hf heval hstate
    | s s' ret id annotation k vret heval
    | s s' ret k
    | s test body orelse k ret
    | s st k ret
    | s ret
    | s s' k
    | s expr v k heval
    | s st0 st1 k 
    | s st k
    | s test body orelse k
    | s test body orelse k
    | s test body orelse k
    | s test body orelse k
    | test body orelse s k heval
    | test body orelse s k
]; intros s2 st2  k2 h; try now inversion h.
- inversion h; subst; clear h.
  rewrite hvalue in H7; injection H7; intros; subst.
  now split.
- inversion h; subst; clear h. 
  rewrite hf in H8; injection H8; clear H8; intros; subst.
  rewrite heval in H9; injection H9; clear H9; intros; subst.
  now rewrite hstate in H10; injection H10; clear H10; intros; subst.
- inversion h; subst; clear h. 
  rewrite hf in H1; injection H1; clear H1; intros; subst.
  rewrite heval in H3; injection H3; clear H3; intros; subst.
  now rewrite hstate in H8; injection H8; clear H8; intros; subst.
- inversion h; subst; clear h. 
  now rewrite heval in H8; injection H8; intros; subst.
- inversion h; subst; clear h; [now idtac | ].
  now rewrite H in H8; discriminate H8.
- inversion h; subst; clear h; [ | now idtac ].
  now rewrite H in H8; discriminate H8.
- inversion h; subst; clear h; [ now idtac | ].
  now rewrite H in H8; injection H8; intros; subst.
- inversion h; subst; clear h; [ | now  idtac].
  now rewrite H in H8; injection H8; intros; subst.
- inversion h; subst; clear h; [ now idtac | ].
  now rewrite heval in H7; injection H7; intros; subst.
- inversion h; subst; clear h; [ | now idtac ].
  now rewrite H in H8; injection H8; intros; subst.
Qed.
