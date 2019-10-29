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

Lemma evaluate_int_operator : forall op z, evaluate_unary_operator op (Value.Integer z) =
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
Definition EState := RawState0 Value.t.

Definition get_st_value (st: EState) k := get0 _ st k.

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
            | (Some vl, Some vr) => Some (Value.Boolean (evaluate_comparison_operator vl op vr))
            | (_, _) => None
            end
    | Expression.False_ => Some (Value.Boolean false)
    | Expression.Integer z => Some (Value.Integer z)
    | Expression.List el =>
            match eval_list now el with
            | Some vl => Some (Value.Sequence vl)
            | None => None
            end
    | Expression.Id id => get_st_value now id
    | Expression.Set_ el =>
            match eval_list now el with
            | Some vl => Some (Value.Sequence vl)
            | None => None
            end
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
with eval_list (now: EState) (elist : Expression.tlist) : option Value.tlist :=
  match elist with
  | Expression.Nil => Some (Value.Nil)
  | Expression.Cons hd tl =>
          match (eval now hd, eval_list now tl) with
          | (Some hd, Some tl) => Some (Value.Cons hd tl)
          | (_, _) => None
          end
  end
.

(** * Information about function definitions

We store in here all relevant information about known functions:
- their <<name>>.
- their signature, as a list of <<parameters>> and a <<return annotation>>.
- the actual <<body>> statement of the function.
*)
Record func : Set := mkFun {
    name: string; (* TODO(T53097965): support dotted access for methods *)
    parameters: list (string * Typ.t); (* TODO(T53097965): support more parameter kinds *)
    (* decorators: Expression.t list; *)
    return_annotation: Typ.t; (* no option, set to TNone if need be *)
    (* async: bool; *)
    (* parent: Reference.t option; (1* The class owning the method. *1) *)
    body: Statement.t;
}.

(** Global state for the statement operational semantic. It is made of
the state for expression evaluation, along with information about know
functions. *)
Definition State := RawState Value.t func.

(* Creates an empty state, keeping the function information around. It is
used to create the initial state for a sub-function call. *)
Definition reset_local (now: State) : State := Empty _ _ (info _ _ now).

Fixpoint prepare_call (now: State)
    (arguments: list Value.t)
    (parameters: list (string * Typ.t)) : State :=
    match (arguments, parameters) with
    | (nil, nil) => now 
    | (v :: arguments, ((name, _) :: parameters)) =>
            prepare_call (set _ _ now name v) arguments parameters
    | (_, _) => now (* maybe option state ? So far we don't need it *)
end.

(** * Continuations

Type of continuation, to deal with the execution flow.
- <<KStop>> means that there is nothing left to execute.
- <<KSeq st k>> is the sequence operator. When the current statement is
  evaluated, continue the evaluation with <<st>> and then with <<k>>.
- <<KWhile test body orelse k>> means that we are currently evaluating
  a while loop. The payload will drive what happens when we reach the end
  of the loop or a control operator. Once the loop is done, continue with
    <<k>>.
- <<KCallFun state target annotation k>> means that we are executing a
  sub-routine/function. Before the call, the current state was <<state>>.
  Once the call returns, the output value (if any) will be stored in
  the <<target>> identifier (which should be of type <<annotation>>. The
  execution then resumes using the <<k>> continuation.
-  <<KCallProc state k>> is the same as the <<KCallFun>> continuation for
  "procedure", which are functions that don't return anything, so there is
  no need to keep track of the target.
*)
Inductive Cont : Set :=
  | KStop: Cont
  | KSeq: Statement.t -> Cont -> Cont
  | KWhile: forall (test: Expression.t) (body orelse: Statement.t) (k: Cont),
          Cont
  | KCallFun: forall (call_state: State)
                     (target: Lvalue.t)
                     (annotation: option Typ.t)
                     (k: Cont),
                     Cont
  | KCallProc: forall (call_state: State) (k: Cont), Cont
.

(** *  Single Step Semantic

    <<sss s st k s' st' k'>> means that in the <<State>> <<s>>, the
    statement <<st>> with contiuation <<k>> will
    evaluation in the statement <<st'>> with continuation <<k'>>. As a
    result the new evaluation <<State>> is <<s'>>.
*)
Inductive sss: State -> Statement.t -> Cont ->
               State -> Statement.t -> Cont -> Prop  :=
    | sssAssign: forall s id annotation value k v,
            eval (state _ _ s) value = Some v ->
            sss s (Statement.Assign (Lvalue.Id id) annotation value) k
                (set _ _ s id v) Statement.Pass k
    | sssCallFun: forall s id annotation callee arguments func k
            (vargs: Value.tlist),
            get_info _ _ s callee = Some func ->
            eval_list (state _ _ s) (from_list (List.map snd arguments)) = Some vargs ->
            sss s (Statement.Call (Some id) annotation (Expression.Id callee) arguments) k
               (prepare_call (reset_local s) vargs (parameters func)) (body func) (KCallFun s id annotation k)
    | sssCallProc: forall s callee arguments func k (vargs: Value.tlist),
            get_info _ _  s callee = Some func ->
            eval_list (state _ _ s) (from_list (List.map snd arguments)) = Some vargs ->
            sss s (Statement.Call None None (Expression.Id callee) arguments) k
               (prepare_call (reset_local s) vargs (parameters func)) (body func) (KCallProc s k)
    | sssReturnFun: forall s s' ret id annotation k vret,
            eval (state _ _ s) ret = Some vret ->
            sss s (Statement.Return (Some ret)) (KCallFun s' (Lvalue.Id id) annotation k)
                (set _ _ s' id vret) Statement.Pass k
    | sssReturnProc: forall s s' k, sss s (Statement.Return None) (KCallProc s' k) s' Statement.Pass k
    | sssExpression : forall s expr v k, 
            eval (state _ _ s) expr = Some v ->
            sss s (Statement.Expression expr) k s Statement.Pass k
    | sssSeq: forall s st0 st1 k,
            sss s (Statement.Seq st0 st1) k s st0 (KSeq st1 k)
    | sssPassSeq: forall s st k,
            sss s Statement.Pass (KSeq st k) s st k
    | sssWhileTrue:  forall s test body orelse k,
            eval (state _ _ s) test = Some (Value.Boolean true) ->
            sss s (Statement.While test body orelse) k s body (KWhile test body orelse k)
    | sssWhileFalse: forall s test body orelse k,
            eval (state _ _ s) test = Some (Value.Boolean false) ->
            sss s (Statement.While test body orelse) k s orelse k
    | sssPassWhileTrue: forall s test body orelse k,
            eval (state _ _ s) test = Some (Value.Boolean true) ->
            sss s Statement.Pass (KWhile test body orelse k) s body (KWhile test body orelse k)
    | sssPassWhileFalse: forall s test body orelse k,
            eval (state _ _ s) test = Some (Value.Boolean false) ->
            sss s Statement.Pass (KWhile test body orelse k) s orelse k
    | sssIfTrue: forall test body orelse s k,
            eval (state _ _ s) test = Some (Value.Boolean true) ->
            sss s (Statement.If test body orelse) k s body k
    | sssIfFalse: forall test body orelse s k,
            eval (state _ _ s) test = Some (Value.Boolean false) ->
            sss s (Statement.If test body orelse) k s orelse k
.

(** Multi step version of sss *)
Inductive sssn: State -> Statement.t -> Cont ->
               State -> Statement.t -> Cont -> Prop  :=
 | sss_refl : forall s st k, sssn s st k s st k
 | sss_trans:  forall s st k s' st' k' s'' st'' k'',
         sss s st k s' st' k' -> sssn s' st' k' s'' st'' k'' ->
         sssn s st k s'' st'' k''
.

Hint Constructors sss sssn : core.

Lemma sssn_step: forall s st k s' st' k', sss s st k s' st' k' ->
    sssn s st k s' st' k'.
Proof.
intros ? ? ? ? ? ?  h.
eapply sss_trans; [now apply h | now apply sss_refl ].
Qed.

Lemma sssn_trans: forall s st k s' st' k' s'' st'' k'',
         sssn s st k s' st' k' -> sssn s' st' k' s'' st'' k'' ->
         sssn s st k s'' st'' k''.
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
Definition YID : Lvalue.t  := Lvalue.Id "y"%string.
Definition X : Expression.t := Expression.Id "x"%string.
Definition Ret x := Statement.Return (Some x).
Definition int_id : func := mkFun "int_id"%string 
    ((("x"%string), Typ.Integer) :: nil)
    Typ.Integer
    (Ret X).
Definition Int z : Expression.t := Expression.Integer z.

Definition test_state : State := Empty _ _ 
                   (set_map _ (@empty _) "int_id"%string int_id).
Definition final_state : State :=
    set _ _ test_state "y"%string (Value.Integer 42).

Definition Prog :=
    Statement.Call (Some YID) None (Expression.Id "int_id"%string) ((None, Int 42) :: nil).

(* initial state: value = Empty / func = int_id(x:int) : int { return x }
   program = y := int_id(42)
   final state: value = {y := 42 } / func = int_id(x:int) : int { return x}
*)
Lemma sss_int_id_42: sssn test_state Prog KStop final_state Statement.Pass KStop.
Proof.
unfold Prog.
eapply sss_trans.
  apply sssCallFun.
  simpl; unfold set_map; simpl.
  now reflexivity.

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

(* end hide *)

(** Single Step Semantic is a deterministic relation *)
Lemma sss_determinist: forall s0 st0 k0 s1 st1 k1 s2 st2 k2,
    sss s0 st0 k0 s1 st1 k1 -> sss s0 st0 k0 s2 st2 k2 ->
    (s1 = s2 /\ st1 = st2 /\ k1 = k2).
Proof.
intros s0 st0 k0 s1 st1 k1 s2 st2 k2 h; revert s2 st2 k2.
destruct h as [
      s id annotation value k v hvalue
    | s id annotation callee arguments func k vargs hf heval
    | s callee arguments func k vargs hf heval
    | s s' ret id annotation k vret heval
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
  rewrite hf in H8; injection H8; intros; subst.
  now rewrite heval in H9; injection H9; intros; subst.
- inversion h; subst; clear h. 
  rewrite hf in H2; injection H2; intros; subst.
  now rewrite heval in H7; injection H7; intros; subst.
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
End Test.
