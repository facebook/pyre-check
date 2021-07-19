Require Import List ZArith String.
Require Pyre.Lvalue.
Require Pyre.Value.
Require Pyre.Expression.
Require Import Pyre.Eval.
Require Pyre.Statement.
Require Pyre.Typ.
Require Import Pyre.Map.

Import Pyre.Value.ListHelpers.
Import Pyre.Typ.ListHelpers.
Import Pyre.Expression.ListHelpers.

(** * Typing judgements *)

(** Typing context (Map from identifiers to types).
    The bool parameter is only used in the typing of
    Statement.t 
*)
Definition TContext := SMap.t (bool * Typ.t).
Definition Empty : TContext := SMap.empty _.
Section TypingExpression.

Import Pyre.Expression.

(** Typing judgement for expressions *)
Inductive ok : TContext -> Expression.t -> Typ.t -> Prop :=
  | typFalse: forall context, ok context False_ Typ.Boolean
  | typTrue: forall context, ok context True_ Typ.Boolean
  | typInteger: forall context z, ok context (Integer z) Typ.Integer
  | typString: forall context str, ok context (String str) Typ.String
  | typNone: forall  context,  ok context None Typ.None
  | tyBop: forall context b0 b1 op,
          ok context b0 Typ.Boolean ->
          ok context b1 Typ.Boolean ->
          ok context (BooleanOperator b0 op b1) Typ.Boolean
  | tyCmp: forall context e0 e1 op,
          ok context e0 Typ.Integer ->
          ok context e1 Typ.Integer ->
          ok context (ComparisonOperator e0 op e1) Typ.Boolean
  | TyList: forall context (l: Expression.tlist) ty,
          ok_list context l ty -> 
          ok context (List l) (Typ.List ty)
  | TyTuple: forall context (l: Expression.tlist) lty,
          ok_tuple context l lty ->
          ok context (Tuple l) (Typ.Tuple lty)
  | TyNot: forall context expr,
          ok context expr Typ.Boolean ->
          ok context (UnaryOperator Not expr) Typ.Boolean
  | TyUnary: forall context expr op,
         op <> Not -> 
         ok context expr Typ.Integer ->
         ok context (UnaryOperator op expr) Typ.Integer
  | TyTernary : forall context target test alternative typ,
          ok context target typ ->
          ok context alternative typ ->
          ok context test Typ.Boolean ->
          ok context (Ternary target test alternative) typ
  | TyId: forall context id b T,
          SMap.find id context = Some (b, T) ->
          ok context (Id id) T
with ok_list: TContext -> Expression.tlist -> Typ.t -> Prop :=
  | tyListNil : forall context typ, ok_list context Nil typ
  | tyListCons : forall context hd tl typ,
          ok context hd typ ->
          ok_list context tl typ ->
          ok_list context (Cons hd tl) typ
with ok_tuple : TContext -> Expression.tlist -> Typ.tlist -> Prop :=
  | tyTupleSingl : forall context expr typ,
          ok context expr typ ->
          ok_tuple context (Cons expr Nil) (Typ.Cons typ Typ.Nil)
  | tyTupleCons : forall context expr le typ lt,
          ok context expr typ ->
          ok_tuple context le lt ->
          ok_tuple context (Cons expr le) (Typ.Cons typ lt)
.

(* begin hide *)
Scheme ok_ind' := Induction for ok Sort Prop
  with ok_list_ind' := Induction for ok_list Sort Prop
  with ok_tuple_ind' := Induction for ok_tuple Sort Prop.

Combined Scheme ok_induc from ok_ind', ok_list_ind', ok_tuple_ind'.

Lemma ok_tuple_len: forall context le lt,
    ok_tuple context le lt -> List.length le = List.length lt.
Proof.
intro context.
induction 1; simpl in *; now intuition.
Qed.

Lemma ok_tuple_len_pos : forall context le lt,
    ok_tuple context le lt -> List.length le > 0.
Proof.
intro context.
induction 1; simpl in *; now intuition.
Qed.

Lemma ok_list_spec: forall context l typ,
    ok_list context l typ -> Forall (fun expr => ok context expr typ) l.
Proof.
induction 1 as [ context typ | context hd tl typ h0 h1 hi ];
    [ now apply Forall_nil | ].
now apply Forall_cons.
Qed.

Lemma ok_list_forall: forall context (l: Expression.tlist) typ,
    Forall (fun expr => ok context expr typ) l -> ok_list context l typ.
Proof.
intro context.
induction l as [ | hd tl hi]; intros typ h; inversion h; subst; clear h.
- now constructor.
- apply hi in H2.
  now constructor.
Qed.

Lemma ok_tuple_spec: forall context l lt, ok_tuple context l lt ->
    List.length l > 0 /\
    List.length l = List.length lt /\
    Forall (fun et => ok context (fst et) (snd et)) (List.combine l lt).
Proof.
induction 1 as [context expr typ h | context expr typ es et h hs hi ].
- intuition.
  now apply Forall_cons.
- now simpl; intuition.
Qed.

Lemma ok_tuple_forall: forall context (l: Expression.tlist) (lt: Typ.tlist),
    List.length l > 0 ->
    List.length l = List.length lt ->
    Forall (fun et => ok context (fst et) (snd et)) (List.combine l lt) ->
    ok_tuple context l lt.
Proof.
induction l as [ | hd tl hi];
       intros lt hp hl h; inversion h; subst; clear h; simpl in *.
- now idtac.
- destruct lt as [ | typ typs]; [now idtac | ]. 
  simpl in H0.
  discriminate H0.
- destruct x as [v' t']; simpl in *.
  destruct lt as [ | typ typs]; [now idtac | ]. 
  injection hl; clear hl; intro hl.
  simpl in H; injection H; clear H; intros heq0 heq1; subst.
  (* special case for list of size 1 *)
  case_eq tl.
  + intros ? h; subst.
    simpl in hl.
    destruct typs as [ | ]; [ | now idtac].
    now constructor.
  + intros ? ? htl ?; subst.
    constructor; [ now idtac | ].
    now apply hi; simpl; intuition.
Qed.

Lemma ok_tuple_forall2: forall context (l: Expression.tlist) (lt: Typ.tlist),
    List.length lt > 0 ->
    List.length l = List.length lt ->
    Forall (fun et => ok context (fst et) (snd et)) (List.combine l lt) ->
    ok_tuple context l lt. 
Proof.
intros context l lt h heq hf.
now apply ok_tuple_forall; intuition.
Qed.
(* end hide *)

End TypingExpression.

Hint Constructors ok ok_list ok_tuple : core.

Section TypingValue.

Import Value.

(** Typing judgement for values *)
Inductive okv : Value.t -> Typ.t -> Prop :=
  | TyVBoolean: forall b, okv (Boolean b) Typ.Boolean
  | TyVInteger: forall i, okv (Integer i) Typ.Integer
  | TyVString: forall str, okv (String str) Typ.String
  | TyVList: forall (l: Value.tlist) T,
          okv_list l T ->
          okv (Sequence l) (Typ.List T)
  | TyVTuple: forall (l: Value.tlist) (lt: Typ.tlist),
          okv_tuple l lt ->
          okv (Sequence l) (Typ.Tuple lt)
  | TyNone: okv Value.None Typ.None
with okv_list: Value.tlist -> Typ.t -> Prop :=
  | TyVNil: forall typ, okv_list Nil typ
  | TyVCons: forall hd tl typ,
          okv hd typ ->
          okv_list tl typ ->
          okv_list (Cons hd tl) typ
with okv_tuple: Value.tlist -> Typ.tlist -> Prop :=
  | TyVSingle: forall expr typ,
          okv expr typ ->
          okv_tuple (Cons expr Nil) (Typ.Cons typ Typ.Nil)
  | TyVTuples: forall expr typ es typs,
          okv expr typ ->
          okv_tuple es typs ->
          okv_tuple (Cons expr es) (Typ.Cons typ typs)
.

(* begin hide *)
Lemma okv_list_spec: forall lv typ, okv_list lv typ ->
    Forall (fun v => okv v typ) lv.
Proof.
induction 1 as [ | hd tl typ h0 h1 hi ]; [ now apply Forall_nil | ].
now apply Forall_cons.
Qed.

Lemma okv_list_forall: forall (lv: Value.tlist) typ,
    Forall (fun v => okv v typ) lv -> okv_list lv typ.
Proof.
induction lv as [ | hd tl hi]; intros typ h; inversion h; subst; clear h.
- now constructor.
- apply hi in H2.
  now constructor.
Qed.

Lemma okv_tuple_spec: forall lv lt, okv_tuple lv lt ->
    List.length lv > 0 /\
    List.length lv = List.length lt /\
    Forall (fun vt => okv (fst vt) (snd vt)) (List.combine lv lt).
Proof.
induction 1 as [ expr typ h | expr typ es et h hs hi ].
- intuition.
  now apply Forall_cons.
- now simpl; intuition.
Qed.

Lemma okv_tuple_forall: forall (lv: Value.tlist) (lt: Typ.tlist),
    List.length lv > 0 ->
    List.length lv = List.length lt ->
    Forall (fun vt => okv (fst vt) (snd vt)) (List.combine lv lt) ->
    okv_tuple lv lt.
Proof.
induction lv as [ | hd tl hi];
       intros lt hp hl h; inversion h; subst; clear h; simpl in *.
- now idtac.
- destruct lt as [ | typ typs]; [now idtac | ]. 
  simpl in H0.
  discriminate H0.
- destruct x as [v' t']; simpl in *.
  destruct lt as [ | typ typs]; [now idtac | ]. 
  injection hl; clear hl; intro hl.
  simpl in H; injection H; clear H; intros heq0 heq1; subst.
  (* special case for list of size 1 *)
  case_eq tl.
  + intros ? h; subst.
    simpl in hl.
    destruct typs as [ | ]; [ | now idtac].
    now constructor.
  + intros ? ? htl ?; subst.
    constructor; [ now idtac | ].
    now apply hi; simpl; intuition.
Qed.

Lemma okv_tuple_forall2: forall (lv: Value.tlist) (lt: Typ.tlist),
    List.length lt > 0 ->
    List.length lv = List.length lt ->
    Forall (fun vt => okv (fst vt) (snd vt)) (List.combine lv lt) ->
    okv_tuple lv lt.
Proof.
intros lv lt h heq hf.
now apply okv_tuple_forall; intuition.
Qed.
(* end hide *)

End TypingValue.

Hint Constructors okv okv_list okv_tuple : core.

(** Well-formed <<EState>> w.r.t. some <<TContext>>:
    for any typed variable in the context, there must be a value of the
    right type associated to in the the state. *)
Definition well_formed_estate (state : EState) (context: TContext) : Prop :=
    (forall k b T, SMap.find k context = Some (b, T) ->
     exists v, SMap.find k state = Some v /\ okv v T)
.

(* begin hide *)
Lemma well_formed_estate_get: forall state context,
  well_formed_estate state context ->
  forall id b T, SMap.find id context = Some (b, T) ->
  exists v, SMap.find id state = Some v /\ okv v T.
Proof.
intros s context hwf k T.
now apply hwf.
Qed.

Lemma ty_bool_int : forall context expr typ, ok context expr typ ->
    forall state, well_formed_estate state context ->
    (typ = Typ.Boolean -> exists b, eval state expr = Some (Value.Boolean b)) /\
    (typ = Typ.Integer -> exists z, eval state expr = Some (Value.Integer z)).
Proof.
induction 1 as [
    | context
    | context z
    | context str
    | context
    | context left right op hl hil hr hir
    | context left right op hl hil hr hir
    | context l typ hl
    | context l typs hl
    | context expr he hie
    | context expr op hop he hie
    | context target test alterntaive typ htarget hitarget
            halt hialt htest hitest
    | context id b T h
]; intros state hwf; subst; split; intro heq; subst;
   try discriminate; simpl in *.
+ now exists false.
+ now exists true.
+ now exists z.
+ destruct (hil state hwf) as [h0 _].
  destruct (hir state hwf) as [h1 _].
  destruct (h0 refl_equal) as [x0 hx0].
  destruct (h1 refl_equal) as [x1 hx1].
  rewrite hx0, hx1.
  now exists (evaluate_boolean_operator x0 op x1).
+ destruct (hil state hwf) as [_ h0].
  destruct (hir state hwf) as [_ h1].
  destruct (h0 refl_equal) as [x0 hx0].
  destruct (h1 refl_equal) as [x1 hx1].
  rewrite hx0, hx1.
  now exists (evaluate_comparison_operator (Value.Integer x0) op (Value.Integer x1)).
+ destruct (hie state hwf) as [h0 _]. 
  destruct (h0 refl_equal) as [ x0 hx0 ].
  rewrite hx0.
  now exists (negb x0).
+ destruct (hie state hwf) as [_ h0].
  destruct (h0 refl_equal) as [ x0 hx0 ].
  rewrite hx0; simpl.
  rewrite evaluate_unary_int_operator; destruct op; try (now elim hop).
  * now exists (-x0 -1)%Z.
  * now exists ( - x0 )%Z.
  * now exists x0.
+ destruct (hitarget state hwf) as [h0 _].
  destruct (hialt state hwf) as [h1 _].
  destruct (hitest state hwf) as [h2 _].
  destruct (h0 refl_equal) as [x0 hx0].
  destruct (h1 refl_equal) as [x1 hx1].
  destruct (h2 refl_equal) as [x2 hx2].
  rewrite hx2; destruct x2. 
  * rewrite hx0.
    now exists x0.
  * rewrite hx1.
    now exists x1.
+ destruct (hitarget state hwf) as [_ h0].
  destruct (hialt state hwf) as [_ h1].
  destruct (hitest state hwf) as [h2 _].
  destruct (h0 refl_equal) as [x0 hx0].
  destruct (h1 refl_equal) as [x1 hx1].
  destruct (h2 refl_equal) as [x2 hx2].
  rewrite hx2; destruct x2. 
  * rewrite hx0.
    now exists x0.
  * rewrite hx1.
    now exists x1.
+ apply well_formed_estate_get with (state := state) in h; [ | now idtac ].
  destruct h as [ v [-> hv]].
  inversion hv; subst; clear hv.
  now exists b0.
+ apply well_formed_estate_get with (state := state) in h; [ | now idtac ].
  destruct h as [ v [-> hv]].
  inversion hv; subst; clear hv.
  now exists i.
Qed.
(* end hide *)

(** Every expression typed as <<Typ.Boolean>> must  be an actual <<bool>> *)
Lemma ty_bool : forall context expr, ok context expr Typ.Boolean ->
    forall state, well_formed_estate state context ->
    exists b, eval state expr = Some (Value.Boolean b).
Proof.
intros context expr h s hwf; eapply ty_bool_int in h as [ h0 _].
- now apply h0.
- now apply hwf.
Qed.

(** Every expression typed as <<Typ.Integer>> must  be an actual <<Z>> *)
Lemma ty_int : forall context expr, ok context expr Typ.Integer ->
    forall state, well_formed_estate state context ->
    exists z, eval state expr = Some (Value.Integer z).
Proof.
intros context expr h s hwf; eapply ty_bool_int in h as [ _ h0].
- now apply h0.
- now apply hwf.
Qed.

(* begin hide *)
Lemma ty_string_ : forall context expr typ, ok context expr typ -> 
    typ = Typ.String ->
    forall state, well_formed_estate state context ->
    exists str, eval state expr = Some (Value.String str).
Proof.
induction 1 as [
    | context
    | context z
    | context str
    | context
    | context left right op hl hil hr hir
    | context left right op hl hil hr hir
    | context l typ hl
    | context l typs hl
    | context expr he hie
    | context expr op hop he hie
    | context target test alterntaive typ htarget hitarget
            halt hialt htest hitest
    | context id b T h
] ; intros heq state hwf; subst; try discriminate; simpl in *.
- now exists str.
- apply ty_bool with (state := state) in htest as [ b hb ];[ | now idtac ].
  rewrite hb.
  destruct (hitarget refl_equal state hwf) as [s1 hs1].
  destruct (hialt refl_equal state hwf) as [s2 hs2].
  rewrite hs1, hs2.
  destruct b.
  + now exists s1.
  + now exists s2.
- apply well_formed_estate_get with (state := state) in h; [ | now idtac].
  destruct h as [v [-> hv]].
  inversion hv; subst; clear hv.
  now exists str.
Qed.
(* end hide *)

(** Every expression typed as <<Typ.String>> must  be an actual <<string>> *)
Lemma ty_string : forall context expr, ok context expr Typ.String ->
    forall state, well_formed_estate state context ->
    exists str, eval state expr = Some (Value.String str).
Proof.
intros context expr h state hwf.
now apply ty_string_ with (context := context) (state := state) in h.
Qed.

(* begin hide *)
Lemma ty_none_: forall context expr typ, ok context expr typ ->
    typ = Typ.None -> forall state, well_formed_estate state context ->
    eval state expr = Some Value.None.
Proof.
induction 1 as [
    | context
    | context z
    | context str
    | context
    | context left right op hl hil hr hir
    | context left right op hl hil hr hir
    | context l typ hl
    | context l typs hl
    | context expr he hie
    | context expr op hop he hie
    | context target test alterntaive typ htarget hitarget
            halt hialt htest hitest
    | context id b T h
] ; intros heq state hwf; subst; try discriminate; simpl in *.
- reflexivity.
- apply ty_bool with (state := state) in htest as [ b hb ];[ | now idtac ].
  rewrite hb.
  rewrite (hitarget refl_equal state hwf).
  rewrite (hialt refl_equal state hwf).
  now destruct b.
- apply well_formed_estate_get with (state := state) in h; [ | now idtac].
  destruct h as [v [-> hv]].
  inversion hv; subst; clear hv.
  reflexivity.
Qed.
(* end hide *)

(** Every expression typed as <<Typ.None>>  must be <<None>> *)
Lemma ty_none: forall context expr, ok context expr Typ.None ->
    forall state, well_formed_estate state context ->
    eval state expr = Some Value.None.
Proof.
intros  context expr h state hwf.
now apply ty_none_ with (context := context) (state := state) in h.
Qed.

Definition is_sequence (typ : Typ.t) : option Typ.t :=
    match typ with
    | Typ.List T => Some T
    | _ => None
    end.

Definition is_tuple (typ : Typ.t) : list Typ.t :=
    match typ with
    | Typ.Tuple l => l
    | _ => nil
end.

Section TypingStatement.

Import Pyre.Statement.

(* TODO(T53097965):
   use the strings in a correct way. For the moment they are totally
   discarded and I consider that the order of arguments matches the
   the order of parameters: we only consider positional arguments.
 *)

(** Takes a list of arguments and a list of function parameter definitions 
    and (pairwise) checks that expressions are typed with the corresponding type, 
    in context <<context>>. *)
Fixpoint check_arguments context (arguments: list (option string * Expression.t))
    (parameters: list (string * Typ.t)) : Prop :=
    match (arguments, parameters) with
    | ((_, arg) :: arguments, (_, ty) :: parameters) =>
            ok context arg ty /\ check_arguments context arguments parameters
    | (nil, nil) => True
    | _ => False
    end.

Lemma check_arguments_length: forall context arguments parameters,
  check_arguments context arguments parameters ->
  List.length arguments = List.length parameters.
Proof.
intro context; induction arguments as [| [ ? arg] arguments hi];
    intros [| [name ?] parameters] h; simpl in *; try now elim h.
destruct h as [hok hc].
now rewrite (hi _ hc).
Qed.

(** Helper to prepare the typing context of sub-routine/function calls *)
Fixpoint prepare_call_context (now: TContext)
    (parameters: list (string * Typ.t)) : TContext :=
    match parameters with
    | nil => now
    | (name, ty) :: parameters =>
        prepare_call_context (SMap.add name (true, ty) now) parameters
end.

(** General context for type checking statements: it is made of an expression
    typing context and some information about declared function *)
Definition Context := @Map.State (bool * Typ.t).
Definition TContext_of_Context (context : Context) : TContext := RawState context.
Coercion TContext_of_Context : Context >-> TContext.

Definition mkContext context info : Context :=
  mkState _ context info.

(** Typing judgement for statements

    <<typ C0 T st C1 >> means that in the typing context <<C0>>, evaluating the
    statement <<st>> will generate the context <<C1>>.
    <<T>> is the current expected return type (or None in case of procedure /
    global statement).
*)
Inductive typ : Context -> Typ.t -> Statement.t -> Context -> Prop :=
  | typAssignIdNone: forall (context: Context) return_type id value T,
      SMap.find id context = None ->
      ok context value T ->
      typ context return_type
      (Assign (Lvalue.Id id) None value) (set context id (false, T))
  | typAssignIdNoneWeak: forall (context: Context) return_type id value T Tid,
      SMap.find id context = Some (false, Tid) ->
      ok context value T ->
      typ context return_type
      (Assign (Lvalue.Id id) None value) (set context id (false, T))
  | typAssignIdNoneStrong: forall (context: Context) return_type id value T,
      SMap.find id context = Some (true, T) ->
      ok context value T ->
      typ context return_type
      (Assign (Lvalue.Id id) None value) (set context id (true, T))
  | typAssignIdAnnot: forall (context: Context) return_type id value T,
      ok context value T ->
      typ context return_type
      (Assign (Lvalue.Id id) (Some T) value) (set context id (true, T))
  | typExpression: forall (context: Context) return_type expr T,
      ok context expr T ->
      typ context return_type (Expression expr) context
  | typIf: forall (context context': Context) return_type test body orelse,
      ok context test Typ.Boolean ->
      typ context return_type body context' ->
      typ context return_type orelse context' ->
      typ context return_type (If test body orelse) context'
  | tyPass: forall (context: Context) return_type,
      typ context return_type Pass context
  | tySeq: forall (context context' context'': Context) return_type st next,
      typ context return_type st context' ->
      typ context' return_type next context'' ->
      typ context return_type (Seq st next) context''
  | typWhile: forall (context context': Context) return_type test body orelse,
      ok context test Typ.Boolean ->
      typ context return_type body context ->
      typ context return_type orelse context' ->
      typ context return_type (While test body orelse) context'
  | typCallFun: forall (context: Context) return_type target annotation
      function_name arguments function_def,
      get_info context function_name = Some function_def ->
      return_annotation function_def = annotation ->
      check_arguments context arguments (parameters function_def) ->
      typ
        context
        return_type
        (Call (Some (Lvalue.Id target, annotation)) (Expression.Id function_name) arguments)
        (set context target (true, return_annotation function_def))
  | typCallProc: forall (context: Context) return_type function_name arguments
      function_def,
      get_info context function_name = Some function_def ->
      return_annotation function_def = Typ.None ->
      check_arguments context arguments (parameters function_def) ->
      typ context return_type (Call None (Expression.Id function_name) arguments) context
  | typReturn: forall (context context': Context) ret T,
          ok context ret T ->
          typ context T (Return ret) context'
.

(** Well-formed conditions for function definition: <<function_info>> is a
    set of defined functions. In this context, a function <<f>> is
    well-formed if its body is correctly typed w.r.t.  its arguments types
    and return annotation. *)
Definition well_formed_function function_info (f: func) : Prop :=
  (exists return_context,
    typ (mkContext (prepare_call_context Empty (parameters f)) function_info)
        (return_annotation f)
        (body f) return_context)
.

(** General well-formed statement for State/Context:
    - The underlying evaluation state / typing context must be well-formed.
    - They must store the same information about function definitions.
    - Every function definition recorded so far must be well-formed.
*)
Definition well_formed (state: Eval.State) (context: Context) : Prop :=
    (* underlying states are coherent *)
    well_formed_estate state context /\
    (* same function definitions in both context *)
    (FuncState state = FuncState context) /\
    (* functions are correctly typed *)
    (forall function_name function_def,
      get_info context function_name = Some function_def ->
      well_formed_function (FuncState context) function_def)
.

(* begin hide *)
Lemma well_formed_get: forall state context, well_formed state context ->
    forall id b T, SMap.find id context = Some (b, T) ->
    exists v, SMap.find id state = Some v /\ okv v T.
Proof.
intros [s ?] [context ?] [hwf ?] k T h; simpl in *.
now apply hwf.
Qed.

Lemma well_formed_info_eq: forall state context, well_formed state context ->
  FuncState state = FuncState context.
Proof.
intros s context h; now apply h.
Qed.

Lemma well_formed_function_get: forall state context,
  well_formed state context ->
  forall function_name function_def,
  get_info context function_name = Some function_def ->
  well_formed_function (FuncState context) function_def.
Proof.
intros s context h f func hg; now apply h in hg.
Qed.

Lemma well_formed_get_info: forall state context,
  well_formed state context ->
  forall function_name,
  get_info context function_name = get_info state function_name.
Proof.
intros s context [? [h ?]] f; unfold get_info.
now rewrite h.
Qed.

Lemma well_formed_function_get_alt: forall state context,
  well_formed state context ->
  forall function_name function_def,
  get_info state function_name = Some function_def ->
  well_formed_function (FuncState state) function_def.
Proof.
intros s context h f  func hg.
rewrite (well_formed_info_eq _ _ h).
apply well_formed_function_get with s f; intuition.
now rewrite well_formed_get_info with (state := s).
Qed.
(* end hide *)

(** Typing judgement for continuations

    <<ktyp C0 T k>> encodes the fact that continuation <<k>> will correctly
    execute under the context <<C0>>. The type <<T>> is the current return
    type, or Typ.None if a procedure or a global statement is involved.
*)
Inductive ktyp : Context -> Typ.t -> Cont -> Prop :=
    | ktypKStop : forall context return_type,
        ktyp context return_type KStop
    | ktypSeq: forall context context' return_type st k,
        typ context return_type st context' ->
        ktyp context' return_type k -> 
        ktyp context return_type (KSeq st k)
    | ktypKWhile: forall (context context': Context) return_type test
        body orelse k,
        ok context test Typ.Boolean ->
        typ context return_type body context ->
        typ context return_type orelse context' ->
        ktyp context' return_type k ->
        ktyp context return_type (KWhile test body orelse k)
    | ktypKCallProc: forall context call_context return_type saved_state k,
        well_formed saved_state call_context ->
        ktyp call_context return_type k ->
        ktyp context Typ.None (KCall saved_state None k)
    | ktypKCallFun: forall context call_context return_type saved_state target
        annotation k,
        well_formed saved_state (remove call_context target) ->
        SMap.find target call_context = Some (true, annotation) ->
        ktyp call_context return_type k ->
        ktyp context annotation (KCall saved_state (Some (Lvalue.Id target, annotation)) k)
.

End TypingStatement.

Hint Constructors typ ktyp : core.

(** * Type soundness

    Proof that a correctly typed expression always evaluate to a value,
    and this value has the same type as the input expression.
*)

Lemma eval_ok_ :
    (forall context expr typ, ok context expr typ ->
        forall state, well_formed_estate state context ->
        exists v, eval state expr = Some v /\  okv v typ) /\
    (forall context le typ, ok_list context le typ ->
        forall state, well_formed_estate state context ->
        exists lv, eval_list state le = Some lv /\
        okv_list lv typ) /\
    (forall context le lt, ok_tuple context le lt ->
        forall state, well_formed_estate state context ->
        exists lv, eval_list state le = Some lv /\ okv_tuple lv lt).
Proof.
apply ok_induc.
(* ok *)
- intros context state hwf; subst; exists (Value.Boolean false); now split.
- intros context state hwf; subst; exists (Value.Boolean true); now split.
- intros context z state hwf; subst; exists (Value.Integer z); now split.
- intros context str state hwf; subst; exists (Value.String str); now split.
- intros context state hwf; subst; exists Value.None; now split.
- intros context b0 b1 op h0 ? h1 ? state hwf; simpl; subst.
  apply ty_bool with (state := state) in h0; [ | now idtac].
  destruct h0 as [vb0 ->].
  apply ty_bool with (state := state) in h1; [ | now idtac ].
  destruct h1 as [vb1 ->].
  now eexists; split; [ reflexivity | idtac].
- intros context e0 e1 op h0 ? h1 ? state hwf; simpl; subst.
  apply ty_int with (state := state) in h0; [ | now idtac ].
  destruct h0 as [v0 ->].
  apply ty_int with (state := state) in h1; [ | now idtac ].
  destruct h1 as [v1 ->].
  now eexists; split; [ reflexivity | idtac].
- intros context l ty h hi state hwf; simpl.
  destruct (hi _ hwf) as  [vl [-> h0]].
  now eexists; split; [reflexivity  | constructor ].
- intros context l ty h hi state hwf; simpl.
  destruct (hi _ hwf) as  [vl [-> h0]].
  now eexists; split; [reflexivity  | constructor ].
- intros context expr h hi state hwf; simpl.
  apply ty_bool with (state := state) in h as [v ->]; [ | now idtac ].
  now eexists; split; [reflexivity | ].
- intros context expr op hop h hi state hwf; simpl.
  apply ty_int with (state := state) in h as [v ->]; [ | now idtac ].
  rewrite evaluate_unary_int_operator.
  destruct op; try (now elim hop); now eexists; split; [reflexivity | ].
- intros context target test alt typ htarget hitarget halt hialt htest hitest
    state hwf; simpl.
  apply ty_bool with (state := state) in htest as [b ->]; [ | now idtac ].
  destruct (hitarget _ hwf) as [vt [-> hvt]].
  destruct (hialt _ hwf) as [va [-> hva]].
  now destruct b; eexists; split; [ reflexivity | | reflexivity | ].
- intros context id b typ hg state hwf; simpl.
  now apply well_formed_estate_get with (context := context) (b := b);
    [ now apply hwf | ].
(* ok_list *)
- intros context typ state hwf; simpl.
  now exists Value.Nil; intuition.
- intros context hd tl typ h0 hi0 h1 hi1 state hwf; simpl.
  destruct (hi0 _ hwf) as [vhd [-> hv]].
  destruct (hi1 _ hwf) as [vtl [-> ht]].
  now exists (Value.Cons vhd vtl); intuition.
(* ok_tuple *)
- intros context expr typ h hi state hwf; simpl.
  destruct (hi _ hwf) as [v [-> ?]].
  now  exists (Value.Cons v Value.Nil); intuition.
- intros context expr el typ tl h0 hi0 h1 hi1 state hwf; simpl.
  destruct (hi0 _ hwf) as [vhd [-> ?]].
  destruct (hi1 _ hwf) as [vtl [-> ?]].
  now exists (Value.Cons vhd vtl); intuition.
Qed.

Lemma eval_ok: forall context expr typ, ok context expr typ ->
    forall state, well_formed_estate state context ->
    exists v, eval state expr = Some v /\ okv v typ.
Proof.
intros.
now eapply eval_ok_; eauto.
Qed.

Lemma eval_list_ok: forall context le typ, ok_list context le typ ->
    forall state, well_formed_estate state context ->
    exists lv, eval_list state le = Some lv /\ okv_list lv typ.
Proof.
intros.
now eapply eval_ok_; eauto.
Qed.

Lemma eval_tuple_ok: forall context le lt, ok_tuple context le lt ->
    forall state, well_formed_estate state context ->
    exists lv, eval_list state le = Some lv /\ okv_tuple lv lt.
Proof.
intros.
now eapply eval_ok_; eauto.
Qed.

Lemma ok_sequence : forall context expr T, ok context expr T ->
    forall T', is_sequence T = Some T' ->
    forall state, well_formed_estate state context ->
    exists lv, eval state expr = Some (Value.Sequence lv).
Proof.
induction 1 as [
    | context
    | context z
    | context str
    | context
    | context left right op hl hr
    | context left right op hl hr
    | context l typ hl
    | context l typs hl
    | context expr he
    | context expr op hop he
    | context target test alterntaive typ htarget hit halt hia htest hitest
    | context id b T h
]; intros T' hT state hwf; simpl; subst;
  try (unfold is_sequence in hT; simpl in hT; discriminate).
- apply eval_list_ok with (state := state) in hl as [lv [-> ?]]; [ | now idtac ].
  now exists lv.
- destruct (hit T' hT state hwf) as [l1 ->].
  destruct (hia T' hT state hwf) as [l2 ->].
  apply ty_bool with (state := state) in htest as [b ->]; [ | now idtac ].
  now destruct b; eexists; reflexivity.
- apply well_formed_estate_get with (state := state) in h as [v [-> h1]];
    [ | now idtac ].
  inversion h1; subst; clear h1; try discriminate; now eexists; reflexivity.
Qed.

Lemma ty_list: forall context expr T, ok context expr (Typ.List T) ->
    forall state, well_formed_estate state context ->
    exists lv, eval state expr = Some (Value.Sequence lv) /\
               okv (Value.Sequence lv) (Typ.List T).
Proof.
intros context expr T hok state hwf.
apply eval_ok with (state := state) in hok; [ | now idtac].
destruct hok as [v [-> hv]].
inversion hv; subst; clear hv.
exists l; split; [ now idtac | ].
now constructor.
Qed.

(* begin hide *)
Lemma well_formed_estate_add: forall state context,
  well_formed_estate state context ->
  forall id b v T, okv v T ->
  well_formed_estate (SMap.add id v state) (SMap.add id (b, T) context).
Proof.
intros state context hwf id b v T hok.
intros k b' S.
rewrite !find_add_smap.
destruct String.eqb.
- intro h; injection h; clear h; intros; subst.
  exists v; split; [ | assumption ].
  reflexivity.
- now apply hwf.
Qed.

Lemma well_formed_set: forall state context,
  well_formed state context ->
  forall id b v T, okv v T ->
  well_formed (set state id v) (set context id (b, T)).
Proof.
intros state context hwf id b v T hok; split; [| split].
- now destruct hwf; apply well_formed_estate_add.
- now apply hwf.
- intros f func; simpl.
  now apply hwf.
Qed.

(* Helper lemma to build valid context and state for prepare_call_*:
   In a well_formed global context/state, if some arguments are correctly
   typed, then calling prepare_call_* on a well formed context/state pair leads
   to a new well formed context/state pair.

   We'll then use this function with the trival "empty" pair to build 
   the context/state we need for function calls *)
Lemma well_formed_estate_prepare_:
  (* current context/state is well formed *)
  forall current_context current_state,
  well_formed_estate current_state current_context ->
  (* arguments are typed by parameters *)
  forall arguments parameters,
  check_arguments current_context arguments parameters ->
  forall value_arguments,
  eval_list current_state (from_list (map snd arguments)) = Some value_arguments ->
  (* updating a well formed pair leads to a new well formed pair *)
  forall state context new_state,
  well_formed_estate state context ->
  prepare_call_state state value_arguments parameters = Some new_state ->
  well_formed_estate new_state (prepare_call_context context parameters).
Proof.
intros current_context current_state hwf_current.
induction arguments as [ | [? expr] arguments hi];
    intros parameters hcheck value_arguments heval state context new_state hwf hprep;
    simpl in *.
- destruct parameters as []; [ | now elim hcheck ]; simpl.
  injection heval; intro; subst; clear heval; simpl in hprep.
  now injection hprep; intro; subst; clear hprep.
- destruct parameters as [ | [ ? ty] parameters ]; [now elim hcheck | ]; simpl.
  case_eq (eval current_state expr);
    [intros v hv | intro hn; rewrite hn in heval; discriminate heval ].
  rewrite hv in heval.
  case_eq (eval_list current_state (from_list (map snd arguments)));
    [intros vargs hvs | intro hn; rewrite hn in heval; discriminate heval ].
  rewrite  hvs in heval; injection heval; clear heval; intro hvargs;
    subst; simpl.
  destruct hcheck as [hok hcheck].
  simpl in hprep.
  apply (hi _ hcheck _ hvs (SMap.add s v state) _); [ | assumption ].
  apply well_formed_estate_add; [ now idtac | ].
  apply eval_ok with (state := current_state) in hok; [ | now idtac ].
  destruct hok as [w [h1 h2]].
  now rewrite hv in h1; injection h1; clear h1; intro h1; subst.
Qed.

Lemma well_formed_estate_prepare:
  forall context state arguments parameters value_arguments,
  well_formed_estate state context ->
  check_arguments context arguments parameters ->
  eval_list state (from_list (map snd arguments)) = Some value_arguments ->
  forall new_state,
  prepare_call_state Eval.Empty value_arguments parameters =  Some new_state ->
  well_formed_estate new_state (prepare_call_context Empty parameters).
Proof.
intros context state arguments parameters value_arguments hwf hcheck heval 
  new_state hprep.
eapply well_formed_estate_prepare_.
- now apply hwf.
- now apply hcheck.
- now apply heval.
- intros k b T h; discriminate h.
- now apply hprep.
Qed.

Lemma well_formed_estate_add_remove: forall state context,
  well_formed_estate state context ->
  forall id b T,
  well_formed_estate state (SMap.remove id (SMap.add id (b, T) context)).
Proof.
intros s ctx hwf id b T id' b' T'; simpl.
rewrite find_remove_smap.
case_eq (id' =? id)%string; intros heq; [ intro; discriminate | ].
rewrite find_add_smap, heq; intro hg.
now apply hwf in hg.
Qed.
(* end hide *)

(** * Type Preservation

    Under a well-formed context/state, if a statement and a continuation
    are correctly typed, then any operational step leads to new correctly
    typed statement and resolution, and the resulting states are still
    well-formed.
*)
Lemma sss_preservation: forall context context' state state' stmt stmt'
  k k' return_type,
  typ context return_type stmt context' ->
  ktyp context' return_type k ->
  well_formed state context ->
  sss state stmt k state' stmt' k' ->
  exists ocontext ocontext' return_type',
  typ ocontext return_type' stmt' ocontext' /\
  ktyp ocontext' return_type' k' /\
  well_formed state' ocontext.
Proof.
intros context context' state state' stmt stmt' k k' return_type h;
  revert state state' stmt' k k';
destruct h as [
    context return_type id expr T hget okvalue
  | context return_type id expr T Tid hget okvalue
  | context return_type id expr T hget okvalue
  | context return_type id expr T okvalue
  | context return_type expr T h
  | context context' return_type test body orelse oktest hbody horlese
  | context return_type
  | context context' context'' return_type stmt next hst hnext
  | context context' return_type test body orlelse hok hbody horelse
  | context return_type target annotation f arguments func hg hret hcheck
  | context return_type f arguments func hg hret hcheck
  | context context' ret T hok
]; intros s s' stmt' k k' hk hwf hs.
(* Assign *)
- destruct (eval_ok _ _ _ okvalue s (proj1 hwf)) as [v [hv0 hv1]].
  inversion hs; subst; clear hs.
  exists (set context id (false, T)); exists (set context id (false, T));
    exists return_type; intuition.
  rewrite hv0 in H7; injection H7; clear H7; intros ?; subst.
  now apply well_formed_set.
- destruct (eval_ok _ _ _ okvalue s (proj1 hwf)) as [v [hv0 hv1]].
  inversion hs; subst; clear hs.
  exists (set context id (false, T)); exists (set context id (false, T));
    exists return_type; intuition.
  rewrite hv0 in H7; injection H7; clear H7; intros ?; subst.
  now apply well_formed_set.
- destruct (eval_ok _ _ _ okvalue s (proj1 hwf)) as [v [hv0 hv1]].
  inversion hs; subst; clear hs.
  exists (set context id (true, T)); exists (set context id (true, T));
    exists return_type; intuition.
  rewrite hv0 in H7; injection H7; clear H7; intros ?; subst.
  now apply well_formed_set.
- destruct (eval_ok _ _ _ okvalue s (proj1 hwf)) as [v [hv0 hv1]].
  inversion hs; subst; clear hs.
  exists (set context id (true, T)); exists (set context id (true, T));
    exists return_type; intuition.
  rewrite hv0 in H7; injection H7; clear H7; intros ?; subst.
  now apply well_formed_set.
(* Expression *)
- inversion hs; subst; clear hs.
  exists context; exists context; exists return_type; now split.
(* If *)
- inversion hs; subst; clear hs.
  + exists context; exists context'; exists return_type; now split.
  + exists context; exists context'; exists return_type; now split.
(* Pass *)
- inversion hs; subst; clear hs.
  + inversion hk; subst; clear hk. 
    exists call_context; exists call_context; exists return_type0; now split.
  + inversion hk; subst; clear hk. 
    exists context; exists context'; exists return_type; now split.
  + inversion hk; subst; clear hk.
    exists context; exists context; exists return_type; intuition.
    now apply ktypKWhile with (context' := context') (return_type := return_type).
  + inversion hk; subst; clear hk.
    exists context; exists context'; exists return_type; now split.
(* Seq *)
- inversion hs; subst; clear hs.
  exists context; exists context'; exists return_type; intuition.
  now apply ktypSeq with (context' := context'') (return_type := return_type).
(* While *)
- inversion hs; subst; clear hs.
  + exists context; exists context; exists return_type; intuition.
    now apply ktypKWhile with (context' := context') (return_type := return_type).
  + exists context; exists context'; exists return_type; now split.
(* Call *)
- inversion hs; subst; clear hs.
  unfold get_info in H8; unfold get_info in hg.
  rewrite (well_formed_info_eq _ _ hwf), hg in H8; injection H8; intros;
    subst; clear H8.
  apply well_formed_function_get with (state := s) in hg as [fctx hf]; [| now idtac].
  exists (mkContext (prepare_call_context Empty (parameters func0))
  (FuncState context)).
  exists fctx; exists (return_annotation func0); split; [ assumption | split ].
  + apply ktypKCallFun with 
      (call_context := set context target (true, return_annotation func0))
      (return_type := return_type); intuition.
      * split; [ now apply well_formed_estate_add_remove; apply hwf | ].
        split; [ now apply hwf | ].
        intros f' func'; simpl.
        now apply hwf.
      * now rewrite find_add, String.eqb_refl.
  + split; [ | split ].
    * eapply well_formed_estate_prepare;
      [now apply hwf | now apply hcheck | now apply H9| assumption].
    * now apply hwf.
    * intros f' func'; simpl.
      now apply hwf.
- inversion hs; subst; clear hs.
  unfold get_info in H1; unfold get_info in hg.
  rewrite (well_formed_info_eq  _ _ hwf), hg in H1; injection H1; intros; subst; clear H1.
  apply well_formed_function_get with (state := s) in hg as [ fctx hf ]; [ | now idtac ].
  exists (mkContext (prepare_call_context Empty (parameters func0)) (FuncState context)).
  rewrite hret in hf.
  exists fctx; exists Typ.None; split; [ assumption | split ].
  + now apply ktypKCallProc with (call_context := context) (return_type := return_type).
  + split; [ | split ].
    * eapply well_formed_estate_prepare;
      [now apply hwf | now apply hcheck | now apply H3|  assumption ].
    * now apply hwf.
    * intros f' func'; simpl.
      now apply hwf.
(* Return *)
- inversion hs; subst; clear hs.
  + inversion hk; subst; clear hk.
    exists call_context; exists call_context; exists return_type;
      split; [now constructor | ].
    split; [assumption | ].
    destruct H5 as [hewf h].
    split; [ | now apply h].
    intros x bx Tx hx.
    unfold get in H7.
    generalize (hewf x bx Tx).
    simpl.
    rewrite find_remove_smap, find_add_smap.
    case_eq (x =? id)%string; intro heq.
    * intros _.
      apply String.eqb_eq in heq; subst.
      unfold TContext_of_Context in hx.
      rewrite hx in H7; injection H7; clear H7; intro; subst.
      apply eval_ok with (state := RawState s) in hok; [ | now apply hwf ].
      destruct hok as [w [hw ?]].
      unfold EState_of_State in H1.
      rewrite hw in H1; injection H1; clear H1; intro; subst.
      now exists vret.
    * intro hg; apply hg in hx as [v [hv hv2]].
      now exists v.
  + inversion hk; subst; clear hk.
    exists call_context; exists call_context; exists return_type;
      split; [now constructor | now split ].
  + inversion hk; subst; clear hk.
    exists context; exists context'0; exists T;
      split; [ now constructor | now split ].
  + inversion hk; subst; clear hk.
    exists context; exists context'0; exists T;
      split; [ now constructor | now split ].
  + inversion hk; subst; clear hk.
    exists context; exists context; exists Typ.None;
      split; [ now constructor | now split ].
Qed.

(** If some <<arguments>> are correctly typed by some <<parameters>> then
    evaluating the <<parameters>> leads to a list of values. *)
Lemma check_arguments_eval: forall (arguments : list (option string * Expression.t))
  state context parameters,
  well_formed_estate state context ->
  check_arguments context arguments parameters ->
  exists value_arguments,
  eval_list state (from_list (map snd arguments)) = Some value_arguments.
Proof.
induction arguments as [ | [? arg] arguments hi];
    intros state context parameters hwf hcheck; simpl in *.
- destruct parameters; [| now elim hcheck].
  now exists Value.Nil.
- destruct parameters as [ | [? ty] parameters]; [ now elim hcheck |  ]; simpl in *.
  destruct hcheck as [ha has].
  apply eval_ok with (state := state) in ha as [ v [-> hv]]; [ | now idtac].
  destruct (hi _ _ _ hwf has) as [vs ->].
  now exists (Value.Cons v vs).
Qed.

(** A <<done>> pair of statement and continuation is a situation where
    there is no further possible reduction, and is considered a normal
    final situation.
    For the moment, we consider:
    - statement == Pass and continuation == KStop: the "genuine" final state.
    - statement == Pass and continuation == KCall: this one can only happen
        when a return statement is missing. We can't rule it out using
        typing only. We should make sure that all programs we check
        have the relevant 'return' invocation in all functions.
*)
Definition done st k :=
  match st with 
  | Statement.Pass =>
    match k with
    | KStop 
    | KCall _ _ _ => True
    | _ => False
  end
  | _ => False
end.

(** In this system, a correctly typed triple <<s, st, k>> is either a
    final (<<done>>) configuration, or can be reduced at least one step 
    using the operational semantic.

    With preservation and determinism, we have the main correctness tools
    for our system. *)
Lemma progress: forall context context' return_type stmt k,
    typ context return_type stmt context' ->
    ktyp context' return_type k ->
    forall state, well_formed state context ->
    (done stmt k) \/ (exists state' stmt' k', sss state stmt k state' stmt' k').
Proof.
destruct 1 as [
    context return_type id expr T hget okvalue
  | context return_type id expr T Tid hget okvalue
  | context return_type id expr T hget okvalue
  | context return_type id expr T okvalue
  | context return_type expr T h
  | context context' return_type test body orelse oktest hbody horlese
  | context return_type
  | context context' context'' return_type st next hst hnext
  | context context' return_type test body orlelse hok hbody horelse
  | context return_type target annotation function_name arguments function_def
      hg hret hcheck
  | context return_type function_name arguments function_def hg hret hcheck 
  | context context' ret T hok
]; intros hk state hwf.
- apply eval_ok with (state := RawState state) in okvalue; [ | now apply hwf ].
  destruct okvalue as [v hv].
  now right; repeat eexists; econstructor; apply hv.
- apply eval_ok with (state := RawState state) in okvalue; [ | now apply hwf ].
  destruct okvalue as [v hv].
  now right; repeat eexists; econstructor; apply hv.
- apply eval_ok with (state := RawState state) in okvalue; [ | now apply hwf ].
  destruct okvalue as [v hv].
  now right; repeat eexists; econstructor; apply hv.
- apply eval_ok with (state := RawState state) in okvalue; [ | now apply hwf ].
  destruct okvalue as [v hv].
  now right; repeat eexists; econstructor; apply hv.
- apply eval_ok with (state := RawState state) in h; [ | now apply hwf ].
  destruct h as [v hv].
  now right; repeat eexists; econstructor; apply hv.
- right.
  apply ty_bool with (state := RawState state) in oktest; [ | now apply hwf].
  destruct oktest as [[] hb];
    repeat eexists; econstructor; exact hb.
- inversion hk; subst; clear hk.
  + now left; split; firstorder.
  + now right; repeat eexists; econstructor.
  + right.
    apply ty_bool with (state := RawState state) in H; [ | now apply hwf ].
    destruct H as [[] hb];
      repeat eexists; econstructor; exact hb.
  + now right; repeat eexists; econstructor.
  + now left; split; firstorder.
- now right; repeat eexists; econstructor.
- right.
  apply ty_bool with (state := RawState state) in hok; [ | now apply hwf ].
  destruct hok as [[] hb]; now repeat eexists; econstructor.
- right.
  assert (hlen: List.length arguments = List.length (parameters function_def))
    by now apply check_arguments_length in hcheck. 
  apply check_arguments_eval with (state := RawState state) in hcheck; [ | now apply hwf].
  destruct hcheck as [vs hvs].
  assert (hlen2: List.length arguments = List.length vs) by
    now apply eval_list_length in hvs;
        rewrite <- hvs, Expression.ListHelpers.from_list_cancel, map_length.
  destruct (prepare_call_state_same_length Eval.Empty vs (parameters function_def))
    as [ after hafter ]; [now rewrite hlen2 in hlen | ].
  repeat eexists.
  econstructor; [| now apply hvs| now apply hafter].
  apply well_formed_get_info with (function_name := function_name) in hwf;
    rewrite <- hwf.
  now apply hg.
- right.
  assert (hlen: List.length arguments = List.length (parameters function_def))
    by now apply check_arguments_length in hcheck. 
  apply check_arguments_eval with (state := RawState state) in hcheck; [ | now apply hwf].
  destruct hcheck as [vs hvs].
  assert (hlen2: List.length arguments = List.length vs) by
    now apply eval_list_length in hvs;
        rewrite <- hvs, Expression.ListHelpers.from_list_cancel, map_length.
  destruct (prepare_call_state_same_length Eval.Empty vs (parameters function_def))
    as [ after hafter ]; [ now rewrite hlen2 in hlen | ].
  repeat eexists.
  econstructor; [| now apply hvs| now apply hafter].
  apply well_formed_get_info with (function_name := function_name) in hwf;
    rewrite <- hwf.
  now apply hg.
- inversion hk; subst; clear hk.
  + right; repeat eexists.
    now constructor.
  + right; repeat eexists.
    now econstructor.
  + right; repeat eexists.
    now econstructor.
  + right; repeat eexists.
    now econstructor.
  + apply eval_ok with (state := RawState state) in hok as [v [hv1 hv2]];
     [ | now apply hwf ].
    right; exists (set saved_state target v); exists Statement.Pass; exists k0.
    now constructor.
Qed.

(* begin hide *)
Module Test.

(* Creating a test context with no type information on variable, and
the same function information that Eval.Test.test_state *)
Definition test_context: Context :=
  mkState _ (SMap.empty _) (FuncState Eval.Test.test_state).

(* Final typing context, with y:int as only type information *)
Definition final_context : Context :=
  set test_context "y"%string (true, Typ.Integer).

(* We show here that in the context `test_context`, and under any return type,
   type checking the program Eval.Test.Prog only introduces y:int in the
     context *)
Lemma Typing_Prog: forall return_type,
  typ test_context return_type Eval.Test.Prog final_context.
Proof.
intro return_type.
unfold Test.Prog, final_context, Test.YID.
apply typCallFun with (function_def := Test.int_id).
- apply SMap.find_1; apply SMap.add_1.
  reflexivity.
- reflexivity.
- simpl; split; [| exact I].
  now constructor.
Qed.

Definition test_context2 : Context :=
  mkState _ (SMap.empty _) (FuncState Eval.Test.test_state2).

Lemma Typing_Prog2: forall return_type,
  typ test_context2 return_type Eval.Test.Prog2 test_context2.
Proof.
intro return_type.
apply typCallProc with (function_def := Test.expect_int).
- apply SMap.find_1; apply SMap.add_1.
  reflexivity.
- reflexivity.
- simpl; split; [| exact I].
  now constructor.
Qed.

(* Some well-formed proofs *)
Lemma well_formed_int_id : well_formed_function (FuncState test_context) Test.int_id.
Proof.
unfold well_formed_function.
exists (mkContext (prepare_call_context Empty (parameters Test.int_id))
       (FuncState test_context)).
constructor.
econstructor.
simpl.
apply SMap.find_1; apply SMap.add_1.
reflexivity.
Qed.

Lemma well_formed_test: well_formed Test.test_state test_context.
Proof.
split; [| split; [ reflexivity | intros fname fdef ]].
- intros id T.
  unfold test_context; simpl.
  intros b h; discriminate h.
- unfold get_info; simpl.
  rewrite find_add_smap.
  case_eq (fname =? "int_id")%string; intro heq.
  + intro h; injection h; clear h; intro h; subst.
    exact well_formed_int_id.
  + intro h; discriminate h.
Qed.

(* Successive valid assignement mixing weak and strong update *)
Definition Prog_Assign_good :=
  Statement.to_seq
    (Statement.Assign Eval.Test.XID None (Eval.Test.Int 42) ::
    Statement.Assign Eval.Test.XID None Expression.True_ ::
    Statement.Assign Eval.Test.XID (Some Typ.Integer) (Eval.Test.Int 42) ::
    Statement.Assign Eval.Test.XID None (Eval.Test.Int 42) ::
    Statement.Assign Eval.Test.XID (Some Typ.Boolean) Expression.False_ ::
    nil).

Definition EmptyContext : Context :=
  mkState _ (SMap.empty _) (SMap.empty _).

Lemma Prog_Assign_good_typed: forall return_type,
  typ EmptyContext return_type Prog_Assign_good
    (* resulting context. It's content is not really relevant *)
    (set (set (set (set
      (set EmptyContext  "x" (false, Typ.Integer))
        "x" (false, Typ.Boolean))
        "x" (true, Typ.Integer))
        "x" (true, Typ.Integer))
        "x" (true, Typ.Boolean)).
Proof.
unfold EmptyContext.
intros return_type.
econstructor; [ | econstructor; [ | econstructor; [ | econstructor ]]].
- apply typAssignIdNone; [ reflexivity | ].
  now constructor.
- unfold set; simpl.
  apply typAssignIdNoneWeak with (Tid := Typ.Integer); [ | now constructor ].
  apply SMap.find_1; apply SMap.add_1.
  reflexivity.
- apply typAssignIdAnnot.
  now constructor.
- apply typAssignIdNoneStrong; [ | now constructor ].
  apply SMap.find_1; apply SMap.add_1.
  reflexivity.
- apply typAssignIdAnnot.
  now constructor.
Qed.

(* Invalid assignement *)
Definition Prog_Assign_wrong1 :=
  Statement.Assign Eval.Test.XID (Some Typ.Integer) Expression.True_.

(* Invalid assignement after user annotation *)
Definition Prog_Assign_wrong2 :=
  Statement.to_seq
    (Statement.Assign Eval.Test.XID (Some Typ.Integer) (Eval.Test.Int 42) ::
     Statement.Assign Eval.Test.XID None Expression.True_ ::
     nil).

Lemma Prog_Assign_wrong1_untypable: forall context return_type return_context,
  typ context return_type Prog_Assign_wrong1 return_context -> False.
Proof.
intros context return_type return_context h.
inversion h; subst; clear h.
now inversion H5.
Qed.

Lemma Prog_Assign_wrong2_untypable: forall context return_type return_context,
  typ context return_type Prog_Assign_wrong2 return_context -> False.
Proof.
intros context return_type return_context h.
inversion h; subst; clear h.
inversion H3; subst; clear H3.
inversion H5; subst; clear H5.
- now rewrite find_add, String.eqb_refl in H3.
- rewrite find_add, String.eqb_refl in H3.
  now injection H3.
- rewrite find_add, String.eqb_refl in H3.
  injection H3; clear H3; intro hT.
  rewrite <- hT in H6.
  now inversion H6.
Qed.

End Test.
(* end hide *)
