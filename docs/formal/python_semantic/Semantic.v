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

(** Typing context (Map from identifiers to types) *)
Definition TContext := RawState0 Typ.t.

Definition get_context_type (ctx: TContext) (k: string) : option Typ.t :=
    get0 _ ctx k.

Section TypingExpression.

Import Pyre.Expression.

(** Typing judgement for expressions *)
Inductive ok : TContext -> Expression.t -> Typ.t -> Prop :=
  | typFalse: forall ctx, ok ctx False_ Typ.Boolean
  | typTrue: forall ctx, ok ctx True_ Typ.Boolean
  | typInteger: forall ctx z, ok ctx (Integer z) Typ.Integer
  | typString: forall ctx str, ok ctx (String str) Typ.String
  | typNone: forall  ctx,  ok ctx None Typ.None
  | tyBop: forall ctx b0 b1 op,
          ok ctx b0 Typ.Boolean ->
          ok ctx b1 Typ.Boolean ->
          ok ctx (BooleanOperator b0 op b1) Typ.Boolean
  | tyCmp: forall ctx e0 e1 op,
          ok ctx e0 Typ.Integer ->
          ok ctx e1 Typ.Integer ->
          ok ctx (ComparisonOperator e0 op e1) Typ.Boolean
  | TyList: forall ctx (l: Expression.tlist) ty,
          ok_list ctx l ty -> 
          ok ctx (List l) (Typ.List ty)
  | TyTuple: forall ctx (l: Expression.tlist) lty,
          ok_tuple ctx l lty ->
          ok ctx (Tuple l) (Typ.Tuple lty)
  | TyNot: forall ctx expr,
          ok ctx expr Typ.Boolean ->
          ok ctx (UnaryOperator Not expr) Typ.Boolean
  | TyUnary: forall ctx expr op,
         op <> Not -> 
         ok ctx expr Typ.Integer ->
         ok ctx (UnaryOperator op expr) Typ.Integer
  | TyTernary : forall ctx target test alternative typ,
          ok ctx target typ ->
          ok ctx alternative typ ->
          ok ctx test Typ.Boolean ->
          ok ctx (Ternary target test alternative) typ
  | TyId: forall ctx id T,
          get_context_type ctx id = Some T ->
          ok  ctx (Id id) T
with ok_list: TContext -> Expression.tlist -> Typ.t -> Prop :=
  | tyListNil : forall ctx typ, ok_list ctx Nil typ
  | tyListCons : forall ctx hd tl typ,
          ok ctx hd typ ->
          ok_list ctx tl typ ->
          ok_list ctx (Cons hd tl) typ
with ok_tuple : TContext -> Expression.tlist -> Typ.tlist -> Prop :=
  | tyTupleSingl : forall ctx expr typ,
          ok ctx expr typ ->
          ok_tuple ctx (Cons expr Nil) (Typ.Cons typ Typ.Nil)
  | tyTupleCons : forall ctx expr le typ lt,
          ok ctx expr typ ->
          ok_tuple ctx le lt ->
          ok_tuple ctx (Cons expr le) (Typ.Cons typ lt)
.

(* begin hide *)
Scheme ok_ind' := Induction for ok Sort Prop
  with ok_list_ind' := Induction for ok_list Sort Prop
  with ok_tuple_ind' := Induction for ok_tuple Sort Prop.

Combined Scheme ok_induc from ok_ind', ok_list_ind', ok_tuple_ind'.

Lemma ok_tuple_len: forall ctx le lt,
    ok_tuple ctx le lt -> List.length le = List.length lt.
Proof.
intro ctx.
induction 1; simpl in *; now intuition.
Qed.

Lemma ok_tuple_len_pos : forall ctx le lt,
    ok_tuple ctx le lt -> List.length le > 0.
Proof.
intro ctx.
induction 1; simpl in *; now intuition.
Qed.

Lemma ok_list_spec: forall ctx l typ,
    ok_list ctx l typ -> Forall (fun expr => ok ctx expr typ) l.
Proof.
induction 1 as [ ctx typ | ctx hd tl typ h0 h1 hi ];
    [ now apply Forall_nil | ].
now apply Forall_cons.
Qed.

Lemma ok_list_forall: forall ctx (l: Expression.tlist) typ,
    Forall (fun expr => ok ctx expr typ) l -> ok_list ctx l typ.
Proof.
intro ctx.
induction l as [ | hd tl hi]; intros typ h; inversion h; subst; clear h.
- now constructor.
- apply hi in H2.
  now constructor.
Qed.

Lemma ok_tuple_spec: forall ctx l lt, ok_tuple ctx l lt ->
    List.length l > 0 /\
    List.length l = List.length lt /\
    Forall (fun et => ok ctx (fst et) (snd et)) (List.combine l lt).
Proof.
induction 1 as [ctx expr typ h | ctx expr typ es et h hs hi ].
- intuition.
  now apply Forall_cons.
- now simpl; intuition.
Qed.

Lemma ok_tuple_forall: forall ctx (l: Expression.tlist) (lt: Typ.tlist),
    List.length l > 0 ->
    List.length l = List.length lt ->
    Forall (fun et => ok ctx (fst et) (snd et)) (List.combine l lt) ->
    ok_tuple ctx l lt.
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

Lemma ok_tuple_forall2: forall ctx (l: Expression.tlist) (lt: Typ.tlist),
    List.length lt > 0 ->
    List.length l = List.length lt ->
    Forall (fun et => ok ctx (fst et) (snd et)) (List.combine l lt) ->
    ok_tuple ctx l lt. 
Proof.
intros ctx l lt h heq hf.
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
Definition well_formed_estate (s : EState) (ctx: TContext) : Prop :=
    (forall k T, get0 _ ctx k = Some T ->
     exists v, get0 _ s k = Some v /\ okv v T)
.

(* begin hide *)
Lemma well_formed_estate_get: forall s ctx, well_formed_estate s ctx ->
    forall k T, get_context_type ctx k = Some T -> exists v,
    get_state_value s k = Some v /\ okv v T.
Proof.
intros s ctx hwf k T.
now apply hwf.
Qed.

Lemma ty_bool_int : forall ctx expr typ, ok ctx expr typ ->
    forall s, well_formed_estate s ctx ->
    (typ = Typ.Boolean -> exists b, eval s expr = Some (Value.Boolean b)) /\
    (typ = Typ.Integer -> exists z, eval s expr = Some (Value.Integer z)).
Proof.
induction 1 as [
    | ctx
    | ctx z
    | ctx str
    | ctx
    | ctx left right op hl hil hr hir
    | ctx left right op hl hil hr hir
    | ctx l typ hl
    | ctx l typs hl
    | ctx expr he hie
    | ctx expr op hop he hie
    | ctx target test alterntaive typ htarget hitarget
            halt hialt htest hitest
    | ctx id T h
]; intros s hwf; subst; split; intro heq; subst;
   try discriminate; simpl in *.
+ now exists false.
+ now exists true.
+ now exists z.
+ destruct (hil s hwf) as [h0 _].
  destruct (hir s hwf) as [h1 _].
  destruct (h0 refl_equal) as [x0 hx0].
  destruct (h1 refl_equal) as [x1 hx1].
  rewrite hx0, hx1.
  now exists (evaluate_boolean_operator x0 op x1).
+ destruct (hil s hwf) as [_ h0].
  destruct (hir s hwf) as [_ h1].
  destruct (h0 refl_equal) as [x0 hx0].
  destruct (h1 refl_equal) as [x1 hx1].
  rewrite hx0, hx1.
  now exists (evaluate_comparison_operator (Value.Integer x0) op (Value.Integer x1)).
+ destruct (hie s hwf) as [h0 _]. 
  destruct (h0 refl_equal) as [ x0 hx0 ].
  rewrite hx0.
  now exists (negb x0).
+ destruct (hie s hwf) as [_ h0].
  destruct (h0 refl_equal) as [ x0 hx0 ].
  rewrite hx0; simpl.
  rewrite evaluate_unary_int_operator; destruct op; try (now elim hop).
  * now exists (-x0 -1)%Z.
  * now exists ( - x0 )%Z.
  * now exists x0.
+ destruct (hitarget s hwf) as [h0 _].
  destruct (hialt s hwf) as [h1 _].
  destruct (hitest s hwf) as [h2 _].
  destruct (h0 refl_equal) as [x0 hx0].
  destruct (h1 refl_equal) as [x1 hx1].
  destruct (h2 refl_equal) as [x2 hx2].
  rewrite hx2; destruct x2. 
  * rewrite hx0.
    now exists x0.
  * rewrite hx1.
    now exists x1.
+ destruct (hitarget s hwf) as [_ h0].
  destruct (hialt s hwf) as [_ h1].
  destruct (hitest s hwf) as [h2 _].
  destruct (h0 refl_equal) as [x0 hx0].
  destruct (h1 refl_equal) as [x1 hx1].
  destruct (h2 refl_equal) as [x2 hx2].
  rewrite hx2; destruct x2. 
  * rewrite hx0.
    now exists x0.
  * rewrite hx1.
    now exists x1.
+ apply well_formed_estate_get with (s := s) in h; [ | now idtac ].
  destruct h as [ v [-> hv]].
  inversion hv; subst; clear hv.
  now exists b.
+ apply well_formed_estate_get with (s := s) in h; [ | now idtac ].
  destruct h as [ v [-> hv]].
  inversion hv; subst; clear hv.
  now exists i.
Qed.
(* end hide *)

(** Every expression typed as <<Typ.Boolean>> must  be an actual <<bool>> *)
Lemma ty_bool : forall ctx expr, ok ctx expr Typ.Boolean ->
    forall s, well_formed_estate s ctx ->
    exists b, eval s expr = Some (Value.Boolean b).
Proof.
intros ctx expr h s hwf; eapply ty_bool_int in h as [ h0 _].
- now apply h0.
- now apply hwf.
Qed.

(** Every expression typed as <<Typ.Integer>> must  be an actual <<Z>> *)
Lemma ty_int : forall ctx expr, ok ctx expr Typ.Integer ->
    forall s, well_formed_estate s ctx ->
    exists z, eval s expr = Some (Value.Integer z).
Proof.
intros ctx expr h s hwf; eapply ty_bool_int in h as [ _ h0].
- now apply h0.
- now apply hwf.
Qed.

(* begin hide *)
Lemma ty_string_ : forall ctx expr typ, ok ctx expr typ -> 
    typ = Typ.String -> forall s, well_formed_estate s ctx ->
    exists str, eval s expr = Some (Value.String str).
Proof.
induction 1 as [
    | ctx
    | ctx z
    | ctx str
    | ctx
    | ctx left right op hl hil hr hir
    | ctx left right op hl hil hr hir
    | ctx l typ hl
    | ctx l typs hl
    | ctx expr he hie
    | ctx expr op hop he hie
    | ctx target test alterntaive typ htarget hitarget
            halt hialt htest hitest
    | ctx id T h
] ; intros heq s hwf; subst; try discriminate; simpl in *.
- now exists str.
- apply ty_bool with (s := s) in htest as [ b hb ];[ | now idtac ].
  rewrite hb.
  destruct (hitarget refl_equal s hwf) as [s1 hs1].
  destruct (hialt refl_equal s hwf) as [s2 hs2].
  rewrite hs1, hs2.
  destruct b.
  + now exists s1.
  + now exists s2.
- apply well_formed_estate_get with (s := s) in h; [ | now idtac].
  destruct h as [v [-> hv]].
  inversion hv; subst; clear hv.
  now exists str.
Qed.
(* end hide *)

(** Every expression typed as <<Typ.String>> must  be an actual <<string>> *)
Lemma ty_string : forall ctx expr, ok ctx expr Typ.String ->
    forall s, well_formed_estate s ctx ->
    exists str, eval s expr = Some (Value.String str).
Proof.
intros ctx expr h s hwf.
now apply ty_string_ with (ctx := ctx) (s := s) in h.
Qed.

(* begin hide *)
Lemma ty_none_: forall ctx expr typ, ok ctx expr typ ->
    typ = Typ.None -> forall s, well_formed_estate s ctx ->
    eval s expr = Some Value.None.
Proof.
induction 1 as [
    | ctx
    | ctx z
    | ctx str
    | ctx
    | ctx left right op hl hil hr hir
    | ctx left right op hl hil hr hir
    | ctx l typ hl
    | ctx l typs hl
    | ctx expr he hie
    | ctx expr op hop he hie
    | ctx target test alterntaive typ htarget hitarget
            halt hialt htest hitest
    | ctx id T h
] ; intros heq s hwf; subst; try discriminate; simpl in *.
- reflexivity.
- apply ty_bool with (s := s) in htest as [ b hb ];[ | now idtac ].
  rewrite hb.
  rewrite (hitarget refl_equal s hwf).
  rewrite (hialt refl_equal s hwf).
  now destruct b.
- apply well_formed_estate_get with (s := s) in h; [ | now idtac].
  destruct h as [v [-> hv]].
  inversion hv; subst; clear hv.
  reflexivity.
Qed.
(* end hide *)

(** Every expression typed as <<Typ.None>>  must be <<None>> *)
Lemma ty_none: forall ctx expr, ok ctx expr Typ.None ->
    forall s, well_formed_estate s ctx -> eval s expr = Some Value.None.
Proof.
intros  ctx expr h s hwf.
now apply ty_none_ with (ctx := ctx) (s := s) in h.
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

(** * Type soundness

    Proof that a correctly typed expression always evaluate to a value,
    and this value has the same type as the input expression.
*)

Lemma eval_ok_ :
    (forall ctx expr typ, ok ctx expr typ ->
        forall s, well_formed_estate s ctx ->
        exists v, eval s expr = Some v /\  okv v typ) /\
    (forall ctx le typ, ok_list ctx le typ ->
        forall s, well_formed_estate s ctx ->
        exists lv, eval_list s le = Some lv /\
        okv_list lv typ) /\
    (forall ctx le lt, ok_tuple ctx le lt ->
        forall s, well_formed_estate s ctx ->
        exists lv, eval_list s le = Some lv /\ okv_tuple lv lt).
Proof.
apply ok_induc.
(* ok *)
- intros ctx s hwf; subst; exists (Value.Boolean false); now split.
- intros ctx s hwf; subst; exists (Value.Boolean true); now split.
- intros ctx z s hwf; subst; exists (Value.Integer z); now split.
- intros ctx str s hwf; subst; exists (Value.String str); now split.
- intros ctx s hwf; subst; exists Value.None; now split.
- intros ctx b0 b1 op h0 ? h1 ? s hwf; simpl; subst.
  apply ty_bool with (s := s) in h0; [ | now idtac].
  destruct h0 as [vb0 ->].
  apply ty_bool with (s := s) in h1; [ | now idtac ].
  destruct h1 as [vb1 ->].
  now eexists; split; [ reflexivity | idtac].
- intros ctx e0 e1 op h0 ? h1 ? s hwf; simpl; subst.
  apply ty_int with (s := s) in h0; [ | now idtac ].
  destruct h0 as [v0 ->].
  apply ty_int with (s := s) in h1; [ | now idtac ].
  destruct h1 as [v1 ->].
  now eexists; split; [ reflexivity | idtac].
- intros ctx l ty h hi s hwf; simpl.
  destruct (hi _ hwf) as  [vl [-> h0]].
  now eexists; split; [reflexivity  | constructor ].
- intros ctx l ty h hi s hwf; simpl.
  destruct (hi _ hwf) as  [vl [-> h0]].
  now eexists; split; [reflexivity  | constructor ].
- intros ctx expr h hi s hwf; simpl.
  apply ty_bool with (s := s) in h as [v ->]; [ | now idtac ].
  now eexists; split; [reflexivity | ].
- intros ctx expr op hop h hi s hwf; simpl.
  apply ty_int with (s := s) in h as [v ->]; [ | now idtac ].
  rewrite evaluate_unary_int_operator.
  destruct op; try (now elim hop); now eexists; split; [reflexivity | ].
- intros ctx target test alt typ htarget hitarget halt hialt htest hitest
    s hwf; simpl.
  apply ty_bool with (s := s) in htest as [b ->]; [ | now idtac ].
  destruct (hitarget _ hwf) as [vt [-> hvt]].
  destruct (hialt _ hwf) as [va [-> hva]].
  now destruct b; eexists; split; [ reflexivity | | reflexivity | ].
- intros ctx id typ hg s hwf; simpl.
  now apply well_formed_estate_get with (ctx := ctx); [ now apply hwf | ].
(* ok_list *)
- intros ctx typ s hwf; simpl.
  now exists Value.Nil; intuition.
- intros ctx hd tl typ h0 hi0 h1 hi1 s hwf; simpl.
  destruct (hi0 _ hwf) as [vhd [-> hv]].
  destruct (hi1 _ hwf) as [vtl [-> ht]].
  now exists (Value.Cons vhd vtl); intuition.
(* ok_tuple *)
- intros ctx expr typ h hi s hwf; simpl.
  destruct (hi _ hwf) as [v [-> ?]].
  now  exists (Value.Cons v Value.Nil); intuition.
- intros ctx expr el typ tl h0 hi0 h1 hi1 s hwf; simpl.
  destruct (hi0 _ hwf) as [vhd [-> ?]].
  destruct (hi1 _ hwf) as [vtl [-> ?]].
  now exists (Value.Cons vhd vtl); intuition.
Qed.

Lemma eval_ok: forall ctx expr typ, ok ctx expr typ ->
    forall s, well_formed_estate s ctx ->
    exists v, eval s expr = Some v /\ okv v typ.
Proof.
intros.
now eapply eval_ok_; eauto.
Qed.

Lemma eval_list_ok: forall ctx le typ, ok_list ctx le typ ->
    forall s, well_formed_estate s ctx ->
    exists lv, eval_list s le = Some lv /\ okv_list lv typ.
Proof.
intros.
now eapply eval_ok_; eauto.
Qed.

Lemma eval_tuple_ok: forall ctx le lt, ok_tuple ctx le lt ->
    forall s, well_formed_estate s ctx ->
    exists lv, eval_list s le = Some lv /\ okv_tuple lv lt.
Proof.
intros.
now eapply eval_ok_; eauto.
Qed.

Lemma ok_sequence : forall ctx expr T, ok ctx expr T ->
    forall T', is_sequence T = Some T' ->
    forall s, well_formed_estate s ctx ->
    exists lv, eval s expr = Some (Value.Sequence lv).
Proof.
induction 1 as [
    | ctx
    | ctx z
    | ctx str
    | ctx
    | ctx left right op hl hr
    | ctx left right op hl hr
    | ctx l typ hl
    | ctx l typs hl
    | ctx expr he
    | ctx expr op hop he
    | ctx target test alterntaive typ htarget hit halt hia htest hitest
    | ctx id T h
]; intros T' hT s hwf; simpl; subst;
  try (unfold is_sequence in hT; simpl in hT; discriminate).
- apply eval_list_ok with (s := s) in hl as [lv [-> ?]]; [ | now idtac ].
  now exists lv.
- destruct (hit T' hT s hwf) as [l1 ->].
  destruct (hia T' hT s hwf) as [l2 ->].
  apply ty_bool with (s := s) in htest as [b ->]; [ | now idtac ].
  now destruct b; eexists; reflexivity.
- apply well_formed_estate_get with (s := s) in h as [v [-> h1]];
    [ | now idtac ].
  inversion h1; subst; clear h1; try discriminate; now eexists; reflexivity.
Qed.

Lemma ty_list: forall ctx expr T, ok ctx expr (Typ.List T) ->
    forall s, well_formed_estate s ctx ->
    exists lv, eval s expr = Some (Value.Sequence lv) /\
               okv (Value.Sequence lv) (Typ.List T).
Proof.
intros ctx expr T hok s hwf.
apply eval_ok with (s := s) in hok; [ | now idtac].
destruct hok as [v [-> hv]].
inversion hv; subst; clear hv.
exists l; split; [ now idtac | ].
now constructor.
Qed.

(* begin hide *)
Lemma well_formed_estate_set: forall s ctx,
  well_formed_estate s ctx ->
  forall id v T, okv v T ->
  well_formed_estate (set0 _ s id v) (set0 _ ctx id T).
Proof.
intros s ctx hwf id v T hok.
intros k S; unfold set, set0, get0; simpl.
rewrite !get_set_map.
destruct String.eqb.
- intro h; injection h; clear h; intro; subst.
  now exists v.
- now apply hwf.
Qed.
