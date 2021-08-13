(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Analysis
open Ast
open Statement
open Expression
open Pyre

type regular_targets = {
  implicit_self: bool;
  collapse_tito: bool;
  targets: Target.t list;
}
[@@deriving eq, show { with_path = false }]

type raw_callees =
  | ConstructorTargets of {
      new_targets: Target.t list;
      init_targets: Target.t list;
    }
  | RegularTargets of regular_targets
  | HigherOrderTargets of {
      higher_order_function: regular_targets;
      callable_argument: int * regular_targets;
    }
[@@deriving eq, show { with_path = false }]

type unprocessed_callees =
  | Named of {
      callee_name: string;
      callees: raw_callees;
    }
  | Synthetic of raw_callees String.Map.Tree.t

type callees =
  | Callees of raw_callees
  | SyntheticCallees of raw_callees String.Map.Tree.t
[@@deriving eq]

let pp_raw_callees_option formatter = function
  | None -> Format.fprintf formatter "None"
  | Some callees -> pp_raw_callees formatter callees


let pp_callees formatter = function
  | Callees callees -> Format.fprintf formatter "%s" (show_raw_callees callees)
  | SyntheticCallees map ->
      String.Map.Tree.to_alist map
      |> List.map ~f:(fun (key, value) -> Format.sprintf "%s: %s" key (show_raw_callees value))
      |> String.concat ~sep:", "
      |> Format.fprintf formatter "%s"


let show_callees callees = Format.asprintf "%a" pp_callees callees

let merge_targets left right =
  match left, right with
  | ( Some
        (RegularTargets
          {
            implicit_self = left_implicit_self;
            targets = left_targets;
            collapse_tito = left_collapse_tito;
          }),
      Some (RegularTargets { implicit_self = right_implicit_self; targets = right_targets; _ }) )
    when Bool.equal left_implicit_self right_implicit_self ->
      Some
        (RegularTargets
           {
             implicit_self = left_implicit_self;
             targets = List.rev_append left_targets right_targets;
             collapse_tito = left_collapse_tito;
           })
  | ( Some (ConstructorTargets { new_targets = left_new_targets; init_targets = left_init_targets }),
      Some
        (ConstructorTargets { new_targets = right_new_targets; init_targets = right_init_targets })
    ) ->
      Some
        (ConstructorTargets
           {
             new_targets = List.rev_append left_new_targets right_new_targets;
             init_targets = List.rev_append left_init_targets right_init_targets;
           })
  | Some left, None -> Some left
  | None, Some right -> Some right
  | None, None -> None
  | _ ->
      (* TODO(T77637504): We should probably error here. *)
      Log.warning "Failed to merge some call graph targets.";
      None


let defining_attribute ~resolution parent_type attribute =
  let global_resolution = Resolution.global_resolution resolution in
  Type.split parent_type
  |> fst
  |> Type.primitive_name
  >>= fun class_name ->
  GlobalResolution.attribute_from_class_name
    ~transitive:true
    ~resolution:global_resolution
    ~name:attribute
    ~instantiated:parent_type
    class_name
  >>= fun instantiated_attribute ->
  if Annotated.Attribute.defined instantiated_attribute then
    Some instantiated_attribute
  else
    Resolution.fallback_attribute ~resolution ~name:attribute class_name


let rec resolve_ignoring_optional ~resolution expression =
  let resolve_expression_to_type expression =
    match Resolution.resolve_expression_to_type resolution expression, Node.value expression with
    | ( Type.Callable ({ Type.Callable.kind = Anonymous; _ } as callable),
        Expression.Name (Name.Identifier function_name) )
      when function_name |> String.is_prefix ~prefix:"$local_" ->
        (* Treat nested functions as named callables. *)
        Type.Callable { callable with kind = Named (Reference.create function_name) }
    | annotation, _ -> annotation
  in
  let annotation =
    match Node.value expression with
    | Expression.Name (Name.Attribute { base; attribute; _ }) -> (
        let base_type =
          resolve_ignoring_optional ~resolution base
          |> fun annotation -> Type.optional_value annotation |> Option.value ~default:annotation
        in
        match defining_attribute ~resolution base_type attribute with
        | Some _ -> Resolution.resolve_attribute_access resolution ~base_type ~attribute
        | None -> resolve_expression_to_type expression
        (* Lookup the base_type for the attribute you were interested in *))
    | _ -> resolve_expression_to_type expression
  in
  Type.optional_value annotation |> Option.value ~default:annotation


type callee_kind =
  | Method of { is_direct_call: bool }
  | Function

let is_local identifier = String.is_prefix ~prefix:"$" identifier

let rec is_all_names = function
  | Expression.Name (Name.Identifier identifier) when not (is_local identifier) -> true
  | Name (Name.Attribute { base; attribute; _ }) when not (is_local attribute) ->
      is_all_names (Node.value base)
  | _ -> false


let rec callee_kind ~resolution callee callee_type =
  let is_super_call =
    let rec is_super callee =
      match Node.value callee with
      | Expression.Call { callee = { Node.value = Name (Name.Identifier "super"); _ }; _ } -> true
      | Call { callee; _ } -> is_super callee
      | Name (Name.Attribute { base; _ }) -> is_super base
      | _ -> false
    in
    is_super callee
  in
  match callee_type with
  | _ when is_super_call -> Method { is_direct_call = true }
  | Type.Parametric { name = "BoundMethod"; _ } ->
      Method { is_direct_call = is_all_names (Node.value callee) }
  | Type.Callable _ -> (
      match Node.value callee with
      | Expression.Name (Name.Attribute { base; _ }) ->
          let parent_type = resolve_ignoring_optional ~resolution base in
          let is_class () =
            parent_type
            |> GlobalResolution.class_definition (Resolution.global_resolution resolution)
            |> Option.is_some
          in
          if Type.is_meta parent_type then
            Method { is_direct_call = true }
          else if is_class () then
            Method { is_direct_call = false }
          else
            Function
      | _ -> Function)
  | Type.Union (callee_type :: _) -> callee_kind ~resolution callee callee_type
  | _ ->
      (* We must be dealing with a callable class. *)
      Method { is_direct_call = false }


let strip_optional annotation = Type.optional_value annotation |> Option.value ~default:annotation

let strip_meta annotation =
  if Type.is_meta annotation then
    Type.single_parameter annotation
  else
    annotation


(* Figure out what target to pick for an indirect call that resolves to implementation_target.
   E.g., if the receiver type is A, and A derives from Base, and the target is Base.method, then
   targeting the override tree of Base.method is wrong, as it would include all siblings for A.

 * Instead, we have the following cases:
 * a) receiver type matches implementation_target's declaring type -> override implementation_target
 * b) no implementation_target override entries are subclasses of A -> real implementation_target
 * c) some override entries are subclasses of A -> search upwards for actual implementation,
 *    and override all those where the override name is
 *  1) the override target if it exists in the override shared mem
 *  2) the real target otherwise
 *)
let compute_indirect_targets ~resolution ~receiver_type implementation_target =
  (* Target name must be the resolved implementation target *)
  let global_resolution = Resolution.global_resolution resolution in
  let get_class_type = GlobalResolution.parse_reference global_resolution in
  let get_actual_target method_name =
    if DependencyGraphSharedMemory.overrides_exist method_name then
      Target.create_override method_name
    else
      Target.create_method method_name
  in
  let receiver_type = receiver_type |> strip_meta |> strip_optional |> Type.weaken_literals in
  let declaring_type = Reference.prefix implementation_target in
  if
    declaring_type
    >>| Reference.equal (Type.class_name receiver_type)
    |> Option.value ~default:false
  then (* case a *)
    [get_actual_target implementation_target]
  else
    let target_callable = Target.create_method implementation_target in
    match DependencyGraphSharedMemory.get_overriding_types ~member:implementation_target with
    | None ->
        (* case b *)
        [target_callable]
    | Some overriding_types ->
        (* case c *)
        let keep_subtypes candidate =
          let candidate_type = get_class_type candidate in
          GlobalResolution.less_or_equal global_resolution ~left:candidate_type ~right:receiver_type
        in
        let override_targets =
          let create_override_target class_name =
            let method_name = Reference.last implementation_target in
            Reference.create ~prefix:class_name method_name |> get_actual_target
          in
          List.filter overriding_types ~f:keep_subtypes
          |> fun subtypes -> List.map subtypes ~f:create_override_target
        in
        target_callable :: override_targets


let collapse_tito ~resolution ~callee ~callable_type =
  (* For most cases, it is simply incorrect to not collapse tito, as it will lead to incorrect
   * mapping from input to output taint. However, the collapsing of tito adversely affects our
   * analysis in the case of the builder pattern, i.e.
   *
   * class C:
   *  def set_field(self, field) -> "C":
   *    self.field = field
   *    return self
   *
   * In this case, collapsing tito leads to field
   * tainting the entire `self` for chained call. To prevent this problem, we special case
   * builders to preserve the tito structure. *)
  match callable_type with
  | Type.Parametric { name = "BoundMethod"; parameters = [_; Type.Parameter.Single implicit] } ->
      let return_annotation =
        (* To properly substitute type variables, we simulate `callee.__call__` for the bound
           method. *)
        let to_simulate =
          Node.create_with_default_location
            (Expression.Name
               (Name.Attribute { base = callee; attribute = "__call__"; special = true }))
        in
        Resolution.resolve_expression resolution to_simulate
        |> snd
        |> function
        | Type.Callable { Type.Callable.implementation; _ } ->
            Type.Callable.Overload.return_annotation implementation
        | _ -> Type.Top
      in
      not (Type.equal implicit return_annotation)
  | _ -> true


let rec resolve_callees_from_type
    ~resolution
    ?(resolving_callable_class = false)
    ?receiver_type
    ~callee_kind
    ~collapse_tito
    callable_type
  =
  let resolve_callees_from_type ?(resolving_callable_class = resolving_callable_class) =
    resolve_callees_from_type ~resolving_callable_class
  in
  match callable_type with
  | Type.Callable { kind = Named name; _ } -> (
      match receiver_type with
      | Some receiver_type ->
          let targets =
            match callee_kind with
            | Method { is_direct_call = true } -> [Target.create_method name]
            | _ -> compute_indirect_targets ~resolution ~receiver_type name
          in
          Some (RegularTargets { implicit_self = true; targets; collapse_tito })
      | None ->
          let target =
            match callee_kind with
            | Method _ -> Target.create_method name
            | _ -> Target.create_function name
          in
          Some (RegularTargets { implicit_self = false; targets = [target]; collapse_tito }))
  | Type.Callable { kind = Anonymous; _ } -> None
  | Type.Parametric { name = "BoundMethod"; parameters = [Single callable; Single receiver_type] }
    ->
      resolve_callees_from_type ~resolution ~receiver_type ~callee_kind ~collapse_tito callable
  | Type.Union (element :: elements) ->
      let first_targets =
        resolve_callees_from_type ~resolution ~callee_kind ?receiver_type ~collapse_tito element
      in
      List.fold elements ~init:first_targets ~f:(fun combined_targets new_target ->
          resolve_callees_from_type
            ~resolution
            ?receiver_type
            ~callee_kind
            ~collapse_tito
            new_target
          |> merge_targets combined_targets)
  | Type.Parametric { name = "type"; _ } -> resolve_constructor_callee ~resolution callable_type
  | callable_type -> (
      (* Handle callable classes. `typing.Type` interacts specially with __call__, so we choose to
         ignore it for now to make sure our constructor logic via `cls()` still works. *)
      match
        Resolution.resolve_attribute_access
          resolution
          ~base_type:callable_type
          ~attribute:"__call__"
      with
      | Type.Any
      | Type.Top ->
          None
      (* Callable protocol. *)
      | Type.Callable { kind = Anonymous; _ } ->
          Type.primitive_name callable_type
          >>= fun primitive_callable_name ->
          Some
            (RegularTargets
               {
                 implicit_self = true;
                 targets =
                   [
                     `Method
                       { Target.class_name = primitive_callable_name; method_name = "__call__" };
                   ];
                 collapse_tito;
               })
      | annotation ->
          if not resolving_callable_class then
            resolve_callees_from_type
              ~resolution
              ~resolving_callable_class:true
              ~callee_kind
              ~collapse_tito
              annotation
          else
            None)


and resolve_constructor_callee ~resolution class_type =
  match
    ( Resolution.resolve_attribute_access resolution ~base_type:class_type ~attribute:"__new__",
      Resolution.resolve_attribute_access resolution ~base_type:class_type ~attribute:"__init__" )
  with
  | Type.Any, _
  | Type.Top, _
  | _, Type.Any
  | _, Type.Top ->
      None
  | new_callable_type, init_callable_type -> (
      let new_targets, init_targets =
        ( resolve_callees_from_type
            ~resolution
            ~receiver_type:class_type
            ~callee_kind:(Method { is_direct_call = true })
            ~collapse_tito:true
            new_callable_type,
          resolve_callees_from_type
            ~resolution
            ~receiver_type:class_type
            ~callee_kind:(Method { is_direct_call = true })
            ~collapse_tito:true
            init_callable_type )
      in
      match new_targets, init_targets with
      | ( Some (RegularTargets { targets = new_targets; _ }),
          Some (RegularTargets { targets = init_targets; _ }) ) ->
          Some (ConstructorTargets { new_targets; init_targets })
      | _ -> None)


let resolve_callee_from_defining_expression
    ~resolution
    ~callee:{ Node.value = callee; _ }
    ~implementing_class
  =
  match implementing_class, callee with
  | Type.Top, Expression.Name name when is_all_names callee ->
      (* If implementing_class is unknown, this must be a function rather than a method. We can use
         global resolution on the callee. *)
      GlobalResolution.global
        (Resolution.global_resolution resolution)
        (Ast.Expression.name_to_reference_exn name)
      >>= fun { AttributeResolution.Global.undecorated_signature; _ } ->
      undecorated_signature
      >>= fun undecorated_signature ->
      resolve_callees_from_type
        ~resolution
        ~callee_kind:Function
        ~collapse_tito:true
        (Type.Callable undecorated_signature)
  | _ -> (
      let implementing_class_name =
        if Type.is_meta implementing_class then
          Type.parameters implementing_class
          >>= fun parameters ->
          List.nth parameters 0
          >>= function
          | Single implementing_class -> Some implementing_class
          | _ -> None
        else
          Some implementing_class
      in
      match implementing_class_name with
      | Some implementing_class_name ->
          let class_primitive =
            match implementing_class_name with
            | Parametric { name; _ } -> Some name
            | Primitive name -> Some name
            | _ -> None
          in
          let method_name =
            match callee with
            | Expression.Name (Name.Attribute { attribute; _ }) -> Some attribute
            | _ -> None
          in
          method_name
          >>= (fun method_name ->
                class_primitive >>| fun class_name -> Format.sprintf "%s.%s" class_name method_name)
          >>| Reference.create
          (* Here, we blindly reconstruct the callable instead of going through the global
             resolution, as Pyre doesn't have an API to get the undecorated signature of methods. *)
          >>= fun name ->
          let callable_type =
            Type.Callable
              {
                Type.Callable.kind = Named name;
                implementation = { annotation = Type.Any; parameters = Type.Callable.Defined [] };
                overloads = [];
              }
          in
          resolve_callees_from_type
            ~resolution
            ~receiver_type:implementing_class
            ~callee_kind:(Method { is_direct_call = false })
            ~collapse_tito:true
            callable_type
      | _ -> None)


let transform_special_calls ~resolution { Call.callee; arguments } =
  match callee, arguments with
  | ( {
        Node.value =
          Expression.Name
            (Name.Attribute
              {
                base = { Node.value = Expression.Name (Name.Identifier "functools"); _ };
                attribute = "partial";
                _;
              });
        _;
      },
      { Call.Argument.value = actual_callable; _ } :: actual_arguments ) ->
      Some { Call.callee = actual_callable; arguments = actual_arguments }
  | ( {
        Node.value =
          Name
            (Name.Attribute
              {
                base = { Node.value = Expression.Name (Name.Identifier "multiprocessing"); _ };
                attribute = "Process";
                _;
              });
        _;
      },
      [
        { Call.Argument.value = process_callee; name = Some { Node.value = "$parameter$target"; _ } };
        {
          Call.Argument.value = { Node.value = Expression.Tuple process_arguments; _ };
          name = Some { Node.value = "$parameter$args"; _ };
        };
      ] ) ->
      Some
        {
          Call.callee = process_callee;
          arguments =
            List.map process_arguments ~f:(fun value -> { Call.Argument.value; name = None });
        }
  | _ -> SpecialCallResolution.redirect ~resolution { Call.callee; arguments }


let redirect_special_calls ~resolution call =
  match transform_special_calls ~resolution call with
  | Some call -> call
  | None -> Annotated.Call.redirect_special_calls ~resolution call


let resolve_recognized_callees ~resolution ~callee ~callee_type =
  (* Special treatment for a set of hardcoded decorators returning callable classes. *)
  match Node.value callee, callee_type with
  | ( _,
      Type.Parametric
        {
          name = "BoundMethod";
          parameters = [Single (Parametric { name; _ }); Single implementing_class];
        } )
    when Set.mem Recognized.allowlisted_callable_class_decorators name ->
      resolve_callee_from_defining_expression ~resolution ~callee ~implementing_class
  | Expression.Name (Name.Attribute { base; _ }), Parametric { name; _ }
    when Set.mem Recognized.allowlisted_callable_class_decorators name ->
      (* Because of the special class, we don't get a bound method & lose the self argument for
         non-classmethod LRU cache wrappers. Reconstruct self in this case. *)
      resolve_ignoring_optional ~resolution base
      |> fun implementing_class ->
      resolve_callee_from_defining_expression ~resolution ~callee ~implementing_class
  | Expression.Name name, _
    when is_all_names (Node.value callee)
         && Type.Set.mem SpecialCallResolution.recognized_callable_target_types callee_type ->
      Ast.Expression.name_to_reference name
      >>| Reference.show
      >>| fun name ->
      let collapse_tito = collapse_tito ~resolution ~callee ~callable_type:callee_type in
      RegularTargets { implicit_self = false; targets = [`Function name]; collapse_tito }
  | _ -> None


let resolve_callees_ignoring_decorators ~resolution ~collapse_tito callee =
  let global_resolution = Resolution.global_resolution resolution in
  let open UnannotatedGlobalEnvironment in
  match Node.value callee with
  | Expression.Name name when is_all_names (Node.value callee) -> (
      (* Resolving expressions that do not reference local variables or parameters. *)
      let name = Ast.Expression.name_to_reference_exn name in
      match GlobalResolution.resolve_exports global_resolution name with
      | Some
          (ResolvedReference.ModuleAttribute
            { export = ResolvedReference.Exported (Module.Export.Name.Define _); remaining = []; _ })
        ->
          Some
            (RegularTargets
               { implicit_self = false; targets = [`Function (Reference.show name)]; collapse_tito })
      | Some
          (ResolvedReference.ModuleAttribute
            {
              from;
              name;
              export = ResolvedReference.Exported Module.Export.Name.Class;
              remaining = [attribute];
              _;
            }) -> (
          let class_name = Reference.create ~prefix:from name |> Reference.show in
          GlobalResolution.class_definition global_resolution (Type.Primitive class_name)
          >>| Node.value
          >>| ClassSummary.attributes
          >>= Identifier.SerializableMap.find_opt attribute
          >>| Node.value
          >>= function
          | { kind = Method { static; _ }; _ } ->
              Some
                (RegularTargets
                   {
                     implicit_self = not static;
                     targets = [`Method { Target.class_name; method_name = attribute }];
                     collapse_tito;
                   })
          | _ -> None)
      | _ -> None)
  | Expression.Name (Name.Attribute { base; attribute; _ }) -> (
      (* Resolve `base.attribute` by looking up the type of `base`. *)
      match resolve_ignoring_optional ~resolution base with
      | Type.Primitive class_name
      | Type.Parametric { name = "type"; parameters = [Single (Type.Primitive class_name)] } -> (
          GlobalResolution.class_definition global_resolution (Type.Primitive class_name)
          >>| Node.value
          >>| ClassSummary.attributes
          >>= Identifier.SerializableMap.find_opt attribute
          >>| Node.value
          >>= function
          | { kind = Method _; _ } ->
              Some
                (RegularTargets
                   {
                     implicit_self = true;
                     targets = [`Method { Target.class_name; method_name = attribute }];
                     collapse_tito;
                   })
          | _ -> None)
      | _ -> None)
  | _ -> None


let resolve_regular_callees ~resolution ~callee =
  let callee_type = resolve_ignoring_optional ~resolution callee in
  match resolve_recognized_callees ~resolution ~callee ~callee_type with
  | Some callees -> Some callees
  | None -> (
      let callee_kind = callee_kind ~resolution callee callee_type in
      let collapse_tito = collapse_tito ~resolution ~callee ~callable_type:callee_type in
      match resolve_callees_from_type ~callee_kind ~resolution ~collapse_tito callee_type with
      | Some callees -> Some callees
      | None -> resolve_callees_ignoring_decorators ~resolution ~collapse_tito callee)


let resolve_callees ~resolution ~call =
  let { Call.callee; arguments } = redirect_special_calls ~resolution call in
  let higher_order_function_argument =
    let get_higher_order_function_targets index { Call.Argument.value = argument; _ } =
      match resolve_regular_callees ~resolution ~callee:argument with
      | Some (RegularTargets regular_targets) -> Some (index, regular_targets)
      | _ -> None
    in
    List.find_mapi arguments ~f:get_higher_order_function_targets
  in
  let regular_callees = resolve_regular_callees ~resolution ~callee in
  match higher_order_function_argument, regular_callees with
  | Some callable_argument, Some (RegularTargets higher_order_function) ->
      Some (HigherOrderTargets { higher_order_function; callable_argument })
  | _ -> regular_callees


let get_property_defining_parents ~resolution ~base ~attribute =
  let annotation =
    Resolution.resolve_expression_to_type resolution base |> strip_meta |> strip_optional
  in
  let rec get_defining_parents annotation =
    match annotation with
    | Type.Union annotations
    | Type.Variable { Type.Variable.Unary.constraints = Type.Variable.Explicit annotations; _ } ->
        List.concat_map annotations ~f:get_defining_parents
    | _ -> (
        match defining_attribute ~resolution annotation attribute with
        | Some property when Annotated.Attribute.property property ->
            [Annotated.Attribute.parent property |> Reference.create]
        | _ -> [])
  in
  get_defining_parents annotation


let resolve_property_targets ~resolution ~base ~attribute ~setter =
  match get_property_defining_parents ~resolution ~base ~attribute with
  | [] -> None
  | defining_parents ->
      let target_of_parent parent =
        let targets =
          let receiver_type = resolve_ignoring_optional ~resolution base in
          if Type.is_meta receiver_type then
            [Target.create_method (Reference.create ~prefix:parent attribute)]
          else
            let callee = Reference.create ~prefix:parent attribute in
            compute_indirect_targets ~resolution ~receiver_type callee
        in
        if setter then
          let to_setter target =
            match target with
            | `OverrideTarget { Target.class_name; method_name } ->
                `OverrideTarget { Target.class_name; method_name = method_name ^ "$setter" }
            | `Method { Target.class_name; method_name } ->
                `Method { Target.class_name; method_name = method_name ^ "$setter" }
            | _ -> target
          in
          List.map targets ~f:to_setter
        else
          targets
      in
      let targets = List.concat_map defining_parents ~f:target_of_parent in
      Some (RegularTargets { implicit_self = true; targets; collapse_tito = true })


let call_name { Call.callee; _ } =
  match Node.value callee with
  | Name (Name.Attribute { attribute; _ }) -> attribute
  | Name (Name.Identifier name) -> name
  | _ ->
      (* Fall back to something that hopefully identifies the call well. *)
      Expression.show callee


(* This is a bit of a trick. The only place that knows where the local annotation map keys is the
   fixpoint (shared across the type check and additional static analysis modules). By having a
   fixpoint that always terminates (by having a state = unit), we re-use the fixpoint id's without
   having to hackily recompute them. *)
module DefineCallGraph (Context : sig
  val global_resolution : GlobalResolution.t

  val local_annotations : LocalAnnotationMap.ReadOnly.t option

  val parent : Reference.t option

  val callees_at_location : unprocessed_callees Location.Table.t
end) =
struct
  type assignment_target = { location: Location.t }

  type visitor_t = {
    resolution: Resolution.t;
    assignment_target: assignment_target option;
  }

  module NodeVisitor = struct
    type nonrec t = visitor_t

    let expression_visitor ({ resolution; assignment_target } as state) { Node.value; location } =
      let register_targets ~callee_name targets =
        Option.iter targets ~f:(fun data ->
            let data =
              match Location.Table.find Context.callees_at_location location with
              | Some (Named { callee_name = existing_callee_name; callees }) ->
                  Synthetic
                    (String.Map.Tree.of_alist_reduce
                       ~f:(fun existing _ -> existing)
                       [existing_callee_name, callees; callee_name, data])
              | Some (Synthetic map) -> Synthetic (String.Map.Tree.set map ~key:callee_name ~data)
              | None -> Named { callee_name; callees = data }
            in
            Location.Table.set Context.callees_at_location ~key:location ~data)
      in
      match value with
      | Expression.Call call ->
          resolve_callees ~resolution ~call |> register_targets ~callee_name:(call_name call);
          state
      | Expression.Name (Name.Attribute { Name.Attribute.base; attribute; _ }) ->
          let setter =
            match assignment_target with
            | Some { location = assignment_target_location } ->
                Location.equal assignment_target_location location
            | None -> false
          in
          resolve_property_targets ~resolution ~base ~attribute ~setter
          |> register_targets ~callee_name:attribute;
          state
      | Expression.ComparisonOperator comparison -> (
          match ComparisonOperator.override ~location comparison with
          | Some { Node.value = Expression.Call call; _ } ->
              resolve_callees ~resolution ~call |> register_targets ~callee_name:(call_name call);
              state
          | _ -> state)
      | _ -> state


    let statement_visitor state _ = state

    let generator_visitor ({ resolution; _ } as state) generator =
      (* Since generators create variables that Pyre sees as scoped within the generator, handle
         them by adding the generator's bindings to the resolution. *)
      {
        state with
        resolution =
          Resolution.resolve_assignment
            resolution
            (Ast.Statement.Statement.generator_assignment generator);
      }


    let node state = function
      | Visit.Expression expression -> expression_visitor state expression
      | Visit.Statement statement -> statement_visitor state statement
      | Visit.Generator generator -> generator_visitor state generator
      | _ -> state


    let visit_statement_children _ statement =
      match Node.value statement with
      | Statement.Assign _
      | Statement.Define _
      | Statement.Class _ ->
          false
      | _ -> true
  end

  module CalleeVisitor = Visit.MakeNodeVisitor (NodeVisitor)

  include Fixpoint.Make (struct
    type t = unit [@@deriving show]

    let less_or_equal ~left:_ ~right:_ = true

    let widen ~previous:_ ~next:_ ~iteration:_ = ()

    let forward_statement ~resolution ~statement =
      match Node.value statement with
      | Statement.Assign { Assign.target; value; _ } ->
          CalleeVisitor.visit_expression
            ~state:
              (ref { resolution; assignment_target = Some { location = Node.location target } })
            target;
          CalleeVisitor.visit_expression ~state:(ref { resolution; assignment_target = None }) value
      | _ ->
          CalleeVisitor.visit_statement
            ~state:(ref { resolution; assignment_target = None })
            statement


    let forward ~key _ ~statement =
      let resolution =
        TypeCheck.resolution_with_key
          ~global_resolution:Context.global_resolution
          ~local_annotations:Context.local_annotations
          ~parent:Context.parent
          ~key
          (module TypeCheck.DummyContext)
      in
      forward_statement ~resolution ~statement


    let backward ~key:_ _ ~statement:_ = ()
  end)
end

let call_graph_of_define
    ~environment
    ~define:({ Define.signature = { Define.Signature.name; parent; _ }; _ } as define)
  =
  let callees_at_location = Location.Table.create () in
  let module DefineFixpoint = DefineCallGraph (struct
    let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment

    let local_annotations =
      TypeEnvironment.ReadOnly.get_local_annotations environment (Node.value name)


    let parent = parent

    let callees_at_location = callees_at_location
  end)
  in
  (* Handle parameters. *)
  let () =
    let resolution =
      TypeCheck.resolution
        (TypeEnvironment.ReadOnly.global_resolution environment)
        (module TypeCheck.DummyContext)
    in
    List.iter
      define.Ast.Statement.Define.signature.parameters
      ~f:(fun { Node.value = { Parameter.value; _ }; _ } ->
        Option.iter value ~f:(fun value ->
            DefineFixpoint.CalleeVisitor.visit_expression
              ~state:(ref { DefineFixpoint.resolution; assignment_target = None })
              value))
  in
  let deduplicate callees =
    let deduplicate_targets targets = List.dedup_and_sort ~compare:Target.compare targets in
    let deduplicate_regular_targets { targets; implicit_self; collapse_tito } =
      { targets = deduplicate_targets targets; implicit_self; collapse_tito }
    in
    match callees with
    | RegularTargets regular_targets -> RegularTargets (deduplicate_regular_targets regular_targets)
    | ConstructorTargets { new_targets; init_targets } ->
        ConstructorTargets
          {
            new_targets = deduplicate_targets new_targets;
            init_targets = deduplicate_targets init_targets;
          }
    | HigherOrderTargets { higher_order_function; callable_argument = index, callable_argument } ->
        HigherOrderTargets
          {
            higher_order_function = deduplicate_regular_targets higher_order_function;
            callable_argument = index, deduplicate_regular_targets callable_argument;
          }
  in

  DefineFixpoint.forward ~cfg:(Cfg.create define) ~initial:() |> ignore;
  Location.Table.to_alist callees_at_location
  |> List.map ~f:(fun (key, value) ->
         match value with
         | Synthetic map -> key, SyntheticCallees (Core.String.Map.Tree.map ~f:deduplicate map)
         | Named { callees; _ } -> key, Callees (deduplicate callees))
  |> Location.Map.of_alist_exn


module SharedMemory = struct
  include
    Memory.WithCache.Make
      (Target.CallableKey)
      (struct
        type t = callees Location.Map.Tree.t

        let prefix = Prefix.make ()

        let description = "call graphs of defines"

        let unmarshall value = Marshal.from_string value 0
      end)

  let add ~callable ~callees = add callable (Location.Map.to_tree callees)

  let get ~callable = get callable >>| Location.Map.of_tree

  let get_or_compute ~callable ~environment ~define =
    match get ~callable with
    | Some map -> map
    | None ->
        let callees = call_graph_of_define ~environment ~define in
        add ~callable ~callees;
        callees


  let remove callables = KeySet.of_list callables |> remove_batch
end

let create_callgraph ?(use_shared_memory = false) ~environment ~source =
  let fold_defines dependencies = function
    | { Node.value = define; _ } when Define.is_overloaded_function define -> dependencies
    | define ->
        let call_graph_of_define =
          if use_shared_memory then
            SharedMemory.get_or_compute
              ~callable:(Target.create define)
              ~environment
              ~define:(Node.value define)
          else
            call_graph_of_define ~environment ~define:(Node.value define)
        in
        let callees found_callees =
          let targets_of_callees = function
            | RegularTargets { targets; _ } -> targets
            | ConstructorTargets { new_targets; init_targets } ->
                List.rev_append new_targets init_targets
            | HigherOrderTargets
                {
                  higher_order_function = { targets = higher_order_targets; _ };
                  callable_argument = _, { targets = argument_targets; _ };
                } ->
                List.rev_append higher_order_targets argument_targets
          in
          match found_callees with
          | Callees callees -> targets_of_callees callees
          | SyntheticCallees map ->
              String.Map.Tree.data map |> List.concat_map ~f:targets_of_callees
        in
        Location.Map.data call_graph_of_define
        |> List.concat_map ~f:callees
        |> List.dedup_and_sort ~compare:Target.compare
        |> fun callees ->
        Target.CallableMap.set dependencies ~key:(Target.create define) ~data:callees
  in
  Preprocessing.defines ~include_nested:true source
  |> List.fold ~init:Target.CallableMap.empty ~f:fold_defines
