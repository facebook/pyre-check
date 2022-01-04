(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Expression
open Pyre
open Statement
open Analysis
module Error = AnalysisError
open TypeInferenceData

let refine_local ~resolution ~name ~annotation =
  match name_to_reference name with
  | Some reference -> Resolution.refine_local resolution ~reference ~annotation
  | None -> resolution


module ErrorMap = struct
  type key = {
    location: Location.t;
    kind: int;
  }
  [@@deriving compare, sexp]

  module Map = Map.Make (struct
    type nonrec t = key

    let compare = compare_key

    let sexp_of_t = sexp_of_key

    let t_of_sexp = key_of_sexp
  end)

  let add ~errors ({ Error.location = { Location.WithModule.start; stop; _ }; _ } as error) =
    let location = { Location.start; stop } in
    Map.set errors ~key:{ location; kind = Error.code error } ~data:error
end

module type Context = sig
  val configuration : Configuration.Analysis.t

  val qualifier : Reference.t

  val define : Define.t Node.t

  val resolution_fixpoint : LocalAnnotationMap.t option

  val error_map : TypeCheck.LocalErrorMap.t option
end

module type Signature = sig
  type t [@@deriving eq]

  val create : ?bottom:bool -> resolution:Resolution.t -> unit -> t

  val initial : resolution:Resolution.t -> t

  val initial_forward : resolution:Resolution.t -> t

  val initial_backward : forward:t -> t

  include Fixpoint.State with type t := t
end

module State (Context : Context) = struct
  module TypeCheckContext = struct
    let qualifier = Context.qualifier

    let debug = Context.configuration.debug

    let constraint_solving_style = Configuration.Analysis.default_constraint_solving_style

    let define = Context.define

    let resolution_fixpoint = Context.resolution_fixpoint

    let error_map = Context.error_map

    module Builder = Callgraph.NullBuilder
  end

  module TypeCheckState = TypeCheck.State (TypeCheckContext)

  type non_bottom_t = {
    resolution: Resolution.t;
    errors: Error.t ErrorMap.Map.t;
  }

  type t =
    | Bottom
    | Value of non_bottom_t

  let value_exn = function
    | Bottom -> failwith "expected value, got bottom"
    | Value value -> value


  let pp format = function
    | Bottom -> Format.fprintf format " Bottom: true\n"
    | Value { resolution; errors } ->
        let global_resolution = Resolution.global_resolution resolution in
        let expected =
          let parser = GlobalResolution.annotation_parser global_resolution in
          let { Node.value = { Define.signature; _ }; _ } = Context.define in
          Annotated.Callable.return_annotation_without_applying_decorators ~signature ~parser
        in
        let errors =
          let error_to_string error =
            let error =
              let lookup reference =
                GlobalResolution.ast_environment global_resolution
                |> fun ast_environment ->
                AstEnvironment.ReadOnly.get_real_path_relative
                  ~configuration:Context.configuration
                  ast_environment
                  reference
              in
              Error.instantiate ~show_error_traces:true ~lookup error
            in
            Format.asprintf
              "    %a -> %s"
              Location.WithPath.pp
              (Error.Instantiated.location error)
              (Error.Instantiated.description error)
          in
          List.map (Map.data errors) ~f:error_to_string |> String.concat ~sep:"\n"
        in
        Format.fprintf
          format
          "  Expected return: %a\n  Resolution:\n%a\n  Errors:\n%s\n"
          Type.pp
          expected
          Resolution.pp
          resolution
          errors


  let show state = Format.asprintf "%a" pp state

  and equal left right =
    (* Ignore errors in unit tests. *)
    match left, right with
    | Bottom, Bottom -> true
    | Value left, Value right -> Resolution.refinements_equal left.resolution right.resolution
    | _ -> false


  let bottom = Bottom

  let create_with_errors ~errors ~resolution = Value { resolution; errors }

  let create ?(bottom = false) ~resolution () =
    if bottom then
      Bottom
    else
      create_with_errors ~errors:ErrorMap.Map.empty ~resolution


  let errors = function
    | Bottom -> []
    | Value { resolution; errors } ->
        let global_resolution = Resolution.global_resolution resolution in
        Map.data errors
        |> Error.deduplicate
        |> fun errors ->
        if Context.configuration.debug then
          errors
        else
          Error.filter ~resolution:global_resolution errors


  let less_or_equal ~left ~right =
    match left, right with
    | Bottom, Bottom -> true
    | Bottom, Value _ -> true
    | Value _, Bottom -> false
    | Value left, Value right ->
        let errors_subset =
          let left_errors = Map.data left.errors |> Error.Set.of_list in
          let right_errors = Map.data right.errors |> Error.Set.of_list in
          Set.is_subset left_errors ~of_:right_errors
        in
        errors_subset
        &&
        let global_resolution = Resolution.global_resolution left.resolution in
        Refinement.Store.less_or_equal
          ~global_resolution
          ~left:(Resolution.annotation_store left.resolution)
          ~right:(Resolution.annotation_store right.resolution)


  let widening_threshold = 3

  let widen ~previous ~next ~iteration =
    match previous, next with
    | Bottom, Bottom -> Bottom
    | Value _, Bottom -> previous
    | Bottom, Value _ -> next
    | Value previous, Value next ->
        let combine_errors ~key:_ left_error right_error =
          if iteration + 1 >= widening_threshold then
            { left_error with Error.kind = Error.Top }
          else
            Error.join
              ~resolution:(Resolution.global_resolution previous.resolution)
              left_error
              right_error
        in
        Value
          {
            previous with
            errors = Map.merge_skewed previous.errors next.errors ~combine:combine_errors;
            resolution =
              Resolution.outer_widen_refinements
                ~iteration
                ~widening_threshold
                previous.resolution
                next.resolution;
          }


  let join left right = widen ~previous:left ~next:right ~iteration:0

  let initial ~resolution =
    let state = TypeCheckState.initial ~resolution in
    let resolution = TypeCheckState.resolution state |> Option.value ~default:resolution in
    let errors =
      Context.error_map
      >>| TypeCheck.LocalErrorMap.all_errors
      |> Option.value ~default:[]
      |> List.fold ~init:ErrorMap.Map.empty ~f:(fun errors error -> ErrorMap.add ~errors error)
    in
    Value { resolution; errors }


  let forward ~statement_key:_ state ~statement:({ Node.value; _ } as statement) =
    match state with
    | Bottom -> Bottom
    | Value ({ resolution; errors } as state) -> (
        let global_resolution = Resolution.global_resolution resolution in
        let resolve annotation =
          Resolution.resolve_expression_to_type resolution annotation |> Type.weaken_literals
        in
        let validate_return ~expression ~actual =
          let create_missing_return_error expression actual =
            let {
              Node.location = define_location;
              value =
                {
                  Define.signature =
                    { async; return_annotation = return_annotation_expression; _ } as signature;
                  _;
                } as define;
            }
              =
              Context.define
            in
            let return_annotation =
              let annotation =
                let parser = GlobalResolution.annotation_parser global_resolution in
                Annotated.Callable.return_annotation_without_applying_decorators ~signature ~parser
              in
              if async then
                Type.coroutine_value annotation |> Option.value ~default:Type.Top
              else
                annotation
            in
            let return_annotation = Type.Variable.mark_all_variables_as_bound return_annotation in
            let actual =
              GlobalResolution.resolve_mutable_literals
                global_resolution
                ~resolve:(Resolution.resolve_expression_to_type resolution)
                ~expression
                ~resolved:actual
                ~expected:return_annotation
              |> GlobalResolution.resolved_type
            in
            let contains_literal_any =
              return_annotation_expression
              >>| Type.expression_contains_any
              |> Option.value ~default:false
            in
            if
              (not (Define.has_return_annotation define))
              || (contains_literal_any && Type.contains_prohibited_any return_annotation)
            then
              let given_annotation =
                Option.some_if (Define.has_return_annotation define) return_annotation
              in
              Some
                (Error.create
                   ~location:(Location.with_module ~qualifier:Context.qualifier define_location)
                   ~define:Context.define
                   ~kind:
                     (Error.MissingReturnAnnotation
                        {
                          name = Reference.create "$return_annotation";
                          annotation = Some actual;
                          given_annotation;
                          evidence_locations = [];
                          thrown_at_source = true;
                        }))
            else
              None
          in
          match create_missing_return_error expression actual with
          | None -> state
          | Some error ->
              let emit_error
                  errors
                  ({ Error.location = { Location.WithModule.start; stop; _ }; _ } as error)
                =
                let error =
                  let location = { Location.start; stop } in
                  match Map.find errors { ErrorMap.location; kind = Error.code error } with
                  | Some other_error -> Error.join ~resolution:global_resolution error other_error
                  | None -> error
                in
                ErrorMap.add ~errors error
              in
              { state with errors = emit_error errors error }
        in
        match value with
        | Statement.Expression
            {
              Node.value =
                Call
                  {
                    callee =
                      {
                        Node.value =
                          Name
                            (Name.Attribute
                              {
                                attribute = "__setitem__";
                                base = { Node.value = Name name; _ } as base;
                                _;
                              });
                        _;
                      };
                    arguments = [{ Call.Argument.value = key; _ }; { Call.Argument.value; _ }];
                  };
              _;
            }
          when is_simple_name name && Type.is_dictionary (resolve base) ->
            let resolution =
              refine_local
                ~resolution
                ~name
                ~annotation:
                  (Annotation.create_mutable
                     (Type.dictionary ~key:(resolve key) ~value:(resolve value)))
            in
            Value { state with resolution }
        | Statement.Expression
            {
              Node.value =
                Call
                  {
                    callee =
                      {
                        Node.value =
                          Name
                            (Name.Attribute
                              {
                                attribute = "append";
                                base = { Node.value = Name name; _ } as base;
                                _;
                              });
                        _;
                      };
                    arguments = [{ Call.Argument.value; _ }];
                  };
              _;
            }
          when is_simple_name name && Type.is_list (resolve base) ->
            let base_element =
              match resolve base with
              | Type.Parametric { name = "list"; parameters = [Single parameter] } -> parameter
              | base -> base
            in
            let annotation =
              GlobalResolution.join
                (Resolution.global_resolution resolution)
                (resolve value)
                base_element
              |> Type.list
              |> Annotation.create_mutable
            in
            Value { state with resolution = refine_local ~resolution ~name ~annotation }
        | Statement.Assign
            {
              value = { value = Dictionary { keywords = []; entries = [] }; _ };
              target = { Node.value = Name name; _ };
              _;
            }
          when is_simple_name name ->
            let resolution =
              refine_local
                ~resolution
                ~name
                ~annotation:
                  (Annotation.create_mutable (Type.dictionary ~key:Type.Bottom ~value:Type.Bottom))
            in
            Value { state with resolution }
        | Expression expression -> (
            let { Node.value = { Define.signature = { async; _ }; _ }; _ } = Context.define in
            match expression with
            | { Node.value = Expression.Yield yielded; _ } ->
                let yield_type =
                  match yielded with
                  | Some expression -> Resolution.resolve_expression_to_type resolution expression
                  | None -> Type.none
                in
                let actual =
                  if async then
                    Type.async_generator ~yield_type ()
                  else
                    Type.generator ~yield_type ()
                in
                Value (validate_return ~expression:None ~actual)
            | { Node.value = Expression.YieldFrom yielded_from; _ } ->
                let actual =
                  Resolution.resolve_expression_to_type resolution yielded_from
                  |> GlobalResolution.type_of_iteration_value ~global_resolution
                  |> Option.value ~default:Type.Any
                in
                Value (validate_return ~expression:None ~actual)
            | _ -> Value { state with resolution })
        | Statement.Assign
            { value = { value = List []; _ }; target = { Node.value = Name name; _ }; _ }
          when is_simple_name name ->
            let resolution =
              refine_local
                ~resolution
                ~name
                ~annotation:(Annotation.create_mutable (Type.list Type.Bottom))
            in
            Value { state with resolution }
        | Statement.Return { Return.expression; _ } ->
            let actual =
              Option.value_map
                expression
                ~f:(Resolution.resolve_expression_to_type resolution)
                ~default:Type.none
            in
            Value (validate_return ~expression ~actual)
        | _ -> (
            match Resolution.resolve_statement resolution statement with
            | Resolution.Unreachable -> Bottom
            | Resolution.Reachable { resolution; errors = statement_errors } ->
                Value
                  {
                    state with
                    resolution;
                    errors =
                      List.fold statement_errors ~init:errors ~f:(fun errors error ->
                          ErrorMap.add ~errors error);
                  }))


  let return_reference = Reference.create "$return"

  let initial_forward ~resolution =
    let { Node.value = { Define.signature = { parameters; parent; _ }; _ } as define; _ } =
      Context.define
    in
    let ({ resolution; _ } as state) = value_exn (initial ~resolution) in
    let make_parameter_name name =
      name
      |> String.filter ~f:(function
             | '*' -> false
             | _ -> true)
      |> Reference.create
    in
    let reset_parameter index resolution { Node.value = { Parameter.name; value; annotation }; _ } =
      match index, parent with
      | 0, Some _ when Define.is_method define && not (Define.is_static_method define) -> resolution
      | _ ->
          let reset =
            match annotation, value with
            | Some annotation, None
              when Type.is_any
                     (GlobalResolution.parse_annotation
                        (Resolution.global_resolution resolution)
                        annotation) ->
                true
            | None, None -> true
            | _ -> false
          in
          if reset then
            Resolution.new_local
              resolution
              ~reference:(make_parameter_name name)
              ~annotation:(Annotation.create_mutable Type.Bottom)
          else
            resolution
    in
    Value { state with resolution = List.foldi ~init:resolution ~f:reset_parameter parameters }


  let initial_backward ~forward =
    match forward with
    | Bottom -> Bottom
    | Value { resolution; errors } ->
        let expected_return =
          let parser =
            GlobalResolution.annotation_parser (Resolution.global_resolution resolution)
          in
          let { Node.value = { Define.signature; _ }; _ } = Context.define in
          Annotation.create_mutable
            (Annotated.Callable.return_annotation_without_applying_decorators ~signature ~parser)
        in
        let resolution =
          let resolution_with_return =
            Resolution.with_annotation_store resolution ~annotation_store:Refinement.Store.empty
            |> Resolution.new_local ~reference:return_reference ~annotation:expected_return
          in
          let filter name (annotation : Annotation.t) =
            not
              (Type.contains_unknown annotation.annotation
              || Type.is_not_instantiated annotation.annotation
              || Reference.equal name return_reference)
          in
          Resolution.update_refinements_with_filter
            ~old_resolution:resolution_with_return
            ~new_resolution:resolution
            ~filter
        in
        create_with_errors ~errors ~resolution


  let update_only_existing_annotations initial_state new_state =
    match initial_state, new_state with
    | ( Value ({ resolution = old_resolution; _ } as initial),
        Value { resolution = new_resolution; _ } ) ->
        let resolution = Resolution.update_existing_refinements ~old_resolution ~new_resolution in
        Value { initial with resolution }
    | _ -> new_state


  let check_entry = function
    | Bottom -> Bottom
    | Value ({ resolution; errors; _ } as state) ->
        let { Node.value = { Define.signature = { parameters; _ }; _ } as define; _ } =
          Context.define
        in
        let add_parameter_errors errors { Node.value = { Parameter.name; annotation; _ }; location }
          =
          let add_missing_parameter_error ~given_annotation =
            let reference = Reference.create name in
            Resolution.get_local resolution ~reference
            >>= (fun actual ->
                  Option.some_if (not (Type.is_any (Annotation.annotation actual))) actual)
            >>| (fun { Annotation.annotation; _ } ->
                  let error =
                    Error.create
                      ~location:(Location.with_module ~qualifier:Context.qualifier location)
                      ~kind:
                        (Error.MissingParameterAnnotation
                           {
                             name = reference;
                             annotation = Some annotation;
                             given_annotation;
                             evidence_locations = [];
                             thrown_at_source = true;
                           })
                      ~define:Context.define
                  in
                  ErrorMap.add ~errors error)
            |> Option.value ~default:errors
          in
          match annotation with
          | None -> add_missing_parameter_error ~given_annotation:None
          | Some annotation
            when Type.is_any
                   (GlobalResolution.parse_annotation
                      (Resolution.global_resolution resolution)
                      annotation) ->
              add_missing_parameter_error ~given_annotation:(Some Type.Any)
          | _ -> errors
        in
        let parameters =
          if Define.is_method define && not (Define.is_static_method define) then
            List.tl parameters |> Option.value ~default:[]
          else
            parameters
        in
        Value { state with errors = List.fold parameters ~init:errors ~f:add_parameter_errors }


  let backward ~statement_key:_ state ~statement =
    match state with
    | Bottom -> Bottom
    | Value ({ resolution; _ } as state) ->
        Type.Variable.Namespace.reset ();
        let resolve_assign annotation target_annotation =
          match annotation, target_annotation with
          | Type.Top, Type.Top -> None
          | Type.Top, target_annotation -> Some target_annotation
          | _ -> Some annotation
        in
        let forward_expression ~state:{ resolution; _ } ~expression =
          Resolution.resolve_expression_to_type resolution expression
        in
        let annotate_call_accesses statement resolution =
          let propagate resolution { Call.callee; arguments } =
            let resolved = forward_expression ~state ~expression:callee in
            let callable =
              match resolved with
              | Type.Callable callable -> Some callable
              | Type.Parametric { name = "BoundMethod"; _ } -> (
                  GlobalResolution.attribute_from_annotation
                    (Resolution.global_resolution resolution)
                    ~parent:resolved
                    ~name:"__call__"
                  >>| Annotated.Attribute.annotation
                  >>| Annotation.annotation
                  >>= function
                  | Type.Callable callable -> Some callable
                  | _ -> None)
              | _ -> None
            in
            match callable with
            | Some
                {
                  Type.Callable.implementation =
                    { Type.Callable.parameters = Type.Callable.Defined parameters; _ };
                  _;
                } ->
                let rec infer_annotations_list parameters arguments resolution =
                  let rec infer_annotation resolution parameter_annotation argument =
                    let state = { state with resolution } in
                    match Node.value argument with
                    | Expression.Name name when is_simple_name name ->
                        let reference = name_to_reference_exn name in
                        let resolved = forward_expression ~state ~expression:argument in
                        resolve_assign parameter_annotation resolved
                        >>| (fun refined ->
                              Resolution.refine_local
                                resolution
                                ~reference
                                ~annotation:(Annotation.create_mutable refined))
                        |> Option.value ~default:resolution
                    | Tuple arguments -> (
                        match parameter_annotation with
                        | Type.Tuple (Concrete parameter_annotations)
                          when List.length arguments = List.length parameter_annotations ->
                            List.fold2_exn
                              ~init:resolution
                              ~f:infer_annotation
                              parameter_annotations
                              arguments
                        | _ -> resolution)
                    | _ -> resolution
                  in
                  match parameters, arguments with
                  | ( Type.Callable.Parameter.Named { annotation; _ } :: parameters,
                      { Call.Argument.value = argument; _ } :: arguments ) ->
                      infer_annotation resolution annotation argument
                      |> infer_annotations_list parameters arguments
                  | _ -> resolution
                in
                infer_annotations_list parameters arguments resolution
            | _ -> resolution
          in
          Visit.collect_calls statement
          |> List.map ~f:Node.value
          |> List.fold ~init:resolution ~f:propagate
        in
        let resolution =
          match Node.value statement with
          | Statement.Assign { Assign.target; value; _ } -> (
              (* Get the annotations of the targets and set the 'value' to be the meet *)
              let rec propagate_assign resolution target_annotation value =
                let state = { state with resolution } in
                match Node.value value with
                | Expression.Name (Name.Identifier identifier) ->
                    let resolution =
                      let resolved = forward_expression ~state ~expression:value in
                      resolve_assign target_annotation resolved
                      >>| (fun refined ->
                            Resolution.refine_local
                              resolution
                              ~reference:(Reference.create identifier)
                              ~annotation:(Annotation.create_mutable refined))
                      |> Option.value ~default:resolution
                    in
                    annotate_call_accesses statement resolution
                | Call
                    {
                      callee =
                        {
                          value =
                            Name
                              (Name.Attribute
                                { attribute = "__iadd__"; base = { Node.value = Name name; _ }; _ });
                          _;
                        };
                      arguments = [{ Call.Argument.value; _ }];
                    } ->
                    let resolution =
                      resolve_assign target_annotation (forward_expression ~state ~expression:value)
                      >>| (fun refined ->
                            refine_local
                              ~resolution
                              ~name
                              ~annotation:(Annotation.create_mutable refined))
                      |> Option.value ~default:resolution
                    in
                    annotate_call_accesses statement resolution
                | Call _
                | Name _ ->
                    annotate_call_accesses statement resolution
                (* Recursively break down tuples such as x : Tuple[int, string] = y, z *)
                | Tuple values ->
                    let parameters =
                      match target_annotation with
                      | Type.Tuple (Concrete parameters) -> parameters
                      | Type.Tuple (Concatenation concatenation) ->
                          Type.OrderedTypes.Concatenation.extract_sole_unbounded_annotation
                            concatenation
                          >>| (fun annotation -> List.map values ~f:(fun _ -> annotation))
                          |> Option.value ~default:[]
                      | _ -> []
                    in
                    if List.length values = List.length parameters then
                      List.fold2_exn ~init:resolution ~f:propagate_assign parameters values
                    else
                      resolution
                | _ -> resolution
              in
              match Node.value target, Node.value value with
              | Tuple targets, Tuple values when List.length targets = List.length values ->
                  let target_annotations =
                    let resolve expression =
                      let resolved =
                        forward_expression ~state:{ state with resolution } ~expression
                      in
                      resolved
                    in
                    List.map targets ~f:resolve
                  in
                  List.fold2_exn ~init:resolution ~f:propagate_assign target_annotations values
              | _, _ ->
                  let resolved =
                    forward_expression ~state:{ state with resolution } ~expression:target
                  in
                  propagate_assign resolution resolved value)
          | Return { Return.expression = Some { Node.value = Name name; _ }; _ }
            when is_simple_name name ->
              let return_annotation =
                Option.value_exn (Resolution.get_local resolution ~reference:return_reference)
              in
              refine_local ~resolution ~name ~annotation:return_annotation
          | Return { Return.expression = Some { Node.value = Tuple expressions; _ }; _ } -> (
              let return_annotation =
                Option.value_exn (Resolution.get_local resolution ~reference:return_reference)
                |> Annotation.annotation
              in
              match return_annotation with
              | Tuple (Concrete parameters)
                when Int.equal (List.length parameters) (List.length expressions) ->
                  List.fold2_exn
                    parameters
                    expressions
                    ~init:resolution
                    ~f:(fun resolution annotation expression ->
                      match Node.value expression with
                      | Name name when is_simple_name name ->
                          refine_local
                            ~resolution
                            ~name
                            ~annotation:(Annotation.create_mutable annotation)
                      | _ -> resolution)
              | _ -> resolution)
          | _ -> annotate_call_accesses statement resolution
        in
        Value { state with resolution }
end

(* Perform a local type analysis to infer parameter and return type annotations. *)
let infer_local
    ~configuration
    ~global_resolution
    ~source:{ Source.source_path = { SourcePath.qualifier; _ }; _ }
    ~define:
      ({ Node.location; value = { Define.signature = { name; _ }; _ } as define } as define_node)
  =
  let module State = State (struct
    let configuration = configuration

    let qualifier = qualifier

    let define = Node.create ~location define

    let resolution_fixpoint = Some (LocalAnnotationMap.empty ())

    let error_map = Some (TypeCheck.LocalErrorMap.empty ())
  end)
  in
  let resolution = TypeCheck.resolution global_resolution (module State.TypeCheckContext) in

  let module Fixpoint = Fixpoint.Make (State) in
  Log.log ~section:`Check "Checking %a" Reference.pp name;
  let dump = Define.dump define in
  if dump then (
    Log.dump "Checking `%s`..." (Log.Color.yellow (Reference.show name));
    Log.dump "AST:\n%s" (Annotated.Define.create define_node |> Annotated.Define.show));
  let print_state name state =
    if dump then
      Log.dump "%s state:\n%a" name State.pp state;
    state
  in
  let cfg = Cfg.create define in
  let backward_fixpoint ~initial_forward ~initialize_backward =
    let rec fixpoint iteration ~initial_forward ~initialize_backward =
      let invariants =
        Fixpoint.forward ~cfg ~initial:initial_forward
        |> Fixpoint.exit
        >>| (fun forward_state -> initialize_backward ~forward:forward_state)
        |> Option.value ~default:initial_forward
        |> fun initial -> Fixpoint.backward ~cfg ~initial
      in
      let entry =
        invariants
        |> Fixpoint.entry
        >>| State.update_only_existing_annotations initial_forward
        >>| (fun post -> State.widen ~previous:initial_forward ~next:post ~iteration)
        |> Option.value ~default:initial_forward
      in
      if State.less_or_equal ~left:entry ~right:initial_forward then
        invariants
      else
        fixpoint (iteration + 1) ~initial_forward:entry ~initialize_backward
    in
    fixpoint 0 ~initial_forward ~initialize_backward
  in
  let exit =
    backward_fixpoint
      ~initial_forward:(State.initial_forward ~resolution)
      ~initialize_backward:State.initial_backward
    |> Fixpoint.entry
    >>| print_state "Entry"
    >>| State.check_entry
  in
  exit >>| State.errors |> Option.value ~default:[]


(* Infer parameter types of an overriding method when the base method is annotated. *)
let infer_parameters_from_parent
    ~global_resolution
    ~source:{ Source.source_path = { SourcePath.qualifier; _ }; _ }
    ~define:({ Node.value = { Define.signature = { parent; parameters; _ }; _ }; _ } as define)
  =
  let overridden_callable =
    parent
    >>| Reference.show
    >>= GlobalResolution.overrides
          ~resolution:global_resolution
          ~name:(Define.unqualified_name (Node.value define))
  in
  let missing_parameter_errors overridden_attribute =
    match Annotation.annotation (Annotated.Attribute.annotation overridden_attribute) with
    | Type.Parametric
        {
          name = "BoundMethod";
          parameters =
            [
              Single
                (Type.Callable
                  { implementation = { parameters = Defined overridden_parameters; _ }; _ });
              _;
            ];
        }
    | Type.Callable
        { Type.Callable.implementation = { parameters = Defined overridden_parameters; _ }; _ } ->
        let should_annotate name =
          match Identifier.sanitized name with
          | "self"
          | "cls" ->
              false
          | _ -> true
        in
        let missing_parameter_error = function
          | `Both (overridden_parameter, overriding_parameter) -> (
              match
                ( Type.Callable.RecordParameter.annotation overridden_parameter,
                  Type.Callable.RecordParameter.annotation overriding_parameter )
              with
              | ( Some overridden_annotation,
                  Some { Node.value = { Parameter.name; annotation = None; _ }; location } )
                when (not (Type.is_any overridden_annotation))
                     && (not (Type.contains_variable overridden_annotation))
                     && should_annotate name ->
                  Some
                    (Error.create
                       ~location:(Location.with_module ~qualifier location)
                       ~kind:
                         (Error.MissingParameterAnnotation
                            {
                              name = Reference.create name;
                              annotation = Some overridden_annotation;
                              given_annotation = None;
                              evidence_locations = [];
                              thrown_at_source = true;
                            })
                       ~define)
              | _ -> None)
          | `Left _ -> None
          | `Right _ -> None
        in
        let overriding_parameters =
          let to_type_parameter ({ Node.value = { Parameter.name; _ }; _ } as parameter) =
            { Type.Callable.RecordParameter.name; annotation = parameter; default = false }
          in
          List.map parameters ~f:to_type_parameter |> Type.Callable.Parameter.create
        in
        Type.Callable.Parameter.zip overridden_parameters overriding_parameters
        |> List.filter_map ~f:missing_parameter_error
    | _ -> []
  in
  overridden_callable >>| missing_parameter_errors |> Option.value ~default:[]


let merge_errors ~global_resolution errors =
  errors
  |> List.map ~f:(fun ({ Error.kind; _ } as error) ->
         { error with kind = Error.weaken_literals kind })
  |> Error.join_at_source ~resolution:global_resolution
  |> List.sort ~compare:Error.compare


let legacy_infer_for_define
    ~configuration
    ~global_resolution
    ~source:({ Source.source_path = { SourcePath.qualifier; relative; _ }; _ } as source)
    ~define:({ Node.location; value = { Define.signature = { name; _ }; _ } } as define)
  =
  try
    let local_errors = infer_local ~configuration ~global_resolution ~source ~define in
    let global_errors = infer_parameters_from_parent ~global_resolution ~source ~define in
    let errors = List.rev_append global_errors local_errors in
    merge_errors ~global_resolution errors
  with
  | ClassHierarchy.Untracked annotation ->
      Statistics.event
        ~name:"undefined type during type inference"
        ~integers:[]
        ~normals:["handle", relative; "define", Reference.show name; "type", annotation]
        ();
      if configuration.debug then
        [
          Error.create
            ~location:(Location.with_module ~qualifier location)
            ~kind:(Error.AnalysisFailure (UnexpectedUndefinedType annotation))
            ~define;
        ]
      else
        []


let infer_for_define ~configuration ~global_resolution ~source ~qualifier ~filename_lookup ~define =
  let timer = Timer.start () in
  let { Node.location; value = { Define.signature; _ } } = define in
  let abstract = Define.Signature.is_abstract_method signature in
  let error_to_inference { AnalysisError.location; kind; _ } =
    let open AnalysisError in
    match kind with
    | MissingReturnAnnotation { annotation = Some type_; _ } when not abstract ->
        Some Inference.{ type_; target = Return }
    | MissingParameterAnnotation { name; annotation = Some type_; _ } ->
        Some Inference.{ type_; target = Parameter { name } }
    | MissingGlobalAnnotation { name; annotation = Some type_; _ } ->
        Some Inference.{ type_; target = Global { name; location } }
    | MissingAttributeAnnotation
        { parent; missing_annotation = { name; annotation = Some type_; _ } } ->
        Some
          Inference.
            { type_; target = Attribute { parent = type_to_reference parent; name; location } }
    | _ -> None
  in
  let add_missing_annotation_error ~global_resolution ~lookup result error =
    match error_to_inference error with
    | None -> result
    | Some raw ->
        raw |> Inference.create |> LocalResult.add_inference ~global_resolution ~lookup result
  in
  let errors = legacy_infer_for_define ~configuration ~global_resolution ~source ~define in
  let result =
    List.fold
      ~init:
        (LocalResult.from_signature ~global_resolution ~lookup:filename_lookup ~qualifier define)
      ~f:(add_missing_annotation_error ~global_resolution ~lookup:filename_lookup)
      errors
  in
  let number_of_lines = location.stop.line - location.start.line + 1 in
  Statistics.performance
    ~flush:false
    ~randomly_log_every:1000
    ~always_log_time_threshold:0.050 (* 20 milliseconds *)
    ~section:`Infer
    ~name:"SingleDefineInfer"
    ~timer
    ~normals:
      [
        "name", Reference.show signature.name;
        "path", filename_lookup qualifier |> Option.value ~default:"*";
        "request kind", "SingleDefineInfer";
      ]
    ~integers:["number of lines", number_of_lines; "line", location.start.line]
    ();
  result


let should_analyze_define
    ~skip_annotated
    ~global_resolution
    { Node.value = { Define.signature = { return_annotation; parameters; _ }; _ } as define; _ }
  =
  let alias_environment = GlobalResolution.alias_environment global_resolution in
  let is_missing_or_invalid maybe_expression =
    let resolve_type expression =
      expression
      |> AliasEnvironment.ReadOnly.parse_annotation_without_validating_type_parameters
           alias_environment
    in
    maybe_expression >>| resolve_type >>| Type.is_untyped |> Option.value ~default:true
  in
  let is_parameter_missing_any_or_alias { Node.value = { Parameter.annotation; _ }; _ } =
    is_missing_or_invalid annotation
  in
  (not skip_annotated)
  || Define.is_toplevel define
  || Define.is_class_toplevel define
  || Define.is_constructor define
  || is_missing_or_invalid return_annotation
  || parameters |> List.exists ~f:is_parameter_missing_any_or_alias


let empty_infer_for_define ~global_resolution ~qualifier ~define =
  let lookup _ = None in
  TypeInferenceData.LocalResult.from_signature ~global_resolution ~lookup ~qualifier define


let infer_for_module
    ?(skip_annotated = true)
    ~configuration
    ~global_resolution
    ~filename_lookup
    ({ Ast.Source.source_path = { qualifier; _ } as source_path; _ } as source)
  =
  Log.debug "Running infer for %s..." source_path.relative;
  (* We cannot use should_analyze_define as a filter because we need to know about all defines in
     order to reliably exclude duplicates from overloads *)
  let check define =
    if should_analyze_define ~skip_annotated ~global_resolution define then
      infer_for_define ~configuration ~global_resolution ~source ~qualifier ~filename_lookup ~define
    else
      empty_infer_for_define ~global_resolution ~qualifier ~define
  in
  source |> Preprocessing.defines ~include_toplevels:true |> List.map ~f:check


module Testing = struct
  let define_names_to_analyze ~global_resolution source =
    source
    |> Preprocessing.defines ~include_toplevels:true
    |> List.filter ~f:(should_analyze_define ~skip_annotated:true ~global_resolution)
    |> List.map ~f:(fun define -> define |> Node.value |> Define.name)
end
