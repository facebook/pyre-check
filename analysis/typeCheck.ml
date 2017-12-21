(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Configuration
open Expression
open Pyre
open Statement


module Error = PyreError


module State = struct
  (* `environment` provides access to the global type environment.

     `errors` is a map from locations to errors. We assume there can be only
     one error per location.

     `annotations` is a map from accesses to annotations. The map itself does not have a bottom or
     top element.

     The order is defined by values of the map, i.e.
     left <= right <=>
        keys(left) \subset \keys(right) \and
        \forall key \in keys(left): left(key) <= right(key)

     The join takes the union of keys and does an element-wise join on the
     values. *)
  type t = {
    environment: (module Environment.Reader);
    errors: Error.t Location.Map.t;
    annotations: Annotation.t Instantiated.Access.Map.t;
    define: Statement.define Node.t;
    lookup: Lookup.t option;
  }


  type coverage = {
    full: int;
    partial: int;
    untyped: int;
  }


  let resolution { environment; annotations; _ } =
    Environment.resolution
      environment
      ~annotations
      ()


  let pp
      format
      ({
        annotations;
        errors;
        define = { Node.value = define; _ };
        _;
      } as state) =
    let resolution = resolution state in
    let expected =
      let open Annotated in
      Define.create define
      |> Define.return_annotation ~resolution
    in
    let annotations =
      let annotation_to_string (name, annotation) =
        Format.asprintf
          "    %a -> %a"
          Instantiated.Access.pp name
          Annotation.pp annotation
      in
      List.map (Map.to_alist annotations) ~f:annotation_to_string
      |> String.concat ~sep:"\n"
    in
    let errors =
      let error_to_string (location, error) =
        Format.asprintf
          "    %a -> %s"
          Location.pp location
          (Error.description error ~detailed:true)
      in
      List.map (Map.to_alist errors) ~f:error_to_string
      |> String.concat ~sep:"\n"
    in
    Format.fprintf
      format
      "  Expected return: %a\n  Types:\n%s\n  Errors:\n%s\n"
      Type.pp expected
      annotations
      errors


  let show state =
    Format.asprintf "%a" pp state


  let create ~environment ~annotations ~define ?lookup () =
    {
      environment;
      errors = Location.Map.empty;
      annotations = Instantiated.Access.Map.of_alist_exn annotations;
      define;
      lookup;
    }


  let errors
      ignore_lines
      configuration
      ({
        errors;
        define = ({ Node.location; value = define; _ } as define_node);
        _;
      } as state) =
    let resolution = resolution state in

    let class_initialization_errors errors =
      let open Annotated in
      (* Ensure non-nullable typed fields are instantiated in init. *)
      if not (Instantiated.Define.is_constructor define) then
        errors
      else
        let check_class_fields class_definition =
          let propagate_initialization_errors errors field =
            let expected = Annotation.annotation (Field.annotation field) in
            match Field.name field with
            | Access name
              when not (Type.equal expected Type.Top || Option.is_some (Field.value field)) ->
                let assign_exists { Statement.Define.body; _ } name =
                  let iterate initial { Node.value; _ } =
                    match value with
                    | Assign {
                        Assign.target = { Node.value = Access (self::access); _ };
                        value = Some _;
                        compound = None;
                        _;
                      } ->
                        if Instantiated.Access.equal [self] (Instantiated.Access.create "self") &&
                           Instantiated.Access.equal access name then
                          true
                        else
                          initial
                    | _ -> initial
                  in
                  List.fold ~f:iterate ~init:false body
                in
                if assign_exists define name || Type.is_optional expected then
                  errors
                else
                  let error =
                    {
                      Error.location = Field.location field;
                      kind = Error.UninitializedField {
                          Error.name;
                          parent = class_definition;
                          mismatch = {
                            Error.expected;
                            actual = (Type.optional expected)
                          };
                        };
                      define = define_node;
                    }
                  in
                  error :: errors
            | _ -> errors
          in
          Class.field_fold
            ~initial:errors
            ~resolution
            ~f:propagate_initialization_errors
            class_definition
        in
        Define.parent_definition ~resolution (Define.create define)
        >>| check_class_fields
        |> Option.value ~default:errors
    in

    let ignore_errors ignore_lines errors =
      let _, errors =
        let ignore_map = Int.Map.of_alist_exn ignore_lines in
        let ignore error =
          Map.find ignore_map (Error.location error |> Location.line)
          >>| (fun ignored ->
              List.is_empty ignored || List.mem ~equal:(=) ignored (Error.code error)
            )
          |> Option.value ~default:false
        in
        List.partition_tf ~f:ignore errors
      in
      errors
    in

    let ignore_none_confusion errors =
      (* Special-case ignore errors complaining that -> None function returns explicit None *)
      let is_none_confusion error =
        match error with
        | {
          Error.kind = Error.IncompatibleParameterType {
              Error.mismatch = { Error.actual; expected };
              _;
            };
          _;
        }
        | { Error.kind = Error.IncompatibleReturnType { Error.actual; expected; }; _ }
        | {
          Error.kind = Error.IncompatibleType { Error.mismatch = { Error.actual; expected }; _ };
          _;
        } ->
            not (Type.equal actual (Type.Optional Type.Bottom) &&
                 Resolution.less_or_equal resolution ~left:Type.void ~right:expected)
        | _ ->
            true
      in
      List.filter ~f:is_none_confusion errors
    in

    let ignore_unimplemented_returns errors =
      let define_implemented error =
        match error with
        | {
          Error.kind = Error.IncompatibleReturnType _;
          define = { Node.value = { Statement.Define.body; _ }; _ };
          _;
        } ->
            let rec check_statements = function
              | [{ Node.value = Statement.Pass; _ }; { Node.value = Statement.Return None; _ }] ->
                  false
              | { Node.value =
                    Statement.Expression { Node.value = Expression.String _; _ }; _ } :: tail ->
                  check_statements tail
              | _ ->
                  true
            in
            check_statements body
        | _ ->
            true
      in
      List.filter ~f:define_implemented errors
    in

    let filter_errors errors =
      let open Error in
      let suppress_in_strict ({ kind; _ } as error) =
        if due_to_analysis_limitations error then
          match kind with
          | IncompatibleParameterType { mismatch = _; _ }
          | IncompatibleReturnType _
          | MissingReturnAnnotation _ ->
              false
          (* Temporarily suppressing the missing parameter errors, to be investigated later *)
          | MissingParameterAnnotation _
          | UndefinedMethod _
          | UndefinedType _
          | UninitializedField _
          | _ ->
              true
        else
          false
      in

      let suppress_in_default ({ kind; Error.define = { Node.value = define; _ }; _ } as error) =
        due_to_analysis_limitations error ||
        due_to_mismatch_with_any error ||
        Instantiated.Define.is_untyped define ||
        (match kind with
         | MissingReturnAnnotation { annotation = actual; _ }
         | MissingParameterAnnotation { annotation = actual; _ }
         | MissingAnnotation { annotation = actual; _ } ->
             Type.equal actual Type.Object ||
             not (Type.is_primitive actual && configuration.infer)
         | UndefinedMethod _
         | UndefinedType _
         | UninitializedField _ ->
             true
         | _ ->
             false)
      in

      let suppress_in_infer ({ kind; _ } as error) =
        match kind with
        | MissingReturnAnnotation { annotation = actual; _ }
        | MissingParameterAnnotation { annotation = actual; _ }
        | MissingAnnotation { annotation = actual; _ } ->
            due_to_analysis_limitations error ||
            Type.equal actual Type.Object || Type.is_bottom actual
        | _ ->
            true
      in

      (* Angelic assumption: `Top` indicates we've hit a limitation of the
         analysis and decide not to report on it. *)
      let filtered, errors =
        let filter ({ kind; _ } as error) =
          if configuration.gradual then
            if configuration.strict then
              suppress_in_strict error
            else if configuration.declare then
              true
            else
              suppress_in_default error
          else if configuration.infer then
            suppress_in_infer error
          else
            match kind with
            | MissingReturnAnnotation _
            | MissingParameterAnnotation _
            | MissingAnnotation _
            | UndefinedMethod _
            | UndefinedType _ ->
                true
            | _ ->
                due_to_analysis_limitations error
        in
        List.partition_tf ~f:filter errors
      in
      List.iter
        ~f:(fun error -> Log.debug "Not reporting %a" pp error)
        filtered;
      if List.is_empty filtered then
        Log.debug "Verified"
      else
        Log.debug
          "Suppressed %d of %d errors"
          (List.length filtered)
          (List.length filtered + List.length errors);
      errors
    in

    Map.data errors
    |> Error.join_at_define ~resolution ~location
    |> class_initialization_errors
    |> ignore_none_confusion
    |> ignore_unimplemented_returns
    |> apply_if ~f:(ignore_errors ignore_lines) ~condition:configuration.gradual
    |> apply_if ~f:filter_errors ~condition:(not configuration.debug || configuration.infer)


  let coverage { annotations; _ } =
    let aggregate
        ~key:_
        ~data:{ Annotation.annotation; _ }
        ({ full; partial; untyped } as coverage) =
      if Type.is_untyped annotation then
        { coverage with untyped = untyped + 1 }
      else if Type.is_partially_typed annotation then
        { coverage with partial = partial + 1 }
      else
        { coverage with full = full + 1 }
    in
    Map.fold ~init:{ full = 0; partial = 0; untyped = 0 } ~f:aggregate annotations


  let initial_forward
      ?lookup
      environment
      ({
        Node.location;
        value = ({ Define.parent; parameters; _ } as define);
      } as define_node) =
    let { annotations; errors; _ } as initial =
      create ~environment ~annotations:[]  ~define:define_node ?lookup ()
    in
    let resolution = resolution initial in

    (* Check parameters. *)
    let annotations, errors =
      let parameter
          index
          (annotations, errors)
          { Node.location; value = { Parameter.name; value; annotation }} =
        let access = [Access.Identifier name] in
        match index, parent with
        | 0, Some parent
          when Instantiated.Define.is_method define &&
               not (Instantiated.Define.is_static_method define) ->
            let annotation =
              if Instantiated.Define.is_class_method define then
                (* First parameter of a method is a class object. *)
                Type.Object
              else
                (* First parameter of a method is the callee object. *)
                Resolution.parse_annotation
                  resolution
                  (Node.create (Access parent))
            in
            Map.add ~key:access ~data:(Annotation.create annotation) annotations, errors
        | _ ->
            let add_missing_parameter_error value ~due_to_any =
              let annotation = Annotated.resolve ~resolution value in
              let error =
                {
                  Error.location;
                  kind = Error.MissingParameterAnnotation { Error.name; annotation; due_to_any };
                  define = define_node;
                }
              in
              Map.add ~key:access ~data:(Annotation.create annotation) annotations,
              Map.add ~key:location ~data:error errors
            in
            begin
              match value, annotation with
              | Some value, Some annotation when
                  Type.equal (Resolution.parse_annotation resolution annotation) Type.Object ->
                  add_missing_parameter_error value ~due_to_any:true
              | Some value, None ->
                  add_missing_parameter_error value ~due_to_any:false
              | _, Some annotation ->
                  let annotation =
                    match Resolution.parse_annotation resolution annotation with
                    | Type.Variable { Type.constraints; _ }
                      when not (List.is_empty constraints) ->
                        Type.union constraints
                    | annotation -> annotation
                  in
                  Map.add
                    ~key:access
                    ~data:(Annotation.create_immutable ~global:false annotation)
                    annotations,
                  errors
              | _ ->
                  Map.add ~key:access ~data:(Annotation.create Type.Bottom) annotations,
                  errors
            end
      in
      List.foldi ~init:(annotations, errors) ~f:parameter parameters
    in

    (* Check behavioral subtyping. *)
    let errors =
      if Instantiated.Define.is_constructor define ||
         Instantiated.Define.is_class_method define ||
         Instantiated.Define.is_static_method define then
        errors
      else
        let open Annotated in
        (Define.create define
         |> Define.method_definition ~resolution
         >>= fun definition -> Method.overrides definition ~resolution
         >>| fun overridden_method ->
         (* Check strengthening of postcondition. *)
         let errors =
           let expected = Method.return_annotation overridden_method ~resolution in
           let actual = Method.return_annotation definition ~resolution in
           if not (Resolution.less_or_equal resolution ~left:actual ~right:expected) then
             let error =
               {
                 Error.location;
                 kind = Error.InconsistentOverride {
                     Error.overridden_method;
                     override = Error.WeakenedPostcondition;
                     mismatch = { Error.actual; expected };
                   };
                 define = define_node;
               }
             in
             Map.add ~key:location ~data:error errors
           else
             errors
         in

         (* Check weakening of precondition. *)
         let parameters = Method.parameter_annotations_positional definition ~resolution in
         let parameter ~key ~data errors =
           let actual = data in
           match Map.find parameters key with
           | Some expected ->
               if not (Resolution.less_or_equal resolution ~left:actual ~right:expected) then
                 let error =
                   {
                     Error.location;
                     kind = Error.InconsistentOverride {
                         Error.overridden_method;
                         override = Error.StrengthenedPrecondition;
                         mismatch = { Error.actual; expected };
                       };
                     define = define_node;
                   }
                 in
                 Map.add ~key:location ~data:error errors
               else
                 errors
           | None ->
               let error =
                 {
                   Error.location;
                   kind = Error.InconsistentOverride {
                       Error.overridden_method;
                       override = Error.StrengthenedPrecondition;
                       mismatch = { Error.actual; expected = Type.void };
                     };
                   define = define_node;
                 }
               in
               Map.add ~key:location ~data:error errors
         in
         Map.fold
           ~init:errors
           ~f:parameter
           (Method.parameter_annotations_positional overridden_method ~resolution))
        |> Option.value ~default:errors
    in

    { initial with annotations; errors }


  let less_or_equal left right =
    let resolution = resolution left in

    let entry_less_or_equal other less_or_equal ~key ~data sofar =
      sofar && match Map.find other key with
      | Some other ->
          less_or_equal data other
      | _ ->
          false
    in
    Map.fold
      ~init:true
      ~f:(entry_less_or_equal right.errors (Error.less_or_equal ~resolution))
      left.errors &&
    Map.fold
      ~init:true
      ~f:(entry_less_or_equal right.annotations (AnnotationOrder.less_or_equal ~resolution))
      left.annotations


  let equal left right =
    (* Ignore errors in unit tests. *)
    Map.equal Annotation.equal left.annotations right.annotations


  let rec join left right =
    let resolution = resolution left in

    let merge join ~key:_ = function
      | `Both (left, right) ->
          Some (join left right)
      | `Left state
      | `Right state ->
          Some state
    in
    {
      left with
      errors =
        Map.merge
          ~f:(merge (Error.join ~resolution))
          left.errors
          right.errors;
      annotations =
        Map.merge
          ~f:(merge (AnnotationOrder.join ~resolution))
          left.annotations
          right.annotations;
    }


  and meet left right =
    let resolution = resolution left in

    let merge meet ~key:_ = function
      | `Both (left, right) ->
          Some (meet left right)
      | `Left _
      | `Right _ ->
          None
    in
    {
      left with
      errors =
        Map.merge
          ~f:(merge (Error.meet ~resolution))
          left.errors
          right.errors;
      annotations =
        Map.merge
          ~f:(merge (AnnotationOrder.meet ~resolution))
          left.annotations
          right.annotations;
    }


  and initial_backward ~environment define ~forward:{ annotations; errors; _ } =
    let resolution = Environment.resolution environment () in
    let expected_return =
      Annotated.Define.create (Node.value define)
      |> Annotated.Define.return_annotation ~resolution
      |> Annotation.create
    in
    let backward_initial_state =
      create ~environment ~annotations:[Preprocessing.return_access, expected_return] ~define ()
    in
    let combine_annotations left right =
      let add_annotation ~key ~data map =
        if Type.is_unknown data.Annotation.annotation ||
           Type.is_bottom data.Annotation.annotation ||
           Instantiated.Access.equal key Preprocessing.return_access then
          map
        else
          Map.add ~key ~data map
      in
      Map.fold ~init:left ~f:add_annotation right
    in
    {
      backward_initial_state with
      errors;
      annotations = combine_annotations backward_initial_state.annotations annotations
    }


  and update_only_existing_annotations initial_state new_state =
    let update ~key ~data map =
      if Option.is_some (Map.find map key) then
        Map.add ~key ~data map
      else
        map
    in
    {
      initial_state with
      annotations = Map.fold ~init:initial_state.annotations ~f:update new_state.annotations
    }


  and widening_threshold =
    10


  and widen ~previous ~next ~iteration =
    let resolution = resolution previous in

    let merge widen ~key:_ = function
      | `Both (previous, next) ->
          Some (widen ~previous ~next ~iteration)
      | `Left state
      | `Right state ->
          Some state
    in
    {
      previous with
      errors =
        Map.merge
          ~f:(merge (Error.widen ~resolution))
          previous.errors
          next.errors;
      annotations =
        Map.merge
          ~f:(merge (AnnotationOrder.widen ~resolution ~widening_threshold))
          previous.annotations
          next.annotations;
    }


  and forward
      ({ environment;
         errors;
         annotations;
         define = ({ Node.value = { Define.async; _ } as define; _ } as define_node);
         lookup } as state)
      statement =
    let update_resolution = resolution in
    let resolution = resolution state in
    let expected =
      let annotation =
        let open Annotated in
        Define.create define
        |> Define.return_annotation ~resolution
      in
      if async && Type.is_awaitable annotation then
        Type.awaitable_value annotation
      else
        annotation
    in

    let fold_assign ~accumulator ~f ~target ~value =
      let rec propagate_assign accumulator target value_annotation =
        match Node.value target with
        | Access access ->
            f ~target ~access ~value_annotation accumulator
        (* Recursively break down tuples such as x, y = z : Tuple[int, string] *)
        | Tuple targets ->
            let parameters =
              match value_annotation with
              | Type.Tuple (Type.Bounded parameters) ->
                  parameters
              | Type.Tuple (Type.Unbounded parameter) ->
                  List.map ~f:(fun _ -> parameter) targets
              | _ ->
                  []
            in
            if List.length targets = List.length parameters then
              List.fold2_exn
                ~init:accumulator
                ~f:propagate_assign
                targets
                parameters
            else
              accumulator
        | _ ->
            accumulator
      in
      match (Node.value target), (Node.value value) with
      (* Tuples of individual assignments *)
      | Tuple targets, Tuple values
        when List.length targets = List.length values ->
          let value_annotations =
            List.map
              ~f:(Annotated.resolve ~resolution)
              values
          in
          List.fold2_exn
            ~init:accumulator
            ~f:propagate_assign
            targets
            value_annotations
      | _, _ ->
          let value_annotation = Annotated.resolve ~resolution value in
          propagate_assign accumulator target value_annotation
    in

    let rec forward_annotations state statement =
      let resolution = update_resolution state in
      match Node.value statement with
      | Assign { Assign.compound = Some _; _ } ->
          annotations
      | Assign {
          Assign.target = { Node.value = Access access; _ };
          annotation = Some annotation;
          _;
        } ->
          (* Type annotations override values. *)
          let annotation =
            Resolution.parse_annotation resolution annotation
            |> Annotation.create_immutable ~global:false
          in
          Map.add ~key:access ~data:annotation annotations
      | Assign { Assign.target; value = Some value; _ } ->
          let open Annotated in
          let forward_annotations
              ~target:_
              ~access
              ~value_annotation
              annotations =
            let annotation = Map.find annotations access in
            let element = Access.last_element ~resolution (Annotated.Access.create access) in
            match annotation, element with
            | Some annotation, _ when Annotation.is_immutable annotation ->
                Map.add
                  ~key:access
                  ~data:(AnnotationOrder.refine ~resolution annotation value_annotation)
                  annotations
            | _, Access.Element.Field (Access.Element.Defined field) ->
                let refined =
                  AnnotationOrder.refine
                    ~resolution
                    (Field.annotation field)
                    value_annotation
                in
                Map.add ~key:access ~data:refined annotations
            | _, _ ->
                Map.add ~key:access ~data:(Annotation.create value_annotation) annotations
          in
          fold_assign ~target ~value ~f:forward_annotations ~accumulator:annotations
      | Assign _ ->
          annotations

      | Assert { Assert.test; _ } ->
          let rec asserted annotations expression =
            match Node.value expression with
            | Access [
                Access.Call {
                  Node.value = {
                    Call.name = { Node.value = Access [Access.Identifier name]; _ };
                    arguments = [
                      { Argument.name = None; value = { Node.value = Access access; _ } };
                      { Argument.name = None; value = annotation };
                    ];
                  };
                  _;
                };
              ] when Identifier.show name = "isinstance" ->
                let annotation =
                  match annotation with
                  | { Node.value = Tuple elements; _ } ->
                      Type.Union
                        (List.map
                           ~f:(Resolution.parse_annotation resolution)
                           elements)
                  | _ ->
                      Resolution.parse_annotation resolution annotation
                in
                Map.add ~key:access ~data:(Annotation.create annotation) annotations
            | UnaryOperator {
                UnaryOperator.operator = UnaryOperator.Not;
                operand = {
                  Node.value =
                    Access [
                      Access.Call {
                        Node.value = {
                          Call.name = { Node.value = Access [Access.Identifier name]; _ };
                          arguments = [
                            { Argument.name = None; value = { Node.value = Access access; _ } };
                            { Argument.name = None; value = annotation };
                          ];
                        };
                        _;
                      };
                    ];
                  _;
                };
              } when Identifier.show name = "isinstance" ->
                begin
                  match Map.find annotations access with
                  | Some { Annotation.annotation = Type.Union parameters; _ } ->
                      let parameters = Type.Set.of_list parameters in
                      let constraints =
                        (match annotation with
                         | { Node.value = Tuple elements; _ } ->
                             List.map
                               ~f:(Resolution.parse_annotation resolution)
                               elements
                         | _ ->
                             [Resolution.parse_annotation resolution annotation])
                        |> Type.Set.of_list
                      in
                      let constrained =
                        Set.diff parameters constraints
                        |> Set.to_list
                        |> Type.union
                      in
                      Map.add ~key:access ~data:(Annotation.create constrained) annotations
                  | _ ->
                      annotations
                end

            | Access access ->
                let open Annotated in
                let element = Access.last_element ~resolution (Annotated.Access.create access) in
                begin
                  match Map.find annotations access, element with
                  | Some { Annotation.annotation = Type.Optional parameter; _ }, _ ->
                      Map.add ~key:access ~data:(Annotation.create parameter) annotations
                  | _, Access.Element.Field (Access.Element.Defined field) ->
                      begin
                        match Field.annotation field with
                        | {
                          Annotation.annotation = Type.Optional parameter;
                          mutability = Annotation.Mutable
                        }
                        | {
                          Annotation.annotation = _;
                          mutability = Annotation.Immutable {
                              Annotation.original = Type.Optional parameter;
                              _;
                            };
                        } ->
                            let refined =
                              AnnotationOrder.refine
                                ~resolution
                                (Field.annotation field)
                                parameter
                            in
                            Map.add ~key:access ~data:refined annotations
                        | _ -> annotations
                      end
                  | _ ->
                      annotations
                end
            | BooleanOperator { BooleanOperator.left; operator; right } ->
                let { annotations; _ } =
                  let update state expression =
                    forward_annotations
                      state
                      (Statement.assume expression)
                  in
                  match operator with
                  | BooleanOperator.And ->
                      let annotations = forward_annotations state (Statement.assume left) in
                      let left = update state left in
                      let right = update { state with annotations } right in
                      let merge ~key:_ = function
                        | `Both (left, right) -> Some (AnnotationOrder.meet ~resolution left right)
                        | `Left left -> Some left
                        | `Right right -> Some right
                      in
                      let annotations = Map.merge ~f:merge left right in
                      { state with annotations }
                  | BooleanOperator.Or ->
                      let negated_left =
                        update state (Expression.normalize (Expression.negate left))
                      in
                      let left = update state left in
                      let right = update { state with annotations = negated_left } right in
                      join { state with annotations = left } { state with annotations = right }
                in
                annotations
            | ComparisonOperator {
                ComparisonOperator.left;
                right = [
                  ComparisonOperator.IsNot,
                  { Node.value = Access [ Access.Identifier identifier; ]; _ }
                ];
              } when Identifier.show identifier = "None" ->
                asserted annotations left
            | ComparisonOperator {
                ComparisonOperator.left = { Node.value = Access access; _ };
                right = [
                  ComparisonOperator.Is,
                  { Node.value = Access [ Access.Identifier identifier; ]; _ }
                ];
              } when Identifier.show identifier = "None" ->
                let open Annotated in
                let element = Access.last_element ~resolution (Annotated.Access.create access) in
                begin
                  match element with
                  | Access.Element.Field (Access.Element.Defined field) ->
                      let refined =
                        AnnotationOrder.refine
                          ~resolution
                          (Field.annotation field)
                          (Type.Optional Type.Bottom)
                      in
                      Map.add ~key:access ~data:refined annotations
                  | _ ->
                      Map.add
                        ~key:access
                        ~data:(Annotation.create (Type.Optional Type.Bottom))
                        annotations
                end
            | _ ->
                annotations
          in
          asserted annotations test

      | Define _ ->
          (* Don't propagate accesses in nested functions, they're analyzed separately. *)
          annotations

      | Global identifiers ->
          let access = Instantiated.Access.create_from_identifiers identifiers in
          let annotation =
            Resolution.resolve resolution (Node.create (Access access))
            |> Annotation.create_immutable ~global:true
          in
          Map.add ~key:access ~data:annotation annotations

      | _ ->
          (* Walk through accesses and infer annotations as we go. *)
          let propagated =
            let propagate annotations access =
              let propagate _ ~annotations ~resolved:_ ~element =
                lookup >>| Lookup.update ~element |> ignore;
                annotations
              in
              Annotated.Access.fold
                ~resolution:(Resolution.with_annotations resolution annotations)
                ~initial:annotations
                ~f:propagate
                (Annotated.Access.create access)
            in
            Visit.collect_accesses statement
            |> List.fold ~init:annotations ~f:propagate
          in
          let set_mutability ~key ~data =
            let mutability =
              Map.find annotations key
              >>| Annotation.mutability
              |> Option.value ~default:Annotation.Mutable
            in
            { data with Annotation.mutability }
          in
          Map.mapi ~f:set_mutability propagated
    in
    let annotations = forward_annotations state statement in

    (* Gets typing errors that occur when trying to execute 'statement' in 'state' *)
    let errors =
      let add_errors errors =
        let add_error sofar error =
          Map.add ~key:error.Error.location ~data:error sofar
        in
        List.fold ~init:errors ~f:add_error
      in

      let check_parameters ~resolution call =
        function
        | Some ({ Signature.instantiated = callee; _ } as signature) ->
            let check_parameter ~argument:_ ~position ~offset ~location ~name ~actual ~expected =
              if not (Resolution.less_or_equal resolution ~left:actual ~right:expected) then
                begin
                  let start_position =
                    (* Account for the `self` parameter in instance methods. *)
                    if Instantiated.Define.is_method callee &&
                       not (Instantiated.Define.is_static_method callee) &&
                       not (Instantiated.Define.is_constructor callee) then
                      0
                    else
                      1
                  in
                  Some {
                    Error.location;
                    kind = Error.IncompatibleParameterType {
                        Error.name;
                        position = position + offset + start_position;
                        callee;
                        mismatch = { Error.expected; actual };
                      };
                    define = define_node;
                  }
                end
              else
                None
            in
            Annotated.Call.check_parameters
              ~resolution
              ~check_parameter
              ~add_error:(fun errors error -> error :: errors)
              ~init:[]
              call
              signature
        | None -> []
      in
      (* Check a full access sequence, including available fields and calls. *)
      let check_access ~resolution errors access =
        let check_access new_errors ~annotations:_ ~resolved:_ ~element =
          if not (List.is_empty new_errors) then
            new_errors
          else
            let open Annotated.Access.Element in
            match element with
            | Call { call; callee; _ } ->
                check_parameters ~resolution call callee
            | Method { location; access; annotation; call; callee; backup; } ->
                let annotation = Annotation.annotation annotation in
                let unresolved_method_errors =
                  match callee, Resolution.class_definition resolution annotation with
                  | None, Some _ ->
                      [
                        {
                          Error.location;
                          kind = Error.UndefinedMethod {
                              Error.annotation;
                              call;
                            };
                          define = define_node;
                        }
                      ]
                  | None, None ->
                      [
                        {
                          Error.location;
                          kind = Error.UndefinedType annotation;
                          define = define_node;
                        }
                      ]
                  | Some _, _ ->
                      []
                in
                let parameter_errors =
                  (* We check the actual call first, if there are errors and there is a backup call
                     for that particular method we check that, flipping the arguments,
                     e.g. `x.__add__(y) -> y.__radd__(x) iff the first call raises NotImplemented`.
                     If the backup method type-checks we accept the call. If not we record the
                     original error. *)
                  let errors = check_parameters ~resolution call callee in
                  if not (List.is_empty errors) then
                    backup
                    >>= (fun (call, callee) ->
                        let backup_errors =
                          let arguments =
                            [
                              {
                                Argument.name = None;
                                value = Node.create (Access access);
                              };
                            ]
                          in
                          check_parameters
                            ~resolution
                            (Annotated.Call.with_arguments call arguments)
                            (Some callee)
                        in
                        if List.is_empty backup_errors then
                          Some []
                        else
                          None)
                    |> Option.value ~default:errors
                  else
                    errors
                in
                unresolved_method_errors @ parameter_errors
            | _ -> new_errors
        in
        Annotated.Access.fold
          ~resolution
          ~initial:[]
          ~f:check_access
          (Annotated.Access.create access)
        |> add_errors errors
      in

      let rec check_entry ~resolution errors { Dictionary.key; value } =
        let errors = check_expression ~resolution errors key in
        check_expression ~resolution errors value

      and check_generator
          ~resolution
          errors
          { Comprehension.target; iterator; conditions; _ } = (* TODO(T23723699): check async. *)
        let errors = check_expression ~resolution errors target in
        let errors = check_expression ~resolution errors iterator in
        List.fold
          ~init:errors
          ~f:(check_expression ~resolution)
          conditions
      and check_expression ~resolution errors expression =
        match expression.Node.value with
        | Access access ->
            let errors = check_access ~resolution errors access in
            let check_single_access errors access =
              match access with
              | Access.Identifier _ ->
                  errors

              | Access.Call { Node.value = { Call.name; arguments }; _ } ->
                  let check_argument errors { Argument.value; _ } =
                    check_expression ~resolution errors value
                  in
                  let errors = check_expression ~resolution errors name in
                  List.fold ~f:check_argument ~init:errors arguments

              | Access.Expression expression -> check_expression ~resolution errors expression
              | Access.Subscript subscripts ->
                  let check_subscript errors = function
                    | Access.Index expression -> check_expression ~resolution errors expression
                    | Access.Slice { Access.lower; upper; step } ->
                        let check_optional_expression expression errors =
                          match expression with
                          | Some expression -> check_expression ~resolution errors expression
                          | None -> errors
                        in
                        check_optional_expression lower errors
                        |> check_optional_expression upper
                        |> check_optional_expression step
                  in
                  List.fold ~f:check_subscript ~init:errors subscripts

            in
            List.fold ~f:check_single_access ~init:errors access

        | Await expression ->
            let errors = check_expression ~resolution errors expression in
            let actual = (Annotated.resolve ~resolution expression) in
            let is_awaitable =
              TypeOrder.less_or_equal
                (Resolution.order resolution)
                ~left:actual
                ~right:(Type.awaitable Type.Object)
            in
            if not is_awaitable then
              [{
                Error.location = expression.Node.location;
                kind = Error.IncompatibleAwaitableType actual;
                define = define_node;
              }]
              |> add_errors errors
            else
              errors

        | BinaryOperator { BinaryOperator.left; right; _ } ->
            let errors = check_expression ~resolution errors left in
            check_expression ~resolution errors right

        | BooleanOperator { BooleanOperator.left; operator; right } ->
            let { errors; _ } =
              let forward state expression = forward state (Node.create (Expression expression)) in
              match operator with
              | BooleanOperator.And ->
                  let annotations = forward_annotations state (Statement.assume left) in
                  let right = forward { state with annotations } right in
                  let left = forward state left in
                  meet left right
              | BooleanOperator.Or ->
                  let negated_left = Expression.normalize (Expression.negate left) in
                  let annotations = forward_annotations state (Statement.assume negated_left) in
                  let right = forward { state with annotations } right in
                  let left = forward state left in
                  join left right
            in
            errors

        | ComparisonOperator { ComparisonOperator.left; right; _ } ->
            let errors = check_expression ~resolution errors left in
            let accumulate errors (_, expression) =
              check_expression ~resolution errors expression
            in
            List.fold ~f:accumulate ~init:errors right

        | Dictionary { Dictionary.entries; keywords } ->
            let errors =
              List.fold
                ~f:(check_entry ~resolution)
                ~init:errors
                entries
            in
            (match keywords with
             | None -> errors
             | Some keyword -> check_expression ~resolution errors keyword)

        | DictionaryComprehension { Comprehension.element; generators } ->
            let errors = check_entry ~resolution errors element in
            List.fold
              ~f:(check_generator ~resolution)
              ~init:errors
              generators

        | Lambda { Lambda.body; _ } ->
            check_expression ~resolution errors body

        | List list
        | Set list
        | Tuple list ->
            List.fold
              ~f:(check_expression ~resolution)
              ~init:errors
              list

        | Generator { Comprehension.element; generators }
        | ListComprehension { Comprehension.element; generators }
        | SetComprehension { Comprehension.element; generators } ->
            let errors = check_expression ~resolution errors element in
            List.fold
              ~f:(check_generator ~resolution)
              ~init:errors
              generators

        | Starred starred ->
            (match starred with
             | Starred.Once expression
             | Starred.Twice expression ->
                 check_expression ~resolution errors expression)

        | Ternary { Ternary.target; test; alternative } ->
            let errors = check_expression ~resolution errors test in
            let errors = check_expression ~resolution errors alternative in
            let { errors; _ } =
              let annotations = forward_annotations state (Statement.assume test) in
              forward { state with annotations; errors } (Node.create (Expression target))
            in
            errors

        | UnaryOperator { UnaryOperator.operand; operator = _ } ->
            check_expression ~resolution errors operand

        | Expression.Yield yield ->
            (match yield with
             | None -> errors
             | Some expression -> check_expression ~resolution errors expression)

        (* Trivial base cases *)
        | String _ | Complex _ | Bytes _ | Float _ | Format _ | Integer _ | False | True ->
            errors
      in

      match statement with
      | { Node.location; value = Return return } ->
          let actual =
            Option.value_map
              return
              ~default:Type.void
              ~f:(Annotated.resolve ~resolution)
          in
          if not (Resolution.less_or_equal resolution ~left:actual ~right:expected) &&
             not (Instantiated.Define.is_abstract_method define) &&
             not (Instantiated.Define.is_overloaded_method define)
          then
            let error =
              {
                Error.location;
                kind = Error.IncompatibleReturnType {
                    Error.expected;
                    actual
                  };
                define = define_node;
              }
            in
            add_errors errors [error]
          else if Type.equal expected Type.Top || Type.equal expected Type.Object then
            let error =
              {
                Error.location;
                kind = Error.MissingReturnAnnotation {
                    Error.annotation = actual;
                    evidence_locations = [location.Location.start.Location.line];
                    due_to_any = Type.equal expected Type.Object;
                  };
                define = define_node;
              }
            in
            add_errors errors [error]
          else
            errors

      | {
        Node.value = Assign { Assign.target; annotation = None; value = Some value; _ };
        _;
      } ->
          let check_assign ~target:{ Node.location; _ } ~access ~value_annotation errors =
            let add_incompatible_type_error ~expected ~parent ~name ~declare_location errors =
              if Resolution.less_or_equal resolution ~left:value_annotation ~right:expected then
                errors
              else
                let error =
                  {
                    Error.location;
                    kind = Error.IncompatibleType {
                        Error.name;
                        parent;
                        mismatch = { Error.expected; actual = value_annotation };
                        declare_location;
                      };
                    define = define_node;
                  }
                in
                Map.add ~key:error.Error.location ~data:error errors
            in
            let add_missing_annotation_error ~expected ~parent ~name ~declare_location errors =
              if ((Type.is_unknown expected) || (Type.equal expected Type.Object)) &&
                 not (Type.is_unknown value_annotation) then
                let error =
                  {
                    Error.location = declare_location;
                    kind = Error.MissingAnnotation {
                        Error.name;
                        annotation = value_annotation;
                        parent;
                        evidence_locations = [location];
                        due_to_any = Type.equal expected Type.Object;
                      };
                    define = define_node;
                  }
                in
                Map.add ~key:error.Error.location ~data:error errors
              else
                errors
            in
            let errors =
              let open Annotated in
              match Access.last_element ~resolution (Access.create access) with
              | Access.Element.Field (Access.Element.Defined field) ->
                  let expected = Annotation.original (Field.annotation field) in
                  let name = Instantiated.Access.access (Node.create (Field.name field)) in
                  errors
                  |> add_incompatible_type_error
                    ~expected
                    ~parent:(Some (Field.parent field))
                    ~name
                    ~declare_location:(Field.location field)
                  |> add_missing_annotation_error
                    ~expected
                    ~parent:(Some (Field.parent field))
                    ~name
                    ~declare_location:(Field.location field)
              | Access.Element.Field (Access.Element.Undefined { Access.Element.name; parent }) ->
                  parent
                  >>= (fun parent ->
                      (match Class.body parent with
                       | { Node.location; _ } :: _ ->
                           Some (
                             add_missing_annotation_error
                               ~expected:Type.Top
                               ~parent:(Some parent)
                               ~name
                               ~declare_location:location
                               errors)
                       | _ -> None))
                  |> Option.value ~default:errors
              | _ ->
                  let name = access in
                  match Map.find annotations access with
                  | Some {
                      Annotation.mutability = Annotation.Immutable {
                          Annotation.scope = Annotation.Global;
                          original = expected;
                        };
                      _;
                    } ->
                      errors
                      |> add_incompatible_type_error
                        ~expected
                        ~parent:None
                        ~name
                        ~declare_location:location
                      |> add_missing_annotation_error
                        ~expected
                        ~parent:None
                        ~declare_location:location
                        ~name
                  | Some {
                      Annotation.mutability = Annotation.Immutable {
                          Annotation.scope = Annotation.Local;
                          original = expected;
                        };
                      _;
                    } ->
                      add_incompatible_type_error
                        ~expected
                        ~parent:None
                        ~name
                        ~declare_location:location
                        errors
                  | _ ->
                      errors
            in
            let errors = check_expression ~resolution errors value in
            check_expression ~resolution errors target
          in
          fold_assign ~target ~value ~f:check_assign ~accumulator:errors

      | { Node.value = Assert { Assert.test; _ }; _ } ->
          check_expression ~resolution errors test

      | { Node.value = Class _; _ }
      | { Node.value = Define _; _ } ->
          (* Don't check accesses in nested classes and functions, they're analyzed separately. *)
          errors

      | { Node.value = Expression expression; _ } ->
          check_expression ~resolution errors expression
      | { Node.location; value = Statement.Yield { Node.value = Expression.Yield return; _ }; _ } ->
          let actual =
            Option.value_map return ~default:Type.void ~f:(Annotated.resolve ~resolution)
            |> Type.generator
          in
          if not (Resolution.less_or_equal resolution ~left:actual ~right:expected) then
            add_errors
              errors
              [
                {
                  Error.location;
                  kind = Error.IncompatibleReturnType { Error.expected; actual };
                  define = define_node;
                };
              ]
          else if Type.equal expected Type.Top || Type.equal expected Type.Object then
            add_errors errors [{
                Error.location;
                kind = Error.MissingReturnAnnotation {
                    Error.annotation = actual;
                    evidence_locations = [location.Location.start.Location.line];
                    due_to_any = Type.equal expected Type.Object;
                  };
                define = define_node;
              }]
          else
            errors

      |
        {
          Node.location;
          value = Statement.YieldFrom { Node.value = Expression.Yield (Some return); _ };
          _;
        } ->
          let actual =
            match Annotated.resolve ~resolution return with
            | Type.Parametric { Type.name; parameters = [parameter] }
              when Identifier.show name = "typing.Iterator" ->
                Type.generator parameter
            | annotation -> Type.generator annotation
          in
          if not (Resolution.less_or_equal resolution ~left:actual ~right:expected) then
            add_errors
              errors
              [
                {
                  Error.location;
                  kind = Error.IncompatibleReturnType { Error.expected; actual };
                  define = define_node;
                };
              ]
          else if Type.equal expected Type.Top || Type.equal expected Type.Object then
            add_errors errors [{
                Error.location;
                kind = Error.MissingReturnAnnotation {
                    Error.annotation = actual;
                    evidence_locations = [location.Location.start.Location.line];
                    due_to_any = Type.equal expected Type.Object;
                  };
                define = define_node;
              }]
          else
            errors

      | statement ->
          Visit.collect_accesses statement
          |> List.fold ~init:errors ~f:(check_access ~resolution)
    in
    { state with environment; errors; annotations; lookup }


  let backward
      statement
      ({ errors; annotations; lookup; _ } as state) =
    let resolution = resolution state in
    let resolve_assign annotation target_annotation =
      match annotation, target_annotation with
      | Type.Top, Type.Top -> None
      | Type.Top, target_annotation -> Some target_annotation
      | _ -> Some annotation
    in

    let annotate_call_accesses statement annotations =
      let propagate annotations access =
        let infer_annotations annotations (call: Annotated.Call.t) signature =
          let rec infer_annotation
              annotations
              parameter_annotation
              argument =
            match parameter_annotation, argument with
            | Some parameter_annotation,
              ({ Node.value = Access value; _ } as argument_value) ->
                resolve_assign
                  (Resolution.parse_annotation resolution parameter_annotation)
                  (Annotated.resolve ~resolution argument_value)
                >>| (fun refined ->
                    Map.add ~key:value ~data:(Annotation.create refined) annotations)
                |> Option.value ~default:annotations
            | Some parameter_annotation,
              { Node.value = Tuple arguments; _ } ->
                let rec remove_initial_identifiers annotation =
                  match annotation with
                  | Access ((Access.Identifier _) :: tail) ->
                      remove_initial_identifiers (Access tail)
                  | Access no_identifiers -> no_identifiers
                  | _ -> []
                in
                let parameters =
                  match (Resolution.parse_annotation resolution parameter_annotation) with
                  | Type.Tuple (Type.Bounded _) ->
                      begin
                        match remove_initial_identifiers (Node.value parameter_annotation) with
                        | [ Access.Subscript subscript ] ->
                            let extract_index = function
                              | Access.Index index -> Some index
                              | _ -> None
                            in
                            List.map ~f:extract_index subscript
                        | _ ->[]
                      end
                  | _ ->
                      []
                in
                if List.length arguments = List.length parameters then
                  List.fold2_exn
                    ~init:annotations
                    ~f:infer_annotation
                    parameters
                    arguments
                else
                  annotations
            | _, _ -> annotations
          in
          match signature with
          | Some {
              Signature.instantiated = {
                Define.parameters;
                _;
              };
              _;
            } ->
              let rec infer_annotations_list parameters arguments annotations =
                match parameters, arguments with
                | { Node.value = { Parameter.annotation; _ }; _ } :: parameters,
                  { Argument.value = argument; _ } :: arguments ->
                    let new_annotations =
                      infer_annotation annotations annotation argument
                    in
                    infer_annotations_list parameters arguments new_annotations
                | _ -> annotations
              in
              infer_annotations_list
                parameters
                (Annotated.Call.arguments call)
                annotations
          | _ -> annotations
        in
        let propagate_access type_accumulator ~annotations:_ ~resolved:_ ~element =
          let open Annotated.Access.Element in
          match element with
          | Call { call; callee; _ } ->
              infer_annotations type_accumulator call callee
          | Method { call; callee; _ } ->
              infer_annotations
                type_accumulator
                (Annotated.Call.prepend_self_argument call)
                callee
          | _ -> type_accumulator
        in
        Annotated.Access.fold
          ~resolution
          ~initial:annotations
          ~f:propagate_access
          (Annotated.Access.create access)
      in
      Visit.collect_accesses statement
      |> List.fold ~init:annotations ~f:propagate
    in

    let annotations =
      match Node.value statement with
      | Assign {
          Assign.target;
          value = Some ({ Node.value = Access value_access; _ } as value);
          Assign.compound = Some _;
          _ } ->
          resolve_assign
            (Annotated.resolve ~resolution target)
            (Annotated.resolve ~resolution value)
          >>| (fun refined ->
              Map.add ~key:value_access ~data:(Annotation.create refined) annotations)
          |> Option.value ~default:annotations
      | Assign { Assign.target; value = Some value; Assign.compound = None; _ } -> (
          (* Get the annotations of the targets and set the 'value' to be the meet *)
          let rec propagate_assign annotations target_annotation value =
            match Node.value value with
            | Access value_access ->
                let annotations =
                  match value_access with
                  | [Access.Identifier _] ->
                      resolve_assign target_annotation (Annotated.resolve ~resolution value)
                      >>| (fun refined ->
                          Map.add ~key:value_access ~data:(Annotation.create refined) annotations)
                      |> Option.value ~default:annotations
                  | _ ->
                      annotations
                in
                (* Optimistic assumption: after seeing x = y, we optimistically retain type of x *)
                annotations
                |> annotate_call_accesses statement
            (* Recursively break down tuples such as x : Tuple[int, string] = y, z *)
            | Tuple values  ->
                let parameters =
                  match target_annotation with
                  | Type.Tuple (Type.Bounded parameters) ->
                      parameters
                  | Type.Tuple (Type.Unbounded parameter) ->
                      List.map ~f:(fun _ -> parameter) values
                  | _ ->
                      []
                in
                if List.length values = List.length parameters then
                  List.fold2_exn
                    ~init:annotations
                    ~f:propagate_assign
                    parameters
                    values
                else annotations
            | _ ->
                annotations
          in
          match (Node.value target), (Node.value value) with
          | Tuple targets, Tuple values
            when List.length targets = List.length values ->
              let target_annotations =
                List.map
                  ~f:(Annotated.resolve ~resolution)
                  targets
              in
              List.fold2_exn
                ~init:annotations
                ~f:propagate_assign
                target_annotations
                values
          | _, _ ->
              let target_annotation = Annotated.resolve ~resolution target in
              propagate_assign annotations target_annotation value)
      | _ -> annotate_call_accesses statement annotations
    in

    { state with errors; annotations; lookup }


  let check_entry
      resolution
      ({
        annotations;
        define = ({ Node.value = { Define.parameters; _ } as define; _ } as define_node);
        errors;
        _;
      } as state) =
    let add_parameter_errors
        errors
        { Node.value = { Parameter.name; annotation; _ }; location } =
      let access = [Access.Identifier name] in
      let add_missing_parameter_error ~due_to_any =
        Map.find annotations access
        >>| (fun { Annotation.annotation; _ } ->
            let error = {
              Error.location;
              kind = Error.MissingParameterAnnotation { Error.name; annotation; due_to_any };
              define = define_node;
            }
            in
            Map.find errors location
            |> Option.value ~default:error
            |> fun data -> Map.add ~key:error.Error.location ~data errors)
        |> Option.value ~default:errors
      in
      match annotation with
      | None ->
          add_missing_parameter_error ~due_to_any:false
      | Some annotation
        when Type.equal (Resolution.parse_annotation resolution annotation) Type.Object ->
          add_missing_parameter_error ~due_to_any:true
      | _ -> errors
    in
    let parameters =
      if Instantiated.Define.is_method define &&
         not (Instantiated.Define.is_static_method define) then
        List.tl parameters
        |> Option.value ~default:[]
      else
        parameters
    in
    { state with errors = List.fold parameters ~init:errors ~f:add_parameter_errors}
end


module Fixpoint = Fixpoint.Make(State)


type result = {
  errors: Error.t list;
  lookup: Lookup.t option;
}


type check_result = {
  error_list: Error.t list;
  type_coverage: State.coverage;
}


let check configuration environment ({ Source.path; _ } as source) =
  Log.debug "Checking %s..." path;
  let resolution = Environment.resolution environment () in

  let dequalify_map = Preprocessing.dequalify_map source in

  let lookup = Lookup.create () in

  let check ({ Node.location; value = { Define.name; parent; _ } as define } as define_node) =
    let dump = Instantiated.Define.dump define in

    if dump then
      begin
        Log.dump
          "Checking `%s`..."
          (Log.Color.yellow (Instantiated.Access.show name));
        Log.dump "AST:\n%s" (Annotated.Define.create define |> Annotated.Define.show);
      end;

    let print_state name state =
      if dump then
        Log.dump "%s state:\n%a" name State.pp state;
      state
    in

    let dump_cfg cfg fixpoint =
      let precondition table id =
        match Hashtbl.find table id with
        | Some { State.annotations; _ } ->
            let stringify ~key ~data label =
              let annotation_string =
                Type.show (Annotation.annotation data)
                |> String.strip ~drop:((=) '`')
              in
              label ^ "\n" ^ Instantiated.Access.show key ^ ": " ^ annotation_string
            in
            Map.fold ~f:stringify ~init:"" annotations
        | None -> ""
      in
      if Instantiated.Define.dump_cfg define then
        begin
          let name =
            match parent with
            | Some parent ->
                Format.sprintf
                  "%s.%s"
                  (Instantiated.Access.show parent)
                  (Instantiated.Access.show name)
            | None ->
                Instantiated.Access.show name
          in
          let file =
            let path =
              Path.create_relative
                ~root:(Configuration.pyre_root configuration)
                ~relative:("cfgs" ^/ name ^ ".dot")
            in
            File.create
              ~content:(Some (Cfg.to_dot ~precondition:(precondition fixpoint) cfg))
              path
          in
          Log.debug "Outputting CFG:`%a`" File.pp file;
          File.write file
        end;
      fixpoint
    in

    try
      let cfg = Cfg.create define in
      let initial_forward =
        State.initial_forward
          ~lookup
          environment
          { Node.location; value = define }
      in
      let exit =
        if not configuration.infer then
          Fixpoint.forward cfg ~initial:initial_forward
          |> dump_cfg cfg
          |> Fixpoint.exit
          >>| print_state "Exit"
        else
          Fixpoint.backward
            cfg
            ~initial_forward
            ~initialize_backward:(State.initial_backward ~environment define_node)
          |> Fixpoint.entry
          >>| print_state "Entry"
          >>| State.check_entry resolution
      in
      let error_list =
        exit
        >>| State.errors (Source.ignore_lines source) configuration
        |> Option.value ~default:[]
      in
      let type_coverage =
        exit
        >>| State.coverage
        |> Option.value ~default:{ State.full = 0; partial = 0; untyped = 0 }
      in
      { error_list; type_coverage }
    with
    | TypeOrder.Undefined annotation ->
        Log.event
          ~name:"undefined type"
          ~root:(Path.last configuration.project_root)
          ~integers:[]
          ~normals:[
            "path", path;
            "define", Instantiated.Access.show name;
            "type", Type.show annotation;
          ]
          ();
        {
          error_list =
            if configuration.strict || configuration.debug then
              [{
                Error.location;
                kind = Error.UndefinedType annotation;
                define = define_node;
              }]
            else
              [];
          type_coverage = { State.full = 0; partial = 0; untyped = 0; };
        }
  in

  let source_coverage check_output =
    let { State.full; partial; untyped } =
      List.fold
        ~init:{ State.full = 0; partial = 0; untyped = 0; }
        ~f:(fun
             { State.full; partial; untyped; }
             {
               type_coverage = {
                 State.full = previous_full;
                 partial = previous_partial;
                 untyped = previous_untyped;
               };
               _;
             } ->
             {
               State.full = full + previous_full;
               partial = partial + previous_partial;
               untyped = untyped + previous_untyped;
             }
           )
        check_output
    in
    let error_list =
      List.fold
        ~init:[]
        ~f:(fun errors current ->
            let { error_list; _ } = current in
            List.append errors error_list
          )
        check_output
    in
    Log.coverage
      ~coverage:[
        "full_type_coverage", full;
        "partial_type_coverage", partial;
        "no_type_coverage", untyped;
        "ignore_coverage", List.length (Source.ignore_lines source);
        "total_errors", List.length error_list;
      ]
      ~root:(Path.last configuration.project_root)
      ~normals:[]
      ();
    error_list
  in

  let rec recursive_infer_source added_global_errors iterations =
    let add_errors_to_environment errors =
      let add_error (changed, globals_added_sofar) error =
        let module Reader = (val environment : Environment.Reader) in
        match error with
        | {
          Error.kind = Error.MissingReturnAnnotation { Error.annotation; _ };
          define = ({ Node.value = define; location } as define_node);
          _;
        } ->
            let is_redundant
                ({ Node.value = { Statement.Define.return_annotation; _ }; _ } as define_node) =
              define_node.Node.location = location &&
              return_annotation = Some (Type.expression annotation)
            in
            begin
              match Reader.function_definitions define.Statement.Define.name with
              | Some define_node_list when List.exists ~f:is_redundant define_node_list ->
                  changed, globals_added_sofar
              | _ ->
                  let define =
                    {
                      define with
                      Statement.Define.return_annotation = Some (Type.expression annotation)
                    }
                  in
                  Reader.register_definition
                    ~path
                    { define_node with Node.value = define };
                  true, globals_added_sofar
            end
        | {
          Error.kind = Error.MissingParameterAnnotation { Error.name; annotation; _ };
          define = ({ Node.value = define; location } as define_node);
          _;
        } ->
            let is_redundant
                ({ Node.value = { Statement.Define.parameters; _ }; _ } as define_node) =
              let find_parameter { Node.value = parameter; _ } =
                parameter.Parameter.name = name &&
                parameter.Parameter.annotation = Some (Type.expression annotation)
              in
              define_node.Node.location = location &&
              List.exists ~f:find_parameter parameters
            in
            begin
              match Reader.function_definitions define.Statement.Define.name with
              | Some define_node_list when List.exists ~f:is_redundant define_node_list ->
                  changed, globals_added_sofar
              | _ ->
                  let update_parameter parameters name annotation =
                    let update updated ({ Node.value = parameter; _ } as parameter_node) =
                      if Parameter.name parameter_node = name then
                        let updated_parameter =
                          {
                            parameter_node with
                            Node.value = ({
                                parameter with
                                Parameter.annotation = Some annotation
                              })
                          }
                        in
                        updated_parameter :: updated
                      else
                        parameter_node :: updated
                    in
                    List.fold ~init:[] ~f:update parameters
                  in
                  let define =
                    let annotation = Type.expression annotation in
                    {
                      define with
                      Statement.Define.parameters =
                        update_parameter define.Statement.Define.parameters name annotation
                    }
                  in
                  Reader.register_definition
                    ~path
                    { define_node with Node.value = define };
                  true, globals_added_sofar
            end
        | { Error.kind = Error.MissingAnnotation { Error.name; annotation; parent; _ }; _ } ->
            begin
              match Reader.globals name with
              | Some annotation when not (Type.is_unknown (Annotation.annotation annotation)) ->
                  changed, globals_added_sofar
              | _ ->
                  let key =
                    match parent with
                    | Some parent -> (Annotated.Class.name parent) @ name
                    | _ -> name
                  in
                  let data =
                    Annotation.create_immutable ~global:true ~original:(Some Type.Top) annotation
                  in
                  Reader.register_global ~path ~key ~data;
                  true, error :: globals_added_sofar
            end
        | _ ->
            changed, globals_added_sofar
      in
      List.fold ~init:(false, []) ~f:add_error errors
    in
    let errors =
      Preprocessing.defines source
      |> List.map ~f:check
      |> List.map ~f:(fun { error_list; _ } -> error_list)
      |> List.concat
      |> Error.join_at_source ~resolution
    in
    let (changed, newly_added_global_errors) = add_errors_to_environment errors in
    if changed && iterations <= State.widening_threshold then
      recursive_infer_source (newly_added_global_errors @ added_global_errors) (iterations + 1)
    else
      errors @ added_global_errors
      |> List.map ~f:(Error.dequalify dequalify_map environment)
      |> List.sort ~cmp:Error.compare
      |> fun errors -> { errors; lookup = Some lookup }
  in

  if configuration.infer && configuration.recursive_infer then
    recursive_infer_source [] 0
  else
    Preprocessing.defines source
    |> List.map ~f:check
    |> source_coverage
    |> Error.join_at_source ~resolution
    |> List.map ~f:(Error.dequalify dequalify_map environment)
    |> List.sort ~cmp:Error.compare
    |> fun errors -> { errors; lookup = Some lookup }
