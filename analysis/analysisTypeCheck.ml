(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Configuration
open Expression
open Pyre
open Statement

module Annotation = AnalysisAnnotation
module Cfg = AnalysisCfg
module Environment = AnalysisEnvironment
module Error = AnalysisError
module Lookup = AnalysisLookup
module Preprocessing = AnalysisPreprocessing
module Refinement = AnalysisRefinement
module Resolution = AnalysisResolution
module Signature = AnalysisSignature
module Type = AnalysisType
module TypeOrder = AnalysisTypeOrder
module Coverage = AnalysisCoverage


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
    configuration: Configuration.t;
    environment: (module Environment.Handler);
    errors: Error.t Location.Map.t;
    annotations: Annotation.t Access.Map.t;
    define: Define.t Node.t;
    lookup: Lookup.t option;
  }


  let resolution { environment; annotations; define = { Node.value = define; _ }; _ } =
    let resolution =
      Environment.resolution
        environment
        ~annotations
        ()
    in
    Resolution.with_define resolution define


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
          Access.pp name
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


  let create
      ?(configuration = Configuration.create ())
      ~environment
      ~annotations
      ~define
      ?lookup
      () =
    {
      configuration;
      environment;
      errors = Location.Map.empty;
      annotations = Access.Map.of_alist_exn annotations;
      define;
      lookup;
    }


  let errors
      ({
        configuration;
        errors;
        annotations;
        define = ({ Node.location; value = define; _ } as define_node);
        _;
      } as state) =
    let resolution = resolution state in

    let class_initialization_errors errors =
      let open Annotated in
      (* Ensure non-nullable typed attributes are instantiated in init. *)
      if not (Statement.Define.is_constructor define) then
        errors
      else
        let check_class_attributes class_definition =
          let propagate_initialization_errors errors attribute =
            let expected = Annotation.annotation (Attribute.annotation attribute) in
            match Attribute.name attribute with
            | Access name
              when not (Type.equal expected Type.Top ||
                        Type.is_optional expected ||
                        Option.is_some (Attribute.value attribute)) ->
                let access =
                  (Expression.Access.Identifier (Identifier.create "self")) :: name
                in
                if Map.mem annotations access then
                  errors
                else
                  let error =
                    {
                      Error.location = Attribute.location attribute;
                      kind = Error.UninitializedAttribute {
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
          Class.attribute_fold
            ~include_generated_attributes:false
            ~initial:errors
            ~resolution
            ~f:propagate_initialization_errors
            class_definition
        in
        Define.parent_definition ~resolution (Define.create define)
        >>| check_class_attributes
        |> Option.value ~default:errors
    in

    Map.data errors
    |> Error.join_at_define ~resolution ~location
    |> class_initialization_errors
    |> Error.filter ~configuration ~resolution


  let coverage { annotations; _ } =
    Map.data annotations
    |> Coverage.aggregate


  let initial_forward
      ?(configuration = Configuration.create ())
      ?lookup
      environment
      ({
        Node.location;
        value = ({ Define.parent; parameters; _ } as define);
      } as define_node) =
    let { annotations; errors; _ } as initial =
      create ~configuration ~environment ~annotations:[]  ~define:define_node ?lookup ()
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
          when Define.is_method define &&
               not (Define.is_static_method define) ->
            let annotation =
              let annotation =
                Resolution.parse_annotation
                  resolution
                  (Node.create_with_default_location (Access parent))
              in
              if Define.is_class_method define then
                (* First parameter of a method is a class object. *)
                Type.meta annotation
              else
                (* First parameter of a method is the callee object. *)
                annotation
            in
            Map.set ~key:access ~data:(Annotation.create annotation) annotations, errors
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
              Map.set ~key:access ~data:(Annotation.create annotation) annotations,
              Map.set ~key:location ~data:error errors
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
                  Map.set
                    ~key:access
                    ~data:(Annotation.create_immutable ~global:false annotation)
                    annotations,
                  errors
              | _ ->
                  Map.set ~key:access ~data:(Annotation.create Type.Bottom) annotations,
                  errors
            end
      in
      List.foldi ~init:(annotations, errors) ~f:parameter parameters
    in

    (* Check behavioral subtyping. *)
    let errors =
      if Define.is_constructor define ||
         Define.is_class_method define ||
         Define.is_static_method define then
        errors
      else
        let open Annotated in
        (Define.create define
         |> Define.method_definition ~resolution
         >>= fun definition -> Method.overloads definition ~resolution
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
             Map.set ~key:location ~data:error errors
           else
             errors
         in

         (* Check weakening of precondition. *)
         let parameters = Method.parameter_annotations_positional definition ~resolution in
         let parameter ~key ~data errors =
           let expected = data in
           match Map.find parameters key with
           | Some actual ->
               begin
                 try
                   if not (Type.equal Type.Top expected) &&
                      not (Resolution.less_or_equal resolution ~left:expected ~right:actual) then
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
                     Map.set ~key:location ~data:error errors
                   else
                     errors
                 with TypeOrder.Untracked _ ->
                   (* TODO(T27409168): Error here. *)
                   errors
               end
           | None ->
               let error =
                 {
                   Error.location;
                   kind = Error.InconsistentOverride {
                       Error.overridden_method;
                       override = Error.StrengthenedPrecondition;
                       mismatch = { Error.actual = Type.none; expected };
                     };
                   define = define_node;
                 }
               in
               Map.set ~key:location ~data:error errors
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
      ~f:(entry_less_or_equal right.annotations (Refinement.less_or_equal ~resolution))
      left.annotations


  let equal left right =
    (* Ignore errors in unit tests. *)
    Map.equal Annotation.equal left.annotations right.annotations


  let rec join left right =
    let resolution = resolution left in

    let merge_errors ~key:_ = function
      | `Both (left, right) ->
          Some (Error.join ~resolution left right)
      | `Left state
      | `Right state ->
          Some state
    in
    let merge_annotations ~key:_ = function
      | `Both (left, right) ->
          Some (Refinement.join ~resolution left right)
      | `Left _
      | `Right _ ->
          Some (Annotation.create Type.Top)
    in
    {
      left with
      errors = Map.merge ~f:merge_errors left.errors right.errors;
      annotations = Map.merge ~f:merge_annotations left.annotations right.annotations;
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
          ~f:(merge (Refinement.meet ~resolution))
          left.annotations
          right.annotations;
    }


  and initial_backward
      ?(configuration = Configuration.create ())
      ~environment
      define
      ~forward:{ annotations; errors; _ } =
    let resolution = Environment.resolution environment () in
    let expected_return =
      Annotated.Define.create (Node.value define)
      |> Annotated.Define.return_annotation ~resolution
      |> Annotation.create
    in
    let backward_initial_state =
      create
        ~configuration
        ~environment
        ~annotations:[Preprocessing.return_access, expected_return]
        ~define
        ()
    in
    let combine_annotations left right =
      let add_annotation ~key ~data map =
        if Type.is_unknown data.Annotation.annotation ||
           Type.is_not_instantiated data.Annotation.annotation ||
           Access.equal key Preprocessing.return_access then
          map
        else
          Map.set ~key ~data map
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
      if Map.mem map key then
        Map.set ~key ~data map
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

    let widen_errors ~key:_ = function
      | `Both (previous, next) ->
          Some (Error.widen ~resolution ~previous ~next ~iteration)
      | `Left state
      | `Right state ->
          Some state
    in
    let widen_annotations ~key = function
      | `Both (previous, next) ->
          Some (Refinement.widen ~resolution ~widening_threshold ~previous ~next ~iteration)
      | `Left previous
      | `Right previous when List.length key = 1 ->
          let widened =
            Refinement.widen
              ~resolution
              ~widening_threshold
              ~previous
              ~next:(Annotation.create Type.unbound)
              ~iteration
          in
          Some widened
      | `Left previous
      | `Right previous ->
          Some previous
      | _ ->
          None
    in
    {
      previous with
      errors = Map.merge ~f:widen_errors previous.errors next.errors;
      annotations = Map.merge ~f:widen_annotations previous.annotations next.annotations;
    }


  and forward
      ({
        configuration;
        environment;
        errors;
        annotations;
        define = ({ Node.value = { Define.async; _ } as define; _ } as define_node);
        lookup;
      } as state)
      ({ Node.location; _ } as statement) =
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
          Map.set ~key:access ~data:annotation annotations
      | Assign assign ->
          let open Annotated in
          let forward_annotations
              ~access:{ Node.value = access; _ }
              ~value_annotation
              annotations =
            let annotation = Map.find annotations access in
            let element =
              Access.last_element ~resolution (Annotated.Access.create access)
            in
            match annotation, element with
            | Some annotation, _ when Annotation.is_immutable annotation ->
                Map.set
                  ~key:access
                  ~data:(Refinement.refine ~resolution annotation value_annotation)
                  annotations
            | _, Access.Element.Attribute attribute when Attribute.defined attribute ->
                let refined =
                  Refinement.refine
                    ~resolution
                    (Attribute.annotation attribute)
                    value_annotation
                in
                Map.set ~key:access ~data:refined annotations
            | _, _ ->
                Map.set ~key:access ~data:(Annotation.create value_annotation) annotations
          in
          Assign.create assign
          |> Assign.fold ~resolution ~f:forward_annotations ~initial:annotations

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
                let updated_annotation =
                  match Map.find annotations access with
                  | Some existing_annotation when
                      Refinement.less_or_equal
                        ~resolution
                        existing_annotation
                        (Annotation.create annotation) ->
                      existing_annotation
                  | _ ->
                      Annotation.create annotation
                in
                Map.set ~key:access ~data:updated_annotation annotations
            | UnaryOperator {
                UnaryOperator.operator = UnaryOperator.Not;
                operand = {
                  Node.value =
                    Access [
                      Access.Call {
                        Node.value = {
                          Call.name = {
                            Node.value = Access [Access.Identifier name];
                            _;
                          };
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
                        begin
                          match annotation with
                          | { Node.value = Tuple elements; _ } ->
                              List.map
                                ~f:(Resolution.parse_annotation resolution)
                                elements
                          | _ ->
                              [Resolution.parse_annotation resolution annotation]
                        end
                        |> Type.Set.of_list
                      in
                      let constrained =
                        Set.diff parameters constraints
                        |> Set.to_list
                        |> Type.union
                      in
                      Map.set ~key:access ~data:(Annotation.create constrained) annotations
                  | _ ->
                      annotations
                end

            | Access access ->
                let open Annotated in
                let element =
                  Access.last_element ~resolution (Annotated.Access.create access)
                in
                begin
                  match Map.find annotations access, element with
                  | Some { Annotation.annotation = Type.Optional parameter; _ }, _ ->
                      Map.set ~key:access ~data:(Annotation.create parameter) annotations
                  | _, Access.Element.Attribute attribute when Attribute.defined attribute ->
                      begin
                        match Attribute.annotation attribute with
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
                              Refinement.refine
                                ~resolution
                                (Attribute.annotation attribute)
                                parameter
                            in
                            Map.set ~key:access ~data:refined annotations
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
                        | `Both (left, right) -> Some (Refinement.meet ~resolution left right)
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
                  { Node.value = Access [Access.Identifier identifier; ]; _ }
                ];
              } when Identifier.show identifier = "None" ->
                asserted annotations left
            | ComparisonOperator {
                ComparisonOperator.left = { Node.value = Access access; _ };
                right = [
                  ComparisonOperator.Is,
                  { Node.value = Access [Access.Identifier identifier; ]; _ }
                ];
              } when Identifier.show identifier = "None" ->
                let open Annotated in
                let element =
                  Access.last_element ~resolution (Annotated.Access.create access)
                in
                begin
                  match element with
                  | Access.Element.Attribute attribute when Attribute.defined attribute ->
                      let refined =
                        Refinement.refine
                          ~resolution
                          (Attribute.annotation attribute)
                          (Type.Optional Type.Bottom)
                      in
                      Map.set ~key:access ~data:refined annotations
                  | _ ->
                      Map.set
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
          let access = Access.create_from_identifiers identifiers in
          let annotation =
            Resolution.resolve resolution (Node.create_with_default_location (Access access))
            |> Annotation.create_immutable ~global:true
          in
          Map.set ~key:access ~data:annotation annotations

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
          Map.set ~key:error.Error.location ~data:error sofar
        in
        List.fold ~init:errors ~f:add_error
      in

      let rec check_parameters ~resolution call = function
        | Some ({ Signature.instantiated = callee; _ } as signature) ->
            let incompatible_parameter_errors =
              let check_parameter ~argument:_ ~position ~offset ~location ~name ~actual ~expected =
                if not (Resolution.less_or_equal resolution ~left:actual ~right:expected) then
                  begin
                    let start_position =
                      (* Account for the `self` parameter in instance methods. *)
                      if Define.is_method callee &&
                         not (Define.is_static_method callee) then
                        0
                      else
                        1
                    in
                    Some {
                      Error.location;
                      kind = Error.IncompatibleParameterType {
                          Error.name = Some name;
                          position = position + offset + start_position;
                          callee = Some callee;
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
            in
            let argument_expression_errors =
              let argument_expression_errors { Argument.value; _ } =
                check_expression ~resolution Location.Map.empty value
                |> Map.data
              in
              List.concat_map ~f:argument_expression_errors (Annotated.Call.arguments call)
            in
            incompatible_parameter_errors @ argument_expression_errors
        | None ->
            []

      and check_access ~resolution errors { Node.location; value = access } =
        let check_access new_errors ~annotations:_ ~resolved:_ ~element =
          if not (List.is_empty new_errors) then
            new_errors
          else
            let open Annotated.Access.Element in
            match element with
            | Call { call; callee; _ } ->
                let undefined_function_error =
                  match callee with
                  | None ->
                      [
                        {
                          Error.location;
                          kind = Error.UndefinedFunction {
                              Error.annotation = None;
                              call;
                            };
                          define = define_node;
                        }
                      ]
                  | _ ->
                      []
                in
                let parameter_errors = check_parameters ~resolution call callee in
                undefined_function_error @ parameter_errors
            | Callable
                (Annotated.Signature.NotFound {
                    Annotated.Signature.callable = { Type.Callable.kind; _ };
                    reason = Some (Annotated.Signature.Mismatch mismatch);
                    _;
                  }) ->
                let open Annotated.Signature in
                let mismatch, name, position, location =
                  let { Annotated.Signature.actual; expected; name; position } =
                    Node.value mismatch
                  in
                  { Error.actual; expected }, name, position, (Node.location mismatch)
                in
                let callee =
                  match kind with
                  | Type.Callable.Named access ->
                      Resolution.function_definitions resolution access
                      |> List.hd
                      >>| Node.value
                  | _ ->
                      None
                in
                [
                  {
                    Error.location;
                    kind = Error.IncompatibleParameterType {
                        Error.name;
                        position;
                        callee;
                        mismatch;
                      };
                    define = define_node;
                  };
                ]
            | Method { location; access; annotation; call; callee; backup; } ->
                let annotation = Annotation.original annotation in
                let unresolved_method_errors =
                  match callee, Resolution.class_definition resolution annotation with
                  | None, Some _ ->
                      [
                        {
                          Error.location;
                          kind = Error.UndefinedFunction {
                              Error.annotation = Some annotation;
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
                                value =
                                  Node.create_with_default_location (Access (Access.create "self"));
                              };
                              {
                                Argument.name = None;
                                value = Node.create_with_default_location (Access access);
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
            | Attribute attribute when not (Annotated.Attribute.defined attribute) ->
                let open Annotated in
                if Location.equal location Location.any then
                  begin
                    Statistics.event
                      ~name:"undefined attribute without location"
                      ~configuration
                      ~normals:["attribute", (Expression.Access.show (Attribute.access attribute))]
                      ();
                    []
                  end
                else
                  [
                    {
                      Error.location;
                      kind = Error.UndefinedAttribute {
                          Error.annotation =
                            Class.annotation
                              ~resolution
                              (Attribute.parent attribute);
                          attribute = Attribute.access attribute;
                          class_attribute = Attribute.class_attribute attribute;
                        };
                      define = define_node;
                    }
                  ] @ new_errors
            | _ ->
                new_errors
        in
        Annotated.Access.fold
          ~resolution
          ~initial:[]
          ~f:check_access
          (Annotated.Access.create access)
        |> add_errors errors

      and forward_expression state expression =
        forward state (Node.create_with_default_location (Expression expression))

      and check_entry ~resolution errors { Dictionary.key; value } =
        let errors = check_expression ~resolution errors key in
        check_expression ~resolution errors value

      and check_generator
          state
          { Comprehension.target; iterator = { Node.location; _ } as iterator; conditions; _ } =
        (* TODO(T23723699): check async. *)
        (* Propagate `target = iterator.__iter__().__next__()`. *)
        let assign =
          let access =
            let iterator =
              match iterator with
              | { Node.value = Access access; _ } -> access
              | _ -> []
            in
            let call name =
              Access.Call
                (Node.create
                   ~location
                   {
                     Call.name =
                       Node.create
                         ~location
                         (Access [Access.Identifier (Identifier.create name)]);
                     arguments = [];
                   })
            in
            iterator @ [call "__iter__"; call "__next__"]
          in
          Assign {
            Assign.target;
            annotation = None;
            value = Some (Node.create ~location (Access access));
            compound = None;
            parent = None;
          }
          |> Node.create ~location
        in

        (* Check conditions. *)
        let state = forward state assign in
        List.map ~f:Statement.assume conditions
        |> List.fold ~init:state ~f:forward


      and check_expression ~resolution errors { Node.location; value } =
        match value with
        | Access access ->
            let errors = check_access ~resolution errors (Node.create ~location access) in
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
                    | Access.Index expression ->
                        check_expression ~resolution errors expression
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
            let actual = Annotated.resolve ~resolution expression in
            let is_awaitable =
              TypeOrder.less_or_equal
                (Resolution.order resolution)
                ~left:actual
                ~right:(Type.awaitable Type.Object)
            in
            if not is_awaitable then
              [{
                Error.location;
                kind = Error.IncompatibleAwaitableType actual;
                define = define_node;
              }]
              |> add_errors errors
            else
              errors

        | BinaryOperator { BinaryOperator.left; right; _ } ->
            let errors = check_expression ~resolution errors left in
            check_expression ~resolution errors right

        | BooleanOperator {
            BooleanOperator.left;
            operator;
            right = ({ Node.location; _ } as right);
          } ->
            let { errors; _ } =
              let right =
                let assume =
                  match operator with
                  | BooleanOperator.And ->
                      left;
                  | BooleanOperator.Or ->
                      Expression.normalize (Expression.negate left);
                in
                [
                  Statement.assume assume;
                  Node.create ~location (Statement.Expression right);
                ]
              in
              (* We only analyze right, as it contains the assumption of `left` as a
                 statement that will be checked. *)
              List.fold ~init:state ~f:forward right
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
            begin
              match keywords with
              | None -> errors
              | Some keyword -> check_expression ~resolution errors keyword
            end

        | DictionaryComprehension { Comprehension.element; generators } ->
            let ({ errors; _ } as state) = List.fold ~f:check_generator ~init:state generators in
            check_entry ~resolution:(update_resolution state) errors element


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
            let ({ errors; _ } as state) = List.fold ~f:check_generator ~init:state generators in
            check_expression ~resolution:(update_resolution state) errors element

        | Starred starred ->
            begin
              match starred with
              | Starred.Once expression
              | Starred.Twice expression ->
                  check_expression ~resolution errors expression
            end

        | Ternary { Ternary.target; test; alternative } ->
            let errors = check_expression ~resolution errors test in
            let errors = check_expression ~resolution errors alternative in
            let { errors; _ } =
              let annotations = forward_annotations state (Statement.assume test) in
              forward_expression { state with annotations; errors } target
            in
            errors

        | UnaryOperator { UnaryOperator.operand; operator = _ } ->
            check_expression ~resolution errors operand

        | Expression.Yield yield ->
            begin
              match yield with
              | None -> errors
              | Some expression -> check_expression ~resolution errors expression
            end

        (* Trivial base cases *)
        | String _ | Complex _ | Bytes _ | Float _ | Format _ | Integer _ | False | True ->
            errors
      in

      match Node.value statement with
      | Assign ({ Assign.target; annotation = None; value = Some value; _ } as assign) ->
          let check_assign ~access:{ Node.location; value = access } ~value_annotation errors =
            let add_incompatible_type_error ~expected ~parent ~name ~declare_location errors =
              if Resolution.less_or_equal resolution ~left:value_annotation ~right:expected then
                errors
              else
                let error =
                  match parent with
                  | Some parent ->
                      {
                        Error.location;
                        kind = Error.IncompatibleAttributeType {
                            Error.parent;
                            incompatible_type = {
                              Error.name;
                              mismatch = { Error.expected; actual = value_annotation };
                              declare_location;
                            };
                          };
                        define = define_node;
                      }
                  | None ->
                      {
                        Error.location;
                        kind = Error.IncompatibleVariableType {
                            Error.name;
                            mismatch = { Error.expected; actual = value_annotation };
                            declare_location;
                          };
                        define = define_node;
                      }
                in
                Map.set ~key:error.Error.location ~data:error errors
            in
            let add_missing_annotation_error ~expected ~parent ~name ~declare_location errors =
              if ((Type.is_unknown expected) || (Type.equal expected Type.Object)) &&
                 not (Type.is_unknown value_annotation) then
                let error =
                  match parent with
                  | Some parent ->
                      {
                        Error.location = declare_location;
                        kind = Error.MissingAttributeAnnotation {
                            Error.parent;
                            missing_annotation = {
                              Error.name;
                              annotation = value_annotation;
                              evidence_locations = [location];
                              due_to_any = Type.equal expected Type.Object;
                            };
                          };
                        define = define_node;
                      }
                  | None ->
                      {
                        Error.location = declare_location;
                        kind = Error.MissingGlobalAnnotation {
                            Error.name;
                            annotation = value_annotation;
                            evidence_locations = [location];
                            due_to_any = Type.equal expected Type.Object;
                          };
                        define = define_node;
                      }
                in
                Map.find errors declare_location
                >>| Error.join ~resolution error
                |> Option.value ~default:error
                |> fun data -> Map.set ~key:declare_location ~data errors
              else
                errors
            in
            let errors =
              let open Annotated in
              match Access.last_element ~resolution (Access.create access) with
              | Access.Element.Attribute attribute when Attribute.defined attribute ->
                  let expected = Annotation.original (Attribute.annotation attribute) in
                  let name =
                    Expression.Access.access
                      (Node.create_with_default_location (Attribute.name attribute))
                  in
                  errors
                  |> add_incompatible_type_error
                    ~expected
                    ~parent:(Some (Attribute.parent attribute))
                    ~name
                    ~declare_location:(Attribute.location attribute)
                  |> add_missing_annotation_error
                    ~expected
                    ~parent:(Some (Attribute.parent attribute))
                    ~name
                    ~declare_location:(Attribute.location attribute)
              | Access.Element.Attribute attribute when not (Attribute.defined attribute) ->
                  let parent = Attribute.parent attribute in
                  begin
                    match Class.body parent with
                    | { Node.location; _ } :: _ ->
                        add_missing_annotation_error
                          ~expected:Type.Top
                          ~parent:(Some parent)
                          ~name:(Attribute.access attribute)
                          ~declare_location:location
                          errors
                    | _ ->
                        errors
                  end
              | _ ->
                  let name = access in
                  let module Handler = (val environment : Environment.Handler) in
                  let location =
                    Handler.globals access
                    >>| Node.location
                    |> Option.value ~default:location
                  in
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
          Annotated.Assign.create assign
          |> Annotated.Assign.fold ~resolution ~f:check_assign ~initial:errors
      | Assign _ ->
          (* TODO(T26146217): add coverage. *)
          errors

      | Assert { Assert.test; _ } ->
          check_expression ~resolution errors test

      | Delete _ ->
          (* TODO(T26146217): add coverage. *)
          errors

      | Expression expression ->
          check_expression ~resolution errors expression

      | Raise (Some expression) ->
          check_expression ~resolution errors expression
      | Raise _ ->
          errors

      | Return return ->
          let actual =
            Option.value_map
              return
              ~default:Type.none
              ~f:(Annotated.resolve ~resolution)
          in
          if not (Resolution.less_or_equal resolution ~left:actual ~right:expected) &&
             not (Define.is_abstract_method define) &&
             not (Define.is_overloaded_method define) &&
             not (Type.is_none actual && Type.is_generator expected)
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

      | Statement.Yield { Node.value = Expression.Yield return; _ } ->
          let errors =
            return
            >>| check_expression ~resolution errors
            |> Option.value ~default:errors
          in
          let actual =
            Option.value_map return ~default:Type.none ~f:(Annotated.resolve ~resolution)
            |> Type.generator ~async
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
      | Statement.Yield _ ->
          errors

      | YieldFrom { Node.value = Expression.Yield (Some return); _ } ->
          let errors = check_expression ~resolution errors return in
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
      | YieldFrom _ ->
          errors

      | Class _ | Define _ ->
          (* Don't check accesses in nested classes and functions, they're analyzed separately. *)
          errors

      | For _  | If _ | Try _ | With _ | While _ ->
          (* Check happens implicitly in the resulting control flow. *)
          errors

      | Break | Continue | Global _ | Import _ | Nonlocal _ | Pass | Stub _ ->
          errors
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
                    Map.set ~key:value ~data:(Annotation.create refined) annotations)
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
                        | [Access.Subscript subscript] ->
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
              infer_annotations type_accumulator call callee
          | _ ->
              type_accumulator
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
              Map.set ~key:value_access ~data:(Annotation.create refined) annotations)
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
                          Map.set ~key:value_access ~data:(Annotation.create refined) annotations)
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
            Map.set ~key:error.Error.location ~data:error errors)
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
      if Define.is_method define &&
         not (Define.is_static_method define) then
        List.tl parameters
        |> Option.value ~default:[]
      else
        parameters
    in
    { state with errors = List.fold parameters ~init:errors ~f:add_parameter_errors}
end


module Fixpoint = AnalysisFixpoint.Make(State)


module Result = struct
  type t = {
    errors: Error.t list;
    lookup: Lookup.t option;
    coverage: Coverage.t;
  }
end


module SingleSourceResult = struct
  type t = {
    errors: Error.t list;
    coverage: Coverage.t;
  }


  let errors { errors; _ } =
    errors


  let coverage { coverage; _ } =
    coverage
end


let check configuration environment ?mode_override ({ Source.path; _ } as source) =
  Log.debug "Checking %s..." path;
  let resolution = Environment.resolution environment () in

  let dequalify_map = Preprocessing.dequalify_map source in

  let lookup = Lookup.create () in

  let check ({ Node.location; value = { Define.name; parent; _ } as define } as define_node) =
    Log.log ~section:`Check "Checking %a" Access.pp name;
    let dump = Define.dump define in

    if dump then
      begin
        Log.dump
          "Checking `%s`..."
          (Log.Color.yellow (Access.show name));
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
              label ^ "\n" ^ Access.show key ^ ": " ^ annotation_string
            in
            Map.fold ~f:stringify ~init:"" annotations
        | None -> ""
      in
      if Define.dump_cfg define then
        begin
          let name =
            match parent with
            | Some parent ->
                Format.sprintf
                  "%s.%s"
                  (Access.show parent)
                  (Access.show name)
            | None ->
                Access.show name
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
          ~configuration
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
            ~initialize_backward:(State.initial_backward ~configuration ~environment define_node)
          |> Fixpoint.entry
          >>| print_state "Entry"
          >>| State.check_entry resolution
      in
      let (module Handler: Environment.Handler) = environment in
      let errors =
        let errors =
          exit
          >>| State.errors
          |> Option.value ~default:[]
        in
        if configuration.debug then
          errors
        else
          let keep_error ({ Error.location = { Location.path; _ }; _ } as error) =
            let mode =
              match mode_override with
              | Some mode ->
                  mode
              | None ->
                  Handler.mode path
                  |> Option.value ~default:Source.Default
            in
            not (Error.suppress ~mode error)
          in
          List.filter ~f:keep_error errors
      in
      let coverage =
        exit
        >>| State.coverage
        |> Option.value ~default:(Coverage.create ())
      in
      { SingleSourceResult.errors; coverage }
    with
    | TypeOrder.Untracked annotation ->
        Statistics.event
          ~name:"undefined type"
          ~configuration
          ~integers:[]
          ~normals:[
            "path", path;
            "define", Access.show name;
            "type", Type.show annotation;
          ]
          ();
        {
          SingleSourceResult.errors =
            if configuration.debug then
              [{
                Error.location;
                kind = Error.UndefinedType annotation;
                define = define_node;
              }]
            else
              [];
          coverage = Coverage.create ~crashes:1 ();
        }
  in

  let rec recursive_infer_source added_global_errors iterations =
    let add_errors_to_environment errors =
      let add_error (changed, globals_added_sofar) error =
        let module Handler = (val environment : Environment.Handler) in
        let add_missing_annotation_error ~access ~name ~location ~annotation =
          match Handler.globals name with
          | Some { Node.value; _ }
            when not (Type.is_unknown (Annotation.annotation value)) ->
              changed, globals_added_sofar
          | _ ->
              let global =
                Annotation.create_immutable ~global:true ~original:(Some Type.Top) annotation
                |> Node.create ~location
              in
              Handler.register_global ~path ~access ~global;
              true, error :: globals_added_sofar
        in
        match error with
        | {
          Error.kind = Error.MissingReturnAnnotation { Error.annotation; _ };
          define = ({ Node.value = define; location } as define_node);
          _;
        } ->
            let is_redundant
                ({ Node.value = { Define.return_annotation; _ }; _ } as define_node) =
              define_node.Node.location = location &&
              return_annotation = Some (Type.expression annotation)
            in
            begin
              match Handler.function_definitions define.Define.name with
              | Some define_node_list when List.exists ~f:is_redundant define_node_list ->
                  changed, globals_added_sofar
              | _ ->
                  let define =
                    {
                      define with
                      Define.return_annotation = Some (Type.expression annotation)
                    }
                  in
                  Handler.register_definition
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
                ({ Node.value = { Define.parameters; _ }; _ } as define_node) =
              let find_parameter { Node.value = parameter; _ } =
                parameter.Parameter.name = name &&
                parameter.Parameter.annotation = Some (Type.expression annotation)
              in
              define_node.Node.location = location &&
              List.exists ~f:find_parameter parameters
            in
            begin
              match Handler.function_definitions define.Define.name with
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
                      Define.parameters =
                        update_parameter define.Define.parameters name annotation
                    }
                  in
                  Handler.register_definition
                    ~path
                    { define_node with Node.value = define };
                  true, globals_added_sofar
            end
        | {
          Error.kind = Error.MissingAttributeAnnotation {
              Error.parent;
              missing_annotation = { Error.name; annotation; _ };
            };
          location;
          _;
        } ->
            add_missing_annotation_error
              ~access:((Annotated.Class.name parent) @ name)
              ~name
              ~location
              ~annotation
        | {
          Error.kind = Error.MissingGlobalAnnotation { Error.name; annotation; _ };
          location;
          _;
        } ->
            add_missing_annotation_error ~access:name ~name ~location ~annotation
        | _ ->
            changed, globals_added_sofar
      in
      List.fold ~init:(false, []) ~f:add_error errors
    in
    let errors =
      Preprocessing.defines source
      |> List.map ~f:check
      |> List.map ~f:SingleSourceResult.errors
      |> List.concat
      |> Error.join_at_source ~resolution
    in
    let (changed, newly_added_global_errors) = add_errors_to_environment errors in
    if changed && iterations <= State.widening_threshold then
      recursive_infer_source (newly_added_global_errors @ added_global_errors) (iterations + 1)
    else
      errors @ added_global_errors
      |> List.map ~f:(Error.dequalify dequalify_map environment)
      |> List.sort ~compare:Error.compare
      |> fun errors -> {
        Result.errors;
        lookup = Some lookup;
        coverage = Coverage.create ();
      }
  in

  if configuration.infer && configuration.recursive_infer then
    recursive_infer_source [] 0
  else
    let results = Preprocessing.defines source |> List.map ~f:check in

    let errors =
      List.map ~f:SingleSourceResult.errors results
      |> List.concat
      |> Error.join_at_source ~resolution
      |> List.map ~f:(Error.dequalify dequalify_map environment)
      |> List.sort ~compare:Error.compare
    in

    let coverage =
      List.map ~f:SingleSourceResult.coverage results
      |> Coverage.aggregate_over_source ~source
    in
    Coverage.log coverage ~configuration ~total_errors:(List.length errors) ~path;

    { Result.errors; lookup = Some lookup; coverage }
