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
module Type = AnalysisType
module TypeOrder = AnalysisTypeOrder
module Coverage = AnalysisCoverage


module State = struct
  type nested_define = Define.t

  and t = {
    configuration: Configuration.t;
    resolution: Resolution.t;
    errors: Error.t Location.Map.t;
    define: Define.t Node.t;
    lookup: Lookup.t option;
    nested_defines: nested_define Location.Map.t;
    bottom: bool;
    resolution_fixpoint: (Annotation.t Access.Map.t) Int.Map.t
  }


  let pp_nested_define format { Define.name; _ } =
    Format.fprintf format "%a" Access.pp name


  let show_nested_define nested =
    Format.asprintf "%a" pp_nested_define nested


  let pp
      format
      {
        resolution;
        errors;
        define = { Node.value = define; _ };
        nested_defines;
        bottom;
        _;
      } =
    let expected = Annotated.Callable.return_annotation ~define ~resolution in
    let nested_defines =
      let nested_define_to_string nested_define =
        Format.asprintf
          "    %a"
          pp_nested_define
          nested_define
      in
      Map.data nested_defines
      |> List.map ~f:nested_define_to_string
      |> String.concat ~sep:"\n"
    in
    let annotations =
      let annotation_to_string (name, annotation) =
        Format.asprintf
          "    %a -> %a"
          Access.pp name
          Annotation.pp annotation
      in
      Resolution.annotations resolution
      |> Map.to_alist
      |> List.map ~f:annotation_to_string
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
      "  Bottom: %b\n  Expected return: %a\n  Nested defines:\n%s\n  Types:\n%s\n  Errors:\n%s\n"
      bottom
      Type.pp expected
      nested_defines
      annotations
      errors


  let show state =
    Format.asprintf "%a" pp state


  let equal_nested_define left right =
    (* Ignore initial state. *)
    Define.equal left right


  and equal left right =
    (* Ignore errors in unit tests. *)
    Map.equal
      Annotation.equal
      (Resolution.annotations left.resolution)
      (Resolution.annotations right.resolution)


  let create
      ?(configuration = Configuration.create ())
      ~resolution
      ~define
      ?lookup
      ?(resolution_fixpoint = Int.Map.empty)
      () =
    {
      configuration;
      resolution;
      errors = Location.Map.empty;
      define;
      lookup;
      nested_defines = Location.Map.empty;
      bottom = false;
      resolution_fixpoint;
    }


  let errors
      {
        configuration;
        resolution;
        errors;
        define = ({
            Node.location;
            value = { Statement.Define.return_annotation; _ } as define;
          } as define_node);
        _;
      } =
    let constructor_errors errors =
      if not (Statement.Define.is_constructor define) then
        errors
      else
        begin
          match return_annotation with
          | Some ({ Node.location; _ } as annotation) ->
              let annotation = Resolution.parse_annotation resolution annotation in
              if Type.is_none annotation then
                errors
              else
                let error =
                  {
                    Error.location;
                    kind = Error.IncompatibleConstructorAnnotation annotation;
                    define = define_node;
                  }
                in
                error :: errors
          | _ ->
              errors
        end
    in
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
                  (Expression.Access.Identifier (Statement.Define.self_identifier define)) :: name
                in
                if Map.mem (Resolution.annotations resolution) access then
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
    |> constructor_errors
    |> Error.filter ~configuration ~resolution


  let coverage { resolution; _ } =
    Resolution.annotations resolution
    |> Map.data
    |> Coverage.aggregate


  let nested_defines { nested_defines; _ } =
    let process_define (location, nested) =
      Node.create ~location nested
    in
    Map.to_alist nested_defines
    |> List.map ~f:process_define


  let initial
      ?(configuration = Configuration.create ())
      ?lookup
      ~resolution
      ({
        Node.location;
        value = ({ Define.parent; parameters; _ } as define);
      } as define_node) =
    let resolution = Resolution.with_parent resolution ~parent in
    let { resolution; errors; _ } as initial =
      create ~configuration ~resolution ~define:define_node ?lookup ()
    in
    (* Check parameters. *)
    let annotations, errors =
      try
        let parameter
            index
            (annotations, errors)
            { Node.location; value = { Parameter.name; value; annotation }} =
          let access =
            name
            |> Identifier.show
            |> String.filter ~f:(fun character -> character <> '*')
            |> Identifier.create
            |> fun name -> [Access.Identifier name]
          in
          let { Annotation.annotation; mutability }, errors =
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
                Annotation.create annotation, errors
            | _ ->
                let add_missing_parameter_error value ~due_to_any =
                  let annotation = Annotated.resolve ~resolution value in
                  let error =
                    {
                      Error.location;
                      kind = Error.MissingParameterAnnotation {
                          Error.name = access;
                          annotation;
                          due_to_any;
                        };
                      define = define_node;
                    }
                  in
                  Annotation.create annotation,
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
                        | Type.Variable { Type.constraints = Type.Explicit constraints; _ } ->
                            Type.union constraints
                        | annotation ->
                            annotation
                      in
                      Annotation.create_immutable ~global:false annotation,
                      errors
                  | _ ->
                      Annotation.create Type.Bottom,
                      errors
                end
          in
          let annotation =
            if String.is_prefix ~prefix:"**" (Identifier.show name) then
              Type.dictionary ~key:Type.string ~value:annotation
            else if String.is_prefix ~prefix:"*" (Identifier.show name) then
              Type.sequence annotation
            else
              annotation
          in
          Map.set annotations ~key:access ~data:{ Annotation.annotation; mutability },
          errors
        in
        List.foldi ~init:((Resolution.annotations resolution), errors) ~f:parameter parameters
      with
      | TypeOrder.Untracked annotation ->
          let untracked_error =
            {
              Error.location;
              kind = Error.UndefinedType annotation;
              define = define_node;
            }
          in
          Resolution.annotations resolution,
          Map.set ~key:location ~data:untracked_error errors
    in

    (* Check behavioral subtyping. *)
    let errors =
      try
        if Define.is_constructor define ||
           Define.is_class_method define ||
           Define.is_static_method define ||
           Define.is_dunder_method define then
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
             if Type.is_resolved expected &&
                not (Resolution.less_or_equal resolution ~left:actual ~right:expected) then
               let error =
                 {
                   Error.location;
                   kind = Error.InconsistentOverride {
                       Error.overridden_method;
                       override = Error.WeakenedPostcondition { Error.actual; expected };
                     };
                   define = define_node;
                 }
               in
               Map.set ~key:location ~data:error errors
             else
               errors
           in

           (* Check weakening of precondition. *)
           let parameters =
             let remove_unused_parameter_denotation ~key ~data sofar =
               Identifier.Map.set sofar ~key:(Identifier.remove_leading_underscores key) ~data
             in
             Method.parameter_annotations definition ~resolution
             |> Map.fold ~init:Identifier.Map.empty ~f:remove_unused_parameter_denotation
           in
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
                               override =
                                 Error.StrengthenedPrecondition
                                   (Error.Found { Error.actual; expected });
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
                 let parameter_name =
                   Identifier.show_sanitized key
                   |> Identifier.create
                   |> fun name -> [Expression.Access.Identifier name]
                 in
                 let error =
                   {
                     Error.location;
                     kind = Error.InconsistentOverride {
                         Error.overridden_method;
                         override = Error.StrengthenedPrecondition (Error.NotFound parameter_name);
                       };
                     define = define_node;
                   }
                 in
                 Map.set ~key:location ~data:error errors
           in
           Map.fold
             ~init:errors
             ~f:parameter
             (Method.parameter_annotations overridden_method ~resolution))
          |> Option.value ~default:errors
      with
      | TypeOrder.Untracked annotation ->
          let untracked_error =
            {
              Error.location;
              kind = Error.UndefinedType annotation;
              define = define_node;
            }
          in
          Map.set ~key:location ~data:untracked_error errors
    in

    let resolution = Resolution.with_annotations resolution ~annotations in
    { initial with resolution; errors; }


  let less_or_equal ~left:({ resolution; _ } as left) ~right =
    if left.bottom then
      true
    else if right.bottom then
      false
    else
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
        ~f:(entry_less_or_equal
              (Resolution.annotations right.resolution)
              (Refinement.less_or_equal ~resolution))
        (Resolution.annotations left.resolution)


  let join_resolutions left_resolution right_resolution =
    let merge_annotations ~key:_ = function
      | `Both (left, right) ->
          Some (Refinement.join ~resolution:left_resolution left right)
      | `Left _
      | `Right _ ->
          Some (Annotation.create Type.Top)
    in
    let annotations =
      Map.merge
        ~f:merge_annotations
        (Resolution.annotations left_resolution)
        (Resolution.annotations right_resolution)
    in
    Resolution.with_annotations left_resolution ~annotations


  let join ({ resolution; _ } as left) right =
    if left.bottom then
      right
    else if right.bottom then
      left
    else
      let merge_errors ~key:_ = function
        | `Both (left, right) ->
            Some (Error.join ~resolution left right)
        | `Left state
        | `Right state ->
            Some state
      in
      {
        left with
        errors = Map.merge ~f:merge_errors left.errors right.errors;
        resolution = join_resolutions left.resolution right.resolution;
      }


  let widening_threshold = 10


  let rec meet ({ resolution; _ } as left) right =
    if left.bottom then
      left
    else if right.bottom then
      right
    else
      let merge meet ~key:_ = function
        | `Both (left, right) ->
            Some (meet left right)
        | `Left _
        | `Right _ ->
            None
      in
      let annotations =
        Map.merge
          ~f:(merge (Refinement.meet ~resolution))
          (Resolution.annotations left.resolution)
          (Resolution.annotations right.resolution);
      in
      {
        left with
        errors =
          Map.merge
            ~f:(merge (Error.meet ~resolution))
            left.errors
            right.errors;
        resolution = Resolution.with_annotations resolution ~annotations;
      }


  let widen ~previous:({ resolution; _ } as previous) ~next ~iteration =
    if previous.bottom then
      next
    else if next.bottom then
      previous
    else
      let widen_errors ~key:_ = function
        | `Both (previous, next) ->
            Some (Error.widen ~resolution ~previous ~next ~iteration)
        | `Left state
        | `Right state ->
            Some state
      in
      let widen_annotations ~key annotation =
        match annotation with
        | `Both (previous, next) ->
            Some (Refinement.widen ~resolution ~widening_threshold ~previous ~next ~iteration)
        | `Left previous
        | `Right previous when List.length key = 1 ->
            let widened =
              Refinement.widen
                ~resolution
                ~widening_threshold
                ~previous
                ~next:(Annotation.create Type.undeclared)
                ~iteration
            in
            Some widened
        | `Left previous
        | `Right previous ->
            Some previous
        | _ ->
            None
      in
      let annotations =
        Map.merge
          ~f:widen_annotations
          (Resolution.annotations previous.resolution)
          (Resolution.annotations next.resolution)
      in
      {
        previous with
        errors = Map.merge ~f:widen_errors previous.errors next.errors;
        resolution = Resolution.with_annotations resolution ~annotations;
      }


  let rec forward
      ?key
      ({
        configuration;
        resolution;
        errors;
        define = ({ Node.value = { Define.async; _ } as define; _ } as define_node);
        lookup;
        nested_defines;
        _;
      } as state)
      ~statement:({ Node.location; _ } as statement) =

    let expected =
      let annotation =
        Annotated.Callable.return_annotation ~define ~resolution
      in
      if async then
        Resolution.join resolution (Type.awaitable Type.Bottom) annotation
        |> Type.awaitable_value
      else
        annotation
    in

    let is_assert_function access =
      List.take_while access ~f:(function | Access.Identifier _ -> true | _ -> false)
      |> Access.show
      |> Set.mem Recognized.assert_functions
    in

    let update_annotation_maps ~resolution ~lookup ~access ~location ~annotation =
      (* The lookup database needs a Type.t only. *)
      lookup
      >>| Lookup.update ~location ~annotation:(Annotation.annotation annotation)
      |> ignore;
      Resolution.set_local resolution ~access ~annotation
    in

    let rec forward_annotations
        ({ resolution; _ } as state)
        ({ Node.value = statement_value; location = statement_location } as statement) =
      match statement_value with
      | Assign {
          Assign.target = { Node.value = Access access; location };
          annotation = Some annotation;
          _;
        } ->
          (* Type annotations override values. *)
          let annotation =
            Resolution.parse_annotation resolution annotation
            |> Annotation.create_immutable ~global:false
          in
          update_annotation_maps ~resolution ~lookup ~access ~location ~annotation
      | Assign assign ->
          let open Annotated in
          let open Access.Element in
          let forward_annotations
              ~target:({ Node.location; _ } as target)
              ~value_annotation
              resolution =
            let refine ~resolution ~target ~value_annotation =
              let access = Expression.access target in
              let annotation = Map.find (Resolution.annotations resolution) access in
              let element =
                Access.last_element ~resolution (Annotated.Access.create access)
              in
              let refined =
                match annotation, element with
                | Some annotation, _ when Annotation.is_immutable annotation ->
                    Refinement.refine ~resolution annotation value_annotation
                | _, Attribute { origin = Instance attribute; defined; _ }
                  when defined ->
                    Refinement.refine ~resolution (Attribute.annotation attribute) value_annotation
                | _, _ ->
                    Annotation.create value_annotation
              in
              update_annotation_maps ~resolution ~lookup ~access ~location ~annotation:refined
            in
            match Node.value target with
            | Tuple targets ->
                let rec refine_tuple resolution target =
                  match Node.value target with
                  | Tuple targets ->
                      List.fold targets ~init:resolution ~f:refine_tuple
                  | _ ->
                      refine
                        ~resolution
                        ~target
                        ~value_annotation:Type.Top
                in
                List.fold targets ~init:resolution ~f:refine_tuple
            | _ ->
                refine ~resolution ~target ~value_annotation
          in
          Assign.create assign
          |> Assign.fold ~resolution ~f:forward_annotations ~initial:resolution

      | Assert { Assert.test; _ } ->
          begin
            match Node.value test with
            | Access [
                Access.Identifier name;
                Access.Call {
                  Node.value = [
                    { Argument.name = None; value = { Node.value = Access access; _ } };
                    { Argument.name = None; value = annotation };
                  ];
                  _;
                }
              ] when Identifier.show name = "isinstance" ->
                let annotation =
                  match annotation with
                  | { Node.value = Tuple elements; _ } ->
                      Type.Union (List.map ~f:(Resolution.parse_annotation resolution) elements)
                  | _ ->
                      Resolution.parse_annotation resolution annotation
                in
                let updated_annotation =
                  match Resolution.get_local resolution ~access with
                  | Some existing_annotation when
                      Refinement.less_or_equal
                        ~resolution
                        existing_annotation
                        (Annotation.create annotation) ->
                      existing_annotation
                  | _ ->
                      Annotation.create annotation
                in
                update_annotation_maps
                  ~resolution
                  ~lookup
                  ~access
                  ~location
                  ~annotation:updated_annotation
            | UnaryOperator {
                UnaryOperator.operator = UnaryOperator.Not;
                operand = {
                  Node.value =
                    Access [
                      Access.Identifier name;
                      Access.Call {
                        Node.value = [
                          { Argument.name = None; value = { Node.location; value = Access access} };
                          { Argument.name = None; value = annotation };
                        ];
                        _;
                      };
                    ];
                  _;
                };
              } when Identifier.show name = "isinstance" ->
                begin
                  match Resolution.get_local resolution ~access with
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
                      update_annotation_maps
                        ~resolution
                        ~lookup
                        ~access
                        ~location
                        ~annotation:(Annotation.create constrained)
                  | _ ->
                      resolution
                end

            | Access access ->
                let open Annotated in
                let open Access.Element in
                let element = Access.last_element ~resolution (Access.create access) in
                begin
                  match Resolution.get_local resolution ~access, element with
                  | Some { Annotation.annotation = Type.Optional parameter; _ }, _ ->
                      update_annotation_maps
                        ~resolution
                        ~lookup
                        ~access
                        ~location:(Node.location test)
                        ~annotation:(Annotation.create parameter)
                  | _, Attribute { origin = Instance attribute; defined; _ }
                    when defined ->
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
                            update_annotation_maps
                              ~resolution
                              ~lookup
                              ~access
                              ~location:(Node.location test)
                              ~annotation:refined
                        | _ ->
                            resolution
                      end
                  | _ ->
                      resolution
                end

            | BooleanOperator { BooleanOperator.left; operator; right } ->
                let { resolution; _ } =
                  let update state expression =
                    forward_annotations
                      state
                      (Statement.assume expression)
                    |> Resolution.annotations
                  in
                  match operator with
                  | BooleanOperator.And ->
                      let resolution = forward_annotations state (Statement.assume left) in
                      let left = update state left in
                      let right = update { state with resolution } right in
                      let merge ~key:_ = function
                        | `Both (left, right) -> Some (Refinement.meet ~resolution left right)
                        | `Left left -> Some left
                        | `Right right -> Some right
                      in
                      let annotations = Map.merge ~f:merge left right in
                      let resolution = Resolution.with_annotations resolution ~annotations in
                      { state with resolution }
                  | BooleanOperator.Or ->
                      let negated_left =
                        update state (Expression.normalize (Expression.negate left))
                      in
                      let resolution =
                        Resolution.with_annotations resolution ~annotations:negated_left
                      in
                      let left = update state left in
                      let right =
                        update { state with resolution } right
                      in
                      join
                        {
                          state with
                          resolution = Resolution.with_annotations resolution ~annotations:left;
                        }
                        {
                          state with
                          resolution = Resolution.with_annotations resolution ~annotations:right;
                        }
                in
                resolution
            | ComparisonOperator {
                ComparisonOperator.left;
                right = [
                  ComparisonOperator.IsNot,
                  { Node.value = Access [Access.Identifier identifier; ]; _ }
                ];
              } when Identifier.show identifier = "None" ->
                let { resolution; _ } = forward state ~statement:(Statement.assume left) in
                resolution
            | ComparisonOperator {
                ComparisonOperator.left = { Node.location; value = Access access };
                right = [
                  ComparisonOperator.Is,
                  { Node.value = Access [Access.Identifier identifier; ]; _ }
                ];
              } when Identifier.show identifier = "None" ->
                let open Annotated in
                let open Access.Element in
                let element = Access.last_element ~resolution (Access.create access) in
                let refined =
                  match element with
                  | Attribute { origin = Instance attribute; defined; _ } when defined ->
                      Refinement.refine
                        ~resolution
                        (Attribute.annotation attribute)
                        (Type.Optional Type.Bottom)
                  | _ ->
                      Annotation.create (Type.Optional Type.Bottom)
                in
                update_annotation_maps ~resolution ~lookup ~access ~location ~annotation:refined
            | _ ->
                resolution
          end
      | Expression { Node.value = Access access; _; } when is_assert_function access ->
          let find_assert_test access =
            match access with
            | Expression.Record.Access.Call {
                Node.value = [{ Argument.value = test ; _ }];
                _;
              } -> Some test
            | _ -> None
          in
          List.find_map access ~f:find_assert_test
          >>| (fun assertion -> forward_annotations state (Statement.assume assertion))
          |> Option.value ~default:resolution

      | Stub (Stub.Class { Class.name; _ })
      | Class { Class.name; _ } ->
          begin
            match Resolution.get_local resolution ~access:name with
            | Some _ ->
                resolution
            | None ->
                (* Add a dummy annotation for locally defined classes. *)
                if not (Define.is_toplevel define) then
                  Resolution.set_local
                    resolution
                    ~access:[List.last_exn name]
                    ~annotation:(Annotation.create Type.Object)
                else
                  resolution
          end

      | Define { Define.name; _ } ->
          (* Don't propagate accesses in nested functions, they're analyzed separately.
             Add a dummy annotation for the last element of the name, as adding the full name causes
             module lookup issues. *)
          begin
            match Resolution.get_local resolution ~access:name with
            | Some _ ->
                resolution
            | None ->
                Resolution.set_local
                  resolution
                  ~access:name
                  ~annotation:(Annotation.create Type.Object)
          end
      | Global identifiers ->
          let access = Access.create_from_identifiers identifiers in
          let annotation =
            Resolution.resolve resolution (Node.create_with_default_location (Access access))
            |> Annotation.create_immutable ~global:true
          in
          update_annotation_maps
            ~resolution
            ~lookup
            ~access
            ~location:statement_location
            ~annotation

      | _ ->
          (* Walk through accesses and infer annotations as we go. *)
          let propagated =
            let propagate resolution { Node.value = access; location } =
              let propagate _ ~resolution ~resolved ~element:_ =
                lookup
                >>| Lookup.update ~location ~annotation:(Annotation.annotation resolved)
                |> ignore;
                resolution
              in
              Annotated.Access.fold
                ~resolution
                ~initial:resolution
                ~f:propagate
                (Annotated.Access.create access)
            in
            Visit.collect_accesses_with_location statement
            |> List.fold ~init:resolution ~f:propagate
          in
          let annotations =
            let set_mutability ~key ~data =
              let mutability =
                Resolution.get_local resolution ~access:key
                >>| Annotation.mutability
                |> Option.value ~default:Annotation.Mutable
              in
              { data with Annotation.mutability }
            in
            Map.mapi ~f:set_mutability (Resolution.annotations propagated)
          in
          Resolution.with_annotations resolution ~annotations
    in

    (* Gets typing errors that occur when trying to execute 'statement' in 'state' *)
    let errors =
      let add_errors errors =
        let add_error sofar error =
          Map.set ~key:error.Error.location ~data:error sofar
        in
        List.fold ~init:errors ~f:add_error
      in

      let rec check_access ~resolution errors { Node.location; value = access } =
        let open Annotated in
        let open Access.Element in
        let check_access new_errors ~resolution:_ ~resolved:_ ~element =
          if not (List.is_empty new_errors) then
            new_errors
          else
            match element with
            | Signature {
                signature =
                  (Signature.NotFound {
                      Signature.callable = { Type.Callable.kind; _ };
                      reason = Some reason;
                      _;
                    });
                _;
              } ->
                let open Signature in
                let error =
                  let callee =
                    match kind with
                    | Type.Callable.Named access ->
                        Some access
                    | _ ->
                        None
                  in
                  match reason with
                  | Mismatch mismatch ->
                      let mismatch, name, position, location =
                        let { Annotated.Signature.actual; expected; name; position } =
                          Node.value mismatch
                        in
                        { Error.actual; expected }, name, position, (Node.location mismatch)
                      in
                      {
                        Error.location;
                        kind =
                          Error.IncompatibleParameterType {
                            Error.name =
                              (name
                               >>| fun name -> Expression.Access.create_from_identifiers [name]);
                            position;
                            callee;
                            mismatch;
                          };
                        define = define_node;
                      }
                  | MissingArgument name  ->
                      {
                        Error.location;
                        kind = Error.MissingArgument { Error.callee; name };
                        define = define_node;
                      }
                  | TooManyArguments { expected; provided } ->
                      {
                        Error.location;
                        kind = Error.TooManyArguments { Error.callee; expected; provided };
                        define = define_node;
                      }
                in
                [error]

            | Attribute { attribute; origin; defined } when not defined ->
                let open Annotated in
                if Location.equal location Location.any then
                  begin
                    Statistics.event
                      ~name:"undefined attribute without location"
                      ~configuration
                      ~normals:["attribute", (Expression.Access.show attribute)]
                      ();
                    []
                  end
                else
                  let kind =
                    match origin with
                    | Instance class_attribute ->
                        Error.UndefinedAttribute {
                          Error.attribute;
                          origin =
                            Error.Class {
                              Error.annotation =
                                Class.annotation
                                  ~resolution
                                  (Attribute.parent class_attribute);
                              class_attribute = Attribute.class_attribute class_attribute;
                            };
                        }
                    | Module access when not (List.is_empty access) ->
                        Error.UndefinedAttribute {
                          Error.attribute;
                          origin = Error.Module access;
                        }
                    | Module _ ->
                        Error.UndefinedName attribute
                  in
                  { Error.location; kind; define = define_node } :: new_errors
            | _ ->
                new_errors
        in
        Access.fold ~resolution ~initial:[] ~f:check_access (Access.create access)
        |> add_errors errors

      and forward_expression state expression =
        forward state ~statement:(Node.create_with_default_location (Expression expression))

      and check_entry ~resolution errors { Dictionary.key; value } =
        let errors = check_expression ~resolution errors key in
        check_expression ~resolution errors value

      and check_generator
          state
          { Comprehension.target; iterator = { Node.location; _ } as iterator; conditions; _ } =
        (* TODO(T23723699): check async. *)
        (* Propagate the target type information. *)
        let iterator =
          let value =
            Access (Expression.access iterator @ [
                Access.Identifier (Identifier.create "__iter__");
                Access.Call (Node.create ~location []);
                Access.Identifier (Identifier.create "__next__");
                Access.Call (Node.create ~location []);
              ])
            |> Node.create ~location
          in
          Assign {
            Assign.target;
            annotation = None;
            value = Some value;
            parent = None;
          }
          |> Node.create ~location
        in
        (* Assign iterator. *)
        let state = forward state ~statement:iterator in
        (* Check conditions. *)
        List.map ~f:Statement.assume conditions
        |> List.fold ~init:state ~f:(fun state statement -> forward state ~statement)


      and check_expression ~resolution errors { Node.location; value } =
        match value with
        | Access access ->
            let errors = check_access ~resolution errors (Node.create ~location access) in
            (* Special case reveal_type() and cast(). *)
            let errors = match access with
              | [
                Expression.Access.Identifier reveal_type;
                Expression.Access.Call {
                  Node.location;
                  value = [{ Expression.Argument.value; _ }] };
              ] when reveal_type = Identifier.create "reveal_type" ->
                  let annotation = Annotated.resolve ~resolution value in
                  [{
                    Error.location;
                    kind = Error.RevealedType { Error.expression = value; annotation };
                    define = define_node;
                  }]
                  |> add_errors errors
              | [
                Expression.Access.Identifier typing;
                Expression.Access.Identifier cast;
                Expression.Access.Call {
                  Node.value = [
                    { Expression.Argument.value = cast_annotation; _ };
                    { Expression.Argument.value; _ };
                  ];
                  _;
                }
              ] when Identifier.equal typing (Identifier.create "typing") &&
                     Identifier.equal cast (Identifier.create "cast") ->
                  let cast_annotation = Annotated.resolve ~resolution cast_annotation in
                  let actual_annotation = Annotated.resolve ~resolution value in
                  if Type.is_meta cast_annotation &&
                     Type.equal (Type.single_parameter cast_annotation) actual_annotation then
                    add_errors errors [{
                        Error.location;
                        kind = Error.RedundantCast actual_annotation;
                        define = define_node;
                      }]
                  else
                    errors
              | _ ->
                  errors
            in
            let check_single_access errors access =
              match access with
              | Access.Identifier _ ->
                  Resolution.get_local resolution ~access:[access]
                  >>| Annotation.annotation
                  |> Option.iter ~f:(function annotation ->
                      lookup
                      (* T30344109: this location is imprecise. *)
                      >>| Lookup.update ~location ~annotation
                      |> ignore);
                  errors

              | Access.Call { Node.value = arguments; _ } ->
                  let check_argument
                      errors
                      { Argument.value = { Node.location = value_location; _ } as value; name } =
                    let location =
                      (* If this is a named parameter, extend the lookup location to include the
                         name as well. *)
                      name
                      >>| (fun { Node.location = { Location.start; _ } ; _ } ->
                          { value_location with Location.start })
                      |> Option.value ~default:value_location
                    in
                    lookup
                    >>| Lookup.update
                      ~location
                      ~annotation:(Annotated.resolve ~resolution value)
                    |> ignore;
                    check_expression ~resolution errors value
                  in
                  List.fold ~f:check_argument ~init:errors arguments

              | Access.Expression expression ->
                  check_expression ~resolution errors expression
            in
            List.fold ~f:check_single_access ~init:errors access

        | Await expression ->
            let errors = check_expression ~resolution errors expression in
            let actual = Annotated.resolve ~resolution expression in
            let is_awaitable =
              Resolution.less_or_equal
                resolution
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
              let state = { state with resolution } in
              List.fold ~init:state ~f:(fun state statement -> forward state ~statement) right
            in
            errors

        | ComparisonOperator { ComparisonOperator.left; right; _ } ->
            let errors = check_expression ~resolution errors left in
            let accumulate errors (_, expression) =
              check_expression ~resolution errors expression
            in
            List.fold ~f:accumulate ~init:errors right

        | Dictionary { Dictionary.entries; keywords } ->
            let errors = List.fold ~f:(check_entry ~resolution) ~init:errors entries in
            begin
              match keywords with
              | None -> errors
              | Some keyword -> check_expression ~resolution errors keyword
            end

        | DictionaryComprehension { Comprehension.element; generators } ->
            let state = { state with resolution } in
            let { resolution; errors; _ } = List.fold ~f:check_generator ~init:state generators in
            check_entry ~resolution errors element

        | FormatString { FormatString.expression_list; _ } ->
            List.fold ~f:(check_expression ~resolution) ~init:errors expression_list

        | Lambda { Lambda.body; parameters } ->
            let resolution =
              let add_parameter resolution { Node.value = { Parameter.name; _ }; _ } =
                update_annotation_maps
                  ~resolution
                  ~lookup
                  ~access:(Access.create_from_identifiers [name])
                  ~location
                  ~annotation:(Annotation.create Type.Object)
              in
              List.fold ~f:add_parameter ~init:resolution parameters
            in
            check_expression ~resolution errors body

        | List list
        | Set list
        | Tuple list ->
            List.fold ~f:(check_expression ~resolution) ~init:errors list

        | Generator { Comprehension.element; generators }
        | ListComprehension { Comprehension.element; generators }
        | SetComprehension { Comprehension.element; generators } ->
            let state = { state with resolution } in
            let { resolution; errors; _ } = List.fold ~f:check_generator ~init:state generators in
            check_expression ~resolution errors element

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
            let state = { state with resolution } in
            let { errors; _ } =
              let resolution = forward_annotations state (Statement.assume test) in
              forward_expression { state with resolution; errors } target
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
        | String _ | Complex _ | Bytes _ | Float _ | Integer _ | False | True ->
            errors
      in

      match Node.value statement with
      | Assign ({
          Assign.annotation = explicit_annotation;
          value = Some value;
          _;
        } as assign) ->
          let check_assign ~target:({ Node.location; _ } as target) ~value_annotation errors =
            let access = Expression.access target in
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
              let open Access.Element in
              match Access.last_element ~resolution (Access.create access) with
              | Attribute { origin = Instance attribute; defined; _ } when defined ->
                  let expected = Annotation.original (Attribute.annotation attribute) in
                  let name =
                    Expression.access
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
              | Attribute { origin = Instance attribute; defined; _ } when not defined ->
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
                  let location =
                    Resolution.global resolution access
                    >>| Node.location
                    |> Option.value ~default:location
                  in
                  let existing_annotation, is_explicit =
                    match explicit_annotation with
                    (* Explicit annotations override our existing knowledge of the target's type. *)
                    | Some annotation ->
                        Resolution.parse_annotation resolution annotation
                        |> Annotation.create_immutable ~global:true,
                        true
                    | None ->
                        let resolved =
                          let default =
                            Resolution.resolve resolution target
                            |> Annotation.create
                          in
                          Resolution.get_local resolution ~access
                          |> Option.value ~default
                        in
                        resolved , false
                  in
                  match existing_annotation with
                  | {
                    Annotation.mutability = Annotation.Immutable {
                        Annotation.scope = Annotation.Global;
                        original = expected;
                      };
                    _;
                  } ->
                      let errors =
                        add_incompatible_type_error
                          ~expected
                          ~parent:None
                          ~name
                          ~declare_location:location
                          errors
                      in
                      (* Don't error when encountering an explicit annotation or a type alias. *)
                      if is_explicit || Type.is_meta value_annotation then
                        errors
                      else
                        add_missing_annotation_error
                          ~expected
                          ~parent:None
                          ~declare_location:location
                          ~name
                          errors
                  | {
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
                  | annotation when Type.is_tuple (Annotation.annotation annotation) ->
                      add_incompatible_type_error
                        ~expected:(Annotation.annotation annotation)
                        ~parent:None
                        ~name:(Expression.Access.create "left hand side")
                        ~declare_location:location
                        errors
                  | _ ->
                      errors
            in
            let errors = check_expression ~resolution errors value in
            let check_target =
              match Node.value target with
              | Access access when List.length access = 1 ->
                  false
              | Tuple _ ->
                  false
              | List _ ->
                  false
              | _ ->
                  true
            in
            if check_target then
              check_expression ~resolution errors target
            else
              errors
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

      | Return { Return.expression; is_implicit } ->
          let actual =
            Option.value_map
              expression
              ~default:Type.none
              ~f:(Annotated.resolve ~resolution)
          in
          if not (Resolution.less_or_equal resolution ~left:actual ~right:expected) &&
             not (Define.is_abstract_method define) &&
             not (Define.is_overloaded_method define) &&
             not (Type.is_none actual &&
                  (Annotated.Define.create define |> Annotated.Define.is_generator)) &&
             not (Type.is_none actual && Type.is_noreturn expected) then
            let error =
              {
                Error.location;
                kind = Error.IncompatibleReturnType {
                    Error.mismatch = { Error.expected; actual };
                    is_implicit;
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
                  kind = Error.IncompatibleReturnType {
                      Error.mismatch = { Error.expected; actual };
                      is_implicit = false;
                    };
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
                  kind = Error.IncompatibleReturnType {
                      Error.mismatch = { Error.expected; actual };
                      is_implicit = false;
                    };
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

      | Import { Import.from; imports } ->
          let imports =
            match from with
            | Some from -> [from]
            | None -> List.map imports ~f:(fun { Import.name; _ } -> name)
          in
          let to_import_error import =
            let add_import_error errors ~resolution:_ ~resolved:_ ~element =
              let open Annotated.Access.Element in
              match element with
              | Attribute { origin = Module _; defined = false; _ } ->
                  {
                    Error.location;
                    kind = Error.UndefinedImport import;
                    define = define_node;
                  } :: errors
              | _ ->
                  errors
            in
            Annotated.Access.fold
              ~f:add_import_error
              ~resolution
              ~initial:[]
              (Annotated.Access.create import)
          in
          List.concat_map ~f:to_import_error imports
          |> add_errors errors

      | YieldFrom _ ->
          errors

      | Class _ | Define _ ->
          (* Don't check accesses in nested classes and functions, they're analyzed separately. *)
          errors

      | For _  | If _ | Try _ | With _ | While _ ->
          (* Check happens implicitly in the resulting control flow. *)
          errors

      | Break | Continue | Global _ | Nonlocal _ | Pass | Stub _ ->
          errors
    in
    let resolution = forward_annotations state statement in

    let terminates_control_flow =
      match statement with
      | { Node.value = Expression expression; _ } ->
          Resolution.resolve resolution expression
          |> Type.is_noreturn
      | _ ->
          false
    in

    let state = { state with resolution; errors; lookup; bottom = terminates_control_flow } in

    let nested_defines =
      let schedule ~define = Map.set nested_defines ~key:location ~data:define
      in
      match Node.value statement with
      | Class { Class.name; body; _ } ->
          schedule ~define:(Define.create_class_toplevel ~qualifier:name ~statements:body)
      | Define define ->
          schedule ~define
      | _ ->
          nested_defines
    in

    let state =
      let resolution_fixpoint =
        match key with
        | Some key ->
            Int.Map.set
              state.resolution_fixpoint
              ~key
              ~data:(Resolution.annotations state.resolution)
        | None ->
            state.resolution_fixpoint
      in
      { state with resolution_fixpoint }
    in

    { state with nested_defines }


  let backward state ~statement:_ =
    state
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


let check
    configuration
    environment
    ?mode_override
    ({ Source.path; qualifier; statements; _ } as source) =
  Log.debug "Checking %s..." path;

  let resolution = Environment.resolution environment () in
  let lookup = Lookup.create () in

  let check
      ~define:({ Node.location; value = { Define.name; parent; _ } as define } as define_node)
      ~initial
      ~queue =
    Log.log ~section:`Check "Checking %a" Access.pp name;
    let dump = Define.dump define in

    if dump then
      begin
        Log.dump
          "Checking `%s`..."
          (Log.Color.yellow (Access.show name));
        Log.dump "AST:\n%s" (Annotated.Define.create define |> Annotated.Define.show);
      end;

    let dump_cfg cfg fixpoint =
      let precondition table id =
        match Hashtbl.find table id with
        | Some { State.resolution; _ } ->
            let stringify ~key ~data label =
              let annotation_string =
                Type.show (Annotation.annotation data)
                |> String.strip ~drop:((=) '`')
              in
              label ^ "\n" ^ Access.show key ^ ": " ^ annotation_string
            in
            Map.fold ~f:stringify ~init:"" (Resolution.annotations resolution)
        | None -> ""
      in
      if Define.dump_cfg define then
        begin
          let name =
            match parent with
            | Some parent -> parent @ name
            | None -> name
          in
          Path.create_relative
            ~root:(Configuration.pyre_root configuration)
            ~relative:(Format.asprintf "cfgs%a.dot" Access.pp name)
          |> File.create ~content:(Some (Cfg.to_dot ~precondition:(precondition fixpoint) cfg))
          |> File.write
        end;
      fixpoint
    in

    try
      let exit =
        let cfg = Cfg.create define in
        Fixpoint.forward ~cfg ~initial
        |> dump_cfg cfg
        |> Fixpoint.exit
      in
      if dump then exit >>| Log.dump "Exit state:\n%a" State.pp |> ignore;

      let () =
        (* Write fixpoint type resolutions to shared memory *)
        let open AnalysisTypeResolutionSharedMemory in
        let dump_resolutions { State.resolution_fixpoint; _ } =
          let serialize ~key ~data:annotations accumulator =
            {
              key;
              annotations = Access.Map.to_alist annotations
            } :: accumulator
          in
          Int.Map.fold resolution_fixpoint ~init:[] ~f:serialize
          |> AnalysisTypeResolutionSharedMemory.add name
        in
        exit
        >>| dump_resolutions
        |> ignore
      in

      let () =
        (* Schedule nested functions for analysis. *)
        if Define.is_toplevel define || Define.is_class_toplevel define then
          match exit with
          | Some ({ State.resolution; _ } as exit) ->
              State.nested_defines exit
              |> List.iter ~f:(fun define -> Queue.enqueue queue (define, resolution))
          | None ->
              ()
      in

      let errors =
        exit
        >>| State.errors
        |> Option.value ~default:[]
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
            [{
              Error.location;
              kind = Error.UndefinedType annotation;
              define = define_node;
            }];
          coverage = Coverage.create ~crashes:1 ();
        }
  in

  let results =
    let queue =
      let queue = Queue.create () in
      let toplevel =
        let location =
          {
            Location.path;
            start = { Location.line = 0; column = 0 };
            stop = { Location.line = 0; column = 0 };
          }
        in
        Define.create_toplevel ~qualifier ~statements
        |> Node.create ~location
      in
      Queue.enqueue queue (toplevel, resolution);
      queue
    in
    let rec results ~queue =
      match Queue.dequeue queue with
      | Some (define, resolution) ->
          let initial =
            State.initial
              ~configuration
              ~lookup
              ~resolution
              define
          in
          let result = check ~define ~initial ~queue in
          result :: results ~queue
      | _ ->
          []
    in
    results ~queue
  in

  let errors =
    let filter errors =
      if configuration.debug then
        errors
      else
        let keep_error ({ Error.location = { Location.path; _ }; _ } as error) =
          let mode =
            match mode_override with
            | Some mode ->
                mode
            | None ->
                let (module Handler: Environment.Handler) = environment in
                Handler.mode path
                |> Option.value ~default:Source.Default
          in
          not (Error.suppress ~mode error)
        in
        List.filter ~f:keep_error errors
    in
    List.map ~f:SingleSourceResult.errors results
    |> List.map ~f:filter
    |> List.concat
    |> Error.join_at_source ~resolution
    |> List.map ~f:(Error.dequalify (Preprocessing.dequalify_map source) environment)
    |> List.sort ~compare:Error.compare
  in

  let coverage =
    List.map ~f:SingleSourceResult.coverage results
    |> Coverage.aggregate_over_source ~source
  in
  Coverage.log coverage ~configuration ~total_errors:(List.length errors) ~path;

  { Result.errors; lookup = Some lookup; coverage }
