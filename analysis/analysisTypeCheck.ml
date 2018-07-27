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
    errors: Error.t Location.Reference.Map.t;
    define: Define.t Node.t;
    nested_defines: nested_define Location.Reference.Map.t;
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
          Location.Instantiated.pp
          (Location.instantiate ~lookup:(fun hash -> AstSharedMemory.get_path ~hash) location)
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
      ?(resolution_fixpoint = Int.Map.empty)
      () =
    {
      configuration;
      resolution;
      errors = Location.Reference.Map.empty;
      define;
      nested_defines = Location.Reference.Map.empty;
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
                  Error.create
                    ~location
                    ~kind:(Error.IncompatibleConstructorAnnotation annotation)
                    ~define:define_node
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
                    Error.create
                      ~location:(Attribute.location attribute)
                      ~kind:(
                        Error.UninitializedAttribute {
                          Error.name;
                          parent = class_definition;
                          mismatch = {
                            Error.expected;
                            actual = (Type.optional expected)
                          };
                        })
                      ~define:define_node
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
    |> Error.join_at_define
      ~resolution
      ~location:(Location.instantiate ~lookup:(fun hash -> AstSharedMemory.get_path ~hash) location)
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
      ~resolution
      ({
        Node.location;
        value = ({ Define.parent; parameters; _ } as define);
      } as define_node) =
    let resolution = Resolution.with_parent resolution ~parent in
    let { resolution; errors; _ } as initial =
      create ~configuration ~resolution ~define:define_node ()
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
                    Error.create
                      ~location
                      ~kind:(Error.MissingParameterAnnotation {
                          Error.name = access;
                          annotation;
                          due_to_any;
                        })
                      ~define:define_node
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
            Error.create
              ~location
              ~kind:(Error.UndefinedType annotation)
              ~define:define_node
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
                 Error.create
                   ~location
                   ~kind:(Error.InconsistentOverride {
                       Error.overridden_method;
                       override = Error.WeakenedPostcondition { Error.actual; expected };
                     })
                   ~define:define_node
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
                         Error.create
                           ~location
                           ~kind:(Error.InconsistentOverride {
                               Error.overridden_method;
                               override =
                                 Error.StrengthenedPrecondition
                                   (Error.Found { Error.actual; expected });
                             })
                           ~define:define_node
                       in
                       Map.set ~key:location ~data:error errors
                     else
                       errors
                   with TypeOrder.Untracked _ ->
                     (* TODO(T27409168): Error here. *)
                     errors
                 end
             | None ->
                 let has_keyword_and_anonymous_starred_parameters =
                   let starred =
                     let collect_starred_parameters ~key ~data:_ starred =
                       if String.is_prefix (Identifier.show key) ~prefix:"*" then
                         key :: starred
                       else
                         starred
                     in
                     Map.fold ~f:collect_starred_parameters ~init:[] parameters
                   in
                   let count_stars parameter =
                     Identifier.show parameter
                     |> String.take_while ~f:(fun character -> character = '*')
                     |> String.length
                   in
                   List.exists ~f:(fun parameter -> count_stars parameter = 1) starred
                   && List.exists ~f:(fun parameter -> count_stars parameter = 2) starred
                 in
                 if has_keyword_and_anonymous_starred_parameters then
                   errors
                 else
                   let parameter_name =
                     Identifier.show_sanitized key
                     |> Identifier.create
                     |> fun name -> [Expression.Access.Identifier name]
                   in
                   let error =
                     Error.create
                       ~location
                       ~kind:(Error.InconsistentOverride {
                           Error.overridden_method;
                           override =
                             Error.StrengthenedPrecondition (Error.NotFound parameter_name);
                         })
                       ~define:define_node
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
            Error.create
              ~location
              ~kind:(Error.UndefinedType annotation)
              ~define:define_node
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


  let add_error ~state:({ errors; _ } as state) error =
    {
      state with
      errors = Map.set errors ~key:(Location.reference (Error.location error)) ~data:error;
    }


  type resolved = {
    state: t;
    resolved: Type.t;
  }


  let rec forward_expression
      ~state:({ resolution; errors; define; _ } as state)
      ~expression:{ Node.location; value } =
    let rec forward_entry ~state ~entry:{ Dictionary.key; value } =
      let { state; _ } = forward_expression ~state ~expression:key in
      let { state; _ } = forward_expression ~state ~expression:value in
      state
    in
    let forward_generator
        ~state
        ~generator:{
          Comprehension.target;
          iterator = { Node.location; _ } as iterator;
          conditions;
          _;
        } =
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
      let state = forward_statement ~state ~statement:iterator in
      List.map ~f:Statement.assume conditions
      |> List.fold ~init:state ~f:(fun state statement -> forward_statement ~state ~statement)
    in
    match value with
    | Access access ->
        let resolution =
          (* Walk through accesses and infer annotations as we go. *)
          let propagated =
            let propagate _ ~resolution ~resolved:_ ~element:_ =
              resolution
            in
            Annotated.Access.fold
              ~resolution
              ~initial:resolution
              ~f:propagate
              (Annotated.Access.create access)
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
        let state = { state with resolution } in
        let state =
          let open Annotated in
          let open Access.Element in
          let forward_access new_errors ~resolution:_ ~resolved:_ ~element =
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
                        Error.create
                          ~location
                          ~kind:
                            (Error.IncompatibleParameterType {
                                Error.name =
                                  (name
                                   >>| fun name -> Expression.Access.create_from_identifiers [name]);
                                position;
                                callee;
                                mismatch;
                              })
                          ~define
                    | MissingArgument name ->
                        Error.create
                          ~location
                          ~kind:(Error.MissingArgument { Error.callee; name })
                          ~define
                    | TooManyArguments { expected; provided } ->
                        Error.create
                          ~location
                          ~kind:(Error.TooManyArguments { Error.callee; expected; provided })
                          ~define
                  in
                  [error]

              | Attribute { attribute; origin; defined } when not defined ->
                  let open Annotated in
                  if Location.Reference.equal location Location.Reference.any then
                    begin
                      Statistics.event
                        ~name:"undefined attribute without location"
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
                    (Error.create ~location ~kind ~define) :: new_errors
              | _ ->
                  new_errors
          in
          Access.fold (Access.create access) ~resolution ~initial:[] ~f:forward_access
          |> List.fold ~init:state ~f:(fun state error -> add_error ~state error)
        in

        (* Special case reveal_type() and cast(). *)
        let state =
          match access with
          | [
            Expression.Access.Identifier reveal_type;
            Expression.Access.Call {
              Node.location;
              value = [{ Expression.Argument.value; _ }] };
          ] when reveal_type = Identifier.create "reveal_type" ->
              let annotation = Annotated.resolve ~resolution value in
              Error.create
                ~location
                ~kind:(Error.RevealedType { Error.expression = value; annotation })
                ~define
              |> add_error ~state
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
                Error.create
                  ~location
                  ~kind:(Error.RedundantCast actual_annotation)
                  ~define
                |> add_error ~state
              else
                state
          | _ ->
              state
        in
        let forward_access state access =
          match access with
          | Access.Identifier _ ->
              state
          | Access.Call { Node.value = arguments; _ } ->
              let forward_argument state { Argument.value; _ } =
                let { state; _ } = forward_expression ~state ~expression:value in
                state
              in
              List.fold arguments ~f:forward_argument ~init:state
          | Access.Expression expression ->
              let { state; _ } = forward_expression ~state ~expression in
              state
        in
        let state = List.fold access ~f:forward_access ~init:state in
        { state; resolved = Type.Top }

    | Await expression ->
        let { state; _ } = forward_expression ~state ~expression in
        let actual = Annotated.resolve ~resolution expression in
        let is_awaitable =
          Resolution.less_or_equal
            resolution
            ~left:actual
            ~right:(Type.awaitable Type.Object)
        in
        let state =
          if not is_awaitable then
            Error.create
              ~location
              ~kind:(Error.IncompatibleAwaitableType actual)
              ~define
            |> add_error ~state
          else
            state
        in
        { state; resolved = Type.Top }

    | BooleanOperator {
        BooleanOperator.left;
        operator;
        right = ({ Node.location; _ } as right);
      } ->
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
        let state =
          List.fold
            ~init:state
            ~f:(fun state statement -> forward_statement ~state ~statement)
            right
        in
        { state; resolved = Type.Top }

    | ComparisonOperator { ComparisonOperator.left; right; _ } ->
        let { state; _ } = forward_expression ~state ~expression:left in
        let accumulate state (_, expression) =
          let { state; _ } = forward_expression ~state ~expression in
          state
        in
        let state = List.fold right ~f:accumulate ~init:state in
        { state; resolved = Type.Top }

    | Complex _ ->
        { state; resolved = Type.complex }

    | Dictionary { Dictionary.entries; keywords } ->
        let state =
          List.fold
            entries
            ~f:(fun state entry -> forward_entry ~state ~entry)
            ~init:state
        in
        let state =
          match keywords with
          | None ->
              state
          | Some keyword ->
              let { state; _ } = forward_expression ~state ~expression:keyword in
              state
        in
        { state; resolved = Type.Top }

    | DictionaryComprehension { Comprehension.element; generators } ->
        let state = { state with resolution } in
        let state =
          List.fold
            generators
            ~f:(fun state generator -> forward_generator ~state ~generator)
            ~init:state
        in
        let state = forward_entry ~state ~entry:element in
        { state; resolved = Type.Top }

    | False ->
        { state; resolved = Type.bool }

    | Float _ ->
        { state; resolved = Type.float }

    | Integer _ ->
        { state; resolved = Type.integer }

    | Lambda { Lambda.body; parameters } ->
        let resolution =
          let add_parameter resolution { Node.value = { Parameter.name; _ }; _ } =
            let name =
              let name = Identifier.show name in
              String.chop_prefix name ~prefix:"*"
              |> Option.value ~default:name
            in
            Resolution.set_local
              resolution
              ~access:(Access.create name)
              ~annotation:(Annotation.create Type.Object)
          in
          List.fold ~f:add_parameter ~init:resolution parameters
        in
        forward_expression ~state:{ state with resolution } ~expression:body

    | List elements
    | Set elements
    | Tuple elements ->
        let state =
          List.fold
            elements
            ~f:(fun state expression ->
                let { state; _ } = forward_expression ~state ~expression in
                state)
            ~init:state
        in
        { state; resolved = Type.Top }

    | Generator { Comprehension.element; generators }
    | ListComprehension { Comprehension.element; generators }
    | SetComprehension { Comprehension.element; generators } ->
        let state = { state with resolution } in
        let state =
          List.fold
            generators
            ~f:(fun state generator -> forward_generator ~state ~generator)
            ~init:state
        in
        forward_expression ~state ~expression:element

    | Starred starred ->
        begin
          match starred with
          | Starred.Once expression
          | Starred.Twice expression ->
              forward_expression ~state ~expression
        end

    | String { StringLiteral.kind = StringLiteral.Format expressions; _ } ->
        let state =
          List.fold
            expressions
            ~f:(fun state expression ->
                let { state; _ } = forward_expression ~state ~expression in
                state)
            ~init:state
        in
        { state; resolved = Type.Top }

    | Ternary { Ternary.target; test; alternative } ->
        let state = { state with resolution } in
        let target_state =
          forward_statement ~state ~statement:(Statement.assume test)
          |> fun state ->
          let { state; _ } = forward_expression ~state ~expression:target in
          state
        in
        let alternative_state =
          forward_statement ~state ~statement:(Statement.assume (Expression.negate test))
          |> fun state ->
          let { state; _ } = forward_expression ~state ~expression:alternative in
          state
        in
        let state = join target_state alternative_state in
        { state; resolved = Type.Top }

    | True ->
        { state; resolved = Type.bool }

    | UnaryOperator { UnaryOperator.operand; operator = _ } ->
        forward_expression ~state ~expression:operand

    | Expression.Yield yield ->
        let state =
          match yield with
          | None ->
              state
          | Some expression ->
              let { state; _ } = forward_expression ~state ~expression in
              state
        in
        { state; resolved = Type.Top }

    (* Trivial base cases *)
    | Ellipses | String _ ->
        { state; resolved = Type.Top }


  and forward_statement
      ~state:({
          resolution;
          define = ({
              Node.value = { Define.async; _ } as define_without_location;
              _;
            } as define);
          _;
        } as state)
      ~statement:{ Node.location; value } =
    let instantiate location =
      Location.instantiate ~lookup:(fun hash -> AstSharedMemory.get_path ~hash) location
    in
    let is_assert_function access =
      List.take_while access ~f:(function | Access.Identifier _ -> true | _ -> false)
      |> Access.show
      |> Set.mem Recognized.assert_functions
    in
    let expected =
      let annotation =
        Annotated.Callable.return_annotation ~define:define_without_location ~resolution
      in
      if async then
        Resolution.join resolution (Type.awaitable Type.Bottom) annotation
        |> Type.awaitable_value
      else
        annotation
    in
    match value with
    | Assign ({
        Assign.target;
        annotation = explicit_annotation;
        value;
        _;
      } as assign) ->
        let { resolution; _ } as state =
          value
          >>| (fun expression ->
               let { state; _ } = forward_expression ~state ~expression in
               state)
          |> Option.value ~default:state
        in
        let state =
          let resolution =
            match target, explicit_annotation with
            | { Node.value = Access access; _ }, Some annotation ->
                (* Type annotations override values. *)
                let annotation =
                  Resolution.parse_annotation resolution annotation
                  |> Annotation.create_immutable ~global:false
                in
                Resolution.set_local resolution ~access ~annotation
            | _ ->
                let open Annotated in
                let open Access.Element in
                let forward_annotations
                    ~target
                    ~value_annotation
                    state =
                  let refine ~state:({ resolution; _ } as state) ~target ~value_annotation =
                    let resolution =
                      let access = Expression.access target in
                      let annotation = Map.find (Resolution.annotations resolution) access in
                      let element =
                        Access.last_element
                          ~resolution
                          (Annotated.Access.create access)
                      in
                      let refined =
                        match annotation, element with
                        | Some annotation, _ when Annotation.is_immutable annotation ->
                            Refinement.refine ~resolution annotation value_annotation
                        | _, Attribute { origin = Instance attribute; defined; _ }
                          when defined ->
                            Refinement.refine
                              ~resolution
                              (Attribute.annotation attribute)
                              value_annotation
                        | _, _ ->
                            Annotation.create value_annotation
                      in
                      Resolution.set_local resolution ~access ~annotation:refined
                    in
                    { state with resolution }
                  in
                  match Node.value target with
                  | Tuple targets ->
                      let rec refine_tuple state target =
                        match Node.value target with
                        | Tuple targets -> List.fold targets ~init:state ~f:refine_tuple
                        | _ -> refine ~state ~target ~value_annotation:Type.Top
                      in
                      List.fold targets ~init:state ~f:refine_tuple
                  | _ ->
                      refine ~state ~target ~value_annotation
                in
                Assign.create assign
                |> Assign.fold ~resolution ~f:forward_annotations ~initial:state
                |> fun { resolution; _ } -> resolution
          in
          { state with resolution }
        in
        let forward_assign ~target:({ Node.location; _ } as target) ~value_annotation state =
          let access = Expression.access target in
          let add_incompatible_type_error ~expected ~parent ~name ~declare_location state =
            if Resolution.less_or_equal resolution ~left:value_annotation ~right:expected then
              state
            else
              let error =
                match parent with
                | Some parent ->
                    Error.create
                      ~location
                      ~kind:(Error.IncompatibleAttributeType {
                          Error.parent;
                          incompatible_type = {
                            Error.name;
                            mismatch = { Error.expected; actual = value_annotation };
                            declare_location;
                          };
                        })
                      ~define
                | None ->
                    Error.create
                      ~location
                      ~kind:(Error.IncompatibleVariableType {
                          Error.name;
                          mismatch = { Error.expected; actual = value_annotation };
                          declare_location;
                        })
                      ~define
              in
              add_error ~state error
          in
          let add_missing_annotation_error
              ~expected
              ~parent
              ~name
              ~declare_location
              ({ errors; _ } as state) =
            if ((Type.is_unknown expected) || (Type.equal expected Type.Object)) &&
               not (Type.is_unknown value_annotation) then
              let evidence_location = instantiate location in
              let error =
                match parent with
                | Some parent ->
                    Error.create
                      ~location:declare_location
                      ~kind:(Error.MissingAttributeAnnotation {
                          Error.parent;
                          missing_annotation = {
                            Error.name;
                            annotation = value_annotation;
                            evidence_locations = [evidence_location];
                            due_to_any = Type.equal expected Type.Object;
                          };
                        })
                      ~define
                | None ->
                    Error.create
                      ~location:declare_location
                      ~kind:(Error.MissingGlobalAnnotation {
                          Error.name;
                          annotation = value_annotation;
                          evidence_locations = [evidence_location];
                          due_to_any = Type.equal expected Type.Object;
                        })
                      ~define
              in
              Map.find errors declare_location
              >>| Error.join ~resolution error
              |> Option.value ~default:error
              |> add_error ~state
            else
              state
          in
          let state =
            let open Annotated in
            let open Access.Element in
            match Access.last_element ~resolution (Access.create access) with
            | Attribute { origin = Instance attribute; defined; _ } when defined ->
                let expected = Annotation.original (Attribute.annotation attribute) in
                let name =
                  Expression.access
                    (Node.create_with_default_location (Attribute.name attribute))
                in
                state
                |> add_incompatible_type_error
                  ~expected
                  ~parent:(Some (Attribute.parent attribute))
                  ~name
                  ~declare_location:(Attribute.location attribute |> instantiate)
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
                        state
                  | _ ->
                      state
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
                    let state =
                      add_incompatible_type_error
                        ~expected
                        ~parent:None
                        ~name
                        ~declare_location:(instantiate location)
                        state
                    in
                    (* Don't error when encountering an explicit annotation or a type alias. *)
                    if is_explicit || Type.is_meta value_annotation then
                      state
                    else
                      add_missing_annotation_error
                        ~expected
                        ~parent:None
                        ~declare_location:location
                        ~name
                        state
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
                      ~declare_location:(instantiate location)
                      state
                | annotation when Type.is_tuple (Annotation.annotation annotation) ->
                    add_incompatible_type_error
                      ~expected:(Annotation.annotation annotation)
                      ~parent:None
                      ~name:(Expression.Access.create "left hand side")
                      ~declare_location:(instantiate location)
                      state
                | _ ->
                    state
          in
          let check_target =
            match Node.value target with
            | Access access when List.length access = 1 -> false
            | Tuple _ -> false
            | List _ -> false
            | _ -> true
          in
          if check_target then
            let { state; _ } = forward_expression ~state ~expression:target in
            state
          else
            state
        in
        Annotated.Assign.create assign
        |> Annotated.Assign.fold ~resolution ~f:forward_assign ~initial:state

    | Assert { Assert.test; _ } ->
        let { resolution; _ } as state =
          let { state; _ } = forward_expression ~state ~expression:test in
          state
        in
        let resolution =
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
              Resolution.set_local
                resolution
                ~access
                ~annotation:updated_annotation
          | UnaryOperator {
              UnaryOperator.operator = UnaryOperator.Not;
              operand = {
                Node.value =
                  Access [
                    Access.Identifier name;
                    Access.Call {
                      Node.value = [
                        { Argument.name = None; value = { Node.value = Access access; _ } };
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
                    Resolution.set_local
                      resolution
                      ~access
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
                    Resolution.set_local
                      resolution
                      ~access
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
                          Resolution.set_local
                            resolution
                            ~access
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
                  forward_statement ~state ~statement:(Statement.assume expression)
                  |> fun { resolution; _ } -> Resolution.annotations resolution
                in
                match operator with
                | BooleanOperator.And ->
                    let resolution =
                      forward_statement ~state ~statement:(Statement.assume left)
                      |> fun { resolution; _ } -> resolution
                    in
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
              let { resolution; _ } = forward_statement ~state ~statement:(Statement.assume left) in
              resolution
          | ComparisonOperator {
              ComparisonOperator.left = { Node.value = Access access; _ };
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
              Resolution.set_local resolution ~access ~annotation:refined
          | _ ->
              resolution
        in
        { state with resolution }
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
        >>| (fun assertion -> forward_statement ~state ~statement:(Statement.assume assertion))
        |> Option.value ~default:state

    | Delete _ ->
        (* TODO(T26146217): add coverage. *)
        state

    | Expression expression ->
        let { state; _ } = forward_expression ~state ~expression in
        state

    | Global identifiers ->
        let resolution =
          let access = Access.create_from_identifiers identifiers in
          let annotation =
            Resolution.resolve resolution (Node.create_with_default_location (Access access))
            |> Annotation.create_immutable ~global:true
          in
          Resolution.set_local
            resolution
            ~access
            ~annotation
        in
        { state with resolution }

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
                Error.create
                  ~location
                  ~kind:(Error.UndefinedImport import)
                  ~define
                :: errors
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
        |> List.fold ~init:state ~f:(fun state error -> add_error ~state error)

    | Raise (Some expression) ->
        let { state; _ } = forward_expression ~state ~expression in
        state
    | Raise _ ->
        state

    | Return { Return.expression; is_implicit } ->
        let actual =
          Option.value_map
            expression
            ~default:Type.none
            ~f:(Annotated.resolve ~resolution)
        in
        if not (Resolution.less_or_equal resolution ~left:actual ~right:expected) &&
           not (Define.is_abstract_method define_without_location) &&
           not (Define.is_overloaded_method define_without_location) &&
           not (Type.is_none actual &&
                (Annotated.Define.create define_without_location
                |> Annotated.Define.is_generator)) &&
           not (Type.is_none actual && Type.is_noreturn expected) then
          let error =
            Error.create
              ~location
              ~kind:(Error.IncompatibleReturnType {
                  Error.mismatch = { Error.expected; actual };
                  is_implicit;
                })
              ~define
          in
          add_error ~state error
        else if Type.equal expected Type.Top || Type.equal expected Type.Object then
          let error =
            Error.create
              ~location
              ~kind:(Error.MissingReturnAnnotation {
                  Error.annotation = actual;
                  evidence_locations = [location.Location.start.Location.line];
                  due_to_any = Type.equal expected Type.Object;
                })
              ~define
          in
          add_error ~state error
        else
          state

    | Statement.Yield { Node.value = Expression.Yield return; _ } ->
        let state =
          return
          >>| (fun expression ->
               let { state; _ } = forward_expression ~state ~expression in
               state)
          |> Option.value ~default:state
        in
        let actual =
          Option.value_map return ~default:Type.none ~f:(Annotated.resolve ~resolution)
          |> Type.generator ~async
        in
        if not (Resolution.less_or_equal resolution ~left:actual ~right:expected) then
          Error.create
            ~location
            ~kind:(Error.IncompatibleReturnType {
                Error.mismatch = { Error.expected; actual };
                is_implicit = false;
              })
            ~define
          |> add_error ~state
        else if Type.equal expected Type.Top || Type.equal expected Type.Object then
          Error.create
            ~location
            ~kind:(Error.MissingReturnAnnotation {
                Error.annotation = actual;
                evidence_locations = [location.Location.start.Location.line];
                due_to_any = Type.equal expected Type.Object;
              })
            ~define
          |> add_error ~state
        else
          state
    | Statement.Yield _ ->
        state

    | YieldFrom { Node.value = Expression.Yield (Some return); _ } ->
        let { state; _ } = forward_expression ~state ~expression:return in
        let actual =
          match Annotated.resolve ~resolution return with
          | Type.Parametric { Type.name; parameters = [parameter] }
            when Identifier.show name = "typing.Iterator" ->
              Type.generator parameter
          | annotation -> Type.generator annotation
        in
        if not (Resolution.less_or_equal resolution ~left:actual ~right:expected) then
          Error.create
            ~location
            ~kind:(Error.IncompatibleReturnType {
                Error.mismatch = { Error.expected; actual };
                is_implicit = false;
              })
            ~define
          |> add_error ~state
        else if Type.equal expected Type.Top || Type.equal expected Type.Object then
          Error.create
            ~location
            ~kind:(Error.MissingReturnAnnotation {
                Error.annotation = actual;
                evidence_locations = [location.Location.start.Location.line];
                due_to_any = Type.equal expected Type.Object;
              })
            ~define
          |> add_error ~state
        else
          state
    | YieldFrom _ ->
        state

    | Class _ | Define _ ->
        (* Don't check accesses in nested classes and functions, they're analyzed
           separately. *)
        state

    | For _  | If _ | Try _ | With _ | While _ ->
        (* Check happens implicitly in the resulting control flow. *)
        state

    | Break | Continue | Nonlocal _ | Pass ->
        state


  let rec forward
      ?key
      ({
        resolution;
        resolution_fixpoint;
        errors;
        define = { Node.value = { Define.async; _ }; _ };
        nested_defines;
        bottom;
        _;
      } as state)
      ~statement:({ Node.location; _ } as statement) =
    let state =
      if bottom then
        state
      else
        forward_statement ~state ~statement
    in

    let terminates_control_flow =
      match statement with
      | { Node.value = Expression expression; _ } ->
          Resolution.resolve resolution expression
          |> Type.is_noreturn
      | _ ->
          false
    in
    let state = { state with bottom = terminates_control_flow } in

    let nested_defines =
      let schedule ~define = Map.set nested_defines ~key:location ~data:define in
      match Node.value statement with
      | Class { Class.name; body; _ } ->
          schedule ~define:(Define.create_class_toplevel ~qualifier:name ~statements:body)
      | Define define when not (Define.is_stub define) ->
          schedule ~define
      | _ ->
          nested_defines
    in

    let state =
      let resolution_fixpoint =
        match key with
        | Some key -> Map.set resolution_fixpoint ~key ~data:(Resolution.annotations resolution)
        | None -> resolution_fixpoint
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
          ~integers:[]
          ~normals:[
            "path", path;
            "define", Access.show name;
            "type", Type.show annotation;
          ]
          ();
        {
          SingleSourceResult.errors =
            [
              Error.create
                ~location
                ~kind:(Error.UndefinedType annotation)
                ~define:define_node;
            ];
          coverage = Coverage.create ~crashes:1 ();
        }
  in

  let results =
    let queue =
      let queue = Queue.create () in
      let toplevel =
        let location =
          {
            Location.path = path;
            start = { Location.line = 0; column = 0 };
            stop = { Location.line = 0; column = 0 };
          }
          |> Location.reference
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
        let keep_error error =
          let mode =
            match mode_override with
            | Some mode ->
                mode
            | None ->
                let (module Handler: Environment.Handler) = environment in
                Handler.mode (Error.path error)
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
  Coverage.log coverage ~total_errors:(List.length errors) ~path;

  { Result.errors; coverage }
