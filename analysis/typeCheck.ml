(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
open Ast
open Expression
open Statement
module Error = AnalysisError

module ErrorKey = struct
  type t = {
    location: Location.Instantiated.t;
    kind: int
  }
  [@@deriving compare, sexp]

  module Map = Map.Make (struct
    type nonrec t = t

    let compare = compare

    let sexp_of_t = sexp_of_t

    let t_of_sexp = t_of_sexp
  end)

  let add_error ~errors ({ Error.location; _ } as error) =
    Map.set errors ~key:{ location; kind = Error.code error } ~data:error
end

module type Context = sig
  val configuration : Configuration.Analysis.t

  val define : Define.t Node.t
end

module type Signature = sig
  type t [@@deriving eq]

  val create
    :  ?bottom:bool ->
    ?errors:Error.t ErrorKey.Map.t ->
    resolution:Resolution.t ->
    ?resolution_fixpoint:ResolutionSharedMemory.annotation_map Int.Map.Tree.t ->
    unit ->
    t

  val resolution : t -> Resolution.t

  val error_map : t -> Error.t ErrorKey.Map.t

  val errors : t -> Error.t list

  val coverage : t -> Coverage.t

  val initial : resolution:Resolution.t -> t

  (* Visible for testing. *)
  type resolved = {
    state: t;
    resolved: Type.t
  }

  val parse_and_check_annotation : ?bind_variables:bool -> state:t -> Expression.t -> t * Type.t

  val forward_expression : state:t -> expression:Expression.t -> resolved

  val forward_statement : state:t -> statement:Statement.t -> t

  include Fixpoint.State with type t := t
end

module State (Context : Context) = struct
  type nested_define_state = {
    nested_resolution: Resolution.t;
    nested_bottom: bool
  }

  type nested_define = {
    nested: Define.t;
    initial: nested_define_state
  }

  type partitioned = {
    consistent_with_boundary: Type.t;
    not_consistent_with_boundary: Type.t option
  }

  and t = {
    resolution: Resolution.t;
    errors: Error.t ErrorKey.Map.t;
    check_return: bool;
    nested_defines: nested_define Location.Reference.Map.t;
    bottom: bool;
    resolution_fixpoint: ResolutionSharedMemory.annotation_map Int.Map.Tree.t
  }

  let pp_nested_define format { nested = { Define.signature = { name; _ }; _ }; _ } =
    Format.fprintf format "%a" Reference.pp name


  let pp format
         { resolution; errors; nested_defines; bottom; _ } =
    let expected =
      Annotated.Callable.return_annotation ~define:(Node.value Context.define) ~resolution
    in
    let nested_defines =
      let nested_define_to_string nested_define =
        Format.asprintf "    %a" pp_nested_define nested_define
      in
      Map.data nested_defines |> List.map ~f:nested_define_to_string |> String.concat ~sep:"\n"
    in
    let annotations =
      let annotation_to_string (name, annotation) =
        Format.asprintf "    %a -> %a" Reference.pp name Annotation.pp annotation
      in
      Resolution.annotations resolution
      |> Map.to_alist
      |> List.map ~f:annotation_to_string
      |> String.concat ~sep:"\n"
    in
    let errors =
      let error_to_string error =
        Format.asprintf
          "    %a -> %s"
          Location.Instantiated.pp
          (Error.location error)
          (Error.description error ~show_error_traces:true)
      in
      List.map (Map.data errors) ~f:error_to_string |> String.concat ~sep:"\n"
    in
    Format.fprintf
      format
      "  Bottom: %b\n\
      \  Expected return: %a\n\
      \  Nested defines:\n\
       %s\n\
      \  Types:\n\
       %s\n\
      \  Errors:\n\
       %s\n"
      bottom
      Type.pp
      expected
      nested_defines
      annotations
      errors


  let show state = Format.asprintf "%a" pp state

  and equal left right =
    (* Ignore errors in unit tests. *)
    Map.equal
      Annotation.equal
      (Resolution.annotations left.resolution)
      (Resolution.annotations right.resolution)
    && left.bottom = right.bottom


  let create
      ?(bottom = false)
      ?(errors = ErrorKey.Map.empty)
      ~resolution
      ?(resolution_fixpoint = Int.Map.Tree.empty)
      ()
    =
    { resolution;
      errors;
      check_return = true;
      nested_defines = Location.Reference.Map.empty;
      bottom;
      resolution_fixpoint
    }


  let add_invalid_type_parameters_errors ~resolution ~location ~errors annotation =
    let mismatches, annotation = Resolution.check_invalid_type_parameters resolution annotation in
    let add_error errors mismatch =
      Error.create ~location ~kind:(Error.InvalidTypeParameters mismatch) ~define:Context.define
      |> ErrorKey.add_error ~errors
    in
    List.fold mismatches ~f:add_error ~init:errors, annotation


  let parse_and_check_annotation
      ?(bind_variables = true)
      ~state:({ errors; resolution; _ } as state)
      ({ Node.location; _ } as expression)
    =
    let check_and_correct_annotation ~resolution ~location ~annotation ~resolved errors =
      let is_aliased_to_any =
        (* Special-case expressions typed as Any to be valid types. *)
        match annotation with
        | Type.Primitive _ -> Type.is_any resolved
        | _ -> false
      in
      let check_untracked_annotation errors annotation =
        if Resolution.is_tracked resolution annotation || is_aliased_to_any then
          errors
        else if not (Type.is_unknown resolved || Type.is_any resolved) then
          Error.create
            ~location
            ~kind:(Error.InvalidType (InvalidType annotation))
            ~define:Context.define
          :: errors
        else
          Error.create ~location ~kind:(Error.UndefinedType annotation) ~define:Context.define
          :: errors
      in
      let check_invalid_variables resolution errors variable =
        if not (Resolution.type_variable_exists resolution ~variable) then
          let error =
            let origin =
              if Define.is_toplevel (Node.value Context.define) then
                Error.Toplevel
              else if Define.is_class_toplevel (Node.value Context.define) then
                Error.ClassToplevel
              else
                Error.Define
            in
            Error.create
              ~location
              ~kind:(Error.InvalidTypeVariable { annotation = variable; origin })
              ~define:Context.define
          in
          error :: errors
        else
          errors
      in
      let resolution =
        match annotation with
        | Type.Callable
            { Type.Callable.implementation =
                { Type.Callable.parameters = Type.Callable.Defined parameters; _ }
            ; _
            } ->
            let parameters =
              List.map parameters ~f:Type.Callable.Parameter.annotation
              |> List.concat_map ~f:Type.Variable.all_free_variables
            in
            List.fold
              parameters
              ~f:(fun resolution variable -> Resolution.add_type_variable resolution ~variable)
              ~init:resolution
        | _ -> resolution
      in
      let critical_errors =
        List.fold ~init:[] ~f:check_untracked_annotation (Type.elements annotation)
        |> fun errors ->
        Type.Variable.all_free_variables annotation
        |> List.fold ~f:(check_invalid_variables resolution) ~init:errors
      in
      if List.is_empty critical_errors then
        add_invalid_type_parameters_errors annotation ~resolution ~location ~errors
      else
        let errors =
          List.fold critical_errors ~init:errors ~f:(fun errors error ->
              ErrorKey.add_error ~errors error)
        in
        errors, Type.Top
    in
    let annotation =
      Resolution.parse_annotation
        ~allow_untracked:true
        ~allow_invalid_type_parameters:true
        resolution
        expression
    in
    let errors =
      if Type.is_top annotation then (* Could not even parse expression. *)
        Error.create
          ~location
          ~kind:(Error.InvalidType (InvalidType (Type.Primitive (Expression.show expression))))
          ~define:Context.define
        |> ErrorKey.add_error ~errors
      else
        errors
    in
    let resolved = Resolution.resolve resolution expression in
    let errors, annotation =
      check_and_correct_annotation errors ~resolution ~location ~annotation ~resolved
    in
    let annotation =
      if bind_variables then Type.Variable.mark_all_variables_as_bound annotation else annotation
    in
    { state with errors }, annotation


  let resolution { resolution; _ } = resolution

  let error_map { errors; _ } = errors

  let errors { resolution; errors; _ } =
    let { Node.value = { Define.signature = { name; _ }; _ } as define; location } =
      Context.define
    in
    let class_initialization_errors errors =
      (* Ensure non-nullable typed attributes are instantiated in init. This must happen after
         typechecking is finished to access the annotations added to resolution. *)
      let check_attributes_initialized () =
        let open Annotated in
        Define.parent_definition ~resolution (Define.create define)
        >>| (fun definition ->
              let propagate_initialization_errors errors attribute =
                let expected = Annotation.annotation (Attribute.annotation attribute) in
                let location = Attribute.location attribute in
                match Attribute.name attribute with
                | name when not (Type.is_top expected || Attribute.initialized attribute) ->
                    let reference =
                      Reference.create_from_list [Statement.Define.self_identifier define; name]
                    in
                    if
                      Map.mem (Resolution.annotations resolution) reference
                      && not (Statement.Define.is_class_toplevel define)
                    then
                      errors
                    else
                      let error =
                        Error.create
                          ~location
                          ~kind:
                            (Error.UninitializedAttribute
                               { name;
                                 parent = Annotated.Class.annotation definition;
                                 mismatch =
                                   { Error.expected;
                                     actual = Type.optional expected;
                                     actual_expressions = [];
                                     due_to_invariance = false
                                   }
                               })
                          ~define:Context.define
                      in
                      error :: errors
                | name ->
                    let actual = expected in
                    let check_override
                        ( { Node.value = { Attribute.annotation; name; final; _ }; _ } as
                        overridden_attribute )
                      =
                      let expected = Annotation.annotation annotation in
                      if
                        ( Resolution.less_or_equal resolution ~left:actual ~right:expected
                        || Type.is_top actual )
                        && not final
                      then
                        errors
                      else
                        let kind =
                          if final then
                            Error.InvalidAssignment (Final (Reference.create name))
                          else
                            Error.InconsistentOverride
                              { overridden_method = name;
                                parent =
                                  Attribute.parent overridden_attribute
                                  |> Type.show
                                  |> Reference.create;
                                override_kind = Attribute;
                                override =
                                  Error.WeakenedPostcondition
                                    (Error.create_mismatch
                                       ~resolution
                                       ~actual
                                       ~actual_expression:None
                                       ~expected
                                       ~covariant:false)
                              }
                        in
                        Error.create ~location ~kind ~define:Context.define :: errors
                    in
                    Class.overrides ~resolution ~name definition
                    >>| check_override
                    |> Option.value ~default:errors
              in
              Class.attribute_fold
                ~include_generated_attributes:false
                ~initial:errors
                ~resolution
                ~f:propagate_initialization_errors
                definition)
        |> Option.value ~default:errors
      in
      let check_bases () =
        let open Annotated in
        let is_final { Call.Argument.name; value } =
          let add_error { Resolution.is_final; _ } =
            if is_final then
              let error =
                Error.create
                  ~location
                  ~kind:(Error.InvalidInheritance (Class (Expression.show value)))
                  ~define:Context.define
              in
              error :: errors
            else
              errors
          in
          match name, value with
          | None, { Node.value = Name name; _ } when Expression.is_simple_name name ->
              let reference = Reference.from_name_exn name in
              Resolution.class_metadata resolution (Type.Primitive (Reference.show reference))
              >>| add_error
              |> Option.value ~default:errors
          | _ -> errors
        in
        Define.parent_definition ~resolution (Define.create define)
        >>| Class.bases
        >>| List.map ~f:is_final
        >>| List.concat
        |> Option.value ~default:errors
      in
      if Define.is_constructor define && not (Define.is_stub define) then
        let base_errors = check_bases () in
        List.append base_errors (check_attributes_initialized ())
      else if Define.is_class_toplevel define then
        let no_explicit_class_constructor =
          let name = Reference.prefix name >>| Reference.show |> Option.value ~default:"" in
          Resolution.class_definition resolution (Type.Primitive name)
          >>| Annotated.Class.create
          >>| Annotated.Class.constructors ~resolution
          >>| List.is_empty
          |> Option.value ~default:false
        in
        if no_explicit_class_constructor then
          let base_errors = check_bases () in
          List.append base_errors (check_attributes_initialized ())
        else
          errors
      else
        errors
    in
    let overload_errors errors =
      let annotation = Resolution.get_local resolution ~reference:name in
      let check_implementation errors =
        match annotation with
        | Some { annotation = Type.Callable { implementation; _ }; _ }
          when Statement.Define.is_overloaded_method define
               && Type.Callable.Overload.is_undefined implementation ->
            let error =
              Error.create
                ~location
                ~kind:(Error.MissingOverloadImplementation name)
                ~define:Context.define
            in
            error :: errors
        | _ -> errors
      in
      let check_compatible_return_types errors =
        match annotation with
        | Some
            { annotation =
                Type.Callable
                  { overloads; implementation = { annotation = implementation_annotation; _ }; _ }
            ; _
            }
          when not (Statement.Define.is_overloaded_method define) ->
            List.fold
              ~init:errors
              ~f:(fun sofar { annotation; _ } ->
                if
                  Resolution.is_consistent_with
                    resolution
                    annotation
                    implementation_annotation
                    ~expression:None
                then
                  sofar
                else
                  let error =
                    Error.create
                      ~location
                      ~kind:
                        (Error.IncompatibleOverload
                           { implementation_annotation; overload_annotation = annotation; name })
                      ~define:Context.define
                  in
                  error :: sofar)
              overloads
        | _ -> errors
      in
      check_implementation errors |> check_compatible_return_types
    in
    Map.data errors
    |> Error.deduplicate
    |> class_initialization_errors
    |> overload_errors
    |> Error.filter ~configuration:Context.configuration ~resolution


  let coverage { resolution; _ } =
    Resolution.annotations resolution |> Map.data |> Coverage.aggregate


  let nested_defines { nested_defines; _ } =
    let process_define (location, { nested; initial = { nested_resolution; _ } }) =
      Node.create ~location nested, nested_resolution
    in
    Map.to_alist nested_defines |> List.map ~f:process_define


  let less_or_equal ~left:({ resolution; _ } as left) ~right =
    if left.bottom then
      true
    else if right.bottom then
      false
    else
      let entry_less_or_equal other less_or_equal ~key ~data sofar =
        sofar
        &&
        match Map.find other key with
        | Some other -> less_or_equal data other
        | _ -> false
      in
      let left_errors = Map.data left.errors |> Error.Set.of_list in
      let right_errors = Map.data right.errors |> Error.Set.of_list in
      Set.is_subset left_errors ~of_:right_errors
      && Map.fold
           ~init:true
           ~f:(entry_less_or_equal right.nested_defines (fun _ _ -> true))
           left.nested_defines
      && Map.fold
           ~init:true
           ~f:
             (entry_less_or_equal
                (Resolution.annotations right.resolution)
                (Refinement.less_or_equal ~resolution))
           (Resolution.annotations left.resolution)


  let join_resolutions left_resolution right_resolution =
    let merge_annotations ~key:_ = function
      | `Both (left, right) -> Some (Refinement.join ~resolution:left_resolution left right)
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


  let join left right =
    if left.bottom then
      right
    else if right.bottom then
      left
    else
      let join_nested_defines ~key:_ = function
        | `Left nested_define
        | `Right nested_define
        | `Both (_, nested_define) ->
            Some nested_define
      in
      let join_resolutions left_resolution right_resolution =
        let merge_annotations ~key:_ = function
          | `Both (left, right) -> Some (Refinement.join ~resolution:left_resolution left right)
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
      let combine_errors ~key:_ left_error right_error =
        Error.join ~resolution:left.resolution left_error right_error
      in
      { left with
        errors = Map.merge_skewed left.errors right.errors ~combine:combine_errors;
        nested_defines = Map.merge left.nested_defines right.nested_defines ~f:join_nested_defines;
        resolution = join_resolutions left.resolution right.resolution
      }


  let widening_threshold = 10

  let widen ~previous:({ resolution; _ } as previous) ~next ~iteration =
    if previous.bottom then
      next
    else if next.bottom then
      previous
    else
      let join_nested_defines ~key:_ = function
        | `Left nested_define
        | `Right nested_define
        | `Both (_, nested_define) ->
            Some nested_define
      in
      let widen_annotations ~key annotation =
        match annotation with
        | `Both (previous, next) ->
            Some (Refinement.widen ~resolution ~widening_threshold ~previous ~next ~iteration)
        | `Left previous
        | `Right previous
          when Reference.length key = 1 ->
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
        | _ -> None
      in
      let annotations =
        Map.merge
          ~f:widen_annotations
          (Resolution.annotations previous.resolution)
          (Resolution.annotations next.resolution)
      in
      let join_resolution_fixpoints ~key:_ = function
        | `Left next_resolution
        | `Right next_resolution
        | `Both (_, next_resolution) ->
            Some next_resolution
      in
      let resolution_fixpoint =
        Int.Map.Tree.merge
          ~f:join_resolution_fixpoints
          previous.resolution_fixpoint
          next.resolution_fixpoint
      in
      let combine_errors ~key:_ left_error right_error =
        if iteration > widening_threshold then
          { left_error with Error.kind = Error.Top }
        else
          Error.join ~resolution:previous.resolution left_error right_error
      in
      { previous with
        errors = Map.merge_skewed previous.errors next.errors ~combine:combine_errors;
        nested_defines =
          Map.merge ~f:join_nested_defines previous.nested_defines next.nested_defines;
        resolution = Resolution.with_annotations resolution ~annotations;
        resolution_fixpoint
      }


  let emit_raw_error ~state:({ errors; resolution; _ } as state) ({ Error.location; _ } as error) =
    let error =
      match Map.find errors { ErrorKey.location; kind = Error.code error } with
      | Some other_error -> Error.join ~resolution error other_error
      | None -> error
    in
    { state with errors = ErrorKey.add_error ~errors error }


  let emit_error ~state ~location ~kind =
    Error.create ~location ~kind ~define:Context.define |> emit_raw_error ~state


  type resolved = {
    state: t;
    resolved: Type.t
  }

  let rec initial ~resolution =
    let { Node.location;
          value =
            { Define.signature = { name; parent; parameters; return_annotation; decorators; _ }
            ; _
            } as define
        }
      =
      Context.define
    in
    let check_decorators state =
      let check_final_decorator state =
        if Option.is_none parent && Define.is_final_method define then
          emit_error
            ~state
            ~location
            ~kind:(Error.InvalidInheritance (NonMethodFunction "typing.final"))
        else
          state
      in
      let check_decorator state decorator =
        let is_whitelisted decorator =
          let has_suffix { Node.value; _ } suffix =
            match value with
            | Expression.Name (Expression.Name.Attribute { attribute; _ })
              when String.equal attribute suffix ->
                true
            | _ -> false
          in
          let is_property_derivative decorator =
            has_suffix decorator "setter"
            || has_suffix decorator "getter"
            || has_suffix decorator "deleter"
          in
          let is_click_derivative decorator = has_suffix decorator "command" in
          (* TODO (T41383196): Properly deal with @property and @click *)
          is_property_derivative decorator || is_click_derivative decorator
        in
        if is_whitelisted decorator then
          state
        else
          let { state; _ } = forward_expression ~state ~expression:decorator in
          state
      in
      List.fold decorators ~init:state ~f:check_decorator |> check_final_decorator
    in
    let check_return_annotation state =
      let add_variance_error (state, annotation) =
        let state =
          match annotation with
          | Type.Variable variable when Type.Variable.Unary.is_contravariant variable ->
              emit_error
                ~state
                ~location
                ~kind:(Error.InvalidTypeVariance { annotation; origin = Error.Return })
          | _ -> state
        in
        state, annotation
      in
      let update_define (state, annotation) =
        if Type.is_unknown annotation then
          { state with check_return = false }
        else
          state
      in
      return_annotation
      >>| parse_and_check_annotation ~state
      >>| add_variance_error
      >>| update_define
      |> Option.value ~default:state
    in
    let check_parameter_annotations ({ resolution; resolution_fixpoint; _ } as state) =
      let state, annotations =
        let check_parameter
            index
            (state, annotations)
            { Node.location; value = { Parameter.name; value; annotation } }
          =
          let add_incompatible_variable_error ~state ~value annotation default =
            if
              Type.is_any default
              || Resolution.less_or_equal resolution ~left:default ~right:annotation
              || Resolution.constraints_solution_exists resolution ~left:default ~right:annotation
            then
              state
            else
              let instantiate location =
                Location.instantiate
                  ~lookup:(fun hash -> Ast.SharedMemory.Handles.get ~hash)
                  location
              in
              emit_error
                ~state
                ~location
                ~kind:
                  (Error.IncompatibleVariableType
                     { name = Reference.create name;
                       mismatch =
                         Error.create_mismatch
                           ~resolution
                           ~expected:annotation
                           ~actual:default
                           ~actual_expression:value
                           ~covariant:true;
                       declare_location = instantiate location
                     })
          in
          let add_missing_parameter_annotation_error ~state ~given_annotation annotation =
            let name = name |> Identifier.sanitized in
            if
              String.equal name "*"
              || String.is_prefix ~prefix:"_" name
              || Option.is_some given_annotation
                 && (String.is_prefix ~prefix:"**" name || String.is_prefix ~prefix:"*" name)
            then
              state
            else
              emit_error
                ~state
                ~location
                ~kind:
                  (Error.MissingParameterAnnotation
                     { name = Reference.create name;
                       annotation;
                       given_annotation;
                       evidence_locations = [];
                       thrown_at_source = true
                     })
          in
          let add_final_parameter_annotation_error ~state =
            emit_error ~state ~location ~kind:(Error.InvalidType (FinalParameter name))
          in
          let add_variance_error (state, annotation) =
            let state =
              match annotation with
              | Type.Variable variable
                when (not (Statement.Define.is_constructor define))
                     && Type.Variable.Unary.is_covariant variable ->
                  emit_error
                    ~state
                    ~location
                    ~kind:(Error.InvalidTypeVariance { annotation; origin = Error.Parameter })
              | _ -> state
            in
            state, annotation
          in
          let state, { Annotation.annotation; mutability } =
            match index, parent with
            | 0, Some parent
            (* __new__ does not require an annotation for __cls__, even though it is a static
               method. *)
              when not
                     ( Define.is_class_toplevel define
                     || Define.is_static_method define
                        && not (String.equal (Define.unqualified_name define) "__new__") ) -> (
                let resolved, is_class_method =
                  let parent_annotation =
                    let parent_name = Reference.show parent in
                    let parent_type = Type.Primitive parent_name in
                    let variables =
                      TypeOrder.variables (Resolution.order resolution) parent_type
                    in
                    match variables with
                    | None
                    | Some [] ->
                        parent_type
                    | Some variables ->
                        Type.Parametric { name = parent_name; parameters = variables }
                    | exception _ -> parent_type
                  in
                  if Define.is_class_method define || Define.is_class_property define then
                    (* First parameter of a method is a class object. *)
                    Type.meta parent_annotation, true
                  else (* First parameter of a method is the callee object. *)
                    parent_annotation, false
                in
                match annotation with
                | Some annotation ->
                    let state, annotation =
                      parse_and_check_annotation ~state ~bind_variables:false annotation
                    in
                    let compatible =
                      Resolution.constraints_solution_exists
                        resolution
                        ~left:annotation
                        ~right:resolved
                    in
                    let state =
                      let name = Identifier.sanitized name in
                      let kind =
                        if compatible then
                          None
                        else if
                          (is_class_method && String.equal name "cls")
                          || ((not is_class_method) && String.equal name "self")
                        then (* Assume the user incorrectly tried to type the implicit parameter *)
                          Some
                            (Error.InvalidMethodSignature { annotation = Some annotation; name })
                        else (* Assume the user forgot to specify the implicit parameter *)
                          Some
                            (Error.InvalidMethodSignature
                               { annotation = None;
                                 name = (if is_class_method then "cls" else "self")
                               })
                      in
                      match kind with
                      | Some kind -> emit_error ~state ~location ~kind
                      | None -> state
                    in
                    state, Annotation.create annotation
                | None -> state, Annotation.create resolved )
            | _ -> (
                let annotation_and_state =
                  annotation
                  >>| parse_and_check_annotation ~state ~bind_variables:false
                  >>| add_variance_error
                in
                let contains_prohibited_any parsed_annotation =
                  let contains_literal_any =
                    annotation >>| Type.expression_contains_any |> Option.value ~default:false
                  in
                  contains_literal_any
                  && not (Resolution.is_string_to_any_mapping resolution parsed_annotation)
                in
                match annotation_and_state, value with
                | Some (_, annotation), Some value when Type.contains_final annotation ->
                    let { resolved = value_annotation; _ } =
                      forward_expression ~state ~expression:value
                    in
                    ( add_final_parameter_annotation_error ~state,
                      Annotation.create_immutable
                        ~global:false
                        ~original:(Some annotation)
                        value_annotation )
                | Some (_, annotation), Some value when contains_prohibited_any annotation ->
                    let { resolved = value_annotation; _ } =
                      forward_expression ~state ~expression:value
                    in
                    ( add_missing_parameter_annotation_error
                        ~state
                        ~given_annotation:(Some annotation)
                        (Some value_annotation),
                      Annotation.create_immutable
                        ~global:false
                        ~original:(Some annotation)
                        value_annotation )
                | Some (_, annotation), _ when Type.contains_final annotation ->
                    ( add_final_parameter_annotation_error ~state,
                      Annotation.create_immutable ~global:false annotation )
                | Some (_, annotation), None when contains_prohibited_any annotation ->
                    ( add_missing_parameter_annotation_error
                        ~state
                        ~given_annotation:(Some annotation)
                        None,
                      Annotation.create_immutable ~global:false annotation )
                | Some (state, annotation), value ->
                    let state =
                      value
                      >>| (fun value -> forward_expression ~state ~expression:value)
                      >>| (fun { resolved; _ } -> resolved)
                      >>| add_incompatible_variable_error ~state ~value annotation
                      |> Option.value ~default:state
                    in
                    state, Annotation.create_immutable ~global:false annotation
                | None, Some value ->
                    let { resolved = annotation; _ } =
                      forward_expression ~state ~expression:value
                    in
                    ( add_missing_parameter_annotation_error
                        ~state
                        ~given_annotation:None
                        (Some annotation),
                      Annotation.create annotation )
                | None, None ->
                    ( add_missing_parameter_annotation_error ~state ~given_annotation:None None,
                      Annotation.create Type.Any ) )
          in
          let annotation =
            let annotation = Type.Variable.mark_all_variables_as_bound annotation in
            if String.is_prefix ~prefix:"**" name then
              Type.dictionary ~key:Type.string ~value:annotation
            else if String.is_prefix ~prefix:"*" name then
              Type.Tuple (Type.Unbounded annotation)
            else
              annotation
          in
          let mutability =
            match mutability with
            | Immutable { original; scope; final } ->
                Annotation.Immutable
                  { original = Type.Variable.mark_all_variables_as_bound original; scope; final }
            | _ -> mutability
          in
          let reference =
            name |> String.filter ~f:(fun character -> character <> '*') |> Reference.create
          in
          state, Map.set annotations ~key:reference ~data:{ Annotation.annotation; mutability }
        in
        match parameters, parent with
        | [], Some _ when not (Define.is_class_toplevel define || Define.is_static_method define)
          ->
            let state =
              let name =
                if Define.is_class_method define || Define.is_class_property define then
                  "cls"
                else
                  "self"
              in
              emit_error
                ~state
                ~location
                ~kind:(Error.InvalidMethodSignature { annotation = None; name })
            in
            state, Resolution.annotations resolution
        | _ ->
            List.foldi
              ~init:(state, Resolution.annotations resolution)
              ~f:check_parameter
              parameters
      in
      let resolution = Resolution.with_annotations resolution ~annotations in
      let resolution_fixpoint =
        let precondition = Reference.Map.Tree.empty in
        let postcondition = Resolution.annotations resolution |> Reference.Map.to_tree in
        let key = [%hash: int * int] (Cfg.entry_index, 0) in
        Int.Map.Tree.set resolution_fixpoint ~key ~data:{ precondition; postcondition }
      in
      { state with resolution; resolution_fixpoint }
    in
    let check_base_annotations state =
      if Define.is_class_toplevel define then
        let open Annotated in
        let check_base state { Call.Argument.value; _ } =
          parse_and_check_annotation ~state value |> fst
        in
        let bases =
          Define.create define
          |> Define.parent_definition ~resolution
          >>| Class.bases
          |> Option.value ~default:[]
        in
        List.fold ~init:state ~f:check_base bases
      else
        state
    in
    let check_behavioral_subtyping ({ errors; _ } as state) =
      let errors =
        try
          if
            Define.is_constructor define
            || Define.is_class_method define
            || Define.is_dunder_method define
          then
            errors
          else
            let open Annotated in
            Define.create define
            |> Define.parent_definition ~resolution
            >>= (fun definition ->
                  Class.overrides
                    definition
                    ~resolution
                    ~name:(Statement.Define.unqualified_name define)
                  >>| fun overridden_attribute ->
                  let errors =
                    if Attribute.final overridden_attribute then
                      let parent = overridden_attribute |> Attribute.parent |> Type.show in
                      let error =
                        Error.create
                          ~location
                          ~kind:(Error.InvalidOverride { parent; decorator = Final })
                          ~define:Context.define
                      in
                      ErrorKey.add_error ~errors error
                    else
                      errors
                  in
                  let errors =
                    if
                      not
                        (Bool.equal
                           (Attribute.static overridden_attribute)
                           (Statement.Define.is_static_method define))
                    then
                      let parent = overridden_attribute |> Attribute.parent |> Type.show in
                      let decorator =
                        if Attribute.static overridden_attribute then
                          Error.StaticSuper
                        else
                          Error.StaticOverride
                      in
                      let error =
                        Error.create
                          ~location
                          ~kind:(Error.InvalidOverride { parent; decorator })
                          ~define:Context.define
                      in
                      ErrorKey.add_error ~errors error
                    else
                      errors
                  in
                  (* Check strengthening of postcondition. *)
                  match Annotation.annotation (Attribute.annotation overridden_attribute) with
                  | Type.Callable { Type.Callable.implementation; _ }
                    when not (Statement.Define.is_static_method define) ->
                      let original_implementation =
                        Reference.expression name
                        |> Resolution.resolve resolution
                        |> function
                        | Type.Callable
                            { Type.Callable.implementation = original_implementation; _ } ->
                            original_implementation
                        | annotation -> raise (TypeOrder.Untracked annotation)
                      in
                      let errors =
                        let expected = Type.Callable.Overload.return_annotation implementation in
                        let actual =
                          Type.Callable.Overload.return_annotation original_implementation
                        in
                        if
                          Type.Variable.all_variables_are_resolved expected
                          && not (Resolution.less_or_equal resolution ~left:actual ~right:expected)
                        then
                          let error =
                            Error.create
                              ~location
                              ~kind:
                                (Error.InconsistentOverride
                                   { overridden_method = Statement.Define.unqualified_name define;
                                     parent =
                                       Attribute.parent overridden_attribute
                                       |> Type.show
                                       |> Reference.create;
                                     override_kind = Method;
                                     override =
                                       Error.WeakenedPostcondition
                                         (Error.create_mismatch
                                            ~resolution
                                            ~actual
                                            ~actual_expression:None
                                            ~expected
                                            ~covariant:false)
                                   })
                              ~define:Context.define
                          in
                          ErrorKey.add_error ~errors error
                        else
                          errors
                      in
                      (* Check weakening of precondition. *)
                      let overriding_parameters =
                        Method.create ~define ~parent:(Annotated.Class.annotation definition)
                        |> Method.parameter_annotations ~resolution
                        |> List.map ~f:(fun (name, annotation) -> name, annotation, false)
                        |> Type.Callable.Parameter.create
                      in
                      let check_parameter errors overridden_parameter =
                        let expected = Type.Callable.Parameter.annotation overridden_parameter in
                        let found =
                          match overridden_parameter with
                          | Type.Callable.Parameter.Anonymous { index; _ } ->
                              List.nth overriding_parameters index
                          | KeywordOnly { name = overridden_name; _ }
                          | Named { name = overridden_name; _ } ->
                              (* TODO(T44178876): ensure index match as well for named parameters *)
                              let equal_name = function
                                | Type.Callable.Parameter.KeywordOnly { name; _ }
                                | Type.Callable.Parameter.Named { name; _ } ->
                                    Identifier.equal
                                      (Identifier.remove_leading_underscores name)
                                      (Identifier.remove_leading_underscores overridden_name)
                                | _ -> false
                              in
                              List.find overriding_parameters ~f:equal_name
                          | Variable _ ->
                              let find_variable_parameter = function
                                | Type.Callable.Parameter.Variable _ -> true
                                | _ -> false
                              in
                              List.find overriding_parameters ~f:find_variable_parameter
                          | Keywords _ ->
                              let find_variable_parameter = function
                                | Type.Callable.Parameter.Keywords _ -> true
                                | _ -> false
                              in
                              List.find overriding_parameters ~f:find_variable_parameter
                        in
                        match found with
                        | Some actual -> (
                            let is_compatible =
                              let expected = Type.Variable.mark_all_variables_as_bound expected in
                              Resolution.constraints_solution_exists
                                resolution
                                ~left:expected
                                ~right:(Type.Callable.Parameter.annotation actual)
                            in
                            try
                              if (not (Type.is_top expected)) && not is_compatible then
                                let error =
                                  Error.create
                                    ~location
                                    ~kind:
                                      (Error.InconsistentOverride
                                         { overridden_method =
                                             Statement.Define.unqualified_name define;
                                           parent =
                                             Attribute.parent overridden_attribute
                                             |> Type.show
                                             |> Reference.create;
                                           override_kind = Method;
                                           override =
                                             Error.StrengthenedPrecondition
                                               (Error.Found
                                                  (Error.create_mismatch
                                                     ~resolution
                                                     ~actual:
                                                       (Type.Callable.Parameter.annotation actual)
                                                     ~actual_expression:None
                                                     ~expected
                                                     ~covariant:false))
                                         })
                                    ~define:Context.define
                                in
                                ErrorKey.add_error ~errors error
                              else
                                errors
                            with
                            | TypeOrder.Untracked _ ->
                                (* TODO(T27409168): Error here. *)
                                errors )
                        | None ->
                            let has_keyword_and_anonymous_starred_parameters =
                              List.exists overriding_parameters ~f:(function
                                  | Keywords _ -> true
                                  | _ -> false)
                              && List.exists overriding_parameters ~f:(function
                                     | Variable _ -> true
                                     | _ -> false)
                            in
                            if has_keyword_and_anonymous_starred_parameters then
                              errors
                            else
                              let error =
                                Error.create
                                  ~location
                                  ~kind:
                                    (Error.InconsistentOverride
                                       { overridden_method =
                                           Statement.Define.unqualified_name define;
                                         override_kind = Method;
                                         parent =
                                           Attribute.parent overridden_attribute
                                           |> Type.show
                                           |> Reference.create;
                                         override =
                                           Error.StrengthenedPrecondition
                                             (Error.NotFound overridden_parameter)
                                       })
                                  ~define:Context.define
                              in
                              ErrorKey.add_error ~errors error
                      in
                      Type.Callable.Overload.parameters implementation
                      |> Option.value ~default:[]
                      |> List.fold ~init:errors ~f:check_parameter
                  | _ -> errors)
            |> Option.value ~default:errors
        with
        | TypeOrder.Untracked _ -> errors
      in
      { state with errors }
    in
    let check_constructor_return state =
      if not (Statement.Define.is_constructor define) then
        state
      else
        match return_annotation with
        | Some ({ Node.location; _ } as annotation) -> (
          match define with
          | { Statement.Define.signature = { Statement.Define.name; _ }; _ }
            when String.equal (Reference.last name) "__new__" ->
              (* TODO(T45018328): Error here. `__new__` is a special undecorated class method, and
                 we really ought to be checking its return type against typing.Type[Cls]. *)
              state
          | _ ->
              let annotation = Resolution.parse_annotation resolution annotation in
              if Type.is_none annotation then
                state
              else
                emit_error
                  ~state
                  ~location
                  ~kind:(Error.IncompatibleConstructorAnnotation annotation) )
        | _ -> state
    in
    create ~resolution:(Resolution.with_parent resolution ~parent) ()
    |> check_decorators
    |> check_return_annotation
    |> check_parameter_annotations
    |> check_base_annotations
    |> check_behavioral_subtyping
    |> check_constructor_return


  and forward_expression ~state:({ resolution; _ } as state)
                         ~expression:{ Node.location; value } =
    let rec forward_entry ~state ~entry:{ Dictionary.key; value } =
      let { state; resolved = key_resolved } = forward_expression ~state ~expression:key in
      let { state; resolved = value_resolved } = forward_expression ~state ~expression:value in
      Type.weaken_literals key_resolved, Type.weaken_literals value_resolved, state
    in
    let forward_generator
        ~state
        ~generator:{ Comprehension.target;
                     iterator = { Node.location; _ } as iterator;
                     conditions;
                     async
                   }
      =
      (* Propagate the target type information. *)
      let iterator =
        let value =
          if async then
            let aiter =
              { Node.location;
                value =
                  Call
                    { callee =
                        { Node.location;
                          value =
                            Name
                              (Name.Attribute
                                 { base = iterator; attribute = "__aiter__"; special = true })
                        };
                      arguments = []
                    }
              }
            in
            { Node.location;
              value =
                Call
                  { callee =
                      { Node.location;
                        value =
                          Name
                            (Name.Attribute
                               { base = aiter; attribute = "__anext__"; special = true })
                      };
                    arguments = []
                  }
            }
            |> fun target -> Node.create ~location (Await target)
          else
            let iter =
              { Node.location;
                value =
                  Call
                    { callee =
                        { Node.location;
                          value =
                            Name
                              (Name.Attribute
                                 { base = iterator; attribute = "__iter__"; special = true })
                        };
                      arguments = []
                    }
              }
            in
            { Node.location;
              value =
                Call
                  { callee =
                      { Node.location;
                        value =
                          Name
                            (Name.Attribute { base = iter; attribute = "__next__"; special = true })
                      };
                    arguments = []
                  }
            }
        in
        Assign { Assign.target; annotation = None; value; parent = None } |> Node.create ~location
      in
      let state =
        let { errors; _ } = state in
        let ({ errors = iterator_errors; _ } as state) =
          forward_statement ~state:{ state with errors = ErrorKey.Map.empty } ~statement:iterator
        in
        (* Don't throw Incompatible Variable errors on the generated iterator assign; we are
           temporarily minting a variable in a new scope and old annotations should be ignored. *)
        let errors =
          let is_not_assignment_error = function
            | { Error.kind = Error.IncompatibleVariableType _; _ } -> false
            | _ -> true
          in
          Map.filter ~f:is_not_assignment_error iterator_errors
          |> fun iterator_errors ->
          Map.merge_skewed
            ~combine:(fun ~key:_ left right -> Error.join ~resolution left right)
            iterator_errors
            errors
        in
        { state with errors }
      in
      List.map conditions ~f:Statement.assume
      |> List.fold ~init:state ~f:(fun state statement -> forward_statement ~state ~statement)
    in
    let forward_comprehension ~element ~generators =
      let { state; resolved } =
        List.fold
          generators
          ~f:(fun state generator -> forward_generator ~state ~generator)
          ~init:state
        |> fun state -> forward_expression ~state ~expression:element
      in
      (* Discard generator-local variables. *)
      { state = { state with resolution }; resolved = Type.weaken_literals resolved }
    in
    let forward_elements ~state ~elements =
      let forward_element { state = { resolution; _ } as state; resolved } expression =
        match Node.value expression with
        | Expression.Starred (Expression.Starred.Once expression) ->
            let { state; resolved = new_resolved } = forward_expression ~state ~expression in
            let parameter =
              match Resolution.join resolution new_resolved (Type.iterable Type.Bottom) with
              | Type.Parametric { parameters = [parameter]; _ } -> parameter
              | _ -> Type.Any
            in
            { state; resolved = Resolution.join resolution resolved parameter }
        | _ ->
            let { state; resolved = new_resolved } = forward_expression ~state ~expression in
            { state; resolved = Resolution.join resolution resolved new_resolved }
      in
      let correct_bottom { state; resolved } =
        let resolved =
          if Type.is_unbound resolved then
            Type.variable "_T" |> Type.Variable.mark_all_free_variables_as_escaped
          else
            resolved
        in
        { state; resolved }
      in
      List.fold elements ~init:{ state; resolved = Type.Bottom } ~f:forward_element
      |> (fun { state; resolved } -> { state; resolved = Type.weaken_literals resolved })
      |> correct_bottom
    in
    let forward_reference ~state reference =
      let reference = Resolution.resolve_exports resolution ~reference in
      let annotation =
        let local_annotation = Resolution.get_local resolution ~reference in
        match local_annotation, Reference.prefix reference with
        | Some annotation, _ -> Some annotation
        | None, Some qualifier -> (
            (* Fallback to use a __getattr__ callable as defined by PEP 484. *)
            let getattr =
              Resolution.get_local
                resolution
                ~reference:(Reference.create ~prefix:qualifier "__getattr__")
              >>| Annotation.annotation
            in
            let correct_getattr_arity signature =
              Type.Callable.Overload.parameters signature
              >>| (fun parameters -> List.length parameters == 1)
              |> Option.value ~default:false
            in
            match getattr with
            | Some (Callable { overloads = [signature]; _ })
            | Some (Callable { implementation = signature; _ })
              when correct_getattr_arity signature ->
                Some
                  (Annotation.create_immutable
                     ~global:true
                     ~original:(Some Type.Top)
                     (Type.Callable.Overload.return_annotation signature))
            | _ -> None )
        | _ -> None
      in
      match annotation with
      | Some annotation when Type.is_undeclared (Annotation.annotation annotation) ->
          let state =
            Error.UndefinedName reference |> fun kind -> emit_error ~state ~location ~kind
          in
          { state; resolved = Annotation.annotation annotation }
      | Some annotation -> { state; resolved = Annotation.annotation annotation }
      | None -> (
        match Resolution.module_definition resolution reference with
        | None when not (Resolution.is_suppressed_module resolution reference) ->
            let state =
              match Reference.prefix reference with
              | Some qualifier when not (Reference.is_empty qualifier) ->
                  if Option.is_some (Resolution.module_definition resolution qualifier) then
                    Error.UndefinedAttribute
                      { attribute = Reference.last reference; origin = Error.Module qualifier }
                    |> (fun kind -> Error.create ~location ~kind ~define:Context.define)
                    |> emit_raw_error ~state
                  else
                    state
              | _ ->
                  Error.create
                    ~location
                    ~kind:(Error.UndefinedName reference)
                    ~define:Context.define
                  |> emit_raw_error ~state
            in
            { state; resolved = Type.Top }
        | _ -> { state; resolved = Type.Top } )
    in
    let forward_callable ~state ~callee ~resolved ~arguments =
      let state =
        let forward_argument state { Call.Argument.value; _ } =
          forward_expression ~state ~expression:value |> fun { state; _ } -> state
        in
        List.fold arguments ~f:forward_argument ~init:state
      in
      let find_method ~parent ~name =
        parent
        |> Resolution.class_definition resolution
        >>| Annotated.Class.create
        >>| Annotated.Class.attribute ~resolution ~name ~instantiated:parent ~transitive:true
        >>= fun attribute ->
        Option.some_if (Annotated.Attribute.defined attribute) attribute
        >>| Annotated.Attribute.annotation
        >>| Annotation.annotation
        >>= function
        | Type.Callable callable -> Some callable
        | _ -> None
      in
      let signatures =
        let callables =
          let callable = function
            | meta when Type.is_meta meta -> (
                let backup = find_method ~parent:meta ~name:"__call__" in
                match Type.single_parameter meta with
                | TypedDictionary { name; fields; total } ->
                    Type.TypedDictionary.constructor ~name ~fields ~total |> Option.some
                | Variable { constraints = Type.Variable.Unary.Unconstrained; _ } -> backup
                | Variable { constraints = Type.Variable.Unary.Explicit constraints; _ }
                  when List.length constraints > 1 ->
                    backup
                | Any -> backup
                | meta_parameter -> (
                    let parent =
                      match meta_parameter with
                      | Variable { constraints = Type.Variable.Unary.Explicit [parent]; _ } ->
                          parent
                      | Variable { constraints = Type.Variable.Unary.Bound parent; _ } -> parent
                      | _ -> meta_parameter
                    in
                    Resolution.class_definition resolution parent
                    >>| Annotated.Class.create
                    >>| Annotated.Class.constructor ~instantiated:meta_parameter ~resolution
                    >>= function
                    | Type.Callable callable -> Some callable
                    | _ -> None ) )
            | Type.Callable callable -> Some callable
            | resolved -> find_method ~parent:resolved ~name:"__call__"
          in
          match resolved with
          | Type.Union annotations -> List.map annotations ~f:callable |> Option.all
          | annotation -> callable annotation >>| fun callable -> [callable]
        in
        let signature callable =
          let signature = Annotated.Signature.select ~arguments ~resolution ~callable in
          match signature with
          | Annotated.Signature.NotFound _ -> (
            match Node.value callee, callable, arguments with
            | ( Name (Name.Attribute { base; _ }),
                { Type.Callable.kind = Type.Callable.Named name; _ },
                [{ Call.Argument.value; _ }] ) ->
                let backup = function
                  (* cf. https://docs.python.org/3/reference/datamodel.html#object.__radd__ *)
                  | "__add__" -> Some "__radd__"
                  | "__sub__" -> Some "__rsub__"
                  | "__mul__" -> Some "__rmul__"
                  | "__matmul__" -> Some "__rmatmul__"
                  | "__truediv__" -> Some "__rtruediv__"
                  | "__floordiv__" -> Some "__rfloordiv__"
                  | "__mod__" -> Some "__rmod__"
                  | "__divmod__" -> Some "__rdivmod__"
                  | "__pow__" -> Some "__rpow__"
                  | "__lshift__" -> Some "__rlshift__"
                  | "__rshift__" -> Some "__rrshift__"
                  | "__and__" -> Some "__rand__"
                  | "__xor__" -> Some "__rxor__"
                  | "__or__" -> Some "__ror__"
                  | _ -> None
                in
                let backup_name = backup (Reference.last name) in
                let arguments = [{ Call.Argument.value = base; name = None }] in
                backup_name
                >>= (fun name -> find_method ~parent:(Resolution.resolve resolution value) ~name)
                >>| (fun callable -> Annotated.Signature.select ~arguments ~resolution ~callable)
                |> Option.value ~default:signature
            | _ -> signature )
          | Annotated.Signature.Found
              ({ kind = Type.Callable.Named access; implementation; _ } as callable)
            when String.equal "__init__" (Reference.last access) ->
              let definition = Resolution.class_definition resolution implementation.annotation in
              let gather_abstract_methods sofar { Node.value = class_definition; _ } =
                let abstract_methods, base_methods =
                  class_definition
                  |> Statement.Class.defines
                  |> List.partition_tf ~f:Statement.Define.is_abstract_method
                in
                let sofar =
                  if Statement.Class.is_abstract class_definition then
                    abstract_methods
                    |> List.filter ~f:(fun method_definition ->
                           not (Statement.Define.is_property method_definition))
                    |> List.map ~f:Statement.Define.unqualified_name
                    |> List.fold ~init:sofar ~f:Set.add
                  else
                    sofar
                in
                base_methods
                |> List.map ~f:Statement.Define.unqualified_name
                |> List.fold ~init:sofar ~f:Set.remove
              in
              definition
              >>| (fun definition ->
                    let abstract_methods =
                      definition
                      |> Annotated.Class.create
                      |> Annotated.Class.successors ~resolution
                      |> List.filter_map ~f:(fun name ->
                             Resolution.class_definition resolution (Type.Primitive name))
                      |> List.cons definition
                      |> List.rev
                      |> List.fold ~init:String.Set.empty ~f:gather_abstract_methods
                    in
                    if Set.is_empty abstract_methods then
                      signature
                    else
                      Annotated.Signature.NotFound
                        { callable;
                          reason =
                            Some
                              (AbstractClassInstantiation
                                 { method_names = Set.to_list abstract_methods;
                                   class_name =
                                     (definition |> fun { Node.value = { name; _ }; _ } -> name)
                                 })
                        })
              |> Option.value ~default:signature
          | _ -> signature
        in
        callables >>| List.map ~f:signature
      in
      let signature =
        let not_found = function
          | Annotated.Signature.NotFound _ -> true
          | _ -> false
        in
        match signatures >>| List.partition_tf ~f:not_found with
        (* Prioritize missing signatures for union type checking. *)
        | Some (not_found :: _, _) -> Some not_found
        | Some ([], Annotated.Signature.Found callable :: found) ->
            let callables =
              let extract = function
                | Annotated.Signature.Found callable -> callable
                | _ -> failwith "Not all signatures were found."
              in
              List.map found ~f:extract
            in
            let signature =
              let joined_callable =
                List.map callables ~f:(fun callable -> Type.Callable callable)
                |> List.fold ~init:(Type.Callable callable) ~f:(Resolution.join resolution)
              in
              match joined_callable with
              | Type.Callable callable -> Annotated.Signature.Found callable
              | _ -> Annotated.Signature.NotFound { callable; reason = None }
            in
            Some signature
        | _ -> None
      in
      match signature with
      | Some (Annotated.Signature.Found { implementation = { annotation; _ }; _ }) ->
          { state; resolved = annotation }
      | Some
          (Annotated.Signature.NotFound
            { callable = { implementation = { annotation; _ }; kind; implicit; _ } as callable;
              reason = Some reason
            }) ->
          let state =
            let open Annotated.Signature in
            let error =
              let callee =
                match kind with
                | Type.Callable.Named callable -> Some callable
                | _ -> None
              in
              match reason with
              | InvalidKeywordArgument { Node.location; value = { expression; annotation } } ->
                  let kind = Error.InvalidArgument (Error.Keyword { expression; annotation }) in
                  Error.create ~location ~kind ~define:Context.define
              | InvalidVariableArgument { Node.location; value = { expression; annotation } } ->
                  let kind = Error.InvalidArgument (Error.Variable { expression; annotation }) in
                  Error.create ~location ~kind ~define:Context.define
              | Mismatch mismatch ->
                  let { Annotated.Signature.actual; actual_expression; expected; name; position } =
                    Node.value mismatch
                  in
                  let mismatch, name, position, location =
                    ( Error.create_mismatch
                        ~resolution
                        ~actual
                        ~actual_expression:(Some actual_expression)
                        ~expected
                        ~covariant:true,
                      name,
                      position,
                      Node.location mismatch )
                  in
                  let kind =
                    let normal =
                      Error.IncompatibleParameterType { name; position; callee; mismatch }
                    in
                    match implicit, callee >>| Reference.as_list with
                    | ( Some
                          { implicit_annotation = Type.TypedDictionary { fields; name; total }; _ },
                        Some [_; method_name] ) ->
                        if Type.TypedDictionary.is_special_mismatch ~method_name ~position ~total
                        then
                          match actual with
                          | Type.Literal (Type.String missing_key) ->
                              Error.TypedDictionaryKeyNotFound
                                { typed_dictionary_name = name; missing_key }
                          | Type.Primitive "str" ->
                              Error.TypedDictionaryAccessWithNonLiteral
                                (List.map fields ~f:(fun { name; _ } -> name))
                          | _ -> normal
                        else
                          normal
                    | _ -> normal
                  in
                  Error.create ~location ~kind ~define:Context.define
              | MissingArgument parameter ->
                  Error.create
                    ~location
                    ~kind:(Error.MissingArgument { callee; parameter })
                    ~define:Context.define
              | MutuallyRecursiveTypeVariables ->
                  Error.create
                    ~location
                    ~kind:(Error.MutuallyRecursiveTypeVariables callee)
                    ~define:Context.define
              | TooManyArguments { expected; provided } ->
                  Error.create
                    ~location
                    ~kind:(Error.TooManyArguments { callee; expected; provided })
                    ~define:Context.define
              | UnexpectedKeyword name ->
                  Error.create
                    ~location
                    ~kind:(Error.UnexpectedKeyword { callee; name })
                    ~define:Context.define
              | AbstractClassInstantiation { class_name; method_names } ->
                  Error.create
                    ~location
                    ~kind:(Error.AbstractClassInstantiation { class_name; method_names })
                    ~define:Context.define
              | CallingParameterVariadicTypeVariable ->
                  Error.create
                    ~location
                    ~kind:(Error.NotCallable (Type.Callable callable))
                    ~define:Context.define
            in
            emit_raw_error ~state error
          in
          { state; resolved = annotation }
      | _ ->
          let state =
            if Type.equal Type.Any resolved || Type.equal Type.Top resolved then
              state
            else
              Error.NotCallable resolved
              |> (fun kind -> Error.create ~location ~kind ~define:Context.define)
              |> emit_raw_error ~state
          in
          { state; resolved = Type.Top }
    in
    let join_resolved ~resolution left right =
      { state = join left.state right.state;
        resolved = Resolution.join resolution left.resolved right.resolved
      }
    in
    let is_terminating_error error =
      let open Error in
      match kind error with
      | UndefinedAttribute _
      | UndefinedName _ ->
          true
      | _ -> false
    in
    match value with
    | Await expression ->
        let { state; resolved } = forward_expression ~state ~expression in
        let state =
          let is_awaitable =
            Resolution.less_or_equal resolution ~left:resolved ~right:(Type.awaitable Type.Top)
          in
          if not is_awaitable then
            emit_error ~state ~location ~kind:(Error.IncompatibleAwaitableType resolved)
          else
            state
        in
        let resolved =
          Resolution.join resolution (Type.awaitable Type.Bottom) resolved |> Type.awaitable_value
        in
        { state; resolved }
    | BooleanOperator { BooleanOperator.left; operator; right } ->
        let assume =
          let assume =
            match operator with
            | BooleanOperator.And -> left
            | BooleanOperator.Or -> Expression.normalize (Expression.negate left)
          in
          Statement.assume assume
        in
        let { state = state_left; resolved = resolved_left } =
          forward_expression ~state ~expression:left
        in
        let { state = state_right; resolved = resolved_right } =
          forward_expression ~state:(forward_statement ~state ~statement:assume) ~expression:right
        in
        let resolved =
          match resolved_left, resolved_right, operator with
          | Optional resolved_left, resolved_right, BooleanOperator.Or ->
              Resolution.join resolution resolved_left resolved_right
          (* Zero is also falsy. *)
          | Optional integer, resolved_right, BooleanOperator.And
            when Type.equal integer Type.integer ->
              Type.optional (Resolution.join resolution (Type.literal_integer 0) resolved_right)
          | Optional _, resolved_right, BooleanOperator.And -> Type.optional resolved_right
          | resolved_left, resolved_right, _ ->
              Resolution.join resolution resolved_left resolved_right
        in
        { state = join state_left state_right; resolved }
    | Call { callee = { Node.value = Name (Name.Identifier "super"); _ } as callee; arguments }
      -> (
        (* Resolve `super()` calls. *)
        let superclass =
          Resolution.parent resolution
          >>| (fun parent -> Type.Primitive (Reference.show parent))
          >>= Resolution.class_metadata resolution
          >>| (fun { Resolution.successors; _ } -> successors)
          >>| List.filter ~f:(fun name ->
                  Option.is_some (Resolution.class_definition resolution (Type.Primitive name)))
          >>= List.hd
        in
        match superclass with
        | Some superclass -> { state; resolved = Type.Primitive superclass }
        | None ->
            let { resolved; _ } = forward_expression ~state ~expression:callee in
            forward_callable ~state ~callee ~resolved ~arguments )
    | Call
        { callee = { Node.value = Name (Name.Identifier "type"); _ };
          arguments = [{ Call.Argument.value; _ }]
        } ->
        (* Resolve `type()` calls. *)
        let resolved = Resolution.resolve resolution value |> Type.meta in
        { state; resolved }
    | Call
        { callee = { Node.value = Name (Name.Identifier (("abs" | "repr" | "str") as name)); _ };
          arguments = [{ Call.Argument.value; _ }]
        } ->
        (* Resolve function redirects. *)
        Call
          { callee =
              { Node.location;
                value =
                  Name
                    (Name.Attribute
                       { base = value; attribute = "__" ^ name ^ "__"; special = true })
              };
            arguments = []
          }
        |> Node.create ~location
        |> fun expression -> forward_expression ~state ~expression
    | Call
        { callee = { Node.location; value = Name (Name.Identifier "reveal_type") };
          arguments = [{ Call.Argument.value; _ }]
        } ->
        (* Special case reveal_type(). *)
        let { state; resolved = annotation } = forward_expression ~state ~expression:value in
        let state =
          emit_error ~state ~location ~kind:(Error.RevealedType { expression = value; annotation })
        in
        { state; resolved = Type.none }
    | Call
        { callee =
            { Node.location;
              value =
                Name
                  (Name.Attribute
                    { base = { Node.value = Name (Name.Identifier "typing"); _ };
                      attribute = "cast"
                    ; _
                    })
            };
          arguments = [{ Call.Argument.value = cast_annotation; _ }; { Call.Argument.value; _ }]
        } ->
        let contains_literal_any = Type.expression_contains_any cast_annotation in
        let state, cast_annotation = parse_and_check_annotation ~state cast_annotation in
        let { state; resolved; _ } = forward_expression ~state ~expression:value in
        let state =
          if contains_literal_any then
            emit_error
              ~state
              ~location
              ~kind:
                (Error.ProhibitedAny
                   { Error.name = Reference.create "typing.cast";
                     annotation = None;
                     given_annotation = Some cast_annotation;
                     evidence_locations = [];
                     thrown_at_source = true
                   })
          else if Type.equal cast_annotation resolved then
            emit_error ~state ~location ~kind:(Error.RedundantCast resolved)
          else
            state
        in
        { state; resolved = cast_annotation }
    | Call
        { callee = { Node.value = Name (Name.Identifier "isinstance"); _ };
          arguments =
            [{ Call.Argument.value = expression; _ }; { Call.Argument.value = annotations; _ }]
        } ->
        (* We special case type inference for `isinstance` in asserted, and the typeshed stubs are
           imprecise (doesn't correctly declare the arguments as a recursive tuple. *)
        let state =
          let { state; _ } = forward_expression ~state ~expression in
          let previous_errors = Map.length state.errors in
          let state, annotations =
            let rec collect_types (state, collected) = function
              | { Node.value = Tuple annotations; _ } ->
                  let state, new_annotations =
                    List.fold annotations ~init:(state, []) ~f:collect_types
                  in
                  state, new_annotations @ collected
              | expression ->
                  let { state; resolved } = forward_expression ~state ~expression in
                  let new_annotations =
                    match resolved with
                    | Type.Tuple (Type.Bounded (Concrete annotations)) ->
                        List.map annotations ~f:(fun annotation ->
                            annotation, Node.location expression)
                    | Type.Tuple (Type.Unbounded annotation)
                    | annotation ->
                        [annotation, Node.location expression]
                  in
                  state, new_annotations @ collected
            in
            collect_types (state, []) annotations
          in
          if Map.length state.errors > previous_errors then
            state
          else
            let add_incompatible_non_meta_error state (non_meta, location) =
              emit_error
                ~state
                ~location
                ~kind:
                  (Error.IncompatibleParameterType
                     { name = None;
                       position = 2;
                       callee = Some (Reference.create "isinstance");
                       mismatch =
                         { Error.actual = non_meta;
                           actual_expressions = [];
                           expected = Type.meta Type.Any;
                           due_to_invariance = false
                         }
                     })
            in
            List.find annotations ~f:(fun (annotation, _) -> not (Type.is_meta annotation))
            >>| add_incompatible_non_meta_error state
            |> Option.value ~default:state
        in
        { state; resolved = Type.bool }
    | Call
        { callee =
            { Node.value =
                Name (Name.Attribute { attribute = "assertIsNotNone" | "assertTrue"; _ })
            ; _
            } as callee;
          arguments = [{ Call.Argument.value = expression; _ }] as arguments
        } ->
        let { resolution; _ } =
          forward_statement ~state ~statement:(Statement.assume expression)
        in
        let { state; resolved = resolved_callee } =
          forward_expression ~state:{ state with resolution } ~expression:callee
        in
        forward_callable ~state ~callee ~resolved:resolved_callee ~arguments
    | Call
        { callee =
            { Node.value = Name (Name.Attribute { attribute = "assertFalse"; _ }); _ } as callee;
          arguments = [{ Call.Argument.value = expression; _ }] as arguments
        } ->
        let { resolution; _ } =
          forward_statement ~state ~statement:(Statement.assume (Expression.negate expression))
        in
        let { state; resolved = resolved_callee } =
          forward_expression ~state:{ state with resolution } ~expression:callee
        in
        forward_callable ~state ~callee ~resolved:resolved_callee ~arguments
    | Call { callee; arguments } ->
        let { state = { errors = callee_errors; _ }; resolved = resolved_callee } =
          forward_expression ~state:{ state with errors = ErrorKey.Map.empty } ~expression:callee
        in
        let { state = { errors = updated_errors; _ } as updated_state; resolved } =
          forward_callable ~state ~callee ~resolved:resolved_callee ~arguments
        in
        if
          Map.is_empty (Map.filter ~f:is_terminating_error callee_errors)
          || not (Type.is_top resolved_callee || Type.is_undeclared resolved_callee)
        then
          let errors =
            Map.merge_skewed
              ~combine:(fun ~key:_ left right -> Error.join ~resolution left right)
              callee_errors
              updated_errors
          in
          { state = { updated_state with errors }; resolved }
        else (* Do not throw more errors if callee already contains terminating error. *)
          let errors =
            Map.fold callee_errors ~init:state.errors ~f:(fun ~key:_ ~data errors ->
                ErrorKey.add_error ~errors data)
          in
          { state = { state with errors }; resolved }
    | ComparisonOperator { ComparisonOperator.left; right; operator = ComparisonOperator.In }
    | ComparisonOperator { ComparisonOperator.left; right; operator = ComparisonOperator.NotIn } ->
        let { state; resolved = iterator } = forward_expression ~state ~expression:right in
        let modified_call =
          let rec has_method name annotation =
            match annotation with
            | Type.Union annotations -> List.for_all annotations ~f:(has_method name)
            | _ ->
                Resolution.class_definition resolution annotation
                >>| Annotated.Class.create
                >>| Annotated.Class.has_method ~transitive:true ~resolution ~name
                |> Option.value ~default:false
          in
          let { Node.location; _ } = left in
          if has_method "__contains__" iterator then
            { Node.location;
              value =
                Call
                  { callee =
                      { Node.location;
                        value =
                          Name
                            (Name.Attribute
                               { base = right; attribute = "__contains__"; special = true })
                      };
                    arguments = [{ Call.Argument.name = None; value = left }]
                  }
            }
          else if has_method "__iter__" iterator then
            let iter =
              { Node.location;
                value =
                  Call
                    { callee =
                        { Node.location;
                          value =
                            Name
                              (Name.Attribute
                                 { base = right; attribute = "__iter__"; special = true })
                        };
                      arguments = []
                    }
              }
            in
            let next =
              { Node.location;
                value =
                  Call
                    { callee =
                        { Node.location;
                          value =
                            Name
                              (Name.Attribute
                                 { base = iter; attribute = "__next__"; special = true })
                        };
                      arguments = []
                    }
              }
            in
            { Node.location;
              value =
                Call
                  { callee =
                      { Node.location;
                        value =
                          Name
                            (Name.Attribute { base = next; attribute = "__eq__"; special = true })
                      };
                    arguments = [{ Call.Argument.name = None; value = left }]
                  }
            }
          else
            let getitem =
              { Node.location;
                value =
                  Call
                    { callee =
                        { Node.location;
                          value =
                            Name
                              (Name.Attribute
                                 { base = right; attribute = "__getitem__"; special = true })
                        };
                      arguments =
                        [ { Call.Argument.name = None;
                            value = { Node.location; value = Expression.Integer 0 }
                          } ]
                    }
              }
            in
            { Node.location;
              value =
                Call
                  { callee =
                      { Node.location;
                        value =
                          Name
                            (Name.Attribute
                               { base = getitem; attribute = "__eq__"; special = true })
                      };
                    arguments = [{ Call.Argument.name = None; value = left }]
                  }
            }
        in
        forward_expression ~state ~expression:modified_call
    | ComparisonOperator ({ ComparisonOperator.left; right; _ } as operator) -> (
      match ComparisonOperator.override operator with
      | Some expression -> forward_expression ~state ~expression
      | None ->
          forward_expression ~state ~expression:left
          |> (fun { state; _ } -> forward_expression ~state ~expression:right)
          |> fun state -> { state with resolved = Type.bool } )
    | Complex _ -> { state; resolved = Type.complex }
    | Dictionary { Dictionary.entries; keywords } ->
        let key, value, state =
          let forward_entry (key, value, state) entry =
            let new_key, new_value, state = forward_entry ~state ~entry in
            ( Resolution.join resolution key new_key,
              Resolution.join resolution value new_value,
              state )
          in
          List.fold entries ~f:forward_entry ~init:(Type.Bottom, Type.Bottom, state)
        in
        let key =
          if List.is_empty keywords && Type.is_unbound key then
            Type.variable "_KT" |> Type.Variable.mark_all_free_variables_as_escaped
          else
            key
        in
        let value =
          if List.is_empty keywords && Type.is_unbound value then
            Type.variable "_VT" |> Type.Variable.mark_all_free_variables_as_escaped
          else
            value
        in
        let resolved, state =
          let forward_keyword (resolved, state) keyword =
            let { state; resolved = keyword_resolved } =
              forward_expression ~state ~expression:keyword
            in
            Resolution.join resolution resolved keyword_resolved, state
          in
          List.fold keywords ~f:forward_keyword ~init:(Type.dictionary ~key ~value, state)
        in
        { state; resolved }
    | DictionaryComprehension { Comprehension.element; generators } ->
        let key, value, state =
          List.fold
            generators
            ~f:(fun state generator -> forward_generator ~state ~generator)
            ~init:state
          |> fun state -> forward_entry ~state ~entry:element
        in
        (* Discard generator-local variables. *)
        { state = { state with resolution }; resolved = Type.dictionary ~key ~value }
    | Ellipsis -> { state; resolved = Type.Any }
    | False -> { state; resolved = Type.Literal (Type.Boolean false) }
    | Float _ -> { state; resolved = Type.float }
    | Generator { Comprehension.element; generators } ->
        let { state; resolved } = forward_comprehension ~element ~generators in
        { state; resolved = Type.generator resolved }
    | Integer literal -> { state; resolved = Type.literal_integer literal }
    | Lambda { Lambda.body; parameters } ->
        let resolution_with_parameters =
          let add_parameter resolution { Node.value = { Parameter.name; _ }; _ } =
            let name = String.chop_prefix name ~prefix:"*" |> Option.value ~default:name in
            Resolution.set_local
              resolution
              ~reference:(Reference.create name)
              ~annotation:(Annotation.create Type.Any)
          in
          List.fold ~f:add_parameter ~init:resolution parameters
        in
        let { state; resolved } =
          forward_expression
            ~state:{ state with resolution = resolution_with_parameters }
            ~expression:body
        in
        (* Judgement call, many more people want to pass in `lambda: 0` to `defaultdict` than want
           to write a function that take in callables with literal return types. If you really want
           that behavior you can always write a real inner function with a literal return type *)
        let resolved = Type.weaken_literals resolved in
        let create_parameter { Node.value = { Parameter.name; value; _ }; _ } =
          name, Type.Any, Option.is_some value
        in
        let parameters =
          List.map parameters ~f:create_parameter
          |> Type.Callable.Parameter.create
          |> fun parameters -> Type.Callable.Defined parameters
        in
        { state = { state with resolution };
          resolved = Type.Callable.create ~parameters ~annotation:resolved ()
        }
    | List elements ->
        let { state; resolved } = forward_elements ~state ~elements in
        { state; resolved = Type.list resolved }
    | ListComprehension { Comprehension.element; generators } ->
        let { state; resolved } = forward_comprehension ~element ~generators in
        { state; resolved = Type.list resolved }
    | Name (Name.Identifier identifier) -> forward_reference ~state (Reference.create identifier)
    | Name (Name.Attribute { base; attribute; special } as name) ->
        let reference = Reference.from_name name in
        let { state = { errors = base_errors; _ }; resolved = resolved_base } =
          forward_expression ~state:{ state with errors = ErrorKey.Map.empty } ~expression:base
        in
        let ({ errors; _ } as state), resolved_base =
          if Type.Variable.contains_escaped_free_variable resolved_base then
            let state =
              Error.IncompleteType
                { target = base;
                  annotation = resolved_base;
                  attempted_action = Error.AttributeAccess attribute
                }
              |> (fun kind -> Error.create ~location ~kind ~define:Context.define)
              |> emit_raw_error ~state
            in
            state, Type.Variable.convert_all_escaped_free_variables_to_anys resolved_base
          else
            state, resolved_base
        in
        let { state = { errors = updated_errors; _ } as updated_state; resolved } =
          if Type.is_undeclared resolved_base then
            let state =
              reference
              >>| (fun reference -> Error.UndefinedName reference)
              >>| (fun kind -> emit_error ~state ~location ~kind)
              |> Option.value ~default:state
            in
            { state; resolved = resolved_base }
          else if Type.equal resolved_base Type.Top then (* Global or local. *)
            reference
            >>| forward_reference ~state
            |> Option.value ~default:{ state; resolved = Type.Top }
          else if Type.is_callable resolved_base then (* Nested function. *)
            let resolved =
              reference >>= fun reference -> Resolution.get_local resolution ~reference
            in
            match resolved with
            | Some annotation -> { state; resolved = Annotation.annotation annotation }
            | None -> { state; resolved = Type.Top }
          else (* Attribute access. *)
            match Annotated.Class.resolve_class ~resolution resolved_base with
            | None ->
                let state =
                  Error.UndefinedAttribute
                    { attribute;
                      origin = Error.Class { annotation = resolved_base; class_attribute = false }
                    }
                  |> (fun kind -> Error.create ~location ~kind ~define:Context.define)
                  |> emit_raw_error ~state
                in
                { state; resolved = Type.Top }
            | Some [] -> { state; resolved = Type.Top }
            | Some (head :: tail) ->
                let name = attribute in
                let find_attribute
                    { Annotated.Class.instantiated; class_attributes; class_definition }
                  =
                  let attribute =
                    Annotated.Class.attribute
                      class_definition
                      ~transitive:true
                      ~class_attributes
                      ~special_method:special
                      ~resolution
                      ~name
                      ~instantiated
                  in
                  let attribute =
                    if not (Annotated.Attribute.defined attribute) then
                      Annotated.Class.fallback_attribute class_definition ~resolution ~name
                      |> Option.value ~default:attribute
                    else
                      attribute
                  in
                  let undefined_target =
                    if Annotated.Attribute.defined attribute then
                      None
                    else
                      Some instantiated
                  in
                  (attribute, undefined_target), Annotated.Attribute.annotation attribute
                in
                let head_definition, head_resolved = find_attribute head in
                let tail_definitions, tail_resolveds =
                  List.map ~f:find_attribute tail |> List.unzip
                in
                let state =
                  let definition =
                    List.find
                      (head_definition :: tail_definitions)
                      ~f:(fun (_, undefined_target) ->
                        match undefined_target with
                        | None -> false
                        | _ -> true)
                    |> Option.value ~default:head_definition
                  in
                  match reference, definition with
                  | Some reference, (_, Some target) when Type.equal Type.undeclared target ->
                      Error.UndefinedName reference
                      |> (fun kind -> Error.create ~location ~kind ~define:Context.define)
                      |> emit_raw_error ~state
                  | _, (attribute, Some target) ->
                      Error.UndefinedAttribute
                        { attribute = name;
                          origin =
                            Error.Class
                              { annotation = target;
                                class_attribute = Annotated.Attribute.class_attribute attribute
                              }
                        }
                      |> (fun kind -> Error.create ~location ~kind ~define:Context.define)
                      |> emit_raw_error ~state
                  | _ -> state
                in
                let resolved =
                  let apply_global_override resolved =
                    let annotation =
                      reference
                      >>= fun reference ->
                      Resolution.get_local
                        resolution
                        ~reference
                        ~global_fallback:(Type.is_meta (Annotation.annotation resolved))
                    in
                    match annotation with
                    | Some local -> local
                    | None -> resolved
                  in
                  let join sofar element =
                    let refined = Refinement.join ~resolution sofar element in
                    { refined with annotation = Type.union [sofar.annotation; element.annotation] }
                  in
                  List.fold tail_resolveds ~init:head_resolved ~f:join
                  |> apply_global_override
                  |> Annotation.annotation
                in
                { state; resolved }
        in
        if
          Map.is_empty (Map.filter ~f:is_terminating_error base_errors)
          || not (Type.is_top resolved_base || Type.is_undeclared resolved_base)
        then
          let errors =
            Map.fold base_errors ~init:updated_errors ~f:(fun ~key:_ ~data errors ->
                ErrorKey.add_error ~errors data)
          in
          { state = { updated_state with errors }; resolved }
        else (* Do not throw more errors if base already contains terminating error. *)
          let errors =
            Map.fold base_errors ~init:errors ~f:(fun ~key:_ ~data errors ->
                ErrorKey.add_error ~errors data)
          in
          { state = { state with errors }; resolved }
    | Set elements ->
        let { state; resolved } = forward_elements ~state ~elements in
        { state; resolved = Type.set resolved }
    | SetComprehension { Comprehension.element; generators } ->
        let { state; resolved } = forward_comprehension ~element ~generators in
        { state; resolved = Type.set resolved }
    | Starred starred ->
        let state =
          match starred with
          | Starred.Once expression
          | Starred.Twice expression ->
              forward_expression ~state ~expression
        in
        { state with resolved = Type.Top }
    | String { StringLiteral.kind = StringLiteral.Format expressions; _ } ->
        let state =
          List.fold
            expressions
            ~f:(fun state expression ->
              forward_expression ~state ~expression |> fun { state; _ } -> state)
            ~init:state
        in
        { state; resolved = Type.string }
    | String { StringLiteral.kind = StringLiteral.Bytes; _ } -> { state; resolved = Type.bytes }
    | String { StringLiteral.kind = StringLiteral.String; value } ->
        { state; resolved = Type.literal_string value }
    | String { StringLiteral.kind = StringLiteral.Mixed _; _ } ->
        (* NOTE: We may run into this case with nested f-strings. Treat them as literal strings
           until the parser gets full support of them. *)
        { state; resolved = Type.string }
    | Ternary { Ternary.target; test; alternative } ->
        let state = { state with resolution } in
        let target =
          forward_statement ~state ~statement:(Statement.assume test)
          |> fun state -> forward_expression ~state ~expression:target
        in
        let alternative =
          forward_statement ~state ~statement:(Statement.assume (Expression.negate test))
          |> fun state -> forward_expression ~state ~expression:alternative
        in
        let { state; resolved } = join_resolved ~resolution target alternative in
        (* The resolution is local to the ternary expression and should not be propagated out. *)
        { state = { state with resolution }; resolved }
    | True -> { state; resolved = Type.Literal (Type.Boolean true) }
    | Tuple elements ->
        let state, resolved =
          let forward_element (state, resolved) expression =
            let { state; resolved = new_resolved } = forward_expression ~state ~expression in
            state, new_resolved :: resolved
          in
          List.fold elements ~f:forward_element ~init:(state, [])
        in
        { state; resolved = Type.tuple (List.rev resolved) }
    | UnaryOperator ({ UnaryOperator.operand; _ } as operator) -> (
      match UnaryOperator.override operator with
      | Some expression -> forward_expression ~state ~expression
      | None ->
          let state = forward_expression ~state ~expression:operand in
          { state with resolved = Type.bool } )
    | Expression.Yield (Some expression) ->
        let { state; resolved } = forward_expression ~state ~expression in
        { state; resolved = Type.generator resolved }
    | Expression.Yield None -> { state; resolved = Type.generator Type.none }


  and forward_statement
      ~state:({ resolution; check_return; _ } as state)
      ~statement:{ Node.location; value }
    =
    let { Node.location = define_location;
          value =
            { Define.signature =
                { async;
                  parent = define_parent;
                  return_annotation = return_annotation_expression
                ; _
                };
              body
            } as define
        }
      =
      Context.define
    in
    let instantiate location =
      Location.instantiate ~lookup:(fun hash -> Ast.SharedMemory.Handles.get ~hash) location
    in
    (* We weaken type inference of mutable literals for assignments and returns to get around the
       invariance of containers when we can prove that casting to a supertype is safe. *)
    let validate_return ~expression ~state ~actual ~is_implicit =
      let return_annotation =
        let annotation = Annotated.Callable.return_annotation ~define ~resolution in
        if async then
          Type.coroutine_value annotation
        else
          annotation
      in
      let return_annotation = Type.Variable.mark_all_variables_as_bound return_annotation in
      let actual =
        Resolution.resolve_mutable_literals
          resolution
          ~expression
          ~resolved:actual
          ~expected:return_annotation
      in
      let check_incompatible_return state =
        if
          check_return
          && (not
                (Resolution.constraints_solution_exists
                   resolution
                   ~left:actual
                   ~right:return_annotation))
          && (not (Define.is_abstract_method define))
          && (not (Define.is_overloaded_method define))
          && (not (Type.is_none actual && Annotated.Callable.is_generator define))
          && not (Type.is_none actual && Type.is_noreturn return_annotation)
        then
          let rec check_unimplemented = function
            | [ { Node.value = Statement.Pass; _ }
              ; { Node.value = Statement.Return { Return.expression = None; _ }; _ } ] ->
                true
            | { Node.value = Statement.Expression { Node.value = Expression.String _; _ }; _ }
              :: tail ->
                check_unimplemented tail
            | _ -> false
          in
          emit_error
            ~state
            ~location
            ~kind:
              (Error.IncompatibleReturnType
                 { mismatch =
                     Error.create_mismatch
                       ~resolution
                       ~actual
                       ~actual_expression:expression
                       ~expected:return_annotation
                       ~covariant:true;
                   is_implicit;
                   is_unimplemented = check_unimplemented body;
                   define_location
                 })
        else
          state
      in
      let check_missing_return state =
        let contains_literal_any =
          return_annotation_expression
          >>| Type.expression_contains_any
          |> Option.value ~default:false
        in
        if
          (not (Define.has_return_annotation define))
          || contains_literal_any
             && not (Resolution.is_string_to_any_mapping resolution return_annotation)
        then
          let given_annotation =
            Option.some_if (Define.has_return_annotation define) return_annotation
          in
          emit_error
            ~state
            ~location:define_location
            ~kind:
              (Error.MissingReturnAnnotation
                 { name = Reference.create "$return_annotation";
                   annotation = Some actual;
                   given_annotation;
                   evidence_locations = [instantiate location];
                   thrown_at_source = true
                 })
        else
          state
      in
      state |> check_incompatible_return |> check_missing_return
    in
    match value with
    | Assign { Assign.target; annotation; value; parent } ->
        let state, original_annotation =
          annotation
          >>| parse_and_check_annotation ~state
          >>| (fun (state, annotation) -> state, Some annotation)
          |> Option.value ~default:(state, None)
        in
        let is_final = original_annotation >>| Type.is_final |> Option.value ~default:false in
        let original_annotation =
          original_annotation
          >>| fun annotation ->
          ( if Type.is_final annotation then
              Type.final_value annotation
          else
            Type.class_variable_value annotation )
          |> Option.value ~default:annotation
        in
        let parsed =
          Resolution.parse_annotation ~allow_invalid_type_parameters:true resolution value
        in
        let is_type_alias =
          (* Consider anything with a RHS that is a type to be an alias. *)
          match Node.value value with
          | Expression.String _ -> false
          | _ -> (
            match parsed with
            | Type.Top -> Option.is_some (Type.Variable.parse_declaration value)
            | Type.Optional Type.Bottom -> false
            | annotation -> not (Resolution.contains_untracked resolution annotation) )
        in
        let state, resolved =
          let { state = { resolution; _ } as new_state; resolved } =
            forward_expression ~state ~expression:value
          in
          let resolved = Type.remove_undeclared resolved in
          (* TODO(T35601774): We need to suppress subscript related errors on generic classes. *)
          if is_type_alias then
            let errors, _ =
              add_invalid_type_parameters_errors ~resolution ~location ~errors:state.errors parsed
            in
            let errors =
              match parsed with
              | Variable variable when Type.Variable.Unary.contains_subvariable variable ->
                  let kind =
                    AnalysisError.InvalidType
                      (AnalysisError.NestedTypeVariables (Type.Variable.Unary variable))
                  in
                  Error.create ~location ~kind ~define:Context.define |> ErrorKey.add_error ~errors
              | _ -> errors
            in
            { state with resolution; errors }, resolved
          else
            new_state, resolved
        in
        let guide =
          (* This is the annotation determining how we recursively break up the assignment. *)
          match original_annotation with
          | Some annotation when not (Type.is_unknown annotation) -> annotation
          | _ -> resolved
        in
        let explicit = Option.is_some annotation in
        let rec forward_assign
            ~state:({ resolution; _ } as state)
            ~target:({ Node.location; value = target_value } as target)
            ~guide
            ~resolved
            ~expression
          =
          let is_named_tuple annotation =
            Resolution.less_or_equal resolution ~left:annotation ~right:Type.named_tuple
          in
          let get_named_tuple_parameters annotation =
            let namedtuple_attribute_annotations attributes =
              let open Annotated.Class.Attribute in
              let filter_attribute { Node.value = { annotation; name; _ }; _ } =
                let fields =
                  let is_fields = function
                    | { Node.value = { name = "_fields"; _ }; _ } -> true
                    | _ -> false
                  in
                  match List.find ~f:is_fields attributes >>| Node.value with
                  | Some { value = { Node.value = Tuple fields; _ }; _ } -> fields
                  | _ -> []
                in
                let equals name field =
                  match Node.value field with
                  | String { StringLiteral.value; _ } -> String.equal name value
                  | _ -> false
                in
                if List.exists ~f:(equals name) fields then
                  Some (Annotation.annotation annotation)
                else
                  None
              in
              List.filter_map ~f:filter_attribute attributes
            in
            annotation
            |> Option.some_if (is_named_tuple annotation)
            >>= Resolution.class_definition resolution
            >>| Annotated.Class.create
            >>| Annotated.Class.attributes ~resolution
            >>| namedtuple_attribute_annotations
            |> Option.value ~default:[]
          in
          let is_uniform_sequence annotation =
            match annotation with
            | Type.Tuple (Type.Unbounded _) -> true
            (* Bounded tuples subclass iterable, but should be handled in the nonuniform case. *)
            | Type.Tuple (Type.Bounded _) -> false
            | _ ->
                Resolution.less_or_equal
                  resolution
                  ~left:annotation
                  ~right:(Type.iterable Type.Top)
          in
          let uniform_sequence_parameter annotation =
            match annotation with
            | Type.Tuple (Type.Unbounded parameter) -> parameter
            | _ -> (
                Resolution.join resolution annotation (Type.iterable Type.Bottom)
                |> function
                | Type.Parametric { parameters = [parameter]; _ } -> parameter
                | _ -> Type.Top )
          in
          let is_nonuniform_sequence ~minimum_length annotation =
            (* TODO(32692300): this should support tuple subclasses as well. *)
            match annotation with
            | Type.Tuple (Type.Bounded (Concrete parameters))
              when minimum_length <= List.length parameters ->
                true
            | annotation
              when is_named_tuple annotation
                   && minimum_length <= List.length (get_named_tuple_parameters annotation) ->
                true
            | _ -> false
          in
          let nonuniform_sequence_parameters annotation =
            match annotation with
            | Type.Tuple (Type.Bounded (Concrete parameters)) -> parameters
            | annotation when is_named_tuple annotation -> get_named_tuple_parameters annotation
            | _ -> []
          in
          match target_value with
          | Name name ->
              let reference, attribute, resolved_base =
                match name with
                | Name.Identifier identifier -> Some (Reference.create identifier), None, None
                | Name.Attribute { base; attribute; _ } ->
                    let resolved = Resolution.resolve resolution base in
                    let parent, class_attributes =
                      if Type.is_meta resolved then
                        Type.single_parameter resolved, true
                      else
                        resolved, false
                    in
                    let parent_class =
                      Resolution.class_definition resolution parent >>| Annotated.Class.create
                    in
                    let reference =
                      match base with
                      | { Node.value = Name name; _ } when Expression.is_simple_name name ->
                          Some (Reference.create ~prefix:(Reference.from_name_exn name) attribute)
                      | _ ->
                          parent_class
                          >>| Annotated.Class.name
                          >>| fun prefix -> Reference.create ~prefix attribute
                    in
                    let attribute =
                      parent_class
                      >>| Annotated.Class.attribute
                            ~resolution
                            ~name:attribute
                            ~instantiated:parent
                            ~transitive:true
                            ~class_attributes
                      >>| fun annotated -> annotated, attribute
                    in
                    reference, attribute, Some resolved
              in
              let target_annotation =
                let local =
                  Reference.from_name name
                  >>= fun reference -> Resolution.get_local resolution ~reference
                in
                match local, attribute with
                | Some annotation, _ -> annotation
                | None, Some ({ Node.value = { Annotated.Attribute.annotation; _ }; _ }, _) ->
                    annotation
                | _ ->
                    let { state = { errors; _ }; resolved } =
                      forward_expression
                        ~state:{ state with errors = ErrorKey.Map.empty }
                        ~expression:target
                    in
                    if Map.is_empty errors then
                      Annotation.create_immutable ~global:true resolved
                    else
                      Annotation.create Type.Top
              in
              let state =
                match reference with
                | Some reference ->
                    let check_global_final_reassignment state =
                      if Annotation.is_final target_annotation then
                        let kind = Error.InvalidAssignment (Final reference) in
                        emit_error ~state ~location ~kind
                      else
                        state
                    in
                    let check_class_final_reassignment state =
                      match attribute with
                      | Some ({ Node.value = { Annotated.Attribute.final = true; _ }; _ }, _)
                        when Option.is_none original_annotation ->
                          emit_error
                            ~state
                            ~location
                            ~kind:(Error.InvalidAssignment (Final reference))
                      | _ -> state
                    in
                    let check_assign_class_variable_on_instance state =
                      match resolved_base, attribute with
                      | ( Some parent,
                          Some
                            ( { Node.value =
                                  { Annotated.Attribute.class_attribute = true;
                                    name = class_variable
                                  ; _
                                  }
                              ; _
                              },
                              _ ) )
                        when Option.is_none original_annotation && not (Type.is_meta parent) ->
                          emit_error
                            ~state
                            ~location
                            ~kind:
                              (Error.InvalidAssignment
                                 (ClassVariable { class_name = Type.show parent; class_variable }))
                      | _ -> state
                    in
                    let check_final_is_outermost_qualifier state =
                      original_annotation
                      >>| (fun annotation ->
                            if Type.contains_final annotation then
                              emit_error
                                ~state
                                ~location
                                ~kind:(Error.InvalidType (FinalNested annotation))
                            else
                              state)
                      |> Option.value ~default:state
                    in
                    let check_is_readonly_property state =
                      match attribute with
                      | Some
                          ( { Node.value = { Annotated.Attribute.property = Some ReadOnly; _ }; _ },
                            _ )
                        when Option.is_none original_annotation ->
                          emit_error
                            ~state
                            ~location
                            ~kind:(Error.InvalidAssignment (ReadOnly reference))
                      | _ -> state
                    in
                    let check_undefined_attribute_target state =
                      match resolved_base, attribute with
                      | Some parent, Some (attribute, name)
                        when not (Annotated.Attribute.defined attribute) ->
                          emit_error
                            ~state
                            ~location
                            ~kind:
                              (Error.UndefinedAttribute
                                 { attribute = name;
                                   origin =
                                     Error.Class
                                       { annotation = parent;
                                         class_attribute = Type.is_meta resolved
                                       }
                                 })
                      | _ -> state
                    in
                    check_global_final_reassignment state
                    |> check_class_final_reassignment
                    |> check_assign_class_variable_on_instance
                    |> check_final_is_outermost_qualifier
                    |> check_is_readonly_property
                    |> check_undefined_attribute_target
                | _ -> state
              in
              let expected, is_immutable =
                match original_annotation, target_annotation with
                | Some original, _ -> original, true
                | _, target_annotation when Annotation.is_immutable target_annotation ->
                    Annotation.original target_annotation, true
                | _ -> Type.Top, false
              in
              let resolved =
                Resolution.resolve_mutable_literals resolution ~expression ~resolved ~expected
              in
              let is_typed_dictionary_initialization =
                (* Special-casing to avoid throwing errors *)
                let open Type in
                match expected with
                | Parametric { name = "type"; parameters = [parameter] }
                  when is_typed_dictionary parameter ->
                    is_unknown resolved
                | _ -> false
              in
              let state =
                let is_valid_enumeration_assignment =
                  let parent_annotation =
                    match parent with
                    | None -> Type.Top
                    | Some reference -> Type.Primitive (Reference.show reference)
                  in
                  let resolved = Type.weaken_literals resolved in
                  let compatible =
                    if explicit then
                      Resolution.less_or_equal resolution ~left:expected ~right:resolved
                    else
                      true
                  in
                  Resolution.less_or_equal
                    resolution
                    ~left:parent_annotation
                    ~right:Type.enumeration
                  && compatible
                in
                let is_incompatible =
                  let expression_is_ellipses =
                    match expression with
                    | Some { Node.value = Expression.Ellipsis; _ } -> true
                    | _ -> false
                  in
                  is_immutable
                  && (not expression_is_ellipses)
                  && (not
                        (Resolution.constraints_solution_exists
                           resolution
                           ~left:resolved
                           ~right:expected))
                  && (not is_typed_dictionary_initialization)
                  && not is_valid_enumeration_assignment
                in
                let open Annotated in
                match attribute, reference with
                | Some (attribute, name), _ when is_incompatible ->
                    Error.IncompatibleAttributeType
                      { parent = Attribute.parent attribute;
                        incompatible_type =
                          { Error.name = Reference.create name;
                            mismatch =
                              Error.create_mismatch
                                ~resolution
                                ~actual:resolved
                                ~actual_expression:expression
                                ~expected
                                ~covariant:true;
                            declare_location = instantiate (Attribute.location attribute)
                          }
                      }
                    |> fun kind -> emit_error ~state ~location ~kind
                | _, Some reference when is_incompatible ->
                    Error.IncompatibleVariableType
                      { Error.name = reference;
                        mismatch =
                          Error.create_mismatch
                            ~resolution
                            ~actual:resolved
                            ~actual_expression:expression
                            ~expected
                            ~covariant:true;
                        declare_location = instantiate location
                      }
                    |> fun kind -> emit_error ~state ~location ~kind
                | _ -> state
              in
              (* Check for missing annotations. *)
              let error =
                let insufficiently_annotated, thrown_at_source =
                  let is_reassignment =
                    (* Special-casing re-use of typed parameters as attributes *)
                    match name, Node.value value with
                    | ( Name.Attribute
                          { base = { Node.value = Name (Name.Identifier self); _ }; attribute; _ },
                        Name _ )
                      when Identifier.sanitized self = "self" ->
                        let sanitized = Expression.show_sanitized value in
                        is_immutable
                        && (not (Type.is_unknown expected))
                        && ( String.equal attribute sanitized
                           || String.equal attribute ("_" ^ sanitized) )
                    | _ -> false
                  in
                  match annotation with
                  | Some annotation when Type.expression_contains_any annotation ->
                      original_annotation
                      >>| Resolution.is_string_to_any_mapping resolution
                      |> Option.value ~default:false
                      |> not
                      |> fun insufficient -> insufficient, true
                  | None when is_immutable && not is_reassignment ->
                      let is_toplevel =
                        Define.is_toplevel define
                        || Define.is_class_toplevel define
                        || Define.is_constructor define
                      in
                      let contains_any annotation =
                        if Resolution.is_string_to_any_mapping resolution annotation then
                          false
                        else
                          Type.contains_any annotation
                      in
                      Type.equal expected Type.Top || contains_any expected, is_toplevel
                  | _ -> false, false
                in
                let actual_annotation, evidence_locations =
                  if Type.equal resolved Type.Top then
                    None, []
                  else
                    Some resolved, [instantiate location]
                in
                let is_illegal_attribute_annotation
                    { Node.value = { AnnotatedClass.Attribute.parent = attribute_parent; _ }; _ }
                  =
                  let parent_annotation =
                    match define_parent with
                    | None -> Type.Top
                    | Some reference -> Type.Primitive (Reference.show reference)
                  in
                  explicit && not (Type.equal parent_annotation attribute_parent)
                in
                let parent_class =
                  match name with
                  | Name.Attribute { base; _ } ->
                      Resolution.resolve resolution base
                      |> Annotated.Class.resolve_class ~resolution
                  | _ -> None
                in
                match name, parent_class with
                | Name.Identifier identifier, _ ->
                    let reference = Reference.create identifier in
                    if
                      Resolution.is_global ~reference resolution
                      && insufficiently_annotated
                      && not is_type_alias
                    then
                      let global_location =
                        Reference.delocalize reference
                        |> Resolution.global resolution
                        >>| Node.location
                        |> Option.value ~default:location
                      in
                      Error.create
                        ~location:global_location
                        ~kind:
                          (Error.MissingGlobalAnnotation
                             { Error.name = reference;
                               annotation = actual_annotation;
                               given_annotation = Option.some_if is_immutable expected;
                               evidence_locations;
                               thrown_at_source
                             })
                        ~define:Context.define
                      |> Option.some
                    else if explicit && insufficiently_annotated then
                      let value_annotation = Resolution.parse_annotation resolution value in
                      Error.create
                        ~location
                        ~kind:
                          (Error.ProhibitedAny
                             { Error.name = reference;
                               annotation = actual_annotation;
                               given_annotation = Option.some_if is_immutable expected;
                               evidence_locations;
                               thrown_at_source = true
                             })
                        ~define:Context.define
                      |> Option.some_if
                           (not (Resolution.is_string_to_any_mapping resolution value_annotation))
                    else if is_type_alias && Type.expression_contains_any value then
                      let value_annotation = Resolution.parse_annotation resolution value in
                      Error.create
                        ~location
                        ~kind:
                          (Error.ProhibitedAny
                             { Error.name = reference;
                               annotation = None;
                               given_annotation = Some value_annotation;
                               evidence_locations;
                               thrown_at_source = true
                             })
                        ~define:Context.define
                      |> Option.some_if
                           (not (Resolution.is_string_to_any_mapping resolution value_annotation))
                    else
                      None
                | Name.Attribute { base = { Node.value = Name base; _ }; attribute; _ }, None
                  when Expression.is_simple_name base && insufficiently_annotated ->
                    (* Module *)
                    let reference = Reference.from_name_exn base in
                    let definition = Resolution.module_definition resolution reference in
                    if Option.is_some definition then
                      Error.create
                        ~location
                        ~kind:
                          (Error.MissingGlobalAnnotation
                             { Error.name = Reference.create ~prefix:reference attribute;
                               annotation = actual_annotation;
                               given_annotation = Option.some_if is_immutable expected;
                               evidence_locations;
                               thrown_at_source = true
                             })
                        ~define:Context.define
                      |> Option.some
                    else if explicit && not is_type_alias then
                      Error.create
                        ~location
                        ~kind:
                          (Error.ProhibitedAny
                             { Error.name = Reference.create ~prefix:reference attribute;
                               annotation = actual_annotation;
                               given_annotation = Option.some_if is_immutable expected;
                               evidence_locations;
                               thrown_at_source = true
                             })
                        ~define:Context.define
                      |> Option.some
                    else
                      None
                | ( Name.Attribute { attribute; _ },
                    Some
                      ({ Annotated.Class.instantiated; class_attributes; class_definition } :: _) )
                  ->
                    (* Instance *)
                    let reference = Reference.create attribute in
                    let attribute =
                      Annotated.Class.attribute
                        ~resolution
                        ~name:attribute
                        ~instantiated
                        ~class_attributes
                        ~transitive:true
                        class_definition
                    in
                    if is_illegal_attribute_annotation attribute then
                      (* Non-self attributes may not be annotated. *)
                      Error.create
                        ~location
                        ~kind:(Error.IllegalAnnotationTarget target)
                        ~define:Context.define
                      |> Option.some
                    else if Annotated.Class.Attribute.defined attribute && insufficiently_annotated
                    then
                      let attribute_location = Annotated.Attribute.location attribute in
                      Error.create
                        ~location:attribute_location
                        ~kind:
                          (Error.MissingAttributeAnnotation
                             { parent = Annotated.Attribute.parent attribute;
                               missing_annotation =
                                 { Error.name = reference;
                                   annotation = actual_annotation;
                                   given_annotation = Option.some_if is_immutable expected;
                                   evidence_locations;
                                   thrown_at_source
                                 }
                             })
                        ~define:Context.define
                      |> Option.some
                    else if insufficiently_annotated && explicit && not is_type_alias then
                      Error.create
                        ~location
                        ~kind:
                          (Error.ProhibitedAny
                             { Error.name = reference;
                               annotation = actual_annotation;
                               given_annotation = Option.some_if is_immutable expected;
                               evidence_locations;
                               thrown_at_source = true
                             })
                        ~define:Context.define
                      |> Option.some
                    else
                      None
                | _ ->
                    Error.create
                      ~location
                      ~kind:(Error.IllegalAnnotationTarget target)
                      ~define:Context.define
                    |> Option.some_if explicit
              in
              let state = error >>| emit_raw_error ~state |> Option.value ~default:state in
              let is_valid_annotation =
                error
                >>| Error.kind
                |> function
                | Some (Error.IllegalAnnotationTarget _) -> false
                | _ -> true
              in
              (* Propagate annotations. *)
              let state =
                match Reference.from_name name with
                | Some reference ->
                    let annotation =
                      if explicit && is_valid_annotation then
                        let annotation =
                          Annotation.create_immutable
                            ~global:(Resolution.is_global ~reference resolution)
                            ~final:is_final
                            guide
                        in
                        if Type.is_concrete resolved && not (Type.is_ellipsis resolved) then
                          Refinement.refine ~resolution annotation resolved
                        else
                          annotation
                      else if is_immutable then
                        Refinement.refine ~resolution target_annotation guide
                      else
                        Annotation.create guide
                    in
                    let state, annotation =
                      if
                        (not explicit)
                        && (not is_type_alias)
                        && Type.Variable.contains_escaped_free_variable
                             (Annotation.annotation annotation)
                      then
                        let kind =
                          Error.IncompleteType
                            { target = { Node.location; value = target_value };
                              annotation = resolved;
                              attempted_action = Naming
                            }
                        in
                        let converted =
                          Type.Variable.convert_all_escaped_free_variables_to_anys
                            (Annotation.annotation annotation)
                        in
                        ( emit_error ~state ~location ~kind,
                          { annotation with annotation = converted } )
                      else
                        state, annotation
                    in
                    let resolution = Resolution.set_local resolution ~reference ~annotation in
                    { state with resolution }
                | None -> state
              in
              state
          | List elements
          | Tuple elements
            when is_uniform_sequence guide ->
              let propagate state element =
                match Node.value element with
                | Starred (Starred.Once target) ->
                    let guide = uniform_sequence_parameter guide |> Type.list in
                    let resolved = uniform_sequence_parameter resolved |> Type.list in
                    forward_assign ~state ~target ~guide ~resolved ~expression:None
                | _ ->
                    let guide = uniform_sequence_parameter guide in
                    let resolved = uniform_sequence_parameter resolved in
                    forward_assign ~state ~target:element ~guide ~resolved ~expression:None
              in
              List.fold elements ~init:state ~f:propagate
          | List elements
          | Tuple elements
            when is_nonuniform_sequence ~minimum_length:(List.length elements) guide ->
              let left, starred, right =
                let is_starred { Node.value; _ } =
                  match value with
                  | Starred (Starred.Once _) -> true
                  | _ -> false
                in
                let left, tail =
                  List.split_while elements ~f:(fun element -> not (is_starred element))
                in
                let starred, right =
                  let starred, right = List.split_while tail ~f:is_starred in
                  let starred =
                    match starred with
                    | [{ Node.value = Starred (Starred.Once starred); _ }] -> [starred]
                    | _ -> []
                  in
                  starred, right
                in
                left, starred, right
              in
              let annotations =
                let annotations = nonuniform_sequence_parameters guide in
                let left, tail = List.split_n annotations (List.length left) in
                let starred, right = List.split_n tail (List.length tail - List.length right) in
                let starred =
                  if not (List.is_empty starred) then
                    let annotation =
                      List.fold starred ~init:Type.Bottom ~f:(Resolution.join resolution)
                      |> Type.list
                    in
                    [annotation]
                  else
                    []
                in
                left @ starred @ right
              in
              let resolved =
                match resolved with
                | Type.Tuple (Type.Bounded (Concrete annotations))
                  when List.length annotations = List.length elements ->
                    annotations
                | _ -> List.map elements ~f:(fun _ -> Type.Top)
              in
              let assignees = left @ starred @ right in
              let state, annotations =
                if List.length annotations <> List.length assignees then
                  let state =
                    emit_error
                      ~state
                      ~location
                      ~kind:
                        (Error.Unpack
                           { expected_count = List.length assignees;
                             unpack_problem = CountMismatch (List.length annotations)
                           })
                  in
                  state, List.map assignees ~f:(fun _ -> Type.Top)
                else
                  state, annotations
              in
              List.zip_exn assignees annotations
              |> List.zip_exn resolved
              |> List.fold ~init:state ~f:(fun state (resolved, (target, guide)) ->
                     forward_assign ~state ~target ~guide ~resolved ~expression:None)
          | List elements
          | Tuple elements ->
              let kind =
                match guide with
                | Type.Tuple (Type.Bounded (Concrete parameters)) ->
                    Error.Unpack
                      { expected_count = List.length elements;
                        unpack_problem = CountMismatch (List.length parameters)
                      }
                | annotation when is_named_tuple annotation ->
                    Error.Unpack
                      { expected_count = List.length elements;
                        unpack_problem =
                          CountMismatch (List.length (get_named_tuple_parameters annotation))
                      }
                | _ ->
                    Error.Unpack
                      { expected_count = List.length elements;
                        unpack_problem = UnacceptableType guide
                      }
              in
              let state = emit_error ~state ~location ~kind in
              List.fold elements ~init:state ~f:(fun state target ->
                  forward_assign ~state ~target ~guide:Type.Top ~resolved:Type.Top ~expression:None)
          | _ ->
              if Option.is_some annotation then
                emit_error ~state ~location ~kind:(Error.IllegalAnnotationTarget target)
              else
                state
        in
        forward_assign ~state ~target ~guide ~resolved ~expression:(Some value)
    | Assert { Assert.test; _ } -> (
        let ({ resolution; _ } as state) =
          forward_expression ~state ~expression:test |> fun { state; _ } -> state
        in
        let parse_refinement_annotation annotation =
          let parse_meta annotation =
            match parse_and_check_annotation ~state annotation |> snd with
            | Type.Top -> (
              (* Try to resolve meta-types given as expressions. *)
              match Resolution.resolve resolution annotation with
              | annotation when Type.is_meta annotation -> Type.single_parameter annotation
              | Type.Tuple (Bounded (Concrete elements)) when List.for_all ~f:Type.is_meta elements
                ->
                  List.map ~f:Type.single_parameter elements |> Type.union
              | Type.Tuple (Unbounded element) when Type.is_meta element ->
                  Type.single_parameter element
              | _ -> Type.Top )
            | annotation -> annotation
          in
          match annotation with
          | { Node.value = Tuple elements; _ } ->
              List.map ~f:parse_meta elements |> fun elements -> Type.Union elements
          | _ -> parse_meta annotation
        in
        let partition annotation ~boundary =
          let consistent_with_boundary, not_consistent_with_boundary =
            let rec extract_union_members = function
              | Type.Union parameters -> parameters
              | Type.Optional optional -> Type.none :: extract_union_members optional
              | annotation -> [annotation]
            in
            extract_union_members annotation
            |> List.partition_tf ~f:(fun left ->
                   Resolution.is_consistent_with resolution left boundary ~expression:None)
          in
          let not_consistent_with_boundary =
            if List.is_empty not_consistent_with_boundary then
              None
            else
              Some (Type.union not_consistent_with_boundary)
          in
          let consistent_with_boundary = Type.union consistent_with_boundary in
          { consistent_with_boundary; not_consistent_with_boundary }
        in
        let get_attribute_annotation ~resolution ~parent ~name =
          parent
          |> Resolution.class_definition resolution
          >>| Annotated.Class.create
          >>| Annotated.Class.attribute ~resolution ~name ~instantiated:parent ~transitive:true
          >>= fun attribute ->
          Option.some_if (Annotated.Attribute.defined attribute) attribute
          >>| Annotated.Attribute.annotation
        in
        match Node.value test with
        | False ->
            (* Explicit bottom. *)
            { state with bottom = true }
        | ComparisonOperator
            { left =
                { Node.value =
                    Call
                      { callee = { Node.value = Name (Name.Identifier "type"); _ };
                        arguments =
                          [{ Call.Argument.name = None; value = { Node.value = Name name; _ } }]
                      }
                ; _
                };
              operator = ComparisonOperator.Is;
              right = annotation
            }
        | Call
            { callee = { Node.value = Name (Name.Identifier "isinstance"); _ };
              arguments =
                [ { Call.Argument.name = None; value = { Node.value = Name name; _ } }
                ; { Call.Argument.name = None; value = annotation } ]
            }
          when Expression.is_simple_name name ->
            let reference = Reference.from_name_exn name in
            let annotation = parse_refinement_annotation annotation in
            let updated_annotation =
              let refinement_unnecessary existing_annotation =
                Refinement.less_or_equal
                  ~resolution
                  existing_annotation
                  (Annotation.create annotation)
                && not (Type.equal (Annotation.annotation existing_annotation) Type.Bottom)
              in
              match Resolution.get_local resolution ~reference with
              (* Allow Anys [especially from placeholder stubs] to clobber *)
              | _ when Type.equal annotation Type.Any -> Annotation.create annotation
              | Some existing_annotation when refinement_unnecessary existing_annotation ->
                  existing_annotation
              (* Clarify Anys if possible *)
              | Some existing_annotation
                when Type.equal (Annotation.annotation existing_annotation) Type.Any ->
                  Annotation.create annotation
              | None -> Annotation.create annotation
              | Some existing_annotation ->
                  let { consistent_with_boundary; _ } =
                    partition (Annotation.annotation existing_annotation) ~boundary:annotation
                  in
                  if Type.equal consistent_with_boundary Type.Bottom then
                    Annotation.create annotation
                  else
                    Annotation.create consistent_with_boundary
            in
            let resolution =
              Resolution.set_local resolution ~reference ~annotation:updated_annotation
            in
            { state with resolution }
        | ComparisonOperator
            { left =
                { Node.value =
                    Call
                      { callee =
                          { Node.value =
                              Name (Name.Identifier ("type" as type_refinement_function_name))
                          ; _
                          };
                        arguments = [{ Call.Argument.name = None; value }]
                      }
                ; _
                };
              operator = ComparisonOperator.IsNot;
              right = annotation_expression
            }
        | UnaryOperator
            { UnaryOperator.operator = UnaryOperator.Not;
              operand =
                { Node.value =
                    Call
                      { callee =
                          { Node.value =
                              Name
                                (Name.Identifier ("isinstance" as type_refinement_function_name))
                          ; _
                          };
                        arguments =
                          [ { Call.Argument.name = None; value }
                          ; { Call.Argument.name = None; value = annotation_expression } ]
                      }
                ; _
                }
            } -> (
            let annotation = parse_refinement_annotation annotation_expression in
            let contradiction_error =
              match annotation with
              | Type.Top ->
                  let { resolved; _ } =
                    forward_expression ~state ~expression:annotation_expression
                  in
                  Some
                    (Error.create
                       ~location:(Node.location test)
                       ~kind:
                         (Error.IncompatibleParameterType
                            { name = None;
                              position = 1;
                              callee = Some (Reference.create type_refinement_function_name);
                              mismatch =
                                { Error.expected = Type.meta (Type.variable "T");
                                  actual = resolved;
                                  actual_expressions = [annotation_expression];
                                  due_to_invariance = false
                                }
                            })
                       ~define:Context.define)
              | expected ->
                  let { resolved; _ } = forward_expression ~state ~expression:value in
                  if
                    Type.is_unbound resolved
                    || Type.is_unknown resolved
                    || not (Resolution.less_or_equal resolution ~left:resolved ~right:expected)
                  then
                    None
                  else
                    Some
                      (Error.create
                         ~location:(Node.location test)
                         ~kind:
                           (Error.ImpossibleIsinstance
                              { mismatch =
                                  Error.create_mismatch
                                    ~resolution
                                    ~expected
                                    ~actual:resolved
                                    ~actual_expression:(Some value)
                                    ~covariant:true;
                                expression = value
                              })
                         ~define:Context.define)
            in
            let resolve ~reference =
              match Resolution.get_local resolution ~reference with
              | Some { annotation = previous_annotation; _ } ->
                  let { not_consistent_with_boundary; _ } =
                    partition previous_annotation ~boundary:annotation
                  in
                  not_consistent_with_boundary
                  >>| Annotation.create
                  >>| (fun annotation -> Resolution.set_local resolution ~reference ~annotation)
                  |> Option.value ~default:resolution
              | _ -> resolution
            in
            match contradiction_error, value with
            | Some error, _ -> emit_raw_error ~state:{ state with bottom = true } error
            | _, { Node.value = Name name; _ } when Expression.is_simple_name name ->
                { state with resolution = resolve ~reference:(Reference.from_name_exn name) }
            | _ -> state )
        | Call
            { callee = { Node.value = Name (Name.Identifier "all"); _ };
              arguments = [{ Call.Argument.name = None; value = { Node.value = Name name; _ } }]
            }
          when Expression.is_simple_name name ->
            let resolution =
              let reference = Reference.from_name_exn name in
              match Resolution.get_local resolution ~reference with
              | Some
                  { Annotation.annotation =
                      Type.Parametric { name; parameters = [Type.Optional parameter] } as
                      annotation
                  ; _
                  }
                when Resolution.less_or_equal
                       resolution
                       ~left:annotation
                       ~right:(Type.iterable (Type.Optional parameter)) ->
                  Resolution.set_local
                    resolution
                    ~reference
                    ~annotation:
                      (Annotation.create (Type.Parametric { name; parameters = [parameter] }))
              | _ -> resolution
            in
            { state with resolution }
        | Name name when Expression.is_simple_name name ->
            let reference = Reference.from_name_exn name in
            let resolution =
              match Resolution.get_local resolution ~reference, name with
              | Some { Annotation.annotation = Type.Optional parameter; _ }, _ ->
                  Resolution.set_local
                    resolution
                    ~reference
                    ~annotation:(Annotation.create parameter)
              | _, Name.Attribute { base; attribute; _ } -> (
                  let parent = Resolution.resolve resolution base in
                  let attribute_annotation =
                    get_attribute_annotation ~resolution ~parent ~name:attribute
                  in
                  match attribute_annotation with
                  | Some
                      ( { Annotation.annotation = Type.Optional parameter;
                          mutability = Annotation.Mutable
                        } as annotation )
                  | Some
                      ( { Annotation.annotation = _;
                          mutability =
                            Annotation.Immutable
                              { Annotation.original = Type.Optional parameter; _ }
                        } as annotation ) ->
                      let refined = Refinement.refine ~resolution annotation parameter in
                      Resolution.set_local resolution ~reference ~annotation:refined
                  | _ -> resolution )
              | _ -> resolution
            in
            { state with resolution }
        | BooleanOperator { BooleanOperator.left; operator; right } -> (
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
                let negated_left = update state (Expression.normalize (Expression.negate left)) in
                let resolution =
                  Resolution.with_annotations resolution ~annotations:negated_left
                in
                let left = update state left in
                let right = update { state with resolution } right in
                join
                  { state with
                    resolution = Resolution.with_annotations resolution ~annotations:left
                  }
                  { state with
                    resolution = Resolution.with_annotations resolution ~annotations:right
                  } )
        | ComparisonOperator
            { ComparisonOperator.left;
              operator = ComparisonOperator.IsNot;
              right = { Node.value = Name (Name.Identifier "None"); _ }
            } ->
            forward_statement ~state ~statement:(Statement.assume left)
        | ComparisonOperator
            { ComparisonOperator.left = { Node.value = Name name; _ };
              operator = ComparisonOperator.Is;
              right = { Node.value = Name (Name.Identifier "None"); _ }
            }
          when Expression.is_simple_name name -> (
            let reference = Reference.from_name_exn name in
            let refined =
              match name with
              | Name.Attribute { base; attribute; _ } ->
                  let parent = Resolution.resolve resolution base in
                  let attribute_annotation =
                    get_attribute_annotation ~resolution ~parent ~name:attribute
                  in
                  attribute_annotation
                  >>| (fun annotation ->
                        Refinement.refine ~resolution annotation (Type.Optional Type.Bottom))
                  |> Option.value ~default:(Annotation.create (Type.Optional Type.Bottom))
              | _ -> Annotation.create (Type.Optional Type.Bottom)
            in
            match Resolution.get_local ~global_fallback:false resolution ~reference with
            | Some previous ->
                if Refinement.less_or_equal ~resolution refined previous then
                  let resolution =
                    Resolution.set_local resolution ~reference ~annotation:refined
                  in
                  { state with resolution }
                else
                  (* Keeping previous state, since it is more refined. *)
                  (* TODO: once T38750424 is done, we should really return bottom if previous is
                     not <= refined and refined is not <= previous, as this is an obvious
                     contradiction. *)
                  state
            | None ->
                let resolution = Resolution.set_local resolution ~reference ~annotation:refined in
                { state with resolution } )
        | ComparisonOperator
            { ComparisonOperator.left = { Node.value = Name name; _ };
              operator = ComparisonOperator.In;
              right
            }
          when Expression.is_simple_name name ->
            let reference = Reference.from_name_exn name in
            let { resolved; _ } = forward_expression ~state ~expression:right in
            let iterable = Resolution.join resolution resolved (Type.iterable Type.Bottom) in
            if Type.is_iterable iterable then
              let refined = Annotation.create (Type.single_parameter iterable) in
              match Resolution.get_local ~global_fallback:false resolution ~reference with
              | Some previous when not (Annotation.is_immutable previous) ->
                  if Refinement.less_or_equal ~resolution refined previous then
                    let resolution =
                      Resolution.set_local resolution ~reference ~annotation:refined
                    in
                    { state with resolution }
                  else (* Keeping previous state, since it is more refined. *)
                    state
              | None when not (Resolution.is_global resolution ~reference) ->
                  let resolution =
                    Resolution.set_local resolution ~reference ~annotation:refined
                  in
                  { state with resolution }
              | _ -> state
            else
              state
        | _ -> state )
    | Delete expression ->
        (* TODO(T41338881): Actually remove bindings from resolution. *)
        let { state; _ } = forward_expression ~state ~expression in
        state
    | Expression
        { Node.value = Call { callee; arguments = { Call.Argument.value = test; _ } :: _ }; _ }
      when Core.Set.mem Recognized.assert_functions (Expression.show callee) ->
        forward_statement ~state ~statement:(Statement.assume test)
    | Expression expression ->
        forward_expression ~state ~expression
        |> fun { state; resolved } ->
        if Type.is_noreturn resolved then
          { state with bottom = true }
        else
          state
    | Global identifiers ->
        let resolution =
          let expression =
            List.map ~f:(Node.create ~location) identifiers
            |> Expression.create_name_from_identifiers
            |> (fun name -> Name name)
            |> Node.create ~location
          in
          let annotation =
            let { resolved; _ } = forward_expression ~state ~expression in
            Annotation.create_immutable resolved ~global:true
          in
          Resolution.set_local
            resolution
            ~reference:(Reference.create_from_list identifiers)
            ~annotation
        in
        { state with resolution }
    | Import { Import.from; imports } ->
        let imports =
          match from with
          | Some from -> [from]
          | None -> List.map imports ~f:(fun { Import.name; _ } -> name)
        in
        let add_import_error state import =
          let error, _ =
            let check_import (error, lead) import =
              let import = lead @ [import] in
              let reference = Reference.create_from_list import in
              match error with
              | Some error -> Some error, import
              | None ->
                  let error =
                    Error.create
                      ~location
                      ~kind:(Error.UndefinedImport reference)
                      ~define:Context.define
                  in
                  let local = Resolution.get_local resolution ~reference in
                  let module_definition = Resolution.module_definition resolution reference in
                  if Option.is_some local || Option.is_some module_definition then
                    None, import
                  else
                    Some error, import
            in
            List.fold ~f:check_import ~init:(None, []) (Reference.as_list import)
          in
          error >>| emit_raw_error ~state |> Option.value ~default:state
        in
        imports
        |> List.filter ~f:(fun import -> not (Resolution.is_suppressed_module resolution import))
        |> List.fold ~init:state ~f:add_import_error
    | Raise (Some expression) -> forward_expression ~state ~expression |> fun { state; _ } -> state
    | Raise None -> state
    | Return { Return.expression; is_implicit } ->
        let { state; resolved = actual } =
          Option.value_map
            expression
            ~default:{ state; resolved = Type.none }
            ~f:(fun expression -> forward_expression ~state ~expression)
        in
        validate_return ~expression ~state ~actual ~is_implicit
    | Statement.Yield { Node.value = Expression.Yield return; _ } ->
        let { state; resolved = actual } =
          match return with
          | Some expression ->
              let { state; resolved } = forward_expression ~state ~expression in
              { state; resolved = Type.generator ~async resolved }
          | None -> { state; resolved = Type.generator ~async Type.none }
        in
        validate_return ~expression:None ~state ~actual ~is_implicit:false
    | Statement.Yield _ -> state
    | YieldFrom { Node.value = Expression.Yield (Some return); _ } ->
        let { state; resolved } = forward_expression ~state ~expression:return in
        let actual =
          match Resolution.join resolution resolved (Type.iterator Type.Bottom) with
          | Type.Parametric { name = "typing.Iterator"; parameters = [parameter] } ->
              Type.generator parameter
          | annotation -> Type.generator annotation
        in
        validate_return ~expression:None ~state ~actual ~is_implicit:false
    | YieldFrom _ -> state
    | Class _
    | Define _ ->
        (* Don't check accesses in nested classes and functions, they're analyzed separately. *)
        state
    | For _
    | If _
    | Try _
    | With _
    | While _ ->
        (* Check happens implicitly in the resulting control flow. *)
        state
    | Break
    | Continue
    | Nonlocal _
    | Pass ->
        state


  let forward
      ?key
      ({ resolution; resolution_fixpoint; nested_defines; bottom; _ } as state)
      ~statement:({ Node.location; _ } as statement)
    =
    let state =
      if bottom then
        state
      else
        forward_statement ~state ~statement
    in
    let state =
      let nested_defines =
        let schedule ~variables ~define =
          let update = function
            | Some ({ initial = { nested_resolution; nested_bottom }; _ } as nested) ->
                let resolution, bottom =
                  if nested_bottom then
                    state.resolution, state.bottom
                  else if state.bottom then
                    nested_resolution, nested_bottom
                  else
                    join_resolutions nested_resolution state.resolution, false
                in
                Some
                  { nested with
                    initial = { nested_resolution = resolution; nested_bottom = bottom }
                  }
            | None ->
                let { resolution = initial_resolution; _ } = initial ~resolution in
                let nested_resolution =
                  let update ~key ~data initial_resolution =
                    Resolution.set_local initial_resolution ~reference:key ~annotation:data
                  in
                  let add_variable resolution variable =
                    Resolution.add_type_variable resolution ~variable
                  in
                  Resolution.annotations resolution
                  |> Map.fold ~init:initial_resolution ~f:update
                  |> fun resolution -> List.fold variables ~init:resolution ~f:add_variable
                in
                Some { nested = define; initial = { nested_resolution; nested_bottom = false } }
          in
          Map.change ~f:update nested_defines location
        in
        match Node.value statement with
        | Class ({ Class.name; body; _ } as definition) ->
            let variables =
              Node.create ~location definition
              |> Annotated.Class.create
              |> Annotated.Class.generics ~resolution
              |> List.filter_map ~f:(function
                     | Type.Variable variable -> Some (Type.Variable.Unary variable)
                     | _ -> None)
            in
            schedule
              ~variables
              ~define:(Define.create_class_toplevel ~parent:name ~statements:body)
        | Define ({ Define.signature = { parameters; _ }; _ } as define) ->
            let variables =
              let extract_variables { Node.value = { Parameter.annotation; _ }; _ } =
                match annotation with
                | None -> []
                | Some annotation ->
                    let annotation = Resolution.parse_annotation resolution annotation in
                    Type.Variable.all_free_variables annotation
              in
              List.concat_map parameters ~f:extract_variables
              |> List.dedup_and_sort ~compare:Type.Variable.compare
            in
            schedule ~variables ~define
        | _ -> nested_defines
      in
      { state with nested_defines }
    in
    let state =
      let resolution_fixpoint =
        match key, state with
        | Some key, { resolution = post_resolution; _ } ->
            let precondition = Resolution.annotations resolution |> Reference.Map.to_tree in
            let postcondition = Resolution.annotations post_resolution |> Reference.Map.to_tree in
            Int.Map.Tree.set resolution_fixpoint ~key ~data:{ precondition; postcondition }
        | None, _ -> resolution_fixpoint
      in
      { state with resolution_fixpoint }
    in
    state


  let backward ?key:_ state ~statement:_ = state
end

type result = {
  errors: Error.t list;
  coverage: Coverage.t
}

let resolution (module Handler : Environment.Handler) ?(annotations = Reference.Map.empty) () =
  let define =
    Define.create_toplevel ~qualifier:None ~statements:[] |> Node.create_with_default_location
  in
  let module State = State (struct
    let configuration = Configuration.Analysis.create ()

    let define = define
  end)
  in
  let aliases = Handler.aliases in
  let order = (module Handler.TypeOrderHandler : TypeOrder.Handler) in
  let state_without_resolution =
    let empty_resolution =
      Resolution.create
        ~annotations:Reference.Map.empty
        ~order:(module Handler.TypeOrderHandler)
        ~resolve:(fun ~resolution:_ _ -> Type.Top)
        ~aliases:(fun _ -> None)
        ~global:(fun _ -> None)
        ~module_definition:(fun _ -> None)
        ~class_definition:(fun _ -> None)
        ~class_metadata:(fun _ -> None)
        ~constructor:(fun ~resolution:_ _ -> None)
        ~generics:(fun ~resolution:_ _ -> [])
        ~undecorated_signature:(fun _ -> None)
        ~attributes:(fun ~resolution:_ _ -> None)
        ~is_protocol:(fun _ -> false)
        ()
    in
    { State.errors = ErrorKey.Map.empty;
      check_return = true;
      nested_defines = Location.Reference.Map.empty;
      bottom = false;
      resolution_fixpoint = Int.Map.Tree.empty;
      resolution = empty_resolution
    }
  in
  let resolve ~resolution expression =
    let state = { state_without_resolution with State.resolution } in
    State.forward_expression ~state ~expression |> fun { State.resolved; _ } -> resolved
  in
  let constructor ~resolution class_name =
    let instantiated = Type.Primitive class_name in
    Handler.class_definition class_name
    >>| AnnotatedClass.create
    >>| AnnotatedClass.constructor ~instantiated ~resolution
  in
  let generics ~resolution class_definition =
    AnnotatedClass.create class_definition |> AnnotatedClass.generics ~resolution
  in
  let is_protocol annotation =
    Type.split annotation
    |> fst
    |> Type.primitive_name
    >>= Handler.class_definition
    >>| AnnotatedClass.create
    >>| AnnotatedClass.is_protocol
    |> Option.value ~default:false
  in
  let attributes ~resolution annotation =
    Resolution.class_definition resolution annotation
    >>| AnnotatedClass.create
    >>| AnnotatedClass.attributes ~resolution ~transitive:true ~instantiated:annotation
  in
  Resolution.create
    ~annotations
    ~order
    ~resolve
    ~aliases
    ~global:Handler.globals
    ~module_definition:Handler.module_definition
    ~class_definition:Handler.class_definition
    ~class_metadata:Handler.class_metadata
    ~constructor
    ~undecorated_signature:Handler.undecorated_signature
    ~generics
    ~attributes
    ~is_protocol
    ()


let resolution_with_key ~environment ~parent ~name ~key =
  let annotations =
    match key, ResolutionSharedMemory.get name with
    | Some key, Some map ->
        map
        |> Int.Map.of_tree
        |> (fun map -> Int.Map.find map key)
        >>| (fun { precondition; _ } -> precondition)
        >>| Reference.Map.of_tree
        |> Option.value ~default:Reference.Map.empty
    | _ -> Reference.Map.empty
  in
  resolution environment ~annotations () |> Resolution.with_parent ~parent


let name = "TypeCheck"

let run
    ~configuration
    ~environment
    ~source:( { Source.handle;
                statements;
                qualifier;
                metadata = { Source.Metadata.local_mode; debug; version; number_of_lines; _ }
              ; _
              } as source )
  =
  let timer = Timer.start () in
  Log.log ~section:`Check "Checking `%a`..." File.Handle.pp handle;
  let resolution = resolution environment () in
  let configuration =
    (* Override file-specific local debug configuraiton *)
    let local_strict, declare =
      match local_mode with
      | Source.Strict -> true, false
      | Source.Declare -> false, true
      | _ -> false, false
    in
    Configuration.Analysis.localize configuration ~local_debug:debug ~local_strict ~declare
  in
  ResolutionSharedMemory.Keys.LocalChanges.push_stack ();
  let results =
    let queue =
      let queue = Queue.create () in
      ( if not (version < 3) then
          let toplevel =
            let location =
              { Location.path = File.Handle.show handle;
                start = { Location.line = 1; column = 1 };
                stop = { Location.line = 1; column = 1 }
              }
              |> Location.reference
            in
            Define.create_toplevel ~qualifier:(Some qualifier) ~statements |> Node.create ~location
          in
          Queue.enqueue queue (toplevel, resolution) );
      queue
    in
    let rec results ~queue =
      match Queue.dequeue queue with
      | Some
          ( ( { Node.location; value = { Define.signature = { name; _ }; _ } as define } as
            define_node ),
            resolution ) ->
          let result =
            let module Context = struct
              let configuration = configuration

              let define = define_node
            end
            in
            let module State = State (Context) in
            let module Fixpoint = Fixpoint.Make (State) in
            let check
                ~define:{ Node.value = { Define.signature = { name; parent; _ }; _ } as define; _ }
                ~initial
                ~queue
              =
              Log.log ~section:`Check "Checking %a" Reference.pp name;
              let dump = Define.dump define in
              if dump then (
                Log.dump "Checking `%s`..." (Log.Color.yellow (Reference.show name));
                Log.dump "AST:\n%a" Define.pp define );
              let dump_cfg cfg fixpoint =
                let precondition table id =
                  match Hashtbl.find table id with
                  | Some { State.resolution; _ } ->
                      let stringify ~key ~data label =
                        let annotation_string =
                          Type.show (Annotation.annotation data)
                          |> String.strip ~drop:(Char.equal '`')
                        in
                        label ^ "\n" ^ Reference.show key ^ ": " ^ annotation_string
                      in
                      Map.fold ~f:stringify ~init:"" (Resolution.annotations resolution)
                  | None -> ""
                in
                if Define.dump_cfg define then
                  let name =
                    match parent with
                    | Some parent -> Reference.combine parent name
                    | None -> name
                  in
                  Path.create_relative
                    ~root:(Configuration.Analysis.pyre_root configuration)
                    ~relative:(Format.asprintf "cfgs%a.dot" Reference.pp name)
                  |> File.create ~content:(Cfg.to_dot ~precondition:(precondition fixpoint) cfg)
                  |> File.write
              in
              let exit =
                let cfg = Cfg.create define in
                let fixpoint = Fixpoint.forward ~cfg ~initial in
                dump_cfg cfg fixpoint;
                Fixpoint.exit fixpoint
              in
              if dump then exit >>| Log.dump "Exit state:\n%a" State.pp |> ignore;
              (* Write fixpoint type resolutions to shared memory *)
              let dump_resolutions { State.resolution_fixpoint; _ } =
                if configuration.store_type_check_resolution then
                  ResolutionSharedMemory.add ~handle name resolution_fixpoint
              in
              exit >>| dump_resolutions |> ignore;
              (* Schedule nested functions for analysis. *)
              exit >>| State.nested_defines >>| List.iter ~f:(Queue.enqueue queue) |> ignore;
              let errors = exit >>| State.errors |> Option.value ~default:[] in
              let coverage =
                exit >>| State.coverage |> Option.value ~default:(Coverage.create ())
              in
              { errors; coverage }
            in
            try
              let initial = State.initial ~resolution in
              if Define.is_stub define then
                { errors = State.errors initial; coverage = Coverage.create () }
              else
                check ~define:define_node ~initial ~queue
            with
            | TypeOrder.Untracked annotation ->
                Statistics.event
                  ~name:"undefined type"
                  ~integers:[]
                  ~normals:
                    [ "handle", File.Handle.show handle;
                      "define", Reference.show name;
                      "type", Type.show annotation ]
                  ();
                if Define.dump define then
                  Log.dump
                    "Analysis crashed because of untracked type `%s`."
                    (Log.Color.red (Type.show annotation));
                let undefined_error =
                  Error.create
                    ~location
                    ~kind:(Error.AnalysisFailure annotation)
                    ~define:define_node
                in
                { errors = [undefined_error]; coverage = Coverage.create ~crashes:1 () }
          in
          result :: results ~queue
      | _ -> []
    in
    results ~queue
  in
  (* These local changes allow us to add keys incrementally in a worker process without worrying
     about removing (which can only be done by a master. *)
  ResolutionSharedMemory.Keys.LocalChanges.commit_all ();
  ResolutionSharedMemory.Keys.LocalChanges.pop_stack ();
  let errors =
    let mode error =
      let (module Handler : Environment.Handler) = environment in
      Handler.local_mode (Error.path error |> File.Handle.create)
      |> fun local_mode -> Ast.Source.mode ~configuration ~local_mode
    in
    let filter { errors; _ } =
      if configuration.debug then
        errors
      else
        let keep_error error = not (Error.suppress ~mode:(mode error) ~resolution error) in
        List.filter ~f:keep_error errors
    in
    let mark_as_hint error =
      match mode error with
      | Default when Error.language_server_hint error -> { error with severity = Hint }
      | _ -> error
    in
    let filter_hints { Error.severity; _ } =
      let { Configuration.Analysis.include_hints; _ } = configuration in
      match severity with
      | Hint when not include_hints -> false
      | _ -> true
    in
    List.map results ~f:filter
    |> List.map ~f:(Error.join_at_define ~resolution)
    |> List.concat
    |> Error.join_at_source ~resolution
    |> List.map ~f:mark_as_hint
    |> List.filter ~f:filter_hints
    |> List.map ~f:(Error.dequalify (Preprocessing.dequalify_map source) ~resolution)
    |> List.sort ~compare:Error.compare
  in
  let coverage =
    List.map results ~f:(fun { coverage; _ } -> coverage) |> Coverage.aggregate_over_source ~source
  in
  Coverage.log coverage ~total_errors:(List.length errors) ~path:(File.Handle.show handle);
  Coverage.add coverage ~handle;
  Statistics.performance
    ~flush:false
    ~randomly_log_every:100
    ~section:`Check
    ~name:"SingleFileTypeCheck"
    ~timer
    ~normals:["handle", File.Handle.show handle; "request kind", "SingleFileTypeCheck"]
    ~integers:["number of lines", number_of_lines]
    ();
  errors
