(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
open Ast
open Expression
open Statement
module StatementDefine = Define
module ExpressionCall = Call
module Error = AnalysisError

type class_name_and_is_abstract_and_is_protocol = {
  class_name: string;
  is_abstract: bool;
  is_protocol: bool;
}

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

  type t = Error.t Map.t

  let add ~errors ({ Error.location = { Location.WithModule.start; stop; _ }; _ } as error) =
    let location = { Location.start; stop } in
    Map.set errors ~key:{ location; kind = Error.code error } ~data:error
end

module type Context = sig
  val qualifier : Reference.t

  val debug : bool

  val define : Define.t Node.t

  module Builder : Callgraph.Builder
end

module type Signature = sig
  type t [@@deriving eq]

  val create
    :  ?bottom:bool ->
    ?errors:ErrorMap.t ->
    resolution:Resolution.t ->
    ?resolution_fixpoint:LocalAnnotationMap.t ->
    unit ->
    t

  val resolution : t -> Resolution.t

  val error_map : t -> ErrorMap.t

  val initial : resolution:Resolution.t -> t

  type base =
    | Class of Type.t
    | Instance of Type.t
    | Super of Type.t

  and resolved = {
    state: t;
    resolved: Type.t;
    resolved_annotation: Annotation.t option;
    base: base option;
  }
  [@@deriving show]

  val parse_and_check_annotation : ?bind_variables:bool -> state:t -> Expression.t -> t * Type.t

  val forward_expression : state:t -> expression:Expression.t -> resolved

  val forward_statement : state:t -> statement:Statement.t -> t

  include Fixpoint.State with type t := t
end

module State (Context : Context) = struct
  type partitioned = {
    consistent_with_boundary: Type.t;
    not_consistent_with_boundary: Type.t option;
  }

  and t = {
    resolution: Resolution.t;
    errors: ErrorMap.t;
    bottom: bool;
    resolution_fixpoint: LocalAnnotationMap.t;
  }

  let pp format { resolution; errors; bottom; _ } =
    let global_resolution = Resolution.global_resolution resolution in
    let expected =
      let parser = GlobalResolution.annotation_parser global_resolution in
      let { Node.value = { Define.signature; _ }; _ } = Context.define in
      Annotated.Callable.return_annotation_without_applying_decorators ~signature ~parser
    in
    let annotations =
      let annotation_to_string (name, refinement_unit) =
        Format.asprintf "    %a -> %a" Reference.pp name RefinementUnit.pp refinement_unit
      in
      Resolution.annotation_store resolution
      |> Map.to_alist
      |> List.map ~f:annotation_to_string
      |> String.concat ~sep:"\n"
    in
    let errors =
      let error_to_string error =
        let error =
          let lookup reference =
            GlobalResolution.ast_environment global_resolution
            |> fun ast_environment -> AstEnvironment.ReadOnly.get_relative ast_environment reference
          in
          Error.instantiate ~lookup error
        in
        Format.asprintf
          "    %a -> %s"
          Location.WithPath.pp
          (Error.Instantiated.location error)
          (Error.Instantiated.description error ~show_error_traces:true)
      in
      List.map (Map.data errors) ~f:error_to_string |> String.concat ~sep:"\n"
    in
    Format.fprintf
      format
      "  Bottom: %b\n  Expected return: %a\n  Types:\n%s\n  Errors:\n%s\n"
      bottom
      Type.pp
      expected
      annotations
      errors


  let show state = Format.asprintf "%a" pp state

  and equal left right =
    (* Ignore errors in unit tests. *)
    Map.equal
      RefinementUnit.equal
      (Resolution.annotation_store left.resolution)
      (Resolution.annotation_store right.resolution)
    && Bool.equal left.bottom right.bottom


  let create
      ?(bottom = false)
      ?(errors = ErrorMap.Map.empty)
      ~resolution
      ?(resolution_fixpoint = LocalAnnotationMap.empty)
      ()
    =
    { resolution; errors; bottom; resolution_fixpoint }


  let add_invalid_type_parameters_errors ~resolution ~location ~errors annotation =
    let mismatches, annotation =
      GlobalResolution.check_invalid_type_parameters resolution annotation
    in
    let add_error errors mismatch =
      Error.create
        ~location:(Location.with_module ~qualifier:Context.qualifier location)
        ~kind:(Error.InvalidTypeParameters mismatch)
        ~define:Context.define
      |> ErrorMap.add ~errors
    in
    List.fold mismatches ~f:add_error ~init:errors, annotation


  let add_untracked_annotation_errors ~resolution ~location ~errors annotation =
    let untracked_annotation_error class_name =
      match class_name with
      | "..." -> None
      | _ -> (
          match GlobalResolution.is_tracked resolution class_name with
          | true -> None
          | false ->
              Some
                (Error.create
                   ~location:(Location.with_module ~qualifier:Context.qualifier location)
                   ~kind:(Error.UndefinedType (Primitive class_name))
                   ~define:Context.define) )
    in
    let untracked = List.filter_map (Type.elements annotation) ~f:untracked_annotation_error in
    let errors =
      List.fold untracked ~init:errors ~f:(fun errors error -> ErrorMap.add ~errors error)
    in
    errors, List.is_empty untracked


  let parse_and_check_annotation
      ?(bind_variables = true)
      ~state:({ errors; resolution; _ } as state)
      ({ Node.location; _ } as expression)
    =
    let global_resolution = Resolution.global_resolution resolution in
    let check_and_correct_annotation ~resolution ~location ~annotation errors =
      let check_invalid_variables resolution variable =
        if not (Resolution.type_variable_exists resolution ~variable) then
          let origin =
            if Define.is_toplevel (Node.value Context.define) then
              Error.Toplevel
            else if Define.is_class_toplevel (Node.value Context.define) then
              Error.ClassToplevel
            else
              Error.Define
          in
          Error.create
            ~location:(Location.with_module ~qualifier:Context.qualifier location)
            ~kind:(Error.InvalidTypeVariable { annotation = variable; origin })
            ~define:Context.define
          |> Option.some
        else
          None
      in
      let resolution =
        match annotation with
        | Type.Callable { Type.Callable.implementation = { Type.Callable.parameters; _ }; _ } ->
            let parameters =
              Type.Callable.create ~annotation:Type.Top ~parameters ()
              |> Type.Variable.all_free_variables
            in
            List.fold
              parameters
              ~f:(fun resolution variable -> Resolution.add_type_variable resolution ~variable)
              ~init:resolution
        | _ -> resolution
      in
      let all_primitives_and_variables_are_valid, errors =
        let errors, no_untracked =
          add_untracked_annotation_errors ~resolution:global_resolution ~location ~errors annotation
        in
        let invalid_variables_errors =
          Type.Variable.all_free_variables annotation
          |> List.filter_map ~f:(check_invalid_variables resolution)
        in
        let add_errors errors ~add =
          List.fold ~init:errors ~f:(fun errors error -> ErrorMap.add ~errors error) add
        in
        ( no_untracked && List.is_empty invalid_variables_errors,
          add_errors errors ~add:invalid_variables_errors )
      in
      if all_primitives_and_variables_are_valid then
        add_invalid_type_parameters_errors
          annotation
          ~resolution:global_resolution
          ~location
          ~errors
      else
        errors, Type.Top
    in
    let annotation =
      GlobalResolution.parse_annotation
        ~allow_untracked:true
        ~allow_invalid_type_parameters:true
        global_resolution
        expression
    in
    let errors =
      match annotation with
      | Type.Top ->
          Error.create
            ~location:(Location.with_module ~qualifier:Context.qualifier location)
            ~kind:
              (Error.InvalidType
                 (InvalidType
                    { annotation = Type.Primitive (Expression.show expression); expected = "" }))
            ~define:Context.define
          |> ErrorMap.add ~errors
      | Type.Callable { implementation = { annotation = Type.Top; _ }; _ } ->
          Error.create
            ~location:(Location.with_module ~qualifier:Context.qualifier location)
            ~kind:
              (Error.InvalidType
                 (InvalidType
                    {
                      annotation = Type.Primitive (Expression.show expression);
                      expected = "`Callable[[<parameters>], <return type>]`";
                    }))
            ~define:Context.define
          |> ErrorMap.add ~errors
      | _ -> errors
    in
    let errors, annotation =
      check_and_correct_annotation errors ~resolution ~location ~annotation
    in
    let annotation =
      if bind_variables then Type.Variable.mark_all_variables_as_bound annotation else annotation
    in
    { state with errors }, annotation


  let resolution { resolution; _ } = resolution

  let error_map { errors; _ } = errors

  let less_or_equal ~left:({ resolution; _ } as left) ~right =
    let global_resolution = Resolution.global_resolution resolution in
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
           ~f:
             (entry_less_or_equal
                (Resolution.annotation_store right.resolution)
                (RefinementUnit.less_or_equal ~global_resolution))
           (Resolution.annotation_store left.resolution)


  let join left right =
    if left.bottom then
      right
    else if right.bottom then
      left
    else
      let join_resolutions left_resolution right_resolution =
        let merge_annotation_stores ~key:_ = function
          | `Both (left, right) ->
              Some
                (RefinementUnit.join
                   ~global_resolution:(Resolution.global_resolution left_resolution)
                   left
                   right)
          | `Left _
          | `Right _ ->
              Some (RefinementUnit.create ~base:(Annotation.create Type.Top) ())
        in
        let annotation_store =
          Map.merge
            ~f:merge_annotation_stores
            (Resolution.annotation_store left_resolution)
            (Resolution.annotation_store right_resolution)
        in
        Resolution.with_annotation_store left_resolution ~annotation_store
      in
      let combine_errors ~key:_ left_error right_error =
        Error.join ~resolution:(Resolution.global_resolution left.resolution) left_error right_error
      in
      {
        left with
        errors = Map.merge_skewed left.errors right.errors ~combine:combine_errors;
        resolution = join_resolutions left.resolution right.resolution;
      }


  let widening_threshold = 10

  let widen ~previous:({ resolution; _ } as previous) ~next ~iteration =
    let global_resolution = Resolution.global_resolution resolution in
    if previous.bottom then
      next
    else if next.bottom then
      previous
    else
      let widen_annotations ~key annotation =
        match annotation with
        | `Both (previous, next) ->
            Some
              (RefinementUnit.widen
                 ~global_resolution
                 ~widening_threshold
                 ~previous
                 ~next
                 ~iteration)
        | `Left previous
        | `Right previous
          when Reference.length key = 1 ->
            let widened =
              RefinementUnit.widen
                ~global_resolution
                ~widening_threshold
                ~previous
                ~next:(RefinementUnit.create ~base:(Annotation.create Type.undeclared) ())
                ~iteration
            in
            Some widened
        | `Left previous
        | `Right previous ->
            Some previous
        | _ -> None
      in
      let annotation_store =
        Map.merge
          ~f:widen_annotations
          (Resolution.annotation_store previous.resolution)
          (Resolution.annotation_store next.resolution)
      in
      let resolution_fixpoint =
        LocalAnnotationMap.merge previous.resolution_fixpoint next.resolution_fixpoint
      in
      let combine_errors ~key:_ left_error right_error =
        if iteration > widening_threshold then
          { left_error with Error.kind = Error.Top }
        else
          Error.join ~resolution:global_resolution left_error right_error
      in
      {
        previous with
        errors = Map.merge_skewed previous.errors next.errors ~combine:combine_errors;
        resolution = Resolution.with_annotation_store resolution ~annotation_store;
        resolution_fixpoint;
      }


  let emit_raw_error
      ~state:({ errors; resolution; _ } as state)
      ({ Error.location = { Location.WithModule.start; stop; _ }; _ } as error)
    =
    let error =
      let location = { Location.start; stop } in
      match Map.find errors { ErrorMap.location; kind = Error.code error } with
      | Some other_error ->
          Error.join ~resolution:(Resolution.global_resolution resolution) error other_error
      | None -> error
    in
    { state with errors = ErrorMap.add ~errors error }


  let emit_error ~state ~location ~kind =
    Error.create
      ~location:(Location.with_module ~qualifier:Context.qualifier location)
      ~kind
      ~define:Context.define
    |> emit_raw_error ~state


  type base =
    | Class of Type.t
    | Instance of Type.t
    | Super of Type.t

  and resolved = {
    state: t;
    resolved: Type.t;
    resolved_annotation: Annotation.t option;
    base: base option;
  }
  [@@deriving show]

  let type_of_signature ~resolution signature =
    let global_resolution = Resolution.global_resolution resolution in
    GlobalResolution.create_overload ~resolution:global_resolution signature
    |> Type.Callable.create_from_implementation


  let type_of_parent ~global_resolution parent =
    let parent_name = Reference.show parent in
    let parent_type = Type.Primitive parent_name in
    let variables = GlobalResolution.variables global_resolution parent_name in
    match variables with
    | None
    | Some [] ->
        parent_type
    | Some variables ->
        let variables =
          List.map variables ~f:(function
              | Unary variable -> Type.Parameter.Single (Type.Variable variable)
              | ListVariadic variadic -> Group (Type.Variable.Variadic.List.self_reference variadic)
              | ParameterVariadic parameters ->
                  CallableParameters (Type.Variable.Variadic.Parameters.self_reference parameters))
        in
        Type.Parametric { name = parent_name; parameters = variables }
    | exception _ -> parent_type


  let rec initial ~resolution =
    let global_resolution = Resolution.global_resolution resolution in
    let {
      Node.location;
      value =
        {
          Define.signature =
            { name; parent; parameters; return_annotation; decorators; async; nesting_define; _ } as
            signature;
          captures;
          _;
        } as define;
    }
      =
      Context.define
    in
    (* Add type variables *)
    let outer_scope_variables, current_scope_variables =
      let type_variables_of_class class_name =
        let unarize unary =
          let fix_invalid_parameters_in_bounds unary =
            match
              GlobalResolution.check_invalid_type_parameters global_resolution (Type.Variable unary)
            with
            | _, Type.Variable unary -> unary
            | _ -> failwith "did not transform"
          in
          fix_invalid_parameters_in_bounds unary |> fun unary -> Type.Variable.Unary unary
        in
        let extract = function
          | Type.Variable.Unary unary -> unarize unary
          | ListVariadic variadic -> Type.Variable.ListVariadic variadic
          | ParameterVariadic variable -> ParameterVariadic variable
        in
        Reference.show class_name
        |> GlobalResolution.variables global_resolution
        >>| List.map ~f:extract
        |> Option.value ~default:[]
      in
      let type_variables_of_define signature =
        let parser =
          GlobalResolution.annotation_parser ~allow_invalid_type_parameters:true global_resolution
        in
        let define_variables =
          AnnotatedCallable.create_overload_without_applying_decorators ~parser signature
          |> (fun { parameters; _ } -> Type.Callable.create ~parameters ~annotation:Type.Top ())
          |> Type.Variable.all_free_variables
          |> List.dedup_and_sort ~compare:Type.Variable.compare
        in
        let parent_variables =
          let { Define.Signature.parent; _ } = signature in
          (* PEP484 specifies that scope of the type variables of the outer class doesn't cover the
             inner one. We are able to inspect only 1 level of nesting class as a result. *)
          Option.value_map parent ~f:type_variables_of_class ~default:[]
        in
        List.append parent_variables define_variables
      in
      match Define.is_class_toplevel define with
      | true ->
          let class_name = Option.value_exn parent in
          [], type_variables_of_class class_name
      | false ->
          let define_variables = type_variables_of_define signature in
          let nesting_define_variables =
            let rec walk_nesting_define sofar = function
              | None -> sofar
              | Some define_name -> (
                  (* TODO (T57339384): This operation should only depend on the signature, not the
                     body *)
                  match GlobalResolution.define_body global_resolution define_name with
                  | None -> sofar
                  | Some
                      {
                        Node.value =
                          {
                            Define.signature = { Define.Signature.nesting_define; _ } as signature;
                            _;
                          };
                        _;
                      } ->
                      let sofar = List.rev_append (type_variables_of_define signature) sofar in
                      walk_nesting_define sofar nesting_define )
            in
            walk_nesting_define [] nesting_define
          in
          nesting_define_variables, define_variables
    in
    let resolution =
      List.append current_scope_variables outer_scope_variables
      |> List.fold ~init:resolution ~f:(fun resolution variable ->
             Resolution.add_type_variable resolution ~variable)
    in
    let instantiate location =
      let ast_environment = GlobalResolution.ast_environment global_resolution in
      let location = Location.with_module ~qualifier:Context.qualifier location in
      Location.WithModule.instantiate
        ~lookup:(AstEnvironment.ReadOnly.get_relative ast_environment)
        location
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
            | Expression.Name (Name.Attribute { attribute; _ }) when String.equal attribute suffix
              ->
                true
            | _ -> false
          in
          let is_property_derivative decorator =
            has_suffix decorator "setter"
            || has_suffix decorator "getter"
            || has_suffix decorator "deleter"
          in
          let is_attr_validator decorator = has_suffix decorator "validator" in
          let is_click_derivative decorator = has_suffix decorator "command" in
          (* TODO (T41383196): Properly deal with @property and @click *)
          is_property_derivative decorator
          || is_click_derivative decorator
          || is_attr_validator decorator
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
      let add_missing_return_error ~state annotation =
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
        let contains_literal_any =
          Type.contains_prohibited_any return_annotation
          && annotation >>| Type.expression_contains_any |> Option.value ~default:false
        in
        if
          ((not (Define.is_toplevel define)) && not (Define.is_class_toplevel define))
          && not (Option.is_some annotation)
          || contains_literal_any
        then
          emit_error
            ~state
            ~location
            ~kind:
              (Error.MissingReturnAnnotation
                 {
                   name = Reference.create "$return_annotation";
                   annotation = None;
                   given_annotation =
                     Option.some_if (Define.has_return_annotation define) return_annotation;
                   evidence_locations = [];
                   thrown_at_source = true;
                 })
        else
          state
      in
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
      let state = add_missing_return_error ~state return_annotation in
      return_annotation
      >>| parse_and_check_annotation ~state
      >>| add_variance_error
      >>| fst
      |> Option.value ~default:state
    in
    let add_capture_annotations state =
      let process_signature ({ Define.Signature.name = { Node.value = name; _ }; _ } as signature) =
        if Reference.is_local name then
          type_of_signature ~resolution signature
          |> Type.Variable.mark_all_variables_as_bound ~specific:outer_scope_variables
          |> Annotation.create
          |> (fun annotation -> Resolution.set_local resolution ~reference:name ~annotation)
          |> fun resolution -> { state with resolution }
        else
          state
      in
      let process_capture state { Define.Capture.name; kind } =
        let state, annotation =
          match kind with
          | Define.Capture.Kind.Annotation None ->
              emit_error ~state ~location ~kind:(Error.MissingCaptureAnnotation name), Type.Any
          | Define.Capture.Kind.Annotation (Some annotation_expression) ->
              parse_and_check_annotation ~state annotation_expression
          | Define.Capture.Kind.DefineSignature signature ->
              ( state,
                type_of_signature ~resolution signature
                |> Type.Variable.mark_all_variables_as_bound ~specific:outer_scope_variables )
          | Define.Capture.Kind.Self parent -> state, type_of_parent ~global_resolution parent
          | Define.Capture.Kind.ClassSelf parent ->
              state, type_of_parent ~global_resolution parent |> Type.meta
        in
        let annotation = Annotation.create_immutable annotation in
        let resolution =
          let { resolution; _ } = state in
          let reference = Reference.create name in
          Resolution.set_local resolution ~reference ~annotation
        in
        { state with resolution }
      in
      let state = process_signature signature in
      List.fold captures ~init:state ~f:process_capture
    in
    let check_parameter_annotations ({ resolution; resolution_fixpoint; _ } as state) =
      let state, annotation_store =
        let make_parameter_name name =
          name |> String.filter ~f:(fun character -> character <> '*') |> Reference.create
        in
        let check_parameter
            index
            (state, annotation_store)
            { Node.location; value = { Parameter.name; value; annotation } }
          =
          let add_incompatible_variable_error ~state annotation default =
            if
              Type.is_any default
              || GlobalResolution.less_or_equal global_resolution ~left:default ~right:annotation
              || GlobalResolution.constraints_solution_exists
                   global_resolution
                   ~left:default
                   ~right:annotation
            then
              state
            else
              emit_error
                ~state
                ~location
                ~kind:
                  (Error.IncompatibleVariableType
                     {
                       incompatible_type =
                         {
                           name = Reference.create name;
                           mismatch =
                             Error.create_mismatch
                               ~resolution:global_resolution
                               ~expected:annotation
                               ~actual:default
                               ~covariant:true;
                         };
                       declare_location = instantiate location;
                     })
          in
          let add_missing_parameter_annotation_error ~state ~given_annotation annotation =
            let name = name |> Identifier.sanitized in
            let is_dunder_new_method_for_named_tuple =
              Define.is_method define
              && Reference.is_suffix
                   ~suffix:(Reference.create ".__new__")
                   (Node.value define.signature.name)
              && Option.value_map
                   ~default:false
                   ~f:(name_is ~name:"typing.NamedTuple")
                   return_annotation
            in
            if
              String.equal name "*"
              || String.is_prefix ~prefix:"_" name
              || Option.is_some given_annotation
                 && (String.is_prefix ~prefix:"**" name || String.is_prefix ~prefix:"*" name)
              || is_dunder_new_method_for_named_tuple
              || String.equal name "/"
            then
              state
            else
              emit_error
                ~state
                ~location
                ~kind:
                  (Error.MissingParameterAnnotation
                     {
                       name = Reference.create name;
                       annotation;
                       given_annotation;
                       evidence_locations = [];
                       thrown_at_source = true;
                     })
          in
          let add_final_parameter_annotation_error ~state =
            emit_error ~state ~location ~kind:(Error.InvalidType (FinalParameter name))
          in
          let add_variance_error (state, annotation) =
            let state =
              match annotation with
              | Type.Variable variable
                when (not (Define.is_constructor define))
                     && Type.Variable.Unary.is_covariant variable ->
                  emit_error
                    ~state
                    ~location
                    ~kind:(Error.InvalidTypeVariance { annotation; origin = Error.Parameter })
              | _ -> state
            in
            state, annotation
          in
          let parse_as_unary () =
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
                    let parent_annotation = type_of_parent ~global_resolution parent in
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
                        GlobalResolution.constraints_solution_exists
                          global_resolution
                          ~left:resolved
                          ~right:annotation
                      in
                      let state =
                        let name = Identifier.sanitized name in
                        let kind =
                          if compatible then
                            None
                          else if
                            (is_class_method && String.equal name "cls")
                            || ((not is_class_method) && String.equal name "self")
                          then
                            (* Assume the user incorrectly tried to type the implicit parameter *)
                            Some
                              (Error.InvalidMethodSignature { annotation = Some annotation; name })
                          else (* Assume the user forgot to specify the implicit parameter *)
                            Some
                              (Error.InvalidMethodSignature
                                 {
                                   annotation = None;
                                   name = (if is_class_method then "cls" else "self");
                                 })
                        in
                        match kind with
                        | Some kind -> emit_error ~state ~location ~kind
                        | None -> state
                      in
                      state, Annotation.create annotation
                  | None -> state, Annotation.create resolved )
              | _ -> (
                  let state, parsed_annotation =
                    annotation
                    >>| parse_and_check_annotation ~state ~bind_variables:false
                    >>| add_variance_error
                    >>| (fun (state, annotation) -> state, Some annotation)
                    |> Option.value ~default:(state, None)
                  in
                  let contains_prohibited_any parsed_annotation =
                    let contains_literal_any =
                      annotation >>| Type.expression_contains_any |> Option.value ~default:false
                    in
                    contains_literal_any && Type.contains_prohibited_any parsed_annotation
                  in
                  let value_annotation =
                    value
                    >>| (fun expression -> forward_expression ~state ~expression)
                    >>| fun { resolved; _ } -> resolved
                  in
                  let state =
                    match parsed_annotation, value_annotation with
                    | Some annotation, Some value_annotation ->
                        add_incompatible_variable_error ~state annotation value_annotation
                    | _ -> state
                  in
                  match parsed_annotation, value_annotation with
                  | Some annotation, Some value_annotation when Type.contains_final annotation ->
                      ( add_final_parameter_annotation_error ~state,
                        Annotation.create_immutable ~original:(Some annotation) value_annotation )
                  | Some annotation, Some value_annotation when contains_prohibited_any annotation
                    ->
                      ( add_missing_parameter_annotation_error
                          ~state
                          ~given_annotation:(Some annotation)
                          (Some value_annotation),
                        Annotation.create_immutable ~original:(Some annotation) value_annotation )
                  | Some annotation, _ when Type.contains_final annotation ->
                      ( add_final_parameter_annotation_error ~state,
                        Annotation.create_immutable annotation )
                  | Some annotation, None when contains_prohibited_any annotation ->
                      ( add_missing_parameter_annotation_error
                          ~state
                          ~given_annotation:(Some annotation)
                          None,
                        Annotation.create_immutable annotation )
                  | Some annotation, _ -> state, Annotation.create_immutable annotation
                  | None, Some value_annotation ->
                      ( add_missing_parameter_annotation_error
                          ~state
                          ~given_annotation:None
                          (Some value_annotation),
                        Annotation.create value_annotation )
                  | None, None ->
                      ( add_missing_parameter_annotation_error ~state ~given_annotation:None None,
                        Annotation.create Type.Any ) )
            in
            let apply_starred_annotations annotation =
              if String.is_prefix ~prefix:"**" name then
                Type.dictionary ~key:Type.string ~value:annotation
              else if String.is_prefix ~prefix:"*" name then
                Type.Tuple (Type.Unbounded annotation)
              else
                annotation
            in
            let annotation =
              Type.Variable.mark_all_variables_as_bound annotation |> apply_starred_annotations
            in
            let mutability =
              match mutability with
              | Annotation.Immutable { Annotation.original; final } ->
                  let original =
                    Type.Variable.mark_all_variables_as_bound original |> apply_starred_annotations
                  in
                  Annotation.Immutable { Annotation.original; final }
              | _ -> mutability
            in
            state, { Annotation.annotation; mutability }
          in
          let state, { Annotation.annotation; mutability } =
            if String.is_prefix ~prefix:"**" name then
              parse_as_unary ()
            else if String.is_prefix ~prefix:"*" name then
              let make_tuple bounded =
                Type.Tuple (Bounded bounded)
                |> Type.Variable.mark_all_variables_as_bound
                |> Annotation.create
                |> fun annotation -> state, annotation
              in
              let parsed_as_concatenation () =
                annotation
                >>= GlobalResolution.parse_as_concatenation global_resolution
                >>| fun concatenation -> Type.OrderedTypes.Concatenation concatenation
              in
              match parsed_as_concatenation () with
              | Some map -> make_tuple map
              | None -> parse_as_unary ()
            else
              parse_as_unary ()
          in
          ( state,
            Map.set
              annotation_store
              ~key:(make_parameter_name name)
              ~data:(RefinementUnit.create ~base:{ Annotation.annotation; mutability } ()) )
        in
        let number_of_stars name = Identifier.split_star name |> fst |> String.length in
        match List.rev parameters, parent with
        | [], Some _ when not (Define.is_class_toplevel define || Define.is_static_method define) ->
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
            state, Resolution.annotation_store resolution
        | ( {
              Node.value = { name = second_name; value = None; annotation = Some second_annotation };
              _;
            }
            :: {
                 Node.value =
                   { name = first_name; value = None; annotation = Some first_annotation };
                 _;
               }
               :: reversed_head,
            _ )
          when number_of_stars first_name = 1 && number_of_stars second_name = 2 -> (
            match
              GlobalResolution.parse_as_parameter_specification_instance_annotation
                global_resolution
                ~variable_parameter_annotation:first_annotation
                ~keywords_parameter_annotation:second_annotation
            with
            | Some variable ->
                let add_annotations
                    {
                      Type.Variable.Variadic.Parameters.Components.positional_component;
                      keyword_component;
                    }
                  =
                  Resolution.annotation_store resolution
                  |> Map.set
                       ~key:(make_parameter_name first_name)
                       ~data:
                         (RefinementUnit.create ~base:(Annotation.create positional_component) ())
                  |> Map.set
                       ~key:(make_parameter_name second_name)
                       ~data:(RefinementUnit.create ~base:(Annotation.create keyword_component) ())
                in
                if Resolution.type_variable_exists resolution ~variable:(ParameterVariadic variable)
                then
                  let annotations =
                    Type.Variable.Variadic.Parameters.mark_as_bound variable
                    |> Type.Variable.Variadic.Parameters.decompose
                    |> add_annotations
                  in
                  List.rev reversed_head |> List.foldi ~init:(state, annotations) ~f:check_parameter
                else
                  let state =
                    let origin =
                      if Define.is_toplevel (Node.value Context.define) then
                        Error.Toplevel
                      else if Define.is_class_toplevel (Node.value Context.define) then
                        Error.ClassToplevel
                      else
                        Error.Define
                    in
                    emit_error
                      ~state
                      ~location
                      ~kind:
                        (Error.InvalidTypeVariable
                           { annotation = ParameterVariadic variable; origin })
                  in
                  state, add_annotations { positional_component = Top; keyword_component = Top }
            | None ->
                List.foldi
                  ~init:(state, Resolution.annotation_store resolution)
                  ~f:check_parameter
                  parameters )
        | _ ->
            List.foldi
              ~init:(state, Resolution.annotation_store resolution)
              ~f:check_parameter
              parameters
      in
      let resolution = Resolution.with_annotation_store resolution ~annotation_store in
      let resolution_fixpoint =
        let postcondition = Resolution.annotation_store resolution in
        let key = [%hash: int * int] (Cfg.entry_index, 0) in
        LocalAnnotationMap.set resolution_fixpoint ~key ~postcondition
      in
      { state with resolution; resolution_fixpoint }
    in
    let check_base_annotations state =
      if Define.is_class_toplevel define then
        let open Annotated in
        let check_base state { ExpressionCall.Argument.value; _ } =
          let state_with_errors, parsed = parse_and_check_annotation ~state value in
          let is_actual_any () =
            match
              GlobalResolution.parse_annotation
                global_resolution
                value
                ~allow_primitives_from_empty_stubs:true
            with
            | Any -> true
            | _ -> false
          in
          match parsed with
          | Type.Parametric { name = "type"; parameters = [Single Type.Any] } ->
              (* Inheriting from type makes you a metaclass, and we don't want to
               * suggest that instead you need to use typing.Type[Something] *)
              state
          | Top
          (* There's some other problem we already errored on *)
          | Primitive _
          | Parametric _ ->
              state_with_errors
          | Any when not (is_actual_any ()) -> state_with_errors
          | annotation ->
              emit_error
                ~state:state_with_errors
                ~location:(Node.location value)
                ~kind:(InvalidInheritance (UninheritableType annotation))
        in
        let bases =
          Node.create define ~location
          |> Define.create
          |> Define.parent_definition ~resolution:global_resolution
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
            begin
              match define with
              | { Ast.Statement.Define.signature = { parent = Some parent; _ }; _ } -> (
                  Class.overrides
                    (Reference.show parent)
                    ~resolution:global_resolution
                    ~name:(StatementDefine.unqualified_name define)
                  >>| fun overridden_attribute ->
                  let errors =
                    match AnnotatedAttribute.visibility overridden_attribute with
                    | ReadOnly (Refinable { overridable = false }) ->
                        let parent = overridden_attribute |> Attribute.parent in
                        let error =
                          Error.create
                            ~location:(Location.with_module ~qualifier:Context.qualifier location)
                            ~kind:(Error.InvalidOverride { parent; decorator = Final })
                            ~define:Context.define
                        in
                        ErrorMap.add ~errors error
                    | _ -> errors
                  in
                  let errors =
                    if
                      not
                        (Bool.equal
                           (Attribute.static overridden_attribute)
                           (StatementDefine.is_static_method define))
                    then
                      let parent = overridden_attribute |> Attribute.parent in
                      let decorator =
                        if Attribute.static overridden_attribute then
                          Error.StaticSuper
                        else
                          Error.StaticOverride
                      in
                      let error =
                        Error.create
                          ~location:(Location.with_module ~qualifier:Context.qualifier location)
                          ~kind:(Error.InvalidOverride { parent; decorator })
                          ~define:Context.define
                      in
                      ErrorMap.add ~errors error
                    else
                      errors
                  in
                  (* Check strengthening of postcondition. *)
                  match Annotation.annotation (Attribute.annotation overridden_attribute) with
                  | Type.Callable { Type.Callable.implementation; _ }
                    when not (StatementDefine.is_static_method define) ->
                      let original_implementation =
                        resolve_reference_type ~state:{ state with resolution } (Node.value name)
                        |> function
                        | Type.Callable
                            { Type.Callable.implementation = original_implementation; _ } ->
                            original_implementation
                        | annotation -> raise (ClassHierarchy.Untracked annotation)
                      in
                      let errors =
                        let expected = Type.Callable.Overload.return_annotation implementation in
                        let actual =
                          Type.Callable.Overload.return_annotation original_implementation
                        in
                        if
                          Type.Variable.all_variables_are_resolved expected
                          && not
                               (GlobalResolution.less_or_equal
                                  global_resolution
                                  ~left:actual
                                  ~right:expected)
                        then
                          let error =
                            Error.create
                              ~location:(Location.with_module ~qualifier:Context.qualifier location)
                              ~kind:
                                (Error.InconsistentOverride
                                   {
                                     overridden_method = StatementDefine.unqualified_name define;
                                     parent =
                                       Attribute.parent overridden_attribute |> Reference.create;
                                     override_kind = Method;
                                     override =
                                       Error.WeakenedPostcondition
                                         (Error.create_mismatch
                                            ~resolution:global_resolution
                                            ~actual
                                            ~expected
                                            ~covariant:false);
                                   })
                              ~define:Context.define
                          in
                          ErrorMap.add ~errors error
                        else
                          errors
                      in
                      (* Check weakening of precondition. *)
                      let overriding_parameters =
                        let parameter_annotations
                            { StatementDefine.signature = { parameters; _ }; _ }
                            ~resolution
                          =
                          let element { Node.value = { Parameter.name; annotation; _ }; _ } =
                            let annotation =
                              annotation
                              >>| (fun annotation ->
                                    GlobalResolution.parse_annotation resolution annotation)
                              |> Option.value ~default:Type.Top
                            in
                            name, annotation
                          in
                          List.map parameters ~f:element
                        in
                        parameter_annotations define ~resolution:global_resolution
                        |> List.map ~f:(fun (name, annotation) ->
                               { Type.Callable.Parameter.name; annotation; default = false })
                        |> Type.Callable.Parameter.create
                      in
                      let check_parameter errors overridden_parameter =
                        let validate_match ~expected = function
                          | Some actual -> (
                              let is_compatible =
                                let expected = Type.Variable.mark_all_variables_as_bound expected in
                                GlobalResolution.constraints_solution_exists
                                  global_resolution
                                  ~left:expected
                                  ~right:actual
                              in
                              try
                                if (not (Type.is_top expected)) && not is_compatible then
                                  let error =
                                    Error.create
                                      ~location:
                                        (Location.with_module ~qualifier:Context.qualifier location)
                                      ~kind:
                                        (Error.InconsistentOverride
                                           {
                                             overridden_method =
                                               StatementDefine.unqualified_name define;
                                             parent =
                                               Attribute.parent overridden_attribute
                                               |> Reference.create;
                                             override_kind = Method;
                                             override =
                                               Error.StrengthenedPrecondition
                                                 (Error.Found
                                                    (Error.create_mismatch
                                                       ~resolution:global_resolution
                                                       ~actual
                                                       ~expected
                                                       ~covariant:false));
                                           })
                                      ~define:Context.define
                                  in
                                  ErrorMap.add ~errors error
                                else
                                  errors
                              with
                              | ClassHierarchy.Untracked _ ->
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
                                    ~location:
                                      (Location.with_module ~qualifier:Context.qualifier location)
                                    ~kind:
                                      (Error.InconsistentOverride
                                         {
                                           overridden_method =
                                             StatementDefine.unqualified_name define;
                                           override_kind = Method;
                                           parent =
                                             Attribute.parent overridden_attribute
                                             |> Reference.create;
                                           override =
                                             Error.StrengthenedPrecondition
                                               (Error.NotFound overridden_parameter);
                                         })
                                    ~define:Context.define
                                in
                                ErrorMap.add ~errors error
                        in
                        match overridden_parameter with
                        | Type.Callable.Parameter.Anonymous { index; annotation; _ } ->
                            List.nth overriding_parameters index
                            >>= (function
                                  | Anonymous { annotation; _ }
                                  | Named { annotation; _ } ->
                                      Some annotation
                                  | _ -> None)
                            |> validate_match ~expected:annotation
                        | KeywordOnly { name = overridden_name; annotation; _ }
                        | Named { name = overridden_name; annotation; _ } ->
                            (* TODO(T44178876): ensure index match as well for named parameters *)
                            let equal_name = function
                              | Type.Callable.Parameter.KeywordOnly { name; annotation; _ }
                              | Type.Callable.Parameter.Named { name; annotation; _ } ->
                                  Option.some_if
                                    (Identifier.equal
                                       (Identifier.remove_leading_underscores name)
                                       (Identifier.remove_leading_underscores overridden_name))
                                    annotation
                              | _ -> None
                            in
                            List.find_map overriding_parameters ~f:equal_name
                            |> validate_match ~expected:annotation
                        | Variable (Concrete annotation) ->
                            let find_variable_parameter = function
                              | Type.Callable.Parameter.Variable (Concrete annotation) ->
                                  Some annotation
                              | _ -> None
                            in
                            List.find_map overriding_parameters ~f:find_variable_parameter
                            |> validate_match ~expected:annotation
                        | Keywords annotation ->
                            let find_variable_parameter = function
                              | Type.Callable.Parameter.Keywords annotation -> Some annotation
                              | _ -> None
                            in
                            List.find_map overriding_parameters ~f:find_variable_parameter
                            |> validate_match ~expected:annotation
                        | Variable (Concatenation _) ->
                            (* TODO(T44178876): There is no reasonable way to compare either of
                               these alone, which is the central issue with this comparison
                               strategy. For now, let's just ignore this *)
                            errors
                      in
                      Type.Callable.Overload.parameters implementation
                      |> Option.value ~default:[]
                      |> List.fold ~init:errors ~f:check_parameter
                  | _ -> errors )
              | _ -> None
            end
            |> Option.value ~default:errors
        with
        | ClassHierarchy.Untracked _ -> errors
      in
      { state with errors }
    in
    let check_constructor_return state =
      if not (Define.is_constructor define) then
        state
      else
        match return_annotation with
        | Some ({ Node.location; _ } as annotation) -> (
            match define with
            | { Define.signature = { Define.Signature.name = { Node.value = name; _ }; _ }; _ }
              when String.equal (Reference.last name) "__new__" ->
                (* TODO(T45018328): Error here. `__new__` is a special undecorated static method,
                   and we really ought to be checking its return type against typing.Type[Cls]. *)
                state
            | _ ->
                let annotation = GlobalResolution.parse_annotation global_resolution annotation in
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
    |> check_return_annotation
    |> add_capture_annotations
    |> check_decorators
    |> check_parameter_annotations
    |> check_base_annotations
    |> check_behavioral_subtyping
    |> check_constructor_return


  and forward_expression ~state:({ resolution; _ } as state) ~expression:{ Node.location; value } =
    let global_resolution = Resolution.global_resolution resolution in
    let forward_entry ~state ~entry:{ Dictionary.Entry.key; value } =
      let { state; resolved = key_resolved; _ } = forward_expression ~state ~expression:key in
      let { state; resolved = value_resolved; _ } = forward_expression ~state ~expression:value in
      Type.weaken_literals key_resolved, Type.weaken_literals value_resolved, state
    in
    let forward_generator ~state ~generator:({ Comprehension.Generator.conditions; _ } as generator)
      =
      (* Propagate the target type information. *)
      let iterator =
        Statement.Assign (Ast.Statement.Statement.generator_assignment generator)
        |> Node.create ~location
      in
      let state =
        let { errors; resolution_fixpoint; _ } = state in
        let ({ errors = iterator_errors; _ } as state) =
          forward_statement ~state:{ state with errors = ErrorMap.Map.empty } ~statement:iterator
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
            ~combine:(fun ~key:_ left right -> Error.join ~resolution:global_resolution left right)
            iterator_errors
            errors
        in
        { state with errors; resolution_fixpoint }
      in
      List.map conditions ~f:Statement.assume
      |> List.fold ~init:state ~f:(fun state statement -> forward_statement ~state ~statement)
    in
    let forward_comprehension ~element ~generators =
      let { state; resolved; _ } =
        List.fold
          generators
          ~f:(fun state generator -> forward_generator ~state ~generator)
          ~init:state
        |> fun state -> forward_expression ~state ~expression:element
      in
      (* Discard generator-local variables. *)
      {
        state = { state with resolution };
        resolved = Type.weaken_literals resolved;
        resolved_annotation = None;
        base = None;
      }
    in
    let forward_elements ~state ~elements =
      let forward_element { state; resolved; _ } expression =
        match Node.value expression with
        | Expression.Starred (Starred.Once expression) ->
            let { state; resolved = new_resolved; _ } = forward_expression ~state ~expression in
            let parameter =
              (* TODO (T56720048): Stop joining with iterable bottom *)
              match
                GlobalResolution.join global_resolution new_resolved (Type.iterable Type.Bottom)
              with
              | Type.Parametric { parameters = [Single parameter]; _ } -> parameter
              | _ -> Type.Any
            in
            {
              state;
              resolved = GlobalResolution.join global_resolution resolved parameter;
              resolved_annotation = None;
              base = None;
            }
        | _ ->
            let { state; resolved = new_resolved; _ } = forward_expression ~state ~expression in
            {
              state;
              resolved = GlobalResolution.join global_resolution resolved new_resolved;
              resolved_annotation = None;
              base = None;
            }
      in
      let correct_bottom { state; resolved; _ } =
        let resolved =
          if Type.is_unbound resolved then
            Type.variable "_T" |> Type.Variable.mark_all_free_variables_as_escaped
          else
            resolved
        in
        { state; resolved; resolved_annotation = None; base = None }
      in
      List.fold
        elements
        ~init:{ state; resolved = Type.Bottom; resolved_annotation = None; base = None }
        ~f:forward_element
      |> (fun { state; resolved; _ } ->
           {
             state;
             resolved = Type.weaken_literals resolved;
             resolved_annotation = None;
             base = None;
           })
      |> correct_bottom
    in
    let forward_reference ~state reference =
      let reference = GlobalResolution.resolve_exports global_resolution ~reference in
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
            let create_annotation signature =
              Annotation.create_immutable
                ~original:(Some Type.Top)
                (Type.Callable.Overload.return_annotation signature)
            in
            match getattr with
            | Some (Callable { overloads = [signature]; _ }) when correct_getattr_arity signature ->
                Some (create_annotation signature)
            | Some (Callable { implementation = signature; _ }) when correct_getattr_arity signature
              ->
                Some (create_annotation signature)
            | _ -> None )
        | _ -> None
      in
      match annotation with
      | Some annotation when Type.is_undeclared (Annotation.annotation annotation) ->
          let state =
            Error.UndefinedName reference |> fun kind -> emit_error ~state ~location ~kind
          in
          {
            state;
            resolved = Annotation.annotation annotation;
            resolved_annotation = Some annotation;
            base = None;
          }
      | Some annotation ->
          {
            state;
            resolved = Annotation.annotation annotation;
            resolved_annotation = Some annotation;
            base = None;
          }
      | None -> (
          match GlobalResolution.module_exists global_resolution reference with
          | false when not (GlobalResolution.is_suppressed_module global_resolution reference) ->
              let state =
                match Reference.prefix reference with
                | Some qualifier when not (Reference.is_empty qualifier) ->
                    if GlobalResolution.module_exists global_resolution qualifier then
                      Error.UndefinedAttribute
                        { attribute = Reference.last reference; origin = Error.Module qualifier }
                      |> (fun kind ->
                           Error.create
                             ~location:(Location.with_module ~qualifier:Context.qualifier location)
                             ~kind
                             ~define:Context.define)
                      |> emit_raw_error ~state
                    else
                      state
                | _ ->
                    Error.create
                      ~location:(Location.with_module ~qualifier:Context.qualifier location)
                      ~kind:(Error.UndefinedName reference)
                      ~define:Context.define
                    |> emit_raw_error ~state
              in
              { state; resolved = Type.Top; resolved_annotation = None; base = None }
          | _ -> { state; resolved = Type.Top; resolved_annotation = None; base = None } )
    in
    let forward_callable ~state ~target ~dynamic ~callee ~resolved ~arguments =
      let state =
        let forward_argument state { Call.Argument.value; _ } =
          forward_expression ~state ~expression:value |> fun { state; _ } -> state
        in
        List.fold arguments ~f:forward_argument ~init:state
      in
      let find_method ~parent ~name =
        Type.split parent
        |> fst
        |> Type.primitive_name
        >>= GlobalResolution.attribute_from_class_name
              ~resolution:global_resolution
              ~name
              ~instantiated:parent
              ~transitive:true
        >>= fun attribute ->
        Option.some_if (Annotated.Attribute.defined attribute) attribute
        >>| Annotated.Attribute.annotation
        >>| Annotation.annotation
        >>= function
        | Type.Callable callable -> Some callable
        | _ -> None
      in
      (* When an operator does not exist on the left operand but its inverse exists on the right
         operand, the missing attribute error would not have been thrown for the original operator.
         Build up the original error in case the inverse operator does not typecheck. *)
      let potential_missing_operator_error =
        match resolved, Node.value callee, target with
        | Type.Top, Expression.Name (Attribute { attribute; _ }), Some target
          when Option.is_some (inverse_operator attribute)
               && (not (Type.is_any target))
               && not (Type.is_unbound target) ->
            Some
              (Error.UndefinedAttribute
                 {
                   attribute;
                   origin = Error.Class { annotation = target; class_attribute = false };
                 })
        | _ -> None
      in
      let signatures =
        let callables, arguments, was_operator_inverted =
          let callable = function
            | meta when Type.is_meta meta -> (
                let backup = find_method ~parent:meta ~name:"__call__" in
                match Type.single_parameter meta with
                | TypedDictionary { name; fields; total } ->
                    Type.TypedDictionary.constructor ~name ~fields ~total |> Option.some
                | Variable { constraints = Type.Variable.Unconstrained; _ } -> backup
                | Variable { constraints = Type.Variable.Explicit constraints; _ }
                  when List.length constraints > 1 ->
                    backup
                | Any -> backup
                | meta_parameter -> (
                    let parent =
                      match meta_parameter with
                      | Variable { constraints = Type.Variable.Explicit [parent]; _ } -> parent
                      | Variable { constraints = Type.Variable.Bound parent; _ } -> parent
                      | _ -> meta_parameter
                    in
                    parent
                    |> Type.split
                    |> fst
                    |> Type.primitive_name
                    >>= GlobalResolution.constructor
                          ~instantiated:meta_parameter
                          ~resolution:global_resolution
                    >>= function
                    | Type.Callable callable -> Some callable
                    | _ -> None ) )
            | Type.Callable callable -> Some callable
            | resolved -> find_method ~parent:resolved ~name:"__call__"
          in
          let rec get_callables = function
            | Type.Union annotations ->
                List.map annotations ~f:callable |> Option.all, arguments, false
            | Type.Variable { constraints = Type.Variable.Bound parent; _ } -> get_callables parent
            | Type.Top -> (
                match Node.value callee, arguments with
                | Expression.Name (Attribute { base; attribute; _ }), [{ Call.Argument.value; _ }]
                  ->
                    let inverted_arguments = [{ Call.Argument.value = base; name = None }] in
                    inverse_operator attribute
                    >>= (fun name ->
                          find_method ~parent:(resolve_expression_type ~state value) ~name)
                    >>= (fun found_callable ->
                          let resolved_base = resolve_expression_type ~state base in
                          if Type.is_any resolved_base || Type.is_unbound resolved_base then
                            callable resolved >>| fun callable -> [callable], arguments, false
                          else
                            Some ([found_callable], inverted_arguments, true))
                    |> Option.value_map
                         ~default:(None, arguments, false)
                         ~f:(fun (callables, arguments, was_operator_inverted) ->
                           Some callables, arguments, was_operator_inverted)
                | _ -> None, arguments, false )
            | annotation -> (callable annotation >>| fun callable -> [callable]), arguments, false
          in
          get_callables resolved
        in
        Context.Builder.add_callee
          ~global_resolution
          ~target
          ~callables
          ~arguments
          ~dynamic
          ~qualifier:Context.qualifier
          ~callee;
        let signature callable =
          let signature =
            GlobalResolution.signature_select
              ~arguments
              ~global_resolution
              ~resolve:(resolve_expression_type ~state)
              ~callable
          in
          match signature with
          | AttributeResolution.NotFound _ -> (
              match Node.value callee, callable, arguments with
              | ( Name (Name.Attribute { base; _ }),
                  { Type.Callable.kind = Type.Callable.Named name; _ },
                  [{ Call.Argument.value; _ }] )
                when not was_operator_inverted ->
                  let arguments = [{ Call.Argument.value = base; name = None }] in
                  inverse_operator (Reference.last name)
                  >>= (fun name -> find_method ~parent:(resolve_expression_type ~state value) ~name)
                  >>| (fun callable ->
                        GlobalResolution.signature_select
                          ~arguments
                          ~global_resolution:(Resolution.global_resolution resolution)
                          ~resolve:(resolve_expression_type ~state)
                          ~callable)
                  |> Option.value ~default:signature
              | _ -> signature )
          | AttributeResolution.Found
              ({ kind = Type.Callable.Named access; implementation; _ } as callable)
            when String.equal "__init__" (Reference.last access) ->
              Type.split implementation.annotation
              |> fst
              |> Type.primitive_name
              >>| (function
                    | class_name ->
                        let abstract_methods =
                          Annotated.Class.get_abstract_attributes
                            ~resolution:global_resolution
                            class_name
                          |> List.map ~f:Annotated.Attribute.name
                        in
                        if not (List.is_empty abstract_methods) then
                          AttributeResolution.NotFound
                            {
                              callable;
                              reason =
                                Some
                                  (AbstractClassInstantiation
                                     { class_name = Reference.create class_name; abstract_methods });
                            }
                        else if
                          GlobalResolution.is_protocol global_resolution (Primitive class_name)
                        then
                          AttributeResolution.NotFound
                            {
                              callable;
                              reason = Some (ProtocolInstantiation (Reference.create class_name));
                            }
                        else
                          signature)
              |> Option.value ~default:signature
          | _ -> signature
        in
        callables >>| List.map ~f:signature
      in
      let signature =
        let not_found = function
          | AttributeResolution.NotFound _ -> true
          | _ -> false
        in
        match signatures >>| List.partition_tf ~f:not_found with
        (* Prioritize missing signatures for union type checking. *)
        | Some (not_found :: _, _) -> Some not_found
        | Some ([], AttributeResolution.Found callable :: found) ->
            let callables =
              let extract = function
                | AttributeResolution.Found callable -> callable
                | _ -> failwith "Not all signatures were found."
              in
              List.map found ~f:extract
            in
            let signature =
              let joined_callable =
                List.map callables ~f:(fun callable -> Type.Callable callable)
                |> List.fold
                     ~init:(Type.Callable callable)
                     ~f:(GlobalResolution.join global_resolution)
              in
              match joined_callable with
              | Type.Callable callable -> AttributeResolution.Found callable
              | _ -> AttributeResolution.NotFound { callable; reason = None }
            in
            Some signature
        | _ -> None
      in
      match signature with
      | Some (AttributeResolution.Found { implementation = { annotation; _ }; _ }) ->
          { state; resolved = annotation; resolved_annotation = None; base = None }
      | Some
          (AttributeResolution.NotFound
            {
              callable = { implementation = { annotation; _ }; kind; implicit; _ } as callable;
              reason = Some reason;
            }) ->
          let state =
            let error =
              let callee =
                match kind with
                | Type.Callable.Named callable -> Some callable
                | _ -> None
              in
              match reason with
              | AbstractClassInstantiation { class_name; abstract_methods } ->
                  let location = Location.with_module ~qualifier:Context.qualifier location in
                  Error.create
                    ~location
                    ~kind:
                      (Error.InvalidClassInstantiation
                         (Error.AbstractClassInstantiation { class_name; abstract_methods }))
                    ~define:Context.define
              | CallingParameterVariadicTypeVariable ->
                  let location = Location.with_module ~qualifier:Context.qualifier location in
                  Error.create
                    ~location
                    ~kind:(Error.NotCallable (Type.Callable callable))
                    ~define:Context.define
              | InvalidKeywordArgument { Node.location; value = { expression; annotation } } ->
                  let kind = Error.InvalidArgument (Error.Keyword { expression; annotation }) in
                  let location = Location.with_module ~qualifier:Context.qualifier location in
                  Error.create ~location ~kind ~define:Context.define
              | InvalidVariableArgument { Node.location; value = { expression; annotation } } ->
                  let kind =
                    Error.InvalidArgument (Error.ConcreteVariable { expression; annotation })
                  in
                  let location = Location.with_module ~qualifier:Context.qualifier location in
                  Error.create ~location ~kind ~define:Context.define
              | Mismatch mismatch ->
                  let { AttributeResolution.actual; expected; name; position } =
                    Node.value mismatch
                  in
                  let mismatch, name, position, location =
                    ( Error.create_mismatch
                        ~resolution:global_resolution
                        ~actual
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
                  let location = Location.with_module ~qualifier:Context.qualifier location in
                  Error.create ~location ~kind ~define:Context.define
              | MismatchWithListVariadicTypeVariable { variable; mismatch } ->
                  let location = Location.with_module ~qualifier:Context.qualifier location in
                  Error.create
                    ~location
                    ~kind:(Error.InvalidArgument (ListVariadicVariable { variable; mismatch }))
                    ~define:Context.define
              | MissingArgument parameter ->
                  let location = Location.with_module ~qualifier:Context.qualifier location in
                  Error.create
                    ~location
                    ~kind:(Error.MissingArgument { callee; parameter })
                    ~define:Context.define
              | MutuallyRecursiveTypeVariables ->
                  let location = Location.with_module ~qualifier:Context.qualifier location in
                  Error.create
                    ~location
                    ~kind:(Error.MutuallyRecursiveTypeVariables callee)
                    ~define:Context.define
              | ProtocolInstantiation class_name ->
                  let location = Location.with_module ~qualifier:Context.qualifier location in
                  Error.create
                    ~location
                    ~kind:(Error.InvalidClassInstantiation (ProtocolInstantiation class_name))
                    ~define:Context.define
              | TooManyArguments { expected; provided } ->
                  let location = Location.with_module ~qualifier:Context.qualifier location in
                  Error.create
                    ~location
                    ~kind:(Error.TooManyArguments { callee; expected; provided })
                    ~define:Context.define
              | UnexpectedKeyword name ->
                  let location = Location.with_module ~qualifier:Context.qualifier location in
                  Error.create
                    ~location
                    ~kind:(Error.UnexpectedKeyword { callee; name })
                    ~define:Context.define
            in
            emit_raw_error ~state error
          in
          { state; resolved = annotation; resolved_annotation = None; base = None }
      | _ ->
          let state =
            match resolved, potential_missing_operator_error with
            | Type.Top, Some kind ->
                Error.create
                  ~location:(Location.with_module ~qualifier:Context.qualifier location)
                  ~kind
                  ~define:Context.define
                |> emit_raw_error ~state
            | Type.Any, _
            | Type.Top, _ ->
                state
            | _ ->
                Error.NotCallable resolved
                |> (fun kind ->
                     Error.create
                       ~location:(Location.with_module ~qualifier:Context.qualifier location)
                       ~kind
                       ~define:Context.define)
                |> emit_raw_error ~state
          in
          { state; resolved = Type.Top; resolved_annotation = None; base = None }
    in
    let join_resolved left right =
      {
        state = join left.state right.state;
        resolved = GlobalResolution.join global_resolution left.resolved right.resolved;
        resolved_annotation = None;
        base = None;
      }
    in
    let is_terminating_error { Error.kind; _ } =
      let open Error in
      match kind with
      | UndefinedAttribute _
      | UndefinedName _ ->
          true
      | _ -> false
    in
    match value with
    | Await expression ->
        let { state; resolved; _ } = forward_expression ~state ~expression in
        let state =
          let is_awaitable =
            GlobalResolution.less_or_equal
              global_resolution
              ~left:resolved
              ~right:(Type.awaitable Type.Top)
          in
          if not is_awaitable then
            emit_error ~state ~location ~kind:(Error.IncompatibleAwaitableType resolved)
          else
            state
        in
        let resolved =
          GlobalResolution.join global_resolution (Type.awaitable Type.Bottom) resolved
          |> Type.awaitable_value
          |> Option.value ~default:Type.Top
        in
        { state; resolved; resolved_annotation = None; base = None }
    | BooleanOperator { BooleanOperator.left; operator; right } ->
        let assume =
          let assume =
            match operator with
            | BooleanOperator.And -> left
            | BooleanOperator.Or -> normalize (negate left)
          in
          Statement.assume assume
        in
        let { state = state_left; resolved = resolved_left; _ } =
          forward_expression ~state ~expression:left
        in
        let { state = state_right; resolved = resolved_right; _ } =
          forward_expression ~state:(forward_statement ~state ~statement:assume) ~expression:right
        in
        let resolved =
          match resolved_left, resolved_right, operator with
          | Optional resolved_left, resolved_right, BooleanOperator.Or ->
              GlobalResolution.join global_resolution resolved_left resolved_right
          (* Zero is also falsy. *)
          | Optional integer, resolved_right, BooleanOperator.And
            when Type.equal integer Type.integer ->
              Type.optional
                (GlobalResolution.join global_resolution (Type.literal_integer 0) resolved_right)
          | Optional _, resolved_right, BooleanOperator.And -> Type.optional resolved_right
          | resolved_left, resolved_right, _ ->
              GlobalResolution.join global_resolution resolved_left resolved_right
        in
        { state = join state_left state_right; resolved; resolved_annotation = None; base = None }
    | Call { callee = { Node.value = Name (Name.Identifier "super"); _ } as callee; arguments } -> (
        let metadata =
          Resolution.parent resolution
          >>| (fun parent -> Type.Primitive (Reference.show parent))
          >>= GlobalResolution.class_metadata global_resolution
        in
        (* Resolve `super()` calls. *)
        let superclass { ClassMetadataEnvironment.successors; extends_placeholder_stub_class; _ } =
          if extends_placeholder_stub_class then
            None
          else
            List.find successors ~f:(GlobalResolution.class_exists global_resolution)
        in
        match metadata >>= superclass with
        | Some superclass ->
            let resolved = Type.Primitive superclass in
            { state; resolved; resolved_annotation = None; base = Some (Super resolved) }
        | None ->
            let { resolved; _ } = forward_expression ~state ~expression:callee in
            forward_callable ~state ~target:None ~callee ~dynamic:false ~resolved ~arguments )
    | Call
        {
          callee = { Node.value = Name (Name.Identifier "type"); _ };
          arguments = [{ Call.Argument.value; _ }];
        } ->
        (* Resolve `type()` calls. *)
        let resolved = resolve_expression_type ~state value |> Type.meta in
        { state; resolved; resolved_annotation = None; base = None }
    | Call
        {
          callee = { Node.location; value = Name (Name.Identifier "reveal_type") };
          arguments = [{ Call.Argument.value; _ }];
        } ->
        (* Special case reveal_type(). *)
        let is_type_alias =
          match Node.value value with
          | Expression.Name _ ->
              let name = Expression.show (delocalize value) in
              GlobalResolution.aliases global_resolution name |> Option.is_some
          | _ -> false
        in
        let annotation =
          let annotation = resolve_expression ~state value in
          if
            (not (Annotation.is_immutable annotation))
            && Type.is_untyped (Annotation.annotation annotation)
            || is_type_alias
          then
            let parsed = GlobalResolution.parse_annotation global_resolution value in
            if Type.is_untyped parsed then annotation else Annotation.create (Type.meta parsed)
          else
            annotation
        in
        let state =
          emit_error ~state ~location ~kind:(Error.RevealedType { expression = value; annotation })
        in
        { state; resolved = Type.none; resolved_annotation = None; base = None }
    | Call
        {
          callee = { Node.location; value = Name name };
          arguments = [{ Call.Argument.value = cast_annotation; _ }; { Call.Argument.value; _ }];
        }
      when Option.equal
             Reference.equal
             (name_to_reference name)
             (Some (Reference.create "typing.cast"))
           || Option.equal
                Reference.equal
                (name_to_reference name)
                (Some (Reference.create "pyre_extensions.safe_cast")) ->
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
                   {
                     missing_annotation =
                       {
                         Error.name = name_to_reference_exn name;
                         annotation = None;
                         given_annotation = Some cast_annotation;
                         evidence_locations = [];
                         thrown_at_source = true;
                       };
                     is_type_alias = false;
                   })
          else if Type.equal cast_annotation resolved then
            emit_error ~state ~location ~kind:(Error.RedundantCast resolved)
          else if
            Reference.equal
              (name_to_reference_exn name)
              (Reference.create "pyre_extensions.safe_cast")
            && GlobalResolution.less_or_equal
                 global_resolution
                 ~left:cast_annotation
                 ~right:resolved
          then
            emit_error
              ~state
              ~location
              ~kind:(Error.UnsafeCast { expression = value; annotation = resolved })
          else
            state
        in
        { state; resolved = cast_annotation; resolved_annotation = None; base = None }
    | Call
        {
          callee = { Node.value = Name (Name.Identifier "isinstance"); _ } as callee;
          arguments =
            [{ Call.Argument.value = expression; _ }; { Call.Argument.value = annotations; _ }] as
            arguments;
        } ->
        let callables =
          let { resolved; _ } = forward_expression ~state ~expression:callee in
          match resolved with
          | Type.Callable callable -> Some [callable]
          | _ -> None
        in
        Context.Builder.add_callee
          ~global_resolution
          ~target:None
          ~callables
          ~arguments
          ~dynamic:false
          ~qualifier:Context.qualifier
          ~callee;

        (* Be angelic and compute errors using the typeshed annotation for isinstance. *)

        (* We special case type inference for `isinstance` in asserted, and the typeshed stubs are
           imprecise (doesn't correctly declare the arguments as a recursive tuple. *)
        let state =
          let { state; _ } = forward_expression ~state ~expression in
          let previous_errors = Map.length state.errors in
          let state, annotations =
            let rec collect_types (state, collected) = function
              | { Node.value = Expression.Tuple annotations; _ } ->
                  let state, new_annotations =
                    List.fold annotations ~init:(state, []) ~f:collect_types
                  in
                  state, new_annotations @ collected
              | expression ->
                  let { state; resolved; _ } = forward_expression ~state ~expression in
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
                     {
                       name = None;
                       position = 2;
                       callee = Some (Reference.create "isinstance");
                       mismatch =
                         {
                           Error.actual = non_meta;
                           expected =
                             Type.union
                               [
                                 Type.meta Type.Any; Type.Tuple (Type.Unbounded (Type.meta Type.Any));
                               ];
                           due_to_invariance = false;
                         };
                     })
            in
            let rec is_compatible annotation =
              match annotation with
              | _ when Type.is_meta annotation -> true
              | Type.Tuple (Type.Unbounded annotation) -> Type.is_meta annotation
              | Type.Tuple (Type.Bounded (Type.OrderedTypes.Concrete annotations)) ->
                  List.for_all ~f:Type.is_meta annotations
              | Type.Union annotations -> List.for_all annotations ~f:is_compatible
              | _ -> false
            in
            List.find annotations ~f:(fun (annotation, _) -> not (is_compatible annotation))
            >>| add_incompatible_non_meta_error state
            |> Option.value ~default:state
        in
        { state; resolved = Type.bool; resolved_annotation = None; base = None }
    | Call
        {
          callee =
            {
              Node.value = Name (Name.Attribute { attribute = "assertIsNotNone" | "assertTrue"; _ });
              _;
            } as callee;
          arguments = [{ Call.Argument.value = expression; _ }] as arguments;
        } ->
        let state = forward_statement ~state ~statement:(Statement.assume expression) in
        let { state; resolved = resolved_callee; _ } =
          forward_expression ~state ~expression:callee
        in
        forward_callable
          ~state
          ~target:None
          ~dynamic:true
          ~callee
          ~resolved:resolved_callee
          ~arguments
    | Call
        {
          callee =
            { Node.value = Name (Name.Attribute { attribute = "assertFalse"; _ }); _ } as callee;
          arguments = [{ Call.Argument.value = expression; _ }] as arguments;
        } ->
        let state = forward_statement ~state ~statement:(Statement.assume (negate expression)) in
        let { state; resolved = resolved_callee; _ } =
          forward_expression ~state ~expression:callee
        in
        forward_callable
          ~state
          ~target:None
          ~dynamic:true
          ~callee
          ~resolved:resolved_callee
          ~arguments
    | Call call ->
        let { Call.callee; arguments } = AnnotatedCall.redirect_special_calls ~resolution call in
        let { state = { errors = callee_errors; _ }; resolved = resolved_callee; base; _ } =
          forward_expression ~state:{ state with errors = ErrorMap.Map.empty } ~expression:callee
        in
        let { state = { errors = updated_errors; _ } as updated_state; resolved; _ } =
          let target_and_dynamic resolved_callee =
            if Type.is_meta resolved_callee then
              Some (Type.single_parameter resolved_callee), false
            else
              match base with
              | Some (Instance resolved) when not (Type.is_top resolved) -> Some resolved, true
              | Some (Class resolved) when not (Type.is_top resolved) -> Some resolved, false
              | Some (Super resolved) when not (Type.is_top resolved) -> Some resolved, false
              | _ -> None, false
          in
          match resolved_callee with
          | Type.Parametric { name = "type"; parameters = [Single (Type.Union resolved_callees)] }
            ->
              let forward_inner_callable (state, annotations) inner_resolved_callee =
                let target, dynamic = target_and_dynamic inner_resolved_callee in
                forward_callable
                  ~state
                  ~target
                  ~dynamic
                  ~callee
                  ~resolved:inner_resolved_callee
                  ~arguments
                |> fun { state; resolved; _ } -> state, resolved :: annotations
              in
              let state, return_annotations =
                List.fold_left
                  ~f:forward_inner_callable
                  ~init:(state, [])
                  (List.map ~f:Type.meta resolved_callees)
              in
              {
                state;
                resolved = Type.union return_annotations;
                resolved_annotation = None;
                base = None;
              }
          | _ ->
              let target, dynamic = target_and_dynamic resolved_callee in
              forward_callable ~state ~target ~dynamic ~callee ~resolved:resolved_callee ~arguments
        in
        if
          Map.is_empty (Map.filter ~f:is_terminating_error callee_errors)
          || not (Type.is_top resolved_callee || Type.is_undeclared resolved_callee)
        then
          let errors =
            Map.merge_skewed
              ~combine:(fun ~key:_ left right ->
                Error.join ~resolution:global_resolution left right)
              callee_errors
              updated_errors
          in
          {
            state = { updated_state with errors };
            resolved;
            resolved_annotation = None;
            base = None;
          }
        else (* Do not throw more errors if callee already contains terminating error. *)
          let errors =
            Map.fold callee_errors ~init:state.errors ~f:(fun ~key:_ ~data errors ->
                ErrorMap.add ~errors data)
          in
          { state = { state with errors }; resolved; resolved_annotation = None; base = None }
    | ComparisonOperator { ComparisonOperator.left; right; operator = ComparisonOperator.In }
    | ComparisonOperator { ComparisonOperator.left; right; operator = ComparisonOperator.NotIn } ->
        let resolve_in_call
            (state, joined_annotation)
            { Type.instantiated; class_name; class_attributes }
          =
          let resolve_method
              ?(class_attributes = false)
              ?(special_method = false)
              class_name
              instantiated
              name
            =
            GlobalResolution.attribute_from_class_name
              ~transitive:true
              ~class_attributes
              class_name
              ~resolution:global_resolution
              ~special_method
              ~name
              ~instantiated
            >>| Annotated.Attribute.annotation
            >>| Annotation.annotation
            >>= function
            | Type.Top -> None
            | annotation -> Some annotation
          in
          let { state; resolved; _ } =
            match
              resolve_method
                ~class_attributes
                ~special_method:true
                class_name
                instantiated
                "__contains__"
            with
            | Some resolved ->
                let callee =
                  {
                    Node.location;
                    value =
                      Expression.Name
                        (Name.Attribute { base = right; attribute = "__contains__"; special = true });
                  }
                in
                forward_callable
                  ~state
                  ~target:(Some instantiated)
                  ~dynamic:true
                  ~callee
                  ~resolved
                  ~arguments:[{ Call.Argument.name = None; value = left }]
            | None -> (
                match
                  resolve_method
                    ~class_attributes
                    ~special_method:true
                    class_name
                    instantiated
                    "__iter__"
                with
                | Some iter_callable ->
                    let forward_callable =
                      let callee =
                        {
                          Node.location;
                          value =
                            Expression.Name
                              (Name.Attribute
                                 { base = right; attribute = "__iter__"; special = true });
                        }
                      in
                      forward_callable ~dynamic:true ~callee
                    in
                    (* Since we can't call forward_expression with the general type (we don't have a
                       good way of saying "synthetic expression with type T", simulate what happens
                       ourselves. *)
                    let forward_method ~method_name ~arguments { state; resolved = parent; _ } =
                      Type.split parent
                      |> fst
                      |> Type.primitive_name
                      >>= (fun class_name -> resolve_method class_name parent method_name)
                      >>| fun callable ->
                      forward_callable
                        ~state
                        ~target:(Some parent)
                        ~resolved:callable
                        ~arguments:
                          (List.map arguments ~f:(fun value -> { Call.Argument.name = None; value }))
                    in
                    forward_callable
                      ~state
                      ~target:(Some instantiated)
                      ~resolved:iter_callable
                      ~arguments:[]
                    |> forward_method ~method_name:"__next__" ~arguments:[]
                    >>= forward_method ~method_name:"__eq__" ~arguments:[left]
                    |> Option.value
                         ~default:
                           { state; resolved = Type.Top; resolved_annotation = None; base = None }
                | None ->
                    let call =
                      let getitem =
                        {
                          Node.location;
                          value =
                            Expression.Call
                              {
                                callee =
                                  {
                                    Node.location;
                                    value =
                                      Name
                                        (Name.Attribute
                                           {
                                             base = right;
                                             attribute = "__getitem__";
                                             special = true;
                                           });
                                  };
                                arguments =
                                  [
                                    {
                                      Call.Argument.name = None;
                                      value = { Node.location; value = Expression.Integer 0 };
                                    };
                                  ];
                              };
                        }
                      in
                      {
                        Node.location;
                        value =
                          Expression.Call
                            {
                              callee =
                                {
                                  Node.location;
                                  value =
                                    Name
                                      (Name.Attribute
                                         { base = getitem; attribute = "__eq__"; special = true });
                                };
                              arguments = [{ Call.Argument.name = None; value = left }];
                            };
                      }
                    in
                    forward_expression ~state ~expression:call )
          in
          state, GlobalResolution.join global_resolution joined_annotation resolved
        in
        let { state; resolved; _ } = forward_expression ~state ~expression:right in
        let state, resolved =
          Type.resolve_class resolved
          >>| List.fold ~f:resolve_in_call ~init:(state, Type.Bottom)
          |> Option.value ~default:(state, Type.Bottom)
        in
        let resolved = if Type.equal resolved Type.Bottom then Type.Top else resolved in
        { state; resolved; resolved_annotation = None; base = None }
    | ComparisonOperator ({ ComparisonOperator.left; right; _ } as operator) -> (
        match ComparisonOperator.override operator with
        | Some expression -> forward_expression ~state ~expression
        | None ->
            forward_expression ~state ~expression:left
            |> (fun { state; _ } -> forward_expression ~state ~expression:right)
            |> fun state ->
            { state with resolved = Type.bool; resolved_annotation = None; base = None } )
    | Complex _ -> { state; resolved = Type.complex; resolved_annotation = None; base = None }
    | Dictionary { Dictionary.entries; keywords } ->
        let key, value, state =
          let forward_entry (key, value, state) entry =
            let new_key, new_value, state = forward_entry ~state ~entry in
            ( GlobalResolution.join global_resolution key new_key,
              GlobalResolution.join global_resolution value new_value,
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
            let { state; resolved = keyword_resolved; _ } =
              forward_expression ~state ~expression:keyword
            in
            GlobalResolution.join global_resolution resolved keyword_resolved, state
          in
          List.fold keywords ~f:forward_keyword ~init:(Type.dictionary ~key ~value, state)
        in
        { state; resolved; resolved_annotation = None; base = None }
    | DictionaryComprehension { Comprehension.element; generators } ->
        let key, value, state =
          List.fold
            generators
            ~f:(fun state generator -> forward_generator ~state ~generator)
            ~init:state
          |> fun state -> forward_entry ~state ~entry:element
        in
        (* Discard generator-local variables. *)
        {
          state = { state with resolution };
          resolved = Type.dictionary ~key ~value;
          resolved_annotation = None;
          base = None;
        }
    | Ellipsis -> { state; resolved = Type.Any; resolved_annotation = None; base = None }
    | False ->
        {
          state;
          resolved = Type.Literal (Type.Boolean false);
          resolved_annotation = None;
          base = None;
        }
    | Float _ -> { state; resolved = Type.float; resolved_annotation = None; base = None }
    | Generator { Comprehension.element; generators } ->
        let { state; resolved; _ } = forward_comprehension ~element ~generators in
        { state; resolved = Type.generator resolved; resolved_annotation = None; base = None }
    | Integer literal ->
        { state; resolved = Type.literal_integer literal; resolved_annotation = None; base = None }
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
        let { state; resolved; _ } =
          forward_expression
            ~state:{ state with resolution = resolution_with_parameters }
            ~expression:body
        in
        (* Judgement call, many more people want to pass in `lambda: 0` to `defaultdict` than want
           to write a function that take in callables with literal return types. If you really want
           that behavior you can always write a real inner function with a literal return type *)
        let resolved = Type.weaken_literals resolved in
        let create_parameter { Node.value = { Parameter.name; value; _ }; _ } =
          { Type.Callable.Parameter.name; annotation = Type.Any; default = Option.is_some value }
        in
        let parameters =
          List.map parameters ~f:create_parameter
          |> Type.Callable.Parameter.create
          |> fun parameters -> Type.Callable.Defined parameters
        in
        {
          state = { state with resolution };
          resolved = Type.Callable.create ~parameters ~annotation:resolved ();
          resolved_annotation = None;
          base = None;
        }
    | List elements ->
        let { state; resolved; _ } = forward_elements ~state ~elements in
        { state; resolved = Type.list resolved; resolved_annotation = None; base = None }
    | ListComprehension { Comprehension.element; generators } ->
        let { state; resolved; _ } = forward_comprehension ~element ~generators in
        { state; resolved = Type.list resolved; resolved_annotation = None; base = None }
    | Name (Name.Identifier identifier) -> forward_reference ~state (Reference.create identifier)
    | Name (Name.Attribute { base; attribute; special } as name) ->
        let reference = name_to_reference name in
        let { state = { errors = base_errors; _ }; resolved = resolved_base; base = super_base; _ } =
          forward_expression ~state:{ state with errors = ErrorMap.Map.empty } ~expression:base
        in
        let ({ errors; _ } as state), resolved_base =
          if Type.Variable.contains_escaped_free_variable resolved_base then
            let state =
              Error.IncompleteType
                {
                  target = base;
                  annotation = resolved_base;
                  attempted_action = Error.AttributeAccess attribute;
                }
              |> (fun kind ->
                   Error.create
                     ~location:(Location.with_module ~qualifier:Context.qualifier location)
                     ~kind
                     ~define:Context.define)
              |> emit_raw_error ~state
            in
            state, Type.Variable.convert_all_escaped_free_variables_to_anys resolved_base
          else
            state, resolved_base
        in
        let {
          state = { errors = updated_errors; _ } as updated_state;
          resolved;
          resolved_annotation;
          _;
        }
          =
          if Type.is_undeclared resolved_base then
            let state =
              reference
              >>| (fun reference -> Error.UndefinedName reference)
              >>| (fun kind -> emit_error ~state ~location ~kind)
              |> Option.value ~default:state
            in
            { state; resolved = resolved_base; resolved_annotation = None; base = None }
          else if Type.equal resolved_base Type.Top then (* Global or local. *)
            reference
            >>| forward_reference ~state
            |> Option.value
                 ~default:{ state; resolved = Type.Top; resolved_annotation = None; base = None }
          else if Type.is_callable resolved_base then (* Nested function. *)
            let resolved =
              reference >>= fun reference -> Resolution.get_local resolution ~reference
            in
            match resolved with
            | Some annotation ->
                {
                  state;
                  resolved = Annotation.annotation annotation;
                  resolved_annotation = Some annotation;
                  base = None;
                }
            | None ->
                let state =
                  let name =
                    match resolved_base with
                    | Type.Callable { Type.Callable.kind = Named name; _ } -> Some name
                    | _ -> None
                  in
                  Error.UndefinedAttribute { attribute; origin = Error.Callable name }
                  |> (fun kind ->
                       Error.create
                         ~location:(Location.with_module ~qualifier:Context.qualifier location)
                         ~kind
                         ~define:Context.define)
                  |> emit_raw_error ~state
                in
                { state; resolved = Type.Top; resolved_annotation = None; base = None }
          else (* Attribute access. *)
            let find_attribute ({ Type.instantiated; class_attributes; class_name } as resolved) =
              let name = attribute in
              match
                GlobalResolution.attribute_from_class_name
                  class_name
                  ~transitive:(not (is_private_attribute attribute))
                  ~class_attributes
                  ~special_method:special
                  ~resolution:global_resolution
                  ~name
                  ~instantiated
              with
              | Some attribute ->
                  let attribute =
                    if not (Annotated.Attribute.defined attribute) then
                      Annotated.Class.fallback_attribute class_name ~resolution ~name
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
                  (* Collect @property's in the call graph. *)
                  Some
                    ( resolved,
                      (attribute, undefined_target),
                      Annotated.Attribute.annotation attribute )
              | None -> None
            in
            match
              Type.resolve_class resolved_base >>| List.map ~f:find_attribute >>= Option.all
            with
            | None ->
                let state =
                  Error.UndefinedAttribute
                    {
                      attribute;
                      origin = Error.Class { annotation = resolved_base; class_attribute = false };
                    }
                  |> (fun kind ->
                       Error.create
                         ~location:(Location.with_module ~qualifier:Context.qualifier location)
                         ~kind
                         ~define:Context.define)
                  |> emit_raw_error ~state
                in
                { state; resolved = Type.Top; resolved_annotation = None; base = None }
            | Some [] -> { state; resolved = Type.Top; resolved_annotation = None; base = None }
            | Some (head :: tail) ->
                let name = attribute in
                let head_resolved_class, head_definition, head_resolved = head in
                let tail_resolved_classes, tail_definitions, tail_resolveds = List.unzip3 tail in
                begin
                  let attributes =
                    List.map (head_definition :: tail_definitions) ~f:fst
                    |> fun definitions ->
                    List.zip_exn
                      definitions
                      (List.map
                         (head_resolved_class :: tail_resolved_classes)
                         ~f:(fun { Type.instantiated; _ } -> instantiated))
                  in
                  Context.Builder.add_property_callees
                    ~global_resolution
                    ~resolved_base
                    ~attributes
                    ~location
                    ~qualifier:Context.qualifier
                    ~name
                end;
                let state =
                  let definition =
                    List.find (head_definition :: tail_definitions) ~f:(fun (_, undefined_target) ->
                        match undefined_target with
                        | None -> false
                        | _ -> true)
                    |> Option.value ~default:head_definition
                  in
                  match reference, definition with
                  | Some reference, (_, Some target) when Type.equal Type.undeclared target ->
                      Error.UndefinedName reference
                      |> (fun kind ->
                           Error.create
                             ~location:(Location.with_module ~qualifier:Context.qualifier location)
                             ~kind
                             ~define:Context.define)
                      |> emit_raw_error ~state
                  | _, (attribute, Some target) ->
                      if Option.is_some (inverse_operator name) then
                        (* Defer any missing attribute error until the inverse operator has been
                           typechecked. *)
                        state
                      else
                        Error.UndefinedAttribute
                          {
                            attribute = name;
                            origin =
                              Error.Class
                                {
                                  annotation = target;
                                  class_attribute = Annotated.Attribute.class_attribute attribute;
                                };
                          }
                        |> (fun kind ->
                             Error.create
                               ~location:
                                 (Location.with_module ~qualifier:Context.qualifier location)
                               ~kind
                               ~define:Context.define)
                        |> emit_raw_error ~state
                  | _ ->
                      let enclosing_class_reference =
                        let open Annotated in
                        Define.parent_definition
                          ~resolution:(Resolution.global_resolution resolution)
                          (Define.create Context.define)
                        >>= fun definition -> Some (AnnotatedClass.name definition)
                      in
                      let base_class =
                        if Type.is_meta resolved_base then
                          Type.class_name (Type.single_parameter resolved_base)
                        else
                          Type.class_name resolved_base
                      in
                      let is_accessed_in_base_class =
                        Option.value_map
                          ~default:false
                          ~f:(Reference.equal_sanitized base_class)
                          enclosing_class_reference
                      in
                      if is_private_attribute attribute && not is_accessed_in_base_class then
                        Error.UndefinedAttribute
                          {
                            attribute = name;
                            origin =
                              Error.Class { annotation = resolved_base; class_attribute = false };
                          }
                        |> (fun kind ->
                             Error.create
                               ~location:
                                 (Location.with_module ~qualifier:Context.qualifier location)
                               ~kind
                               ~define:Context.define)
                        |> emit_raw_error ~state
                      else
                        state
                in
                let resolved =
                  let apply_global_override resolved =
                    let annotation =
                      reference
                      >>= fun reference ->
                      Resolution.get_local_with_attributes
                        resolution
                        ~name:(create_name_from_reference ~location:Location.any reference)
                        ~global_fallback:(Type.is_meta (Annotation.annotation resolved))
                    in
                    match annotation with
                    | Some local -> local
                    | None -> resolved
                  in
                  let join sofar element =
                    let refined =
                      RefinementUnit.join
                        ~global_resolution
                        (RefinementUnit.create ~base:sofar ())
                        (RefinementUnit.create ~base:element ())
                      |> RefinementUnit.base
                      |> Option.value ~default:(Annotation.create Type.Bottom)
                    in
                    { refined with annotation = Type.union [sofar.annotation; element.annotation] }
                  in
                  List.fold tail_resolveds ~init:head_resolved ~f:join |> apply_global_override
                in
                {
                  state;
                  resolved = Annotation.annotation resolved;
                  resolved_annotation = Some resolved;
                  base = None;
                }
        in
        let base =
          match super_base with
          | Some (Super _) -> super_base
          | _ ->
              let is_global_meta =
                let is_global () =
                  match base with
                  | { Node.value = Name name; _ } ->
                      name_to_identifiers name
                      >>| Reference.create_from_list
                      >>= GlobalResolution.global global_resolution
                      |> Option.is_some
                  | _ -> false
                in
                Type.is_meta resolved_base && is_global ()
              in
              if is_global_meta then
                Some (Class resolved_base)
              else
                Some (Instance resolved_base)
        in
        if
          Map.is_empty (Map.filter ~f:is_terminating_error base_errors)
          || not (Type.is_top resolved_base || Type.is_undeclared resolved_base)
        then
          let errors =
            Map.fold base_errors ~init:updated_errors ~f:(fun ~key:_ ~data errors ->
                ErrorMap.add ~errors data)
          in
          { state = { updated_state with errors }; resolved; resolved_annotation; base }
        else (* Do not throw more errors if base already contains terminating error. *)
          let errors =
            Map.fold base_errors ~init:errors ~f:(fun ~key:_ ~data errors ->
                ErrorMap.add ~errors data)
          in
          { state = { state with errors }; resolved; resolved_annotation; base }
    | Set elements ->
        let { state; resolved; _ } = forward_elements ~state ~elements in
        { state; resolved = Type.set resolved; resolved_annotation = None; base = None }
    | SetComprehension { Comprehension.element; generators } ->
        let { state; resolved; _ } = forward_comprehension ~element ~generators in
        { state; resolved = Type.set resolved; resolved_annotation = None; base = None }
    | Starred starred ->
        let state =
          match starred with
          | Starred.Once expression
          | Starred.Twice expression ->
              forward_expression ~state ~expression
        in
        { state with resolved = Type.Top; resolved_annotation = None; base = None }
    | String { StringLiteral.kind = StringLiteral.Format expressions; _ } ->
        let state =
          List.fold
            expressions
            ~f:(fun state expression ->
              forward_expression ~state ~expression |> fun { state; _ } -> state)
            ~init:state
        in
        { state; resolved = Type.string; resolved_annotation = None; base = None }
    | String { StringLiteral.kind = StringLiteral.Bytes; _ } ->
        { state; resolved = Type.bytes; resolved_annotation = None; base = None }
    | String { StringLiteral.kind = StringLiteral.String; value } ->
        { state; resolved = Type.literal_string value; resolved_annotation = None; base = None }
    | String { StringLiteral.kind = StringLiteral.Mixed _; _ } ->
        (* NOTE: We may run into this case with nested f-strings. Treat them as literal strings
           until the parser gets full support of them. *)
        { state; resolved = Type.string; resolved_annotation = None; base = None }
    | Ternary { Ternary.target; test; alternative } ->
        let state = { state with resolution } in
        let target =
          forward_statement ~state ~statement:(Statement.assume test)
          |> fun state -> forward_expression ~state ~expression:target
        in
        let alternative =
          forward_statement ~state ~statement:(Statement.assume (negate test))
          |> fun state -> forward_expression ~state ~expression:alternative
        in
        let { state; resolved; _ } = join_resolved target alternative in
        (* The resolution is local to the ternary expression and should not be propagated out. *)
        { state = { state with resolution }; resolved; resolved_annotation = None; base = None }
    | True ->
        {
          state;
          resolved = Type.Literal (Type.Boolean true);
          resolved_annotation = None;
          base = None;
        }
    | Tuple elements ->
        let state, resolved =
          let forward_element (state, resolved) expression =
            let { state; resolved = new_resolved; _ } = forward_expression ~state ~expression in
            state, new_resolved :: resolved
          in
          List.fold elements ~f:forward_element ~init:(state, [])
        in
        {
          state;
          resolved = Type.tuple (List.rev resolved);
          resolved_annotation = None;
          base = None;
        }
    | UnaryOperator ({ UnaryOperator.operand; _ } as operator) -> (
        match UnaryOperator.override operator with
        | Some expression -> forward_expression ~state ~expression
        | None ->
            let state = forward_expression ~state ~expression:operand in
            { state with resolved = Type.bool; resolved_annotation = None; base = None } )
    | WalrusOperator { value; _ } ->
        (* TODO(T53600647): Bind a type to the target. *)
        forward_expression ~state ~expression:value
    | Expression.Yield (Some expression) ->
        let { state; resolved; _ } = forward_expression ~state ~expression in
        { state; resolved = Type.generator resolved; resolved_annotation = None; base = None }
    | Expression.Yield None ->
        { state; resolved = Type.generator Type.none; resolved_annotation = None; base = None }


  and forward_statement
      ~state:({ resolution; _ } as state)
      ~statement:({ Node.location; value } as statement)
    =
    let global_resolution = Resolution.global_resolution resolution in
    let {
      Node.location = define_location;
      value =
        {
          Define.signature =
            {
              async;
              parent = define_parent;
              return_annotation = return_annotation_expression;
              generator;
              _;
            } as signature;
          body;
          _;
        } as define;
    }
      =
      Context.define
    in
    let instantiate location =
      let ast_environment = GlobalResolution.ast_environment global_resolution in
      let location = Location.with_module ~qualifier:Context.qualifier location in
      Location.WithModule.instantiate
        ~lookup:(AstEnvironment.ReadOnly.get_relative ast_environment)
        location
    in
    (* We weaken type inference of mutable literals for assignments and returns to get around the
       invariance of containers when we can prove that casting to a supertype is safe. *)
    let validate_return ~expression ~state ~actual ~is_implicit =
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
          ~resolve:(resolve_expression_type ~state)
          ~expression
          ~resolved:actual
          ~expected:return_annotation
      in
      let check_incompatible_return state =
        if
          Define.has_return_annotation define
          && (not
                (GlobalResolution.constraints_solution_exists
                   global_resolution
                   ~left:actual
                   ~right:return_annotation))
          && (not (Define.is_abstract_method define))
          && (not (Define.is_overloaded_function define))
          && (not (Type.is_none actual && generator))
          && not (Type.is_none actual && Type.is_noreturn return_annotation)
        then
          let rec check_unimplemented = function
            | [
                { Node.value = Statement.Pass; _ };
                { Node.value = Statement.Return { Return.expression = None; _ }; _ };
              ] ->
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
                 {
                   mismatch =
                     Error.create_mismatch
                       ~resolution:global_resolution
                       ~actual
                       ~expected:return_annotation
                       ~covariant:true;
                   is_implicit;
                   is_unimplemented = check_unimplemented body;
                   define_location;
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
          || (contains_literal_any && Type.contains_prohibited_any return_annotation)
        then
          let given_annotation =
            Option.some_if (Define.has_return_annotation define) return_annotation
          in
          emit_error
            ~state
            ~location:define_location
            ~kind:
              (Error.MissingReturnAnnotation
                 {
                   name = Reference.create "$return_annotation";
                   annotation = Some actual;
                   given_annotation;
                   evidence_locations = [instantiate location];
                   thrown_at_source = true;
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
          >>| (fun annotation -> Type.final_value annotation |> Option.value ~default:annotation)
          >>| fun annotation ->
          Type.class_variable_value annotation |> Option.value ~default:annotation
        in
        let parsed =
          GlobalResolution.parse_annotation
            ~allow_untracked:true
            ~allow_invalid_type_parameters:true
            global_resolution
            value
        in
        let is_type_alias =
          let value_is_type =
            (* Consider non-locals with a RHS that is a type to be an alias. *)
            let is_type_value = function
              | Type.Top ->
                  Option.is_some
                    (Type.Variable.parse_declaration value ~target:(Reference.create ""))
              | Type.Optional Type.Bottom -> false
              | annotation -> not (GlobalResolution.contains_untracked global_resolution annotation)
            in
            match Node.value value with
            | Expression.String _ -> false
            | Expression.Name name when is_simple_name name ->
                let local =
                  Resolution.get_local
                    ~global_fallback:false
                    ~reference:(name_to_reference_exn name)
                    resolution
                in
                is_type_value parsed && not (Option.is_some local)
            | _ -> is_type_value parsed
          in
          match original_annotation with
          | None -> value_is_type
          | Some annotation ->
              (Type.is_type_alias annotation && Define.is_toplevel define)
              || Type.is_meta annotation
                 && Type.is_typed_dictionary (Type.single_parameter annotation)
        in
        (* Ensure that we actually visit the target and resolve any property calls. *)
        let _ = forward_expression ~state ~expression:target in
        let state, resolved =
          let { state = { resolution; _ } as new_state; resolved; _ } =
            forward_expression ~state ~expression:value
          in
          let resolved = Type.remove_undeclared resolved in
          (* TODO(T35601774): We need to suppress subscript related errors on generic classes. *)
          if is_type_alias then
            let add_annotation_errors errors =
              add_invalid_type_parameters_errors
                ~resolution:global_resolution
                ~location
                ~errors
                parsed
              |> fst
              |> fun errors ->
              add_untracked_annotation_errors ~resolution:global_resolution ~location ~errors parsed
              |> fst
            in
            let add_type_variable_errors errors =
              match parsed with
              | Variable variable when Type.Variable.Unary.contains_subvariable variable ->
                  let kind =
                    AnalysisError.InvalidType
                      (AnalysisError.NestedTypeVariables (Type.Variable.Unary variable))
                  in
                  Error.create
                    ~location:(Location.with_module ~qualifier:Context.qualifier location)
                    ~kind
                    ~define:Context.define
                  |> ErrorMap.add ~errors
              | _ -> errors
            in
            let errors = state.errors |> add_annotation_errors |> add_type_variable_errors in
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
          let is_uniform_sequence annotation =
            match annotation with
            | Type.Tuple (Type.Unbounded _) -> true
            (* Bounded tuples subclass iterable, but should be handled in the nonuniform case. *)
            | Type.Tuple (Type.Bounded _) -> false
            | _ ->
                (not (NamedTuple.is_named_tuple ~global_resolution ~annotation))
                && GlobalResolution.less_or_equal
                     global_resolution
                     ~left:annotation
                     ~right:(Type.iterable Type.Top)
          in
          let uniform_sequence_parameter annotation =
            match annotation with
            | Type.Tuple (Type.Unbounded parameter) -> parameter
            | _ -> (
                (* TODO (T56720048): Stop joining with iterable bottom *)
                GlobalResolution.join global_resolution annotation (Type.iterable Type.Bottom)
                |> function
                | Type.Parametric { parameters = [Single parameter]; _ } -> parameter
                | _ -> Type.Top )
          in
          let nonuniform_sequence_parameters annotation =
            match annotation with
            | Type.Tuple (Type.Bounded (Concrete parameters)) -> Some parameters
            | annotation when NamedTuple.is_named_tuple ~global_resolution ~annotation ->
                NamedTuple.field_annotations ~global_resolution annotation
            | _ -> None
          in
          match target_value with
          | Expression.Name name ->
              let target_annotation = resolve_expression ~state target in
              let reference, attribute, resolved_base =
                match name with
                | Name.Identifier identifier -> Some (Reference.create identifier), None, None
                | Name.Attribute { base; attribute; _ } ->
                    let name = attribute in
                    let resolved = resolve_expression_type ~state base in
                    let parent, class_attributes =
                      if Type.is_meta resolved then
                        Type.single_parameter resolved, true
                      else
                        resolved, false
                    in
                    let parent_class_name = Type.split parent |> fst |> Type.primitive_name in
                    let reference =
                      match base with
                      | { Node.value = Name name; _ } when is_simple_name name ->
                          Some (Reference.create ~prefix:(name_to_reference_exn name) attribute)
                      | _ ->
                          parent_class_name
                          >>| Reference.create
                          >>| fun prefix -> Reference.create ~prefix attribute
                    in
                    let attribute =
                      parent_class_name
                      >>= GlobalResolution.attribute_from_class_name
                            ~resolution:global_resolution
                            ~name:attribute
                            ~instantiated:parent
                            ~transitive:true
                            ~class_attributes
                      >>| fun annotated -> annotated, attribute
                    in
                    begin
                      match attribute with
                      | Some (attribute, _)
                        when AnnotatedAttribute.property attribute
                             && AnnotatedAttribute.visibility attribute
                                = AnnotatedAttribute.ReadWrite ->
                          Context.Builder.add_property_setter_callees
                            ~attribute
                            ~instantiated_parent:parent
                            ~name
                            ~location:(Location.with_module ~qualifier:Context.qualifier location)
                      | _ -> ()
                    end;
                    reference, attribute, Some resolved
              in
              let is_undefined_attribute parent =
                (* Check if __setattr__ method is defined to accept value of type `Any` *)
                let is_setattr_any_defined =
                  let attribute =
                    match Type.resolve_class parent with
                    | Some [{ instantiated; class_name; _ }] ->
                        GlobalResolution.attribute_from_class_name
                          class_name
                          ~class_attributes:false
                          ~transitive:false
                          ~resolution:global_resolution
                          ~name:"__setattr__"
                          ~instantiated
                    | _ -> None
                  in
                  match attribute with
                  | Some attribute when Annotated.Attribute.defined attribute -> (
                      match Annotated.Attribute.annotation attribute |> Annotation.annotation with
                      | Type.Callable
                          {
                            implementation =
                              {
                                Type.Callable.parameters =
                                  Type.Callable.Defined (_ :: value_parameter :: _);
                                _;
                              };
                            _;
                          } ->
                          Type.Callable.Parameter.annotation value_parameter
                          |> Option.value_map ~default:false ~f:Type.is_any
                      | _ -> false )
                  | _ -> false
                in
                not is_setattr_any_defined
              in
              let state =
                match reference with
                | Some reference ->
                    let check_final_reassignment state =
                      let error () =
                        emit_error
                          ~state
                          ~location
                          ~kind:(Error.InvalidAssignment (FinalAttribute reference))
                      in
                      let read_only_non_property_attribute =
                        let open AnnotatedAttribute in
                        let relevant_properties attribute =
                          visibility attribute, property attribute, initialized attribute
                        in
                        match attribute >>| fst >>| relevant_properties with
                        | Some (ReadOnly _, false, Implicitly) when Define.is_constructor define ->
                            false
                        | Some (ReadOnly _, false, _) -> true
                        | _ -> false
                      in
                      if read_only_non_property_attribute && Option.is_none original_annotation then
                        error ()
                      else if Option.is_none attribute && Annotation.is_final target_annotation then
                        error ()
                      else
                        state
                    in
                    let check_assign_class_variable_on_instance state =
                      match
                        ( resolved_base,
                          attribute >>| fst >>| Annotated.Attribute.class_attribute,
                          attribute >>| fst >>| Annotated.Attribute.name )
                      with
                      | Some parent, Some true, Some class_variable
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
                      match
                        ( attribute >>| fst >>| Annotated.Attribute.visibility,
                          attribute >>| fst >>| Annotated.Attribute.property )
                      with
                      | Some (ReadOnly _), Some true when Option.is_none original_annotation ->
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
                          if is_undefined_attribute parent then
                            emit_error
                              ~state
                              ~location
                              ~kind:
                                (Error.UndefinedAttribute
                                   {
                                     attribute = name;
                                     origin =
                                       Error.Class
                                         {
                                           annotation = parent;
                                           class_attribute = Type.is_meta resolved;
                                         };
                                   })
                          else
                            state
                      | _ -> state
                    in
                    let check_nested_explicit_type_alias state =
                      match name, original_annotation with
                      | Name.Identifier identifier, Some annotation
                        when Type.is_type_alias annotation && not (Define.is_toplevel define) ->
                          emit_error
                            ~state
                            ~location
                            ~kind:(Error.InvalidType (NestedAlias identifier))
                      | _ -> state
                    in
                    check_final_reassignment state
                    |> check_assign_class_variable_on_instance
                    |> check_final_is_outermost_qualifier
                    |> check_is_readonly_property
                    |> check_undefined_attribute_target
                    |> check_nested_explicit_type_alias
                | _ -> state
              in
              let expected, is_immutable =
                match original_annotation, target_annotation with
                | Some original, _ when not (Type.is_type_alias original) -> original, true
                | _, target_annotation when Annotation.is_immutable target_annotation ->
                    Annotation.original target_annotation, true
                | _ -> Type.Top, false
              in
              let resolved =
                GlobalResolution.resolve_mutable_literals
                  global_resolution
                  ~resolve:(resolve_expression_type ~state)
                  ~expression
                  ~resolved
                  ~expected
              in
              let is_typed_dictionary_initialization =
                (* Special-casing to avoid throwing errors *)
                let open Type in
                match expected with
                | Parametric { name = "type"; parameters = [Single parameter] }
                  when is_typed_dictionary parameter ->
                    is_unknown resolved
                | _ -> false
              in
              let state =
                let resolved =
                  match resolved with
                  | Type.Parametric _ -> Type.weaken_literals resolved
                  | _ -> resolved
                in
                let is_valid_enumeration_assignment =
                  let parent_annotation =
                    match parent with
                    | None -> Type.Top
                    | Some reference -> Type.Primitive (Reference.show reference)
                  in
                  let compatible =
                    if explicit then
                      GlobalResolution.less_or_equal
                        global_resolution
                        ~left:expected
                        ~right:resolved
                    else
                      true
                  in
                  GlobalResolution.less_or_equal
                    global_resolution
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
                        (GlobalResolution.constraints_solution_exists
                           global_resolution
                           ~left:resolved
                           ~right:expected))
                  && (not is_typed_dictionary_initialization)
                  && (not is_valid_enumeration_assignment)
                  && not (Annotation.is_final target_annotation)
                in
                let open Annotated in
                match attribute, reference with
                | Some (attribute, name), _ when is_incompatible ->
                    Error.IncompatibleAttributeType
                      {
                        parent = Primitive (Attribute.parent attribute);
                        incompatible_type =
                          {
                            Error.name = Reference.create name;
                            mismatch =
                              Error.create_mismatch
                                ~resolution:global_resolution
                                ~actual:resolved
                                ~expected
                                ~covariant:true;
                          };
                      }
                    |> fun kind -> emit_error ~state ~location ~kind
                | _, Some reference when is_incompatible ->
                    Error.IncompatibleVariableType
                      {
                        incompatible_type =
                          {
                            Error.name = reference;
                            mismatch =
                              Error.create_mismatch
                                ~resolution:global_resolution
                                ~actual:resolved
                                ~expected
                                ~covariant:true;
                          };
                        declare_location = instantiate location;
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
                      when String.equal (Identifier.sanitized self) "self" ->
                        let sanitized =
                          Ast.Transform.sanitize_expression value |> Expression.show
                        in
                        is_immutable
                        && (not (Type.is_unknown expected))
                        && ( String.equal attribute sanitized
                           || String.equal attribute ("_" ^ sanitized) )
                    | _ -> false
                  in
                  match annotation with
                  | Some annotation when Type.expression_contains_any annotation ->
                      original_annotation
                      >>| Type.contains_prohibited_any
                      |> Option.value ~default:false
                      |> fun insufficient -> insufficient, true
                  | None when is_immutable && not is_reassignment ->
                      let is_toplevel =
                        Define.is_toplevel define
                        || Define.is_class_toplevel define
                        || Define.is_constructor define
                      in
                      ( Type.equal expected Type.Top || Type.contains_prohibited_any expected,
                        is_toplevel )
                  | _ -> false, false
                in
                let actual_annotation, evidence_locations =
                  if Type.equal resolved Type.Top then
                    None, []
                  else
                    Some resolved, [instantiate location]
                in
                let is_illegal_attribute_annotation attribute =
                  let attribute_parent = AnnotatedAttribute.parent attribute in
                  let parent_annotation =
                    match define_parent with
                    | None -> Type.Top
                    | Some reference -> Type.Primitive (Reference.show reference)
                  in
                  explicit && not (Type.equal parent_annotation (Primitive attribute_parent))
                in
                let parent_class =
                  match name with
                  | Name.Attribute { base; _ } ->
                      resolve_expression_type ~state base |> Type.resolve_class
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
                        |> GlobalResolution.global_location global_resolution
                        |> Option.value ~default:location
                      in
                      Error.create
                        ~location:
                          (Location.with_module ~qualifier:Context.qualifier global_location)
                        ~kind:
                          (Error.MissingGlobalAnnotation
                             {
                               Error.name = reference;
                               annotation = actual_annotation;
                               given_annotation = Option.some_if is_immutable expected;
                               evidence_locations;
                               thrown_at_source;
                             })
                        ~define:Context.define
                      |> Option.some
                    else if explicit && insufficiently_annotated then
                      Error.create
                        ~location:(Location.with_module ~qualifier:Context.qualifier location)
                        ~kind:
                          (Error.ProhibitedAny
                             {
                               missing_annotation =
                                 {
                                   Error.name = reference;
                                   annotation = actual_annotation;
                                   given_annotation = Option.some_if is_immutable expected;
                                   evidence_locations;
                                   thrown_at_source = true;
                                 };
                               is_type_alias = false;
                             })
                        ~define:Context.define
                      |> Option.some
                    else if is_type_alias && Type.expression_contains_any value then
                      let value_annotation =
                        GlobalResolution.parse_annotation global_resolution value
                      in
                      Error.create
                        ~location:(Location.with_module ~qualifier:Context.qualifier location)
                        ~kind:
                          (Error.ProhibitedAny
                             {
                               missing_annotation =
                                 {
                                   Error.name = reference;
                                   annotation = None;
                                   given_annotation = Some value_annotation;
                                   evidence_locations;
                                   thrown_at_source = true;
                                 };
                               is_type_alias;
                             })
                        ~define:Context.define
                      |> Option.some_if (Type.contains_prohibited_any value_annotation)
                    else
                      None
                | Name.Attribute { base = { Node.value = Name base; _ }; attribute; _ }, None
                  when is_simple_name base && insufficiently_annotated ->
                    (* Module *)
                    let reference = name_to_reference_exn base in
                    if
                      explicit
                      && (not is_type_alias)
                      && not (GlobalResolution.module_exists global_resolution reference)
                    then
                      Error.create
                        ~location:(Location.with_module ~qualifier:Context.qualifier location)
                        ~kind:
                          (Error.ProhibitedAny
                             {
                               missing_annotation =
                                 {
                                   Error.name = Reference.create ~prefix:reference attribute;
                                   annotation = actual_annotation;
                                   given_annotation = Option.some_if is_immutable expected;
                                   evidence_locations;
                                   thrown_at_source = true;
                                 };
                               is_type_alias = false;
                             })
                        ~define:Context.define
                      |> Option.some
                    else
                      None
                | ( Name.Attribute { attribute; _ },
                    Some ({ Type.instantiated; class_attributes; class_name } :: _) ) -> (
                    (* Instance *)
                    let reference = Reference.create attribute in
                    let attribute =
                      GlobalResolution.attribute_from_class_name
                        ~resolution:global_resolution
                        ~name:attribute
                        ~instantiated
                        ~class_attributes
                        ~transitive:true
                        class_name
                    in
                    match attribute with
                    | Some attribute ->
                        if is_illegal_attribute_annotation attribute then
                          (* Non-self attributes may not be annotated. *)
                          Error.create
                            ~location:(Location.with_module ~qualifier:Context.qualifier location)
                            ~kind:(Error.IllegalAnnotationTarget target)
                            ~define:Context.define
                          |> Option.some
                        else if
                          Annotated.Class.Attribute.defined attribute && insufficiently_annotated
                        then
                          Error.create
                            ~location:(Location.with_module ~qualifier:Context.qualifier location)
                            ~kind:
                              (Error.MissingAttributeAnnotation
                                 {
                                   parent = Primitive (Annotated.Attribute.parent attribute);
                                   missing_annotation =
                                     {
                                       Error.name = reference;
                                       annotation = actual_annotation;
                                       given_annotation = Option.some_if is_immutable expected;
                                       evidence_locations;
                                       thrown_at_source;
                                     };
                                 })
                            ~define:Context.define
                          |> Option.some
                        else if insufficiently_annotated && explicit && not is_type_alias then
                          Error.create
                            ~location:(Location.with_module ~qualifier:Context.qualifier location)
                            ~kind:
                              (Error.ProhibitedAny
                                 {
                                   missing_annotation =
                                     {
                                       Error.name = reference;
                                       annotation = actual_annotation;
                                       given_annotation = Option.some_if is_immutable expected;
                                       evidence_locations;
                                       thrown_at_source = true;
                                     };
                                   is_type_alias = false;
                                 })
                            ~define:Context.define
                          |> Option.some
                        else
                          None
                    | None -> None )
                | _ ->
                    Error.create
                      ~location:(Location.with_module ~qualifier:Context.qualifier location)
                      ~kind:(Error.IllegalAnnotationTarget target)
                      ~define:Context.define
                    |> Option.some_if explicit
              in
              let state = error >>| emit_raw_error ~state |> Option.value ~default:state in
              let is_valid_annotation =
                match error with
                | Some { Error.kind = IllegalAnnotationTarget _; _ } -> false
                | _ -> true
              in
              (* Propagate annotations. *)
              let state =
                let is_global =
                  match name with
                  | Identifier identifier ->
                      Resolution.is_global resolution ~reference:(Reference.create identifier)
                  | Attribute _ as name when is_simple_name name ->
                      Resolution.is_global resolution ~reference:(name_to_reference_exn name)
                  | _ -> false
                in
                if is_global && not (Define.is_toplevel Context.define.value) then
                  state
                else
                  let refine_annotation annotation refined =
                    RefinementUnit.refine ~global_resolution annotation refined
                  in
                  let annotation =
                    if explicit && is_valid_annotation then
                      let annotation = Annotation.create_immutable ~final:is_final guide in
                      if Type.is_concrete resolved && not (Type.is_ellipsis resolved) then
                        refine_annotation annotation resolved
                      else
                        annotation
                    else if is_immutable then
                      refine_annotation target_annotation guide
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
                          {
                            target = { Node.location; value = target_value };
                            annotation = resolved;
                            attempted_action = Naming;
                          }
                      in
                      let converted =
                        Type.Variable.convert_all_escaped_free_variables_to_anys
                          (Annotation.annotation annotation)
                      in
                      emit_error ~state ~location ~kind, { annotation with annotation = converted }
                    else
                      state, annotation
                  in
                  let resolution =
                    match name with
                    | Identifier identifier ->
                        Resolution.set_local
                          resolution
                          ~reference:(Reference.create identifier)
                          ~annotation
                    | Attribute _ as name when is_simple_name name -> (
                        match resolved_base, attribute with
                        | Some parent, Some (attribute, _)
                          when not
                                 ( Annotated.Attribute.defined attribute
                                 || is_undefined_attribute parent ) ->
                            Resolution.set_local_with_attributes resolution ~name ~annotation
                        | _ -> resolution )
                    | _ -> resolution
                  in
                  { state with resolution }
              in
              state
          | List elements
          | Tuple elements
            when is_uniform_sequence guide ->
              let propagate state element =
                match Node.value element with
                | Expression.Starred (Starred.Once target) ->
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
          | Tuple elements ->
              let left, starred, right =
                let is_starred { Node.value; _ } =
                  match value with
                  | Expression.Starred (Starred.Once _) -> true
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
              let assignees = left @ starred @ right in
              let state, annotations =
                match guide with
                | Type.Any -> state, List.map assignees ~f:(fun _ -> Type.Any)
                | Type.Top -> state, List.map assignees ~f:(fun _ -> Type.Top)
                | _ -> (
                    match nonuniform_sequence_parameters guide with
                    | None ->
                        let state =
                          emit_error
                            ~state
                            ~location
                            ~kind:
                              (Error.Unpack
                                 {
                                   expected_count = List.length assignees;
                                   unpack_problem = UnacceptableType guide;
                                 })
                        in
                        state, List.map assignees ~f:(fun _ -> Type.Top)
                    | Some annotations ->
                        let annotations =
                          let has_starred_assignee = not (List.is_empty starred) in
                          let left, tail = List.split_n annotations (List.length left) in
                          let starred, right =
                            List.split_n tail (List.length tail - List.length right)
                          in
                          let starred =
                            if not (List.is_empty starred) then
                              let annotation =
                                List.fold
                                  starred
                                  ~init:Type.Bottom
                                  ~f:(GlobalResolution.join global_resolution)
                                |> Type.list
                              in
                              [annotation]
                            else if has_starred_assignee then
                              [Type.tuple []]
                            else
                              []
                          in
                          left @ starred @ right
                        in
                        if List.length annotations <> List.length assignees then
                          let state =
                            emit_error
                              ~state
                              ~location
                              ~kind:
                                (Error.Unpack
                                   {
                                     expected_count = List.length assignees;
                                     unpack_problem = CountMismatch (List.length annotations);
                                   })
                          in
                          state, List.map assignees ~f:(fun _ -> Type.Top)
                        else
                          state, annotations )
              in
              List.zip_exn assignees annotations
              |> List.fold ~init:state ~f:(fun state (target, guide) ->
                     forward_assign ~state ~target ~guide ~resolved:guide ~expression:None)
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
                match resolve_expression_type ~state annotation with
                | annotation when Type.is_meta annotation -> Type.single_parameter annotation
                | Type.Tuple (Bounded (Concrete elements))
                  when List.for_all ~f:Type.is_meta elements ->
                    List.map ~f:Type.single_parameter elements |> Type.union
                | Type.Tuple (Unbounded element) when Type.is_meta element ->
                    Type.single_parameter element
                | _ -> Type.Top )
            | annotation -> annotation
          in
          match annotation with
          | { Node.value = Expression.Tuple elements; _ } ->
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
        let rec refinable_annotation name =
          match
            Resolution.get_local_with_attributes ~global_fallback:false ~name resolution, name
          with
          | Some local_annotation, _ -> Some local_annotation
          | _, Name.Attribute { base = { Node.value = Name base; _ }; attribute; _ } -> (
              let attribute =
                refinable_annotation base
                >>| Annotation.annotation
                >>= fun parent ->
                Type.split parent
                |> fst
                |> Type.primitive_name
                >>= GlobalResolution.attribute_from_class_name
                      ~resolution:global_resolution
                      ~name:attribute
                      ~instantiated:parent
                      ~transitive:true
              in
              match
                ( attribute >>| AnnotatedAttribute.visibility,
                  attribute >>| AnnotatedAttribute.defined,
                  attribute >>| AnnotatedAttribute.annotation )
              with
              | Some (ReadOnly (Refinable _)), Some true, Some annotation -> Some annotation
              | _ -> None )
          | _ -> None
        in
        let set_local name annotation =
          Resolution.set_local_with_attributes resolution ~name ~annotation
        in
        match Node.value test with
        | False ->
            (* Explicit bottom. *)
            { state with bottom = true }
        | ComparisonOperator
            {
              left =
                {
                  Node.value =
                    Call
                      {
                        callee = { Node.value = Name (Name.Identifier "type"); _ };
                        arguments =
                          [{ Call.Argument.name = None; value = { Node.value = Name name; _ } }];
                      };
                  _;
                };
              operator = ComparisonOperator.Is;
              right = annotation;
            }
        | Call
            {
              callee = { Node.value = Name (Name.Identifier "isinstance"); _ };
              arguments =
                [
                  { Call.Argument.name = None; value = { Node.value = Name name; _ } };
                  { Call.Argument.name = None; value = annotation };
                ];
            }
          when is_simple_name name ->
            let annotation = parse_refinement_annotation annotation in
            let resolution =
              let refinement_unnecessary existing_annotation =
                RefinementUnit.less_or_equal
                  ~global_resolution
                  (RefinementUnit.create ~base:existing_annotation ())
                  (RefinementUnit.create ~base:(Annotation.create annotation) ())
                && (not (Type.equal (Annotation.annotation existing_annotation) Type.Bottom))
                && not (Type.equal (Annotation.annotation existing_annotation) Type.Any)
              in
              match refinable_annotation name with
              (* Allow Anys [especially from placeholder stubs] to clobber *)
              | Some _ when Type.is_any annotation -> Annotation.create annotation |> set_local name
              | Some existing_annotation when refinement_unnecessary existing_annotation ->
                  set_local name existing_annotation
              (* Clarify Anys if possible *)
              | Some existing_annotation
                when Type.equal (Annotation.annotation existing_annotation) Type.Any ->
                  Annotation.create annotation |> set_local name
              | None -> resolution
              | Some existing_annotation ->
                  let { consistent_with_boundary; _ } =
                    partition (Annotation.annotation existing_annotation) ~boundary:annotation
                  in
                  if Type.equal consistent_with_boundary Type.Bottom then
                    Annotation.create annotation |> set_local name
                  else
                    Annotation.create consistent_with_boundary |> set_local name
            in
            { state with resolution }
        | Call
            {
              callee = { Node.value = Name (Name.Identifier "callable"); _ };
              arguments = [{ Call.Argument.name = None; value = { Node.value = Name name; _ } }];
            }
          when is_simple_name name ->
            let resolution =
              match refinable_annotation name with
              | Some existing_annotation ->
                  let undefined =
                    Type.Callable.create ~parameters:Undefined ~annotation:Type.object_primitive ()
                  in
                  let { consistent_with_boundary; _ } =
                    partition (Annotation.annotation existing_annotation) ~boundary:undefined
                  in
                  if Type.equal consistent_with_boundary Type.Bottom then
                    Annotation.create undefined |> set_local name
                  else
                    Annotation.create consistent_with_boundary |> set_local name
              | _ -> resolution
            in
            { state with resolution }
        | ComparisonOperator
            {
              left =
                {
                  Node.value =
                    Call
                      {
                        callee = { Node.value = Name (Name.Identifier "type"); _ };
                        arguments = [{ Call.Argument.name = None; value }];
                      };
                  _;
                };
              operator = ComparisonOperator.IsNot;
              right = annotation_expression;
            }
        | UnaryOperator
            {
              UnaryOperator.operator = UnaryOperator.Not;
              operand =
                {
                  Node.value =
                    Call
                      {
                        callee = { Node.value = Name (Name.Identifier "isinstance"); _ };
                        arguments =
                          [
                            { Call.Argument.name = None; value };
                            { Call.Argument.name = None; value = annotation_expression };
                          ];
                      };
                  _;
                };
            } -> (
            let expected = parse_refinement_annotation annotation_expression in
            let contradiction_error =
              let { resolved; _ } = forward_expression ~state ~expression:value in
              if
                Type.is_unbound resolved
                || Type.is_unknown resolved
                || Type.is_any resolved
                || not
                     (GlobalResolution.less_or_equal
                        global_resolution
                        ~left:resolved
                        ~right:expected)
              then
                None
              else
                Some
                  (Error.create
                     ~location:
                       (Location.with_module ~qualifier:Context.qualifier (Node.location test))
                     ~kind:
                       (Error.ImpossibleAssertion
                          { statement; expression = value; annotation = resolved })
                     ~define:Context.define)
            in
            let resolve ~name =
              match Resolution.get_local_with_attributes resolution ~name with
              | Some { annotation = previous_annotation; _ } ->
                  let { not_consistent_with_boundary; _ } =
                    partition previous_annotation ~boundary:expected
                  in
                  not_consistent_with_boundary
                  >>| Annotation.create
                  >>| (fun annotation ->
                        Resolution.set_local_with_attributes resolution ~name ~annotation)
                  |> Option.value ~default:resolution
              | _ -> resolution
            in
            match contradiction_error, value with
            | Some error, _ -> emit_raw_error ~state:{ state with bottom = true } error
            | _, { Node.value = Name name; _ } when is_simple_name name ->
                { state with resolution = resolve ~name }
            | _ -> state )
        | UnaryOperator
            {
              UnaryOperator.operator = UnaryOperator.Not;
              operand =
                {
                  Node.value =
                    Call
                      {
                        callee = { Node.value = Name (Name.Identifier "callable"); _ };
                        arguments =
                          [{ Call.Argument.name = None; value = { Node.value = Name name; _ } }];
                      };
                  _;
                };
            }
          when is_simple_name name ->
            let resolution =
              match refinable_annotation name with
              | Some existing_annotation ->
                  let { not_consistent_with_boundary; _ } =
                    partition
                      (Annotation.annotation existing_annotation)
                      ~boundary:
                        (Type.Callable.create
                           ~parameters:Undefined
                           ~annotation:Type.object_primitive
                           ())
                  in
                  not_consistent_with_boundary
                  >>| Annotation.create
                  >>| (fun annotation -> set_local name annotation)
                  |> Option.value ~default:resolution
              | _ -> resolution
            in
            { state with resolution }
        | Call
            {
              callee = { Node.value = Name (Name.Identifier "all"); _ };
              arguments = [{ Call.Argument.name = None; value = { Node.value = Name name; _ } }];
            }
          when is_simple_name name ->
            let resolution =
              match Resolution.get_local_with_attributes resolution ~name with
              | Some
                  {
                    Annotation.annotation =
                      Type.Parametric
                        { name = parametric_name; parameters = [Single (Type.Optional parameter)] }
                      as annotation;
                    _;
                  }
                when GlobalResolution.less_or_equal
                       global_resolution
                       ~left:annotation
                       ~right:(Type.iterable (Type.Optional parameter)) ->
                  Resolution.set_local_with_attributes
                    resolution
                    ~name
                    ~annotation:
                      (Annotation.create
                         (Type.Parametric
                            { name = parametric_name; parameters = [Single parameter] }))
              | _ -> resolution
            in
            { state with resolution }
        | Name name when is_simple_name name -> (
            match refinable_annotation name with
            | Some { Annotation.annotation = Type.Optional Type.Bottom; _ } ->
                Error.create
                  ~location:(Location.with_module ~qualifier:Context.qualifier (Node.location test))
                  ~kind:
                    (Error.ImpossibleAssertion
                       { statement; expression = test; annotation = Type.Optional Type.Bottom })
                  ~define:Context.define
                |> emit_raw_error ~state:{ state with bottom = true }
            | Some ({ Annotation.annotation = Type.Optional parameter; _ } as annotation) ->
                let resolution =
                  Resolution.set_local_with_attributes
                    resolution
                    ~name
                    ~annotation:{ annotation with Annotation.annotation = parameter }
                in
                { state with resolution }
            | _ -> state )
        | BooleanOperator { BooleanOperator.left; operator; right } -> (
            let update state expression =
              forward_statement ~state ~statement:(Statement.assume expression)
              |> fun { resolution; _ } -> Resolution.annotation_store resolution
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
                  | `Both (left, right) -> Some (RefinementUnit.meet ~global_resolution left right)
                  | `Left left -> Some left
                  | `Right right -> Some right
                in
                let annotation_store = Map.merge ~f:merge left right in
                let resolution = Resolution.with_annotation_store resolution ~annotation_store in
                { state with resolution }
            | BooleanOperator.Or ->
                let negated_left = update state (normalize (negate left)) in
                let resolution =
                  Resolution.with_annotation_store resolution ~annotation_store:negated_left
                in
                let left = update state left in
                let right = update { state with resolution } right in
                join
                  {
                    state with
                    resolution = Resolution.with_annotation_store resolution ~annotation_store:left;
                  }
                  {
                    state with
                    resolution = Resolution.with_annotation_store resolution ~annotation_store:right;
                  } )
        | ComparisonOperator
            {
              ComparisonOperator.left;
              operator = ComparisonOperator.IsNot;
              right = { Node.value = Name (Name.Identifier "None"); _ };
            } ->
            forward_statement ~state ~statement:(Statement.assume left)
        | ComparisonOperator
            {
              ComparisonOperator.left = { Node.value = Name name; _ };
              operator = ComparisonOperator.Is;
              right = { Node.value = Name (Name.Identifier "None"); _ };
            }
          when is_simple_name name -> (
            let refined = Annotation.create (Type.Optional Type.Bottom) in
            match refinable_annotation name with
            | Some previous ->
                if
                  RefinementUnit.less_or_equal
                    ~global_resolution
                    (RefinementUnit.create ~base:refined ())
                    (RefinementUnit.create ~base:previous ())
                then
                  {
                    state with
                    resolution =
                      Resolution.set_local_with_attributes resolution ~name ~annotation:refined;
                  }
                else
                  (* Keeping previous state, since it is more refined. *)
                  (* TODO: once T38750424 is done, we should really return bottom if previous is not
                     <= refined and refined is not <= previous, as this is an obvious contradiction. *)
                  state
            | None -> state )
        | ComparisonOperator
            {
              ComparisonOperator.left = { Node.value = Name name; _ };
              operator = ComparisonOperator.In;
              right;
            }
          when is_simple_name name ->
            let reference = name_to_reference_exn name in
            let { resolved; _ } = forward_expression ~state ~expression:right in
            (* TODO (T56720048): Stop joining with iterable bottom *)
            let iterable =
              GlobalResolution.join global_resolution resolved (Type.iterable Type.Bottom)
            in
            if Type.is_iterable iterable then
              match Type.single_parameter iterable with
              | Type.Any
              | Type.Bottom ->
                  state
              | element_type -> (
                  let annotation =
                    Resolution.get_local_with_attributes ~global_fallback:false ~name resolution
                  in
                  match annotation with
                  | Some previous ->
                      let refined =
                        if Annotation.is_immutable previous then
                          Annotation.create_immutable
                            ~original:(Some (Annotation.original previous))
                            element_type
                        else
                          Annotation.create element_type
                      in
                      if
                        RefinementUnit.less_or_equal
                          ~global_resolution
                          (RefinementUnit.create ~base:refined ())
                          (RefinementUnit.create ~base:previous ())
                      then
                        let resolution =
                          Resolution.set_local_with_attributes resolution ~name ~annotation:refined
                        in
                        { state with resolution }
                      else (* Keeping previous state, since it is more refined. *)
                        state
                  | None when not (Resolution.is_global resolution ~reference) ->
                      let resolution =
                        Resolution.set_local_with_attributes
                          resolution
                          ~name
                          ~annotation:(Annotation.create element_type)
                      in
                      { state with resolution }
                  | _ -> state )
            else
              state
        | ComparisonOperator
            {
              ComparisonOperator.left = { Node.value = Name (Name.Identifier "None"); _ };
              operator = ComparisonOperator.NotIn;
              right = { Node.value = Name name; _ };
            }
          when is_simple_name name -> (
            let annotation =
              Resolution.get_local_with_attributes ~global_fallback:false ~name resolution
            in
            match annotation with
            | Some annotation -> (
                match Annotation.annotation annotation with
                | t when Type.is_none t ->
                    Error.create
                      ~location:
                        (Location.with_module ~qualifier:Context.qualifier (Node.location test))
                      ~kind:
                        (Error.ImpossibleAssertion
                           {
                             statement;
                             expression = test;
                             annotation = Type.list (Type.Optional Type.Bottom);
                           })
                      ~define:Context.define
                    |> emit_raw_error ~state:{ state with bottom = true }
                | Type.Parametric { name = "list"; parameters = [Single (Type.Optional parameter)] }
                  ->
                    let resolution =
                      Resolution.set_local_with_attributes
                        resolution
                        ~name
                        ~annotation:{ annotation with Annotation.annotation = Type.list parameter }
                    in
                    { state with resolution }
                | _ -> state )
            | _ -> state )
        | _ -> state )
    | Delete expression ->
        let resolution =
          match Node.value expression with
          | Name (Identifier identifier) ->
              Resolution.unset_local resolution ~reference:(Reference.create identifier)
          | _ -> resolution
        in
        let { state; _ } = forward_expression ~state ~expression in
        { state with resolution }
    | Expression
        { Node.value = Call { callee; arguments = { Call.Argument.value = test; _ } :: _ }; _ }
      when Core.Set.mem Recognized.assert_functions (Expression.show callee) ->
        forward_statement ~state ~statement:(Statement.assume test)
    | Expression expression ->
        forward_expression ~state ~expression
        |> fun { state; resolved; _ } ->
        if Type.is_noreturn resolved then
          { state with bottom = true }
        else
          state
    | Raise { Raise.expression = Some expression; _ } ->
        let { state; resolved; _ } = forward_expression ~state ~expression in
        let expected = Type.Primitive "BaseException" in
        let actual =
          if Type.is_meta resolved then
            Type.single_parameter resolved
          else
            resolved
        in
        if GlobalResolution.less_or_equal global_resolution ~left:actual ~right:expected then
          state
        else
          emit_error
            ~state
            ~location
            ~kind:(Error.InvalidException { expression; annotation = resolved })
    | Raise _ -> state
    | Return { Return.expression; is_implicit } ->
        let { state; resolved = actual; _ } =
          Option.value_map
            expression
            ~default:{ state; resolved = Type.none; resolved_annotation = None; base = None }
            ~f:(fun expression -> forward_expression ~state ~expression)
        in
        validate_return ~expression ~state ~actual ~is_implicit
    | Statement.Yield { Node.value = Expression.Yield return; _ } ->
        let { state; resolved = actual; _ } =
          match return with
          | Some expression ->
              let { state; resolved; _ } = forward_expression ~state ~expression in
              {
                state;
                resolved = Type.generator ~async resolved;
                resolved_annotation = None;
                base = None;
              }
          | None ->
              {
                state;
                resolved = Type.generator ~async Type.none;
                resolved_annotation = None;
                base = None;
              }
        in
        validate_return ~expression:None ~state ~actual ~is_implicit:false
    | Statement.Yield _ -> state
    | YieldFrom { Node.value = Expression.Yield (Some return); _ } ->
        let { state; resolved; _ } = forward_expression ~state ~expression:return in
        let actual =
          match GlobalResolution.join global_resolution resolved (Type.iterator Type.Bottom) with
          | Type.Parametric { name = "typing.Iterator"; parameters = [Single parameter] } ->
              Type.generator parameter
          | annotation -> Type.generator annotation
        in
        validate_return ~expression:None ~state ~actual ~is_implicit:false
    | YieldFrom _ -> state
    | Define { signature = { Define.Signature.name = { Node.value = name; _ }; _ } as signature; _ }
      ->
        if Reference.is_local name then
          type_of_signature ~resolution signature
          |> Type.Variable.mark_all_variables_as_bound
               ~specific:(Resolution.all_type_variables_in_scope resolution)
          |> Annotation.create
          |> (fun annotation -> Resolution.set_local resolution ~reference:name ~annotation)
          |> fun resolution -> { state with resolution }
        else
          state
    | Class _ ->
        (* Don't check accesses in nested classes and functions, they're analyzed separately. *)
        state
    | For _
    | If _
    | Try _
    | With _
    | While _ ->
        (* Check happens implicitly in the resulting control flow. *)
        state
    | Import _ ->
        (* Check happens after typing is done. *)
        state
    | Break
    | Continue
    | Global _
    | Nonlocal _
    | Pass ->
        state


  and resolve_expression ~state expression =
    forward_expression ~state ~expression
    |> fun { resolved; resolved_annotation; _ } ->
    resolved_annotation |> Option.value ~default:(Annotation.create resolved)


  and resolve_expression_type ~state expression =
    resolve_expression ~state expression |> Annotation.annotation


  and resolve_reference_type ~state reference =
    from_reference ~location:Location.any reference |> resolve_expression_type ~state


  let forward ?key ({ bottom; resolution; _ } as state) ~statement =
    let ({ resolution_fixpoint; _ } as state) =
      if bottom then
        state
      else
        forward_statement ~state ~statement
    in
    let state =
      let resolution_fixpoint =
        match key, state with
        | Some key, { resolution = post_resolution; _ } ->
            let precondition = Resolution.annotation_store resolution in
            let postcondition = Resolution.annotation_store post_resolution in
            LocalAnnotationMap.set resolution_fixpoint ~key ~precondition ~postcondition
        | None, _ -> resolution_fixpoint
      in
      { state with resolution_fixpoint }
    in
    state


  let backward ?key:_ state ~statement:_ = state
end

module CheckResult = struct
  type t = {
    errors: Error.t list;
    local_annotations: LocalAnnotationMap.t option;
  }

  let aggregate_errors results =
    List.fold results ~init:[] ~f:(fun errors_sofar { errors; _ } ->
        List.append errors errors_sofar)
end

let resolution global_resolution ?(annotation_store = Reference.Map.empty) () =
  let define =
    Define.create_toplevel ~qualifier:None ~statements:[] |> Node.create_with_default_location
  in
  (* TODO: Eliminate the need of creating a dummy state here *)
  let module State = State (struct
    let qualifier = Reference.empty

    let debug = false

    let define = define

    module Builder = Callgraph.NullBuilder
  end)
  in
  let state_without_resolution =
    let empty_resolution =
      Resolution.create
        ~global_resolution
        ~annotation_store:Reference.Map.empty
        ~resolve:(fun ~resolution:_ _ -> Annotation.create Type.Top)
        ~resolve_assignment:(fun ~resolution _ -> resolution)
        ~resolve_assertion:(fun ~resolution ~asserted_expression:_ -> resolution)
        ()
    in
    {
      State.errors = ErrorMap.Map.empty;
      bottom = false;
      resolution_fixpoint = LocalAnnotationMap.empty;
      resolution = empty_resolution;
    }
  in
  let resolve ~resolution expression =
    let state = { state_without_resolution with State.resolution } in
    State.forward_expression ~state ~expression
    |> fun { State.resolved; resolved_annotation; _ } ->
    resolved_annotation |> Option.value ~default:(Annotation.create resolved)
  in
  let resolve_assignment ~resolution assign =
    let state = { state_without_resolution with State.resolution } in
    State.forward_statement
      ~state
      ~statement:(Ast.Node.create_with_default_location (Statement.Assign assign))
    |> State.resolution
  in
  let resolve_assertion ~resolution ~asserted_expression =
    let state = { state_without_resolution with State.resolution } in
    let assertion =
      {
        Ast.Statement.Assert.test = asserted_expression;
        message = None;
        origin = Ast.Statement.Assert.Origin.Assertion;
      }
    in
    State.forward_statement
      ~state
      ~statement:(Ast.Node.create_with_default_location (Statement.Assert assertion))
    |> State.resolution
  in
  Resolution.create
    ~global_resolution
    ~annotation_store
    ~resolve
    ~resolve_assignment
    ~resolve_assertion
    ()


let resolution_with_key ~global_resolution ~local_annotations ~parent ~key =
  let annotation_store =
    match key, local_annotations with
    | Some key, Some map ->
        LocalAnnotationMap.get_precondition map key |> Option.value ~default:Reference.Map.empty
    | _ -> Reference.Map.empty
  in
  resolution global_resolution ~annotation_store () |> Resolution.with_parent ~parent


let emit_errors_on_exit (module Context : Context) ~errors_sofar ~resolution () =
  let ( {
          Node.value =
            { Define.signature = { name = { Node.value = name; _ }; _ } as signature; _ } as define;
          location;
        } as define_node )
    =
    Context.define
  in
  let global_resolution = Resolution.global_resolution resolution in
  let class_initialization_errors errors =
    let check_protocol_properties definition errors =
      if Node.value definition |> ClassSummary.is_protocol then
        let private_protocol_property_errors =
          GlobalResolution.attributes
            ~transitive:false
            ~include_generated_attributes:true
            ~resolution:global_resolution
            (Reference.show (Annotated.Class.name definition))
          >>| List.map ~f:AnnotatedAttribute.name
          >>| List.filter ~f:is_private_attribute
          >>| List.map ~f:(fun name ->
                  Error.create
                    ~location:(Location.with_module ~qualifier:Context.qualifier location)
                    ~kind:
                      (Error.PrivateProtocolProperty
                         { name; parent = Annotated.Class.annotation definition })
                    ~define:Context.define)
          |> Option.value ~default:[]
        in
        private_protocol_property_errors @ errors
      else
        errors
    in
    (* Ensure all attributes are instantiated. This must happen after typechecking is finished to
       access the annotations added to resolution in the constructor. If a constructor does not
       exist, this function is triggered in the toplevel. *)
    let check_attribute_initialization ~is_dynamically_initialized definition errors =
      if
        (not (ClassSummary.is_protocol (Node.value definition)))
        && not (AnnotatedClass.has_abstract_base definition)
      then
        let unimplemented_errors =
          let uninitialized_attributes =
            let add_uninitialized ({ class_name; _ } as name_and_metadata) attribute_map =
              let attributes =
                GlobalResolution.attributes
                  ~include_generated_attributes:true
                  ~resolution:global_resolution
                  class_name
                |> Option.value ~default:[]
              in
              let is_uninitialized attribute =
                match Annotated.Attribute.initialized attribute with
                | NotInitialized -> not (is_dynamically_initialized attribute)
                | _ -> false
              in
              let add_to_map sofar attribute =
                let annotation =
                  GlobalResolution.instantiate_attribute
                    ~resolution:global_resolution
                    ?instantiated:None
                    attribute
                  |> Annotated.Attribute.annotation
                  |> Annotation.annotation
                in
                let name = Annotated.Attribute.name attribute in
                match String.Map.add sofar ~key:name ~data:(annotation, name_and_metadata) with
                | `Ok map -> map
                | `Duplicate -> sofar
              in
              List.filter attributes ~f:is_uninitialized
              |> List.fold ~init:attribute_map ~f:add_to_map
            in
            let remove_initialized { class_name; _ } attribute_map =
              let attributes =
                GlobalResolution.attributes
                  ~transitive:true
                  ~include_generated_attributes:true
                  ~resolution:global_resolution
                  class_name
                |> Option.value ~default:[]
              in
              let is_initialized attribute =
                (* TODO(T54083014): Don't error on properties overriding attributes, even if they
                   are read-only and therefore not marked as initialized on the attribute object. We
                   should error in the future that this is an inconsistent override. *)
                match Annotated.Attribute.initialized attribute with
                | NotInitialized -> Annotated.Attribute.property attribute
                | _ -> true
              in

              List.filter attributes ~f:is_initialized
              |> List.map ~f:AnnotatedAttribute.name
              |> List.fold ~init:attribute_map ~f:Map.remove
            in
            if AnnotatedClass.has_abstract_base definition then
              []
            else
              let abstract_superclasses, concrete_superclasses, _superclasses_missing_metadata =
                let { Node.value = { ClassSummary.name; _ }; _ } = definition in
                let class_name = Reference.show name in
                let is_protocol_or_abstract class_name =
                  match
                    GlobalResolution.class_metadata global_resolution (Primitive class_name)
                  with
                  | Some { is_protocol; is_abstract; _ } when is_protocol || is_abstract ->
                      `Fst { class_name; is_abstract; is_protocol }
                  | Some { is_protocol; is_abstract; _ } ->
                      `Snd { class_name; is_abstract; is_protocol }
                  | None -> `Trd ()
                in
                GlobalResolution.successors class_name ~resolution:global_resolution
                |> List.partition3_map ~f:is_protocol_or_abstract
              in
              let name_and_metadata =
                let { Node.value = { ClassSummary.name; _ }; _ } = definition in
                {
                  class_name = Reference.show name;
                  is_abstract = ClassSummary.is_abstract (Node.value definition);
                  is_protocol = ClassSummary.is_protocol (Node.value definition);
                }
              in
              List.cons name_and_metadata abstract_superclasses
              |> List.fold_right ~init:String.Map.empty ~f:add_uninitialized
              |> (fun attribute_map ->
                   List.fold_right
                     ~init:attribute_map
                     ~f:remove_initialized
                     (List.cons name_and_metadata concrete_superclasses))
              |> String.Map.to_alist
          in
          uninitialized_attributes
          |> List.filter_map
               ~f:(fun ( name,
                         (annotation, { class_name = original_class_name; is_protocol; is_abstract })
                       )
                       ->
                 let expected = annotation in
                 if Type.is_top expected then
                   None
                 else
                   let error_kind =
                     if is_protocol then
                       Error.Protocol (Reference.create original_class_name)
                     else if is_abstract then
                       Error.Abstract (Reference.create original_class_name)
                     else
                       Error.Class
                   in
                   Some
                     (Error.create
                        ~location:(Location.with_module ~qualifier:Context.qualifier location)
                        ~kind:
                          (Error.UninitializedAttribute
                             {
                               name;
                               parent = Annotated.Class.annotation definition;
                               mismatch =
                                 { Error.expected; actual = expected; due_to_invariance = false };
                               kind = error_kind;
                             })
                        ~define:Context.define))
        in
        unimplemented_errors @ errors
      else
        errors
    in
    if Define.is_constructor define && not (Define.is_stub define) then
      let check_attributes_initialized errors =
        let open Annotated in
        let definition =
          Define.parent_definition ~resolution:global_resolution (Define.create define_node)
        in
        match definition with
        | Some definition ->
            let is_dynamically_initialized attribute =
              let reference =
                Reference.create_from_list
                  [StatementDefine.self_identifier define; Attribute.name attribute]
              in
              Map.mem (Resolution.annotation_store resolution) reference
            in
            check_attribute_initialization ~is_dynamically_initialized definition errors
        | None -> errors
      in
      errors |> check_attributes_initialized
    else if Define.is_class_toplevel define then
      let check_bases errors =
        let open Annotated in
        let is_final errors { ExpressionCall.Argument.name; value } =
          let add_error { ClassMetadataEnvironment.is_final; _ } =
            if is_final then
              let error =
                Error.create
                  ~location:(Location.with_module ~qualifier:Context.qualifier location)
                  ~kind:(Error.InvalidInheritance (ClassName (Expression.show value)))
                  ~define:Context.define
              in
              error :: errors
            else
              errors
          in
          match name, value with
          | None, { Node.value = Name name; _ } when is_simple_name name ->
              let reference = name_to_reference_exn name in
              GlobalResolution.class_metadata
                global_resolution
                (Type.Primitive (Reference.show reference))
              >>| add_error
              |> Option.value ~default:errors
          | _ -> errors
        in
        Define.parent_definition ~resolution:global_resolution (Define.create define_node)
        >>| Class.bases
        >>| List.fold ~init:errors ~f:is_final
        |> Option.value ~default:errors
      in
      let check_protocol definition errors = check_protocol_properties definition errors in
      let check_attributes definition errors =
        (* Error on uninitialized attributes if there was no constructor in which to do so. *)
        if
          not
            (AnnotatedClass.has_explicit_constructor
               (AnnotatedClass.name definition |> Reference.show)
               ~resolution:global_resolution)
        then
          check_attribute_initialization
            ~is_dynamically_initialized:(fun _ -> false)
            definition
            errors
        else
          errors
      in
      let check_overrides
          ( { Node.value = { ClassSummary.attribute_components; name = class_name; _ }; _ } as
          definition )
          errors
        =
        let components =
          Ast.Statement.Class.attributes ~include_generated_attributes:true attribute_components
        in

        let override_errors =
          let open Annotated in
          GlobalResolution.attributes
            ~include_generated_attributes:false
            ~resolution:global_resolution
            (Reference.show (AnnotatedClass.name definition))
          >>| List.filter_map ~f:(fun attribute ->
                  let annotation =
                    GlobalResolution.instantiate_attribute
                      ~resolution:global_resolution
                      ?instantiated:None
                      attribute
                    |> Annotated.Attribute.annotation
                    |> Annotation.annotation
                  in
                  let name = Annotated.Attribute.name attribute in
                  let actual = annotation in
                  let check_override overridden_attribute =
                    let annotation =
                      Annotated.Attribute.annotation overridden_attribute |> Annotation.annotation
                    in
                    let name = Annotated.Attribute.name overridden_attribute in
                    let visibility = Annotated.Attribute.visibility overridden_attribute in
                    let expected = annotation in
                    let overridable =
                      match visibility with
                      | ReadOnly (Refinable { overridable }) -> overridable
                      | _ -> true
                    in
                    if
                      ( GlobalResolution.less_or_equal global_resolution ~left:actual ~right:expected
                      || Type.is_top actual
                      || Type.contains_variable actual )
                      && overridable
                    then (* TODO(T53997072): Support type variable instantiation for overrides. *)
                      None
                    else
                      let kind =
                        if not overridable then
                          Error.InvalidAssignment (FinalAttribute (Reference.create name))
                        else
                          Error.InconsistentOverride
                            {
                              overridden_method = name;
                              parent = Attribute.parent overridden_attribute |> Reference.create;
                              override_kind = Attribute;
                              override =
                                Error.WeakenedPostcondition
                                  (Error.create_mismatch
                                     ~resolution:global_resolution
                                     ~actual
                                     ~expected
                                     ~covariant:false);
                            }
                      in
                      let location =
                        Identifier.SerializableMap.find_opt name components
                        >>| Node.location
                        |> Option.value ~default:location
                      in

                      Some
                        (Error.create
                           ~location:(Location.with_module ~qualifier:Context.qualifier location)
                           ~kind
                           ~define:Context.define)
                  in
                  Class.overrides ~resolution:global_resolution ~name (Reference.show class_name)
                  >>| check_override
                  |> Option.value ~default:None)
          |> Option.value ~default:[]
        in
        override_errors @ errors
      in
      let check_redefined_class definition errors =
        (* Detect when a class from an import is redefined. This relies on the fact that
           resolve_exports always chooses an imported class if it exists, so we can compare that
           against the current class definition to determine if it is shadowing an imported class. *)
        let class_name = AnnotatedClass.name definition in
        let exported_name =
          GlobalResolution.resolve_exports global_resolution ~reference:class_name
        in
        if not (Reference.equal class_name exported_name) then
          let is_shadowed_class_imported =
            not
              (Option.equal
                 Reference.equal
                 (Reference.prefix class_name)
                 (Reference.prefix exported_name))
          in
          let error =
            Error.create
              ~location:(Location.with_module ~qualifier:Context.qualifier location)
              ~kind:
                (Error.RedefinedClass
                   {
                     current_class = class_name;
                     shadowed_class = exported_name;
                     is_shadowed_class_imported;
                   })
              ~define:Context.define
          in
          error :: errors
        else
          errors
      in
      let name = Reference.prefix name >>| Reference.show |> Option.value ~default:"" in
      GlobalResolution.class_definition global_resolution (Type.Primitive name)
      >>| Annotated.Class.create
      >>| (fun definition ->
            errors
            |> check_bases
            |> check_protocol definition
            |> check_attributes definition
            |> check_overrides definition
            |> check_redefined_class definition)
      |> Option.value ~default:errors
    else
      errors
  in
  let overload_errors errors =
    let annotation = Resolution.resolve_reference resolution name in
    let ({ Type.Callable.annotation = current_overload_annotation; _ } as current_overload) =
      GlobalResolution.create_overload ~resolution:global_resolution signature
    in
    let overload_to_callable overload =
      Type.Callable
        {
          implementation = { overload with annotation = Type.Any };
          kind = Anonymous;
          overloads = [];
          implicit = None;
        }
    in
    let check_implementation_exists errors =
      match annotation with
      | Type.Callable { implementation; _ }
        when Define.is_overloaded_function define
             && Type.Callable.Overload.is_undefined implementation ->
          let error =
            Error.create
              ~location:(Location.with_module ~qualifier:Context.qualifier location)
              ~kind:(Error.MissingOverloadImplementation name)
              ~define:Context.define
          in
          error :: errors
      | _ -> errors
    in
    let check_compatible_return_types errors =
      match annotation with
      | Type.Callable
          { implementation = { annotation = implementation_annotation; _ } as implementation; _ }
        when Define.is_overloaded_function define ->
          let errors_sofar =
            if
              Resolution.is_consistent_with
                resolution
                current_overload_annotation
                implementation_annotation
                ~expression:None
            then
              errors
            else
              let error =
                Error.create
                  ~location:(Location.with_module ~qualifier:Context.qualifier location)
                  ~kind:
                    (Error.IncompatibleOverload
                       (ReturnType
                          {
                            implementation_annotation;
                            overload_annotation = current_overload_annotation;
                            name;
                          }))
                  ~define:Context.define
              in
              error :: errors
          in
          if
            not
              (GlobalResolution.less_or_equal
                 global_resolution
                 ~right:(overload_to_callable current_overload)
                 ~left:(overload_to_callable implementation))
          then
            let error =
              Error.create
                ~location:(Location.with_module ~qualifier:Context.qualifier location)
                ~define:Context.define
                ~kind:(Error.IncompatibleOverload (Parameters { name; location }))
            in
            error :: errors_sofar
          else
            errors_sofar
      | _ -> errors
    in
    let check_unmatched_overloads errors =
      match annotation with
      | Type.Callable { overloads; _ } when Define.is_overloaded_function define ->
          let preceding, following_and_including =
            List.split_while overloads ~f:(fun other ->
                not (Type.Callable.equal_overload Type.equal other current_overload))
          in
          if List.is_empty following_and_including then
            errors
          else
            let right = overload_to_callable current_overload in
            List.find preceding ~f:(fun preceder ->
                GlobalResolution.less_or_equal
                  global_resolution
                  ~left:(overload_to_callable preceder)
                  ~right)
            >>| (fun matching_overload ->
                  Error.create
                    ~location:(Location.with_module ~qualifier:Context.qualifier location)
                    ~define:Context.define
                    ~kind:
                      (Error.IncompatibleOverload
                         (Unmatchable { name; unmatched_location = location; matching_overload })))
            >>| (fun error -> error :: errors)
            |> Option.value ~default:errors
      | _ -> errors
    in
    errors
    |> check_implementation_exists
    |> check_compatible_return_types
    |> check_unmatched_overloads
  in
  class_initialization_errors errors_sofar |> overload_errors


let emit_error (module Context : Context) ~errors_sofar ~kind ~location =
  Error.create
    ~location:(Location.with_module ~qualifier:Context.qualifier location)
    ~kind
    ~define:Context.define
  :: errors_sofar


(* TODO (T59974010): Migrate error emission logic from `State.forward_expression` to this function *)
let emit_errors_in_expression (module Context : Context) ~errors_sofar ~resolution expression =
  let rec emit_errors_in_expression ~errors_sofar ~resolution { Node.value = expression; _ } =
    match expression with
    | Expression.Await await -> emit_errors_in_expression ~resolution ~errors_sofar await
    | BooleanOperator { BooleanOperator.left; right; _ } ->
        let errors_sofar = emit_errors_in_expression ~resolution ~errors_sofar left in
        emit_errors_in_expression ~resolution ~errors_sofar right
    | Call { Call.callee; arguments } ->
        let errors_sofar =
          List.fold arguments ~init:errors_sofar ~f:(fun errors_sofar { Call.Argument.value; _ } ->
              emit_errors_in_expression ~resolution ~errors_sofar value)
        in
        emit_errors_in_expression ~resolution ~errors_sofar callee
    | ComparisonOperator { ComparisonOperator.left; right; _ } ->
        let errors_sofar = emit_errors_in_expression ~resolution ~errors_sofar left in
        emit_errors_in_expression ~resolution ~errors_sofar right
    | Dictionary { Dictionary.entries; keywords } ->
        let errors_sofar =
          List.fold entries ~init:errors_sofar ~f:(fun errors_sofar entry ->
              emit_errors_in_dictionary_entry ~resolution ~errors_sofar entry)
        in
        List.fold keywords ~init:errors_sofar ~f:(fun errors_sofar keyword ->
            emit_errors_in_expression ~resolution ~errors_sofar keyword)
    | List expressions
    | Set expressions
    | Tuple expressions ->
        List.fold expressions ~init:errors_sofar ~f:(fun errors_sofar expression ->
            emit_errors_in_expression ~resolution ~errors_sofar expression)
    | Starred (Once expression | Twice expression) ->
        emit_errors_in_expression ~resolution ~errors_sofar expression
    | String { kind = Format expressions; _ } ->
        List.fold expressions ~init:errors_sofar ~f:(fun errors_sofar expression ->
            emit_errors_in_expression ~resolution ~errors_sofar expression)
    | Ternary { target; test; alternative } ->
        let errors_sofar = emit_errors_in_expression ~resolution ~errors_sofar test in
        let errors_sofar = emit_errors_in_expression ~resolution ~errors_sofar target in
        emit_errors_in_expression ~resolution ~errors_sofar alternative
    | UnaryOperator { operand; _ } -> emit_errors_in_expression ~resolution ~errors_sofar operand
    | WalrusOperator { target; value } ->
        let errors_sofar = emit_errors_in_expression ~resolution ~errors_sofar target in
        emit_errors_in_expression ~resolution ~errors_sofar value
    | Yield (Some expression) -> emit_errors_in_expression ~resolution ~errors_sofar expression
    | Name _
    | Lambda _
    | Generator _
    | ListComprehension _
    | SetComprehension _
    | DictionaryComprehension _ ->
        (* TODO: Process these expressions *)
        errors_sofar
    | Complex _
    | Ellipsis
    | False
    | Float _
    | Integer _
    | String _
    | True
    | Yield None ->
        (* Not possible to error *)
        errors_sofar
  and emit_errors_in_dictionary_entry ~resolution ~errors_sofar { Dictionary.Entry.key; value } =
    let errors_sofar = emit_errors_in_expression ~resolution ~errors_sofar key in
    emit_errors_in_expression ~resolution ~errors_sofar value
  in
  emit_errors_in_expression ~errors_sofar ~resolution expression


let emit_errors_in_body
    (module Context : Context)
    ~global_resolution
    ~errors_sofar
    ~cfg
    ~local_annotations
    ()
  =
  (* TODO (T59974010): Migrate error emission logic from `State.forward_statement` to this function *)
  let emit_errors_in_statement
      ~pre_resolution
      ~post_resolution:_
      ~errors_sofar
      { Node.value = statement; location }
    =
    match statement with
    | Statement.Import { Import.from; imports } ->
        let check_import import =
          let rec check_lead lead = function
            | [] -> None
            | name :: rest -> (
                let lead = lead @ [name] in
                let reference = Reference.create_from_list lead in
                match GlobalResolution.module_exists global_resolution reference with
                | true -> check_lead lead rest
                | false -> (
                    match Resolution.resolve_reference pre_resolution reference with
                    | Type.Any ->
                        (* Import from Any is ok *)
                        None
                    | _ -> Some reference ) )
          in
          match GlobalResolution.is_suppressed_module global_resolution import with
          | true -> None
          | false -> check_lead [] (Reference.as_list import)
        in
        let undefined_imports =
          match from with
          | Some { Node.value = from; _ } -> Option.to_list (check_import from)
          | None ->
              List.filter_map imports ~f:(fun { Import.name = { Node.value = name; _ }; _ } ->
                  check_import name)
        in
        let add_import_error errors_sofar reference =
          emit_error
            (module Context)
            ~errors_sofar
            ~kind:(Error.UndefinedImport reference)
            ~location
        in
        List.fold undefined_imports ~init:errors_sofar ~f:add_import_error
    | Statement.Class { Class.bases; _ } when bases <> [] ->
        (* Check that variance isn't widened on inheritence *)
        let check_base errors_sofar { Call.Argument.value = base; _ } =
          let check_pair errors_sofar extended actual =
            match extended, actual with
            | ( Type.Variable { Type.Record.Variable.RecordUnary.variance = left; _ },
                Type.Variable { Type.Record.Variable.RecordUnary.variance = right; _ } ) -> (
                match left, right with
                | Type.Variable.Covariant, Type.Variable.Invariant
                | Type.Variable.Contravariant, Type.Variable.Invariant
                | Type.Variable.Covariant, Type.Variable.Contravariant
                | Type.Variable.Contravariant, Type.Variable.Covariant ->
                    emit_error
                      (module Context)
                      ~errors_sofar
                      ~location
                      ~kind:
                        (Error.InvalidTypeVariance
                           { annotation = extended; origin = Error.Inheritance actual })
                | _ -> errors_sofar )
            | _, _ -> errors_sofar
          in
          match GlobalResolution.parse_annotation global_resolution base with
          | Type.Parametric { name; parameters = extended_parameters }
            when not (String.equal name "typing.Generic") ->
              Type.Parameter.all_singles extended_parameters
              >>| (fun extended_parameters ->
                    let actual_parameters =
                      GlobalResolution.variables global_resolution name
                      >>= Type.Variable.all_unary
                      >>| List.map ~f:(fun unary -> Type.Variable unary)
                      |> Option.value ~default:[]
                    in
                    match
                      List.fold2
                        extended_parameters
                        actual_parameters
                        ~init:errors_sofar
                        ~f:check_pair
                    with
                    | Ok errors -> errors
                    | Unequal_lengths -> errors_sofar)
              |> Option.value ~default:errors_sofar
          | _ -> errors_sofar
        in
        List.fold bases ~f:check_base ~init:errors_sofar
    | _ -> errors_sofar
  in
  let walk_statement node_id statement_index errors_sofar statement =
    let pre_annotations, post_annotations =
      let key = [%hash: int * int] (node_id, statement_index) in
      ( LocalAnnotationMap.get_precondition local_annotations key
        |> Option.value ~default:Reference.Map.empty,
        LocalAnnotationMap.get_postcondition local_annotations key
        |> Option.value ~default:Reference.Map.empty )
    in
    let pre_resolution, post_resolution =
      ( resolution global_resolution ~annotation_store:pre_annotations (),
        resolution global_resolution ~annotation_store:post_annotations () )
    in
    emit_errors_in_statement ~pre_resolution ~post_resolution ~errors_sofar statement
  in
  let walk_cfg_node ~key:node_id ~data:cfg_node errors_sofar =
    let statements = Cfg.Node.statements cfg_node in
    List.foldi statements ~init:errors_sofar ~f:(walk_statement node_id)
  in
  Hashtbl.fold cfg ~init:errors_sofar ~f:walk_cfg_node


(* TODO (T59974010): Take errors out of state and get rid of the `errors_in_state` parameter *)
let emit_errors
    (module Context : Context)
    ~global_resolution
    ~errors_in_state
    ~cfg
    ~local_annotations
    ()
  =
  let errors_in_body =
    emit_errors_in_body
      (module Context)
      ~global_resolution
      ~errors_sofar:errors_in_state
      ~cfg
      ~local_annotations
      ()
  in
  let exit_resolution =
    let annotation_store =
      LocalAnnotationMap.get_postcondition local_annotations Cfg.exit_index
      |> Option.value ~default:Reference.Map.empty
    in
    resolution global_resolution ~annotation_store ()
  in
  emit_errors_on_exit (module Context) ~errors_sofar:errors_in_body ~resolution:exit_resolution ()


let filter_errors (module Context : Context) ~global_resolution errors =
  if Context.debug then
    errors
  else
    Error.filter ~resolution:global_resolution errors


let name = "TypeCheck"

let exit_state ~resolution (module Context : Context) =
  let module State = State (Context) in
  let module Fixpoint = Fixpoint.Make (State) in
  let initial = State.initial ~resolution in
  let {
    Node.value =
      { Define.signature = { Define.Signature.name = { Node.value = name; _ }; _ }; _ } as define;
    _;
  }
    =
    Context.define
  in
  let global_resolution = Resolution.global_resolution resolution in
  if Define.is_stub define then
    let { State.resolution; errors; _ } = initial in
    let errors_sofar = Map.data errors |> Error.deduplicate in
    ( emit_errors_on_exit (module Context) ~errors_sofar ~resolution ()
      |> filter_errors (module Context) ~global_resolution,
      None,
      None )
  else (
    Log.log ~section:`Check "Checking %a" Reference.pp name;
    Context.Builder.initialize ();
    let dump = Define.dump define in
    if dump then (
      Log.dump "Checking `%s`..." (Log.Color.yellow (Reference.show name));
      Log.dump "AST:\n%a" Define.pp define );
    if Define.dump_locations define then
      Log.dump "AST with Locations:\n%s" (Define.show_json define);
    let cfg = Cfg.create define in
    let exit =
      let fixpoint = Fixpoint.forward ~cfg ~initial in
      Fixpoint.exit fixpoint
    in
    if dump then Option.iter exit ~f:(Log.dump "Exit state:\n%a" State.pp);

    let callees = Context.Builder.get_all_callees () in
    let errors, local_annotations =
      match exit with
      | None -> [], None
      | Some { State.errors; resolution_fixpoint; _ } ->
          let errors_in_state = Map.data errors |> Error.deduplicate in
          ( emit_errors
              (module Context)
              ~global_resolution
              ~errors_in_state
              ~cfg
              ~local_annotations:resolution_fixpoint
              ()
            |> filter_errors (module Context) ~global_resolution,
            Some resolution_fixpoint )
    in
    errors, local_annotations, Some callees )


let check_define
    ~configuration:{ Configuration.Analysis.debug; _ }
    ~resolution
    ~qualifier
    ~call_graph_builder:(module Builder : Callgraph.Builder)
    ( {
        Node.location;
        value = { Define.signature = { name = { Node.value = name; _ }; _ }; _ } as define;
      } as define_node )
  =
  try
    let errors, local_annotations, callees =
      let module Context = struct
        let qualifier = qualifier

        let debug = debug

        let define = define_node

        module Builder = Builder
      end
      in
      exit_state ~resolution (module Context)
    in
    let caller =
      if Define.is_property_setter define then
        Callgraph.PropertySetterCaller name
      else
        Callgraph.FunctionCaller name
    in
    Option.iter callees ~f:(fun callees -> Callgraph.set ~caller ~callees);
    { CheckResult.errors; local_annotations }
  with
  | ClassHierarchy.Untracked annotation ->
      Statistics.event
        ~name:"undefined type"
        ~integers:[]
        ~normals:
          [
            "module", Reference.show qualifier;
            "define", Reference.show name;
            "type", Type.show annotation;
          ]
        ();
      if Define.dump define then
        Log.dump
          "Analysis crashed because of untracked type `%s`."
          (Log.Color.red (Type.show annotation));
      let undefined_error =
        Error.create
          ~location:(Location.with_module ~qualifier location)
          ~kind:(Error.AnalysisFailure annotation)
          ~define:define_node
      in
      { errors = [undefined_error]; local_annotations = None }


let get_or_recompute_local_annotations ~environment name =
  match TypeEnvironment.ReadOnly.get_local_annotations environment name with
  | Some _ as local_annotations -> local_annotations
  | None -> (
      (* Local annotations not preserved in shared memory. Recompute it. *)
      let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment in
      match GlobalResolution.define_body global_resolution name with
      | None -> None
      | Some define_node ->
          let _, local_annotations, _ =
            let resolution = resolution global_resolution () in
            let module Context = struct
              (* Doesn't matter what the qualifier is since we won't be using it *)
              let qualifier = Reference.empty

              let debug = false

              let define = define_node

              module Builder = Callgraph.NullBuilder
            end
            in
            exit_state ~resolution (module Context)
          in
          local_annotations )


let check_function_definition
    ~configuration
    ~resolution
    ~name
    ?call_graph_builder
    { FunctionDefinition.body; siblings; qualifier }
  =
  let timer = Timer.start () in
  Log.log ~section:`Check "Checking `%a`..." Reference.pp name;

  let check_define =
    let call_graph_builder =
      match call_graph_builder with
      | Some call_graph_builder -> call_graph_builder
      | None -> (module Callgraph.DefaultBuilder : Callgraph.Builder)
    in
    check_define ~configuration ~resolution ~call_graph_builder ~qualifier
  in
  let sibling_bodies = List.map siblings ~f:(fun { FunctionDefinition.Sibling.body; _ } -> body) in
  let sibling_results = List.map sibling_bodies ~f:(fun define_node -> check_define define_node) in
  let result =
    let open CheckResult in
    match body with
    | None -> { errors = aggregate_errors sibling_results; local_annotations = None }
    | Some define_node ->
        let ({ local_annotations; _ } as body_result) = check_define define_node in
        { errors = aggregate_errors (body_result :: sibling_results); local_annotations }
  in

  let number_of_lines =
    let bodies =
      match body with
      | None -> sibling_bodies
      | Some body -> body :: sibling_bodies
    in
    List.fold bodies ~init:0 ~f:(fun sofar body -> sofar + Node.number_of_lines body)
  in
  Statistics.performance
    ~flush:false
    ~randomly_log_every:1000
    ~section:`Check
    ~name:"SingleDefineTypeCheck"
    ~timer
    ~normals:["name", Reference.show name; "request kind", "SingleDefineTypeCheck"]
    ~integers:["number of lines", number_of_lines]
    ();
  result


let run_on_define ~configuration ~environment ?call_graph_builder name =
  let global_resolution =
    let global_environment = TypeEnvironment.global_environment environment in
    match configuration with
    | { Configuration.Analysis.incremental_style = FineGrained; _ } ->
        GlobalResolution.create global_environment ~dependency:(TypeCheckDefine name)
    | _ -> GlobalResolution.create global_environment
  in
  let resolution = resolution global_resolution () in
  match GlobalResolution.function_definition global_resolution name with
  | None -> ()
  | Some definition ->
      let { CheckResult.errors; local_annotations } =
        check_function_definition ~configuration ~resolution ~name ?call_graph_builder definition
      in
      let () =
        if configuration.store_type_check_resolution then
          (* Write fixpoint type resolutions to shared memory *)
          let local_annotations =
            Option.value local_annotations ~default:LocalAnnotationMap.empty
          in
          TypeEnvironment.set_local_annotations environment name local_annotations
      in
      TypeEnvironment.set_errors environment name errors


let run_on_defines ~scheduler ~configuration ~environment ?call_graph_builder defines =
  let timer = Timer.start () in

  let number_of_defines = List.length defines in
  Log.info "Checking %d functions..." number_of_defines;
  let map _ names =
    let analyze_define number_defines define_name =
      run_on_define ~configuration ~environment ?call_graph_builder define_name;
      number_defines + 1
    in
    List.fold names ~init:0 ~f:analyze_define
  in
  let reduce left right =
    let number_defines = left + right in
    Log.log ~section:`Progress "Processed %d of %d functions" number_defines number_of_defines;
    number_defines
  in
  let _ =
    Scheduler.map_reduce
      scheduler
      ~policy:
        (Scheduler.Policy.fixed_chunk_size
           ~minimum_chunk_size:10
           ~minimum_chunks_per_worker:2
           ~preferred_chunk_size:500
           ())
      ~configuration
      ~initial:0
      ~map
      ~reduce
      ~inputs:defines
      ()
  in

  Statistics.performance ~name:"check_TypeCheck" ~phase_name:"Type check" ~timer ()


let legacy_run_on_modules ~scheduler ~configuration ~environment ?call_graph_builder qualifiers =
  Profiling.track_shared_memory_usage ~name:"Before legacy type check" ();

  let all_defines =
    let unannotated_global_environment =
      GlobalResolution.unannotated_global_environment
        (TypeEnvironment.global_resolution environment)
    in
    let map _ qualifiers =
      List.concat_map qualifiers ~f:(fun qualifier ->
          UnannotatedGlobalEnvironment.ReadOnly.all_defines_in_module
            unannotated_global_environment
            qualifier)
    in
    Scheduler.map_reduce
      scheduler
      ~policy:
        (Scheduler.Policy.fixed_chunk_count
           ~minimum_chunks_per_worker:1
           ~minimum_chunk_size:100
           ~preferred_chunks_per_worker:5
           ())
      ~configuration
      ~initial:[]
      ~map
      ~reduce:List.append
      ~inputs:qualifiers
      ()
  in

  run_on_defines ~scheduler ~configuration ~environment ?call_graph_builder all_defines;
  Statistics.event
    ~section:`Memory
    ~name:"shared memory size post-typecheck"
    ~integers:["size", Memory.heap_size ()]
    ();
  Profiling.track_shared_memory_usage ~name:"After legacy type check" ()
