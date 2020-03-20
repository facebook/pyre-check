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

module LocalErrorMap = struct
  type t = Error.t list Int.Table.t

  let empty () = Int.Table.create ()

  let set error_map ~key ~errors = Int.Table.set error_map ~key ~data:errors

  let all_errors error_map = Int.Table.data error_map |> List.concat
end

module type Context = sig
  val qualifier : Reference.t

  val debug : bool

  val define : Define.t Node.t

  module Builder : Callgraph.Builder
end

module type Signature = sig
  type t [@@deriving eq]

  val create : resolution:Resolution.t -> unit -> t

  val create_unreachable : unit -> t

  val all_errors : t -> Error.t list

  val initial : resolution:Resolution.t -> t

  val parse_and_check_annotation
    :  ?bind_variables:bool ->
    resolution:Resolution.t ->
    Expression.t ->
    Error.t list * Type.t

  include Fixpoint.State with type t := t
end

module State (Context : Context) = struct
  type partitioned = {
    consistent_with_boundary: Type.t;
    not_consistent_with_boundary: Type.t option;
  }

  and t = {
    (* None means the state in unreachable *)
    resolution: Resolution.t option;
    resolution_fixpoint: LocalAnnotationMap.t;
    error_map: LocalErrorMap.t;
  }

  let pp format { resolution; error_map; _ } =
    match resolution with
    | None -> Format.fprintf format "  <UNREACHABLE STATE>\n"
    | Some resolution ->
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
                |> fun ast_environment ->
                AstEnvironment.ReadOnly.get_relative ast_environment reference
              in
              Error.instantiate ~lookup error
            in
            Format.asprintf
              "    %a -> %s"
              Location.WithPath.pp
              (Error.Instantiated.location error)
              (Error.Instantiated.description error ~show_error_traces:true)
          in
          List.map (LocalErrorMap.all_errors error_map) ~f:error_to_string
          |> String.concat ~sep:"\n"
        in
        Format.fprintf
          format
          "  Expected return: %a\n  Types:\n%s\n  Errors:\n%s\n"
          Type.pp
          expected
          annotations
          errors


  let show state = Format.asprintf "%a" pp state

  and equal left right =
    match left.resolution, right.resolution with
    | None, None -> true
    | Some left_resolution, Some right_resolution ->
        Map.equal
          RefinementUnit.equal
          (Resolution.annotation_store left_resolution)
          (Resolution.annotation_store right_resolution)
    | _, _ -> false


  let create ~resolution () =
    {
      resolution = Some resolution;
      resolution_fixpoint = LocalAnnotationMap.empty ();
      error_map = LocalErrorMap.empty ();
    }


  let create_unreachable () =
    {
      resolution = None;
      resolution_fixpoint = LocalAnnotationMap.empty ();
      error_map = LocalErrorMap.empty ();
    }


  let emit_error ~errors ~location ~kind =
    Error.create
      ~location:(Location.with_module ~qualifier:Context.qualifier location)
      ~kind
      ~define:Context.define
    :: errors


  let add_invalid_type_parameters_errors ~resolution ~location ~errors annotation =
    let mismatches, annotation =
      GlobalResolution.check_invalid_type_parameters resolution annotation
    in
    let add_error errors mismatch =
      emit_error ~errors ~location ~kind:(Error.InvalidTypeParameters mismatch)
    in
    List.fold mismatches ~f:add_error ~init:errors, annotation


  let add_untracked_annotation_errors ~resolution ~location ~errors annotation =
    let is_untracked_name class_name =
      match class_name with
      | "..." -> false
      | _ -> not (GlobalResolution.is_tracked resolution class_name)
    in
    let untracked = List.filter (Type.elements annotation) ~f:is_untracked_name in
    let errors =
      List.fold untracked ~init:errors ~f:(fun errors name ->
          emit_error ~errors ~location ~kind:(Error.UndefinedType (Primitive name)))
    in
    errors, List.is_empty untracked


  let parse_and_check_annotation
      ?(bind_variables = true)
      ~resolution
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
          Error.InvalidTypeVariable { annotation = variable; origin } |> Option.some
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
        let invalid_variable_error_kinds =
          Type.Variable.all_free_variables annotation
          |> List.filter_map ~f:(check_invalid_variables resolution)
        in
        ( no_untracked && List.is_empty invalid_variable_error_kinds,
          List.fold invalid_variable_error_kinds ~init:errors ~f:(fun errors kind ->
              emit_error ~errors ~location ~kind) )
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
      GlobalResolution.parse_annotation ~validation:NoValidation global_resolution expression
    in
    let errors =
      match annotation with
      | Type.Top ->
          emit_error
            ~errors:[]
            ~location
            ~kind:
              (Error.InvalidType
                 (InvalidType
                    { annotation = Type.Primitive (Expression.show expression); expected = "" }))
      | Type.Callable { implementation = { annotation = Type.Top; _ }; _ } ->
          emit_error
            ~errors:[]
            ~location
            ~kind:
              (Error.InvalidType
                 (InvalidType
                    {
                      annotation = Type.Primitive (Expression.show expression);
                      expected = "`Callable[[<parameters>], <return type>]`";
                    }))
      | _ -> []
    in
    let errors, annotation =
      check_and_correct_annotation errors ~resolution ~location ~annotation
    in
    let annotation =
      if bind_variables then Type.Variable.mark_all_variables_as_bound annotation else annotation
    in
    errors, annotation


  let resolution { resolution; _ } = resolution

  let all_errors { error_map; _ } = LocalErrorMap.all_errors error_map

  let less_or_equal ~left ~right =
    match left.resolution, right.resolution with
    | None, _ -> true
    | _, None -> false
    | Some left_resolution, Some right_resolution ->
        let entry_less_or_equal other less_or_equal ~key ~data sofar =
          sofar
          &&
          match Map.find other key with
          | Some other -> less_or_equal data other
          | _ -> false
        in
        Map.fold
          ~init:true
          ~f:
            (entry_less_or_equal
               (Resolution.annotation_store right_resolution)
               RefinementUnit.equal)
          (Resolution.annotation_store left_resolution)


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


  let join left right =
    match left.resolution, right.resolution with
    | None, _ -> right
    | _, None -> left
    | Some left_resolution, Some right_resolution ->
        { left with resolution = Some (join_resolutions left_resolution right_resolution) }


  let widening_threshold = 10

  let widen ~previous ~next ~iteration =
    match previous.resolution, next.resolution with
    | None, _ -> next
    | _, None -> previous
    | Some previous_resolution, Some next_resolution ->
        let global_resolution = Resolution.global_resolution previous_resolution in
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
            (Resolution.annotation_store previous_resolution)
            (Resolution.annotation_store next_resolution)
        in
        {
          resolution = Some (Resolution.with_annotation_store next_resolution ~annotation_store);
          resolution_fixpoint = next.resolution_fixpoint;
          error_map = next.error_map;
        }


  type base =
    | Class of Type.t
    | Instance of Type.t
    | Super of Type.t
  [@@deriving show]

  module Resolved = struct
    type t = {
      resolution: Resolution.t;
      errors: Error.t list;
      resolved: Type.t;
      resolved_annotation: Annotation.t option;
      base: base option;
    }
  end

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
        let variables = GlobalResolution.variables global_resolution in
        let define_variables =
          AnnotatedCallable.create_overload_without_applying_decorators ~parser ~variables signature
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
    let check_decorators resolution errors =
      let check_final_decorator errors =
        if Option.is_none parent && Define.is_final_method define then
          emit_error
            ~errors
            ~location
            ~kind:(Error.InvalidInheritance (NonMethodFunction "typing.final"))
        else
          errors
      in
      let check_decorator errors decorator =
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
          errors
        else
          let { Resolved.errors = decorator_errors; _ } =
            forward_expression ~resolution ~expression:decorator
          in
          List.append decorator_errors errors
      in
      List.fold decorators ~init:errors ~f:check_decorator |> check_final_decorator
    in
    let check_return_annotation resolution errors =
      let add_missing_return_error ~errors annotation =
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
            ~errors
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
          errors
      in
      let add_variance_error errors annotation =
        match annotation with
        | Type.Variable variable when Type.Variable.Unary.is_contravariant variable ->
            emit_error
              ~errors
              ~location
              ~kind:(Error.InvalidTypeVariance { annotation; origin = Error.Return })
        | _ -> errors
      in
      let errors = add_missing_return_error ~errors return_annotation in
      match return_annotation with
      | None -> errors
      | Some return_annotation ->
          let annotation_errors, annotation =
            parse_and_check_annotation ~resolution return_annotation
          in
          let errors = List.append annotation_errors errors in
          add_variance_error errors annotation
    in
    let add_capture_annotations resolution errors =
      let process_signature ({ Define.Signature.name = { Node.value = name; _ }; _ } as signature) =
        if Reference.is_local name then
          type_of_signature ~resolution signature
          |> Type.Variable.mark_all_variables_as_bound ~specific:outer_scope_variables
          |> Annotation.create
          |> fun annotation -> Resolution.set_local resolution ~reference:name ~annotation
        else
          resolution
      in
      let process_capture (resolution, errors) { Define.Capture.name; kind } =
        let resolution, errors, annotation =
          match kind with
          | Define.Capture.Kind.Annotation None ->
              ( resolution,
                emit_error ~errors ~location ~kind:(Error.MissingCaptureAnnotation name),
                Type.Any )
          | Define.Capture.Kind.Annotation (Some annotation_expression) ->
              let annotation_errors, annotation =
                parse_and_check_annotation ~resolution annotation_expression
              in
              resolution, List.append annotation_errors errors, annotation
          | Define.Capture.Kind.DefineSignature signature ->
              ( resolution,
                errors,
                type_of_signature ~resolution signature
                |> Type.Variable.mark_all_variables_as_bound ~specific:outer_scope_variables )
          | Define.Capture.Kind.Self parent ->
              resolution, errors, type_of_parent ~global_resolution parent
          | Define.Capture.Kind.ClassSelf parent ->
              resolution, errors, type_of_parent ~global_resolution parent |> Type.meta
        in
        let annotation = Annotation.create_immutable annotation in
        let resolution =
          let reference = Reference.create name in
          Resolution.set_local resolution ~reference ~annotation
        in
        resolution, errors
      in
      let resolution = process_signature signature in
      List.fold captures ~init:(resolution, errors) ~f:process_capture
    in
    let check_parameter_annotations resolution errors =
      let errors, annotation_store =
        let make_parameter_name name =
          name |> String.filter ~f:(fun character -> character <> '*') |> Reference.create
        in
        let check_parameter
            index
            (errors, annotation_store)
            { Node.location; value = { Parameter.name; value; annotation } }
          =
          let add_incompatible_variable_error ~errors annotation default =
            if
              Type.is_any default
              || GlobalResolution.less_or_equal global_resolution ~left:default ~right:annotation
              || GlobalResolution.constraints_solution_exists
                   global_resolution
                   ~left:default
                   ~right:annotation
            then
              errors
            else
              emit_error
                ~errors
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
          let add_missing_parameter_annotation_error ~errors ~given_annotation annotation =
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
              errors
            else
              emit_error
                ~errors
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
          let add_final_parameter_annotation_error ~errors =
            emit_error ~errors ~location ~kind:(Error.InvalidType (FinalParameter name))
          in
          let add_variance_error errors annotation =
            match annotation with
            | Type.Variable variable
              when (not (Define.is_constructor define)) && Type.Variable.Unary.is_covariant variable
              ->
                emit_error
                  ~errors
                  ~location
                  ~kind:(Error.InvalidTypeVariance { annotation; origin = Error.Parameter })
            | _ -> errors
          in
          let parse_as_unary () =
            let errors, { Annotation.annotation; mutability } =
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
                      let errors, annotation =
                        let annotation_errors, annotation =
                          parse_and_check_annotation ~resolution ~bind_variables:false annotation
                        in
                        List.append annotation_errors errors, annotation
                      in
                      let compatible =
                        GlobalResolution.constraints_solution_exists
                          global_resolution
                          ~left:resolved
                          ~right:annotation
                      in
                      let errors =
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
                        | Some kind -> emit_error ~errors ~location ~kind
                        | None -> errors
                      in
                      errors, Annotation.create annotation
                  | None -> errors, Annotation.create resolved )
              | _ -> (
                  let errors, parsed_annotation =
                    match annotation with
                    | None -> errors, None
                    | Some annotation ->
                        let anntation_errors, annotation =
                          parse_and_check_annotation ~resolution ~bind_variables:false annotation
                        in
                        let errors = List.append anntation_errors errors in
                        let errors = add_variance_error errors annotation in
                        errors, Some annotation
                  in
                  let contains_prohibited_any parsed_annotation =
                    let contains_literal_any =
                      annotation >>| Type.expression_contains_any |> Option.value ~default:false
                    in
                    contains_literal_any && Type.contains_prohibited_any parsed_annotation
                  in
                  let value_annotation =
                    value
                    >>| (fun expression -> forward_expression ~resolution ~expression)
                    >>| fun { resolved; _ } -> resolved
                  in
                  let errors =
                    match parsed_annotation, value_annotation with
                    | Some annotation, Some value_annotation ->
                        add_incompatible_variable_error ~errors annotation value_annotation
                    | _ -> errors
                  in
                  match parsed_annotation, value_annotation with
                  | Some annotation, Some _ when Type.contains_final annotation ->
                      ( add_final_parameter_annotation_error ~errors,
                        Annotation.create_immutable annotation )
                  | Some annotation, Some value_annotation when contains_prohibited_any annotation
                    ->
                      ( add_missing_parameter_annotation_error
                          ~errors
                          ~given_annotation:(Some annotation)
                          (Some value_annotation),
                        Annotation.create_immutable annotation )
                  | Some annotation, _ when Type.contains_final annotation ->
                      ( add_final_parameter_annotation_error ~errors,
                        Annotation.create_immutable annotation )
                  | Some annotation, None when contains_prohibited_any annotation ->
                      ( add_missing_parameter_annotation_error
                          ~errors
                          ~given_annotation:(Some annotation)
                          None,
                        Annotation.create_immutable annotation )
                  | Some annotation, _ -> errors, Annotation.create_immutable annotation
                  | None, Some value_annotation ->
                      ( add_missing_parameter_annotation_error
                          ~errors
                          ~given_annotation:None
                          (Some value_annotation),
                        Annotation.create Type.Any )
                  | None, None ->
                      ( add_missing_parameter_annotation_error ~errors ~given_annotation:None None,
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
            errors, { Annotation.annotation; mutability }
          in
          let errors, { Annotation.annotation; mutability } =
            if String.is_prefix ~prefix:"**" name then
              parse_as_unary ()
            else if String.is_prefix ~prefix:"*" name then
              let make_tuple bounded =
                Type.Tuple (Bounded bounded)
                |> Type.Variable.mark_all_variables_as_bound
                |> Annotation.create
                |> fun annotation -> errors, annotation
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
          ( errors,
            Map.set
              annotation_store
              ~key:(make_parameter_name name)
              ~data:(RefinementUnit.create ~base:{ Annotation.annotation; mutability } ()) )
        in
        let number_of_stars name = Identifier.split_star name |> fst |> String.length in
        match List.rev parameters, parent with
        | [], Some _ when not (Define.is_class_toplevel define || Define.is_static_method define) ->
            let errors =
              let name =
                if Define.is_class_method define || Define.is_class_property define then
                  "cls"
                else
                  "self"
              in
              emit_error
                ~errors
                ~location
                ~kind:(Error.InvalidMethodSignature { annotation = None; name })
            in
            errors, Resolution.annotation_store resolution
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
                  List.rev reversed_head
                  |> List.foldi ~init:(errors, annotations) ~f:check_parameter
                else
                  let errors =
                    let origin =
                      if Define.is_toplevel (Node.value Context.define) then
                        Error.Toplevel
                      else if Define.is_class_toplevel (Node.value Context.define) then
                        Error.ClassToplevel
                      else
                        Error.Define
                    in
                    emit_error
                      ~errors
                      ~location
                      ~kind:
                        (Error.InvalidTypeVariable
                           { annotation = ParameterVariadic variable; origin })
                  in
                  errors, add_annotations { positional_component = Top; keyword_component = Top }
            | None ->
                List.foldi
                  ~init:(errors, Resolution.annotation_store resolution)
                  ~f:check_parameter
                  parameters )
        | _ ->
            List.foldi
              ~init:(errors, Resolution.annotation_store resolution)
              ~f:check_parameter
              parameters
      in
      Resolution.with_annotation_store resolution ~annotation_store, errors
    in
    let check_base_annotations resolution errors =
      if Define.is_class_toplevel define then
        let open Annotated in
        let check_base old_errors ({ ExpressionCall.Argument.value; _ } as base) =
          let annotation_errors, parsed = parse_and_check_annotation ~resolution value in
          let errors = List.append annotation_errors old_errors in
          match parsed with
          | Type.Parametric { name = "type"; parameters = [Single Type.Any] } ->
              (* Inheriting from type makes you a metaclass, and we don't want to
               * suggest that instead you need to use typing.Type[Something] *)
              old_errors
          | Primitive base_name ->
              let is_subclass_typed_dictionary =
                define.signature.parent
                >>| Reference.show
                >>| (fun subclass_name ->
                      GlobalResolution.is_typed_dictionary
                        ~resolution:global_resolution
                        (Primitive subclass_name))
                |> Option.value ~default:false
              in
              if
                is_subclass_typed_dictionary
                && not
                     ( GlobalResolution.is_typed_dictionary
                         ~resolution:global_resolution
                         (Primitive base_name)
                     || Type.TypedDictionary.is_builtin_typed_dictionary_class base_name )
              then
                emit_error
                  ~errors
                  ~location:(Node.location value)
                  ~kind:
                    (InvalidInheritance
                       (UninheritableType
                          { annotation = parsed; is_parent_class_typed_dictionary = true }))
              else
                errors
          | Top
          (* There's some other problem we already errored on *)
          | Parametric _ ->
              errors
          | Any when GlobalResolution.base_is_from_placeholder_stub global_resolution base -> errors
          | annotation ->
              emit_error
                ~errors
                ~location:(Node.location value)
                ~kind:
                  (InvalidInheritance
                     (UninheritableType { annotation; is_parent_class_typed_dictionary = false }))
        in
        let bases =
          Node.create define ~location
          |> Define.create
          |> Define.parent_definition ~resolution:global_resolution
          >>| Node.value
          >>| ClassSummary.bases
          |> Option.value ~default:[]
        in
        List.fold ~init:errors ~f:check_base bases
      else
        errors
    in
    let check_behavioral_subtyping resolution errors =
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
                GlobalResolution.overrides
                  (Reference.show parent)
                  ~resolution:global_resolution
                  ~name:(StatementDefine.unqualified_name define)
                >>| fun overridden_attribute ->
                let errors =
                  match AnnotatedAttribute.visibility overridden_attribute with
                  | ReadOnly (Refinable { overridable = false }) ->
                      let parent = overridden_attribute |> Attribute.parent in
                      emit_error
                        ~errors
                        ~location
                        ~kind:(Error.InvalidOverride { parent; decorator = Final })
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
                    emit_error ~errors ~location ~kind:(Error.InvalidOverride { parent; decorator })
                  else
                    errors
                in
                (* Check strengthening of postcondition. *)
                match Annotation.annotation (Attribute.annotation overridden_attribute) with
                | Type.Callable { Type.Callable.implementation; _ }
                  when not (StatementDefine.is_static_method define) ->
                    let original_implementation =
                      resolve_reference_type ~resolution (Node.value name)
                      |> function
                      | Type.Callable { Type.Callable.implementation = original_implementation; _ }
                        ->
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
                        emit_error
                          ~errors
                          ~location
                          ~kind:
                            (Error.InconsistentOverride
                               {
                                 overridden_method = StatementDefine.unqualified_name define;
                                 parent = Attribute.parent overridden_attribute |> Reference.create;
                                 override_kind = Method;
                                 override =
                                   Error.WeakenedPostcondition
                                     (Error.create_mismatch
                                        ~resolution:global_resolution
                                        ~actual
                                        ~expected
                                        ~covariant:false);
                               })
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
                                emit_error
                                  ~errors
                                  ~location
                                  ~kind:
                                    (Error.InconsistentOverride
                                       {
                                         overridden_method = StatementDefine.unqualified_name define;
                                         parent =
                                           Attribute.parent overridden_attribute |> Reference.create;
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
                              emit_error
                                ~errors
                                ~location
                                ~kind:
                                  (Error.InconsistentOverride
                                     {
                                       overridden_method = StatementDefine.unqualified_name define;
                                       override_kind = Method;
                                       parent =
                                         Attribute.parent overridden_attribute |> Reference.create;
                                       override =
                                         Error.StrengthenedPrecondition
                                           (Error.NotFound overridden_parameter);
                                     })
                      in
                      match overridden_parameter with
                      | Type.Callable.Parameter.PositionalOnly { index; annotation; _ } ->
                          List.nth overriding_parameters index
                          >>= (function
                                | PositionalOnly { annotation; _ }
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
                          (* TODO(T44178876): There is no reasonable way to compare either of these
                             alone, which is the central issue with this comparison strategy. For
                             now, let's just ignore this *)
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
    let check_constructor_return errors =
      if not (Define.is_constructor define) then
        errors
      else
        match return_annotation with
        | Some ({ Node.location; _ } as annotation) -> (
            match define with
            | { Define.signature = { Define.Signature.name = { Node.value = name; _ }; _ }; _ }
              when String.equal (Reference.last name) "__new__" ->
                (* TODO(T45018328): Error here. `__new__` is a special undecorated static method,
                   and we really ought to be checking its return type against typing.Type[Cls]. *)
                errors
            | _ ->
                let annotation = GlobalResolution.parse_annotation global_resolution annotation in
                if Type.is_none annotation then
                  errors
                else
                  emit_error
                    ~errors
                    ~location
                    ~kind:(Error.IncompatibleConstructorAnnotation annotation) )
        | _ -> errors
    in
    let resolution, errors =
      let resolution = Resolution.with_parent resolution ~parent in
      let resolution, errors = add_capture_annotations resolution [] in
      let resolution, errors = check_parameter_annotations resolution errors in
      let errors =
        check_return_annotation resolution errors
        |> check_decorators resolution
        |> check_base_annotations resolution
        |> check_behavioral_subtyping resolution
        |> check_constructor_return
      in
      resolution, errors
    in
    let state =
      let resolution_fixpoint = LocalAnnotationMap.empty () in
      let error_map = LocalErrorMap.empty () in
      let postcondition = Resolution.annotation_store resolution in
      let key = [%hash: int * int] (Cfg.entry_index, 0) in
      LocalAnnotationMap.set resolution_fixpoint ~key ~postcondition;
      LocalErrorMap.set error_map ~key ~errors;
      { resolution = Some resolution; resolution_fixpoint; error_map }
    in
    state


  and forward_expression ~resolution ~expression:{ Node.location; value } =
    let global_resolution = Resolution.global_resolution resolution in
    let forward_entry ~resolution ~errors ~entry:{ Dictionary.Entry.key; value } =
      let { Resolved.resolution; resolved = key_resolved; errors = key_errors; _ } =
        forward_expression ~resolution ~expression:key
      in
      let { Resolved.resolution; resolved = value_resolved; errors = value_errors; _ } =
        forward_expression ~resolution ~expression:value
      in
      ( Type.weaken_literals key_resolved,
        Type.weaken_literals value_resolved,
        resolution,
        List.concat [key_errors; value_errors; errors] )
    in
    let forward_generator
        ~resolution
        ~errors
        ~generator:({ Comprehension.Generator.conditions; _ } as generator)
      =
      (* Propagate the target type information. *)
      let iterator =
        Statement.Assign (Ast.Statement.Statement.generator_assignment generator)
        |> Node.create ~location
      in
      let resolution, errors =
        let iterator_resolution, iterator_errors =
          let post_resolution, errors = forward_statement ~resolution ~statement:iterator in
          Option.value post_resolution ~default:resolution, errors
        in
        let iterator_errors =
          (* Don't throw Incompatible Variable errors on the generated iterator assign; we are
             temporarily minting a variable in a new scope and old annotations should be ignored. *)
          let is_not_assignment_error = function
            | { Error.kind = Error.IncompatibleVariableType _; _ } -> false
            | _ -> true
          in
          List.filter ~f:is_not_assignment_error iterator_errors
        in
        (* We want the resolution in the iterator assignment -- they will become useful in
           `forward_comprehension`. Don't worry about annotation store pollutions as we will throw
           away generator-local variables there. *)
        iterator_resolution, List.append iterator_errors errors
      in
      List.map conditions ~f:Statement.assume
      |> List.fold ~init:(resolution, errors) ~f:(fun (resolution, errors) statement ->
             let resolution, new_errors =
               let post_resolution, errors = forward_statement ~resolution ~statement in
               Option.value post_resolution ~default:resolution, errors
             in
             resolution, List.append new_errors errors)
    in
    let forward_comprehension ~resolution ~errors ~element ~generators =
      let resolved, errors =
        List.fold
          generators
          ~f:(fun (resolution, errors) generator ->
            forward_generator ~resolution ~errors ~generator)
          ~init:(resolution, errors)
        |> fun (resolution, errors) ->
        let { Resolved.resolved; errors = element_errors; _ } =
          forward_expression ~resolution ~expression:element
        in
        resolved, List.append element_errors errors
      in
      (* Discard generator-local variables. *)
      {
        Resolved.resolution;
        resolved = Type.weaken_literals resolved;
        resolved_annotation = None;
        base = None;
        errors;
      }
    in
    let forward_elements ~resolution ~errors ~elements =
      let forward_element { Resolved.resolution; resolved; errors; _ } expression =
        match Node.value expression with
        | Expression.Starred (Starred.Once expression) ->
            let { Resolved.resolution; resolved = new_resolved; errors = new_errors; _ } =
              forward_expression ~resolution ~expression
            in
            let parameter =
              match
                GlobalResolution.extract_type_parameters
                  global_resolution
                  ~target:"typing.Iterable"
                  ~source:new_resolved
              with
              | Some [element_type] -> element_type
              | _ -> Type.Any
            in
            {
              Resolved.resolution;
              resolved = GlobalResolution.join global_resolution resolved parameter;
              errors = List.append new_errors errors;
              resolved_annotation = None;
              base = None;
            }
        | _ ->
            let { Resolved.resolution; resolved = new_resolved; errors = new_errors; _ } =
              forward_expression ~resolution ~expression
            in
            {
              resolution;
              resolved = GlobalResolution.join global_resolution resolved new_resolved;
              errors = List.append new_errors errors;
              resolved_annotation = None;
              base = None;
            }
      in
      let correct_bottom { Resolved.resolution; resolved; errors; _ } =
        let resolved =
          if Type.is_unbound resolved then
            Type.variable "_T" |> Type.Variable.mark_all_free_variables_as_escaped
          else
            resolved
        in
        { Resolved.resolution; errors; resolved; resolved_annotation = None; base = None }
      in
      List.fold
        elements
        ~init:
          {
            Resolved.resolution;
            errors;
            resolved = Type.Bottom;
            resolved_annotation = None;
            base = None;
          }
        ~f:forward_element
      |> (fun { Resolved.resolution; errors; resolved; _ } ->
           {
             Resolved.resolution;
             errors;
             resolved = Type.weaken_literals resolved;
             resolved_annotation = None;
             base = None;
           })
      |> correct_bottom
    in
    let forward_reference ~resolution ~errors reference =
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
          let errors =
            Error.UndefinedName reference |> fun kind -> emit_error ~errors ~location ~kind
          in
          {
            Resolved.resolution;
            errors;
            resolved = Annotation.annotation annotation;
            resolved_annotation = Some annotation;
            base = None;
          }
      | Some annotation ->
          {
            resolution;
            errors;
            resolved = Annotation.annotation annotation;
            resolved_annotation = Some annotation;
            base = None;
          }
      | None -> (
          match GlobalResolution.module_exists global_resolution reference with
          | false when not (GlobalResolution.is_suppressed_module global_resolution reference) ->
              let errors =
                match Reference.prefix reference with
                | Some qualifier when not (Reference.is_empty qualifier) ->
                    if GlobalResolution.module_exists global_resolution qualifier then
                      emit_error
                        ~errors
                        ~location
                        ~kind:
                          (Error.UndefinedAttribute
                             {
                               attribute = Reference.last reference;
                               origin = Error.Module qualifier;
                             })
                    else
                      errors
                | _ -> emit_error ~errors ~location ~kind:(Error.UndefinedName reference)
              in
              { resolution; errors; resolved = Type.Top; resolved_annotation = None; base = None }
          | _ ->
              { resolution; errors; resolved = Type.Top; resolved_annotation = None; base = None } )
    in
    let forward_callable ~resolution ~errors ~target ~dynamic ~callee ~resolved ~arguments =
      let resolution, errors =
        let forward_argument (resolution, errors) { Call.Argument.value; _ } =
          forward_expression ~resolution ~expression:value
          |> fun { resolution; errors = new_errors; _ } -> resolution, List.append new_errors errors
        in
        List.fold arguments ~f:forward_argument ~init:(resolution, errors)
      in
      let find_method ~parent ~name =
        let attribute_of_annotation instantiated ~name =
          Type.split instantiated
          |> fst
          |> Type.primitive_name
          >>= GlobalResolution.attribute_from_class_name
                ~resolution:global_resolution
                ~name
                ~instantiated
                ~transitive:true
          >>= fun attribute ->
          Option.some_if (Annotated.Attribute.defined attribute) attribute
          >>| Annotated.Attribute.annotation
          >>| Annotation.annotation
        in
        attribute_of_annotation parent ~name
        >>= function
        | Type.Callable callable -> Some callable
        | Parametric { name = "BoundMethod"; _ } as parent -> (
            match attribute_of_annotation parent ~name:"__call__" with
            | Some (Type.Callable callable) -> Some callable
            | _ -> None )
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
                | TypedDictionary { name; fields } ->
                    Type.TypedDictionary.constructor ~name ~fields |> Option.some
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
                    >>| GlobalResolution.constructor
                          ~instantiated:meta_parameter
                          ~resolution:global_resolution
                    >>= function
                    | Type.Callable callable -> Some callable
                    | other -> find_method ~parent:other ~name:"__call__" ) )
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
                          find_method ~parent:(resolve_expression_type ~resolution value) ~name)
                    >>= (fun found_callable ->
                          let resolved_base = resolve_expression_type ~resolution base in
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
              ~resolve:(resolve_expression_type ~resolution)
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
                  >>= (fun name ->
                        find_method ~parent:(resolve_expression_type ~resolution value) ~name)
                  >>| (fun callable ->
                        GlobalResolution.signature_select
                          ~arguments
                          ~global_resolution:(Resolution.global_resolution resolution)
                          ~resolve:(resolve_expression_type ~resolution)
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
                          GlobalResolution.attributes
                            ~transitive:true
                            class_name
                            ~resolution:global_resolution
                          >>| List.filter ~f:AnnotatedAttribute.abstract
                          |> Option.value ~default:[]
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
      let error_from_not_found
          ~callable:
            ( { Type.Callable.implementation = { annotation; parameters }; kind; implicit; _ } as
            callable )
          ~reason
        =
        let errors =
          let error_location, error_kind =
            let callee =
              match kind with
              | Type.Callable.Named callable -> Some callable
              | _ -> None
            in
            match reason with
            | AttributeResolution.AbstractClassInstantiation { class_name; abstract_methods } ->
                ( location,
                  Error.InvalidClassInstantiation
                    (Error.AbstractClassInstantiation { class_name; abstract_methods }) )
            | CallingParameterVariadicTypeVariable ->
                location, Error.NotCallable (Type.Callable callable)
            | InvalidKeywordArgument { Node.location; value = { expression; annotation } } ->
                location, Error.InvalidArgument (Error.Keyword { expression; annotation })
            | InvalidVariableArgument { Node.location; value = { expression; annotation } } ->
                location, Error.InvalidArgument (Error.ConcreteVariable { expression; annotation })
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
                  let typed_dictionary_error
                      ~method_name
                      ~position
                      { Type.Record.TypedDictionary.fields; name = typed_dictionary_name }
                    =
                    if
                      Type.TypedDictionary.is_special_mismatch
                        ~class_name:typed_dictionary_name
                        ~method_name
                        ~position
                        ~total:(Type.TypedDictionary.are_fields_total fields)
                    then
                      match actual with
                      | Type.Literal (Type.String field_name) ->
                          let required_field_exists =
                            List.exists
                              ~f:(fun { Type.Record.TypedDictionary.name; required; _ } ->
                                String.equal name field_name && required)
                              fields
                          in
                          if required_field_exists then
                            Error.TypedDictionaryInvalidOperation
                              { typed_dictionary_name; field_name; method_name; mismatch }
                          else
                            Error.TypedDictionaryKeyNotFound
                              { typed_dictionary_name; missing_key = field_name }
                      | Type.Primitive "str" ->
                          Error.TypedDictionaryAccessWithNonLiteral
                            (List.map fields ~f:(fun { name; _ } -> name))
                      | _ -> normal
                    else
                      match method_name with
                      | "__setitem__" ->
                          let field_name =
                            match parameters with
                            | Type.Record.Callable.Defined
                                [
                                  Type.Record.Callable.RecordParameter.Named
                                    {
                                      name = "k";
                                      annotation = Type.Literal (Type.String field_name);
                                      _;
                                    };
                                  _;
                                ] ->
                                field_name
                            | _ -> failwith "Expected __setitem__ callable to have a key parameter"
                          in
                          Error.TypedDictionaryInvalidOperation
                            { typed_dictionary_name; field_name; method_name; mismatch }
                      | _ -> normal
                  in
                  match implicit, callee >>| Reference.as_list with
                  | ( Some { implicit_annotation = Type.TypedDictionary typed_dictionary; _ },
                      Some [_; method_name] ) ->
                      typed_dictionary_error ~method_name ~position typed_dictionary
                  | ( Some { implicit_annotation = Type.Primitive _ as annotation; _ },
                      Some [_; method_name] ) ->
                      GlobalResolution.get_typed_dictionary ~resolution:global_resolution annotation
                      >>| typed_dictionary_error ~method_name ~position
                      |> Option.value ~default:normal
                  | _ -> normal
                in
                location, kind
            | MismatchWithListVariadicTypeVariable { variable; mismatch } ->
                location, Error.InvalidArgument (ListVariadicVariable { variable; mismatch })
            | MissingArgument parameter -> location, Error.MissingArgument { callee; parameter }
            | MutuallyRecursiveTypeVariables ->
                location, Error.MutuallyRecursiveTypeVariables callee
            | ProtocolInstantiation class_name ->
                location, Error.InvalidClassInstantiation (ProtocolInstantiation class_name)
            | TooManyArguments { expected; provided } ->
                location, Error.TooManyArguments { callee; expected; provided }
            | UnexpectedKeyword name -> location, Error.UnexpectedKeyword { callee; name }
          in
          emit_error ~errors ~location:error_location ~kind:error_kind
        in
        {
          Resolved.resolution;
          errors;
          resolved = annotation;
          resolved_annotation = None;
          base = None;
        }
      in

      let not_found = function
        | AttributeResolution.NotFound _ -> true
        | _ -> false
      in
      match signatures >>| List.partition_tf ~f:not_found with
      (* Prioritize missing signatures for union type checking. *)
      | Some (AttributeResolution.NotFound { callable; reason = Some reason } :: _, _) ->
          error_from_not_found ~callable ~reason
      | Some ([], head :: tail) ->
          let resolved =
            let extract = function
              | AttributeResolution.Found { Type.Callable.implementation = { annotation; _ }; _ } ->
                  annotation
              | _ -> failwith "Not all signatures were found."
            in
            List.map tail ~f:extract
            |> List.fold ~f:(GlobalResolution.join global_resolution) ~init:(extract head)
          in
          { resolution; errors; resolved; resolved_annotation = None; base = None }
      | _ ->
          let errors =
            match resolved, potential_missing_operator_error with
            | Type.Top, Some kind -> emit_error ~errors ~location ~kind
            | Type.Any, _
            | Type.Top, _ ->
                errors
            | _ -> emit_error ~errors ~location ~kind:(Error.NotCallable resolved)
          in
          { resolution; errors; resolved = Type.Top; resolved_annotation = None; base = None }
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
    | Await expression -> (
        let { Resolved.resolution; resolved; errors; _ } =
          forward_expression ~resolution ~expression
        in
        match resolved with
        | Type.Any ->
            { resolution; resolved = Type.Any; errors; resolved_annotation = None; base = None }
        | _ -> (
            match
              GlobalResolution.extract_type_parameters
                global_resolution
                ~target:"typing.Awaitable"
                ~source:resolved
            with
            | Some [awaited_type] ->
                {
                  resolution;
                  resolved = awaited_type;
                  errors;
                  resolved_annotation = None;
                  base = None;
                }
            | _ ->
                let errors =
                  emit_error ~errors ~location ~kind:(Error.IncompatibleAwaitableType resolved)
                in
                { resolution; resolved = Type.Any; errors; resolved_annotation = None; base = None }
            ) )
    | BooleanOperator { BooleanOperator.left; operator; right } ->
        let assume =
          let assume =
            match operator with
            | BooleanOperator.And -> left
            | BooleanOperator.Or -> normalize (negate left)
          in
          Statement.assume assume
        in
        let { Resolved.resolution = resolution_left; resolved = resolved_left; _ } =
          forward_expression ~resolution ~expression:left
        in
        let resolution_right, resolved_right, errors =
          let resolution, errors_left =
            let post_resolution, errors = forward_statement ~resolution ~statement:assume in
            Option.value post_resolution ~default:resolution, errors
          in
          let { Resolved.resolution; resolved; errors = errors_right; _ } =
            forward_expression ~resolution ~expression:right
          in
          resolution, resolved, List.append errors_left errors_right
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
        let resolution = join_resolutions resolution_left resolution_right in
        { resolution; errors; resolved; resolved_annotation = None; base = None }
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
            {
              resolution;
              errors = [];
              resolved;
              resolved_annotation = None;
              base = Some (Super resolved);
            }
        | None ->
            let { Resolved.resolved; _ } = forward_expression ~resolution ~expression:callee in
            forward_callable
              ~resolution
              ~errors:[]
              ~target:None
              ~callee
              ~dynamic:false
              ~resolved
              ~arguments )
    | Call
        {
          callee = { Node.value = Name (Name.Identifier "type"); _ };
          arguments = [{ Call.Argument.value; _ }];
        } ->
        (* Resolve `type()` calls. *)
        let resolved = resolve_expression_type ~resolution value |> Type.meta in
        { resolution; errors = []; resolved; resolved_annotation = None; base = None }
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
          let annotation = resolve_expression ~resolution value in
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
        let errors =
          emit_error
            ~errors:[]
            ~location
            ~kind:(Error.RevealedType { expression = value; annotation })
        in
        { resolution; errors; resolved = Type.none; resolved_annotation = None; base = None }
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
        let errors, cast_annotation = parse_and_check_annotation ~resolution cast_annotation in
        let resolution, resolved, errors =
          let { Resolved.resolution; resolved; errors = value_errors; _ } =
            forward_expression ~resolution ~expression:value
          in
          resolution, resolved, List.append value_errors errors
        in
        let errors =
          if contains_literal_any then
            emit_error
              ~errors
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
            emit_error ~errors ~location ~kind:(Error.RedundantCast resolved)
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
              ~errors
              ~location
              ~kind:(Error.UnsafeCast { expression = value; annotation = resolved })
          else
            errors
        in
        { resolution; errors; resolved = cast_annotation; resolved_annotation = None; base = None }
    | Call
        {
          callee = { Node.value = Name (Name.Identifier "isinstance"); _ } as callee;
          arguments =
            [{ Call.Argument.value = expression; _ }; { Call.Argument.value = annotations; _ }] as
            arguments;
        } ->
        let callables =
          let { Resolved.resolved; _ } = forward_expression ~resolution ~expression:callee in
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
        let resolution, errors =
          let { Resolved.resolution; errors; _ } = forward_expression ~resolution ~expression in
          let resolution, errors, annotations =
            let rec collect_types (state, errors, collected) = function
              | { Node.value = Expression.Tuple annotations; _ } ->
                  List.fold annotations ~init:(state, errors, collected) ~f:collect_types
              | expression ->
                  let { Resolved.resolution; resolved; errors = expression_errors; _ } =
                    forward_expression ~resolution ~expression
                  in
                  let new_annotations =
                    match resolved with
                    | Type.Tuple (Type.Bounded (Concrete annotations)) ->
                        List.map annotations ~f:(fun annotation ->
                            annotation, Node.location expression)
                    | Type.Tuple (Type.Unbounded annotation)
                    | annotation ->
                        [annotation, Node.location expression]
                  in
                  resolution, List.append expression_errors errors, new_annotations @ collected
            in
            collect_types (resolution, errors, []) annotations
          in
          let add_incompatible_non_meta_error errors (non_meta, location) =
            emit_error
              ~errors
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
                             [Type.meta Type.Any; Type.Tuple (Type.Unbounded (Type.meta Type.Any))];
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
          let errors =
            List.find annotations ~f:(fun (annotation, _) -> not (is_compatible annotation))
            >>| add_incompatible_non_meta_error errors
            |> Option.value ~default:errors
          in
          resolution, errors
        in
        { resolution; errors; resolved = Type.bool; resolved_annotation = None; base = None }
    | Call
        {
          callee =
            {
              Node.value = Name (Name.Attribute { attribute = "assertIsNotNone" | "assertTrue"; _ });
              _;
            } as callee;
          arguments = [{ Call.Argument.value = expression; _ }] as arguments;
        } ->
        let resolution, resolved, errors =
          let resolution, assume_errors =
            let post_resolution, errors =
              forward_statement ~resolution ~statement:(Statement.assume expression)
            in
            Option.value post_resolution ~default:resolution, errors
          in
          let { Resolved.resolution; resolved; errors = callee_errors; _ } =
            forward_expression ~resolution ~expression:callee
          in
          resolution, resolved, List.append assume_errors callee_errors
        in
        forward_callable ~resolution ~errors ~target:None ~dynamic:true ~callee ~resolved ~arguments
    | Call
        {
          callee =
            { Node.value = Name (Name.Attribute { attribute = "assertFalse"; _ }); _ } as callee;
          arguments = [{ Call.Argument.value = expression; _ }] as arguments;
        } ->
        let resolution, resolved, errors =
          let resolution, assume_errors =
            let post_resolution, errors =
              forward_statement ~resolution ~statement:(Statement.assume (negate expression))
            in
            Option.value post_resolution ~default:resolution, errors
          in
          let { Resolved.resolution; resolved; errors = callee_errors; _ } =
            forward_expression ~resolution ~expression:callee
          in
          resolution, resolved, List.append assume_errors callee_errors
        in
        forward_callable ~resolution ~errors ~target:None ~dynamic:true ~callee ~resolved ~arguments
    | Call call ->
        let { Call.callee; arguments } = AnnotatedCall.redirect_special_calls ~resolution call in
        let { Resolved.errors = callee_errors; resolved = resolved_callee; base; _ } =
          forward_expression ~resolution ~expression:callee
        in
        let { Resolved.resolution = updated_resolution; resolved; errors = updated_errors; _ } =
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
              let forward_inner_callable (resolution, errors, annotations) inner_resolved_callee =
                let target, dynamic = target_and_dynamic inner_resolved_callee in
                forward_callable
                  ~resolution
                  ~errors
                  ~target
                  ~dynamic
                  ~callee
                  ~resolved:inner_resolved_callee
                  ~arguments
                |> fun { resolution; resolved; errors = new_errors; _ } ->
                resolution, List.append new_errors errors, resolved :: annotations
              in
              let resolution, errors, return_annotations =
                List.fold_left
                  ~f:forward_inner_callable
                  ~init:(resolution, callee_errors, [])
                  (List.map ~f:Type.meta resolved_callees)
              in
              {
                resolution;
                errors;
                resolved = Type.union return_annotations;
                resolved_annotation = None;
                base = None;
              }
          | _ ->
              let target, dynamic = target_and_dynamic resolved_callee in
              forward_callable
                ~resolution
                ~errors:callee_errors
                ~target
                ~dynamic
                ~callee
                ~resolved:resolved_callee
                ~arguments
        in
        if
          List.is_empty (List.filter ~f:is_terminating_error callee_errors)
          || not (Type.is_top resolved_callee || Type.is_undeclared resolved_callee)
        then
          {
            resolution = updated_resolution;
            errors = updated_errors;
            resolved;
            resolved_annotation = None;
            base = None;
          }
        else (* Do not throw more errors if callee already contains terminating error. *)
          { resolution; errors = callee_errors; resolved; resolved_annotation = None; base = None }
    | ComparisonOperator { ComparisonOperator.left; right; operator = ComparisonOperator.In }
    | ComparisonOperator { ComparisonOperator.left; right; operator = ComparisonOperator.NotIn } ->
        let resolve_in_call
            (resolution, errors, joined_annotation)
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
          let { Resolved.resolution; resolved; errors; _ } =
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
                  ~resolution
                  ~errors
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
                    let forward_method
                        ~method_name
                        ~arguments
                        { Resolved.resolution; resolved = parent; errors; _ }
                      =
                      Type.split parent
                      |> fst
                      |> Type.primitive_name
                      >>= (fun class_name -> resolve_method class_name parent method_name)
                      >>| fun callable ->
                      forward_callable
                        ~resolution
                        ~errors
                        ~target:(Some parent)
                        ~resolved:callable
                        ~arguments:
                          (List.map arguments ~f:(fun value -> { Call.Argument.name = None; value }))
                    in
                    forward_callable
                      ~resolution
                      ~errors
                      ~target:(Some instantiated)
                      ~resolved:iter_callable
                      ~arguments:[]
                    |> forward_method ~method_name:"__next__" ~arguments:[]
                    >>= forward_method ~method_name:"__eq__" ~arguments:[left]
                    |> Option.value
                         ~default:
                           {
                             Resolved.resolution;
                             errors;
                             resolved = Type.Top;
                             resolved_annotation = None;
                             base = None;
                           }
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
                    forward_expression ~resolution ~expression:call )
          in
          resolution, errors, GlobalResolution.join global_resolution joined_annotation resolved
        in
        let { Resolved.resolution; resolved; errors; _ } =
          forward_expression ~resolution ~expression:right
        in
        let resolution, errors, resolved =
          Type.resolve_class resolved
          >>| List.fold ~f:resolve_in_call ~init:(resolution, errors, Type.Bottom)
          |> Option.value ~default:(resolution, errors, Type.Bottom)
        in
        let resolved = if Type.equal resolved Type.Bottom then Type.Top else resolved in
        { resolution; errors; resolved; resolved_annotation = None; base = None }
    | ComparisonOperator ({ ComparisonOperator.left; right; _ } as operator) -> (
        let resolution, errors, left =
          match left with
          | { Node.value = WalrusOperator { target; _ }; _ } ->
              let { Resolved.resolution; errors; _ } =
                forward_expression ~resolution ~expression:left
              in
              resolution, errors, target
          | _ -> resolution, [], left
        in
        let operator = { operator with left } in
        match ComparisonOperator.override operator with
        | Some expression ->
            let resolved = forward_expression ~resolution ~expression in
            { resolved with errors = List.append errors resolved.errors }
        | None ->
            forward_expression ~resolution ~expression:left
            |> (fun { Resolved.resolution; errors = left_errors; _ } ->
                 let { Resolved.resolution; errors = right_errors; _ } =
                   forward_expression ~resolution ~expression:right
                 in
                 resolution, List.concat [errors; left_errors; right_errors])
            |> fun (resolution, errors) ->
            {
              Resolved.resolution;
              errors;
              resolved = Type.bool;
              resolved_annotation = None;
              base = None;
            } )
    | Complex _ ->
        {
          resolution;
          errors = [];
          resolved = Type.complex;
          resolved_annotation = None;
          base = None;
        }
    | Dictionary { Dictionary.entries; keywords } ->
        let key, value, resolution, errors =
          let forward_entry (key, value, resolution, errors) entry =
            let new_key, new_value, resolution, errors = forward_entry ~resolution ~errors ~entry in
            ( GlobalResolution.join global_resolution key new_key,
              GlobalResolution.join global_resolution value new_value,
              resolution,
              errors )
          in
          List.fold entries ~f:forward_entry ~init:(Type.Bottom, Type.Bottom, resolution, [])
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
        let resolved, resolution, errors =
          let forward_keyword (resolved, resolution, errors) keyword =
            let { Resolved.resolution; resolved = keyword_resolved; errors = new_errors; _ } =
              forward_expression ~resolution ~expression:keyword
            in
            ( GlobalResolution.join global_resolution resolved keyword_resolved,
              resolution,
              List.append new_errors errors )
          in
          List.fold
            keywords
            ~f:forward_keyword
            ~init:(Type.dictionary ~key ~value, resolution, errors)
        in
        { resolution; errors; resolved; resolved_annotation = None; base = None }
    | DictionaryComprehension { Comprehension.element; generators } ->
        let key, value, _, errors =
          List.fold
            generators
            ~f:(fun (resolution, errors) generator ->
              forward_generator ~resolution ~errors ~generator)
            ~init:(resolution, [])
          |> fun (resolution, errors) -> forward_entry ~resolution ~errors ~entry:element
        in
        (* Discard generator-local variables. *)
        {
          resolution;
          errors;
          resolved = Type.dictionary ~key ~value;
          resolved_annotation = None;
          base = None;
        }
    | Ellipsis ->
        { resolution; errors = []; resolved = Type.Any; resolved_annotation = None; base = None }
    | False ->
        {
          resolution;
          errors = [];
          resolved = Type.Literal (Type.Boolean false);
          resolved_annotation = None;
          base = None;
        }
    | Float _ ->
        { resolution; errors = []; resolved = Type.float; resolved_annotation = None; base = None }
    | Generator { Comprehension.element; generators } ->
        let { Resolved.resolution; resolved; errors; _ } =
          forward_comprehension ~resolution ~errors:[] ~element ~generators
        in
        {
          resolution;
          errors;
          resolved = Type.generator resolved;
          resolved_annotation = None;
          base = None;
        }
    | Integer literal ->
        {
          resolution;
          errors = [];
          resolved = Type.literal_integer literal;
          resolved_annotation = None;
          base = None;
        }
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
        let { Resolved.resolved; errors; _ } =
          forward_expression ~resolution:resolution_with_parameters ~expression:body
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
          resolution;
          errors;
          resolved = Type.Callable.create ~parameters ~annotation:resolved ();
          resolved_annotation = None;
          base = None;
        }
    | List elements ->
        let { Resolved.resolution; resolved; errors; _ } =
          forward_elements ~resolution ~errors:[] ~elements
        in
        {
          resolution;
          errors;
          resolved = Type.list resolved;
          resolved_annotation = None;
          base = None;
        }
    | ListComprehension { Comprehension.element; generators } ->
        let { Resolved.resolution; resolved; errors; _ } =
          forward_comprehension ~resolution ~errors:[] ~element ~generators
        in
        {
          resolution;
          errors;
          resolved = Type.list resolved;
          resolved_annotation = None;
          base = None;
        }
    | Name (Name.Identifier identifier) ->
        forward_reference ~resolution ~errors:[] (Reference.create identifier)
    | Name (Name.Attribute { base; attribute; special } as name) ->
        let reference = name_to_reference name in
        let { Resolved.errors = base_errors; resolved = resolved_base; base = super_base; _ } =
          forward_expression ~resolution ~expression:base
        in
        let errors, resolved_base =
          if Type.Variable.contains_escaped_free_variable resolved_base then
            let errors =
              emit_error
                ~errors:[]
                ~location
                ~kind:
                  (Error.IncompleteType
                     {
                       target = base;
                       annotation = resolved_base;
                       attempted_action = Error.AttributeAccess attribute;
                     })
            in
            errors, Type.Variable.convert_all_escaped_free_variables_to_anys resolved_base
          else
            [], resolved_base
        in
        let {
          Resolved.resolution = updated_resolution;
          errors = updated_errors;
          resolved;
          resolved_annotation;
          _;
        }
          =
          let access_as_attribute () =
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
                      Resolution.fallback_attribute class_name ~resolution ~name
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
                let errors =
                  emit_error
                    ~errors
                    ~location
                    ~kind:
                      (Error.UndefinedAttribute
                         {
                           attribute;
                           origin =
                             Error.Class { annotation = resolved_base; class_attribute = false };
                         })
                in
                {
                  Resolved.resolution;
                  errors;
                  resolved = Type.Top;
                  resolved_annotation = None;
                  base = None;
                }
            | Some [] ->
                {
                  Resolved.resolution;
                  errors;
                  resolved = Type.Top;
                  resolved_annotation = None;
                  base = None;
                }
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
                let errors =
                  let definition =
                    List.find (head_definition :: tail_definitions) ~f:(fun (_, undefined_target) ->
                        match undefined_target with
                        | None -> false
                        | _ -> true)
                    |> Option.value ~default:head_definition
                  in
                  match reference, definition with
                  | Some reference, (_, Some target) when Type.equal Type.undeclared target ->
                      emit_error ~errors ~location ~kind:(Error.UndefinedName reference)
                  | _, (attribute, Some target) ->
                      if Option.is_some (inverse_operator name) then
                        (* Defer any missing attribute error until the inverse operator has been
                           typechecked. *)
                        errors
                      else
                        emit_error
                          ~errors
                          ~location
                          ~kind:
                            (Error.UndefinedAttribute
                               {
                                 attribute = name;
                                 origin =
                                   Error.Class
                                     {
                                       annotation = target;
                                       class_attribute =
                                         Annotated.Attribute.class_attribute attribute;
                                     };
                               })
                  | _ ->
                      let enclosing_class_reference =
                        let open Annotated in
                        Define.parent_definition
                          ~resolution:(Resolution.global_resolution resolution)
                          (Define.create Context.define)
                        >>| Node.value
                        >>| ClassSummary.name
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
                        emit_error
                          ~errors
                          ~location
                          ~kind:
                            (Error.UndefinedAttribute
                               {
                                 attribute = name;
                                 origin =
                                   Error.Class
                                     { annotation = resolved_base; class_attribute = false };
                               })
                      else
                        errors
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
                  resolution;
                  errors;
                  resolved = Annotation.annotation resolved;
                  resolved_annotation = Some resolved;
                  base = None;
                }
          in
          if Type.is_undeclared resolved_base then
            let errors =
              reference
              >>| (fun reference -> Error.UndefinedName reference)
              >>| (fun kind -> emit_error ~errors ~location ~kind)
              |> Option.value ~default:errors
            in
            {
              resolution;
              errors;
              resolved = resolved_base;
              resolved_annotation = None;
              base = None;
            }
          else if Type.equal resolved_base Type.Top then (* Global or local. *)
            reference
            >>| forward_reference ~resolution ~errors
            |> Option.value
                 ~default:
                   {
                     Resolved.resolution;
                     errors;
                     resolved = Type.Top;
                     resolved_annotation = None;
                     base = None;
                   }
          (* TODO(T63892020): We need to fix up qualification so nested classes and functions are
             just normal locals rather than attributes of the enclosing function, which they really
             are not *)
          else if Type.is_callable resolved_base then
            let resolved =
              reference >>= fun reference -> Resolution.get_local resolution ~reference
            in
            match resolved with
            | Some annotation ->
                {
                  resolution;
                  errors;
                  resolved = Annotation.annotation annotation;
                  resolved_annotation = Some annotation;
                  base = None;
                }
            | None -> access_as_attribute ()
          else (* Attribute access. *)
            access_as_attribute ()
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
          List.is_empty (List.filter ~f:is_terminating_error base_errors)
          || not (Type.is_top resolved_base || Type.is_undeclared resolved_base)
        then
          {
            resolution = updated_resolution;
            errors = List.append base_errors updated_errors;
            resolved;
            resolved_annotation;
            base;
          }
        else (* Do not throw more errors if base already contains terminating error. *)
          { resolution; errors = base_errors; resolved; resolved_annotation; base }
    | Set elements ->
        let { Resolved.resolution; resolved; errors; _ } =
          forward_elements ~resolution ~errors:[] ~elements
        in
        {
          resolution;
          errors;
          resolved = Type.set resolved;
          resolved_annotation = None;
          base = None;
        }
    | SetComprehension { Comprehension.element; generators } ->
        let { Resolved.resolution; resolved; errors; _ } =
          forward_comprehension ~resolution ~errors:[] ~element ~generators
        in
        {
          resolution;
          errors;
          resolved = Type.set resolved;
          resolved_annotation = None;
          base = None;
        }
    | Starred starred ->
        let resolved =
          match starred with
          | Starred.Once expression
          | Starred.Twice expression ->
              forward_expression ~resolution ~expression
        in
        { resolved with resolved = Type.Top; resolved_annotation = None; base = None }
    | String { StringLiteral.kind = StringLiteral.Format expressions; _ } ->
        let resolution, errors =
          List.fold
            expressions
            ~f:(fun (resolution, errors) expression ->
              forward_expression ~resolution ~expression
              |> fun { resolution; errors = new_errors; _ } ->
              resolution, List.append new_errors errors)
            ~init:(resolution, [])
        in
        { resolution; errors; resolved = Type.string; resolved_annotation = None; base = None }
    | String { StringLiteral.kind = StringLiteral.Bytes; _ } ->
        { resolution; errors = []; resolved = Type.bytes; resolved_annotation = None; base = None }
    | String { StringLiteral.kind = StringLiteral.String; value } ->
        {
          resolution;
          errors = [];
          resolved = Type.literal_string value;
          resolved_annotation = None;
          base = None;
        }
    | String { StringLiteral.kind = StringLiteral.Mixed _; _ } ->
        (* NOTE: We may run into this case with nested f-strings. Treat them as literal strings
           until the parser gets full support of them. *)
        { resolution; errors = []; resolved = Type.string; resolved_annotation = None; base = None }
    | Ternary { Ternary.target; test; alternative } ->
        let target_resolved, target_errors =
          forward_statement ~resolution ~statement:(Statement.assume test)
          |> fun (post_resolution, test_errors) ->
          let resolution = Option.value post_resolution ~default:resolution in
          let { Resolved.resolved; errors; _ } =
            forward_expression ~resolution ~expression:target
          in
          resolved, List.append test_errors errors
        in
        let alternative_resolved, alternative_errors =
          forward_statement ~resolution ~statement:(Statement.assume (negate test))
          |> fun (post_resolution, test_errors) ->
          let resolution = Option.value post_resolution ~default:resolution in
          let { Resolved.resolved; errors; _ } =
            forward_expression ~resolution ~expression:alternative
          in
          resolved, List.append test_errors errors
        in
        let resolved =
          GlobalResolution.join global_resolution target_resolved alternative_resolved
        in
        let errors = List.append target_errors alternative_errors in
        (* The resolution is local to the ternary expression and should not be propagated out. *)
        { resolution; errors; resolved; resolved_annotation = None; base = None }
    | True ->
        {
          resolution;
          errors = [];
          resolved = Type.Literal (Type.Boolean true);
          resolved_annotation = None;
          base = None;
        }
    | Tuple elements ->
        let resolution, errors, resolved =
          let forward_element (resolution, errors, resolved) expression =
            let { Resolved.resolution; resolved = new_resolved; errors = new_errors; _ } =
              forward_expression ~resolution ~expression
            in
            resolution, List.append new_errors errors, new_resolved :: resolved
          in
          List.fold elements ~f:forward_element ~init:(resolution, [], [])
        in
        {
          resolution;
          errors;
          resolved = Type.tuple (List.rev resolved);
          resolved_annotation = None;
          base = None;
        }
    | UnaryOperator ({ UnaryOperator.operand; _ } as operator) -> (
        match UnaryOperator.override operator with
        | Some expression -> forward_expression ~resolution ~expression
        | None ->
            let resolved = forward_expression ~resolution ~expression:operand in
            { resolved with resolved = Type.bool; resolved_annotation = None; base = None } )
    | WalrusOperator { value; target } ->
        let statement =
          {
            Node.value = Statement.Assign { value; target; annotation = None; parent = None };
            location;
          }
        in
        let resolution, errors =
          let post_resolution, errors = forward_statement ~resolution ~statement in
          Option.value post_resolution ~default:resolution, errors
        in
        let resolved = forward_expression ~resolution ~expression:value in
        { resolved with errors = List.append errors resolved.errors }
    | Expression.Yield (Some expression) ->
        let { Resolved.resolution; resolved; errors; _ } =
          forward_expression ~resolution ~expression
        in
        {
          resolution;
          errors;
          resolved = Type.generator resolved;
          resolved_annotation = None;
          base = None;
        }
    | Expression.Yield None ->
        {
          resolution;
          errors = [];
          resolved = Type.generator Type.none;
          resolved_annotation = None;
          base = None;
        }


  and forward_statement ~resolution ~statement:{ Node.location; value } =
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
    let validate_return ~expression ~resolution ~errors ~actual ~is_implicit =
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
          ~resolve:(resolve_expression_type ~resolution)
          ~expression
          ~resolved:actual
          ~expected:return_annotation
      in
      let check_incompatible_return errors =
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
            ~errors
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
          errors
      in
      let check_missing_return errors =
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
            ~errors
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
          errors
      in
      check_incompatible_return errors |> check_missing_return
    in
    match value with
    | Assign { Assign.target; annotation; value; parent } ->
        let errors, is_final, original_annotation =
          match annotation with
          | None -> [], false, None
          | Some annotation ->
              let annotation_errors, parsed_annotation =
                parse_and_check_annotation ~resolution annotation
              in
              let unwrap ~f annotation = f annotation |> Option.value ~default:annotation in
              ( annotation_errors,
                Type.is_final parsed_annotation,
                unwrap parsed_annotation ~f:Type.final_value
                |> unwrap ~f:Type.class_variable_value
                |> Option.some )
        in
        let parsed =
          GlobalResolution.parse_annotation ~validation:NoValidation global_resolution value
        in
        let is_type_alias =
          match original_annotation with
          | None -> (
              (* Consider non-locals with a RHS that is a type to be an alias. *)
              let is_type_value = function
                | Type.Top ->
                    Option.is_some
                      (Type.Variable.parse_declaration value ~target:(Reference.create ""))
                | Type.Optional Type.Bottom -> false
                | annotation ->
                    not (GlobalResolution.contains_untracked global_resolution annotation)
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
              | _ -> is_type_value parsed )
          | Some annotation ->
              (Type.is_type_alias annotation && Define.is_toplevel define)
              || Type.is_meta annotation
                 && Type.is_typed_dictionary (Type.single_parameter annotation)
        in
        let resolution, errors, resolved =
          let { Resolved.resolution; errors = new_errors; resolved; _ } =
            forward_expression ~resolution ~expression:value
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
              |> fun (errors, _) ->
              let errors, _ =
                add_untracked_annotation_errors
                  ~resolution:global_resolution
                  ~location
                  ~errors
                  parsed
              in
              errors
            in
            let add_type_variable_errors errors =
              match parsed with
              | Variable variable when Type.Variable.Unary.contains_subvariable variable ->
                  emit_error
                    ~errors
                    ~location
                    ~kind:
                      (AnalysisError.InvalidType
                         (AnalysisError.NestedTypeVariables (Type.Variable.Unary variable)))
              | Variable { constraints = Explicit [explicit]; _ } ->
                  emit_error
                    ~errors
                    ~location
                    ~kind:(AnalysisError.InvalidType (AnalysisError.SingleExplicit explicit))
              | _ -> errors
            in
            let errors = add_annotation_errors errors |> add_type_variable_errors in
            resolution, errors, resolved
          else
            resolution, List.append new_errors errors, resolved
        in
        let guide =
          (* This is the annotation determining how we recursively break up the assignment. *)
          match original_annotation with
          | Some annotation when not (Type.contains_unknown annotation) -> annotation
          | _ -> resolved
        in
        let explicit = Option.is_some annotation in
        let rec forward_assign
            ~resolution
            ~errors
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
                match
                  GlobalResolution.extract_type_parameters
                    global_resolution
                    ~target:"typing.Iterable"
                    ~source:annotation
                with
                | Some [element_type] -> element_type
                | _ -> Type.Any )
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
              let reference, attribute, resolved_base, target_annotation =
                match name with
                | Name.Identifier identifier ->
                    let reference = Reference.create identifier in

                    ( Some reference,
                      None,
                      None,
                      from_reference ~location:Location.any reference
                      |> resolve_expression ~resolution )
                | Name.Attribute { base; attribute; _ } ->
                    let name = attribute in
                    let resolved = resolve_expression_type ~resolution base in
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
                    let target_annotation =
                      match attribute with
                      | Some (attribute, _) -> AnnotatedAttribute.annotation attribute
                      | _ ->
                          (* The reason why we need to do resolve_expression on the entire target
                             again is to deal with imported globals. To fix it, we ought to stop
                             representing imported globals as `Expression.Name.Attribute`. *)
                          resolve_expression ~resolution target
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
                    reference, attribute, Some resolved, target_annotation
              in
              let is_undefined_attribute parent =
                (* TODO(T64156088): This ought to be done in a much more principled way, by running
                   signature select against the particular type *)
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
                      | Type.Parametric
                          {
                            name = "BoundMethod";
                            parameters =
                              [
                                Single
                                  (Type.Callable
                                    {
                                      implementation =
                                        { parameters = Defined (_ :: _ :: value_parameter :: _); _ };
                                      _;
                                    });
                                _;
                              ];
                          }
                      | Type.Callable
                          {
                            implementation = { parameters = Defined (_ :: value_parameter :: _); _ };
                            _;
                          } ->
                          Type.Callable.Parameter.annotation value_parameter
                          |> Option.value_map ~default:false ~f:Type.is_any
                      | _ -> false )
                  | _ -> false
                in
                not is_setattr_any_defined
              in
              let errors =
                match reference with
                | Some reference ->
                    let check_final_reassignment errors =
                      let error () =
                        emit_error
                          ~errors
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
                        errors
                    in
                    let check_assign_class_variable_on_instance errors =
                      match
                        ( resolved_base,
                          attribute >>| fst >>| Annotated.Attribute.class_attribute,
                          attribute >>| fst >>| Annotated.Attribute.name )
                      with
                      | Some parent, Some true, Some class_variable
                        when Option.is_none original_annotation && not (Type.is_meta parent) ->
                          emit_error
                            ~errors
                            ~location
                            ~kind:
                              (Error.InvalidAssignment
                                 (ClassVariable { class_name = Type.show parent; class_variable }))
                      | _ -> errors
                    in
                    let check_final_is_outermost_qualifier errors =
                      original_annotation
                      >>| (fun annotation ->
                            if Type.contains_final annotation then
                              emit_error
                                ~errors
                                ~location
                                ~kind:(Error.InvalidType (FinalNested annotation))
                            else
                              errors)
                      |> Option.value ~default:errors
                    in
                    let check_is_readonly_property errors =
                      match
                        ( attribute >>| fst >>| Annotated.Attribute.visibility,
                          attribute >>| fst >>| Annotated.Attribute.property )
                      with
                      | Some (ReadOnly _), Some true when Option.is_none original_annotation ->
                          emit_error
                            ~errors
                            ~location
                            ~kind:(Error.InvalidAssignment (ReadOnly reference))
                      | _ -> errors
                    in
                    let check_undefined_attribute_target errors =
                      match resolved_base, attribute with
                      | Some parent, Some (attribute, name)
                        when not (Annotated.Attribute.defined attribute) ->
                          let is_meta_typed_dictionary =
                            Type.is_meta parent
                            && GlobalResolution.is_typed_dictionary
                                 ~resolution:global_resolution
                                 (Type.single_parameter parent)
                          in
                          if is_meta_typed_dictionary then
                            (* Ignore the error from the attribute declaration `Movie.name = ...`,
                               which would raise an error because `name` was removed as an attribute
                               from the TypedDictionary. *)
                            errors
                          else if is_undefined_attribute parent then
                            emit_error
                              ~errors
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
                            errors
                      | _ -> errors
                    in
                    let check_nested_explicit_type_alias errors =
                      match name, original_annotation with
                      | Name.Identifier identifier, Some annotation
                        when Type.is_type_alias annotation && not (Define.is_toplevel define) ->
                          emit_error
                            ~errors
                            ~location
                            ~kind:(Error.InvalidType (NestedAlias identifier))
                      | _ -> errors
                    in
                    check_final_reassignment errors
                    |> check_assign_class_variable_on_instance
                    |> check_final_is_outermost_qualifier
                    |> check_is_readonly_property
                    |> check_undefined_attribute_target
                    |> check_nested_explicit_type_alias
                | _ -> errors
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
                  ~resolve:(resolve_expression_type ~resolution)
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
                    contains_unknown resolved
                | _ -> false
              in
              let errors =
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
                    |> fun kind -> emit_error ~errors ~location ~kind
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
                    |> fun kind -> emit_error ~errors ~location ~kind
                | _ -> errors
              in
              (* Check for missing annotations. *)
              let check_annotation errors =
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
                        && (not (Type.contains_unknown expected))
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
                  explicit
                  (* [Movie.items: int] would raise an error because [Mapping] also has [items]. *)
                  && (not
                        (GlobalResolution.is_typed_dictionary
                           ~resolution:global_resolution
                           parent_annotation))
                  && not (Type.equal parent_annotation (Primitive attribute_parent))
                in
                let parent_class =
                  match name with
                  | Name.Attribute { base; _ } ->
                      resolve_expression_type ~resolution base |> Type.resolve_class
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
                      ( emit_error
                          ~errors
                          ~location:global_location
                          ~kind:
                            (Error.MissingGlobalAnnotation
                               {
                                 Error.name = reference;
                                 annotation = actual_annotation;
                                 given_annotation = Option.some_if is_immutable expected;
                                 evidence_locations;
                                 thrown_at_source;
                               }),
                        true )
                    else if explicit && insufficiently_annotated then
                      ( emit_error
                          ~errors
                          ~location
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
                               }),
                        true )
                    else if is_type_alias && Type.expression_contains_any value then
                      let value_annotation =
                        GlobalResolution.parse_annotation global_resolution value
                      in
                      let errors =
                        if Type.contains_prohibited_any value_annotation then
                          emit_error
                            ~errors
                            ~location
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
                        else
                          errors
                      in
                      errors, true
                    else
                      errors, true
                | Name.Attribute { base = { Node.value = Name base; _ }; attribute; _ }, None
                  when is_simple_name base && insufficiently_annotated ->
                    (* Module *)
                    let reference = name_to_reference_exn base in
                    if
                      explicit
                      && (not is_type_alias)
                      && not (GlobalResolution.module_exists global_resolution reference)
                    then
                      ( emit_error
                          ~errors
                          ~location
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
                               }),
                        true )
                    else
                      errors, true
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
                          ( emit_error ~errors ~location ~kind:(Error.IllegalAnnotationTarget target),
                            false )
                        else if Annotated.Attribute.defined attribute && insufficiently_annotated
                        then
                          ( emit_error
                              ~errors
                              ~location
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
                                   }),
                            true )
                        else if insufficiently_annotated && explicit && not is_type_alias then
                          ( emit_error
                              ~errors
                              ~location
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
                                   }),
                            true )
                        else
                          errors, true
                    | None ->
                        if
                          insufficiently_annotated
                          && GlobalResolution.is_typed_dictionary
                               ~resolution:global_resolution
                               (Type.Primitive class_name)
                        then
                          ( emit_error
                              ~errors
                              ~location
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
                                   }),
                            true )
                        else
                          errors, true )
                | _ ->
                    if explicit then
                      ( emit_error ~errors ~location ~kind:(Error.IllegalAnnotationTarget target),
                        false )
                    else
                      errors, true
              in
              let errors, is_valid_annotation = check_annotation errors in
              (* Propagate annotations. *)
              let is_global =
                match name with
                | Identifier identifier ->
                    Resolution.is_global resolution ~reference:(Reference.create identifier)
                | Attribute _ as name when is_simple_name name ->
                    Resolution.is_global resolution ~reference:(name_to_reference_exn name)
                | _ -> false
              in
              if is_global && not (Define.is_toplevel Context.define.value) then
                resolution, errors
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
                let errors, annotation =
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
                    emit_error ~errors ~location ~kind, { annotation with annotation = converted }
                  else
                    errors, annotation
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
                resolution, errors
          | List elements
          | Tuple elements
            when is_uniform_sequence guide ->
              let propagate (resolution, errors) element =
                match Node.value element with
                | Expression.Starred (Starred.Once target) ->
                    let guide = uniform_sequence_parameter guide |> Type.list in
                    let resolved = uniform_sequence_parameter resolved |> Type.list in
                    forward_assign ~resolution ~errors ~target ~guide ~resolved ~expression:None
                | _ ->
                    let guide = uniform_sequence_parameter guide in
                    let resolved = uniform_sequence_parameter resolved in
                    forward_assign
                      ~resolution
                      ~errors
                      ~target:element
                      ~guide
                      ~resolved
                      ~expression:None
              in
              List.fold elements ~init:(resolution, errors) ~f:propagate
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
              let errors, annotations =
                match guide with
                | Type.Any -> errors, List.map assignees ~f:(fun _ -> Type.Any)
                | Type.Top -> errors, List.map assignees ~f:(fun _ -> Type.Top)
                | _ -> (
                    match nonuniform_sequence_parameters guide with
                    | None ->
                        let errors =
                          emit_error
                            ~errors
                            ~location
                            ~kind:
                              (Error.Unpack
                                 {
                                   expected_count = List.length assignees;
                                   unpack_problem = UnacceptableType guide;
                                 })
                        in
                        errors, List.map assignees ~f:(fun _ -> Type.Top)
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
                          let errors =
                            emit_error
                              ~errors
                              ~location
                              ~kind:
                                (Error.Unpack
                                   {
                                     expected_count = List.length assignees;
                                     unpack_problem = CountMismatch (List.length annotations);
                                   })
                          in
                          errors, List.map assignees ~f:(fun _ -> Type.Top)
                        else
                          errors, annotations )
              in
              List.zip_exn assignees annotations
              |> List.fold
                   ~init:(resolution, errors)
                   ~f:(fun (resolution, errors) (target, guide) ->
                     forward_assign
                       ~resolution
                       ~errors
                       ~target
                       ~guide
                       ~resolved:guide
                       ~expression:None)
          | _ ->
              if Option.is_some annotation then
                ( resolution,
                  emit_error ~errors ~location ~kind:(Error.IllegalAnnotationTarget target) )
              else
                resolution, errors
        in
        let resolution, errors =
          forward_assign ~resolution ~errors ~target ~guide ~resolved ~expression:(Some value)
        in
        Some resolution, errors
    | Assert { Assert.test; _ } -> (
        let resolution, errors =
          forward_expression ~resolution ~expression:test
          |> fun { Resolved.resolution; errors; _ } -> resolution, errors
        in
        let parse_refinement_annotation annotation =
          let parse_meta annotation =
            match parse_and_check_annotation ~resolution annotation |> snd with
            | Type.Top -> (
                (* Try to resolve meta-types given as expressions. *)
                match resolve_expression_type ~resolution annotation with
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
            None, errors
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
            Some resolution, errors
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
            Some resolution, errors
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
            let contradiction =
              if Type.contains_unknown expected || Type.is_any expected then
                None
              else
                let { Resolved.resolved; _ } = forward_expression ~resolution ~expression:value in
                if
                  Type.is_unbound resolved
                  || Type.contains_unknown resolved
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
                    ( Node.location test,
                      Error.ImpossibleAssertion { test; expression = value; annotation = resolved }
                    )
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
            match contradiction, value with
            | Some (location, kind), _ -> None, emit_error ~errors ~location ~kind
            | _, { Node.value = Name name; _ } when is_simple_name name ->
                Some (resolve ~name), errors
            | _ -> Some resolution, errors )
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
            Some resolution, errors
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
            Some resolution, errors
        | Name name when is_simple_name name -> (
            match refinable_annotation name with
            | Some { Annotation.annotation = Type.Optional Type.Bottom; _ } ->
                ( None,
                  emit_error
                    ~errors
                    ~location:(Node.location test)
                    ~kind:
                      (Error.ImpossibleAssertion
                         { test; expression = test; annotation = Type.Optional Type.Bottom }) )
            | Some ({ Annotation.annotation = Type.Optional parameter; _ } as annotation) ->
                let resolution =
                  Resolution.set_local_with_attributes
                    resolution
                    ~name
                    ~annotation:{ annotation with Annotation.annotation = parameter }
                in
                Some resolution, errors
            | _ -> Some resolution, errors )
        | BooleanOperator { BooleanOperator.left; operator; right } -> (
            let update resolution expression =
              forward_statement ~resolution ~statement:(Statement.assume expression)
              |> fun (post_resolution, _) ->
              let resolution = Option.value post_resolution ~default:resolution in
              Resolution.annotation_store resolution
            in
            match operator with
            | BooleanOperator.And ->
                let left_annotation_store = update resolution left in
                let resolution =
                  forward_statement ~resolution ~statement:(Statement.assume left)
                  |> fun (post_resolution, _) -> Option.value post_resolution ~default:resolution
                in
                let right_annotation_store = update resolution right in
                let merge ~key:_ = function
                  | `Both (left, right) -> Some (RefinementUnit.meet ~global_resolution left right)
                  | `Left left -> Some left
                  | `Right right -> Some right
                in
                let annotation_store =
                  Map.merge ~f:merge left_annotation_store right_annotation_store
                in
                let resolution = Resolution.with_annotation_store resolution ~annotation_store in
                Some resolution, errors
            | BooleanOperator.Or ->
                let negated_left = update resolution (normalize (negate left)) in
                let left_annotation_store = update resolution left in
                let resolution =
                  Resolution.with_annotation_store resolution ~annotation_store:negated_left
                in
                let right_annotation_store = update resolution right in
                ( Some
                    (join_resolutions
                       (Resolution.with_annotation_store
                          resolution
                          ~annotation_store:left_annotation_store)
                       (Resolution.with_annotation_store
                          resolution
                          ~annotation_store:right_annotation_store)),
                  errors ) )
        | ComparisonOperator
            {
              ComparisonOperator.left;
              operator = ComparisonOperator.IsNot;
              right = { Node.value = Name (Name.Identifier "None"); _ };
            } ->
            forward_statement ~resolution ~statement:(Statement.assume left)
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
                  ( Some (Resolution.set_local_with_attributes resolution ~name ~annotation:refined),
                    errors )
                else
                  (* Keeping previous state, since it is more refined. *)
                  (* TODO: once T38750424 is done, we should really return bottom if previous is not
                     <= refined and refined is not <= previous, as this is an obvious contradiction. *)
                  Some resolution, errors
            | None -> Some resolution, errors )
        | ComparisonOperator
            {
              ComparisonOperator.left = { Node.value = Name name; _ };
              operator = ComparisonOperator.In;
              right;
            }
          when is_simple_name name -> (
            let reference = name_to_reference_exn name in
            let { Resolved.resolved; _ } = forward_expression ~resolution ~expression:right in
            match
              GlobalResolution.extract_type_parameters
                global_resolution
                ~target:"typing.Iterable"
                ~source:resolved
            with
            | Some [element_type] -> (
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
                      ( Some
                          (Resolution.set_local_with_attributes
                             resolution
                             ~name
                             ~annotation:refined),
                        errors )
                    else (* Keeping previous state, since it is more refined. *)
                      Some resolution, errors
                | None when not (Resolution.is_global resolution ~reference) ->
                    let resolution =
                      Resolution.set_local_with_attributes
                        resolution
                        ~name
                        ~annotation:(Annotation.create element_type)
                    in
                    Some resolution, errors
                | _ -> Some resolution, errors )
            | _ -> Some resolution, errors )
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
                | Type.Parametric { name = "list"; parameters = [Single (Type.Optional parameter)] }
                  ->
                    let resolution =
                      Resolution.set_local_with_attributes
                        resolution
                        ~name
                        ~annotation:{ annotation with Annotation.annotation = Type.list parameter }
                    in
                    Some resolution, errors
                | _ -> Some resolution, errors )
            | _ -> Some resolution, errors )
        | WalrusOperator { target; _ } ->
            let resolution, _ =
              forward_statement ~resolution ~statement:(Statement.assume target)
            in
            resolution, errors
        | _ -> Some resolution, errors )
    | Delete expression ->
        let { Resolved.resolution; errors; _ } = forward_expression ~resolution ~expression in
        let resolution =
          match Node.value expression with
          | Name (Identifier identifier) ->
              Resolution.unset_local resolution ~reference:(Reference.create identifier)
          | _ -> resolution
        in
        Some resolution, errors
    | Expression
        { Node.value = Call { callee; arguments = { Call.Argument.value = test; _ } :: _ }; _ }
      when Core.Set.mem Recognized.assert_functions (Expression.show callee) ->
        forward_statement ~resolution ~statement:(Statement.assume test)
    | Expression expression ->
        forward_expression ~resolution ~expression
        |> fun { Resolved.resolution; resolved; errors; _ } ->
        if Type.is_noreturn resolved then
          None, errors
        else
          Some resolution, errors
    | Raise { Raise.expression = Some expression; _ } ->
        let { Resolved.resolution; resolved; errors; _ } =
          forward_expression ~resolution ~expression
        in
        let expected = Type.Primitive "BaseException" in
        let actual =
          if Type.is_meta resolved then
            Type.single_parameter resolved
          else
            resolved
        in
        let errors =
          if GlobalResolution.less_or_equal global_resolution ~left:actual ~right:expected then
            errors
          else
            emit_error
              ~errors
              ~location
              ~kind:(Error.InvalidException { expression; annotation = resolved })
        in
        Some resolution, errors
    | Raise _ -> Some resolution, []
    | Return { Return.expression; is_implicit } ->
        let { Resolved.resolution; resolved = actual; errors; _ } =
          Option.value_map
            expression
            ~default:
              {
                Resolved.resolution;
                errors = [];
                resolved = Type.none;
                resolved_annotation = None;
                base = None;
              }
            ~f:(fun expression -> forward_expression ~resolution ~expression)
        in
        Some resolution, validate_return ~expression ~resolution ~errors ~actual ~is_implicit
    | Statement.Yield { Node.value = Expression.Yield return; _ } ->
        let { Resolved.resolution; resolved = actual; errors; _ } =
          match return with
          | Some expression ->
              let { Resolved.resolution; resolved; errors; _ } =
                forward_expression ~resolution ~expression
              in
              {
                resolution;
                errors;
                resolved = Type.generator ~async resolved;
                resolved_annotation = None;
                base = None;
              }
          | None ->
              {
                resolution;
                errors = [];
                resolved = Type.generator ~async Type.none;
                resolved_annotation = None;
                base = None;
              }
        in
        ( Some resolution,
          validate_return ~expression:None ~resolution ~errors ~actual ~is_implicit:false )
    | Statement.Yield _ -> Some resolution, []
    | YieldFrom { Node.value = Expression.Yield (Some return); _ } ->
        let { Resolved.resolution; resolved; errors; _ } =
          forward_expression ~resolution ~expression:return
        in
        let actual =
          match
            GlobalResolution.extract_type_parameters
              global_resolution
              ~target:"typing.Iterator"
              ~source:resolved
          with
          | Some [parameter] -> Type.generator parameter
          | _ -> Type.generator Type.Any
        in
        ( Some resolution,
          validate_return ~expression:None ~resolution ~errors ~actual ~is_implicit:false )
    | YieldFrom _ -> Some resolution, []
    | Define { signature = { Define.Signature.name = { Node.value = name; _ }; _ } as signature; _ }
      ->
        let resolution =
          if Reference.is_local name then
            type_of_signature ~resolution signature
            |> Type.Variable.mark_all_variables_as_bound
                 ~specific:(Resolution.all_type_variables_in_scope resolution)
            |> Annotation.create
            |> fun annotation -> Resolution.set_local resolution ~reference:name ~annotation
          else
            resolution
        in
        Some resolution, []
    | Import { Import.from; imports } ->
        let check_import import =
          let rec check_lead lead = function
            | [] -> None
            | name :: rest -> (
                let lead = lead @ [name] in
                let reference = Reference.create_from_list lead in
                match GlobalResolution.module_exists global_resolution reference with
                | true -> check_lead lead rest
                | false -> (
                    match resolve_reference_type ~resolution reference with
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
        ( Some resolution,
          List.fold undefined_imports ~init:[] ~f:(fun errors reference ->
              emit_error ~errors ~location ~kind:(Error.UndefinedImport reference)) )
    | Class { Class.bases; _ } when bases <> [] ->
        (* Check that variance isn't widened on inheritence *)
        let check_base errors { Call.Argument.value = base; _ } =
          let check_pair errors extended actual =
            match extended, actual with
            | ( Type.Variable { Type.Record.Variable.RecordUnary.variance = left; _ },
                Type.Variable { Type.Record.Variable.RecordUnary.variance = right; _ } ) -> (
                match left, right with
                | Type.Variable.Covariant, Type.Variable.Invariant
                | Type.Variable.Contravariant, Type.Variable.Invariant
                | Type.Variable.Covariant, Type.Variable.Contravariant
                | Type.Variable.Contravariant, Type.Variable.Covariant ->
                    emit_error
                      ~errors
                      ~location
                      ~kind:
                        (Error.InvalidTypeVariance
                           { annotation = extended; origin = Error.Inheritance actual })
                | _ -> errors )
            | _, _ -> errors
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
                      List.fold2 extended_parameters actual_parameters ~init:errors ~f:check_pair
                    with
                    | Ok errors -> errors
                    | Unequal_lengths -> errors)
              |> Option.value ~default:errors
          | _ -> errors
        in
        Some resolution, List.fold bases ~f:check_base ~init:[]
    | Class _ ->
        (* Don't check accesses in nested classes and functions, they're analyzed separately. *)
        Some resolution, []
    | For _
    | If _
    | Try _
    | With _
    | While _ ->
        (* Check happens implicitly in the resulting control flow. *)
        Some resolution, []
    | Break
    | Continue
    | Global _
    | Nonlocal _
    | Pass ->
        Some resolution, []


  and resolve_expression ~resolution expression =
    forward_expression ~resolution ~expression
    |> fun { Resolved.resolved; resolved_annotation; _ } ->
    resolved_annotation |> Option.value ~default:(Annotation.create resolved)


  and resolve_expression_type ~resolution expression =
    resolve_expression ~resolution expression |> Annotation.annotation


  and resolve_reference_type ~resolution reference =
    from_reference ~location:Location.any reference |> resolve_expression_type ~resolution


  let forward ~key ({ resolution; _ } as state) ~statement =
    match resolution with
    | None -> state
    | Some resolution ->
        let post_resolution, errors = forward_statement ~resolution ~statement in
        let () =
          LocalErrorMap.set state.error_map ~key ~errors;
          match post_resolution with
          | None -> ()
          | Some post_resolution ->
              let precondition = Resolution.annotation_store resolution in
              let postcondition = Resolution.annotation_store post_resolution in
              LocalAnnotationMap.set state.resolution_fixpoint ~key ~precondition ~postcondition
        in
        { state with resolution = post_resolution }


  let backward ~key:_ state ~statement:_ = state
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
  let resolve_expression ~resolution expression =
    State.forward_expression ~resolution ~expression
    |> fun { State.Resolved.resolved; resolved_annotation; resolution = new_resolution; _ } ->
    new_resolution, resolved_annotation |> Option.value ~default:(Annotation.create resolved)
  in
  let resolve_statement ~resolution statement =
    State.forward_statement ~resolution ~statement
    |> fun (resolution, errors) ->
    match resolution with
    | None -> Resolution.Unreachable
    | Some resolution -> Resolution.Reachable { resolution; errors }
  in
  Resolution.create ~global_resolution ~annotation_store ~resolve_expression ~resolve_statement ()


let resolution_with_key ~global_resolution ~local_annotations ~parent ~key =
  let annotation_store =
    Option.value_map
      local_annotations
      ~f:(fun map ->
        LocalAnnotationMap.ReadOnly.get_precondition map key
        |> Option.value ~default:Reference.Map.empty)
      ~default:Reference.Map.empty
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
      if ClassSummary.is_protocol definition then
        let private_protocol_property_errors =
          GlobalResolution.attributes
            ~transitive:false
            ~include_generated_attributes:true
            ~resolution:global_resolution
            (Reference.show (ClassSummary.name definition))
          >>| List.map ~f:AnnotatedAttribute.name
          >>| List.filter ~f:is_private_attribute
          >>| List.map ~f:(fun name ->
                  Error.create
                    ~location:(Location.with_module ~qualifier:Context.qualifier location)
                    ~kind:
                      (Error.PrivateProtocolProperty
                         {
                           name;
                           parent = Type.Primitive (ClassSummary.name definition |> Reference.show);
                         })
                    ~define:Context.define)
          |> Option.value ~default:[]
        in
        private_protocol_property_errors @ errors
      else
        errors
    in
    (* Ensure all attributes are instantiated. *)
    let check_attribute_initialization definition errors =
      if
        (not (ClassSummary.is_protocol definition))
        && (not (ClassSummary.is_abstract definition))
        && not
             (GlobalResolution.is_typed_dictionary
                ~resolution:global_resolution
                (Type.Primitive (Reference.show (ClassSummary.name definition))))
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
                | NotInitialized -> true
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
            if ClassSummary.is_abstract definition then
              []
            else
              let abstract_superclasses, concrete_superclasses, _superclasses_missing_metadata =
                let { ClassSummary.name; _ } = definition in
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
                let { ClassSummary.name; _ } = definition in
                {
                  class_name = Reference.show name;
                  is_abstract = ClassSummary.is_abstract definition;
                  is_protocol = ClassSummary.is_protocol definition;
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
                               parent =
                                 Type.Primitive (ClassSummary.name definition |> Reference.show);
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
    if Define.is_class_toplevel define then
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
        >>| Node.value
        >>| ClassSummary.bases
        >>| List.fold ~init:errors ~f:is_final
        |> Option.value ~default:errors
      in
      let check_protocol definition errors = check_protocol_properties definition errors in
      let check_overrides
          ({ ClassSummary.attribute_components; name = class_name; _ } as definition)
          errors
        =
        let components =
          Ast.Statement.Class.attributes ~include_generated_attributes:true attribute_components
        in

        let override_errors_for_typed_dictionary class_name =
          let open Type.Record.TypedDictionary in
          let get_typed_dictionary_fields class_name =
            GlobalResolution.get_typed_dictionary
              ~resolution:global_resolution
              (Type.Primitive class_name)
            >>| (fun typed_dictionary -> typed_dictionary.fields)
            |> Option.value ~default:[]
          in
          let field_name_to_successor_fields_map =
            let get_successor_map_entries successor_name =
              get_typed_dictionary_fields successor_name
              |> List.map ~f:(fun (field : Type.t typed_dictionary_field) ->
                     field.name, (successor_name, field))
            in
            GlobalResolution.successors ~resolution:global_resolution class_name
            |> List.concat_map ~f:get_successor_map_entries
            |> Map.of_alist_multi (module String)
          in
          let colliding_successor_fields (field : Type.t typed_dictionary_field) =
            let matching_successors =
              Map.find_multi field_name_to_successor_fields_map field.name
            in
            let is_inherited_field =
              List.exists matching_successors ~f:(fun (_, successor_field) ->
                  [%equal: Type.t typed_dictionary_field] field successor_field)
            in
            if is_inherited_field then
              []
            else
              List.map matching_successors ~f:(fun (successor_name, successor_field) ->
                  field, (successor_name, successor_field))
          in
          let wrongly_overriding_fields =
            get_typed_dictionary_fields class_name |> List.concat_map ~f:colliding_successor_fields
          in
          let create_override_error
              ((field : Type.t typed_dictionary_field), (successor_name, successor_field))
            =
            let kind =
              Error.InconsistentOverride
                {
                  overridden_method = field.name;
                  parent = Reference.create successor_name;
                  override_kind = Attribute;
                  override =
                    Error.WeakenedPostcondition
                      {
                        actual = field.annotation;
                        expected = successor_field.annotation;
                        due_to_invariance = false;
                      };
                }
            in
            let location =
              Identifier.SerializableMap.find_opt class_name components
              >>| Node.location
              |> Option.value ~default:location
            in
            Error.create
              ~location:(Location.with_module ~qualifier:Context.qualifier location)
              ~kind
              ~define:Context.define
          in
          List.map wrongly_overriding_fields ~f:create_override_error
        in
        let override_errors =
          let open Annotated in
          let class_name = Reference.show class_name in
          if
            GlobalResolution.is_typed_dictionary
              ~resolution:global_resolution
              (Type.Primitive class_name)
          then
            override_errors_for_typed_dictionary class_name
          else
            GlobalResolution.attributes
              ~include_generated_attributes:false
              ~resolution:global_resolution
              (Reference.show (ClassSummary.name definition))
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
                        ( GlobalResolution.less_or_equal
                            global_resolution
                            ~left:actual
                            ~right:expected
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
                    GlobalResolution.overrides ~resolution:global_resolution ~name class_name
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
        let class_name = ClassSummary.name definition in
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
      >>| Node.value
      >>| (fun definition ->
            errors
            |> check_bases
            |> check_protocol definition
            |> check_attribute_initialization definition
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
    let resolution =
      let { State.resolution; _ } = initial in
      Option.value_exn resolution
    in
    let errors_sofar = State.all_errors initial |> Error.deduplicate in
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
      | Some ({ State.resolution_fixpoint; resolution = post_resolution; _ } as state) ->
          let errors_sofar = State.all_errors state |> Error.deduplicate in
          let resolution = Option.value post_resolution ~default:resolution in
          ( emit_errors_on_exit (module Context) ~errors_sofar ~resolution ()
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
          local_annotations >>| LocalAnnotationMap.read_only )


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
            match local_annotations with
            | Some local_annotations -> local_annotations
            | None -> LocalAnnotationMap.empty ()
          in
          TypeEnvironment.set_local_annotations
            environment
            name
            (LocalAnnotationMap.read_only local_annotations)
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
