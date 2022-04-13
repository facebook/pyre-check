(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open Ast
open Analysis
open Expression

module DefinitionsCache (Type : sig
  type t
end) =
struct
  let cache : Type.t Reference.Table.t = Reference.Table.create ()

  let set key value = Hashtbl.set cache ~key ~data:value

  let get = Hashtbl.find cache

  let invalidate () = Hashtbl.clear cache
end

module ClassDefinitionsCache = DefinitionsCache (struct
  type t = Statement.Class.t Node.t list option
end)

let containing_source ~resolution reference =
  let global_resolution = Resolution.global_resolution resolution in
  let ast_environment = GlobalResolution.ast_environment global_resolution in
  let rec qualifier ~lead ~tail =
    match tail with
    | head :: (_ :: _ as tail) ->
        let new_lead = Reference.create ~prefix:lead head in
        if not (GlobalResolution.module_exists global_resolution new_lead) then
          lead
        else
          qualifier ~lead:new_lead ~tail
    | _ -> lead
  in
  qualifier ~lead:Reference.empty ~tail:(Reference.as_list reference)
  |> AstEnvironment.ReadOnly.get_processed_source ast_environment


let class_summaries ~resolution reference =
  match ClassDefinitionsCache.get reference with
  | Some result -> result
  | None ->
      let open Option in
      let result =
        containing_source ~resolution reference
        >>| Preprocessing.classes
        >>| List.filter ~f:(fun { Node.value = { Statement.Class.name; _ }; _ } ->
                Reference.equal reference name)
        (* Prefer earlier definitions. *)
        >>| List.rev
      in
      ClassDefinitionsCache.set reference result;
      result


(* Find a method definition matching the given predicate. *)
let find_method_definitions ~resolution ?(predicate = fun _ -> true) name =
  let open Statement in
  let get_matching_define = function
    | {
        Node.value =
          Statement.Define ({ signature = { name = define_name; _ } as signature; _ } as define);
        _;
      } ->
        if Reference.equal define_name name && predicate define then
          let global_resolution = Resolution.global_resolution resolution in
          let parser = GlobalResolution.annotation_parser global_resolution in
          let variables = GlobalResolution.variables global_resolution in
          Annotated.Define.Callable.create_overload_without_applying_decorators
            ~parser
            ~variables
            signature
          |> Option.some
        else
          None
    | _ -> None
  in
  Reference.prefix name
  >>= class_summaries ~resolution
  >>= List.hd
  >>| (fun definition -> definition.Node.value.Class.body)
  >>| List.filter_map ~f:get_matching_define
  |> Option.value ~default:[]


module Global = struct
  type t =
    | Class
    | Module
    | Attribute of Type.t
  [@@deriving show]
end

(* Resolve global symbols, ignoring decorators. *)
let resolve_global ~resolution name =
  let global_resolution = Resolution.global_resolution resolution in
  (* Resolve undecorated functions. *)
  match GlobalResolution.global global_resolution name with
  | Some { AttributeResolution.Global.undecorated_signature = Some signature; _ } ->
      Some (Global.Attribute (Type.Callable signature))
  | _ -> (
      (* Resolve undecorated methods. *)
      match find_method_definitions ~resolution name with
      | [callable] -> Some (Global.Attribute (Type.Callable.create_from_implementation callable))
      | first :: _ :: _ as overloads ->
          (* Note that we use the first overload as the base implementation, which might be unsound. *)
          Some
            (Global.Attribute
               (Type.Callable.create
                  ~overloads
                  ~parameters:first.parameters
                  ~annotation:first.annotation
                  ()))
      | [] -> (
          (* Fall back for anything else. *)
          let annotation =
            from_reference name ~location:Location.any
            |> Resolution.resolve_expression_to_annotation resolution
          in
          match Annotation.annotation annotation with
          | Type.Parametric { name = "type"; _ }
            when GlobalResolution.class_exists global_resolution (Reference.show name) ->
              Some Global.Class
          | Type.Top when GlobalResolution.module_exists global_resolution name ->
              Some Global.Module
          | Type.Top when not (Annotation.is_immutable annotation) ->
              (* FIXME: We are relying on the fact that nonexistent functions & attributes resolve
                 to mutable annotation, while existing ones resolve to immutable annotation. This is
                 fragile! *)
              None
          | annotation -> Some (Global.Attribute annotation)))


type parameter_requirements = {
  anonymous_parameters_positions: Int.Set.t;
  parameter_set: String.Set.t;
  has_star_parameter: bool;
  has_star_star_parameter: bool;
}

let create_parameters_requirements ~type_parameters =
  let get_parameters_requirements requirements type_parameter =
    let open Type.Callable.RecordParameter in
    match type_parameter with
    | PositionalOnly { index; _ } ->
        {
          requirements with
          anonymous_parameters_positions =
            Int.Set.add requirements.anonymous_parameters_positions index;
        }
    | Named { name; _ }
    | KeywordOnly { name; _ } ->
        let name = Identifier.sanitized name in
        { requirements with parameter_set = String.Set.add requirements.parameter_set name }
    | Variable _ -> { requirements with has_star_parameter = true }
    | Keywords _ -> { requirements with has_star_star_parameter = true }
  in
  let init =
    {
      anonymous_parameters_positions = Int.Set.empty;
      parameter_set = String.Set.empty;
      has_star_parameter = false;
      has_star_star_parameter = false;
    }
  in
  List.fold_left type_parameters ~f:get_parameters_requirements ~init


let demangle_class_attribute name =
  if String.is_substring ~substring:"__class__" name then
    String.split name ~on:'.'
    |> List.rev
    |> function
    | attribute :: "__class__" :: rest -> List.rev (attribute :: rest) |> String.concat ~sep:"."
    | _ -> name
  else
    name


let model_verification_error ~path ~location kind = { ModelVerificationError.kind; path; location }

let verify_model_syntax ~path ~location ~callable_name ~normalized_model_parameters =
  (* Ensure that the parameter's default value is either not present or `...` to catch common errors
     when declaring models. *)
  let check_default_value (_, _, original) =
    match Node.value original with
    | { Parameter.value = None; _ }
    | { Parameter.value = Some { Node.value = Expression.Constant Constant.Ellipsis; _ }; _ } ->
        None
    | { Parameter.value = Some expression; name; _ } ->
        Some
          (model_verification_error
             ~path
             ~location
             (InvalidDefaultValue { callable_name = Reference.show callable_name; name; expression }))
  in
  List.find_map normalized_model_parameters ~f:check_default_value
  |> function
  | Some error -> Error error
  | None -> Ok ()


let verify_imported_model ~path ~location ~callable_name ~callable_annotation =
  match callable_annotation with
  | Some { Type.Callable.kind = Type.Callable.Named actual_name; _ }
    when not (Reference.equal callable_name actual_name) ->
      Error
        (model_verification_error
           ~path
           ~location
           (ImportedFunctionModel { name = callable_name; actual_name }))
  | _ -> Ok ()


let model_compatible_errors ~callable_overload ~normalized_model_parameters =
  let open ModelVerificationError in
  (* Once a requirement has been satisfied, it is removed from requirement object. At the end, we
     check whether there remains unsatisfied requirements. *)
  let validate_model_parameter position (errors, requirements) (model_parameter, _, _) =
    match model_parameter with
    | AccessPath.Root.LocalResult
    | AccessPath.Root.Variable _ ->
        failwith
          ("LocalResult|Variable won't be generated by AccessPath.Root.normalize_parameters, "
          ^ "and they cannot be compared with type_parameters.")
    | AccessPath.Root.PositionalParameter { name; positional_only = true; _ } ->
        if Int.Set.mem requirements.anonymous_parameters_positions position then
          errors, requirements
        else
          ( IncompatibleModelError.UnexpectedPositionalOnlyParameter
              {
                name;
                position;
                valid_positions = Int.Set.elements requirements.anonymous_parameters_positions;
              }
            :: errors,
            requirements )
    | AccessPath.Root.PositionalParameter { name; positional_only = false; _ }
    | AccessPath.Root.NamedParameter { name } ->
        let name = Identifier.sanitized name in
        if String.is_prefix name ~prefix:"__" then (* It is an positional only parameter. *)
          if Int.Set.mem requirements.anonymous_parameters_positions position then
            errors, requirements
          else if requirements.has_star_parameter then
            (* If all positional only parameter quota is used, it might be covered by a `*args` *)
            errors, requirements
          else
            ( IncompatibleModelError.UnexpectedPositionalOnlyParameter
                {
                  name;
                  position;
                  valid_positions = Int.Set.elements requirements.anonymous_parameters_positions;
                }
              :: errors,
              requirements )
        else
          let { parameter_set; has_star_parameter; has_star_star_parameter; _ } = requirements in
          (* Consume an required or optional named parameter. *)
          if String.Set.mem parameter_set name then
            let parameter_set = String.Set.remove parameter_set name in
            errors, { requirements with parameter_set }
          else if has_star_star_parameter then
            (* If the name is not found in the set, it is covered by `**kwargs` *)
            errors, requirements
          else if has_star_parameter then (* positional parameters can be covered by `*args` *)
            match model_parameter with
            | PositionalParameter _ -> errors, requirements
            | _ -> UnexpectedNamedParameter name :: errors, requirements
          else
            IncompatibleModelError.UnexpectedNamedParameter name :: errors, requirements
    | AccessPath.Root.StarParameter _ ->
        if requirements.has_star_parameter then
          errors, requirements
        else
          IncompatibleModelError.UnexpectedStarredParameter :: errors, requirements
    | AccessPath.Root.StarStarParameter _ ->
        if requirements.has_star_star_parameter then
          errors, requirements
        else
          IncompatibleModelError.UnexpectedDoubleStarredParameter :: errors, requirements
  in
  match callable_overload with
  | { Type.Callable.parameters = Type.Callable.Defined type_parameters; _ } ->
      let parameter_requirements = create_parameters_requirements ~type_parameters in
      let errors, _ =
        List.foldi
          normalized_model_parameters
          ~f:validate_model_parameter
          ~init:([], parameter_requirements)
      in
      List.map
        ~f:(fun reason -> { IncompatibleModelError.reason; overload = Some callable_overload })
        errors
  | _ -> []


let verify_signature
    ~path
    ~location
    ~normalized_model_parameters
    ~name:callable_name
    callable_annotation
  =
  let open Result in
  verify_model_syntax ~path ~location ~callable_name ~normalized_model_parameters
  >>= fun () ->
  verify_imported_model ~path ~location ~callable_name ~callable_annotation
  >>= fun () ->
  match callable_annotation with
  | Some ({ Type.Callable.implementation; overloads; _ } as callable) ->
      let errors =
        model_compatible_errors ~callable_overload:implementation ~normalized_model_parameters
      in
      let errors =
        if (not (List.is_empty errors)) && not (List.is_empty overloads) then
          (* We might be refering to a parameter defined in an overload. *)
          let errors_in_overloads =
            List.map overloads ~f:(fun callable_overload ->
                model_compatible_errors ~callable_overload ~normalized_model_parameters)
          in
          if List.find ~f:List.is_empty errors_in_overloads |> Option.is_some then
            []
          else
            errors @ List.concat errors_in_overloads
        else
          List.map ~f:ModelVerificationError.IncompatibleModelError.strip_overload errors
      in
      if not (List.is_empty errors) then
        Error
          (model_verification_error
             ~path
             ~location
             (IncompatibleModelError
                { name = Reference.show callable_name; callable_type = callable; errors }))
      else
        Ok ()
  | _ -> Ok ()


let verify_global ~path ~location ~resolution ~name =
  let name = demangle_class_attribute (Reference.show name) |> Reference.create in
  let global = resolve_global ~resolution name in
  match global with
  | Some Global.Class ->
      Error
        (model_verification_error ~path ~location (ModelingClassAsAttribute (Reference.show name)))
  | Some Global.Module ->
      Error
        (model_verification_error ~path ~location (ModelingModuleAsAttribute (Reference.show name)))
  | Some (Global.Attribute (Type.Callable _))
  | Some
      (Global.Attribute
        (Type.Parametric
          { name = "BoundMethod"; parameters = [Type.Parameter.Single (Type.Callable _); _] })) ->
      Error
        (model_verification_error
           ~path
           ~location
           (ModelingCallableAsAttribute (Reference.show name)))
  | Some (Global.Attribute _)
  | None -> (
      let global_resolution = Resolution.global_resolution resolution in
      let class_summary =
        Reference.prefix name
        >>| Reference.show
        >>| (fun class_name -> Type.Primitive class_name)
        >>= GlobalResolution.class_summary global_resolution
        >>| Node.value
      in
      match class_summary, global with
      | Some ({ name = class_name; _ } as class_summary), _ ->
          let attributes =
            ClassSummary.attributes ~include_generated_attributes:false class_summary
          in
          let constructor_attributes = ClassSummary.constructor_attributes class_summary in
          let attribute_name = Reference.last name in
          if
            Identifier.SerializableMap.mem attribute_name attributes
            || Identifier.SerializableMap.mem attribute_name constructor_attributes
          then
            Ok ()
          else
            Error
              (model_verification_error
                 ~path
                 ~location
                 (MissingAttribute
                    { class_name = Reference.show class_name; attribute_name = Reference.last name }))
      | None, Some _ -> Ok ()
      | None, None -> (
          let module_name = Reference.first name in
          let module_resolved = resolve_global ~resolution (Reference.create module_name) in
          match module_resolved with
          | Some _ ->
              Error
                (model_verification_error
                   ~path
                   ~location
                   (MissingSymbol { module_name; symbol_name = Reference.show name }))
          | None ->
              Error
                (model_verification_error
                   ~path
                   ~location
                   (NotInEnvironment { module_name; name = Reference.show name }))))
