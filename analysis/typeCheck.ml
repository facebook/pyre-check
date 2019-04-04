(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre

open Ast
open Expression
open Statement

module Error = AnalysisError


module AccessState = struct
  (* Keep track of objects whose type might be determined later on or that might serve as implicit
     argument to a call. *)
  type target = {
    reference: Reference.t;
    annotation: Type.t;
  }

  type found_origin =
    | Instance of Annotated.Attribute.t
    | Module of Reference.t
  [@@deriving show]

  type undefined_origin =
    | Instance of { attribute: Annotated.Attribute.t; instantiated_target: Type.t }
    | Module of Reference.t
    | TypeWithoutClass of Type.t
  [@@deriving show]

  type definition =
    | Defined of found_origin
    | Undefined of undefined_origin
  [@@deriving show]

  type element =
    | Signature of {
        signature: AnnotatedSignature.t;
        callees: Type.Callable.t list;
        arguments: Argument.t list;
        accesses_incomplete_type: (Access.general_access * Type.t) option;
      }
    | Attribute of {
        attribute: Identifier.t;
        definition: definition;
        accesses_incomplete_type: (Access.general_access * Type.t) option;
      }
    | NotCallable of Type.t
    | Value
  [@@deriving show]

  type 'accumulator t = {
    resolution: Resolution.t;
    accumulator: 'accumulator;
    f: 'accumulator
      -> resolution: Resolution.t
      -> resolved: Annotation.t
      -> element: element
      -> lead: Access.t
      -> 'accumulator;
    resolved: Annotation.t option;
    target: target option;
    continue: bool;
  }

  type attribute_access_data = {
    instantiated: Type.t;
    class_attributes: bool;
    class_definition: Annotated.Class.t;
  }

  exception TypeWithoutClass of Type.t


  let create ~resolution ~accumulator ~f ?resolved ?target ?(continue = true) () =
    { resolution; accumulator; f; resolved; target; continue }


  let annotation { annotation; _ } =
    annotation


  let redirect ~resolution ~access =
    (* Resolve special-cased calls. *)
    match access with
    (* Resolve `super()` calls. *)
    | (Access.Identifier "super") :: (Access.Call _) :: tail ->
        (Resolution.parent resolution
         >>| (fun parent -> Resolution.parse_reference resolution parent)
         >>= Resolution.class_metadata resolution
         >>| (fun { Resolution.successors; _ } -> successors)
         >>|  List.filter
           ~f:(fun name -> Option.is_some (Resolution.class_definition resolution name))
         >>| List.hd
         >>| function
         | Some superclass ->
             let resolution =
               Resolution.set_local
                 resolution
                 ~reference:(Reference.create "$super")
                 ~annotation:(Annotation.create superclass)
             in
             Access.SimpleAccess (Access.Identifier "$super" :: tail), resolution
         | None ->
             Access.SimpleAccess access, resolution)
        |> Option.value ~default:(Access.SimpleAccess access, resolution)
    (* Resolve `type()` calls. *)
    | (Access.Identifier "type")
      :: (Access.Call { Node.value = [{ Argument.value; _ }]; _ })
      :: tail ->
        let resolution =
          let annotation =
            Resolution.resolve resolution value
            |> Type.meta
            |> Annotation.create
          in
          Resolution.set_local resolution ~reference:(Reference.create "$type") ~annotation
        in
        SimpleAccess (Access.Identifier "$type" :: tail), resolution
    (* Resolve function redirects. *)
    | (Access.Identifier name) :: (Access.Call { Node.value = arguments; location }) :: tail ->
        Access.redirect ~arguments ~location ~name:[Access.Identifier name]
        >>| (function
            | Access.SimpleAccess redirect ->
                Access.SimpleAccess (redirect @ tail)
            | Access.ExpressionAccess { expression; access } ->
                Access.ExpressionAccess { expression; access = access @ tail })
        |> Option.value ~default:(Access.SimpleAccess access),
        resolution

    | _ ->
        Access.SimpleAccess access, resolution


  let resolve_exports ~resolution ~access =
    (* This is necessary due to export/module name conflicts: P59503092 *)
    let exported =
      let widening_threshold = 25 in
      let rec resolve_exports_fixpoint ~access ~visited ~count =
        if Set.mem visited access || count > widening_threshold then
          access
        else
          let rec resolve_exports ~lead ~tail =
            match tail with
            | head :: tail ->
                Resolution.module_definition resolution (Reference.from_access lead)
                >>| (fun definition ->
                    match Module.aliased_export definition (Reference.from_access [head]) with
                    | Some export ->
                        (Reference.access export) @ tail
                    | _ ->
                        resolve_exports ~lead:(lead @ [head]) ~tail)
                |> Option.value ~default:access
            | _ ->
                access
          in
          match access with
          | head :: tail ->
              resolve_exports_fixpoint
                ~access:(resolve_exports ~lead:[head] ~tail)
                ~visited:(Set.add visited access)
                ~count:(count + 1)
          | _ ->
              access
      in
      resolve_exports_fixpoint ~access ~visited:Access.Set.empty ~count:0
    in
    (* Sanity check that resolved exports map to existing modules. *)
    let exported_is_valid_module =
      List.hd exported
      >>| (fun head ->
          Resolution.module_definition resolution (Reference.from_access [head])
          |> Option.is_some)
      |> Option.value ~default:false
    in
    if not (Access.equal exported access) && not exported_is_valid_module then
      access
    else
      exported


  let step
      ({ resolution; accumulator; f; _ } as state)
      ?element
      ?resolved
      ?target
      ?(continue = true)
      ~lead
      () =
    let accumulator =
      let resolved = Option.value resolved ~default:(Annotation.create Type.Top) in
      let element = Option.value element ~default:Value in
      f accumulator ~resolution ~resolved ~element ~lead
    in
    {
      state with
      accumulator;
      resolved;
      target;
      continue;
    }


  let abort state ?element ~lead () =
    step state ?element ~continue:false ~lead ()
end

module State = struct
  type nested_define_state = {
    nested_resolution: Resolution.t;
    nested_bottom: bool;
  }

  type nested_define = {
    nested: Define.t;
    initial: nested_define_state;
  }

  and t = {
    configuration: Configuration.Analysis.t;
    resolution: Resolution.t;
    errors: Error.Set.t;
    define: Define.t Node.t;
    nested_defines: nested_define Location.Reference.Map.t;
    bottom: bool;
    resolution_fixpoint: ResolutionSharedMemory.annotation_map Int.Map.Tree.t
  }


  let pp_nested_define format { nested = { Define.signature = { name; _ }; _ }; _ } =
    Format.fprintf format "%a" Reference.pp name


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
          Reference.pp name
          Annotation.pp annotation
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
      List.map (Set.to_list errors) ~f:error_to_string
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
    Define.equal left.nested right.nested


  and equal left right =
    (* Ignore errors in unit tests. *)
    Map.equal
      Annotation.equal
      (Resolution.annotations left.resolution)
      (Resolution.annotations right.resolution) &&
    left.bottom = right.bottom


  let create
      ?(configuration = Configuration.Analysis.create ())
      ?(bottom = false)
      ~resolution
      ~define
      ?(resolution_fixpoint = Int.Map.Tree.empty)
      () =
    {
      configuration;
      resolution;
      errors = Error.Set.empty;
      define;
      nested_defines = Location.Reference.Map.empty;
      bottom;
      resolution_fixpoint;
    }

  let add_invalid_type_parameters_errors ~resolution ~location ~define ~errors annotation =
    let mismatches, annotation = Resolution.check_invalid_type_parameters resolution annotation in
    let add_error
        errors
        { Resolution.name; expected_number_of_parameters; given_number_of_parameters } =
      Error.create
        ~location
        ~kind:(Error.InvalidTypeParameters {
            annotation = Type.Primitive name;
            expected_number_of_parameters;
            given_number_of_parameters;
          })
        ~define
      |> Set.add errors
    in
    List.fold mismatches ~f:add_error ~init:errors, annotation

  let check_and_correct_annotation ~resolution ~location ~define ~annotation ~resolved errors =
    let is_aliased_to_any =
      (* Special-case expressions typed as Any to be valid types. *)
      match annotation with
      | Type.Primitive _ -> Type.equal resolved Type.Any
      | _ -> false
    in
    let check_untracked_annotation errors annotation =
      if Resolution.is_tracked resolution annotation || is_aliased_to_any then
        errors
      else if not (Type.is_unknown resolved || Type.equal resolved Type.Any) then
        Error.create ~location ~kind:(Error.InvalidType annotation) ~define :: errors
      else
        Error.create ~location ~kind:(Error.UndefinedType annotation) ~define :: errors
    in
    let check_invalid_variables resolution errors variable =
      if not (Resolution.type_variable_exists resolution ~variable) then
        let error =
          let origin =
            if Define.is_toplevel (Node.value define) then
              Error.Toplevel
            else if Define.is_class_toplevel (Node.value define) then
              Error.ClassToplevel
            else
              Error.Define
          in
          Error.create
            ~location
            ~kind:(Error.InvalidTypeVariable { annotation = variable; origin })
            ~define
        in
        error :: errors
      else
        errors
    in
    let resolution =
      match annotation with
      | Type.Callable {
          Type.Callable.implementation = {
            Type.Callable.parameters = Type.Callable.Defined parameters;
            _;
          };
          _;
        } ->
          let parameters =
            List.map parameters ~f:Type.Callable.Parameter.annotation
            |> List.concat_map ~f:Type.free_variables
            |> List.map ~f:(fun variable -> Type.Variable variable)
          in
          List.fold
            parameters
            ~f:(fun resolution variable -> Resolution.add_type_variable resolution ~variable)
            ~init:resolution
      | _ ->
          resolution
    in
    let critical_errors =
      List.fold ~init:[] ~f:check_untracked_annotation (Type.elements annotation)
      |> (fun errors ->
          Type.free_variables annotation
          |> List.map ~f:(fun variable -> Type.Variable variable)
          |> List.fold
            ~f:(check_invalid_variables resolution)
            ~init:errors)
    in
    if List.is_empty critical_errors then
      add_invalid_type_parameters_errors annotation ~resolution ~location ~define ~errors
    else
      let errors =
        List.fold critical_errors ~init:errors ~f:(fun errors error -> Set.add errors error)
      in
      errors, Type.Top

  let parse_and_check_annotation
      ?(bind_variables = true)
      ~state:({ errors; define; resolution; _ } as state)
      ({ Node.location; _ } as expression) =
    let annotation =
      Resolution.parse_annotation
        ~allow_untracked:true
        ~allow_invalid_type_parameters:true
        resolution
        expression
    in
    let errors =
      if Type.equal annotation Type.Top then
        (* Could not even parse expression. *)
        Error.create
          ~location
          ~kind:(Error.InvalidType (Type.Primitive (Expression.show expression)))
          ~define
        |> Set.add errors
      else
        errors
    in
    let resolved = Resolution.resolve resolution expression in
    let errors, annotation =
      check_and_correct_annotation errors ~resolution ~location ~define ~annotation ~resolved
    in
    let annotation =
      if bind_variables then Type.mark_variables_as_bound annotation else annotation
    in
    { state with errors }, annotation


  let errors
      {
        configuration;
        resolution;
        errors;
        define = ({
            Node.value = { Define.signature = { name; _ }; _ } as define;
            _;
          } as define_node);
        _;
      } =
    let class_initialization_errors errors =
      (* Ensure non-nullable typed attributes are instantiated in init.
         This must happen after typechecking is finished to access the annotations
         added to resolution. *)
      let check_attributes_initialized define =
        let open Annotated in
        (Define.parent_definition ~resolution (Define.create define)
         >>| fun definition ->
         let propagate_initialization_errors errors attribute =
           let expected = Annotation.annotation (Attribute.annotation attribute) in
           match Attribute.name attribute with
           | name
             when not (Type.equal expected Type.Top ||
                       Attribute.initialized attribute) ->
               let reference =
                 Reference.create_from_list [(Statement.Define.self_identifier define); name]
               in
               if Map.mem (Resolution.annotations resolution) reference &&
                  not (Statement.Define.is_class_toplevel define) then
                 errors
               else
                 let error =
                   Error.create
                     ~location:(Attribute.location attribute)
                     ~kind:(
                       Error.UninitializedAttribute {
                         name;
                         parent = Annotated.Class.annotation definition ~resolution;
                         mismatch = {
                           Error.expected;
                           actual = (Type.optional expected);
                           actual_expressions = [];
                           due_to_invariance = false;
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
           definition)
        |> Option.value ~default:errors
      in
      if Define.is_constructor define then
        check_attributes_initialized define
      else if Define.is_class_toplevel define then
        begin
          let no_explicit_class_constructor =
            let name =
              Reference.prefix name
              >>| Reference.show
              |> Option.value ~default:""
            in
            Resolution.class_definition resolution (Type.Primitive name)
            >>| (fun { Node.value = definition; _ } -> Class.constructors definition)
            >>| List.is_empty
            |> Option.value ~default:false
          in
          if no_explicit_class_constructor then
            check_attributes_initialized define
          else
            errors
        end
      else
        errors
    in
    Set.to_list errors
    |> Error.join_at_define
      ~resolution
    |> class_initialization_errors
    |> Error.filter ~configuration ~resolution


  let coverage { resolution; _ } =
    Resolution.annotations resolution
    |> Map.data
    |> Coverage.aggregate


  let nested_defines { nested_defines; _ } =
    let process_define (location, { nested; initial = { nested_resolution; _ } }) =
      Node.create ~location nested, nested_resolution
    in
    Map.to_alist nested_defines
    |> List.map ~f:process_define


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
      Set.is_subset left.errors ~of_:right.errors &&
      Map.fold
        ~init:true
        ~f:(entry_less_or_equal right.nested_defines (fun _ _ -> true))
        left.nested_defines &&
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


  let join left right =
    if left.bottom then
      right
    else if right.bottom then
      left
    else
      let join_nested_defines ~key:_ = function
        | `Left nested_define
        | `Right nested_define
        | `Both (_, nested_define) -> Some nested_define
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
        errors = Set.union left.errors right.errors;
        nested_defines =
          Map.merge ~f:join_nested_defines left.nested_defines right.nested_defines;
        resolution = join_resolutions left.resolution right.resolution;
      }


  let widening_threshold = 10


  let rec meet ({ resolution; _ } as left) right =
    if left.bottom then
      left
    else if right.bottom then
      right
    else
      let annotations =
        let merge meet ~key:_ = function
          | `Both (left, right) ->
              Some (meet left right)
          | `Left _
          | `Right _ ->
              None
        in
        Map.merge
          (Resolution.annotations left.resolution)
          (Resolution.annotations right.resolution)
          ~f:(merge (Refinement.meet ~resolution));
      in
      let errors = Set.union left.errors right.errors in
      { left with errors; resolution = Resolution.with_annotations resolution ~annotations }


  let widen ~previous:({ resolution; _ } as previous) ~next ~iteration =
    if previous.bottom then
      next
    else if next.bottom then
      previous
    else
      let join_nested_defines ~key:_ = function
        | `Left nested_define
        | `Right nested_define
        | `Both (_, nested_define) -> Some nested_define
      in
      let widen_annotations ~key annotation =
        match annotation with
        | `Both (previous, next) ->
            Some (Refinement.widen ~resolution ~widening_threshold ~previous ~next ~iteration)
        | `Left previous
        | `Right previous when Reference.length key = 1 ->
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
      let join_resolution_fixpoints ~key:_ = function
        | `Left next_resolution
        | `Right next_resolution
        | `Both (_, next_resolution) -> Some next_resolution
      in
      let resolution_fixpoint = Int.Map.Tree.merge
          ~f:join_resolution_fixpoints
          previous.resolution_fixpoint
          next.resolution_fixpoint
      in
      {
        previous with
        errors = Set.union previous.errors next.errors;
        nested_defines =
          Map.merge ~f:join_nested_defines previous.nested_defines next.nested_defines;
        resolution = Resolution.with_annotations resolution ~annotations;
        resolution_fixpoint
      }


  let emit_raw_error ~state:({ errors; _ } as state) error =
    { state with errors = Set.add errors error }


  let emit_error ~state ~location ~kind ~define =
    Error.create ~location ~kind ~define
    |> emit_raw_error ~state


  type resolved = {
    state: t;
    resolved: Type.t;
  }

  (* Fold over an access path. Callbacks will be passed the current `accumulator`, the current
     `annotations`, the `resolved` type of the expression so far, as well as the kind of `element`
     we're currently folding over. *)
  let forward_access ~resolution ~initial ~f ?expression access =
    let open AccessState in
    let rec fold ~state ~lead ~tail =
      let { accumulator; resolved; target; resolution; _ } = state in
      let accesses_incomplete_type =
        match resolved with
        | Some { Annotation.annotation; _ } when Type.contains_escaped_free_variable annotation ->
            let full_lead =
              expression
              >>| (fun expression -> Access.ExpressionAccess { expression; access = lead })
              |> Option.value ~default:(Access.SimpleAccess lead)
            in
            Some (full_lead, annotation)
        | _ ->
            None
      in
      let resolved =
        let convert_escaped_to_anys ({ Annotation.annotation; _ } as full_annotation) =
          let annotation = Type.convert_escaped_free_variables_to_anys annotation in
          { full_annotation with annotation }
        in
        resolved
        >>| convert_escaped_to_anys
      in
      let find_method ~parent ~name =
        parent
        |> Resolution.class_definition resolution
        >>| Annotated.Class.create
        >>| Annotated.Class.attribute ~resolution ~name ~instantiated:parent ~transitive:true
        >>= fun attribute -> Option.some_if (Annotated.Attribute.defined attribute) attribute
        >>| Annotated.Attribute.annotation
        >>| Annotation.annotation
        >>= function
        | Type.Callable callable ->
            Some callable
        | _ ->
            None
      in
      let resolve_callables callables ~arguments:{ Node.location; value = arguments } =
        let signatures =
          let signature callable =
            let resolve_independent_callable () =
              let signature = Annotated.Signature.select ~arguments ~resolution ~callable in
              let backup () =
                let name, backup_argument =
                  match target, List.rev lead, callable with
                  | Some _, _ :: rest, { Type.Callable.kind = Type.Callable.Named name; _ } ->
                      (Reference.access name), List.rev rest
                  | _ ->
                      [], []
                in
                match arguments, Access.backup ~name with
                | [{ Argument.value; _ }], Some name ->
                    let resolution, arguments =
                      (* If we have an access with an expression, we can't blindly put the access in
                         there as we need the type of the parent to get passed in as an access to
                         allow manipulating the access chain. This will go away if we properly
                         abstract call nodes away. *)
                      match expression with
                      | Some expression ->
                          let argument_type =
                            let backup_argument =
                              match backup_argument with
                              | [] ->
                                  expression
                              | _ ->
                                  Node.create
                                    (Access
                                       (Access.ExpressionAccess
                                          { expression; access = backup_argument }))
                                    ~location
                            in
                            Resolution.resolve resolution backup_argument
                          in
                          let backup_argument = [Access.Identifier "$backup_argument"] in
                          let resolution =
                            Resolution.set_local
                              resolution
                              ~reference:(Reference.create "$backup_argument")
                              ~annotation:(Annotation.create argument_type)
                          in
                          let arguments = [{
                              Argument.value = {
                                Node.location;
                                value = Access (Access.SimpleAccess backup_argument);
                              };
                              name = None;
                            }]
                          in
                          resolution, arguments
                      | None ->
                          (* Just an access chain. Pass it in directly. *)
                          let arguments = [{
                              Argument.value = {
                                Node.location;
                                value = Access (SimpleAccess backup_argument);
                              };
                              name = None;
                            }]
                          in
                          resolution, arguments
                    in
                    Resolution.resolve resolution value
                    |> fun annotation -> find_method ~parent:annotation ~name
                    >>| fun callable -> Annotated.Signature.select ~arguments ~resolution ~callable
                | _ ->
                    None
              in

              match signature, target with
              | Annotated.Signature.NotFound _, Some _ ->
                  backup ()
                  |> Option.value ~default:signature
              | _ ->
                  signature
            in
            resolve_independent_callable ()
          in
          List.map callables ~f:signature
        in

        let signature, callees =
          let not_found = function | Annotated.Signature.NotFound _ -> true | _ -> false in
          match List.partition_tf signatures ~f:not_found with
          (* Prioritize missing signatures for union type checking. *)
          | not_found :: _, _ ->
              Some not_found, []
          | [], (Annotated.Signature.Found callable) :: found ->
              let callables =
                let extract = function
                  | Annotated.Signature.Found callable -> callable
                  | _ -> failwith "Not all signatures were found."
                in
                List.map found ~f:extract
              in
              let callees = callable :: callables in
              let signature =
                let joined_callable =
                  List.map callables ~f:(fun callable -> Type.Callable callable)
                  |> List.fold ~init:(Type.Callable callable) ~f:(Resolution.join resolution)
                in
                match joined_callable with
                | Type.Callable callable ->
                    Annotated.Signature.Found callable
                | _ ->
                    Annotated.Signature.NotFound { callable; reason = None }
              in
              Some signature, callees
          | _ ->
              None, []
        in

        let step signature annotation =
          step
            { state with resolution }
            ~element:(Signature { signature; callees; arguments; accesses_incomplete_type})
            ~resolved:(Annotation.create annotation)
            ~lead
            ()
        in

        match signature with
        | Some
            (Annotated.Signature.Found { implementation = { annotation; _ }; _ } as signature)
        | Some
            (Annotated.Signature.NotFound {
                callable = { implementation = { annotation; _ }; _ };
                _;
              } as signature) ->
            step signature annotation
        | None ->
            abort state ~lead ()
      in

      match tail with
      | head :: tail ->
          let qualifier = lead in
          let lead = lead @ [head] in

          let ({ AccessState.continue; accumulator; _ } as state) =
            match resolved, head with
            (* Typed context: operations are on a class definition. *)
            | Some resolved, Access.Call arguments ->
                (* Callable invocation. *)
                let resolved = Annotation.annotation resolved in
                let callables =
                  let callable =  function
                    | meta when Type.is_meta meta ->
                        let callable =
                          let backup = find_method ~parent:meta ~name:"__call__" in
                          match Type.single_parameter meta with
                          | TypedDictionary { name; fields; total } ->
                              Type.TypedDictionary.constructor ~name ~fields ~total
                              |> Option.some
                          | Variable { constraints = Type.Unconstrained; _ } ->
                              backup
                          | Variable { constraints = Type.Explicit constraints; _ }
                            when List.length constraints > 1 ->
                              backup
                          | Any ->
                              backup
                          | meta_parameter ->
                              let parent =
                                match meta_parameter with
                                | Variable { constraints = Type.Explicit [parent]; _ } ->
                                    parent
                                | Variable { constraints = Type.Bound parent; _ } ->
                                    parent
                                | _ ->
                                    meta_parameter
                              in
                              Resolution.class_definition resolution parent
                              >>| Annotated.Class.create
                              >>| Annotated.Class.constructor
                                ~instantiated:meta_parameter
                                ~resolution
                              >>= function | Type.Callable callable -> Some callable | _ -> None
                        in
                        callable
                    | Type.Callable callable ->
                        Some callable
                    | resolved ->
                        find_method ~parent:resolved ~name:"__call__"
                  in
                  match resolved with
                  | Type.Union annotations ->
                      List.map annotations ~f:callable
                      |> Option.all
                  | annotation ->
                      callable annotation
                      >>| (fun callable -> [callable])
                in
                callables
                >>| resolve_callables  ~arguments
                |> (function
                    | Some state ->
                        state
                    | None ->
                        abort state ~element:(NotCallable resolved) ~lead ())

            | Some resolved, Access.Identifier _
              when Type.is_callable (Annotation.annotation resolved) ->
                (* Nested function. *)
                Resolution.get_local resolution ~reference:(Reference.from_access lead)
                >>| (fun resolved -> step state ~resolved ~lead ())
                |> Option.value ~default:(abort ~lead state ())

            | Some resolved, Access.Identifier name ->
                (* Attribute access. *)
                let rec extract_access_data annotation ~inside_meta =
                  let reset_instantiated data = { data with instantiated = annotation } in
                  let was_variable, annotation =
                    match annotation with
                    | Type.Variable variable -> true, Type.upper_bound variable
                    | _ -> false, annotation
                  in
                  let data =
                    match annotation with
                    | Type.Top | Type.Bottom | Type.Any ->
                        []
                    | Type.Union annotations ->
                        List.concat_map annotations ~f:(extract_access_data ~inside_meta)
                    | annotation when Type.equal annotation Type.ellipsis ->
                        []
                    | annotation when Type.is_meta annotation ->
                        Type.single_parameter annotation
                        |> extract_access_data ~inside_meta:true
                    | _ ->
                        begin
                          match Resolution.class_definition resolution annotation with
                          | Some class_definition ->
                              [{
                                instantiated = annotation;
                                class_attributes = inside_meta;
                                class_definition = Annotated.Class.create class_definition;
                              }]
                          | None ->
                              raise (TypeWithoutClass annotation)
                        end
                  in
                  if was_variable then List.map data ~f:reset_instantiated else data
                in
                let parent_annotation = Annotation.annotation resolved in
                let attribute_step ~definition ~resolved =
                  let continue =
                    match definition with
                    | Defined _ -> true
                    | Undefined _ -> false
                  in
                  let target =
                    {
                      AccessState.reference = (Reference.from_access qualifier);
                      annotation = parent_annotation;
                    }
                  in
                  let element =
                    AccessState.Attribute { attribute = name; definition; accesses_incomplete_type }
                  in
                  step state ~resolved ~target ~element ~continue ~lead ()
                in
                let find_attributes ~head_data ~tail_data =
                  let find_attribute { instantiated; class_attributes; class_definition } =
                    let attribute =
                      Annotated.Class.attribute
                        class_definition
                        ~transitive:true
                        ~class_attributes
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
                    let definition =
                      if Annotated.Attribute.defined attribute then
                        Defined (Instance attribute)
                      else
                        Undefined (Instance { attribute; instantiated_target = instantiated })
                    in
                    definition, Annotated.Attribute.annotation attribute
                  in
                  let join_definitions ~head_definition ~tail_definitions =
                    List.find
                      (head_definition:: tail_definitions)
                      ~f:(function | Undefined _ -> true | _ -> false)
                    |> Option.value ~default:(head_definition)
                  in
                  let join_resolveds ~head_resolved ~tail_resolveds =
                    let apply_global_override resolved =
                      let open Annotation in
                      let annotation =
                        Resolution.get_local
                          resolution
                          ~reference:(Reference.from_access lead)
                          ~global_fallback:(Type.is_meta parent_annotation)
                      in
                      match annotation with
                      | Some { annotation; mutability = Immutable { scope = Global; original } }
                        when Type.is_unknown original ->
                          {
                            annotation;
                            mutability = Immutable {
                                scope = Global;
                                original = Annotation.original resolved;
                              }
                          }
                      | Some local ->
                          local
                      | None ->
                          resolved
                    in
                    let join sofar element =
                      let refined = Refinement.join ~resolution sofar element in
                      {
                        refined with
                        annotation = Type.union [sofar.annotation; element.annotation];
                      }
                    in
                    List.fold tail_resolveds ~init:head_resolved ~f:join
                    |> apply_global_override
                  in
                  let head_definition, head_resolved = find_attribute head_data in
                  let tail_definitions, tail_resolveds =
                    List.map tail_data ~f:find_attribute
                    |> List.unzip
                  in
                  attribute_step
                    ~definition:(join_definitions ~head_definition ~tail_definitions)
                    ~resolved:(join_resolveds ~head_resolved ~tail_resolveds)
                in
                begin
                  try
                    match extract_access_data parent_annotation ~inside_meta:false with
                    | [] ->
                        abort ~lead state ()
                    | head_data :: tail_data ->
                        find_attributes ~head_data ~tail_data
                  with TypeWithoutClass type_without_class ->
                    attribute_step
                      ~definition:(Undefined (TypeWithoutClass type_without_class))
                      ~resolved:(Annotation.create Type.Top)
                end
            | None, Access.Identifier name ->
                (* Module or global variable. *)
                begin
                  let annotation =
                    let local_annotation =
                      Resolution.get_local resolution ~reference:(Reference.from_access lead)
                    in
                    match local_annotation with
                    | Some annotation ->
                        Some annotation
                    | _ ->
                        (* Fallback to use a __getattr__ callable as defined by PEP 484. *)
                        let getattr =
                          Resolution.get_local
                            resolution
                            ~reference:(
                              Reference.create
                                ~prefix:(Reference.from_access qualifier)
                                "__getattr__")
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
                            Some (
                              Annotation.create_immutable
                                ~global:true
                                ~original:(Some Type.Top)
                                (Type.Callable.Overload.return_annotation signature)
                            )
                        | _ ->
                            None
                  in
                  match annotation with
                  | Some resolved ->
                      (* Locally known variable (either local or global). *)
                      let suppressed =
                        let rec suppressed lead = function
                          | head :: tail ->
                              begin
                                let lead = lead @ [head] in
                                let definition =
                                  Resolution.module_definition
                                    resolution
                                    (Reference.create_from_list lead)
                                in
                                match definition with
                                | Some definition when Module.empty_stub definition -> true
                                | _ -> suppressed lead tail
                              end
                          | [] ->
                              false
                        in
                        Annotation.annotation resolved
                        |> Type.class_name
                        |> Reference.as_list
                        |> suppressed []
                      in
                      if not suppressed then
                        step state ~resolved ~lead ()
                      else
                        abort state ~lead ()
                  | None ->
                      begin
                        let definition =
                          Resolution.module_definition
                            resolution
                            (Reference.from_access lead)
                        in
                        match definition with
                        | Some definition when Module.empty_stub definition ->
                            abort state ~lead ()
                        | Some _ ->
                            AccessState.create ~resolution ~accumulator ~f ()
                        | None ->
                            let element =
                              AccessState.Attribute {
                                attribute = name;
                                definition = Undefined (Module (Reference.from_access qualifier));
                                accesses_incomplete_type;
                              }
                            in
                            abort state ~element ~lead ()
                      end
                end
            | _ ->
                abort state ~lead ()
          in
          if continue then
            fold ~state ~lead ~tail
          else
            accumulator
      | _ ->
          accumulator
    in
    let access = resolve_exports ~resolution ~access in
    let resolved =
      expression
      >>| Resolution.resolve resolution
      >>| Annotation.create
    in
    fold
      ~state:(AccessState.create ?resolved ~accumulator:initial ~f ~resolution ())
      ~lead:[]
      ~tail:access


  let last_element ~resolution access =
    forward_access
      ~resolution
      ~initial:AccessState.Value
      ~f:(fun _ ~resolution:_ ~resolved:_ ~element ~lead:_ -> element)
      access

  let rec initial
      ?(configuration = Configuration.Analysis.create ())
      ~resolution
      ({
        Node.location;
        value = ({
            Define.signature = { name; parent; parameters; return_annotation; decorators; _ }
          ; _ } as define)
      } as define_node) =
    let check_decorators state =
      let check_decorator state decorator =
        let is_whitelisted decorator =
          let has_suffix { Node.value; _ } suffix =
            match value with
            | Expression.Access (Access.SimpleAccess access_list) ->
                let last_identifier =
                  let is_identifier = function
                    | Access.Identifier _ -> true
                    | _ -> false
                  in
                  List.find (List.rev access_list) ~f:is_identifier
                in
                begin
                  match last_identifier with
                  | Some (Access.Identifier identifier)
                    when String.equal identifier suffix ->
                      true
                  | _ ->
                      false
                end
            | _ -> false
          in
          let is_property_derivative decorator =
            has_suffix decorator "setter" ||
            has_suffix decorator "getter" ||
            has_suffix decorator "deleter"
          in
          let is_click_derivative decorator =
            has_suffix decorator "command"
          in
          (* TODO (T41383196): Properly deal with @property and @click *)
          is_property_derivative decorator ||
          is_click_derivative decorator ||
          Set.exists Recognized.whitelisted_decorators
            ~f:(Expression.exists_in_list ~match_prefix:true ~expression_list:[decorator])
        in
        if is_whitelisted decorator then
          state
        else
          let { state; _ } = forward_expression ~state ~expression:decorator in
          state
      in
      List.fold decorators ~init:state ~f:check_decorator
    in
    let check_return_annotation state =
      let add_variance_error (state, annotation) =
        let state =
          if Type.is_contravariant annotation then
            emit_error
              ~state
              ~location
              ~kind:(Error.InvalidTypeVariance { annotation; origin = Error.Return })
              ~define:define_node
          else
            state
        in
        state, annotation
      in
      let update_define (state, annotation) =
        let updated_define =
          if Type.is_unknown annotation then
            let signature =
              { define.signature with return_annotation = Some (Type.expression annotation) }
            in
            { define with signature }
          else
            define
        in
        { state with define = { define_node with Node.value = updated_define } }
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
            { Node.location; value = { Parameter.name; value; annotation } } =
          let add_incompatible_variable_error ~state ~value annotation default =
            if Type.equal default Type.Any ||
               Resolution.less_or_equal resolution ~left:default ~right:annotation ||
               Resolution.constraints_solution_exists resolution ~left:default ~right:annotation
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
                ~kind:(Error.IncompatibleVariableType {
                    name = Reference.create name;
                    mismatch =
                      Error.create_mismatch
                        ~resolution
                        ~expected:annotation
                        ~actual:default
                        ~actual_expression:value
                        ~covariant:true;
                    declare_location = instantiate location;
                  })
                ~define:define_node
          in
          let add_missing_parameter_annotation_error ~state ~given_annotation annotation =
            let name = name |> Identifier.sanitized in
            if
              name = "*" ||
              String.is_prefix ~prefix:"_" name ||
              Option.is_some given_annotation &&
              (String.is_prefix ~prefix:"**" name || String.is_prefix ~prefix:"*" name)
            then
              state
            else
              emit_error
                ~state
                ~location
                ~kind:(Error.MissingParameterAnnotation {
                    name = Reference.create name;
                    annotation;
                    given_annotation;
                    evidence_locations = [];
                    thrown_at_source = true;
                  })
                ~define:define_node
          in
          let add_variance_error (state, annotation) =
            let state =
              if not (Statement.Define.is_constructor define) && Type.is_covariant annotation then
                emit_error
                  ~state
                  ~location
                  ~kind:(Error.InvalidTypeVariance { annotation; origin = Error.Parameter })
                  ~define:define_node
              else
                state
            in
            state, annotation
          in
          let state, { Annotation.annotation; mutability } =
            match index, parent with
            | 0, Some parent
              when not (Define.is_class_toplevel define || Define.is_static_method define) ->
                let resolved, is_class_method =
                  let parent_annotation =
                    Resolution.parse_reference resolution parent
                    |> function
                    | Type.Primitive name ->
                        let variables =
                          TypeOrder.variables (Resolution.order resolution) (Type.Primitive name)
                        in
                        begin
                          match variables with
                          | None
                          | Some [] ->
                              Type.Primitive name
                          | Some variables ->
                              Type.Parametric { name; parameters = variables }
                          | exception _ ->
                              Type.Primitive name

                        end
                    | annotation ->
                        annotation
                  in
                  if Define.is_class_method define || Define.is_class_property define then
                    (* First parameter of a method is a class object. *)
                    Type.meta parent_annotation, true
                  else
                    (* First parameter of a method is the callee object. *)
                    parent_annotation, false
                in
                begin
                  match annotation with
                  | Some annotation ->
                      let state, annotation =
                        parse_and_check_annotation ~state ~bind_variables:false annotation
                      in
                      let compatible =
                        Resolution.less_or_equal resolution ~left:annotation ~right:resolved ||
                        (* TODO(T41994014) This should be reversed once solve_less_or_equal supports
                           when the variable is on the left  *)
                        Resolution.constraints_solution_exists
                          resolution
                          ~left:resolved
                          ~right:annotation
                      in
                      let state =
                        let name = Identifier.sanitized name in
                        let kind =
                          if compatible then
                            None
                          else if
                            (is_class_method && name = "cls") ||
                            (not is_class_method && name = "self")
                          then
                            (* Assume the user incorrectly tried to type the implicit parameter *)
                            Some (
                              Error.InvalidMethodSignature { annotation = Some annotation; name }
                            )
                          else
                            (* Assume the user forgot to specify the implicit parameter *)
                            Some(
                              Error.InvalidMethodSignature {
                                annotation = None;
                                name = if is_class_method then "cls" else "self";
                              }
                            )
                        in
                        match kind with
                        | Some kind ->
                            emit_error ~state ~location ~kind ~define:define_node
                        | None ->
                            state
                      in
                      state, Annotation.create annotation
                  | None ->
                      state, Annotation.create resolved
                end
            | _ ->
                let annotation_and_state =
                  annotation
                  >>| parse_and_check_annotation ~state ~bind_variables:false
                  >>| add_variance_error
                in
                let contains_prohibited_any parsed_annotation =
                  let contains_literal_any =
                    annotation
                    >>| Type.expression_contains_any
                    |> Option.value ~default:false
                  in
                  contains_literal_any &&
                  not (Resolution.is_string_to_any_mapping resolution parsed_annotation)
                in
                match annotation_and_state, value with
                | Some (_, annotation), Some value
                  when contains_prohibited_any annotation ->
                    let { resolved = value_annotation; _ } =
                      forward_expression ~state ~expression:value
                    in
                    add_missing_parameter_annotation_error
                      ~state
                      ~given_annotation:(Some annotation)
                      (Some value_annotation),
                    Annotation.create_immutable
                      ~global:false
                      ~original:(Some annotation)
                      value_annotation
                | Some (_, annotation), None
                  when contains_prohibited_any annotation ->
                    add_missing_parameter_annotation_error
                      ~state
                      ~given_annotation:(Some annotation)
                      None,
                    Annotation.create_immutable ~global:false annotation
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
                    add_missing_parameter_annotation_error
                      ~state
                      ~given_annotation:None
                      (Some annotation),
                    Annotation.create annotation
                | None, None ->
                    add_missing_parameter_annotation_error ~state ~given_annotation:None None,
                    Annotation.create Type.Any
          in
          let annotation =
            let annotation = Type.mark_variables_as_bound annotation in
            if String.is_prefix ~prefix:"**" name then
              Type.dictionary ~key:Type.string ~value:annotation
            else if String.is_prefix ~prefix:"*" name then
              Type.Tuple (Type.Unbounded annotation)
            else
              annotation
          in
          let mutability =
            match mutability with
            | Immutable { original; scope } ->
                Annotation.Immutable {
                  original = Type.mark_variables_as_bound original;
                  scope;
                }
            | _ ->
                mutability
          in
          let reference =
            name
            |> String.filter ~f:(fun character -> character <> '*')
            |> Reference.create
          in
          state, Map.set annotations ~key:reference ~data:{ Annotation.annotation; mutability }
        in
        match parameters, parent with
        | [], Some _
          when not (Define.is_class_toplevel define || Define.is_static_method define) ->
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
                ~define:define_node
            in
            state, (Resolution.annotations resolution)
        | _ ->
            List.foldi
              ~init:(state, (Resolution.annotations resolution))
              ~f:check_parameter
              parameters
      in
      let resolution = Resolution.with_annotations resolution ~annotations in
      let resolution_fixpoint =
        let precondition = Reference.Map.Tree.empty in
        let postcondition =
          Resolution.annotations resolution
          |> Reference.Map.to_tree
        in
        let key = ([%hash: int * int] (Cfg.entry_index, 0)) in
        Int.Map.Tree.set resolution_fixpoint ~key ~data:{ precondition; postcondition }
      in
      { state with resolution; resolution_fixpoint }
    in
    let check_base_annotations state =
      if Define.is_class_toplevel define then
        let open Annotated in
        let check_base state { Argument.value; _ } =
          parse_and_check_annotation ~state value
          |> fst
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
          if Define.is_constructor define ||
             Define.is_class_method define ||
             Define.is_static_method define ||
             Define.is_dunder_method define then
            errors
          else
            let open Annotated in
            (Define.create define
             |> Define.parent_definition ~resolution
             >>= fun definition ->
             Class.overrides
               definition
               ~resolution
               ~name:(Statement.Define.unqualified_name define)
             >>| fun overridden_attribute ->
             (* Check strengthening of postcondition. *)
             match Annotation.annotation (Attribute.annotation overridden_attribute) with
             | Type.Callable { Type.Callable.implementation; _ } ->
                 let original_implementation =
                   Reference.expression name
                   |> Resolution.resolve resolution
                   |> function
                   | Type.Callable { Type.Callable.implementation = original_implementation; _ } ->
                       original_implementation;
                   | annotation ->
                       raise (TypeOrder.Untracked annotation)
                 in
                 let errors =
                   let expected = Type.Callable.Overload.return_annotation implementation in
                   let actual = Type.Callable.Overload.return_annotation original_implementation in
                   if Type.is_resolved expected &&
                      not (Resolution.less_or_equal resolution ~left:actual ~right:expected) then
                     let error =
                       Error.create
                         ~location
                         ~kind:(Error.InconsistentOverride {
                             overridden_method = Statement.Define.unqualified_name define;
                             parent =
                               Attribute.parent overridden_attribute
                               |> Type.show
                               |> Reference.create;
                             override =
                               Error.WeakenedPostcondition
                                 (Error.create_mismatch
                                    ~resolution
                                    ~actual
                                    ~actual_expression:None
                                    ~expected
                                    ~covariant:false)
                           })
                         ~define:define_node
                     in
                     Set.add errors error
                   else
                     errors
                 in
                 (* Check weakening of precondition. *)
                 let overriding_parameters =
                   let remove_unused_parameter_denotation ~key ~data map =
                     String.Map.set map ~key:(Identifier.remove_leading_underscores key) ~data
                   in
                   Method.create ~define ~parent:(Annotated.Class.annotation definition ~resolution)
                   |> Method.parameter_annotations ~resolution
                   |> Map.fold ~init:String.Map.empty ~f:remove_unused_parameter_denotation
                 in
                 let check_parameter errors overridden_parameter =
                   let expected = Type.Callable.Parameter.annotation overridden_parameter in
                   let name =
                     Type.Callable.Parameter.name overridden_parameter
                     |> Identifier.remove_leading_underscores
                   in
                   match Map.find overriding_parameters name with
                   | Some actual ->
                       begin
                         let is_compatible =
                           let expected = Type.mark_variables_as_bound expected in
                           Resolution.constraints_solution_exists
                             resolution
                             ~left:expected
                             ~right:actual
                         in
                         try
                           if not (Type.equal Type.Top expected) && not is_compatible then
                             let error =
                               Error.create
                                 ~location
                                 ~kind:(Error.InconsistentOverride {
                                     overridden_method = Statement.Define.unqualified_name define;
                                     parent =
                                       Attribute.parent overridden_attribute
                                       |> Type.show
                                       |> Reference.create;
                                     override =
                                       Error.StrengthenedPrecondition
                                         (Error.Found
                                            (Error.create_mismatch
                                               ~resolution
                                               ~actual
                                               ~actual_expression:None
                                               ~expected
                                               ~covariant:false));
                                   })
                                 ~define:define_node
                             in
                             Set.add errors error
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
                             if String.is_prefix key ~prefix:"*" then
                               key :: starred
                             else
                               starred
                           in
                           Map.fold ~f:collect_starred_parameters ~init:[] overriding_parameters
                         in
                         let count_stars parameter =
                           parameter
                           |> String.take_while ~f:(fun character -> character = '*')
                           |> String.length
                         in
                         List.exists ~f:(fun parameter -> count_stars parameter = 1) starred
                         && List.exists ~f:(fun parameter -> count_stars parameter = 2) starred
                       in
                       if has_keyword_and_anonymous_starred_parameters then
                         errors
                       else
                         let error =
                           Error.create
                             ~location
                             ~kind:(Error.InconsistentOverride {
                                 overridden_method = Statement.Define.unqualified_name define;
                                 parent =
                                   Attribute.parent overridden_attribute
                                   |> Type.show
                                   |> Reference.create;
                                 override =
                                   Error.StrengthenedPrecondition (
                                     Error.NotFound (Identifier.sanitized name)
                                   );
                               })
                             ~define:define_node
                         in
                         Set.add errors error
                 in
                 Type.Callable.Overload.parameters implementation
                 |> Option.value ~default:[]
                 |> List.fold ~init:errors ~f:check_parameter
             | _ ->
                 errors)
            |> Option.value ~default:errors
        with
        | TypeOrder.Untracked _ ->
            errors
      in
      { state with errors }
    in
    let check_constructor_return state =
      if not (Statement.Define.is_constructor define) then
        state
      else
        match return_annotation with
        | Some ({ Node.location; _ } as annotation) ->
            let annotation = Resolution.parse_annotation resolution annotation in
            if Type.is_none annotation then
              state
            else
              emit_error
                ~state
                ~location
                ~kind:(Error.IncompatibleConstructorAnnotation annotation)
                ~define:define_node
        | _ ->
            state
    in
    create
      ~configuration
      ~resolution:(Resolution.with_parent resolution ~parent)
      ~define:define_node
      ()
    |> check_decorators
    |> check_return_annotation
    |> check_parameter_annotations
    |> check_base_annotations
    |> check_behavioral_subtyping
    |> check_constructor_return

  and forward_expression
      ~state:({ resolution; define; _ } as state)
      ~expression:{ Node.location; value } =
    (* Redirect accesses. *)
    let value, ({ resolution; _ } as state) =
      match value with
      | Access (SimpleAccess access) ->
          let access, resolution = AccessState.redirect ~resolution ~access in
          Access access, { state with resolution }
      | _ ->
          value, state
    in
    let rec forward_entry ~state ~entry:{ Dictionary.key; value } =
      let { state; resolved = key_resolved } = forward_expression ~state ~expression:key in
      let { state; resolved = value_resolved } = forward_expression ~state ~expression:value in
      (Type.weaken_literals key_resolved), (Type.weaken_literals value_resolved), state
    in
    let forward_generator
        ~state
        ~generator:{
        Comprehension.target;
        iterator = { Node.location; _ } as iterator;
        conditions;
        async;
      } =
      (* Propagate the target type information. *)
      let iterator =
        let value =
          if async then
            (Expression.Access.combine
               iterator
               [
                 Access.Identifier "__aiter__";
                 Access.Call (Node.create ~location []);
                 Access.Identifier "__anext__";
                 Access.Call (Node.create ~location []);
               ])
            |> (fun target -> Await { Node.location; value = (Access target) })
            |> Node.create ~location
          else
            (Expression.Access.combine
               iterator
               [
                 Access.Identifier "__iter__";
                 Access.Call (Node.create ~location []);
                 Access.Identifier "__next__";
                 Access.Call (Node.create ~location []);
               ])
            |> fun access -> { Node.location; value = Access access }
        in
        Assign { Assign.target; annotation = None; value; parent = None }
        |> Node.create ~location
      in
      let state =
        let { errors; _ } = state in
        let ({ errors = iterator_errors; _ } as state) =
          forward_statement ~state:{ state with errors = Error.Set.empty } ~statement:iterator
        in
        (* Don't throw Incompatible Variable errors on the generated iterator assign; we are
           temporarily minting a variable in a new scope and old annotations should be ignored. *)
        let errors =
          let is_not_assignment_error = function
            | { Error.kind = Error.IncompatibleVariableType _; _ } -> false
            | _ -> true
          in
          Set.filter ~f:(is_not_assignment_error) iterator_errors
          |> Set.union errors
        in
        { state with errors = errors }
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
      let forward_element { state = ({ resolution; _ } as state); resolved } expression =
        match Node.value expression with
        | Expression.Starred (Expression.Starred.Once expression) ->
            let { state; resolved = new_resolved } = forward_expression ~state ~expression in
            let parameter =
              match Resolution.join resolution new_resolved (Type.iterable Type.Bottom) with
              | Type.Parametric { parameters = [parameter]; _ } ->
                  parameter
              | _ ->
                  Type.Any
            in
            { state; resolved = Resolution.join resolution resolved parameter }
        | _ ->
            let { state; resolved = new_resolved } = forward_expression ~state ~expression in
            { state; resolved = Resolution.join resolution resolved new_resolved }
      in
      let correct_bottom { state; resolved } =
        let resolved =
          if Type.equal Type.Bottom resolved then
            Type.variable "_T"
            |> Type.mark_free_variables_as_escaped
          else
            resolved
        in
        { state; resolved }
      in
      List.fold elements ~init:{ state; resolved = Type.Bottom } ~f:forward_element
      |> (fun { state; resolved } -> { state; resolved = Type.weaken_literals resolved })
      |> correct_bottom
    in
    let join_resolved ~resolution left right =
      {
        state = join left.state right.state;
        resolved = Resolution.join resolution left.resolved right.resolved;
      }
    in
    let forward_access_step (found_error, state, _) ~resolution ~resolved ~element ~lead =
      let state = { state with resolution } in
      if found_error then
        found_error, state, Annotation.annotation resolved
      else if
        Type.exists
          (Annotation.annotation resolved)
          ~predicate:(fun annotation -> Type.equal annotation Type.undeclared) then
        let state =
          emit_error
            ~state
            ~location
            ~kind:(Error.UndefinedName (Reference.from_access lead))
            ~define
        in
        true, state, Annotation.annotation resolved
      else
        let error =
          match element with
          | AccessState.Signature {
              signature =
                (Annotated.Signature.NotFound {
                    callable = { Type.Callable.kind; implicit; _ };
                    reason = Some reason;
                  });
              _;
            } ->
              let open Annotated.Signature in
              let error =
                let callee =
                  match kind with
                  | Type.Callable.Named access -> Some access
                  | _ -> None
                in
                match reason with
                | InvalidKeywordArgument {
                    Node.location;
                    value = { expression; annotation }
                  } ->
                    let kind =
                      Error.InvalidArgument (Error.Keyword { expression; annotation })
                    in
                    Error.create ~location ~kind ~define
                | InvalidVariableArgument {
                    Node.location;
                    value = { expression; annotation }
                  } ->
                    let kind =
                      Error.InvalidArgument (Error.Variable { expression; annotation })
                    in
                    Error.create ~location ~kind ~define
                | Mismatch mismatch ->
                    let
                      { Annotated.Signature.actual; actual_expression; expected; name; position } =
                      Node.value mismatch
                    in
                    let mismatch, name, position, location =
                      Error.create_mismatch
                        ~resolution
                        ~actual
                        ~actual_expression:(Some actual_expression)
                        ~expected
                        ~covariant:true,
                      name,
                      position,
                      (Node.location mismatch)
                    in
                    let kind =
                      let normal =
                        (Error.IncompatibleParameterType {
                            name;
                            position;
                            callee;
                            mismatch;
                          })
                      in
                      begin
                        match implicit, callee >>| Reference.as_list with
                        | Some {
                            implicit_annotation = Type.TypedDictionary { fields; name; total};
                            _;
                          },
                          Some [ _; method_name ] ->
                            if
                              Type.TypedDictionary.is_special_mismatch ~method_name ~position ~total
                            then
                              match actual with
                              | Type.Literal (Type.String missing_key) ->
                                  Error.TypedDictionaryKeyNotFound
                                    { typed_dictionary_name = name; missing_key }
                              | Type.Primitive "str" ->
                                  Error.TypedDictionaryAccessWithNonLiteral
                                    (List.map fields ~f:(fun { name; _ } -> name))
                              | _ ->
                                  normal
                            else
                              normal
                        | _ ->
                            normal
                      end
                    in
                    Error.create
                      ~location
                      ~kind
                      ~define
                | MissingArgument name ->
                    Error.create
                      ~location
                      ~kind:(Error.MissingArgument { callee; name })
                      ~define
                | MutuallyRecursiveTypeVariables ->
                    Error.create
                      ~location
                      ~kind:(Error.MutuallyRecursiveTypeVariables callee)
                      ~define
                | TooManyArguments { expected; provided } ->
                    Error.create
                      ~location
                      ~kind:(Error.TooManyArguments { callee; expected; provided })
                      ~define
                | UnexpectedKeyword name ->
                    Error.create
                      ~location
                      ~kind:(Error.UnexpectedKeyword { callee; name })
                      ~define
              in
              Some error

          | AccessState.Signature { accesses_incomplete_type = Some (target, annotation); _ } ->
              let kind =
                Error.IncompleteType { target; annotation; attempted_action = Error.Calling }
              in
              Some (Error.create ~location ~kind ~define)
          | Attribute { attribute; definition = Undefined origin; _ } ->
              if Location.Reference.equal location Location.Reference.any then
                begin
                  Statistics.event
                    ~name:"undefined attribute without location"
                    ~normals:["attribute", attribute]
                    ();
                  None
                end
              else
                let kind =
                  match origin with
                  | Instance { attribute = class_attribute; instantiated_target } ->
                      let open Annotated in
                      if Type.equal instantiated_target Type.undeclared then
                        Error.UndefinedName (Reference.from_access lead)
                      else
                        Error.UndefinedAttribute {
                          attribute;
                          origin =
                            Error.Class {
                              annotation = instantiated_target;
                              class_attribute = Attribute.class_attribute class_attribute;
                            };
                        }

                  | Module reference when Reference.is_empty reference ->
                      Error.UndefinedName (Reference.create attribute)
                  | Module reference ->
                      Error.UndefinedAttribute { attribute; origin = Error.Module reference }
                  | TypeWithoutClass annotation ->
                      Error.UndefinedAttribute {
                        attribute;
                        origin = Error.Class { annotation; class_attribute = false };
                      }
                in
                Some (Error.create ~location ~kind ~define)
          | Attribute { accesses_incomplete_type = Some (target, annotation); attribute; _ } ->
              let kind =
                Error.IncompleteType {
                  target;
                  annotation;
                  attempted_action = Error.AttributeAccess attribute;
                }
              in
              Some (Error.create ~location ~kind ~define)
          | NotCallable Type.Any ->
              None
          | NotCallable annotation ->
              let kind = Error.NotCallable annotation in
              Some (Error.create ~location ~kind ~define)
          | _ ->
              None
        in
        Option.is_some error,
        error >>| emit_raw_error ~state |> Option.value ~default:state,
        Annotation.annotation resolved
    in
    match value with
    | Access
        (Access.SimpleAccess [
            Expression.Access.Identifier "reveal_type";
            Expression.Access.Call {
              Node.location;
              value = [{ Expression.Argument.value; _ }] };
          ]) ->
        (* Special case reveal_type(). *)
        let { state; resolved = annotation } = forward_expression ~state ~expression:value in
        let state =
          emit_error
            ~state
            ~location
            ~kind:(Error.RevealedType { expression = value; annotation })
            ~define
        in
        { state; resolved = Type.none }
    | Access
        (Access.SimpleAccess [
            Expression.Access.Identifier "typing";
            Expression.Access.Identifier "cast";
            Expression.Access.Call {
              Node.value = [
                { Expression.Argument.value = cast_annotation; _ };
                { Expression.Argument.value; _ };
              ];
              location;
            }
          ]) ->
        let contains_literal_any = Type.expression_contains_any cast_annotation in
        let state, cast_annotation = parse_and_check_annotation ~state cast_annotation in
        let { state; resolved; _ } = forward_expression ~state ~expression:value in
        let state =
          if contains_literal_any then
            emit_error
              ~state
              ~location
              ~kind:(Error.ProhibitedAny {
                  Error.name = Reference.create "typing.cast";
                  annotation = None;
                  given_annotation = Some cast_annotation;
                  evidence_locations = [];
                  thrown_at_source = true;
                })
              ~define
          else if Type.equal cast_annotation resolved then
            emit_error
              ~state
              ~location
              ~kind:(Error.RedundantCast resolved)
              ~define
          else
            state
        in
        { state; resolved = cast_annotation }
    | Access
        (Access.SimpleAccess [
            Access.Identifier "isinstance";
            Access.Call {
              Node.value = [
                { Argument.value = expression; _ };
                { Argument.value = annotations; _ };
              ];
              _;
            }]) ->
        (* We special case type inference for `isinstance` in asserted, and the typeshed stubs are
           imprecise (doesn't correctly declare the arguments as a recursive tuple. *)
        let state =
          let { state; _ } = forward_expression ~state ~expression in
          let previous_errors = Set.length state.errors in
          let state, annotations =
            let rec collect_types (state, collected) = function
              | { Node.value = Tuple annotations; _ } ->
                  let (state, new_annotations) =
                    List.fold annotations ~init:(state, []) ~f:collect_types
                  in
                  state, new_annotations @ collected
              | expression ->
                  let { state; resolved } = forward_expression ~state ~expression in
                  let new_annotations =
                    match resolved with
                    | Type.Tuple (Type.Bounded annotations) ->
                        List.map
                          annotations
                          ~f:(fun annotation -> annotation, Node.location expression)
                    | Type.Tuple (Type.Unbounded annotation)
                    | annotation ->
                        [annotation, Node.location expression]
                  in
                  state, new_annotations @ collected
            in
            collect_types (state, []) annotations
          in
          if Set.length state.errors > previous_errors then
            state
          else
            let add_incompatible_non_meta_error state (non_meta, location) =
              emit_error
                ~state
                ~location
                ~kind:(Error.IncompatibleParameterType {
                    name = None;
                    position = 2;
                    callee = Some (Reference.create "isinstance");
                    mismatch = {
                      Error.actual = non_meta;
                      actual_expressions = [];
                      expected = Type.meta Type.Any;
                      due_to_invariance = false;
                    }})
                ~define
            in
            List.find annotations ~f:(fun (annotation, _) -> not (Type.is_meta annotation))
            >>| add_incompatible_non_meta_error state
            |> Option.value ~default:state
        in
        { state; resolved = Type.bool }
    | Access access ->
        let state =
          match access with
          | Access.ExpressionAccess { expression; _ } ->
              let { state; _ } = forward_expression ~state ~expression in
              state
          | SimpleAccess _ ->
              state
        in
        (* Walk through the access. *)
        let _, state, resolved =
          match access with
          | Access.SimpleAccess access ->
              forward_access
                access
                ~resolution
                ~initial:(false, state, Type.Top)
                ~f:forward_access_step
          | Access.ExpressionAccess { expression; access} ->
              forward_access
                ~expression
                ~resolution
                ~initial:(false, state, Type.Top)
                ~f:forward_access_step
                access
        in

        let state =
          (* Check arguments and nested expressions. *)
          let forward_access state access =
            match access with
            | Access.Identifier _ ->
                state
            | Access.Call { Node.value = arguments; _ } ->
                let forward_argument state { Argument.value; _ } =
                  forward_expression ~state ~expression:value
                  |> fun { state; _ } -> state
                in
                List.fold arguments ~f:forward_argument ~init:state
          in
          match access with
          | Access.SimpleAccess access
          | Access.ExpressionAccess { access; _ } ->
              List.fold access ~f:forward_access ~init:state
        in
        { state; resolved }

    | Await expression ->
        let { state; resolved } = forward_expression ~state ~expression in
        let state =
          let is_awaitable =
            Resolution.less_or_equal
              resolution
              ~left:resolved
              ~right:(Type.awaitable Type.Top)
          in
          if not is_awaitable then
            emit_error
              ~state
              ~location
              ~kind:(Error.IncompatibleAwaitableType resolved)
              ~define
          else
            state
        in
        let resolved =
          Resolution.join resolution (Type.awaitable Type.Bottom) resolved
          |> Type.awaitable_value
        in
        { state; resolved }

    | BooleanOperator {
        BooleanOperator.left;
        operator;
        right;
      } ->
        let assume =
          let assume =
            match operator with
            | BooleanOperator.And -> left;
            | BooleanOperator.Or -> Expression.normalize (Expression.negate left);
          in
          Statement.assume assume
        in
        let { state = state_left; resolved = resolved_left } =
          forward_expression ~state ~expression:left
        in
        let { state = state_right; resolved = resolved_right } =
          forward_expression
            ~state:(forward_statement ~state ~statement:assume)
            ~expression:right
        in
        let resolved =
          match resolved_left, resolved_right, operator with
          | Optional resolved_left, resolved_right, BooleanOperator.Or ->
              Resolution.join resolution resolved_left resolved_right
          (* Zero is also falsy. *)
          | Optional integer, resolved_right, BooleanOperator.And
            when Type.equal integer Type.integer ->
              Type.optional (Resolution.join resolution (Type.literal_integer 0) resolved_right)
          | Optional _, resolved_right, BooleanOperator.And ->
              Type.optional resolved_right
          | resolved_left, resolved_right, _ ->
              Resolution.join resolution resolved_left resolved_right
        in
        { state = join state_left state_right; resolved }

    | Call _ ->
        (* TODO: T37313693 *)
        { state; resolved = Type.Top }

    | ComparisonOperator { ComparisonOperator.left; right; operator = ComparisonOperator.In }
    | ComparisonOperator { ComparisonOperator.left; right; operator = ComparisonOperator.NotIn } ->
        let { state; resolved = iterator } = forward_expression ~state ~expression:right in
        let rec has_method name annotation =
          match annotation with
          | Type.Union annotations ->
              List.for_all annotations ~f:(has_method name)
          | _ ->
              Resolution.class_definition resolution annotation
              >>| Annotated.Class.create
              >>| Annotated.Class.has_method ~transitive:true ~resolution ~name
              |> Option.value ~default:false
        in
        let converted_call =
          let { Node.location; _ } = left in
          if has_method "__contains__" iterator then
            let arguments = [{ Argument.name = None; value = left }] in
            (Access.combine
               right
               (Access.call ~arguments ~location ~name:"__contains__" ()))
          else if has_method "__iter__" iterator then
            (Access.combine
               right
               (Access.call ~location ~name:"__iter__" () @
                Access.call ~location ~name:"__next__" () @
                Access.call
                  ~arguments:[{ Argument.name = None; value = left }]
                  ~location
                  ~name:"__eq__"
                  ()))
          else
            (Access.combine
               right
               (Access.call
                  ~arguments:[{
                      Argument.name = None;
                      value = { Node.value = Expression.Integer 0; location };
                    }]
                  ~location
                  ~name:"__getitem__"
                  () @
                Access.call
                  ~arguments:[{ Argument.name = None; value = left }]
                  ~location
                  ~name:"__eq__"
                  ()))
        in
        converted_call
        |> (fun access -> Node.create (Access access) ~location)
        |> fun call -> forward_expression ~state ~expression:call

    | ComparisonOperator ({ ComparisonOperator.left; right; _ } as operator) ->
        begin
          match ComparisonOperator.override operator with
          | Some expression ->
              forward_expression ~state ~expression
          | None ->
              forward_expression ~state ~expression:left
              |> (fun { state; _ } -> forward_expression ~state ~expression:right)
              |> fun state -> { state with resolved = Type.bool }
        end

    | Complex _ ->
        { state; resolved = Type.complex }

    | Dictionary { Dictionary.entries; keywords } ->
        let key, value, state =
          let forward_entry (key, value, state) entry =
            let new_key, new_value, state = forward_entry ~state ~entry in
            Resolution.join resolution key new_key,
            Resolution.join resolution value new_value,
            state
          in
          List.fold entries ~f:forward_entry ~init:(Type.Bottom, Type.Bottom, state)
        in
        let key =
          if List.is_empty keywords && Type.equal Type.Bottom key then
            Type.variable "_KT"
            |> Type.mark_free_variables_as_escaped
          else
            key
        in
        let value =
          if List.is_empty keywords && Type.equal Type.Bottom value then
            Type.variable "_VT"
            |> Type.mark_free_variables_as_escaped
          else
            value
        in
        let resolved, state =
          let forward_keyword (resolved, state) keyword =
            let { state; resolved = keyword_resolved } =
              forward_expression ~state ~expression:keyword
            in
            Resolution.join resolution resolved keyword_resolved,
            state
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


    | Ellipsis ->
        { state; resolved = Type.ellipsis }

    | False ->
        { state; resolved = Type.bool }

    | Float _ ->
        { state; resolved = Type.float }

    | Generator { Comprehension.element; generators } ->
        let { state; resolved } = forward_comprehension ~element ~generators in
        { state; resolved = Type.generator resolved }

    | Integer literal ->
        { state; resolved = Type.literal_integer literal}

    | Lambda { Lambda.body; parameters } ->
        let resolution_with_parameters =
          let add_parameter resolution { Node.value = { Parameter.name; _ }; _ } =
            let name =
              String.chop_prefix name ~prefix:"*"
              |> Option.value ~default:name
            in
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
           to write a function that take in callables with literal return types.  If you really want
           that behavior you can always write a real inner function with a literal return type *)
        let resolved = Type.weaken_literals resolved in
        let create_parameter { Node.value = { Parameter.name; _ }; _ } =
          Type.Callable.Parameter.Named {
            Type.Callable.Parameter.name;
            annotation = Type.Any;
            default = false;
          }
        in
        let parameters =
          List.map parameters ~f:create_parameter
          |> fun parameters -> Type.Callable.Defined parameters
        in
        {
          state = { state with resolution };
          resolved = Type.Callable.create ~parameters ~annotation:resolved ();
        }

    | List elements ->
        let { state; resolved } = forward_elements ~state ~elements in
        { state; resolved = Type.list resolved }

    | ListComprehension { Comprehension.element; generators } ->
        let { state; resolved } = forward_comprehension ~element ~generators in
        { state; resolved = Type.list resolved }

    | Name _ ->
        (* TODO: T37313693 *)
        { state; resolved = Type.Top }

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
                forward_expression ~state ~expression
                |> fun { state; _ } -> state)
            ~init:state
        in
        { state; resolved = Type.string }

    | String { StringLiteral.kind = StringLiteral.Bytes; _ } ->
        { state; resolved = Type.bytes }

    | String { StringLiteral.kind = StringLiteral.String; value } ->
        { state; resolved = Type.literal_string value}

    | String { StringLiteral.kind = StringLiteral.Mixed _; _ } ->
        (* NOTE: We may run into this case with nested f-strings. Treat them
           as literal strings until the parser gets full support of them. *)
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
        (* The resolution is local to the ternary expression and
           should not be propagated out. *)
        { state = { state with resolution }; resolved }

    | True ->
        { state; resolved = Type.bool }

    | Tuple elements ->
        let state, resolved =
          let forward_element (state, resolved) expression =
            let { state; resolved = new_resolved } = forward_expression ~state ~expression in
            state, new_resolved :: resolved
          in
          List.fold
            elements
            ~f:forward_element
            ~init:(state, [])
        in
        { state; resolved = Type.tuple (List.rev resolved) }

    | UnaryOperator ({ UnaryOperator.operand; _ } as operator) ->
        begin
          match UnaryOperator.override operator with
          | Some expression ->
              forward_expression ~state ~expression
          | None ->
              let state = forward_expression ~state ~expression:operand in
              { state with resolved = Type.bool }
        end

    | Expression.Yield (Some expression) ->
        let { state; resolved } = forward_expression ~state ~expression in
        { state; resolved = Type.generator resolved }
    | Expression.Yield None ->
        { state; resolved = Type.generator Type.none }


  and forward_statement
      ~state:({
          resolution;
          define = ({
              Node.location = define_location;
              value = { Define.signature = {
                  async;
                  parent = define_parent;
                  return_annotation = return_annotation_expression;
                  _;
                }; body } as define;
            } as define_node);
          _;
        } as state)
      ~statement:{ Node.location; value } =
    let instantiate location =
      Location.instantiate ~lookup:(fun hash -> Ast.SharedMemory.Handles.get ~hash) location
    in
    (* We weaken type inference of mutable literals for assignments and returns
       to get around the invariance of containers when we can prove that casting to
       a supertype is safe. *)
    let validate_return ~expression ~state ~actual ~is_implicit =
      let return_annotation =
        let annotation =
          Annotated.Callable.return_annotation ~define ~resolution
        in
        if async then
          Type.coroutine_value annotation
        else
          annotation
      in
      let return_annotation = Type.mark_variables_as_bound return_annotation in
      let actual =
        Resolution.resolve_mutable_literals
          resolution
          ~expression
          ~resolved:actual
          ~expected:return_annotation
      in
      let check_incompatible_return state =
        if not (Resolution.constraints_solution_exists
                  resolution ~left:actual ~right:return_annotation) &&
           not (Define.is_abstract_method define) &&
           not (Define.is_overloaded_method define) &&
           not (Type.is_none actual && (Annotated.Callable.is_generator define)) &&
           not (Type.is_none actual && Type.is_noreturn return_annotation) then
          let rec check_unimplemented = function
            | [{ Node.value = Statement.Pass; _ };
               { Node.value = Statement.Return { Return.expression = None; _ }; _ }] ->
                true
            | {
              Node.value = Statement.Expression { Node.value = Expression.String _; _ };
              _;
            } :: tail ->
                check_unimplemented tail
            | _ ->
                false
          in
          emit_error
            ~state
            ~location
            ~kind:(Error.IncompatibleReturnType
                     {
                       mismatch =
                         (Error.create_mismatch
                            ~resolution
                            ~actual
                            ~actual_expression:expression
                            ~expected:return_annotation
                            ~covariant:true);
                       is_implicit;
                       is_unimplemented = check_unimplemented body
                     })
            ~define:define_node
        else
          state
      in
      let check_missing_return state =
        let contains_literal_any =
          return_annotation_expression
          >>| Type.expression_contains_any
          |> Option.value ~default:false
        in
        if not (Define.has_return_annotation define) ||
           (contains_literal_any &&
            not (Resolution.is_string_to_any_mapping resolution return_annotation))
        then
          let given_annotation =
            Option.some_if (Define.has_return_annotation define) return_annotation
          in
          emit_error
            ~state
            ~location:define_location
            ~kind:(Error.MissingReturnAnnotation {
                name = Reference.create "$return_annotation";
                annotation = Some actual;
                given_annotation;
                evidence_locations = [instantiate location];
                thrown_at_source = true;
              })
            ~define:define_node
        else
          state
      in
      state
      |> check_incompatible_return
      |> check_missing_return
    in
    match value with
    | Assign { Assign.target; annotation; value; parent } ->
        let state, original_annotation =
          annotation
          >>| parse_and_check_annotation ~state
          >>| (fun (state, annotation) -> state, Some annotation)
          |> Option.value ~default:(state, None)
        in
        let original_annotation =
          original_annotation
          >>| (fun annotation ->
              Type.class_variable_value annotation
              |> Option.value ~default:annotation)
        in
        let parsed =
          Resolution.parse_annotation ~allow_invalid_type_parameters:true resolution value
        in
        let is_type_alias =
          (* Consider anything with a RHS that is a type to be an alias. *)
          match Node.value value with
          | Expression.String _ -> false
          | _ ->
              begin
                match parsed with
                | Type.Top -> false
                | Type.Optional Type.Bottom -> false
                | annotation -> not (Resolution.contains_untracked resolution annotation)
              end
        in
        let state, resolved =
          let { state = { resolution; _ } as new_state; resolved } =
            forward_expression ~state ~expression:value
          in
          let resolved = Type.remove_undeclared resolved in
          (* TODO(T35601774): We need to suppress subscript related errors on generic classes. *)
          if is_type_alias then
            let errors, _ =
              add_invalid_type_parameters_errors
                ~resolution
                ~location
                ~define:define_node
                ~errors:state.errors
                parsed
            in
            { state with resolution; errors }, resolved
          else
            new_state, resolved
        in
        let guide =
          (* This is the annotation determining how we recursively break up the assignment. *)
          match original_annotation with
          | Some annotation when not (Type.is_unknown annotation) -> annotation
          | _ -> if Type.equal Type.ellipsis resolved then Type.Top else resolved
        in
        let explicit = Option.is_some annotation in
        let rec forward_assign
            ~state:({ resolution; _ } as state)
            ~target:{ Node.location; value = target_value }
            ~guide
            ~resolved
            ~expression =
          let is_named_tuple annotation =
            Resolution.less_or_equal
              resolution
              ~left:annotation
              ~right:Type.named_tuple
          in
          let get_named_tuple_parameters annotation =
            let namedtuple_attribute_annotations attributes =
              let open Annotated.Class.Attribute in
              let filter_attribute { Node.value = { annotation; name; _ }; _ } =
                let fields =
                  let is_fields = function
                    | { Node.value = { name = "_fields"; _ }; _ } ->
                        true
                    | _ ->
                        false
                  in
                  match List.find ~f:is_fields attributes >>| Node.value with
                  | Some { value = { Node.value = Tuple fields; _ }; _ } -> fields
                  | _ -> []
                in
                let equals name field =
                  match Node.value field with
                  | String { StringLiteral.value; _ } -> name = value
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
            | Type.Tuple (Type.Unbounded _) ->
                true
            (* Bounded tuples subclass iterable, but should be handled in the nonuniform case. *)
            | Type.Tuple (Type.Bounded _) ->
                false
            | _ ->
                Resolution.less_or_equal
                  resolution
                  ~left:annotation
                  ~right:(Type.iterable Type.Top)
          in
          let uniform_sequence_parameter annotation =
            match annotation with
            | Type.Tuple (Type.Unbounded parameter) ->
                parameter
            | _ ->
                Resolution.join resolution annotation (Type.iterable Type.Bottom)
                |> function
                | Type.Parametric { parameters = [parameter]; _ } -> parameter
                | _ -> Type.Top
          in
          let is_nonuniform_sequence ~minimum_length annotation =
            (* TODO(32692300): this should support tuple subclasses as well. *)
            match annotation with
            | Type.Tuple (Type.Bounded parameters)
              when minimum_length <= List.length parameters ->
                true
            | annotation
              when is_named_tuple annotation &&
                   minimum_length <= List.length (get_named_tuple_parameters annotation) ->
                true
            | _ ->
                false
          in
          let nonuniform_sequence_parameters annotation =
            match annotation with
            | Type.Tuple (Type.Bounded parameters) -> parameters
            | annotation when is_named_tuple annotation -> get_named_tuple_parameters annotation
            | _ -> []
          in

          match target_value with
          | Access (SimpleAccess access) ->
              let target_annotation, element =
                let fold _ ~resolution:_ ~resolved ~element ~lead:_ = resolved, element in
                forward_access
                  ~f:fold
                  ~resolution
                  ~initial:((Annotation.create Type.Top), AccessState.Value)
                  access
              in
              let expected, is_immutable =
                match original_annotation with
                | Some original ->
                    original, true
                | _ ->
                    if Annotation.is_immutable target_annotation then
                      Annotation.original target_annotation, true
                    else
                      Type.Top, false
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
                | _ ->
                    false
              in
              let state =
                let is_valid_enumeration_assignment =
                  let parent_annotation =
                    parent
                    >>| Resolution.parse_reference resolution
                    |> (fun annotation -> Option.value annotation ~default:Type.Top)
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
                    ~right:Type.enumeration &&
                  compatible
                in
                if is_immutable &&
                   not (Type.equal resolved Type.ellipsis) &&
                   not (Resolution.constraints_solution_exists
                          resolution ~left:resolved ~right:expected) &&
                   not is_typed_dictionary_initialization &&
                   not is_valid_enumeration_assignment then
                  let kind =
                    let open Annotated in
                    match element with
                    | Attribute
                        {
                          attribute = access;
                          definition = Undefined (Instance { attribute; _ });
                          _;
                        }
                    | Attribute
                        { attribute = access; definition = Defined (Instance attribute); _ } ->
                        Error.IncompatibleAttributeType {
                          parent = Attribute.parent attribute;
                          incompatible_type = {
                            Error.name = Reference.create access;
                            mismatch =
                              (Error.create_mismatch
                                 ~resolution
                                 ~actual:resolved
                                 ~actual_expression:expression
                                 ~expected
                                 ~covariant:true);
                            declare_location = instantiate (Attribute.location attribute);
                          };
                        }
                    | _ ->
                        Error.IncompatibleVariableType {
                          Error.name = Reference.from_access access;
                          mismatch =
                            (Error.create_mismatch
                               ~resolution
                               ~actual:resolved
                               ~actual_expression:expression
                               ~expected
                               ~covariant:true);
                          declare_location = instantiate location;
                        }
                  in
                  emit_error ~state ~location ~kind ~define:define_node
                else
                  state
              in

              (* Check for missing annotations. *)
              let error =
                let insufficiently_annotated, thrown_at_source =
                  let is_reassignment =
                    (* Special-casing re-use of typed parameters as attributes *)
                    match Node.value value with
                    | Access (SimpleAccess value_access) ->
                        let target_access = Access.show_sanitized (List.tl_exn access) in
                        let value_access = Access.show_sanitized value_access in
                        Annotation.is_immutable target_annotation &&
                        not (Type.is_unknown expected) &&
                        (target_access = value_access || target_access = "_" ^ value_access)
                    | _ ->
                        false
                  in
                  match annotation with
                  | Some annotation when Type.expression_contains_any annotation ->
                      original_annotation
                      >>| Resolution.is_string_to_any_mapping resolution
                      |> Option.value ~default:false
                      |> not
                      |> (fun insufficient -> insufficient, true)
                  | None when is_immutable && not is_reassignment ->
                      let is_toplevel =
                        Define.is_toplevel define ||
                        Define.is_class_toplevel define ||
                        Define.is_constructor define
                      in
                      let contains_any annotation =
                        if Resolution.is_string_to_any_mapping resolution annotation then
                          false
                        else
                          Type.contains_any annotation
                      in
                      Type.equal expected Type.Top || contains_any expected, is_toplevel
                  | _ ->
                      false, false
                in
                let actual_annotation, evidence_locations =
                  if Type.equal resolved Type.Top || Type.equal resolved Type.ellipsis then
                    None, []
                  else
                    Some resolved, [instantiate location]
                in
                let is_illegal_attribute_annotation
                    {
                      Node.value = { AnnotatedClass.Attribute.parent = attribute_parent; _ };
                      _;
                    } =
                  let parent_annotation =
                    define_parent
                    >>| Resolution.parse_reference resolution
                    |> (fun annotation -> Option.value annotation ~default:Type.Top)
                  in
                  explicit && (not (Type.equal parent_annotation attribute_parent))
                in
                let reference = Reference.from_access access in
                match element with
                | Attribute { attribute = access; definition = Defined (Module _); _ }
                  when insufficiently_annotated ->
                    Error.create
                      ~location
                      ~kind:(Error.MissingGlobalAnnotation {
                          Error.name = Reference.create access;
                          annotation = actual_annotation;
                          given_annotation = Option.some_if is_immutable expected;
                          evidence_locations;
                          thrown_at_source = true;
                        })
                      ~define:define_node
                    |> Option.some
                | Attribute { definition = Undefined (Instance { attribute; _ }); _ }
                | Attribute { definition = Defined (Instance attribute); _ }
                  when is_illegal_attribute_annotation attribute ->
                    (* Non-self attributes may not be annotated. *)
                    Error.create
                      ~location
                      ~kind:(Error.IllegalAnnotationTarget target)
                      ~define:define_node
                    |> Option.some
                | Attribute { attribute = access; definition = Defined (Instance attribute); _ }
                  when insufficiently_annotated ->
                    let attribute_location = Annotated.Attribute.location attribute in
                    Error.create
                      ~location:attribute_location
                      ~kind:(Error.MissingAttributeAnnotation {
                          parent = Annotated.Attribute.parent attribute;
                          missing_annotation = {
                            Error.name = Reference.create access;
                            annotation = actual_annotation;
                            given_annotation = Option.some_if is_immutable expected;
                            evidence_locations;
                            thrown_at_source;
                          };
                        })
                      ~define:define_node
                    |> Option.some
                | Attribute _ when insufficiently_annotated && not is_type_alias ->
                    Error.create
                      ~location
                      ~kind:(Error.ProhibitedAny {
                          Error.name = Reference.from_access access;
                          annotation = actual_annotation;
                          given_annotation = Option.some_if is_immutable expected;
                          evidence_locations;
                          thrown_at_source = true;
                        })
                      ~define:define_node
                    |> Option.some
                | Attribute _ ->
                    None
                | Value
                  when (Resolution.is_global ~reference resolution) &&
                       insufficiently_annotated &&
                       not is_type_alias ->
                    let global_location =
                      Reference.from_access access
                      |> Reference.delocalize
                      |> Resolution.global resolution
                      >>| Node.location
                      |> Option.value ~default:location
                    in
                    Error.create
                      ~location:global_location
                      ~kind:(Error.MissingGlobalAnnotation {
                          Error.name = Reference.from_access access;
                          annotation = actual_annotation;
                          given_annotation = Option.some_if is_immutable expected;
                          evidence_locations;
                          thrown_at_source;
                        })
                      ~define:define_node
                    |> Option.some
                | Value when is_type_alias && Type.expression_contains_any value ->
                    let value_annotation = Resolution.parse_annotation resolution value in
                    if Resolution.is_string_to_any_mapping resolution value_annotation then
                      None
                    else
                      Error.create
                        ~location
                        ~kind:(Error.ProhibitedAny {
                            Error.name = Reference.from_access access;
                            annotation = None;
                            given_annotation = Some value_annotation;
                            evidence_locations;
                            thrown_at_source = true;
                          })
                        ~define:define_node
                      |> Option.some
                | _ ->
                    begin
                      match explicit, access with
                      | false, _
                      | _, [_] ->
                          None
                      | _ ->
                          Error.create
                            ~location
                            ~kind:(Error.IllegalAnnotationTarget target)
                            ~define:define_node
                          |> Option.some
                    end
              in
              let state = error >>| emit_raw_error ~state |> Option.value ~default:state in
              let is_valid_annotation =
                error
                >>| Error.kind
                |> function | Some (Error.IllegalAnnotationTarget _) -> false | _ -> true
              in

              (* Propagate annotations. *)
              let state =
                let reference = Reference.from_access access in
                let annotation =
                  if explicit && is_valid_annotation then
                    let annotation =
                      Annotation.create_immutable
                        ~global:(Resolution.is_global ~reference resolution)
                        guide
                    in
                    if Type.is_concrete resolved && not (Type.is_ellipsis resolved) then
                      Refinement.refine ~resolution annotation resolved
                    else
                      annotation
                  else if Annotation.is_immutable target_annotation then
                    Refinement.refine ~resolution target_annotation guide
                  else
                    Annotation.create guide
                in
                let state, annotation =
                  if not explicit &&
                     not is_type_alias &&
                     Type.contains_escaped_free_variable (Annotation.annotation annotation) then
                    let kind =
                      Error.IncompleteType {
                        target = Access.SimpleAccess access;
                        annotation = resolved;
                        attempted_action = Naming;
                      }
                    in
                    let converted =
                      Type.convert_escaped_free_variables_to_anys (Annotation.annotation annotation)
                    in
                    emit_error ~state ~location ~kind ~define:define_node,
                    { annotation with annotation = converted }
                  else
                    state, annotation
                in
                let resolution = Resolution.set_local resolution ~reference ~annotation in
                { state with resolution }
              in
              state
          | List elements
          | Tuple elements
            when is_uniform_sequence guide ->
              let propagate state element =
                match Node.value element with
                | Starred (Starred.Once target) ->
                    let guide =
                      uniform_sequence_parameter guide
                      |> Type.list
                    in
                    let resolved =
                      uniform_sequence_parameter resolved
                      |> Type.list
                    in
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
                  List.split_while
                    elements
                    ~f:(fun element -> not (is_starred element))
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
                | Type.Tuple (Type.Bounded annotations)
                  when List.length annotations = List.length elements ->
                    annotations
                | _ ->
                    List.map elements ~f:(fun _ -> Type.Top)
              in
              let assignees = left @ starred @ right in
              let state, annotations =
                if List.length annotations <> List.length assignees then
                  let state =
                    emit_error
                      ~state
                      ~location
                      ~kind:(Error.Unpack {
                          expected_count = List.length assignees;
                          unpack_problem = CountMismatch (List.length annotations);
                        })
                      ~define:define_node
                  in
                  state, List.map assignees ~f:(fun _ -> Type.Top)
                else
                  state, annotations
              in
              List.zip_exn assignees annotations
              |> List.zip_exn resolved
              |> List.fold
                ~init:state
                ~f:(fun state (resolved, (target, guide)) ->
                    forward_assign ~state ~target ~guide ~resolved ~expression:None)
          | List elements
          | Tuple elements ->
              let kind =
                match guide with
                | Type.Tuple (Type.Bounded parameters) ->
                    (Error.Unpack {
                        expected_count = List.length elements;
                        unpack_problem = CountMismatch (List.length parameters);
                      })
                | annotation when is_named_tuple annotation ->
                    (Error.Unpack {
                        expected_count = List.length elements;
                        unpack_problem = CountMismatch (
                            List.length (get_named_tuple_parameters annotation)
                          );
                      })
                | _ ->
                    (Error.Unpack {
                        expected_count = List.length elements;
                        unpack_problem = UnacceptableType guide;
                      })
              in
              let state = emit_error ~state ~location ~kind ~define:define_node in
              List.fold
                elements
                ~init:state
                ~f:(fun state target ->
                    forward_assign
                      ~state
                      ~target
                      ~guide:Type.Top
                      ~resolved:Type.Top
                      ~expression:None)
          | _ ->
              if Option.is_some annotation then
                emit_error
                  ~state
                  ~location
                  ~kind:(Error.IllegalAnnotationTarget target)
                  ~define:define_node
              else
                state
        in
        forward_assign ~state ~target ~guide ~resolved ~expression:(Some value)

    | Assert { Assert.test; _ } ->
        begin
          let { resolution; _ } as state =
            forward_expression ~state ~expression:test
            |> fun { state; _ } -> state
          in
          let parse_isinstance_annotation annotation =
            let parse_meta annotation =
              match parse_and_check_annotation ~state annotation |> snd with
              | Type.Top ->
                  (* Try to resolve meta-types given as expressions. *)
                  begin
                    match Resolution.resolve resolution annotation with
                    | annotation when Type.is_meta annotation ->
                        Type.single_parameter annotation
                    | Type.Tuple Bounded elements
                      when List.for_all ~f:Type.is_meta elements ->
                        List.map ~f:Type.single_parameter elements
                        |> Type.union
                    | Type.Tuple Unbounded element when Type.is_meta element ->
                        Type.single_parameter element
                    | _ ->
                        Type.Top
                  end
              | annotation ->
                  annotation
            in
            match annotation with
            | { Node.value = Tuple elements; _ } ->
                List.map ~f:parse_meta elements
                |> (fun elements -> Type.Union elements)
            | _ ->
                parse_meta annotation
          in
          match Node.value test with
          | False ->
              (* Explicit bottom. *)
              { state with bottom = true }

          | Access
              (SimpleAccess [
                  Access.Identifier "isinstance";
                  Access.Call {
                    Node.value = [
                      {
                        Argument.name = None;
                        value = { Node.value = Access (SimpleAccess access); _ };
                      };
                      { Argument.name = None; value = annotation };
                    ];
                    _;
                  }
                ]) ->
              let reference = Reference.from_access access in
              let annotation = parse_isinstance_annotation annotation in
              let updated_annotation =
                let refinement_unnecessary existing_annotation =
                  Refinement.less_or_equal
                    ~resolution
                    existing_annotation
                    (Annotation.create annotation)
                  && not (Type.equal (Annotation.annotation existing_annotation) Type.Bottom)
                  && not (Type.equal (Annotation.annotation existing_annotation) Type.ellipsis)
                in
                match Resolution.get_local resolution ~reference with
                | Some existing_annotation when refinement_unnecessary existing_annotation ->
                    existing_annotation
                | _ ->
                    Annotation.create annotation
              in
              let resolution =
                Resolution.set_local
                  resolution
                  ~reference
                  ~annotation:updated_annotation
              in
              { state with resolution }

          | UnaryOperator {
              UnaryOperator.operator = UnaryOperator.Not;
              operand = {
                Node.value =
                  Access
                    (Access.SimpleAccess [
                        Access.Identifier "isinstance";
                        Access.Call {
                          Node.value = [
                            { Argument.name = None; value };
                            { Argument.name = None; value = annotation_expression };
                          ];
                          _;
                        };
                      ]);
                _;
              };
            } ->
              begin
                let annotation = parse_isinstance_annotation annotation_expression in
                let contradiction_error =
                  match annotation with
                  | Type.Top ->
                      let { resolved; _ } =
                        forward_expression ~state ~expression:annotation_expression
                      in
                      Some
                        (Error.create
                           ~location:(Node.location test)
                           ~kind:(Error.IncompatibleParameterType {
                               name = None;
                               position = 1;
                               callee = Some (Reference.create "isinstance");
                               mismatch = {
                                 Error.expected = Type.meta (Type.variable "T");
                                 actual = resolved;
                                 actual_expressions = [annotation_expression];
                                 due_to_invariance = false;
                               }
                             })
                           ~define:define_node)
                  | expected ->
                      let { resolved; _ } = forward_expression ~state ~expression:value in
                      if
                        Type.equal resolved Type.Bottom
                        || Type.is_unknown resolved
                        || not (Resolution.less_or_equal resolution ~left:resolved ~right:expected)
                      then
                        None
                      else
                        Some
                          (Error.create
                             ~location:(Node.location test)
                             ~kind:(Error.ImpossibleIsinstance {
                                 mismatch =
                                   (Error.create_mismatch
                                      ~resolution
                                      ~expected
                                      ~actual:resolved
                                      ~actual_expression:(Some value)
                                      ~covariant:true);
                                 expression = value;
                               })
                             ~define:define_node)
                in
                let resolve ~reference =
                  match Resolution.get_local resolution ~reference with
                  | Some {
                      Annotation.annotation = (Type.Optional (Type.Union parameters)) as unrefined;
                      _;
                    }
                  | Some { Annotation.annotation = (Type.Union parameters) as unrefined; _ } ->
                      let parameters =
                        match unrefined with
                        | Type.Optional _ ->
                            Type.none :: parameters
                        | _ ->
                            parameters
                      in
                      let parameters, constraints =
                        let is_not_list = function
                          | Type.Parametric { name = "list"; _ } -> false
                          | _ -> true
                        in
                        match annotation with
                        | Type.Union elements ->
                            parameters, elements
                        | Type.Parametric { name = "list"; parameters = [Type.Any] } ->
                            List.filter parameters ~f:is_not_list, []
                        | _ ->
                            parameters, [annotation]
                      in
                      let constrained =
                        Set.diff (Type.Set.of_list parameters) (Type.Set.of_list constraints)
                        |> Set.to_list
                        |> Type.union
                      in
                      Resolution.set_local
                        resolution
                        ~reference
                        ~annotation:(Annotation.create constrained)
                  | _ ->
                      resolution
                in
                match contradiction_error, value with
                | Some error, _ ->
                    emit_raw_error ~state:{ state with bottom = true } error
                | _, { Node.value = Access (Access.SimpleAccess access); _ } ->
                    { state with resolution = resolve ~reference:(Reference.from_access access) }
                | _ ->
                    state
              end

          | Access
              (Access.SimpleAccess [
                  Access.Identifier "all";
                  Access.Call {
                    Node.value = [
                      {
                        Argument.name = None;
                        value = { Node.value = Access (Access.SimpleAccess access); _ };
                      };
                    ];
                    _;
                  }
                ]) ->
              let resolution =
                let reference = Reference.from_access access in
                match Resolution.get_local resolution ~reference with
                | Some {
                    Annotation.annotation =
                      (Type.Parametric { name; parameters = [Type.Optional parameter] })
                      as annotation;
                    _
                  } when Resolution.less_or_equal
                      resolution
                      ~left:annotation
                      ~right:(Type.iterable (Type.Optional parameter)) ->
                    Resolution.set_local
                      resolution
                      ~reference
                      ~annotation:(
                        Annotation.create
                          (Type.Parametric {
                              name;
                              parameters = [parameter]
                            })
                      )
                | _ ->
                    resolution
              in
              { state with resolution }

          | Access (SimpleAccess access) ->
              let element = last_element ~resolution access in
              let reference = Reference.from_access access in
              let resolution =
                match Resolution.get_local resolution ~reference, element with
                | Some { Annotation.annotation = Type.Optional parameter; _ }, _ ->
                    Resolution.set_local
                      resolution
                      ~reference
                      ~annotation:(Annotation.create parameter)
                | _, Attribute { definition = Defined (Instance attribute); _ } ->
                    begin
                      match Annotated.Attribute.annotation attribute with
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
                              (Annotated.Attribute.annotation attribute)
                              parameter
                          in
                          Resolution.set_local
                            resolution
                            ~reference
                            ~annotation:refined
                      | _ ->
                          resolution
                    end
                | _ ->
                    resolution
              in
              { state with resolution }

          | BooleanOperator { BooleanOperator.left; operator; right } ->
              begin
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
              end

          | ComparisonOperator {
              ComparisonOperator.left;
              operator = ComparisonOperator.IsNot;
              right = { Node.value = Access (SimpleAccess [Access.Identifier "None"]); _ };
            } ->
              forward_statement ~state ~statement:(Statement.assume left)

          | ComparisonOperator {
              ComparisonOperator.left = { Node.value = Access (SimpleAccess access); _ };
              operator = ComparisonOperator.Is;
              right = { Node.value = Access (SimpleAccess [Access.Identifier "None"]); _ };
            } ->
              let reference = Reference.from_access access in
              begin
                let refined =
                  let element = last_element ~resolution access in
                  match element with
                  | Attribute { definition = Defined  (Instance attribute); _ } ->
                      Refinement.refine
                        ~resolution
                        (Annotated.Attribute.annotation attribute)
                        (Type.Optional Type.Bottom)
                  | _ ->
                      Annotation.create (Type.Optional Type.Bottom)
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
                      (* TODO: once T38750424 is done, we should really return bottom if
                         previous is not <= refined and refined is not <= previous, as
                         this is an obvious contradiction. *)
                      state
                | None ->
                    let resolution =
                      Resolution.set_local resolution ~reference ~annotation:refined
                    in
                    { state with resolution }
              end
          | ComparisonOperator {
              ComparisonOperator.left = { Node.value = Access (SimpleAccess access); _ };
              operator = ComparisonOperator.In;
              right;
            } ->
              let reference = Reference.from_access access in
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
                    else
                      (* Keeping previous state, since it is more refined. *)
                      state
                | None when not (Resolution.is_global resolution ~reference) ->
                    let resolution =
                      Resolution.set_local resolution ~reference ~annotation:refined
                    in
                    { state with resolution }
                | _ ->
                    state
              else
                state
          | _ ->
              state
        end

    | Expression { Node.value = Access (SimpleAccess access); _ }
      when Access.is_assert_function access ->
        let find_assert_test access =
          match access with
          | Expression.Record.Access.Call {
              Node.value = { Argument.value = test; _ } :: _;
              _;
            } -> Some test
          | _ -> None
        in
        List.find_map access ~f:find_assert_test
        >>| (fun assertion -> forward_statement ~state ~statement:(Statement.assume assertion))
        |> Option.value ~default:state

    | Delete expression ->
        (* TODO(T41338881): Actually remove bindings from resolution. *)
        let { state; _ } = forward_expression ~state ~expression in
        state

    | Expression expression ->
        forward_expression ~state ~expression
        |> fun { state; resolved } ->
        if Type.is_noreturn resolved then
          { state with bottom = true }
        else
          state

    | Global identifiers ->
        let resolution =
          let access = Access.create_from_identifiers identifiers in
          let annotation =
            let { resolved; _ } =
              forward_expression
                ~state
                ~expression:(Access.expression access)
            in
            Annotation.create_immutable resolved ~global:true
          in
          Resolution.set_local
            resolution
            ~reference:(Reference.from_access access)
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
          let add_import_error errors ~resolution:_ ~resolved:_ ~element ~lead:_ =
            match element with
            | AccessState.Attribute { definition = Undefined (Module _); _ } ->
                Error.create
                  ~location
                  ~kind:(Error.UndefinedImport (Reference.from_access import))
                  ~define:define_node
                :: errors
            | _ ->
                errors
          in
          forward_access ~f:add_import_error ~resolution ~initial:[] import
        in
        List.concat_map ~f:to_import_error imports
        |> List.fold ~init:state ~f:(fun state error -> emit_raw_error ~state error)

    | Raise (Some expression) ->
        forward_expression ~state ~expression
        |> fun { state; _ } -> state
    | Raise None ->
        state

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
          | None ->
              { state; resolved = Type.generator ~async Type.none }
        in
        validate_return ~expression:None ~state ~actual ~is_implicit:false

    | Statement.Yield _ ->
        state

    | YieldFrom { Node.value = Expression.Yield (Some return); _ } ->
        let { state; resolved } = forward_expression ~state ~expression:return in
        let actual =
          match Resolution.join resolution resolved (Type.iterator Type.Bottom) with
          | Type.Parametric { name = "typing.Iterator"; parameters = [parameter] } ->
              Type.generator parameter
          | annotation ->
              Type.generator annotation
        in
        validate_return ~expression:None ~state ~actual ~is_implicit:false

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


  let forward
      ?key
      ({
        resolution;
        resolution_fixpoint;
        nested_defines;
        bottom;
        configuration;
        _;
      } as state)
      ~statement:({ Node.location; _ } as statement) =
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
                Some {
                  nested with
                  initial = { nested_resolution = resolution; nested_bottom = bottom };
                }
            | None ->
                let ({ resolution = initial_resolution; _ }) =
                  initial
                    ~configuration
                    ~resolution
                    (Node.create define ~location)
                in
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
                Some {
                  nested = define;
                  initial = { nested_resolution; nested_bottom = false };
                }
          in
          Map.change ~f:update nested_defines location
        in
        match Node.value statement with
        | Class ({ Class.name; body; _ } as definition) ->
            let variables =
              (Node.create ~location definition)
              |> Annotated.Class.create
              |> Annotated.Class.generics ~resolution
            in
            schedule
              ~variables
              ~define:(Define.create_class_toplevel ~qualifier:name ~statements:body)
        | Define ({ Define.signature = { parameters; _ }; _ } as define)
          when not (Define.is_stub define) ->
            let variables =
              let extract_variables { Node.value = { Parameter.annotation; _ }; _ } =
                match annotation with
                | None -> []
                | Some annotation ->
                    let annotation = Resolution.parse_annotation resolution annotation in
                    Type.free_variables annotation
                    |> List.map ~f:(fun variable -> Type.Variable variable)
              in
              List.concat_map parameters ~f:extract_variables
              |> List.dedup_and_sort ~compare:Type.compare
            in
            schedule ~variables ~define
        | _ ->
            nested_defines
      in
      { state with nested_defines }
    in

    let state =
      let resolution_fixpoint =
        match key, state with
        | Some key, { resolution = post_resolution; _ } ->
            let precondition =
              Resolution.annotations resolution
              |> Reference.Map.to_tree
            in
            let postcondition =
              Resolution.annotations post_resolution
              |> Reference.Map.to_tree
            in
            Int.Map.Tree.set resolution_fixpoint ~key ~data:{ precondition; postcondition }
        | None, _ ->
            resolution_fixpoint
      in
      { state with resolution_fixpoint }
    in

    state


  let backward ?key:_ state ~statement:_ =
    state
end


module Fixpoint = Fixpoint.Make(State)


type result = {
  errors: Error.t list;
  coverage: Coverage.t;
}


let resolution (module Handler: Environment.Handler) ?(annotations = Reference.Map.empty) () =
  let aliases = Handler.aliases in

  let class_metadata annotation =
    let primitive, _ = Type.split annotation in
    Handler.class_metadata primitive
  in

  let class_definition annotation =
    let primitive, _ = Type.split annotation in
    Handler.class_definition primitive
  in

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
        ~constructor:(fun ~instantiated:_ ~resolution:_ _ -> Type.Top)
        ~implements:(fun  ~resolution:_ ~protocol:_ _ -> TypeOrder.DoesNotImplement)
        ~generics:(fun ~resolution:_ _ -> [])
        ()
    in
    {
      State.configuration = Configuration.Analysis.create ();
      errors = Error.Set.empty;
      define =
        Define.create_toplevel ~qualifier:None ~statements:[]
        |> Node.create_with_default_location;
      nested_defines = Location.Reference.Map.empty;
      bottom = false;
      resolution_fixpoint = Int.Map.Tree.empty;
      resolution = empty_resolution;
    }
  in
  let resolve ~resolution expression =
    let state = { state_without_resolution with State.resolution } in
    State.forward_expression ~state ~expression
    |> fun { State.resolved; _ } -> resolved
  in

  let constructor ~instantiated ~resolution class_node =
    AnnotatedClass.create class_node
    |> AnnotatedClass.constructor ~instantiated ~resolution
  in

  let implements ~resolution ~protocol annotation =
    let implements protocol =
      if AnnotatedClass.is_protocol protocol then
        match annotation with
        | Type.Callable callable ->
            AnnotatedClass.callable_implements ~resolution callable ~protocol
        | _ ->
            class_definition annotation
            >>| AnnotatedClass.create
            >>| AnnotatedClass.implements ~resolution ~protocol
            |> Option.value ~default:TypeOrder.DoesNotImplement
      else
        TypeOrder.DoesNotImplement
    in
    class_definition protocol
    >>| AnnotatedClass.create
    >>| implements
    |> Option.value ~default:TypeOrder.DoesNotImplement
  in

  let generics ~resolution class_definition =
    AnnotatedClass.create class_definition
    |>  AnnotatedClass.generics ~resolution
  in

  Resolution.create
    ~annotations
    ~order
    ~resolve
    ~aliases
    ~global:Handler.globals
    ~module_definition:Handler.module_definition
    ~class_definition
    ~class_metadata
    ~constructor
    ~implements
    ~generics
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
    | _ ->
        Reference.Map.empty
  in
  resolution environment ~annotations ()
  |> Resolution.with_parent ~parent


let name =
  "TypeCheck"


let run
    ~configuration
    ~environment
    ~source:({
        Source.handle;
        qualifier;
        statements;
        metadata = { Source.Metadata.local_mode; debug; version; number_of_lines; _ };
        _;
      } as source) =
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
    Configuration.Analysis.localize
      configuration
      ~local_debug:debug
      ~local_strict
      ~declare
  in

  ResolutionSharedMemory.Keys.LocalChanges.push_stack ();
  let check
      ~define:{ Node.value = ({ Define.signature = { name; parent; _ }; _ } as define); _ }
      ~initial
      ~queue =
    Log.log ~section:`Check "Checking %a" Reference.pp name;
    let dump = Define.dump define in

    if dump then
      begin
        Log.dump
          "Checking `%s`..."
          (Log.Color.yellow (Reference.show name));
        Log.dump "AST:\n%a" Define.pp define;
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
              label ^ "\n" ^ Reference.show key ^ ": " ^ annotation_string
            in
            Map.fold ~f:stringify ~init:"" (Resolution.annotations resolution)
        | None -> ""
      in
      if Define.dump_cfg define then
        begin
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
        end
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
    exit
    >>| dump_resolutions
    |> ignore;

    (* Schedule nested functions for analysis. *)
    exit
    >>| State.nested_defines
    >>| List.iter ~f:(Queue.enqueue queue)
    |> ignore;

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
    { errors; coverage }
  in

  let results =
    let queue =
      let queue = Queue.create () in
      if not (version < 3) then
        begin
          let toplevel =
            let location =
              {
                Location.path = File.Handle.show handle;
                start = { Location.line = 1; column = 1 };
                stop = { Location.line = 1; column = 1 };
              }
              |> Location.reference
            in
            Define.create_toplevel ~qualifier:(Some qualifier) ~statements
            |> Node.create ~location
          in
          Queue.enqueue queue (toplevel, resolution)
        end;
      queue
    in
    let rec results ~queue =
      match Queue.dequeue queue with
      | Some (
          ({
            Node.location;
            value = ({ Define.signature = { name; _ }; _ } as define) } as define_node),
          resolution
        ) ->
          let result =
            try
              let initial =
                State.initial
                  ~configuration
                  ~resolution
                  define_node
              in
              check ~define:define_node ~initial ~queue
            with
            | TypeOrder.Untracked annotation ->
                Statistics.event
                  ~name:"undefined type"
                  ~integers:[]
                  ~normals:[
                    "handle", (File.Handle.show handle);
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
                    ~location
                    ~kind:(Error.AnalysisFailure annotation)
                    ~define:define_node;
                in
                {
                  errors = [undefined_error];
                  coverage = Coverage.create ~crashes:1 ();
                }
          in
          result :: results ~queue
      | _ ->
          []
    in
    results ~queue
  in

  (* These local changes allow us to add keys incrementally in a worker process without
     worrying about removing (which can only be done by a master. *)
  ResolutionSharedMemory.Keys.LocalChanges.commit_all ();
  ResolutionSharedMemory.Keys.LocalChanges.pop_stack ();
  let errors =
    let filter errors =
      if configuration.debug then
        errors
      else
        let keep_error error =
          if Location.Instantiated.equal (Error.location error) Location.Instantiated.synthetic then
            false
          else
            let mode =
              let (module Handler: Environment.Handler) = environment in
              Handler.local_mode
                (Error.path error
                 |> File.Handle.create)
              |> (fun local_mode -> Ast.Source.mode ~configuration ~local_mode)
            in
            not (Error.suppress ~mode ~resolution error)
        in
        List.filter ~f:keep_error errors
    in
    List.map results ~f:(fun { errors; _ } -> errors )
    |> List.map ~f:filter
    |> List.concat
    |> Error.join_at_source ~resolution
    |> List.map ~f:(Error.dequalify (Preprocessing.dequalify_map source) ~resolution)
    |> List.sort ~compare:Error.compare
  in

  let coverage =
    List.map results ~f:(fun { coverage; _ } -> coverage)
    |> Coverage.aggregate_over_source ~source
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
