(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre

open Ast
open Expression
open Statement

module Error = AnalysisError


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
    errors: Error.t Location.Reference.Map.t;
    define: Define.t Node.t;
    nested_defines: nested_define Location.Reference.Map.t;
    bottom: bool;
    resolution_fixpoint: ResolutionSharedMemory.annotation_map Int.Map.Tree.t
  }


  let pp_nested_define format { nested = { Define.name; _ }; _ } =
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
          (Location.instantiate ~lookup:(fun hash -> Ast.SharedMemory.Handles.get ~hash) location)
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
      errors = Location.Reference.Map.empty;
      define;
      nested_defines = Location.Reference.Map.empty;
      bottom;
      resolution_fixpoint;
    }


  let errors
      {
        configuration;
        resolution;
        errors;
        define = ({
            Node.location;
            value = { Define.name; return_annotation; body; _ } as define;
          } as define_node);
        _;
      } =
    let constructor_errors errors =
      if not (Statement.Define.is_constructor define) ||
         String.is_suffix (Access.show name) ~suffix:"__enter__" then  (* Yikes... *)
        errors
      else
        (* Return errors. *)
        let errors =
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
        in
        (* Create missing attribute errors. *)
        match Annotated.Define.parent_definition ~resolution (Annotated.Define.create define) with
        | Some class_definition ->
            let annotation = AnnotatedClass.annotation ~resolution class_definition in
            let untyped_assignment_error errors { Node.location; value } =
              match value with
              | Assign {
                  Assign.annotation = None;
                  target = {
                    Node.value = Access ((Expression.Access.Identifier self) :: ([_] as access));
                    _;
                  };
                  _;
                } when Identifier.equal self (Statement.Define.self_identifier define) ->
                  let attribute_annotation =
                    Annotated.Class.attribute
                      class_definition
                      ~resolution
                      ~name:access
                      ~instantiated:annotation
                    |> Annotated.Attribute.annotation
                    |> Annotation.annotation
                  in
                  if Type.equal attribute_annotation Type.Object then
                    Error.create
                      ~location
                      ~kind:(Error.MissingAttributeAnnotation {
                          parent = annotation;
                          missing_annotation = {
                            Error.name = access;
                            annotation = None;
                            evidence_locations = [];
                            due_to_any = false;
                          };
                        })
                      ~define:define_node
                    :: errors
                  else
                    errors
              | _ ->
                  errors
            in
            List.fold body ~f:untyped_assignment_error ~init:errors
        | None ->
            errors
    in
    let class_initialization_errors errors =
      (* Ensure non-nullable typed attributes are instantiated in init. *)
      let check_attributes_initialized define =
        let open Annotated in
        (Define.parent_definition ~resolution (Define.create define)
         >>| fun definition ->
         let propagate_initialization_errors errors attribute =
           let expected = Annotation.annotation (Attribute.annotation attribute) in
           match Attribute.name attribute with
           | Access name
             when not (Type.equal expected Type.Top ||
                       Type.is_optional expected ||
                       Attribute.initialized attribute) ->
               let access =
                 (Expression.Access.Identifier (Statement.Define.self_identifier define)) :: name
               in
               if Map.mem (Resolution.annotations resolution) access &&
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
              List.rev name
              |> List.tl
              |> Option.value ~default:[]
              |> List.rev
              |> Access.show
            in
            Resolution.class_definition resolution (Type.primitive name)
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
    Map.data errors
    |> Error.join_at_define
      ~resolution
      ~location:(
        Location.instantiate
          location
          ~lookup:(fun hash -> Ast.SharedMemory.Handles.get ~hash))
    |> class_initialization_errors
    |> constructor_errors
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
      Map.fold
        ~init:true
        ~f:(entry_less_or_equal right.errors (Error.less_or_equal ~resolution))
        left.errors &&
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
        errors = Map.merge ~f:merge_errors left.errors right.errors;
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
      let errors =
        let merge ~key:_ = function
          | `Both (left, right) ->
              Some (Error.join ~resolution left right)
          | `Left state
          | `Right state ->
              Some state
        in
        Map.merge left.errors right.errors ~f:merge;
      in
      { left with errors; resolution = Resolution.with_annotations resolution ~annotations }


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
      let join_resolution_fixpoints ~key:_ = function
        | `Left next_resolution
        | `Right next_resolution
        | `Both (_, next_resolution) -> Some next_resolution
      in
      let resolution_fixpoint =
        Int.Map.Tree.merge
          ~f:join_resolution_fixpoints
          previous.resolution_fixpoint
          next.resolution_fixpoint
      in
      {
        previous with
        errors = Map.merge ~f:widen_errors previous.errors next.errors;
        nested_defines =
          Map.merge ~f:join_nested_defines previous.nested_defines next.nested_defines;
        resolution = Resolution.with_annotations resolution ~annotations;
        resolution_fixpoint
      }


  let add_error ~state:({ errors; _ } as state) error =
    {
      state with
      errors = Map.set errors ~key:(Location.reference (Error.location error)) ~data:error;
    }


  let check_annotation ~resolution ~location ~define ~annotation =
    let check_untracked_annotation errors annotation =
      if Resolution.is_tracked resolution annotation then
        errors
      else
        Error.create ~location ~kind:(Error.UndefinedType annotation) ~define :: errors
    in
    let check_missing_type_parameters errors annotation =
      match annotation with
      | Type.Primitive _ ->
          let generics =
            Resolution.class_definition resolution annotation
            >>| Annotated.Class.create
            >>| Annotated.Class.generics ~resolution
            |> Option.value ~default:[]
          in
          if not (List.is_empty generics) then
            let error =
              Error.create
                ~location
                ~kind:(Error.MissingTypeParameters {
                    annotation;
                    number_of_parameters = List.length generics;
                  })
                ~define
            in
            error :: errors
          else
            errors
      | _ ->
          errors
    in
    let primitives = Type.primitives annotation in
    let primitives_and_parametric_base =
      match Type.split annotation with
      | _, [] -> primitives
      | primitive, _ -> primitive :: primitives
    in
    List.fold ~init:[] ~f:check_untracked_annotation primitives_and_parametric_base
    |> (fun errors -> List.fold primitives ~f:check_missing_type_parameters ~init:errors)


  type resolved = {
    state: t;
    resolved: Type.t;
  }


  let rec initial
      ?(configuration = Configuration.Analysis.create ())
      ~resolution
      ({
        Node.location;
        value = ({ Define.name; parent; parameters; return_annotation; _ } as define);
      } as define_node) =
    let resolution = Resolution.with_parent resolution ~parent in
    let { errors; resolution; resolution_fixpoint; _ } as initial =
      create ~configuration ~resolution ~define:define_node ()
    in
    (* Check return annotation. *)
    let errors =
      match return_annotation with
      | Some ({ Node.location; _ } as annotation) ->
          let annotation = Resolution.parse_annotation resolution annotation in
          check_annotation ~resolution ~location ~define:define_node ~annotation
          |> List.fold
            ~init:errors
            ~f:(fun errors error -> Map.set ~key:location ~data:error errors)
      | None ->
          errors
    in
    (* Check parameters. *)
    let annotations, errors =
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
          let add_incompatible_variable_error errors annotation default =
            if Type.equal default Type.Object ||
               Resolution.less_or_equal resolution ~left:default ~right:annotation then
              errors
            else
              let error =
                let instantiate location =
                  Location.instantiate
                    ~lookup:(fun hash -> Ast.SharedMemory.Handles.get ~hash)
                    location
                in
                Error.create
                  ~location
                  ~kind:(Error.IncompatibleVariableType {
                      name = [Expression.Access.Identifier name];
                      mismatch =
                        Error.create_mismatch
                          ~resolution
                          ~expected:annotation
                          ~actual:default
                          ~covariant:true;
                      declare_location = instantiate location;
                    })
                  ~define:define_node
              in
              Map.set ~key:location ~data:error errors
          in
          match index, parent with
          | 0, Some parent
            when Define.is_method define &&
                 not (Define.is_static_method define) ->
              let resolved =
                let parent_annotation =
                  Access.expression parent
                  |> Resolution.parse_annotation resolution
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
                  Type.meta parent_annotation
                else
                  (* First parameter of a method is the callee object. *)
                  parent_annotation
              in
              begin
                match annotation with
                | Some annotation ->
                    let annotation = Resolution.parse_annotation resolution annotation in
                    let errors =
                      check_annotation ~resolution ~location ~define:define_node ~annotation
                      |> List.fold
                        ~init:errors
                        ~f:(fun errors error -> Map.set ~key:location ~data:error errors)
                    in
                    let errors =
                      add_incompatible_variable_error errors annotation resolved
                    in
                    Annotation.create resolved, errors
                | None ->
                    Annotation.create resolved, errors
              end
          | _ ->
              let add_missing_parameter_error ~annotation ~due_to_any errors =
                let error =
                  let sanitized_access =
                    name
                    |> Identifier.sanitized
                    |> fun name -> [Access.Identifier name]
                  in
                  Error.create
                    ~location
                    ~kind:(Error.MissingParameterAnnotation {
                        name = sanitized_access;
                        annotation;
                        due_to_any;
                      })
                    ~define:define_node
                in
                Map.set ~key:location ~data:error errors
              in
              match annotation, value with
              | Some annotation, Some value
                when Type.equal (Resolution.parse_annotation resolution annotation) Type.Object ->
                  let { resolved = annotation; _ } =
                    forward_expression ~state:initial ~expression:value
                  in
                  Annotation.create annotation,
                  add_missing_parameter_error ~annotation ~due_to_any:true errors
              | Some annotation, None
                when Type.equal (Resolution.parse_annotation resolution annotation) Type.Object ->
                  let annotation =
                    Resolution.parse_annotation resolution annotation
                    |> Annotation.create_immutable ~global:false
                  in
                  annotation,
                  add_missing_parameter_error ~annotation:Type.Bottom ~due_to_any:true errors
              | Some annotation, value ->
                  let annotation = Resolution.parse_annotation resolution annotation in
                  let errors =
                    check_annotation ~resolution ~location ~define:define_node ~annotation
                    |> List.fold
                      ~init:errors
                      ~f:(fun errors error -> Map.set ~key:location ~data:error errors)
                  in
                  let errors =
                    value
                    >>| (fun value -> forward_expression ~state:initial ~expression:value)
                    >>| (fun {resolved; _ } -> resolved)
                    >>| add_incompatible_variable_error errors annotation
                    |> Option.value ~default:errors
                  in
                  let annotation =
                    match annotation with
                    | Type.Variable { constraints = Type.Explicit constraints; _ } ->
                        Type.union constraints
                    | _ ->
                        annotation
                  in
                  Annotation.create_immutable ~global:false annotation, errors
              | None, Some value ->
                  let { resolved = annotation; _ } =
                    forward_expression ~state:initial ~expression:value
                  in
                  Annotation.create annotation,
                  add_missing_parameter_error ~annotation ~due_to_any:false errors
              | None, None ->
                  Annotation.create Type.Bottom,
                  add_missing_parameter_error ~annotation:Type.Bottom ~due_to_any:false errors
        in
        let annotation =
          if String.is_prefix ~prefix:"**" (Identifier.show name) then
            Type.dictionary ~key:Type.string ~value:annotation
          else if String.is_prefix ~prefix:"*" (Identifier.show name) then
            Type.Tuple (Type.Unbounded annotation)
          else
            annotation
        in
        Map.set annotations ~key:access ~data:{ Annotation.annotation; mutability },
        errors
      in
      List.foldi ~init:((Resolution.annotations resolution), errors) ~f:parameter parameters
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
                 name
                 |> Statement.Access.expression
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
                             |> Expression.Access.create;
                           override =
                             Error.WeakenedPostcondition
                               (Error.create_mismatch
                                  ~resolution
                                  ~actual
                                  ~expected
                                  ~covariant:false)
                         })
                       ~define:define_node
                   in
                   Map.set ~key:location ~data:error errors
                 else
                   errors
               in
               (* Check weakening of precondition. *)
               let overriding_parameters =
                 let remove_unused_parameter_denotation ~key ~data map =
                   Identifier.Map.set map ~key:(Identifier.remove_leading_underscores key) ~data
                 in
                 Method.create ~define ~parent:(Annotated.Class.annotation definition ~resolution)
                 |> Method.parameter_annotations ~resolution
                 |> Map.fold ~init:Identifier.Map.empty ~f:remove_unused_parameter_denotation
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
                       try
                         if not (Type.equal Type.Top expected) &&
                            not (Resolution.less_or_equal
                                   resolution
                                   ~left:expected
                                   ~right:actual) then
                           let error =
                             Error.create
                               ~location
                               ~kind:(Error.InconsistentOverride {
                                   overridden_method = Statement.Define.unqualified_name define;
                                   parent =
                                     Attribute.parent overridden_attribute
                                     |> Type.show
                                     |> Expression.Access.create;
                                   override =
                                     Error.StrengthenedPrecondition
                                       (Error.Found
                                          (Error.create_mismatch
                                             ~resolution
                                             ~actual
                                             ~expected
                                             ~covariant:false));
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
                         Map.fold ~f:collect_starred_parameters ~init:[] overriding_parameters
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
                         Identifier.show_sanitized name
                         |> Identifier.create
                         |> fun name -> [Expression.Access.Identifier name]
                       in
                       let error =
                         Error.create
                           ~location
                           ~kind:(Error.InconsistentOverride {
                               overridden_method = Statement.Define.unqualified_name define;
                               parent =
                                 Attribute.parent overridden_attribute
                                 |> Type.show
                                 |> Expression.Access.create;
                               override =
                                 Error.StrengthenedPrecondition (Error.NotFound parameter_name);
                             })
                           ~define:define_node
                       in
                       Map.set ~key:location ~data:error errors
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

    let resolution = Resolution.with_annotations resolution ~annotations in
    let resolution_fixpoint =
      let precondition = Access.Map.Tree.empty in
      let postcondition =
        Resolution.annotations resolution
        |> Access.Map.to_tree
      in
      let key = ([%hash: int * int] (Cfg.entry_index, 0)) in
      Int.Map.Tree.set resolution_fixpoint ~key ~data:{ precondition; postcondition }
    in
    { initial with errors; resolution; resolution_fixpoint }


  and forward_expression
      ~state:({ resolution; define; _ } as state)
      ~expression:{ Node.location; value } =
    let rec forward_entry ~state ~entry:{ Dictionary.key; value } =
      let { state; resolved = key_resolved } = forward_expression ~state ~expression:key in
      let { state; resolved = value_resolved } = forward_expression ~state ~expression:value in
      key_resolved, value_resolved, state
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
            Access (Expression.access iterator @ [
                Access.Identifier (Identifier.create "__aiter__");
                Access.Call (Node.create ~location []);
                Access.Identifier (Identifier.create "__anext__");
                Access.Call (Node.create ~location []);
              ])
            |> Node.create ~location
            |> (fun target -> Await target)
            |> Node.create ~location
          else
            Access (Expression.access iterator @ [
                Access.Identifier (Identifier.create "__iter__");
                Access.Call (Node.create ~location []);
                Access.Identifier (Identifier.create "__next__");
                Access.Call (Node.create ~location []);
              ])
            |> Node.create ~location
        in
        Assign { Assign.target; annotation = None; value; parent = None }
        |> Node.create ~location
      in
      let state = forward_statement ~state ~statement:iterator in
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
      { state = { state with resolution }; resolved }
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
                  Type.Object
            in
            { state; resolved = Resolution.join resolution resolved parameter }
        | _ ->
            let { state; resolved = new_resolved } = forward_expression ~state ~expression in
            { state; resolved = Resolution.join resolution resolved new_resolved }
      in
      List.fold elements ~init:{ state; resolved = Type.Bottom } ~f:forward_element
    in
    let join_resolved ~resolution left right =
      {
        state = join left.state right.state;
        resolved = Resolution.join resolution left.resolved right.resolved;
      }
    in
    match value with
    | Access [
        Expression.Access.Identifier reveal_type;
        Expression.Access.Call {
          Node.location;
          value = [{ Expression.Argument.value; _ }] };
      ] when reveal_type = Identifier.create "reveal_type" ->
        (* Special case reveal_type(). *)
        let { state; resolved = annotation } = forward_expression ~state ~expression:value in
        let state =
          Error.create
            ~location
            ~kind:(Error.RevealedType { expression = value; annotation })
            ~define
          |> add_error ~state
        in
        { state; resolved = Type.none }
    | Access [
        Expression.Access.Identifier typing;
        Expression.Access.Identifier cast;
        Expression.Access.Call {
          Node.value = [
            { Expression.Argument.value = cast_annotation; _ };
            { Expression.Argument.value; _ };
          ];
          location;
        }
      ] when Identifier.equal typing (Identifier.create "typing") &&
             Identifier.equal cast (Identifier.create "cast") ->
        let cast_annotation = Resolution.parse_annotation resolution cast_annotation in
        let state =
          check_annotation ~resolution ~location ~define ~annotation:cast_annotation
          |> List.fold ~init:state ~f:(fun state error -> add_error ~state error)
        in
        let { resolved; _ } = forward_expression ~state ~expression:value in
        let state =
          if Type.equal cast_annotation resolved then
            Error.create
              ~location
              ~kind:(Error.RedundantCast resolved)
              ~define
            |> add_error ~state
          else
            state
        in
        { state; resolved = cast_annotation }
    | Access [
        Access.Identifier isinstance;
        Access.Call {
          Node.value = [
            { Argument.value = expression; _ };
            { Argument.value = annotations; _ };
          ];
          _;
        }] when Identifier.show isinstance = "isinstance" ->
        (* We special case type inference for `isinstance` in asserted, and the typeshed stubs are
           imprecise (doesn't correctly declare the arguments as a recursive tuple. *)
        let state =
          let { state; _ } = forward_expression ~state ~expression in
          let previous_errors = Map.length state.errors in
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
          if Map.length state.errors > previous_errors then
            state
          else
            let add_incompatible_non_meta_error state (non_meta, location) =
              Error.create
                ~location
                ~kind:(Error.IncompatibleParameterType {
                    name = None;
                    position = 2;
                    callee = Some [Access.Identifier isinstance];
                    mismatch = {
                      Error.actual = non_meta;
                      expected = Type.meta Type.Object;
                      due_to_invariance = false;
                    }})
                ~define
              |> add_error ~state
            in
            List.find annotations ~f:(fun (annotation, _) -> not (Type.is_meta annotation))
            >>| add_incompatible_non_meta_error state
            |> Option.value ~default:state
        in
        { state; resolved = Type.bool }
    | Access access ->
        (* Walk through the access. *)
        let _, state, resolved =
          let open Annotated in
          let open Access in
          let forward_access (found_error, state, _) ~resolution ~resolved ~element ~lead =
            let state = { state with resolution } in
            if found_error then
              found_error, state, Annotation.annotation resolved
            else if
              Type.exists
                (Annotation.annotation resolved)
                ~predicate:(fun annotation -> Type.equal annotation Type.undeclared) then
              let state =
                Error.UndefinedName lead
                |> (fun kind -> Error.create ~location ~kind ~define)
                |> add_error ~state
              in
              true, state, Annotation.annotation resolved
            else
              let error =
                match element with
                | Signature {
                    signature =
                      (Signature.NotFound {
                          callable = { Type.Callable.kind; _ };
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
                            Error.create_mismatch ~resolution ~actual ~expected ~covariant:true,
                            name,
                            position,
                            (Node.location mismatch)
                          in
                          Error.create
                            ~location
                            ~kind:
                              (Error.IncompatibleParameterType {
                                  name =
                                    (name
                                     >>| fun name ->
                                     Expression.Access.create_from_identifiers [name]);
                                  position;
                                  callee;
                                  mismatch;
                                })
                            ~define
                      | MissingArgument name ->
                          Error.create
                            ~location
                            ~kind:(Error.MissingArgument { callee; name })
                            ~define
                      | TooManyArguments { expected; provided } ->
                          Error.create
                            ~location
                            ~kind:(Error.TooManyArguments { callee; expected; provided })
                            ~define
                      | TypedDictionaryAccessWithNonLiteral expression ->
                          Error.create
                            ~location
                            ~kind:(Error.TypedDictionaryAccessWithNonLiteral expression)
                            ~define
                      | TypedDictionaryMissingKey { typed_dictionary_name; missing_key } ->
                          Error.create
                            ~location
                            ~kind:(Error.TypedDictionaryKeyNotFound {
                                typed_dictionary_name;
                                missing_key;
                              })
                            ~define
                      | UnexpectedKeyword name ->
                          Error.create
                            ~location
                            ~kind:(Error.UnexpectedKeyword { callee; name })
                            ~define
                    in
                    Some error

                | Attribute { attribute; origin; defined } when not defined ->
                    let open Annotated in
                    if Location.Reference.equal location Location.Reference.any then
                      begin
                        Statistics.event
                          ~name:"undefined attribute without location"
                          ~normals:["attribute", (Expression.Access.show attribute)]
                          ();
                        None
                      end
                    else
                      let kind =
                        match origin with
                        | Instance class_attribute ->
                            let annotation = Attribute.parent class_attribute in
                            if Type.equal annotation Type.undeclared then
                              Error.UndefinedName lead
                            else
                              Error.UndefinedAttribute {
                                attribute;
                                origin =
                                  Error.Class {
                                    annotation;
                                    class_attribute = Attribute.class_attribute class_attribute;
                                  };
                              }
                        | Module access when not (List.is_empty access) ->
                            Error.UndefinedAttribute {
                              attribute;
                              origin = Error.Module access;
                            }
                        | Module _ ->
                            Error.UndefinedName attribute
                      in
                      Some (Error.create ~location ~kind ~define)
                | NotCallable Type.Object ->
                    None
                | NotCallable annotation->
                    let kind = Error.NotCallable annotation in
                    Some (Error.create ~location ~kind ~define)
                | _ ->
                    None
              in
              Option.is_some error,
              error >>| add_error ~state |> Option.value ~default:state,
              Annotation.annotation resolved
          in
          Access.fold
            (Access.create access)
            ~resolution
            ~initial:(false, state, Type.Top)
            ~f:forward_access
        in

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
          | Access.Expression expression ->
              forward_expression ~state ~expression
              |> fun { state; _ } -> state
        in
        let state = List.fold access ~f:forward_access ~init:state in

        { state; resolved }

    | Await expression ->
        let { state; resolved } = forward_expression ~state ~expression in
        let state =
          let is_awaitable =
            Resolution.less_or_equal
              resolution
              ~left:resolved
              ~right:(Type.awaitable Type.Object)
          in
          if not is_awaitable then
            Error.create
              ~location
              ~kind:(Error.IncompatibleAwaitableType resolved)
              ~define
            |> add_error ~state
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
        let state =
          match operator with
          | BooleanOperator.And -> meet state_left state_right;
          | BooleanOperator.Or -> join state_left state_right;
        in
        let resolved =
          match resolved_left, resolved_right, operator with
          | Optional resolved_left, resolved_right, BooleanOperator.Or ->
              Resolution.join resolution resolved_left resolved_right
          | Optional _, resolved_right, BooleanOperator.And ->
              Type.optional resolved_right
          | resolved_left, resolved_right, _ ->
              Resolution.join resolution resolved_left resolved_right
        in
        { state; resolved }

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
              >>| Annotated.Class.has_method ~transitive:true ~resolution ~name:(Access.create name)
              |> Option.value ~default:false
        in
        let converted_call =
          let { Node.location; _ } = left in
          if has_method "__contains__" iterator then
            let arguments = [{ Argument.name = None; value = left }] in
            Access ((access right) @ Access.call ~arguments ~location ~name:"__contains__" ())
          else if has_method "__iter__" iterator then
            Access (
              (access right) @
              Access.call ~location ~name:"__iter__" () @
              Access.call ~location ~name:"__next__" () @
              Access.call
                ~arguments:[{ Argument.name = None; value = left }]
                ~location
                ~name:"__eq__"
                ()
            )
          else
            Access (
              access right @
              Access.call
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
                ()
            )
        in
        converted_call
        |> Node.create ~location
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

    | Ellipses ->
        { state; resolved = Type.ellipses }

    | False ->
        { state; resolved = Type.bool }

    | Float _ ->
        { state; resolved = Type.float }

    | Generator { Comprehension.element; generators } ->
        let { state; resolved } = forward_comprehension ~element ~generators in
        { state; resolved = Type.generator resolved }

    | Integer _ ->
        { state; resolved = Type.integer }

    | Lambda { Lambda.body; parameters } ->
        let resolution_with_parameters =
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
        let { state; resolved } =
          forward_expression
            ~state:{ state with resolution = resolution_with_parameters }
            ~expression:body
        in
        let create_parameter { Node.value = { Parameter.name; _ }; _ } =
          Type.Callable.Parameter.Named {
            Type.Callable.Parameter.name = Access.create (Identifier.show name);
            annotation = Type.Object;
            default = false;
          }
        in
        let parameters =
          List.map parameters ~f:create_parameter
          |> fun parameters -> Type.Callable.Defined parameters
        in
        {
          state = { state with resolution };
          resolved = Type.callable ~parameters ~annotation:resolved ();
        }

    | List elements ->
        let { state; resolved } = forward_elements ~state ~elements in
        { state; resolved = Type.list resolved }

    | ListComprehension { Comprehension.element; generators } ->
        let { state; resolved } = forward_comprehension ~element ~generators in
        { state; resolved = Type.list resolved }

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

    | String { StringLiteral.kind = StringLiteral.String; _ } ->
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
        join_resolved ~resolution target alternative

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
              Node.value = { Define.async; _ } as define_without_location;
              _;
            } as define);
          _;
        } as state)
      ~statement:{ Node.location; value } =
    (* We weaken type inference of mutable literals for assignments and returns
       to get around the invariance of containers when we can prove that casting to
       a supertype is safe. *)
    let resolve_mutable_literals resolution ~expression ~resolved ~expected =
      match expression with
      | Some { Node.value = Expression.List _; _ }
      | Some { Node.value = Expression.ListComprehension _; _ } ->
          begin
            match resolved, expected with
            | Type.Parametric { name = actual_name; parameters = [actual] },
              Type.Parametric { name = expected_name; parameters = [expected_parameter] }
              when Identifier.equal actual_name (Identifier.create "list") &&
                   Identifier.equal expected_name (Identifier.create "list") &&
                   Resolution.less_or_equal resolution ~left:actual ~right:expected_parameter ->
                expected
            | _ ->
                resolved
          end

      | Some { Node.value = Expression.Set _; _ }
      | Some { Node.value = Expression.SetComprehension _; _ } ->
          begin
            match resolved, expected with
            | Type.Parametric { name = actual_name; parameters = [actual] },
              Type.Parametric { name = expected_name; parameters = [expected_parameter] }
              when Identifier.equal actual_name (Identifier.create "set") &&
                   Identifier.equal expected_name (Identifier.create "set") &&
                   Resolution.less_or_equal resolution ~left:actual ~right:expected_parameter ->
                expected
            | _ ->
                resolved
          end

      | Some { Node.value = Expression.Dictionary _; _ }
      | Some { Node.value = Expression.DictionaryComprehension _; _ } ->
          begin
            match resolved, expected with
            | Type.Parametric { name = actual_name; parameters = [actual_key; actual_value] },
              Type.Parametric {
                name = expected_name;
                parameters = [expected_key; expected_value];
              }
              when Identifier.equal actual_name (Identifier.create "dict") &&
                   Identifier.equal expected_name (Identifier.create "dict") &&
                   Resolution.less_or_equal resolution ~left:actual_key ~right:expected_key &&
                   Resolution.less_or_equal
                     resolution
                     ~left:actual_value
                     ~right:expected_value ->
                expected
            | _ ->
                resolved
          end

      | _ ->
          resolved
    in
    let validate_return ~expression ~state ~actual ~is_implicit =
      let return_annotation =
        let annotation =
          Annotated.Callable.return_annotation ~define:define_without_location ~resolution
        in
        if async then
          Type.awaitable_value annotation
        else
          annotation
      in
      let actual =
        resolve_mutable_literals resolution ~expression ~resolved:actual ~expected:return_annotation
      in
      try
        if not (Resolution.less_or_equal resolution ~left:actual ~right:return_annotation) &&
           not (Define.is_abstract_method define_without_location) &&
           not (Define.is_overloaded_method define_without_location) &&
           not (Type.is_none actual &&
                (Annotated.Define.create define_without_location
                 |> Annotated.Define.is_generator)) &&
           not (Type.is_none actual && Type.is_noreturn return_annotation) then
          let error =
            Error.create
              ~location
              ~kind:(Error.IncompatibleReturnType
                       {
                         mismatch =
                           (Error.create_mismatch
                              ~resolution
                              ~actual
                              ~expected:return_annotation
                              ~covariant:true);
                         is_implicit;
                       })
              ~define
          in
          add_error ~state error
        else if Type.equal return_annotation Type.Top ||
                Type.equal return_annotation Type.Object then
          let error =
            Error.create
              ~location
              ~kind:(Error.MissingReturnAnnotation {
                  annotation = actual;
                  evidence_locations = [location.Location.start.Location.line];
                  due_to_any = Type.equal return_annotation Type.Object;
                })
              ~define
          in
          add_error ~state error
        else
          state
      with TypeOrder.Untracked _ ->
        state
    in
    let instantiate location =
      Location.instantiate ~lookup:(fun hash -> Ast.SharedMemory.Handles.get ~hash) location
    in
    match value with
    | Assign { Assign.target; annotation; value; _ } ->
        let state =
          annotation
          >>| Resolution.parse_annotation resolution
          >>| (fun annotation -> check_annotation ~resolution ~location ~define ~annotation)
          >>| List.fold ~init:state ~f:(fun state error -> add_error ~state error)
          |> Option.value ~default:state
        in
        let { state = { resolution; _ } as state; resolved } =
          forward_expression ~state ~expression:value
        in
        let guide =
          (* This is the annotation determining how we recursively break up the assignment. *)
          annotation
          >>| Resolution.parse_annotation resolution
          |> Option.value ~default:resolved
        in
        let explicit = Option.is_some annotation in
        let rec forward_assign
            ~state:({ resolution; errors; _ } as state)
            ~target:{ Node.location; value }
            ~guide
            ~resolved
            ~expression =

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
                  ~right:(Type.iterable Type.Top) ||
                Resolution.less_or_equal
                  resolution
                  ~left:annotation
                  ~right:Type.named_tuple
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
            (* TODO(32692300): this should support tuple subclasses (e.g. named tuples) as well. *)
            match annotation with
            | Type.Tuple (Type.Bounded parameters)
              when minimum_length <= List.length parameters ->
                true
            | _ ->
                false
          in
          let nonuniform_sequence_parameters ~minimum_length annotation =
            match annotation with
            | Type.Tuple (Type.Bounded parameters)
              when minimum_length <= List.length parameters ->
                parameters
            | _ ->
                let rec parameters = function
                  | count when count > 0 -> Type.Top :: (parameters (count - 1))
                  | _ -> []
                in
                parameters minimum_length
          in

          match value, guide with
          | Access access, guide ->
              let annotation, element =
                let annotation, element =
                  let open Annotated in
                  let fold _ ~resolution:_ ~resolved ~element ~lead:_ =
                    resolved, element
                  in
                  Access.create access
                  |> Access.fold
                    ~f:fold
                    ~resolution
                    ~initial:(Annotation.create Type.Top, Access.Value)
                in
                if element = Annotated.Access.Value && explicit then
                  Annotation.create_immutable ~global:false guide, element
                else
                  annotation, element
              in
              let expected = Annotation.original annotation in
              let resolved = resolve_mutable_literals resolution ~expression ~resolved ~expected in
              (* Check if assignment is valid. *)
              let state =
                let error =
                  if not explicit &&
                     not (Type.equal resolved Type.ellipses) &&
                     Annotation.is_immutable annotation &&
                     not (Resolution.less_or_equal resolution ~left:resolved ~right:expected) then
                    let kind =
                      let open Annotated in
                      let open Access in
                      match element with
                      | Attribute { attribute = access; origin = Instance attribute; _ } ->
                          Error.IncompatibleAttributeType {
                            parent = Attribute.parent attribute;
                            incompatible_type = {
                              Error.name = access;
                              mismatch =
                                (Error.create_mismatch
                                   ~resolution
                                   ~actual:resolved
                                   ~expected
                                   ~covariant:true);
                              declare_location = instantiate (Attribute.location attribute);
                            };
                          }
                      | _ ->
                          Error.IncompatibleVariableType {
                            Error.name = access;
                            mismatch =
                              (Error.create_mismatch
                                 ~resolution
                                 ~actual:resolved
                                 ~expected
                                 ~covariant:true);
                            declare_location = instantiate location;
                          }
                    in
                    Some (Error.create ~location ~kind ~define)
                  else
                    None
                in
                error >>| add_error ~state |> Option.value ~default:state
              in

              (* Check for missing annotations. *)
              let state =
                let error =
                  let error =
                    let open Annotated in
                    let open Access in
                    let insufficiently_annotated =
                      (Type.equal expected Type.Top || Type.equal expected Type.Object) &&
                      not (Type.equal resolved Type.Top || Type.equal resolved Type.ellipses)
                    in
                    let is_type_alias access =
                      Expression.Access.expression access
                      |> Resolution.parse_annotation resolution
                      |> Resolution.is_instantiated resolution
                    in
                    match element with
                    | Attribute { attribute = access; origin = Module _; defined }
                      when defined && insufficiently_annotated ->
                        Some (
                          location,
                          Error.create
                            ~location
                            ~kind:(Error.MissingGlobalAnnotation {
                                Error.name = access;
                                annotation = Some resolved;
                                evidence_locations = [instantiate location];
                                due_to_any = Type.equal expected Type.Object;
                              })
                            ~define
                        )
                    | Attribute { attribute = access; origin = Instance attribute; _ }
                      when insufficiently_annotated ->
                        let attribute_location = Attribute.location attribute in
                        Some (
                          attribute_location,
                          Error.create
                            ~location:attribute_location
                            ~kind:(Error.MissingAttributeAnnotation {
                                parent = Attribute.parent attribute;
                                missing_annotation = {
                                  Error.name = access;
                                  annotation = Some resolved;
                                  evidence_locations = [instantiate location];
                                  due_to_any = Type.equal expected Type.Object;
                                };
                              })
                            ~define
                        )
                    | Value
                      when Type.equal expected Type.Top &&
                           not (Type.equal resolved Type.Top) &&
                           not (is_type_alias access) ->
                        let global_location =
                          Resolution.global resolution (Expression.Access.delocalize access)
                          >>| Node.location
                          |> Option.value ~default:location
                        in
                        Some (
                          global_location,
                          Error.create
                            ~location:global_location
                            ~kind:(Error.MissingGlobalAnnotation {
                                Error.name = access;
                                annotation = Some resolved;
                                evidence_locations = [instantiate location];
                                due_to_any = Type.equal expected Type.Object;
                              })
                            ~define
                        )
                    | _ ->
                        None
                  in
                  (* Join errors on their declaration location to suggest annotations. *)
                  error
                  >>| fun (location, error) ->
                  Map.find errors location
                  >>| (Error.join ~resolution error)
                  |> Option.value ~default:error
                in
                error >>| add_error ~state |> Option.value ~default:state
              in

              (* Propagate annotations. *)
              let state =
                let resolution =
                  let annotation =
                    if Annotation.is_immutable annotation then
                      Refinement.refine ~resolution annotation guide
                    else if explicit then
                      Annotation.create_immutable ~global:false guide
                    else
                      Annotation.create guide
                  in
                  Resolution.set_local resolution ~access ~annotation
                in
                { state with resolution }
              in
              state
          | List elements, guide
          | Tuple elements, guide
            when is_uniform_sequence guide ->
              let propagate state element =
                match Node.value element with
                | Starred (Starred.Once target) ->
                    forward_assign ~state ~target ~guide ~resolved ~expression:None
                | _ ->
                    let guide = uniform_sequence_parameter guide in
                    let resolved = uniform_sequence_parameter resolved in
                    forward_assign ~state ~target:element ~guide ~resolved ~expression:None
              in
              List.fold elements ~init:state ~f:propagate
          | List elements, guide
          | Tuple elements, guide
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
                let annotations =
                  nonuniform_sequence_parameters
                    ~minimum_length:(List.length elements)
                    guide
                in
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
                    Error.create
                      ~location
                      ~kind:(Error.Unpack {
                          expected_count = List.length assignees;
                          unpack_problem = CountMismatch (List.length annotations);
                        })
                      ~define
                    |> add_error ~state
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
          | List elements, guide
          | Tuple elements, guide ->
              let kind =
                match guide with
                | Type.Tuple (Type.Bounded parameters) ->
                    (Error.Unpack {
                        expected_count = List.length elements;
                        unpack_problem = CountMismatch (List.length parameters);
                      })
                | _ ->
                    (Error.Unpack {
                        expected_count = List.length elements;
                        unpack_problem = UnacceptableType guide;
                      })
              in
              let state =
                Error.create
                  ~location
                  ~kind
                  ~define
                |> add_error ~state
              in
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
              state
        in
        forward_assign ~state ~target ~guide ~resolved ~expression:(Some value)

    | Assert { Assert.test; _ } ->
        let { resolution; _ } as state =
          forward_expression ~state ~expression:test
          |> fun { state; _ } -> state
        in
        let contradiction_error =
          match Node.value test with
          | UnaryOperator {
              UnaryOperator.operator = UnaryOperator.Not;
              operand = {
                Node.value =
                  Access [
                    Access.Identifier name;
                    Access.Call {
                      Node.value = [
                        { Argument.name = None; value };
                        { Argument.name = None; value = annotation };
                      ];
                      _;
                    };
                  ];
                _;
              };
            } when Identifier.show name = "isinstance" ->
              begin
                match Resolution.parse_meta_annotation resolution annotation with
                | Some expected ->
                    let { resolved; _ } = forward_expression ~state ~expression:value in
                    if (Type.equal resolved Type.Bottom
                        || Type.is_unknown resolved
                        || not (Resolution.less_or_equal resolution ~left:resolved ~right:expected))
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
                                    ~covariant:true);
                               expression = value;
                             })
                           ~define)
                | None ->
                    let { resolved; _ } = forward_expression ~state ~expression:annotation in
                    Some
                      (Error.create
                         ~location:(Node.location test)
                         ~kind:(Error.IncompatibleParameterType {
                             name = None;
                             position = 1;
                             callee = Some (Access.create "isinstance");
                             mismatch = {
                               Error.expected = Type.meta (Type.variable "T");
                               actual = resolved;
                               due_to_invariance = false;
                             }
                           })
                         ~define)
              end
          | _ ->
              None
        in
        let explicit_bottom =
          match Node.value test with
          | False -> true
          | _ -> false
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
                    Type.Union (List.map elements ~f:(Resolution.parse_annotation resolution))
                | _ ->
                    Resolution.parse_annotation resolution annotation
              in
              let updated_annotation =
                let refinement_unnecessary existing_annotation =
                  Refinement.less_or_equal
                    ~resolution
                    existing_annotation
                    (Annotation.create annotation)
                  && not (Type.equal (Annotation.annotation existing_annotation) Type.Bottom)
                in
                match Resolution.get_local resolution ~access with
                | Some existing_annotation when refinement_unnecessary existing_annotation ->
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
                        { Argument.name = None; value = annotation };];
                      _;
                    };
                  ];
                _;
              };
            } when Identifier.show name = "isinstance" ->
              begin
                match Resolution.get_local resolution ~access with
                | Some {
                    Annotation.annotation = (Type.Optional (Type.Union parameters)) as unrefined;
                    _;
                  }
                | Some { Annotation.annotation = (Type.Union parameters) as unrefined; _ } ->
                    let parameters =
                      match unrefined with
                      | Type.Optional _ ->
                          Type.Set.of_list (Type.none :: parameters)
                      | _ ->
                          Type.Set.of_list parameters
                    in
                    let constraints =
                      begin
                        match annotation with
                        | { Node.value = Tuple elements; _ } ->
                            List.filter_map
                              elements
                              ~f:(Resolution.parse_meta_annotation resolution)
                        | _ ->
                            Resolution.parse_meta_annotation resolution annotation
                            |> Option.to_list
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
              let open Access in
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
              operator = ComparisonOperator.IsNot;
              right = { Node.value = Access [Access.Identifier identifier; ]; _ };
            } when Identifier.show identifier = "None" ->
              let { resolution; _ } = forward_statement ~state ~statement:(Statement.assume left) in
              resolution
          | ComparisonOperator {
              ComparisonOperator.left = { Node.value = Access access; _ };
              operator = ComparisonOperator.Is;
              right = { Node.value = Access [Access.Identifier identifier; ]; _ };
            } when Identifier.show identifier = "None" ->
              let open Annotated in
              let open Access in
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
        begin
          match contradiction_error, explicit_bottom with
          | Some error, _ ->
              add_error ~state:{ state with bottom = true } error
          | None, true ->
              { state with bottom = true }
          | None, false ->
              { state with resolution }
        end

    | Expression { Node.value = Access access; _ } when Access.is_assert_function access ->
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

    | Delete _ ->
        (* TODO(T26146217): add coverage. *)
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
          let add_import_error errors ~resolution:_ ~resolved:_ ~element ~lead:_ =
            let open Annotated.Access in
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
          | Type.Parametric { name; parameters = [parameter] }
            when Identifier.show name = "typing.Iterator" ->
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
        let schedule ~define =
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
                    Resolution.set_local initial_resolution ~access:key ~annotation:data
                  in
                  Resolution.annotations resolution
                  |> Map.fold ~init:initial_resolution ~f:update
                in
                Some {
                  nested = define;
                  initial = { nested_resolution; nested_bottom = false };
                }
          in
          Map.change ~f:update nested_defines location
        in
        match Node.value statement with
        | Class { Class.name; body; _ } ->
            schedule ~define:(Define.create_class_toplevel ~qualifier:name ~statements:body)
        | Define define when not (Define.is_stub define) ->
            schedule ~define
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
              |> Access.Map.to_tree
            in
            let postcondition =
              Resolution.annotations post_resolution
              |> Access.Map.to_tree
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


let resolution (module Handler: Environment.Handler) ?(annotations = Access.Map.empty) () =
  let parse_annotation = Type.create ~aliases:Handler.aliases in

  let class_representation annotation =
    let primitive, _ = Type.split annotation in
    Handler.class_definition primitive
  in

  let class_definition annotation =
    match class_representation annotation with
    | Some { class_definition; _ } ->
        Some class_definition
    | None ->
        None
  in

  let order = (module Handler.TypeOrderHandler : TypeOrder.Handler) in
  let state_without_resolution =
    let empty_resolution =
      Resolution.create
        ~annotations:Access.Map.empty
        ~order:(module Handler.TypeOrderHandler)
        ~resolve:(fun ~resolution:_ _ -> Type.Top)
        ~parse_annotation:(fun _ -> Type.Top)
        ~global:(fun _ -> None)
        ~module_definition:(fun _ -> None)
        ~class_definition:(fun _ -> None)
        ~class_representation:(fun _ -> None)
        ()
    in
    {
      State.configuration = Configuration.Analysis.create ();
      errors = Location.Reference.Map.empty;
      define =
        Define.create_toplevel ~qualifier:[] ~statements:[]
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
  Resolution.create
    ~annotations
    ~order
    ~resolve
    ~parse_annotation
    ~global:Handler.globals
    ~module_definition:Handler.module_definition
    ~class_definition
    ~class_representation
    ()


let resolution_with_key ~environment ~parent ~access ~key =
  let annotations =
    match key, ResolutionSharedMemory.get access with
    | Some key, Some map ->
        map
        |> Int.Map.of_tree
        |> (fun map -> Int.Map.find map key)
        >>| (fun { precondition; _ } -> precondition)
        >>| Access.Map.of_tree
        |> Option.value ~default:Access.Map.empty
    | _ ->
        Access.Map.empty
  in
  resolution environment ~annotations ()
  |> Resolution.with_parent ~parent


let check
    ~configuration
    ~environment
    ~source:({ Source.handle; qualifier; statements; _ } as source) =
  Log.debug "Checking %s..." (File.Handle.show handle);

  let resolution = resolution environment () in

  let check
      ~define:{ Node.value = ({ Define.name; parent; _ } as define); _ }
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
            ~root:(Configuration.Analysis.pyre_root configuration)
            ~relative:(Format.asprintf "cfgs%a.dot" Access.pp name)
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

    let () =
      (* Write fixpoint type resolutions to shared memory *)
      let dump_resolutions { State.resolution_fixpoint; _ } =
        ResolutionSharedMemory.add name resolution_fixpoint
      in
      exit
      >>| dump_resolutions
      |> ignore
    in

    let () =
      (* Schedule nested functions for analysis. *)
      exit
      >>| State.nested_defines
      >>| List.iter ~f:(Queue.enqueue queue)
      |> ignore
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
  in

  let results =
    let queue =
      let queue = Queue.create () in
      let toplevel =
        let location =
          {
            Location.path = File.Handle.show handle;
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
      | Some (
          ({ Node.location; value = ({ Define.name; _ } as define) } as define_node),
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
                    "define", Access.show name;
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
                  SingleSourceResult.errors = [undefined_error];
                  coverage = Coverage.create ~crashes:1 ();
                }
          in
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
            not (Error.suppress ~mode error)
        in
        List.filter ~f:keep_error errors
    in
    List.map results ~f:SingleSourceResult.errors
    |> List.map ~f:filter
    |> List.concat
    |> Error.join_at_source ~resolution
    |> List.map ~f:(Error.dequalify (Preprocessing.dequalify_map source) ~resolution)
    |> List.sort ~compare:Error.compare
  in

  let coverage =
    List.map results ~f:SingleSourceResult.coverage
    |> Coverage.aggregate_over_source ~source
  in
  Coverage.log coverage ~total_errors:(List.length errors) ~path:(File.Handle.show handle);

  { Result.errors; coverage }
