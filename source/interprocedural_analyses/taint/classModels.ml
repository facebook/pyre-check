(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* ClassModels: infers a set of models for methods of a given class.
 *
 * For instance, this defines the behavior of `NamedTuple` and dataclasses,
 * rather than trying to infer the behavior from the actual implementation,
 * which is highly dynamic and not well suited for static analysis (e.g,
 * `NamedTuple` uses `exec`:
 * https://hg.python.org/cpython/file/b14308524cff/Lib/collections/__init__.py#l368).
 *)

open Core
open Pyre
open Ast
open Analysis
open Interprocedural
open Domains

let infer ~environment ~user_models =
  Log.info "Computing inferred models...";
  let timer = Timer.start () in
  let global_resolution = TypeEnvironment.ReadOnly.global_resolution environment in
  let add_tito ~input_root ~input_path ~output_path ~collapse_depth existing_state =
    let leaf =
      BackwardTaint.singleton CallInfo.Tito Sinks.LocalReturn Frame.initial
      |> BackwardState.Tree.create_leaf
      |> BackwardState.Tree.transform Features.ReturnAccessPathTree.Self Map ~f:(fun _ ->
             Features.ReturnAccessPathTree.create
               [Part (Features.ReturnAccessPathTree.Path, (output_path, collapse_depth))])
    in
    BackwardState.assign ~root:input_root ~path:input_path leaf existing_state
  in
  let add_parameter_tito ~positional position existing_state attribute =
    let input_root =
      if positional then
        AccessPath.Root.PositionalParameter { position; name = attribute; positional_only = false }
      else
        AccessPath.Root.NamedParameter { name = attribute }
    in
    add_tito
      ~input_root
      ~input_path:[]
      ~output_path:[Abstract.TreeDomain.Label.create_name_index attribute]
      ~collapse_depth:Features.CollapseDepth.no_collapse
      existing_state
  in
  let add_sink_from_attribute_model ~positional class_name position existing_state attribute =
    let qualified_attribute =
      Target.create_object (Reference.create ~prefix:class_name attribute)
    in
    match Registry.get user_models qualified_attribute with
    | Some { Model.backward = { sink_taint; _ }; _ } ->
        let taint = BackwardState.read ~root:GlobalModel.global_root ~path:[] sink_taint in
        let root =
          if positional then
            AccessPath.Root.PositionalParameter
              { position; name = attribute; positional_only = false }
          else
            AccessPath.Root.NamedParameter { name = attribute }
        in
        BackwardState.assign ~weak:true ~root ~path:[] taint existing_state
    | None -> existing_state
  in
  let get_attributes_in_alphabetical_order class_name =
    GlobalResolution.class_summary global_resolution (Type.Primitive class_name)
    >>| Node.value
    >>| ClassSummary.attributes ~include_generated_attributes:false ~in_test:false
    |> Option.value ~default:Identifier.SerializableMap.empty
  in
  let has_attribute class_name name =
    let attributes = get_attributes_in_alphabetical_order class_name in
    Identifier.SerializableMap.mem name attributes
  in
  let get_attributes_in_declaration_order class_name =
    let compare_by_location left right =
      Ast.Location.compare (Node.location left) (Node.location right)
    in
    get_attributes_in_alphabetical_order class_name
    |> Identifier.SerializableMap.bindings
    |> List.unzip
    |> snd
    |> List.sort ~compare:compare_by_location
  in

  let compute_dataclass_models class_name =
    let attributes =
      get_attributes_in_declaration_order class_name
      |> List.map ~f:(fun { Node.value = { ClassSummary.Attribute.name; _ }; _ } -> name)
    in
    let taint_in_taint_out =
      List.foldi
        ~f:(fun position -> add_parameter_tito ~positional:true (position + 1))
        ~init:BackwardState.empty
        attributes
    in
    let sink_taint =
      List.foldi attributes ~init:BackwardState.empty ~f:(fun position ->
          add_sink_from_attribute_model ~positional:true (Reference.create class_name) (position + 1))
    in
    [
      ( Target.Method { Target.class_name; method_name = "__init__"; kind = Normal },
        {
          Model.forward = Model.Forward.empty;
          backward = { Model.Backward.taint_in_taint_out; sink_taint };
          sanitizers = Model.Sanitizers.empty;
          modes = Model.ModeSet.empty;
        } );
    ]
  in
  (* We always generate a special `_fields` attribute for NamedTuples which is a tuple containing
     field names. *)
  let compute_named_tuple_models class_name =
    (* If a user-specified __new__ exist, don't override it. *)
    if has_attribute class_name "__new__" then
      []
    else
      (* Should not omit this model. Otherwise the mode is "obscure", thus leading to a tito model,
         which joins the taint on every element of the tuple. *)
      [
        ( Target.Method { Target.class_name; method_name = "__new__"; kind = Normal },
          {
            Model.forward = Model.Forward.empty;
            backward = Model.Backward.empty;
            sanitizers = Model.Sanitizers.empty;
            modes = Model.ModeSet.empty;
          } );
      ]
  in
  let compute_typed_dict_models class_name =
    let fields =
      GlobalResolution.get_typed_dictionary
        ~resolution:global_resolution
        (Type.Primitive class_name)
      >>| (fun { Type.Record.TypedDictionary.fields; _ } -> fields)
      >>| List.map ~f:(fun { Analysis.Type.Record.TypedDictionary.name; required = _; _ } -> name)
      |> Option.value ~default:[]
    in
    let taint_in_taint_out =
      List.foldi ~f:(add_parameter_tito ~positional:false) ~init:BackwardState.empty fields
      (* `TypedDict.__init__ also accepts iterables and **kwargs. *)
      |> add_tito
           ~input_root:
             (AccessPath.Root.PositionalParameter
                { position = 1; name = "__iterable"; positional_only = true })
           ~input_path:[Abstract.TreeDomain.Label.AnyIndex]
           ~output_path:[Abstract.TreeDomain.Label.AnyIndex]
           ~collapse_depth:0
      |> add_tito
           ~input_root:
             (AccessPath.Root.PositionalParameter
                { position = 1; name = "__iterable"; positional_only = true })
           ~input_path:[AccessPath.dictionary_keys]
           ~output_path:[AccessPath.dictionary_keys]
           ~collapse_depth:Features.CollapseDepth.no_collapse
      |> add_tito
           ~input_root:(AccessPath.Root.StarStarParameter { excluded = fields })
           ~input_path:[]
           ~output_path:[Abstract.TreeDomain.Label.AnyIndex]
           ~collapse_depth:Features.CollapseDepth.no_collapse
    in
    let sink_taint =
      List.foldi
        fields
        ~init:BackwardState.empty
        ~f:(add_sink_from_attribute_model ~positional:false (Reference.create class_name))
    in
    [
      ( Target.Method { Target.class_name; method_name = "__init__"; kind = Normal },
        {
          Model.forward = Model.Forward.empty;
          backward = { Model.Backward.taint_in_taint_out; sink_taint };
          sanitizers = Model.Sanitizers.empty;
          modes = Model.ModeSet.empty;
        } );
    ]
  in
  let compute_models class_name class_summary =
    if
      UnannotatedGlobalEnvironment.ReadOnly.exists_matching_class_decorator
        (TypeEnvironment.ReadOnly.unannotated_global_environment environment)
        ~names:["dataclasses.dataclass"; "dataclass"]
        class_summary
    then
      compute_dataclass_models class_name
    else if
      CallResolution.is_transitive_successor_ignoring_untracked
        global_resolution
        ~reflexive:false
        ~predecessor:class_name
        ~successor:"typing.NamedTuple"
    then
      compute_named_tuple_models class_name
    else if
      CallResolution.is_transitive_successor_ignoring_untracked
        global_resolution
        ~reflexive:false
        ~predecessor:class_name
        ~successor:"TypedDictionary"
      || CallResolution.is_transitive_successor_ignoring_untracked
           global_resolution
           ~reflexive:false
           ~predecessor:class_name
           ~successor:"NonTotalTypedDictionary"
    then
      compute_typed_dict_models class_name
    else
      []
  in
  let inferred_models class_name =
    GlobalResolution.class_summary global_resolution (Type.Primitive class_name)
    >>| compute_models class_name
    |> Option.value ~default:[]
  in
  let all_classes =
    TypeEnvironment.ReadOnly.global_resolution environment
    |> GlobalResolution.unannotated_global_environment
    |> UnannotatedGlobalEnvironment.ReadOnly.all_classes
  in
  let models =
    List.concat_map all_classes ~f:inferred_models |> Registry.of_alist ~join:Model.join_user_models
  in
  Statistics.performance
    ~name:"Computed inferred models"
    ~phase_name:"Computing inferred models"
    ~timer
    ~integers:["models", Registry.size models]
    ();
  models
