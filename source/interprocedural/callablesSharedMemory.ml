(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Statement
open Pyre
module AstResult = PyrePysaApi.AstResult

let class_method_decorators = ["classmethod"; "abstractclassmethod"; "abc.abstractclassmethod"]

let static_method_decorators = ["staticmethod"; "abstractstaticmethod"; "abc.abstractstaticmethod"]

module CallableSignature = Analysis.PyrePysaEnvironment.CallableSignature

let callable_signature_from_define_for_pyre1 ~pyre1_api ~target ~qualifier define =
  let method_kind =
    match Target.get_regular target with
    | Target.Regular.Method { method_name = "__new__"; _ } -> Some Target.MethodKind.Static
    | Target.Regular.Method _ ->
        let define = Node.value define in
        if List.exists class_method_decorators ~f:(Ast.Statement.Define.has_decorator define) then
          Some Target.MethodKind.Class
        else if List.exists static_method_decorators ~f:(Ast.Statement.Define.has_decorator define)
        then
          Some Target.MethodKind.Static
        else
          Some Target.MethodKind.Instance
    | _ -> None
  in
  {
    CallableSignature.qualifier;
    location = AstResult.Some define.Node.location;
    define_name = define.Node.value.signature.name;
    parameters = AstResult.Some define.Node.value.signature.parameters;
    return_annotation = AstResult.Some define.Node.value.signature.return_annotation;
    decorators = AstResult.Some define.Node.value.signature.decorators;
    captures =
      Analysis.PyrePysaEnvironment.ReadOnly.get_captures_from_define pyre1_api define.Node.value;
    method_kind;
    is_stub_like = Define.is_stub define.Node.value;
  }


let get_signature_and_definition ~pyre_api callable =
  let define_name = Target.define_name_exn callable in
  match pyre_api with
  | PyrePysaApi.ReadOnly.Pyrefly pyrefly_api ->
      PyreflyApi.ReadOnly.get_callable_signature_opt pyrefly_api define_name
      >>| fun signature ->
      let define = PyreflyApi.ReadOnly.get_define_opt pyrefly_api define_name in
      signature, define
  | PyrePysaApi.ReadOnly.Pyre1 pyre1_api ->
      Target.get_definitions ~pyre1_api ~warn_multiple_definitions:false define_name
      >>= fun { Target.qualifier; callables; _ } ->
      Target.Map.find_opt callable callables
      >>| fun define ->
      let signature =
        callable_signature_from_define_for_pyre1 ~pyre1_api ~target:callable ~qualifier define
      in
      signature, AstResult.Some define


let get_signature_and_definition_for_test = get_signature_and_definition

module DefineAndQualifier = struct
  type t = {
    qualifier: Reference.t;
    define: Define.t Node.t;
  }
end

module DefinesSharedMemory =
  Hack_parallel.Std.SharedMemory.FirstClassWithKeys.Make
    (Target.SharedMemoryKey)
    (struct
      type t = DefineAndQualifier.t AstResult.t

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = "callable defines"
    end)

module SignaturesSharedMemory =
  Hack_parallel.Std.SharedMemory.FirstClass.WithCache.Make
    (Target.SharedMemoryKey)
    (struct
      type t = CallableSignature.t

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = "callable signatures"
    end)

module ReadWrite = struct
  type t = {
    defines: DefinesSharedMemory.t;
    signatures: SignaturesSharedMemory.t;
    (* When using pyrefly, read from the pyrefly API first, and fall back to shared memory if the
       callable does not exist (for artificial defines such as decorated targets). *)
    pyrefly_api: PyreflyApi.ReadOnly.t option;
  }

  let empty () =
    {
      defines = DefinesSharedMemory.create ();
      signatures = SignaturesSharedMemory.create ();
      pyrefly_api = None;
    }


  let cleanup { defines; signatures; _ } =
    let keys = SignaturesSharedMemory.KeySet.of_list (DefinesSharedMemory.keys defines) in
    let () = DefinesSharedMemory.cleanup ~clean_old:true defines in
    let () = SignaturesSharedMemory.remove_old_batch signatures keys in
    let () = SignaturesSharedMemory.remove_batch signatures keys in
    ()


  let from_callables ~scheduler ~scheduler_policy ~pyre_api callables =
    match pyre_api with
    | PyrePysaApi.ReadOnly.Pyrefly pyrefly_api ->
        (* For pyrefly, signatures are pre-computed when parsing sources. *)
        {
          defines = DefinesSharedMemory.create ();
          signatures = SignaturesSharedMemory.create ();
          pyrefly_api = Some pyrefly_api;
        }
    | PyrePysaApi.ReadOnly.Pyre1 _ ->
        let define_shared_memory = DefinesSharedMemory.create () in
        let signature_shared_memory = SignaturesSharedMemory.create () in
        let define_shared_memory_add_only = DefinesSharedMemory.add_only define_shared_memory in
        let define_empty_shared_memory =
          DefinesSharedMemory.AddOnly.create_empty define_shared_memory_add_only
        in
        let map =
          List.fold ~init:define_empty_shared_memory ~f:(fun define_shared_memory target ->
              match get_signature_and_definition ~pyre_api target with
              | None -> define_shared_memory
              | Some (signature, define) ->
                  let define_and_qualifier =
                    AstResult.map define ~f:(fun define ->
                        {
                          DefineAndQualifier.define;
                          qualifier = signature.CallableSignature.qualifier;
                        })
                  in
                  SignaturesSharedMemory.add signature_shared_memory target signature;
                  DefinesSharedMemory.AddOnly.add define_shared_memory target define_and_qualifier)
        in
        let define_shared_memory_add_only =
          Scheduler.map_reduce
            scheduler
            ~policy:scheduler_policy
            ~initial:define_shared_memory_add_only
            ~map
            ~reduce:(fun left right ->
              DefinesSharedMemory.AddOnly.merge_same_handle_disjoint_keys
                ~smaller:left
                ~larger:right)
            ~inputs:callables
            ()
        in
        {
          defines = DefinesSharedMemory.from_add_only define_shared_memory_add_only;
          signatures = signature_shared_memory;
          pyrefly_api = None;
        }


  let add_alist_sequential ({ signatures; _ } as handle) entries =
    let () =
      List.iter entries ~f:(fun (target, signature, _) ->
          SignaturesSharedMemory.add signatures target signature)
    in
    let defines =
      entries
      |> List.map ~f:(fun (target, { CallableSignature.qualifier; _ }, define) ->
             target, AstResult.Some { DefineAndQualifier.qualifier; define })
      |> DefinesSharedMemory.add_alist_sequential handle.defines
    in
    { handle with defines }
end

module ReadOnly = struct
  type t = {
    defines: DefinesSharedMemory.ReadOnly.t;
    signatures: SignaturesSharedMemory.t;
    pyrefly_api: PyreflyApi.ReadOnly.t option;
  }

  let read_only { ReadWrite.defines; signatures; pyrefly_api } =
    { defines = DefinesSharedMemory.read_only defines; signatures; pyrefly_api }


  let option_to_ast_result = function
    | Some ast_result -> ast_result
    | None -> AstResult.Pyre1NotFound


  (* Return the define name for looking up a target in pyrefly. Returns [None] for targets that
     pyrefly does not know about: decorated targets (which have synthetic defines in shared memory),
     override targets and object targets (which have no define name). *)
  let define_name_for_pyrefly_lookup target =
    let regular = Target.get_regular target in
    match Target.Regular.kind regular with
    | Some Decorated -> None
    | _ -> Target.Regular.define_name regular


  let get_define_from_pyrefly ~pyrefly_api target =
    define_name_for_pyrefly_lookup target
    >>= fun define_name ->
    PyreflyApi.ReadOnly.get_callable_metadata_opt pyrefly_api define_name
    >>| fun { PyreflyApi.CallableMetadata.module_qualifier; _ } ->
    PyreflyApi.ReadOnly.get_define_opt pyrefly_api define_name
    |> AstResult.map ~f:(fun define -> { DefineAndQualifier.define; qualifier = module_qualifier })


  let get_define_from_shared_memory ~defines target =
    DefinesSharedMemory.ReadOnly.get ~cache:true defines target |> option_to_ast_result


  let get_define { defines; pyrefly_api; _ } target =
    match pyrefly_api with
    | Some pyrefly_api -> (
        match get_define_from_pyrefly ~pyrefly_api target with
        | Some result -> result
        | None -> get_define_from_shared_memory ~defines target)
    | None -> get_define_from_shared_memory ~defines target


  let get_signature_from_pyrefly ~pyrefly_api target =
    define_name_for_pyrefly_lookup target
    >>= PyreflyApi.ReadOnly.get_callable_signature_opt pyrefly_api


  let get_signature_from_shared_memory ~signatures target =
    SignaturesSharedMemory.get signatures target


  let get_signature { signatures; pyrefly_api; _ } target =
    match pyrefly_api with
    | Some pyrefly_api -> (
        match get_signature_from_pyrefly ~pyrefly_api target with
        | Some _ as result -> result
        | None -> get_signature_from_shared_memory ~signatures target)
    | None -> get_signature_from_shared_memory ~signatures target


  let get_location handle target =
    target
    |> Target.strip_parameters
    |> get_signature handle
    >>| (fun { CallableSignature.qualifier; location; _ } ->
          AstResult.map location ~f:(Location.with_module ~module_reference:qualifier))
    |> option_to_ast_result


  let get_location_opt handle target = get_location handle target |> AstResult.to_option

  let get_qualifier handle target =
    get_signature handle target >>| fun { CallableSignature.qualifier; _ } -> qualifier


  let get_method_kind_from_pyrefly ~pyrefly_api target =
    define_name_for_pyrefly_lookup target
    >>= PyreflyApi.ReadOnly.get_callable_metadata_opt pyrefly_api
    >>| PyreflyApi.CallableMetadata.get_method_kind


  let get_method_kind_from_shared_memory ~signatures method_target =
    method_target
    |> Target.from_regular
    |> SignaturesSharedMemory.get signatures
    >>= fun { CallableSignature.method_kind; _ } -> method_kind


  (* Return `is_class_method` and `is_static_method`. *)
  let get_method_kind { signatures; pyrefly_api; _ } target =
    (* For `Override`, we just check its corresponding method. *)
    let method_target = target |> Target.get_regular |> Target.Regular.override_to_method in
    let method_kind =
      let method_target_as_target = method_target |> Target.from_regular in
      match pyrefly_api with
      | Some pyrefly_api -> (
          match get_method_kind_from_pyrefly ~pyrefly_api method_target_as_target with
          | Some result -> result
          | None -> get_method_kind_from_shared_memory ~signatures method_target)
      | None -> get_method_kind_from_shared_memory ~signatures method_target
    in
    match method_kind, method_target with
    | _, Target.Regular.Method { method_name = "__new__"; _ } -> false, true
    | Some Class, _ -> true, false
    | Some Static, _ -> false, true
    | Some Instance, _ -> false, false
    | None, _ -> false, false


  let is_stub_like_from_pyrefly ~pyrefly_api target =
    define_name_for_pyrefly_lookup target
    >>= PyreflyApi.ReadOnly.is_stub_like_callable_opt pyrefly_api


  let is_stub_like_from_shared_memory ~signatures target =
    SignaturesSharedMemory.get signatures target
    >>| fun { CallableSignature.is_stub_like; _ } -> is_stub_like


  let is_stub_like { signatures; pyrefly_api; _ } target =
    match pyrefly_api with
    | Some pyrefly_api -> (
        match is_stub_like_from_pyrefly ~pyrefly_api target with
        | Some _ as result -> result
        | None -> is_stub_like_from_shared_memory ~signatures target)
    | None -> is_stub_like_from_shared_memory ~signatures target


  let get_captures_from_pyrefly ~pyrefly_api target =
    define_name_for_pyrefly_lookup target
    >>= PyreflyApi.ReadOnly.get_callable_captures_opt pyrefly_api


  let get_captures_from_shared_memory ~signatures target =
    SignaturesSharedMemory.get signatures target
    >>| fun { CallableSignature.captures; _ } -> captures


  let get_captures { signatures; pyrefly_api; _ } target =
    match pyrefly_api with
    | Some pyrefly_api -> (
        match get_captures_from_pyrefly ~pyrefly_api target with
        | Some _ as result -> result
        | None -> get_captures_from_shared_memory ~signatures target)
    | None -> get_captures_from_shared_memory ~signatures target


  let callable_from_reference_from_pyrefly ~pyrefly_api name =
    PyreflyApi.ReadOnly.get_callable_metadata_opt pyrefly_api name
    >>| fun metadata ->
    match PyreflyApi.CallableMetadata.get_method_kind metadata with
    | Some _ -> Target.create_method_from_reference name
    | None -> Target.create_function name


  let callable_from_reference_from_shared_memory ~defines name =
    let function_target = Target.create_function name in
    if DefinesSharedMemory.ReadOnly.mem defines function_target then
      Some function_target
    else
      let method_target = Target.create_method_from_reference name in
      if DefinesSharedMemory.ReadOnly.mem defines method_target then
        Some method_target
      else
        None


  let callable_from_reference { defines; pyrefly_api; _ } name =
    match pyrefly_api with
    | Some pyrefly_api -> (
        match callable_from_reference_from_pyrefly ~pyrefly_api name with
        | Some _ as result -> result
        | None -> callable_from_reference_from_shared_memory ~defines name)
    | None -> callable_from_reference_from_shared_memory ~defines name


  let mem_from_pyrefly ~pyrefly_api target =
    match define_name_for_pyrefly_lookup target with
    | Some define_name ->
        Option.is_some (PyreflyApi.ReadOnly.get_callable_metadata_opt pyrefly_api define_name)
    | None -> false


  let mem_from_shared_memory ~signatures target = SignaturesSharedMemory.mem signatures target

  let mem { signatures; pyrefly_api; _ } target =
    match pyrefly_api with
    | Some pyrefly_api ->
        mem_from_pyrefly ~pyrefly_api target || mem_from_shared_memory ~signatures target
    | None -> mem_from_shared_memory ~signatures target
end
