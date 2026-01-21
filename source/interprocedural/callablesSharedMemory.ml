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

module CallableSignature = struct
  type t = {
    qualifier: Reference.t;
    define_name: Reference.t;
    location: Location.t AstResult.t;
    parameters: Expression.Parameter.t list AstResult.t;
    return_annotation: Expression.t option AstResult.t;
    decorators: Expression.t list AstResult.t;
    captures: Analysis.TaintAccessPath.CapturedVariable.t list;
    method_kind: Target.MethodKind.t option;
    is_stub_like: bool;
  }

  let from_define_for_pyre1 ~pyre1_api ~target ~qualifier define =
    let method_kind =
      match Target.get_regular target with
      | Target.Regular.Method { method_name = "__new__"; _ } -> Some Target.MethodKind.Static
      | Target.Regular.Method _ ->
          let define = Node.value define in
          if List.exists class_method_decorators ~f:(Ast.Statement.Define.has_decorator define) then
            Some Target.MethodKind.Class
          else if
            List.exists static_method_decorators ~f:(Ast.Statement.Define.has_decorator define)
          then
            Some Target.MethodKind.Static
          else
            Some Target.MethodKind.Instance
      | _ -> None
    in
    {
      qualifier;
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


  let from_define_for_pyrefly
      ~define_name
      ~metadata:
        {
          PyreflyApi.CallableMetadata.module_qualifier;
          is_classmethod;
          is_staticmethod;
          is_stub;
          parent_is_class;
          _;
        }
      ~captures
      define
    =
    let define_signature =
      AstResult.map ~f:(fun { Node.value = { Define.signature; _ }; _ } -> signature) define
    in
    let method_kind =
      if is_staticmethod then
        Some Target.MethodKind.Static
      else if is_classmethod then
        Some Target.MethodKind.Class
      else if parent_is_class then
        Some Target.MethodKind.Instance
      else
        None
    in
    let is_stub_like =
      match define with
      | _ when is_stub -> true
      | AstResult.Some _ -> false
      | AstResult.ParseError -> true
      | AstResult.TestFile -> true
      | AstResult.Synthesized -> true
      | AstResult.Pyre1NotFound -> failwith "unreachable"
    in
    {
      qualifier = module_qualifier;
      location = AstResult.map define ~f:Node.location;
      define_name;
      parameters =
        AstResult.map ~f:(fun { Define.Signature.parameters; _ } -> parameters) define_signature;
      return_annotation =
        AstResult.map
          ~f:(fun { Define.Signature.return_annotation; _ } -> return_annotation)
          define_signature;
      decorators =
        AstResult.map ~f:(fun { Define.Signature.decorators; _ } -> decorators) define_signature;
      captures;
      method_kind;
      is_stub_like;
    }
end

let get_signature_and_definition ~pyre_api callable =
  let define_name = Target.define_name_exn callable in
  match pyre_api with
  | PyrePysaApi.ReadOnly.Pyrefly pyrefly_api ->
      let metadata = PyreflyApi.ReadOnly.get_callable_metadata pyrefly_api define_name in
      let define = PyreflyApi.ReadOnly.get_define_opt pyrefly_api define_name in
      let captures = PyreflyApi.ReadOnly.get_callable_captures pyrefly_api define_name in
      let signature =
        CallableSignature.from_define_for_pyrefly ~define_name ~metadata ~captures define
      in
      Some (signature, define)
  | PyrePysaApi.ReadOnly.Pyre1 pyre1_api ->
      Target.get_definitions ~pyre1_api ~warn_multiple_definitions:false define_name
      >>= fun { Target.qualifier; callables; _ } ->
      Target.Map.find_opt callable callables
      >>| fun define ->
      let signature =
        CallableSignature.from_define_for_pyre1 ~pyre1_api ~target:callable ~qualifier define
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
  }

  let empty () =
    { defines = DefinesSharedMemory.create (); signatures = SignaturesSharedMemory.create () }


  let cleanup { defines; signatures } =
    let keys = SignaturesSharedMemory.KeySet.of_list (DefinesSharedMemory.keys defines) in
    let () = DefinesSharedMemory.cleanup ~clean_old:true defines in
    let () = SignaturesSharedMemory.remove_old_batch signatures keys in
    let () = SignaturesSharedMemory.remove_batch signatures keys in
    ()


  let from_callables ~scheduler ~scheduler_policy ~pyre_api callables =
    let define_shared_memory = DefinesSharedMemory.create () in
    let signature_shared_memory = SignaturesSharedMemory.create () in
    let define_shared_memory_add_only = DefinesSharedMemory.add_only define_shared_memory in
    let define_empty_shared_memory =
      DefinesSharedMemory.AddOnly.create_empty define_shared_memory_add_only
    in
    let map =
      List.fold ~init:define_empty_shared_memory ~f:(fun define_shared_memory target ->
          (* TODO(T225700656): When using pyrefly, this leads to copying ASTs from the
             PyreflyApi.ReadOnly.t handle to the DefinesSharedMemory.t handle. We could save
             performance by avoiding the copy, and directly use
             `PyreflyApi.ReadOnly.get_define_opt`. *)
          match get_signature_and_definition ~pyre_api target with
          | None -> define_shared_memory
          | Some (signature, define) ->
              let define_and_qualifier =
                AstResult.map define ~f:(fun define ->
                    { DefineAndQualifier.define; qualifier = signature.CallableSignature.qualifier })
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
          DefinesSharedMemory.AddOnly.merge_same_handle_disjoint_keys ~smaller:left ~larger:right)
        ~inputs:callables
        ()
    in
    {
      defines = DefinesSharedMemory.from_add_only define_shared_memory_add_only;
      signatures = signature_shared_memory;
    }


  let add_alist_sequential { defines; signatures } entries =
    let () =
      List.iter entries ~f:(fun (target, signature, _) ->
          SignaturesSharedMemory.add signatures target signature)
    in
    let defines =
      entries
      |> List.map ~f:(fun (target, { CallableSignature.qualifier; _ }, define) ->
             target, AstResult.Some { DefineAndQualifier.qualifier; define })
      |> DefinesSharedMemory.add_alist_sequential defines
    in
    { defines; signatures }
end

module ReadOnly = struct
  type t = {
    defines: DefinesSharedMemory.ReadOnly.t;
    signatures: SignaturesSharedMemory.t;
  }

  let read_only { ReadWrite.defines; signatures } =
    { defines = DefinesSharedMemory.read_only defines; signatures }


  let option_to_ast_result = function
    | Some ast_result -> ast_result
    | None -> AstResult.Pyre1NotFound


  let get_define { defines; _ } target =
    DefinesSharedMemory.ReadOnly.get ~cache:true defines target |> option_to_ast_result


  let get_location { signatures; _ } target =
    target
    |> Target.strip_parameters
    |> SignaturesSharedMemory.get signatures
    >>| (fun { CallableSignature.qualifier; location; _ } ->
          AstResult.map location ~f:(Location.with_module ~module_reference:qualifier))
    |> option_to_ast_result


  let get_location_opt handle target = get_location handle target |> AstResult.to_option

  let get_signature { signatures; _ } target = SignaturesSharedMemory.get signatures target

  let get_qualifier handle target =
    get_signature handle target >>| fun { CallableSignature.qualifier; _ } -> qualifier


  (* Return `is_class_method` and `is_static_method`. *)
  let get_method_kind { signatures; _ } target =
    (* For `Override`, we just check its corresponding method. *)
    let method_target = target |> Target.get_regular |> Target.Regular.override_to_method in
    let method_kind =
      method_target
      |> Target.from_regular
      |> SignaturesSharedMemory.get signatures
      >>= fun { CallableSignature.method_kind; _ } -> method_kind
    in
    match method_kind, method_target with
    | _, Target.Regular.Method { method_name = "__new__"; _ } -> false, true
    | Some Class, _ -> true, false
    | Some Static, _ -> false, true
    | Some Instance, _ -> false, false
    | None, _ -> false, false


  let is_stub_like { signatures; _ } target =
    SignaturesSharedMemory.get signatures target
    >>| fun { CallableSignature.is_stub_like; _ } -> is_stub_like


  let get_captures { signatures; _ } target =
    SignaturesSharedMemory.get signatures target
    >>| fun { CallableSignature.captures; _ } -> captures


  (* Return the function or method target from a reference *)
  let callable_from_reference { defines; _ } name =
    let function_target = Target.create_function name in
    if DefinesSharedMemory.ReadOnly.mem defines function_target then
      Some function_target
    else
      let method_target = Target.create_method_from_reference name in
      if DefinesSharedMemory.ReadOnly.mem defines method_target then
        Some method_target
      else
        None


  let mem { signatures; _ } target = SignaturesSharedMemory.mem signatures target
end
