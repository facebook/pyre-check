(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Ast
open Analysis
open Core
open Pyre
open Statement
open Expression

let inlined_original_function_name = "__original_function"

let inlined_wrapper_function_name = "__wrapper"

let all_decorators environment =
  let unannotated_global_environment =
    TypeEnvironment.ReadOnly.global_resolution environment
    |> GlobalResolution.unannotated_global_environment
  in
  let decorator_set = Reference.Hash_set.create () in
  let add_decorators define_reference =
    let add_decorator_to_set { Decorator.name = { Node.value; _ }; _ } =
      Base.Hash_set.add decorator_set value
    in
    UnannotatedGlobalEnvironment.ReadOnly.get_define_body
      unannotated_global_environment
      define_reference
    |> Option.iter ~f:(fun { Node.value = { Define.signature = { decorators; _ }; _ }; _ } ->
           List.iter ~f:add_decorator_to_set decorators)
  in
  let _ =
    UnannotatedGlobalEnvironment.ReadOnly.all_defines unannotated_global_environment
    |> List.iter ~f:add_decorators
  in
  Base.Hash_set.to_list decorator_set


let all_decorator_bodies environment =
  all_decorators environment
  |> List.filter_map ~f:(fun decorator_name ->
         GlobalResolution.get_decorator_define
           (TypeEnvironment.ReadOnly.global_resolution environment)
           decorator_name
         >>| fun decorator_define -> decorator_name, decorator_define)
  |> Reference.Map.of_alist
  |> function
  | `Ok map -> map
  | _ -> Reference.Map.empty


(* Pysa doesn't care about metadata like `unbound_names`. So, strip them. *)
let sanitize_define
    ?(strip_decorators = true)
    ({ Define.signature = { decorators; _ } as signature; _ } as define)
  =
  {
    define with
    signature = { signature with decorators = (if strip_decorators then [] else decorators) };
    unbound_names = [];
  }


let sanitize_defines ~strip_decorators source =
  let module SanitizeDefines = Transform.MakeStatementTransformer (struct
    type t = unit

    let statement _ = function
      | { Node.value = Statement.Define define; location } ->
          ( (),
            [{ Node.value = Statement.Define (sanitize_define ~strip_decorators define); location }]
          )
      | statement -> (), [statement]
  end)
  in
  let { SanitizeDefines.source; _ } = SanitizeDefines.transform () source in
  source


let rename_local_variable ~from ~to_ { Node.value = statement; location } =
  let rename_identifier = function
    | Expression.Name (Name.Identifier identifier) when Identifier.equal identifier from ->
        Expression.Name (Name.Identifier to_)
    | expression -> expression
  in
  { Node.value = Transform.transform_expressions ~transform:rename_identifier statement; location }


let requalify_name ~old_qualifier ~new_qualifier = function
  (* TODO(T69755379): Handle Name.Attribute too. *)
  | Name.Identifier identifier as name ->
      let reference = Reference.create identifier in
      if Reference.is_local reference then
        match
          Reference.delocalize reference
          |> Reference.drop_prefix ~prefix:old_qualifier
          |> Reference.as_list
        with
        | [name] ->
            Name.Identifier (Preprocessing.qualify_local_identifier ~qualifier:new_qualifier name)
        | _ -> name
      else
        name
  | name -> name


let requalify_define ~old_qualifier ~new_qualifier define =
  let transform = function
    | Expression.Name name -> Expression.Name (requalify_name ~old_qualifier ~new_qualifier name)
    | expression -> expression
  in
  match Transform.transform_expressions ~transform (Statement.Define define) with
  | Statement.Define define -> define
  | _ -> failwith "expected define"


let create_function_call_to
    ~location
    { Define.signature = { name = { Node.value = function_name; _ }; async; parameters; _ }; _ }
  =
  let parameter_to_argument { Node.value = { Parameter.name; _ }; location } =
    {
      Call.Argument.name = None;
      value = Expression.Name (create_name ~location name) |> Node.create ~location;
    }
  in
  let call =
    Expression.Call
      {
        callee =
          Expression.Name (create_name_from_reference ~location function_name)
          |> Node.create ~location;
        arguments = List.map parameters ~f:parameter_to_argument;
      }
  in
  if async then Expression.Await (Node.create ~location call) else call


let rename_define ~new_name ({ Define.signature = { name; _ } as signature; _ } as define) =
  { define with Define.signature = { signature with name = { name with Node.value = new_name } } }


let get_higher_order_function_parameter ~environment { Define.signature = { parameters; _ }; _ } =
  match parameters with
  | [{ Node.value = { Parameter.name; annotation = Some annotation; _ }; _ }] -> (
      let resolution =
        TypeCheck.resolution
          (TypeEnvironment.ReadOnly.global_resolution environment)
          (module TypeCheck.DummyContext)
      in
      match Resolution.resolve_expression_to_type resolution annotation with
      | Type.Parametric
          {
            name = "type";
            parameters = [Single (Type.Callable _ | Type.Primitive "typing.Callable")];
          } ->
          (* We only support simple decorators that accept a callable. *)
          Some name
      | _ -> None )
  | _ -> None


let extract_wrapper_define { Define.body; _ } =
  let nested_defines =
    List.filter_map body ~f:(function
        | { Node.value = Statement.Define wrapper_define; _ } -> Some wrapper_define
        | _ -> None)
  in
  match nested_defines with
  | [wrapper_define] -> Some wrapper_define
  | _ -> None


let inline_decorator_in_define
    ~location
    ~wrapper_define:
      ( {
          Define.signature =
            { name = { Node.value = wrapper_define_name; _ }; _ } as wrapper_signature;
          _;
        } as wrapper_define )
    ~higher_order_function_parameter_name
    ({ Define.signature = { name = { Node.value = original_function_name; _ }; _ }; _ } as define)
  =
  let qualifier = original_function_name in
  let inlined_original_define =
    sanitize_define define
    |> rename_define ~new_name:(Reference.create inlined_original_function_name)
    |> requalify_define
         ~old_qualifier:qualifier
         ~new_qualifier:(Reference.create ~prefix:qualifier inlined_original_function_name)
  in
  let inlined_original_define_statement =
    Statement.Define inlined_original_define |> Node.create ~location
  in
  let inlined_wrapper_define =
    sanitize_define wrapper_define
    |> rename_define ~new_name:(Reference.create inlined_wrapper_function_name)
    |> requalify_define
         ~old_qualifier:(Reference.delocalize wrapper_define_name)
         ~new_qualifier:(Reference.create ~prefix:qualifier inlined_wrapper_function_name)
  in
  let inlined_wrapper_define_statement =
    Statement.Define inlined_wrapper_define
    |> Node.create ~location
    |> rename_local_variable
         ~from:higher_order_function_parameter_name
         ~to_:(Preprocessing.qualify_local_identifier ~qualifier inlined_original_function_name)
  in
  let return_call_to_wrapper =
    Statement.Return
      {
        is_implicit = false;
        expression =
          Some (create_function_call_to ~location inlined_wrapper_define |> Node.create ~location);
      }
    |> Node.create ~location
  in
  let body =
    [inlined_original_define_statement; inlined_wrapper_define_statement; return_call_to_wrapper]
  in
  { define with body; signature = wrapper_signature }
  |> sanitize_define
  |> rename_define ~new_name:qualifier


let inline_decorators ~environment ~decorator_bodies source =
  let module Transform = Transform.Make (struct
    type t = unit

    let transform_expression_children _ _ = true

    let transform_children _ _ = true

    let expression _ expression = expression

    let statement _ statement =
      let statement =
        match statement with
        | {
         Node.value =
           Statement.Define
             ( {
                 signature =
                   { decorators = [{ Decorator.name = { Node.value = decorator_name; _ }; _ }]; _ };
                 _;
               } as define );
         location;
        } ->
            let decorator_body = Map.find decorator_bodies decorator_name in
            let inlined_define =
              match
                ( decorator_body >>= extract_wrapper_define,
                  decorator_body >>= get_higher_order_function_parameter ~environment )
              with
              | Some wrapper_define, Some higher_order_function_parameter_name ->
                  inline_decorator_in_define
                    ~location
                    ~wrapper_define
                    ~higher_order_function_parameter_name
                    define
                  |> Option.some
              | _ -> None
            in
            let postprocess decorated_define =
              let statement = { statement with value = Statement.Define decorated_define } in
              Source.create [statement]
              |> Preprocessing.qualify
              |> Preprocessing.populate_nesting_defines
              |> Preprocessing.populate_captures
              |> Source.statements
              |> function
              | [statement] -> statement
              | _ -> failwith "impossible"
            in
            inlined_define >>| postprocess |> Option.value ~default:statement
        | _ -> statement
      in
      (), [statement]
  end)
  in
  Transform.transform () source |> Transform.source
