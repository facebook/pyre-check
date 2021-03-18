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

let args_local_variable_name = "__args"

let kwargs_local_variable_name = "__kwargs"

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


let rename_local_variables ~pairs define =
  let rename_map = Identifier.Map.of_alist pairs in
  match rename_map with
  | `Duplicate_key _ -> define
  | `Ok rename_map -> (
      let rename_identifier = function
        | Expression.Name (Name.Identifier identifier) ->
            let renamed_identifier =
              Map.find rename_map identifier |> Option.value ~default:identifier
            in
            Expression.Name (Name.Identifier renamed_identifier)
        | expression -> expression
      in
      match
        Transform.transform_expressions ~transform:rename_identifier (Statement.Define define)
      with
      | Statement.Define define -> define
      | _ -> failwith "impossible" )


let rename_local_variable ~from ~to_ define = rename_local_variables ~pairs:[from, to_] define

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


let convert_parameter_to_argument { Node.value = { Parameter.name; _ }; location } =
  let name_expression name =
    Expression.Name (create_name ~location name) |> Node.create ~location
  in
  if String.is_prefix ~prefix:"**" name then
    Expression.Starred (Twice (name_expression (String.drop_prefix name 2)))
    |> Node.create ~location
  else if String.is_prefix ~prefix:"*" name then
    Expression.Starred (Once (name_expression (String.drop_prefix name 1))) |> Node.create ~location
  else
    name_expression name


let create_function_call_to ~location ~callee_name { Define.Signature.parameters; async; _ } =
  let call =
    Expression.Call
      {
        callee =
          Expression.Name (create_name_from_reference ~location callee_name)
          |> Node.create ~location;
        arguments =
          List.map parameters ~f:(fun parameter ->
              { Call.Argument.name = None; value = convert_parameter_to_argument parameter });
      }
  in
  if async then Expression.Await (Node.create ~location call) else call


let rename_define ~new_name ({ Define.signature = { name; _ } as signature; _ } as define) =
  { define with Define.signature = { signature with name = { name with Node.value = new_name } } }


type decorator_data = {
  wrapper_define: Define.t;
  higher_order_function_parameter_name: Identifier.t;
}

let extract_decorator_data ~is_decorator_factory { Define.signature = { parameters; _ }; body; _ } =
  let get_nested_defines body =
    List.filter_map body ~f:(function
        | { Node.value = Statement.Define wrapper_define; _ } -> Some wrapper_define
        | _ -> None)
  in
  let sole_wrapper_function_and_callable_parameter ~parameters body =
    match get_nested_defines body, parameters with
    | [wrapper_define], [{ Node.value = { Parameter.name; _ }; _ }] ->
        Some { wrapper_define; higher_order_function_parameter_name = name }
    | _ -> None
  in
  if is_decorator_factory then
    match get_nested_defines body with
    | [{ Define.body; signature = { parameters; _ }; _ }] ->
        sole_wrapper_function_and_callable_parameter ~parameters body
    | _ -> None
  else
    sole_wrapper_function_and_callable_parameter ~parameters body


let make_args_assignment_from_parameters
    ~args_local_variable_name
    { Define.Signature.parameters; _ }
  =
  let location = Location.any in
  let elements =
    List.map parameters ~f:convert_parameter_to_argument
    |> List.filter_map ~f:(function
           | { Node.value = Expression.Starred (Twice _); _ } -> None
           | element -> Some element)
  in
  Statement.Assign
    {
      target =
        Expression.Name
          (create_name_from_reference ~location (Reference.create args_local_variable_name))
        |> Node.create ~location;
      annotation = None;
      parent = None;
      value = Expression.Tuple elements |> Node.create ~location;
    }
  |> Node.create ~location


let make_kwargs_assignment_from_parameters
    ~kwargs_local_variable_name
    { Define.Signature.parameters; _ }
  =
  let location = Location.any in
  let parameter_to_keyword_or_entry parameter =
    match convert_parameter_to_argument parameter with
    | { Node.value = Expression.Starred (Twice keyword); _ } -> `Fst keyword
    | { Node.value = Expression.Starred (Once _); _ } -> `Snd ()
    | argument ->
        let raw_argument_string = Expression.show argument in
        let argument_string =
          String.chop_prefix ~prefix:"$parameter$" raw_argument_string
          |> Option.value ~default:raw_argument_string
        in
        `Trd
          {
            Dictionary.Entry.key =
              Expression.String (StringLiteral.create argument_string)
              |> Node.create_with_default_location;
            value = argument;
          }
  in
  let keywords, _, entries = List.partition3_map parameters ~f:parameter_to_keyword_or_entry in
  Statement.Assign
    {
      target =
        Expression.Name
          (create_name_from_reference ~location (Reference.create kwargs_local_variable_name))
        |> Node.create ~location;
      annotation = None;
      parent = None;
      value = Expression.Dictionary { Dictionary.keywords; entries } |> Node.create ~location;
    }
  |> Node.create ~location


(* If a function always passes on its `args` and `kwargs` to `callee_name`, then replace its broad
   signature `def foo( *args, **kwargs) -> None` with the precise signature `def foo(x: int, y: str)
   -> None`. Pass precise arguments to any calls to `callee_name`.

   In order to support uses of `args` and `kwargs` within `wrapper_define`, we create synthetic
   local variables `__args` and `__kwargs` that contain all the arguments. *)
let replace_signature_if_always_passing_on_arguments
    ~callee_name
    ~new_signature:({ Define.Signature.parameters = new_parameters; _ } as new_signature)
    ({ Define.signature = { parameters = wrapper_parameters; _ }; _ } as wrapper_define)
  =
  match wrapper_parameters with
  | [
   { Node.value = { name = args_parameter; _ }; _ };
   { Node.value = { name = kwargs_parameter; _ }; _ };
  ]
    when String.is_prefix ~prefix:"*" args_parameter
         && (not (String.is_prefix ~prefix:"**" args_parameter))
         && String.is_prefix ~prefix:"**" kwargs_parameter -> (
      let args_parameter = String.drop_prefix args_parameter 1 in
      let kwargs_parameter = String.drop_prefix kwargs_parameter 2 in
      let always_passes_on_args_kwargs = ref true in
      let pass_precise_arguments_instead_of_args_kwargs = function
        | Expression.Call
            {
              Call.callee = { Node.value = Name (Identifier given_callee_name); location };
              arguments;
              _;
            } as expression
          when Identifier.equal callee_name given_callee_name -> (
            match arguments with
            | [
             {
               Call.Argument.name = None;
               value =
                 {
                   Node.value =
                     Starred (Once { Node.value = Name (Identifier given_args_variable); _ });
                   _;
                 };
             };
             {
               Call.Argument.name = None;
               value =
                 {
                   Node.value =
                     Starred (Twice { Node.value = Name (Identifier given_kwargs_variable); _ });
                   _;
                 };
             };
            ]
              when Identifier.equal args_local_variable_name given_args_variable
                   && Identifier.equal kwargs_local_variable_name given_kwargs_variable ->
                create_function_call_to
                  ~location
                  ~callee_name:(Reference.create callee_name)
                  new_signature
            | _ ->
                (* The wrapper is calling the original function as something other than `original(
                   *args, **kwargs)`. This means it probably has a different signature from the
                   original function, so give up on making it have the same signature. *)
                always_passes_on_args_kwargs := false;
                expression )
        | expression -> expression
      in

      (* We have to rename `args` and `kwargs` to `__args` and `__kwargs` before transforming calls
         to `callee`.

         Otherwise, if we rename them after replacing calls to `callee`, we might inadvertently
         rename any `args` or `kwargs` present in `callee`'s parameters. *)
      let define_with_renamed_args_kwargs =
        rename_local_variable ~from:args_parameter ~to_:args_local_variable_name wrapper_define
        |> rename_local_variable ~from:kwargs_parameter ~to_:kwargs_local_variable_name
      in
      match
        Transform.transform_expressions
          ~transform:pass_precise_arguments_instead_of_args_kwargs
          (Statement.Define define_with_renamed_args_kwargs)
      with
      | Statement.Define define when !always_passes_on_args_kwargs ->
          let define_with_original_signature =
            { define with signature = { define.signature with parameters = new_parameters } }
          in
          let args_local_assignment =
            make_args_assignment_from_parameters ~args_local_variable_name new_signature
          in
          let kwargs_local_assignment =
            make_kwargs_assignment_from_parameters ~kwargs_local_variable_name new_signature
          in
          Some
            {
              define_with_original_signature with
              Define.body =
                args_local_assignment
                :: kwargs_local_assignment
                :: define_with_original_signature.body;
            }
      | _ -> None )
  | _ -> None


let inline_decorator_in_define
    ~location
    ~wrapper_define:
      ( {
          Define.signature =
            { name = { Node.value = wrapper_define_name; _ }; _ } as wrapper_signature;
          _;
        } as wrapper_define )
    ~qualifier
    ~higher_order_function_parameter_name
    ({ Define.signature = original_signature; _ } as define)
  =
  let inlined_original_define =
    sanitize_define define
    |> rename_define ~new_name:(Reference.create inlined_original_function_name)
    |> requalify_define
         ~old_qualifier:qualifier
         ~new_qualifier:(Reference.create ~prefix:qualifier inlined_original_function_name)
  in
  let wrapper_define, outer_signature =
    match
      replace_signature_if_always_passing_on_arguments
        ~callee_name:higher_order_function_parameter_name
        ~new_signature:original_signature
        wrapper_define
    with
    | Some ({ Define.signature; _ } as wrapper_define) -> wrapper_define, signature
    | None -> wrapper_define, wrapper_signature
  in
  let inlined_original_define_statement =
    Statement.Define inlined_original_define |> Node.create ~location
  in
  let ( { Define.signature = { name = { Node.value = inlined_wrapper_define_name; _ }; _ }; _ } as
      inlined_wrapper_define )
    =
    sanitize_define wrapper_define
    |> rename_define ~new_name:(Reference.create inlined_wrapper_function_name)
    |> requalify_define
         ~old_qualifier:(Reference.delocalize wrapper_define_name)
         ~new_qualifier:(Reference.create ~prefix:qualifier inlined_wrapper_function_name)
  in
  let inlined_wrapper_define_statement =
    Statement.Define
      (rename_local_variable
         inlined_wrapper_define
         ~from:higher_order_function_parameter_name
         ~to_:(Preprocessing.qualify_local_identifier ~qualifier inlined_original_function_name))
    |> Node.create ~location
  in
  let return_call_to_wrapper =
    Statement.Return
      {
        is_implicit = false;
        expression =
          Some
            ( create_function_call_to
                ~location
                ~callee_name:inlined_wrapper_define_name
                inlined_wrapper_define.signature
            |> Node.create ~location );
      }
    |> Node.create ~location
  in
  let body =
    [inlined_original_define_statement; inlined_wrapper_define_statement; return_call_to_wrapper]
  in
  { define with body; signature = outer_signature }
  |> sanitize_define
  |> rename_define ~new_name:qualifier


let apply_decorator
    ~location
    ~qualifier
    ~decorator_bodies
    ~decorator:{ Decorator.name = { Node.value = decorator_name; _ }; arguments }
    define
  =
  let decorator_body = Map.find decorator_bodies decorator_name in
  match
    decorator_body >>= extract_decorator_data ~is_decorator_factory:(Option.is_some arguments)
  with
  | Some { wrapper_define; higher_order_function_parameter_name } ->
      inline_decorator_in_define
        ~location
        ~qualifier
        ~wrapper_define
        ~higher_order_function_parameter_name
        define
      |> Option.some
  | _ -> None


let inline_decorators ~environment:_ ~decorator_bodies source =
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
             ({ signature = { name = { Node.value = name; _ }; decorators; _ }; _ } as define);
         location;
        } ->
            let postprocess decorated_define =
              let statement = { statement with value = Statement.Define decorated_define } in
              Source.create [statement]
              |> Preprocessing.qualify
              |> Preprocessing.populate_nesting_defines
              |> Preprocessing.populate_captures
              |> Source.statements
              |> function
              | [statement] -> Some statement
              | _ -> None
            in
            let apply_decorator_with_qualifier index define_sofar decorator =
              let qualifier =
                Reference.combine
                  name
                  (Reference.create_from_list
                     (List.init
                        (List.length decorators - index - 1)
                        ~f:(fun _ -> inlined_original_function_name)))
              in
              define_sofar >>= apply_decorator ~location ~qualifier ~decorator_bodies ~decorator
            in
            List.foldi (List.rev decorators) ~init:(Some define) ~f:apply_decorator_with_qualifier
            >>= postprocess
            |> Option.value ~default:statement
        | _ -> statement
      in
      (), [statement]
  end)
  in
  Transform.transform () source |> Transform.source
