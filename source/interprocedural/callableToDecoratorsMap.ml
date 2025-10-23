(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Data_structures
open Ast
open Statement
open Expression
open Pyre
module AstResult = PyrePysaApi.AstResult

module Decorators = struct
  type t = {
    decorators: Expression.t list;
    define_location: Location.t;
  }
end

module DecoratedDefineBody = struct
  type t = {
    decorated_callable: Target.t;
    define_name: Reference.t;
    return_expression: Expression.t;
    attribute_access: Name.t;
    attribute_access_location: Location.t;
  }
end

let ignored_decorator_prefixes_for_higher_order =
  ["sqlalchemy.testing"; "pytest"] |> SerializableStringSet.of_list


let ignored_decorators_for_higher_order =
  [
    "property";
    "dataclass";
    "functools.cache";
    "functools.cached_property";
    "functools.lru_cache";
    "functools.total_ordering";
    "functools.singledispatch";
    "functools.wraps";
    "enum.verify";
    "enum.unique";
    "enum.property";
    "enum.member";
    "enum.nonmember";
    "enum.global_enum";
    "typing.final";
    "typing.runtime_checkable";
    "typing.dataclass_transform";
    "typing.overload";
    "typing.no_type_check";
    "typing.no_type_check_decorator";
    "typing.override";
    "typing.type_check_only";
    "atexit.register";
    "contextlib.contextmanager";
    "contextlib.asynccontextmanager";
    "abc.abstractmethod";
    "abc.abstractproperty";
    "pyre_extensions.override";
    "typing_extensions.deprecated";
    "typing_extensions.final";
    "typing_extensions.overload";
    "typing_extensions.override";
    "typing_extensions.runtime_checkable";
    "click.command";
    "click.group";
    "click.argument";
    "click.option";
    "click.password_option";
    "click.confirmation_option";
    "click.version_option";
    "click.help_option";
    "click.pass_context";
    "click.pass_obj";
    "click.make_pass_decorator";
    "click.decorators.pass_meta_key";
    "overrides.override";
    "overrides.overrides";
  ]
  @ CallablesSharedMemory.class_method_decorators
  @ CallablesSharedMemory.static_method_decorators
  @ Recognized.ignored_decorators_for_higher_order
  |> SerializableStringSet.of_list


let filter_decorator decorator =
  if
    Analysis.DecoratorPreprocessing.has_any_decorator_action
      ~actions:(Analysis.DecoratorPreprocessing.Action.Set.of_list [Discard])
      decorator
  then
    false
  else
    match Ast.Statement.Decorator.from_expression decorator with
    | Some { Decorator.name = { Node.value = decorator_name; _ }; _ } ->
        let decorator_name = Reference.show decorator_name in
        (not (SerializableStringSet.mem decorator_name ignored_decorators_for_higher_order))
        && not
             (SerializableStringSet.exists
                (fun prefix -> String.is_prefix ~prefix decorator_name)
                ignored_decorator_prefixes_for_higher_order)
    | None -> true


let collect_decorators ~callables_to_definitions_map callable =
  callable
  |> Option.some_if (Target.is_normal callable)
  >>| CallablesSharedMemory.ReadOnly.get_signature callables_to_definitions_map
  >>= function
  | Some
      {
        CallablesSharedMemory.CallableSignature.decorators = AstResult.Some decorators;
        location = AstResult.Some define_location;
        _;
      } ->
      let decorators = decorators |> List.filter ~f:filter_decorator |> List.rev in
      if List.is_empty decorators then
        None
      else
        Some { Decorators.decorators; define_location }
  | _ -> None


module SharedMemory = struct
  module T =
    Hack_parallel.Std.SharedMemory.FirstClassWithKeys.Make
      (Target.SharedMemoryKey)
      (struct
        type t = Decorators.t

        let prefix = Hack_parallel.Std.Prefix.make ()

        let description = "callables to decorators"
      end)

  type t = T.t

  module ReadOnly = struct
    type t = T.ReadOnly.t

    let get = T.ReadOnly.get ~cache:true

    let get_decorators readonly callable =
      callable |> get readonly >>| fun { decorators; _ } -> decorators
  end

  let empty = T.create

  let read_only = T.read_only

  let targets_with_decorators = T.keys

  let cleanup = T.cleanup ~clean_old:true

  let save_decorator_counts_to_directory
      ~static_analysis_configuration:
        {
          Configuration.StaticAnalysis.save_results_to;
          output_format;
          configuration = { local_root; _ };
          _;
        }
      ~scheduler
      shared_memory
    =
    let module DecoratorCount = struct
      type t = {
        count: int;
        decorator: string;
      }
      [@@deriving compare]

      let to_json { count; decorator } =
        [
          {
            NewlineDelimitedJson.Line.kind = DecoratorCount;
            data = `Assoc ["count", `Int count; "decorator", `String decorator];
          };
        ]
    end
    in
    let create_decorator_count = function
      | [] -> None
      | decorator :: _ as decorators ->
          Some { DecoratorCount.count = List.length decorators; decorator }
    in
    let show_decorator decorator =
      match decorator.Node.value with
      | Expression.Call { Call.callee; _ } -> (* Decorator factory. *) Expression.show callee
      | _ -> Expression.show decorator
    in
    let decorator_counts =
      shared_memory
      |> T.to_alist
      |> List.map ~f:(fun (_, { Decorators.decorators; _ }) ->
             List.map ~f:show_decorator decorators)
      |> List.concat
      |> List.sort_and_group ~compare:String.compare
      |> List.filter_map ~f:create_decorator_count
      |> List.sort ~compare:DecoratorCount.compare
      |> List.rev
    in
    let filename_prefix = "decorator-counts" in
    match save_results_to with
    | Some directory -> (
        Log.info "Writing the decorator counts to `%s`" (PyrePath.absolute directory);
        match output_format with
        | Configuration.TaintOutputFormat.Json ->
            NewlineDelimitedJson.write_file
              ~path:(PyrePath.append directory ~element:(Format.asprintf "%s.json" filename_prefix))
              ~configuration:(`Assoc ["repo", `String (PyrePath.absolute local_root)])
              ~to_json_lines:DecoratorCount.to_json
              decorator_counts
        | Configuration.TaintOutputFormat.ShardedJson ->
            NewlineDelimitedJson.remove_sharded_files ~directory ~filename_prefix;
            NewlineDelimitedJson.write_sharded_files
              ~scheduler
              ~directory
              ~filename_prefix
              ~configuration:(`Assoc ["repo", `String (PyrePath.absolute local_root)])
              ~to_json_lines:DecoratorCount.to_json
              decorator_counts)
    | None -> ()


  (* We assume `DecoratorPreprocessing.setup_preprocessing` is called before since we use its shared
     memory here. *)
  let create ~callables_to_definitions_map ~scheduler ~scheduler_policy callables =
    (* TODO(T240882988): This ends up copying decorators from `CallablesSharedMemory` to
       `CallableToDecoratorsMap.SharedMemory`. Instead, we could just store the
       `callables_to_definitions_map` handle and a set of targets with decorators. *)
    let shared_memory = T.create () in
    let shared_memory_add_only = T.add_only shared_memory in
    let empty_shared_memory = T.AddOnly.create_empty shared_memory_add_only in
    let map =
      List.fold ~init:empty_shared_memory ~f:(fun shared_memory target ->
          match collect_decorators ~callables_to_definitions_map target with
          | Some value -> T.AddOnly.add shared_memory target value
          | None -> shared_memory)
    in
    let shared_memory_add_only =
      Scheduler.map_reduce
        scheduler
        ~policy:scheduler_policy
        ~initial:shared_memory_add_only
        ~map
        ~reduce:(fun left right ->
          T.AddOnly.merge_same_handle_disjoint_keys ~smaller:left ~larger:right)
        ~inputs:callables
        ()
    in
    T.from_add_only shared_memory_add_only


  let is_decorated decorators callable =
    Target.is_normal callable && T.ReadOnly.mem decorators callable


  (* Redirect any call to callable `foo` to its decorated version, if any. *)
  let redirect_to_decorated decorators callable =
    if is_decorated decorators callable then
      Target.set_kind Target.Decorated callable
    else
      callable


  (**
     * For any target that might be decorated, return the expression that calls the decorators.
     *
     * For instance:
     * ```
     * @decorator
     * @decorator_factory(1, 2)
     * def foo(): pass
     * ```
     * would resolve into expression `decorator(decorator_factory(1, 2)(foo))`.
     *)
  let decorated_callable_body decorators callable =
    let create_decorator_call previous_argument decorator =
      Node.create
        ~location:decorator.Node.location
          (* Avoid later registering all callees to the same location. *)
        (Expression.Call
           {
             Call.callee =
               Ast.Expression.delocalize
                 ~create_origin:(fun ~expression attributes ->
                   Some
                     (Origin.create
                        ?base:(Ast.Expression.origin expression)
                        ~location:(Node.location expression)
                        (Origin.ForDecoratedTargetCallee attributes)))
                 decorator
               (* TODO: `decorator` might be a local variable whose definition should be included
                  here, in order to resolve its callee. *);
             arguments = [{ Call.Argument.name = None; value = previous_argument }];
             origin =
               Some (Origin.create ~location:decorator.Node.location Origin.ForDecoratedTarget);
           })
    in
    callable
    |> Option.some_if (Target.is_normal callable)
    >>= ReadOnly.get decorators
    >>| fun { decorators; define_location } ->
    let define_name = Target.define_name_exn callable in
    let attribute_access_location = define_location in
    let attribute_access =
      Ast.Expression.create_name_from_reference
        ~location:attribute_access_location
        ~create_origin:(fun attributes ->
          Some
            (Origin.create
               ~location:attribute_access_location
               (Origin.ForDecoratedTargetCallee attributes)))
        define_name
    in
    let decorated_callable = Target.set_kind Target.Decorated callable in
    let define_name = Reference.create ~prefix:(Target.define_name_exn callable) "@decorated" in
    let return_expression =
      List.fold
        decorators
        ~init:(Expression.Name attribute_access |> Node.create ~location:attribute_access_location)
        ~f:create_decorator_call
    in
    {
      DecoratedDefineBody.decorated_callable;
      define_name;
      return_expression;
      attribute_access;
      attribute_access_location;
    }


  let decorated_callable_define decorators callable =
    decorated_callable_body decorators callable
    >>| fun { DecoratedDefineBody.return_expression; define_name; _ } ->
    {
      Define.signature =
        {
          Define.Signature.name = define_name;
          parameters = [];
          decorators = [];
          return_annotation = None;
          async = false;
          generator = false;
          parent = NestingContext.create_toplevel ();
          (* The class owning the method *)
          legacy_parent = None;
          type_params = [];
        };
      captures = [];
      unbound_names = [];
      body =
        [
          Statement.Return { Return.is_implicit = false; expression = Some return_expression }
          |> Node.create_with_default_location;
        ];
    }


  let register_decorator_defines decorators callables_to_definitions_map =
    let artificial_decorator_defines = Reference.create "artificial_decorator_defines" in
    let read_only_decorators = read_only decorators in
    decorators
    |> targets_with_decorators
    |> List.map ~f:(fun callable ->
           let define =
             Option.value_exn
               (decorated_callable_define read_only_decorators callable)
               ~message:(Format.asprintf "Could not get decorated define for %a" Target.pp callable)
             |> Node.create_with_default_location
           in
           let decorated_callable = Target.set_kind Target.Decorated callable in
           ( decorated_callable,
             CallablesSharedMemory.CallableSignature.from_define_for_pyre1
               ~target:callable
               ~qualifier:artificial_decorator_defines
               define,
             define ))
    |> CallablesSharedMemory.ReadWrite.add_alist_sequential callables_to_definitions_map


  let decorated_targets decorators =
    decorators |> targets_with_decorators |> List.map ~f:(Target.set_kind Target.Decorated)
end
