(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* This module defines an AST preprocessing step that can be used to inline
 * or remove python decorators from decorated functions. This is generally
 * unsound, but is used in the taint analysis (Pysa) to improve the precision
 * of the analysis. *)

open Ast
open Core
open Statement

module Action = struct
  module T = struct
    type t = (* Remove that decorator from decorated function, assuming it is a no-op. *)
      | Discard
    [@@deriving compare, equal, show, sexp]

    let to_mode = function
      | Discard -> "IgnoreDecorator"
  end

  include T
  module Set = Stdlib.Set.Make (T)
end

module Configuration = struct
  type t = {
    actions: Action.t Reference.SerializableMap.t;
    enable_discarding: bool;
  }
  [@@deriving compare, equal, sexp]

  let disable_preprocessing =
    { actions = Reference.SerializableMap.empty; enable_discarding = false }
end

module ConfigurationSharedMemory =
  Memory.WithCache.Make
    (Memory.SingletonKey)
    (struct
      type t = Configuration.t

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = "Configuration for decorator preprocessing."
    end)

module OptionsSharedMemory =
  Memory.WithCache.Make
    (SharedMemoryKeys.StringKey)
    (struct
      type t = bool

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = "Options for decorator preprocessing."
    end)

module DecoratorActionsSharedMemory =
  Memory.WithCache.Make
    (SharedMemoryKeys.ReferenceKey)
    (struct
      type t = Action.t

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = "What action to take on a given decorator."
    end)

let setup_preprocessing ({ Configuration.actions; enable_discarding } as configuration) =
  ConfigurationSharedMemory.add Memory.SingletonKey.key configuration;
  OptionsSharedMemory.add "enable_discarding" enable_discarding;
  Reference.SerializableMap.iter DecoratorActionsSharedMemory.add actions


let get_configuration () = ConfigurationSharedMemory.get Memory.SingletonKey.key

module DecoratorListValue = struct
  type t = Ast.Expression.t list

  let prefix = Hack_parallel.Std.Prefix.make ()

  let description = "Original decorators for a decorated function that has been preprocessed."
end

(** Mapping from a decorated function to its original decorator list. *)
module DecoratedCallableToOriginalDecorators =
  Memory.WithCache.Make (SharedMemoryKeys.ReferenceKey) (DecoratorListValue)

let get_define_decorators { Define.signature = { decorators; _ }; _ } = decorators

let add_original_decorators_mapping ~original_define ~new_define =
  if
    (List.length (get_define_decorators original_define)
    != List.length (get_define_decorators new_define)) [@alert "-deprecated"]
  then
    DecoratedCallableToOriginalDecorators.add
      (Define.name new_define)
      (get_define_decorators original_define)


let original_decorators_from_preprocessed_signature ~define_name ~decorators =
  DecoratedCallableToOriginalDecorators.get define_name |> Option.value ~default:decorators


let has_decorator_action decorator_name action =
  Option.equal Action.equal (DecoratorActionsSharedMemory.get decorator_name) (Some action)


let has_any_decorator_action ~actions decorator =
  match Decorator.from_expression decorator with
  | None -> true
  | Some { Decorator.name = { Node.value = decorator_name; _ }; _ } ->
      Action.Set.exists (fun action -> has_decorator_action decorator_name action) actions


let discard_decorators_for_define define =
  let actions = Action.Set.singleton Action.Discard in
  let decorators =
    define
    |> get_define_decorators
    |> List.filter ~f:(fun decorator -> not (has_any_decorator_action ~actions decorator))
  in
  { define with Define.signature = { define.signature with decorators } }


let preprocess_source source =
  let should_discard = OptionsSharedMemory.get "enable_discarding" |> Option.value ~default:false in
  let { Source.module_path = { ModulePath.raw = { ModulePath.Raw.relative = _; _ }; _ }; _ } =
    source
  in
  let module Transform = Transform.Make (struct
    type t = unit

    let transform_expression_children _ _ = true

    let transform_children state _ = state, true

    let expression _ expression = expression

    let statement _ statement =
      let statement =
        match statement with
        | { Node.value = Statement.Define original_define; location = _ } ->
            let define =
              if should_discard then
                discard_decorators_for_define original_define
              else
                original_define
            in
            let () = add_original_decorators_mapping ~original_define ~new_define:define in
            { statement with value = Statement.Define define }
        | _ -> statement
      in
      (), [statement]
  end)
  in
  if should_discard then
    Transform.transform () source |> Transform.source
  else
    source
