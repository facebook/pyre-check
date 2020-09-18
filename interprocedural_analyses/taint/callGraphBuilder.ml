(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Pyre
open Core
open Analysis
open Ast
module DefaultBuilder = Callgraph.DefaultBuilder

let property_setter_table = Location.WithModule.Table.create ()

let initialize () =
  DefaultBuilder.initialize ();
  Hashtbl.clear property_setter_table


let add_callee
    ~global_resolution
    ~target
    ~callables
    ~arguments
    ~dynamic
    ~qualifier
    ~callee_type
    ~callee
  =
  let resolution =
    Analysis.TypeCheck.resolution
      global_resolution
      (* TODO(T65923817): Eliminate the need of creating a dummy context here *)
      (module TypeCheck.DummyContext)
  in
  (* Add argument callables. *)
  let () =
    let add_callables_of_argument argument =
      let callable_and_implicit =
        Resolution.resolve_expression_to_type resolution argument.Expression.Call.Argument.value
        |> function
        | Type.Callable callable -> Some (callable, None)
        | Type.Parametric
            {
              name = "BoundMethod";
              parameters = [Single (Type.Callable callable); Single implicit];
            } ->
            Some (callable, Some implicit)
        | _ -> None
      in
      match callable_and_implicit with
      | Some (callable, implicit) ->
          DefaultBuilder.add_callee
            ~global_resolution
            ~target:implicit
            ~callables:(Some [callable])
            ~arguments
            ~dynamic:false
            ~qualifier
            ~callee_type
            ~callee:argument.Expression.Call.Argument.value
      | _ -> ()
    in
    List.iter arguments ~f:add_callables_of_argument
  in
  let callables =
    match
      Interprocedural.CallResolution.transform_special_calls
        ~resolution
        { Expression.Call.callee; arguments }
    with
    | Some { Expression.Call.callee = transformed_call; arguments = transformed_arguments } ->
        begin
          match Resolution.resolve_expression_to_type resolution transformed_call with
          | Type.Callable callable ->
              DefaultBuilder.add_callee
                ~global_resolution
                ~target:None
                ~callables:(Some [callable])
                ~arguments:transformed_arguments
                ~dynamic:false
                ~qualifier
                ~callee_type
                ~callee:transformed_call
          (* Some callables are decorated with a decorator that transforms them to a class that
             stores & calls the callable opaquely. From Pysa's perspective, the type of the callable
             is now `DecoratedClass`, and since we normally rely on the callable type to keep the
             target names around, this means that we will no longer have an accurate call graph.

             The SpecialCallResolution has a contract that it'll transform callables into the
             underlying callable name for a small, special set of targets, so if we're in this case,
             we make the angelic assumption that the base expression has a 1:1 match to the actual
             callable that Pysa models. *)
          | annotation
            when Type.Set.mem SpecialCallResolution.recognized_callable_target_types annotation -> (
              let name =
                Node.value transformed_call
                |> (function
                     | Name name -> Some name
                     | _ -> None)
                >>= Ast.Expression.name_to_reference
              in
              match name with
              | Some name ->
                  DefaultBuilder.add_callee
                    ~global_resolution
                    ~target:None
                    ~callables:
                      (Some
                         [
                           {
                             Type.Callable.kind = Named name;
                             implementation =
                               {
                                 Type.Callable.annotation = Type.Any;
                                 parameters = Type.Callable.Undefined;
                               };
                             overloads = [];
                           };
                         ])
                    ~arguments:transformed_arguments
                    ~dynamic:false
                    ~qualifier
                    ~callee_type
                    ~callee:transformed_call
              | _ -> () )
          | _ -> ()
        end;
        callables
    | None -> (
        match target, callables with
        | Some parent, Some ([{ Type.Callable.kind = Named name; _ }] as callables)
          when String.equal (Reference.last name) "__init__" -> (
            (* If we've added a __init__ call, it originates from a constructor. Search for __new__
               and add it manually. This is not perfect (__init__ might have been explicitly called,
               meaning that __new__ wasn't called), but will, in the worst case, lead to
               over-analysis, which will have a perf implication but not a consistency one. *)
            Resolution.resolve_expression_to_type
              resolution
              (Node.create_with_default_location
                 (Expression.Expression.Name
                    (Expression.Name.Attribute
                       {
                         Expression.Name.Attribute.base = Type.expression parent;
                         attribute = "__new__";
                         special = false;
                       })))
            |> function
            | Type.Callable callable -> Some (callable :: callables)
            | _ -> Some callables )
        | _, Some [({ Type.Callable.kind = Anonymous; _ } as callable)] ->
            (* TODO(T66895305): The names of callable protocol callables aren't propagated
               currently, reconstruct them. *)
            if
              Option.is_some
                (GlobalResolution.attribute_from_annotation
                   ~special_method:true
                   global_resolution
                   ~parent:callee_type
                   ~name:"__call__")
            then
              Type.primitive_name callee_type
              >>| fun parent ->
              [
                {
                  callable with
                  Type.Callable.kind =
                    Named (Reference.create ~prefix:(Reference.create parent) "__call__");
                };
              ]
            else
              Some [callable]
        | _ -> callables )
  in
  DefaultBuilder.add_callee
    ~global_resolution
    ~target
    ~callables
    ~arguments
    ~dynamic
    ~qualifier
    ~callee_type
    ~callee


let add_property_callees ~global_resolution ~resolved_base ~attributes ~name ~qualifier ~location =
  DefaultBuilder.add_property_callees
    ~global_resolution
    ~resolved_base
    ~attributes
    ~name
    ~qualifier
    ~location


let add_property_setter_callees ~attribute ~instantiated_parent ~name ~location =
  DefaultBuilder.add_property_setter_callees ~attribute ~instantiated_parent ~name ~location


let get_all_callees () = DefaultBuilder.get_all_callees ()
