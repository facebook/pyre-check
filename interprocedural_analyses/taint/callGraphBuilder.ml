(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Analysis
open Ast
module DefaultBuilder = Callgraph.DefaultBuilder

let property_setter_table = Location.WithModule.Table.create ()

let initialize () =
  DefaultBuilder.initialize ();
  Hashtbl.clear property_setter_table


let add_callee ~global_resolution ~target ~callables ~arguments ~dynamic ~qualifier ~callee =
  let callables =
    match
      Interprocedural.CallResolution.transform_special_calls { Expression.Call.callee; arguments }
    with
    | Some { Expression.Call.callee = transformed_call; arguments = transformed_arguments } ->
        let resolution = Analysis.TypeCheck.resolution global_resolution () in
        begin
          match Resolution.resolve resolution transformed_call with
          | Type.Callable callable ->
              DefaultBuilder.add_callee
                ~global_resolution
                ~target:None
                ~callables:(Some [callable])
                ~arguments:transformed_arguments
                ~dynamic:false
                ~qualifier
                ~callee:transformed_call
          | _ -> ()
        end;
        callables
    | None -> (
        match target, callables with
        | Some parent, Some ([{ Type.Callable.kind = Named name; _ }] as callables)
          when Reference.last name = "__init__" -> (
            (* If we've added a __init__ call, it originates from a constructor. Search for __new__
               and add it manually. This is not perfect (__init__ might have been explicitly called,
               meaning that __new__ wasn't called), but will, in the worst case, lead to
               over-analysis, which will have a perf implication but not a consistency one. *)
            let resolution = Analysis.TypeCheck.resolution global_resolution () in
            Resolution.resolve
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
        | _ -> callables )
  in
  DefaultBuilder.add_callee
    ~global_resolution
    ~target
    ~callables
    ~arguments
    ~dynamic
    ~qualifier
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
