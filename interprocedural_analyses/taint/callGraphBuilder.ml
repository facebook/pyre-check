(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

module DefaultBuilder = Analysis.Callgraph.DefaultBuilder

let initialize () = DefaultBuilder.initialize ()

let add_callee ~global_resolution ~target ~callables ~dynamic ~callee =
  DefaultBuilder.add_callee ~global_resolution ~target ~callables ~dynamic ~callee


let add_property_callees ~global_resolution ~resolved_base ~attributes ~name ~location =
  DefaultBuilder.add_property_callees ~global_resolution ~resolved_base ~attributes ~name ~location


let get_all_callees () = DefaultBuilder.get_all_callees ()
