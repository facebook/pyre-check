(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

module Key = struct
  type t = {
    expression_identifier: ExpressionIdentifier.t;
    (* Types in a parameterized version of a regular target are the same as the regular target. Race
       conditions are possible, since multiple processes can write into the same key, but this
       should be fine since they all write the same value. *)
    callable: Target.Regular.t;
  }
  [@@deriving compare, sexp]

  let to_string key = key |> sexp_of_t |> Core.Sexp.to_string
end

(* `Type.t` is a function of `Expression.t`, which can be uniquely identified by file paths and
   `ExpressionIdentifier.t`, due to artificial expressions that may share the same expression
   locations. File paths are approximated by callables. *)
module T =
  Hack_parallel.Std.SharedMemory.FirstClass.WithCache.Make
    (Key)
    (struct
      type t = Type.t

      let prefix = Hack_parallel.Std.Prefix.make ()

      let description = "caching type queries over expressions"
    end)

type t = {
  handle: T.t;
  callables_to_definitions_map: Target.CallablesSharedMemory.ReadOnly.t;
}

let create ~callables_to_definitions_map () = { handle = T.create (); callables_to_definitions_map }

(* Compute the type of the given expression, or retrieve its type from the cache. `callable` is the
   callable whose source code contains the given expression. *)
let compute_or_retrieve_type
    { handle; callables_to_definitions_map }
    ~pyre_in_context
    ~callable
    expression
  =
  let key =
    {
      Key.callable = Target.get_regular callable;
      expression_identifier = ExpressionIdentifier.of_expression expression;
    }
  in
  match T.get handle key with
  | Some type_ -> type_
  | None ->
      let type_ =
        CallResolution.resolve_ignoring_errors
          ~pyre_in_context
          ~callables_to_definitions_map
          expression
      in
      let () = T.add handle key type_ in
      type_
