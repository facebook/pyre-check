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

type t =
  | Pyre1 of {
      handle: T.t;
      callables_to_definitions_map: CallablesSharedMemory.ReadOnly.t;
    }
  | Pyrefly of PyreflyApi.ReadOnly.t

let create ~pyre_api ~callables_to_definitions_map () =
  match pyre_api with
  | PyrePysaApi.ReadOnly.Pyre1 _ -> Pyre1 { handle = T.create (); callables_to_definitions_map }
  | PyrePysaApi.ReadOnly.Pyrefly pyrefly_api -> Pyrefly pyrefly_api


(* Compute the type of the given expression, or retrieve its type from the cache. `callable` is the
   callable whose source code contains the given expression. *)
let compute_or_retrieve_pyre_type
    type_of_expression_shared_memory
    ~pyre_in_context
    ~callable
    expression
  =
  match type_of_expression_shared_memory with
  | Pyrefly _ ->
      failwith "unimplemented: TypeOfExpressionSharedMemory.compute_or_retrieve_pyre_type"
  | Pyre1 { handle; callables_to_definitions_map } -> (
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
          type_)


let compute_or_retrieve_pysa_type
    type_of_expression_shared_memory
    ~pyre_in_context
    ~callable
    expression
  =
  match type_of_expression_shared_memory with
  | Pyre1 _ ->
      compute_or_retrieve_pyre_type
        type_of_expression_shared_memory
        ~pyre_in_context
        ~callable
        expression
      |> PyrePysaApi.PysaType.from_pyre1_type
  | Pyrefly pyrefly_api -> (
      match Ast.Expression.origin expression with
      | Some _ ->
          (* This is an artificial expression that pyrefly doesn't know about. *)
          PyrePysaApi.PysaType.from_pyrefly_type Analysis.PyrePysaEnvironment.PyreflyType.top
      | None ->
          (* TODO(T225700656): pyre_in_context should store the current module qualifier *)
          let { PyreflyApi.CallableMetadata.module_qualifier; _ } =
            PyreflyApi.ReadOnly.get_callable_metadata pyrefly_api (Target.define_name_exn callable)
          in
          PyreflyApi.ReadOnly.get_type_of_expression
            pyrefly_api
            ~qualifier:module_qualifier
            ~location:(Ast.Node.location expression)
          |> Option.value
               ~default:
                 (PyrePysaApi.PysaType.from_pyrefly_type
                    Analysis.PyrePysaEnvironment.PyreflyType.top))
