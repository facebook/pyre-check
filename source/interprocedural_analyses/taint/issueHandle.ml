(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Interprocedural
module AccessPath = Analysis.TaintAccessPath

module Sink = struct
  type t =
    | Call of {
        callee: Target.t;
        index: int;
        parameter: AccessPath.Root.t;
      }
    | Global of {
        callee: Target.t;
        index: int;
      }
    | Return
    | LiteralStringSink of Sinks.t
    | ConditionalTestSink of Sinks.t
    | StringFormat of {
        callee: Target.t;
        index: int;
        parameter_index: int;
      }
  [@@deriving compare, equal, hash, sexp, show]

  (* Since we use interning, the default comparison wouldn't be deterministic:
   * i.e, two different runs might produce a different order. This function is
   * meant to produce a consistent order between runs. *)
  let deterministic_compare left right =
    match left, right with
    | Call { callee = left_callee; _ }, Call { callee = right_callee; _ }
    | Global { callee = left_callee; _ }, Global { callee = right_callee; _ }
    | StringFormat { callee = left_callee; _ }, StringFormat { callee = right_callee; _ } ->
        let callee_compare = Target.compare left_callee right_callee in
        if callee_compare <> 0 then
          callee_compare
        else
          compare left right
    | _ -> compare left right


  let strip_callee_parameters = function
    | Call ({ callee; _ } as call) -> Call { call with callee = Target.strip_parameters callee }
    | Global ({ callee; _ } as global) ->
        Global { global with callee = Target.strip_parameters callee }
    | StringFormat ({ callee; _ } as string_format) ->
        StringFormat { string_format with callee = Target.strip_parameters callee }
    | sink -> sink


  let make_call ~call_target:{ CallGraph.CallTarget.target; index; _ } ~root =
    let root =
      (* Ignore extra information in the parameter in order to group issues together. *)
      let open AccessPath.Root in
      match root with
      | LocalResult -> LocalResult
      | PositionalParameter { name; _ } -> NamedParameter { name }
      | NamedParameter { name } -> NamedParameter { name }
      | StarParameter _ -> StarParameter { position = 0 }
      | StarStarParameter _ -> StarStarParameter { excluded = [] }
      | Variable name -> Variable name
      | CapturedVariable name -> CapturedVariable name
    in
    let target = Target.for_issue_handle target in
    Call { callee = target; index; parameter = root }


  let make_global ~call_target:{ CallGraph.CallTarget.target; index; _ } =
    Global { callee = target; index }


  let to_json = function
    | Call { callee; index; parameter } ->
        `Assoc
          [
            "kind", `String "Call";
            "callee", `String (Target.external_name callee);
            "index", `Int index;
            "parameter", `String (AccessPath.Root.show_for_issue_handle parameter);
          ]
    | Global { callee; index } ->
        `Assoc
          [
            "kind", `String "Global";
            "callee", `String (Target.external_name callee);
            "index", `Int index;
          ]
    | Return -> `Assoc ["kind", `String "Return"]
    | LiteralStringSink sink ->
        `Assoc ["kind", `String "LiteralStringSink"; "sink", `String (Sinks.show sink)]
    | ConditionalTestSink sink ->
        `Assoc ["kind", `String "ConditionalTestSink"; "sink", `String (Sinks.show sink)]
    | StringFormat { callee; index; parameter_index } ->
        `Assoc
          [
            "kind", `String "StringFormat";
            "callee", `String (Target.external_name callee);
            "index", `Int index;
            "parameter_index", `Int parameter_index;
          ]
end

module T = struct
  type t = {
    code: int;
    callable: Target.t;
    sink: Sink.t;
  }
  [@@deriving compare, equal, hash, sexp, show]

  let strip_all_callable_parameters ({ callable; sink; _ } as handle) =
    {
      handle with
      callable = Target.strip_parameters callable;
      sink = Sink.strip_callee_parameters sink;
    }


  let deterministic_compare
      { callable = left_callable; code = left_code; sink = left_sink }
      { callable = right_callable; code = right_code; sink = right_sink }
    =
    let callable_compare = Target.compare left_callable right_callable in
    if callable_compare <> 0 then
      callable_compare
    else
      let code_compare = Int.compare left_code right_code in
      if code_compare <> 0 then
        code_compare
      else
        Sink.deterministic_compare left_sink right_sink


  let master_handle { code; callable; sink = sink_handle; _ } =
    let version = 0 (* Increment the version on format change. *) in
    let sink_handle =
      match sink_handle with
      | Call { callee; index; parameter } ->
          Format.asprintf
            "Call|%s|%d|%s"
            (Target.external_name callee)
            index
            (AccessPath.Root.show_for_issue_handle parameter)
      | Global { callee; index } ->
          Format.asprintf "Global|%s|%d" (Target.external_name callee) index
      | Return -> "Return"
      | LiteralStringSink sink -> Format.asprintf "LiteralStringSink|%a" Sinks.pp sink
      | ConditionalTestSink sink -> Format.asprintf "ConditionalTestSink|%a" Sinks.pp sink
      | StringFormat { callee; index; parameter_index } ->
          Format.asprintf
            "StringFormat|%s|%d|%d"
            (Target.external_name callee)
            index
            parameter_index
    in
    let full_handle =
      Format.asprintf "%s:%d:%d:%s" (Target.external_name callable) code version sink_handle
    in
    let hash = full_handle |> Md5.digest_string |> Md5.to_hex in
    let short_handle =
      String.sub
        full_handle
        ~pos:0
        ~len:(min (String.length full_handle) (255 - String.length hash - 1))
    in
    Format.asprintf "%s:%s" short_handle hash


  let name = "IssueHandle"
end

include T
module SerializableMap = Data_structures.SerializableMap.Make (T)
module Set = Stdlib.Set.Make (T)
