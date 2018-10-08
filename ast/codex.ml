(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Pyre
open Expression


(** Given a JSON that's an assoc at the top level, change the key's binding to value,
  * keeping the rest of the attributes the same. *)
let modify_json key value json =
  let open Yojson.Safe.Util in
  let keys = keys json in
  let json_value other_key =
    other_key,
    if other_key = key then
      value
    else
      member other_key json
  in
  `Assoc (List.map ~f:json_value keys)

(* Given an access name such as a.b.c, evaluate to c, the shorthand for the access. *)
let get_access_basename name =
  (* Split is always non-empty since String.split "" evaluates to [""]. *)
  let split = String.split ~on:'.' name in
  List.last_exn split


let access_with_parent name parent =
  parent
  >>| (fun parent -> (Access.show parent) ^ "." ^ (Access.show name))
  |> Option.value ~default:(Access.show name)


module ArgumentData = struct
  type t = {
    args: string list;
    ty: string [@key "type"];
    defaults: string list;
    kwarg: string option;
    vararg: string option;
  }
  [@@deriving eq, show, to_yojson]
end

module CodexNode = struct
  module Function = struct
    type t = {
      docstring: string option [@key "docstring"];
      name: string [@key "name"];
      rank: int [@key "rank"];
      comments: string option [@key "comments"];
      location: int list [@key "location"];
      ty: string [@key "type"];
      source: string [@key "source"];
      symbols: string list [@key "symbols"];
      decorators: string list [@key "decorators"];
      arguments: ArgumentData.t [@key "arguments"];
    }
    [@@deriving eq, show, to_yojson]


    (* Can't derive to_yojson for functions because of symbols. *)
    let to_yojson node =
      let node_json = to_yojson node in
      modify_json "symbols" (`Assoc []) node_json
  end


  module Variable = struct
    type t = {
      name: string;
      ty: string [@key "type"];
      default: string option;
      location: int list;
    }
    [@@deriving eq, show, to_yojson]
  end


  module Class = struct
    type 'a t = {
      docstring: string option;
      name: string;
      rank: int;
      comments: string option;
      location: int list;
      members: 'a list;
      supers: string list;
      mro: string list;
      ty: string;
    }
    [@@deriving eq, show]
  end


  type t =
    | FunctionNode of Function.t
    | ClassNode of t Class.t
    | VariableNode of Variable.t
  [@@deriving eq, show]


  let get_name = function
    | FunctionNode function_node -> get_access_basename function_node.Function.name
    | ClassNode class_node -> get_access_basename class_node.Class.name
    | VariableNode variable_node -> get_access_basename variable_node.Variable.name


  let rec class_node_to_yojson {
      Class.docstring;
      name;
      rank;
      comments;
      location;
      members;
      supers;
      mro;
      ty;
    } =
    let string_option_to_yojson string_option =
      Option.value ~default:`Null (string_option >>| (fun x -> `String x))
    in
    `Assoc
      [
        "docstring", string_option_to_yojson docstring;
        "name", `String name;
        "rank", `Int rank;
        "comments", string_option_to_yojson comments;
        "location", `List (List.map ~f:(fun value  -> `Int value) location);
        "members",
        `Assoc (List.map ~f:(fun member -> (get_name member, to_yojson member)) members);
        "supers", `List (List.map ~f:(fun super -> `String super) supers);
        "mro", `List (List.map ~f:(fun mro -> `String mro) mro);
        "type", `String ty;
      ]
  and to_yojson node =
    match node with
    | ClassNode class_node -> class_node_to_yojson class_node
    | FunctionNode function_node -> Function.to_yojson function_node
    | VariableNode variable_node -> Variable.to_yojson variable_node
end

module PythonModule = struct
  type t = {
    name: string;
    docstring: string option;
    rank: int;
    filename: string;
    members: CodexNode.t list;
  }
  [@@deriving eq, show, to_yojson]
  (* Hijack to_yojson here because members is expected to be an Assoc by the spec. *)
  let to_yojson ({ members; _ } as abstract_module) =
    let members = `Assoc
        (List.map
           ~f:(fun member -> CodexNode.get_name member, CodexNode.to_yojson member)
           members)
    in
    to_yojson abstract_module
    |> fun json -> modify_json "members" members json
end



let arguments_codex_representation parameters =
  (* Evaluates to a triple of [arguments], vararg option, kwarg option *)
  let categorize_parameters parameters =
    let names =
      List.map
        ~f:(fun { Node.value = { Parameter.name; _ }; _ } -> Identifier.show name)
        parameters
    in
    List.filter ~f:(fun name -> not (String.is_prefix ~prefix:"*" name)) names,
    List.find
      ~f:(fun name ->
          (String.is_prefix ~prefix:"*" name) &&
          not (String.is_prefix ~prefix:"**" name))
      names,
    List.find ~f:(String.is_prefix ~prefix:"**") names
  in
  (* If a has a default value, evaluates to [show value], otherwise evaluates to []. *)
  let get_default_value parameter =
    match parameter.Node.value.Parameter.value with
    | None -> []
    | Some value -> [Expression.show value] in
  let args, vararg, kwarg = categorize_parameters parameters in
  {
    ArgumentData.args;
    ty = "arguments";
    defaults = List.concat_map ~f:get_default_value parameters;
    kwarg;
    vararg;
  }


let rec source_statement_codex_representation
    path
    {
      Node.location = {
        Location.start = { Location.line = start_line; column };
        stop = { Location.line = stop_line; _ };
        _;
      };
      value
    } =
  match value with
  | Statement.Class { Statement.Class.body; name; docstring; _ } -> [
      CodexNode.ClassNode {
        CodexNode.Class.docstring;
        name = Access.show name;
        rank = 0;
        comments = None;
        location = [start_line; column];
        members =
          List.concat_map ~f:(source_statement_codex_representation path) body;
        supers = [];
        mro = [];
        ty = "class";
      };
    ]
  | Statement.Define {
      Statement.Define.name;
      docstring;
      parent;
      decorators;
      parameters;
      _;
    } -> [
      let source =
        try
          path
          |> In_channel.read_lines
          |> (fun lines -> List.drop lines (start_line - 1))
          |> (fun lines -> List.take lines (stop_line - start_line + 1))
          |> String.concat ~sep:"\n"
          |> fun source -> source ^ "\n" (* Existing pydex always emits the trailing \n *)
        with
        | Sys_error _ -> ""
      in
      CodexNode.FunctionNode {
        CodexNode.Function.docstring;
        name =
          access_with_parent name parent;
        rank = 0;
        comments = None;
        location = [start_line; column];
        ty = "function";
        source;
        symbols = [];
        decorators = List.map ~f:Expression.show decorators;
        arguments = arguments_codex_representation parameters;
      };
    ]
  | Statement.Assign {
      Statement.Assign.parent;
      value;
      target = { Node.location; value = assign_value };
      _;
    } -> (
      match assign_value with
      | Access access -> [
          CodexNode.VariableNode {
            CodexNode.Variable.name = access_with_parent access parent;
            default = Some (Expression.show value);
            location =
              [location.Location.start.Location.line; location.Location.start.Location.column];
            ty = "variable";
          }
        ]
      | _ -> []
    )
  | _ -> []


let source_to_codex_representation ~configuration { Source.handle; statements; docstring; _ } =
  let path =
    File.Handle.to_path ~configuration handle
    >>| Path.absolute
    |> Option.value ~default:""
  in
  {
    PythonModule.name = Access.show (Source.qualifier ~handle);
    docstring;
    rank = 0;
    filename = path;
    members =
      List.concat_map
        statements
        ~f:(source_statement_codex_representation path);
  }


let source_to_json ~configuration source =
  let representation = source_to_codex_representation ~configuration source in
  (get_access_basename representation.PythonModule.name, PythonModule.to_yojson representation)
