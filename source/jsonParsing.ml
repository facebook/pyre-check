(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

module YojsonUtils = struct
  open Yojson.Safe.Util

  let with_default ~extract ~extract_optional ?default json =
    match default with
    | None -> extract json
    | Some default -> extract_optional json |> Option.value ~default


  let to_bool_with_default = with_default ~extract:to_bool ~extract_optional:to_bool_option

  let to_int_with_default = with_default ~extract:to_int ~extract_optional:to_int_option

  let to_string_with_default = with_default ~extract:to_string ~extract_optional:to_string_option

  let to_path json = to_string json |> PyrePath.create_absolute

  (* The absent of explicit `~default` parameter means that the corresponding JSON field is
     mandantory. *)
  let bool_member ?default name json = member name json |> to_bool_with_default ?default

  let int_member ?default name json = member name json |> to_int_with_default ?default

  let string_member ?default name json = member name json |> to_string_with_default ?default

  let optional_member ~f name json =
    member name json
    |> function
    | `Null -> None
    | _ as element -> Some (f element)


  let optional_string_member = optional_member ~f:to_string

  let optional_int_member = optional_member ~f:to_int

  let path_member name json = member name json |> to_path

  let optional_path_member = optional_member ~f:to_path

  let list_member ?default ~f name json =
    member name json
    |> fun element ->
    match element, default with
    | `Null, Some default -> default
    | _, _ -> convert_each f element


  let optional_list_member ~f name json =
    member name json
    |> function
    | `Null -> None
    | element -> Some (convert_each f element)


  let string_list_member = list_member ~f:to_string

  let path_list_member = list_member ~f:to_path
end

module JsonAst = struct
  open Core
  module Result = Core.Result

  module Location = struct
    type position = {
      line: int;
      column: int;
    }
    [@@deriving equal, show, compare]

    and t = {
      start: position;
      stop: position;
    }
    [@@deriving equal, show, compare]

    let null_position = { line = -1; column = -1 }

    let null_location = { start = null_position; stop = null_position }

    let pp_start formatter { start = { line; column }; _ } =
      Format.fprintf formatter "%d:%d" line column


    let from_decoded_range range =
      let (start_line, start_column), (end_line, end_column) = range in
      {
        start = { line = start_line; column = start_column };
        stop = { line = end_line; column = end_column };
      }
  end

  module LocationWithPath = struct
    type t = {
      location: Location.t;
      path: PyrePath.t;
    }
    [@@deriving equal, show, compare]

    let create ~location ~path = { location; path }
  end

  module Node = struct
    type 'a t = {
      location: Location.t;
      value: 'a;
    }
    [@@deriving equal, show, compare]
  end

  exception
    ParseException of {
      message: string;
      location: Location.t;
    }

  module ParseError = struct
    type t = {
      message: string;
      location: Location.t;
    }
  end

  module Json = struct
    type expression =
      [ `Null
      | `Bool of bool
      | `String of string
      | `Float of float
      | `Int of int
      | `List of t list
      | `Assoc of (string * t) list
      ]

    and t = expression Node.t [@@deriving equal, show, compare]

    exception
      TypeError of {
        message: string;
        json: t;
      }

    let null_node = { Node.location = Location.null_location; value = `Null }

    let from_string_exn input =
      let decode decoder =
        match Jsonm.decode decoder with
        | `Lexeme lexeme -> lexeme
        | `Error error ->
            raise
              (ParseException
                 {
                   message = Format.asprintf "%a" Jsonm.pp_error error;
                   location = Location.from_decoded_range (Jsonm.decoded_range decoder);
                 })
        | `End
        | `Await ->
            raise
              (ParseException
                 {
                   message = "Encountered end of file or input";
                   location = Location.from_decoded_range (Jsonm.decoded_range decoder);
                 })
      in
      let rec parse_value value post_parse_function decoder =
        let location = Location.from_decoded_range (Jsonm.decoded_range decoder) in
        match value with
        | `Os -> parse_object [] post_parse_function decoder
        | `As -> parse_array [] post_parse_function decoder
        | (`Null | `Bool _ | `String _ | `Float _) as value ->
            post_parse_function { Node.location; value } decoder
        | _ ->
            raise (ParseException { message = "Encountered unexpected token or element"; location })
      and parse_array current_values post_parse_function decoder =
        let location = Location.from_decoded_range (Jsonm.decoded_range decoder) in
        match decode decoder with
        | `Ae ->
            post_parse_function { Node.value = `List (List.rev current_values); location } decoder
        | element ->
            parse_value
              element
              (fun value -> parse_array (value :: current_values) post_parse_function)
              decoder
      and parse_object current_children post_parse_function decoder =
        let location = Location.from_decoded_range (Jsonm.decoded_range decoder) in
        match decode decoder with
        | `Oe ->
            post_parse_function
              { Node.value = `Assoc (List.rev current_children); location }
              decoder
        | `Name name ->
            parse_value
              (decode decoder)
              (fun value -> parse_object ((name, value) :: current_children) post_parse_function)
              decoder
        | _ ->
            raise (ParseException { message = "Encountered unexpected token or element"; location })
      in
      let decoder = Jsonm.decoder (`String input) in
      parse_value (decode decoder) (fun v _ -> v) decoder


    let from_string input =
      try Result.Ok (from_string_exn input) with
      | ParseException { message; location } -> Result.Error { ParseError.message; location }


    module Util = struct
      let type_error expected json =
        let message current = Format.sprintf "Expected %s, got %s" expected current in
        let obj =
          match json.Node.value with
          | `Assoc _ -> "object"
          | `Int _ -> "integer"
          | `Float _ -> "float"
          | `Null -> "null"
          | `String _ -> "string"
          | `Bool _ -> "boolean"
          | `List _ -> "list"
        in
        TypeError { message = message obj; json }


      let keys node =
        match node.Node.value with
        | `Assoc children -> List.map ~f:fst children
        | _ -> []


      (* Extract a child element with a particular key from a a json object. Raises TypeError for
         other types. *)
      let member_exn key node =
        match node.Node.value with
        | `Assoc children -> (
            match List.Assoc.find children key ~equal:String.equal with
            | Some child -> child
            | None -> null_node)
        | _ -> raise (type_error "object" node)


      let member key node =
        try member_exn key node with
        | _ -> null_node


      let to_bool node =
        match node.Node.value with
        | `Bool b -> Some b
        | _ -> None


      let to_string_exn node =
        match node.Node.value with
        | `String s -> s
        | _ -> raise (type_error "string" node)


      let to_int_exn node =
        match node.Node.value with
        | `Int i -> i
        | `Float f -> int_of_float f
        | _ -> raise (type_error "integer" node)


      let to_int node =
        try Some (to_int_exn node) with
        | _ -> None


      let to_list_exn node =
        match node.Node.value with
        | `List l -> l
        | _ -> raise (type_error "list" node)


      let to_location_exn node =
        match node.Node.value with
        | `Null -> raise (type_error "non-null" node)
        | _ -> node.Node.location
    end
  end
end
