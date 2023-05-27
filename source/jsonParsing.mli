(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Contains utility functions for YoJson and a json parser returning a proper AST with locations. *)

module YojsonUtils : sig
  (** A small helper module to convert {!Yojson.Safe.t} into corresponding OCaml data types. *)

  (** [to_bool_with_default ?default json] tries to convert [json] into a boolean value.

      - If [json] is indeed a boolean, return the corresponding boolean.
      - If [json] is [null] and [default] is specified, return [default].
      - Otherwise, raise {!Yojson.Safe.Util.Type_error}. *)
  val to_bool_with_default : ?default:bool -> Yojson.Safe.t -> bool

  (** [to_int_with_default ?default json] tries to convert [json] into a integer value.

      - If [json] is indeed an int, return the corresponding int.
      - If [json] is [null] and [default] is specified, return [default].
      - Otherwise, raise {!Yojson.Safe.Util.Type_error}. *)
  val to_int_with_default : ?default:int -> Yojson.Safe.t -> int

  (** [to_string_with_default ?default json] tries to convert [json] into a string value.

      - If [json] is indeed an string, return the corresponding string.
      - If [json] is [null] and [default] is specified, return [default].
      - Otherwise, raise {!Yojson.Safe.Util.Type_error}. *)
  val to_string_with_default : ?default:string -> Yojson.Safe.t -> string

  (** [to_path json] tries to convert [json] into a {!PyrePath.t} if [json] is a string value.
      Otherwise, raise {!Yojson.Safe.Util.Type_error}. *)
  val to_path : Yojson.Safe.t -> PyrePath.t

  (** [bool_member ?default name json] tries to look up key [name] in [json] as a dictionary.

      - If [json] is indeed a dictionary and [name] is presented, convert the corresponding value to
        boolean and return it.
      - If [json] is indeed a directory but [name] is not presented, then return [default] if that's
        specified.
      - In all other cases, raise {!Yojson.Safe.Util.Type_error}. *)
  val bool_member : ?default:bool -> string -> Yojson.Safe.t -> bool

  (** [int_member ?default name json] tries to look up key [name] in [json] as a dictionary.

      - If [json] is indeed a dictionary and [name] is presented, convert the corresponding value to
        int and return it.
      - If [json] is indeed a directory but [name] is not presented, then return [default] if that's
        specified.
      - In all other cases, raise {!Yojson.Safe.Util.Type_error}. *)
  val int_member : ?default:int -> string -> Yojson.Safe.t -> int

  (** [string_member ?default name json] tries to look up key [name] in [json] as a dictionary.

      - If [json] is indeed a dictionary and [name] is presented as string, return it.
      - If [json] is indeed a directory but [name] is not presented, then return [default] if that's
        specified.
      - In all other cases, raise {!Yojson.Safe.Util.Type_error}. *)
  val string_member : ?default:string -> string -> Yojson.Safe.t -> string

  (** [path_member ?default name json] tries to look up key [name] in [json] as a dictionary.

      - If [json] is indeed a dictionary and [name] is presented, convert the corresponding value to
        {!PyrePath.t} and return it.
      - In all other cases, raise {!Yojson.Safe.Util.Type_error}. *)
  val path_member : string -> Yojson.Safe.t -> PyrePath.t

  (** [list_member ?default ~f name json] tries to look up key [name] in [json] as a dictionary.

      - If [json] is indeed a dictionary, [name] is presented, and the corresponding value is a
        list, then convert each element of the list using `f` and return the result list.
      - If [json] is indeed a directory but [name] is not presented, then return [default] if that's
        specified.
      - In all other cases, raise {!Yojson.Safe.Util.Type_error}. *)
  val list_member
    :  ?default:'a list ->
    f:(Yojson.Safe.t -> 'a) ->
    string ->
    Yojson.Safe.t ->
    'a list

  (** [optional_list_member ~f name json] tries to look up key [name] in [json] as a dictionary.

      - If [json] is indeed a dictionary, [name] is presented, and the corresponding value is a
        list, then convert each element of the list using `f` and return the result list.
      - If [json] is indeed a directory but [name] is not presented, then return [None].
      - In all other cases, raise {!Yojson.Safe.Util.Type_error}. *)
  val optional_list_member : f:(Yojson.Safe.t -> 'a) -> string -> Yojson.Safe.t -> 'a list option

  (** [string_list_member ?default ~f name json] tries to look up key [name] in [json] as a
      dictionary.

      - If [json] is indeed a dictionary, [name] is presented, convert the corresponding value to a
        list of string.
      - If [json] is indeed a directory but [name] is not presented, then return [default] if that's
        specified.
      - In all other cases, raise {!Yojson.Safe.Util.Type_error}. *)
  val string_list_member : ?default:string list -> string -> Yojson.Safe.t -> string list

  (** [path_list_member ?default ~f name json] tries to look up key [name] in [json] as a
      dictionary.

      - If [json] is indeed a dictionary, [name] is presented, convert the corresponding value to a
        list of {!PyrePath.t}.
      - If [json] is indeed a directory but [name] is not presented, then return [default] if that's
        specified.
      - In all other cases, raise {!Yojson.Safe.Util.Type_error}. *)
  val path_list_member : ?default:PyrePath.t list -> string -> Yojson.Safe.t -> PyrePath.t list

  (** [optional_string_member name json] tries to look up key [name] in [json] as a dictionary.

      - If [json] is indeed a dictionary and [name] is presented, convert the corresponding value to
        a string and return it.
      - If [json] is indeed a directory but [name] is not presented, return [None].
      - In all other cases, raise {!Yojson.Safe.Util.Type_error}. *)
  val optional_string_member : string -> Yojson.Safe.t -> string option

  (** [optional_int_member name json] tries to look up key [name] in [json] as a dictionary.

      - If [json] is indeed a dictionary and [name] is presented, convert the corresponding value to
        a int and return it.
      - If [json] is indeed a directory but [name] is not presented, return [None].
      - In all other cases, raise {!Yojson.Safe.Util.Type_error}. *)
  val optional_int_member : string -> Yojson.Safe.t -> int option

  (** [optional_path_member name json] tries to look up key [name] in [json] as a dictionary.

      - If [json] is indeed a dictionary and [name] is presented, convert the corresponding value to
        a {!PyrePath.t} and return it.
      - If [json] is indeed a directory but [name] is not presented, return [None].
      - In all other cases, raise {!Yojson.Safe.Util.Type_error}. *)
  val optional_path_member : string -> Yojson.Safe.t -> PyrePath.t option
end

module JsonAst : sig
  module Location : sig
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

    val pp_start : Format.formatter -> t -> unit

    val null_location : t

    val from_decoded_range : (int * int) * (int * int) -> t
  end

  module LocationWithPath : sig
    type t = {
      location: Location.t;
      path: PyrePath.t;
    }
    [@@deriving equal, show, compare]

    val create : location:Location.t -> path:PyrePath.t -> t
  end

  exception
    ParseException of {
      message: string;
      location: Location.t;
    }

  module Node : sig
    type 'a t = {
      location: Location.t;
      value: 'a;
    }
    [@@deriving equal, show, compare]
  end

  module ParseError : sig
    type t = {
      message: string;
      location: Location.t;
    }
  end

  module Json : sig
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

    val from_string_exn : string -> t (* throws Parse Exception *)

    val from_string : string -> (t, ParseError.t) result

    val null_node : t

    module Util : sig
      val keys : t -> string list

      val member_exn : string -> t -> t (* throws Type Error *)

      val member : string -> t -> t (* Returns null node if not found *)

      val to_bool : t -> bool option

      val to_string_exn : t -> string

      val to_int_exn : t -> int

      val to_int : t -> int option

      val to_list_exn : t -> t list

      val to_location_exn : t -> Location.t (* throws Type Error *)
    end
  end
end
