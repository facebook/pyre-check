(** Copyright 2016-present Facebook. All rights reserved. **)

open Core
open Sexplib.Std

open Expression
open Statement


module Access = struct
  type t = Expression.t Access.t
  [@@deriving compare, eq, sexp, show]


  module Set = Set.Make(struct
      type nonrec t = t
      let compare = compare
      let sexp_of_t = sexp_of_t
      let t_of_sexp = t_of_sexp
    end)


  module Map = Map.Make(struct
      type nonrec t = t
      let compare = compare
      let sexp_of_t = sexp_of_t
      let t_of_sexp = t_of_sexp
    end)


  include Hashable.Make(struct
      type nonrec t = t
      let compare = compare
      let hash = Hashtbl.hash
      let sexp_of_t = sexp_of_t
      let t_of_sexp = t_of_sexp
    end)


  let create name =
    if String.equal name "..." then
      [Access.Identifier (Identifier.create name)]
    else
      String.split ~on:'.' name
      |> List.map ~f:(fun name -> Access.Identifier (Identifier.create name))


  let create_from_identifiers identifiers =
    List.map ~f:(fun identifier -> Access.Identifier identifier) identifiers


  let access ({ Node.value; _ } as expression) =
    match value with
    | Access access -> access
    | _ -> [Access.Expression expression]
end


type access = Access.t
[@@deriving compare, eq, sexp, show]


module Define = struct
  type t = Statement.t Define.t
  [@@deriving compare, eq, sexp, show]


  let is_method { Define.name; parent; _ } =
    Option.is_some parent && List.length name = 1


  let has_decorator { Define.decorators; _ } decorator =
    let open Expression in
    let rec is_decorator expected actual =
      match expected, actual with
      | (expected_decorator :: expected_decorators),
        { Node.location; value = Access ((Access.Identifier identifier) :: identifiers) }
        when Identifier.show identifier = expected_decorator ->
          if List.is_empty expected_decorators && List.is_empty identifiers then
            true
          else
            is_decorator expected_decorators { Node.location; value = Access identifiers }
      | _ ->
          false
    in
    List.exists ~f:(is_decorator (String.split ~on:'.' decorator)) decorators


  let is_abstract_method define =
    has_decorator define "abstractmethod" ||
    has_decorator define "abc.abstractmethod" ||
    has_decorator define "abstractproperty" ||
    has_decorator define "abc.abstractproperty"


  let is_overloaded_method define =
    has_decorator define "overload" ||
    has_decorator define "typing.overload"


  let is_static_method define =
    has_decorator define "staticmethod"


  let is_class_method define =
    has_decorator define "classmethod"


  let is_constructor { Define.name; parent; _ } =
    let string_name = Access.show name in
    match parent with
    | None -> false
    | Some parent ->
        Access.show parent = string_name ||
        string_name = "__init__"


  let is_untyped { Define.return_annotation; _ } =
    Option.is_none return_annotation


  let contains_call { Define.body; _ } name =
    let matches = function
      | {
          Node.value = Expression {
            Node.value = Expression.Access [
              Expression.Access.Call {
                Node.value = {
                  Expression.Call.name = {
                    Node.value = Expression.Access access;
                    _;
                  };
                  _;
                };
                _;
              };
            ];
            _;
          };
          _;
        } when Access.show access = name ->
          true
      | _ ->
          false
    in
    List.exists ~f:matches body


  let dump define =
    contains_call define "pyre_dump"


  let dump_cfg define =
    contains_call define "pyre_dump_cfg"
end


type define = Define.t
[@@deriving compare, eq, sexp, show]
