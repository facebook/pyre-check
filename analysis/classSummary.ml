(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Statement

type t = {
  name: Reference.t;
  bases: Expression.Call.Argument.t list;
  decorators: Expression.t list;
  attribute_components: Class.AttributeComponents.t;
}
[@@deriving compare, eq, sexp, show, hash]

let create
    ({ Ast.Statement.Class.name = { Node.value = name; _ }; bases; decorators; _ } as definition)
  =
  { name; bases; decorators; attribute_components = Class.AttributeComponents.create definition }


let is_unit_test { name; _ } =
  let name = Reference.show name in
  String.equal name "unittest.TestCase" || String.equal name "unittest.case.TestCase"


let is_protocol { bases; _ } =
  let is_protocol { Expression.Call.Argument.name; value = { Node.value; _ } } =
    let open Expression in
    match name, value with
    | ( None,
        Expression.Call
          {
            callee =
              {
                Node.value =
                  Name
                    (Attribute
                      {
                        base =
                          {
                            Node.value =
                              Name
                                (Attribute
                                  {
                                    base = { Node.value = Name (Identifier typing); _ };
                                    attribute = "Protocol";
                                    _;
                                  });
                            _;
                          };
                        attribute = "__getitem__";
                        _;
                      });
                _;
              };
            _;
          } )
    | ( None,
        Name
          (Attribute
            { base = { Node.value = Name (Identifier typing); _ }; attribute = "Protocol"; _ }) )
      when String.equal typing "typing" || String.equal typing "typing_extensions" ->
        true
    | _ -> false
  in
  List.exists ~f:is_protocol bases


let has_decorator { decorators; _ } decorator =
  Expression.exists_in_list ~expression_list:decorators decorator


let is_final definition = has_decorator definition "typing.final"

let is_abstract { bases; _ } =
  let abstract_metaclass { Expression.Call.Argument.value; _ } =
    let open Expression in
    match value with
    | {
     Node.value =
       Expression.Name
         (Attribute
           { base = { Node.value = Name (Identifier "abc"); _ }; attribute = "ABCMeta" | "ABC"; _ });
     _;
    } ->
        true
    | _ -> false
  in
  List.exists bases ~f:abstract_metaclass
