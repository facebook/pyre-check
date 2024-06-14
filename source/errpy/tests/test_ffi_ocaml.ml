(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
module Ast = Errpy.Ast
module Parser = Errpy.Parser

[@@@ocaml.warning "-42"]

(* Simple tests to ensure we can invoke ERRPY via the FFI interface from OCaml and marshall the
   output ast to an OCaml datastructure and handle comments correctly *)

let test_parser_ast input_code ?(expected_recoverable_errors = []) expected_ast =
  let print_recoverable_errors recoverable_errors =
    recoverable_errors
    |> List.map Ast.show_recoverableerrorwithlocation
    |> String.concat ", "
    |> Format.asprintf "[%s]"
  in
  match Parser.parse_module input_code with
  | Ok (mod_, recoverable_errors) ->
      assert_equal expected_ast mod_ ~printer:Ast.show_mod_;
      assert_equal expected_recoverable_errors recoverable_errors ~printer:print_recoverable_errors
  | Error err -> assert_failure err


let expected_output_simple =
  Ast.Module
    {
      body =
        [
          {
            Ast.desc =
              Ast.Assign
                {
                  targets =
                    [
                      {
                        Ast.desc = Ast.Name { id = "a"; ctx = Ast.Store };
                        lineno = 1;
                        col_offset = 0;
                        end_lineno = Some 1;
                        end_col_offset = Some 1;
                      };
                    ];
                  value =
                    {
                      Ast.desc = Ast.Constant { value = Some (Ast.Num (Ast.Int 8)); kind = None };
                      lineno = 1;
                      col_offset = 2;
                      end_lineno = Some 1;
                      end_col_offset = Some 3;
                    };
                  type_comment = None;
                };
            lineno = 1;
            col_offset = 0;
            end_lineno = Some 1;
            end_col_offset = Some 3;
          };
        ];
      type_ignores = [];
    }


let () = test_parser_ast "a=8" expected_output_simple

let expected_output_comments =
  Ast.Module
    {
      body =
        [
          {
            Ast.desc =
              Ast.FunctionDef
                {
                  name = "foo";
                  args =
                    {
                      Ast.posonlyargs = [];
                      args =
                        [
                          {
                            Ast.arg = "a";
                            annotation = None;
                            type_comment = None;
                            lineno = 2;
                            col_offset = 4;
                            end_lineno = Some 2;
                            end_col_offset = Some 5;
                          };
                          {
                            Ast.arg = "skip_loads";
                            annotation = None;
                            type_comment = None;
                            lineno = 4;
                            col_offset = 4;
                            end_lineno = Some 4;
                            end_col_offset = Some 14;
                          };
                        ];
                      vararg = None;
                      kwonlyargs = [];
                      kw_defaults = [];
                      kwarg = None;
                      defaults =
                        [
                          {
                            Ast.desc =
                              Ast.Constant { value = Some (Ast.Num (Ast.Int 34)); kind = None };
                            lineno = 2;
                            col_offset = 6;
                            end_lineno = Some 2;
                            end_col_offset = Some 8;
                          };
                          {
                            Ast.desc = Ast.Constant { value = Some (Ast.Bool true); kind = None };
                            lineno = 4;
                            col_offset = 15;
                            end_lineno = Some 4;
                            end_col_offset = Some 19;
                          };
                        ];
                    };
                  body =
                    [
                      {
                        Ast.desc = Ast.Pass;
                        lineno = 6;
                        col_offset = 4;
                        end_lineno = Some 6;
                        end_col_offset = Some 8;
                      };
                    ];
                  decorator_list = [];
                  type_params = [];
                  returns = None;
                  type_comment = None;
                };
            lineno = 1;
            col_offset = 0;
            end_lineno = Some 6;
            end_col_offset = Some 8;
          };
        ];
      type_ignores = [];
    }


let () =
  test_parser_ast
    "def foo(\n    a=34,\n    # a comment here\n    skip_loads=True,\n):\n    pass"
    expected_output_comments


let expected_output_resolvable_error =
  Ast.Module
    {
      body =
        [
          {
            Ast.desc =
              Ast.Expr
                {
                  Ast.desc =
                    Ast.Tuple
                      {
                        elts =
                          [
                            {
                              Ast.desc = Ast.Name { id = "garbage"; ctx = Ast.Load };
                              lineno = 1;
                              col_offset = 2;
                              end_lineno = Some 1;
                              end_col_offset = Some 9;
                            };
                            {
                              Ast.desc = Ast.Name { id = "a"; ctx = Ast.Load };
                              lineno = 1;
                              col_offset = 13;
                              end_lineno = Some 1;
                              end_col_offset = Some 14;
                            };
                          ];
                        ctx = Ast.Load;
                      };
                  lineno = 1;
                  col_offset = 0;
                  end_lineno = Some 1;
                  end_col_offset = Some 15;
                };
            lineno = 1;
            col_offset = 0;
            end_lineno = Some 1;
            end_col_offset = Some 15;
          };
        ];
      type_ignores = [];
    }


let expected_errors_resolvable_error =
  [
    {
      Ast.error = "SyntaxError: invalid syntax";
      lineno = 1;
      col_offset = 11;
      end_lineno = 1;
      end_col_offset = 12;
    };
  ]


let () =
  test_parser_ast
    "( garbage a, a)"
    ~expected_recoverable_errors:expected_errors_resolvable_error
    expected_output_resolvable_error


let expected_ast_nullable_kw_defaults =
  Ast.Module
    {
      body =
        [
          {
            Ast.desc =
              Ast.FunctionDef
                {
                  name = "foo";
                  args =
                    {
                      Ast.posonlyargs = [];
                      args = [];
                      vararg =
                        Some
                          {
                            Ast.arg = "d";
                            annotation = None;
                            type_comment = None;
                            lineno = 1;
                            col_offset = 9;
                            end_lineno = Some 1;
                            end_col_offset = Some 10;
                          };
                      kwonlyargs =
                        [
                          {
                            Ast.arg = "e";
                            annotation = None;
                            type_comment = None;
                            lineno = 1;
                            col_offset = 12;
                            end_lineno = Some 1;
                            end_col_offset = Some 13;
                          };
                          {
                            Ast.arg = "f";
                            annotation = None;
                            type_comment = None;
                            lineno = 1;
                            col_offset = 15;
                            end_lineno = Some 1;
                            end_col_offset = Some 16;
                          };
                        ];
                      kw_defaults =
                        (* kw_defaults is defined as: expr option list; *)
                        [
                          None;
                          Some
                            {
                              Ast.desc =
                                Ast.Constant { value = Some (Ast.Num (Ast.Int 3)); kind = None };
                              lineno = 1;
                              col_offset = 17;
                              end_lineno = Some 1;
                              end_col_offset = Some 18;
                            };
                        ];
                      kwarg = None;
                      defaults = [];
                    };
                  body =
                    [
                      {
                        Ast.desc = Ast.Pass;
                        lineno = 1;
                        col_offset = 21;
                        end_lineno = Some 1;
                        end_col_offset = Some 25;
                      };
                    ];
                  decorator_list = [];
                  type_params = [];
                  returns = None;
                  type_comment = None;
                };
            lineno = 1;
            col_offset = 0;
            end_lineno = Some 1;
            end_col_offset = Some 25;
          };
        ];
      type_ignores = [];
    }


(* kw_defaults is defined as: expr option list; in the rust and ocaml ast files *)
let () = test_parser_ast "def foo(*d, e, f=3): pass" expected_ast_nullable_kw_defaults
