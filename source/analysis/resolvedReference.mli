(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

type export =
  | FromModuleGetattr
  | Exported of Ast.Module.Export.Name.t
[@@deriving sexp, compare, hash]

type t =
  | Module of Ast.Reference.t
  | ModuleAttribute of {
      from: Ast.Reference.t;
      name: Ast.Identifier.t;
      export: export;
      remaining: Ast.Identifier.t list;
    }
  | PlaceholderStub of {
      stub_module: Ast.Reference.t;
      remaining: Ast.Identifier.t list;
    }
[@@deriving sexp, compare, hash]

val as_module_toplevel_reference : t -> Ast.Reference.t option
