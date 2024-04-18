(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* ResolvedReference.t represents the result of asking what a particular
 * fully-qualified name means. The possibilities are:
 * - a module (for example asyncio.tasks). Note that the python runtime treats
 *   modules as attributes of their parents but Pyre does not currently,
 *   instead we always work with fully qualified names.
 * - an attribute of a module, in which case the fully-qualified name can be
 *   partitioned into its module component and the remainder (for example the
 *   fully-qualified name of a nested class can have many names after the
 *   module component)
 * - a name masked by a placeholder stub, which Pyre should treat as `Any`.
 *)

open Core

type export =
  | FromModuleGetattr
  | Exported of Ast.Module.Export.Name.t
[@@deriving show, sexp, compare, hash]

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
[@@deriving show, sexp, compare, hash]

(* In type-checking code, we want to short-circuit recursion for Expression.Name resolution whenever
   the name points to a top-level module attribute. We do not want to short-circuit for nested
   attributes. This function facilitates making that decision. *)
let as_module_toplevel_reference = function
  | Module qualifier -> Some qualifier
  | PlaceholderStub { stub_module; remaining } ->
      Some (Ast.Reference.combine stub_module (Ast.Reference.create_from_list remaining))
  | ModuleAttribute { from; name; remaining = []; _ } ->
      Some (Ast.Reference.create ~prefix:from name)
  | ModuleAttribute _ -> None
