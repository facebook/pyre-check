(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core
open Ast
open Statement

module Export = struct
  module Name = struct
    type t =
      | Class
      | Define of { is_getattr_any: bool }
      | GlobalVariable
    [@@deriving sexp, compare, hash, show]
  end

  type t =
    | NameAlias of {
        from: Reference.t;
        name: Identifier.t;
      }
    | Module of Reference.t
    | Name of Name.t
  [@@deriving sexp, compare, hash]
end

module ExportMap = struct
  type t = Export.t Identifier.Map.Tree.t [@@deriving sexp]

  let empty = Identifier.Map.Tree.empty

  let lookup map name =
    match Identifier.Map.Tree.find map name with
    | Some _ as export -> export
    | None -> (
        match name with
        | "__doc__"
        | "__file__"
        | "__name__"
        | "__package__"
        | "__path__"
        | "__dict__" ->
            Some (Export.Name Export.Name.GlobalVariable)
        | _ -> None)


  let to_alist = Identifier.Map.Tree.to_alist

  let compare = Identifier.Map.Tree.compare_direct Export.compare

  let equal = [%compare.equal: t]
end

type t =
  | Explicit of {
      exports: ExportMap.t;
      empty_stub: bool;
    }
  | Implicit of { empty_stub: bool }
[@@deriving eq, sexp, compare]

let empty_stub = function
  | Implicit { empty_stub }
  | Explicit { empty_stub; _ } ->
      empty_stub


let pp format printed_module =
  Format.fprintf format "MODULE[empty_stub = %b]" (empty_stub printed_module)


let show = Format.asprintf "%a" pp

let create_for_testing ~stub = Explicit { exports = ExportMap.empty; empty_stub = stub }

let create
    ({ Source.module_path; typecheck_flags = { Source.TypecheckFlags.local_mode; _ }; _ } as source)
  =
  let is_stub = ModulePath.is_stub module_path in
  let exports =
    let open UnannotatedGlobal in
    let is_getattr_any
        { signature = { Define.Signature.name; parameters; return_annotation; _ }; _ }
      =
      match Reference.last name with
      | "__getattr__" -> (
          match parameters with
          | [{ Node.value = { Expression.Parameter.annotation = parameter_annotation; _ }; _ }] -> (
              match parameter_annotation, return_annotation with
              | ( ( Some
                      {
                        Node.value = Expression.Expression.Name (Expression.Name.Identifier "str");
                        _;
                      }
                  | None ),
                  Some
                    {
                      Node.value =
                        Expression.Expression.Name
                          ( Expression.Name.Identifier "Any"
                          | Expression.Name.Attribute
                              {
                                Expression.Name.Attribute.base =
                                  {
                                    Node.value =
                                      Expression.Expression.Name
                                        (Expression.Name.Identifier "typing");
                                    _;
                                  };
                                attribute = "Any";
                                special = false;
                              } );
                      _;
                    } ) ->
                  true
              | _ -> false)
          | _ -> false)
      | _ -> false
    in
    let collect_export sofar { Collector.Result.name; unannotated_global } =
      match unannotated_global with
      | Imported (ImportModule { implicit_alias; _ } | ImportFrom { implicit_alias; _ })
        when implicit_alias && is_stub ->
          (* Stub files do not re-export unaliased imports *)
          sofar
      | Imported (ImportModule { target; implicit_alias }) ->
          let exported_module_name =
            if implicit_alias then
              Option.value_exn (Reference.head target)
            else
              target
          in
          Identifier.Map.Tree.set sofar ~key:name ~data:(Export.Module exported_module_name)
      | Imported (ImportFrom { from; target; _ }) ->
          Identifier.Map.Tree.set sofar ~key:name ~data:(Export.NameAlias { from; name = target })
      | Define defines ->
          Identifier.Map.Tree.set
            sofar
            ~key:name
            ~data:
              Export.(Name (Name.Define { is_getattr_any = List.exists defines ~f:is_getattr_any }))
      | Class -> Identifier.Map.Tree.set sofar ~key:name ~data:Export.(Name Name.Class)
      | SimpleAssign _
      | TupleAssign _ ->
          Identifier.Map.Tree.set sofar ~key:name ~data:Export.(Name Name.GlobalVariable)
    in
    let init =
      let export_from_missing_classes { Node.value = { Class.name; _ }; _ } =
        Reference.last name, Export.(Name Name.Class)
      in
      let exports_from_missing_classes missing_classes =
        List.map missing_classes ~f:export_from_missing_classes |> Identifier.Map.Tree.of_alist_exn
      in
      (* TODO(stroxler): Deduplicate this logic from
         UnannotatedGlobalEnvironment.ModuleComponents.class_summaries_of_source *)
      match Reference.as_list (ModulePath.qualifier module_path) with
      | [] -> exports_from_missing_classes MissingFromStubs.missing_builtin_classes
      | ["typing"] -> exports_from_missing_classes MissingFromStubs.missing_typing_classes
      | ["typing_extensions"] ->
          exports_from_missing_classes MissingFromStubs.missing_typing_extensions_classes
      | _ -> Identifier.Map.Tree.empty
    in
    Collector.from_source source |> List.fold ~init ~f:collect_export
  in
  Explicit { exports; empty_stub = is_stub && Source.TypecheckFlags.is_placeholder_stub local_mode }


let create_implicit ?(empty_stub = false) () = Implicit { empty_stub }

let get_export considered_module name =
  let exports =
    match considered_module with
    | Explicit { exports; _ } -> exports
    | Implicit _ -> ExportMap.empty
  in
  ExportMap.lookup exports name


let get_all_exports considered_module =
  match considered_module with
  | Explicit { exports; _ } -> ExportMap.to_alist exports
  | Implicit _ -> []


let is_implicit = function
  | Explicit _ -> false
  | Implicit _ -> true
