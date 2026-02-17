(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core
open Ast

module UnannotatedGlobal = struct
  type define_signature = {
    signature: Statement.Define.Signature.t;
    location: Location.WithModule.t;
  }
  [@@deriving sexp, equal, compare]

  type import =
    | ImportModule of {
        target: Reference.t;
        implicit_alias: bool;
      }
    | ImportFrom of {
        from: Reference.t;
        target: Identifier.t;
        implicit_alias: bool;
      }
  [@@deriving sexp, equal, compare]

  type t =
    | SimpleAssign of {
        explicit_annotation: Expression.t option;
        value: Expression.t option;
        target_location: Location.WithModule.t;
      }
    | TypeStatement of {
        type_params: Expression.TypeParam.t list;
        value: Expression.t;
        target_location: Location.WithModule.t;
      }
    | TupleAssign of {
        value: Expression.t option;
        target_location: Location.WithModule.t;
        index: int;
        total_length: int;
      }
    | Imported of import
    | Define of define_signature list
    | Class
  [@@deriving sexp, equal, compare]

  let raw_alist_of_source { Source.statements; module_path = { ModulePath.qualifier; _ }; _ } =
    let open Ast.Statement in
    let open Ast.Expression in
    let rec visit_statement ~qualifier globals { Node.value; location } =
      match value with
      | Statement.Assign
          {
            Assign.target = { Node.value = Expression.Name (Name.Identifier identifier); location };
            annotation;
            value;
            _;
          } ->
          (* For the purpose of handing type aliases, we are parsing statements so we can pass them
             to the TypeAliasEnvironment layer. Here, identifier is the LHS of the statement. Ex: if
             the AST input is the statement ListOrSet = List[T] | Set[T] then we get that the
             identifier is ListOrSet and the annotation is None and the value is List[T] | Set[T].
             Notice that we return the name as part of a tuple, and then we record the expression
             location as the target. *)
          ( Identifier.sanitized identifier,
            SimpleAssign
              {
                explicit_annotation = annotation;
                value;
                target_location = Location.with_module ~module_reference:qualifier location;
              } )
          :: globals
      | Statement.Assign { Assign.target = { Node.value = Expression.Tuple elements; _ }; value; _ }
        ->
          let valid =
            let total_length = List.length elements in
            let is_simple_name index = function
              | { Node.value = Expression.Name (Name.Identifier identifier); location } ->
                  Some
                    ( Identifier.sanitized identifier,
                      TupleAssign
                        {
                          value;
                          target_location =
                            Location.with_module ~module_reference:qualifier location;
                          index;
                          total_length;
                        } )
              | _ -> None
            in
            List.mapi elements ~f:is_simple_name
          in
          List.rev_append (Option.all valid |> Option.value ~default:[]) globals
      | Statement.Import { Import.from = None; imports } ->
          let collect_module_import sofar { Node.value = { Import.name = target; alias }; _ } =
            let implicit_alias, name =
              match alias with
              | None ->
                  (* `import a.b` will bind name `a` in the current module *)
                  true, Reference.as_list target |> List.hd_exn
              | Some alias -> false, alias
            in
            (name, Imported (ImportModule { target; implicit_alias })) :: sofar
          in
          List.fold imports ~init:globals ~f:collect_module_import
      | Statement.Import { Import.from = Some { Node.value = from; _ }; imports } ->
          let collect_name_import sofar { Node.value = { Import.name = target; alias }; _ } =
            (* `target` must be an unqualified identifier *)
            match Reference.show target with
            | "*" ->
                (* Don't register x.* as a global when a user writes `from x import *`. *)
                sofar
            | target ->
                let from =
                  match Reference.as_list from with
                  | ["future"; "builtins"]
                  | ["builtins"] ->
                      Reference.empty
                  | _ -> from
                in
                let implicit_alias, name =
                  match alias with
                  | None -> true, target
                  | Some alias -> false, alias
                in
                (name, Imported (ImportFrom { from; target; implicit_alias })) :: sofar
          in
          List.fold imports ~init:globals ~f:collect_name_import
      | Statement.Class { Class.name; _ } -> (name |> Reference.last, Class) :: globals
      | Statement.Define { Define.signature = { Define.Signature.name; _ } as signature; _ } ->
          ( name |> Reference.last,
            Define
              [{ signature; location = Location.with_module ~module_reference:qualifier location }]
          )
          :: globals
      | Statement.If { If.body; orelse; _ } ->
          (* TODO(T28732125): Properly take an intersection here. *)
          List.fold ~init:globals ~f:(visit_statement ~qualifier) (body @ orelse)
      | Statement.Try { Try.body; handlers; orelse; finally; handles_exception_group = _ } ->
          let globals = List.fold ~init:globals ~f:(visit_statement ~qualifier) body in
          let globals =
            let handlers_statements =
              List.concat_map handlers ~f:(fun { Try.Handler.body; _ } -> body)
            in
            List.fold ~init:globals ~f:(visit_statement ~qualifier) handlers_statements
          in
          let globals = List.fold ~init:globals ~f:(visit_statement ~qualifier) orelse in
          List.fold ~init:globals ~f:(visit_statement ~qualifier) finally
      | Statement.With { With.body; _ } ->
          List.fold ~init:globals ~f:(visit_statement ~qualifier) body
      | Statement.TypeAlias
          {
            TypeAlias.name = { Node.value = Expression.Name (Name.Identifier identifier); location };
            value;
            type_params;
          } ->
          (* We added a new variant to UG to store type params *)
          ( Identifier.sanitized identifier,
            TypeStatement
              {
                type_params;
                value;
                target_location = Location.with_module ~module_reference:qualifier location;
              } )
          :: globals
      | _ -> globals
    in
    List.fold ~init:[] ~f:(visit_statement ~qualifier) statements |> List.rev
end

module Export = struct
  module Name = struct
    type t =
      | Class
      | Define of { is_getattr_any: bool }
      | GlobalVariable
    [@@deriving sexp, equal, compare, hash, show]
  end

  type t =
    | NameAlias of {
        from: Reference.t;
        name: Identifier.t;
      }
    | Module of Reference.t
    | Name of Name.t
  [@@deriving sexp, equal, compare, hash]
end

module ExportMap = struct
  type t = Export.t Identifier.Map.Tree.t [@@deriving sexp, equal]

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

module Metadata = struct
  type t =
    | Explicit of { exports: ExportMap.t }
    | Implicit
  [@@deriving sexp, equal, compare]

  let pp format = function
    | Explicit _ -> Format.fprintf format "EXPLICIT_MODULE"
    | Implicit -> Format.fprintf format "IMPLICIT_MODULE"


  let show = Format.asprintf "%a" pp

  let create_for_testing () = Explicit { exports = ExportMap.empty }

  let create ({ Source.module_path; _ } as source) =
    let is_stub = ModulePath.is_stub module_path in
    let exports =
      let open UnannotatedGlobal in
      let is_getattr_any
          { signature = { Statement.Define.Signature.name; parameters; return_annotation; _ }; _ }
        =
        match Reference.last name with
        | "__getattr__" -> (
            match parameters with
            | [{ Node.value = { Expression.Parameter.annotation = parameter_annotation; _ }; _ }]
              -> (
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
                                  origin = _;
                                } );
                        _;
                      } ) ->
                    true
                | _ -> false)
            | _ -> false)
        | _ -> false
      in
      let unannotated_globals = UnannotatedGlobal.raw_alist_of_source source in
      (* Extract explicit __all__ names so that stub files can re-export names listed in __all__
         even without the `y as y` pattern. *)
      let explicit_dunder_all_names =
        if is_stub then
          let open Expression in
          let extract_dunder_all = function
            | ( "__all__",
                SimpleAssign
                  { value = Some { Node.value = Expression.(List names | Tuple names); _ }; _ } ) ->
                let to_identifier = function
                  | { Node.value = Expression.Constant (Constant.String { value = name; _ }); _ } ->
                      Some name
                  | _ -> None
                in
                Some (List.filter_map ~f:to_identifier names)
            | _ -> None
          in
          List.find_map unannotated_globals ~f:extract_dunder_all
          |> Option.map ~f:(Set.of_list (module String))
        else
          None
      in
      let collect_export sofar (name, unannotated_global) =
        match unannotated_global with
        | Imported (ImportModule { implicit_alias; _ } | ImportFrom { implicit_alias; _ })
          when implicit_alias
               && is_stub
               && not
                    (Option.value_map explicit_dunder_all_names ~default:false ~f:(fun set ->
                         Set.mem set name)) ->
            (* Stub files do not re-export unaliased imports, unless they appear in __all__ *)
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
                Export.(
                  Name (Name.Define { is_getattr_any = List.exists defines ~f:is_getattr_any }))
        | Class -> Identifier.Map.Tree.set sofar ~key:name ~data:Export.(Name Name.Class)
        | SimpleAssign _
        | TypeStatement _
        | TupleAssign _ ->
            Identifier.Map.Tree.set sofar ~key:name ~data:Export.(Name Name.GlobalVariable)
      in
      let init =
        let export_from_missing_classes { Node.value = { Statement.Class.name; _ }; _ } =
          Reference.last name, Export.(Name Name.Class)
        in
        let exports_from_missing_classes missing_classes =
          List.map missing_classes ~f:export_from_missing_classes
          |> Identifier.Map.Tree.of_alist_exn
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
      unannotated_globals |> List.fold ~init ~f:collect_export
    in
    Explicit { exports }


  let create_implicit () = Implicit

  let get_export considered_module name =
    let exports =
      match considered_module with
      | Explicit { exports; _ } -> exports
      | Implicit -> ExportMap.empty
    in
    ExportMap.lookup exports name


  let get_all_exports considered_module =
    match considered_module with
    | Explicit { exports; _ } -> ExportMap.to_alist exports
    | Implicit -> []


  let is_implicit = function
    | Explicit _ -> false
    | Implicit -> true
end

module Components = struct
  type t = {
    module_metadata: Metadata.t;
    class_summaries: ClassSummary.t Ast.Node.t Identifier.Map.Tree.t;
    unannotated_globals: UnannotatedGlobal.t Identifier.Map.Tree.t;
  }
  [@@deriving equal, sexp]

  let unannotated_globals_of_source source =
    let merge_defines unannotated_globals_alist =
      let not_defines, defines =
        List.partition_map unannotated_globals_alist ~f:(function
            | name, UnannotatedGlobal.Define defines -> Either.Second (name, defines)
            | x -> Either.First x)
      in
      let add_to_map sofar (name, defines) =
        let merge_with_existing to_merge = function
          | None -> Some to_merge
          | Some existing -> Some (to_merge @ existing)
        in
        Map.change sofar name ~f:(merge_with_existing defines)
      in
      List.fold defines ~f:add_to_map ~init:Identifier.Map.empty
      |> Map.to_alist
      |> List.map ~f:(fun (name, defines) -> name, UnannotatedGlobal.Define (List.rev defines))
      |> fun defines -> List.append defines not_defines
    in
    let drop_classes unannotated_globals =
      let is_not_class = function
        | _, UnannotatedGlobal.Class -> false
        | _ -> true
      in
      List.filter unannotated_globals ~f:is_not_class
    in
    UnannotatedGlobal.raw_alist_of_source source |> merge_defines |> drop_classes


  let class_summaries_of_source ({ Source.module_path = { ModulePath.qualifier; _ }; _ } as source) =
    (* TODO (T57944324): Support checking classes that are nested inside function bodies *)
    let module ClassCollector = Visit.MakeStatementVisitor (struct
      type t = Statement.Class.t Node.t list

      let visit_children _ = true

      let statement _ sofar = function
        | { Node.location; value = Statement.Statement.Class definition } ->
            { Node.location; value = definition } :: sofar
        | _ -> sofar
    end)
    in
    let classes = ClassCollector.visit [] source in
    let classes =
      match Reference.as_list qualifier with
      | [] -> classes @ MissingFromStubs.missing_builtin_classes
      | ["typing"] -> classes @ MissingFromStubs.missing_typing_classes
      | ["typing_extensions"] -> classes @ MissingFromStubs.missing_typing_extensions_classes
      | _ -> classes
    in
    let definition_to_summary
        { Node.location; value = { Statement.Class.name; _ } as class_definition }
      =
      let primitive = Reference.show name in
      primitive, { Node.location; value = ClassSummary.create ~qualifier class_definition }
    in
    List.map classes ~f:definition_to_summary


  let of_source source =
    let identifier_map_of_alist_prefer_first =
      let prefer_left left _ = left in
      Identifier.Map.Tree.of_alist_reduce ~f:prefer_left
    in
    {
      module_metadata = Metadata.create source;
      class_summaries = class_summaries_of_source source |> identifier_map_of_alist_prefer_first;
      unannotated_globals =
        unannotated_globals_of_source source |> identifier_map_of_alist_prefer_first;
    }


  let implicit_module () =
    {
      module_metadata = Metadata.create_implicit ();
      class_summaries = Identifier.Map.Tree.empty;
      unannotated_globals = Identifier.Map.Tree.empty;
    }
end

let wildcard_exports_of_raw_source ({ Source.module_path; _ } as source) =
  let open Expression in
  let extract_dunder_all = function
    | ( "__all__",
        UnannotatedGlobal.SimpleAssign
          { value = Some { Node.value = Expression.(List names | Tuple names); _ }; _ } ) ->
        let to_identifier = function
          | { Node.value = Expression.Constant (Constant.String { value = name; _ }); _ } ->
              Some name
          | _ -> None
        in
        Some (List.filter_map ~f:to_identifier names)
    | _ -> None
  in
  let unannotated_globals = UnannotatedGlobal.raw_alist_of_source source in
  match List.find_map unannotated_globals ~f:extract_dunder_all with
  | Some names -> names |> List.dedup_and_sort ~compare:Identifier.compare
  | _ ->
      let unannotated_globals =
        (* Stubs have a slightly different rule with re-export *)
        let filter_unaliased_import = function
          | ( _,
              UnannotatedGlobal.Imported
                (ImportModule { implicit_alias; _ } | ImportFrom { implicit_alias; _ }) ) ->
              not implicit_alias
          | _ -> true
        in
        if ModulePath.is_stub module_path then
          List.filter unannotated_globals ~f:filter_unaliased_import
        else
          unannotated_globals
      in
      List.map unannotated_globals ~f:(fun (name, _) -> name)
      |> List.filter ~f:(fun name -> not (String.is_prefix name ~prefix:"_"))
      |> List.dedup_and_sort ~compare:Identifier.compare
