(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Pyre

module Class = AnnotatedClass
module Attribute = Class.Attribute
module Method = Class.Method
module Define = AnnotatedDefine
module Signature = AnnotatedSignature


type t = Access.t
[@@deriving compare, eq, sexp, show, hash]


let create access =
  access


type origin =
  | Instance of Attribute.t
  | Module of Access.t
[@@deriving show]


type element =
  | Signature of {
      signature: AnnotatedSignature.t;
      callees: Type.Callable.t list;
      arguments: Argument.t list;
    }
  | Attribute of { attribute: Access.t; origin: origin; defined: bool }
  | NotCallable of Type.t
  | Value
[@@deriving show]


module State = struct
  (* Keep track of objects whose type might be determined later on or that might serve as implicit
     argument to a call. *)
  type target = {
    access: Access.t;
    annotation: Type.t;
  }

  type 'accumulator t = {
    resolution: Resolution.t;
    accumulator: 'accumulator;
    f: 'accumulator
      -> resolution: Resolution.t
      -> resolved: Annotation.t
      -> element: element
      -> lead: Access.t
      -> 'accumulator;
    resolved: Annotation.t option;
    target: target option;
    continue: bool;
  }


  let create ~resolution ~accumulator ~f ?resolved ?target ?(continue = true) () =
    { resolution; accumulator; f; resolved; target; continue }


  let step
      ({ resolution; accumulator; f; _ } as state)
      ?element
      ?resolved
      ?target
      ?(continue = true)
      ~lead
      () =
    let accumulator =
      let resolved = Option.value resolved ~default:(Annotation.create Type.Top) in
      let element = Option.value element ~default:Value in
      f accumulator ~resolution ~resolved ~element ~lead
    in
    {
      state with
      accumulator;
      resolved;
      target;
      continue;
    }


  let abort state ?element ~lead () =
    step state ?element ~continue:false ~lead ()


  let annotation { annotation; _ } =
    annotation
end


(* Fold over an access path. Callbacks will be passed the current `accumulator`, the current
    `annotations`, the `resolved` type of the expression so far, as well as the kind of `element`
    we're currently folding over. *)
let fold ~resolution ~initial ~f access =
  (* Resolve `super()` calls. *)
  let access, resolution =
    match access with
    | (Access.Identifier name) :: (Access.Call _) :: tail
      when Identifier.show name = "super" ->
        (Resolution.parent resolution
         >>| (fun parent ->
             Access.expression parent
             |> Resolution.parse_annotation resolution)
         >>= Resolution.class_representation resolution
         >>| (fun { Resolution.successors; _ } -> successors)
         >>|  List.filter
           ~f:(fun definition -> Option.is_some (Resolution.class_definition resolution definition))
         >>| List.hd
         >>| function
         | Some superclass ->
             let super = Access.Identifier (Identifier.create "$super") in
             let resolution =
               Resolution.set_local
                 resolution
                 ~access:[super]
                 ~annotation:(Annotation.create superclass)
             in
             super :: tail, resolution
         | None ->
             access, resolution)
        |> Option.value ~default:(access, resolution)
    | _ ->
        access, resolution
  in

  (* Resolve `type()` calls. *)
  let access, resolution =
    match access with
    | (Access.Identifier name) :: (Access.Call { Node.value = [{ Argument.value; _ }]; _ }) :: tail
      when Identifier.show name = "type" ->
        let access = Access.Identifier (Identifier.create "$type") in
        let resolution =
          let annotation =
            Resolution.resolve resolution value
            |> Type.meta
            |> Annotation.create
          in
          Resolution.set_local resolution ~access:[access] ~annotation
        in
        access :: tail, resolution
    | _ ->
        access, resolution
  in

  (* Resolve function redirects. E.g. resolve `abs(x)` to `x.__abs__()`. *)
  let access =
    match access with
    | (Access.Identifier name) :: (Access.Call { Node.value = arguments; location }) :: tail ->
        Access.redirect ~arguments ~location ~name:[Access.Identifier name]
        >>| (fun redirect -> redirect @ tail)
        |> Option.value ~default:access
    | _ ->
        access
  in

  (* Resolve module exports. *)
  let access =
    (* This is necessary due to export/module name conflicts: P59503092 *)
    let widening_threshold = 25 in
    let rec resolve_exports_fixpoint ~access ~visited ~count =
      if Set.mem visited access || count > widening_threshold then
        access
      else
        let rec resolve_exports ~lead ~tail =
          match tail with
          | head :: tail ->
              Resolution.module_definition resolution lead
              >>| (fun definition ->
                  match
                    Module.aliased_export definition [head] with
                  | Some export ->
                      export @ tail
                  | _ ->
                      resolve_exports ~lead:(lead @ [head]) ~tail)
              |> Option.value ~default:access
          | _ ->
              access
        in
        match access with
        | head :: tail ->
            resolve_exports_fixpoint
              ~access:(resolve_exports ~lead:[head] ~tail)
              ~visited:(Set.add visited access)
              ~count:(count + 1)
        | _ ->
            access
    in
    resolve_exports_fixpoint ~access ~visited:Access.Set.empty ~count:0
  in

  let rec fold ~state ~lead ~tail =
    let { State.accumulator; resolved; target; resolution; _ } = state in
    let find_method ~parent ~name =
      parent
      |> Resolution.class_definition resolution
      >>| Class.create
      >>| Class.attribute ~resolution ~name ~instantiated:parent
      >>= fun attribute -> Option.some_if (Attribute.defined attribute) attribute
      >>| Attribute.annotation
      >>| Annotation.annotation
      >>= function
      | Type.Callable callable ->
          Some callable
      | _ ->
          None
    in
    let resolve_callables
        ~implicit_annotation
        ~callables
        ~arguments:{ Node.location; value = arguments } =
      let signatures =
        let signature callable =
          let resolve_independent_callable () =
            let signature = Signature.select ~arguments ~resolution ~callable in
            let backup () =
              let name, backup_argument =
                match target, List.rev lead with
                | Some _, (Access.Identifier _ as name) :: rest -> [name], rest
                | _ -> [], []
              in
              match arguments, Access.backup ~name with
              | [{ Argument.value; _ }], Some name ->
                  let arguments = [{
                      Argument.value = {
                        Node.location;
                        value = Expression.Access backup_argument;
                      };
                      name = None;
                    }]
                  in
                  Resolution.resolve resolution value
                  |> fun annotation -> find_method ~parent:annotation ~name
                  >>| fun callable -> Signature.select ~arguments ~resolution ~callable
              | _ ->
                  None
            in

            match signature, target with
            | Signature.NotFound _, Some _ ->
                backup ()
                |> Option.value ~default:signature
            | _ ->
                signature
          in
          let resolve_typed_dictionary_get_item_callable ~fields ~name =
            match arguments with
            | {
              Record.Argument.value = {
                Node.value = Expression.String { value = key; _ };
                _;
              };
              _;
            } :: [] ->
                begin
                  match List.find fields ~f:(fun { Type.name; _ } -> name = key) with
                  | Some { annotation; _ } ->
                      Signature.Found {
                        callable = Type.Callable.with_return_annotation callable ~annotation;
                        constraints = Type.Map.empty;
                      }
                  | None ->
                      Signature.NotFound {
                        callable =
                          Type.Callable.with_return_annotation
                            callable
                            ~annotation:Type.Top;
                        reason =
                          Some (Signature.TypedDictionaryMissingKey {
                              typed_dictionary_name = name;
                              missing_key = key;
                            });
                      }
                end
            | _ ->
                let keys = List.map fields ~f:(fun { name; _ } -> name) in
                Signature.NotFound {
                  callable = Type.Callable.with_return_annotation callable ~annotation:Type.Top;
                  reason = Some (Signature.TypedDictionaryAccessWithNonLiteral keys);
                }
          in
          let resolve_typed_dictionary_set_item_callable ~fields ~name =
            match arguments with
            | { Argument.value = { Node.value = Expression.String { value = key; _ }; _ }; _ }
              :: _value :: [] ->
                begin
                  let annotation =
                    match List.find fields ~f:(fun { Type.name; _ } -> name = key) with
                    | Some { annotation; _ } -> Some annotation
                    | _ -> None
                  in
                  match annotation with
                  | Some annotation ->
                      Type.TypedDictionary.setter ~callable ~annotation
                      |> fun callable -> Signature.select ~arguments ~resolution ~callable
                  | None ->
                      Signature.NotFound {
                        callable;
                        reason =
                          Some (Signature.TypedDictionaryMissingKey {
                              typed_dictionary_name = name;
                              missing_key = key;
                            });
                      }
                end
            | _ ->
                let keys = List.map fields ~f:(fun { name; _ } -> name) in
                Signature.NotFound {
                  callable;
                  reason = Some (Signature.TypedDictionaryAccessWithNonLiteral keys);
                }
          in
          let tail_is access name =
            match List.last access with
            | Some (Access.Identifier get_item) -> Identifier.show get_item = name
            | _ -> false
          in
          match implicit_annotation, callable with
          | Some (Type.TypedDictionary { fields; name }),
            { Type.Record.Callable.kind = Named access; _ }
            when tail_is access "__getitem__" ->
              resolve_typed_dictionary_get_item_callable ~fields ~name
          | Some (Type.TypedDictionary { fields; name }),
            { Type.Record.Callable.kind = Named access; _ }
            when tail_is access "__setitem__" ->
              resolve_typed_dictionary_set_item_callable ~fields ~name
          | _ ->
              resolve_independent_callable ()
        in
        List.map callables ~f:signature
      in

      (* Determine type. E.g. `[].append(1)` will determine the list to be of type `List[int]`. *)
      let resolution =
        let update_resolution resolution signature =
          target
          >>= (fun { State.access; annotation } ->
              Signature.determine signature ~resolution ~annotation
              >>| (fun determined ->
                  match access with
                  | [Access.Identifier _] ->
                      Resolution.set_local
                        resolution
                        ~access
                        ~annotation:(Annotation.create determined)
                  | _ ->
                      resolution))
          |> Option.value ~default:resolution
        in
        List.fold signatures ~init:resolution ~f:update_resolution
      in

      let signature, callees =
        let not_found = function | Signature.NotFound _ -> true | _ -> false in
        match List.partition_tf signatures ~f:not_found with
        | [], [] ->
            None, []
        | not_found :: _, _ ->
            Some not_found, []
        | [], (Signature.Found { callable; constraints } ) :: found ->
            let callables, all_constraints =
              let extract = function
                | Signature.Found { callable; constraints } -> callable, constraints
                | _ -> failwith "Not all sigantures were found."
              in
              List.map found ~f:extract
              |> List.unzip
            in
            let callees = callable :: callables in
            let signature =
              let constraints =
                let join_constraints left right =
                  let merge ~key:_ = function
                    | `Left value -> Some value
                    | `Right value -> Some value
                    | `Both (left, right) -> Some (Resolution.join resolution left right)
                  in
                  Map.merge left right ~f:merge
                in
                List.fold all_constraints ~init:constraints ~f:join_constraints
              in
              let joined_callable =
                List.map callables ~f:(fun callable -> Type.Callable callable)
                |> List.fold ~init:(Type.Callable callable) ~f:(Resolution.join resolution)
              in
              match joined_callable with
              | Type.Callable callable ->
                  Signature.Found { callable; constraints }
              | _ ->
                  Signature.NotFound { callable; reason = None }
            in
            Some signature, callees
        | _ ->
            None, []
      in

      match signature with
      | Some
          (Signature.Found { callable = { implementation = { annotation; _ }; _ }; _ } as signature)
      | Some
          (Signature.NotFound {
              callable = { implementation = { annotation; _ }; _ };
              _;
            } as signature)
        when Type.is_resolved annotation ->
          State.step
            { state with State.resolution }
            ~element:(Signature { signature; callees; arguments })
            ~resolved:(Annotation.create annotation)
            ~lead
            ()
      | _ ->
          State.abort state ~lead ()
    in

    let local_attributes ~resolved ~lead ~name =
      let annotation = Annotation.annotation resolved in
      let attributes =
        let find_attribute annotation =
          let instantiated, class_attributes =
            if Type.is_meta annotation then
              Type.single_parameter annotation, true
            else
              annotation, false
          in
          Resolution.class_definition resolution instantiated
          >>| Class.create
          >>| (fun definition ->
              let attribute =
                Class.attribute
                  definition
                  ~transitive:true
                  ~class_attributes
                  ~resolution
                  ~name
                  ~instantiated
              in
              if not (Attribute.defined attribute) then
                Class.fallback_attribute definition ~resolution ~access:name
                |> Option.value ~default:attribute
              else
                attribute)
          |> Option.to_list
        in
        match annotation with
        | Type.Union annotations ->
            List.map annotations ~f:find_attribute
            |> List.concat
        | Type.Variable { constraints = Bound annotation; _ }
        | annotation ->
            find_attribute annotation
      in
      let resolved =
        match attributes with
        | [attribute] ->
            begin
              let is_class = Type.is_meta annotation in
              match Resolution.get_local resolution ~access:lead ~global_fallback:is_class with
              | Some ({
                  Annotation.mutability = Immutable { Annotation.scope = Global; original };
                  _;
                } as local) when Type.is_unknown original ->
                  let original = Annotation.original (Attribute.annotation attribute) in
                  { local with mutability = Immutable { Annotation.scope = Global; original } }
              | Some local ->
                  local
              | None ->
                  Attribute.annotation attribute
            end
        | _ ->
            attributes
            |> List.map ~f:Attribute.annotation
            |> List.map ~f:Annotation.annotation
            |> Type.union
            |> Annotation.create
      in
      resolved, attributes
    in

    match tail with
    | head :: tail ->
        let qualifier = lead in
        let lead = lead @ [head] in

        let ({ State.continue; accumulator; _ } as state) =
          match resolved, head with
          (* Typed context: operations are on a class definition. *)
          | Some resolved, Access.Call arguments ->
              (* Callable invocation. *)
              let resolved = Annotation.annotation resolved in
              let target =
                if Type.is_meta resolved then
                  Some { State.access = lead; annotation = Type.single_parameter resolved }
                else
                  target
              in
              let implicit_annotation, callables =
                match resolved with
                | meta when Type.is_meta resolved ->
                    let callable =
                      match Type.single_parameter meta with
                      | TypedDictionary { name; fields } ->
                          Type.TypedDictionary.constructor ~name ~fields
                          |> Option.some
                      | meta_parameter ->
                          let class_definition =
                            Resolution.class_definition resolution meta_parameter
                            >>| Class.create
                          in
                          match class_definition >>| Class.constructor ~resolution with
                          | Some (Type.Callable callable) -> Some callable
                          | _ -> None
                    in
                    target >>| State.annotation,
                    Option.to_list callable
                | Type.Callable callable ->
                    target >>| State.annotation,
                    [callable]
                | Type.Union annotations when List.for_all annotations ~f:Type.is_callable ->
                    let extract_callable = function
                      | Type.Callable callable -> callable
                      | _ -> failwith "Not a callable"
                    in
                    target >>| State.annotation,
                    List.map annotations ~f:extract_callable
                | _ ->
                    let callable = find_method ~parent:resolved ~name:(Access.create "__call__") in
                    Some resolved,
                    Option.to_list callable
              in
              if List.is_empty callables then
                State.abort state ~element:(NotCallable resolved) ~lead ()
              else
                resolve_callables ~implicit_annotation ~callables ~arguments

          | Some resolved, Access.Identifier _
            when Type.is_callable (Annotation.annotation resolved) ->
              (* Nested function. *)
              Resolution.get_local resolution ~access:lead
              >>| (fun resolved -> State.step state ~resolved ~lead ())
              |> Option.value ~default:(State.abort ~lead state ())

          | Some resolved, Access.Identifier _ ->
              (* Attribute access. *)
              let target =
                {
                  State.access = qualifier;
                  annotation = Annotation.annotation resolved;
                }
              in
              let resolved, attributes = local_attributes ~resolved ~lead ~name:[head] in
              if List.is_empty attributes then
                State.abort ~lead state ()
              else
                let defined = List.for_all attributes ~f:Attribute.defined in
                let element =
                  Attribute {
                    attribute = [head];
                    (* TODO(T37504097): pass attributes to client. *)
                    origin = Instance (List.hd_exn attributes);
                    defined;
                  }
                in
                State.step state ~resolved ~target ~element ~continue:defined ~lead ()

          | None, Access.Expression expression ->
              (* Arbitrary expression. *)
              let resolved = Annotation.create (Resolution.resolve resolution expression) in
              State.step state ~resolved ~lead ()

          | None, _ ->
              (* Module or global variable. *)
              begin
                let annotation =
                  match Resolution.get_local resolution ~access:lead with
                  | Some annotation ->
                      Some annotation
                  | _ ->
                      (* Fallback to use a __getattr__ callable as defined by PEP 484. *)
                      let getattr =
                        Resolution.get_local
                          resolution
                          ~access:(
                            qualifier @
                            [Access.Identifier (Identifier.create "__getattr__")]
                          )
                        >>| Annotation.annotation
                      in
                      let correct_getattr_arity signature =
                        Type.Callable.Overload.parameters signature
                        >>| (fun parameters -> List.length parameters == 1)
                        |> Option.value ~default:false
                      in
                      match getattr with
                      | Some (Callable { overloads = [signature]; _ })
                      | Some (Callable { implementation = signature; _ })
                        when correct_getattr_arity signature ->
                          Some (
                            Annotation.create_immutable
                              ~global:true
                              ~original:(Some Type.Top)
                              (Type.Callable.Overload.return_annotation signature)
                          )
                      | _ ->
                          None
                in
                match annotation with
                | Some resolved ->
                    (* Locally known variable (either local or global). *)
                    let suppressed =
                      let rec suppressed lead = function
                        | head :: tail ->
                            begin
                              let lead = lead @ [head] in
                              match Resolution.module_definition resolution lead with
                              | Some definition when Module.empty_stub definition -> true
                              | _ -> suppressed lead tail
                            end
                        | [] ->
                            false
                      in
                      Annotation.annotation resolved
                      |> Type.class_name
                      |> suppressed []
                    in
                    if not suppressed then
                      State.step state ~resolved ~lead ()
                    else
                      State.abort state ~lead ()
                | None ->
                    begin
                      match Resolution.module_definition resolution lead with
                      | Some definition when Module.empty_stub definition ->
                          State.abort state ~lead ()
                      | Some _ ->
                          State.create ~resolution ~accumulator ~f ()
                      | None ->
                          let element =
                            Attribute {
                              attribute = [head];
                              origin = Module qualifier;
                              defined = false;
                            }
                          in
                          State.abort state ~element ~lead ()
                    end
              end

          | _ ->
              State.abort state ~lead ()
        in
        if continue then
          fold ~state ~lead ~tail
        else
          accumulator
    | _ ->
        accumulator
  in
  fold ~state:(State.create ~accumulator:initial ~f ~resolution ()) ~lead:[] ~tail:access


let last_element ~resolution access =
  fold
    ~resolution
    ~initial:Value
    ~f:(fun _ ~resolution:_ ~resolved:_ ~element ~lead:_ -> element)
    access
