(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Pyre
open Statement

module Annotation = AnalysisAnnotation
module Resolution = AnalysisResolution
module Type = AnalysisType

module Assign = AnnotatedAssign
module Class = AnnotatedClass
module Attribute = Class.Attribute
module Method = Class.Method
module Define = AnnotatedDefine
module Signature = AnnotatedSignature


type t = Access.t
[@@deriving compare, eq, sexp, show, hash]


let create access =
  access


module Element = struct
  type origin =
    | Instance of Attribute.t
    | Module of Access.t
  [@@deriving show]

  type attribute = {
    attribute: Access.t;
    origin: origin;
    defined: bool;
  }
  [@@deriving show]

  type signature = {
    signature: Signature.t;
    arguments: Argument.t list;
  }
  [@@deriving show]

  type t =
    | Signature of signature
    | Attribute of attribute
    | Value
  [@@deriving show]
end


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
      -> element: Element.t
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
      () =
    let accumulator =
      let resolved = Option.value resolved ~default:(Annotation.create Type.Top) in
      let element = Option.value element ~default:Element.Value in
      f accumulator ~resolution ~resolved ~element
    in
    {
      state with
      accumulator;
      resolved;
      target;
      continue;
    }


  let abort state ?element () =
    step state ?element ~continue:false ()


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
    | { Node.value = Access.Identifier name; location }
      :: { Node.value = Access.Call _; _ }
      :: tail
      when Identifier.show name = "super" ->
        (Resolution.parent resolution
         >>| (fun parent ->
             Resolution.parse_annotation
               resolution
               (Node.create_with_default_location (Access parent)))
         >>= Resolution.class_definition resolution
         >>| Class.create
         >>| Class.immediate_superclasses ~resolution
         >>| function
         | Some superclass ->
             let super = Access.identifier ~location (Identifier.create "$super") in
             let resolution =
               Resolution.set_local
                 resolution
                 ~access:[super]
                 ~annotation:(Class.annotation ~resolution superclass |> Annotation.create)
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
    | { Node.value = Access.Identifier name; location }
      :: { Node.value = Access.Call [{ Argument.value; _ }]; _ }
      :: tail
      when Identifier.show name = "type" ->
        let access = Access.identifier ~location (Identifier.create "$type") in
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
    | ({ Node.value = Access.Identifier _; _ } as identifier_node)
      :: { Node.value = Access.Call arguments; location }
      :: tail ->
        Access.redirect ~arguments ~location ~name:[identifier_node]
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
                    Module.aliased_export definition [head],
                    Module.wildcard_aliases definition with
                  | Some export, _ ->
                      export @ tail
                  | _, modules ->
                      let exists_in_wildcard_exports module_name =
                        Resolution.module_definition resolution module_name
                        >>| (fun definition -> Module.in_wildcard_exports definition [head])
                        |> Option.value ~default:false
                      in
                      match List.find ~f:exists_in_wildcard_exports modules with
                      | Some module_name -> module_name @ [head] @ tail
                      | _ -> resolve_exports ~lead:(lead @ [head]) ~tail)
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
    let resolve_callable
        ~implicit_annotation
        ~callable
        ~arguments:{ Node.value = arguments; location } =
      let signature =
        let implicit, resolution =
          let { Type.Callable.implicit; _ } = callable in
          match implicit_annotation with
          | Some annotation
            when implicit <> Type.Callable.Function &&
                 (not (Type.is_meta annotation) || implicit = Type.Callable.Class) ->
              let self = Access.create "$self" in
              let annotation = Annotation.create annotation in
              Some self, (Resolution.set_local resolution ~access:self ~annotation)
          | _ ->
              None, resolution
        in
        let arguments =
          implicit >>|
          (fun implicit ->
             let argument =
               {
                 Argument.name = None;
                 value = Node.create ~location (Access implicit);
               }
             in
             argument :: arguments)
          |> Option.value ~default:arguments
        in
        let signature = Signature.select ~arguments ~resolution ~callable in

        let backup () =
          let name =
            match target, List.rev lead with
            | Some _, ({ Node.value = Access.Identifier _ ; _ } as name) :: _ -> [name]
            | _ -> []
          in
          match Access.backup ~arguments ~name with
          | Some (([{ Argument.value; _ }; _ ] as arguments), name) ->
              Resolution.resolve resolution value
              |> Type.expression
              |> Expression.access
              |> (fun access -> access @ name)
              |> (fun access -> Resolution.get_local_callable resolution ~access)
              >>| (fun callable -> Signature.select ~arguments ~resolution ~callable)
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

      (* Determine type. E.g. `[].append(1)` will determine the list to be of type `List[int]`. *)
      let resolution =
        target
        >>= (fun { State.access; annotation } ->
            Signature.determine signature ~resolution ~annotation
            >>| fun determined ->
            Resolution.set_local resolution ~access ~annotation:(Annotation.create determined))
        |> Option.value ~default:resolution
      in

      match signature with
      | Signature.Found {
          Signature.callable = {
            Type.Callable.overloads = [{ Type.Callable.annotation; _ }];
            _;
          };
          _;
        }
      | Signature.NotFound {
          Signature.callable = {
            Type.Callable.overloads = [{ Type.Callable.annotation; _ }];
            _;
          };
          _;
        } when Type.is_resolved annotation ->
          State.step
            { state with State.resolution }
            ~element:(Element.Signature { Element.signature; arguments })
            ~resolved:(Annotation.create annotation)
            ()

      | _ ->
          State.abort state ()
    in

    let local_attribute ~resolved ~lead ~name =
      let annotation = Annotation.annotation resolved in
      let find_attribute ~annotation =
        let instantiated, class_attributes =
          if Type.is_meta annotation then
            Type.single_parameter annotation, true
          else
            annotation, false
        in
        Resolution.class_definition resolution instantiated
        >>| Class.create
        >>| Class.attribute ~transitive:true ~class_attributes ~resolution ~name ~instantiated
      in
      let attribute =
        match annotation with
        | Type.Union annotations ->
            let attributes =
              List.filter_map ~f:(fun annotation -> find_attribute ~annotation) annotations
            in
            let defined =
              List.find
                ~f:(fun attribute -> not (Attribute.defined attribute))
                attributes
            in
            if Option.is_some defined then defined else List.hd attributes
        | annotation ->
            find_attribute ~annotation
      in
      attribute
      >>| fun attribute ->
      let resolved = Attribute.annotation attribute in
      if not (Attribute.defined attribute) then
        match Class.fallback_attribute ~resolution ~access:name (Attribute.parent attribute) with
        | Some attribute ->
            Attribute.annotation attribute,
            attribute
        | None ->
            Resolution.get_local resolution ~access:lead
            |> Option.value ~default:(Annotation.create Type.Top),
            attribute
      else
        let resolved =
          (* Local definitions can override attributes. *)
          Resolution.get_local resolution ~access:lead |> Option.value ~default:resolved
        in
        resolved, attribute
    in

    match tail with
    | head :: tail ->
        let qualifier = lead in
        let lead = lead @ [head] in

        let ({ State.continue; accumulator; _ } as state) =
          match resolved, head with
          (* Typed context: operations are on a class definition. *)
          | Some resolved, { Node.value = Access.Call arguments; location } ->
              (* Callable invocation. *)
              let resolved = Annotation.annotation resolved in
              let target =
                if Type.is_meta resolved then
                  Some { State.access = lead; annotation = Type.single_parameter resolved }
                else
                  target
              in
              let implicit_annotation, callable =
                match resolved with
                | meta when Type.is_meta resolved ->
                    target >>| State.annotation,
                    Type.single_parameter meta
                    |> Type.class_name
                    |> (fun name -> name @ (Access.create "__init__"))
                    |> (fun access -> Resolution.get_local_callable resolution ~access)
                | Type.Callable callable ->
                    target >>| State.annotation,
                    Some callable
                | _ ->
                    Some resolved,
                    Type.class_name resolved
                    |> (fun name -> name @ (Access.create "__call__"))
                    |> (fun access -> Resolution.get_local_callable resolution ~access)
              in
              callable
              >>| (fun callable ->
                  resolve_callable
                    ~implicit_annotation
                    ~callable
                    ~arguments:(Node.create ~location arguments))
              |> Option.value ~default:(State.abort state ())

          | Some resolved, { Node.value = Access.Identifier _; _ }
            when Type.is_callable (Annotation.annotation resolved) ->
              (* Nested function. *)
              Resolution.get_local resolution ~access:lead
              >>| (fun resolved -> State.step state ~resolved ())
              |> Option.value ~default:(State.abort state ())

          | Some resolved, { Node.value = Access.Identifier _; _ } ->
              (* Attribute access. *)
              let target =
                {
                  State.access = qualifier;
                  annotation = Annotation.annotation resolved;
                }
              in
              local_attribute ~resolved ~lead ~name:[head]
              >>| (fun (resolved, attribute) ->
                  let defined = Attribute.defined attribute in
                  let element =
                    Element.Attribute {
                      Element.attribute = [head];
                      origin = Element.Instance attribute;
                      defined;
                    }
                  in
                  State.step state ~resolved ~target ~element ~continue:defined ())
              |> Option.value ~default:(State.abort state ())

          | None, { Node.value = Access.Expression expression; _ } ->
              (* Arbitrary expression. *)
              let resolved = Annotation.create (Resolution.resolve resolution expression) in
              State.step state ~resolved ()

          | None, _ ->
              (* Module or global variable. *)
              begin
                match Resolution.get_local resolution ~access:lead with
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
                      State.step state ~resolved ()
                    else
                      State.abort state ()
                | None ->
                    begin
                      match Resolution.module_definition resolution lead with
                      | Some definition when Module.empty_stub definition ->
                          State.abort state ()
                      | Some _ ->
                          State.create ~resolution ~accumulator ~f ()
                      | None ->
                          let element =
                            Element.Attribute {
                              Element.attribute = [head];
                              origin = Element.Module qualifier;
                              defined = false;
                            }
                          in
                          State.abort state ~element ()
                    end
              end

          | _ ->
              State.abort state ()
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
    ~initial:Element.Value
    ~f:(fun _ ~resolution:_ ~resolved:_ ~element -> element)
    access
