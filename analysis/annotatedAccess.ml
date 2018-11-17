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
      let element = Option.value element ~default:Element.Value in
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
    let resolve_callable
        ~implicit_annotation
        ~callable
        ~arguments:{ Node.value = arguments; location } =
      let resolve_independent_callable
          ~implicit_annotation
          ~callable
          ~arguments
          ~location =
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
              | Some _, (Access.Identifier _ as name) :: _ -> [name]
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

        match signature with
        | Signature.Found {
            Signature.callable = {
              Type.Callable.implementation = { Type.Callable.annotation; _ };
              _;
            };
            _;
          }
        | Signature.NotFound {
            Signature.callable = {
              Type.Callable.implementation = { Type.Callable.annotation; _ };
              _;
            };
            _;
          } when Type.is_resolved annotation ->
            State.step
              { state with State.resolution }
              ~element:(Element.Signature { Element.signature; arguments })
              ~resolved:(Annotation.create annotation)
              ~lead
              ()

        | _ ->
            State.abort state ~lead ()
      in

      let resolve_typed_dictionary_get_item_callable ~fields ~callable ~arguments =
        let fail () =
          State.step
            state
            ~element:(Element.Signature {
                Element.signature = Signature.NotFound { callable; reason = None };
                arguments;
              })
            ~resolved:(Annotation.create Type.Top)
            ~lead
            ()
        in
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
                  State.step
                    state
                    ~element:(Element.Signature {
                        Element.signature = Signature.Found {
                            callable;
                            constraints = Type.Map.empty;
                          };
                        arguments;
                      })
                    ~resolved:(Annotation.create annotation)
                    ~lead
                    ()
              | None ->
                  (* TODO(T35907494): Add a reason that triggers an AnalysisError *)
                  fail ()
            end
        | _ ->
            (* TODO(T35907494): Add a reason that triggers an AnalysisError *)
            fail ()
      in

      let tail_is_get_item access =
        match List.last access with
        | Some (Access.Identifier get_item) -> Identifier.show get_item = "__getitem__"
        | _ -> false
      in
      match implicit_annotation, callable with
      | Some (Type.TypedDictionary { fields; _ }), { Type.Record.Callable.kind = Named access; _ }
        when tail_is_get_item access ->
          resolve_typed_dictionary_get_item_callable ~fields ~callable ~arguments
      | _ ->
          resolve_independent_callable ~implicit_annotation ~callable ~arguments ~location
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
        let fallback_attribute =
          Attribute.parent attribute
          |> Resolution.class_definition resolution
          >>| Class.create
          >>= Class.fallback_attribute ~resolution ~access:name
        in
        match fallback_attribute with
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
          | Some resolved, Access.Call arguments ->
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
                    let callable =
                      let class_definition =
                        Resolution.class_definition resolution (Type.single_parameter meta)
                        >>| Class.create
                      in
                      match class_definition >>| Class.constructor ~resolution with
                      | Some (Type.Callable callable) -> Some callable
                      | _ -> None
                    in
                    target >>| State.annotation,
                    callable
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
              >>| (fun callable -> resolve_callable ~implicit_annotation ~callable ~arguments)
              |> Option.value ~default:(State.abort state ~lead ())

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
                  State.step state ~resolved ~target ~element ~continue:defined ~lead ())
              |> Option.value ~default:(State.abort state ~lead ())

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
                            Element.Attribute {
                              Element.attribute = [head];
                              origin = Element.Module qualifier;
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
    ~initial:Element.Value
    ~f:(fun _ ~resolution:_ ~resolved:_ ~element ~lead:_ -> element)
    access
