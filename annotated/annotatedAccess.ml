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
module TypeOrder = AnalysisTypeOrder

module Assign = AnnotatedAssign
module Class = AnnotatedClass
module Attribute = Class.Attribute
module Method = Class.Method
module Define = AnnotatedDefine
module Call = AnnotatedCall


module Signature = struct
  include AnalysisSignature


  let return_annotation ~resolution = function
    | Some { instantiated = callee; _ } ->
        let return_annotation =
          Define.create callee
          |> Define.return_annotation ~resolution
        in
        (* If we pick an uninstantiated signature, the type variable can escape. Replace those
           type variables with bottom. Note that this loses the information of which type
           variable an annotation came from, but prevents analysis crashes. *)
        if TypeOrder.is_instantiated
            (Resolution.order resolution)
            return_annotation then
          return_annotation
        else
          Type.Bottom
    | None ->
        Type.Top


  (* Calls on methods can determine previously undetermined annotations. E.g. `a.append(1)` can
      determine the type of `a: List[Bottom]` to `a: List[int]`. *)
  let determine ~annotation ~resolution signature =
    let annotation = Annotation.annotation annotation in
    signature
    >>| (fun { constraints; _ } ->
        let primitive, parameters = Type.split annotation in
        let free_variables =
          (Resolution.class_definition resolution) primitive
          >>| Class.create
          >>| Class.free_variables ~resolution ~parameters
          |> Option.value ~default:[]
        in
        let inferred =
          let instantiate parameter = function
            | Some variable ->
                Map.find constraints variable
                |> Option.value ~default:parameter
            | _ -> parameter
          in
          match List.map2 ~f:instantiate parameters free_variables with
          | List.Or_unequal_lengths.Ok inferred -> inferred
          | List.Or_unequal_lengths.Unequal_lengths -> parameters
        in
        match annotation with
        | Type.Parametric parametric ->
            Type.Parametric { parametric with Type.parameters = inferred }
        | _ -> annotation)
    |> Option.value ~default:annotation


  let pick ~resolution ~check_parameters ~insert_implicit_arguments ~call signatures =
    match signatures with
    (* This match is done for performance. In the overwhelming majority of cases,
       there is only one signature for a call, and doing the redundant parameter check
       would add 13 seconds of overhead on instagram (order of magnitude: 100k function
       definitions, 95s total runtime beforehand). *)
    | [signature] -> Some signature
    | _ ->
        let count_call_errors ~resolution call callee =
          let order = Resolution.order resolution in
          let check_parameter
              ~argument
              ~position:_
              ~offset:_
              ~location:_
              ~name
              ~actual
              ~expected =
            if not (TypeOrder.less_or_equal order ~left:actual ~right:expected ||
                    Type.mismatch_with_any actual expected ||
                    Type.equal actual Type.Top ||
                    Type.equal expected Type.Top) ||
               (String.is_prefix ~prefix:"**" (Identifier.show name) &&
                Argument.is_positional argument) then
              Some ()
            else
              None
          in
          let add_error errors _ = errors + 1 in
          check_parameters
            ~resolution
            ~check_parameter
            ~add_error
            ~init:0
            (insert_implicit_arguments call ~callee:(Some callee) ~location:Location.any)
            callee
        in
        let no_error_call =
          (* The find exists for performance reasons. Without it, typechecking would slow down
             by ~2.5x. *)
          List.find
            ~f:(fun signature -> count_call_errors ~resolution call signature = 0)
            signatures
        in
        match no_error_call with
        | Some signature -> Some signature
        | None ->
            List.map
              ~f:(fun signature -> signature, count_call_errors ~resolution call signature)
              signatures
            |> List.min_elt ~compare:(fun (_, left) (_, right) -> Int.compare left right)
            >>| fst
end


type t = Access.t
[@@deriving compare, eq, sexp, show, hash]


let create access =
  access


module Element = struct
  type call = {
    location: Location.t;
    call: Call.t;
    callee: Signature.t option
  }


  type method_call = {
    location: Location.t;
    access: Access.t;
    annotation: Annotation.t;
    call: Call.t;
    callee: Signature.t option;
    backup: (Call.t * Signature.t) option;
  }


  type t =
    | Call of call
    | Callable of AnnotatedSignature.t
    | Attribute of Attribute.t
    | Method of method_call
    | Value
end


module Result = struct
  type 'accumulator t = {
    resolution: Resolution.t;
    accumulator: 'accumulator;
    resolved: Annotation.t option;
    abort: bool;
  }


  let create ~resolution ~accumulator ?resolved ?(abort = false) () =
    { resolution; accumulator; resolved; abort }


  let abort ~resolution ~accumulator =
    {
      resolution;
      accumulator;
      resolved = Some (Annotation.create Type.Top);
      abort = true;
    }
end


(* Fold over an access path. Callbacks will be passed the current `accumulator`, the current
    `annotations`, the `resolved` type of the expression so far, as well as the kind of `element`
    we're currently folding over. *)
let fold ~resolution ~initial ~f access =
  let define = match Resolution.define resolution with
    | Some define -> define
    | None ->
        Define.create_toplevel []
        |> Define.define
  in

  (* Resolve `super()` calls. *)
  let access, resolution =
    match access with
    | (Access.Call { Node.value = { Expression.Call.name; _ }; _ }) :: tail
      when Expression.show name = "super" ->
        (Define.create define
         |> Define.parent_definition ~resolution
         >>| Class.immediate_superclasses ~resolution
         >>| function
         | Some superclass ->
             let super = Access.Identifier (Identifier.create "$super") in
             let resolution =
               let annotation =
                 Class.annotation ~resolution superclass
                 |> Annotation.create
               in
               let annotations =
                 Map.set
                   ~key:[super]
                   ~data:annotation
                   (Resolution.annotations resolution)
               in
               Resolution.with_annotations resolution annotations
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
    | (Access.Call {
        Node.value = {
          Expression.Call.name;
          arguments = [{ Argument.value; _ }];
          _;
        };
        _;
      }) :: tail
      when Expression.show name = "type" ->
        let access = Access.Identifier (Identifier.create "$type") in
        let resolution =
          let annotation =
            Resolution.resolve resolution value
            |> Type.meta
            |> Annotation.create
          in
          let annotations =
            Map.set
              ~key:[access]
              ~data:annotation
              (Resolution.annotations resolution)
          in
          Resolution.with_annotations resolution annotations
        in
        access :: tail, resolution
    | _ ->
        access, resolution
  in

  (* Resolve function redirects. E.g. resolve `abs(x)` to `x.__abs__()`. *)
  let access =
    match access with
    | (Access.Call { Node.value = call; _ }) :: tail ->
        Call.create ~kind:Call.Function call
        |> Call.redirect
        >>| (fun { Call.access; call } -> access @ call @ tail)
        |> Option.value ~default:access
    | _ ->
        access
  in

  let rec fold ~accumulator ~lead ~tail ~resolved ~resolution =
    let annotations = Resolution.annotations resolution in

    let local_annotation access =
      match Map.find annotations access with
      | Some resolved ->
          Some resolved
      | None ->
          Resolution.global resolution access
          >>| Node.value
    in

    let pick_signature call signatures =
      Signature.pick
        ~resolution
        ~check_parameters:Call.check_parameters
        ~insert_implicit_arguments:Call.insert_implicit_arguments
        ~call
        signatures
    in

    match tail with
    | head :: tail ->
        (* Resolve module exports. TODO(T26918135): this should be a fixpoint. *)
        let qualifier, head =
          (Resolution.module_definition resolution lead
           >>= Module.resolve_export ~head)
          |> Option.value ~default:(lead, head)
        in

        (* Resolve callables. E.g. `f()` could be either a function or a call to `f.__call__()`. *)
        let resolved, qualifier, head =
          match resolved, head with
          | None,
            Access.Call {
              Node.value = {
                Expression.Call.name = { Node.value = Access name; _ };
                _;
              };
              _;
            } ->
              (local_annotation (qualifier @ name)
               >>= fun resolved ->
               let annotation = Annotation.annotation resolved in
               let is_object_call =
                 not (Type.equal annotation Type.Top) &&
                 not (Resolution.is_function resolution (qualifier @ name))
               in
               if is_object_call then
                 if Type.is_meta annotation then
                   annotation
                   |> Type.single_parameter
                   |> Type.split
                   |> fst
                   |> Type.expression
                   |> (function
                       | { Node.value = Access access; _ } -> Some (List.rev access)
                       | _ -> None)
                   >>= (function
                       | name :: reversed_qualifier ->
                           let call = Access.call_with_name ~access:head ~name:[name] in
                           Some (None, List.rev reversed_qualifier, call)
                       | _ ->
                           None)
                 else
                   let call = Access.call_with_name ~access:head ~name:(Access.create "__call__") in
                   Some (Some resolved, qualifier, call)
               else
                 None)
              |> Option.value ~default:(resolved, qualifier, head)
          | _ ->
              resolved, qualifier, head
        in

        let lead = qualifier @ [head] in

        let { Result.resolution; resolved; accumulator; abort } =
          match resolved, head with
          (* Typed context: operations are on a class definition. *)
          | Some resolved,
            Access.Call { Node.location; value = call }
            when not (Type.is_callable (Annotation.annotation resolved)) ->
              (* Method call. *)
              let resolved, call =
                if Type.is_meta (Annotation.annotation resolved) then
                  begin
                    let resolved =
                      match Annotation.annotation resolved |> Type.parameters with
                      | [parameter] -> Annotation.create parameter
                      | _ -> failwith "Not a meta annotation"
                    in
                    resolved,
                    Call.create ~kind:Call.Function call
                  end
                else
                  resolved,
                  Call.create ~kind:Call.Method call in

              let callee =
                Resolution.method_signature
                  resolution
                  (Annotation.original resolved)
                  (Call.call call)
                  (Call.argument_annotations ~resolution call)
                |> pick_signature call
              in
              let backup =
                Call.backup call
                >>= fun call ->
                begin
                  match Call.call call with
                  | { Expression.Call.arguments = [{ Argument.value; _ }]; _ } ->
                      let resolved = Resolution.resolve resolution value in
                      Resolution.method_signature
                        resolution
                        resolved
                        (Call.call call)
                        (Call.argument_annotations ~resolution call)
                      |> pick_signature call
                  | _ -> None
                end
                >>= fun signature -> Some (call, signature)
              in
              let element =
                {
                  Element.location;
                  access = qualifier;
                  annotation = resolved;
                  call = Call.insert_implicit_arguments ~location ~callee call;
                  callee;
                  backup;
                }
              in
              let determined = Signature.determine ~annotation:resolved ~resolution callee in
              let resolved = Annotation.create (Signature.return_annotation ~resolution callee) in
              let annotations =
                Map.find annotations qualifier
                >>| (fun existing ->
                    Map.set
                      ~key:qualifier
                      ~data:{ existing with Annotation.annotation = determined }
                      annotations)
                |> Option.value ~default:annotations
              in
              Result.create
                ~resolution:(Resolution.with_annotations resolution annotations)
                ~resolved
                ~accumulator:
                  (f accumulator ~annotations ~resolved ~element:(Element.Method element))
                ()

          | Some resolved,
            Access.Call { Node.value = ({ Expression.Call.name; _ } as call); _ }
            when Type.is_resolved (Annotation.annotation resolved) &&
                 Type.is_callable (Annotation.annotation resolved) &&
                 Expression.show name = "__call__" ->
              (* Callable invocation. *)
              begin
                let callable =
                  let call = Call.create ~kind:Call.Function call in
                  match Annotation.annotation resolved with
                  | Type.Callable callable -> AnnotatedSignature.select call ~resolution ~callable
                  | _ -> failwith "Failed to extract callable"
                in
                match callable with
                | AnnotatedSignature.Found {
                    Type.Callable.overloads = [{ Type.Callable.annotation; _ }];
                    _;
                  } ->
                    let resolved = Annotation.create annotation in
                    let accumulator =
                      f
                        accumulator
                        ~annotations
                        ~resolved
                        ~element:(Element.Callable callable)
                    in
                    Result.create ~resolution ~resolved ~accumulator ()
                | _ ->
                    let accumulator =
                      f
                        accumulator
                        ~annotations
                        ~resolved:(Annotation.create Type.Top)
                        ~element:(Element.Callable callable)
                    in
                    Result.abort ~resolution ~accumulator
              end

          | Some resolved, Access.Identifier _ ->
              (* Attribute access. *)
              let attribute ~annotation =
                let annotation, class_attributes =
                  if Type.is_meta annotation then
                    let annotation =
                      match Type.parameters annotation with
                      | [parameter] -> parameter
                      | _ -> failwith "Not a meta annotation"
                    in
                    annotation,
                    true
                  else
                    annotation,
                    false
                in
                Resolution.class_definition resolution annotation
                >>| Class.create
                >>| fun definition ->
                let attribute =
                  Class.attribute
                    ~transitive:true
                    ~class_attributes
                    ~resolution
                    ~name:[head]
                    ~instantiated:annotation
                    definition
                in
                attribute, definition
              in
              let attribute_with_definition =
                match Annotation.annotation resolved with
                | Type.Union annotations ->
                    List.filter_map ~f:(fun annotation -> attribute ~annotation) annotations
                    |> List.hd (* TODO(T27165573): More sophisticated algorithm for picking here. *)
                | annotation ->
                    attribute ~annotation
              in
              begin
                match attribute_with_definition with
                | Some (attribute, definition) ->
                    (* Handle async attributes. *)
                    let resolved =
                      if Attribute.async attribute then
                        Attribute.annotation attribute
                        |> Annotation.annotation
                        |> Type.awaitable
                        |> Annotation.create
                      else
                        Attribute.annotation attribute
                    in
                    let attribute, resolved =
                      if not (Attribute.defined attribute) then
                        match Class.fallback_attribute ~resolution ~access:[head] definition with
                        | Some attribute ->
                            attribute,
                            Attribute.annotation attribute
                        | None ->
                            attribute,
                            Map.find annotations lead
                            |> Option.value ~default:(Annotation.create Type.Top)
                      else
                        attribute,
                        (* Local definitions can override attributes. *)
                        Map.find annotations lead
                        |> Option.value ~default:resolved
                    in
                    let element = Element.Attribute attribute in
                    Result.create
                      ~resolution
                      ~resolved
                      ~accumulator:(f accumulator ~annotations ~resolved ~element)
                      ()
                | None ->
                    Result.abort ~resolution ~accumulator
              end

          | Some resolved, Access.Subscript subscript ->
              (* Array access. *)
              let resolved =
                let resolved =
                  match subscript, Annotation.annotation resolved with
                  | [Access.Index _],
                    (Type.Parametric {
                        Type.parameters;
                        _;
                      }) ->
                      (* TODO(T22845396): improve temporary fix *)
                      begin
                        match parameters with
                        | _ :: parameter :: _ -> parameter
                        | parameter :: _ -> parameter
                        | [] -> Type.Top
                      end
                  | [Access.Slice _], resolved ->
                      resolved
                  | _ ->
                      Type.Top
                in
                Annotation.create resolved
              in
              Result.create
                ~resolution
                ~resolved
                ~accumulator:(f accumulator ~annotations ~resolved ~element:Element.Value)
                ()

          | Some resolved, _ ->
              (* TODO(T26558543): Undefined access on type. *)
              Result.create ~resolution ~resolved ~accumulator ()

          (* Untyped context: this is either a module, variable, or function call. *)
          | None,
            Access.Call { Node.location; value = call } ->
              (* Call. *)
              let call = Call.create ~kind:Call.Function call in
              let callee =
                Resolution.function_signature
                  resolution
                  qualifier
                  (Call.call call)
                  (Call.argument_annotations ~resolution call)
                |> pick_signature call
              in
              let resolved = Annotation.create (Signature.return_annotation ~resolution callee) in
              let element =
                Element.Call {
                  Element.location;
                  call = Call.insert_implicit_arguments ~location ~callee call;
                  callee;
                }
              in
              Result.create
                ~resolution
                ~resolved
                ~accumulator:(f accumulator ~annotations ~resolved ~element)
                ()

          | None, Access.Expression expression ->
              (* Arbitrary expression. *)
              let resolved = Annotation.create (Resolution.resolve resolution expression) in
              Result.create
                ~resolution
                ~resolved
                ~accumulator:(f accumulator ~annotations ~resolved ~element:Element.Value)
                ()

          | None, Access.Identifier identifier when Identifier.show identifier = "None" ->
              (* None. *)
              let resolved = Annotation.create (Type.optional Type.Bottom) in
              Result.create
                ~resolution
                ~resolved
                ~accumulator:(f accumulator ~annotations ~resolved ~element:Element.Value)
                ()

          | None, _ ->
              (* Module or global variable. *)
              begin
                match local_annotation lead with
                | Some resolved ->
                    (* Locally known variable (either local or global). *)
                    Result.create
                      ~resolution
                      ~resolved
                      ~accumulator:
                        (f accumulator ~annotations ~resolved ~element:Element.Value)
                      ()
                | None ->
                    if Resolution.is_module resolution lead then
                      (* Skip over modules. *)
                      Result.create ~resolution ~accumulator ()
                    else
                      (* Attempt to resolve meta variables. E.g. `module.Class` to
                         `typing.Type[module.Class]`. *)
                      (Resolution.parse_annotation
                         resolution
                         (Node.create_with_default_location (Expression.Access lead))
                       |> Resolution.class_definition resolution
                       >>| Class.create
                       >>| Class.annotation ~resolution
                       >>| Type.meta
                       >>| Annotation.create
                       >>| fun resolved -> Result.create ~resolution ~resolved ~accumulator ())
                      (* TODO(T26558543): undefined global access. *)
                      |> Option.value ~default:(Result.abort ~resolution ~accumulator)
              end
        in
        if abort then
          accumulator
        else
          fold ~resolution ~accumulator ~lead ~tail ~resolved
    | _ ->
        accumulator
  in
  fold ~resolution ~accumulator:initial ~lead:[] ~tail:access ~resolved:None


let last_element ~resolution access =
  fold
    ~resolution
    ~initial:Element.Value
    ~f:(fun _ ~annotations:_ ~resolved:_ ~element -> element)
    access
