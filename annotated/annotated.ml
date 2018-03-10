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


let return_annotation ({ Define.return_annotation; async; _ } as define) ~resolution =
  let annotation =
    Option.value_map
      return_annotation
      ~f:(Resolution.parse_annotation resolution)
      ~default:Type.Top
  in
  if async then
    Type.awaitable annotation
  else
  if Define.is_coroutine define then
    begin
      match annotation with
      | Type.Parametric { Type.name; parameters = [_; _; return_annotation] }
        when Identifier.show name = "typing.Generator" ->
          Type.awaitable return_annotation
      | _ ->
          Type.Top
    end
  else
    annotation


let parameter_annotations { Define.parameters; _ } ~resolution =
  let element { Node.value = { Parameter.name; annotation; _ }; _ } =
    let annotation =
      (annotation
       >>| fun annotation -> Resolution.parse_annotation resolution annotation)
      |> Option.value ~default:Type.Top
    in
    name, annotation
  in
  List.map ~f:element parameters
  |> Identifier.Map.of_alist_exn


let parameter_annotations_positional { Define.parameters; _ } ~resolution =
  let element index { Node.value = { Parameter.annotation; _ }; _ } =
    let annotation =
      (annotation
       >>| fun annotation -> Resolution.parse_annotation resolution annotation)
      |> Option.value ~default:Type.Top
    in
    index, annotation
  in
  List.mapi ~f:element parameters
  |> Int.Map.of_alist_exn


module Define = struct
  type t = Define.t
  [@@deriving compare, eq, sexp, show, hash]


  let create definition =
    definition


  let create_toplevel statements =
    {
      Define.name = Expression.Access.create "$toplevel";
      parameters = [];
      body = statements;
      decorators = [];
      docstring = None;
      return_annotation = Some (Type.expression Type.none);
      async = false;
      generated = false;
      parent = None;
    }


  let define annotated = annotated


  let parameter_annotations define ~resolution =
    parameter_annotations define ~resolution


  let parameter_annotations_positional define ~resolution =
    parameter_annotations_positional define ~resolution


  let return_annotation define ~resolution =
    return_annotation define ~resolution


  let parent_definition { Define.parent; _ } ~resolution =
    match parent with
    | Some parent ->
        let annotation =
          Resolution.parse_annotation
            resolution
            (Node.create_with_default_location (Access parent))
        in
        Resolution.class_definition resolution annotation
        >>| Class.create
    | _ -> None


  let method_definition define ~resolution =
    parent_definition define ~resolution
    >>| fun parent -> Class.Method.create ~define ~parent


  (* Given a callee f and an its argument at index index, evaluates to the parameter name the
   *  argument corresponds to. *)
  let infer_argument_name { Define.parameters; _ } ~index ~argument =
    let parameter_names = List.map ~f:Parameter.name parameters in
    let star_index =
      List.find_mapi
        ~f:(fun index name ->
            if String.prefix (Identifier.show name) 1 = "*" then
              Some index
            else
              None)
        parameter_names
    in
    match argument.Argument.name, star_index with
    | None, None ->
        List.nth parameter_names index
    | None, Some star_index ->
        if star_index <= index then
          List.nth parameter_names star_index
        else
          List.nth parameter_names index
    | Some name, _ -> Some name


  let apply_decorators define ~resolution =
    let return_annotation = return_annotation define ~resolution in
    match Define.has_decorator define "contextlib.contextmanager", return_annotation with
    | true, AnalysisType.Parametric { AnalysisType.name; parameters = [single_parameter] }
      when Identifier.show name = "typing.Iterator" ->
        {
          define with
          Define.return_annotation =
            Some
              (AnalysisType.Parametric {
                  AnalysisType.name = Identifier.create "contextlib.GeneratorContextManager";
                  parameters = [single_parameter];
                }
               |> Type.expression);
        }
    | _ ->
        define
end


module Signature = struct
  include AnalysisSignature


  let return_annotation ~resolution = function
    | Some { instantiated = callee; _ } ->
        Define.create callee
        |> Define.return_annotation ~resolution
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
            (insert_implicit_arguments ~callee:(Some callee) ~location:Location.any call)
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
            |> List.min_elt ~cmp:(fun (_, left) (_, right) -> Int.compare left right)
            >>| fst
end


module Call = struct
  type kind =
    | Function
    | Method
  [@@deriving compare, eq, sexp, show, hash]


  type t = {
    call: Call.t;
    kind: kind;
  }
  [@@deriving compare, eq, sexp, show, hash]


  let name_equal { call = { Call.name = left; _ }; _ } { call = { Call.name = right; _ }; _ } =
    Expression.equal left right


  let create ~kind call =
    { call; kind }


  let call { call; _ } =
    call


  let name { call = { Call.name; _ }; _ } =
    name


  let arguments { call = { Call.arguments; _ }; _ } =
    arguments


  let with_arguments { call; kind } arguments =
    { call = { call with Call.arguments }; kind }


  let insert_implicit_arguments ~callee ~location { call; kind } =
    let call =
      callee
      >>| (fun { Signature.instantiated = callee; _ } ->
          let prepend_self call =
            let self =
              {
                Argument.name = None;
                value = Node.create ~location (Access (Access.create "self"));
              }
            in
            { call with Call.arguments = self :: call.Call.arguments }
          in
          match kind with
          | Method ->
              if Statement.Define.is_static_method callee then
                call
              else
                prepend_self call
          | Function ->
              if Statement.Define.is_class_method callee ||
                 (Statement.Define.is_constructor callee &&
                  not (Call.is_explicit_constructor_call call)) then
                prepend_self call
              else
                call)
      |> Option.value ~default:call
    in
    { call; kind }


  type redirect = {
    access: Access.t;
    call: Access.t;
  }


  let redirect { call = { Call.name; arguments }; kind = _ } =
    match name, arguments with
    | { Node.location; value = Access [Access.Identifier name]; _ },
      [{
        Argument.value = { Node.value = Access access; _ };
        _;
      } as argument] ->
        begin
          match Identifier.show name with
          | "abs" -> Some "__abs__"
          | "repr" -> Some "__repr__"
          | "str" -> Some "__str__"
          | _ -> None
        end
        >>| (fun name ->
            let call =
              [Access.Call {
                  Node.location;
                  value = {
                    Call.name = {
                      Node.location;
                      value = Access (Access.create name);
                    };
                    arguments = [argument];
                  };
                }]
            in
            { access; call })

    | _ -> None


  let backup { call = { Call.name; arguments }; kind } =
    match name with
    | { Node.location; value = Access [Access.Identifier name]; _ } ->
        (* cf. https://docs.python.org/3/reference/datamodel.html#object.__radd__ *)
        begin
          match Identifier.show name with
          | "__add__" -> Some "__radd__"
          | "__sub__" -> Some "__rsub__"
          | "__mul__" -> Some "__rmul__"
          | "__matmul__" -> Some "__rmatmul__"
          | "__truediv__" -> Some "__rtruediv__"
          | "__floordiv__" -> Some "__rfloordiv__"
          | "__mod__" -> Some "__rmod__"
          | "__divmod__" -> Some "__rdivmod__"
          | "__pow__" -> Some "__rpow__"
          | "__lshift__" -> Some "__rlshift__"
          | "__rshift__" -> Some "__rrshift__"
          | "__and__" -> Some "__rand__"
          | "__xor__" -> Some "__rxor__"
          | "__or__" -> Some "__ror__"
          | _ -> None
        end
        >>| (fun name ->
            {
              call = {
                Call.name = {
                  Node.location;
                  value = Access (Access.create name);
                };
                arguments;
              };
              kind;
            })
    | _ -> None


  let argument_annotations { call = { Call.arguments; _ }; kind = _ } ~resolution =
    let extract_argument { Argument.value; _ } =
      match value with
      | { Node.location; Node.value = Starred (Starred.Once expression) } ->
          {
            Node.location;
            value = Signature.Starred (Resolution.resolve resolution expression);
          }
      | { Node.location; _ } ->
          {
            Node.location;
            value = Signature.Normal {
                Signature.annotation = Resolution.resolve resolution value;
                value;
              }
          }
    in
    List.map ~f:extract_argument arguments


  let check_parameters
      ~resolution
      ~check_parameter
      ~add_error
      ~init
      call
      { Signature.instantiated = callee; _ } =
    let parameter_ok
        ~position
        ~offset
        { Node.location; value } =
      (* Get the argument's name. *)
      List.nth (arguments call) position
      >>= fun argument ->
      Define.infer_argument_name
        (Define.create callee)
        ~index:(position + offset)
        ~argument
      >>= fun name ->
      (* Get the type of the argument. *)
      let parameter_map = Define.parameter_annotations (Define.create callee) ~resolution in
      begin
        match Map.find parameter_map name with
        | Some annotation when not (Type.is_meta annotation) -> Some annotation
        | _ -> None
      end
      >>= fun expected ->

      (* Compare to the actual type. *)
      begin
        match value with
        | Signature.Normal { Signature.annotation = actual; _ }
        | Signature.Starred (Type.Parametric { Type.parameters = [actual]; _ }) ->
            Some actual
        | _ ->
            None
      end
      >>= fun actual ->
      check_parameter ~argument ~position ~offset ~location ~name ~actual ~expected
    in
    let accumulate_errors position (offset, errors) = function
      | { Node.value = Signature.Normal _; _ } as argument ->
          begin
            match parameter_ok ~position ~offset argument with
            | Some error -> offset, add_error errors error
            | _ -> offset, errors
          end
      | { Node.value = Signature.Starred _; _ } as argument ->
          (* Angelic assumption: if we get a type error with a starred argument we move on
           * to the next argument, otherwise we keep consuming parameters. The offset tries
           * to match the next argument to one left of where it would be normally matched.
          *)
          begin
            match parameter_ok ~position ~offset argument with
            | Some _ -> offset - 1, errors
            | _ -> offset, errors
          end
    in
    argument_annotations ~resolution call
    |> List.foldi ~init:(0, init) ~f:accumulate_errors
    |> snd
end


module Access = struct
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
          let qualifier = lead in
          let lead = lead @ [head] in
          let { Result.resolution; resolved; accumulator; abort } =
            match resolved, head with
            (* Typed context: operations are on a class definition. *)
            | Some resolved,
              Access.Call { Node.location; value = call } ->
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
                    match call with
                    | {
                      Call.call = { Expression.Call.arguments = [{ Argument.value; _ }]; _ };
                      _;
                    } ->
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
                  |> Option.value
                    ~default:
                      (Map.set ~key:qualifier ~data:(Annotation.create determined) annotations)
                in
                Result.create
                  ~resolution:(Resolution.with_annotations resolution annotations)
                  ~resolved
                  ~accumulator:
                    (f accumulator ~annotations ~resolved ~element:(Element.Method element))
                  ()

            | Some resolved, Access.Identifier _ ->
                (* Attribute access. *)
                let resolved, class_attributes =
                  if Type.is_meta (Annotation.annotation resolved) then
                    let resolved =
                      match Annotation.annotation resolved |> Type.parameters with
                      | [parameter] -> Annotation.create parameter
                      | _ -> failwith "Not a meta annotation"
                    in
                    resolved,
                    true
                  else
                    resolved,
                    false
                in
                let definition =
                  Resolution.class_definition
                    resolution
                    (Annotation.annotation resolved)
                in
                (definition
                 >>| Class.create
                 >>| fun definition ->
                 let attribute =
                   Class.attribute
                     ~transitive:true
                     ~class_attributes
                     ~resolution
                     ~name:[head]
                     ~instantiated:(Annotation.annotation resolved)
                     definition
                 in

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

                 if not (Attribute.defined attribute) then
                   let attribute, resolved =
                     match Class.fallback_attribute ~resolution ~access:[head] definition with
                     | Some attribute ->
                         attribute,
                         Attribute.annotation attribute
                     | None ->
                         attribute,
                         Map.find annotations lead
                         |> Option.value ~default:(Annotation.create Type.Top)
                   in
                   let element = Element.Attribute attribute in
                   Result.create
                     ~resolution
                     ~resolved
                     ~accumulator:(f accumulator ~annotations ~resolved ~element)
                     ()
                 else
                   let resolved =
                     (* Local definitions can override attributes. *)
                     Map.find annotations lead
                     |> Option.value ~default:resolved
                   in
                   let element = Element.Attribute attribute in
                   Result.create
                     ~resolution
                     ~resolved
                     ~accumulator:(f accumulator ~annotations ~resolved ~element)
                     ())
                |> Option.value ~default:(Result.abort ~resolution ~accumulator)

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
                  let resolved =
                    match Map.find annotations lead with
                    | Some resolved ->
                        Some resolved
                    | None ->
                        Resolution.global resolution lead
                        >>| fun { Resolution.annotation; _ } -> annotation
                  in
                  match resolved with
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
end


let rec resolve ~resolution expression =
  let with_generators resolution generators =
    let add_generator resolution {
        Comprehension.target = { Node.value = target_value; _ } as target;
        iterator;
        conditions;
        async = _; (* TODO(T23723699): resolve async comprehensions. *)
      } =
      let iterator_type =
        match
          TypeOrder.join
            (Resolution.order resolution)
            (resolve ~resolution iterator)
            (Type.iterable Type.Bottom)
        with
        | Type.Parametric { Type.parameters = [parameter]; _ } ->
            parameter
        | _ ->
            Type.Object
      in
      let rec collect_optionals { Node.value; _ } =
        match value with
        | BooleanOperator {
            BooleanOperator.left;
            operator = BooleanOperator.And;
            right;
          } ->
            List.rev_append (collect_optionals left) (collect_optionals right)

        | Access access ->
            [access]

        | ComparisonOperator {
            Expression.ComparisonOperator.left = { Node.value = Access access; _ };
            right =
              [
                Expression.ComparisonOperator.IsNot,
                { Node.value = Access [Expression.Access.Identifier identifier]; _ }
              ];
          } when Identifier.show identifier = "None" ->
            [access]

        | _ -> []
      in
      let optional_accesses = List.concat_map ~f:collect_optionals conditions in
      let iterator_type = match iterator_type, target_value with
        | Type.Optional annotation, Access target_value ->
            if List.mem ~equal:Access.equal optional_accesses target_value then
              annotation
            else
              iterator_type
        | _ -> iterator_type
      in
      let annotations =
        let rec add annotations_sofar target annotation =
          match Node.value target, annotation with
          | Access access, _ ->
              Map.set annotations_sofar ~key:access ~data:(Annotation.create annotation)
          | Tuple accesses, Type.Tuple (Type.Bounded parameters)
            when List.length accesses = List.length parameters ->
              List.fold2_exn ~init:annotations_sofar ~f:add accesses parameters
          | Tuple accesses, Type.Tuple (Type.Unbounded parameter) ->
              let parameters = List.map ~f:(fun _ -> parameter) accesses in
              List.fold2_exn ~init:annotations_sofar ~f:add accesses parameters
          | _ -> annotations_sofar
        in
        add (Resolution.annotations resolution) target iterator_type
      in
      Resolution.with_annotations resolution annotations
    in
    List.fold ~init:resolution ~f:add_generator generators;
  in

  match Node.value expression with
  | Access access ->
      let annotation _ ~annotations:_ ~resolved ~element:_ = resolved in
      Access.fold
        ~resolution
        ~initial:(Annotation.create Type.Top)
        ~f:annotation
        (Access.create access)
      |> Annotation.annotation

  | Await expression ->
      resolve ~resolution expression
      |> Type.awaitable_value

  | BinaryOperator _ ->
      failwith "Binary Operator inference not supported"

  | BooleanOperator { BooleanOperator.left; right; operator } ->
      let left_type =
        match operator with
        | BooleanOperator.Or ->
            Type.optional_value (resolve ~resolution left)
        | _ ->
            resolve ~resolution left
      in
      TypeOrder.join
        (Resolution.order resolution)
        left_type
        (resolve ~resolution right)

  | Bytes _ ->
      Type.bytes

  | ComparisonOperator operator ->
      let fold_comparisons sofar = function
        | Some call ->
            TypeOrder.meet
              (Resolution.order resolution)
              sofar
              (resolve ~resolution call)
        | None ->
            TypeOrder.meet
              (Resolution.order resolution)
              sofar
              Type.bool
      in
      ComparisonOperator.override operator
      |> List.fold ~init:Type.Top ~f:fold_comparisons

  | Complex _ ->
      Type.complex

  | Dictionary { Dictionary.entries; _ } ->
      let key, values =
        let pair_wise_join (key_sofar, values) { Dictionary.key; value } =
          TypeOrder.join (Resolution.order resolution) key_sofar (resolve ~resolution key),
          (resolve ~resolution value) :: values
        in
        List.fold entries ~init:(Type.Bottom, []) ~f:pair_wise_join
      in
      if List.is_empty entries then
        Type.dictionary ~key:Type.Bottom ~value:Type.Bottom
      else
        Type.dictionary ~key ~value:(Type.union values)

  | DictionaryComprehension { Comprehension.element = { Dictionary.key; value }; generators } ->
      let resolution = with_generators resolution generators in
      Type.dictionary
        ~key:(Type.assume_any (resolve ~resolution key))
        ~value:(Type.assume_any (resolve ~resolution value))

  | Generator { Comprehension.element; generators } ->
      let resolution = with_generators resolution generators in
      Type.generator (resolve ~resolution element |> Type.assume_any)

  | False ->
      Type.bool

  | Float _ ->
      Type.float

  | Format _ ->
      Type.string

  | Integer _ ->
      Type.integer

  | Lambda { Lambda.body; _ } ->
      Type.lambda (resolve ~resolution body)

  | List elements ->
      List.map ~f:(resolve ~resolution) elements
      |> List.fold
        ~init:Type.Bottom
        ~f:(TypeOrder.join (Resolution.order resolution))
      |> Type.list

  | ListComprehension { Comprehension.element; generators } ->
      let resolution = with_generators resolution generators in
      Type.list (resolve ~resolution element)

  | Set elements ->
      List.map ~f:(resolve ~resolution) elements
      |> List.fold
        ~init:Type.Bottom
        ~f:(TypeOrder.join (Resolution.order resolution))
      |> Type.set

  | SetComprehension { Comprehension.element; generators } ->
      let resolution = with_generators resolution generators in
      Type.set (resolve ~resolution element)

  | Starred _ ->
      Type.Object

  | String _ ->
      Type.string

  | Ternary { Ternary.target; alternative; test; } ->
      let deoptionalize key access resolution =
        let updated_annotation =
          match resolve ~resolution access with
          | Type.Optional parameter -> Annotation.create parameter
          | parameter -> Annotation.create parameter
        in
        Map.set ~key ~data:updated_annotation (Resolution.annotations resolution)
        |> Resolution.with_annotations resolution
      in
      let updated_resolution =
        match Node.value test with
        | Expression.Access access ->
            deoptionalize access test resolution
        | Expression.ComparisonOperator {
            Expression.ComparisonOperator.left = {
              Node.value = Expression.Access access;
              _;
            } as access_node;
            right = [
              Expression.ComparisonOperator.IsNot,
              { Node.value = Access [Expression.Access.Identifier identifier]; _ }
            ];
          } when Identifier.show identifier = "None" ->
            deoptionalize access access_node resolution
        | _ -> resolution
      in
      TypeOrder.join
        (Resolution.order updated_resolution)
        (resolve ~resolution:updated_resolution target)
        (resolve ~resolution alternative)

  | True ->
      Type.bool

  | Tuple elements ->
      Type.tuple (List.map elements ~f:(resolve ~resolution))

  | UnaryOperator operator ->
      UnaryOperator.override operator
      >>| resolve ~resolution
      |> Option.value ~default:Type.bool

  | Expression.Yield _ ->
      Type.yield Type.Object
