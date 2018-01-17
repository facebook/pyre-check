(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Pyre
open Statement


let return_annotation { Define.return_annotation; async; _ } ~resolution =
  let annotation =
    Option.value_map
      return_annotation
      ~f:(Resolution.parse_annotation resolution)
      ~default:Type.Top
  in
  if async then Type.awaitable annotation else annotation


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


module Assign = struct
  type t = Assign.t
  [@@deriving compare, eq, sexp, show]


  let create assign =
    assign


  let fold ~resolution ~initial ~f { Assign.target; value; _ } =
    value
    >>| (fun value ->
        let rec fold_simple_assign accumulator { Node.location; value } value_annotation =
          match value with
          | Access access ->
              f ~access:(Node.create ~location access) ~value_annotation accumulator
          | Tuple targets ->
              (* Recursively break down tuples such as x, y = z : Tuple[int, string] *)
              let parameters =
                match value_annotation with
                | Type.Tuple (Type.Bounded parameters) ->
                    parameters
                | Type.Tuple (Type.Unbounded parameter) ->
                    List.map ~f:(fun _ -> parameter) targets
                | _ ->
                    []
              in
              if List.length targets = List.length parameters then
                List.fold2_exn ~init:accumulator ~f:fold_simple_assign targets parameters
              else
                accumulator
          | _ ->
              accumulator
        in
        begin
          match (Node.value target), (Node.value value) with
          (* Tuples of individual assignments *)
          | Tuple targets, Tuple values
            when List.length targets = List.length values ->
              List.map ~f:(Resolution.resolve resolution) values
              |> List.fold2_exn ~init:initial ~f:fold_simple_assign targets
          | _, _ ->
              fold_simple_assign initial target (Resolution.resolve resolution value)
        end)
    |> Option.value ~default:initial
end


module Class = struct
  type t = Class.t Node.t
  [@@deriving compare, eq, sexp, show]


  module Assign = Statement.Assign


  let name_equal
      { Node.value = { Class.name = left; _ }; _ }
      { Node.value = { Class.name = right; _ }; _ } =
    Access.equal left right


  type parent_class = t
  [@@deriving compare, eq, sexp, show]


  let create definition =
    definition


  let create_parent =
    create


  let name { Node.value = { Class.name; _ }; _ } =
    name


  let bases { Node.value = { Class.bases; _ }; _ } =
    bases


  let body { Node.value = { Class.body; _ }; _ } =
    body


  let annotation { Node.value = { Class.name; _ }; _ } ~resolution =
    Resolution.parse_annotation resolution (Node.create (Access name))


  module Attribute = struct
    type t = {
      name: Expression.expression;
      parent: parent_class;
      annotation: Annotation.t;
      value: Expression.t option;
      location: Location.t;
      defined: bool;
    }
    [@@deriving eq, show]


    let create
        ~resolution
        ~parent
        ?(defined = true)
        {
          Node.location;
          value = {
            Assign.target = { Node.value = target; _ };
            annotation;
            value;
            _;
          };
        } =
      let annotation =
        match annotation, value with
        | Some annotation, Some value ->
            Annotation.create_immutable
              ~global:true
              ~original:(Some (Resolution.parse_annotation resolution annotation))
              (Resolution.resolve resolution value)
        | Some annotation, None ->
            Annotation.create_immutable
              ~global:true
              (Resolution.parse_annotation resolution annotation)
        | None, Some value ->
            Annotation.create_immutable
              ~global:true
              ~original:(Some Type.Top)
              (Resolution.parse_annotation resolution value)
        | _ ->
            Annotation.create_immutable ~global:true Type.Top
      in
      { name = target; parent; annotation; value; location; defined }


    let name { name; _ } =
      name


    let access { name; _ } =
      match name with
      | Expression.Access access -> access
      | _ -> []


    let annotation { annotation; _ } =
      annotation


    let parent { parent; _ } =
      parent


    let value { value; _ } =
      value


    let location { location; _ } =
      location


    let defined { defined; _ } =
      defined
  end


  module Method = struct
    type t = {
      define: Define.t;
      parent: parent_class
    }
    [@@deriving compare, eq, sexp, show]


    let create ~define ~parent =
      { define; parent }


    let name { define = { Define.name; _ }; _ } =
      name


    let define { define; _ } =
      define


    let parent { parent; _ } =
      parent


    let parameter_annotations { define; _ } ~resolution =
      parameter_annotations define ~resolution


    let parameter_annotations_positional { define; _ } ~resolution =
      parameter_annotations_positional define ~resolution


    let return_annotation { define; _ } ~resolution =
      return_annotation define ~resolution


    let overrides { define = { Define.name; _ }; parent } ~resolution =
      let find_overrides sofar annotation =
        match sofar with
        | Some _ -> sofar
        | None ->
            Resolution.class_definition resolution annotation
            >>= (fun ({ Node.value = { Class.body; _ }; _ } as parent) ->
                let find_override { Node.value = statement; _ } =
                  match statement with
                  | Statement.Stub (Stub.Define ({ Define.name = other; _ } as define))
                  | Statement.Define ({ Define.name = other; _ } as define)
                    when Access.equal name other ->
                      Some define
                  | _ ->
                      None
                in
                List.find_map ~f:find_override body
                >>| fun define ->
                create ~define ~parent:(create_parent parent))
      in
      TypeOrder.successors_fold
        (Resolution.order resolution)
        ~initial:None
        ~f:find_overrides
        (annotation parent ~resolution)


    let implements
        { define; _ }
        ~resolution:_
        ~protocol_method:{ define = protocol; _ } =
      let open Define in
      let parameter_equal
          { Node.value = { Parameter.annotation; _ }; _ }
          { Node.value = { Parameter.annotation = protocol_annotation; _ }; _ } =
        Option.equal Expression.equal annotation protocol_annotation
      in
      Access.equal define.name protocol.name &&
      Option.equal Expression.equal define.return_annotation protocol.return_annotation &&
      List.equal ~equal:parameter_equal define.parameters protocol.parameters
  end


  let generics { Node.value = { Class.bases; _ }; _ } ~resolution =
    let generic { Argument.value; _ } =
      let annotation = Resolution.parse_annotation resolution value in
      match annotation with
      | Type.Parametric { Type.parameters; _ }
        when Type.is_generic annotation ->
          Some parameters
      | _ ->
          None in
    List.find_map ~f:generic bases
    |> Option.value ~default:[]


  let free_variables { Node.value = { Class.bases; _ }; _ } ~resolution ~parameters =
    let rec iterate ~free_variables ~bases ~parameters =
      match bases, parameters with
      | { Argument.value; _ } :: bases, Type.Bottom :: parameters ->
          let free_variables =
            match Resolution.parse_annotation resolution value with
            | (Type.Parametric {
                Type.parameters;
                _;
              } as annotation)
              when Type.is_generic annotation ->
                (* The parameters need to be added in reverse order here because they're reversed
                   once again at the end. *)
                (List.map ~f:Option.return parameters |> List.rev) @ free_variables
            | _ ->
                free_variables
          in
          iterate ~free_variables ~bases ~parameters
      | _ :: bases, _ :: parameters ->
          iterate ~free_variables:(None :: free_variables) ~bases ~parameters
      | _ ->
          free_variables
    in
    iterate ~free_variables:[] ~bases ~parameters |> List.rev


  let superclasses definition ~resolution =
    TypeOrder.successors (Resolution.order resolution) (annotation definition ~resolution)
    |> List.filter_map ~f:(Resolution.class_definition resolution)
    |> List.map ~f:create


  let constructors ({ Node.value = { Class.name; body; _ }; _ } as definition) ~resolution =
    let constructors =
      let declared =
        let extract_constructor = function
          | { Node.value = Statement.Stub (Stub.Define define); _ }
          | { Node.value = Statement.Define define; _ }
            when Define.is_constructor define ->
              Some define
          | _ ->
              None in
        List.filter_map ~f:extract_constructor body
      in
      if List.is_empty declared then
        [Define.create_generated_constructor (Node.value definition)]
      else
        declared
    in
    (* Adjust return name and return type. *)
    let adjust constructor =
      let annotation =
        let generics = generics ~resolution definition in
        let parsed =
          Resolution.parse_annotation
            resolution
            (Node.create (Access name))
        in
        if List.is_empty generics then
          parsed
        else
          match parsed with
          | Type.Primitive name ->
              Type.Parametric {
                Type.name;
                parameters = generics;
              }
          | Type.Parametric _ ->
              parsed
          | _ ->
              failwith "Parsed type was not primitive!"
      in
      {
        constructor with
        Define.name;
        return_annotation = Some (Type.expression annotation);
      }
    in
    List.map ~f:adjust constructors


  let methods ({ Node.value = { Class.body; _ }; _ } as definition) =
    let extract_define = function
      | { Node.value = Statement.Stub (Stub.Define define); _ }
      | { Node.value = Define define; _ } ->
          Some (Method.create ~define ~parent:definition)
      | _ ->
          None
    in
    List.filter_map ~f:extract_define body


  let is_protocol { Node.value = { Class.bases; _ }; _ } =
    let is_protocol { Argument.name; value } =
      match name, Expression.show value with
      | None , "typing.Protocol" ->
          true
      | Some name, "abc.ABCMeta" when Identifier.show name = "metaclass" ->
          true
      | _ ->
          false
    in
    List.exists ~f:is_protocol bases


  let implements definition ~resolution ~protocol =
    let rec implements instance_methods protocol_methods =
      match instance_methods, protocol_methods with
      | _, [] ->
          true
      | [], _ :: _ ->
          false
      | instance_method :: instance_methods,
        ((protocol_method :: protocol_methods) as old_protocol_methods) ->
          if Method.implements ~resolution ~protocol_method instance_method then
            implements instance_methods protocol_methods
          else
            implements instance_methods old_protocol_methods
    in
    implements (methods definition) (methods protocol)


  let attribute_fold ?(transitive = false) definition ~initial ~f ~resolution =
    let fold_definition initial ({ Node.value = definition; _ } as parent) =
      let fold_attribute_assign accumulator assign =
        Attribute.create ~resolution ~parent assign
        |> f accumulator
      in
      Statement.Class.attribute_assigns definition
      |> Map.data
      |> List.fold ~init:initial ~f:fold_attribute_assign
    in
    let definitions =
      if transitive then
        definition :: superclasses ~resolution definition
      else
        [definition]
    in
    List.fold ~f:fold_definition ~init:initial definitions


  let attributes ?(transitive = false) definition ~resolution  =
    attribute_fold
      ~transitive
      ~initial:[]
      ~resolution
      ~f:(fun sofar next -> next :: sofar)
      definition
    |> List.rev


  let attribute ?(transitive = false) ({ Node.location; _ } as definition) ~resolution ~name =
    let undefined =
      Attribute.create
        ~resolution
        ~parent:definition
        ~defined:false
        {
          Node.location;
          value = {
            Statement.Assign.target = Node.create (Expression.Access name);
            annotation = None;
            value = None;
            compound = None;
            parent = None;
          }
        }
    in
    let search sofar attribute =
      match sofar with
      | Some attribute -> Some attribute
      | None ->
          if Expression.equal_expression (Access name) (Attribute.name attribute) then
            Some attribute
          else
            None
    in
    attribute_fold ~transitive ~initial:None ~f:search ~resolution definition
    |> Option.value ~default:undefined
end


module Attribute = Class.Attribute
module Method = Class.Method


module Define = struct
  type t = Define.t
  [@@deriving compare, eq, sexp, show]


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
            (Node.create (Access parent))
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
end


module BinaryOperator = struct
  type t = Expression.t BinaryOperator.t
  [@@deriving compare, eq, sexp, show]


  let create operator =
    operator


  let override {
      BinaryOperator.left = ({ Node.location; _ } as left);
      operator;
      right;
    } =
    let name =
      let open BinaryOperator in
      match operator with
      | Add -> "__add__"
      | At -> "__matmul__"
      | BitAnd -> "__and__"
      | BitOr -> "__or__"
      | BitXor -> "__xor__"
      | Divide -> "__truediv__"
      | FloorDivide -> "__floordiv__"
      | LeftShift -> "__lshift__"
      | Modulo -> "__mod__"
      | Multiply -> "__mul__"
      | Power -> "__pow__"
      | RightShift -> "__rshift__"
      | Subtract -> "__sub__"
    in
    {
      Node.location;
      value = Access
          ((Access.access left) @ [
              Access.Call {
                Node.location;
                value = {
                  Call.name = {
                    Node.location;
                    value = Access (Access.create name);
                  };
                  arguments = [{ Argument.name = None; value = right }];
                };
              }
            ]);
    }
end


module Call = struct
  type kind =
    | Function
    | Method
  [@@deriving compare, eq, sexp, show]


  type t = {
    call: Call.t;
    kind: kind;
  }
  [@@deriving compare, eq, sexp, show]


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


  let insert_implicit_arguments ~callee { call; kind } =
    let call =
      callee
      >>| (fun { Signature.instantiated = callee; _ } ->
          let prepend_self call =
            let self =
              {
                Argument.name = None;
                value = Node.create (Access (Access.create "self"));
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
        (match Identifier.show name with
         | "abs" -> Some "__abs__"
         | "repr" -> Some "__repr__"
         | "str" -> Some "__str__"
         | _ -> None)
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
        (match Identifier.show name with
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
         | _ -> None)
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
      (match Map.find parameter_map name with
       | Some annotation when not (Type.is_meta annotation) -> Some annotation
       | _ -> None)
      >>= fun expected ->

      (* Compare to the actual type. *)
      (match value with
       | Signature.Normal { Signature.annotation = actual; _ }
       | Signature.Starred (Type.Parametric { Type.parameters = [actual]; _ }) ->
           Some actual
       | _ ->
           None)
      >>= fun actual ->
      check_parameter ~argument ~position ~offset ~location ~name ~actual ~expected
    in
    let accumulate_errors position (offset, errors) = function
      | { Node.value = Signature.Normal _; _ } as argument ->
          (match parameter_ok ~position ~offset argument with
           | Some error -> offset, add_error errors error
           | _ -> offset, errors)
      | { Node.value = Signature.Starred _; _ } as argument ->
          (* Angelic assumption: if we get a type error with a starred argument we move on
           * to the next argument, otherwise we keep consuming parameters. The offset tries
           * to match the next argument to one left of where it would be normally matched.
          *)
          (match parameter_ok ~position ~offset argument with
           | Some _ -> offset - 1, errors
           | _ -> offset, errors)
    in
    argument_annotations ~resolution call
    |> List.foldi ~init:(0, init) ~f:accumulate_errors
    |> snd
end


module ComparisonOperator = struct
  type t = Expression.t ComparisonOperator.t
  [@@deriving compare, eq, sexp, show]


  let override { ComparisonOperator.left; right } =
    let simple_override ({ Node.location; _ } as left) (operator, right) =
      let open ComparisonOperator in
      (match operator with
       | Equals -> Some "__eq__"
       | GreaterThan -> Some "__gt__"
       | GreaterThanOrEquals -> Some "__ge__"
       | In -> Some "__contains__"
       | Is
       | IsNot -> None
       | LessThan -> Some "__lt__"
       | LessThanOrEquals -> Some "__le__"
       | NotEquals -> Some "__ne__"
       | NotIn -> None)
      >>| (fun name ->
          {
            Node.location;
            value = Access
                ((Access.access left) @ [
                    Access.Call {
                      Node.location;
                      value = {
                        Expression.Call.name = {
                          Node.location;
                          value = Access (Access.create name);
                        };
                        arguments = [{ Argument.name = None; value = right }];
                      };
                    }
                  ]);
          })
    in
    let rec collect left operands =
      match operands with
      | (operator, right) :: operands ->
          (simple_override left (operator, right)) :: (collect right operands)
      | [] -> []
    in
    collect left right
end


module UnaryOperator = struct
  type t = Expression.t UnaryOperator.t
  [@@deriving compare, eq, sexp, show]


  let override { UnaryOperator.operator; operand = ({ Node.location; _ } as operand) } =
    let open UnaryOperator in
    (match operator with
     | Invert -> Some "__invert__"
     | Negative -> Some "__neg__"
     | Not -> None
     | Positive -> Some "__pos__")
    >>| (fun name ->
        {
          Node.location;
          value = Access
              ((Access.access operand) @ [
                  Access.Call {
                    Node.location;
                    value = {
                      Expression.Call.name = {
                        Node.location;
                        value = Access (Access.create name);
                      };
                      arguments = [];
                    };
                  }
                ]);
        })
end


module Access = struct
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
      | Array
      | Call of call
      | Expression
      | Attribute of Attribute.t
      | Global
      | Identifier
      | Method of method_call
  end


  type t = Access.t
  [@@deriving compare, eq, sexp, show]


  let create access =
    access


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
    let return_annotation resolution = function
      | Some { Signature.instantiated = callee; _ } ->
          Define.create callee
          |> Define.return_annotation ~resolution
      | None ->
          Type.Top
    in

    (* Calls on methods can determine previously undetermined annotations. E.g. `a.append(1)` can
        determine the type of `a: List[Bottom]` to `a: List[int]`. *)
    let determine_annotation ~element:{ Element.annotation; callee; _ } =
      let annotation = Annotation.annotation annotation in
      callee
      >>| (fun { Signature.constraints; _ } ->
          let primitive, parameters = Type.split annotation in
          let free_variables =
            (Resolution.class_definition resolution) primitive
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
    in

    (* Resolve `super()` calls. *)
    let access, resolution =
      match access with
      | (Access.Call { Node.value = { Expression.Call.name; _ }; _ }) :: tail
        when Expression.show name = "super" ->
          (Define.create define
           |> Define.parent_definition ~resolution
           >>| Class.superclasses ~resolution
           >>| function
           | superclass :: _ ->
               let super = Access.Identifier (Identifier.create "$super") in
               let resolution =
                 let annotation =
                   Class.annotation ~resolution superclass
                   |> Annotation.create
                 in
                 let annotations =
                   Map.add
                     ~key:[super]
                     ~data:annotation
                     (Resolution.annotations resolution)
                 in
                 Resolution.with_annotations resolution annotations
               in
               super :: tail, resolution
           | _ ->
               access, resolution)
          |> Option.value ~default:(access, resolution)
      | _ ->
          access, resolution
    in

    let rec fold ~accumulator ~reversed_lead ~tail ~annotation ~resolution =
      let annotations = Resolution.annotations resolution in
      let pick_signature call signatures =
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
              Call.check_parameters
                ~resolution
                ~check_parameter
                ~add_error
                ~init:0
                (Call.insert_implicit_arguments ~callee:(Some callee) call)
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
      in

      let rec step annotation reversed_lead =
        match annotation, reversed_lead with
        | Some (_, annotation), _ when Type.is_meta (Annotation.annotation annotation) ->
            (* Pretend that we're calling a method on the class object. This is not entierly
               acurate: `Type[A]` has a slightly broader interface than `A`. *)
            let annotation_name =
              let extract_access { Node.value; _ } =
                match value with
                | Access access -> access
                | _ -> failwith "Annotation expression is not an access"
              in
              Annotation.annotation annotation
              |> Type.parameters
              |> List.hd_exn
              |> Type.expression
              |> extract_access
              |> List.rev
            in
            step None (reversed_lead @ annotation_name)

        | Some (access, annotation),
          [Access.Call { Node.location; value = call }] ->
            let call = Call.create ~kind:Call.Method call in
            let callee =
              Resolution.method_signature
                resolution
                (Annotation.original annotation)
                (Call.call call)
                (Call.argument_annotations ~resolution call)
              |> pick_signature call
            in
            let backup =
              Call.backup call
              >>= fun call ->
              (match call with
               | {
                 Call.call = { Expression.Call.arguments = [{ Argument.value; _ }]; _ };
                 _;
               } ->
                   let annotation = Resolution.resolve resolution value in
                   Resolution.method_signature
                     resolution
                     annotation
                     (Call.call call)
                     (Call.argument_annotations ~resolution call)
                   |> pick_signature call
               | _ -> None)
              >>= fun signature -> Some (call, signature)
            in
            let element =
              {
                Element.location;
                access;
                annotation;
                call = Call.insert_implicit_arguments ~callee call;
                callee;
                backup;
              }
            in
            let determined = determine_annotation ~element in
            let resolved = Annotation.create (return_annotation resolution callee) in
            let annotations =
              Map.find annotations access
              >>| (fun existing ->
                  Map.add
                    ~key:access
                    ~data:{ existing with Annotation.annotation = determined }
                    annotations)
              |> Option.value
                ~default:(Map.add ~key:access ~data:(Annotation.create determined) annotations)
            in
            (Resolution.with_annotations resolution annotations),
            resolved,
            (f accumulator ~annotations ~resolved ~element:(Element.Method element))

        | Some (access, annotation), ([Access.Identifier _] as attribute_access) -> (
            (* Attribute access. *)
            let definition =
              Resolution.class_definition
                resolution
                (Annotation.annotation annotation)
            in
            (definition
             >>| fun definition ->
             let attribute =
               Class.attribute
                 ~transitive:true
                 ~resolution
                 ~name:attribute_access
                 definition
             in
             let access = access @ attribute_access in
             if not (Attribute.defined attribute) then
               let resolved =
                 Map.find annotations access
                 |> Option.value ~default:(Annotation.create Type.Top)
               in
               resolution,
               resolved,
               (f accumulator ~annotations ~resolved ~element:(Element.Attribute attribute))
             else
               match Map.find annotations access with
               | Some resolved ->
                   resolution,
                   annotation,
                   (f accumulator ~annotations ~resolved ~element:(Element.Attribute attribute))
               | None ->
                   let resolved = Attribute.annotation attribute in
                   resolution,
                   resolved,
                   (f accumulator ~annotations ~resolved ~element:(Element.Attribute attribute)))
            |> Option.value ~default:(resolution, Annotation.create Type.Top, accumulator))

        | Some (_, annotation), (Access.Subscript subscript) :: _ ->
            (* Array access. *)
            let resolved =
              match subscript, Annotation.annotation annotation with
              | [Access.Index _],
                (Type.Parametric {
                    Type.parameters;
                    _;
                  }) ->
                  (* TODO(T22845396): improve temporary fix *)
                  (match parameters with
                   | _ :: parameter :: _ -> parameter
                   | parameter :: _ -> parameter
                   | [] -> Type.Top)
              | [Access.Slice _], _ ->
                  (Annotation.annotation annotation)
              | _ ->
                  Type.Top
            in
            resolution,
            Annotation.create resolved,
            (f accumulator
               ~annotations
               ~resolved:(Annotation.create resolved)
               ~element:Element.Array)

        | None,
          Access.Call { Node.location; value = call } :: qualifier ->
            (* Call. *)
            begin
              let call = Call.create ~kind:Call.Function call in
              match Call.redirect call with
              | Some { Call.access; call } ->
                  let annotation =
                    Resolution.resolve
                      resolution
                      (Node.create (Access access))
                  in
                  step (Some (access, (Annotation.create annotation))) call
              | None ->
                  let callee =
                    Resolution.function_signature
                      resolution
                      (List.rev qualifier)
                      (Call.call call)
                      (Call.argument_annotations ~resolution call)
                    |> pick_signature call
                  in
                  let resolved = Annotation.create (return_annotation resolution callee) in
                  let element =
                    Element.Call {
                      Element.location;
                      call = Call.insert_implicit_arguments ~callee call;
                      callee;
                    }
                  in
                  resolution,
                  resolved,
                  (f accumulator ~annotations ~resolved ~element)
            end

        | None, Access.Expression expression :: _ ->
            let resolved = Annotation.create (Resolution.resolve resolution expression) in
            resolution,
            resolved,
            (f accumulator ~annotations ~resolved ~element:Element.Expression)

        | None, [Access.Identifier identifier]
          when Identifier.show identifier = "None" ->
            let resolved = Annotation.create (Type.Optional Type.Bottom) in
            resolution,
            resolved,
            (f accumulator ~annotations ~resolved ~element:Element.Identifier)

        | _ ->
            let resolved =
              let lead = List.rev reversed_lead in
              match Map.find annotations lead with
              | Some resolved -> resolved
              | None ->
                  match (Resolution.global resolution) lead with
                  | Some { Resolution.annotation; _ } -> annotation
                  | _ -> Annotation.create Type.Top
            in
            resolution,
            resolved,
            (f accumulator ~annotations ~resolved ~element:Element.Global)
      in
      let resolution, resolved, accumulator = step annotation reversed_lead in

      match Annotation.annotation resolved, tail with
      | Type.Top, head :: tail ->
          fold
            ~resolution
            ~accumulator
            ~reversed_lead:(head :: reversed_lead)
            ~tail
            ~annotation:None
      | _, head :: tail ->
          fold
            ~resolution
            ~accumulator
            ~reversed_lead:[head]
            ~tail
            ~annotation:(Some (List.rev reversed_lead, resolved))
      | _ ->
          accumulator
    in
    fold ~resolution ~accumulator:initial ~reversed_lead:[] ~tail:access ~annotation:None


  let last_element ~resolution access =
    fold
      ~resolution
      ~initial:Element.Global
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
              Map.add annotations_sofar ~key:access ~data:(Annotation.create annotation)
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
        Map.add ~key ~data:updated_annotation (Resolution.annotations resolution)
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
