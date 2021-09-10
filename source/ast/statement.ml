(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre

module Assign = struct
  type t = {
    target: Expression.t;
    annotation: Expression.t option;
    value: Expression.t;
    parent: Reference.t option;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  let is_static_attribute_initialization { parent; _ } = Option.is_some parent

  let location_insensitive_compare left right =
    match Expression.location_insensitive_compare left.target right.target with
    | x when not (Int.equal x 0) -> x
    | _ -> (
        match
          Option.compare Expression.location_insensitive_compare left.annotation right.annotation
        with
        | x when not (Int.equal x 0) -> x
        | _ -> (
            match Expression.location_insensitive_compare left.value right.value with
            | x when not (Int.equal x 0) -> x
            | _ -> [%compare: Reference.t option] left.parent right.parent))
end

module Import = struct
  type import = {
    name: Reference.t Node.t;
    alias: Identifier.t Node.t option;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  type t = {
    from: Reference.t Node.t option;
    imports: import list;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    let location_insensitive_compare_import left right =
      match
        Option.compare
          (Node.location_insensitive_compare [%compare: Identifier.t])
          left.alias
          right.alias
      with
      | x when not (Int.equal x 0) -> x
      | _ -> Node.location_insensitive_compare [%compare: Reference.t] left.name right.name
    in
    match
      Option.compare
        (Node.location_insensitive_compare [%compare: Reference.t])
        left.from
        right.from
    with
    | x when not (Int.equal x 0) -> x
    | _ -> List.compare location_insensitive_compare_import left.imports right.imports
end

module Raise = struct
  type t = {
    expression: Expression.t option;
    from: Expression.t option;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    match
      Option.compare Expression.location_insensitive_compare left.expression right.expression
    with
    | x when not (Int.equal x 0) -> x
    | _ -> Option.compare Expression.location_insensitive_compare left.from right.from
end

module Return = struct
  type t = {
    is_implicit: bool;
    expression: Expression.t option;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    match Bool.compare left.is_implicit right.is_implicit with
    | x when not (Int.equal x 0) -> x
    | _ -> Option.compare Expression.location_insensitive_compare left.expression right.expression
end

module Decorator = struct
  type t = {
    name: Reference.t Node.t;
    arguments: Expression.Call.Argument.t list option;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    match Reference.compare left.name.value right.name.value with
    | x when not (Int.equal x 0) -> x
    | _ ->
        (Option.compare (List.compare Expression.Call.Argument.location_insensitive_compare))
          left.arguments
          right.arguments


  let to_expression { name = { Node.value = name; location }; arguments } =
    let name = Expression.from_reference ~location name in
    match arguments with
    | Some arguments ->
        Node.create ~location (Expression.Expression.Call { callee = name; arguments })
    | None -> name
end

module rec Assert : sig
  module Origin : sig
    type t =
      | Assertion
      | If of { true_branch: bool }
      | While of { true_branch: bool }
    [@@deriving compare, eq, sexp, show, hash, to_yojson]

    val location_insensitive_compare : t -> t -> int
  end

  type t = {
    test: Expression.t;
    message: Expression.t option;
    origin: Origin.t;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end = struct
  module Origin = struct
    type t =
      | Assertion
      | If of { true_branch: bool }
      | While of { true_branch: bool }
    [@@deriving compare, eq, sexp, show, hash, to_yojson]

    let location_insensitive_compare left right : int =
      match left, right with
      | Assertion, Assertion -> 0
      | If left, If right -> Bool.compare left.true_branch right.true_branch
      | While left, While right -> Bool.compare left.true_branch right.true_branch
      | Assertion, _ -> -1
      | If _, _ -> -1
      | While _, _ -> 1
  end

  type t = {
    test: Expression.t;
    message: Expression.t option;
    origin: Origin.t;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    match Expression.location_insensitive_compare left.test right.test with
    | x when not (Int.equal x 0) -> x
    | _ -> (
        match Option.compare Expression.location_insensitive_compare left.message right.message with
        | x when not (Int.equal x 0) -> x
        | _ -> Origin.location_insensitive_compare left.origin right.origin)
end

and Class : sig
  type t = {
    name: Reference.t Node.t;
    base_arguments: Expression.Call.Argument.t list;
    body: Statement.t list;
    decorators: Decorator.t list;
    top_level_unbound_names: Define.NameAccess.t list;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int

  val toplevel_define : t -> Define.t

  val constructors : ?in_test:bool -> t -> Define.t list

  val defines : t -> Define.t list

  val find_define : t -> method_name:Identifier.t -> Define.t Node.t option

  val is_frozen : t -> bool

  val base_classes : t -> Expression.t list

  val metaclass : t -> Expression.t option

  val init_subclass_arguments : t -> Expression.Call.Argument.t list

  type class_t = t [@@deriving compare, eq, sexp, show, hash, to_yojson]
end = struct
  type t = {
    name: Reference.t Node.t;
    base_arguments: Expression.Call.Argument.t list;
    body: Statement.t list;
    decorators: Decorator.t list;
    top_level_unbound_names: Define.NameAccess.t list;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  type class_t = t [@@deriving compare, eq, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    match Node.location_insensitive_compare [%compare: Reference.t] left.name right.name with
    | x when not (Int.equal x 0) -> x
    | _ -> (
        match
          List.compare
            Expression.Call.Argument.location_insensitive_compare
            left.base_arguments
            right.base_arguments
        with
        | x when not (Int.equal x 0) -> x
        | _ -> (
            match List.compare Statement.location_insensitive_compare left.body right.body with
            | x when not (Int.equal x 0) -> x
            | _ -> (
                match
                  List.compare
                    Decorator.location_insensitive_compare
                    left.decorators
                    right.decorators
                with
                | x when not (Int.equal x 0) -> x
                | _ ->
                    List.compare
                      Define.NameAccess.compare
                      left.top_level_unbound_names
                      right.top_level_unbound_names)))


  let toplevel_define { name = { Node.value; _ }; top_level_unbound_names; body; _ } =
    Define.create_class_toplevel
      ~unbound_names:top_level_unbound_names
      ~parent:value
      ~statements:body


  let constructors ?(in_test = false) { body; _ } =
    let constructor = function
      | { Node.value = Statement.Define define; _ } when Define.is_constructor ~in_test define ->
          Some define
      | _ -> None
    in
    List.filter_map ~f:constructor body


  let defines { body; _ } =
    let define = function
      | { Node.value = Statement.Define define; _ } -> Some define
      | _ -> None
    in
    List.filter_map ~f:define body


  let find_define { body; _ } ~method_name =
    let is_define = function
      | { Node.value = Statement.Define define; location }
        when String.equal (Define.unqualified_name define) method_name ->
          Some { Node.value = define; location }
      | _ -> None
    in
    List.filter_map ~f:is_define body |> List.hd


  let is_frozen { decorators; _ } =
    let open Expression in
    let is_frozen_dataclass decorator =
      match decorator with
      | { Decorator.name = { Node.value = name; _ }; arguments = Some arguments }
        when Reference.equal name (Reference.create "dataclasses.dataclass") ->
          let has_frozen_argument Call.Argument.{ name; value } =
            match name, value with
            | Some { Node.value; _ }, { Node.value = Expression.True; _ } ->
                String.equal "frozen" (Identifier.sanitized value)
            | _, _ -> false
          in
          List.exists arguments ~f:has_frozen_argument
      | _ -> false
    in
    List.exists decorators ~f:is_frozen_dataclass


  let base_classes { base_arguments; _ } =
    List.fold
      ~init:[]
      ~f:(fun base_classes { Expression.Call.Argument.name; value } ->
        if Option.is_some name then base_classes else value :: base_classes)
      base_arguments
    |> List.rev


  let metaclass { base_arguments; _ } =
    List.find
      ~f:(fun { Expression.Call.Argument.name; _ } ->
        match name with
        | Some { Node.value = base_argument_name; _ }
          when String.equal base_argument_name "metaclass" ->
            true
        | _ -> false)
      base_arguments
    >>| fun { Expression.Call.Argument.value; _ } -> value


  let init_subclass_arguments { base_arguments; _ } =
    List.filter
      ~f:(fun { Expression.Call.Argument.name; _ } ->
        match name with
        | Some { Node.value = base_argument_name; _ }
          when String.equal base_argument_name "metaclass" ->
            false
        | Some _ -> true
        | None -> false)
      base_arguments
end

and Define : sig
  module Signature : sig
    type t = {
      name: Reference.t Node.t;
      parameters: Expression.Parameter.t list;
      decorators: Decorator.t list;
      return_annotation: Expression.t option;
      async: bool;
      generator: bool;
      (* The class owning the method. *)
      parent: Reference.t option;
      (* If the define is nested, this is the name of the nesting define. *)
      nesting_define: Reference.t option;
    }
    [@@deriving compare, eq, sexp, show, hash, to_yojson]

    val location_insensitive_compare : t -> t -> int

    val create_toplevel : qualifier:Reference.t option -> t

    val create_class_toplevel : parent:Reference.t -> t

    val unqualified_name : t -> Identifier.t

    val self_identifier : t -> Identifier.t

    val is_method : t -> bool

    val is_coroutine : t -> bool

    val is_abstract_method : t -> bool

    val is_overloaded_function : t -> bool

    val is_static_method : t -> bool

    val is_final_method : t -> bool

    val is_class_method : t -> bool

    val is_class_property : t -> bool

    val is_dunder_method : t -> bool

    val is_constructor : ?in_test:bool -> t -> bool

    val is_property_setter : t -> bool

    val is_untyped : t -> bool

    val is_toplevel : t -> bool

    val is_class_toplevel : t -> bool

    val has_decorator : ?match_prefix:bool -> t -> string -> bool

    val has_return_annotation : t -> bool
  end

  module Capture : sig
    module Kind : sig
      type t =
        | Annotation of Expression.t option
        | Self of Reference.t
        | ClassSelf of Reference.t
        | DefineSignature of Define.Signature.t
      [@@deriving compare, eq, sexp, show, hash, to_yojson]
    end

    type t = {
      name: Identifier.t;
      kind: Kind.t;
    }
    [@@deriving compare, eq, sexp, show, hash, to_yojson]
  end

  module NameAccess : sig
    type t = {
      name: Identifier.t;
      location: Location.t;
    }
    [@@deriving compare, eq, sexp, show, hash, to_yojson]
  end

  type t = {
    signature: Signature.t;
    captures: Capture.t list;
    unbound_names: NameAccess.t list;
    body: Statement.t list;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int

  val create_toplevel
    :  unbound_names:NameAccess.t list ->
    qualifier:Reference.t option ->
    statements:Statement.t list ->
    t

  val create_class_toplevel
    :  unbound_names:NameAccess.t list ->
    parent:Reference.t ->
    statements:Statement.t list ->
    t

  val name : t -> Reference.t Node.t

  val unqualified_name : t -> Identifier.t

  val self_identifier : t -> Identifier.t

  val is_method : t -> bool

  val is_coroutine : t -> bool

  val is_abstract_method : t -> bool

  val is_overloaded_function : t -> bool

  val is_static_method : t -> bool

  val is_final_method : t -> bool

  val is_class_method : t -> bool

  val is_class_property : t -> bool

  val is_dunder_method : t -> bool

  val is_constructor : ?in_test:bool -> t -> bool

  val is_test_setup : t -> bool

  val is_property_setter : t -> bool

  val is_untyped : t -> bool

  val is_stub : t -> bool

  val is_toplevel : t -> bool

  val is_class_toplevel : t -> bool

  val is_async : t -> bool

  val dump : t -> bool

  val dump_cfg : t -> bool

  val dump_locations : t -> bool

  val dump_call_graph : t -> bool

  val show_json : t -> string

  val has_decorator : ?match_prefix:bool -> t -> string -> bool

  val has_return_annotation : t -> bool
end = struct
  module Signature = struct
    type t = {
      name: Reference.t Node.t;
      parameters: Expression.Parameter.t list;
      decorators: Decorator.t list;
      return_annotation: Expression.t option;
      async: bool;
      generator: bool;
      (* The class owning the method *)
      parent: Reference.t option;
      (* If the define is nested, this is the name of the nesting define. *)
      nesting_define: Reference.t option;
    }
    [@@deriving compare, eq, sexp, show, hash, to_yojson]

    let location_insensitive_compare left right =
      match Node.location_insensitive_compare [%compare: Reference.t] left.name right.name with
      | x when not (Int.equal x 0) -> x
      | _ -> (
          match
            List.compare
              Expression.Parameter.location_insensitive_compare
              left.parameters
              right.parameters
          with
          | x when not (Int.equal x 0) -> x
          | _ -> (
              match
                List.compare Decorator.location_insensitive_compare left.decorators right.decorators
              with
              | x when not (Int.equal x 0) -> x
              | _ -> (
                  match
                    Option.compare
                      Expression.location_insensitive_compare
                      left.return_annotation
                      right.return_annotation
                  with
                  | x when not (Int.equal x 0) -> x
                  | _ -> (
                      match Bool.compare left.async right.async with
                      | x when not (Int.equal x 0) -> x
                      | _ -> (
                          match Bool.compare left.generator right.generator with
                          | x when not (Int.equal x 0) -> x
                          | _ -> (
                              match [%compare: Reference.t option] left.parent right.parent with
                              | x when not (Int.equal x 0) -> x
                              | _ ->
                                  [%compare: Reference.t option]
                                    left.nesting_define
                                    right.nesting_define))))))


    let create_toplevel ~qualifier =
      {
        name = Reference.create ?prefix:qualifier "$toplevel" |> Node.create_with_default_location;
        parameters = [];
        decorators = [];
        return_annotation = None;
        async = false;
        generator = false;
        parent = None;
        nesting_define = None;
      }


    let create_class_toplevel ~parent =
      {
        name =
          Reference.create ~prefix:parent "$class_toplevel" |> Node.create_with_default_location;
        parameters = [];
        decorators = [];
        return_annotation = None;
        async = false;
        generator = false;
        parent = Some parent;
        nesting_define = None;
      }


    let unqualified_name { name; _ } = Reference.last (Node.value name)

    let self_identifier { parameters; _ } =
      match parameters with
      | { Node.value = { Expression.Parameter.name; _ }; _ } :: _ -> name
      | _ -> "self"


    let is_method { parent; _ } = Option.is_some parent

    let has_decorator ?(match_prefix = false) { decorators; _ } decorator =
      let decorators = List.map decorators ~f:Decorator.to_expression in
      Expression.exists_in_list ~match_prefix ~expression_list:decorators decorator


    let has_return_annotation { return_annotation; _ } = Option.is_some return_annotation

    let is_coroutine signature = has_decorator signature "asyncio.coroutines.coroutine"

    let is_abstract_method signature =
      has_decorator signature "abstractmethod"
      || has_decorator signature "abc.abstractmethod"
      || has_decorator signature "abstractproperty"
      || has_decorator signature "abc.abstractproperty"


    let is_overloaded_function signature =
      has_decorator signature "overload" || has_decorator signature "typing.overload"


    let is_static_method signature =
      (* `__new__` is always a static method. See
         `https://docs.python.org/3/reference/datamodel.html#object.__new__`. *)
      String.equal (unqualified_name signature) "__new__" || has_decorator signature "staticmethod"


    let is_final_method signature = has_decorator signature "typing.final"

    let is_dunder_method signature =
      let name = unqualified_name signature in
      String.is_prefix ~prefix:"__" name && String.is_suffix ~suffix:"__" name


    let is_class_method ({ parent; _ } as signature) =
      let valid_names = ["__init_subclass__"; "__new__"; "__class_getitem__"] in
      Option.is_some parent
      && (Set.exists Recognized.classmethod_decorators ~f:(has_decorator signature)
         || List.mem valid_names (unqualified_name signature) ~equal:String.equal)


    let is_class_property ({ parent; _ } as signature) =
      Option.is_some parent
      && Set.exists Recognized.classproperty_decorators ~f:(has_decorator signature)


    let test_initializers =
      String.Set.of_list
        [
          "asyncSetUp";
          "async_setUp";
          "setUp";
          "_setup";
          "_async_setup";
          "async_with_context";
          "with_context";
          "setUpClass";
        ]


    let is_test_setup ({ parent; _ } as signature) =
      let name = unqualified_name signature in
      if Option.is_none parent then
        false
      else
        Set.mem test_initializers name


    let is_constructor ?(in_test = false) ({ parent; _ } as signature) =
      let name = unqualified_name signature in
      if Option.is_none parent then
        false
      else
        String.equal name "__init__"
        || String.equal name "__new__"
        || String.equal name "__init_subclass__"
        || (in_test && is_test_setup signature)


    let is_property_setter signature =
      has_decorator signature (unqualified_name signature ^ ".setter")


    let is_untyped { return_annotation; parameters; _ } =
      Option.is_none return_annotation
      && List.for_all
           parameters
           ~f:(fun { Node.value = { Expression.Parameter.annotation; _ }; _ } ->
             Option.is_none annotation)


    let is_async { async; _ } = async

    let is_toplevel signature = String.equal (unqualified_name signature) "$toplevel"

    let is_class_toplevel signature = String.equal (unqualified_name signature) "$class_toplevel"
  end

  module Capture = struct
    module Kind = struct
      type t =
        | Annotation of Expression.t option
        | Self of Reference.t
        | ClassSelf of Reference.t
        | DefineSignature of Define.Signature.t
      [@@deriving compare, eq, sexp, show, hash, to_yojson]

      let location_insensitive_compare left right =
        match left, right with
        | Annotation left, Annotation right ->
            Option.compare Expression.location_insensitive_compare left right
        | Self left, Self right -> Reference.compare left right
        | ClassSelf left, ClassSelf right -> Reference.compare left right
        | DefineSignature left, DefineSignature right ->
            Define.Signature.location_insensitive_compare left right
        | Annotation _, _ -> -1
        | Self _, _ -> -1
        | ClassSelf _, _ -> -1
        | DefineSignature _, _ -> 1
    end

    type t = {
      name: Identifier.t;
      kind: Kind.t;
    }
    [@@deriving compare, eq, sexp, show, hash, to_yojson]

    let location_insensitive_compare left right =
      match [%compare: Identifier.t] left.name right.name with
      | x when not (Int.equal x 0) -> x
      | _ -> Kind.location_insensitive_compare left.kind right.kind
  end

  module NameAccess = struct
    type t = {
      name: Identifier.t;
      location: Location.t;
    }
    [@@deriving compare, eq, sexp, show, hash, to_yojson]

    let location_insensitive_compare left right = [%compare: Identifier.t] left.name right.name
  end

  type t = {
    signature: Signature.t;
    captures: Capture.t list;
    unbound_names: NameAccess.t list;
    body: Statement.t list;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    match Signature.location_insensitive_compare left.signature right.signature with
    | x when not (Int.equal x 0) -> x
    | _ -> (
        match List.compare Capture.location_insensitive_compare left.captures right.captures with
        | x when not (Int.equal x 0) -> x
        | _ -> (
            match
              List.compare
                NameAccess.location_insensitive_compare
                left.unbound_names
                right.unbound_names
            with
            | x when not (Int.equal x 0) -> x
            | _ -> List.compare Statement.location_insensitive_compare left.body right.body))


  let create_toplevel ~unbound_names ~qualifier ~statements =
    {
      signature = Signature.create_toplevel ~qualifier;
      captures = [];
      unbound_names;
      body = statements;
    }


  let create_class_toplevel ~unbound_names ~parent ~statements =
    {
      signature = Signature.create_class_toplevel ~parent;
      captures = [];
      unbound_names;
      body = statements;
    }


  let name { signature = { Signature.name; _ }; _ } = name

  let unqualified_name { signature; _ } = Signature.unqualified_name signature

  let self_identifier { signature; _ } = Signature.self_identifier signature

  let is_method { signature; _ } = Signature.is_method signature

  let has_decorator ?(match_prefix = false) { signature; _ } decorator =
    Signature.has_decorator ~match_prefix signature decorator


  let has_return_annotation { signature; _ } = Signature.has_return_annotation signature

  let is_coroutine { signature; _ } = Signature.is_coroutine signature

  let is_abstract_method { signature; _ } = Signature.is_abstract_method signature

  let is_overloaded_function { signature; _ } = Signature.is_overloaded_function signature

  let is_static_method { signature; _ } = Signature.is_static_method signature

  let is_final_method { signature; _ } = Signature.is_final_method signature

  let is_dunder_method { signature; _ } = Signature.is_dunder_method signature

  let is_class_method { signature; _ } = Signature.is_class_method signature

  let is_class_property { signature; _ } = Signature.is_class_property signature

  let is_constructor ?(in_test = false) { signature; _ } =
    Signature.is_constructor ~in_test signature


  let is_test_setup { signature; _ } = Signature.is_test_setup signature

  let is_property_setter { signature; _ } = Signature.is_property_setter signature

  let is_untyped { signature; _ } = Signature.is_untyped signature

  let is_async { signature; _ } = Signature.is_async signature

  let is_toplevel { signature; _ } = Signature.is_toplevel signature

  let is_class_toplevel { signature; _ } = Signature.is_class_toplevel signature

  let contains_call { body; _ } name =
    let open Expression in
    let matches = function
      | {
          Node.value =
            Statement.Expression
              {
                Node.value =
                  Expression.Call
                    { callee = { Node.value = Expression.Name (Name.Identifier identifier); _ }; _ };
                _;
              };
          _;
        }
        when String.equal identifier name ->
          true
      | _ -> false
    in
    List.exists ~f:matches body


  let is_stub { body; _ } =
    let open Expression in
    match List.rev body with
    | { Node.value = Expression { Node.value = Expression.Ellipsis; _ }; _ } :: _
    | _ :: { Node.value = Expression { Node.value = Expression.Ellipsis; _ }; _ } :: _ ->
        true
    | _ -> false


  let dump define = contains_call define "pyre_dump"

  let dump_cfg define = contains_call define "pyre_dump_cfg"

  let dump_locations define = contains_call define "pyre_dump_locations"

  let dump_call_graph define = contains_call define "pyre_dump_call_graph"

  let show_json define = define |> to_yojson |> Yojson.Safe.pretty_to_string
end

and For : sig
  type t = {
    target: Expression.t;
    iterator: Expression.t;
    body: Statement.t list;
    orelse: Statement.t list;
    async: bool;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val preamble : t -> Statement.t

  val location_insensitive_compare : t -> t -> int
end = struct
  type t = {
    target: Expression.t;
    iterator: Expression.t;
    body: Statement.t list;
    orelse: Statement.t list;
    async: bool;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  let preamble { target = { Node.location; _ } as target; iterator; async; _ } =
    let open Expression in
    let value =
      let value =
        let create_call base iterator next =
          Expression.Call
            {
              callee =
                {
                  Node.location;
                  value =
                    Name
                      (Name.Attribute
                         {
                           base =
                             {
                               Node.location;
                               value =
                                 Call
                                   {
                                     callee =
                                       {
                                         Node.location;
                                         value =
                                           Name
                                             (Name.Attribute
                                                { base; attribute = iterator; special = true });
                                       };
                                     arguments = [];
                                   };
                             };
                           attribute = next;
                           special = true;
                         });
                };
              arguments = [];
            }
        in
        if async then
          create_call iterator "__aiter__" "__anext__"
        else
          create_call iterator "__iter__" "__next__"
      in
      if async then
        { Node.location; value = Expression.Await (Node.create value ~location) }
      else
        { Node.location; value }
    in
    {
      Node.location;
      value = Statement.Assign { Assign.target; annotation = None; value; parent = None };
    }


  let location_insensitive_compare left right =
    match Expression.location_insensitive_compare left.target right.target with
    | x when not (Int.equal x 0) -> x
    | _ -> (
        match Expression.location_insensitive_compare left.iterator right.iterator with
        | x when not (Int.equal x 0) -> x
        | _ -> (
            match List.compare Statement.location_insensitive_compare left.body right.body with
            | x when not (Int.equal x 0) -> x
            | _ -> (
                match
                  List.compare Statement.location_insensitive_compare left.orelse right.orelse
                with
                | x when not (Int.equal x 0) -> x
                | _ -> Bool.compare left.async right.async)))
end

and If : sig
  type t = {
    test: Expression.t;
    body: Statement.t list;
    orelse: Statement.t list;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end = struct
  type t = {
    test: Expression.t;
    body: Statement.t list;
    orelse: Statement.t list;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    match Expression.location_insensitive_compare left.test right.test with
    | x when not (Int.equal x 0) -> x
    | _ -> (
        match List.compare Statement.location_insensitive_compare left.body right.body with
        | x when not (Int.equal x 0) -> x
        | _ -> List.compare Statement.location_insensitive_compare left.orelse right.orelse)
end

and Try : sig
  module Handler : sig
    type t = {
      kind: Expression.t option;
      name: Identifier.t option;
      body: Statement.t list;
    }
    [@@deriving compare, eq, sexp, show, hash, to_yojson]

    val location_insensitive_compare : t -> t -> int
  end

  type t = {
    body: Statement.t list;
    handlers: Handler.t list;
    orelse: Statement.t list;
    finally: Statement.t list;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val preamble : Handler.t -> Statement.t list

  val location_insensitive_compare : t -> t -> int
end = struct
  module Handler = struct
    type t = {
      kind: Expression.t option;
      name: Identifier.t option;
      body: Statement.t list;
    }
    [@@deriving compare, eq, sexp, show, hash, to_yojson]

    let location_insensitive_compare left right =
      match Option.compare Expression.location_insensitive_compare left.kind right.kind with
      | x when not (Int.equal x 0) -> x
      | _ -> (
          match [%compare: Identifier.t option] left.name right.name with
          | x when not (Int.equal x 0) -> x
          | _ -> List.compare Statement.location_insensitive_compare left.body right.body)
  end

  type t = {
    body: Statement.t list;
    handlers: Handler.t list;
    orelse: Statement.t list;
    finally: Statement.t list;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  let preamble { Handler.kind; name; _ } =
    let open Expression in
    let assume ~location ~target ~annotation =
      [
        {
          Node.location;
          value =
            Statement.Assign
              {
                Assign.target;
                annotation = None;
                value = Node.create ~location Expression.Ellipsis;
                parent = None;
              };
        };
        {
          Node.location;
          value =
            Assert
              {
                Assert.test =
                  {
                    Node.location;
                    value =
                      Call
                        {
                          callee = { Node.location; value = Name (Name.Identifier "isinstance") };
                          arguments =
                            [
                              { Call.Argument.name = None; value = target };
                              { Call.Argument.name = None; value = annotation };
                            ];
                        };
                  };
                message = None;
                origin = Assert.Origin.Assertion;
              };
        };
      ]
    in
    match kind, name with
    | Some ({ Node.location; value = Name _ | Tuple _; _ } as annotation), Some name ->
        assume ~location ~target:{ Node.location; value = Name (Name.Identifier name) } ~annotation
    | Some ({ Node.location; _ } as expression), _ ->
        (* Insert raw `kind` so that we type check the expression. *)
        [Node.create ~location (Statement.Expression expression)]
    | _ -> []


  let location_insensitive_compare left right =
    match List.compare Statement.location_insensitive_compare left.body right.body with
    | x when not (Int.equal x 0) -> x
    | _ -> (
        match List.compare Handler.location_insensitive_compare left.handlers right.handlers with
        | x when not (Int.equal x 0) -> x
        | _ -> (
            match List.compare Statement.location_insensitive_compare left.orelse right.orelse with
            | x when not (Int.equal x 0) -> x
            | _ -> List.compare Statement.location_insensitive_compare left.finally right.finally))
end

and While : sig
  type t = {
    test: Expression.t;
    body: Statement.t list;
    orelse: Statement.t list;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end = struct
  type t = {
    test: Expression.t;
    body: Statement.t list;
    orelse: Statement.t list;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    match Expression.location_insensitive_compare left.test right.test with
    | x when not (Int.equal x 0) -> x
    | _ -> (
        match List.compare Statement.location_insensitive_compare left.body right.body with
        | x when not (Int.equal x 0) -> x
        | _ -> List.compare Statement.location_insensitive_compare left.orelse right.orelse)
end

and With : sig
  type t = {
    items: (Expression.t * Expression.t option) list;
    body: Statement.t list;
    async: bool;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val preamble : t -> Statement.t list

  val location_insensitive_compare : t -> t -> int
end = struct
  type t = {
    items: (Expression.t * Expression.t option) list;
    body: Statement.t list;
    async: bool;
  }
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  let location_insensitive_compare_item (left_value, left_target) (right_value, right_target) =
    match Expression.location_insensitive_compare left_value right_value with
    | x when not (Int.equal x 0) -> x
    | _ -> Option.compare Expression.location_insensitive_compare left_target right_target


  let location_insensitive_compare left right =
    match List.compare location_insensitive_compare_item left.items right.items with
    | x when not (Int.equal x 0) -> x
    | _ -> (
        match List.compare Statement.location_insensitive_compare left.body right.body with
        | x when not (Int.equal x 0) -> x
        | _ -> Bool.compare left.async right.async)


  let preamble { items; async; _ } =
    let preamble (({ Node.location; _ } as expression), target) =
      let open Expression in
      let enter_call =
        let create_call call_name =
          {
            Node.location;
            value =
              Expression.Call
                {
                  callee =
                    {
                      Node.location;
                      value =
                        Name
                          (Name.Attribute
                             { base = expression; attribute = call_name; special = true });
                    };
                  arguments = [];
                };
          }
        in
        if async then
          Node.create ~location (Expression.Await (create_call "__aenter__"))
        else
          create_call "__enter__"
      in
      match target with
      | Some target ->
          let assign = { Assign.target; annotation = None; value = enter_call; parent = None } in
          Node.create ~location (Statement.Assign assign)
      | None -> Node.create ~location (Statement.Expression enter_call)
    in
    List.map items ~f:preamble
end

and Statement : sig
  type statement =
    | Assign of Assign.t
    | Assert of Assert.t
    | Break
    | Class of Class.t
    | Continue
    | Define of Define.t
    | Delete of Expression.t
    | Expression of Expression.t
    | For of For.t
    | Global of Identifier.t list
    | If of If.t
    | Import of Import.t
    | Nonlocal of Identifier.t list
    | Pass
    | Raise of Raise.t
    | Return of Return.t
    | Try of Try.t
    | With of With.t
    | While of While.t
  [@@deriving compare, eq, sexp, hash, to_yojson]

  type t = statement Node.t [@@deriving compare, eq, sexp, show, hash, to_yojson]

  val assume : ?origin:Assert.Origin.t -> Expression.t -> t

  val generator_assignment : Expression.Comprehension.Generator.t -> Assign.t

  val location_insensitive_compare : t -> t -> int
end = struct
  type statement =
    | Assign of Assign.t
    | Assert of Assert.t
    | Break
    | Class of Class.t
    | Continue
    | Define of Define.t
    | Delete of Expression.t
    | Expression of Expression.t
    | For of For.t
    | Global of Identifier.t list
    | If of If.t
    | Import of Import.t
    | Nonlocal of Identifier.t list
    | Pass
    | Raise of Raise.t
    | Return of Return.t
    | Try of Try.t
    | With of With.t
    | While of While.t
  [@@deriving compare, eq, sexp, show, hash, to_yojson]

  type t = statement Node.t [@@deriving compare, eq, sexp, show, hash, to_yojson]

  let rec location_insensitive_compare_statement left right =
    match left, right with
    | Assign left, Assign right -> Assign.location_insensitive_compare left right
    | Assert left, Assert right -> Assert.location_insensitive_compare left right
    | Break, Break -> 0
    | Class left, Class right -> Class.location_insensitive_compare left right
    | Continue, Continue -> 0
    | Define left, Define right -> Define.location_insensitive_compare left right
    | Delete left, Delete right -> Expression.location_insensitive_compare left right
    | Expression left, Expression right -> Expression.location_insensitive_compare left right
    | For left, For right -> For.location_insensitive_compare left right
    | Global left, Global right -> List.compare Identifier.compare left right
    | If left, If right -> If.location_insensitive_compare left right
    | Import left, Import right -> Import.location_insensitive_compare left right
    | Nonlocal left, Nonlocal right -> List.compare Identifier.compare left right
    | Pass, Pass -> 0
    | Raise left, Raise right -> Raise.location_insensitive_compare left right
    | Return left, Return right -> Return.location_insensitive_compare left right
    | Try left, Try right -> Try.location_insensitive_compare left right
    | With left, With right -> With.location_insensitive_compare left right
    | While left, While right -> While.location_insensitive_compare left right
    | Assign _, _ -> -1
    | Assert _, _ -> -1
    | Break, _ -> -1
    | Class _, _ -> -1
    | Continue, _ -> -1
    | Define _, _ -> -1
    | Delete _, _ -> -1
    | Expression _, _ -> -1
    | For _, _ -> -1
    | Global _, _ -> -1
    | If _, _ -> -1
    | Import _, _ -> -1
    | Nonlocal _, _ -> -1
    | Pass, _ -> -1
    | Raise _, _ -> -1
    | Return _, _ -> -1
    | Try _, _ -> -1
    | With _, _ -> -1
    | While _, _ -> -1


  and location_insensitive_compare left right =
    Node.location_insensitive_compare location_insensitive_compare_statement left right


  let assume ?(origin = Assert.Origin.Assertion) ({ Node.location; _ } as test) =
    { Node.location; value = Assert { Assert.test; message = None; origin } }


  let generator_assignment
      {
        Expression.Comprehension.Generator.target;
        iterator = { Node.location; _ } as iterator;
        async;
        _;
      }
    =
    let open Expression in
    let value =
      if async then
        let aiter =
          {
            Node.location;
            value =
              Expression.Call
                {
                  callee =
                    {
                      Node.location;
                      value =
                        Name
                          (Name.Attribute
                             { base = iterator; attribute = "__aiter__"; special = true });
                    };
                  arguments = [];
                };
          }
        in
        {
          Node.location;
          value =
            Expression.Call
              {
                callee =
                  {
                    Node.location;
                    value =
                      Name
                        (Name.Attribute { base = aiter; attribute = "__anext__"; special = true });
                  };
                arguments = [];
              };
        }
        |> fun target -> Node.create ~location (Expression.Await target)
      else
        let iter =
          {
            Node.location;
            value =
              Expression.Call
                {
                  callee =
                    {
                      Node.location;
                      value =
                        Name
                          (Name.Attribute
                             { base = iterator; attribute = "__iter__"; special = true });
                    };
                  arguments = [];
                };
          }
        in
        {
          Node.location;
          value =
            Expression.Call
              {
                callee =
                  {
                    Node.location;
                    value =
                      Name (Name.Attribute { base = iter; attribute = "__next__"; special = true });
                  };
                arguments = [];
              };
        }
    in
    { Assign.target; annotation = None; value; parent = None }
end

include Statement

let _ = show (* shadowed below *)

module PrettyPrinter = struct
  let pp_decorators formatter = function
    | [] -> ()
    | decorators ->
        let decorators = List.map decorators ~f:Decorator.to_expression in
        Format.fprintf formatter "@[<v>@@(%a)@;@]" Expression.pp_expression_list decorators


  let pp_reference_option formatter = function
    | None -> ()
    | Some reference -> Format.fprintf formatter "%a" Reference.pp reference


  let pp_list formatter pp sep list =
    let rec pp' formatter = function
      | [] -> ()
      | [x] -> Format.fprintf formatter "%a" pp x
      | x :: xs -> Format.fprintf formatter ("%a" ^^ sep ^^ "%a") pp x pp' xs
    in
    pp' formatter list


  let pp_option ?(prefix = "") ?(suffix = "") formatter option pp =
    Option.value_map option ~default:() ~f:(fun value ->
        Format.fprintf formatter "%s%a%s" prefix pp value suffix)


  let pp_expression_option formatter (prefix, option) =
    pp_option ~prefix formatter option Expression.pp


  let pp_async formatter = function
    | true -> Format.fprintf formatter "async@;"
    | false -> ()


  let rec pp_statement_t formatter { Node.value = statement; _ } =
    Format.fprintf formatter "%a" pp_statement statement


  and pp_statement_list formatter = function
    | [] -> ()
    | [statement] -> Format.fprintf formatter "%a" pp_statement_t statement
    | statement :: statement_list ->
        Format.fprintf formatter "%a@;%a" pp_statement_t statement pp_statement_list statement_list


  and pp_assign formatter { Assign.target; annotation; value; parent } =
    Format.fprintf
      formatter
      "%a%a%a = %a"
      pp_reference_option
      parent
      Expression.pp
      target
      pp_expression_option
      (": ", annotation)
      Expression.pp
      value


  and pp_class formatter { Class.name; base_arguments; body; decorators; _ } =
    Format.fprintf
      formatter
      "%a@[<v 2>class %a(%a):@;@[<v>%a@]@;@]"
      pp_decorators
      decorators
      Reference.pp
      (Node.value name)
      Expression.pp_expression_argument_list
      base_arguments
      pp_statement_list
      body


  and pp_define
      formatter
      {
        Define.signature = { name; parameters; decorators; return_annotation; async; parent; _ };
        body;
        captures = _;
        unbound_names = _;
      }
    =
    let return_annotation =
      match return_annotation with
      | Some annotation -> Format.asprintf " -> %a" Expression.pp annotation
      | _ -> ""
    in
    Format.fprintf
      formatter
      "%a@[<v 2>%adef %a%s%a(%a)%s:@;%a@]@;"
      pp_decorators
      decorators
      pp_async
      async
      pp_reference_option
      parent
      (if Option.is_some parent then "#" else "")
      Reference.pp
      (Node.value name)
      Expression.pp_expression_parameter_list
      parameters
      return_annotation
      pp_statement_list
      body


  and pp_statement formatter statement =
    match statement with
    | Assign assign -> Format.fprintf formatter "%a" pp_assign assign
    | Assert { Assert.test; Assert.message; _ } ->
        Format.fprintf
          formatter
          "assert %a, %a"
          Expression.pp
          test
          pp_expression_option
          ("", message)
    | Break -> Format.fprintf formatter "break"
    | Class definition -> Format.fprintf formatter "%a" pp_class definition
    | Continue -> Format.fprintf formatter "continue"
    | Define define -> Format.fprintf formatter "%a" pp_define define
    | Delete expression -> Format.fprintf formatter "del %a" Expression.pp expression
    | Expression expression -> Expression.pp formatter expression
    | For { For.target; iterator; body; orelse; async } ->
        Format.fprintf
          formatter
          "@[<v 2>%afor %a in %a:@;%a@]%a"
          pp_async
          async
          Expression.pp
          target
          Expression.pp
          iterator
          pp_statement_list
          body
          pp_statement_list
          orelse
    | Global globals -> Format.fprintf formatter "global %s" (String.concat globals ~sep:", ")
    | If { If.test; body; orelse } ->
        if List.is_empty orelse then
          Format.fprintf
            formatter
            "@[<v>@[<v 2>if %a:@;%a@]@]@;"
            Expression.pp
            test
            pp_statement_list
            body
        else
          Format.fprintf
            formatter
            "@[<v>@[<v 2>if %a:@;%a@]@]@;@[<v 2>else:@;%a@]"
            Expression.pp
            test
            pp_statement_list
            body
            pp_statement_list
            orelse
    | Import { Import.from; imports } -> (
        let pp_import formatter { Import.name; alias } =
          match alias with
          | None -> Format.fprintf formatter "%a" Reference.pp (Node.value name)
          | Some { Node.value = alias; _ } ->
              Format.fprintf formatter "%a as %a" Reference.pp (Node.value name) Identifier.pp alias
        in
        let pp_imports formatter import_list = pp_list formatter pp_import ", " import_list in
        match from with
        | None -> Format.fprintf formatter "@[<v>import %a@]" pp_imports imports
        | Some { Node.value = from; _ } ->
            Format.fprintf formatter "@[<v>from %a import %a@]" Reference.pp from pp_imports imports
        )
    | Nonlocal nonlocal_list -> pp_list formatter String.pp "," nonlocal_list
    | Pass -> Format.fprintf formatter "%s" "pass"
    | Raise { Raise.expression; _ } ->
        Format.fprintf formatter "raise %a" pp_expression_option ("", expression)
    | Return { Return.expression; _ } ->
        Format.fprintf formatter "return %a" pp_expression_option ("", expression)
    | Try { Try.body; handlers; orelse; finally } ->
        let pp_try_block formatter body =
          Format.fprintf formatter "@[<v 2>try:@;%a@]" pp_statement_list body
        in
        let pp_except_block formatter handlers =
          let pp_as formatter name = pp_option ~prefix:" as " formatter name String.pp in
          let pp_handler formatter { Try.Handler.kind; name; body } =
            Format.fprintf
              formatter
              "@[<v 2>except%a%a:@;%a@]"
              pp_expression_option
              (" ", kind)
              pp_as
              name
              pp_statement_list
              body
          in
          let pp_handler_list formatter handler_list =
            pp_list formatter pp_handler "@;" handler_list
          in
          Format.fprintf formatter "%a" pp_handler_list handlers
        in
        let pp_else_block formatter = function
          | [] -> ()
          | orelse -> Format.fprintf formatter "@[<v 2>else:@;%a@]" pp_statement_list orelse
        in
        let pp_finally_block formatter = function
          | [] -> ()
          | finally ->
              Format.fprintf formatter "@[<v 2>finally:@;@[<v>%a@]@]" pp_statement_list finally
        in
        Format.fprintf
          formatter
          "@[<v>%a@;%a@;%a@;%a@]"
          pp_try_block
          body
          pp_except_block
          handlers
          pp_else_block
          orelse
          pp_finally_block
          finally
    | With { With.items; body; async } ->
        let pp_item formatter (expression, expression_option) =
          Format.fprintf
            formatter
            "%a%a"
            Expression.pp
            expression
            pp_expression_option
            (" as ", expression_option)
        in
        let rec pp_item_list formatter = function
          | [] -> ()
          | [item] -> Format.fprintf formatter "%a" pp_item item
          | item :: item_list ->
              Format.fprintf formatter "%a,%a" pp_item item pp_item_list item_list
        in
        Format.fprintf
          formatter
          "@[<v 2>%a with %a:@;%a@]"
          pp_async
          async
          pp_item_list
          items
          pp_statement_list
          body
    | While { While.test; body; orelse } ->
        Format.fprintf
          formatter
          "@[<v 2>while %a:@;%a@]@[<v>%a@]"
          Expression.pp
          test
          pp_statement_list
          body
          pp_statement_list
          orelse


  let pp = pp_statement_t
end

let pp = PrettyPrinter.pp

let show statement = Format.asprintf "%a" pp statement

let pp_statement = PrettyPrinter.pp_statement

let show_statement statement = Format.asprintf "%a" pp_statement statement

let is_generator statements =
  let open Expression in
  let rec is_expression_generator { Node.value; _ } =
    match value with
    | Expression.Yield _
    | Expression.YieldFrom _ ->
        true
    | Await await -> is_expression_generator await
    | BooleanOperator { BooleanOperator.left; right; _ }
    | ComparisonOperator { ComparisonOperator.left; right; _ } ->
        is_expression_generator left || is_expression_generator right
    | Call { Call.callee; arguments } ->
        is_expression_generator callee
        || List.exists arguments ~f:(fun { Call.Argument.value; _ } ->
               is_expression_generator value)
    | Dictionary { Dictionary.entries; keywords } ->
        List.exists entries ~f:(fun { Dictionary.Entry.key; value } ->
            is_expression_generator key || is_expression_generator value)
        || List.exists keywords ~f:is_expression_generator
    | DictionaryComprehension comprehension ->
        is_comprehension_generator
          ~is_element_generator:(fun { Dictionary.Entry.key; value } ->
            is_expression_generator key || is_expression_generator value)
          comprehension
    | Generator comprehension
    | ListComprehension comprehension
    | SetComprehension comprehension ->
        is_comprehension_generator ~is_element_generator:is_expression_generator comprehension
    | List expressions
    | Set expressions
    | Tuple expressions ->
        List.exists expressions ~f:is_expression_generator
    | Starred Starred.(Once expression | Twice expression) -> is_expression_generator expression
    | String { StringLiteral.kind = StringLiteral.Format expressions; _ } ->
        List.exists expressions ~f:is_expression_generator
    | Ternary { Ternary.target; test; alternative } ->
        is_expression_generator target
        || is_expression_generator test
        || is_expression_generator alternative
    | UnaryOperator { UnaryOperator.operand; _ } -> is_expression_generator operand
    | WalrusOperator { WalrusOperator.value; _ } -> is_expression_generator value
    | Complex _
    | Ellipsis
    | False
    | Float _
    | Integer _
    | Lambda _
    | Name _
    | String _
    | True ->
        false
  and is_comprehension_generator
        : 'a. is_element_generator:('a -> bool) -> 'a Comprehension.t -> bool
    =
   fun ~is_element_generator { Comprehension.element; generators } ->
    is_element_generator element
    || List.exists generators ~f:(fun { Comprehension.Generator.iterator; conditions; _ } ->
           is_expression_generator iterator || List.exists conditions ~f:is_expression_generator)
  in
  let is_optional_expression_generator =
    Option.value_map ~f:is_expression_generator ~default:false
  in
  let rec is_statement_generator { Node.value; _ } =
    match value with
    | Assign { Assign.value; _ } -> is_expression_generator value
    | Assert { Assert.test; message; _ } ->
        is_expression_generator test || is_optional_expression_generator message
    | Delete expression
    | Expression expression ->
        is_expression_generator expression
    | Raise { Raise.expression; from } ->
        is_optional_expression_generator expression || is_optional_expression_generator from
    | Return { Return.expression; _ } -> is_optional_expression_generator expression
    | For { For.iterator; body; orelse; _ } ->
        is_expression_generator iterator
        || is_statements_generator body
        || is_statements_generator orelse
    | If { If.test; body; orelse }
    | While { While.test; body; orelse } ->
        is_expression_generator test
        || is_statements_generator body
        || is_statements_generator orelse
    | Try { Try.body; handlers; orelse; finally } ->
        is_statements_generator body
        || List.exists handlers ~f:(fun { Try.Handler.body; _ } -> is_statements_generator body)
        || is_statements_generator orelse
        || is_statements_generator finally
    | With { With.items; body; _ } ->
        List.exists items ~f:(fun (expression, _) -> is_expression_generator expression)
        || is_statements_generator body
    | Break
    | Continue
    | Class _
    | Define _
    | Global _
    | Import _
    | Nonlocal _
    | Pass ->
        false
  and is_statements_generator statements = List.exists statements ~f:is_statement_generator in
  is_statements_generator statements
