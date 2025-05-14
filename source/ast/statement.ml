(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

open Core
open Pyre

let toplevel_define_name = "$toplevel"

let class_toplevel_define_name = "$class_toplevel"

let name_location
    ~offset_columns
    ~body_location:{ Location.start = { Location.line = start_line; column = start_column }; _ }
    name_string
  =
  let start_column = start_column + offset_columns in
  let stop_column_ =
    let name_length = name_string |> String.length in
    start_column + name_length
  in
  Location.
    {
      start = { line = start_line; column = start_column };
      stop = { line = start_line; column = stop_column_ };
    }


module TypeAlias = struct
  type t = {
    name: Expression.t;
    type_params: Expression.TypeParam.t list;
    value: Expression.t;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    match Expression.location_insensitive_compare left.name right.name with
    | x when not (Int.equal x 0) -> x
    | _ -> (
        match
          List.compare
            Expression.TypeParam.location_insensitive_compare
            left.type_params
            right.type_params
        with
        | x when not (Int.equal x 0) -> x
        | _ -> Expression.location_insensitive_compare left.value right.value)
end

module Assign = struct
  type t = {
    target: Expression.t;
    annotation: Expression.t option;
    value: Expression.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    match Expression.location_insensitive_compare left.target right.target with
    | x when not (Int.equal x 0) -> x
    | _ -> (
        match
          Option.compare Expression.location_insensitive_compare left.annotation right.annotation
        with
        | x when not (Int.equal x 0) -> x
        | _ -> Option.compare Expression.location_insensitive_compare left.value right.value)
end

module AugmentedAssign = struct
  type t = {
    target: Expression.t;
    operator: Expression.BinaryOperator.operator;
    value: Expression.t;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    match Expression.location_insensitive_compare left.target right.target with
    | x when not (Int.equal x 0) -> x
    | _ -> (
        match [%compare: Expression.BinaryOperator.operator] left.operator right.operator with
        | x when not (Int.equal x 0) -> x
        | _ -> Expression.location_insensitive_compare left.value right.value)


  let dunder_method_name operator =
    let open Expression.BinaryOperator in
    match operator with
    | Add -> "__iadd__"
    | Sub -> "__isub__"
    | Mult -> "__imul__"
    | MatMult -> "__imatmul__"
    | Div -> "__itruediv__"
    | Mod -> "__imod__"
    | Pow -> "__ipow__"
    | LShift -> "__ilshift__"
    | RShift -> "__irshift__"
    | BitOr -> "__ior__"
    | BitXor -> "__ixor__"
    | BitAnd -> "__iand__"
    | FloorDiv -> "__ifloordiv__"


  let lower_to_call ~location ~callee_location { target; operator; value } =
    let open Expression in
    let arguments = [{ Call.Argument.name = None; value }] in
    let origin = Some (Origin.create ~location Origin.AugmentedAssign) in
    {
      Call.callee =
        {
          Node.location = callee_location;
          value =
            Expression.Name
              (Name.Attribute
                 { Name.Attribute.base = target; attribute = dunder_method_name operator; origin });
        };
      arguments;
      origin;
    }


  let lower_to_expression ~location ~callee_location assign =
    let open Expression in
    Expression.Call (lower_to_call ~location ~callee_location assign) |> Node.create ~location
end

module Import = struct
  type import = {
    name: Reference.t;
    alias: Identifier.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  type t = {
    from: Reference.t Node.t option;
    imports: import Node.t list;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    let location_insensitive_compare_import left right =
      match Option.compare [%compare: Identifier.t] left.alias right.alias with
      | x when not (Int.equal x 0) -> x
      | _ -> [%compare: Reference.t] left.name right.name
    in
    let location_insensitive_compare_from left right =
      [%compare: Reference.t] (Node.value left) (Node.value right)
    in
    match Option.compare location_insensitive_compare_from left.from right.from with
    | x when not (Int.equal x 0) -> x
    | _ ->
        List.compare
          (Node.location_insensitive_compare location_insensitive_compare_import)
          left.imports
          right.imports
end

module Raise = struct
  type t = {
    expression: Expression.t option;
    from: Expression.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

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
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    match Bool.compare left.is_implicit right.is_implicit with
    | x when not (Int.equal x 0) -> x
    | _ -> Option.compare Expression.location_insensitive_compare left.expression right.expression
end

module Decorator = struct
  type t = {
    name: Reference.t Node.t;
    arguments: Expression.Call.Argument.t list option;
    (* We preserve the original expression, since it might have metadata (cf `origin` attribute) *)
    original_expression: Expression.t;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    match Reference.compare left.name.Node.value right.name.Node.value with
    | x when not (Int.equal x 0) -> x
    | _ ->
        (Option.compare (List.compare Expression.Call.Argument.location_insensitive_compare))
          left.arguments
          right.arguments


  let to_expression { original_expression; _ } = original_expression

  let from_expression ({ Node.value; location } as original_expression) =
    let open Expression in
    match value with
    | Expression.Name name -> (
        match name_to_reference name with
        | Some reference ->
            Some { name = Node.create ~location reference; arguments = None; original_expression }
        | None -> None)
    | Expression.Call { Call.callee = { Node.value; location }; arguments; origin = _ } -> (
        match value with
        | Expression.Name name -> (
            match name_to_reference name with
            | Some reference ->
                Some
                  {
                    name = Node.create ~location reference;
                    arguments = Some arguments;
                    original_expression;
                  }
            | None -> None)
        | _ -> None)
    | _ -> None


  (* In some cases, we need to create an "artificial" decorator that doesn't exist in the original
     AST. This is a helper to create the original expression. *)
  let create_original_expression
      ~create_origin_for_reference
      ~call_origin
      ~name:{ Node.location; value = name }
      ~arguments
    =
    let name =
      Expression.from_reference ~location ~create_origin:create_origin_for_reference name
    in
    match arguments with
    | Some arguments ->
        Node.create
          ~location
          (Expression.Expression.Call
             { Expression.Call.callee = name; arguments; origin = call_origin })
    | None -> name
end

module rec Assert : sig
  module Origin : sig
    type t =
      | If of { true_branch: bool }
      | While of { true_branch: bool }
      | Match of { true_branch: bool }
      | TryHandler
    [@@deriving equal, compare, sexp, show, hash, to_yojson]
  end

  type t = {
    test: Expression.t;
    message: Expression.t option;
    origin: Origin.t Node.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end = struct
  module Origin = struct
    type t =
      | If of { true_branch: bool }
      | While of { true_branch: bool }
      | Match of { true_branch: bool }
      | TryHandler
    [@@deriving equal, compare, sexp, show, hash, to_yojson]
  end

  type t = {
    test: Expression.t;
    message: Expression.t option;
    origin: Origin.t Node.t option;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    (* We ignore locations, but also the origin. *)
    match Expression.location_insensitive_compare left.test right.test with
    | x when not (Int.equal x 0) -> x
    | _ -> Option.compare Expression.location_insensitive_compare left.message right.message
end

and Class : sig
  type t = {
    name: Reference.t;
    base_arguments: Expression.Call.Argument.t list;
    parent: NestingContext.t;
    body: Statement.t list;
    decorators: Expression.t list;
    top_level_unbound_names: Define.NameAccess.t list;
    type_params: Expression.TypeParam.t list;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int

  val toplevel_define : qualifier:Reference.t -> t -> Define.t

  val constructors : ?in_test:bool -> t -> Define.t list

  val defines : t -> Define.t list

  val find_define : t -> method_name:Identifier.t -> Define.t Node.t option

  val is_frozen : t -> bool option

  val base_classes : t -> Expression.t list

  val metaclass : t -> Expression.t option

  val init_subclass_arguments : t -> Expression.Call.Argument.t list

  val name_location : body_location:Location.t -> t -> Location.t

  type class_t = t [@@deriving equal, compare, sexp, show, hash, to_yojson]
end = struct
  type t = {
    name: Reference.t;
    base_arguments: Expression.Call.Argument.t list;
    parent: NestingContext.t;
    body: Statement.t list;
    decorators: Expression.t list;
    top_level_unbound_names: Define.NameAccess.t list;
    type_params: Expression.TypeParam.t list;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  type class_t = t [@@deriving equal, compare, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    match Reference.compare left.name right.name with
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
            match NestingContext.compare left.parent right.parent with
            | x when not (Int.equal x 0) -> x
            | _ -> (
                match List.compare Statement.location_insensitive_compare left.body right.body with
                | x when not (Int.equal x 0) -> x
                | _ -> (
                    match
                      List.compare
                        Expression.location_insensitive_compare
                        left.decorators
                        right.decorators
                    with
                    | x when not (Int.equal x 0) -> x
                    | _ -> (
                        match
                          List.compare
                            Define.NameAccess.compare
                            left.top_level_unbound_names
                            right.top_level_unbound_names
                        with
                        | 0 ->
                            List.compare
                              Expression.TypeParam.location_insensitive_compare
                              left.type_params
                              right.type_params
                        | x -> x)))))


  let toplevel_define ~qualifier { name; top_level_unbound_names; parent; body; _ } =
    Define.create_class_toplevel
      ~unbound_names:top_level_unbound_names
      ~module_name:qualifier
      ~local_context:(NestingContext.create_class ~parent (Reference.last name))
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


  (* Check if a class is a dataclass or not, and if so, check if it frozen or not. TODO T178998636:
     Break this function down further by isolating dataclass options in its own module and using it
     to distinguish between various cases of dataclass/dataclass transforms *)
  let is_frozen { decorators; _ } =
    let open Expression in
    let is_frozen_dataclass decorator =
      match Decorator.from_expression decorator with
      | Some
          {
            Decorator.name = { Node.value = name; _ };
            arguments = Some arguments;
            original_expression = _;
          }
        when Reference.equal name (Reference.create "dataclasses.dataclass") ->
          let has_frozen_argument Call.Argument.{ name; value } =
            match name, value with
            | Some { Node.value; _ }, { Node.value = Expression.Constant Constant.True; _ } ->
                String.equal "frozen" (Identifier.sanitized value)
            | _, _ -> false
          in
          Some (List.exists arguments ~f:has_frozen_argument)
      | _ -> None
    in
    List.find_map decorators ~f:is_frozen_dataclass


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


  let name_location ~body_location { name; _ } =
    let class_and_space_offset = 6 in
    name |> Reference.last |> name_location ~offset_columns:class_and_space_offset ~body_location
end

and Define : sig
  module Signature : sig
    type t = {
      name: Reference.t;
      parameters: Expression.Parameter.t list;
      decorators: Expression.t list;
      return_annotation: Expression.t option;
      async: bool;
      generator: bool;
      parent: NestingContext.t;
      (* The qualified name of the class owning the method. *)
      (* TODO: This is redundant now that we have the `parent` field. It should be replaced by
         `parent` in all cases. *)
      legacy_parent: Reference.t option;
      type_params: Expression.TypeParam.t list;
    }
    [@@deriving equal, compare, sexp, show, hash, to_yojson]

    val location_insensitive_compare : t -> t -> int

    val create_toplevel : Reference.t -> t

    val unqualified_name : t -> Identifier.t

    val self_identifier : t -> Identifier.t

    val is_method : t -> bool

    val is_coroutine : t -> bool

    val is_abstract_method : t -> bool

    val is_overloaded_function : t -> bool

    val is_static_method : t -> bool

    val is_final_method : t -> bool

    val is_override_method : t -> bool

    val is_class_method : t -> bool

    val is_class_property : t -> bool

    val is_enum_member : t -> bool

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
      [@@deriving equal, compare, sexp, show, hash, to_yojson]
    end

    type t = {
      name: Identifier.t;
      kind: Kind.t;
    }
    [@@deriving equal, compare, sexp, show, hash, to_yojson]
  end

  module NameAccess : sig
    type t = {
      name: Identifier.t;
      location: Location.t;
    }
    [@@deriving equal, compare, sexp, show, hash, to_yojson]
  end

  type t = {
    signature: Signature.t;
    captures: Capture.t list;
    unbound_names: NameAccess.t list;
    body: Statement.t list;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int

  val create_toplevel
    :  module_name:Reference.t ->
    unbound_names:NameAccess.t list ->
    statements:Statement.t list ->
    t

  val create_class_toplevel
    :  unbound_names:NameAccess.t list ->
    module_name:Reference.t ->
    local_context:NestingContext.t ->
    statements:Statement.t list ->
    t

  val name : t -> Reference.t

  val unqualified_name : t -> Identifier.t

  val name_location : body_location:Location.t -> t -> Location.t

  val self_identifier : t -> Identifier.t

  val is_method : t -> bool

  val is_coroutine : t -> bool

  val is_abstract_method : t -> bool

  val is_overloaded_function : t -> bool

  val is_static_method : t -> bool

  val is_final_method : t -> bool

  val is_override_method : t -> bool

  val is_class_method : t -> bool

  val is_class_property : t -> bool

  val is_enum_member : t -> bool

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

  val dump_higher_order_call_graph : t -> bool

  val dump_perf_higher_order_call_graph : t -> bool

  val dump_perf : t -> bool

  val show_json : t -> string

  val has_decorator : ?match_prefix:bool -> t -> string -> bool

  val has_return_annotation : t -> bool
end = struct
  module Signature = struct
    type t = {
      name: Reference.t;
      parameters: Expression.Parameter.t list;
      decorators: Expression.t list;
      return_annotation: Expression.t option;
      async: bool;
      generator: bool;
      parent: NestingContext.t;
      (* The class owning the method *)
      legacy_parent: Reference.t option;
      type_params: Expression.TypeParam.t list;
    }
    [@@deriving equal, compare, sexp, show, hash, to_yojson]

    let location_insensitive_compare left right =
      match Reference.compare left.name right.name with
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
                List.compare
                  Expression.location_insensitive_compare
                  left.decorators
                  right.decorators
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
                              match NestingContext.compare left.parent right.parent with
                              | x when not (Int.equal x 0) -> x
                              | _ -> (
                                  match
                                    [%compare: Reference.t option]
                                      left.legacy_parent
                                      right.legacy_parent
                                  with
                                  | x when not (Int.equal x 0) -> x
                                  | _ ->
                                      List.compare
                                        Expression.TypeParam.location_insensitive_compare
                                        left.type_params
                                        right.type_params)))))))


    let create_toplevel module_name =
      {
        name = Reference.create ~prefix:module_name toplevel_define_name;
        parameters = [];
        decorators = [];
        return_annotation = None;
        async = false;
        generator = false;
        parent = NestingContext.create_toplevel ();
        legacy_parent = None;
        type_params = [];
      }


    let create_class_toplevel ~module_name local_context =
      let qualified_class_name = NestingContext.to_qualifier ~module_name local_context in
      {
        name = Reference.create ~prefix:qualified_class_name class_toplevel_define_name;
        parameters = [];
        decorators = [];
        return_annotation = None;
        async = false;
        generator = false;
        parent = local_context;
        legacy_parent = Some qualified_class_name;
        type_params = [];
      }


    let unqualified_name { name; _ } = Reference.last name

    let self_identifier { parameters; _ } =
      match parameters with
      | { Node.value = { Expression.Parameter.name; _ }; _ } :: _ -> name
      | _ -> "self"


    let is_method { legacy_parent; _ } = Option.is_some legacy_parent

    let has_decorator ?(match_prefix = false) { decorators; _ } decorator =
      Expression.exists_in_list ~match_prefix ~expression_list:decorators decorator


    let has_return_annotation { return_annotation; _ } = Option.is_some return_annotation

    let is_coroutine signature = has_decorator signature "asyncio.coroutines.coroutine"

    let is_abstract_method signature =
      has_decorator signature "abstractmethod"
      || has_decorator signature "abc.abstractmethod"
      || has_decorator signature "abstractproperty"
      || has_decorator signature "abc.abstractproperty"


    let is_overloaded_function signature =
      has_decorator signature "overload"
      || has_decorator signature "typing.overload"
      || has_decorator signature "typing_extensions.overload"
      (* TODO(T142322930) Pytest aliases typing.overload, and currently Pyre's alias resolution
         logic runs after the collection of overloads so we have to hardcode support. If we could
         move alias resolution out of UnannotedGlobalEnvironment by adding a new environment layer,
         we could remove this hack. *)
      || has_decorator signature "_pytest.compat.overload"


    let is_static_method signature =
      (* `__new__` is always a static method. See
         `https://docs.python.org/3/reference/datamodel.html#object.__new__`. *)
      String.equal (unqualified_name signature) "__new__" || has_decorator signature "staticmethod"


    let is_final_method signature = has_decorator signature "typing.final"

    let is_override_method signature =
      has_decorator signature "typing.override"
      || has_decorator signature "typing_extensions.override"
      || has_decorator signature "pyre_extensions.override"


    let is_dunder_method signature =
      let name = unqualified_name signature in
      String.is_prefix ~prefix:"__" name && String.is_suffix ~suffix:"__" name


    let is_class_method signature =
      let valid_names = ["__init_subclass__"; "__new__"; "__class_getitem__"] in
      is_method signature
      && (Set.exists Recognized.classmethod_decorators ~f:(has_decorator signature)
         || List.mem valid_names (unqualified_name signature) ~equal:String.equal)


    let is_class_property signature =
      is_method signature
      && Set.exists Recognized.classproperty_decorators ~f:(has_decorator signature)


    let is_enum_member signature = is_method signature && has_decorator signature "enum.member"

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


    let is_test_setup signature =
      is_method signature
      &&
      let name = unqualified_name signature in
      Set.mem test_initializers name


    let is_constructor ?(in_test = false) signature =
      is_method signature
      &&
      let name = unqualified_name signature in
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

    let is_toplevel signature = String.equal (unqualified_name signature) toplevel_define_name

    let is_class_toplevel signature =
      String.equal (unqualified_name signature) class_toplevel_define_name
  end

  module Capture = struct
    module Kind = struct
      type t =
        | Annotation of Expression.t option
        | Self of Reference.t
        | ClassSelf of Reference.t
        | DefineSignature of Define.Signature.t
      [@@deriving equal, compare, sexp, show, hash, to_yojson]

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
    [@@deriving equal, compare, sexp, show, hash, to_yojson]

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
    [@@deriving equal, compare, sexp, show, hash, to_yojson]

    let location_insensitive_compare left right = [%compare: Identifier.t] left.name right.name
  end

  type t = {
    signature: Signature.t;
    captures: Capture.t list;
    unbound_names: NameAccess.t list;
    body: Statement.t list;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

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


  let create_toplevel ~module_name ~unbound_names ~statements =
    {
      signature = Signature.create_toplevel module_name;
      captures = [];
      unbound_names;
      body = statements;
    }


  let create_class_toplevel ~unbound_names ~module_name ~local_context ~statements =
    {
      signature = Signature.create_class_toplevel ~module_name local_context;
      captures = [];
      unbound_names;
      body = statements;
    }


  let name { signature = { Signature.name; _ }; _ } = name

  let unqualified_name { signature; _ } = Signature.unqualified_name signature

  let name_location ~body_location define =
    if Signature.is_class_toplevel define.signature then
      (* This causes lookup.ml to skip class toplevel defines, which is what we want because they
         are handled by reading class bodies. *)
      Location.any
    else
      let def_and_space_offset =
        match define with
        | { signature = { Signature.async = true; _ }; _ } -> 10
        | _ -> 4
      in
      define
      |> unqualified_name
      |> name_location ~offset_columns:def_and_space_offset ~body_location


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

  let is_override_method { signature; _ } = Signature.is_override_method signature

  let is_dunder_method { signature; _ } = Signature.is_dunder_method signature

  let is_class_method { signature; _ } = Signature.is_class_method signature

  let is_class_property { signature; _ } = Signature.is_class_property signature

  let is_enum_member { signature; _ } = Signature.is_enum_member signature

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
                    {
                      Call.callee = { Node.value = Expression.Name (Name.Identifier identifier); _ };
                      _;
                    };
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
    | {
        Node.value = Statement.Expression { Node.value = Expression.Constant Constant.Ellipsis; _ };
        _;
      }
      :: _
    | _
      :: {
           Node.value =
             Statement.Expression { Node.value = Expression.Constant Constant.Ellipsis; _ };
           _;
         }
      :: _ ->
        true
    | _ -> false


  let dump define = contains_call define "pyre_dump"

  let dump_cfg define = contains_call define "pyre_dump_cfg"

  let dump_locations define = contains_call define "pyre_dump_locations"

  let dump_call_graph define = contains_call define "pyre_dump_call_graph"

  let dump_higher_order_call_graph define = contains_call define "pyre_dump_higher_order_call_graph"

  let dump_perf_higher_order_call_graph define =
    contains_call define "pyre_dump_perf_higher_order_call_graph"


  let dump_perf define = contains_call define "pyre_dump_perf"

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
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val synthetic_preamble : t -> Statement.t

  val location_insensitive_compare : t -> t -> int
end = struct
  type t = {
    target: Expression.t;
    iterator: Expression.t;
    body: Statement.t list;
    orelse: Statement.t list;
    async: bool;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  (* Create an assignment statement mimicing the `iterator` to `target` flow. Used when Pyre creates
     the CFG. *)
  let synthetic_preamble
      {
        target = { Node.location = target_location; _ } as target;
        iterator = { Node.location = iterator_location; _ } as iterator;
        async;
        _;
      }
    =
    let location =
      Location.{ start = Location.start target_location; stop = Location.stop iterator_location }
    in
    let open Expression in
    let value =
      let value =
        let create_call base iterator next =
          let iter_origin = Some (Origin.create ~location:iterator_location Origin.ForIter) in
          let next_origin = Some (Origin.create ~location:iterator_location Origin.ForNext) in
          Expression.Call
            {
              Call.callee =
                {
                  Node.location;
                  value =
                    Expression.Name
                      (Name.Attribute
                         {
                           Name.Attribute.base =
                             {
                               Node.location;
                               value =
                                 Expression.Call
                                   {
                                     Call.callee =
                                       {
                                         Node.location;
                                         value =
                                           Expression.Name
                                             (Name.Attribute
                                                {
                                                  Name.Attribute.base;
                                                  attribute = iterator;
                                                  origin = iter_origin;
                                                });
                                       };
                                     arguments = [];
                                     origin = iter_origin;
                                   };
                             };
                           attribute = next;
                           origin = next_origin;
                         });
                };
              arguments = [];
              origin = next_origin;
            }
        in
        if async then
          create_call iterator "__aiter__" "__anext__"
        else
          create_call iterator "__iter__" "__next__"
      in
      if async then
        {
          Node.location;
          Node.value =
            Expression.Await
              {
                Await.operand = Node.create ~location value;
                origin = Some (Origin.create ~location:iterator_location Origin.ForAwait);
              };
        }
      else
        { Node.location; value }
    in
    {
      Node.location;
      Node.value = Statement.Assign { Assign.target; annotation = None; value = Some value };
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
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end = struct
  type t = {
    test: Expression.t;
    body: Statement.t list;
    orelse: Statement.t list;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    match Expression.location_insensitive_compare left.test right.test with
    | x when not (Int.equal x 0) -> x
    | _ -> (
        match List.compare Statement.location_insensitive_compare left.body right.body with
        | x when not (Int.equal x 0) -> x
        | _ -> List.compare Statement.location_insensitive_compare left.orelse right.orelse)
end

and Match : sig
  module Pattern : sig
    type pattern =
      | MatchAs of {
          pattern: t option;
          name: Identifier.t;
        }
      | MatchClass of {
          class_name: Expression.Name.t Node.t;
          patterns: t list;
          keyword_attributes: Identifier.t list;
          keyword_patterns: t list;
        }
      | MatchMapping of {
          keys: Expression.t list;
          patterns: t list;
          rest: Identifier.t option;
        }
      | MatchOr of t list
      | MatchSequence of t list
      | MatchSingleton of Expression.Constant.t
      | MatchStar of Identifier.t option
      | MatchValue of Expression.t
      | MatchWildcard
    [@@deriving equal, compare, sexp, show, hash, to_yojson]

    and t = pattern Node.t [@@deriving equal, compare, sexp, show, hash, to_yojson]

    val location_insensitive_compare : t -> t -> int
  end

  module Case : sig
    type t = {
      pattern: Pattern.t;
      guard: Expression.t option;
      body: Statement.t list;
    }
    [@@deriving equal, compare, sexp, show, hash, to_yojson]

    val location_insensitive_compare : t -> t -> int

    val is_refutable : t -> bool
  end

  type t = {
    subject: Expression.t;
    cases: Case.t list;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end = struct
  module Pattern = struct
    type pattern =
      | MatchAs of {
          pattern: t option;
          name: Identifier.t;
        }
      | MatchClass of {
          class_name: Expression.Name.t Node.t;
          patterns: t list;
          keyword_attributes: Identifier.t list;
          keyword_patterns: t list;
        }
      | MatchMapping of {
          keys: Expression.t list;
          patterns: t list;
          rest: Identifier.t option;
        }
      | MatchOr of t list
      | MatchSequence of t list
      | MatchSingleton of Expression.Constant.t
      | MatchStar of Identifier.t option
      | MatchValue of Expression.t
      | MatchWildcard
    [@@deriving equal, compare, sexp, show, hash, to_yojson]

    and t = pattern Node.t [@@deriving equal, compare, sexp, show, hash, to_yojson]

    let rec location_insensitive_equal_pattern left right =
      let location_insensitive_equal_expression left right =
        Int.equal (Expression.location_insensitive_compare left right) 0
      in
      match left, right with
      | MatchAs left, MatchAs right ->
          Option.equal location_insensitive_equal left.pattern right.pattern
          && Identifier.equal left.name right.name
      | MatchClass left, MatchClass right ->
          [%compare.equal: Expression.Name.t]
            (Node.value left.class_name)
            (Node.value right.class_name)
          && List.equal location_insensitive_equal left.patterns right.patterns
          && List.equal Identifier.equal left.keyword_attributes right.keyword_attributes
          && List.equal location_insensitive_equal left.keyword_patterns right.keyword_patterns
      | MatchMapping left, MatchMapping right ->
          List.equal location_insensitive_equal_expression left.keys right.keys
          && List.equal location_insensitive_equal left.patterns right.patterns
          && Option.equal Identifier.equal left.rest right.rest
      | MatchOr left, MatchOr right
      | MatchSequence left, MatchSequence right ->
          List.equal location_insensitive_equal left right
      | MatchSingleton left, MatchSingleton right ->
          Int.equal (Expression.Constant.location_insensitive_compare left right) 0
      | MatchStar left, MatchStar right -> Option.equal Identifier.equal left right
      | MatchValue left, MatchValue right -> location_insensitive_equal_expression left right
      | MatchWildcard, MatchWildcard -> true
      | _, _ -> false


    and location_insensitive_equal left right =
      Node.location_insensitive_equal location_insensitive_equal_pattern left right


    (* TODO(T104733576): This doesn't give a total order, but we use it for equality checks so it is
       OK. Should be removed as part of removing broader usage of location_insensitive_compare. *)
    let location_insensitive_compare left right =
      match location_insensitive_equal left right with
      | true -> 0
      | false -> -1
  end

  module Case = struct
    type t = {
      pattern: Pattern.t;
      guard: Expression.t option;
      body: Statement.t list;
    }
    [@@deriving equal, compare, sexp, show, hash, to_yojson]

    let location_insensitive_compare left right =
      match Pattern.location_insensitive_compare left.pattern right.pattern with
      | x when not (Int.equal x 0) -> x
      | _ -> (
          match Option.compare Expression.location_insensitive_compare left.guard right.guard with
          | x when not (Int.equal x 0) -> x
          | _ -> List.compare Statement.location_insensitive_compare left.body right.body)


    let is_refutable { Match.Case.guard; pattern; _ } =
      let rec is_pattern_irrefutable { Node.value = pattern; _ } =
        match pattern with
        | Match.Pattern.MatchAs { pattern = None; _ }
        | Match.Pattern.MatchWildcard ->
            true
        | Match.Pattern.MatchAs { pattern = Some pattern; _ } -> is_pattern_irrefutable pattern
        | Match.Pattern.MatchOr patterns -> List.exists ~f:is_pattern_irrefutable patterns
        | _ -> false
      in
      Option.is_some guard || not (is_pattern_irrefutable pattern)
  end

  type t = {
    subject: Expression.t;
    cases: Case.t list;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  let location_insensitive_compare left right =
    match Expression.location_insensitive_compare left.subject right.subject with
    | x when not (Int.equal x 0) -> x
    | _ -> List.compare Case.location_insensitive_compare left.cases right.cases
end

and Try : sig
  module Handler : sig
    type t = {
      kind: Expression.t option;
      name: Identifier.t Node.t option;
      body: Statement.t list;
    }
    [@@deriving equal, compare, sexp, show, hash, to_yojson]

    val location_insensitive_compare : t -> t -> int
  end

  type t = {
    body: Statement.t list;
    handlers: Handler.t list;
    orelse: Statement.t list;
    finally: Statement.t list;
    handles_exception_group: bool;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val preamble : handles_exception_group:bool -> Handler.t -> Statement.t list

  val location_insensitive_compare : t -> t -> int
end = struct
  module Handler = struct
    type t = {
      kind: Expression.t option;
      name: Identifier.t Node.t option;
      body: Statement.t list;
    }
    [@@deriving equal, compare, sexp, show, hash, to_yojson]

    let location_insensitive_compare left right =
      match Option.compare Expression.location_insensitive_compare left.kind right.kind with
      | x when not (Int.equal x 0) -> x
      | _ -> (
          let compare_handler left right =
            [%compare: Identifier.t] (Node.value left) (Node.value right)
          in
          match Option.compare compare_handler left.name right.name with
          | x when not (Int.equal x 0) -> x
          | _ -> List.compare Statement.location_insensitive_compare left.body right.body)
  end

  type t = {
    body: Statement.t list;
    handlers: Handler.t list;
    orelse: Statement.t list;
    finally: Statement.t list;
    handles_exception_group: bool;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  let preamble ~handles_exception_group { Handler.kind; name; _ } =
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
                value = Some (Node.create ~location (Expression.Constant Constant.Ellipsis));
              };
        };
        {
          Node.location;
          value =
            Statement.Assert
              {
                Assert.test =
                  {
                    Node.location;
                    value =
                      Expression.Call
                        {
                          Call.callee =
                            {
                              Node.location;
                              value = Expression.Name (Name.Identifier "isinstance");
                            };
                          arguments =
                            [
                              { Call.Argument.name = None; value = target };
                              { Call.Argument.name = None; value = annotation };
                            ];
                          origin = Some (Origin.create ~location Origin.TryHandlerIsInstance);
                        };
                  };
                message = None;
                origin = Some { Node.location; value = Assert.Origin.TryHandler };
              };
        };
      ]
    in
    match kind, name with
    | ( Some ({ Node.location; value = Expression.Name _ | Expression.Tuple _; _ } as annotation),
        Some { Node.location = name_location; value = name } ) ->
        let annotation =
          if handles_exception_group then
            let exception_group_type =
              match annotation with
              | { Node.value = Expression.Tuple _; _ } ->
                  Node.create
                    ~location
                    (subscript
                       ~create_origin_for_base:(fun _ -> None)
                       ~origin:None
                       "typing.Union"
                       [annotation]
                       ~location)
              | _ -> annotation
            in
            Node.create
              ~location
              (subscript
                 ~create_origin_for_base:(fun _ -> None)
                 ~origin:None
                 "ExceptionGroup"
                 [exception_group_type]
                 ~location)
          else
            annotation
        in
        assume
          ~location
          ~target:
            { Node.location = name_location; Node.value = Expression.Name (Name.Identifier name) }
          ~annotation
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
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val location_insensitive_compare : t -> t -> int
end = struct
  type t = {
    test: Expression.t;
    body: Statement.t list;
    orelse: Statement.t list;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

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
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val preamble : t -> Statement.t list

  val location_insensitive_compare : t -> t -> int
end = struct
  type t = {
    items: (Expression.t * Expression.t option) list;
    body: Statement.t list;
    async: bool;
  }
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

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
        let origin = Some (Origin.create ~location Origin.With) in
        let create_call call_name =
          {
            Node.location;
            value =
              Expression.Call
                {
                  Call.callee =
                    {
                      Node.location;
                      value =
                        Expression.Name
                          (Name.Attribute
                             { Name.Attribute.base = expression; attribute = call_name; origin });
                    };
                  arguments = [];
                  origin;
                };
          }
        in
        if async then
          Node.create
            ~location
            (Expression.Await { Await.operand = create_call "__aenter__"; origin })
        else
          create_call "__enter__"
      in
      match target with
      | Some target ->
          let assign = { Assign.target; annotation = None; value = Some enter_call } in
          Node.create ~location (Statement.Assign assign)
      | None -> Node.create ~location (Statement.Expression enter_call)
    in
    List.map items ~f:preamble
end

and Statement : sig
  type statement =
    | Assign of Assign.t
    | Assert of Assert.t
    | AugmentedAssign of AugmentedAssign.t
    | Break
    | Class of Class.t
    | Continue
    | Define of Define.t
    | Delete of Expression.t list
    | Expression of Expression.t
    | For of For.t
    | Global of Identifier.t list
    | If of If.t
    | Import of Import.t
    | Match of Match.t
    | Nonlocal of Identifier.t list
    | Pass
    | Raise of Raise.t
    | Return of Return.t
    | Try of Try.t
    | TypeAlias of TypeAlias.t
    | With of With.t
    | While of While.t
  [@@deriving equal, compare, sexp, hash, to_yojson]

  type t = statement Node.t [@@deriving equal, compare, sexp, show, hash, to_yojson]

  val assume : origin:Assert.Origin.t Node.t option -> Expression.t -> t

  val generator_assignment : Expression.Comprehension.Generator.t -> Assign.t

  val location_insensitive_compare : t -> t -> int
end = struct
  type statement =
    | Assign of Assign.t
    | Assert of Assert.t
    | AugmentedAssign of AugmentedAssign.t
    | Break
    | Class of Class.t
    | Continue
    | Define of Define.t
    | Delete of Expression.t list
    | Expression of Expression.t
    | For of For.t
    | Global of Identifier.t list
    | If of If.t
    | Import of Import.t
    | Match of Match.t
    | Nonlocal of Identifier.t list
    | Pass
    | Raise of Raise.t
    | Return of Return.t
    | Try of Try.t
    | TypeAlias of TypeAlias.t
    | With of With.t
    | While of While.t
  [@@deriving equal, compare, sexp, show, hash, to_yojson]

  type t = statement Node.t [@@deriving equal, compare, sexp, show, hash, to_yojson]

  let rec location_insensitive_compare_statement left right =
    match left, right with
    | Assign left, Assign right -> Assign.location_insensitive_compare left right
    | Assert left, Assert right -> Assert.location_insensitive_compare left right
    | AugmentedAssign left, AugmentedAssign right ->
        AugmentedAssign.location_insensitive_compare left right
    | Break, Break -> 0
    | Class left, Class right -> Class.location_insensitive_compare left right
    | Continue, Continue -> 0
    | Define left, Define right -> Define.location_insensitive_compare left right
    | Delete left, Delete right -> List.compare Expression.location_insensitive_compare left right
    | Expression left, Expression right -> Expression.location_insensitive_compare left right
    | For left, For right -> For.location_insensitive_compare left right
    | Global left, Global right -> List.compare Identifier.compare left right
    | If left, If right -> If.location_insensitive_compare left right
    | Import left, Import right -> Import.location_insensitive_compare left right
    | Match left, Match right -> Match.location_insensitive_compare left right
    | Nonlocal left, Nonlocal right -> List.compare Identifier.compare left right
    | Pass, Pass -> 0
    | Raise left, Raise right -> Raise.location_insensitive_compare left right
    | Return left, Return right -> Return.location_insensitive_compare left right
    | Try left, Try right -> Try.location_insensitive_compare left right
    | TypeAlias left, TypeAlias right -> TypeAlias.location_insensitive_compare left right
    | With left, With right -> With.location_insensitive_compare left right
    | While left, While right -> While.location_insensitive_compare left right
    | Assign _, _ -> -1
    | Assert _, _ -> -1
    | AugmentedAssign _, _ -> -1
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
    | Match _, _ -> -1
    | Nonlocal _, _ -> -1
    | Pass, _ -> -1
    | Raise _, _ -> -1
    | Return _, _ -> -1
    | Try _, _ -> -1
    | TypeAlias _, _ -> -1
    | With _, _ -> -1
    | While _, _ -> -1


  and location_insensitive_compare left right =
    Node.location_insensitive_compare location_insensitive_compare_statement left right


  let assume ~origin ({ Node.location; _ } as test) =
    { Node.location; value = Statement.Assert { Assert.test; message = None; origin } }


  let generator_assignment
      {
        Expression.Comprehension.Generator.target;
        iterator = { Node.location; _ } as iterator;
        async;
        _;
      }
    =
    let open Expression in
    let iter_origin = Some (Origin.create ~location Origin.GeneratorIter) in
    let next_origin = Some (Origin.create ~location Origin.GeneratorNext) in
    let await_origin = Some (Origin.create ~location Origin.GeneratorAwait) in
    let value =
      if async then
        let aiter =
          {
            Node.location;
            value =
              Expression.Call
                {
                  Call.callee =
                    {
                      Node.location;
                      value =
                        Expression.Name
                          (Name.Attribute
                             {
                               Name.Attribute.base = iterator;
                               attribute = "__aiter__";
                               origin = iter_origin;
                             });
                    };
                  arguments = [];
                  origin = iter_origin;
                };
          }
        in
        {
          Node.location;
          value =
            Expression.Call
              {
                Call.callee =
                  {
                    Node.location;
                    value =
                      Expression.Name
                        (Name.Attribute
                           {
                             Name.Attribute.base = aiter;
                             attribute = "__anext__";
                             origin = next_origin;
                           });
                  };
                arguments = [];
                origin = next_origin;
              };
        }
        |> fun target ->
        Node.create ~location (Expression.Await { Await.operand = target; origin = await_origin })
      else
        let iter =
          {
            Node.location;
            value =
              Expression.Call
                {
                  Call.callee =
                    {
                      Node.location;
                      value =
                        Expression.Name
                          (Name.Attribute
                             {
                               Name.Attribute.base = iterator;
                               attribute = "__iter__";
                               origin = iter_origin;
                             });
                    };
                  arguments = [];
                  origin = iter_origin;
                };
          }
        in
        {
          Node.location;
          value =
            Expression.Call
              {
                Call.callee =
                  {
                    Node.location;
                    value =
                      Expression.Name
                        (Name.Attribute
                           {
                             Name.Attribute.base = iter;
                             attribute = "__next__";
                             origin = next_origin;
                           });
                  };
                arguments = [];
                origin = next_origin;
              };
        }
    in
    { Assign.target; annotation = None; value = Some value }
end

include Statement

let _ = show (* shadowed below *)

module PrettyPrinter = struct
  let pp_decorators formatter = function
    | [] -> ()
    | decorators ->
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


  and pp_assign formatter { Assign.target; annotation; value } =
    Format.fprintf
      formatter
      "%a%a = %a"
      Expression.pp
      target
      pp_expression_option
      (": ", annotation)
      pp_expression_option
      ("", value)


  and pp_augmented_assign formatter { AugmentedAssign.target; operator; value } =
    Format.fprintf
      formatter
      "%a %a= %a"
      Expression.pp
      target
      Expression.BinaryOperator.pp_binary_operator
      operator
      Expression.pp
      value


  and pp_class formatter { Class.name; base_arguments; body; decorators; type_params; _ } =
    Format.fprintf
      formatter
      "%a@[<v 2>class %a%a(%a):@;@[<v>%a@]@;@]"
      pp_decorators
      decorators
      Reference.pp
      name
      Expression.pp_type_param_list
      type_params
      Expression.pp_expression_argument_list
      base_arguments
      pp_statement_list
      body


  and pp_define
      formatter
      {
        Define.signature =
          {
            Define.Signature.name;
            parameters;
            decorators;
            return_annotation;
            async;
            legacy_parent;
            type_params;
            _;
          };
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
      "%a@[<v 2>%adef %a%s%a%a(%a)%s:@;%a@]@;"
      pp_decorators
      decorators
      pp_async
      async
      pp_reference_option
      legacy_parent
      (if Option.is_some legacy_parent then "#" else "")
      Reference.pp
      name
      Expression.pp_type_param_list
      type_params
      Expression.pp_expression_parameter_list
      parameters
      return_annotation
      pp_statement_list
      body


  and pp_statement formatter statement =
    match statement with
    | Assign assign -> Format.fprintf formatter "%a" pp_assign assign
    | AugmentedAssign augmented_assign ->
        Format.fprintf formatter "%a" pp_augmented_assign augmented_assign
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
    | Delete expressions ->
        List.map expressions ~f:Expression.show
        |> String.concat ~sep:", "
        |> Format.fprintf formatter "del %s"
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
        let pp_import formatter { Node.value = { Import.name; alias }; _ } =
          match alias with
          | None -> Format.fprintf formatter "%a" Reference.pp name
          | Some alias -> Format.fprintf formatter "%a as %a" Reference.pp name Identifier.pp alias
        in
        let pp_imports formatter import_list = pp_list formatter pp_import ", " import_list in
        match from with
        | None -> Format.fprintf formatter "@[<v>import %a@]" pp_imports imports
        | Some from ->
            let pp_from formatter { Node.value = reference; _ } =
              Format.fprintf formatter "%a" Reference.pp reference
            in
            Format.fprintf formatter "@[<v>from %a import %a@]" pp_from from pp_imports imports)
    | Match _ -> Format.fprintf formatter "%s" "match"
    | Nonlocal nonlocals ->
        Format.fprintf formatter "nonlocal %s" (String.concat nonlocals ~sep:", ")
    | Pass -> Format.fprintf formatter "%s" "pass"
    | Raise { Raise.expression; _ } ->
        Format.fprintf formatter "raise %a" pp_expression_option ("", expression)
    | Return { Return.expression; _ } ->
        Format.fprintf formatter "return %a" pp_expression_option ("", expression)
    | Try { Try.body; handlers; orelse; finally; handles_exception_group } ->
        let pp_try_block formatter body =
          Format.fprintf formatter "@[<v 2>try:@;%a@]" pp_statement_list body
        in
        let pp_except_block formatter handlers =
          let pp_name_node format { Node.value = name; _ } = Format.fprintf format "%s" name in
          let pp_as formatter name = pp_option ~prefix:" as " formatter name pp_name_node in
          let pp_handler formatter { Try.Handler.kind; name; body } =
            let except_keyword = if handles_exception_group then "except*" else "except" in
            Format.fprintf
              formatter
              "@[<v 2>%s%a%a:@;%a@]"
              except_keyword
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
    | TypeAlias { TypeAlias.name; type_params; value } ->
        Format.fprintf
          formatter
          "type %a%a = %a"
          Expression.pp
          name
          Expression.pp_type_param_list
          type_params
          Expression.pp
          value
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
    | Expression.Await { Await.operand; origin = _ } -> is_expression_generator operand
    | Expression.BinaryOperator { BinaryOperator.left; right; _ }
    | Expression.BooleanOperator { BooleanOperator.left; right; _ }
    | Expression.ComparisonOperator { ComparisonOperator.left; right; _ } ->
        is_expression_generator left || is_expression_generator right
    | Expression.Call { Call.callee; arguments; origin = _ } ->
        is_expression_generator callee
        || List.exists arguments ~f:(fun { Call.Argument.value; _ } ->
               is_expression_generator value)
    | Expression.Dictionary entries ->
        List.exists entries ~f:(fun entry ->
            let open Dictionary.Entry in
            match entry with
            | KeyValue KeyValue.{ key; value } ->
                is_expression_generator key || is_expression_generator value
            | Splat s -> is_expression_generator s)
    | Expression.DictionaryComprehension comprehension ->
        is_comprehension_generator
          ~is_element_generator:(fun Dictionary.Entry.KeyValue.{ key; value } ->
            is_expression_generator key || is_expression_generator value)
          comprehension
    | Expression.Generator comprehension
    | Expression.ListComprehension comprehension
    | Expression.SetComprehension comprehension ->
        is_comprehension_generator ~is_element_generator:is_expression_generator comprehension
    | Expression.List expressions
    | Expression.Set expressions
    | Expression.Tuple expressions ->
        List.exists expressions ~f:is_expression_generator
    | Expression.Starred Starred.(Once expression | Twice expression) ->
        is_expression_generator expression
    | Expression.Slice { Slice.start; stop; step; origin = _ } ->
        is_optional_expression_generator start
        || is_optional_expression_generator stop
        || is_optional_expression_generator step
    | Expression.Subscript { Subscript.base; index; origin = _ } ->
        is_expression_generator base || is_expression_generator index
    | Expression.FormatString substrings ->
        let is_substring_generator = function
          | Substring.(Literal _) -> false
          | Substring.Format format ->
              is_expression_generator format.value
              || is_optional_expression_generator format.format_spec
        in
        List.exists substrings ~f:is_substring_generator
    | Expression.Ternary { Ternary.target; test; alternative } ->
        is_expression_generator target
        || is_expression_generator test
        || is_expression_generator alternative
    | Expression.UnaryOperator { UnaryOperator.operand; _ } -> is_expression_generator operand
    | Expression.WalrusOperator { WalrusOperator.value; _ } -> is_expression_generator value
    | Expression.Constant _
    | Expression.Lambda _
    | Expression.Name _ ->
        false
  and is_comprehension_generator
        : 'a. is_element_generator:('a -> bool) -> 'a Comprehension.t -> bool
    =
   fun ~is_element_generator { Comprehension.element; generators } ->
    is_element_generator element
    || List.exists generators ~f:(fun { Comprehension.Generator.iterator; conditions; _ } ->
           is_expression_generator iterator || List.exists conditions ~f:is_expression_generator)
  and is_optional_expression_generator =
    Option.value_map ~f:is_expression_generator ~default:false
  in
  let rec is_statement_generator { Node.value; _ } =
    match value with
    | Assign { Assign.value = Some value; _ } -> is_expression_generator value
    | AugmentedAssign { AugmentedAssign.value; _ } -> is_expression_generator value
    | Assign _ -> false
    | Assert { Assert.test; message; _ } ->
        is_expression_generator test || is_optional_expression_generator message
    | Delete expressions -> List.exists expressions ~f:is_expression_generator
    | Expression expression -> is_expression_generator expression
    | Raise { Raise.expression; from } ->
        is_optional_expression_generator expression || is_optional_expression_generator from
    | Return { Return.expression; _ } -> is_optional_expression_generator expression
    | For { For.iterator; body; orelse; _ } ->
        is_expression_generator iterator
        || is_statements_generator body
        || is_statements_generator orelse
    | Match { Match.subject; cases } ->
        is_expression_generator subject
        || List.exists cases ~f:(fun { Match.Case.body; guard; _ } ->
               is_optional_expression_generator guard || is_statements_generator body)
    | If { If.test; body; orelse }
    | While { While.test; body; orelse } ->
        is_expression_generator test
        || is_statements_generator body
        || is_statements_generator orelse
    | Try { Try.body; handlers; orelse; finally; _ } ->
        is_statements_generator body
        || List.exists handlers ~f:(fun { Try.Handler.body; _ } -> is_statements_generator body)
        || is_statements_generator orelse
        || is_statements_generator finally
    | TypeAlias { TypeAlias.name; value; _ } ->
        is_expression_generator name || is_expression_generator value
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
