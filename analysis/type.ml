(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Expression
open Pyre
open PyreParser

module Record = struct
  module Variable = struct
    type state =
      | Free of { escaped: bool }
      | InFunction
    [@@deriving compare, eq, sexp, show, hash]

    module RecordNamespace = struct
      type t = int [@@deriving compare, eq, sexp, show, hash]
    end

    module RecordUnary = struct
      type 'annotation constraints =
        | Bound of 'annotation
        | Explicit of 'annotation list
        | Unconstrained
        | LiteralIntegers
      [@@deriving compare, eq, sexp, show, hash]

      type variance =
        | Covariant
        | Contravariant
        | Invariant
      [@@deriving compare, eq, sexp, show, hash]

      type 'annotation record = {
        variable: Identifier.t;
        constraints: 'annotation constraints;
        variance: variance;
        state: state;
        namespace: RecordNamespace.t
      }
      [@@deriving compare, eq, sexp, show, hash]

      let create ?(constraints = Unconstrained) ?(variance = Invariant) name =
        { variable = name; constraints; variance; state = Free { escaped = false }; namespace = 0 }


      let pp_concise format { variable; constraints; variance; _ } ~pp_type =
        let name =
          match constraints with
          | Bound _
          | Explicit _
          | Unconstrained ->
              "Variable"
          | LiteralIntegers -> "IntegerVariable"
        in
        let constraints =
          match constraints with
          | Bound bound -> Format.asprintf " (bound to %a)" pp_type bound
          | Explicit constraints ->
              Format.asprintf
                " <: [%a]"
                (Format.pp_print_list ~pp_sep:(fun format () -> Format.fprintf format ", ") pp_type)
                constraints
          | Unconstrained -> ""
          | LiteralIntegers -> ""
        in
        let variance =
          match variance with
          | Covariant -> "(covariant)"
          | Contravariant -> "(contravariant)"
          | Invariant -> ""
        in
        Format.fprintf
          format
          "%s[%s%s]%s"
          name
          (Identifier.sanitized variable)
          constraints
          variance
    end

    module RecordVariadic = struct
      module RecordParameters = struct
        type record = {
          name: Identifier.t;
          state: state;
          namespace: RecordNamespace.t
        }
        [@@deriving compare, eq, sexp, show, hash]

        let create name = { name; state = Free { escaped = false }; namespace = 1 }
      end

      module RecordList = struct
        type record = {
          name: Identifier.t;
          state: state;
          namespace: RecordNamespace.t
        }
        [@@deriving compare, eq, sexp, show, hash]

        let create name = { name; state = Free { escaped = false }; namespace = 1 }
      end
    end

    type 'a record =
      | Unary of 'a RecordUnary.record
      | ParameterVariadic of RecordVariadic.RecordParameters.record
      | ListVariadic of RecordVariadic.RecordList.record
    [@@deriving compare, eq, sexp, show, hash]
  end

  module OrderedTypes = struct
    type 'annotation t =
      | Concrete of 'annotation list
      | Variable of Variable.RecordVariadic.RecordList.record
      | Any
    [@@deriving compare, eq, sexp, show, hash]

    let pp_concise format variable ~pp_type =
      match variable with
      | Concrete types ->
          Format.fprintf
            format
            "%a"
            (Format.pp_print_list ~pp_sep:(fun format () -> Format.fprintf format ", ") pp_type)
            types
      | Variable { name; _ } -> Format.fprintf format "ListVariadic[%s]" name
      | Any -> Format.fprintf format "..."
  end

  module Callable = struct
    module RecordParameter = struct
      type 'annotation named = {
        name: Identifier.t;
        annotation: 'annotation;
        default: bool
      }
      [@@deriving compare, eq, sexp, show, hash]

      type 'annotation variable =
        | Concrete of 'annotation
        | Variadic of Variable.RecordVariadic.RecordList.record
      [@@deriving compare, eq, sexp, show, hash]

      type 'annotation t =
        | Anonymous of { index: int; annotation: 'annotation; default: bool }
        | Named of 'annotation named
        | KeywordOnly of 'annotation named
        | Variable of 'annotation variable
        | Keywords of 'annotation
      [@@deriving compare, eq, sexp, show, hash]

      let equal equal_annotation left right =
        match left, right with
        | Named left, Named right ->
            left.default = right.default
            && Identifier.equal (Identifier.sanitized left.name) (Identifier.sanitized right.name)
            && equal_annotation left.annotation right.annotation
        | _ -> equal equal_annotation left right


      let show_concise ~pp_type parameter =
        let print_named ~kind { name; annotation; default } =
          let name = Identifier.sanitized name in
          Format.asprintf
            "%s(%s, %a%s)"
            kind
            name
            pp_type
            annotation
            (if default then ", default" else "")
        in
        match parameter with
        | Anonymous { default; annotation; _ } ->
            Format.asprintf "%a%s" pp_type annotation (if default then ", default" else "")
        | Named named -> print_named ~kind:"Named" named
        | KeywordOnly named -> print_named ~kind:"KeywordOnly" named
        | Variable (Concrete annotation) -> Format.asprintf "Variable(%a)" pp_type annotation
        | Variable (Variadic { name; _ }) -> Format.asprintf "Variable(%s)" name
        | Keywords annotation -> Format.asprintf "Keywords(%a)" pp_type annotation
    end

    type kind =
      | Anonymous
      | Named of Reference.t

    and 'annotation implicit_record = {
      implicit_annotation: 'annotation;
      name: Identifier.t
    }

    and 'annotation record_parameters =
      | Defined of 'annotation RecordParameter.t list
      | Undefined
      | ParameterVariadicTypeVariable of Variable.RecordVariadic.RecordParameters.record

    and 'annotation overload = {
      annotation: 'annotation;
      parameters: 'annotation record_parameters
    }

    and invocation =
      | Static
      | Dynamic

    and 'annotation record = {
      kind: kind;
      invocation: invocation;
      implementation: 'annotation overload;
      overloads: 'annotation overload list;
      implicit: 'annotation implicit_record option
    }
    [@@deriving compare, eq, sexp, show, hash]

    let _ = equal_record (* suppress warning about unused generated version *)

    let equal_record equal_annotation left right =
      (* Ignores implicit argument to simplify unit tests. *)
      equal_kind left.kind right.kind
      && equal_overload equal_annotation left.implementation right.implementation
      && List.equal ~equal:(equal_overload equal_annotation) left.overloads right.overloads
  end
end

open Record.Callable
module Parameter = Record.Callable.RecordParameter

type primitive = Identifier.t

and literal =
  | Boolean of bool
  | Integer of int
  | String of string

and tuple =
  | Bounded of t Record.OrderedTypes.t
  | Unbounded of t

and typed_dictionary_field = {
  name: string;
  annotation: t
}

and t =
  | Annotated of t
  | Bottom
  | Callable of t Record.Callable.record
  | Any
  | Literal of literal
  | Optional of t
  | Parametric of { name: Identifier.t; parameters: t list }
  | Primitive of primitive
  | Top
  | Tuple of tuple
  | TypedDictionary of { name: Identifier.t; fields: typed_dictionary_field list; total: bool }
  | Union of t list
  | Variable of t Record.Variable.RecordUnary.record
[@@deriving compare, eq, sexp, show, hash]

let _ = show (* shadowed below *)

type type_t = t [@@deriving compare, eq, sexp, show, hash]

let type_compare = compare

let type_sexp_of_t = sexp_of_t

let type_t_of_sexp = t_of_sexp

module Map = Map.Make (struct
  type nonrec t = t

  let compare = compare

  let sexp_of_t = sexp_of_t

  let t_of_sexp = t_of_sexp
end)

let default_to_bottom map keys =
  let to_bottom solution key =
    Map.update solution key ~f:(function
        | None -> Bottom
        | Some value -> value)
  in
  List.fold keys ~f:to_bottom ~init:map


module Set = Set.Make (struct
  type nonrec t = t

  let compare = compare

  let sexp_of_t = sexp_of_t

  let t_of_sexp = t_of_sexp
end)

include Hashable.Make (struct
  type nonrec t = t

  let compare = compare

  let hash = Hashtbl.hash

  let hash_fold_t = hash_fold_t

  let sexp_of_t = sexp_of_t

  let t_of_sexp = t_of_sexp
end)

module Cache = struct
  include Hashable.Make (struct
    type nonrec t = Expression.expression

    let compare = Expression.compare_expression

    let hash = Expression.hash_expression

    let hash_fold_t = Expression.hash_fold_expression

    let sexp_of_t = Expression.sexp_of_expression

    let t_of_sexp = Expression.expression_of_sexp
  end)

  let cache = Table.create ~size:1023 ()

  let enabled = ref true

  let find element =
    if !enabled then
      Hashtbl.find cache element
    else
      None


  let set ~key ~data =
    if !enabled then
      Hashtbl.set ~key ~data cache
    else
      ()


  let disable () =
    enabled := false;
    Hashtbl.clear cache


  let enable () = enabled := true
end

let is_any = function
  | Any -> true
  | _ -> false


let is_async_iterator = function
  | Parametric { name = "typing.AsyncIterator"; _ } -> true
  | _ -> false


let is_callable = function
  | Callable _ -> true
  | _ -> false


let is_dictionary ?(with_key = None) = function
  | Parametric { name = "dict"; parameters } -> (
    match with_key, parameters with
    | Some key, [key_parameter; _] -> equal key key_parameter
    | _ -> true )
  | _ -> false


let is_ellipsis = function
  | Primitive "ellipsis" -> true
  | _ -> false


let is_final = function
  | Parametric { name = "typing.Final"; _ } -> true
  | _ -> false


let is_generator = function
  | Parametric { name = "typing.Generator" | "typing.AsyncGenerator"; _ } -> true
  | _ -> false


let is_generic_primitive = function
  | Primitive "typing.Generic" -> true
  | _ -> false


let is_iterable = function
  | Parametric { name = "typing.Iterable"; _ } -> true
  | _ -> false


let is_iterator = function
  | Parametric { name = "typing.Iterator"; _ } -> true
  | _ -> false


let is_meta = function
  | Parametric { name = "type"; _ } -> true
  | _ -> false


let is_none = function
  | Optional Bottom -> true
  | _ -> false


let is_noreturn = function
  | Primitive "typing.NoReturn" -> true
  | _ -> false


let is_object = function
  | Primitive "object" -> true
  | _ -> false


let is_optional = function
  | Optional _ -> true
  | _ -> false


let is_optional_primitive = function
  | Primitive "typing.Optional" -> true
  | _ -> false


let is_primitive = function
  | Primitive _ -> true
  | _ -> false


let is_top = function
  | Top -> true
  | _ -> false


let is_tuple = function
  | Tuple _ -> true
  | _ -> false


let is_type_alias = function
  | Primitive "typing.TypeAlias" -> true
  | _ -> false


let is_typed_dictionary = function
  | TypedDictionary _ -> true
  | _ -> false


let is_unbound = function
  | Bottom -> true
  | _ -> false


let reverse_substitute name =
  match name with
  | "collections.defaultdict" -> "typing.DefaultDict"
  | "dict" -> "typing.Dict"
  | "list" -> "typing.List"
  | "set" -> "typing.Set"
  | "type" -> "typing.Type"
  | _ -> name


let rec pp format annotation =
  match annotation with
  | Annotated annotation -> Format.fprintf format "typing.Annotated[%a]" pp annotation
  | Bottom -> Format.fprintf format "undefined"
  | Callable { kind; implementation; overloads; _ } ->
      let kind =
        match kind with
        | Anonymous -> ""
        | Named name -> Format.asprintf "(%a)" Reference.pp name
      in
      let signature_to_string { annotation; parameters } =
        let parameters =
          match parameters with
          | Undefined -> "..."
          | ParameterVariadicTypeVariable { name; _ } -> name
          | Defined parameters ->
              List.map parameters ~f:(Parameter.show_concise ~pp_type:pp)
              |> String.concat ~sep:", "
              |> fun parameters -> Format.asprintf "[%s]" parameters
        in
        Format.asprintf "%s, %a" parameters pp annotation
      in
      let implementation = signature_to_string implementation in
      let overloads =
        let overloads = List.map overloads ~f:signature_to_string in
        if List.is_empty overloads then
          ""
        else
          String.concat ~sep:"][" overloads |> Format.sprintf "[[%s]]"
      in
      Format.fprintf format "typing.Callable%s[%s]%s" kind implementation overloads
  | Any -> Format.fprintf format "typing.Any"
  | Literal (Boolean literal) ->
      Format.fprintf format "typing_extensions.Literal[%s]" (if literal then "True" else "False")
  | Literal (Integer literal) -> Format.fprintf format "typing_extensions.Literal[%d]" literal
  | Literal (String literal) -> Format.fprintf format "typing_extensions.Literal['%s']" literal
  | Optional Bottom -> Format.fprintf format "None"
  | Optional parameter -> Format.fprintf format "typing.Optional[%a]" pp parameter
  | Parametric { name = "typing.Optional" | "Optional"; parameters = [Bottom] } ->
      Format.fprintf format "None"
  | Parametric { name; parameters } ->
      let parameters =
        if List.for_all parameters ~f:(fun parameter -> is_unbound parameter || is_top parameter)
        then
          ""
        else
          List.map parameters ~f:show |> String.concat ~sep:", "
      in
      Format.fprintf format "%s[%s]" (reverse_substitute name) parameters
  | Primitive name -> Format.fprintf format "%a" String.pp name
  | Top -> Format.fprintf format "unknown"
  | Tuple tuple ->
      let parameters =
        match tuple with
        | Bounded parameters ->
            Format.asprintf "%a" (Record.OrderedTypes.pp_concise ~pp_type:pp) parameters
        | Unbounded parameter -> Format.asprintf "%a, ..." pp parameter
      in
      Format.fprintf format "typing.Tuple[%s]" parameters
  | TypedDictionary { name; fields; total } ->
      let fields =
        fields
        |> List.map ~f:(fun { name; annotation } -> Format.asprintf "%s: %a" name pp annotation)
        |> String.concat ~sep:", "
      in
      let totality = if total then "" else " (non-total)" in
      let name =
        if String.equal name "$anonymous" then
          ""
        else
          Format.sprintf " `%s`" name
      in
      let fields =
        let fields_message = Format.sprintf " with fields (%s)" fields in
        if String.equal name "" then
          fields_message
        else if String.length fields_message < 80 then
          fields_message
        else
          ""
      in
      Format.fprintf format "TypedDict%s%s%s" totality name fields
  | Union parameters ->
      Format.fprintf
        format
        "typing.Union[%s]"
        (List.map parameters ~f:show |> String.concat ~sep:", ")
  | Variable unary -> Record.Variable.RecordUnary.pp_concise format unary ~pp_type:pp


and show annotation = Format.asprintf "%a" pp annotation

let rec pp_concise format annotation =
  let pp_comma_separated =
    Format.pp_print_list ~pp_sep:(fun format () -> Format.fprintf format ", ") pp_concise
  in
  let strip_qualification identifier =
    String.split ~on:'.' identifier |> List.last |> Option.value ~default:identifier
  in
  match annotation with
  | Annotated annotation -> Format.fprintf format "typing.Annotated[%a]" pp_concise annotation
  | Bottom -> Format.fprintf format "?"
  | Callable { implementation; _ } ->
      let signature_to_string { annotation; parameters } =
        let parameters =
          match parameters with
          | Undefined -> "..."
          | ParameterVariadicTypeVariable { name; _ } -> name
          | Defined parameters ->
              let parameter = function
                | Parameter.Anonymous { annotation; _ }
                | Parameter.KeywordOnly { annotation; _ }
                | Parameter.Named { annotation; _ } ->
                    Format.asprintf "%a" pp_concise annotation
                | Parameter.Variable (Concrete annotation) ->
                    Format.asprintf "*(%a)" pp_concise annotation
                | Parameter.Variable (Variadic { name; _ }) -> Format.asprintf "*(%s)" name
                | Parameter.Keywords annotation -> Format.asprintf "**(%a)" pp_concise annotation
              in
              List.map parameters ~f:parameter
              |> String.concat ~sep:", "
              |> fun parameters -> Format.asprintf "[%s]" parameters
        in
        Format.asprintf "%s, %a" parameters pp_concise annotation
      in
      Format.fprintf format "Callable[%s]" (signature_to_string implementation)
  | Any -> Format.fprintf format "Any"
  | Literal (Boolean literal) ->
      Format.fprintf format "typing_extensions.Literal[%s]" (if literal then "True" else "False")
  | Literal (Integer literal) -> Format.fprintf format "typing_extensions.Literal[%d]" literal
  | Literal (String literal) -> Format.fprintf format "typing_extensions.Literal['%s']" literal
  | Optional Bottom -> Format.fprintf format "None"
  | Optional parameter -> Format.fprintf format "Optional[%a]" pp_concise parameter
  | Parametric { name = "typing.Optional" | "Optional"; parameters = [Bottom] } ->
      Format.fprintf format "None"
  | Parametric { name; parameters } ->
      let name = strip_qualification (reverse_substitute name) in
      if List.for_all parameters ~f:(fun parameter -> is_unbound parameter || is_top parameter)
      then
        Format.fprintf format "%s[]" name
      else
        Format.fprintf format "%s[%a]" name pp_comma_separated parameters
  | Primitive name -> Format.fprintf format "%s" (strip_qualification name)
  | Top -> Format.fprintf format "unknown"
  | Tuple (Bounded parameters) ->
      Format.fprintf
        format
        "Tuple[%a]"
        (Record.OrderedTypes.pp_concise ~pp_type:pp_concise)
        parameters
  | Tuple (Unbounded parameter) -> Format.fprintf format "Tuple[%a, ...]" pp_concise parameter
  | TypedDictionary { name = "$anonymous"; fields; _ } ->
      let fields =
        fields
        |> List.map ~f:(fun { name; annotation } ->
               Format.asprintf "%s: %a" name pp_concise annotation)
        |> String.concat ~sep:", "
      in
      Format.fprintf format "TypedDict(%s)" fields
  | TypedDictionary { name; _ } -> Format.fprintf format "%s" (strip_qualification name)
  | Union parameters -> Format.fprintf format "Union[%a]" pp_comma_separated parameters
  | Variable { variable; _ } -> Format.fprintf format "%s" (strip_qualification variable)


and show_concise annotation = Format.asprintf "%a" pp_concise annotation

let rec serialize = function
  | Bottom -> "$bottom"
  | annotation -> Format.asprintf "%a" pp annotation


let parametric name parameters = Parametric { name; parameters }

let rec annotated annotation =
  match annotation with
  | Annotated annotation -> annotated annotation
  | _ -> Annotated annotation


let awaitable parameter = Parametric { name = "typing.Awaitable"; parameters = [parameter] }

let coroutine parameters = Parametric { name = "typing.Coroutine"; parameters }

let bool = Primitive "bool"

let bytes = Primitive "bytes"

let complex = Primitive "complex"

let dictionary ~key ~value = Parametric { name = "dict"; parameters = [key; value] }

let enumeration = Primitive "enum.Enum"

let float = Primitive "float"

let number = Primitive "numbers.Number"

let generator ?(async = false) parameter =
  let none = Optional Bottom in
  if async then
    Parametric { name = "typing.AsyncGenerator"; parameters = [parameter; none] }
  else
    Parametric { name = "typing.Generator"; parameters = [parameter; none; none] }


let generic_primitive = Primitive "typing.Generic"

let integer = Primitive "int"

let literal_integer literal = Literal (Integer literal)

let iterable parameter = Parametric { name = "typing.Iterable"; parameters = [parameter] }

let iterator parameter = Parametric { name = "typing.Iterator"; parameters = [parameter] }

let async_iterator parameter =
  Parametric { name = "typing.AsyncIterator"; parameters = [parameter] }


let list parameter = Parametric { name = "list"; parameters = [parameter] }

let meta annotation = Parametric { name = "type"; parameters = [annotation] }

let named_tuple = Primitive "typing.NamedTuple"

let none = Optional Bottom

let object_primitive = Primitive "object"

let rec optional parameter =
  match parameter with
  | Top -> Top
  | Any -> Any
  | Optional _ -> parameter
  | _ -> Optional parameter


let sequence parameter = Parametric { name = "typing.Sequence"; parameters = [parameter] }

let set parameter = Parametric { name = "set"; parameters = [parameter] }

let string = Primitive "str"

let literal_string literal = Literal (String literal)

let tuple parameters = Tuple (Bounded (Concrete parameters))

let undeclared = Primitive "typing.Undeclared"

let union parameters =
  let parameters =
    let rec flattened parameters =
      let flatten sofar = function
        | Union parameters -> flattened parameters @ sofar
        | parameter -> parameter :: sofar
      in
      List.fold ~init:[] ~f:flatten parameters
    in
    let parameters = Set.of_list (flattened parameters) in
    let filter_redundant_annotations sofar annotation =
      match annotation with
      | Optional _ -> annotation :: sofar
      | _ when Set.mem parameters (Optional annotation) -> sofar
      | _ -> annotation :: sofar
    in
    Set.fold ~init:[] ~f:filter_redundant_annotations parameters |> List.sort ~compare
  in
  if List.mem ~equal parameters undeclared then
    Union parameters
  else if List.exists ~f:is_any parameters then
    Any
  else if List.exists ~f:is_top parameters then
    Top
  else
    let normalize parameters =
      let parameters =
        List.filter parameters ~f:(function parameter -> not (is_unbound parameter))
      in
      match parameters with
      | [] -> Bottom
      | [parameter] -> parameter
      | parameters -> Union parameters
    in
    let extract_optional_parameter = function
      | Optional parameter -> parameter
      | parameter -> parameter
    in
    if List.exists parameters ~f:is_optional then
      parameters
      |> List.filter ~f:(fun parameter -> not (is_none parameter))
      |> List.map ~f:extract_optional_parameter
      |> normalize
      |> fun union -> Optional union
    else
      normalize parameters


let variable ?constraints ?variance name =
  Variable (Record.Variable.RecordUnary.create ?constraints ?variance name)


let yield parameter = Parametric { name = "Yield"; parameters = [parameter] }

let parametric_substitution_map =
  [ "typing.ChainMap", "collections.ChainMap";
    "typing.Counter", "collections.Counter";
    "typing.DefaultDict", "collections.defaultdict";
    "typing.Deque", "collections.deque";
    "typing.Dict", "dict";
    "typing.FrozenSet", "frozenset";
    "typing.List", "list";
    "typing.Set", "set";
    "typing.Type", "type";
    "typing_extensions.Protocol", "typing.Protocol" ]
  |> Identifier.Table.of_alist_exn


let rec expression annotation =
  let location = Location.Reference.any in
  let create_name name = Name (Expression.create_name ~location name) in
  let get_item_call base arguments =
    let arguments =
      if List.length arguments > 1 then
        Expression.Tuple arguments
        |> Node.create_with_default_location
        |> fun tuple -> [{ Call.Argument.name = None; value = tuple }]
      else
        let create argument = { Call.Argument.name = None; value = argument } in
        List.map ~f:create arguments
    in
    Call
      { callee =
          { Node.location;
            value =
              Name
                (Name.Attribute
                   { base = { Node.location; value = create_name base };
                     attribute = "__getitem__";
                     special = true
                   })
          };
        arguments
      }
  in
  let convert_annotation annotation =
    match annotation with
    | Annotated annotation -> get_item_call "typing.Annotated" [expression annotation]
    | Bottom -> create_name "$bottom"
    | Callable { implementation; overloads; _ } -> (
        let convert_signature { annotation; parameters } =
          let parameters =
            match parameters with
            | Defined parameters ->
                let convert_parameter parameter =
                  let call ?(default = false) ?name kind annotation =
                    let arguments =
                      let annotation =
                        [{ Call.Argument.name = None; value = expression annotation }]
                      in
                      let default =
                        if default then
                          [ { Call.Argument.name = None;
                              value = Node.create ~location (create_name "default")
                            } ]
                        else
                          []
                      in
                      let name =
                        name
                        >>| (fun name ->
                              [ { Call.Argument.name = None;
                                  value = Node.create ~location (create_name name)
                                } ])
                        |> Option.value ~default:[]
                      in
                      name @ annotation @ default
                    in
                    Call
                      { callee = Node.create ~location (Name (Name.Identifier kind)); arguments }
                    |> Node.create ~location
                  in
                  match parameter with
                  | Parameter.Anonymous { annotation; default; _ } ->
                      call ~default "Anonymous" annotation
                  | Parameter.Keywords annotation -> call "Keywords" annotation
                  | Parameter.Named { name; annotation; default } ->
                      call ~default ~name "Named" annotation
                  | Parameter.KeywordOnly { name; annotation; default } ->
                      call ~default ~name "KeywordOnly" annotation
                  | Parameter.Variable (Concrete annotation) -> call "Variable" annotation
                  | Parameter.Variable (Variadic { name; _ }) -> call "Variable" (Primitive name)
                in
                List (List.map ~f:convert_parameter parameters) |> Node.create ~location
            | Undefined -> Node.create ~location Ellipsis
            | ParameterVariadicTypeVariable { name; _ } -> Node.create ~location (create_name name)
          in
          { Call.Argument.name = None;
            value = Node.create ~location (Expression.Tuple [parameters; expression annotation])
          }
        in
        let base_callable =
          Call
            { callee =
                { Node.location;
                  value =
                    Name
                      (Name.Attribute
                         { base = { Node.location; value = create_name "typing.Callable" };
                           attribute = "__getitem__";
                           special = true
                         })
                };
              arguments = [convert_signature implementation]
            }
        in
        let overloads =
          let convert_overload sofar overload =
            match sofar with
            | None ->
                Call
                  { callee = { Node.location; value = Name (Name.Identifier "__getitem__") };
                    arguments = [convert_signature overload]
                  }
                |> Node.create ~location
                |> Option.some
            | Some expression ->
                Call
                  { callee =
                      { Node.location;
                        value =
                          Name
                            (Name.Attribute
                               { base = expression; attribute = "__getitem__"; special = true })
                      };
                    arguments = [convert_signature overload]
                  }
                |> Node.create ~location
                |> Option.some
          in
          List.fold ~init:None ~f:convert_overload overloads
        in
        match overloads with
        | Some overloads ->
            Call
              { callee =
                  { Node.location;
                    value =
                      Name
                        (Name.Attribute
                           { base = { Node.location; value = base_callable };
                             attribute = "__getitem__";
                             special = true
                           })
                  };
                arguments = [{ Call.Argument.name = None; value = overloads }]
              }
        | None -> base_callable )
    | Any -> create_name "typing.Any"
    | Literal literal ->
        let literal =
          match literal with
          | Boolean true -> Expression.True
          | Boolean false -> Expression.False
          | Integer literal -> Expression.Integer literal
          | String literal -> Expression.String { value = literal; kind = StringLiteral.String }
        in
        get_item_call "typing_extensions.Literal" [Node.create ~location literal]
    | Optional Bottom -> create_name "None"
    | Optional parameter -> get_item_call "typing.Optional" [expression parameter]
    | Parametric { name = "typing.Optional"; parameters = [Bottom] } -> create_name "None"
    | Parametric { name; parameters } ->
        get_item_call (reverse_substitute name) (List.map ~f:expression parameters)
    | Primitive name -> create_name name
    | Top -> create_name "$unknown"
    | Tuple (Bounded (Concrete [])) ->
        get_item_call "typing.Tuple" [Node.create ~location (Expression.Tuple [])]
    | Tuple elements ->
        let parameters =
          match elements with
          | Bounded Any -> [Primitive "..."]
          | Bounded (Variable { name; _ }) -> [Primitive name]
          | Bounded (Concrete parameters) -> parameters
          | Unbounded parameter -> [parameter; Primitive "..."]
        in
        get_item_call "typing.Tuple" (List.map ~f:expression parameters)
    | TypedDictionary { name; fields; total } ->
        let argument =
          let tail =
            let field_to_tuple { name; annotation } =
              Node.create_with_default_location
                (Expression.Tuple
                   [ Node.create_with_default_location
                       (Expression.String { value = name; kind = StringLiteral.String });
                     expression annotation ])
            in
            List.map fields ~f:field_to_tuple
          in
          let totality =
            (if total then Expression.True else Expression.False)
            |> Node.create_with_default_location
          in
          Expression.String { value = name; kind = StringLiteral.String }
          |> Node.create_with_default_location
          |> (fun name -> Expression.Tuple (name :: totality :: tail))
          |> Node.create_with_default_location
        in
        get_item_call "mypy_extensions.TypedDict" [argument]
    | Union parameters -> get_item_call "typing.Union" (List.map ~f:expression parameters)
    | Variable { variable; _ } -> create_name variable
  in
  let value =
    match annotation with
    | Primitive "..." -> Ellipsis
    | _ -> convert_annotation annotation
  in
  Node.create_with_default_location value


module Transform = struct
  type 'state visit_result = {
    transformed_annotation: t;
    new_state: 'state
  }

  module type Transformer = sig
    type state

    val visit : state -> t -> state visit_result

    val visit_children_before : state -> t -> bool

    val visit_children_after : bool
  end

  module Make (Transformer : Transformer) = struct
    let rec visit_annotation ~state annotation =
      let visit_children annotation =
        match annotation with
        | Annotated annotation -> Annotated (visit_annotation annotation ~state)
        | Callable ({ implementation; overloads; _ } as callable) ->
            let open Record.Callable in
            let visit_overload { annotation; parameters } =
              let visit_parameters parameter =
                let visit_defined = function
                  | RecordParameter.Named ({ annotation; _ } as named) ->
                      RecordParameter.Named
                        { named with annotation = visit_annotation annotation ~state }
                  | RecordParameter.KeywordOnly ({ annotation; _ } as named) ->
                      RecordParameter.KeywordOnly
                        { named with annotation = visit_annotation annotation ~state }
                  | RecordParameter.Variable (Concrete annotation) ->
                      RecordParameter.Variable (Concrete (visit_annotation annotation ~state))
                  | RecordParameter.Variable (Variadic _) as parameter -> parameter
                  | RecordParameter.Keywords annotation ->
                      RecordParameter.Keywords (visit_annotation annotation ~state)
                  | RecordParameter.Anonymous ({ annotation; _ } as anonymous) ->
                      RecordParameter.Anonymous
                        { anonymous with annotation = visit_annotation annotation ~state }
                in
                match parameter with
                | Defined defined -> Defined (List.map defined ~f:visit_defined)
                | parameter -> parameter
              in
              { annotation = visit_annotation annotation ~state;
                parameters = visit_parameters parameters
              }
            in
            Callable
              { callable with
                implementation = visit_overload implementation;
                overloads = List.map overloads ~f:visit_overload
              }
        | Optional annotation -> optional (visit_annotation annotation ~state)
        | Parametric { name; parameters } ->
            Parametric { name; parameters = List.map parameters ~f:(visit_annotation ~state) }
        | Tuple (Bounded Any)
        | Tuple (Bounded (Variable _)) ->
            annotation
        | Tuple (Bounded (Concrete annotations)) ->
            Tuple (Bounded (Concrete (List.map annotations ~f:(visit_annotation ~state))))
        | Tuple (Unbounded annotation) -> Tuple (Unbounded (visit_annotation annotation ~state))
        | TypedDictionary ({ fields; _ } as typed_dictionary) ->
            let visit_field ({ annotation; _ } as field) =
              { field with annotation = visit_annotation annotation ~state }
            in
            TypedDictionary { typed_dictionary with fields = List.map fields ~f:visit_field }
        | Union annotations -> union (List.map annotations ~f:(visit_annotation ~state))
        | Variable ({ constraints; _ } as variable) ->
            let constraints =
              match constraints with
              | Record.Variable.RecordUnary.Bound bound ->
                  Record.Variable.RecordUnary.Bound (visit_annotation bound ~state)
              | Explicit constraints ->
                  Explicit (List.map constraints ~f:(visit_annotation ~state))
              | Unconstrained -> Unconstrained
              | LiteralIntegers -> LiteralIntegers
            in
            Variable { variable with constraints }
        | Literal _
        | Bottom
        | Top
        | Any
        | Primitive _ ->
            annotation
      in
      let annotation =
        if Transformer.visit_children_before !state annotation then
          visit_children annotation
        else
          annotation
      in
      let { transformed_annotation; new_state } = Transformer.visit !state annotation in
      state := new_state;
      if Transformer.visit_children_after then
        visit_children transformed_annotation
      else
        transformed_annotation


    let visit state annotation =
      let state = ref state in
      let transformed_annotation = visit_annotation ~state annotation in
      !state, transformed_annotation
  end
end

let exists annotation ~predicate =
  let module ExistsTransform = Transform.Make (struct
    type state = bool

    let visit_children_before _ _ = true

    let visit_children_after = false

    let visit sofar annotation =
      let new_state = sofar || predicate annotation in
      { Transform.transformed_annotation = annotation; new_state }
  end)
  in
  fst (ExistsTransform.visit false annotation)


let is_unknown annotation = exists annotation ~predicate:is_top

let is_undeclared annotation = exists annotation ~predicate:(equal undeclared)

let pp_type = pp

module Callable = struct
  module Parameter = struct
    include Record.Callable.RecordParameter

    type parameter = type_t t [@@deriving compare, eq, sexp, show, hash]

    module Map = Core.Map.Make (struct
      type nonrec t = parameter

      let compare = compare type_compare

      let sexp_of_t = sexp_of_t type_sexp_of_t

      let t_of_sexp = t_of_sexp type_t_of_sexp
    end)

    let create parameters =
      let parameter index (keyword_only, sofar) (name, annotation, default) =
        if String.equal (Identifier.sanitized name) "*" then
          true, sofar
        else
          let star, name = Identifier.split_star name in
          let keyword_only = keyword_only || Identifier.equal star "*" in
          let new_parameter =
            match star with
            | "**" -> Keywords annotation
            | "*" -> Variable (Concrete annotation)
            | _ ->
                let sanitized = Identifier.sanitized name in
                if
                  String.is_prefix sanitized ~prefix:"__"
                  && not (String.is_suffix sanitized ~suffix:"__")
                then
                  Parameter.Anonymous { index; annotation; default }
                else
                  let named = { name; annotation; default } in
                  if keyword_only then
                    Parameter.KeywordOnly named
                  else
                    Parameter.Named named
          in
          keyword_only, new_parameter :: sofar
      in
      List.foldi parameters ~f:parameter ~init:(false, []) |> snd |> List.rev


    let show_concise = show_concise ~pp_type

    let default = function
      | Anonymous { default; _ }
      | KeywordOnly { default; _ }
      | Named { default; _ } ->
          default
      | Keywords _
      | Variable _ ->
          false


    let names_compatible left right =
      match left, right with
      | Variable _, Variable _
      | Keywords _, Keywords _
      | _, Anonymous _
      | Anonymous _, _ ->
          true
      | Named { name = left; _ }, Named { name = right; _ } ->
          let left = Identifier.sanitized left in
          let right = Identifier.sanitized right in
          let left = Identifier.remove_leading_underscores left in
          let right = Identifier.remove_leading_underscores right in
          Identifier.equal left right
      | _ -> false
  end

  include Record.Callable

  type implicit = type_t Record.Callable.implicit_record [@@deriving compare, eq, sexp, show, hash]

  type t = type_t Record.Callable.record [@@deriving compare, eq, sexp, show, hash]

  type parameters = type_t Record.Callable.record_parameters
  [@@deriving compare, eq, sexp, show, hash]

  module Overload = struct
    let parameters { parameters; _ } =
      match parameters with
      | Defined parameters -> Some parameters
      | ParameterVariadicTypeVariable _
      | Undefined ->
          None


    let return_annotation { annotation; _ } = annotation

    let is_undefined { parameters; annotation } =
      match parameters with
      | Undefined -> is_unknown annotation
      | _ -> false
  end

  let from_overloads overloads =
    match overloads with
    | ({ kind = Named _; _ } as initial) :: overloads ->
        let fold sofar signature =
          match sofar, signature with
          | Some sofar, { kind; invocation; implementation; overloads; implicit } ->
              if equal_kind kind sofar.kind then
                Some
                  { kind;
                    invocation;
                    implementation;
                    overloads = sofar.overloads @ overloads;
                    implicit
                  }
              else
                None
          | _ -> None
        in
        List.fold ~init:(Some initial) ~f:fold overloads
    | _ -> None


  let map callable ~f =
    Callable callable
    |> f
    |> function
    | Callable callable -> Some callable
    | _ -> None


  let map_implementation implementation ~f =
    map
      { kind = Anonymous; invocation = Static; implementation; overloads = []; implicit = None }
      ~f
    |> function
    | Some { implementation; _ } -> implementation
    | _ -> failwith "f did not return a callable"


  let map_parameters ({ implementation; overloads; _ } as callable) ~f =
    let for_implementation ({ parameters; _ } as implementation) =
      { implementation with parameters = f parameters }
    in
    { callable with
      implementation = for_implementation implementation;
      overloads = List.map overloads ~f:for_implementation
    }


  let with_return_annotation ({ implementation; overloads; _ } as initial) ~annotation =
    let re_annotate implementation = { implementation with annotation } in
    { initial with
      implementation = re_annotate implementation;
      overloads = List.map ~f:re_annotate overloads
    }


  let create
      ?name
      ?(overloads = [])
      ?(parameters = Undefined)
      ?implicit
      ?(invocation = Static)
      ~annotation
      ()
    =
    let kind = name >>| (fun name -> Named name) |> Option.value ~default:Anonymous in
    Callable { kind; invocation; implementation = { annotation; parameters }; overloads; implicit }


  let create_from_implementation implementation =
    create ~parameters:implementation.parameters ~annotation:implementation.annotation ()
end

let lambda ~parameters ~return_annotation =
  let parameters =
    List.map parameters ~f:(fun (name, annotation) -> name, annotation, false)
    |> Callable.Parameter.create
  in
  Callable
    { kind = Anonymous;
      invocation = Static;
      implementation = { annotation = return_annotation; parameters = Defined parameters };
      overloads = [];
      implicit = None
    }


let primitive_substitution_map =
  let parametric_anys name number_of_anys =
    let rec parameters sofar remaining =
      match remaining with
      | 0 -> sofar
      | _ -> parameters (Any :: sofar) (remaining - 1)
    in
    Parametric { name; parameters = parameters [] number_of_anys }
  in
  [ "$bottom", Bottom;
    "$unknown", Top;
    "None", none;
    "function", Callable.create ~annotation:Any ();
    "dict", parametric_anys "dict" 2;
    "list", list Any;
    "tuple", Tuple (Unbounded Any);
    "type", parametric_anys "type" 1;
    "typing.Any", Any;
    "typing.AsyncContextManager", parametric_anys "typing.AsyncContextManager" 1;
    "typing.AsyncGenerator", parametric_anys "typing.AsyncGenerator" 2;
    "typing.AsyncIterable", parametric_anys "typing.AsyncIterable" 1;
    "typing.AsyncIterator", parametric_anys "typing.AsyncIterator" 1;
    "typing.Awaitable", parametric_anys "typing.Awaitable" 1;
    "typing.Callable", Callable.create ~annotation:Any ();
    "typing.ChainMap", parametric_anys "collections.ChainMap" 1;
    "typing.ContextManager", parametric_anys "typing.ContextManager" 1;
    "typing.Counter", parametric_anys "collections.Counter" 1;
    "typing.Coroutine", parametric_anys "typing.Coroutine" 3;
    "typing.DefaultDict", parametric_anys "collections.defaultdict" 2;
    "typing.Deque", parametric_anys "collections.deque" 1;
    "typing.Dict", parametric_anys "dict" 2;
    "typing.Generator", parametric_anys "typing.Generator" 3;
    "typing.Iterable", parametric_anys "typing.Iterable" 1;
    "typing.Iterator", parametric_anys "typing.Iterator" 1;
    "typing.List", list Any;
    "typing.Mapping", parametric_anys "typing.Mapping" 2;
    "typing.Sequence", parametric_anys "typing.Sequence" 1;
    "typing.Set", parametric_anys "typing.Set" 1;
    "typing.Tuple", Tuple (Unbounded Any);
    "typing.Type", parametric_anys "type" 1;
    "typing_extensions.Protocol", Primitive "typing.Protocol" ]
  |> Identifier.Table.of_alist_exn


let primitive_name = function
  | Primitive name -> Some name
  | _ -> None


type alias =
  | TypeAlias of t
  | VariableAlias of t Record.Variable.record
[@@deriving compare, eq, sexp, show, hash]

let rec create_logic ?(use_cache = true)
                     ~aliases
                     ~variable_aliases
                     { Node.value = expression; _ } =
  match Cache.find expression with
  | Some result when use_cache -> result
  | _ ->
      let result =
        let result =
          let create_logic = create_logic ~use_cache ~aliases ~variable_aliases in
          let resolve_aliases annotation =
            let visited = Hash_set.create () in
            let module ResolveTransform = Transform.Make (struct
              type state = unit

              let visit_children_before _ _ = false

              let visit_children_after = true

              let rec visit _ annotation =
                let rec resolve annotation =
                  if Core.Hash_set.mem visited annotation then
                    annotation
                  else (
                    Core.Hash_set.add visited annotation;
                    match aliases annotation, annotation with
                    | Some aliased, _ ->
                        (* We need to fully resolve aliases to aliases before we go on to resolve
                           the aliases those may contain *)
                        resolve aliased
                    | None, Parametric { name; parameters } -> (
                        let annotation = resolve (Primitive name) in
                        match annotation with
                        | Primitive name -> parametric name parameters
                        | Parametric { name; _ } ->
                            (* TODO(T44787675): Implement actual generic aliases *)
                            parametric name parameters
                        | Union elements ->
                            (* TODO(T44787675): Implement actual generic aliases *)
                            let replace_parameters = function
                              | Parametric { name; _ } -> parametric name parameters
                              | annotation -> annotation
                            in
                            Union (List.map elements ~f:replace_parameters)
                        | _ ->
                            (* This should probably error or something *)
                            parametric name parameters )
                    | _ -> annotation )
                in
                let transformed_annotation = resolve annotation in
                { Transform.transformed_annotation; new_state = () }
            end)
            in
            snd (ResolveTransform.visit () annotation)
          in
          let rec is_typing_callable = function
            | Name
                (Name.Attribute
                  { base = { Node.value = Name (Name.Identifier "typing"); _ };
                    attribute = "Callable"
                  ; _
                  }) ->
                true
            | Name (Name.Attribute { base; _ }) -> is_typing_callable (Node.value base)
            | Call { callee; _ } -> is_typing_callable (Node.value callee)
            | _ -> false
          in
          let parse_callable expression =
            let modifiers, implementation_signature, overload_signatures =
              let get_from_base base implementation_argument overloads_argument =
                match Node.value base with
                | Call { callee; arguments } when Expression.name_is ~name:"typing.Callable" callee
                  ->
                    Some arguments, implementation_argument, overloads_argument
                | Name
                    (Name.Attribute
                      { base = { Node.value = Name (Name.Identifier "typing"); _ };
                        attribute = "Callable"
                      ; _
                      }) ->
                    None, implementation_argument, overloads_argument
                | _ ->
                    (* Invalid base. *)
                    None, None, None
              in
              match expression with
              | Call
                  { callee =
                      { Node.value =
                          Name
                            (Name.Attribute
                              { base =
                                  { Node.value =
                                      Call
                                        { callee =
                                            { Node.value =
                                                Name
                                                  (Name.Attribute
                                                    { base; attribute = "__getitem__"; _ })
                                            ; _
                                            };
                                          arguments = [{ Call.Argument.value = argument; _ }]
                                        }
                                  ; _
                                  };
                                attribute = "__getitem__"
                              ; _
                              })
                      ; _
                      };
                    arguments = [{ Call.Argument.value = overloads_argument; _ }]
                  } ->
                  (* Overloads are provided *)
                  get_from_base base (Some argument) (Some overloads_argument)
              | Call
                  { callee =
                      { Node.value = Name (Name.Attribute { base; attribute = "__getitem__"; _ })
                      ; _
                      };
                    arguments = [{ Call.Argument.value = argument; _ }]
                  } ->
                  (* No overloads provided *)
                  get_from_base base (Some argument) None
              | _ -> None, None, None
            in
            let kind =
              match modifiers with
              | Some
                  ({ Call.Argument.value =
                       { Node.value = Expression.String { StringLiteral.value; _ }; _ }
                   ; _
                   }
                  :: _) ->
                  Named (Reference.create value)
              | _ -> Anonymous
            in
            let undefined = { annotation = Top; parameters = Undefined } in
            let get_signature = function
              | Expression.Tuple [parameters; annotation] ->
                  let parameters =
                    let extract_parameter index parameter =
                      match Node.value parameter with
                      | Call
                          { callee = { Node.value = Name (Name.Identifier name); _ }; arguments }
                        -> (
                          let arguments =
                            List.map arguments ~f:(fun { Call.Argument.value; _ } ->
                                Node.value value)
                          in
                          match name, arguments with
                          | "Anonymous", annotation :: tail ->
                              let default =
                                match tail with
                                | [Name (Name.Identifier "default")] -> true
                                | _ -> false
                              in
                              Parameter.Anonymous
                                { index;
                                  annotation =
                                    create_logic (Node.create_with_default_location annotation);
                                  default
                                }
                          | "Named", Name (Name.Identifier name) :: annotation :: tail ->
                              let default =
                                match tail with
                                | [Name (Name.Identifier "default")] -> true
                                | _ -> false
                              in
                              Parameter.Named
                                { name;
                                  annotation =
                                    create_logic (Node.create_with_default_location annotation);
                                  default
                                }
                          | "KeywordOnly", Name (Name.Identifier name) :: annotation :: tail ->
                              let default =
                                match tail with
                                | [Name (Name.Identifier "default")] -> true
                                | _ -> false
                              in
                              Parameter.KeywordOnly
                                { name;
                                  annotation =
                                    create_logic (Node.create_with_default_location annotation);
                                  default
                                }
                          | "Variable", tail -> (
                              let annotation =
                                match tail with
                                | annotation :: _ ->
                                    create_logic (Node.create_with_default_location annotation)
                                | _ -> Top
                              in
                              let concrete = Parameter.Variable (Concrete annotation) in
                              match annotation with
                              | Primitive name -> (
                                match variable_aliases name with
                                | Some (Record.Variable.ListVariadic variable) ->
                                    Parameter.Variable (Variadic variable)
                                | _ -> concrete )
                              | _ -> concrete )
                          | "Keywords", tail ->
                              let annotation =
                                match tail with
                                | annotation :: _ ->
                                    create_logic (Node.create_with_default_location annotation)
                                | _ -> Top
                              in
                              Parameter.Keywords annotation
                          | _ -> Parameter.Anonymous { index; annotation = Top; default = false } )
                      | _ ->
                          Parameter.Anonymous
                            { index; annotation = create_logic parameter; default = false }
                    in
                    match Node.value parameters with
                    | List [parameter] -> (
                        let normal () = Defined [extract_parameter 0 parameter] in
                        let variable_alias =
                          create_logic parameter |> primitive_name >>= variable_aliases
                        in
                        match variable_alias with
                        | Some (Record.Variable.ListVariadic variable) ->
                            Defined [Parameter.Variable (Variadic variable)]
                        | _ -> normal () )
                    | List parameters -> Defined (List.mapi ~f:extract_parameter parameters)
                    | _ -> (
                      match variable_aliases (Expression.show parameters) with
                      | Some (Record.Variable.ParameterVariadic variable) ->
                          ParameterVariadicTypeVariable variable
                      | _ -> Undefined )
                  in
                  { annotation = create_logic annotation; parameters }
              | _ -> undefined
            in
            let implementation =
              match implementation_signature with
              | Some signature -> get_signature (Node.value signature)
              | None -> undefined
            in
            let overloads =
              let rec parse_overloads = function
                | List arguments -> [get_signature (Tuple arguments)]
                | Call
                    { callee =
                        { Node.value = Name (Name.Attribute { base; attribute = "__getitem__"; _ })
                        ; _
                        };
                      arguments = [{ Call.Argument.value = argument; _ }]
                    } ->
                    get_signature (Node.value argument) :: parse_overloads (Node.value base)
                | _ -> [undefined]
              in
              match overload_signatures with
              | Some signatures -> List.rev (parse_overloads (Node.value signatures))
              | None -> []
            in
            Callable { kind; invocation = Static; implementation; overloads; implicit = None }
          in
          match expression with
          | Call
              { callee;
                arguments =
                  { Call.Argument.value = { Node.value = String { StringLiteral.value; _ }; _ }
                  ; _
                  }
                  :: arguments
              }
            when Expression.name_is ~name:"typing.TypeVar" callee ->
              let constraints =
                let explicits =
                  let explicit = function
                    | { Call.Argument.name = None; value } -> Some (create_logic value)
                    | _ -> None
                  in
                  List.filter_map ~f:explicit arguments
                in
                let bound =
                  let bound = function
                    | { Call.Argument.value; name = Some { Node.value = bound; _ } }
                      when String.equal (Identifier.sanitized bound) "bound" ->
                        Some (create_logic value)
                    | _ -> None
                  in
                  List.find_map ~f:bound arguments
                in
                if not (List.is_empty explicits) then
                  Record.Variable.RecordUnary.Explicit explicits
                else if Option.is_some bound then
                  Bound (Option.value_exn bound)
                else
                  Unconstrained
              in
              let variance =
                let variance_definition = function
                  | { Call.Argument.name = Some { Node.value = name; _ };
                      value = { Node.value = True; _ }
                    }
                    when String.equal (Identifier.sanitized name) "covariant" ->
                      Some Record.Variable.RecordUnary.Covariant
                  | { Call.Argument.name = Some { Node.value = name; _ };
                      value = { Node.value = True; _ }
                    }
                    when String.equal (Identifier.sanitized name) "contravariant" ->
                      Some Contravariant
                  | _ -> None
                in
                List.find_map arguments ~f:variance_definition
                |> Option.value ~default:Record.Variable.RecordUnary.Invariant
              in
              variable value ~constraints ~variance
          | Call
              { callee;
                arguments =
                  [ { Call.Argument.value = { Node.value = String { StringLiteral.value; _ }; _ }
                    ; _
                    } ]
              }
            when Expression.name_is ~name:"typing_extensions.IntVar" callee ->
              variable value ~constraints:LiteralIntegers
          | Call
              { callee;
                arguments =
                  [ { Call.Argument.name = None;
                      value =
                        { Node.value =
                            Expression.Tuple
                              ({ Node.value =
                                   Expression.String { value = typed_dictionary_name; _ }
                               ; _
                               }
                              :: { Node.value = true_or_false; _ } :: fields)
                        ; _
                        }
                    } ]
              }
            when Expression.name_is ~name:"mypy_extensions.TypedDict.__getitem__" callee ->
              let total =
                match true_or_false with
                | Expression.True -> Some true
                | Expression.False -> Some false
                | _ -> None
              in
              let parse_typed_dictionary total =
                let fields =
                  let tuple_to_field = function
                    | { Node.value =
                          Expression.Tuple
                            [ { Node.value = Expression.String { value = field_name; _ }; _ }
                            ; field_annotation ]
                      ; _
                      } ->
                        Some { name = field_name; annotation = create_logic field_annotation }
                    | _ -> None
                  in
                  fields |> List.filter_map ~f:tuple_to_field
                in
                TypedDictionary { name = typed_dictionary_name; fields; total }
              in
              let undefined_primitive =
                Primitive (Expression.show (Node.create_with_default_location expression))
              in
              total >>| parse_typed_dictionary |> Option.value ~default:undefined_primitive
          | Call { callee; arguments }
            when Expression.name_is ~name:"typing_extensions.Literal.__getitem__" callee ->
              let arguments =
                match arguments with
                | [ { Call.Argument.name = None;
                      value = { Node.value = Expression.Tuple arguments; _ }
                    } ] ->
                    Some (List.map arguments ~f:Node.value)
                | [{ Call.Argument.name = None; value = { Node.value = argument; _ } }] ->
                    Some [argument]
                | _ -> None
              in
              let parse = function
                | Expression.Integer literal -> Some (literal_integer literal)
                | Expression.String { StringLiteral.kind = StringLiteral.String; value } ->
                    Some (literal_string value)
                | _ -> None
              in
              arguments >>| List.map ~f:parse >>= Option.all >>| union |> Option.value ~default:Top
          | Call { callee = { Node.value = callee; _ }; _ } when is_typing_callable callee ->
              parse_callable expression
          | Call
              { callee =
                  { Node.value = Name (Name.Attribute { base; attribute = "__getitem__"; _ }); _ };
                arguments = [{ Call.Argument.value = argument; _ }]
              } -> (
              let parametric name =
                let parameters =
                  let parameters =
                    match Node.value argument with
                    | Expression.Tuple elements -> elements
                    | _ -> [argument]
                  in
                  List.map parameters ~f:create_logic
                in
                Parametric { name; parameters } |> resolve_aliases
              in
              match create_logic base, Node.value base with
              | Primitive name, _ -> parametric name
              | _, Name _ -> parametric (Expression.show base)
              | _ -> Top )
          | Name (Name.Identifier identifier) ->
              let sanitized = Identifier.sanitized identifier in
              if sanitized = "None" then
                none
              else
                Primitive sanitized |> resolve_aliases
          | Name (Name.Attribute { base; attribute; _ }) -> (
              let attribute = Identifier.sanitized attribute in
              match create_logic base with
              | Primitive primitive -> Primitive (primitive ^ "." ^ attribute) |> resolve_aliases
              | _ -> Primitive (Expression.show base ^ "." ^ attribute) )
          | Ellipsis -> Primitive "..."
          | String { StringLiteral.value; _ } ->
              let expression =
                try
                  let parsed =
                    Parser.parse [value]
                    |> Source.create
                    |> Preprocessing.preprocess
                    |> Source.statements
                  in
                  match parsed with
                  | [{ Node.value = Statement.Expression { Node.value; _ }; _ }] -> Some value
                  | _ -> None
                with
                | _ -> None
              in
              expression
              >>| Node.create_with_default_location
              >>| create_logic
              |> Option.value ~default:(Primitive value)
          | _ -> Top
        in
        (* Substitutions. *)
        match result with
        | Primitive name -> (
          match Identifier.Table.find primitive_substitution_map name with
          | Some substitute -> substitute
          | None -> result )
        | Parametric { name; parameters } -> (
          match Identifier.Table.find parametric_substitution_map name with
          | Some name -> Parametric { name; parameters }
          | None -> (
            match name with
            | "typing.Annotated" when List.length parameters > 0 ->
                annotated (List.hd_exn parameters)
            | "typing.Optional" when List.length parameters = 1 ->
                optional (List.hd_exn parameters)
            | "tuple"
            | "typing.Tuple" ->
                let tuple : tuple =
                  match parameters with
                  | [parameter; Primitive "..."] -> Unbounded parameter
                  | [Primitive "..."] -> Bounded Any
                  | [Primitive parameter] -> (
                    match variable_aliases parameter with
                    | Some (Record.Variable.ListVariadic variable) -> Bounded (Variable variable)
                    | _ -> Bounded (Concrete parameters) )
                  | _ -> Bounded (Concrete parameters)
                in
                Tuple tuple
            | "typing.Union" -> union parameters
            | _ -> result ) )
        | Union elements -> union elements
        | _ -> result
      in
      if use_cache then
        Cache.set ~key:expression ~data:result;
      result


let create ~aliases =
  let variable_aliases name =
    match aliases name with
    | Some (VariableAlias variable) -> Some variable
    | _ -> None
  in
  let aliases = function
    | Primitive name -> (
      match aliases name with
      | Some (TypeAlias alias) -> Some alias
      | _ -> None )
    | _ -> None
  in
  create_logic ~use_cache:true ~aliases ~variable_aliases


let contains_callable annotation = exists annotation ~predicate:is_callable

let contains_any annotation = exists annotation ~predicate:is_any

module LiteralAnyVisitor = struct
  module Visitor = struct
    type t = bool

    let statement state _ = state

    let expression state { Node.value; _ } =
      match state, value with
      | true, _ -> true
      | false, Name name ->
          Reference.from_name name
          >>| Reference.show
          >>| String.equal "typing.Any"
          |> Option.value ~default:false
      | _, _ -> false
  end

  include Visit.Make (Visitor)

  let expression_contains_any expression =
    let state =
      (* We also want to take into account annotations like `list`, `dict`, etc. *)
      match Node.value expression with
      | Name name when Expression.is_simple_name name ->
          Reference.from_name_exn name
          |> Reference.show
          |> Hashtbl.find primitive_substitution_map
          |> Option.value_map ~default:false ~f:contains_any
          |> fun state -> ref state
      | _ -> ref false
    in
    visit_expression ~state ~visitor:Visitor.expression expression;
    !state
end

(* Check if there is a literal Any provided, not including type aliases to Any. *)
let expression_contains_any = LiteralAnyVisitor.expression_contains_any

let is_not_instantiated annotation =
  let predicate = function
    | Bottom -> true
    | Variable { constraints = Unconstrained; _ } -> true
    | _ -> false
  in
  exists annotation ~predicate


let contains_literal annotation =
  let predicate = function
    | Literal _ -> true
    | _ -> false
  in
  exists annotation ~predicate


let contains_final annotation =
  let predicate = function
    | Parametric { name = "typing.Final"; _ } -> true
    | _ -> false
  in
  exists annotation ~predicate


let collect annotation ~predicate =
  let module CollectorTransform = Transform.Make (struct
    type state = t list

    let visit_children_before _ _ = true

    let visit_children_after = false

    let visit sofar annotation =
      let new_state = if predicate annotation then sofar @ [annotation] else sofar in
      { Transform.transformed_annotation = annotation; new_state }
  end)
  in
  fst (CollectorTransform.visit [] annotation)


let primitives annotation =
  let predicate = function
    | Primitive _ -> true
    | _ -> false
  in
  collect annotation ~predicate


let elements annotation =
  let module CollectorTransform = Transform.Make (struct
    type state = t list

    let visit_children_before _ _ = true

    let visit_children_after = false

    let visit sofar annotation =
      let new_state =
        match annotation with
        | Annotated _ -> Primitive "typing.Annotated" :: sofar
        | Callable _ -> Primitive "typing.Callable" :: sofar
        | Literal _ -> Primitive "typing_extensions.Literal" :: sofar
        | Optional _ -> Primitive "typing.Optional" :: sofar
        | Parametric { name; _ } -> Primitive name :: sofar
        | Primitive _ -> annotation :: sofar
        | Tuple _ -> Primitive "tuple" :: sofar
        | TypedDictionary _ -> Primitive "TypedDictionary" :: sofar
        | Union _ -> Primitive "typing.Union" :: sofar
        | Bottom
        | Any
        | Top
        | Variable _ ->
            sofar
      in
      { Transform.transformed_annotation = annotation; new_state }
  end)
  in
  fst (CollectorTransform.visit [] annotation) |> List.rev


let is_untyped = function
  | Any
  | Bottom
  | Top ->
      true
  | _ -> false


let is_partially_typed annotation = exists annotation ~predicate:is_untyped

let optional_value = function
  | Optional annotation -> annotation
  | annotation -> annotation


let async_generator_value = function
  | Parametric { name = "typing.AsyncGenerator"; parameters = [parameter; _] } ->
      generator parameter
  | _ -> Top


let awaitable_value = function
  | Parametric { name = "typing.Awaitable"; parameters = [parameter] } -> parameter
  | _ -> Top


let coroutine_value = function
  | Parametric { name = "typing.Coroutine"; parameters = [_; _; parameter] } -> parameter
  | _ -> Top


let parameters = function
  | Parametric { parameters; _ } -> parameters
  | _ -> []


let single_parameter = function
  | Parametric { parameters = [parameter]; _ } -> parameter
  | _ -> failwith "Type does not have single parameter"


let instantiate ?(widen = false) ?(visit_children_before = false) annotation ~constraints =
  let module InstantiateTransform = Transform.Make (struct
    type state = unit

    let visit_children_before _ annotation =
      visit_children_before || constraints annotation |> Option.is_none


    let visit_children_after = false

    let visit _ annotation =
      let transformed_annotation =
        match constraints annotation with
        | Some Bottom when widen -> Top
        | Some replacement -> replacement
        | None -> annotation
      in
      { Transform.transformed_annotation; new_state = () }
  end)
  in
  snd (InstantiateTransform.visit () annotation)


let weaken_literals annotation =
  let constraints = function
    | Literal (Integer _) -> Some integer
    | Literal (String _) -> Some string
    | Literal (Boolean _) -> Some bool
    | _ -> None
  in
  instantiate ~constraints annotation


let split annotation =
  let open Record.OrderedTypes in
  match annotation with
  | Optional parameter -> Primitive "typing.Optional", Concrete [parameter]
  | Parametric { name; parameters } -> Primitive name, Concrete parameters
  | Tuple tuple ->
      let parameters =
        match tuple with
        | Bounded parameters -> parameters
        | Unbounded parameter -> Concrete [parameter]
      in
      Primitive "tuple", parameters
  | TypedDictionary { total = true; _ } -> Primitive "TypedDictionary", Concrete []
  | TypedDictionary { total = false; _ } -> Primitive "NonTotalTypedDictionary", Concrete []
  | Literal _ as literal -> weaken_literals literal, Concrete []
  | annotation -> annotation, Concrete []


let class_name annotation =
  let strip_calls =
    let rec collect_identifiers identifiers = function
      | { Node.value = Call { callee = { Node.value = Name (Name.Attribute { base; _ }); _ }; _ }
        ; _
        } ->
          collect_identifiers identifiers base
      | { Node.value = Name (Name.Identifier identifier); _ } -> identifier :: identifiers
      | { Node.value = Name (Name.Attribute { base; attribute; _ }); _ } ->
          collect_identifiers (attribute :: identifiers) base
      | _ -> identifiers
    in
    collect_identifiers []
  in
  split annotation
  |> fst
  |> expression
  |> strip_calls
  |> fun identifiers ->
  if List.is_empty identifiers then
    Reference.create "typing.Any"
  else
    Reference.create_from_list identifiers


let class_variable annotation = parametric "typing.ClassVar" [annotation]

let class_variable_value = function
  | Parametric { name = "typing.ClassVar"; parameters = [parameter] } -> Some parameter
  | _ -> None


let final_value = function
  | Parametric { name = "typing.Final"; parameters = [parameter] } -> Some parameter
  | _ -> None


(* Angelic assumption: Any occurrences of top indicate that we're dealing with Any instead of None.
   See T22792667. *)
let assume_any = function
  | Top -> Any
  | annotation -> annotation


let dequalify_identifier map identifier =
  let rec fold accumulator reference =
    if Reference.Map.mem map reference then
      Reference.combine
        (Reference.Map.find_exn map reference)
        (Reference.create_from_list accumulator)
    else
      match Reference.prefix reference with
      | Some prefix -> fold (Reference.last reference :: accumulator) prefix
      | None -> Reference.create_from_list accumulator
  in
  identifier |> Reference.create |> fold [] |> Reference.show


module Variable : sig
  module Namespace : sig
    include module type of struct
      include Record.Variable.RecordNamespace
    end

    val reset : unit -> unit

    val create_fresh : unit -> t
  end

  type unary_t = type_t Record.Variable.RecordUnary.record
  [@@deriving compare, eq, sexp, show, hash]

  type unary_domain = type_t

  type parameter_variadic_t = Record.Variable.RecordVariadic.RecordParameters.record
  [@@deriving compare, eq, sexp, show, hash]

  type parameter_variadic_domain = Callable.parameters

  type list_variadic_t = Record.Variable.RecordVariadic.RecordList.record
  [@@deriving compare, eq, sexp, show, hash]

  type list_variadic_domain = type_t Record.OrderedTypes.t

  type pair =
    | UnaryPair of unary_t * unary_domain
    | ParameterVariadicPair of parameter_variadic_t * parameter_variadic_domain
    | ListVariadicPair of list_variadic_t * list_variadic_domain

  type t = type_t Record.Variable.record [@@deriving compare, eq, sexp, show, hash]

  type variable_t = t

  module type VariableKind = sig
    type t [@@deriving compare, eq, sexp, show, hash]

    module Map : Core.Map.S with type Key.t = t

    val is_free : t -> bool

    val is_escaped_and_free : t -> bool

    val mark_as_bound : t -> t

    val mark_as_escaped : t -> t

    val namespace : t -> namespace:Namespace.t -> t

    val dequalify : t -> dequalify_map:Reference.t Reference.Map.t -> t

    type domain [@@deriving compare, eq, sexp, show, hash]

    val any : domain

    (* The value in the domain directly corresponding to the variable, i.e. the replacement that
       would leave a type unchanged *)
    val self_reference : t -> domain

    val pair : t -> domain -> pair
  end

  module Unary : sig
    include module type of struct
      include Record.Variable.RecordUnary
    end

    include VariableKind with type t = unary_t and type domain = type_t

    val create : ?constraints:type_t constraints -> ?variance:variance -> string -> t

    val is_contravariant : t -> bool

    val is_covariant : t -> bool

    val upper_bound : t -> type_t

    val is_escaped_and_free : t -> bool

    val contains_subvariable : t -> bool
  end

  module Variadic : sig
    module Parameters : sig
      include VariableKind with type t = parameter_variadic_t and type domain = Callable.parameters

      val name : t -> Identifier.t

      val create : string -> t
    end

    module List : sig
      include VariableKind with type t = list_variadic_t and type domain = list_variadic_domain

      val name : t -> Identifier.t

      val create : string -> t
    end
  end

  module GlobalTransforms : sig
    module type S = sig
      type t

      type domain

      val replace_all : (t -> domain option) -> type_t -> type_t

      val collect_all : type_t -> t list
    end

    module Unary : S with type t = unary_t and type domain = type_t

    module ParameterVariadic :
      S with type t = parameter_variadic_t and type domain = Callable.parameters

    module ListVariadic : S with type t = list_variadic_t and type domain = list_variadic_domain
  end

  include module type of struct
    include Record.Variable
  end

  module Set : Core.Set.S with type Elt.t = t

  val pp_concise : Format.formatter -> t -> unit

  val parse_declaration : Expression.t -> t option

  val dequalify : Reference.t Reference.Map.t -> t -> t

  val namespace : t -> namespace:Namespace.t -> t

  val mark_all_variables_as_bound : type_t -> type_t

  val namespace_all_free_variables : type_t -> namespace:Namespace.t -> type_t

  val all_free_variables : type_t -> t list

  val all_variables_are_resolved : type_t -> bool

  val mark_all_free_variables_as_escaped : ?specific:t list -> type_t -> type_t

  val collapse_all_escaped_variable_unions : type_t -> type_t

  val contains_escaped_free_variable : type_t -> bool

  val convert_all_escaped_free_variables_to_anys : type_t -> type_t

  val converge_all_variable_namespaces : type_t -> type_t
end = struct
  module Namespace = struct
    include Record.Variable.RecordNamespace

    let fresh = ref 1

    let reset () = fresh := 1

    let create_fresh () =
      let namespace = !fresh in
      fresh := namespace + 1;
      namespace
  end

  type unary_t = type_t Record.Variable.RecordUnary.record
  [@@deriving compare, eq, sexp, show, hash]

  type unary_domain = type_t

  type parameter_variadic_t = Record.Variable.RecordVariadic.RecordParameters.record
  [@@deriving compare, eq, sexp, show, hash]

  type parameter_variadic_domain = Callable.parameters

  type list_variadic_t = Record.Variable.RecordVariadic.RecordList.record
  [@@deriving compare, eq, sexp, show, hash]

  type list_variadic_domain = type_t Record.OrderedTypes.t

  type pair =
    | UnaryPair of unary_t * unary_domain
    | ParameterVariadicPair of parameter_variadic_t * parameter_variadic_domain
    | ListVariadicPair of list_variadic_t * list_variadic_domain

  module type VariableKind = sig
    type t [@@deriving compare, eq, sexp, show, hash]

    module Map : Core.Map.S with type Key.t = t

    val is_free : t -> bool

    val is_escaped_and_free : t -> bool

    val mark_as_bound : t -> t

    val mark_as_escaped : t -> t

    val namespace : t -> namespace:Namespace.t -> t

    val dequalify : t -> dequalify_map:Reference.t Reference.Map.t -> t

    type domain [@@deriving compare, eq, sexp, show, hash]

    val any : domain

    val self_reference : t -> domain

    val pair : t -> domain -> pair
  end

  module Unary = struct
    include Record.Variable.RecordUnary

    type t = type_t record [@@deriving compare, eq, sexp, show, hash]

    type domain = type_t [@@deriving compare, eq, sexp, show, hash]

    module Map = Core.Map.Make (struct
      type nonrec t = t

      let compare = compare

      let sexp_of_t = sexp_of_t

      let t_of_sexp = t_of_sexp
    end)

    let any = Any

    let self_reference variable = Variable variable

    let pair variable value = UnaryPair (variable, value)

    let is_contravariant = function
      | { variance = Contravariant; _ } -> true
      | _ -> false


    let is_covariant = function
      | { variance = Covariant; _ } -> true
      | _ -> false


    let is_free = function
      | { state = Free _; _ } -> true
      | _ -> false


    let namespace variable ~namespace = { variable with namespace }

    let mark_as_bound variable = { variable with state = InFunction }

    let local_replace replacement = function
      | Variable variable -> replacement variable
      | _ -> None


    let upper_bound { constraints; _ } =
      match constraints with
      | Unconstrained -> object_primitive
      | Bound bound -> bound
      | Explicit explicits -> union explicits
      | LiteralIntegers -> integer


    let is_escaped_and_free = function
      | { state = Free { escaped }; _ } -> escaped
      | _ -> false


    let contains_subvariable { constraints; _ } =
      let is_variable = function
        | Variable _ -> true
        | _ -> false
      in
      let contains_variable = exists ~predicate:is_variable in
      match constraints with
      | Unconstrained -> false
      | Bound bound -> contains_variable bound
      | Explicit explicits -> List.exists explicits ~f:contains_variable
      | LiteralIntegers -> false


    let mark_as_escaped variable = { variable with state = Free { escaped = true } }

    let local_collect = function
      | Variable variable -> [variable]
      | _ -> []


    let dequalify ({ variable = name; _ } as variable) ~dequalify_map =
      { variable with variable = dequalify_identifier dequalify_map name }
  end

  module Variadic = struct
    module Parameters = struct
      include Record.Variable.RecordVariadic.RecordParameters

      type t = record [@@deriving compare, eq, sexp, show, hash]

      type domain = Callable.parameters [@@deriving compare, eq, sexp, show, hash]

      module Map = Core.Map.Make (struct
        type nonrec t = t

        let compare = compare

        let sexp_of_t = sexp_of_t

        let t_of_sexp = t_of_sexp
      end)

      let name { name; _ } = name

      let any = Callable.Undefined

      let self_reference variable = Callable.ParameterVariadicTypeVariable variable

      let pair variable value = ParameterVariadicPair (variable, value)

      let is_free = function
        | { state = Free _; _ } -> true
        | _ -> false


      let is_escaped_and_free = function
        | { state = Free { escaped }; _ } -> escaped
        | _ -> false


      let mark_as_bound variable = { variable with state = InFunction }

      let namespace variable ~namespace = { variable with namespace }

      let local_replace replacement = function
        | Callable callable ->
            let map = function
              | ParameterVariadicTypeVariable variable ->
                  replacement variable
                  |> Option.value ~default:(ParameterVariadicTypeVariable variable)
              | parameters -> parameters
            in
            Callable.map_parameters callable ~f:map
            |> (fun callable -> Callable callable)
            |> Option.some
        | _ -> None


      let mark_as_escaped variable = { variable with state = Free { escaped = true } }

      let local_collect = function
        | Callable { implementation; overloads; _ } ->
            let extract = function
              | { parameters = ParameterVariadicTypeVariable variable; _ } -> Some variable
              | _ -> None
            in
            List.filter_map (implementation :: overloads) ~f:extract
        | _ -> []


      let dequalify ({ name; _ } as variable) ~dequalify_map =
        { variable with name = dequalify_identifier dequalify_map name }


      let parse_declaration = function
        | { Node.value =
              Call
                { callee =
                    { Node.value =
                        Name
                          (Name.Attribute
                            { base = { Node.value = Name (Name.Identifier "pyre_extensions"); _ };
                              attribute = "ParameterSpecification";
                              special = false
                            })
                    ; _
                    };
                  arguments =
                    [ { Call.Argument.value = { Node.value = String { StringLiteral.value; _ }; _ }
                      ; _
                      } ]
                }
          ; _
          } ->
            Some (create value)
        | _ -> None
    end

    module List = struct
      include Record.Variable.RecordVariadic.RecordList

      type t = record [@@deriving compare, eq, sexp, show, hash]

      type domain = type_t Record.OrderedTypes.t [@@deriving compare, eq, sexp, show, hash]

      module Map = Core.Map.Make (struct
        type nonrec t = t

        let compare = compare

        let sexp_of_t = sexp_of_t

        let t_of_sexp = t_of_sexp
      end)

      let name { name; _ } = name

      let any = Record.OrderedTypes.Any

      let self_reference variable = Record.OrderedTypes.Variable variable

      let pair variable value = ListVariadicPair (variable, value)

      let is_free = function
        | { state = Free _; _ } -> true
        | _ -> false


      let is_escaped_and_free = function
        | { state = Free { escaped }; _ } -> escaped
        | _ -> false


      let mark_as_bound variable = { variable with state = InFunction }

      let namespace variable ~namespace = { variable with namespace }

      (* TODO(T45087986): Add more entries here as we add hosts for these variables *)
      let local_replace replacement = function
        | Tuple (Bounded (Variable variable)) ->
            replacement variable >>| fun ordered_types -> Tuple (Bounded ordered_types)
        | Callable callable ->
            let map = function
              | Defined parameters ->
                  let replace_variadic = function
                    | Callable.Parameter.Variable (Variadic variable) -> (
                      match replacement variable with
                      | None -> [Callable.Parameter.Variable (Variadic variable)]
                      | Some (Variable new_variable) ->
                          [Callable.Parameter.Variable (Variadic new_variable)]
                      | Some Any -> [Callable.Parameter.Variable (Concrete Any)]
                      | Some (Concrete concretes) ->
                          let make_anonymous annotation =
                            Callable.Parameter.Anonymous { index = 0; annotation; default = false }
                          in
                          List.map concretes ~f:make_anonymous )
                    | parameter -> [parameter]
                  in
                  let correct_indices index = function
                    | Callable.Parameter.Anonymous anonymous ->
                        Callable.Parameter.Anonymous { anonymous with index }
                    | parameter -> parameter
                  in
                  List.concat_map parameters ~f:replace_variadic
                  |> List.mapi ~f:correct_indices
                  |> fun defined -> Defined defined
              | parameters -> parameters
            in
            Callable.map_parameters callable ~f:map
            |> (fun callable -> Callable callable)
            |> Option.some
        | _ -> None


      let mark_as_escaped variable = { variable with state = Free { escaped = true } }

      (* TODO(T45087986): Add more entries here as we add hosts for these variables *)
      let local_collect = function
        | Tuple (Bounded (Variable variable)) -> [variable]
        | Callable { implementation; overloads; _ } ->
            let map = function
              | { parameters = Defined parameters; _ } ->
                  let collect_variadic = function
                    | Callable.Parameter.Variable (Variadic variable) -> Some variable
                    | _ -> None
                  in
                  List.filter_map parameters ~f:collect_variadic
              | _ -> []
            in
            implementation :: overloads |> List.concat_map ~f:map
        | _ -> []


      let dequalify ({ name; _ } as variable) ~dequalify_map =
        { variable with name = dequalify_identifier dequalify_map name }


      let parse_declaration = function
        | { Node.value =
              Call
                { callee =
                    { Node.value =
                        Name
                          (Name.Attribute
                            { base = { Node.value = Name (Name.Identifier "pyre_extensions"); _ };
                              attribute = "ListVariadic";
                              special = false
                            })
                    ; _
                    };
                  arguments =
                    [ { Call.Argument.value = { Node.value = String { StringLiteral.value; _ }; _ }
                      ; _
                      } ]
                }
          ; _
          } ->
            Some (create value)
        | _ -> None
    end
  end

  module GlobalTransforms = struct
    module type VariableKind = sig
      include VariableKind

      (* We don't want these to be part of the public interface for Unary or Variadic.Parameters *)
      val local_replace : (t -> domain option) -> type_t -> type_t option

      val local_collect : type_t -> t list
    end

    module Make (Variable : VariableKind) = struct
      include Variable

      let replace_all operation =
        instantiate
          ~visit_children_before:true
          ~constraints:(Variable.local_replace operation)
          ~widen:false


      let map operation =
        replace_all (fun variable -> operation variable |> Variable.self_reference |> Option.some)


      let mark_all_as_bound = map Variable.mark_as_bound

      let namespace_all_free_variables annotation ~namespace =
        let namespace_if_free variable =
          if Variable.is_free variable then
            Variable.namespace variable ~namespace
          else
            variable
        in
        map namespace_if_free annotation


      let mark_as_escaped annotation ~variables ~namespace =
        let mark_as_escaped_if_in_list variable =
          if List.mem variables variable ~equal:Variable.equal then
            Variable.mark_as_escaped variable |> Variable.namespace ~namespace
          else
            variable
        in
        map mark_as_escaped_if_in_list annotation


      (* Sets all of the variables of type Variable.t to the same namespace (-1). This should only
         be used to implement namespace_insensitive_compare *)
      let converge_all_variable_namespaces = map (Variable.namespace ~namespace:(-1))

      let convert_all_escaped_free_variables_to_anys =
        let convert_if_escaped variable =
          if Variable.is_escaped_and_free variable then
            Some Variable.any
          else
            Some (Variable.self_reference variable)
        in
        replace_all convert_if_escaped


      let collect_all annotation =
        let module CollectorTransform = Transform.Make (struct
          type state = Variable.t list

          let visit_children_before _ _ = true

          let visit_children_after = false

          let visit sofar annotation =
            let new_state = Variable.local_collect annotation @ sofar in
            { Transform.transformed_annotation = annotation; new_state }
        end)
        in
        fst (CollectorTransform.visit [] annotation) |> List.rev


      let all_free_variables annotation = collect_all annotation |> List.filter ~f:Variable.is_free

      let contains_escaped_free_variable annotation =
        collect_all annotation |> List.exists ~f:Variable.is_escaped_and_free
    end

    module type S = sig
      type t

      type domain

      val replace_all : (t -> domain option) -> type_t -> type_t

      val collect_all : type_t -> t list
    end

    module Unary = Make (Unary)
    module ParameterVariadic = Make (Variadic.Parameters)
    module ListVariadic = Make (Variadic.List)
  end

  let pp_type = pp

  type t = type_t Record.Variable.record [@@deriving compare, eq, sexp, show, hash]

  type variable_t = t

  include Record.Variable

  module Set = Core.Set.Make (struct
    type nonrec t = t

    let compare = compare

    let sexp_of_t = sexp_of_t

    let t_of_sexp = t_of_sexp
  end)

  let pp_concise format = function
    | Unary variable -> Unary.pp_concise format variable ~pp_type
    | ParameterVariadic { name; _ } ->
        Format.fprintf format "CallableParameterTypeVariable[%s]" name
    | ListVariadic { name; _ } -> Format.fprintf format "ListVariadic[%s]" name


  let parse_declaration expression =
    match Variadic.Parameters.parse_declaration expression with
    | Some variable -> Some (ParameterVariadic variable)
    | None -> (
      match Variadic.List.parse_declaration expression with
      | Some variable -> Some (ListVariadic variable)
      | None -> None )


  let dequalify dequalify_map = function
    | Unary variable -> Unary (Unary.dequalify variable ~dequalify_map)
    | ParameterVariadic variable ->
        ParameterVariadic (Variadic.Parameters.dequalify variable ~dequalify_map)
    | ListVariadic variable -> ListVariadic (Variadic.List.dequalify variable ~dequalify_map)


  let namespace variable ~namespace =
    match variable with
    | Unary variable -> Unary (Unary.namespace variable ~namespace)
    | ParameterVariadic variable ->
        ParameterVariadic (Variadic.Parameters.namespace variable ~namespace)
    | ListVariadic variable -> ListVariadic (Variadic.List.namespace variable ~namespace)


  let mark_all_variables_as_bound annotation =
    GlobalTransforms.Unary.mark_all_as_bound annotation
    |> GlobalTransforms.ParameterVariadic.mark_all_as_bound
    |> GlobalTransforms.ListVariadic.mark_all_as_bound


  let namespace_all_free_variables annotation ~namespace =
    GlobalTransforms.Unary.namespace_all_free_variables annotation ~namespace
    |> GlobalTransforms.ParameterVariadic.namespace_all_free_variables ~namespace
    |> GlobalTransforms.ListVariadic.namespace_all_free_variables ~namespace


  let all_free_variables annotation =
    let unaries =
      GlobalTransforms.Unary.all_free_variables annotation
      |> List.map ~f:(fun variable -> Unary variable)
    in
    let callable_variadics =
      GlobalTransforms.ParameterVariadic.all_free_variables annotation
      |> List.map ~f:(fun variable -> ParameterVariadic variable)
    in
    let list_variadics =
      GlobalTransforms.ListVariadic.all_free_variables annotation
      |> List.map ~f:(fun variable -> ListVariadic variable)
    in
    unaries @ callable_variadics @ list_variadics


  let all_variables_are_resolved annotation = all_free_variables annotation |> List.is_empty

  let mark_all_free_variables_as_escaped ?specific annotation =
    let fresh_namespace = Namespace.create_fresh () in
    let variables =
      match specific with
      | Some variables -> variables
      | None -> all_free_variables annotation
    in
    let partition = function
      | Unary variable -> `Fst variable
      | ParameterVariadic variable -> `Snd variable
      | ListVariadic variable -> `Trd variable
    in
    let specific_unaries, specific_parameters_variadics, specific_list_variadics =
      List.partition3_map ~f:partition variables
    in
    GlobalTransforms.Unary.mark_as_escaped
      annotation
      ~variables:specific_unaries
      ~namespace:fresh_namespace
    |> GlobalTransforms.ParameterVariadic.mark_as_escaped
         ~variables:specific_parameters_variadics
         ~namespace:fresh_namespace
    |> GlobalTransforms.ListVariadic.mark_as_escaped
         ~variables:specific_list_variadics
         ~namespace:fresh_namespace


  let collapse_all_escaped_variable_unions annotation =
    let module ConcreteTransform = Transform.Make (struct
      type state = unit

      let visit_children_before _ _ = true

      let visit_children_after = false

      let visit new_state annotation =
        let transformed_annotation =
          match annotation with
          | Union parameters ->
              let not_escaped_free_variable = function
                | Variable variable -> not (Unary.is_escaped_and_free variable)
                | _ -> true
              in
              List.filter parameters ~f:not_escaped_free_variable |> union
          | _ -> annotation
        in
        { Transform.transformed_annotation; new_state }
    end)
    in
    snd (ConcreteTransform.visit () annotation)


  let contains_escaped_free_variable annotation =
    GlobalTransforms.Unary.contains_escaped_free_variable annotation
    || GlobalTransforms.ParameterVariadic.contains_escaped_free_variable annotation
    || GlobalTransforms.ListVariadic.contains_escaped_free_variable annotation


  let convert_all_escaped_free_variables_to_anys annotation =
    GlobalTransforms.Unary.convert_all_escaped_free_variables_to_anys annotation
    |> GlobalTransforms.ParameterVariadic.convert_all_escaped_free_variables_to_anys
    |> GlobalTransforms.ListVariadic.convert_all_escaped_free_variables_to_anys


  let converge_all_variable_namespaces annotation =
    GlobalTransforms.Unary.converge_all_variable_namespaces annotation
    |> GlobalTransforms.ParameterVariadic.converge_all_variable_namespaces
    |> GlobalTransforms.ListVariadic.converge_all_variable_namespaces
end

let namespace_insensitive_compare left right =
  compare
    (Variable.converge_all_variable_namespaces left)
    (Variable.converge_all_variable_namespaces right)


let is_concrete annotation =
  let module ConcreteTransform = Transform.Make (struct
    type state = bool

    let visit_children_before _ = function
      | Optional Bottom -> false
      | Parametric { name = "typing.Optional" | "Optional"; parameters = [Bottom] } -> false
      | _ -> true


    let visit_children_after = false

    let visit sofar annotation =
      let new_state =
        match annotation with
        | Bottom
        | Top
        | Any ->
            false
        | _ -> sofar
      in
      { Transform.transformed_annotation = annotation; new_state }
  end)
  in
  fst (ConcreteTransform.visit true annotation)
  && not (Variable.contains_escaped_free_variable annotation)


let rec dequalify map annotation =
  let dequalify_string string = string |> dequalify_identifier map in
  let module DequalifyTransform = Transform.Make (struct
    type state = unit

    let visit_children_before _ _ = true

    let visit_children_after = false

    let visit _ annotation =
      let transformed_annotation =
        match annotation with
        | Optional parameter ->
            Parametric { name = dequalify_string "typing.Optional"; parameters = [parameter] }
        | Parametric { name; parameters } ->
            Parametric { name = dequalify_identifier map (reverse_substitute name); parameters }
        | Union parameters -> Parametric { name = dequalify_string "typing.Union"; parameters }
        | Primitive name -> Primitive (dequalify_identifier map name)
        | Variable ({ variable = name; _ } as annotation) ->
            Variable { annotation with variable = dequalify_identifier map name }
        | _ -> annotation
      in
      { Transform.transformed_annotation; new_state = () }
  end)
  in
  snd (DequalifyTransform.visit () annotation)


module TypedDictionary = struct
  let anonymous ~total fields = TypedDictionary { name = "$anonymous"; fields; total }

  let fields_have_colliding_keys left_fields right_fields =
    let found_collision { name = needle_name; annotation = needle_annotation } =
      let same_name_different_annotation { name; annotation } =
        String.equal name needle_name && not (equal annotation needle_annotation)
      in
      List.exists left_fields ~f:same_name_different_annotation
    in
    List.exists right_fields ~f:found_collision


  let field_named_parameters ~default fields =
    let field_to_argument { name; annotation } =
      Record.Callable.RecordParameter.KeywordOnly
        { name = Format.asprintf "$parameter$%s" name; annotation; default }
    in
    List.map ~f:field_to_argument fields |> fun parameters -> Defined parameters


  let constructor ~name ~fields ~total =
    let annotation = TypedDictionary { name; fields; total } in
    { Callable.kind = Named (Reference.create "__init__");
      invocation = Static;
      implementation = { annotation = Top; parameters = Undefined };
      overloads =
        [ { annotation; parameters = field_named_parameters ~default:(not total) fields };
          { annotation;
            parameters =
              Defined
                [ Record.Callable.RecordParameter.Anonymous
                    { index = 0; annotation; default = false } ]
          } ];
      implicit = None
    }


  type special_method = {
    name: string;
    special_index: int option;
    overloads: typed_dictionary_field list -> t Callable.overload list
  }

  let key_parameter name =
    Parameter.Named { name = "k"; annotation = literal_string name; default = false }


  let total_special_methods =
    let getitem_overloads =
      let overload { name; annotation } =
        { annotation; parameters = Defined [key_parameter name] }
      in
      List.map ~f:overload
    in
    let setitem_overloads =
      let overload { name; annotation } =
        { annotation = none;
          parameters =
            Defined [key_parameter name; Named { name = "v"; annotation; default = false }]
        }
      in
      List.map ~f:overload
    in
    let get_overloads =
      let overloads { name; annotation } =
        [ { annotation = Optional annotation; parameters = Defined [key_parameter name] };
          { annotation = Union [annotation; Variable (Variable.Unary.create "_T")];
            parameters =
              Defined
                [ key_parameter name;
                  Named
                    { name = "default";
                      annotation = Variable (Variable.Unary.create "_T");
                      default = false
                    } ]
          } ]
      in
      List.concat_map ~f:overloads
    in
    let setdefault_overloads =
      let overload { name; annotation } =
        { annotation;
          parameters =
            Defined [key_parameter name; Named { name = "default"; annotation; default = false }]
        }
      in
      List.map ~f:overload
    in
    let update_overloads fields =
      [{ annotation = none; parameters = field_named_parameters fields ~default:true }]
    in
    [ { name = "__getitem__"; special_index = Some 1; overloads = getitem_overloads };
      { name = "__setitem__"; special_index = Some 1; overloads = setitem_overloads };
      { name = "get"; special_index = Some 1; overloads = get_overloads };
      { name = "setdefault"; special_index = Some 1; overloads = setdefault_overloads };
      { name = "update"; special_index = None; overloads = update_overloads } ]


  let non_total_special_methods =
    let pop_overloads =
      let overloads { name; annotation } =
        [ { annotation; parameters = Defined [key_parameter name] };
          { annotation = Union [annotation; Variable (Variable.Unary.create "_T")];
            parameters =
              Defined
                [ key_parameter name;
                  Named
                    { name = "default";
                      annotation = Variable (Variable.Unary.create "_T");
                      default = false
                    } ]
          } ]
      in
      List.concat_map ~f:overloads
    in
    let delitem_overloads fields =
      let overload { name; annotation = _ } =
        { annotation = none; parameters = Defined [key_parameter name] }
      in
      List.map ~f:overload fields
    in
    [ { name = "pop"; special_index = Some 1; overloads = pop_overloads };
      { name = "__delitem__"; special_index = Some 1; overloads = delitem_overloads } ]


  let special_overloads ~fields ~method_name ~total =
    let special_methods =
      if total then total_special_methods else non_total_special_methods @ total_special_methods
    in
    List.find special_methods ~f:(fun { name; _ } -> String.equal name method_name)
    >>| fun { overloads; _ } -> overloads fields


  let is_special_mismatch ~method_name ~position ~total =
    let special_methods =
      if total then total_special_methods else non_total_special_methods @ total_special_methods
    in
    List.find special_methods ~f:(fun { name; _ } -> String.equal name method_name)
    >>= (fun { special_index; _ } -> special_index)
    >>| ( = ) position
    |> Option.value ~default:false


  let defines ~t_self_expression ~total =
    let class_name = if total then "TypedDictionary" else "NonTotalTypedDictionary" in
    let define ?self_parameter ?return_annotation name =
      Statement.Define
        { signature =
            { name = Reference.create_from_list [class_name; name];
              parameters =
                [ { Ast.Parameter.name = "self"; value = None; annotation = self_parameter }
                  |> Node.create_with_default_location ];
              decorators = [];
              docstring = None;
              return_annotation;
              async = false;
              parent = Some (Reference.create class_name)
            };
          body = []
        }
      |> Node.create_with_default_location
    in
    if total then
      define ~self_parameter:t_self_expression ~return_annotation:t_self_expression "copy"
      :: List.map total_special_methods ~f:(fun { name; _ } -> define name)
    else
      List.map non_total_special_methods ~f:(fun { name; _ } -> define name)
end

let remove_undeclared annotation =
  let module RemoveUndeclared = Transform.Make (struct
    type state = unit

    let visit_children_before _ _ = true

    let visit_children_after = false

    let visit _ annotation =
      let transformed_annotation =
        match annotation with
        | Parametric { name; parameters } ->
            let declare annotation =
              match annotation with
              | Primitive "typing.Undeclared" -> Any
              | _ -> annotation
            in
            let parameters = List.map parameters ~f:declare in
            Parametric { name; parameters }
        | Union annotations -> (
            let annotations =
              let declared = function
                | Primitive "typing.Undeclared" -> false
                | _ -> true
              in
              List.filter ~f:declared annotations
            in
            match annotations with
            | [] -> Any
            | [annotation] -> annotation
            | _ -> union annotations )
        | _ -> annotation
      in
      { Transform.transformed_annotation; new_state = () }
  end)
  in
  match annotation with
  | Primitive "typing.Undeclared" -> Any
  | _ -> snd (RemoveUndeclared.visit () annotation)


let to_yojson annotation = `String (show annotation)
