(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Pyre
open PyreParser


module Record = struct
  module Variable = struct
    module RecordNamespace = struct
      type t =  int
      [@@deriving compare, eq, sexp, show, hash]
    end
    type state =
      | Free of { escaped: bool }
      | InFunction
      | InSimulatedCall
    [@@deriving compare, eq, sexp, show, hash]


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
      namespace: RecordNamespace.t;
    }
    [@@deriving compare, eq, sexp, show, hash]

    let create ?(constraints = Unconstrained) ?(variance = Invariant) name =
      {
        variable = name;
        constraints;
        variance;
        state = Free { escaped = false };
        namespace = 0;
      }
  end
  module Callable = struct
    module RecordParameter = struct
      type 'annotation named = {
        name: Identifier.t;
        annotation: 'annotation;
        default: bool;
      }
      [@@deriving compare, sexp, show, hash]


      let equal_named equal_annotation left right =
        left.default = right.default &&
        Identifier.equal (Identifier.sanitized left.name) (Identifier.sanitized right.name) &&
        equal_annotation left.annotation right.annotation


      type 'annotation t =
        | Named of 'annotation named
        | Variable of 'annotation named
        | Keywords of 'annotation named
      [@@deriving compare, eq, sexp, show, hash]
    end


    type kind =
      | Anonymous
      | Named of Reference.t


    and 'annotation implicit_record = {
      implicit_annotation: 'annotation;
      name: Identifier.t;
    }


    and 'annotation parameters =
      | Defined of ('annotation RecordParameter.t) list
      | Undefined


    and 'annotation overload = {
      annotation: 'annotation;
      parameters: 'annotation parameters;
    }


    and 'annotation record = {
      kind: kind;
      implementation: 'annotation overload;
      overloads: ('annotation overload) list;
      implicit: ('annotation implicit_record) option;
    }
    [@@deriving compare, eq, sexp, show, hash]


    let _ = equal_record  (* suppress warning about unused generated version *)


    let equal_record equal_annotation left right =
      (* Ignores implicit argument to simplify unit tests. *)
      equal_kind left.kind right.kind &&
      equal_overload equal_annotation left.implementation right.implementation &&
      List.equal ~equal:(equal_overload equal_annotation) left.overloads right.overloads
  end
end


open Record.Callable


module Parameter = Record.Callable.RecordParameter


type primitive = Identifier.t
[@@deriving compare, eq, sexp, show, hash]


type literal =
  | Boolean of bool
  | Integer of int
  | String of string
[@@deriving compare, eq, sexp, show, hash]


type tuple =
  | Bounded of t list
  | Unbounded of t


and typed_dictionary_field = {
  name: string;
  annotation: t;
}


and t =
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
  | Variable of t Record.Variable.record
[@@deriving compare, eq, sexp, show, hash]


let _ = show  (* shadowed below *)


type type_t = t
[@@deriving compare, eq, sexp, show, hash]


let type_compare = compare
let type_sexp_of_t = sexp_of_t
let type_t_of_sexp = t_of_sexp


module Map = Map.Make(struct
    type nonrec t = t
    let compare = compare
    let sexp_of_t = sexp_of_t
    let t_of_sexp = t_of_sexp
  end)


let default_to_bottom map keys =
  let to_bottom solution key =
    Map.update solution key ~f:(function | None -> Bottom | Some value -> value)
  in
  List.fold keys ~f:to_bottom ~init:map


module Set = Set.Make(struct
    type nonrec t = t
    let compare = compare
    let sexp_of_t = sexp_of_t
    let t_of_sexp = t_of_sexp
  end)


include Hashable.Make(struct
    type nonrec t = t
    let compare = compare
    let hash = Hashtbl.hash
    let hash_fold_t = hash_fold_t
    let sexp_of_t = sexp_of_t
    let t_of_sexp = t_of_sexp
  end)


module Cache = struct
  include Hashable.Make(struct
      type nonrec t = Expression.expression
      let compare = Expression.compare_expression
      let hash = Expression.hash_expression
      let hash_fold_t = Expression.hash_fold_expression
      let sexp_of_t = Expression.sexp_of_expression
      let t_of_sexp = Expression.expression_of_sexp
    end)

  let cache =
    Table.create ~size:1023 ()

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

  let enable () =
    enabled := true
end

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
  | Bottom ->
      Format.fprintf format "undefined"
  | Callable { kind; implementation; overloads; _ } ->
      let kind =
        match kind with
        | Anonymous -> ""
        | Named name -> Format.asprintf "(%a)" Reference.pp name
      in
      let signature_to_string { annotation; parameters } =
        let parameters =
          match parameters with
          | Undefined ->
              "..."
          | Defined parameters ->
              let parameter = function
                | Parameter.Named { Parameter.name; annotation; default } ->
                    let name = Identifier.sanitized name in
                    if String.is_prefix ~prefix:"$" name then
                      Format.asprintf
                        "%a%s"
                        pp annotation
                        (if default then ", default" else "")
                    else
                      Format.asprintf
                        "Named(%s, %a%s)"
                        name
                        pp annotation
                        (if default then ", default" else "")
                | Parameter.Variable { Parameter.name; annotation; _ } ->
                    Format.asprintf
                      "Variable(%s, %a)"
                      (Identifier.sanitized name)
                      pp annotation
                | Parameter.Keywords { Parameter.name; annotation; _ } ->
                    Format.asprintf
                      "Keywords(%s, %a)"
                      (Identifier.sanitized name)
                      pp annotation
              in
              List.map parameters ~f:parameter
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
          String.concat ~sep:"][" overloads
          |> Format.sprintf "[[%s]]"
      in
      Format.fprintf format "typing.Callable%s[%s]%s" kind implementation overloads
  | Any ->
      Format.fprintf format "typing.Any"
  | Literal Boolean literal ->
      Format.fprintf format "typing_extensions.Literal[%s]" (if literal then "True" else "False")
  | Literal Integer literal ->
      Format.fprintf format "typing_extensions.Literal[%d]" literal
  | Literal String literal ->
      Format.fprintf format "typing_extensions.Literal['%s']" literal
  | Optional Bottom ->
      Format.fprintf format "None"
  | Optional parameter ->
      Format.fprintf format "typing.Optional[%a]" pp parameter
  | Parametric { name = ("typing.Optional" | "Optional"); parameters = [Bottom] } ->
      Format.fprintf format "None"
  | Parametric { name; parameters } ->
      let parameters =
        if List.for_all
            parameters
            ~f:(fun parameter -> equal parameter Bottom || equal parameter Top) then
          ""
        else
          List.map parameters ~f:show
          |> String.concat ~sep:", "
      in
      Format.fprintf format
        "%s[%s]"
        (reverse_substitute name)
        parameters
  | Primitive name ->
      Format.fprintf format "%a" String.pp name
  | Top ->
      Format.fprintf format "unknown"
  | Tuple tuple ->
      let parameters =
        match tuple with
        | Bounded parameters ->
            List.map parameters ~f:show
            |> String.concat ~sep:", "
        | Unbounded parameter ->
            Format.asprintf "%a, ..." pp parameter
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
      Format.fprintf format "TypedDict%s%s with fields (%s)" totality name fields
  | Union parameters ->
      Format.fprintf format
        "typing.Union[%s]"
        (List.map parameters ~f:show
         |> String.concat ~sep:", ")
  | Variable { variable; constraints; variance; _ } ->
      let name =
        match constraints with
        | Bound _
        | Explicit _
        | Unconstrained ->
            "Variable"
        | LiteralIntegers ->
            "IntegerVariable"
      in
      let constraints =
        match constraints with
        | Bound bound ->
            Format.asprintf " (bound to %a)" pp bound
        | Explicit constraints ->
            Format.asprintf
              " <: [%a]"
              (Format.pp_print_list ~pp_sep:(fun format () -> Format.fprintf format ", ") pp)
              constraints
        | Unconstrained ->
            ""
        | LiteralIntegers ->
            ""
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


and show annotation =
  Format.asprintf "%a" pp annotation


let rec pp_concise format annotation =
  let pp_comma_separated =
    (Format.pp_print_list ~pp_sep:(fun format () -> Format.fprintf format ", ") pp_concise)
  in
  let strip_qualification identifier =
    String.split ~on:'.' identifier
    |> List.last
    |> Option.value ~default:identifier
  in
  match annotation with
  | Bottom ->
      Format.fprintf format "?"
  | Callable { implementation; _ } ->
      let signature_to_string { annotation; parameters } =
        let parameters =
          match parameters with
          | Undefined ->
              "..."
          | Defined parameters ->
              let parameter = function
                | Parameter.Named { Parameter.annotation; _ } ->
                    Format.asprintf "%a" pp_concise annotation
                | Parameter.Variable { Parameter.annotation; _ } ->
                    Format.asprintf "*(%a)" pp_concise annotation
                | Parameter.Keywords { Parameter.annotation; _ } ->
                    Format.asprintf "**(%a)" pp_concise annotation
              in
              List.map parameters ~f:parameter
              |> String.concat ~sep:", "
              |> fun parameters -> Format.asprintf "[%s]" parameters
        in
        Format.asprintf "%s, %a" parameters pp_concise annotation
      in
      Format.fprintf format "Callable[%s]" (signature_to_string implementation)
  | Any ->
      Format.fprintf format "Any"
  | Literal Boolean literal ->
      Format.fprintf format "typing_extensions.Literal[%s]" (if literal then "True" else "False")
  | Literal Integer literal ->
      Format.fprintf format "typing_extensions.Literal[%d]" literal
  | Literal String literal ->
      Format.fprintf format "typing_extensions.Literal['%s']" literal
  | Optional Bottom ->
      Format.fprintf format "None"
  | Optional parameter ->
      Format.fprintf format "Optional[%a]" pp_concise parameter
  | Parametric { name = ("typing.Optional" | "Optional"); parameters = [Bottom] } ->
      Format.fprintf format "None"
  | Parametric { name; parameters } ->
      let name = strip_qualification (reverse_substitute name) in
      if List.for_all
          parameters
          ~f:(fun parameter -> equal parameter Bottom || equal parameter Top)
      then
        Format.fprintf format "%s[]" name
      else
        Format.fprintf format "%s[%a]" name pp_comma_separated parameters
  | Primitive name ->
      Format.fprintf format "%s" (strip_qualification name)
  | Top ->
      Format.fprintf format "unknown"
  | Tuple (Bounded parameters) ->
      Format.fprintf format "Tuple[%a]" pp_comma_separated parameters
  | Tuple (Unbounded parameter) ->
      Format.fprintf format "Tuple[%a, ...]" pp_concise parameter
  | TypedDictionary { name = "$anonymous"; fields; _ } ->
      let fields =
        fields
        |> List.map
          ~f:(fun { name; annotation } -> Format.asprintf "%s: %a" name pp_concise annotation)
        |> String.concat ~sep:", "
      in
      Format.fprintf format "TypedDict(%s)" fields
  | TypedDictionary { name; _ } ->
      Format.fprintf format "%s" (strip_qualification name)
  | Union parameters ->
      Format.fprintf format "Union[%a]" pp_comma_separated parameters
  | Variable { variable; _ } ->
      Format.fprintf format "%s" (strip_qualification variable)


and show_concise annotation =
  Format.asprintf "%a" pp_concise annotation


let rec serialize = function
  | Bottom ->
      "$bottom"
  | annotation ->
      Format.asprintf "%a" pp annotation


let parametric name parameters =
  Parametric { name; parameters }


let awaitable parameter =
  Parametric {
    name = "typing.Awaitable";
    parameters = [parameter];
  }


let coroutine parameters =
  Parametric {
    name = "typing.Coroutine";
    parameters;
  }


let bool =
  Primitive "bool"


let bytes =
  Primitive "bytes"


let complex =
  Primitive "complex"


let dictionary ~key ~value =
  Parametric {
    name = "dict";
    parameters = [key; value];
  }


let ellipsis =
  Primitive "ellipsis"


let enumeration =
  Primitive "enum.Enum"


let float =
  Primitive "float"


let number =
  Primitive "numbers.Number"


let generator ?(async=false) parameter =
  let none = Optional Bottom in
  if async then
    Parametric {
      name = "typing.AsyncGenerator";
      parameters = [parameter; none];
    }
  else
    Parametric {
      name = "typing.Generator";
      parameters = [parameter; none; none];
    }


let generic =
  Primitive "typing.Generic"


let integer =
  Primitive "int"


let literal_integer literal =
  Literal (Integer literal)


let iterable parameter =
  Parametric {
    name = "typing.Iterable";
    parameters = [parameter];
  }


let iterator parameter =
  Parametric {
    name = "typing.Iterator";
    parameters = [parameter];
  }


let async_iterator parameter =
  Parametric {
    name = "typing.AsyncIterator";
    parameters = [parameter];
  }


let lambda ~parameters ~return_annotation =
  Callable {
    kind = Anonymous;
    implementation = {
      annotation = return_annotation;
      parameters =
        Defined
          (List.map
             ~f:(fun (name, parameter) ->
                 if String.is_prefix ~prefix:"**" name then
                   let name = String.drop_prefix name 2 in
                   Parameter.Keywords { Parameter.name; annotation = parameter; default = false }
                 else if String.is_prefix ~prefix:"*" name then
                   let name = String.drop_prefix name 1 in
                   Parameter.Variable { Parameter.name; annotation = parameter; default = false }
                 else
                   Parameter.Named { Parameter.name; annotation = parameter; default = false })
             parameters);
    };
    overloads = [];
    implicit = None;
  }


let list parameter =
  Parametric {
    name = "list";
    parameters = [parameter];
  }


let meta annotation =
  Parametric {
    name = "type";
    parameters = [annotation];
  }


let named_tuple =
  Primitive "typing.NamedTuple"


let none =
  Optional Bottom


let object_primitive =
  Primitive "object"


let rec optional parameter =
  match parameter with
  | Top ->
      Top
  | Any ->
      Any
  | Optional _ ->
      parameter
  | _ ->
      Optional parameter


let sequence parameter =
  Parametric {
    name = "typing.Sequence";
    parameters = [parameter];
  }


let set parameter =
  Parametric {
    name = "set";
    parameters = [parameter];
  }


let string =
  Primitive "str"


let literal_string literal =
  Literal (String literal)


let tuple parameters: t =
  match parameters with
  | [] -> Tuple (Bounded [])
  | _ -> Tuple (Bounded parameters)


let undeclared =
  Primitive "typing.Undeclared"


let union parameters =
  let parameters =
    let rec flattened parameters =
      let flatten sofar = function
        | Union parameters -> (flattened parameters) @ sofar
        | parameter -> parameter :: sofar
      in
      List.fold ~init:[] ~f:flatten parameters
    in
    let parameters = Set.of_list (flattened parameters) in
    let filter_redundant_annotations sofar annotation =
      match annotation with
      | Primitive _ when  Set.mem parameters (optional annotation) ->
          sofar
      | _ ->
          annotation :: sofar
    in
    Set.fold ~init:[] ~f:filter_redundant_annotations parameters
    |> List.sort ~compare
  in
  if List.mem ~equal parameters undeclared then
    Union parameters
  else if List.mem ~equal parameters Any then
    Any
  else if List.mem ~equal parameters Top then
    Top
  else
    let normalize parameters =
      let parameters = List.filter parameters ~f:(function | Bottom -> false | _ -> true) in
      match parameters with
      | [] -> Bottom
      | [parameter] -> parameter
      | parameters -> Union parameters
    in
    let is_optional = function
      | Optional _ -> true
      | _ -> false
    in
    let extract_optional_parameter = function
      | Optional parameter -> parameter
      | parameter -> parameter
    in
    if (List.exists parameters ~f:is_optional) then
      parameters
      |> List.filter ~f:(fun parameter -> not (equal none parameter))
      |> List.map ~f:extract_optional_parameter
      |> normalize
      |> (fun union -> Optional union)
    else
      normalize parameters


let variable ?constraints ?variance name =
  Variable (Record.Variable.create ?constraints ?variance name)


let yield parameter =
  Parametric {
    name = "Yield";
    parameters = [parameter];
  }


let parametric_substitution_map =
  [
    "typing.ChainMap", "collections.ChainMap";
    "typing.Counter", "collections.Counter";
    "typing.DefaultDict", "collections.defaultdict";
    "typing.Deque", "collections.deque";
    "typing.Dict", "dict";
    "typing.FrozenSet", "frozenset";
    "typing.List", "list";
    "typing.Set", "set";
    "typing.Type", "type";
    "typing_extensions.Protocol", "typing.Protocol";
  ]
  |> Identifier.Table.of_alist_exn


let rec expression ?(convert = false) annotation =
  let location = Location.Reference.any in

  let create_name name = Name (Expression.create_name ~location name) in

  let get_item_call base arguments =
    let arguments =
      if List.length arguments > 1 then
        Expression.Tuple arguments
        |> Node.create_with_default_location
        |> (fun tuple -> [{ Call.Argument.name = None; value = tuple }])
      else
        let create argument = { Call.Argument.name = None; value = argument } in
        List.map ~f:create arguments
    in
    Call {
      callee = {
        Node.location;
        value = create_name (base ^ ".__getitem__");
      };
      arguments;
    }
  in

  let convert_annotation annotation =
    match annotation with
    | Bottom -> create_name "$bottom"
    | Callable { implementation; overloads; _ } ->
        let convert_signature { annotation; parameters } =
          let parameters =
            match parameters with
            | Defined parameters ->
                let convert_parameter parameter =
                  let call ?(default = false) name argument annotation =
                    let arguments =
                      let annotation =
                        annotation
                        >>| (fun annotation ->
                            [{
                              Call.Argument.name = None;
                              value = expression ~convert annotation;
                            }])
                        |> Option.value ~default:[]
                      in
                      let default =
                        if default then
                          [{
                            Call.Argument.name = None;
                            value = Node.create ~location (create_name "default");
                          }]
                        else
                          []
                      in
                      [{
                        Call.Argument.name = None;
                        value = Node.create ~location (create_name argument);
                      }]
                      @ annotation @ default
                    in
                    Call {
                      callee = Node.create ~location (Name (Name.Identifier name));
                      arguments;
                    }
                    |> Node.create ~location
                  in
                  match parameter with
                  | Parameter.Keywords { Parameter.name; annotation; _ } ->
                      call "Keywords" name (Some annotation)
                  | Parameter.Named { Parameter.name; annotation; default } ->
                      call "Named" ~default name (Some annotation)
                  | Parameter.Variable { Parameter.name; annotation; _ } ->
                      call "Variable" name (Some annotation)
                in
                List (List.map ~f:convert_parameter parameters)
                |> Node.create ~location
            | Undefined ->
                Node.create ~location Ellipsis
          in
          {
            Call.Argument.name = None;
            value =
              Node.create
                ~location
                (Expression.Tuple [parameters; expression ~convert annotation])
          }
        in
        let base_callable =
          Call {
            callee = {
              Node.location;
              value = create_name "typing.Callable.__getitem__"
            };
            arguments = [convert_signature implementation];
          };
        in
        let overloads =
          let convert_overload sofar overload =
            match sofar with
            | None ->
                Call {
                  callee = {
                    Node.location;
                    value = Name (Name.Identifier "__getitem__");
                  };
                  arguments = [convert_signature overload];
                }
                |> Node.create ~location
                |> Option.some
            | Some expression ->
                Call {
                  callee = {
                    Node.location;
                    value = Name (Name.Attribute {
                        base = expression;
                        attribute = "__getitem__";
                      });
                  };
                  arguments = [convert_signature overload];
                }
                |> Node.create ~location
                |> Option.some
          in
          List.fold ~init:None ~f:convert_overload overloads
        in
        begin
          match overloads with
          | Some overloads ->
              Call {
                callee = {
                  Node.location;
                  value = Name (Name.Attribute {
                      base = {
                        Node.location;
                        value = base_callable
                      };
                      attribute = "__getitem__"
                    });
                };
                arguments = [{ Call.Argument.name = None; value = overloads }];
              }
          | None ->
              base_callable
        end
    | Any -> create_name "typing.Any"
    | Literal literal ->
        let literal =
          match literal with
          | Boolean true ->
              Expression.True
          | Boolean false ->
              Expression.False
          | Integer literal ->
              Expression.Integer literal
          | String literal ->
              Expression.String { value = literal; kind = StringLiteral.String }
        in
        get_item_call "typing_extensions.Literal" [Node.create ~location literal]
    | Optional Bottom ->
        create_name "None"
    | Optional parameter ->
        get_item_call "typing.Optional" [expression ~convert parameter]
    | Parametric { name = "typing.Optional"; parameters = [Bottom] } ->
        create_name "None"
    | Parametric { name; parameters } ->
        get_item_call (reverse_substitute name) (List.map ~f:(expression ~convert) parameters)
    | Primitive name ->
        create_name name
    | Top -> create_name "$unknown"
    | Tuple (Bounded []) ->
        get_item_call "typing.Tuple" [Node.create ~location (Expression.Tuple [])]
    | Tuple elements ->
        let parameters =
          match elements with
          | Bounded parameters -> parameters
          | Unbounded parameter -> [parameter; Primitive "..."]
        in
        get_item_call "typing.Tuple" (List.map ~f:(expression ~convert) parameters)
    | TypedDictionary { name; fields; total } ->
        let argument =
          let tail =
            let field_to_tuple { name; annotation } =
              Node.create_with_default_location (Expression.Tuple [
                  Node.create_with_default_location (Expression.String {
                      value = name;
                      kind = StringLiteral.String;
                    });
                  expression ~convert annotation;
                ])
            in
            List.map fields ~f:field_to_tuple
          in
          let totality =
            (if total then Expression.True else Expression.False)
            |> Node.create_with_default_location
          in
          Expression.String { value = name; kind = StringLiteral.String }
          |> Node.create_with_default_location
          |> (fun name -> Expression.Tuple(name :: totality :: tail))
          |> Node.create_with_default_location
        in
        get_item_call "mypy_extensions.TypedDict" [argument]
    | Union parameters ->
        get_item_call "typing.Union" (List.map ~f:(expression ~convert) parameters)
    | Variable { variable; _ } ->
        create_name variable
  in

  let value =
    match annotation with
    | Primitive "..." -> Ellipsis
    | _ -> convert_annotation annotation
  in
  if convert then
    Expression.convert (Node.create_with_default_location value)
  else
    Node.create_with_default_location value


let access annotation =
  match expression ~convert:true annotation with
  | { Node.value = Access (SimpleAccess access); _ } -> access
  | _ -> failwith "Annotation expression is not an access"


module Transform = struct
  type 'state visit_result =
    { transformed_annotation: t; new_state: 'state }
  module type Transformer = sig
    type state
    val visit: state -> t -> state visit_result
    val visit_children_before: state -> t -> bool
    val visit_children_after: bool
  end
  module Make (Transformer: Transformer) = struct
    let rec visit_annotation ~state annotation =
      let visit_children annotation =
        match annotation with
        | Callable ({ implementation; overloads; _ } as callable) ->
            let open Record.Callable in
            let visit_overload { annotation; parameters } =
              let visit_parameters parameter =
                let visit_named ({ RecordParameter.annotation; _ } as named) =
                  { named with annotation = visit_annotation annotation ~state }
                in
                let visit_defined = function
                  | RecordParameter.Named named ->
                      RecordParameter.Named (visit_named named)
                  | RecordParameter.Variable named ->
                      RecordParameter.Variable (visit_named named)
                  | RecordParameter.Keywords named ->
                      RecordParameter.Keywords (visit_named named)
                in
                match parameter with
                | Defined defined ->
                    Defined (List.map defined ~f:visit_defined)
                | Undefined ->
                    Undefined
              in
              {
                annotation = visit_annotation annotation ~state;
                parameters = visit_parameters parameters;
              }
            in
            Callable {
              callable with
              implementation = visit_overload implementation;
              overloads = List.map overloads ~f:visit_overload;
            }

        | Optional annotation ->
            optional (visit_annotation annotation ~state)

        | Parametric { name; parameters } ->
            Parametric { name; parameters = List.map parameters ~f:(visit_annotation ~state) }

        | Tuple (Bounded annotations) ->
            Tuple (Bounded (List.map annotations ~f:(visit_annotation ~state)))
        | Tuple (Unbounded annotation) ->
            Tuple (Unbounded (visit_annotation annotation ~state))

        | TypedDictionary ({ fields; _ } as typed_dictionary) ->
            let visit_field ({ annotation; _ } as field) =
              { field with annotation = visit_annotation annotation ~state}
            in
            TypedDictionary { typed_dictionary with fields = List.map fields ~f:visit_field}

        | Union annotations ->
            union (List.map annotations ~f:(visit_annotation ~state))

        | Variable ({ constraints; _ } as variable) ->
            let constraints =
              match constraints with
              | Record.Variable.Bound bound ->
                  Record.Variable.Bound (visit_annotation bound ~state)
              | Explicit constraints ->
                  Explicit (List.map constraints ~f:(visit_annotation ~state))
              | Unconstrained ->
                  Unconstrained
              | LiteralIntegers ->
                  LiteralIntegers
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
      let { transformed_annotation; new_state } =
        Transformer.visit !state annotation
      in
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
  let module ExistsTransform = Transform.Make(struct
      type state = bool

      let visit_children_before _ _ =
        true

      let visit_children_after =
        false

      let visit sofar annotation =
        let new_state = sofar || predicate annotation in
        { Transform.transformed_annotation = annotation; new_state }
    end)
  in
  fst (ExistsTransform.visit false annotation)


let is_unknown annotation =
  exists annotation ~predicate:(function | Top -> true | _ -> false)


module Callable = struct
  module Parameter = struct
    include Record.Callable.RecordParameter

    type parameter = type_t t
    [@@deriving compare, eq, sexp, show, hash]

    module Map = Core.Map.Make(struct
        type nonrec t = parameter
        let compare = compare type_compare
        let sexp_of_t = sexp_of_t type_sexp_of_t
        let t_of_sexp = t_of_sexp type_t_of_sexp
      end)

    let create ?(annotation=Any) ?(default=false) name =
      let star, name = Identifier.split_star name in
      let named = { name; annotation; default } in
      match star with
      | "**" -> Keywords named
      | "*" -> Variable named
      | _ -> Named named

    let name = function
      | Named { name; _ } -> name
      | Variable { name; _ } -> ("*" ^ name)
      | Keywords { name; _ } -> ("**" ^ name)


    let annotation = function
      | Named { annotation; _ }
      | Variable { annotation; _ }
      | Keywords { annotation; _ } ->
          annotation


    let default = function
      | Named { default; _ }
      | Variable { default; _ }
      | Keywords { default; _ } ->
          default


    let is_anonymous = function
      | Named { name; _ } when String.is_prefix ~prefix:"$" name ->
          true
      | _ ->
          false


    let names_compatible left right =
      match left, right with
      | Named { name = left; _ }, Named { name = right; _ }
      | Variable { name = left; _ }, Variable { name = right; _ }
      | Keywords { name = left; _ }, Keywords { name = right; _ } ->
          let left = Identifier.sanitized left in
          let right = Identifier.sanitized right in
          if String.is_prefix ~prefix:"$" left ||
             String.is_prefix ~prefix:"$" right then
            true
          else
            let left = Identifier.remove_leading_underscores left in
            let right = Identifier.remove_leading_underscores right in
            Identifier.equal left right
      | _ ->
          false
  end


  include Record.Callable

  type implicit = type_t Record.Callable.implicit_record
  [@@deriving compare, eq, sexp, show, hash]

  type t = type_t Record.Callable.record
  [@@deriving compare, eq, sexp, show, hash]


  module Overload = struct
    let parameters { parameters; _ } =
      match parameters with
      | Defined parameters -> Some parameters
      | Undefined -> None

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
          | Some sofar, { kind; implementation; overloads; implicit } ->
              if equal_kind kind sofar.kind then
                Some {
                  kind;
                  implementation;
                  overloads = sofar.overloads @ overloads;
                  implicit
                }
              else
                None
          | _ ->
              None
        in
        List.fold ~init:(Some initial) ~f:fold overloads
    | _ ->
        None

  let map callable ~f =
    Callable callable
    |> f
    |> (function | Callable callable -> Some callable | _ -> None)


  let map_implementation implementation ~f =
    map { kind = Anonymous; implementation; overloads = []; implicit = None } ~f
    |> (function
        | Some { implementation; _ } -> implementation
        | _ -> failwith "f did not return a callable")


  let with_return_annotation ({ implementation; overloads; _ } as initial) ~annotation =
    let re_annotate implementation = { implementation with annotation } in
    {
      initial with
      implementation = re_annotate implementation;
      overloads = List.map ~f:re_annotate overloads
    }

  let create
      ?name
      ?(overloads = [])
      ?(parameters = Undefined)
      ?implicit
      ~annotation
      () =
    let kind = name >>| (fun name -> Named name) |> Option.value ~default:Anonymous in
    Callable {
      kind;
      implementation = { annotation; parameters };
      overloads;
      implicit;
    }


  let create_from_implementation implementation =
    create ~parameters:implementation.parameters ~annotation:implementation.annotation ()
end


let primitive_substitution_map =
  let parametric_anys name number_of_anys =
    let rec parameters sofar remaining =
      match remaining with
      | 0 -> sofar
      | _ -> parameters (Any :: sofar) (remaining - 1)
    in
    Parametric { name; parameters = (parameters [] number_of_anys) }
  in
  [
    "$bottom", Bottom;
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
    "typing_extensions.Protocol", Primitive "typing.Protocol";
  ]
  |> Identifier.Table.of_alist_exn


let rec create_logic ?(use_cache=true) ~aliases { Node.value = expression; _ } =
  match Cache.find expression with
  | Some result when use_cache ->
      result
  | _ ->
      let parse_access type_access =
        let resolved =
          let resolve_aliases annotation =
            let module ResolveTransform = Transform.Make(struct
                type state = Set.t

                let visit_children_before _ _ =
                  false

                let visit_children_after =
                  true

                let rec visit visited annotation =
                  let rec resolve visited annotation =
                    if Set.mem visited annotation then
                      visited, annotation
                    else
                      let visited = Set.add visited annotation in
                      match aliases annotation, annotation with
                      | Some aliased, _ ->
                          (* We need to fully resolve aliases to aliases before we go on to resolve
                             the aliases those may contain *)
                          resolve visited aliased
                      | None, Parametric { name; parameters } ->
                          let visited, annotation = resolve visited (Primitive name) in
                          visited,
                          begin
                            match annotation with
                            | Primitive name ->
                                parametric name parameters
                            | Parametric { name; _ } ->
                                (* Ignore parameters for now. *)
                                parametric name parameters
                            | Union elements ->
                                let replace_parameters = function
                                  | Parametric { name; _ } -> parametric name parameters
                                  | annotation -> annotation
                                in
                                Union (List.map elements ~f:replace_parameters)
                            | _ ->
                                (* This should probably error or something *)
                                parametric name parameters
                          end
                      | _ ->
                          visited, annotation
                  in
                  let new_state, transformed_annotation = resolve visited annotation in
                  { Transform.transformed_annotation; new_state }
              end)
            in
            snd (ResolveTransform.visit Set.empty annotation)
          in

          let type_access_fold type_access =
            let fold accumulator current =
              match current, accumulator with
              | _, Some Top ->
                  Some Top
              | Access.Identifier current, _ ->
                  let current =
                    Identifier.sanitized current
                    |> fun name -> Access.Identifier name
                  in
                  let state =
                    match accumulator with
                    | Some accumulator ->
                        (access accumulator) @ [current]
                    | None ->
                        [current]
                  in
                  let name = Access.show state in
                  Some (resolve_aliases (Primitive name))
              | _, _ ->
                  Some Top
            in
            List.fold type_access ~f:fold ~init:None
            |> Option.value ~default:Top
          in
          let rec handle_access reversed_lead tail =
            match tail with
            |  (Access.Identifier "__getitem__")
               :: (Access.Call { Node.value = [{ Argument.value = argument; _ }]; _ })
               :: _ ->
                begin
                  let parameters =
                    match Node.value argument with
                    | Expression.Tuple elements -> elements
                    | _ -> [argument]
                  in
                  let parametric name =
                    let parameters = List.map parameters ~f:(create_logic ~use_cache ~aliases) in
                    Parametric { name; parameters }
                    |> resolve_aliases
                  in
                  match type_access_fold (List.rev reversed_lead) with
                  | Primitive name ->
                      parametric name
                  | Top ->
                      Top
                  | _ ->
                      List.rev reversed_lead
                      |> Access.show
                      |> parametric

                end
            | [] ->
                type_access_fold type_access
            | head :: tail ->
                handle_access (head :: reversed_lead) tail
          in
          handle_access [] type_access
        in
        (* Substitutions. *)
        match resolved with
        | Primitive name ->
            begin
              match Identifier.Table.find primitive_substitution_map name with
              | Some substitute -> substitute
              | None -> resolved
            end
        | Parametric { name; parameters } ->
            begin
              match Identifier.Table.find parametric_substitution_map name with
              | Some name ->
                  Parametric { name; parameters }
              | None ->
                  begin
                    match name with
                    | "typing.Optional" when List.length parameters = 1 ->
                        optional (List.hd_exn parameters)

                    | "tuple"
                    | "typing.Tuple" ->
                        let tuple: tuple =
                          match parameters with
                          | [parameter; Primitive "..."] ->
                              Unbounded parameter
                          | _ ->
                              Bounded parameters
                        in
                        Tuple tuple

                    | "typing.Union" ->
                        union parameters

                    | _ ->
                        resolved
                  end
            end
        | Union elements ->
            union elements
        | _ ->
            resolved
      in

      let result =
        let parse_callable ?modifiers ~(signatures: Access.t) () =
          let kind =
            match modifiers with
            | Some ({
                Argument.value = { Node.value = Expression.String { StringLiteral.value; _ }; _ };
                _;
              } :: _) ->
                Named (Reference.create value)
            | _ ->
                Anonymous
          in
          let implementation, overloads =
            let undefined = { annotation = Top; parameters = Undefined } in
            let get_signature argument =
              match Node.value argument with
              | Expression.Tuple [parameters; annotation] ->
                  let parameters =
                    let extract_parameter index parameter =
                      match Node.value parameter with
                      | Access
                          (SimpleAccess
                             [
                               Access.Identifier name;
                               Access.Call { Node.value = arguments; _ };
                             ]) ->
                          begin
                            let arguments =
                              List.map
                                arguments
                                ~f:(fun { Argument.value; _ } -> value)
                            in
                            match name, arguments with
                            | "Named",
                              { Node.value = Access (SimpleAccess [Access.Identifier name]); _ }
                              :: annotation
                              :: tail ->
                                let default =
                                  match tail with
                                  | [{
                                      Node.value =
                                        Access
                                          (SimpleAccess [Access.Identifier "default"]);
                                      _;
                                    }] ->
                                      true
                                  | _ ->
                                      false
                                in
                                Parameter.Named {
                                  Parameter.name;
                                  annotation = create_logic ~use_cache ~aliases annotation;
                                  default;
                                }
                            | "Variable",
                              { Node.value = Access (SimpleAccess [Access.Identifier name]); _ }
                              :: tail ->
                                let annotation =
                                  match tail with
                                  | annotation :: _ -> create_logic ~use_cache ~aliases annotation
                                  | _ -> Top
                                in
                                Parameter.Variable {
                                  Parameter.name;
                                  annotation;
                                  default = false;
                                }
                            | "Keywords",
                              { Node.value = Access (SimpleAccess [Access.Identifier name]); _ }
                              :: tail ->
                                let annotation =
                                  match tail with
                                  | annotation :: _ -> create_logic ~use_cache ~aliases annotation
                                  | _ -> Top
                                in
                                Parameter.Keywords {
                                  Parameter.name;
                                  annotation;
                                  default = false;
                                }
                            | _ ->
                                Parameter.Named {
                                  Parameter.name = "$" ^ Int.to_string index;
                                  annotation = Top;
                                  default = false;
                                }
                          end
                      | _ ->
                          Parameter.Named {
                            Parameter.name = "$" ^ Int.to_string index;
                            annotation = create_logic ~use_cache ~aliases parameter;
                            default = false;
                          }
                    in
                    match Node.value parameters with
                    | List parameters ->
                        Defined (List.mapi ~f:extract_parameter parameters)
                    | _ ->
                        Undefined
                  in
                  { annotation = create_logic ~use_cache ~aliases annotation; parameters }
              | _ ->
                  undefined
            in
            match signatures with
            | (Access.Identifier "__getitem__") ::
              (Access.Call { Node.value = [{ Argument.value = argument; _ }]; _ }) ::
              [] ->
                get_signature argument, []
            | (Access.Identifier "__getitem__") ::
              (Access.Call { Node.value = [{ Argument.value = argument; _ }]; _ }) ::
              (Access.Identifier "__getitem__") ::
              (Access.Call {
                  Node.value = [
                    { Argument.value = { Node.value = overloads_argument; location }; _ }
                  ];
                  _;
                }) ::
              [] ->
                let rec parse_overloads overloads =
                  match overloads with
                  | Expression.List arguments ->
                      [get_signature (Node.create ~location (Expression.Tuple arguments))]
                  | Expression.Access
                      (ExpressionAccess {
                          expression = { Node.value = (Expression.List arguments); _ };
                          access = tail;
                        }) ->
                      get_signature (Node.create ~location (Expression.Tuple arguments))
                      :: (parse_overloads (Access (SimpleAccess tail)))
                  | Expression.Access
                      (SimpleAccess (
                          Access.Identifier "__getitem__"
                          :: Access.Call { Node.value = [{ Argument.value = argument; _ }]; _ }
                          :: tail
                        )) ->
                      get_signature argument :: (parse_overloads (Access (SimpleAccess tail)))
                  | Access (SimpleAccess []) ->
                      []
                  | _ ->
                      [undefined]
                in
                get_signature argument, parse_overloads overloads_argument
            | _ ->
                undefined, []
          in
          Callable { kind; implementation; overloads; implicit = None }
        in
        match expression with
        | Access
            (SimpleAccess [
                Access.Identifier "typing";
                Access.Identifier "TypeVar";
                Access.Call ({
                    Node.value = {
                      Argument.value = { Node.value = String { StringLiteral.value; _ }; _ };
                      _;
                    } :: arguments;
                    _;
                  });
              ]) ->
            let constraints =
              let explicits =
                let explicit = function
                  | { Argument.value; Argument.name = None } ->
                      create_logic ~use_cache ~aliases value
                      |> Option.some
                  | _ ->
                      None
                in
                List.filter_map ~f:explicit arguments
              in
              let bound =
                let bound = function
                  | { Argument.value; Argument.name = Some { Node.value = bound; _ }; }
                    when String.equal (Identifier.sanitized bound) "bound" ->
                      create_logic ~use_cache ~aliases value
                      |> Option.some
                  | _ ->
                      None
                in
                List.find_map ~f:bound arguments
              in
              if not (List.is_empty explicits) then
                Record.Variable.Explicit explicits
              else if Option.is_some bound then
                Bound (Option.value_exn bound)
              else
                Unconstrained
            in
            let variance =
              let variance_definition = function
                | {
                  Argument.name = Some { Node.value = name; _ };
                  Argument.value = { Node.value = True; _ };
                } when String.equal (Identifier.sanitized name) "covariant" ->
                    Some Record.Variable.Covariant
                | {
                  Argument.name = Some { Node.value = name; _ };
                  Argument.value = { Node.value = True; _ };
                } when String.equal (Identifier.sanitized name) "contravariant" ->
                    Some Contravariant
                | _ ->
                    None
              in
              List.find_map arguments ~f:variance_definition
              |> Option.value ~default:Record.Variable.Invariant
            in
            variable value ~constraints ~variance

        | Access
            (SimpleAccess [
                Access.Identifier "pyre_check";
                Access.Identifier "extensions";
                Access.Identifier "IntVar";
                Access.Call ({
                    Node.value = {
                      Argument.value = { Node.value = String { StringLiteral.value; _ }; _ };
                      _;
                    } :: [];
                    _;
                  });
              ]) ->
            variable value ~constraints:LiteralIntegers

        | Access
            (SimpleAccess
               ((Access.Identifier "typing")
                :: (Access.Identifier "Callable")
                :: (Access.Call { Node.value = modifiers; _ })
                :: signatures)) ->
            parse_callable ~modifiers ~signatures ()
        | Access
            (SimpleAccess
               (((Access.Identifier "typing")
                 :: (Access.Identifier "Callable")
                 :: []) as access)) ->
            parse_access access
        | Access
            (SimpleAccess
               ((Access.Identifier "typing") :: (Access.Identifier "Callable") :: signatures)) ->
            parse_callable ~signatures ()

        | Access
            (SimpleAccess
               ([
                 Access.Identifier "mypy_extensions";
                 Access.Identifier "TypedDict";
                 Access.Identifier "__getitem__";
                 Access.Call({
                     Node.value = [{
                         Argument.name = None;
                         value = {
                           Node.value = Expression.Tuple (
                               {
                                 Node.value =
                                   Expression.String { value = typed_dictionary_name; _ };
                                 _;
                               } ::
                               {
                                 Node.value = true_or_false;
                                 _;
                               } ::
                               fields);
                           _;
                         };
                       }];
                     _;
                   });
               ] as access)) ->
            let total =
              match true_or_false with
              | Expression.True -> Some true
              | Expression.False -> Some false
              | _ -> None
            in
            let parse_typed_dictionary total =
              let fields =
                let tuple_to_field =
                  function
                  | {
                    Node.value = Expression.Tuple [
                        { Node.value = Expression.String { value = field_name; _ }; _ };
                        field_annotation;
                      ];
                    _;
                  } ->
                      Some {
                        name = field_name;
                        annotation = create_logic ~use_cache ~aliases field_annotation;
                      }
                  | _ ->
                      None
                in
                fields
                |> List.filter_map ~f:tuple_to_field
              in
              TypedDictionary {
                name = typed_dictionary_name;
                fields;
                total;
              }
            in
            total
            >>| parse_typed_dictionary
            |> Option.value ~default:(parse_access access)

        | Access
            (SimpleAccess [
                Access.Identifier "typing_extensions";
                Access.Identifier "Literal";
                Access.Identifier "__getitem__";
                Access.Call ({
                    Node.value = arguments;
                    _;
                  });
              ]) ->
            let arguments =
              match arguments with
              | [{ Argument.name = None; value = { Node.value = Expression.Tuple arguments; _ } }]
                ->
                  List.map arguments ~f:Node.value
                  |> Option.some
              | [{ Argument.name = None; value = { Node.value = argument; _ } }] ->
                  Some [argument]
              | _ ->
                  None
            in
            let parse = function
              | Expression.Integer literal ->
                  literal_integer literal
                  |> Option.some
              | Expression.String { StringLiteral.kind = StringLiteral.String; value } ->
                  literal_string value
                  |> Option.some
              | _ ->
                  None
            in
            arguments
            >>| List.map ~f:parse
            >>= Option.all
            >>| union
            |> Option.value ~default:Top

        | Access (SimpleAccess access) ->
            parse_access access

        | Ellipsis ->
            Primitive "..."

        | String { StringLiteral.value; _ } ->
            let access =
              try
                let parsed =
                  Parser.parse [value]
                  |> Source.create
                  |> Preprocessing.preprocess
                  |> Preprocessing.convert
                  |> Source.statements
                in
                match parsed with
                | [{
                    Node.value =
                      Statement.Expression { Node.value = Access (SimpleAccess access); _ };
                    _;
                  }] ->
                    access
                | _ ->
                    Access.create value
              with _ ->
                Access.create value
            in
            parse_access access

        | _ ->
            Top
      in
      if use_cache then
        Cache.set ~key:expression ~data:result;
      result


let rec create_logic_new ?(use_cache=true) ~aliases { Node.value = expression; _ } =
  match Cache.find expression with
  | Some result when use_cache ->
      result
  | _ ->
      let result =
        let result =
          let create_logic = create_logic_new ~use_cache ~aliases in
          let resolve_aliases annotation =
            let visited = Hash_set.create () in
            let module ResolveTransform = Transform.Make(struct
                type state = unit

                let visit_children_before _ _ =
                  false

                let visit_children_after =
                  true

                let rec visit _ annotation =
                  let rec resolve annotation =
                    if Core.Hash_set.mem visited annotation then
                      annotation
                    else
                      begin
                        Core.Hash_set.add visited annotation;
                        match aliases annotation, annotation with
                        | Some aliased, _ ->
                            (* We need to fully resolve aliases to aliases before we go on to
                               resolve the aliases those may contain *)
                            resolve aliased
                        | None, Parametric { name; parameters } ->
                            let annotation = resolve (Primitive name) in
                            begin
                              match annotation with
                              | Primitive name ->
                                  parametric name parameters
                              | Parametric { name; _ } ->
                                  (* Ignore parameters for now. *)
                                  parametric name parameters
                              | Union elements ->
                                  let replace_parameters = function
                                    | Parametric { name; _ } -> parametric name parameters
                                    | annotation -> annotation
                                  in
                                  Union (List.map elements ~f:replace_parameters)
                              | _ ->
                                  (* This should probably error or something *)
                                  parametric name parameters
                            end
                        | _ ->
                            annotation
                      end
                  in
                  let transformed_annotation = resolve annotation in
                  { Transform.transformed_annotation; new_state = () }
              end)
            in
            snd (ResolveTransform.visit () annotation)
          in
          let rec is_typing_callable = function
            | Name (
                Name.Attribute {
                  base = { Node.value = Name (Name.Identifier "typing"); _ };
                  attribute = "Callable";
                }
              ) ->
                true
            | Name (Name.Attribute { base; _ } ) ->
                is_typing_callable (Node.value base)
            | Call { callee; _ } ->
                is_typing_callable (Node.value callee)
            | _ ->
                false
          in
          let parse_callable expression =
            let modifiers, implementation_signature, overload_signatures =
              let get_from_base base implementation_argument overloads_argument =
                match Node.value base with
                | Call {
                    callee = {
                      Node.value = Name (
                          Name.Attribute {
                            base = { Node.value = Name (Name.Identifier "typing"); _ };
                            attribute = "Callable";
                          }
                        );
                      _;
                    };
                    arguments;
                  } ->
                    Some arguments, implementation_argument, overloads_argument
                | Name (
                    Name.Attribute {
                      base = { Node.value = Name (Name.Identifier "typing"); _ };
                      attribute = "Callable";
                    }
                  ) ->
                    None, implementation_argument, overloads_argument
                | _ ->
                    (* Invalid base. *)
                    None, None, None
              in
              match expression with
              | Call {
                  callee = {
                    Node.value = Name (
                        Name.Attribute {
                          base = {
                            Node.value = Call {
                                callee = {
                                  Node.value = Name (
                                      Name.Attribute { base; attribute = "__getitem__" }
                                    );
                                  _;
                                };
                                arguments = [{ Call.Argument.value = argument; _ }];
                              };
                            _;
                          };
                          attribute = "__getitem__";
                        });
                    _;
                  };
                  arguments = [{ Call.Argument.value = overloads_argument; _ }];
                } ->
                  (* Overloads are provided *)
                  get_from_base base (Some argument) (Some overloads_argument)
              | Call {
                  callee = {
                    Node.value = Name (Name.Attribute { base; attribute = "__getitem__" });
                    _;
                  };
                  arguments = [{ Call.Argument.value = argument; _ }];
                } ->
                  (* No overloads provided *)
                  get_from_base base (Some argument) None
              | _ ->
                  None, None, None
            in
            let kind =
              match modifiers with
              | Some ({
                  Call.Argument.value = {
                    Node.value = Expression.String { StringLiteral.value; _ };
                    _;
                  };
                  _;
                } :: _) ->
                  Named (Reference.create value)
              | _ ->
                  Anonymous
            in
            let undefined = { annotation = Top; parameters = Undefined } in
            let get_signature = function
              | Expression.Tuple [parameters; annotation] ->
                  let parameters =
                    let extract_parameter index parameter =
                      match Node.value parameter with
                      | Call {
                          callee = { Node.value = Name (Name.Identifier name); _ };
                          arguments;
                        } ->
                          begin
                            let arguments =
                              List.map
                                arguments
                                ~f:(fun { Call.Argument.value; _ } -> Node.value value)
                            in
                            match name, arguments with
                            | "Named", Name (Name.Identifier name) :: annotation :: tail ->
                                let default =
                                  match tail with
                                  | [Name (Name.Identifier "default")] -> true
                                  | _ -> false
                                in
                                Parameter.Named {
                                  Parameter.name;
                                  annotation =
                                    create_logic (Node.create_with_default_location annotation);
                                  default;
                                }
                            | "Variable", Name (Name.Identifier name) :: tail ->
                                let annotation =
                                  match tail with
                                  | annotation :: _ ->
                                      create_logic (Node.create_with_default_location annotation);
                                  | _ ->
                                      Top
                                in
                                Parameter.Variable {
                                  Parameter.name;
                                  annotation;
                                  default = false;
                                }
                            | "Keywords", Name (Name.Identifier name) :: tail ->
                                let annotation =
                                  match tail with
                                  | annotation :: _ ->
                                      create_logic (Node.create_with_default_location annotation);
                                  | _ ->
                                      Top
                                in
                                Parameter.Keywords {
                                  Parameter.name;
                                  annotation;
                                  default = false;
                                }
                            | _ ->
                                Parameter.Named {
                                  Parameter.name = "$" ^ Int.to_string index;
                                  annotation = Top;
                                  default = false;
                                }
                          end
                      | _ ->
                          Parameter.Named {
                            Parameter.name = "$" ^ Int.to_string index;
                            annotation = create_logic parameter;
                            default = false;
                          }
                    in
                    match Node.value parameters with
                    | List parameters ->
                        Defined (List.mapi ~f:extract_parameter parameters)
                    | _ ->
                        Undefined
                  in
                  { annotation = create_logic annotation; parameters }
              | _ ->
                  undefined
            in
            let implementation =
              match implementation_signature with
              | Some signature -> get_signature (Node.value signature)
              | None -> undefined
            in
            let overloads =
              let rec parse_overloads = function
                | List arguments ->
                    [get_signature (Tuple arguments)]
                | Call {
                    callee = {
                      Node.value = Name (
                          Name.Attribute {
                            base;
                            attribute = "__getitem__";
                          });
                      _;
                    };
                    arguments = [{ Call.Argument.value = argument; _ }]
                  } ->
                    get_signature (Node.value argument) :: parse_overloads (Node.value base)
                | _ ->
                    [undefined]
              in
              match overload_signatures with
              | Some signatures -> List.rev (parse_overloads (Node.value signatures))
              | None -> []
            in
            Callable { kind; implementation; overloads; implicit = None }
          in
          match expression with
          | Call {
              callee = {
                Node.value = Name (
                    Name.Attribute {
                      base = { Node.value = Name (Name.Identifier "typing") ; _ };
                      attribute = "TypeVar";
                    });
                _;
              };
              arguments = {
                Call.Argument.value = { Node.value = String { StringLiteral.value; _ }; _ };
                _;
              } :: arguments;
            } ->
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
                    | { Call.Argument.value; name = Some { Node.value = bound; _ }; }
                      when String.equal (Identifier.sanitized bound) "bound" ->
                        Some (create_logic value)
                    | _ ->
                        None
                  in
                  List.find_map ~f:bound arguments
                in
                if not (List.is_empty explicits) then
                  Record.Variable.Explicit explicits
                else if Option.is_some bound then
                  Bound (Option.value_exn bound)
                else
                  Unconstrained
              in
              let variance =
                let variance_definition = function
                  | {
                    Call.Argument.name = Some { Node.value = name; _ };
                    value = { Node.value = True; _ };
                  } when String.equal (Identifier.sanitized name) "covariant" ->
                      Some Record.Variable.Covariant
                  | {
                    Call.Argument.name = Some { Node.value = name; _ };
                    value = { Node.value = True; _ };
                  } when String.equal (Identifier.sanitized name) "contravariant" ->
                      Some Contravariant
                  | _ ->
                      None
                in
                List.find_map arguments ~f:variance_definition
                |> Option.value ~default:Record.Variable.Invariant
              in
              variable value ~constraints ~variance
          | Call {
              callee = {
                Node.value = Name (
                    Name.Attribute {
                      base = {
                        Node.value = Name (
                            Name.Attribute {
                              base = { Node.value = Name (Name.Identifier "pyre_check") ; _ };
                              attribute = "extensions";
                            }
                          );
                        _;
                      };
                      attribute = "IntVar";
                    });
                _;
              };
              arguments = [{
                  Call.Argument.value = { Node.value = String { StringLiteral.value; _ }; _ };
                  _;
                }];
            } ->
              variable value ~constraints:LiteralIntegers
          | Call {
              callee = {
                Node.value = Name (
                    Name.Attribute {
                      base = {
                        Node.value = Name (
                            Name.Attribute {
                              base = { Node.value = Name (Name.Identifier "mypy_extensions") ; _ };
                              attribute = "TypedDict";
                            }
                          );
                        _;
                      };
                      attribute = "__getitem__";
                    });
                _;
              };
              arguments = [{
                  Call.Argument.name = None;
                  value = {
                    Node.value = Expression.Tuple (
                        {
                          Node.value =
                            Expression.String { value = typed_dictionary_name; _ };
                          _;
                        } ::
                        {
                          Node.value = true_or_false;
                          _;
                        } ::
                        fields);
                    _;
                  };
                }];
            } ->
              let total =
                match true_or_false with
                | Expression.True -> Some true
                | Expression.False -> Some false
                | _ -> None
              in
              let parse_typed_dictionary total =
                let fields =
                  let tuple_to_field =
                    function
                    | {
                      Node.value = Expression.Tuple [
                          { Node.value = Expression.String { value = field_name; _ }; _ };
                          field_annotation;
                        ];
                      _;
                    } ->
                        Some { name = field_name; annotation = create_logic field_annotation }
                    | _ ->
                        None
                  in
                  fields
                  |> List.filter_map ~f:tuple_to_field
                in
                TypedDictionary {
                  name = typed_dictionary_name;
                  fields;
                  total;
                }
              in
              let undefined_primitive =
                Primitive (Expression.show (Node.create_with_default_location expression))
              in
              total
              >>| parse_typed_dictionary
              |> Option.value ~default:undefined_primitive
          | Call {
              callee = {
                Node.value = Name (
                    Name.Attribute {
                      base = {
                        Node.value = Name (
                            Name.Attribute {
                              base = { Node.value = Name (Name.Identifier "typing_extensions"); _ };
                              attribute = "Literal";
                            }
                          );
                        _;
                      };
                      attribute = "__getitem__";
                    });
                _;
              };
              arguments;
            } ->
              let arguments =
                match arguments with
                | [{
                    Call.Argument.name = None;
                    value = { Node.value = Expression.Tuple arguments; _ };
                  }] ->
                    Some (List.map arguments ~f:Node.value)
                | [{ Call.Argument.name = None; value = { Node.value = argument; _ } }] ->
                    Some [argument]
                | _ ->
                    None
              in
              let parse = function
                | Expression.Integer literal ->
                    Some (literal_integer literal)
                | Expression.String { StringLiteral.kind = StringLiteral.String; value } ->
                    Some (literal_string value)
                | _ ->
                    None
              in
              arguments
              >>| List.map ~f:parse
              >>= Option.all
              >>| union
              |> Option.value ~default:Top
          | Call { callee = { Node.value = callee; _ }; _ }
            when is_typing_callable callee ->
              parse_callable expression
          | Call {
              callee = {
                Node.value = Name (Name.Attribute { base; attribute = "__getitem__" });
                _;
              };
              arguments = [{ Call.Argument.value = argument; _ }];
            } ->
              let parametric name =
                let parameters =
                  let parameters =
                    match Node.value argument with
                    | Expression.Tuple elements -> elements
                    | _ -> [argument]
                  in
                  List.map parameters ~f:create_logic
                in
                Parametric { name; parameters }
                |> resolve_aliases
              in
              begin
                match create_logic base, Node.value base with
                | Primitive name, _ ->
                    parametric name
                | _, Name _ ->
                    parametric (Expression.show base)
                | _ ->
                    Top
              end
          | Name (Name.Identifier identifier) ->
              let sanitized = Identifier.sanitized identifier in
              if sanitized = "None" then
                none
              else
                Primitive sanitized
                |> resolve_aliases
          | Name (Name.Attribute { base; attribute }) ->
              let attribute = Identifier.sanitized attribute in
              begin
                match create_logic base with
                | Primitive primitive ->
                    Primitive (primitive ^ "." ^ attribute)
                    |> resolve_aliases
                | _ ->
                    Primitive (Expression.show base ^ "." ^ attribute)
              end
          | Ellipsis ->
              Primitive "..."
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
                with _ ->
                  None
              in
              expression
              >>| Node.create_with_default_location
              >>| create_logic
              |> Option.value ~default:(Primitive value)
          | _ ->
              Top
        in
        (* Substitutions. *)
        match result with
        | Primitive name ->
            begin
              match Identifier.Table.find primitive_substitution_map name with
              | Some substitute -> substitute
              | None -> result
            end
        | Parametric { name; parameters } ->
            begin
              match Identifier.Table.find parametric_substitution_map name with
              | Some name ->
                  Parametric { name; parameters }
              | None ->
                  begin
                    match name with
                    | "typing.Optional" when List.length parameters = 1 ->
                        optional (List.hd_exn parameters)
                    | "tuple"
                    | "typing.Tuple" ->
                        let tuple: tuple =
                          match parameters with
                          | [parameter; Primitive "..."] ->
                              Unbounded parameter
                          | _ ->
                              Bounded parameters
                        in
                        Tuple tuple
                    | "typing.Union" ->
                        union parameters
                    | _ ->
                        result
                  end
            end
        | Union elements ->
            union elements
        | _ ->
            result
      in
      if use_cache then
        Cache.set ~key:expression ~data:result;
      result


let create ?(convert = false) ~aliases =
  let aliases = function
    | Primitive name -> aliases name
    | _ -> None
  in
  if convert then
    create_logic ~use_cache:true ~aliases
  else
    create_logic_new ~use_cache:true ~aliases

let create_without_aliases =
  create_logic ~use_cache:false ~aliases:(fun _ -> None)

let contains_callable annotation =
  exists annotation ~predicate:(function | Callable _ -> true | _ -> false)


let is_callable = function
  | Callable _ -> true
  | _ -> false


let is_dictionary ?(with_key = None) = function
  | Parametric { name = "dict"; parameters } ->
      begin
        match with_key, parameters with
        | Some key, key_parameter :: [_] -> equal key key_parameter
        | _ -> true
      end
  | _ ->
      false


let is_ellipsis = function
  | Primitive "ellipsis" -> true
  | _ -> false


let is_final = function
  | Parametric { name = "typing.Final"; _ } -> true
  | _ -> false


let is_generator = function
  | Parametric { name = ("typing.Generator" | "typing.AsyncGenerator"); _ } ->
      true
  | _ ->
      false


let is_generic = function
  | Parametric { name = "typing.Generic"; _ } ->
      true
  | _ ->
      false


let is_iterable = function
  | Parametric { name = "typing.Iterable"; _ } ->
      true
  | _ ->
      false


let is_iterator = function
  | Parametric { name = "typing.Iterator"; _ } ->
      true
  | _ ->
      false


let is_async_iterator = function
  | Parametric { name = "typing.AsyncIterator"; _ } ->
      true
  | _ ->
      false


let is_meta = function
  | Parametric { name = "type"; _ } -> true
  | _ -> false


let is_none = function
  | Optional Bottom -> true
  | _ -> false


let is_noreturn = function
  | Primitive "typing.NoReturn" -> true
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


let is_protocol = function
  | Parametric { name = "typing.Protocol"; _ } ->
      true
  | _ ->
      false


let is_tuple (annotation: t) =
  match annotation with
  | Tuple _ -> true
  | _ -> false


let is_typed_dictionary = function
  | TypedDictionary _ -> true
  | _ -> false


let is_unbound = function
  | Bottom -> true
  | _ -> false


let contains_any annotation =
  exists annotation ~predicate:(function | Any -> true | _ -> false)


let expression_contains_any expression =
  (* Check if there is a literal Any provided, not including type aliases to Any. *)
  create_without_aliases expression
  |> contains_any


let is_type_alias annotation = equal annotation (Primitive "typing.TypeAlias")


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


let collect annotation ~predicate =
  let module CollectorTransform = Transform.Make(struct
      type state = t list

      let visit_children_before _ _ =
        true

      let visit_children_after =
        false

      let visit sofar annotation =
        let new_state = if predicate annotation then sofar @ [annotation] else sofar in
        { Transform.transformed_annotation = annotation; new_state }
    end)
  in
  fst (CollectorTransform.visit [] annotation)


let primitive_name = function
  | Primitive name -> Some name
  | _ -> None


let primitives annotation =
  let predicate = function | Primitive _ -> true | _ -> false in
  collect annotation ~predicate


let elements annotation =
  let module CollectorTransform = Transform.Make(struct
      type state = t list

      let visit_children_before _ _ =
        true

      let visit_children_after =
        false

      let visit sofar annotation =
        let new_state =
          match annotation with
          | Callable _ ->
              Primitive "typing.Callable" :: sofar
          | Literal _ ->
              Primitive "typing_extensions.Literal" :: sofar
          | Optional _ ->
              Primitive "typing.Optional" :: sofar
          | Parametric { name; _ } ->
              Primitive name :: sofar
          | Primitive _ ->
              annotation :: sofar
          | Tuple _ ->
              Primitive "tuple" :: sofar
          | TypedDictionary _ ->
              Primitive "TypedDictionary" :: sofar
          | Union _ ->
              Primitive "typing.Union" :: sofar
          | Bottom
          | Any
          | Top
          | Variable _ ->
              sofar
        in
        { Transform.transformed_annotation = annotation; new_state }
    end)
  in
  fst (CollectorTransform.visit [] annotation)
  |> List.rev


let is_partially_typed annotation =
  exists annotation ~predicate:(function | Any | Top | Bottom -> true | _ -> false)


let is_untyped = function
  | Any
  | Bottom
  | Top -> true
  | _ -> false


let optional_value = function
  | Optional annotation -> annotation
  | annotation -> annotation


let async_generator_value = function
  | Parametric { name = "typing.AsyncGenerator"; parameters = [parameter; _] } ->
      generator parameter
  | _ ->
      Top


let awaitable_value = function
  | Parametric { name = "typing.Awaitable"; parameters = [parameter] } ->
      parameter
  | _ ->
      Top


let coroutine_value = function
  | Parametric { name = "typing.Coroutine"; parameters = [_; _; parameter] } ->
      parameter
  | _ ->
      Top


let parameters = function
  | Parametric { parameters; _ } -> parameters
  | _ -> []


let single_parameter = function
  | Parametric { parameters = [parameter]; _ } -> parameter
  | _ -> failwith "Type does not have single parameter"


let instantiate ?(widen = false) annotation ~constraints =
  let module InstantiateTransform = Transform.Make(struct
      type state = unit

      let visit_children_before _ annotation =
        constraints annotation
        |>  Option.is_none

      let visit_children_after =
        false

      let visit _ annotation =
        let transformed_annotation =
          match constraints annotation with
          | Some Bottom when widen ->
              Top
          | Some replacement ->
              replacement
          | None ->
              annotation
        in
        { Transform.transformed_annotation; new_state = () }
    end)
  in
  snd (InstantiateTransform.visit () annotation)


let weaken_literals annotation =
  let constraints =
    function
    | Literal Integer _ -> Some integer
    | Literal String _ -> Some string
    | Literal Boolean _ -> Some bool
    | _ -> None
  in
  instantiate ~constraints annotation


let split = function
  | Optional parameter ->
      Primitive "typing.Optional", [parameter]
  | Parametric { name; parameters } ->
      Primitive name, parameters
  | Tuple tuple ->
      let parameters =
        match tuple with
        | Bounded parameters -> parameters
        | Unbounded parameter -> [parameter]
      in
      Primitive "tuple", parameters
  | TypedDictionary { total = true; _ } ->
      Primitive "TypedDictionary", []
  | TypedDictionary { total = false; _ } ->
      Primitive "NonTotalTypedDictionary", []
  | (Literal _ as literal) ->
      weaken_literals literal, []
  | annotation ->
      annotation, []


let class_name annotation =
  let open Expression in
  split annotation
  |> fst
  |> expression ~convert:true
  |> Node.value
  |> function
  | Access (SimpleAccess access) ->
      let rec remove_calls stripped = function
        | []
        | Access.Identifier _ :: Access.Call _ :: _ -> List.rev stripped
        | head :: tail -> remove_calls (head :: stripped) tail
      in
      Reference.from_access (remove_calls [] access)
  | _ ->
      Reference.create "typing.Any"


let class_variable annotation =
  parametric "typing.ClassVar" [annotation]


let class_variable_value = function
  | Parametric { name = "typing.ClassVar"; parameters = [parameter] } ->
      Some parameter
  | _ -> None


let final_value = function
  | Parametric { name = "typing.Final"; parameters = [parameter] } ->
      Some parameter
  | _ ->
      None


(* Angelic assumption: Any occurrences of top indicate that we're dealing with Any instead of None.
   See T22792667. *)
let assume_any = function
  | Top -> Any
  | annotation -> annotation

module Variable = struct
  module Namespace = struct
    include Record.Variable.RecordNamespace
    let fresh = ref 1

    let reset () =
      fresh := 1

    let create_fresh () =
      let namespace = !fresh in
      fresh := namespace + 1;
      namespace
  end
  include Record.Variable

  type t = type_t record
  [@@deriving compare, eq, sexp, show, hash]

  let is_contravariant = function
    | { variance = Contravariant; _ } -> true
    | _ -> false

  let is_covariant = function
    | { variance = Covariant; _ } -> true
    | _ -> false

  let is_free = function
    | { state = Free _; _ } -> true
    | _ -> false

  let namespace variable ~namespace =
    { variable with namespace }

  let upper_bound { constraints; _ } =
    match constraints with
    | Unconstrained ->
        object_primitive
    | Bound bound ->
        bound
    | Explicit explicits ->
        union explicits
    | LiteralIntegers ->
        integer

  let is_escaped_and_free = function
    | { state = Free { escaped }; _ } -> escaped
    | _ -> false

  let mark_all_variables_as_bound ?(simulated = false) annotation =
    let state = if simulated then InSimulatedCall else InFunction in
    let constraints annotation =
      match annotation with
      | Variable variable -> Some (Variable { variable with state })
      | _ -> None
    in
    instantiate annotation ~constraints


  let namespace_all_free_variables annotation ~namespace:into_namespace =
    let constraints annotation =
      match annotation with
      | Variable ({ state = Free _; _ } as variable) ->
          Some (Variable (namespace variable ~namespace:into_namespace))
      | _ -> None
    in
    instantiate annotation ~constraints


  let free_all_simulated_bound_variables annotation =
    let constraints annotation =
      match annotation with
      | Variable ({ state = InSimulatedCall;  _ } as variable) ->
          Some (Variable { variable with state = Free { escaped = false } })
      | _ -> None
    in
    instantiate annotation ~constraints


  let all_free_variables annotation =
    let is_free_variable = function
      | Variable { state = Free _; _ } -> true
      | _ -> false
    in
    let to_variable = function
      | Variable variable -> variable
      | _ -> failwith("collect got a non-variable")
    in
    collect annotation ~predicate:is_free_variable
    |> List.map ~f:to_variable


  let all_variables_are_resolved annotation =
    List.is_empty (all_free_variables annotation)


  let instantiate_all_free_variables ~replacement annotation =
    let constraints =
      all_free_variables annotation
      |> List.map ~f:(fun variable -> Variable variable)
      |> List.fold
        ~init:Map.empty
        ~f:(fun constraints variable -> Map.set constraints ~key:variable ~data:replacement)
    in
    instantiate annotation ~constraints:(Map.find constraints)

  let mark_all_free_variables_as_escaped ?specific annotation =
    let fresh_namespace = Namespace.create_fresh () in
    let constraints =
      let variables =
        match specific with
        | Some variables ->
            variables
        | None ->
            all_free_variables annotation
      in
      let mark_as_escaped sofar variable =
        let data =
          namespace { variable with state = Free { escaped = true }} ~namespace:fresh_namespace
          |> (fun variable -> Variable variable)
        in
        Map.set sofar ~key:(Variable variable) ~data
      in
      List.fold variables ~init:Map.empty ~f:mark_as_escaped
    in
    instantiate annotation ~constraints:(Map.find constraints)


  let collapse_all_escaped_variable_unions annotation =
    let module ConcreteTransform = Transform.Make(struct
        type state = unit

        let visit_children_before _ _ =
          true

        let visit_children_after =
          false

        let visit new_state annotation =
          let transformed_annotation =
            match annotation with
            | Union parameters ->
                let not_escaped_free_variable = function
                  | Variable variable -> not (is_escaped_and_free variable)
                  | _ -> true
                in
                List.filter parameters ~f:not_escaped_free_variable
                |> union
            | _ ->
                annotation
          in
          { Transform.transformed_annotation; new_state }
      end)
    in
    snd (ConcreteTransform.visit () annotation)


  let contains_escaped_free_variable =
    let predicate = function
      | Variable variable -> is_escaped_and_free variable
      | _ -> false
    in
    exists ~predicate

  let convert_all_escaped_free_variables_to_anys annotation =
    let constraints = function
      | Variable variable when is_escaped_and_free variable -> Some Any
      | _ -> None
    in
    instantiate annotation ~constraints
end




let is_concrete annotation =
  let module ConcreteTransform = Transform.Make(struct
      type state = bool

      let visit_children_before _ = function
        | Optional Bottom -> false
        | Parametric { name = ("typing.Optional" | "Optional"); parameters = [Bottom] } ->
            false
        | _ -> true

      let visit_children_after =
        false

      let visit sofar annotation =
        let new_state =
          match annotation with
          | Bottom
          | Top
          | Any ->
              false
          | _ ->
              sofar
        in
        { Transform.transformed_annotation = annotation; new_state }
    end)
  in
  fst (ConcreteTransform.visit true annotation) &&
  not (Variable.contains_escaped_free_variable annotation)


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
  identifier
  |> Reference.create
  |> fold []
  |> Reference.show


let rec dequalify map annotation =
  let dequalify_string string = string |> dequalify_identifier map in
  let module DequalifyTransform = Transform.Make(struct
      type state = unit

      let visit_children_before _ _ =
        true

      let visit_children_after =
        false

      let visit _ annotation =
        let transformed_annotation =
          match annotation with
          | Optional parameter ->
              Parametric { name = dequalify_string "typing.Optional"; parameters = [parameter]; }
          | Parametric { name; parameters } ->
              Parametric { name = dequalify_identifier map (reverse_substitute name); parameters; }
          | Union parameters ->
              Parametric { name = dequalify_string "typing.Union"; parameters; }
          | Primitive name ->
              Primitive (dequalify_identifier map name)
          | Variable ({ variable = name; _ } as annotation) ->
              Variable { annotation with variable = dequalify_identifier map name}
          | _ ->
              annotation
        in
        { Transform.transformed_annotation; new_state = () }
    end)
  in
  snd (DequalifyTransform.visit () annotation)


module TypedDictionary = struct
  let anonymous ~total fields =
    TypedDictionary { name = "$anonymous"; fields; total }


  let fields_have_colliding_keys left_fields right_fields =
    let found_collision { name = needle_name; annotation = needle_annotation } =
      let same_name_different_annotation { name; annotation } =
        String.equal name needle_name && not (equal annotation needle_annotation)
      in
      List.exists left_fields ~f:same_name_different_annotation
    in
    List.exists right_fields ~f:found_collision


  let field_named_parameters ~default fields =
    let parameters =
      let single_star =
        Record.Callable.RecordParameter.Variable {
          name = "";
          annotation = Top;
          default = false;
        };
      in
      let field_arguments =
        let field_to_argument { name; annotation } =
          Record.Callable.RecordParameter.Named {
            name = Format.asprintf "$parameter$%s" name;
            annotation;
            default;
          }
        in
        List.map ~f:field_to_argument fields
      in
      single_star :: field_arguments
    in
    Defined parameters


  let constructor ~name ~fields ~total =
    {
      Callable.kind = Named (Reference.create "__init__");
      implementation = {
        annotation = TypedDictionary { name; fields; total };
        parameters = field_named_parameters ~default:(not total) fields
      };
      overloads = [];
      implicit = None;
    }

  type special_method = {
    name: string;
    special_index: int option;
    overloads: typed_dictionary_field list -> t Callable.overload list;
  }

  let key_parameter name =
    Parameter.Named { name = "k"; annotation = literal_string name; default=false }

  let total_special_methods =
    let getitem_overloads =
      let overload { name; annotation } =
        { annotation; parameters = Defined [ key_parameter name ] }
      in
      List.map ~f:overload
    in
    let setitem_overloads =
      let overload { name; annotation } =
        {
          annotation = none;
          parameters = Defined [
              key_parameter name;
              Named { name = "v"; annotation; default = false };
            ];
        }
      in
      List.map ~f:overload
    in
    let get_overloads =
      let overloads { name; annotation } =
        [
          { annotation = Optional annotation; parameters = Defined [ key_parameter name ] };
          {
            annotation = Union [annotation; Variable (Variable.create "_T")];
            parameters = Defined [
                key_parameter name;
                Named {
                  name = "default";
                  annotation = Variable (Variable.create "_T");
                  default = false;
                };
              ];
          };
        ]
      in
      List.concat_map ~f:overloads
    in
    let setdefault_overloads =
      let overload { name; annotation } =
        {
          annotation = annotation;
          parameters = Defined [
              key_parameter name;
              Named { name = "default"; annotation; default = false };
            ];
        }
      in
      List.map ~f:overload
    in
    let update_overloads fields =
      [{ annotation = none; parameters = field_named_parameters fields ~default:true }]
    in
    [
      { name = "__getitem__"; special_index = Some 1; overloads = getitem_overloads };
      { name = "__setitem__"; special_index = Some 1; overloads = setitem_overloads };
      { name = "get"; special_index = Some 1; overloads = get_overloads };
      { name = "setdefault"; special_index = Some 1; overloads = setdefault_overloads};
      { name = "update"; special_index = None; overloads = update_overloads};
    ]

  let non_total_special_methods =
    let pop_overloads =
      let overloads { name; annotation } =
        [
          { annotation = annotation; parameters = Defined [ key_parameter name ] };
          {
            annotation = Union [annotation; Variable (Variable.create "_T")];
            parameters = Defined [
                key_parameter name;
                Named {
                  name = "default";
                  annotation = Variable (Variable.create "_T");
                  default = false;
                };
              ];
          };
        ]
      in
      List.concat_map ~f:overloads
    in
    let delitem_overloads fields =
      let overload { name; annotation = _ } =
        { annotation = none; parameters = Defined [ key_parameter name ] }
      in
      List.map ~f:(overload) fields
    in
    [
      { name = "pop"; special_index = Some 1; overloads = pop_overloads };
      { name = "__delitem__"; special_index = Some 1; overloads = delitem_overloads };
    ]

  let special_overloads ~fields ~method_name ~total =
    let special_methods =
      if total then total_special_methods else non_total_special_methods @ total_special_methods
    in
    List.find special_methods ~f:(fun { name; _ } -> String.equal name method_name)
    >>| (fun { overloads; _ } -> overloads fields)

  let is_special_mismatch ~method_name ~position ~total =
    let special_methods =
      if total then total_special_methods else non_total_special_methods @ total_special_methods
    in
    List.find special_methods ~f:(fun { name; _ } -> String.equal name method_name)
    >>= (fun { special_index; _ } -> special_index)
    >>| ((=) position)
    |> Option.value ~default:false

  let defines ~t_self_expression ~total =
    let class_name = if total then "TypedDictionary" else "NonTotalTypedDictionary" in
    let define ?self_parameter ?return_annotation name =
      Statement.Define {
        signature = {
          name = Reference.create_from_list [class_name; name];
          parameters = [
            { Ast.Parameter.name = "self"; value = None; annotation = self_parameter}
            |> Node.create_with_default_location;
          ];
          decorators = [];
          docstring = None;
          return_annotation;
          async = false;
          parent = Some (Reference.create class_name);
        };
        body = [];
      }
      |> Node.create_with_default_location
    in
    if total then
      define ~self_parameter:t_self_expression ~return_annotation:t_self_expression "copy" ::
      List.map total_special_methods ~f:(fun { name; _ } -> define name)
    else
      List.map non_total_special_methods ~f:(fun { name; _ } -> define name)

end


let remove_undeclared annotation =
  let module RemoveUndeclared = Transform.Make(struct
      type state = unit

      let visit_children_before _ _ =
        true

      let visit_children_after =
        false

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
              Parametric { name; parameters}
          | Union annotations ->
              begin
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
                | _ -> union annotations
              end
          | _ ->
              annotation
        in
        { Transform.transformed_annotation; new_state = () }
    end)
  in
  match annotation with
  | Primitive "typing.Undeclared" ->
      Any
  | _ ->
      snd (RemoveUndeclared.visit () annotation)


let to_yojson annotation =
  `String (show annotation)
