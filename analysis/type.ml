(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression
open Pyre
open PyreParser


module Record = struct
  module Callable = struct
    module RecordParameter = struct
      type 'annotation named = {
        name: Access.t;
        annotation: 'annotation;
        default: bool;
      }
      [@@deriving compare, sexp, show, hash]


      let equal_named equal_annotation left right =
        left.default = right.default &&
        Access.equal (Access.sanitized left.name) (Access.sanitized right.name) &&
        equal_annotation left.annotation right.annotation


      type 'annotation t =
        | Named of 'annotation named
        | Variable of 'annotation named
        | Keywords of 'annotation named
      [@@deriving compare, eq, sexp, show, hash]
    end


    type kind =
      | Anonymous
      | Named of Access.t


    and 'annotation implicit_record = {
      implicit_annotation: 'annotation;
      name: Access.t;
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


    let equal_record equal_annotation left right =
      (* Ignores implicit argument to simplify unit tests. *)
      equal_kind left.kind right.kind &&
      equal_overload equal_annotation left.implementation right.implementation &&
      List.equal ~equal:(equal_overload equal_annotation) left.overloads right.overloads
  end
end


open Record.Callable


module Parameter = Record.Callable.RecordParameter


type tuple =
  | Bounded of t list
  | Unbounded of t


and constraints =
  | Bound of t
  | Explicit of t list
  | Unconstrained


and variance =
  | Covariant
  | Contravariant
  | Invariant


and typed_dictionary_field = {
  name: string;
  annotation: t;
}


and t =
  | Bottom
  | Callable of t Record.Callable.record
  | Deleted
  | Object
  | Optional of t
  | Parametric of { name: Identifier.t; parameters: t list }
  | Primitive of Identifier.t
  | Top
  | Tuple of tuple
  | TypedDictionary of { name: Identifier.t; fields: typed_dictionary_field list }
  | Union of t list
  | Variable of { variable: Identifier.t; constraints: constraints; variance: variance }
[@@deriving compare, eq, sexp, show, hash]


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
  match Identifier.show name with
  | "collections.defaultdict" ->
      Identifier.create "typing.DefaultDict"
  | "dict" ->
      Identifier.create "typing.Dict"
  | "list" ->
      Identifier.create "typing.List"
  | "set" ->
      Identifier.create "typing.Set"
  | "type" ->
      Identifier.create "typing.Type"
  | _ ->
      name


let rec pp format annotation =
  match annotation with
  | Bottom ->
      Format.fprintf format "undefined"
  | Callable { kind; implementation; overloads; _ } ->
      let kind =
        match kind with
        | Anonymous -> ""
        | Named name -> Format.asprintf "(%a)" Access.pp name
      in
      let signature_to_string { annotation; parameters } =
        let parameters =
          match parameters with
          | Undefined ->
              "..."
          | Defined parameters ->
              let parameter = function
                | Parameter.Named { Parameter.name; annotation; default } ->
                    let name = Access.show_sanitized name in
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
                      "Variable(%a, %a)"
                      Access.pp_sanitized name
                      pp annotation
                | Parameter.Keywords { Parameter.name; annotation; _ } ->
                    Format.asprintf
                      "Keywords(%a, %a)"
                      Access.pp_sanitized name
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
  | Deleted ->
      Format.fprintf format "deleted"
  | Object ->
      Format.fprintf format "typing.Any"
  | Optional Bottom ->
      Format.fprintf format "None"
  | Optional parameter ->
      Format.fprintf format "typing.Optional[%a]" pp parameter
  | Parametric { name; parameters }
    when (Identifier.show name = "typing.Optional" or Identifier.show name = "Optional") &&
         parameters = [Bottom] ->
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
        (Identifier.show (reverse_substitute name))
        parameters
  | Primitive name ->
      Format.fprintf format "%a" Identifier.pp name
  | Top ->
      Format.fprintf format "unknown"
  | Tuple tuple ->
      let parameters =
        match tuple with
        | Bounded parameters ->
            List.map parameters ~f:show
            |> String.concat ~sep:", "
        | Unbounded parameter  ->
            Format.asprintf "%a, ..." pp parameter
      in
      Format.fprintf format "typing.Tuple[%s]" parameters
  | TypedDictionary { name; fields } ->
      let fields =
        fields
        |> List.map ~f:(fun { name; annotation } -> Format.asprintf "%s: %a" name pp annotation)
        |> String.concat ~sep:", "
      in
      if Identifier.show name = "$anonymous" then
        Format.fprintf format "TypedDict with fields (%s)" fields
      else
        Format.fprintf format
          "TypedDict `%a` with fields (%s)"
          Identifier.pp
          name
          fields
  | Union parameters ->
      Format.fprintf format
        "typing.Union[%s]"
        (List.map parameters ~f:show
         |> String.concat ~sep:", ")
  | Variable { variable; constraints; variance } ->
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
      in
      let variance =
        match variance with
        | Covariant -> "(covariant)"
        | Contravariant -> "(contravariant)"
        | Invariant -> ""
      in
      Format.fprintf
        format
        "Variable[%s%s]%s"
        (Identifier.show variable)
        constraints
        variance


and show annotation =
  Format.asprintf "%a" pp annotation


let rec serialize = function
  | Bottom ->
      "$bottom"
  | annotation ->
      Format.asprintf "%a" pp annotation


let primitive name =
  Primitive (Identifier.create name)


let parametric name parameters =
  Parametric { name = Identifier.create name; parameters }


let variable ?(constraints = Unconstrained) ?(variance = Invariant) name =
  Variable { variable = Identifier.create name; constraints; variance }


let awaitable parameter =
  Parametric {
    name = Identifier.create "typing.Awaitable";
    parameters = [parameter];
  }


let bool =
  Primitive (Identifier.create "bool")


let bytes =
  Primitive (Identifier.create "bytes")


let callable
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


let complex =
  Primitive (Identifier.create "complex")


let dictionary ~key ~value =
  Parametric {
    name = Identifier.create "dict";
    parameters = [key; value];
  }


let ellipses =
  Primitive (Identifier.create "ellipses")


let float =
  Primitive (Identifier.create "float")


let generator ?(async=false) parameter =
  let none = Optional Bottom in
  if async then
    Parametric {
      name = Identifier.create "typing.AsyncGenerator";
      parameters = [parameter; none];
    }
  else
    Parametric {
      name = Identifier.create "typing.Generator";
      parameters = [parameter; none; none];
    }


let generic =
  Primitive (Identifier.create "typing.Generic")


let integer =
  Primitive (Identifier.create "int")


let iterable parameter =
  Parametric {
    name = Identifier.create "typing.Iterable";
    parameters = [parameter];
  }


let iterator parameter =
  Parametric {
    name = Identifier.create "typing.Iterator";
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
                 let name = Access.show name in
                 if String.is_prefix ~prefix:"**" name then
                   let name =
                     String.drop_prefix name 2
                     |> Access.create
                   in
                   Parameter.Keywords { Parameter.name; annotation = parameter; default = false }
                 else if String.is_prefix ~prefix:"*" name then
                   let name =
                     String.drop_prefix name 1
                     |> Access.create
                   in
                   Parameter.Variable { Parameter.name; annotation = parameter; default = false }
                 else
                   let name = Access.create name in
                   Parameter.Named { Parameter.name; annotation = parameter; default = false })
             parameters);
    };
    overloads = [];
    implicit = None;
  }


let list parameter =
  Parametric {
    name = Identifier.create "list";
    parameters = [parameter];
  }


let meta annotation =
  let parameter =
    match annotation with
    | Variable _ ->
        Object
    | annotation ->
        annotation
  in
  Parametric {
    name = Identifier.create "type";
    parameters = [parameter];
  }


let named_tuple =
  Primitive (Identifier.create "typing.NamedTuple")


let none =
  Optional Bottom


let rec optional parameter =
  match parameter with
  | Top ->
      Top
  | Deleted ->
      Deleted
  | Object ->
      Object
  | Optional _ ->
      parameter
  | _ ->
      Optional parameter


let sequence parameter =
  Parametric {
    name = Identifier.create "typing.Sequence";
    parameters = [parameter];
  }


let set parameter =
  Parametric {
    name = Identifier.create "set";
    parameters = [parameter];
  }


let string =
  Primitive (Identifier.create "str")


let tuple parameters: t =
  match parameters with
  | [] -> Tuple (Unbounded Object)
  | _ -> Tuple (Bounded parameters)


let undeclared =
  primitive "typing.Undeclared"


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
  else if List.mem ~equal parameters Object then
    Object
  else if List.mem ~equal parameters Top then
    Top
  else
    let normalize parameters =
      match parameters with
      | [] -> Bottom
      | [parameter] -> parameter
      | parameters -> Union parameters
    in
    if List.exists parameters ~f:(fun parameter -> equal parameter (Optional Bottom)) then
      Optional
        (normalize
           (List.filter
              parameters
              ~f:(fun parameter -> not (equal parameter (Optional Bottom)))))
    else
      normalize parameters


let yield parameter =
  Parametric {
    name = Identifier.create "Yield";
    parameters = [parameter];
  }


let primitive_substitution_map =
  let parametric_anys name number_of_anys =
    let rec parameters sofar remaining =
      match remaining with
      | 0 -> sofar
      | _ -> parameters (Object :: sofar) (remaining - 1)
    in
    Parametric { name = Identifier.create name; parameters = (parameters [] number_of_anys) }
  in
  [
    "$bottom", Bottom;
    "$deleted", Deleted;
    "$unknown", Top;
    "None", none;
    "function", callable ~annotation:Object ();
    "dict", parametric_anys "dict" 2;
    "list", list Object;
    "object", Object;
    "type", parametric_anys "type" 1;
    "typing.Any", Object;
    "typing.AsyncGenerator", parametric_anys "typing.AsyncGenerator" 2;
    "typing.AsyncIterable", parametric_anys "typing.AsyncIterable" 1;
    "typing.AsyncIterator", parametric_anys "typing.AsyncIterator" 1;
    "typing.Awaitable", parametric_anys "typing.Awaitable" 1;
    "typing.Callable", callable ~annotation:Object ();
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
    "typing.List", list Object;
    "typing.Mapping", parametric_anys "typing.Mapping" 2;
    "typing.Sequence", parametric_anys "typing.Sequence" 1;
    "typing.Set", parametric_anys "typing.Set" 1;
    "typing.Tuple", Tuple (Unbounded Object);
    "typing.Type", parametric_anys "type" 1;
  ]
  |> List.map
    ~f:(fun (original, substitute) -> Identifier.create original, substitute)
  |> Identifier.Map.of_alist_exn


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
  ]
  |> List.map
    ~f:(fun (original, substitute) -> Identifier.create original, Identifier.create substitute)
  |> Identifier.Map.of_alist_exn


let rec create ~aliases { Node.value = expression; _ } =
  match Cache.find expression with
  | Some result ->
      result
  | _ ->
      let rec parse reversed_lead tail =
        let annotation =
          match tail with
          | (Access.Identifier get_item)
            :: (Access.Call { Node.value = [{ Argument.value = argument; _ }]; _ })
            :: _
            when Identifier.show get_item = "__getitem__" ->
              let parameters =
                match Node.value argument with
                | Expression.Tuple elements -> elements
                | _ -> [argument]
              in
              let name =
                List.rev reversed_lead
                |> Access.show
                |> Identifier.create
              in
              Parametric { name; parameters = List.map parameters ~f:(create ~aliases) }
          | (Access.Identifier _ as access) :: tail ->
              parse (access :: reversed_lead) tail
          | [] ->
              let name =
                let sanitized =
                  match reversed_lead with
                  | (Access.Identifier name) :: tail ->
                      let name =
                        Identifier.show_sanitized name
                        |> Identifier.create
                      in
                      (Access.Identifier name) :: tail
                  | _ ->
                      reversed_lead
                in
                List.rev sanitized
                |> Access.show
              in
              if name = "None" then
                none
              else
                Primitive (Identifier.create name)
          | _ ->
              Top
        in

        (* Resolve aliases. *)
        let resolved =
          let rec resolve visited annotation =
            if Set.mem visited annotation then
              annotation
            else
              let visited = Set.add visited annotation in
              match aliases annotation with
              | Some alias ->
                  resolve visited alias
              | _ ->
                  begin
                    match annotation with
                    | Optional annotation ->
                        Optional (resolve visited annotation)
                    | Tuple (Bounded elements) ->
                        Tuple (Bounded (List.map elements ~f:(resolve visited)))
                    | Tuple (Unbounded annotation) ->
                        Tuple (Unbounded (resolve visited annotation))
                    | Parametric { name; parameters } ->
                        begin
                          let parametric name =
                            Parametric {
                              name;
                              parameters = List.map parameters ~f:(resolve visited);
                            }
                          in
                          match aliases (Primitive name) with
                          | Some (Primitive name) ->
                              parametric name
                          | Some (Parametric { name; _ }) ->
                              (* Ignore parameters for now. *)
                              parametric name
                          | Some (Union elements) ->
                              let replace_parameters = function
                                | Parametric parametric -> Parametric { parametric with parameters }
                                | annotation -> annotation
                              in
                              Union (List.map elements ~f:replace_parameters)
                          | _ ->
                              parametric name
                        end
                    | Variable ({ constraints; _ } as variable) ->
                        let constraints =
                          match constraints with
                          | Bound bound ->
                              Bound (resolve visited bound)
                          | Explicit constraints ->
                              Explicit (List.map constraints ~f:(resolve visited))
                          | Unconstrained ->
                              Unconstrained
                        in
                        Variable { variable with constraints }
                    | TypedDictionary { fields; name } ->
                        let fields =
                          let resolve_field_annotation { name; annotation } =
                            { name; annotation = resolve visited annotation }
                          in
                          List.map fields ~f:resolve_field_annotation;
                        in
                        TypedDictionary { name; fields }
                    | Union elements ->
                        Union (List.map elements ~f:(resolve visited))
                    | Bottom
                    | Callable _
                    | Deleted
                    | Object
                    | Primitive _
                    | Top ->
                        annotation
                  end
          in
          resolve Set.empty annotation
        in

        (* Substitutions. *)
        match resolved with
        | Primitive name ->
            begin
              match Identifier.Map.find primitive_substitution_map name with
              | Some substitute -> substitute
              | None -> resolved
            end
        | Parametric { name; parameters } ->
            begin
              match Identifier.Map.find parametric_substitution_map name with
              | Some name ->
                  Parametric { name; parameters }
              | None ->
                  begin
                    match Identifier.show name with
                    | "typing.Optional" when List.length parameters = 1 ->
                        optional (List.hd_exn parameters)

                    | "tuple"
                    | "typing.Tuple" ->
                        let tuple: tuple =
                          match parameters with
                          | [parameter; Primitive ellipses] when Identifier.show ellipses = "..." ->
                              Unbounded parameter
                          | _ -> Bounded parameters
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
                Argument.value = { Node.value = String { StringLiteral.value; _ }; _ };
                _;
              } :: _) ->
                Named (Access.create value)
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
                      | Access [
                          Access.Identifier name;
                          Access.Call { Node.value = arguments; _ };
                        ] ->
                          begin
                            let arguments =
                              List.map
                                arguments
                                ~f:(fun { Argument.value; _ } -> value)
                            in
                            match Identifier.show name, arguments with
                            | "Named",
                              { Node.value = Access name; _ } :: annotation :: tail ->
                                let default =
                                  match tail with
                                  | [{ Node.value = Access [Access.Identifier default]; _ }]
                                    when Identifier.show default = "default" -> true
                                  | _ -> false
                                in
                                Parameter.Named {
                                  Parameter.name;
                                  annotation = create ~aliases annotation;
                                  default;
                                }
                            | "Variable", { Node.value = Access name; _ } :: tail ->
                                let annotation =
                                  match tail with
                                  | annotation :: _ -> create ~aliases annotation
                                  | _ -> Top
                                in
                                Parameter.Variable {
                                  Parameter.name;
                                  annotation;
                                  default = false;
                                }
                            | "Keywords", { Node.value = Access name; _ } :: tail ->
                                let annotation =
                                  match tail with
                                  | annotation :: _ -> create ~aliases annotation
                                  | _ -> Top
                                in
                                Parameter.Keywords {
                                  Parameter.name;
                                  annotation;
                                  default = false;
                                }
                            | _ ->
                                Parameter.Named {
                                  Parameter.name = Access.create ("$" ^ Int.to_string index);
                                  annotation = Top;
                                  default = false;
                                }
                          end
                      | _ ->
                          Parameter.Named {
                            Parameter.name = Access.create ("$" ^ Int.to_string index);
                            annotation = create ~aliases parameter;
                            default = false;
                          }
                    in
                    match Node.value parameters with
                    | List parameters ->
                        Defined (List.mapi ~f:extract_parameter parameters)
                    | _ ->
                        Undefined
                  in
                  { annotation = create ~aliases annotation; parameters }
              | _ ->
                  undefined
            in
            match signatures with
            | (Access.Identifier get_item) ::
              (Access.Call { Node.value = [{ Argument.value = argument; _ }]; _ }) ::
              []
              when Identifier.show get_item = "__getitem__" ->
                get_signature argument, []
            | (Access.Identifier get_item) ::
              (Access.Call { Node.value = [{ Argument.value = argument; _ }]; _ }) ::
              (Access.Identifier get_item_overloads) ::
              (Access.Call {
                  Node.value = [
                    { Argument.value = { Node.value = overloads_argument; location }; _ }
                  ];
                  _;
                }) ::
              []
              when Identifier.show get_item = "__getitem__"
                && Identifier.show get_item_overloads = "__getitem__" ->
                let rec parse_overloads overloads =
                  match overloads with
                  | Expression.List arguments ->
                      [get_signature (Node.create ~location (Expression.Tuple arguments))]
                  | Expression.Access
                      (Access.Expression { Node.value = (Expression.List arguments); _ }
                       :: tail) ->
                      get_signature (Node.create ~location (Expression.Tuple arguments))
                      :: (parse_overloads (Access tail))
                  | Expression.Access (
                      Access.Identifier get_item
                      :: Access.Call { Node.value = [{ Argument.value = argument; _ }]; _ }
                      :: tail
                    )
                    when Identifier.show get_item = "__getitem__" ->
                      get_signature argument :: (parse_overloads (Access tail))
                  | Access [] ->
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
        | Access [
            Access.Identifier typing;
            Access.Identifier typevar;
            Access.Call ({
                Node.value = {
                  Argument.value = { Node.value = String { StringLiteral.value; _ }; _ };
                  _;
                } :: arguments;
                _;
              });
          ]
          when Identifier.show typing = "typing" && Identifier.show typevar = "TypeVar" ->
            let constraints =
              let explicits =
                let explicit = function
                  | { Argument.value; Argument.name = None } ->
                      create value ~aliases
                      |> Option.some
                  | _ ->
                      None
                in
                List.filter_map ~f:explicit arguments
              in
              let bound =
                let bound = function
                  | { Argument.value; Argument.name = Some { Node.value = bound; _ }; }
                    when Identifier.show_sanitized bound = "bound" ->
                      create value ~aliases
                      |> Option.some
                  | _ ->
                      None
                in
                List.find_map ~f:bound arguments
              in
              if not (List.is_empty explicits) then
                Explicit explicits
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
                } when Identifier.show_sanitized name = "covariant" ->
                    Some Covariant
                | {
                  Argument.name = Some { Node.value = name; _ };
                  Argument.value = { Node.value = True; _ };
                } when Identifier.show_sanitized name = "contravariant" ->
                    Some Contravariant
                | _ ->
                    None
              in
              List.find_map arguments ~f:variance_definition
              |> Option.value ~default:Invariant
            in
            Variable {
              variable = Identifier.create value;
              constraints;
              variance;
            }

        | Access
            ((Access.Identifier typing)
             :: (Access.Identifier callable)
             :: (Access.Call { Node.value = modifiers; _ })
             :: signatures)
          when Identifier.show typing = "typing" && Identifier.show callable = "Callable" ->
            parse_callable ~modifiers ~signatures ()
        | Access ((Access.Identifier typing) :: (Access.Identifier callable) :: signatures)
          when Identifier.show typing = "typing" && Identifier.show callable = "Callable" ->
            parse_callable ~signatures ()

        | Access ([
            Access.Identifier mypy_extensions;
            Access.Identifier typed_dictionary;
            Access.Identifier get_item;
            Access.Call({
                Node.value = [{
                    Argument.name = None;
                    value = {
                      Node.value = Expression.Tuple ({
                          Node.value = Expression.String { value = typed_dictionary_name; _ };
                          _;
                        } :: fields);
                      _;
                    };
                  }];
                _;
              });
          ])
          when Identifier.show mypy_extensions = "mypy_extensions" &&
               Identifier.show typed_dictionary = "TypedDict" &&
               Identifier.show get_item  = "__getitem__" ->
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
                    Some { name = field_name; annotation = create field_annotation ~aliases }
                | _ ->
                    None
              in
              fields
              |> List.filter_map ~f:tuple_to_field
            in
            TypedDictionary {
              name = Identifier.create typed_dictionary_name;
              fields;
            }
        | Access access ->
            parse [] access

        | Ellipses ->
            Primitive (Identifier.create "...")

        | String { StringLiteral.value; _ } ->
            let access =
              try
                let parsed =
                  Parser.parse [value]
                  |> Source.create
                  |> Preprocessing.preprocess
                  |> Source.statements
                in
                match parsed with
                | [{ Node.value = Statement.Expression { Node.value = Access access; _ }; _ }] ->
                    access
                | _ ->
                    Access.create value
              with _ ->
                Access.create value
            in
            parse [] access
        | _ ->
            Top
      in
      Cache.set ~key:expression ~data:result;
      result


let rec expression annotation =
  let split name =
    match Identifier.show name with
    | "..." ->
        [Access.Identifier (Identifier.create "...")]
    | name ->
        String.split name ~on:'.'
        |> List.map ~f:Access.create
        |> List.concat
  in

  let get_item_call ?call_parameters parameters =
    let parameter =
      match parameters with
      | _ when List.length parameters > 1 || Option.is_some call_parameters ->
          let tuple =
            let call_parameters =
              call_parameters
              >>| (fun call_parameters -> [call_parameters])
              |> Option.value ~default:[]
            in
            List.map parameters ~f:expression
            |> (fun elements -> Expression.Tuple (call_parameters @ elements))
            |> Node.create_with_default_location
          in
          [{ Argument.name = None; value = tuple }]
      | [parameter] ->
          [{ Argument.name = None; value = expression parameter }]
      | _ ->
          []
    in
    [
      Access.Identifier (Identifier.create "__getitem__");
      Access.Call (Node.create_with_default_location parameter);
    ]
  in

  let rec access annotation =
    match annotation with
    | Bottom -> Access.create "$bottom"
    | Callable { implementation; overloads; _ } ->
        let convert { annotation; parameters } =
          let call_parameters =
            match parameters with
            | Defined parameters ->
                let parameter parameter =
                  let call ?(default = false) name argument annotation =
                    let annotation =
                      annotation
                      >>| (fun annotation ->
                          [{
                            Argument.name = None;
                            value = expression annotation;
                          }])
                      |> Option.value ~default:[]
                    in
                    let arguments =
                      let default =
                        if default then
                          [
                            {
                              Argument.name = None;
                              value = Access.expression (Access.create "default");
                            };
                          ]
                        else
                          []
                      in
                      [{ Argument.name = None; value = Access.expression argument }]
                      @ annotation @ default
                    in
                    Access.expression
                      (Access.call ~arguments ~location:Location.Reference.any ~name ())
                  in
                  match parameter with
                  | Parameter.Keywords { Parameter.name; annotation; _ } ->
                      call "Keywords" name (Some annotation)
                  | Parameter.Named { Parameter.name; annotation; default } ->
                      call "Named" ~default name (Some annotation)
                  | Parameter.Variable { Parameter.name; annotation; _ } ->
                      call "Variable" name (Some annotation)
                in
                List (List.map parameters ~f:parameter)
                |> Node.create_with_default_location
            | Undefined ->
                Node.create_with_default_location Ellipses
          in
          get_item_call ~call_parameters [annotation]
        in
        let overloads =
          let overloads = List.concat_map overloads ~f:convert in
          if List.is_empty overloads then
            []
          else
            [
              Access.Identifier (Identifier.create "__getitem__");
              Access.Call
                (Node.create_with_default_location [
                    {
                      Argument.name = None;
                      value = Node.create_with_default_location (Access overloads);
                    }
                  ]);
            ]
        in
        (Access.create "typing.Callable") @ (convert implementation) @ overloads
    | Deleted -> Access.create "$deleted"
    | Object -> Access.create "object"
    | Optional Bottom ->
        split (Identifier.create "None")
    | Optional parameter ->
        (Access.create "typing.Optional") @ (get_item_call [parameter])
    | Parametric { name; parameters }
      when Identifier.show name = "typing.Optional" && parameters = [Bottom] ->
        split (Identifier.create "None")
    | Parametric { name; parameters } ->
        (split (reverse_substitute name)) @ (get_item_call parameters)
    | Primitive name ->
        split name
    | Top -> Access.create "$unknown"
    | Tuple elements ->
        let parameters =
          match elements with
          | Bounded parameters -> parameters
          | Unbounded parameter -> [parameter; Primitive (Identifier.create "...")]
        in
        (Access.create "typing.Tuple") @ (get_item_call parameters)
    | TypedDictionary { name; fields; } ->
        let argument =
          let tuple =
            let tail =
              let field_to_tuple { name; annotation } =
                Node.create_with_default_location (Expression.Tuple [
                    Node.create_with_default_location (Expression.String {
                        value = name;
                        kind = StringLiteral.String;
                      });
                    expression annotation;
                  ])
              in
              List.map fields ~f:field_to_tuple
            in
            Expression.String { value = Identifier.show name; kind = StringLiteral.String }
            |> Node.create_with_default_location
            |> (fun name -> Expression.Tuple(name :: tail))
            |> Node.create_with_default_location
          in
          { Argument.name = None; value = tuple; }
        in
        (Access.create "mypy_extensions.TypedDict") @ [
          Access.Identifier (Identifier.create "__getitem__");
          Access.Call (Node.create_with_default_location ([argument]));
        ]
    | Union parameters ->
        (Access.create "typing.Union") @ (get_item_call parameters)
    | Variable { variable; _ } -> split variable
  in

  let value =
    match annotation with
    | Primitive name when Identifier.show name = "..." -> Ellipses
    | _ -> Access (access annotation)
  in
  Node.create_with_default_location value


let access annotation =
  match expression annotation with
  | { Node.value = Access access; _ } -> access
  | _ -> failwith "Annotation expression is not an access"


module Transform = struct
  module type Transformer = sig
    type state
    val visit: state -> t -> state * t
    val visit_children: state -> t -> bool
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
              | Bound bound ->
                  Bound (visit_annotation bound ~state)
              | Explicit constraints ->
                  Explicit (List.map constraints ~f:(visit_annotation ~state))
              | Unconstrained ->
                  Unconstrained
            in
            Variable { variable with constraints }

        | Bottom
        | Deleted
        | Top
        | Object
        | Primitive _ ->
            annotation
      in
      let annotation =
        if Transformer.visit_children !state annotation then
          visit_children annotation
        else
          annotation
      in
      let new_state, transformed_annotation =  Transformer.visit !state annotation in
      state := new_state;
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

      let visit_children _ _ =
        true

      let visit sofar annotation =
        if predicate annotation then
          true, annotation
        else
          sofar, annotation
    end)
  in
  fst (ExistsTransform.visit false annotation)


let contains_callable annotation =
  exists annotation ~predicate:(function | Callable _ -> true | _ -> false)


let is_callable = function
  | Callable _ -> true
  | _ -> false


let is_deleted = function
  | Deleted -> true
  | _ -> false


let is_ellipses = function
  | Primitive primitive when Identifier.show primitive = "ellipses" -> true
  | _ -> false


let is_generator = function
  | Parametric { name; _ } ->
      List.mem
        ~equal:String.equal
        ["typing.Generator"; "typing.AsyncGenerator"]
        (Identifier.show name)
  | _ ->
      false


let is_generic = function
  | Parametric { name; _ } ->
      Identifier.show name = "typing.Generic"
  | _ ->
      false


let is_iterable = function
  | Parametric { name; _ } ->
      String.equal (Identifier.show name) "typing.Iterable"
  | _ ->
      false


let is_iterator = function
  | Parametric { name; _ } ->
      String.equal (Identifier.show name) "typing.Iterator"
  | _ ->
      false


let is_meta = function
  | Parametric { name; _ } -> Identifier.show name = "type"
  | _ -> false


let is_none = function
  | Optional Bottom -> true
  | _ -> false


let is_noreturn = function
  | Primitive name -> Identifier.show name = "typing.NoReturn"
  | _ -> false


let is_optional = function
  | Optional _ -> true
  | _ -> false


let is_optional_primitive = function
  | Primitive optional when Identifier.show optional = "typing.Optional" -> true
  | _ -> false


let is_primitive = function
  | Primitive _ -> true
  | _ -> false


let is_protocol = function
  | Parametric { name; _ } ->
      Identifier.show name = "typing.Protocol"
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


let is_unknown annotation =
  exists annotation ~predicate:(function | Top | Deleted -> true | _ -> false)


let is_type_alias annotation = equal annotation (primitive "typing.TypeAlias")


let is_not_instantiated annotation =
  let predicate = function
    | Bottom -> true
    | Variable { constraints = Unconstrained; _ } -> true
    | _ -> false
  in
  exists annotation ~predicate


let collect annotation ~predicate =
  let module CollectorTransform = Transform.Make(struct
      type state = t list

      let visit_children _ _ =
        true

      let visit sofar annotation =
        if predicate annotation then
          sofar @ [annotation], annotation
        else
          sofar, annotation
    end)
  in
  fst (CollectorTransform.visit [] annotation)



let variables annotation =
  let predicate = function | Variable _ -> true | _ -> false in
  collect annotation ~predicate


let primitives annotation =
  let predicate = function | Primitive _ -> true | _ -> false in
  collect annotation ~predicate


let is_resolved annotation =
  List.is_empty (variables annotation)


let is_partially_typed annotation =
  exists annotation ~predicate:(function | Object | Top | Bottom -> true | _ -> false)


let is_untyped = function
  | Object
  | Bottom
  | Top -> true
  | _ -> false


let optional_value = function
  | Optional annotation -> annotation
  | annotation -> annotation


let async_generator_value = function
  | Parametric { name; parameters = [parameter; _] }
    when Identifier.show name = "typing.AsyncGenerator" ->
      generator parameter
  | _ ->
      Top


let awaitable_value = function
  | Parametric { name; parameters = [parameter] } when Identifier.show name = "typing.Awaitable" ->
      parameter
  | _ ->
      Top


let parameters = function
  | Parametric { parameters; _ } -> parameters
  | _ -> []


let single_parameter = function
  | Parametric { parameters = [parameter]; _ } -> parameter
  | _ -> failwith "Type does not have single parameter"


let split = function
  | Optional parameter ->
      primitive "typing.Optional", [parameter]
  | Parametric { name; parameters } ->
      Primitive name, parameters
  | Tuple tuple ->
      let parameters =
        match tuple with
        | Bounded parameters -> parameters
        | Unbounded parameter -> [parameter]
      in
      Primitive (Identifier.create "tuple"), parameters
  | (TypedDictionary _) as typed_dictionary ->
      primitive "TypedDictionary", [typed_dictionary]
  | annotation ->
      annotation, []


let class_name annotation =
  split annotation
  |> fst
  |> expression
  |> Expression.access


let class_variable annotation =
  parametric "typing.ClassVar" [annotation]


let class_variable_value = function
  | Parametric { name; parameters = [parameter] }
    when Identifier.show name = "typing.ClassVar" ->
      Some parameter
  | _ -> None


(* Angelic assumption: Any occurrences of top indicate that we're dealing with Any instead of None.
   See T22792667. *)
let assume_any = function
  | Top -> Object
  | annotation -> annotation


let instantiate ?(widen = false) annotation ~constraints =
  let module InstantiateTransform = Transform.Make(struct
      type state = unit

      let visit_children _ annotation =
        constraints annotation
        |>  Option.is_none

      let visit _ annotation =
        match constraints annotation with
        | Some Bottom when widen ->
            (), Top
        | Some replacement ->
            (), replacement
        | None ->
            (), annotation
    end)
  in
  snd (InstantiateTransform.visit () annotation)


let instantiate_variables ~replacement annotation =
  let constraints =
    variables annotation
    |> List.fold
      ~init:Map.empty
      ~f:(fun constraints variable -> Map.set constraints ~key:variable ~data:replacement)
  in
  instantiate annotation ~constraints:(Map.find constraints)


let rec dequalify map annotation =
  let dequalify_identifier identifier =
    let rec fold accumulator access =
      if Access.Map.mem map access then
        (Access.Map.find_exn map access) @ accumulator
      else
        match access with
        | tail :: rest ->
            fold (tail :: accumulator) rest
        | [] -> accumulator
    in
    Identifier.show identifier
    |> Access.create
    |> List.rev
    |> fold []
    |> Access.show
    |> Identifier.create
  in
  let dequalify_string string = Identifier.create string |> dequalify_identifier in
  let module DequalifyTransform = Transform.Make(struct
      type state = unit

      let visit_children _ _ =
        true

      let visit _ annotation =
        (),
        match annotation with
        | Optional parameter ->
            Parametric { name = dequalify_string "typing.Optional"; parameters = [parameter]; }
        | Parametric { name; parameters } ->
            Parametric { name = dequalify_identifier (reverse_substitute name); parameters; }
        | Union parameters ->
            Parametric { name = dequalify_string "typing.Union"; parameters; }
        | Primitive name ->
            Primitive (dequalify_identifier name)
        | Variable { variable = name; constraints; variance } ->
            Variable { variable = dequalify_identifier name; constraints; variance }
        | _ ->
            annotation
    end)
  in
  snd (DequalifyTransform.visit () annotation)


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

    let name = function
      | Named { name; _ } -> Identifier.create (Access.show name)
      | Variable { name; _ } -> Identifier.create ("*" ^ (Access.show name))
      | Keywords { name; _ } -> Identifier.create ("**" ^ (Access.show name))


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
      | Named { name; _ } when String.is_prefix ~prefix:"$" (Access.show name) ->
          true
      | _ ->
          false


    let names_compatible left right =
      match left, right with
      | Named { name = left; _ }, Named { name = right; _ }
      | Variable { name = left; _ }, Variable { name = right; _ }
      | Keywords { name = left; _ }, Keywords { name = right; _ } ->
          let left = Access.show_sanitized left in
          let right = Access.show_sanitized right in
          if String.is_prefix ~prefix:"$" left ||
             String.is_prefix ~prefix:"$" right then
            true
          else
            let left =
              left
              |> Identifier.create
              |> Identifier.remove_leading_underscores
            in
            let right =
              right
              |> Identifier.create
              |> Identifier.remove_leading_underscores
            in
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


  let with_return_annotation ({ implementation; overloads; _ } as initial) ~annotation =
    let re_annotate implementation = { implementation with annotation } in
    {
      initial with
      implementation = re_annotate implementation;
      overloads = List.map ~f:re_annotate overloads
    }
end


let rec mismatch_with_any left right =
  let compatible left right =
    let symmetric left right =
      (Identifier.show left = "typing.Mapping" && Identifier.show right = "dict") ||
      (Identifier.show left = "collections.OrderedDict" && Identifier.show right = "dict") ||
      (Identifier.show left = "typing.Iterable" && Identifier.show right = "list") ||
      (Identifier.show left = "typing.Iterable" && Identifier.show right = "typing.List") ||
      (Identifier.show left = "typing.Iterable" && Identifier.show right = "set") ||
      (Identifier.show left = "typing.Sequence" && Identifier.show right = "typing.List") ||
      (Identifier.show left = "typing.Sequence" && Identifier.show right = "list")
    in
    Identifier.equal left right ||
    symmetric left right ||
    symmetric right left
  in

  match left, right with
  | Object, Bottom
  | Bottom, Object
  | Object, Optional _
  | Optional _, Object
  | Object, Parametric _
  | Parametric _, Object
  | Object, Primitive _
  | Primitive _, Object
  | Object, Callable _
  | Callable _, Object
  | Object, Top
  | Top, Object
  | Object, Tuple _
  | Tuple _, Object
  | Object, Union _
  | Union _, Object
  | Object, Variable _
  | Variable _, Object ->
      true
  | Callable {
      Callable.implementation = {
        Callable.annotation = left_annotation;
        parameters = left_parameters;
      };
      _;
    },
    Callable {
      Callable.implementation = {
        Callable.annotation = right_annotation;
        parameters = right_parameters;
      };
      _;
    } ->
      let parameters_mismatch_with_any left right =
        match left, right with
        | Defined left, Defined right when List.length left = List.length right ->
            let left = List.map ~f:Callable.Parameter.annotation left in
            let right = List.map ~f:Callable.Parameter.annotation right in
            List.exists2_exn left right ~f:mismatch_with_any
        | _ ->
            false
      in
      let parameters_compatible =
        match left_parameters, right_parameters with
        | Defined left, Defined right when List.length left = List.length right ->
            true
        | Defined _, Defined _ ->
            false
        | _ ->
            true
      in
      if parameters_compatible then
        mismatch_with_any left_annotation right_annotation
        || parameters_mismatch_with_any left_parameters right_parameters
      else
        false
  | Parametric { name; parameters = [left] }, right
    when Identifier.equal name (Identifier.create "typing.Optional") ->
      mismatch_with_any left right
  | left, Parametric { name; parameters = [right] }
    when Identifier.equal name (Identifier.create "typing.Optional") ->
      mismatch_with_any left right
  | Optional left, Optional right
  | Optional left, right
  | left, Optional right ->
      mismatch_with_any left right

  | Parametric left, Parametric right
    when compatible left.name right.name &&
         List.length left.parameters = List.length right.parameters ->
      List.exists2_exn ~f:mismatch_with_any left.parameters right.parameters

  | Parametric { name = iterator; parameters = [iterator_parameter] },
    Parametric { name = generator; parameters = generator_parameter :: _ }
  | Parametric { name = generator; parameters = generator_parameter :: _ },
    Parametric { name = iterator; parameters = [iterator_parameter] }
    when (Identifier.show iterator = "typing.Iterator" ||
          Identifier.show iterator = "typing.Iterable") &&
         Identifier.show generator = "typing.Generator" ->
      mismatch_with_any iterator_parameter generator_parameter

  | Tuple (Bounded left), Tuple (Bounded right) when List.length left = List.length right ->
      List.exists2_exn ~f:mismatch_with_any left right
  | Tuple (Unbounded left), Tuple (Unbounded right) ->
      mismatch_with_any left right
  | Tuple (Bounded bounded), Tuple (Unbounded unbounded)
  | Tuple (Unbounded unbounded), Tuple (Bounded bounded) ->
      begin
        match unbounded, bounded with
        | Object, _ ->
            true
        | unbounded, head :: tail ->
            mismatch_with_any unbounded head ||
            List.for_all ~f:(equal Object) tail
        | _ ->
            false
      end

  | Union left, Union right ->
      let left = Set.of_list left in
      let right = Set.of_list right in
      let mismatched left right =
        Set.length left = Set.length right &&
        Set.mem left Object &&
        not (Set.mem right Object) &&
        Set.length (Set.diff left right) = 1
      in
      mismatched left right || mismatched right left
  | Union union, other
  | other, Union union ->
      List.exists ~f:(mismatch_with_any other) union

  | _ ->
      false


module TypedDictionary = struct
  let anonymous fields =
    TypedDictionary { name = Identifier.create "$anonymous"; fields }


  let fields_have_colliding_keys left_fields right_fields =
    let found_collision { name = needle_name; annotation = needle_annotation } =
      let same_name_different_annotation { name; annotation } =
        name = needle_name && not (equal annotation needle_annotation)
      in
      List.exists left_fields ~f:same_name_different_annotation
    in
    List.exists right_fields ~f:found_collision


  let constructor ~name ~fields =
    let parameters =
      let single_star =
        Record.Callable.RecordParameter.Variable {
          name = Access.create "";
          annotation = Top;
          default = false;
        };
      in
      let field_arguments =
        let field_to_argument { name; annotation } =
          Record.Callable.RecordParameter.Named {
            name = Access.create (Format.asprintf "$parameter$%s" name);
            annotation;
            default = false;
          }
        in
        List.map ~f:field_to_argument fields
      in
      single_star :: field_arguments
    in
    {
      Callable.kind = Named (Access.create "__init__");
      implementation = {
        annotation = TypedDictionary { name; fields };
        parameters = Defined parameters;
      };
      overloads = [];
      implicit = None;
    }


  let setter ~callable:({ implementation; _ } as callable) ~annotation =
    {
      callable with
      implementation = {
        implementation with
        parameters = Defined [
            Named {
              name = Access.create "key";
              annotation = string;
              default = false;
            };
            Named {
              name = Access.create "value";
              annotation;
              default = false;
            };
          ];
      };
    }
end


let to_yojson annotation =
  `String (show annotation)
