(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

open Core

open Ast
open Expression


type parametric =
  {
    name: Identifier.t;
    parameters: t list;
  }


and tuple =
  | Bounded of t list
  | Unbounded of t


and variable =
  {
    variable: Identifier.t;
    constraints: t list;
  }


and t =
  | Bottom
  | Object
  | Optional of t
  | Parametric of parametric
  | Primitive of Identifier.t
  | Top
  | Tuple of tuple
  | Union of t list
  | Variable of variable
[@@deriving compare, eq, sexp, show, hash]


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


module TypeCache = struct
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
  | _ ->
      name


let rec pp format annotation =
  let without_backtick parameters =
    List.map ~f:show parameters
    |> String.concat ~sep:", "
    |> String.substr_replace_all ~pattern:"`" ~with_:"" in
  match annotation with
  | Bottom ->
      Format.fprintf format "`typing.Any`"
  | Object ->
      Format.fprintf format "`typing.Any`"
  | Optional Bottom ->
      Format.fprintf format "`None`"
  | Optional parameter ->
      Format.fprintf format
        "`typing.Optional[%s]`"
        (without_backtick [parameter])
  | Parametric { name; parameters }
    when Identifier.show name = "typing.Optional" && parameters = [Bottom] ->
      Format.fprintf format "`None`"
  | Parametric { name; parameters } ->
      Format.fprintf format
        "`%s[%s]`"
        (Identifier.show (reverse_substitute name))
        (without_backtick parameters)
  | Primitive name ->
      Format.fprintf format "%a" Identifier.pp name
  | Top ->
      Format.fprintf format "`unknown`"
  | Tuple tuple ->
      let parameters =
        match tuple with
        | Bounded parameters ->
            (without_backtick parameters)
        | Unbounded parameter  ->
            (without_backtick [parameter]) ^ ", ..."
      in
      Format.fprintf format "`typing.Tuple[%s]`" parameters
  | Union parameters ->
      Format.fprintf format
        "`typing.Union[%s]`"
        (without_backtick parameters)
  | Variable { variable; constraints } ->
      if constraints = [] then
        Format.fprintf format "%a" Identifier.pp variable
      else
        Format.fprintf
          format
          "%a <: [%a]"
          Identifier.pp
          variable
          (Format.pp_print_list ~pp_sep:(fun format () -> Format.fprintf format ", ") pp)
          constraints


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


let awaitable parameter =
  Parametric {
    name = Identifier.create "typing.Awaitable";
    parameters = [parameter];
  }


let bool =
  Primitive (Identifier.create "bool")


let bytes =
  Primitive (Identifier.create "bytes")


let complex =
  Primitive (Identifier.create "complex")


let dictionary ~key ~value =
  Parametric {
    name = Identifier.create "dict";
    parameters = [key; value];
  }


let float =
  Primitive (Identifier.create "float")


let none =
  Optional Bottom


let generator ?(async=false) parameter =
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


let lambda parameter =
  Parametric {
    name = Identifier.create "lambda";
    parameters = [parameter];
  }


let list parameter =
  Parametric {
    name = Identifier.create "list";
    parameters = [parameter];
  }


let rec optional parameter =
  match parameter with
  | Top ->
      Top
  | Object ->
      Object
  | Optional _ ->
      parameter
  | _ ->
      Optional parameter


let meta = function
  | Variable _ ->
      primitive "type"
  | annotation ->
      Parametric {
        name = Identifier.create "typing.Type";
        parameters = [annotation];
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


let unbound =
  primitive "typing.Unbound"

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
    |> List.sort ~cmp:compare
  in
  if List.mem ~equal parameters Object then
    Object
  else
    match parameters with
    | [] -> Bottom
    | [parameter; Optional Bottom]
    | [Optional Bottom; parameter] -> Optional parameter
    | [parameter] -> parameter
    | _ -> Union parameters


let yield parameter =
  Parametric {
    name = Identifier.create "Yield";
    parameters = [parameter];
  }


let primitive_substitution_map =
  let complex = Primitive (Identifier.create "complex") in
  let object_dict = Parametric { name = Identifier.create "dict"; parameters = [Object; Object] } in
  [
    "object", Object;
    "typing.Any", Object;
    "None", none;
    "numbers.Number", complex;
    "numbers.Complex", complex;
    "numbers.Real", Primitive (Identifier.create "float");
    "numbers.Integral", Primitive (Identifier.create "int");
    "dict", object_dict;
    "typing.Dict", object_dict;
    "typing.Tuple", Tuple (Unbounded Object);
    "typing.List", list Object;
    "$bottom", Bottom;
    "$unknown", Top;
  ]
  |> List.map
    ~f:(fun (original, substitute) -> Identifier.create original, substitute)
  |> Identifier.Map.of_alist_exn


let parametric_substitution_map =
  [
    "typing.DefaultDict", "collections.defaultdict";
    "typing.Dict", "dict";
    "typing.FrozenSet", "frozenset";
    "typing.List", "list";
    "typing.Set", "set";
  ]
  |> List.map
    ~f:(fun (original, substitute) -> Identifier.create original, Identifier.create substitute)
  |> Identifier.Map.of_alist_exn


let create ~aliases { Node.value = expression; _ } =
  match TypeCache.find expression with
  | Some result ->
      result
  | _ ->
      let rec create reversed_lead tail =
        let name reversed_access =
          let show = function
            | Access.Identifier element ->
                Identifier.show element
            | _ ->
                "?" in
          List.rev reversed_access
          |> List.map ~f:show
          |> String.concat ~sep:"."
          |> Identifier.create in

        let annotation =
          match tail with
          | (Access.Identifier _ as access) :: tail ->
              create (access :: reversed_lead) tail
          | (Access.Subscript subscript) :: [] ->
              let parameters =
                let parameter = function
                  | Access.Index { Node.value = Access access; _ } ->
                      create [] access
                  | Access.Index { Node.value = String string; _ } ->
                      create [] (Access.create string)
                  | _ ->
                      Top
                in
                List.map ~f:parameter subscript
              in
              Parametric {
                name = name reversed_lead;
                parameters;
              }
          | [] ->
              let name = name reversed_lead in
              if Identifier.show name = "None" then
                none
              else
                Primitive name
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
                        Tuple (Bounded (List.map ~f:(resolve visited) elements))
                    | Tuple (Unbounded annotation) ->
                        Tuple (Unbounded (resolve visited annotation))
                    | Parametric { parameters; name } ->
                        let name =
                          match aliases (Primitive name) with
                          | Some (Primitive name) -> name
                          | _ -> name
                        in
                        Parametric {
                          parameters = List.map ~f:(resolve visited) parameters;
                          name;
                        }
                    | Variable ({ constraints; _ } as variable) ->
                        Variable {
                          variable with
                          constraints = List.map ~f:(resolve visited) constraints;
                        }
                    | Union elements ->
                        Union (List.map ~f:(resolve visited) elements)
                    | Bottom
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
        match expression with
        | Access [
            Access.Identifier typing;
            Access.Call {
              Node.value = {
                Call.name = {
                  Node.value = Access [Access.Identifier typevar];
                  _;
                };
                arguments = { Argument.value = { Node.value = String name; _ }; _ } :: arguments;
                _;
              };
              _;
            };
          ]
          when Identifier.show typing = "typing" &&
               Identifier.show typevar = "TypeVar" ->
            let constraints =
              let get_constraint = function
                | { Argument.value = { Node.value = Access access; _ }; Argument.name = None } ->
                    Some (create [] access)
                | _ ->
                    None
              in
              List.filter_map ~f:get_constraint arguments
            in
            Variable {
              variable = Identifier.create name;
              constraints;
            }

        | Access access ->
            create [] access
        | String string ->
            let access =
              try
                match ParserParser.parse [string] with
                | [{ Node.value = Statement.Expression { Node.value = Access access; _ }; _ }] ->
                    access
                | _ ->
                    Access.create string
              with _ ->
                Access.create string
            in
            create [] access
        | _ -> Top
      in
      TypeCache.set ~key:expression ~data:result;
      result


let expression annotation =
  let split name =
    Identifier.show name
    |> String.split ~on:'.'
    |> List.map
      ~f:Access.create
    |> List.concat
  in

  let rec access annotation =
    let index parameter = Access.Index (Node.create (Access (access parameter))) in
    match annotation with
    | Bottom -> Access.create "$bottom"
    | Object -> Access.create "object"
    | Optional Bottom -> split (Identifier.create "None")
    | Optional parameter ->
        (Access.create "typing.Optional") @
        [Access.Subscript [index parameter]]
    | Parametric { name; parameters }
      when Identifier.show name = "typing.Optional" && parameters = [Bottom] ->
        split (Identifier.create "None")
    | Parametric { name; parameters } ->
        let subscript = Access.Subscript (List.map ~f:index parameters) in
        (split (reverse_substitute name)) @ [subscript]
    | Primitive name -> split name
    | Top -> Access.create "$unknown"
    | Tuple tuple ->
        let subscript =
          match tuple with
          | Bounded parameters -> List.map ~f:index parameters
          | Unbounded parameter ->
              let ellipses =
                Access.Index (Node.create (Access [Access.Identifier (Identifier.create "...")]))
              in
              [index parameter; ellipses]
        in
        (Access.create "typing.Tuple") @ [Access.Subscript subscript]
    | Union parameters ->
        let subscript = Access.Subscript (List.map ~f:index parameters) in
        (Access.create "typing.Union") @ [subscript]
    | Variable { variable; _ } -> split variable
  in
  Node.create (Access (access annotation))


let access annotation =
  match expression annotation with
  | { Node.value = Access access; _ } -> access
  | _ -> failwith "Annotation expression is not an access"


let is_generator = function
  | Parametric { name; _ } ->
      List.mem
        ~equal:String.equal
        ["typing.Generator"; "typing.AsyncGenerator"]
        (Identifier.show name)
  | _ ->
      false


let is_awaitable = function
  | Parametric { name; parameters = [_] } when Identifier.show name = "typing.Awaitable" ->
      true
  | _ ->
      false


let rec is_callable = function
  | Optional annotation ->
      is_callable annotation
  | Parametric { name; _ } ->
      is_callable (Primitive name)
  | Union parameters ->
      List.exists ~f:is_callable parameters
  | Tuple (Bounded tuple) ->
      List.exists ~f:is_callable tuple
  | Tuple (Unbounded annotation) ->
      is_callable annotation
  | other ->
      equal other (Primitive (Identifier.create "typing.Callable"))


let is_generic = function
  | Parametric { name; _ } ->
      Identifier.show name = "typing.Generic"
  | _ ->
      false


let is_meta = function
  | Parametric { name; _ } -> Identifier.show name = "typing.Type"
  | _ -> false


let is_none = function
  | Optional Bottom -> true
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


let rec is_unknown = function
  | Top ->
      true
  | Optional annotation ->
      is_unknown annotation
  | Parametric { parameters; _ } ->
      List.exists ~f:is_unknown parameters
  | Union parameters ->
      List.exists ~f:is_unknown parameters
  | Tuple (Bounded tuple) ->
      List.exists ~f:is_unknown tuple
  | Tuple (Unbounded annotation) ->
      is_unknown annotation
  | _ ->
      false


let rec is_instantiated = function
  | Bottom ->
      true
  | Optional annotation ->
      is_instantiated annotation
  | Parametric { parameters; _ } ->
      List.exists ~f:is_instantiated parameters
  | Union parameters ->
      List.exists ~f:is_instantiated parameters
  | Variable { constraints; _ } when constraints = [] ->
      true
  | _ ->
      false


let rec variables = function
  | Optional annotation ->
      variables annotation
  | Tuple (Bounded elements) ->
      List.concat_map ~f:variables elements
  | Tuple (Unbounded annotation) ->
      variables annotation
  | Parametric { parameters; _ } ->
      List.concat_map ~f:variables parameters
  | (Variable _) as annotation ->
      [annotation]
  | Union elements ->
      List.concat_map ~f:variables elements
  | Bottom
  | Object
  | Primitive _
  | Top ->
      []


let is_resolved annotation =
  List.is_empty (variables annotation)


let rec is_partially_typed = function
  | Object ->
      true
  | Top ->
      true
  | Bottom ->
      true
  | Parametric { parameters; _ }
  | Union parameters
  | Tuple (Bounded parameters) ->
      List.exists ~f:is_partially_typed parameters
  | Optional annotation
  | Tuple (Unbounded annotation) ->
      is_partially_typed annotation
  | _ ->
      false


let is_untyped = function
  | Object
  | Bottom
  | Top -> true
  | _ -> false


let rec mismatch_with_any left right =
  let compatible left right =
    let symmetric left right =
      (Identifier.show left = "typing.Mapping" && Identifier.show right = "dict") ||
      (Identifier.show left = "typing.Iterable" && Identifier.show right = "list") ||
      (Identifier.show left = "typing.Iterable" && Identifier.show right = "set")
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
  | Object, Top
  | Top, Object
  | Object, Tuple _
  | Tuple _, Object
  | Object, Union _
  | Union _, Object
  | Object, Variable _
  | Variable _, Object ->
      true
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
      if List.mem ~equal union Object then
        mismatch_with_any Object other
      else
        false

  | _ ->
      false


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
  | annotation ->
      annotation, []


let class_variable annotation =
  parametric "typing.ClassVar" [annotation]


let class_variable_value = function
  | Parametric { name; parameters = [parameter] }
    when Identifier.show name = "typing.ClassVar" ->
      Some parameter
  | _ -> None


(* Angelic assumption: Any occurences of top indicate that we're dealing with Any instead of None.
   See T22792667. *)
let assume_any = function
  | Top -> Object
  | annotation -> annotation


let instantiate ?(widen = false) annotation ~constraints =
  let rec instantiate annotation =
    match constraints annotation with
    | Some Bottom when widen ->
        Top
    | Some replacement ->
        replacement
    | None ->
        begin
          match annotation with
          | Optional parameter ->
              optional (instantiate parameter)
          | Parametric ({ parameters; _ } as parametric) ->
              Parametric {
                parametric with
                parameters = List.map ~f:instantiate parameters;
              }
          | Tuple tuple ->
              let tuple =
                match tuple with
                | Bounded parameters ->
                    Bounded (List.map ~f:instantiate parameters)
                | Unbounded parameter ->
                    Unbounded (instantiate parameter)
              in
              Tuple tuple
          | Union parameters ->
              Union (List.map ~f:instantiate parameters)
          | _ ->
              annotation
        end
  in
  instantiate annotation


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
  match annotation with
  | Optional parameter ->
      Parametric {
        name = dequalify_string "typing.Optional";
        parameters = [dequalify map parameter];
      }
  | Parametric { name; parameters } ->
      Parametric {
        name = dequalify_identifier (reverse_substitute name);
        parameters = List.map ~f:(dequalify map) parameters;
      }
  | Union parameters ->
      Parametric {
        name = dequalify_string "typing.Union";
        parameters = List.map ~f:(dequalify map) parameters;
      }
  | Primitive name -> Primitive (dequalify_identifier name)
  | Variable { variable = name; constraints } ->
      Variable {
        variable = dequalify_identifier name;
        constraints = List.map ~f:(dequalify map) constraints;
      }
  | _ -> annotation
