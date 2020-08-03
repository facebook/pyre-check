(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Expression
open Pyre
open PyreParser
module ExpressionParameter = Parameter

module Record = struct
  module Variable = struct
    type state =
      | Free of { escaped: bool }
      | InFunction
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

    module RecordNamespace = struct
      type t = int [@@deriving compare, eq, sexp, show, hash]
    end

    module RecordUnary = struct
      type 'annotation record = {
        variable: Identifier.t;
        constraints: 'annotation constraints;
        variance: variance;
        state: state;
        namespace: RecordNamespace.t;
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
        Format.fprintf format "%s[%s%s]%s" name (Identifier.sanitized variable) constraints variance
    end

    module RecordVariadic = struct
      (* TODO(T47346673): Handle variance on variadics. *)
      module RecordParameters = struct
        type 'annotation record = {
          name: Identifier.t;
          variance: variance;
          state: state;
          namespace: RecordNamespace.t;
        }
        [@@deriving compare, eq, sexp, show, hash]

        module RecordComponents = struct
          type component =
            | KeywordArguments
            | PositionalArguments
          [@@deriving compare, eq, sexp, show, hash]

          type t = {
            component: component;
            variance: variance;
            variable_name: Identifier.t;
            variable_namespace: RecordNamespace.t;
          }
          [@@deriving compare, eq, sexp, show, hash]

          let component_name = function
            | KeywordArguments -> "kwargs"
            | PositionalArguments -> "args"


          let pp_concise format { component; variable_name; _ } =
            Format.fprintf format "%s.%s" variable_name (component_name component)
        end

        let create ?(variance = Invariant) name =
          { name; variance; state = Free { escaped = false }; namespace = 1 }
      end

      module RecordList = struct
        type 'annotation record = {
          name: Identifier.t;
          constraints: 'annotation constraints;
          variance: variance;
          state: state;
          namespace: RecordNamespace.t;
        }
        [@@deriving compare, eq, sexp, show, hash]

        let create ?(constraints = Unconstrained) ?(variance = Invariant) name =
          { name; constraints; variance; state = Free { escaped = false }; namespace = 1 }


        let name { name; _ } = name
      end
    end

    type 'a record =
      | Unary of 'a RecordUnary.record
      | ParameterVariadic of 'a RecordVariadic.RecordParameters.record
      | ListVariadic of 'a RecordVariadic.RecordList.record
    [@@deriving compare, eq, sexp, show, hash]
  end

  module OrderedTypes = struct
    let map_public_name = "pyre_extensions.type_variable_operators.Map"

    let show_type_list types ~pp_type =
      Format.asprintf
        "%a"
        (Format.pp_print_list ~pp_sep:(fun format () -> Format.fprintf format ", ") pp_type)
        types


    module RecordConcatenate = struct
      let public_name = "pyre_extensions.type_variable_operators.Concatenate"

      module Middle = struct
        type 'annotation t = {
          variable: 'annotation Variable.RecordVariadic.RecordList.record;
          mappers: Identifier.t list;
        }
        [@@deriving compare, eq, sexp, show, hash]

        let rec show_concise = function
          | { variable = { name; _ }; mappers = [] } -> name
          | { mappers = head_mapper :: tail_mappers; _ } as mapped ->
              let inner = { mapped with mappers = tail_mappers } in
              Format.asprintf "Map[%s, %s]" head_mapper (show_concise inner)


        let unwrap_if_bare = function
          | { variable; mappers = [] } -> Some variable
          | _ -> None
      end

      type 'annotation wrapping = {
        head: 'annotation list;
        tail: 'annotation list;
      }
      [@@deriving compare, eq, sexp, show, hash]

      type ('middle, 'annotation) t = {
        middle: 'middle;
        wrapping: 'annotation wrapping;
      }
      [@@deriving compare, eq, sexp, show, hash]

      let empty_wrap (middle : 'a Middle.t) = { middle; wrapping = { head = []; tail = [] } }

      let head { wrapping = { head; _ }; _ } = head

      let middle { middle; _ } = middle

      let tail { wrapping = { tail; _ }; _ } = tail

      let unwrap_if_only_middle concatenation =
        Option.some_if
          (List.is_empty (head concatenation) && List.is_empty (tail concatenation))
          (middle concatenation)


      let pp_concatenation format { middle; wrapping } ~pp_type =
        match wrapping with
        | { head = []; tail = [] } -> Format.fprintf format "%s" (Middle.show_concise middle)
        | { head; tail = [] } ->
            Format.fprintf
              format
              "Concatenate[%s, %s]"
              (show_type_list head ~pp_type)
              (Middle.show_concise middle)
        | { head = []; tail } ->
            Format.fprintf
              format
              "Concatenate[%s, %s]"
              (Middle.show_concise middle)
              (show_type_list tail ~pp_type)
        | { head; tail } ->
            Format.fprintf
              format
              "Concatenate[%s, %s, %s]"
              (show_type_list head ~pp_type)
              (Middle.show_concise middle)
              (show_type_list tail ~pp_type)
    end

    type 'annotation record =
      | Concrete of 'annotation list
      | Any
      | Concatenation of ('annotation RecordConcatenate.Middle.t, 'annotation) RecordConcatenate.t
    [@@deriving compare, eq, sexp, show, hash]

    let pp_concise format variable ~pp_type =
      match variable with
      | Concrete types -> Format.fprintf format "%s" (show_type_list types ~pp_type)
      | Any -> Format.fprintf format "..."
      | Concatenation concatenation ->
          Format.fprintf format "%a" (RecordConcatenate.pp_concatenation ~pp_type) concatenation


    let concatenate ~left ~right =
      match left, right with
      | Concrete left, Concrete right -> Some (Concrete (left @ right))
      (* Any can masquerade as the empty list *)
      | other, Any
      | Any, other
      | other, Concrete []
      | Concrete [], other ->
          Some other
      | Concrete left, Concatenation ({ wrapping = { head; tail }; _ } as concatenation) ->
          Some (Concatenation { concatenation with wrapping = { head = left @ head; tail } })
      | Concatenation ({ wrapping = { head; tail }; _ } as concatenation), Concrete right ->
          Some (Concatenation { concatenation with wrapping = { head; tail = tail @ right } })
      | Concatenation _, Concatenation _ -> None
  end

  module Callable = struct
    module RecordParameter = struct
      type 'annotation named = {
        name: Identifier.t;
        annotation: 'annotation;
        default: bool;
      }
      [@@deriving compare, eq, sexp, show, hash]

      type 'annotation variable =
        | Concrete of 'annotation
        | Concatenation of
            ( 'annotation OrderedTypes.RecordConcatenate.Middle.t,
              'annotation )
            OrderedTypes.RecordConcatenate.t
      [@@deriving compare, eq, sexp, show, hash]

      type 'annotation t =
        | PositionalOnly of {
            index: int;
            annotation: 'annotation;
            default: bool;
          }
        | Named of 'annotation named
        | KeywordOnly of 'annotation named
        | Variable of 'annotation variable
        | Keywords of 'annotation
      [@@deriving compare, eq, sexp, show, hash]

      let equal equal_annotation left right =
        match left, right with
        | Named left, Named right ->
            Bool.equal left.default right.default
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
        | PositionalOnly { default; annotation; _ } ->
            Format.asprintf "%a%s" pp_type annotation (if default then ", default" else "")
        | Named named -> print_named ~kind:"Named" named
        | KeywordOnly named -> print_named ~kind:"KeywordOnly" named
        | Variable (Concrete annotation) -> Format.asprintf "Variable(%a)" pp_type annotation
        | Variable (Concatenation concatenation) ->
            Format.asprintf
              "Variable(%a)"
              (OrderedTypes.RecordConcatenate.pp_concatenation ~pp_type)
              concatenation
        | Keywords annotation -> Format.asprintf "Keywords(%a)" pp_type annotation


      let annotation = function
        | PositionalOnly { annotation; _ } -> Some annotation
        | Named { annotation; _ } -> Some annotation
        | KeywordOnly { annotation; _ } -> Some annotation
        | Variable (Concrete annotation) -> Some annotation
        | Keywords annotation -> Some annotation
        | _ -> None
    end

    type kind =
      | Anonymous
      | Named of Reference.t

    and 'annotation parameter_variadic_type_variable = {
      head: 'annotation list;
      variable: 'annotation Variable.RecordVariadic.RecordParameters.record;
    }

    and 'annotation record_parameters =
      | Defined of 'annotation RecordParameter.t list
      | Undefined
      | ParameterVariadicTypeVariable of 'annotation parameter_variadic_type_variable

    and 'annotation overload = {
      annotation: 'annotation;
      parameters: 'annotation record_parameters;
    }

    and 'annotation record = {
      kind: kind;
      implementation: 'annotation overload;
      overloads: 'annotation overload list;
    }
    [@@deriving compare, eq, sexp, show, hash]

    let equal_overload equal_annotation left right =
      equal_record_parameters equal_annotation left.parameters right.parameters
      && equal_annotation left.annotation right.annotation


    let _ = equal_record (* suppress warning about unused generated version *)

    let equal_record equal_annotation left right =
      (* Ignores implicit argument to simplify unit tests. *)
      equal_kind left.kind right.kind
      && equal_overload equal_annotation left.implementation right.implementation
      && List.equal (equal_overload equal_annotation) left.overloads right.overloads
  end

  module Parameter = struct
    type 'annotation record =
      | Single of 'annotation
      | Group of 'annotation OrderedTypes.record
      | CallableParameters of 'annotation Callable.record_parameters
    [@@deriving compare, eq, sexp, show, hash]

    let is_single = function
      | Single single -> Some single
      | CallableParameters _
      | Group _ ->
          None
  end

  module TypedDictionary = struct
    type 'annotation typed_dictionary_field = {
      name: string;
      annotation: 'annotation;
      required: bool;
    }
    [@@deriving compare, eq, sexp, show, hash]

    type 'annotation record = {
      name: Identifier.t;
      fields: 'annotation typed_dictionary_field list;
    }
    [@@deriving compare, eq, sexp, show, hash]
  end
end

module Polynomial = struct
  module Monomial = struct
    type 'a variable = 'a Record.Variable.RecordUnary.record
    [@@deriving compare, eq, sexp, show, hash]

    type 'a variable_degree = {
      variable: 'a variable;
      degree: int;
    }
    [@@deriving eq, sexp, compare, hash, show]

    type 'a t = {
      constant_factor: int;
      variables: 'a variable_degree list;
    }
    [@@deriving eq, sexp, compare, hash, show]

    let variable_name { Record.Variable.RecordUnary.variable; _ } = variable

    let equal_variable_id
        { Record.Variable.RecordUnary.variable; _ }
        { Record.Variable.RecordUnary.variable = variable'; _ }
      =
      String.equal variable variable'


    let has_variable { variables; _ } ~variable =
      List.exists variables ~f:(fun { variable = variable'; _ } ->
          equal_variable_id variable variable')


    let compare_normal { variables = left_variables; _ } { variables = right_variables; _ } =
      let sum_degrees variables =
        List.fold variables ~init:0 ~f:(fun sum { degree; _ } -> sum + degree)
      in
      let total_degree_compare =
        Int.compare (sum_degrees left_variables) (sum_degrees right_variables)
      in
      if total_degree_compare <> 0 then
        total_degree_compare
      else
        let variable_list variables =
          List.map variables ~f:(fun { variable; _ } -> variable_name variable)
        in
        List.compare String.compare (variable_list left_variables) (variable_list right_variables)


    let normalize { constant_factor; variables } =
      let variables =
        List.sort variables ~compare:(fun { variable = variable1; _ } { variable = variable2; _ } ->
            String.compare (variable_name variable1) (variable_name variable2))
      in
      { constant_factor; variables }


    let create_from_list (constant_factor, variables) =
      let variables = List.map variables ~f:(fun (variable, degree) -> { variable; degree }) in
      { constant_factor; variables } |> normalize


    let show_normal ?(concise = false) { constant_factor; variables } =
      let strip_qualification identifier =
        String.split ~on:'.' identifier |> List.last |> Option.value ~default:identifier
      in
      let list_string =
        List.map variables ~f:(fun { variable; degree } ->
            let name = variable_name variable |> if concise then strip_qualification else Fn.id in
            name ^ if degree > 1 then "^" ^ string_of_int degree else "")
      in
      if constant_factor = 1 && List.length list_string > 0 then
        String.concat ~sep:"" list_string
      else if constant_factor = -1 && List.length list_string > 0 then
        "-" ^ String.concat ~sep:"" list_string
      else
        string_of_int constant_factor ^ String.concat ~sep:"" list_string


    let multiply
        { constant_factor = left_factor; variables = left_variables }
        { constant_factor = right_factor; variables = right_variables }
      =
      let multiply_variables left_variables right_variables =
        if List.length left_variables = 0 then
          right_variables
        else if List.length right_variables = 0 then
          left_variables
        else
          let merge_common left_variables right_variables =
            List.filter_map
              left_variables
              ~f:(fun { variable = left_variable; degree = left_degree } ->
                match
                  List.find right_variables ~f:(fun { variable = right_variable; _ } ->
                      equal_variable_id left_variable right_variable)
                with
                | Some { variable = right_variable; degree = right_degree } ->
                    Some { variable = right_variable; degree = left_degree + right_degree }
                | None -> None)
          in
          let merge_difference left_variables right_variables =
            List.filter left_variables ~f:(fun left_variable_degree ->
                not
                  (List.mem
                     right_variables
                     left_variable_degree
                     ~equal:(fun { variable = right_variable; _ } { variable = left_variable; _ } ->
                       equal_variable_id left_variable right_variable)))
          in
          merge_common left_variables right_variables
          @ merge_difference left_variables right_variables
          @ merge_difference right_variables left_variables
      in

      {
        constant_factor = left_factor * right_factor;
        variables = multiply_variables left_variables right_variables;
      }
  end

  type 'a t = 'a Monomial.t list [@@deriving compare, eq, sexp, hash, show]

  let rec show_normal ?(concise = false) polynomial =
    match polynomial with
    | [] -> "0"
    | [x] -> Monomial.show_normal ~concise x
    | x :: xs -> Monomial.show_normal ~concise x ^ " + " ^ show_normal ~concise xs


  let fold1 l ~f =
    match l with
    | [] -> []
    | hd :: tl -> List.fold tl ~init:hd ~f


  let create_from_int value = [{ Monomial.constant_factor = value; variables = [] }]

  let create_from_variable variable =
    [{ Monomial.constant_factor = 1; variables = [{ variable; degree = 1 }] }]


  let is_base_case = function
    | []
    | [{ Monomial.variables = []; _ }]
    | [{ Monomial.variables = [{ degree = 1; _ }]; constant_factor = 1 }] ->
        true
    | _ -> false


  (* Graded lexicographic order:
     https://www.wikiwand.com/en/Monomial_order#/Graded_lexicographic_order *)
  let normalize polynomial =
    List.filter polynomial ~f:(fun { Monomial.constant_factor; _ } -> constant_factor <> 0)
    |> List.map ~f:Monomial.normalize
    |> List.sort ~compare:Monomial.compare_normal


  let create_from_list list = List.map list ~f:Monomial.create_from_list |> normalize

  let merge left_polynomial right_polynomial ~operation =
    let operation =
      match operation with
      | `Plus -> ( + )
      | `Minus -> ( - )
    in
    let rec merge_sorted left_polynomial right_polynomial =
      match left_polynomial, right_polynomial with
      | [], [] -> []
      | left_polynomial, [] -> left_polynomial
      | [], right_polynomial ->
          List.map right_polynomial ~f:(fun { Monomial.constant_factor; variables } ->
              { Monomial.constant_factor = operation 0 constant_factor; variables })
      | ( ({ Monomial.constant_factor = left_factor; variables = left_variables } as left_monomial)
          :: left_polynomial,
          ( { Monomial.constant_factor = right_factor; variables = right_variables } as
          right_monomial )
          :: right_polynomial ) ->
          if Monomial.compare_normal left_monomial right_monomial = 0 then
            { constant_factor = operation left_factor right_factor; variables = left_variables }
            :: merge_sorted left_polynomial right_polynomial
          else if Monomial.compare_normal left_monomial right_monomial < 0 then
            { constant_factor = left_factor; variables = left_variables }
            :: merge_sorted left_polynomial (right_monomial :: right_polynomial)
          else
            { constant_factor = operation 0 right_factor; variables = right_variables }
            :: merge_sorted (left_monomial :: left_polynomial) right_polynomial
    in
    merge_sorted (normalize left_polynomial) (normalize right_polynomial) |> normalize


  let add = merge ~operation:`Plus

  let subtract = merge ~operation:`Minus

  let multiply left_polynomial right_polynomial =
    let multiply_monomial_polynomial monomial ~polynomial =
      List.map polynomial ~f:(Monomial.multiply monomial)
    in
    List.concat_map left_polynomial ~f:(multiply_monomial_polynomial ~polynomial:right_polynomial)
    |> List.map ~f:(fun monomial -> [Monomial.normalize monomial])
    |> fold1 ~f:add


  let rec pow polynomial n =
    if n > 1 then multiply (pow polynomial (n - 1)) polynomial else polynomial


  (* Example: polynomial:(3x + 2yx^2 + y + 3), by_polynomial:(4z+3), variable:x *)
  let replace polynomial ~by:by_polynomial ~variable =
    (* Raise by_polynomial to degree of the variable (e.g x ) it is replacing and multiply it with
       the rest of the expression*)
    let replace_variable_by_polynomial
        ~monomial:{ Monomial.constant_factor; variables }
        ~polynomial
        ~variable
      =
      (* 2yx^2 -> (2,_) *)
      let { Monomial.degree; _ } =
        List.find_exn variables ~f:(fun { Monomial.variable = variable'; _ } ->
            Monomial.equal_variable_id variable variable')
      in
      (* 4z+3 -> (4z+3)^2 *)
      let pow_polynomial = pow polynomial degree in
      (* 2yx^2 -> 2y *)
      let polynomial_from_mono_without_replaced_variable =
        [
          {
            Monomial.constant_factor;
            variables =
              List.filter variables ~f:(fun { variable = variable'; _ } ->
                  not (Monomial.equal_variable_id variable variable'));
          };
        ]
      in
      (* 2y * (4z+3)^2 *)
      multiply polynomial_from_mono_without_replaced_variable pow_polynomial
    in
    (* 3x + 2yx^2 + y + 3 -> (3x + 2yx^2, y + 3) *)
    let modified_polynomial, base_polynomial =
      List.partition_tf polynomial ~f:(Monomial.has_variable ~variable)
    in
    (* 3x + 2yx^2 -> 3 * (4z+3) ; 2y * (4z+3)^2 *)
    let replaced_polynomial =
      List.map modified_polynomial ~f:(fun monomial ->
          replace_variable_by_polynomial ~monomial ~polynomial:by_polynomial ~variable)
    in
    (* (3 * (4z+3)) + (2y * (4z+3)^2) *)
    let merged_polynomial = fold1 replaced_polynomial ~f:add in
    (* (y + 3) + (3 * (4z+3) + 2y * (4z+3)^2) *)
    add base_polynomial merged_polynomial
end

open Record.Callable
module CallableParameter = Record.Callable.RecordParameter

module Primitive = struct
  type t = Identifier.t [@@deriving compare, eq, sexp, show, hash]

  include Hashable.Make (struct
    type t = Identifier.t [@@deriving compare, hash, sexp]
  end)

  module Set = Set.Make (struct
    type t = Identifier.t [@@deriving compare, sexp]
  end)

  let is_unit_test name =
    equal name "unittest.TestCase" || String.equal name "unittest.case.TestCase"
end

module T = struct
  type literal =
    | Boolean of bool
    | Integer of int
    | String of string
    | Bytes of string
    | EnumerationMember of {
        enumeration_type: t;
        member_name: Identifier.t;
      }

  and tuple =
    | Bounded of t Record.OrderedTypes.record
    | Unbounded of t

  and t =
    | Annotated of t
    | Bottom
    | Callable of t Record.Callable.record
    | Any
    | Literal of literal
    | NoneType
    | Parametric of {
        name: Identifier.t;
        parameters: t Record.Parameter.record list;
      }
    | ParameterVariadicComponent of
        Record.Variable.RecordVariadic.RecordParameters.RecordComponents.t
    | Primitive of Primitive.t
    | Top
    | Tuple of tuple
    | Union of t list
    | Variable of t Record.Variable.RecordUnary.record
    | IntExpression of t Polynomial.t
  [@@deriving compare, eq, sexp, show, hash]
end

include T

let _ = show (* shadowed below *)

type class_data = {
  instantiated: t;
  accessed_through_class: bool;
  class_name: Primitive.t;
}

type type_t = t [@@deriving compare, eq, sexp, show, hash]

let polynomial_to_type polynomial =
  match polynomial with
  | [] -> Literal (Integer 0)
  | [{ Polynomial.Monomial.variables = []; constant_factor }] -> Literal (Integer constant_factor)
  | [{ Polynomial.Monomial.variables = [{ degree = 1; variable }]; constant_factor = 1 }] ->
      Variable variable
  | _ -> IntExpression polynomial


let solve_less_or_equal_polynomial ~left ~right ~solve ~impossible =
  match left, right with
  | IntExpression polynomial, _ when Polynomial.is_base_case polynomial ->
      solve ~left:(polynomial_to_type polynomial) ~right
  | _, IntExpression polynomial when Polynomial.is_base_case polynomial ->
      solve ~left ~right:(polynomial_to_type polynomial)
  | ( IntExpression ({ Polynomial.Monomial.constant_factor; variables = [] } :: polynomial),
      Literal (Integer literal) ) ->
      solve ~left:(IntExpression polynomial) ~right:(Literal (Integer (literal - constant_factor)))
  | ( Literal (Integer literal),
      IntExpression ({ Polynomial.Monomial.constant_factor; variables = [] } :: polynomial) ) ->
      solve ~left:(Literal (Integer (literal - constant_factor))) ~right:(IntExpression polynomial)
  | IntExpression [{ Polynomial.Monomial.constant_factor; variables }], Literal (Integer literal)
    when constant_factor <> 1 ->
      if literal mod constant_factor <> 0 then
        impossible
      else
        solve
          ~left:(IntExpression [{ constant_factor = 1; variables }])
          ~right:(Literal (Integer (literal / constant_factor)))
  | Literal (Integer literal), IntExpression [{ Polynomial.Monomial.constant_factor; variables }]
    when constant_factor <> 1 ->
      if literal mod constant_factor <> 0 then
        impossible
      else
        solve
          ~left:(Literal (Integer (literal / constant_factor)))
          ~right:(IntExpression [{ Polynomial.Monomial.constant_factor = 1; variables }])
  | ( IntExpression (({ variables = []; _ } as left_monomial) :: left_polynomial_tail),
      IntExpression ({ variables = []; _ } :: _ as right_polynomial) ) ->
      let right_polynomial = Polynomial.subtract right_polynomial [left_monomial] in
      solve ~left:(IntExpression left_polynomial_tail) ~right:(IntExpression right_polynomial)
  | IntExpression _, Primitive _ -> solve ~left:(Primitive "int") ~right
  | _ -> impossible


let type_to_int_expression = function
  | Literal (Integer literal) -> Some (IntExpression (Polynomial.create_from_int literal))
  | Variable variable -> Some (IntExpression (Polynomial.create_from_variable variable))
  | IntExpression polynomial -> Some (IntExpression polynomial)
  | Primitive "int" -> Some (Primitive "int")
  | Any -> Some Any
  | _ -> None


module Map = Map.Make (T)

let default_to_bottom map keys =
  let to_bottom solution key =
    Map.update solution key ~f:(function
        | None -> Bottom
        | Some value -> value)
  in
  List.fold keys ~f:to_bottom ~init:map


module Set = Set.Make (T)
include Hashable.Make (T)

module Parameter = struct
  include Record.Parameter

  type t = type_t record [@@deriving compare, eq, sexp, show, hash]

  let all_singles parameters = List.map parameters ~f:is_single |> Option.all
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
      | Some key, [Single key_parameter; _] -> equal key key_parameter
      | _ -> true )
  | _ -> false


let is_dictionary_or_mapping = function
  | Parametric { name = "typing.Mapping" | "dict"; _ } -> true
  | _ -> false


let is_ellipsis = function
  | Primitive "ellipsis" -> true
  | _ -> false


let is_final = function
  | Parametric { name = "typing.Final" | "typing_extensions.Final"; _ } -> true
  | Primitive ("typing.Final" | "typing_extensions.Final") -> true
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


let is_list = function
  | Parametric { name = "list"; _ } -> true
  | _ -> false


let is_meta = function
  | Parametric { name = "type"; _ } -> true
  | _ -> false


let is_none = function
  | NoneType -> true
  | _ -> false


let is_noreturn = function
  | Primitive "typing.NoReturn" -> true
  | _ -> false


let is_object = function
  | Primitive "object" -> true
  | _ -> false


let is_optional = function
  | Union [NoneType; _]
  | Union [_; NoneType] ->
      true
  | Parametric { name = "typing.Optional" | "Optional"; _ } -> true
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
  | Parametric { name = "typing.Tuple" | "Tuple"; _ } -> true
  | _ -> false


let is_type_alias = function
  | Primitive "typing.TypeAlias" -> true
  | _ -> false


let is_unbound = function
  | Bottom -> true
  | _ -> false


let is_union = function
  | Union _ -> true
  | _ -> false


let is_falsy = function
  | NoneType
  | Literal (Boolean false)
  | Literal (Integer 0) ->
      true
  | _ -> false


let reverse_substitute name =
  match name with
  | "collections.defaultdict" -> "typing.DefaultDict"
  | "dict" -> "typing.Dict"
  | "list" -> "typing.List"
  | "set" -> "typing.Set"
  | "type" -> "typing.Type"
  | _ -> name


let parameter_variable_type_representation = function
  | { head = []; variable = { name; _ } } -> Primitive name
  | { head; variable = { name; _ } } ->
      let concretes = head @ [Primitive name] in
      Parametric
        {
          name = Record.OrderedTypes.RecordConcatenate.public_name;
          parameters = List.map concretes ~f:(fun concrete -> Record.Parameter.Single concrete);
        }


let show_callable_parameters ~pp_type = function
  | Record.Callable.Undefined -> "..."
  | ParameterVariadicTypeVariable variable ->
      parameter_variable_type_representation variable |> Format.asprintf "%a" pp_type
  | Defined parameters ->
      List.map parameters ~f:(CallableParameter.show_concise ~pp_type)
      |> String.concat ~sep:", "
      |> fun parameters -> Format.asprintf "[%s]" parameters


let pp_parameters ~pp_type format = function
  | [Record.Parameter.Group ordered] ->
      Format.fprintf format "%a" (Record.OrderedTypes.pp_concise ~pp_type) ordered
  | parameters
    when List.for_all parameters ~f:(function
             | Single parameter -> is_unbound parameter || is_top parameter
             | _ -> false) ->
      Format.fprintf format ""
  | parameters ->
      let s format = function
        | Record.Parameter.Single parameter -> Format.fprintf format "%a" pp_type parameter
        | Group ordered_types ->
            Format.fprintf format "[%a]" (Record.OrderedTypes.pp_concise ~pp_type) ordered_types
        | CallableParameters parameters ->
            Format.fprintf format "%s" (show_callable_parameters parameters ~pp_type)
      in
      Format.pp_print_list ~pp_sep:(fun format () -> Format.fprintf format ", ") s format parameters


let pp_typed_dictionary_field ~pp_type format { Record.TypedDictionary.name; annotation; required } =
  Format.fprintf format "%s%s: %a" name (if required then "" else "?") pp_type annotation


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
      let signature_to_string { annotation; parameters; _ } =
        Format.asprintf "%s, %a" (show_callable_parameters parameters ~pp_type:pp) pp annotation
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
  | Literal (Bytes literal) -> Format.fprintf format "typing_extensions.Literal[b'%s']" literal
  | Literal (EnumerationMember { enumeration_type; member_name }) ->
      Format.fprintf format "typing_extensions.Literal[%s.%s]" (show enumeration_type) member_name
  | NoneType -> Format.fprintf format "None"
  | Parametric { name; parameters } ->
      let name = reverse_substitute name in
      Format.fprintf format "%s[%a]" name (pp_parameters ~pp_type:pp) parameters
  | ParameterVariadicComponent component ->
      Record.Variable.RecordVariadic.RecordParameters.RecordComponents.pp_concise format component
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
  | Union [NoneType; parameter]
  | Union [parameter; NoneType] ->
      Format.fprintf format "typing.Optional[%a]" pp parameter
  | Union parameters ->
      Format.fprintf
        format
        "typing.Union[%s]"
        (List.map parameters ~f:show |> String.concat ~sep:", ")
  | Variable unary -> Record.Variable.RecordUnary.pp_concise format unary ~pp_type:pp
  | IntExpression polynomial when Polynomial.is_base_case polynomial ->
      pp format (polynomial_to_type polynomial)
  | IntExpression polynomial ->
      Format.fprintf format "pyre_extensions.IntExpression[%s]" (Polynomial.show_normal polynomial)


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
      let signature_to_string { annotation; parameters; _ } =
        let parameters =
          match parameters with
          | Undefined -> "..."
          | ParameterVariadicTypeVariable variable ->
              parameter_variable_type_representation variable |> Format.asprintf "%a" pp_concise
          | Defined parameters ->
              let parameter = function
                | CallableParameter.PositionalOnly { annotation; default; _ } ->
                    if default then
                      Format.asprintf "%a=..." pp_concise annotation
                    else
                      Format.asprintf "%a" pp_concise annotation
                | KeywordOnly { name; annotation; default }
                | Named { name; annotation; default } ->
                    let name = Identifier.sanitized name in
                    if default then
                      Format.asprintf "%s: %a = ..." name pp_concise annotation
                    else
                      Format.asprintf "%s: %a" name pp_concise annotation
                | Variable (Concrete annotation) -> Format.asprintf "*(%a)" pp_concise annotation
                | Variable (Concatenation concatenation) ->
                    Format.asprintf
                      "*(%a)"
                      (Record.OrderedTypes.RecordConcatenate.pp_concatenation ~pp_type:pp_concise)
                      concatenation
                | Keywords annotation -> Format.asprintf "**(%a)" pp_concise annotation
              in
              List.map parameters ~f:parameter |> String.concat ~sep:", "
        in
        Format.asprintf "(%s) -> %a" parameters pp_concise annotation
      in
      Format.fprintf format "%s" (signature_to_string implementation)
  | Any -> Format.fprintf format "Any"
  | Literal (Boolean literal) ->
      Format.fprintf format "typing_extensions.Literal[%s]" (if literal then "True" else "False")
  | Literal (Integer literal) -> Format.fprintf format "typing_extensions.Literal[%d]" literal
  | Literal (String literal) -> Format.fprintf format "typing_extensions.Literal['%s']" literal
  | Literal (Bytes literal) -> Format.fprintf format "typing_extensions.Literal[b'%s']" literal
  | Literal (EnumerationMember { enumeration_type; member_name }) ->
      Format.fprintf format "typing_extensions.Literal[%s.%s]" (show enumeration_type) member_name
  | NoneType -> Format.fprintf format "None"
  | Parametric { name; parameters } ->
      let name = strip_qualification (reverse_substitute name) in
      Format.fprintf format "%s[%a]" name (pp_parameters ~pp_type:pp) parameters
  | ParameterVariadicComponent component ->
      Record.Variable.RecordVariadic.RecordParameters.RecordComponents.pp_concise format component
  | Primitive name -> Format.fprintf format "%s" (strip_qualification name)
  | Top -> Format.fprintf format "unknown"
  | Tuple (Bounded parameters) ->
      Format.fprintf
        format
        "Tuple[%a]"
        (Record.OrderedTypes.pp_concise ~pp_type:pp_concise)
        parameters
  | Tuple (Unbounded parameter) -> Format.fprintf format "Tuple[%a, ...]" pp_concise parameter
  | Union [NoneType; parameter]
  | Union [parameter; NoneType] ->
      Format.fprintf format "Optional[%a]" pp_concise parameter
  | Union parameters -> Format.fprintf format "Union[%a]" pp_comma_separated parameters
  | Variable { variable; _ } -> Format.fprintf format "%s" (strip_qualification variable)
  | IntExpression polynomial when Polynomial.is_base_case polynomial ->
      pp_concise format (polynomial_to_type polynomial)
  | IntExpression polynomial ->
      Format.fprintf
        format
        "pyre_extensions.IntExpression[%s]"
        (Polynomial.show_normal polynomial ~concise:true)


and show_concise annotation = Format.asprintf "%a" pp_concise annotation

let show_for_hover annotation =
  match annotation with
  | Callable { kind = Named reference; _ } ->
      (* add def [function name] : ... to provide better syntax highlighting for hover *)
      Format.asprintf "def %s%s: ..." (Reference.last reference) (show_concise annotation)
  | _ -> show_concise annotation


let serialize = function
  | Bottom -> "$bottom"
  | annotation -> Format.asprintf "%a" pp annotation


let parametric name parameters = Parametric { name; parameters }

let rec annotated annotation =
  match annotation with
  | Annotated annotation -> annotated annotation
  | _ -> Annotated annotation


let awaitable parameter = Parametric { name = "typing.Awaitable"; parameters = [Single parameter] }

let coroutine parameters = Parametric { name = "typing.Coroutine"; parameters }

let bool = Primitive "bool"

let bytes = Primitive "bytes"

let complex = Primitive "complex"

let dictionary ~key ~value = Parametric { name = "dict"; parameters = [Single key; Single value] }

let mapping_primitive = "typing.Mapping"

let enumeration = Primitive "enum.Enum"

let float = Primitive "float"

let number = Primitive "numbers.Number"

let generator ?(async = false) parameter =
  if async then
    Parametric { name = "typing.AsyncGenerator"; parameters = [Single parameter; Single NoneType] }
  else
    Parametric
      {
        name = "typing.Generator";
        parameters = [Single parameter; Single NoneType; Single NoneType];
      }


let generic_primitive = Primitive "typing.Generic"

let integer = Primitive "int"

let literal_integer literal = Literal (Integer literal)

let iterable parameter = Parametric { name = "typing.Iterable"; parameters = [Single parameter] }

let iterator parameter = Parametric { name = "typing.Iterator"; parameters = [Single parameter] }

let async_iterator parameter =
  Parametric { name = "typing.AsyncIterator"; parameters = [Single parameter] }


let list parameter = Parametric { name = "list"; parameters = [Single parameter] }

let meta annotation = Parametric { name = "type"; parameters = [Single annotation] }

let named_tuple = Primitive "typing.NamedTuple"

let none = NoneType

let object_primitive = Primitive "object"

let sequence parameter = Parametric { name = "typing.Sequence"; parameters = [Single parameter] }

let set parameter = Parametric { name = "set"; parameters = [Single parameter] }

let string = Primitive "str"

let literal_string literal = Literal (String literal)

let literal_bytes literal = Literal (Bytes literal)

let tuple parameters = Tuple (Bounded (Concrete parameters))

let union parameters =
  let parameters =
    let parameter_set = Hash_set.create () in
    let rec add_parameter = function
      | Union parameters -> List.iter parameters ~f:add_parameter
      | Bottom -> ()
      | parameter -> Base.Hash_set.add parameter_set parameter
    in
    List.iter parameters ~f:add_parameter;
    Base.Hash_set.to_list parameter_set |> List.sort ~compare
  in
  if List.exists ~f:is_top parameters then
    Top
  else if List.exists ~f:is_any parameters then
    Any
  else
    match parameters with
    | [] -> Bottom
    | [parameter] -> parameter
    | parameters -> Union parameters


let optional parameter =
  match parameter with
  | Top -> Top
  | Any -> Any
  | Bottom -> Bottom
  | _ -> union [NoneType; parameter]


let variable ?constraints ?variance name =
  Variable (Record.Variable.RecordUnary.create ?constraints ?variance name)


let yield parameter = Parametric { name = "Yield"; parameters = [Single parameter] }

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
    "pyre_extensions.Generic", "typing.Generic";
  ]
  |> Identifier.Table.of_alist_exn


let are_fields_total = List.for_all ~f:(fun { Record.TypedDictionary.required; _ } -> required)

let rec expression annotation =
  let location = Location.any in
  let create_name name = Expression.Name (create_name ~location name) in
  let get_item_call = get_item_call ~location in
  let callable_parameters_expression = function
    | Defined parameters ->
        let convert_parameter parameter =
          let call ?(default = false) ?name kind annotation =
            let arguments =
              let annotation = [{ Call.Argument.name = None; value = annotation }] in
              let default =
                if default then
                  [
                    {
                      Call.Argument.name = None;
                      value = Node.create ~location (create_name "default");
                    };
                  ]
                else
                  []
              in
              let name =
                name
                >>| (fun name ->
                      [
                        {
                          Call.Argument.name = None;
                          value = Node.create ~location (create_name name);
                        };
                      ])
                |> Option.value ~default:[]
              in
              name @ annotation @ default
            in
            Expression.Call
              { callee = Node.create ~location (Expression.Name (Name.Identifier kind)); arguments }
            |> Node.create ~location
          in
          match parameter with
          | CallableParameter.PositionalOnly { annotation; default; _ } ->
              call ~default "PositionalOnly" (expression annotation)
          | Keywords annotation -> call "Keywords" (expression annotation)
          | Named { name; annotation; default } ->
              call ~default ~name "Named" (expression annotation)
          | KeywordOnly { name; annotation; default } ->
              call ~default ~name "KeywordOnly" (expression annotation)
          | Variable (Concrete annotation) -> call "Variable" (expression annotation)
          | Variable (Concatenation concatenation) ->
              call "Variable" (concatenation_expression concatenation)
        in
        Expression.List (List.map ~f:convert_parameter parameters) |> Node.create ~location
    | Undefined -> Node.create ~location Expression.Ellipsis
    | ParameterVariadicTypeVariable variable ->
        parameter_variable_type_representation variable |> expression
  in
  let rec convert_annotation annotation =
    match annotation with
    | Annotated annotation -> get_item_call "typing.Annotated" [expression annotation]
    | Bottom -> create_name "$bottom"
    | Callable { implementation; overloads; _ } -> (
        let convert_signature { annotation; parameters; _ } =
          {
            Call.Argument.name = None;
            value =
              Node.create
                ~location
                (Expression.Tuple [callable_parameters_expression parameters; expression annotation]);
          }
        in
        let base_callable =
          Expression.Call
            {
              callee =
                {
                  Node.location;
                  value =
                    Name
                      (Name.Attribute
                         {
                           base = { Node.location; value = create_name "typing.Callable" };
                           attribute = "__getitem__";
                           special = true;
                         });
                };
              arguments = [convert_signature implementation];
            }
        in
        let overloads =
          let convert_overload sofar overload =
            match sofar with
            | None ->
                Expression.Call
                  {
                    callee = { Node.location; value = Name (Name.Identifier "__getitem__") };
                    arguments = [convert_signature overload];
                  }
                |> Node.create ~location
                |> Option.some
            | Some expression ->
                Expression.Call
                  {
                    callee =
                      {
                        Node.location;
                        value =
                          Name
                            (Name.Attribute
                               { base = expression; attribute = "__getitem__"; special = true });
                      };
                    arguments = [convert_signature overload];
                  }
                |> Node.create ~location
                |> Option.some
          in
          List.fold ~init:None ~f:convert_overload overloads
        in
        match overloads with
        | Some overloads ->
            Expression.Call
              {
                callee =
                  {
                    Node.location;
                    value =
                      Name
                        (Name.Attribute
                           {
                             base = { Node.location; value = base_callable };
                             attribute = "__getitem__";
                             special = true;
                           });
                  };
                arguments = [{ Call.Argument.name = None; value = overloads }];
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
          | Bytes literal -> Expression.String { value = literal; kind = StringLiteral.Bytes }
          | EnumerationMember { enumeration_type; member_name } ->
              Expression.Name
                (Attribute
                   { base = expression enumeration_type; attribute = member_name; special = false })
        in
        get_item_call "typing_extensions.Literal" [Node.create ~location literal]
    | NoneType -> create_name "None"
    | Parametric { name; parameters } ->
        let parameters =
          let expression_of_ordered = function
            | Record.OrderedTypes.Any -> [expression (Primitive "...")]
            | Concrete parameters -> List.map ~f:expression parameters
            | Concatenation concatenation -> [concatenation_expression concatenation]
          in
          let expression_of_parameter = function
            | Record.Parameter.Group ordered ->
                Node.create ~location (Expression.List (expression_of_ordered ordered))
            | Single single -> expression single
            | CallableParameters parameters -> callable_parameters_expression parameters
          in
          match parameters with
          | [Group ordered] -> expression_of_ordered ordered
          | parameters -> List.map parameters ~f:expression_of_parameter
        in
        get_item_call (reverse_substitute name) parameters
    | ParameterVariadicComponent { component; variable_name; _ } ->
        let attribute =
          Record.Variable.RecordVariadic.RecordParameters.RecordComponents.component_name component
        in
        Expression.Name
          (Attribute { base = expression (Primitive variable_name); attribute; special = false })
    | Primitive name -> create_name name
    | Top -> create_name "$unknown"
    | Tuple (Bounded (Concrete [])) ->
        get_item_call "typing.Tuple" [Node.create ~location (Expression.Tuple [])]
    | Tuple elements ->
        let parameters =
          match elements with
          | Bounded Any -> [expression (Primitive "...")]
          | Bounded (Concrete parameters) -> List.map ~f:expression parameters
          | Bounded (Concatenation concatenation) -> [concatenation_expression concatenation]
          | Unbounded parameter -> List.map ~f:expression [parameter; Primitive "..."]
        in
        get_item_call "typing.Tuple" parameters
    | Union [NoneType; parameter]
    | Union [parameter; NoneType] ->
        get_item_call "typing.Optional" [expression parameter]
    | Union parameters -> get_item_call "typing.Union" (List.map ~f:expression parameters)
    | Variable { variable; _ } -> create_name variable
    | IntExpression polynomial when Polynomial.is_base_case polynomial ->
        convert_annotation (polynomial_to_type polynomial)
    | IntExpression polynomial ->
        let convert_int_expression arguments ~operator =
          Expression.Call
            {
              callee =
                Node.create_with_default_location
                  (Expression.Name
                     (Name.Attribute
                        {
                          base =
                            { Node.location; value = create_name ("pyre_extensions." ^ operator) };
                          attribute = "__getitem__";
                          special = true;
                        }));
              arguments =
                [
                  {
                    Call.Argument.name = None;
                    value =
                      Node.create_with_default_location
                        (Expression.Tuple (List.map arguments ~f:Node.create_with_default_location));
                  };
                ];
            }
        in
        let convert_monomial { Polynomial.Monomial.constant_factor; variables } =
          let constant_factor = convert_annotation (Literal (Integer constant_factor)) in
          let variables =
            List.map variables ~f:(fun { variable; degree } ->
                let variable = convert_annotation (Variable variable) in
                if degree = 1 then
                  variable
                else
                  let arguments =
                    List.init degree ~f:(fun _ -> 0) |> List.map ~f:(fun _ -> variable)
                  in
                  convert_int_expression ~operator:"Multiply" arguments)
          in
          if List.length variables = 0 then
            constant_factor
          else
            convert_int_expression ~operator:"Multiply" (constant_factor :: variables)
        in
        convert_int_expression ~operator:"Add" (List.map polynomial ~f:convert_monomial)
  in
  let value =
    match annotation with
    | Primitive "..." -> Expression.Ellipsis
    | _ -> convert_annotation annotation
  in
  Node.create_with_default_location value


and middle_annotation middle =
  let single_wrap ~mapper ~inner =
    Parametric
      {
        name = Record.OrderedTypes.map_public_name;
        parameters = [Single (Primitive mapper); Single inner];
      }
  in
  match middle with
  | { Record.OrderedTypes.RecordConcatenate.Middle.variable = { name; _ }; mappers = [] } ->
      Primitive name
  | { mappers = head_mapper :: tail_mappers; _ } ->
      let inner = { middle with mappers = tail_mappers } in
      single_wrap ~mapper:head_mapper ~inner:(middle_annotation inner)


and concatenation_expression { middle; wrapping } =
  let concatenation_annotation =
    let middle_annotation = middle_annotation middle in
    match wrapping with
    | { head = []; tail = [] } -> middle_annotation
    | { head; tail } ->
        let concretes = head @ (middle_annotation :: tail) in
        Parametric
          {
            name = Record.OrderedTypes.RecordConcatenate.public_name;
            parameters = List.map concretes ~f:(fun concrete -> Record.Parameter.Single concrete);
          }
  in
  concatenation_annotation |> expression


module Transform = struct
  type 'state visit_result = {
    transformed_annotation: t;
    new_state: 'state;
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
        let visit_all = List.map ~f:(visit_annotation ~state) in
        let visit_concatenation
            { Record.OrderedTypes.RecordConcatenate.middle; wrapping = { head; tail } }
          =
          let wrapping =
            {
              Record.OrderedTypes.RecordConcatenate.head =
                List.map head ~f:(visit_annotation ~state);
              tail = List.map tail ~f:(visit_annotation ~state);
            }
          in
          { Record.OrderedTypes.RecordConcatenate.middle; wrapping }
        in
        let visit_ordered_types ordered_types =
          match ordered_types with
          | Record.OrderedTypes.Any -> ordered_types
          | Concrete concretes -> Concrete (visit_all concretes)
          | Concatenation concatenation -> Concatenation (visit_concatenation concatenation)
        in
        let visit_parameters parameter =
          let visit_defined = function
            | RecordParameter.Named ({ annotation; _ } as named) ->
                RecordParameter.Named { named with annotation = visit_annotation annotation ~state }
            | RecordParameter.KeywordOnly ({ annotation; _ } as named) ->
                RecordParameter.KeywordOnly
                  { named with annotation = visit_annotation annotation ~state }
            | RecordParameter.Variable (Concrete annotation) ->
                RecordParameter.Variable (Concrete (visit_annotation annotation ~state))
            | RecordParameter.Variable (Concatenation concatenation) ->
                Variable (Concatenation (visit_concatenation concatenation))
            | RecordParameter.Keywords annotation ->
                RecordParameter.Keywords (visit_annotation annotation ~state)
            | RecordParameter.PositionalOnly ({ annotation; _ } as anonymous) ->
                RecordParameter.PositionalOnly
                  { anonymous with annotation = visit_annotation annotation ~state }
          in
          match parameter with
          | Defined defined -> Defined (List.map defined ~f:visit_defined)
          | ParameterVariadicTypeVariable { head; variable } ->
              ParameterVariadicTypeVariable
                { head = List.map head ~f:(visit_annotation ~state); variable }
          | parameter -> parameter
        in
        match annotation with
        | NoneType -> NoneType
        | Annotated annotation -> Annotated (visit_annotation annotation ~state)
        | Callable ({ implementation; overloads; _ } as callable) ->
            let open Record.Callable in
            let visit_overload ({ annotation; parameters; _ } as overload) =
              {
                overload with
                annotation = visit_annotation annotation ~state;
                parameters = visit_parameters parameters;
              }
            in
            Callable
              {
                callable with
                implementation = visit_overload implementation;
                overloads = List.map overloads ~f:visit_overload;
              }
        | Parametric { name; parameters } ->
            let visit = function
              | Record.Parameter.Group ordered ->
                  Record.Parameter.Group (visit_ordered_types ordered)
              | Single single -> Single (visit_annotation single ~state)
              | CallableParameters parameters -> CallableParameters (visit_parameters parameters)
            in
            Parametric { name; parameters = List.map parameters ~f:visit }
        | Tuple (Bounded ordered) -> Tuple (Bounded (visit_ordered_types ordered))
        | Tuple (Unbounded annotation) -> Tuple (Unbounded (visit_annotation annotation ~state))
        | Union annotations -> union (List.map annotations ~f:(visit_annotation ~state))
        | Variable ({ constraints; _ } as variable) ->
            let constraints =
              match constraints with
              | Record.Variable.Bound bound -> Record.Variable.Bound (visit_annotation bound ~state)
              | Explicit constraints -> Explicit (List.map constraints ~f:(visit_annotation ~state))
              | Unconstrained -> Unconstrained
              | LiteralIntegers -> LiteralIntegers
            in
            Variable { variable with constraints }
        | Literal (EnumerationMember ({ enumeration_type; _ } as enumeration_member)) ->
            Literal
              (EnumerationMember
                 {
                   enumeration_member with
                   enumeration_type = visit_annotation ~state enumeration_type;
                 })
        | ParameterVariadicComponent _
        | Literal _
        | Bottom
        | Top
        | Any
        | IntExpression _
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


let contains_callable annotation = exists annotation ~predicate:is_callable

let contains_any annotation = exists annotation ~predicate:is_any

let contains_unknown annotation = exists annotation ~predicate:is_top

let pp_type = pp

module Callable = struct
  module Parameter = struct
    include Record.Callable.RecordParameter

    type parameter = type_t t [@@deriving compare, eq, sexp, show, hash]

    module Map = Core.Map.Make (struct
      type t = parameter [@@deriving compare, sexp]
    end)

    let create parameters =
      let parameter index (keyword_only, sofar) { name; annotation; default } =
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
                  CallableParameter.PositionalOnly { index; annotation; default }
                else
                  let named = { name; annotation; default } in
                  if keyword_only then
                    KeywordOnly named
                  else
                    Named named
          in
          keyword_only, new_parameter :: sofar
      in
      let add_positional_only index (positional_only, sofar) parameter =
        match parameter with
        | Named { name; _ } when String.equal (Identifier.sanitized name) "/" -> true, sofar
        | Named { annotation; default; _ } when positional_only ->
            let index = List.length parameters - 1 - index in
            positional_only, PositionalOnly { index; annotation; default } :: sofar
        | _ -> positional_only, parameter :: sofar
      in
      List.foldi parameters ~f:parameter ~init:(false, [])
      |> snd
      |> List.foldi ~f:add_positional_only ~init:(false, [])
      |> snd


    let show_concise = show_concise ~pp_type

    let default = function
      | PositionalOnly { default; _ }
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
      | _, PositionalOnly _
      | PositionalOnly _, _ ->
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

    let is_undefined { parameters; annotation; _ } =
      match parameters with
      | Undefined -> contains_unknown annotation
      | _ -> false
  end

  let from_overloads overloads =
    match overloads with
    | ({ kind = Named _; _ } as initial) :: overloads ->
        let fold sofar signature =
          match sofar, signature with
          | Some sofar, { kind; implementation; overloads } ->
              if equal_kind kind sofar.kind then
                Some { kind; implementation; overloads = sofar.overloads @ overloads }
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
    map { kind = Anonymous; implementation; overloads = [] } ~f
    |> function
    | Some { implementation; _ } -> implementation
    | _ -> failwith "f did not return a callable"


  let map_parameters ({ implementation; overloads; _ } as callable) ~f =
    let for_implementation ({ parameters; _ } as implementation) =
      { implementation with parameters = f parameters }
    in
    {
      callable with
      implementation = for_implementation implementation;
      overloads = List.map overloads ~f:for_implementation;
    }


  let map_annotation ({ implementation; overloads; _ } as callable) ~f =
    let for_implementation ({ annotation; _ } as implementation) =
      { implementation with annotation = f annotation }
    in
    {
      callable with
      implementation = for_implementation implementation;
      overloads = List.map overloads ~f:for_implementation;
    }


  let with_return_annotation ({ implementation; overloads; _ } as initial) ~annotation =
    let re_annotate implementation = { implementation with annotation } in
    {
      initial with
      implementation = re_annotate implementation;
      overloads = List.map ~f:re_annotate overloads;
    }


  let create ?name ?(overloads = []) ?(parameters = Undefined) ~annotation () =
    let kind = name >>| (fun name -> Named name) |> Option.value ~default:Anonymous in
    Callable { kind; implementation = { annotation; parameters }; overloads }


  let create_from_implementation implementation =
    create ~parameters:implementation.parameters ~annotation:implementation.annotation ()


  let prepend_anonymous_parameters ~head ~tail =
    let make_anonymous annotation =
      Parameter.PositionalOnly { index = 0; annotation; default = false }
    in
    let correct_indices index = function
      | Parameter.PositionalOnly anonymous -> Parameter.PositionalOnly { anonymous with index }
      | parameter -> parameter
    in
    let head = List.map head ~f:make_anonymous in
    List.mapi ~f:correct_indices (head @ tail)
end

let lambda ~parameters ~return_annotation =
  let parameters =
    List.map parameters ~f:(fun (name, annotation) ->
        { CallableParameter.name; annotation; default = false })
    |> Callable.Parameter.create
  in
  Callable
    {
      kind = Anonymous;
      implementation = { annotation = return_annotation; parameters = Defined parameters };
      overloads = [];
    }


let primitive_substitution_map =
  [
    "$bottom", Bottom;
    "$unknown", Top;
    "None", none;
    "function", Callable.create ~annotation:Any ();
    "typing.Any", Any;
    "typing.ChainMap", Primitive "collections.ChainMap";
    "typing.Counter", Primitive "collections.Counter";
    "typing.DefaultDict", Primitive "collections.defaultdict";
    "typing.Deque", Primitive "collections.deque";
    "typing.Dict", Primitive "dict";
    "typing.List", Primitive "list";
    "typing.OrderedDict", Primitive "collections.OrderedDict";
    "typing.Tuple", Primitive "tuple";
    "typing.Type", Primitive "type";
    "typing_extensions.Protocol", Primitive "typing.Protocol";
    (* This is broken in typeshed:
       https://github.com/python/typeshed/pull/991#issuecomment-288160993 *)
    "PathLike", Primitive "_PathLike";
    "TSelf", variable "_PathLike";
  ]
  |> Identifier.Table.of_alist_exn


let primitive_name = function
  | Primitive name -> Some name
  | _ -> None


let create_concatenation_operator_from_annotation annotation ~variable_aliases =
  let create_map_operator_from_annotation annotation =
    match annotation with
    | Parametric
        {
          name;
          parameters = [Single (Primitive left_parameter); Group (Concatenation right_parameter)];
        }
      when Identifier.equal name Record.OrderedTypes.map_public_name ->
        let open Record.OrderedTypes.RecordConcatenate in
        unwrap_if_only_middle right_parameter
        >>= Middle.unwrap_if_bare
        >>| fun variable ->
        { Record.OrderedTypes.RecordConcatenate.Middle.variable; mappers = [left_parameter] }
    | _ -> None
  in
  match annotation with
  | Parametric { name; parameters } -> (
      match Identifier.equal name Record.OrderedTypes.RecordConcatenate.public_name with
      | true -> (
          let parse_as_middle = function
            | Record.Parameter.Group (Concatenation potential_middle) ->
                let open Record.OrderedTypes.RecordConcatenate in
                unwrap_if_only_middle potential_middle
            | CallableParameters _
            | Group (Concrete _)
            | Group Any ->
                None
            | Record.Parameter.Single potentially_a_map ->
                create_map_operator_from_annotation potentially_a_map
          in
          let parameter_to_parsed =
            List.map parameters ~f:(fun parameter ->
                Record.Parameter.is_single parameter, parse_as_middle parameter)
          in
          let head, middle_and_tail =
            List.split_while parameter_to_parsed ~f:(fun (_, parsed) -> Option.is_none parsed)
          in
          let middle, tail =
            List.split_while middle_and_tail ~f:(fun (_, parsed) -> Option.is_some parsed)
          in
          let fsts = List.map ~f:fst in
          let head = fsts head in
          let tail = fsts tail in
          match Option.all head, middle, Option.all tail with
          | Some head, [(_, Some middle)], Some tail ->
              Some { Record.OrderedTypes.RecordConcatenate.middle; wrapping = { head; tail } }
          | _ -> None )
      | _ ->
          create_map_operator_from_annotation annotation
          >>| fun map -> Record.OrderedTypes.RecordConcatenate.empty_wrap map )
  | Primitive name -> (
      match variable_aliases name with
      | Some (Record.Variable.ListVariadic variable) ->
          Some
            (Record.OrderedTypes.RecordConcatenate.empty_wrap
               { Record.OrderedTypes.RecordConcatenate.Middle.variable; mappers = [] })
      | _ -> None )
  | _ -> None


let create_literal = function
  | Expression.True -> Some (Literal (Boolean true))
  | Expression.False -> Some (Literal (Boolean false))
  | Expression.Integer literal -> Some (Literal (Integer literal))
  | Expression.String { StringLiteral.kind = StringLiteral.String; value } ->
      Some (Literal (String value))
  | Expression.String { StringLiteral.kind = StringLiteral.Bytes; value } ->
      Some (Literal (Bytes value))
  | Expression.Name
      (Attribute { base = { Node.value = Expression.Name base_name; _ }; attribute; _ }) -> (
      match name_to_reference base_name with
      | Some reference ->
          Some
            (Literal
               (EnumerationMember
                  {
                    enumeration_type = Primitive (Reference.show_sanitized reference);
                    member_name = attribute;
                  }))
      | _ -> None )
  | Expression.Name (Identifier "None") -> Some none
  | _ -> None


type alias =
  | TypeAlias of t
  | VariableAlias of t Record.Variable.record
[@@deriving compare, eq, sexp, show, hash]

let rec create_logic ~aliases ~variable_aliases { Node.value = expression; _ } =
  let substitute_ordered_types = function
    | Primitive "..." -> Some Record.OrderedTypes.Any
    | parameter ->
        create_concatenation_operator_from_annotation parameter ~variable_aliases
        >>| fun concatenation -> Record.OrderedTypes.Concatenation concatenation
  in
  let substitute_parameter_variadic = function
    | Primitive name -> (
        match variable_aliases name with
        | Some (ParameterVariadic variable) -> Some { Record.Callable.variable; head = [] }
        | _ -> None )
    | Parametric { name; parameters }
      when Identifier.equal name Record.OrderedTypes.RecordConcatenate.public_name -> (
        match List.rev parameters with
        | Parameter.CallableParameters (ParameterVariadicTypeVariable { variable; head = [] })
          :: reversed_head ->
            Parameter.all_singles reversed_head
            >>| List.rev
            >>| fun head -> { Record.Callable.variable; head }
        | _ -> None )
    | _ -> None
  in

  let result =
    let create_logic = create_logic ~aliases ~variable_aliases in
    let resolve_aliases annotation =
      let visited = Hash_set.create () in
      let module ResolveTransform = Transform.Make (struct
        type state = unit

        let visit_children_before _ _ = false

        let visit_children_after = true

        let visit _ annotation =
          let rec resolve annotation =
            if Core.Hash_set.mem visited annotation then
              annotation
            else (
              Core.Hash_set.add visited annotation;
              match aliases annotation, annotation with
              | Some aliased, _ ->
                  (* We need to fully resolve aliases to aliases before we go on to resolve the
                     aliases those may contain *)
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
      | Expression.Name
          (Name.Attribute
            {
              base = { Node.value = Name (Name.Identifier "typing"); _ };
              attribute = "Callable";
              _;
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
          | Expression.Call { callee; arguments } when name_is ~name:"typing.Callable" callee ->
              Some arguments, implementation_argument, overloads_argument
          | Name
              (Name.Attribute
                {
                  base = { Node.value = Name (Name.Identifier "typing"); _ };
                  attribute = "Callable";
                  _;
                }) ->
              None, implementation_argument, overloads_argument
          | _ ->
              (* Invalid base. *)
              None, None, None
        in
        match expression with
        | Expression.Call
            {
              callee =
                {
                  Node.value =
                    Name
                      (Name.Attribute
                        {
                          base =
                            {
                              Node.value =
                                Call
                                  {
                                    callee =
                                      {
                                        Node.value =
                                          Name
                                            (Name.Attribute { base; attribute = "__getitem__"; _ });
                                        _;
                                      };
                                    arguments = [{ Call.Argument.value = argument; _ }];
                                  };
                              _;
                            };
                          attribute = "__getitem__";
                          _;
                        });
                  _;
                };
              arguments = [{ Call.Argument.value = overloads_argument; _ }];
            } ->
            (* Overloads are provided *)
            get_from_base base (Some argument) (Some overloads_argument)
        | Call
            {
              callee =
                { Node.value = Name (Name.Attribute { base; attribute = "__getitem__"; _ }); _ };
              arguments = [{ Call.Argument.value = argument; _ }];
            } ->
            (* No overloads provided *)
            get_from_base base (Some argument) None
        | _ -> None, None, None
      in
      let kind =
        match modifiers with
        | Some ({ Call.Argument.value = { Node.value = Expression.Name name; _ }; _ } :: _) ->
            Ast.Expression.name_to_reference name
            >>| (fun name -> Named name)
            |> Option.value ~default:Anonymous
        | Some
            ({
               Call.Argument.value =
                 { Node.value = Expression.String { StringLiteral.value; _ }; _ };
               _;
             }
            :: _) ->
            Named (Reference.create value)
        | _ -> Anonymous
      in
      let undefined = { annotation = Top; parameters = Undefined } in
      let get_signature = function
        | Expression.Tuple [parameters; annotation] ->
            let parameters =
              let parse_as_variadic parsed_parameter =
                create_concatenation_operator_from_annotation parsed_parameter ~variable_aliases
                >>| fun concatenation -> CallableParameter.Concatenation concatenation
              in
              let extract_parameter index parameter =
                match Node.value parameter with
                | Expression.Call
                    { callee = { Node.value = Name (Name.Identifier name); _ }; arguments } -> (
                    let arguments =
                      List.map arguments ~f:(fun { Call.Argument.value; _ } -> Node.value value)
                    in
                    match name, arguments with
                    | "PositionalOnly", annotation :: tail ->
                        let default =
                          match tail with
                          | [Name (Name.Identifier "default")] -> true
                          | _ -> false
                        in
                        CallableParameter.PositionalOnly
                          {
                            index;
                            annotation = create_logic (Node.create_with_default_location annotation);
                            default;
                          }
                    | "Named", Name (Name.Identifier name) :: annotation :: tail ->
                        let default =
                          match tail with
                          | [Name (Name.Identifier "default")] -> true
                          | _ -> false
                        in
                        Named
                          {
                            name;
                            annotation = create_logic (Node.create_with_default_location annotation);
                            default;
                          }
                    | "KeywordOnly", Name (Name.Identifier name) :: annotation :: tail ->
                        let default =
                          match tail with
                          | [Name (Name.Identifier "default")] -> true
                          | _ -> false
                        in
                        KeywordOnly
                          {
                            name;
                            annotation = create_logic (Node.create_with_default_location annotation);
                            default;
                          }
                    | "Variable", tail ->
                        let annotation =
                          match tail with
                          | annotation :: _ ->
                              create_logic (Node.create_with_default_location annotation)
                          | _ -> Top
                        in
                        parse_as_variadic annotation
                        |> Option.value ~default:(CallableParameter.Concrete annotation)
                        |> fun variable -> CallableParameter.Variable variable
                    | "Keywords", tail ->
                        let annotation =
                          match tail with
                          | annotation :: _ ->
                              create_logic (Node.create_with_default_location annotation)
                          | _ -> Top
                        in
                        Keywords annotation
                    | _ -> PositionalOnly { index; annotation = Top; default = false } )
                | _ ->
                    PositionalOnly { index; annotation = create_logic parameter; default = false }
              in
              match Node.value parameters with
              | List parameters -> Defined (List.mapi ~f:extract_parameter parameters)
              | _ -> (
                  let parsed = create_logic parameters in
                  match substitute_parameter_variadic parsed with
                  | Some variable -> ParameterVariadicTypeVariable variable
                  | _ -> (
                      match parse_as_variadic parsed with
                      | Some variadic -> Defined [CallableParameter.Variable variadic]
                      | None -> Undefined ) )
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
          | Expression.List arguments -> [get_signature (Tuple arguments)]
          | Call
              {
                callee = { Node.value = Name (Name.Identifier "__getitem__"); _ };
                arguments = [{ Call.Argument.value = argument; _ }];
              } ->
              [get_signature (Node.value argument)]
          | Call
              {
                callee =
                  { Node.value = Name (Name.Attribute { base; attribute = "__getitem__"; _ }); _ };
                arguments = [{ Call.Argument.value = argument; _ }];
              } ->
              get_signature (Node.value argument) :: parse_overloads (Node.value base)
          | _ -> [undefined]
        in
        match overload_signatures with
        | Some signatures -> List.rev (parse_overloads (Node.value signatures))
        | None -> []
      in
      Callable { kind; implementation; overloads }
    in
    let create_parametric ~base ~argument =
      let parametric name =
        let parameters =
          let parse_parameter = function
            | { Node.value = Expression.List elements; _ } ->
                let concrete = List.map elements ~f:create_logic in
                Record.Parameter.Group (Concrete concrete)
            | element -> (
                let parsed = create_logic element in
                match substitute_ordered_types parsed with
                | Some ordered -> Record.Parameter.Group ordered
                | None -> (
                    match substitute_parameter_variadic parsed with
                    | Some variable -> CallableParameters (ParameterVariadicTypeVariable variable)
                    | _ -> Record.Parameter.Single parsed ) )
          in
          match argument with
          | { Node.value = Expression.Tuple elements; _ } -> List.map elements ~f:parse_parameter
          | element -> [parse_parameter element]
        in
        Parametric { name; parameters } |> resolve_aliases
      in
      match create_logic base, Node.value base with
      | Primitive name, _ -> parametric name
      | _, Name _ -> parametric (Expression.show base)
      | _ -> Top
    in
    let create_int_expression_from_arguments arguments ~operation =
      let arguments = List.map arguments ~f:create_logic |> List.map ~f:type_to_int_expression in
      if List.exists arguments ~f:Option.is_none || List.length arguments < 2 then
        Top
      else
        let arguments = List.filter_map arguments ~f:Fn.id in
        let operation, identity_polynomial =
          match operation with
          | `Add -> Polynomial.add, IntExpression (Polynomial.create_from_int 0)
          | `Multiply -> Polynomial.multiply, IntExpression (Polynomial.create_from_int 1)
        in
        let merge_expression left right =
          match left, right with
          | _, Any -> Any
          | left, Primitive "int" when not (T.equal left Any) -> right
          | IntExpression left, IntExpression right -> IntExpression (operation left right)
          | _, _ -> left
        in
        let int_expression = List.fold arguments ~init:identity_polynomial ~f:merge_expression in
        int_expression
    in
    match expression with
    | Call
        {
          callee;
          arguments =
            { Call.Argument.value = { Node.value = String { StringLiteral.value; _ }; _ }; _ }
            :: arguments;
        }
      when name_is ~name:"typing.TypeVar" callee ->
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
              }
              when String.equal (Identifier.sanitized name) "covariant" ->
                Some Record.Variable.Covariant
            | {
                Call.Argument.name = Some { Node.value = name; _ };
                value = { Node.value = True; _ };
              }
              when String.equal (Identifier.sanitized name) "contravariant" ->
                Some Contravariant
            | _ -> None
          in
          List.find_map arguments ~f:variance_definition
          |> Option.value ~default:Record.Variable.Invariant
        in
        variable value ~constraints ~variance
    | Call
        {
          callee;
          arguments =
            [{ Call.Argument.value = { Node.value = String { StringLiteral.value; _ }; _ }; _ }];
        }
      when name_is ~name:"typing_extensions.IntVar" callee ->
        variable value ~constraints:LiteralIntegers
    | Call
        {
          callee =
            { Node.value = Name (Name.Attribute { base; attribute = "__getitem__"; _ }); _ } as
            callee;
          arguments =
            [
              {
                Call.Argument.name = None;
                value = { Node.value = Expression.Tuple arguments; _ } as argument;
                _;
              };
            ];
        }
      when name_is ~name:"pyre_extensions.Add.__getitem__" callee ->
        let created_type = create_int_expression_from_arguments arguments ~operation:`Add in
        ( match created_type with
        | Top -> create_parametric ~base ~argument
        | _ -> created_type )
        |> resolve_aliases
    | Call
        {
          callee =
            { Node.value = Name (Name.Attribute { base; attribute = "__getitem__"; _ }); _ } as
            callee;
          arguments =
            [
              {
                Call.Argument.name = None;
                value = { Node.value = Expression.Tuple arguments; _ } as argument;
                _;
              };
            ];
        }
      when name_is ~name:"pyre_extensions.Multiply.__getitem__" callee ->
        let created_type = create_int_expression_from_arguments arguments ~operation:`Multiply in
        ( match created_type with
        | Top -> create_parametric ~base ~argument
        | _ -> created_type )
        |> resolve_aliases
    | Call { callee; arguments } when name_is ~name:"typing_extensions.Literal.__getitem__" callee
      ->
        let arguments =
          match arguments with
          | [{ Call.Argument.name = None; value = { Node.value = Expression.Tuple arguments; _ } }]
            ->
              Some (List.map arguments ~f:Node.value)
          | [{ Call.Argument.name = None; value = { Node.value = argument; _ } }] -> Some [argument]
          | _ -> None
        in
        arguments
        >>| List.map ~f:create_literal
        >>= Option.all
        >>| union
        |> Option.value ~default:Top
    | Call { callee = { Node.value = callee; _ }; _ } when is_typing_callable callee ->
        parse_callable expression
    | Call
        {
          callee = { Node.value = Name (Name.Attribute { base; attribute = "__getitem__"; _ }); _ };
          arguments = [{ Call.Argument.value = argument; _ }];
        } ->
        create_parametric ~base ~argument
    | Name (Name.Identifier identifier) ->
        let sanitized = Identifier.sanitized identifier in
        if String.equal sanitized "None" then
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
              Parser.parse [value] |> Source.create |> Preprocessing.preprocess |> Source.statements
            in
            match parsed with
            | [{ Node.value = Expression { Node.value; _ }; _ }] -> Some value
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
  | Parametric { name = "typing.Tuple"; parameters }
  | Parametric { name = "tuple"; parameters } -> (
      match parameters with
      | [Single parameter; Group Any] -> Tuple (Unbounded parameter)
      | [Group group] -> Tuple (Bounded group)
      | parameters ->
          Parameter.all_singles parameters
          >>| (fun singles -> Tuple (Bounded (Concrete singles)))
          |> Option.value ~default:Top )
  | Parametric { name; parameters } -> (
      match
        Identifier.Table.find parametric_substitution_map name, Parameter.all_singles parameters
      with
      | Some name, _ -> Parametric { name; parameters }
      | None, Some parameters -> (
          match name with
          | "typing_extensions.Annotated"
          | "typing.Annotated"
            when List.length parameters > 0 ->
              annotated (List.hd_exn parameters)
          | "typing.Optional" when List.length parameters = 1 -> optional (List.hd_exn parameters)
          | "typing.Union" -> union parameters
          | _ -> result )
      | _, None -> result )
  | Union elements -> union elements
  | _ -> result


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
  create_logic ~aliases ~variable_aliases


(* Check if there is a literal Any provided, not including type aliases to Any. *)
let expression_contains_any expression =
  let primitives_with_any_map =
    Identifier.Table.filter ~f:contains_any primitive_substitution_map
  in
  Visit.collect_non_generic_type_names expression
  |> List.exists ~f:(Hashtbl.mem primitives_with_any_map)


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


let contains_final annotation = exists annotation ~predicate:is_final

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


let typed_dictionary_class_name ~total =
  if total then
    "TypedDictionary"
  else
    "NonTotalTypedDictionary"


let elements annotation =
  let module CollectorTransform = Transform.Make (struct
    type state = Primitive.t list

    let visit_children_before _ _ = true

    let visit_children_after = false

    let visit sofar annotation =
      let new_state =
        match annotation with
        | Annotated _ -> "typing.Annotated" :: sofar
        | Callable _ -> "typing.Callable" :: sofar
        | Literal _ -> "typing_extensions.Literal" :: sofar
        | Union [NoneType; _]
        | Union [_; NoneType] ->
            "typing.Optional" :: sofar
        | Parametric { name; _ } -> name :: sofar
        | Primitive annotation -> annotation :: sofar
        | Tuple _ -> "tuple" :: sofar
        | Union _ -> "typing.Union" :: sofar
        | ParameterVariadicComponent _
        | Bottom
        | Any
        | Top
        | NoneType
        | Variable _
        | IntExpression _ ->
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

let is_variable = function
  | Variable _ -> true
  | _ -> false


let contains_variable = exists ~predicate:is_variable

let optional_value = function
  | Union [NoneType; annotation]
  | Union [annotation; NoneType] ->
      Some annotation
  | _ -> None


let async_generator_value = function
  | Parametric { name = "typing.AsyncGenerator"; parameters = [Single parameter; _] } ->
      Some (generator parameter)
  | _ -> None


let awaitable_value = function
  | Parametric { name = "typing.Awaitable"; parameters = [Single parameter] } -> Some parameter
  | _ -> None


let coroutine_value = function
  | Parametric { name = "typing.Coroutine"; parameters = [_; _; Single parameter] } ->
      Some parameter
  | _ -> None


let parameters = function
  | Parametric { parameters; _ } -> Some parameters
  | _ -> None


let type_parameters_for_bounded_tuple_union = function
  | Union annotations ->
      let bounded_tuple_parameters = function
        | Tuple (Bounded (Concrete parameters)) -> Some parameters
        | _ -> None
      in
      List.map annotations ~f:bounded_tuple_parameters
      |> Option.all
      >>= List.transpose
      >>| List.map ~f:union
  | _ -> None


let single_parameter = function
  | Parametric { parameters = [Single parameter]; _ } -> parameter
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
    | Literal (Bytes _) -> Some bytes
    | Literal (Boolean _) -> Some bool
    | Literal (EnumerationMember { enumeration_type; _ }) -> Some enumeration_type
    | _ -> None
  in
  instantiate ~constraints annotation


module OrderedTypes = struct
  include Record.OrderedTypes

  type t = type_t record [@@deriving compare, eq, sexp, show, hash]

  type ordered_types_t = t

  let pp_concise = pp_concise ~pp_type

  module Concatenation = struct
    include Record.OrderedTypes.RecordConcatenate

    let apply_mapping { middle; wrapping = { head; tail } } ~mapper =
      let apply concrete = Parametric { name = mapper; parameters = [Single concrete] } in
      let wrapping = { head = List.map head ~f:apply; tail = List.map tail ~f:apply } in
      let middle = { middle with Middle.mappers = mapper :: middle.Middle.mappers } in
      { middle; wrapping }


    module Middle = struct
      include Record.OrderedTypes.RecordConcatenate.Middle

      let create_bare variable = { variable; mappers = [] }

      let create ~variable ~mappers = { variable; mappers }

      let rec replace_variable middle ~replacement =
        match middle with
        | { Middle.mappers = []; variable } -> replacement variable
        | { Middle.mappers = head_mapper :: tail_mapper; _ } ->
            let inner = { middle with mappers = tail_mapper } in
            let apply concrete =
              Parametric { name = head_mapper; parameters = [Single concrete] }
            in
            let handle_replaced = function
              | Any -> Any
              | Concrete concretes -> Concrete (List.map concretes ~f:apply)
              | Concatenation concatenation ->
                  Concatenation (apply_mapping ~mapper:head_mapper concatenation)
            in
            replace_variable inner ~replacement >>| handle_replaced


      let singleton_replace_variable middle ~replacement =
        let extract = function
          | Some (Concrete [extracted]) -> extracted
          | _ -> failwith "this was a singleton replace"
        in
        replace_variable middle ~replacement:(fun _ -> Some (Concrete [replacement])) |> extract
    end

    let parse expression ~aliases =
      let variable_aliases name =
        match aliases name with
        | Some (VariableAlias variable) -> Some variable
        | _ -> None
      in
      create expression ~aliases |> create_concatenation_operator_from_annotation ~variable_aliases


    let map_head_and_tail { middle; wrapping = { head; tail } } ~f =
      let wrapping = { head = List.map head ~f; tail = List.map tail ~f } in
      { middle; wrapping }


    let map_middle { middle; wrapping } ~f = { middle = f middle; wrapping }

    let replace_variable { middle; wrapping } ~replacement =
      let merge ~inner:{ head; tail } ~outer:{ head = outer_head; tail = outer_tail } =
        { head = outer_head @ head; tail = tail @ outer_tail }
      in
      let actualize ~inner { head; tail } = head @ inner @ tail in
      match Middle.replace_variable middle ~replacement with
      | None -> None
      | Some Any -> Some Any
      | Some (Concrete inner) -> Some (Concrete (actualize ~inner wrapping))
      | Some (Concatenation { middle = inner_middle; wrapping = inner }) ->
          Some (Concatenation { middle = inner_middle; wrapping = merge ~inner ~outer:wrapping })


    let variable { middle = { Middle.variable; _ }; _ } = variable

    let expression = concatenation_expression

    let create ?(head = []) ?(tail = []) middle = { wrapping = { head; tail }; middle }

    let zip concatenation ~against =
      let head = head concatenation in
      let tail = tail concatenation in
      let head_length = List.length head in
      let tail_length = List.length tail in
      let middle_length = List.length against - head_length - tail_length in
      if middle_length >= 0 then
        let middle = middle concatenation in
        let concretes_head = List.sub against ~pos:0 ~len:head_length in
        let concretes_middle = List.sub against ~pos:head_length ~len:middle_length in
        let concretes_tail = List.sub against ~pos:(head_length + middle_length) ~len:tail_length in
        let head = List.zip_exn head concretes_head in
        let tail = List.zip_exn tail concretes_tail in
        Some (create ~head ~tail (middle, concretes_middle))
      else
        None
  end

  let union_upper_bound ordered =
    match ordered with
    | Concrete concretes -> union concretes
    | Any -> Any
    | Concatenation _ -> object_primitive


  let variable ordered_types =
    match ordered_types with
    | Concrete _ -> None
    | Any -> None
    | Concatenation concatenation -> Some (Concatenation.variable concatenation)


  let local_replace_variable ordered_types ~replacement =
    match ordered_types with
    | Concrete _ -> None
    | Any -> None
    | Concatenation concatenation -> Concatenation.replace_variable concatenation ~replacement
end

let split annotation =
  let open Record.Parameter in
  match annotation with
  | Union [NoneType; parameter]
  | Union [parameter; NoneType] ->
      Primitive "typing.Optional", [Single parameter]
  | Parametric { name; parameters } -> Primitive name, parameters
  | Tuple tuple ->
      let parameters =
        match tuple with
        | Bounded parameters -> [Group parameters]
        | Unbounded parameter -> [Single parameter]
      in
      Primitive "tuple", parameters
  | Literal _ as literal -> weaken_literals literal, []
  | Callable _ -> Primitive "typing.Callable", []
  | annotation -> annotation, []


let class_name annotation =
  let strip_calls =
    let rec collect_identifiers identifiers = function
      | {
          Node.value =
            Expression.Call { callee = { Node.value = Name (Name.Attribute { base; _ }); _ }; _ };
          _;
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


let class_variable annotation = parametric "typing.ClassVar" [Single annotation]

let class_variable_value = function
  | Parametric { name = "typing.ClassVar"; parameters = [Single parameter] } -> Some parameter
  | _ -> None


let final_value = function
  | Parametric
      { name = "typing.Final" | "typing_extensions.Final"; parameters = [Single parameter] } ->
      Some parameter
  | Primitive ("typing.Final" | "typing_extensions.Final") -> Some Top
  | _ -> None


(* Angelic assumption: Any occurrences of top indicate that we're dealing with Any instead of None.
   See T22792667. *)
let assume_any = function
  | Top -> Any
  | annotation -> annotation


let dequalify_reference map reference =
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
  fold [] reference


let dequalify_identifier map identifier =
  Reference.create identifier |> dequalify_reference map |> Reference.show


let create_type = create

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

  type parameter_variadic_t = type_t Record.Variable.RecordVariadic.RecordParameters.record
  [@@deriving compare, eq, sexp, show, hash]

  type parameter_variadic_domain = Callable.parameters

  type list_variadic_t = type_t Record.Variable.RecordVariadic.RecordList.record
  [@@deriving compare, eq, sexp, show, hash]

  type list_variadic_domain = OrderedTypes.t

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

    val create
      :  ?constraints:type_t Record.Variable.constraints ->
      ?variance:Record.Variable.variance ->
      string ->
      t

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

      val create : ?variance:Record.Variable.variance -> string -> t

      val parse_instance_annotation
        :  variable_parameter_annotation:Expression.t ->
        keywords_parameter_annotation:Expression.t ->
        aliases:(Primitive.t -> alias option) ->
        t option

      module Components : sig
        include module type of struct
          include Record.Variable.RecordVariadic.RecordParameters.RecordComponents
        end

        type decomposition = {
          positional_component: type_t;
          keyword_component: type_t;
        }

        val combine : decomposition -> parameter_variadic_t option

        val component : t -> component
      end

      val decompose : t -> Components.decomposition
    end

    module List : sig
      include VariableKind with type t = list_variadic_t and type domain = list_variadic_domain

      val name : t -> Identifier.t

      val create
        :  ?constraints:type_t Record.Variable.constraints ->
        ?variance:Record.Variable.variance ->
        string ->
        t
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

  val parse_declaration : Expression.t -> target:Reference.t -> t option

  val dequalify : Reference.t Reference.Map.t -> t -> t

  val namespace : t -> namespace:Namespace.t -> t

  val mark_all_variables_as_bound : ?specific:t list -> type_t -> type_t

  val namespace_all_free_variables : type_t -> namespace:Namespace.t -> type_t

  val all_free_variables : type_t -> t list

  val all_variables_are_resolved : type_t -> bool

  val mark_all_free_variables_as_escaped : ?specific:t list -> type_t -> type_t

  val collapse_all_escaped_variable_unions : type_t -> type_t

  val contains_escaped_free_variable : type_t -> bool

  val convert_all_escaped_free_variables_to_anys : type_t -> type_t

  val converge_all_variable_namespaces : type_t -> type_t

  val zip_on_parameters
    :  parameters:Parameter.t sexp_list ->
    t sexp_list ->
    (Parameter.t * t) sexp_list sexp_option

  val zip_on_two_parameter_lists
    :  left_parameters:Parameter.t sexp_list ->
    right_parameters:Parameter.t sexp_list ->
    t sexp_list ->
    (Parameter.t * Parameter.t * t) sexp_list sexp_option

  val all_unary : t list -> Unary.t list option

  val to_parameter : t -> Parameter.t
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

  type parameter_variadic_t = type_t Record.Variable.RecordVariadic.RecordParameters.record
  [@@deriving compare, eq, sexp, show, hash]

  type parameter_variadic_domain = Callable.parameters

  type list_variadic_t = type_t Record.Variable.RecordVariadic.RecordList.record
  [@@deriving compare, eq, sexp, show, hash]

  type list_variadic_domain = OrderedTypes.t

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
      type t = type_t record [@@deriving compare, sexp]
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
      match constraints with
      | Unconstrained -> false
      | Bound bound -> contains_variable bound
      | Explicit explicits -> List.exists explicits ~f:contains_variable
      | LiteralIntegers -> false


    let mark_as_escaped variable = { variable with state = Free { escaped = true } }

    let local_collect = function
      | Variable variable -> [variable]
      | IntExpression polynomial ->
          List.map polynomial ~f:(fun { variables; _ } ->
              List.map variables ~f:(fun { variable; _ } -> variable))
          |> List.concat
          |> List.dedup_and_sort ~compare
      | _ -> []


    let local_replace replacement = function
      | Variable variable -> replacement variable
      | IntExpression _ as int_expression ->
          let replace_variable variable =
            replacement variable
            >>= type_to_int_expression
            |> Option.value ~default:Any
            |> fun replaced -> variable, replaced
          in
          let replaces = local_collect int_expression |> List.map ~f:replace_variable in
          if List.length replaces = 0 then
            None
          else
            let merge_expression left right =
              match left, right with
              | _, (_, Any) -> Any
              | _, (_, Primitive "int") when not (T.equal left Any) -> integer
              | IntExpression left, (variable, IntExpression right) ->
                  IntExpression (Polynomial.replace left ~by:right ~variable)
              | left, _ -> left
            in
            let replaced_expression = List.fold replaces ~init:int_expression ~f:merge_expression in
            Some replaced_expression
      | _ -> None


    let dequalify ({ variable = name; _ } as variable) ~dequalify_map =
      { variable with variable = dequalify_identifier dequalify_map name }
  end

  module Variadic = struct
    module Parameters = struct
      include Record.Variable.RecordVariadic.RecordParameters

      type t = type_t record [@@deriving compare, eq, sexp, show, hash]

      type domain = Callable.parameters [@@deriving compare, eq, sexp, show, hash]

      module Map = Core.Map.Make (struct
        type t = type_t record [@@deriving compare, sexp]
      end)

      let name { name; _ } = name

      let any = Callable.Undefined

      let self_reference variable = Callable.ParameterVariadicTypeVariable { head = []; variable }

      let pair variable value = ParameterVariadicPair (variable, value)

      let is_free = function
        | { state = Free _; _ } -> true
        | _ -> false


      let is_escaped_and_free = function
        | { state = Free { escaped }; _ } -> escaped
        | _ -> false


      let mark_as_bound variable = { variable with state = InFunction }

      let namespace variable ~namespace = { variable with namespace }

      let local_replace replacement annotation =
        let map = function
          | ParameterVariadicTypeVariable { head; variable } ->
              let apply_head ~head = function
                | ParameterVariadicTypeVariable { head = inner_head; variable } ->
                    ParameterVariadicTypeVariable { head = head @ inner_head; variable }
                | Undefined -> Undefined
                | Defined tail -> Defined (Callable.prepend_anonymous_parameters ~head ~tail)
              in
              replacement variable
              >>| apply_head ~head
              |> Option.value ~default:(ParameterVariadicTypeVariable { head; variable })
          | parameters -> parameters
        in
        match annotation with
        | Callable callable ->
            Callable.map_parameters callable ~f:map
            |> (fun callable -> Callable callable)
            |> Option.some
        | Parametric { name; parameters } ->
            let parameters =
              let map_parameter = function
                | Parameter.CallableParameters parameters ->
                    Parameter.CallableParameters (map parameters)
                | parameter -> parameter
              in
              List.map parameters ~f:map_parameter
            in
            Some (Parametric { name; parameters })
        | _ -> None


      let mark_as_escaped variable = { variable with state = Free { escaped = true } }

      let local_collect = function
        | Callable { implementation; overloads; _ } ->
            let extract = function
              | { parameters = ParameterVariadicTypeVariable { variable; _ }; _ } -> Some variable
              | _ -> None
            in
            List.filter_map (implementation :: overloads) ~f:extract
        | Parametric { parameters; _ } ->
            let extract = function
              | Parameter.CallableParameters (ParameterVariadicTypeVariable { variable; _ }) ->
                  Some variable
              | _ -> None
            in
            List.filter_map parameters ~f:extract
        | _ -> []


      let dequalify ({ name; _ } as variable) ~dequalify_map =
        { variable with name = dequalify_identifier dequalify_map name }


      let parse_declaration value ~target =
        match value with
        | {
         Node.value =
           Expression.Call
             {
               callee =
                 {
                   Node.value =
                     Name
                       (Name.Attribute
                         {
                           base = { Node.value = Name (Name.Identifier "pyre_extensions"); _ };
                           attribute = "ParameterSpecification";
                           special = false;
                         });
                   _;
                 };
               arguments = [{ Call.Argument.value = { Node.value = String _; _ }; _ }];
             };
         _;
        } ->
            Some (create (Reference.show target))
        | _ -> None


      let parse_instance_annotation
          ~variable_parameter_annotation
          ~keywords_parameter_annotation
          ~aliases
        =
        let get_variable name =
          match aliases name with
          | Some (VariableAlias (ParameterVariadic variable)) -> Some variable
          | _ -> None
        in
        let open Record.Variable.RecordVariadic.RecordParameters.RecordComponents in
        match variable_parameter_annotation, keywords_parameter_annotation with
        | ( {
              Node.value =
                Expression.Name
                  (Attribute
                    { base = variable_parameter_base; attribute = variable_parameter_attribute; _ });
              _;
            },
            {
              Node.value =
                Expression.Name
                  (Attribute
                    { base = keywords_parameter_base; attribute = keywords_parameter_attribute; _ });
              _;
            } )
          when Identifier.equal variable_parameter_attribute (component_name PositionalArguments)
               && Identifier.equal keywords_parameter_attribute (component_name KeywordArguments)
          -> (
            match
              ( create_type variable_parameter_base ~aliases,
                create_type keywords_parameter_base ~aliases )
            with
            | Primitive positionals_base, Primitive keywords_base
              when Identifier.equal positionals_base keywords_base ->
                get_variable positionals_base
            | _ -> None )
        | _ -> None


      module Components = struct
        include Record.Variable.RecordVariadic.RecordParameters.RecordComponents

        type decomposition = {
          positional_component: type_t;
          keyword_component: type_t;
        }

        let combine { positional_component; keyword_component } =
          let component_agnostic_equal left right =
            equal
              { left with component = KeywordArguments }
              { right with component = KeywordArguments }
          in
          match positional_component, keyword_component with
          | ( ParameterVariadicComponent
                ({ component = PositionalArguments; _ } as positional_component),
              ParameterVariadicComponent ({ component = KeywordArguments; _ } as keyword_component)
            )
            when component_agnostic_equal positional_component keyword_component ->
              let { variance; variable_name = name; variable_namespace = namespace; _ } =
                positional_component
              in
              Some { name; namespace; variance; state = InFunction }
          | _ -> None


        let component { component; _ } = component
      end

      let decompose { name = variable_name; variance; namespace = variable_namespace; _ } =
        {
          Components.positional_component =
            ParameterVariadicComponent
              { component = PositionalArguments; variable_name; variance; variable_namespace };
          keyword_component =
            ParameterVariadicComponent
              { component = KeywordArguments; variable_name; variance; variable_namespace };
        }
    end

    module List = struct
      include Record.Variable.RecordVariadic.RecordList

      type t = type_t record [@@deriving compare, eq, sexp, show, hash]

      type domain = OrderedTypes.t [@@deriving compare, eq, sexp, show, hash]

      module Map = Core.Map.Make (struct
        type t = type_t record [@@deriving compare, sexp]
      end)

      let any = OrderedTypes.Any

      let self_reference variable =
        OrderedTypes.Concatenation
          (OrderedTypes.Concatenation.empty_wrap
             { OrderedTypes.Concatenation.Middle.variable; mappers = [] })


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
        | Tuple (Bounded bounded) ->
            OrderedTypes.local_replace_variable bounded ~replacement
            >>| fun ordered_types -> Tuple (Bounded ordered_types)
        | Parametric { name; parameters } ->
            let replace = function
              | Record.Parameter.Group ordered ->
                  OrderedTypes.local_replace_variable ordered ~replacement
                  >>| fun group -> Record.Parameter.Group group
              | CallableParameters _
              | Single _ ->
                  None
            in
            let replaced = List.map parameters ~f:(fun parameter -> replace parameter, parameter) in
            if List.exists replaced ~f:(fun (replaced, _) -> Option.is_some replaced) then
              Some
                ( List.map replaced ~f:(fun (replaced, default) -> Option.value replaced ~default)
                |> parametric name )
            else
              None
        | Callable callable ->
            let map = function
              | Defined parameters ->
                  let replace_variadic = function
                    | Callable.Parameter.Variable (Concatenation concatenation) ->
                        let encode_ordered_types_into_parameters = function
                          | OrderedTypes.Any -> [Callable.Parameter.Variable (Concrete Any)]
                          | Concrete concretes ->
                              let make_anonymous annotation =
                                Callable.Parameter.PositionalOnly
                                  { index = 0; annotation; default = false }
                              in
                              List.map concretes ~f:make_anonymous
                          | Concatenation concatenation -> [Variable (Concatenation concatenation)]
                        in
                        OrderedTypes.Concatenation.replace_variable concatenation ~replacement
                        >>| encode_ordered_types_into_parameters
                        |> Option.value
                             ~default:[Callable.Parameter.Variable (Concatenation concatenation)]
                    | parameter -> [parameter]
                  in
                  let correct_indices index = function
                    | Callable.Parameter.PositionalOnly anonymous ->
                        Callable.Parameter.PositionalOnly { anonymous with index }
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
        | Tuple (Bounded bounded) -> OrderedTypes.variable bounded |> Option.to_list
        | Callable { implementation; overloads; _ } ->
            let map = function
              | { parameters = Defined parameters; _ } ->
                  let collect_variadic = function
                    | Callable.Parameter.Variable (Concatenation concatenation) ->
                        Some (OrderedTypes.Concatenation.variable concatenation)
                    | _ -> None
                  in
                  List.filter_map parameters ~f:collect_variadic
              | _ -> []
            in
            implementation :: overloads |> List.concat_map ~f:map
        | Parametric { parameters; _ } ->
            let collect = function
              | Record.Parameter.Group ordered -> OrderedTypes.variable ordered |> Option.to_list
              | CallableParameters _
              | Single _ ->
                  []
            in
            List.concat_map parameters ~f:collect
        | _ -> []


      let dequalify ({ name; _ } as variable) ~dequalify_map =
        { variable with name = dequalify_identifier dequalify_map name }


      let parse_declaration value ~target =
        match value with
        | {
         Node.value =
           Expression.Call
             {
               callee =
                 {
                   Node.value =
                     Name
                       (Name.Attribute
                         {
                           base = { Node.value = Name (Name.Identifier "pyre_extensions"); _ };
                           attribute = "ListVariadic";
                           special = false;
                         });
                   _;
                 };
               arguments = [{ Call.Argument.value = { Node.value = String _; _ }; _ }];
             };
         _;
        } ->
            Some (create (Reference.show target))
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


      let mark_all_as_bound ?specific =
        let in_list =
          match specific with
          | Some variables -> List.mem variables ~equal:Variable.equal
          | None -> fun _ -> true
        in
        let mark_as_bound_if_in_list variable =
          if in_list variable then
            Variable.mark_as_bound variable
          else
            variable
        in
        map mark_as_bound_if_in_list


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
    type t = type_t Record.Variable.record [@@deriving compare, sexp]
  end)

  let pp_concise format = function
    | Unary variable -> Unary.pp_concise format variable ~pp_type
    | ParameterVariadic { name; _ } ->
        Format.fprintf format "CallableParameterTypeVariable[%s]" name
    | ListVariadic { name; _ } -> Format.fprintf format "ListVariadic[%s]" name


  let parse_declaration expression ~target =
    match Variadic.Parameters.parse_declaration expression ~target with
    | Some variable -> Some (ParameterVariadic variable)
    | None -> (
        match Variadic.List.parse_declaration expression ~target with
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


  let partition =
    let partitioner = function
      | Unary variable -> `Fst variable
      | ParameterVariadic variable -> `Snd variable
      | ListVariadic variable -> `Trd variable
    in
    List.partition3_map ~f:partitioner


  let mark_all_variables_as_bound ?specific annotation =
    let specific_unaries, specific_parameters_variadics, specific_list_variadics =
      match specific >>| partition with
      | None -> None, None, None
      | Some (unaries, parameters, lists) -> Some unaries, Some parameters, Some lists
    in
    GlobalTransforms.Unary.mark_all_as_bound ?specific:specific_unaries annotation
    |> GlobalTransforms.ParameterVariadic.mark_all_as_bound ?specific:specific_parameters_variadics
    |> GlobalTransforms.ListVariadic.mark_all_as_bound ?specific:specific_list_variadics


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
    let specific_unaries, specific_parameters_variadics, specific_list_variadics =
      partition variables
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


  let coalesce_if_all_single parameters =
    Parameter.all_singles parameters
    >>| (fun coalesced -> [Parameter.Group (Concrete coalesced)])
    |> Option.value ~default:parameters


  let correct_concrete_group_into_parameters ~variable parameter =
    match variable, parameter with
    | ParameterVariadic _, Parameter.Group (Concrete group) ->
        Parameter.CallableParameters
          (Defined (Callable.prepend_anonymous_parameters ~head:group ~tail:[]))
    | _, other -> other


  let zip_on_parameters ~parameters variables =
    let parameters =
      match variables with
      | [ParameterVariadic _]
      | [ListVariadic _] ->
          coalesce_if_all_single parameters
      | _ -> parameters
    in
    match List.zip parameters variables with
    | Ok zipped ->
        List.map zipped ~f:(fun (parameter, variable) ->
            correct_concrete_group_into_parameters ~variable parameter, variable)
        |> Option.some
    | Unequal_lengths -> None


  let zip_on_two_parameter_lists ~left_parameters ~right_parameters variables =
    let left_parameters, right_parameters =
      match variables with
      | [ParameterVariadic _]
      | [ListVariadic _] ->
          coalesce_if_all_single left_parameters, coalesce_if_all_single right_parameters
      | _ -> left_parameters, right_parameters
    in
    match List.zip left_parameters right_parameters with
    | Ok zipped -> (
        match List.zip zipped variables with
        | Ok zipped ->
            let handle ((left, right), variable) =
              let left = correct_concrete_group_into_parameters ~variable left in
              let right = correct_concrete_group_into_parameters ~variable right in
              left, right, variable
            in
            List.map zipped ~f:handle |> Option.some
        | _ -> None )
    | Unequal_lengths -> None


  let all_unary variables =
    List.map variables ~f:(function
        | Unary unary -> Some unary
        | ListVariadic _
        | ParameterVariadic _ ->
            None)
    |> Option.all


  let to_parameter = function
    | Unary variable -> Parameter.Single (Unary.self_reference variable)
    | ListVariadic variable -> Parameter.Group (Variadic.List.self_reference variable)
    | ParameterVariadic variable ->
        Parameter.CallableParameters (Variadic.Parameters.self_reference variable)
end

let namespace_insensitive_compare left right =
  compare
    (Variable.converge_all_variable_namespaces left)
    (Variable.converge_all_variable_namespaces right)


let is_concrete annotation =
  let module ConcreteTransform = Transform.Make (struct
    type state = bool

    let visit_children_before _ = function
      | NoneType -> false
      | Parametric { name = "typing.Optional" | "Optional"; parameters = [Single Bottom] } -> false
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


let dequalify map annotation =
  let dequalify_string string = string |> dequalify_identifier map in
  let module DequalifyTransform = Transform.Make (struct
    type state = unit

    let visit_children_before _ _ = true

    let visit_children_after = false

    let visit _ annotation =
      let transformed_annotation =
        match annotation with
        | NoneType -> NoneType
        | Union [NoneType; parameter]
        | Union [parameter; NoneType] ->
            Parametric
              { name = dequalify_string "typing.Optional"; parameters = [Single parameter] }
        | Parametric { name; parameters } ->
            Parametric { name = dequalify_identifier map (reverse_substitute name); parameters }
        | Union parameters ->
            Parametric
              {
                name = dequalify_string "typing.Union";
                parameters =
                  List.map parameters ~f:(fun parameter -> Record.Parameter.Single parameter);
              }
        | Tuple (Bounded (Concrete parameters)) ->
            Parametric
              {
                name = dequalify_string "typing.Tuple";
                parameters =
                  List.map parameters ~f:(fun parameter -> Record.Parameter.Single parameter);
              }
        | Primitive name -> Primitive (dequalify_identifier map name)
        | Variable ({ variable = name; _ } as annotation) ->
            Variable { annotation with variable = dequalify_identifier map name }
        | IntExpression polynomial ->
            let variables = Variable.GlobalTransforms.Unary.collect_all annotation in
            let dequalified_polynomial =
              List.fold variables ~init:polynomial ~f:(fun polynomial annotation ->
                  Polynomial.replace
                    polynomial
                    ~by:
                      (Polynomial.create_from_variable
                         {
                           annotation with
                           variable =
                             dequalify_identifier map (Polynomial.Monomial.variable_name annotation);
                         })
                    ~variable:annotation)
            in
            IntExpression dequalified_polynomial
        | Callable ({ kind; _ } as callable) ->
            let kind =
              match kind with
              | Anonymous -> kind
              | Named reference -> Named (dequalify_reference map reference)
            in
            Callable { callable with kind }
        | _ -> annotation
      in
      { Transform.transformed_annotation; new_state = () }
  end)
  in
  snd (DequalifyTransform.visit () annotation)


module TypedDictionary = struct
  open Record.TypedDictionary

  let anonymous fields = { name = "$anonymous"; fields }

  let create_field ~name ~annotation ~required = { name; annotation; required }

  let are_fields_total = are_fields_total

  let same_name { name = left_name; required = _; _ } { name = right_name; required = _; _ } =
    String.equal left_name right_name


  let same_name_different_requiredness
      { name = left_name; required = left_required; _ }
      { name = right_name; required = right_required; _ }
    =
    String.equal left_name right_name && not (Bool.equal left_required right_required)


  let same_name_different_annotation
      { name = left_name; annotation = left_annotation; _ }
      { name = right_name; annotation = right_annotation; _ }
    =
    String.equal left_name right_name && not (equal left_annotation right_annotation)


  let fields_have_colliding_keys left_fields right_fields =
    let found_collision
        { name = needle_name; annotation = needle_annotation; required = needle_required }
      =
      let same_name_different_annotation_or_requiredness { name; annotation; required } =
        String.equal name needle_name
        && ((not (equal annotation needle_annotation)) || not (Bool.equal required needle_required))
      in
      List.exists left_fields ~f:same_name_different_annotation_or_requiredness
    in
    List.exists right_fields ~f:found_collision


  let self_parameter = CallableParameter.Named { name = "self"; annotation = Top; default = false }

  let field_named_parameters ?(all_default = false) fields =
    let field_to_argument { name; annotation; required } =
      Record.Callable.RecordParameter.KeywordOnly
        {
          name = Format.asprintf "$parameter$%s" name;
          annotation;
          default = all_default || not required;
        }
    in
    List.map ~f:field_to_argument fields |> fun parameters -> Defined (self_parameter :: parameters)


  let constructor ~name ~fields =
    let annotation = Primitive name in
    {
      Callable.kind = Named (Reference.create "__init__");
      implementation = { annotation = Top; parameters = Undefined };
      overloads =
        [
          { annotation; parameters = field_named_parameters fields };
          {
            annotation;
            parameters =
              Defined
                [
                  self_parameter;
                  Record.Callable.RecordParameter.PositionalOnly
                    { index = 0; annotation; default = false };
                ];
          };
        ];
    }


  let fields_from_constructor = function
    | {
        Callable.kind = Named name;
        overloads = [{ parameters = Defined (_self :: parameters); _ }; _];
        _;
      }
      when String.equal (Reference.show name) "__init__" ->
        let parameter_to_field = function
          | Record.Callable.RecordParameter.KeywordOnly { name; annotation; default } ->
              Some
                {
                  name = String.split ~on:'$' name |> List.last_exn;
                  annotation;
                  required = not default;
                }
          | _ -> None
        in
        List.map ~f:parameter_to_field parameters |> Option.all
    | _ -> None


  type special_method = {
    name: string;
    special_index: int option;
    overloads: t typed_dictionary_field list -> t Callable.overload list;
  }

  let key_parameter name =
    CallableParameter.Named { name = "k"; annotation = literal_string name; default = false }


  let common_special_methods ~class_name =
    let getitem_overloads =
      let overload { name; annotation; _ } =
        { annotation; parameters = Defined [self_parameter; key_parameter name] }
      in
      List.map ~f:overload
    in
    let setitem_overloads =
      let overload { name; annotation; _ } =
        {
          annotation = none;
          parameters =
            Defined
              [
                self_parameter; key_parameter name; Named { name = "v"; annotation; default = false };
              ];
        }
      in
      List.map ~f:overload
    in
    let get_overloads =
      let overloads { name; annotation; _ } =
        [
          {
            annotation = union [annotation; NoneType];
            parameters = Defined [self_parameter; key_parameter name];
          };
          {
            annotation = Union [annotation; Variable (Variable.Unary.create "_T")];
            parameters =
              Defined
                [
                  self_parameter;
                  key_parameter name;
                  Named
                    {
                      name = "default";
                      annotation = Variable (Variable.Unary.create "_T");
                      default = false;
                    };
                ];
          };
        ]
      in
      List.concat_map ~f:overloads
    in
    let setdefault_overloads =
      let overload { name; annotation; _ } =
        {
          annotation;
          parameters =
            Defined
              [
                self_parameter;
                key_parameter name;
                Named { name = "default"; annotation; default = false };
              ];
        }
      in
      List.map ~f:overload
    in
    let update_overloads fields =
      [
        { annotation = none; parameters = field_named_parameters ~all_default:true fields };
        {
          annotation = none;
          parameters =
            Defined
              [
                self_parameter;
                Record.Callable.RecordParameter.PositionalOnly
                  { index = 0; annotation = Primitive class_name; default = false };
              ];
        };
      ]
    in
    [
      { name = "__getitem__"; special_index = Some 1; overloads = getitem_overloads };
      { name = "__setitem__"; special_index = Some 1; overloads = setitem_overloads };
      { name = "get"; special_index = Some 1; overloads = get_overloads };
      { name = "setdefault"; special_index = Some 1; overloads = setdefault_overloads };
      { name = "update"; special_index = None; overloads = update_overloads };
    ]


  let non_total_special_methods =
    let pop_overloads =
      let overloads { name; annotation; required; _ } =
        if required then
          []
        else
          [
            { annotation; parameters = Defined [self_parameter; key_parameter name] };
            {
              annotation = Union [annotation; Variable (Variable.Unary.create "_T")];
              parameters =
                Defined
                  [
                    self_parameter;
                    key_parameter name;
                    Named
                      {
                        name = "default";
                        annotation = Variable (Variable.Unary.create "_T");
                        default = false;
                      };
                  ];
            };
          ]
      in
      List.concat_map ~f:overloads
    in
    let delitem_overloads fields =
      let overload { name; annotation = _; required } =
        Option.some_if
          (not required)
          { annotation = none; parameters = Defined [self_parameter; key_parameter name] }
      in
      List.filter_map ~f:overload fields
    in
    [
      { name = "pop"; special_index = Some 1; overloads = pop_overloads };
      { name = "__delitem__"; special_index = Some 1; overloads = delitem_overloads };
    ]


  let special_overloads ~class_name ~fields ~method_name =
    let total = are_fields_total fields in
    let special_methods =
      if total then
        common_special_methods ~class_name
      else
        non_total_special_methods @ common_special_methods ~class_name
    in
    List.find special_methods ~f:(fun { name; _ } -> String.equal name method_name)
    >>| fun { overloads; _ } -> overloads fields


  let is_special_mismatch ~class_name ~total ~method_name ~position =
    let special_methods =
      if total then
        common_special_methods ~class_name
      else
        non_total_special_methods @ common_special_methods ~class_name
    in
    List.find special_methods ~f:(fun { name; _ } -> String.equal name method_name)
    >>= (fun { special_index; _ } -> special_index)
    >>| ( = ) position
    |> Option.value ~default:false


  let class_name = typed_dictionary_class_name

  let is_builtin_typed_dictionary_class base_name =
    List.mem [class_name ~total:true; class_name ~total:false] base_name ~equal:String.equal


  let defines ~total ~t_self_expression =
    let open Statement in
    let define ?self_parameter ?return_annotation name =
      Statement.Define
        {
          signature =
            {
              name =
                Reference.create_from_list [class_name ~total; name]
                |> Node.create_with_default_location;
              parameters =
                [
                  { ExpressionParameter.name = "self"; value = None; annotation = self_parameter }
                  |> Node.create_with_default_location;
                ];
              decorators = [];
              return_annotation;
              async = false;
              generator = false;
              parent = Some (Reference.create (class_name ~total));
              nesting_define = None;
            };
          captures = [];
          unbound_names = [];
          body = [];
        }
      |> Node.create_with_default_location
    in
    let common_methods =
      [
        define ~self_parameter:t_self_expression ~return_annotation:t_self_expression "copy";
        define ~self_parameter:t_self_expression ~return_annotation:(expression integer) "__len__";
        define
          ~self_parameter:t_self_expression
          ~return_annotation:(expression (iterator string))
          "__iter__";
      ]
      @ List.map
          (common_special_methods ~class_name:(class_name ~total))
          ~f:(fun { name; _ } -> define name)
    in
    if total then
      common_methods
    else
      common_methods @ List.map non_total_special_methods ~f:(fun { name; _ } -> define name)
end

(* Transform tuples and callables so they are printed correctly when running infer and click to fix. *)
let infer_transform annotation =
  let module InferTransform = Transform.Make (struct
    type state = unit

    let visit_children_before _ _ = true

    let visit_children_after = false

    let visit _ annotation =
      let transformed_annotation =
        let shorten_tuple_type types =
          let parameter = List.hd types |> Option.value ~default:Bottom in
          let should_be_unbound =
            List.fold types ~init:true ~f:(fun all_match next_parameter ->
                if equal parameter next_parameter then
                  all_match
                else
                  false)
          in
          if should_be_unbound then Some (Tuple (Unbounded parameter)) else None
        in
        match annotation with
        | Tuple (Bounded (Concrete types)) when List.length types > 2 ->
            shorten_tuple_type types |> Option.value ~default:annotation
        | Parametric { name = "typing.Tuple"; parameters } when List.length parameters > 2 ->
            let types = List.filter_map parameters ~f:Parameter.is_single in
            if List.length types < List.length parameters then
              annotation
            else
              shorten_tuple_type types |> Option.value ~default:annotation
        | Callable
            ( { implementation = { parameters = Defined parameters; _ } as implementation; _ } as
            callable ) ->
            let parameters =
              let transform_parameter index parameter =
                match parameter with
                | CallableParameter.PositionalOnly { annotation; _ }
                | KeywordOnly { annotation; _ }
                | Named { annotation; _ }
                | Variable (Concrete annotation) ->
                    CallableParameter.PositionalOnly { annotation; default = false; index }
                | _ -> parameter
              in
              List.mapi parameters ~f:transform_parameter
            in
            let implementation = { implementation with parameters = Defined parameters } in
            Callable { callable with implementation }
        | Parametric { name = "typing.Dict"; parameters = [Single Bottom; Single Bottom] } ->
            dictionary ~key:Any ~value:Any
        | Parametric { name = "List" | "typing.List"; parameters = [Single Bottom] } -> list Any
        (* This is broken in typeshed:
           https://github.com/python/typeshed/pull/991#issuecomment-288160993 *)
        | Primitive "_PathLike" -> Primitive "PathLike"
        | Parametric { name = "_PathLike"; parameters } ->
            Parametric { name = "PathLike"; parameters }
        | Parametric { name = "Union" | "typing.Union"; parameters } ->
            Parameter.all_singles parameters >>| union |> Option.value ~default:annotation
        | _ -> annotation
      in
      { Transform.transformed_annotation; new_state = () }
  end)
  in
  snd (InferTransform.visit () annotation)


let contains_prohibited_any annotation =
  let is_string_to_any_mapping
    = (* TODO(T40377122): Remove special-casing of Dict[str, Any] in strict. *)
    function
    | Parametric { name = "typing.Mapping"; parameters = [Single (Primitive "str"); Single Any] }
    | Parametric { name = "dict"; parameters = [Single (Primitive "str"); Single Any] } ->
        true
    | _ -> false
  in
  let module Exists = Transform.Make (struct
    type state = bool

    let visit_children_before _ annotation = not (is_string_to_any_mapping annotation)

    let visit_children_after = false

    let visit sofar annotation =
      { Transform.transformed_annotation = annotation; new_state = sofar || is_any annotation }
  end)
  in
  fst (Exists.visit false annotation)


let to_yojson annotation = `String (show annotation)

let resolve_class annotation =
  let rec extract ~meta original_annotation =
    let annotation =
      match original_annotation with
      | Variable variable -> Variable.Unary.upper_bound variable
      | _ -> original_annotation
    in
    match annotation with
    | Top
    | Bottom
    | Any ->
        Some []
    (* TODO (T65870612): Stop treating NoneType as Optional *)
    | NoneType
    | Union [NoneType; _]
    | Union [_; NoneType] ->
        Some
          [
            {
              instantiated = original_annotation;
              accessed_through_class = meta;
              class_name = "typing.Optional";
            };
          ]
    | Union annotations ->
        let flatten_optional sofar optional =
          match sofar, optional with
          | Some sofar, Some optional -> Some (optional :: sofar)
          | _ -> None
        in
        List.map ~f:(extract ~meta) annotations
        |> List.fold ~init:(Some []) ~f:flatten_optional
        >>| List.concat
        >>| List.rev
    | annotation when is_meta annotation -> single_parameter annotation |> extract ~meta:true
    | _ -> (
        match split annotation |> fst |> primitive_name with
        | Some class_name ->
            Some [{ instantiated = original_annotation; accessed_through_class = meta; class_name }]
        | None -> None )
  in
  extract ~meta:false annotation


let callable_name = function
  | Callable { kind = Named name; _ } -> Some name
  | Parametric
      { name = "BoundMethod"; parameters = [Single (Callable { kind = Named name; _ }); Single _] }
    ->
      Some name
  | _ -> None
