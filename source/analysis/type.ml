(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Type.t is the core representation of the Python static type system for Pyre. It includes Pyre's
   representation of basic types, parametric types and type arguments, literals, and a number of
   other types. *)

open Core
open Ast
open Expression
open Pyre
module ExpressionParameter = Parameter

let dequalify_reference map reference =
  let rec fold accumulator reference =
    if Core.Map.mem map reference then
      Reference.combine (Core.Map.find_exn map reference) (Reference.create_from_list accumulator)
    else
      match Reference.prefix reference with
      | Some prefix -> fold (Reference.last reference :: accumulator) prefix
      | None -> Reference.create_from_list accumulator
  in
  fold [] reference


let dequalify_identifier map identifier =
  Reference.create identifier |> dequalify_reference map |> Reference.show


(* Callable parameter names can be qualified, which means the actual identifier values are globally
   unique and not comparable. Specialize Identifier.t so that `equal` treats names that were the
   same before qualification as equal. *)
module QualifiedParameterName = struct
  type t = Identifier.t [@@deriving compare, sexp, show, hash]

  let sanitized = Identifier.sanitized

  let equal left right = Identifier.equal (sanitized left) (sanitized right)
end

module Record = struct
  (* TODO migeedz: For variance inference, we must add another variation for the variance datatype
     here. The reason we need two varitions is that one variation represents the before variance
     inference and another represents the after. The new variance will include "undefined" and
     "bivariant". Undefined can viewed as: For legacy syntax, the user did not specify variance or
     for PEP695, our algorithm is still resolving variance. Bivariant is different as it means we
     already ran our algorithm and did not conclude variance. Bivariance is not applicable to legacy
     syntax as the user cannot not specify it. *)

  module Variance = struct
    type t =
      | Covariant
      | Contravariant
      | Invariant
      | Bivariant
    [@@deriving compare, equal, sexp, show, hash]

    let show_lowercase = function
      | Covariant -> "covariant"
      | Contravariant -> "contravariant"
      | Invariant -> "invariant"
      | Bivariant -> "bivariant"
  end

  module PreInferenceVariance = struct
    type t =
      | P_Covariant
      | P_Contravariant
      | P_Invariant
      | P_Undefined
    [@@deriving compare, equal, sexp, show, hash]

    let show_lowercase = function
      | P_Covariant -> "P_covariant"
      | P_Contravariant -> "P_contravariant"
      | P_Invariant -> "P_invariant"
      | P_Undefined -> "P_Undefined"
  end

  module TypeVarConstraints = struct
    type 'annotation t =
      | Bound of 'annotation
      | Explicit of 'annotation list
      | Unconstrained
      | LiteralIntegers
    [@@deriving compare, equal, sexp, show, hash]
  end

  module Variable = struct
    type state =
      | Free of { escaped: bool }
      | InFunction
    [@@deriving compare, equal, sexp, show, hash]

    module Namespace = struct
      type t = int [@@deriving compare, equal, sexp, show, hash]
    end

    module TypeVar = struct
      type 'annotation record = {
        name: Identifier.t;
        constraints: 'annotation TypeVarConstraints.t;
        state: state;
        namespace: Namespace.t;
      }
      [@@deriving compare, equal, sexp, show, hash]

      let create ?(constraints = TypeVarConstraints.Unconstrained) name =
        { name; constraints; state = Free { escaped = false }; namespace = 0 }
    end

    module ParamSpec = struct
      type 'annotation record = {
        name: Identifier.t;
        state: state;
        namespace: Namespace.t;
      }
      [@@deriving compare, equal, sexp, show, hash]

      module Components = struct
        type component =
          | KeywordArguments
          | PositionalArguments
        [@@deriving compare, equal, sexp, show, hash]

        type t = {
          component: component;
          variable_name: Identifier.t;
          variable_namespace: Namespace.t;
        }
        [@@deriving compare, equal, sexp, show, hash]
      end

      let create name = { name; state = Free { escaped = false }; namespace = 1 }
    end

    module TypeVarTuple = struct
      type 'annotation record = {
        name: Identifier.t;
        state: state;
        namespace: Namespace.t;
      }
      [@@deriving compare, equal, sexp, show, hash]

      let create name = { name; state = Free { escaped = false }; namespace = 1 }
    end

    type 'a record =
      | TypeVarVariable of 'a TypeVar.record
      | ParamSpecVariable of 'a ParamSpec.record
      | TypeVarTupleVariable of 'a TypeVarTuple.record
    [@@deriving compare, equal, sexp, show, hash]
  end

  module OrderedTypes = struct
    let concatenate_public_names =
      [
        "typing.Concatenate";
        "pyre_extensions.type_variable_operators.Concatenate";
        "typing_extensions.Concatenate";
      ]


    let unpack_public_names =
      ["typing.Unpack"; "pyre_extensions.Unpack"; "typing_extensions.Unpack"]


    module Concatenation = struct
      type 'annotation record_unpackable =
        | Variadic of 'annotation Variable.TypeVarTuple.record
        | UnboundedElements of 'annotation
      [@@deriving compare, equal, sexp, show, hash]

      (* We guarantee that there is exactly one top-level unpacked variadic in this concatenation.

         Note that there may be unpacked variadics within the prefix or suffix, but they are not
         unpacked at the top-level. So, `Tuple[int, *Ts, Tuple[str, *Rs]]` will consider only the
         `*Ts` as the top-level unpacked variadic. *)
      and 'annotation t = {
        prefix: 'annotation list;
        middle: 'annotation record_unpackable;
        suffix: 'annotation list;
      }
      [@@deriving compare, equal, sexp, show, hash]

      let create_unpackable variadic = Variadic variadic

      let create_unbounded_unpackable annotation = UnboundedElements annotation

      let create_from_unpackable ?(prefix = []) ?(suffix = []) unpackable =
        { prefix; middle = unpackable; suffix }


      let create ?prefix ?suffix variadic =
        create_from_unpackable ?prefix ?suffix (Variadic variadic)


      let create_from_unbounded_element ?prefix ?suffix annotation =
        create_from_unpackable ?prefix ?suffix (UnboundedElements annotation)
    end

    type 'annotation record =
      | Concrete of 'annotation list
      | Concatenation of 'annotation Concatenation.t
    [@@deriving compare, equal, sexp, show, hash]

    let create_unbounded_concatenation annotation =
      Concatenation (Concatenation.create_from_unbounded_element annotation)
  end

  module Callable = struct
    module CallableParamType = struct
      type 'annotation named = {
        name: QualifiedParameterName.t;
        annotation: 'annotation;
        default: bool;
      }
      [@@deriving compare, equal, sexp, show, hash]

      type 'annotation variable =
        | Concrete of 'annotation
        | Concatenation of 'annotation OrderedTypes.Concatenation.t
      [@@deriving compare, equal, sexp, show, hash]

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
      [@@deriving compare, equal, sexp, show, hash]

      let annotation = function
        | PositionalOnly { annotation; _ } -> Some annotation
        | Named { annotation; _ } -> Some annotation
        | KeywordOnly { annotation; _ } -> Some annotation
        | Variable (Concrete annotation) -> Some annotation
        | Keywords annotation -> Some annotation
        | _ -> None


      let name = function
        | Named { name; _ }
        | KeywordOnly { name; _ } ->
            Some name
        | Variable _
        | Keywords _
        | PositionalOnly _ ->
            None
    end

    type kind =
      | Anonymous
      | Named of Reference.t

    (* This represents a callable whose parameter signature is determined by a ParamSpec. This could
       be a callable whose parameters are *just* a ParamSpec (e.g. `Callable[P, R]`), or it could be
       one whose parameters add one or more positional-only arguments preceding the param spec, e.g.
       `Callable[Concatenate[T0, T1, P], R]`. *)
    and 'annotation params_from_param_spec = {
      head: 'annotation list;
      variable: 'annotation Variable.ParamSpec.record;
    }

    and 'annotation record_parameters =
      | Defined of 'annotation CallableParamType.t list
      | Undefined
      | FromParamSpec of 'annotation params_from_param_spec

    and 'annotation overload = {
      annotation: 'annotation; (* return type annotation *)
      parameters: 'annotation record_parameters;
    }

    and 'annotation record = {
      kind: kind;
      implementation: 'annotation overload;
      overloads: 'annotation overload list;
    }
    [@@deriving compare, equal, sexp, show, hash]
  end

  module Argument = struct
    type 'annotation record =
      | Single of 'annotation
      | CallableParameters of 'annotation Callable.record_parameters
      | Unpacked of 'annotation OrderedTypes.Concatenation.record_unpackable
    [@@deriving compare, equal, sexp, show, hash]

    let is_single = function
      | Single single -> Some single
      | CallableParameters _
      | Unpacked _ ->
          None
  end

  module RecursiveType = struct
    type 'annotation record = {
      name: Identifier.t;
      body: 'annotation;
    }
    [@@deriving compare, equal, sexp, show, hash]

    let name { name; _ } = name

    let body { body; _ } = body
  end

  module TypeOperation = struct
    module Compose = struct
      type 'annotation t = 'annotation OrderedTypes.record
      [@@deriving compare, equal, sexp, show, hash]
    end

    type 'annotation record = Compose of 'annotation Compose.t
    [@@deriving compare, equal, sexp, show, hash]
  end
end

module CallableParamType = Record.Callable.CallableParamType

module Primitive = struct
  type t = Identifier.t [@@deriving compare, equal, sexp, show, hash]

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
  type literal_string =
    | LiteralValue of string
    | AnyLiteral

  and literal =
    | Boolean of bool
    | Integer of int
    | String of literal_string
    | Bytes of string
    | EnumerationMember of {
        enumeration_type: t;
        member_name: Identifier.t;
      }

  and t =
    | Bottom
    | Callable of t Record.Callable.record
    | Any
    | Literal of literal
    | NoneType
    | Parametric of {
        name: Identifier.t;
        arguments: t Record.Argument.record list;
      }
    | ParamSpecComponent of Record.Variable.ParamSpec.Components.t
    | Primitive of Primitive.t
    | PyreReadOnly of t
    | RecursiveType of t Record.RecursiveType.record
    | Top
    | Tuple of t Record.OrderedTypes.record
    | TypeOperation of t Record.TypeOperation.record
    | Union of t list
    | Variable of t Record.Variable.TypeVar.record
  [@@deriving compare, equal, sexp, show, hash]
end

module Containers = struct
  module Map = Map.Make (T)
  module Set = Set.Make (T)
  include Hashable.Make (T)
end

module Constructors = struct
  open T

  let parametric name arguments = Parametric { name; arguments }

  let awaitable argument = Parametric { name = "typing.Awaitable"; arguments = [Single argument] }

  let coroutine arguments = Parametric { name = "typing.Coroutine"; arguments }

  let bool = Primitive "bool"

  let bytes = Primitive "bytes"

  let complex = Primitive "complex"

  let dictionary ~key ~value = Parametric { name = "dict"; arguments = [Single key; Single value] }

  let mapping_primitive = "typing.Mapping"

  let enumeration = Primitive "enum.Enum"

  let float = Primitive "float"

  let number = Primitive "numbers.Number"

  let generator ?(yield_type = Any) ?(send_type = Any) ?(return_type = Any) () =
    Parametric
      {
        name = "typing.Generator";
        arguments = [Single yield_type; Single send_type; Single return_type];
      }


  let generator_expression yield_type =
    generator ~yield_type ~send_type:NoneType ~return_type:NoneType ()


  let async_generator ?(yield_type = Any) ?(send_type = Any) () =
    Parametric { name = "typing.AsyncGenerator"; arguments = [Single yield_type; Single send_type] }


  let generic_primitive = Primitive "typing.Generic"

  let integer = Primitive "int"

  let literal_integer literal = Literal (Integer literal)

  let iterable argument = Parametric { name = "typing.Iterable"; arguments = [Single argument] }

  let iterator argument = Parametric { name = "typing.Iterator"; arguments = [Single argument] }

  let async_iterator argument =
    Parametric { name = "typing.AsyncIterator"; arguments = [Single argument] }


  let list argument = Parametric { name = "list"; arguments = [Single argument] }

  let class_type annotation = Parametric { name = "type"; arguments = [Single annotation] }

  let extract_from_class_type = function
    | Parametric { name = "type"; arguments = [Single annotation] } -> Some annotation
    | _ -> None


  let named_tuple = Primitive "typing.NamedTuple"

  let none = NoneType

  let object_primitive = Primitive "object"

  let sequence argument = Parametric { name = "typing.Sequence"; arguments = [Single argument] }

  let set argument = Parametric { name = "set"; arguments = [Single argument] }

  let string = Primitive "str"

  let literal_string literal = Literal (String (LiteralValue literal))

  let literal_bytes literal = Literal (Bytes literal)

  let literal_any_string = Literal (String AnyLiteral)

  let tuple arguments = Tuple (Concrete arguments)

  let union arguments =
    let arguments =
      let argument_set = Containers.Hash_set.create () in
      let rec add_argument = function
        | Union arguments -> List.iter arguments ~f:add_argument
        | Bottom -> ()
        | argument -> Base.Hash_set.add argument_set argument
      in
      List.iter arguments ~f:add_argument;
      Base.Hash_set.to_list argument_set
      |> List.sort ~compare
      |> List.filter ~f:(fun argument ->
             match argument with
             | Literal (EnumerationMember { enumeration_type; _ })
               when Base.Hash_set.mem argument_set enumeration_type ->
                 false
             | _ -> true)
    in
    let is_top = function
      | Top -> true
      | _ -> false
    in
    let is_any = function
      | Any -> true
      | _ -> false
    in
    if List.exists ~f:is_top arguments then
      Top
    else if List.exists ~f:is_any arguments then
      Any
    else
      match arguments with
      | [] -> Bottom
      | [argument] -> argument
      | arguments -> Union arguments


  let optional argument =
    match argument with
    | Top -> Top
    | Any -> Any
    | Bottom -> Bottom
    | _ -> union [NoneType; argument]


  let variable ?constraints name = Variable (Record.Variable.TypeVar.create ?constraints name)

  let yield argument = Parametric { name = "Yield"; arguments = [Single argument] }

  let rec pyre_read_only = function
    | PyreReadOnly _ as type_ -> type_
    | NoneType -> NoneType
    | Union elements -> Union (List.map ~f:pyre_read_only elements)
    | (Primitive class_name as type_)
    | (Parametric { name = class_name; _ } as type_)
      when Core.Set.mem Recognized.classes_safe_to_coerce_readonly_to_mutable class_name ->
        (* We trust that it is safe to ignore the `PyreReadOnly` wrapper on these classes. This
           helps reduce noisy errors on classes that are never mutated and reduces the adoption
           burden on users. *)
        type_
    | Any -> Any
    | type_ -> PyreReadOnly type_
end

module VisitWithTransform = struct
  open T

  type 'state visit_result = {
    transformed_annotation: t;
    new_state: 'state;
  }

  module type Implementation = sig
    type state

    val visit : state -> t -> state visit_result

    val visit_children_before : state -> t -> bool

    val visit_children_after : bool
  end

  module Make (Implementation : Implementation) = struct
    let rec visit_annotation ~state annotation =
      let visit_children annotation =
        let visit_all = List.map ~f:(visit_annotation ~state) in
        let rec visit_concatenation { Record.OrderedTypes.Concatenation.prefix; middle; suffix } =
          {
            Record.OrderedTypes.Concatenation.prefix = visit_all prefix;
            middle = visit_unpackable middle;
            suffix = visit_all suffix;
          }
        and visit_unpackable middle =
          match middle with
          | Variadic _ -> middle
          | UnboundedElements annotation -> UnboundedElements (visit_annotation annotation ~state)
        and visit_ordered_types ordered_types =
          match ordered_types with
          | Record.OrderedTypes.Concrete concretes ->
              Record.OrderedTypes.Concrete (visit_all concretes)
          | Concatenation concatenation -> Concatenation (visit_concatenation concatenation)
        in
        let visit_parameters parameter =
          let open Record.Callable in
          let visit_defined = function
            | CallableParamType.Named ({ annotation; _ } as named) ->
                CallableParamType.Named
                  { named with annotation = visit_annotation annotation ~state }
            | CallableParamType.KeywordOnly ({ annotation; _ } as named) ->
                CallableParamType.KeywordOnly
                  { named with annotation = visit_annotation annotation ~state }
            | CallableParamType.Variable (Concrete annotation) ->
                CallableParamType.Variable (Concrete (visit_annotation annotation ~state))
            | CallableParamType.Variable (Concatenation concatenation) ->
                CallableParamType.Variable (Concatenation (visit_concatenation concatenation))
            | CallableParamType.Keywords annotation ->
                CallableParamType.Keywords (visit_annotation annotation ~state)
            | CallableParamType.PositionalOnly ({ annotation; _ } as anonymous) ->
                CallableParamType.PositionalOnly
                  { anonymous with annotation = visit_annotation annotation ~state }
          in
          match parameter with
          | Defined defined -> Defined (List.map defined ~f:visit_defined)
          | FromParamSpec { head; variable } ->
              FromParamSpec { head = List.map head ~f:(visit_annotation ~state); variable }
          | parameter -> parameter
        in
        match annotation with
        | NoneType -> NoneType
        | Callable ({ implementation; overloads; _ } as callable) ->
            let open Record.Callable in
            let visit_overload { annotation; parameters; _ } =
              {
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
        | Parametric { name; arguments } ->
            let visit = function
              | Record.Argument.Single single ->
                  Record.Argument.Single (visit_annotation single ~state)
              | CallableParameters parameters -> CallableParameters (visit_parameters parameters)
              | Unpacked (Variadic _) as unpacked -> unpacked
              | Unpacked (UnboundedElements annotation) ->
                  Unpacked (UnboundedElements (visit_annotation annotation ~state))
            in
            Parametric { name; arguments = List.map arguments ~f:visit }
        | PyreReadOnly type_ -> Constructors.pyre_read_only (visit_annotation type_ ~state)
        | RecursiveType { name; body } ->
            RecursiveType { name; body = visit_annotation ~state body }
        | Tuple ordered_type -> Tuple (visit_ordered_types ordered_type)
        | TypeOperation (Compose ordered_type) ->
            TypeOperation (Compose (visit_ordered_types ordered_type))
        | Union annotations ->
            Constructors.union (List.map annotations ~f:(visit_annotation ~state))
        | Variable ({ constraints; _ } as variable) ->
            let constraints =
              match constraints with
              | Record.TypeVarConstraints.Bound bound ->
                  Record.TypeVarConstraints.Bound (visit_annotation bound ~state)
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
        | ParamSpecComponent _
        | Literal _
        | Bottom
        | Top
        | Any
        | Primitive _ ->
            annotation
      in
      let annotation =
        if Implementation.visit_children_before !state annotation then
          visit_children annotation
        else
          annotation
      in
      let { transformed_annotation; new_state } = Implementation.visit !state annotation in
      state := new_state;
      if Implementation.visit_children_after then
        visit_children transformed_annotation
      else
        transformed_annotation


    let visit state annotation =
      let state = ref state in
      let transformed_annotation = visit_annotation ~state annotation in
      !state, transformed_annotation
  end
end

module Visitors = struct
  open T

  let exists annotation ~predicate =
    let module ExistsVisitor = VisitWithTransform.Make (struct
      type state = bool

      let visit_children_before _ _ = true

      let visit_children_after = false

      let visit sofar annotation =
        let new_state = sofar || predicate annotation in
        { VisitWithTransform.transformed_annotation = annotation; new_state }
    end)
    in
    fst (ExistsVisitor.visit false annotation)


  let collect_types annotation ~predicate =
    let module TypeCollector = VisitWithTransform.Make (struct
      type state = t list

      let visit_children_before _ _ = true

      let visit_children_after = false

      let visit sofar annotation =
        let new_state = if predicate annotation then sofar @ [annotation] else sofar in
        { VisitWithTransform.transformed_annotation = annotation; new_state }
    end)
    in
    fst (TypeCollector.visit [] annotation)


  let collect_primitive_types annotation =
    let predicate = function
      | Primitive _ -> true
      | _ -> false
    in
    collect_types annotation ~predicate


  let collect_names annotation =
    let module NameCollectorImplementation = struct
      type state = {
        names: Primitive.t list;
        recursive_type_names: Primitive.t list;
      }

      let visit_children_before _ _ =
        match annotation with
        | Literal _ -> false
        | _ -> true


      let visit_children_after = false

      let visit { names = sofar; recursive_type_names } annotation =
        let names, recursive_type_names =
          match annotation with
          | Callable _ -> "typing.Callable" :: sofar, recursive_type_names
          | Literal literal ->
              let sofar =
                match literal with
                | String AnyLiteral -> "str" :: sofar
                | _ -> sofar
              in
              "typing_extensions.Literal" :: sofar, recursive_type_names
          | Union [NoneType; _]
          | Union [_; NoneType] ->
              "typing.Optional" :: sofar, recursive_type_names
          | Parametric { name; _ } -> name :: sofar, recursive_type_names
          | Primitive annotation -> annotation :: sofar, recursive_type_names
          | Tuple _ -> "tuple" :: sofar, recursive_type_names
          | TypeOperation (Compose _) -> "pyre_extensions.Compose" :: sofar, recursive_type_names
          | Union _ -> "typing.Union" :: sofar, recursive_type_names
          | PyreReadOnly _ -> "typing._PyreReadOnly_" :: sofar, recursive_type_names
          | RecursiveType { name; _ } -> sofar, name :: recursive_type_names
          | ParamSpecComponent _
          | Bottom
          | Any
          | Top
          | NoneType
          | Variable _ ->
              sofar, recursive_type_names
        in
        {
          VisitWithTransform.transformed_annotation = annotation;
          new_state = { names; recursive_type_names };
        }
    end
    in
    let module NameCollector = VisitWithTransform.Make (NameCollectorImplementation) in
    let { NameCollectorImplementation.names; recursive_type_names } =
      fst (NameCollector.visit { names = []; recursive_type_names = [] } annotation)
    in
    let name_set = Identifier.Set.of_list recursive_type_names in
    (* The recursive alias name is untracked, which would lead to spurious "Annotation is not
       defined" errors. So, filter out any references to it. consider the name as an "element". *)
    List.filter names ~f:(fun element -> not (Core.Set.mem name_set element)) |> List.rev
end

(* Helper functions that extract an inner type if the outer type matches some pattern *)
module Extractions = struct
  open T

  let optional_value = function
    | Union [NoneType; annotation]
    | Union [annotation; NoneType] ->
        Some annotation
    | _ -> None


  let awaitable_value = function
    | Parametric { name = "typing.Awaitable"; arguments = [Single argument] } -> Some argument
    | _ -> None


  let coroutine_value = function
    | Parametric { name = "typing.Coroutine"; arguments = [_; _; Single argument] } -> Some argument
    | _ -> None


  let class_variable_value = function
    | Parametric { name = "typing.ClassVar"; arguments = [Single argument] } -> Some argument
    | _ -> None


  let rec final_value = function
    | Parametric
        { name = "typing.Final" | "typing_extensions.Final"; arguments = [Single argument] } ->
        `Ok argument
    | Parametric { name = "typing.ClassVar"; arguments = [Single argument] } -> final_value argument
    | Primitive ("typing.Final" | "typing_extensions.Final") -> `NoArgument
    | _ -> `NotFinal


  let unpack_value = function
    | Parametric
        {
          name = "typing.Unpack" | "typing_extensions.Unpack" | "pyre_extensions.Unpack";
          arguments = [Single argument];
        } ->
        Some argument
    | _ -> None


  let literal_integer_value = function
    | Literal (Integer value) -> Some value
    | _ -> None
end

module Predicates = struct
  open T

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
    | Parametric { name = "dict"; arguments } -> (
        match with_key, arguments with
        | Some key, [Single key_argument; _] -> equal key key_argument
        | _ -> true)
    | _ -> false


  let is_dictionary_or_mapping = function
    | Parametric { name = "typing.Mapping" | "dict"; _ } -> true
    | _ -> false


  let is_ellipsis = function
    | Primitive "..." -> true
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


  let is_class_type = function
    | Parametric { name = "type"; _ } -> true
    | _ -> false


  let is_none = function
    | NoneType -> true
    | _ -> false


  let is_noreturn_or_never = function
    | Primitive "typing.NoReturn"
    | Primitive "NoReturn"
    | Primitive "Never"
    | Primitive "typing_extensions.Never"
    | Primitive "typing.Never" ->
        true
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


  let is_primitive_string = function
    | Primitive "str" -> true
    | _ -> false


  let is_literal_string = function
    | Literal (String _) -> true
    | _ -> false


  let is_top = function
    | Top -> true
    | _ -> false


  let is_tuple = function
    | Tuple _ -> true
    | Parametric { name = "typing.Tuple" | "Tuple"; _ } -> true
    | _ -> false


  let is_type_alias = function
    | Primitive "typing_extensions.TypeAlias" -> true
    | _ -> false


  let is_unbound = function
    | Bottom -> true
    | _ -> false


  let is_union = function
    | Union _ -> true
    | _ -> false


  let is_variable = function
    | Variable _ -> true
    | _ -> false


  let rec is_falsy = function
    | NoneType
    | Literal (Boolean false)
    | Literal (Integer 0)
    | Literal (String (LiteralValue ""))
    | Literal (Bytes "") ->
        true
    | PyreReadOnly annotated -> is_falsy annotated
    | Union types -> List.for_all types ~f:is_falsy
    | _ -> false


  let rec is_truthy = function
    | Literal (Boolean true)
    | Callable _ ->
        true
    | Literal (Integer i) -> not (Int.equal i 0)
    | Literal (String (LiteralValue value))
    | Literal (Bytes value) ->
        not (String.is_empty value)
    | PyreReadOnly annotated -> is_truthy annotated
    | Union types -> List.for_all types ~f:is_truthy
    | _ -> false


  let is_not_instantiated annotation =
    let predicate = function
      | Bottom -> true
      | Variable { constraints = Unconstrained; _ } -> true
      | _ -> false
    in
    Visitors.exists annotation ~predicate


  let is_unpack = function
    | Parametric
        {
          name = "typing.Unpack" | "typing_extensions.Unpack" | "pyre_extensions.Unpack";
          arguments = [_];
        } ->
        true
    | _ -> false


  let is_untyped = function
    | Any
    | Bottom
    | Top ->
        true
    | _ -> false


  let is_partially_typed annotation = Visitors.exists annotation ~predicate:is_untyped

  let is_class_variable annotation =
    match Extractions.class_variable_value annotation with
    | Some _ -> true
    | None -> false


  let contains_callable annotation = Visitors.exists annotation ~predicate:is_callable

  let contains_any annotation = Visitors.exists annotation ~predicate:is_any

  let contains_unknown annotation = Visitors.exists annotation ~predicate:is_top

  let contains_undefined annotation = Visitors.exists annotation ~predicate:is_unbound

  let contains_literal annotation =
    let predicate = function
      | Literal _ -> true
      | _ -> false
    in
    Visitors.exists annotation ~predicate


  let contains_final annotation =
    let predicate annotation =
      match Extractions.final_value annotation with
      | `Ok _
      | `NoArgument ->
          true
      | `NotFinal -> false
    in
    Visitors.exists annotation ~predicate


  let contains_variable = Visitors.exists ~predicate:is_variable

  let contains_prohibited_any annotation =
    let is_string_to_any_mapping
      = (* TODO(T40377122): Remove special-casing of Dict[str, Any] in strict. *)
      function
      | Parametric { name = "typing.Mapping"; arguments = [Single (Primitive "str"); Single Any] }
      | Parametric { name = "dict"; arguments = [Single (Primitive "str"); Single Any] } ->
          true
      | _ -> false
    in
    let module ProhibitedAnyCollector = VisitWithTransform.Make (struct
      type state = bool

      let visit_children_before _ annotation = not (is_string_to_any_mapping annotation)

      let visit_children_after = false

      let visit sofar annotation =
        {
          VisitWithTransform.transformed_annotation = annotation;
          new_state = sofar || is_any annotation;
        }
    end)
    in
    fst (ProhibitedAnyCollector.visit false annotation)
end

(* The Canonicalization module contains logic related to representing types. Pretty printing uses
   the canonicalizations, but so does some other logic like the `Type.t -> Expression.t`
   conversion. *)
module Canonicalization = struct
  open T

  let reverse_substitute name =
    match name with
    | "collections.defaultdict" -> "typing.DefaultDict"
    | "dict" -> "typing.Dict"
    | "frozenset" -> "typing.FrozenSet"
    | "list" -> "typing.List"
    | "set" -> "typing.Set"
    | "type" -> "typing.Type"
    | _ -> name


  let alternate_name_to_canonical_name_map =
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
      "pyre_extensions.generic.Generic", "typing.Generic";
    ]
    |> Identifier.Table.of_alist_exn


  let parameter_variable_type_representation = function
    | { Record.Callable.head = []; variable = { name; _ } } -> Primitive name
    | { head; variable = { name; _ } } ->
        let concretes = head @ [Primitive name] in
        Parametric
          {
            name = List.hd_exn Record.OrderedTypes.concatenate_public_names;
            arguments = List.map concretes ~f:(fun concrete -> Record.Argument.Single concrete);
          }
end

module PrettyPrinting = struct
  module Variable = struct
    open Record.Variable

    module TypeVar = struct
      open Record.Variable.TypeVar

      let pp_concise format { name; constraints; _ } ~pp_type =
        let description =
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
        Format.fprintf format "%s[%s%s]" description (Identifier.sanitized name) constraints
    end

    module ParamSpec = struct
      module Components = struct
        open Record.Variable.ParamSpec.Components

        let component_name = function
          | KeywordArguments -> "kwargs"
          | PositionalArguments -> "args"


        let pp_concise format { component; variable_name; _ } =
          Format.fprintf format "%s.%s" variable_name (component_name component)
      end
    end

    module TypeVarTuple = struct
      open Record.Variable.TypeVarTuple

      let pp_concise format { name; _ } = Format.fprintf format "%s" name
    end

    let pp_concise ~pp_type format = function
      | TypeVarVariable variable -> TypeVar.pp_concise format variable ~pp_type
      | ParamSpecVariable { name; _ } ->
          Format.fprintf format "CallableParamTypeeterTypeVariable[%s]" name
      | TypeVarTupleVariable { name; _ } -> Format.fprintf format "TypeVarTuple[%s]" name
  end

  module OrderedTypes = struct
    open Record.OrderedTypes

    let show_type_list types ~pp_type =
      Format.asprintf
        "%a"
        (Format.pp_print_list ~pp_sep:(fun format () -> Format.fprintf format ", ") pp_type)
        types


    module Concatenation = struct
      open Record.OrderedTypes.Concatenation

      let rec pp_unpackable ~pp_type format = function
        | Variadic variadic -> Format.fprintf format "*%a" Variable.TypeVarTuple.pp_concise variadic
        | UnboundedElements annotation -> Format.fprintf format "*Tuple[%a, ...]" pp_type annotation


      and pp_concatenation format { prefix; middle; suffix } ~pp_type =
        Format.fprintf
          format
          "%s%s%a%s%s"
          (show_type_list ~pp_type prefix)
          (if List.is_empty prefix then "" else ", ")
          (pp_unpackable ~pp_type)
          middle
          (if List.is_empty suffix then "" else ", ")
          (show_type_list ~pp_type suffix)
    end

    let pp_concise format variable ~pp_type =
      match variable with
      | Concrete types -> Format.fprintf format "%s" (show_type_list types ~pp_type)
      | Concatenation concatenation ->
          Format.fprintf format "%a" (Concatenation.pp_concatenation ~pp_type) concatenation
  end

  module Callable = struct
    module CallableParamType = struct
      open Record.Callable.CallableParamType

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
              (OrderedTypes.Concatenation.pp_concatenation ~pp_type)
              concatenation
        | Keywords annotation -> Format.asprintf "Keywords(%a)" pp_type annotation
    end
  end

  open T

  let show_callable_parameters ~pp_type = function
    | Record.Callable.Undefined -> "..."
    | FromParamSpec variable ->
        Canonicalization.parameter_variable_type_representation variable
        |> Format.asprintf "%a" pp_type
    | Defined parameters ->
        List.map parameters ~f:(Callable.CallableParamType.show_concise ~pp_type)
        |> String.concat ~sep:", "
        |> fun parameters -> Format.asprintf "[%s]" parameters


  let pp_arguments ~pp_type format = function
    | arguments
      when List.for_all arguments ~f:(function
               | Record.Argument.Single argument ->
                   Predicates.is_unbound argument || Predicates.is_top argument
               | _ -> false) ->
        Format.fprintf format ""
    | arguments ->
        let s format = function
          | Record.Argument.Single argument -> Format.fprintf format "%a" pp_type argument
          | CallableParameters parameters ->
              Format.fprintf format "%s" (show_callable_parameters parameters ~pp_type)
          | Unpacked unpackable ->
              Format.fprintf
                format
                "%a"
                (OrderedTypes.Concatenation.pp_unpackable ~pp_type)
                unpackable
        in
        Format.pp_print_list
          ~pp_sep:(fun format () -> Format.fprintf format ", ")
          s
          format
          arguments


  let rec pp format annotation =
    let pp_ordered_type ordered_type =
      match ordered_type with
      | Record.OrderedTypes.Concatenation
          { middle = UnboundedElements annotation; prefix = []; suffix = [] } ->
          Format.asprintf "%a, ..." pp annotation
      | ordered_type -> Format.asprintf "%a" (OrderedTypes.pp_concise ~pp_type:pp) ordered_type
    in
    match annotation with
    | Bottom -> Format.fprintf format "undefined"
    | Callable { kind; implementation; overloads; _ } ->
        let kind =
          match kind with
          | Anonymous -> ""
          | Named name -> Format.asprintf "(%a)" Reference.pp name
        in
        let signature_to_string { Record.Callable.annotation; parameters; _ } =
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
    | Literal (String (LiteralValue literal)) ->
        Format.fprintf format "typing_extensions.Literal['%s']" literal
    | Literal (String AnyLiteral) -> Format.fprintf format "typing_extensions.LiteralString"
    | Literal (Bytes literal) -> Format.fprintf format "typing_extensions.Literal[b'%s']" literal
    | Literal (EnumerationMember { enumeration_type; member_name }) ->
        Format.fprintf format "typing_extensions.Literal[%s.%s]" (show enumeration_type) member_name
    | NoneType -> Format.fprintf format "None"
    | Parametric { name; arguments } ->
        let name = Canonicalization.reverse_substitute name in
        Format.fprintf format "%s[%a]" name (pp_arguments ~pp_type:pp) arguments
    | ParamSpecComponent component -> Variable.ParamSpec.Components.pp_concise format component
    | Primitive name -> Format.fprintf format "%s" name
    | PyreReadOnly type_ -> Format.fprintf format "pyre_extensions.PyreReadOnly[%a]" pp type_
    | RecursiveType { name; body } -> Format.fprintf format "%s (resolves to %a)" name pp body
    | Top -> Format.fprintf format "unknown"
    | Tuple ordered_type -> Format.fprintf format "typing.Tuple[%s]" (pp_ordered_type ordered_type)
    | TypeOperation (Compose ordered_type) ->
        Format.fprintf format "pyre_extensions.Compose[%s]" (pp_ordered_type ordered_type)
    | Union [NoneType; argument]
    | Union [argument; NoneType] ->
        Format.fprintf format "typing.Optional[%a]" pp argument
    | Union arguments ->
        Format.fprintf
          format
          "typing.Union[%s]"
          (List.map arguments ~f:show |> String.concat ~sep:", ")
    | Variable unary -> Variable.TypeVar.pp_concise format unary ~pp_type:pp


  and show annotation = Format.asprintf "%a" pp annotation

  and pp_concise format annotation =
    let pp_pipe_separated =
      Format.pp_print_list ~pp_sep:(fun format () -> Format.fprintf format " | ") pp_concise
    in
    let canonicalize_and_strip_qualification identifier =
      let canonical_identifier =
        Hashtbl.find Canonicalization.alternate_name_to_canonical_name_map identifier
        |> Option.value ~default:identifier
      in
      String.split ~on:'.' canonical_identifier
      |> List.last
      |> Option.value ~default:canonical_identifier
    in
    let signature_to_string { Record.Callable.annotation; parameters; _ } =
      let parameters =
        match parameters with
        | Undefined -> "..."
        | FromParamSpec variable ->
            Canonicalization.parameter_variable_type_representation variable
            |> Format.asprintf "%a" pp_concise
        | Defined parameters ->
            let parameter = function
              | CallableParamType.PositionalOnly { annotation; default; _ } ->
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
                    (OrderedTypes.Concatenation.pp_concatenation ~pp_type:pp_concise)
                    concatenation
              | Keywords annotation -> Format.asprintf "**(%a)" pp_concise annotation
            in
            List.map parameters ~f:parameter |> String.concat ~sep:", "
      in
      Format.asprintf "(%s) -> %a" parameters pp_concise annotation
    in
    match annotation with
    | Bottom -> Format.fprintf format "?"
    | Callable { implementation; _ } ->
        Format.fprintf format "%s" (signature_to_string implementation)
    | Parametric
        { name = "BoundMethod"; arguments = [Single (Callable { implementation; _ }); Single _] } ->
        Format.fprintf format "%s" (signature_to_string implementation)
    | Any -> Format.fprintf format "Any"
    | Literal (Boolean literal) ->
        Format.fprintf format "Literal[%s]" (if literal then "True" else "False")
    | Literal (Integer literal) -> Format.fprintf format "Literal[%d]" literal
    | Literal (String (LiteralValue literal)) -> Format.fprintf format "Literal['%s']" literal
    | Literal (String AnyLiteral) -> Format.fprintf format "LiteralString"
    | Literal (Bytes literal) -> Format.fprintf format "Literal[b'%s']" literal
    | Literal (EnumerationMember { enumeration_type; member_name }) ->
        Format.fprintf format "Literal[%s.%s]" (show enumeration_type) member_name
    | NoneType -> Format.fprintf format "None"
    | Parametric { name; arguments } ->
        let name = canonicalize_and_strip_qualification name in
        Format.fprintf format "%s[%a]" name (pp_arguments ~pp_type:pp_concise) arguments
    | ParamSpecComponent component -> Variable.ParamSpec.Components.pp_concise format component
    | Primitive "..." -> Format.fprintf format "..."
    | Primitive name -> Format.fprintf format "%s" (canonicalize_and_strip_qualification name)
    | PyreReadOnly type_ ->
        Format.fprintf format "pyre_extensions.PyreReadOnly[%a]" pp_concise type_
    | RecursiveType { name; _ } -> Format.fprintf format "%s" name
    | Top -> Format.fprintf format "unknown"
    | Tuple (Concatenation { middle = UnboundedElements argument; prefix = []; suffix = [] }) ->
        Format.fprintf format "tuple[%a, ...]" pp_concise argument
    | Tuple ordered_type ->
        Format.fprintf format "tuple[%a]" (OrderedTypes.pp_concise ~pp_type:pp_concise) ordered_type
    | TypeOperation (Compose ordered_type) ->
        Format.fprintf
          format
          "Compose[%a]"
          (OrderedTypes.pp_concise ~pp_type:pp_concise)
          ordered_type
    | Union [NoneType; argument]
    | Union [argument; NoneType] ->
        Format.fprintf format "%a | None" pp_concise argument
    | Union arguments -> Format.fprintf format "%a" pp_pipe_separated arguments
    | Variable { name; _ } -> Format.fprintf format "%s" (canonicalize_and_strip_qualification name)


  and show_concise annotation = Format.asprintf "%a" pp_concise annotation
end

module Argument = struct
  open T
  include Record.Argument

  type t = T.t record [@@deriving compare, equal, sexp, show, hash]

  let all_singles arguments = List.map arguments ~f:is_single |> Option.all

  let to_variable = function
    | Single (Variable variable) -> Some (Record.Variable.TypeVarVariable variable)
    | CallableParameters (FromParamSpec { head = []; variable }) ->
        Some (ParamSpecVariable variable)
    | Unpacked (Variadic variadic) -> Some (TypeVarTupleVariable variadic)
    | _ -> None


  let is_unpacked = function
    | Unpacked _ -> true
    | _ -> false


  let pp_list = PrettyPrinting.pp_arguments ~pp_type:PrettyPrinting.pp
end

module Transforms = struct
  (* Apply a `type_map` that replaces some types with other types recursively. Use cases of this
     include applying constraint solver results for type variables and replacing types coming from
     empty stubs with `Any`. The `visit_children_before` flag controls whether we apply the mapping
     to inner types before attempting to map a complex type. If it is false, we'll try both before
     and after mapping inner types (preferring the before case if both produce results). *)
  let apply_type_map ?(visit_children_before = false) annotation ~type_map =
    let module ApplyTypeMapTransform = VisitWithTransform.Make (struct
      type state = unit

      let visit_children_before _ annotation =
        visit_children_before || type_map annotation |> Option.is_none


      let visit_children_after = false

      let visit _ annotation =
        let transformed_annotation =
          match type_map annotation with
          | Some replacement -> replacement
          | None -> annotation
        in
        { VisitWithTransform.transformed_annotation; new_state = () }
    end)
    in
    snd (ApplyTypeMapTransform.visit () annotation)
end

module Callable = struct
  open T
  include Record.Callable

  module CallableParamType = struct
    include Record.Callable.CallableParamType

    type parameter = T.t t [@@deriving compare, equal, sexp, show, hash]

    module Map = Core.Map.Make (struct
      type t = parameter [@@deriving compare, sexp]
    end)

    let dummy_star_parameter = { name = "*"; annotation = Bottom; default = false }

    let create parameters =
      let has_pep570_syntax =
        List.find parameters ~f:(fun { name; _ } ->
            String.equal (Identifier.sanitized name) "*"
            || String.equal (Identifier.sanitized name) "/")
        |> Option.is_some
      in
      let create_parameter (index, keyword_only, sofar) { name; annotation; default } =
        if String.equal (Identifier.sanitized name) "*" then
          (* * makes all subsequent named parameters keyword-only *)
          index, true, sofar
        else if String.equal (Identifier.sanitized name) "/" then
          (* / makes all previous named parameters positional-only *)
          let add_positional_only index param =
            match param with
            | Named { annotation; default; _ } -> PositionalOnly { index; annotation; default }
            | _ -> param
          in
          index, keyword_only, List.rev sofar |> List.mapi ~f:add_positional_only |> List.rev
        else
          let star, name = Identifier.split_star name in
          let keyword_only = keyword_only || Identifier.equal star "*" in
          let index, new_parameter =
            match star with
            | "**" -> index + 1, Keywords annotation
            | "*" -> index + 1, Variable (Concrete annotation)
            | _ ->
                (* Parameters that start but do not end with __, occurring in functions that do not
                   have PEP570's special parameter syntax * and /, are treated as positional-only
                   unless they occur after a variadic parameter *)
                let sanitized = Identifier.sanitized name in
                if
                  (not keyword_only)
                  && (not has_pep570_syntax)
                  && Identifier.is_private_name sanitized
                then
                  index + 1, CallableParamType.PositionalOnly { index; annotation; default }
                else
                  let named = { name; annotation; default } in
                  if keyword_only then
                    index + 1, KeywordOnly named
                  else
                    index + 1, Named named
          in
          index, keyword_only, new_parameter :: sofar
      in
      let _, _, parameters = List.fold parameters ~f:create_parameter ~init:(0, false, []) in
      List.rev parameters


    let show_concise =
      PrettyPrinting.Callable.CallableParamType.show_concise ~pp_type:PrettyPrinting.pp


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


    (* Match parameters from two parameter lists `left_parameters` and `right_parameters`.
     *
     * This returns a list of [`Both | `Left | `Right], where:
     * `Both (left, right)` means the parameter `left` from `left_parameters` matches the parameter `right` from `right_parameters`.
     * `Left left` means the parameter `left` from `left_parameters` does not match any parameter in `right_parameters`.
     * `Right right` means the parameter `right` from `right_parameters` does not match any parameters in `left_parameters`.
     *
     * All parameters from `left_parameters` and `right_parameters` only appear once in the result. The kinds of the matched parameters
     * do not necessarily match -- in particular, positional-only parameters can match named ones.
     *)
    let zip left_parameters right_parameters =
      let find_positional_parameter index parameters = List.nth parameters index in
      let find_named_parameter name =
        let equal_name parameter =
          match parameter with
          | KeywordOnly { name = parameter_name; _ }
          | Named { name = parameter_name; _ }
            when QualifiedParameterName.equal parameter_name name ->
              Some parameter
          | _ -> None
        in
        List.find_map ~f:equal_name
      in
      let find_variable_parameter =
        let is_variable = function
          | Variable _ as parameter -> Some parameter
          | _ -> None
        in
        List.find_map ~f:is_variable
      in
      let find_keywords_parameter =
        let is_keywords = function
          | Keywords _ as parameter -> Some parameter
          | _ -> None
        in
        List.find_map ~f:is_keywords
      in
      let find_matching_parameter given_parameters = function
        | PositionalOnly { index; _ } -> find_positional_parameter index given_parameters
        | KeywordOnly { name; _ }
        | Named { name; _ } ->
            (* TODO(T44178876): ensure index match as well for named parameters *)
            find_named_parameter name given_parameters
        | Variable _ -> find_variable_parameter given_parameters
        | Keywords _ -> find_keywords_parameter given_parameters
      in
      let process_left left_parameter =
        match find_matching_parameter right_parameters left_parameter with
        | Some right_parameter -> `Both (left_parameter, right_parameter)
        | None -> `Left left_parameter
      in

      let left_matches = List.map ~f:process_left left_parameters in
      (* It's okay for named args to override positional ones *)
      let has_same_name left_name = function
        | Named { name; _ } -> ( ((name == left_name) [@alert "-deprecated"]))
        | _ -> false
      in
      let is_matched_with_positional_only right_param = function
        | `Both (overridden_parameter, overriding_parameter) -> (
            match overridden_parameter, overriding_parameter with
            | PositionalOnly _, Named { name; _ } -> has_same_name name right_param
            | _ -> false)
        | _ -> false
      in
      let process_right right_parameter =
        match find_matching_parameter left_parameters right_parameter with
        | Some _ -> None
        | None ->
            if List.exists left_matches ~f:(is_matched_with_positional_only right_parameter) then
              None
            else
              Some (`Right right_parameter)
      in
      let right_matches = List.filter_map ~f:process_right right_parameters in
      left_matches @ right_matches
  end

  type t = T.t Record.Callable.record [@@deriving compare, equal, sexp, show, hash]

  type parameters = T.t Record.Callable.record_parameters
  [@@deriving compare, equal, sexp, show, hash]

  module Overload = struct
    let parameters { parameters; _ } =
      match parameters with
      | Defined parameters -> Some parameters
      | FromParamSpec _
      | Undefined ->
          None


    let return_annotation { annotation; _ } = annotation

    let is_undefined { parameters; annotation; _ } =
      match parameters with
      | Undefined -> Predicates.contains_unknown annotation
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


  let map_parameters_with_result ({ implementation; overloads; _ } as callable) ~f =
    let for_implementation ({ parameters; _ } as implementation) =
      Result.map
        ~f:(fun new_parameters -> { implementation with parameters = new_parameters })
        (f parameters)
    in
    let implementation_result = for_implementation implementation in
    let overloads_results = overloads |> List.map ~f:for_implementation |> Result.all in
    Result.combine
      ~ok:(fun implementation overloads -> { callable with implementation; overloads })
      ~err:(fun first _ -> first)
      implementation_result
      overloads_results


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
      CallableParamType.PositionalOnly { index = 0; annotation; default = false }
    in
    let correct_indices index = function
      | CallableParamType.PositionalOnly anonymous ->
          CallableParamType.PositionalOnly { anonymous with index }
      | parameter -> parameter
    in
    let head = List.map head ~f:make_anonymous in
    List.mapi ~f:correct_indices (head @ tail)


  let name = function
    | { kind = Named name; _ } -> Some name
    | _ -> None
end

module RecursiveType = struct
  open T
  include Record.RecursiveType

  let contains_recursive_type_with_name ~name =
    Visitors.exists ~predicate:(function
        | T.RecursiveType { name = inner_name; _ } -> Identifier.equal inner_name name
        | _ -> false)


  let create ~name ~body =
    if contains_recursive_type_with_name ~name body then
      failwith "Body of recursive type contains a recursive type with the same name";
    RecursiveType { name; body }


  let is_recursive_alias_reference ~alias_name =
    Visitors.exists ~predicate:(function
        | Primitive name
        | Parametric { name; _ }
          when Identifier.equal name alias_name ->
            true
        | _ -> false)


  (* We assume that recursive alias names are unique. So, we don't need to worry about accidentally
     renaming a nested recursive type here. *)
  let unfold_recursive_type { name; body } =
    Transforms.apply_type_map
      ~type_map:(function
        | Primitive primitive_name when Identifier.equal primitive_name name ->
            Some (RecursiveType { name; body })
        | _ -> None)
      body


  let body_with_replaced_name ~new_name { name; body } =
    Transforms.apply_type_map
      ~type_map:(function
        | Primitive primitive_name when Identifier.equal primitive_name name ->
            Some (Primitive new_name)
        | _ -> None)
      body


  let replace_references_with_recursive_type ~recursive_type:{ name; body } =
    Transforms.apply_type_map ~type_map:(function
        | Primitive primitive_name when Identifier.equal primitive_name name ->
            Some (RecursiveType { name; body })
        | _ -> None)


  module Namespace = struct
    let fresh = ref 1

    let reset () = fresh := 1

    let create_fresh () =
      let namespace = !fresh in
      fresh := namespace + 1;
      namespace


    let create_fresh_name () = Format.asprintf "$RecursiveType%d" (create_fresh ())
  end
end

module OrderedTypes = struct
  open T
  include Record.OrderedTypes

  module Concatenation = struct
    include Concatenation

    let pp_unpackable =
      PrettyPrinting.OrderedTypes.Concatenation.pp_unpackable ~pp_type:PrettyPrinting.pp


    let unpackable_to_expression ~expression ~location unpackable =
      let index_value =
        match unpackable with
        | Record.OrderedTypes.Concatenation.Variadic variadic ->
            Expression.Name
              (create_name
                 ~location
                 ~create_origin:(fun _ -> None)
                 (Format.asprintf "%a" PrettyPrinting.Variable.TypeVarTuple.pp_concise variadic))
        | UnboundedElements annotation ->
            Ast.Expression.subscript_for_annotation
              ~location
              "typing.Tuple"
              [
                expression annotation; Expression.Constant Constant.Ellipsis |> Node.create ~location;
              ]
      in
      Expression.Subscript
        {
          base =
            Expression.Name (create_name ~location ~create_origin:(fun _ -> None) "typing.Unpack")
            |> Node.create ~location;
          index = index_value |> Node.create ~location;
          origin = None;
        }
      |> Node.create ~location


    let extract_sole_variadic = function
      | { prefix = []; middle = Variadic variadic; suffix = [] } -> Some variadic
      | _ -> None


    let extract_sole_unbounded_annotation = function
      | { prefix = []; middle = UnboundedElements annotation; suffix = [] } -> Some annotation
      | _ -> None


    let is_fully_unbounded concatenation =
      extract_sole_unbounded_annotation concatenation |> Option.is_some
  end

  type t = T.t record [@@deriving compare, equal, sexp, show, hash]

  type ordered_types_t = t

  let pp_concise = PrettyPrinting.OrderedTypes.pp_concise ~pp_type:PrettyPrinting.pp

  let union_upper_bound = function
    | Concrete concretes -> Constructors.union concretes
    | Concatenation { prefix; middle = UnboundedElements unbounded; suffix } ->
        Constructors.union (unbounded :: (prefix @ suffix))
    | Concatenation _ -> Constructors.object_primitive


  let concatenation_from_arguments arguments =
    let unpacked_element_index index argument =
      match argument with
      | Argument.Unpacked _ -> Some index
      | _ -> None
    in
    match List.filter_mapi arguments ~f:unpacked_element_index with
    | [unpacked_index] -> (
        let prefix, rest = List.split_n arguments unpacked_index in
        match rest with
        | Unpacked middle :: suffix -> (
            match Argument.all_singles prefix, Argument.all_singles suffix with
            | Some prefix, Some suffix -> Some (Concatenation { prefix; middle; suffix })
            | _ -> None)
        | _ -> None)
    | _ -> None


  let to_arguments = function
    | Concrete elements -> List.map elements ~f:(fun element -> Argument.Single element)
    | Concatenation { prefix; middle = unpacked; suffix } ->
        List.map prefix ~f:(fun element -> Argument.Single element)
        @ [Argument.Unpacked unpacked]
        @ List.map suffix ~f:(fun element -> Argument.Single element)


  let to_starred_annotation_expression ~expression = function
    | { Concatenation.prefix = []; middle; suffix = [] } ->
        Concatenation.unpackable_to_expression ~expression ~location:Location.any middle
    | concatenation ->
        Constructors.parametric
          (List.hd_exn Record.OrderedTypes.unpack_public_names)
          (to_arguments (Concatenation concatenation))
        |> expression


  let concatenation_from_annotations ~variables annotations =
    let unpacked_element_index index = function
      | Parametric { name; _ }
        when List.exists ~f:(Identifier.equal name) Record.OrderedTypes.unpack_public_names ->
          Some index
      | _ -> None
    in
    match List.filter_mapi annotations ~f:unpacked_element_index with
    | [unpacked_index] -> (
        let prefix, rest = List.split_n annotations unpacked_index in
        match rest with
        | middle :: suffix -> (
            match middle with
            | Parametric { name; arguments = [Single (Primitive variable_name)] }
              when List.exists ~f:(Identifier.equal name) Record.OrderedTypes.unpack_public_names
              -> (
                variables variable_name
                >>= function
                | Record.Variable.TypeVarTupleVariable variadic ->
                    Some (Concatenation.create ~prefix ~suffix variadic)
                | _ -> None)
            | Parametric
                {
                  name;
                  arguments =
                    [
                      Single
                        (Tuple
                          (Concatenation { prefix = inner_prefix; middle; suffix = inner_suffix }));
                    ];
                }
              when List.exists ~f:(Identifier.equal name) Record.OrderedTypes.unpack_public_names ->
                Some { prefix = prefix @ inner_prefix; middle; suffix = inner_suffix @ suffix }
            | _ -> None)
        | _ -> None)
    | _ -> None


  let concatenation_from_unpack_expression ~parse_annotation annotation =
    let unpacked =
      let location = Location.any in
      let wrapped_in_tuple =
        subscript_for_annotation ~location "typing.Tuple" [annotation] |> Node.create ~location
      in
      match parse_annotation wrapped_in_tuple with
      | Tuple (Concatenation concatenation) -> Some concatenation
      | _ -> None
    in
    match annotation with
    | { Node.value = Expression.Starred (Once _); _ } -> unpacked
    | { Node.value = Expression.Subscript { base; _ }; _ }
      when List.exists ~f:(fun n -> name_is ~name:n base) Record.OrderedTypes.unpack_public_names ->
        unpacked
    | _ -> None


  let expand_in_concatenation ~prefix ~suffix = function
    | Concrete dimensions -> Tuple (Concrete (prefix @ dimensions @ suffix))
    | Concatenation { prefix = new_prefix; middle; suffix = new_suffix } ->
        Tuple (Concatenation { prefix = prefix @ new_prefix; middle; suffix = new_suffix @ suffix })


  (* This represents the splitting of two ordered types to match each other in length. The prefix
     contains the prefix elements of known length that both have, the suffix contains the suffix
     elements of known length that both have, and the middle part contains the rest.

     [int, bool, str, int, bool] <: [int, int, *Ts, T]

     will be represented as:

     * prefix_match: [int; bool], [int; int].

     * middle_match: [str; int], *Ts

     * suffix_match: [bool], T.

     We don't include `str` in the prefixes because the corresponding `*Ts` on the right side is not
     of known length. Note that this doesn't check for compatibility; it is a purely length-based
     operation. *)
  type 'annotation ordered_type_split = {
    prefix_pairs: ('annotation * 'annotation) list;
    middle_pair: 'annotation record * 'annotation record;
    suffix_pairs: ('annotation * 'annotation) list;
  }
  [@@deriving compare, equal, sexp, show, hash]

  let concatenate ~left ~right =
    match left, right with
    | Concrete left, Concrete right -> Some (Concrete (left @ right))
    | Concrete left, Concatenation ({ prefix; _ } as concatenation) ->
        Some (Concatenation { concatenation with prefix = left @ prefix })
    | Concatenation ({ suffix; _ } as concatenation), Concrete right ->
        Some (Concatenation { concatenation with suffix = suffix @ right })
    | Concatenation _, Concatenation _ ->
        (* TODO(T84854853). *)
        None


  (** Pair matching elements of the prefixes and suffixes.

      [left_prefix] <middle> [left_suffix]

      [right_prefix] <middle> [right_suffix]

      gets split into:

      * prefix_pairs: 1-1 pairs between left_prefix and right_prefix. If the middle element is an
      unbounded tuple Tuple[X, ...], then pad the prefix with X in order to match the other prefix.

      * suffix_pairs: 1-1 pairs between left_suffix and right_suffix. Pad unbounded elements if
      needed, as above.

      [left_prefix_unpaired] <middle> [left_suffix_unpaired]

      [right_prefix_unpaired] <middle> [right_suffix_unpaired] *)
  let pair_matching_elements
      { Concatenation.prefix = left_prefix; middle = left_middle; suffix = left_suffix }
      { Concatenation.prefix = right_prefix; middle = right_middle; suffix = right_suffix }
    =
    let ( left_prefix,
          left_prefix_unpaired,
          left_suffix_unpaired,
          left_suffix,
          right_prefix,
          right_prefix_unpaired,
          right_suffix_unpaired,
          right_suffix )
      =
      let pad ~to_the_left ~element ~length list =
        let padding = List.init (length - List.length list) ~f:(fun _ -> element) in
        if to_the_left then
          padding @ list
        else
          list @ padding
      in
      let pad_prefix = pad ~to_the_left:true in
      let pad_suffix = pad ~to_the_left:false in
      let prefix_length = Int.min (List.length left_prefix) (List.length right_prefix) in
      let suffix_length = Int.min (List.length left_suffix) (List.length right_suffix) in
      let left_prefix, left_prefix_unpaired = List.split_n left_prefix prefix_length in
      let right_prefix, right_prefix_unpaired = List.split_n right_prefix prefix_length in
      let left_suffix_unpaired, left_suffix =
        List.split_n left_suffix (List.length left_suffix - suffix_length)
      in
      let right_suffix_unpaired, right_suffix =
        List.split_n right_suffix (List.length right_suffix - suffix_length)
      in
      match left_middle, right_middle with
      | UnboundedElements left_unbounded, UnboundedElements right_unbounded ->
          let left_prefix, right_prefix =
            match left_prefix_unpaired, right_prefix_unpaired with
            | [], _ ->
                ( pad_suffix
                    ~element:left_unbounded
                    ~length:(List.length right_prefix + List.length right_prefix_unpaired)
                    left_prefix,
                  right_prefix @ right_prefix_unpaired )
            | _, [] ->
                ( left_prefix @ left_prefix_unpaired,
                  pad_suffix
                    ~element:right_unbounded
                    ~length:(List.length left_prefix + List.length left_prefix_unpaired)
                    right_prefix )
            | _, _ -> left_prefix, right_prefix
          in
          let left_suffix, right_suffix =
            match left_suffix_unpaired, right_suffix_unpaired with
            | [], _ ->
                ( pad_prefix
                    ~element:left_unbounded
                    ~length:(List.length right_suffix + List.length right_suffix_unpaired)
                    left_suffix,
                  right_suffix_unpaired @ right_suffix )
            | _, [] ->
                ( left_suffix_unpaired @ left_suffix,
                  pad_prefix
                    ~element:right_unbounded
                    ~length:(List.length left_suffix + List.length left_suffix_unpaired)
                    right_suffix )
            | _, _ -> left_suffix, right_suffix
          in
          (* There are no unpaired elements because we can always match two unbounded tuple
             concatenations by padding. *)
          left_prefix, [], [], left_suffix, right_prefix, [], [], right_suffix
      | UnboundedElements left_unbounded, _ ->
          let left_prefix, right_prefix, right_prefix_unpaired =
            match left_prefix_unpaired with
            | [] ->
                ( pad_suffix
                    ~element:left_unbounded
                    ~length:(List.length right_prefix + List.length right_prefix_unpaired)
                    left_prefix,
                  right_prefix @ right_prefix_unpaired,
                  [] )
            | _ -> left_prefix, right_prefix, right_prefix_unpaired
          in
          let left_suffix, right_suffix, right_suffix_unpaired =
            match left_suffix_unpaired with
            | [] ->
                ( pad_prefix
                    ~element:left_unbounded
                    ~length:(List.length right_suffix + List.length right_suffix_unpaired)
                    left_suffix,
                  right_suffix_unpaired @ right_suffix,
                  [] )
            | _ -> left_suffix, right_suffix, right_suffix_unpaired
          in
          ( left_prefix,
            left_prefix_unpaired,
            left_suffix_unpaired,
            left_suffix,
            right_prefix,
            right_prefix_unpaired,
            right_suffix_unpaired,
            right_suffix )
      | _, UnboundedElements right_unbounded ->
          let right_prefix, left_prefix, left_prefix_unpaired =
            match right_prefix_unpaired with
            | [] ->
                ( pad_suffix
                    ~element:right_unbounded
                    ~length:(List.length left_prefix + List.length left_prefix_unpaired)
                    right_prefix,
                  left_prefix @ left_prefix_unpaired,
                  [] )
            | _ -> right_prefix, left_prefix, left_prefix_unpaired
          in
          let right_suffix, left_suffix, left_suffix_unpaired =
            match right_suffix_unpaired with
            | [] ->
                ( pad_prefix
                    ~element:right_unbounded
                    ~length:(List.length left_suffix + List.length left_suffix_unpaired)
                    right_suffix,
                  left_suffix_unpaired @ left_suffix,
                  [] )
            | _ -> right_suffix, left_suffix, left_suffix_unpaired
          in
          ( left_prefix,
            left_prefix_unpaired,
            left_suffix_unpaired,
            left_suffix,
            right_prefix,
            right_prefix_unpaired,
            right_suffix_unpaired,
            right_suffix )
      | _, _ ->
          ( left_prefix,
            left_prefix_unpaired,
            left_suffix_unpaired,
            left_suffix,
            right_prefix,
            right_prefix_unpaired,
            right_suffix_unpaired,
            right_suffix )
    in
    match List.zip left_prefix right_prefix, List.zip left_suffix right_suffix with
    | Ok prefix_pairs, Ok suffix_pairs ->
        Some
          ( prefix_pairs,
            left_prefix_unpaired,
            right_prefix_unpaired,
            suffix_pairs,
            left_suffix_unpaired,
            right_suffix_unpaired )
    | _ -> None


  let split_matching_elements_by_length left right =
    let split_concrete_against_concatenation
        ~is_left_concrete
        ~concrete
        ~concatenation:{ Concatenation.prefix; middle; suffix }
      =
      let prefix_length = List.length prefix in
      let suffix_length = List.length suffix in
      let concrete_prefix, concrete_rest = List.split_n concrete prefix_length in
      let concrete_middle, concrete_suffix =
        List.split_n concrete_rest (List.length concrete_rest - suffix_length)
      in
      let prefix_pairs, middle_pair, suffix_pairs =
        match middle, is_left_concrete with
        | UnboundedElements unbounded, _ ->
            let concrete_length = List.length concrete in
            let middle =
              if concrete_length > prefix_length + suffix_length then
                List.init
                  (List.length concrete - List.length prefix - List.length suffix)
                  ~f:(fun _ -> unbounded)
              else
                []
            in
            let pairs =
              if is_left_concrete then
                List.zip concrete (prefix @ middle @ suffix)
              else
                List.zip (prefix @ middle @ suffix) concrete
            in
            pairs, (Concrete [], Concrete []), List.Or_unequal_lengths.Ok []
        | _, true ->
            ( List.zip concrete_prefix prefix,
              (Concrete concrete_middle, Concatenation { prefix = []; middle; suffix = [] }),
              List.zip concrete_suffix suffix )
        | _, false ->
            ( List.zip prefix concrete_prefix,
              (Concatenation { prefix = []; middle; suffix = [] }, Concrete concrete_middle),
              List.zip suffix concrete_suffix )
      in
      match prefix_pairs, middle_pair, suffix_pairs with
      | Ok prefix_pairs, middle_pair, Ok suffix_pairs ->
          Some { prefix_pairs; middle_pair; suffix_pairs }
      | _ -> None
    in
    match left, right with
    | Concrete left, Concrete right -> (
        match List.zip left right with
        | Ok prefix_pairs ->
            Some { prefix_pairs; middle_pair = Concrete [], Concrete []; suffix_pairs = [] }
        | Unequal_lengths -> None)
    | Concrete left, Concatenation concatenation ->
        split_concrete_against_concatenation ~is_left_concrete:true ~concrete:left ~concatenation
    | Concatenation concatenation, Concrete right ->
        split_concrete_against_concatenation ~is_left_concrete:false ~concrete:right ~concatenation
    | ( Concatenation ({ middle = left_middle; _ } as left_record),
        Concatenation ({ middle = right_middle; _ } as right_record) ) -> (
        pair_matching_elements left_record right_record
        >>= fun ( prefix_pairs,
                  left_prefix_unpaired,
                  right_prefix_unpaired,
                  suffix_pairs,
                  left_suffix_unpaired,
                  right_suffix_unpaired ) ->
        match left_middle, right_middle, right_prefix_unpaired, right_suffix_unpaired with
        | UnboundedElements left_unbounded, UnboundedElements right_unbounded, [], [] ->
            Some
              {
                prefix_pairs;
                middle_pair = Concrete [left_unbounded], Concrete [right_unbounded];
                suffix_pairs;
              }
        | _ ->
            let middle_pair =
              ( Concatenation
                  {
                    prefix = left_prefix_unpaired;
                    middle = left_middle;
                    suffix = left_suffix_unpaired;
                  },
                Concatenation
                  {
                    prefix = right_prefix_unpaired;
                    middle = right_middle;
                    suffix = right_suffix_unpaired;
                  } )
            in
            Some { prefix_pairs; middle_pair; suffix_pairs })


  let drop_prefix ~length = function
    | Concrete elements ->
        Concrete (List.drop elements length) |> Option.some_if (length <= List.length elements)
    | Concatenation ({ prefix; middle = UnboundedElements _; _ } as concatenation) ->
        (* We can drop indefinitely many elements after the concrete prefix because the middle
           element is an unbounded tuple. *)
        Concatenation { concatenation with prefix = List.drop prefix length } |> Option.some
    | Concatenation ({ prefix; middle = Variadic _; _ } as concatenation) ->
        (* Variadic or Broadcast middle element may be empty, so we cannot drop any elements past
           the concrete prefix. *)
        List.drop prefix length
        |> Option.some_if (length <= List.length prefix)
        >>| fun new_prefix -> Concatenation { concatenation with prefix = new_prefix }


  let index ~python_index = function
    | Concrete elements ->
        let index =
          if python_index < 0 then List.length elements + python_index else python_index
        in
        List.nth elements index
    | Concatenation { prefix; middle; suffix } -> (
        let result =
          if python_index >= 0 then
            List.nth prefix python_index
          else
            List.nth suffix (python_index + List.length suffix)
        in
        match middle with
        | UnboundedElements unbounded ->
            (* We can index indefinitely many elements in the unbounded tuple. *)
            result |> Option.value ~default:unbounded |> Option.some
        | Variadic _ ->
            (* Variadic middle element may be empty, so we cannot index any elements past the
               concrete prefix. *)
            result)


  let coalesce_ordered_types ordered_types =
    let concatenate_ordered_types = function
      | [] -> Some (Concrete [])
      | head :: tail ->
          let concatenate sofar next = sofar >>= fun left -> concatenate ~left ~right:next in
          List.fold tail ~f:concatenate ~init:(Some head)
    in
    let is_concrete = function
      | Concrete _ -> true
      | _ -> false
    in
    let separated_prefixes_and_suffixes =
      List.concat_map ordered_types ~f:(function
          | Concatenation { prefix; middle; suffix } ->
              [Concrete prefix; Concatenation { prefix = []; middle; suffix = [] }; Concrete suffix]
          | Concrete _ as concrete -> [concrete])
    in
    let concrete_tuples_prefix, rest =
      List.split_while separated_prefixes_and_suffixes ~f:is_concrete
    in
    let concrete_tuples_suffix, middle_items =
      List.rev rest |> List.split_while ~f:is_concrete |> fun (xs, ys) -> List.rev xs, List.rev ys
    in
    match middle_items with
    | _ :: _ :: _ ->
        (* There are multiple unpacked tuples. Coalesce them into a single tuple with a common
           iterable type so that we can compare it to an expected tuple type. *)
        let extract_common_type = function
          | Concrete items -> Some (Constructors.union items)
          | Concatenation { middle = UnboundedElements item_type; prefix; suffix } ->
              Constructors.union (item_type :: (prefix @ suffix)) |> Option.some
          | Concatenation { middle = Variadic _; _ } -> None
        in
        List.map middle_items ~f:extract_common_type
        |> Option.all
        >>| Constructors.union
        >>| (fun item_type ->
              concrete_tuples_prefix
              @ [Concatenation (Concatenation.create_from_unbounded_element item_type)]
              @ concrete_tuples_suffix)
        >>= concatenate_ordered_types
    | _ -> concatenate_ordered_types ordered_types
end

module TypeOperation = struct
  open T
  include Record.TypeOperation

  module Compose = struct
    include Record.TypeOperation.Compose

    type t = T.t Record.TypeOperation.Compose.t

    let flatten_record input =
      let record_to_list = function
        | OrderedTypes.Concrete annotations -> annotations
        | Concatenation { prefix; middle; suffix } ->
            prefix
            @ [TypeOperation (Compose (Concatenation { prefix = []; middle; suffix = [] }))]
            @ suffix
      in
      let map_types_to_records = function
        | TypeOperation (Compose record) -> record
        | other -> Concrete [other]
      in
      record_to_list input |> List.map ~f:map_types_to_records


    let create record =
      let is_potentially_callable = function
        | Callable _
        | Parametric _
        | Variable _
        | Any
        | Primitive _
        | TypeOperation (Compose _) ->
            true
        | _ -> false
      in
      let list_to_record list =
        let combine_records left right =
          left >>= fun inner_left -> OrderedTypes.concatenate ~left:inner_left ~right
        in
        list |> List.fold ~init:(Some (OrderedTypes.Concrete [])) ~f:combine_records
      in
      let is_legal_record input =
        match input with
        | OrderedTypes.Concrete annotations when List.for_all ~f:is_potentially_callable annotations
          ->
            Some input
        | Concatenation { prefix; middle = UnboundedElements element; suffix }
          when is_potentially_callable element ->
            Option.some_if (List.for_all ~f:is_potentially_callable (prefix @ suffix)) input
        | Concatenation { prefix; middle = Variadic _; suffix } ->
            Option.some_if (List.for_all ~f:is_potentially_callable (prefix @ suffix)) input
        | _ -> None
      in
      record
      |> flatten_record
      |> list_to_record
      >>= is_legal_record
      >>| fun result -> TypeOperation (Compose result)
  end

  type t = T.t Record.TypeOperation.record
end

module PyreReadOnly = struct
  open T

  let create = Constructors.pyre_read_only

  let unpack_readonly = function
    | PyreReadOnly type_ -> Some type_
    | _ -> None


  let is_readonly type_ = unpack_readonly type_ |> Option.is_some

  let strip_readonly type_ = Transforms.apply_type_map type_ ~type_map:unpack_readonly

  let contains_readonly type_ = Visitors.exists type_ ~predicate:is_readonly

  (* Lift `PyreReadOnly` from `element_type` to the overall container.

     i.e., turn `make_container PyreReadOnly[Foo]` to `PyreReadOnly[make_container Foo]`. *)
  let lift_readonly_if_possible ~make_container element_type =
    unpack_readonly element_type
    >>| (fun inner_type -> create (make_container inner_type))
    |> Option.value ~default:(make_container element_type)
end

module Variable = struct
  open T
  include Record.Variable

  let name type_variable =
    match type_variable with
    | TypeVarVariable { name; _ } -> name
    | ParamSpecVariable { name; _ } -> name
    | TypeVarTupleVariable { name; _ } -> name


  module Namespace = struct
    include Record.Variable.Namespace

    let fresh = ref 1

    let reset () = fresh := 1

    let create_fresh () =
      let namespace = !fresh in
      fresh := namespace + 1;
      namespace
  end

  type unary_t = T.t Record.Variable.TypeVar.record [@@deriving compare, equal, sexp, show, hash]

  type unary_domain = T.t [@@deriving compare, equal, sexp, show, hash]

  type parameter_variadic_t = T.t Record.Variable.ParamSpec.record
  [@@deriving compare, equal, sexp, show, hash]

  type parameter_variadic_domain = Callable.parameters [@@deriving compare, equal, sexp, show, hash]

  type tuple_variadic_t = T.t Record.Variable.TypeVarTuple.record
  [@@deriving compare, equal, sexp, show, hash]

  type tuple_variadic_domain = T.t OrderedTypes.record [@@deriving compare, equal, sexp, show, hash]

  type pair =
    | TypeVarPair of unary_t * unary_domain
    | ParamSpecPair of parameter_variadic_t * parameter_variadic_domain
    | TypeVarTuplePair of tuple_variadic_t * tuple_variadic_domain
  [@@deriving compare, equal, sexp, show, hash]

  module type VariableKind = sig
    type t [@@deriving compare, equal, sexp, show, hash]

    module Map : Core.Map.S with type Key.t = t

    val is_free : t -> bool

    val is_escaped_and_free : t -> bool

    val mark_as_bound : t -> t

    val mark_as_escaped : t -> t

    val mark_as_free : t -> t

    val namespace : t -> namespace:Namespace.t -> t

    val dequalify : t -> dequalify_map:Reference.t Reference.Map.t -> t

    type domain [@@deriving compare, equal, sexp, show, hash]

    val any : domain

    val self_reference : t -> domain

    val pair : t -> domain -> pair
  end

  module TypeVar = struct
    include Record.Variable.TypeVar

    type t = T.t record [@@deriving compare, equal, sexp, show, hash]

    type domain = T.t [@@deriving compare, equal, sexp, show, hash]

    module Map = Core.Map.Make (struct
      type t = T.t record [@@deriving compare, sexp]
    end)

    let any = Any

    let self_reference variable = Variable variable

    let pair variable value = TypeVarPair (variable, value)

    let is_free = function
      | { state = Free _; _ } -> true
      | _ -> false


    let namespace variable ~namespace = { variable with namespace }

    let mark_as_bound variable = { variable with state = InFunction }

    let upper_bound { constraints; _ } =
      let open Constructors in
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
      | Bound bound -> Predicates.contains_variable bound
      | Explicit explicits -> List.exists explicits ~f:Predicates.contains_variable
      | LiteralIntegers -> false


    let mark_as_escaped variable = { variable with state = Free { escaped = true } }

    let mark_as_free variable = { variable with state = Free { escaped = false } }

    let local_collect = function
      | Variable variable -> [variable]
      | _ -> []


    let local_replace replacement = function
      | Variable variable -> replacement variable
      | _ -> None


    let dequalify ({ name; _ } as variable) ~dequalify_map =
      { variable with name = dequalify_identifier dequalify_map name }
  end

  module ParamSpec = struct
    include Record.Variable.ParamSpec

    type t = T.t record [@@deriving compare, equal, sexp, show, hash]

    type domain = Callable.parameters [@@deriving compare, equal, sexp, show, hash]

    module Map = Core.Map.Make (struct
      type t = T.t record [@@deriving compare, sexp]
    end)

    let name { name; _ } = name

    let any = Callable.Undefined

    let self_reference variable = Callable.FromParamSpec { head = []; variable }

    let pair variable value = ParamSpecPair (variable, value)

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
        | Record.Callable.FromParamSpec { head; variable } ->
            let open Record.Callable in
            let apply_head ~head = function
              | FromParamSpec { head = inner_head; variable } ->
                  FromParamSpec { head = head @ inner_head; variable }
              | Undefined -> Undefined
              | Defined tail -> Defined (Callable.prepend_anonymous_parameters ~head ~tail)
            in
            replacement variable
            >>| apply_head ~head
            |> Option.value ~default:(FromParamSpec { head; variable })
        | parameters -> parameters
      in
      match annotation with
      | Callable callable ->
          Callable.map_parameters callable ~f:map
          |> (fun callable -> Callable callable)
          |> Option.some
      | Parametric { name; arguments } ->
          let arguments =
            let map_argument = function
              | Argument.CallableParameters parameters ->
                  Argument.CallableParameters (map parameters)
              | parameter -> parameter
            in
            List.map arguments ~f:map_argument
          in
          Some (Parametric { name; arguments })
      | _ -> None


    let mark_as_escaped variable = { variable with state = Free { escaped = true } }

    let mark_as_free variable = { variable with state = Free { escaped = false } }

    let local_collect = function
      | Callable { implementation; overloads; _ } ->
          let extract = function
            | { Record.Callable.parameters = FromParamSpec { variable; _ }; _ } -> Some variable
            | _ -> None
          in
          List.filter_map (implementation :: overloads) ~f:extract
      | Parametric { arguments; _ } ->
          let extract = function
            | Argument.CallableParameters (FromParamSpec { variable; _ }) -> Some variable
            | _ -> None
          in
          List.filter_map arguments ~f:extract
      | _ -> []


    let dequalify ({ name; _ } as variable) ~dequalify_map =
      { variable with name = dequalify_identifier dequalify_map name }


    let of_component_annotations ~get_param_spec ~args_annotation ~kwargs_annotation =
      let get_param_spec_base_identifier annotation component_name =
        match annotation with
        | {
         Node.value =
           Expression.Name
             (Attribute { base = { Node.value = Expression.Name base_name; _ }; attribute; _ });
         _;
        }
          when Identifier.equal attribute component_name ->
            name_to_reference base_name >>| Reference.show
        | _ -> None
      in
      let open Record.Variable.ParamSpec.Components in
      let open PrettyPrinting.Variable.ParamSpec.Components in
      match
        ( get_param_spec_base_identifier args_annotation (component_name PositionalArguments),
          get_param_spec_base_identifier kwargs_annotation (component_name KeywordArguments) )
      with
      | Some positionals_base, Some keywords_base
        when Identifier.equal positionals_base keywords_base ->
          get_param_spec positionals_base
      | _ -> None


    module Components = struct
      include Record.Variable.ParamSpec.Components

      type decomposition = {
        positional_component: T.t;
        keyword_component: T.t;
      }

      let combine { positional_component; keyword_component } =
        let component_agnostic_equal left right =
          equal
            { left with component = KeywordArguments }
            { right with component = KeywordArguments }
        in
        match positional_component, keyword_component with
        | ( ParamSpecComponent ({ component = PositionalArguments; _ } as positional_component),
            ParamSpecComponent ({ component = KeywordArguments; _ } as keyword_component) )
          when component_agnostic_equal positional_component keyword_component ->
            let { variable_name = name; variable_namespace = namespace; _ } =
              positional_component
            in
            Some { name; namespace; state = InFunction }
        | _ -> None


      let component { component; _ } = component
    end

    let decompose { name = variable_name; namespace = variable_namespace; _ } =
      {
        Components.positional_component =
          ParamSpecComponent { component = PositionalArguments; variable_name; variable_namespace };
        keyword_component =
          ParamSpecComponent { component = KeywordArguments; variable_name; variable_namespace };
      }
  end

  module TypeVarTuple = struct
    include Record.Variable.TypeVarTuple

    type t = T.t record [@@deriving compare, equal, sexp, show, hash]

    type domain = T.t OrderedTypes.record [@@deriving compare, equal, sexp, show, hash]

    module Map = Core.Map.Make (struct
      type t = T.t record [@@deriving compare, sexp]
    end)

    let synthetic_class_name_for_error = "$synthetic_class_for_variadic_error"

    let any = OrderedTypes.create_unbounded_concatenation Any

    let name { name; _ } = name

    let self_reference variadic =
      OrderedTypes.Concatenation (OrderedTypes.Concatenation.create variadic)


    let pair variable value = TypeVarTuplePair (variable, value)

    let is_free = function
      | { state = Free _; _ } -> true
      | _ -> false


    let namespace variable ~namespace = { variable with namespace }

    let mark_as_bound variable = { variable with state = InFunction }

    let is_escaped_and_free = function
      | { state = Free { escaped }; _ } -> escaped
      | _ -> false


    let mark_as_escaped variable = { variable with state = Free { escaped = true } }

    let mark_as_free variable = { variable with state = Free { escaped = false } }

    let local_collect annotation =
      let collect_unpackable = function
        | Record.OrderedTypes.Concatenation.Variadic variadic -> [variadic]
        | _ -> []
      in
      match annotation with
      | Parametric { arguments; _ } ->
          let extract = function
            | Argument.Unpacked unpackable -> collect_unpackable unpackable
            | _ -> []
          in
          List.concat_map arguments ~f:extract
      | Tuple (Concatenation { middle = unpackable; _ }) -> collect_unpackable unpackable
      | Callable { implementation; overloads; _ } ->
          let extract = function
            | { Record.Callable.parameters = Defined parameters; _ } ->
                List.find_map parameters ~f:(function
                    | Variable (Concatenation { middle = unpackable; _ }) ->
                        Some (collect_unpackable unpackable)
                    | _ -> None)
                |> Option.value ~default:[]
            | _ -> []
          in
          List.concat_map (implementation :: overloads) ~f:extract
      | TypeOperation (Compose (Concatenation { middle = unpackable; _ })) ->
          collect_unpackable unpackable
      | _ -> []


    let local_replace replacement annotation =
      let map_tuple ~f = function
        | Tuple record -> f record
        | other -> other
      in
      let replace_unpackable = function
        | OrderedTypes.Concatenation.Variadic variadic ->
            replacement variadic >>| fun result -> Tuple result
        | _ -> None
      in
      match annotation with
      | Parametric ({ arguments; _ } as parametric) ->
          let replace argument =
            let replaced =
              match argument with
              | Argument.Unpacked unpackable -> (
                  replace_unpackable unpackable
                  >>| function
                  | Tuple record -> OrderedTypes.to_arguments record
                  | other -> [Argument.Single other])
              | _ -> None
            in
            Option.value ~default:[argument] replaced
          in
          let arguments = List.concat_map arguments ~f:replace in
          let default = Parametric { parametric with arguments } |> Option.some in
          let extract_broadcast_error = function
            | Argument.Single
                (Parametric { name = "pyre_extensions.BroadcastError"; _ } as parametric) ->
                Some parametric
            | _ -> None
          in
          List.find_map ~f:extract_broadcast_error arguments
          |> fun result -> Option.first_some result default
      | Tuple (Concatenation { prefix; middle = unpackable; suffix }) ->
          replace_unpackable unpackable
          >>| map_tuple ~f:(OrderedTypes.expand_in_concatenation ~prefix ~suffix)
      | Callable callable -> (
          let replace_variadic parameters_so_far parameters =
            let expanded_parameters =
              match parameters with
              | Callable.CallableParamType.Variable
                  (Concatenation ({ prefix; middle = unpackable; suffix } as concatenation)) ->
                  let encode_ordered_types_into_parameters = function
                    | OrderedTypes.Concrete concretes ->
                        let start_index = List.length parameters_so_far in
                        let make_anonymous_parameter index annotation =
                          Callable.CallableParamType.PositionalOnly
                            { index = start_index + index; annotation; default = false }
                        in
                        List.mapi (prefix @ concretes @ suffix) ~f:make_anonymous_parameter
                        @ parameters_so_far
                    | Concatenation { prefix = new_prefix; middle; suffix = new_suffix } ->
                        [
                          Variable
                            (Concatenation
                               {
                                 prefix = prefix @ new_prefix;
                                 middle;
                                 suffix = new_suffix @ suffix;
                               });
                        ]
                  in
                  let handle_potential_error = function
                    | Parametric { name = "pyre_extensions.BroadcastError"; _ } as broadcast_error
                      ->
                        Error broadcast_error
                    | Tuple record -> Ok (encode_ordered_types_into_parameters record)
                    | other ->
                        Ok
                          [
                            Callable.CallableParamType.PositionalOnly
                              {
                                index = List.length parameters_so_far;
                                annotation = other;
                                default = false;
                              };
                          ]
                  in
                  replace_unpackable unpackable
                  >>| handle_potential_error
                  |> Option.value
                       ~default:
                         (Ok [Callable.CallableParamType.Variable (Concatenation concatenation)])
              | parameter -> Ok [parameter]
            in
            expanded_parameters
            |> Result.map ~f:(fun result -> List.rev_append result parameters_so_far)
          in
          let map_defined = function
            | Record.Callable.Defined parameters ->
                Result.map
                  ~f:(fun result -> Record.Callable.Defined (List.rev result))
                  (List.fold_result ~init:[] ~f:replace_variadic parameters)
            | parameters -> Ok parameters
          in
          match Callable.map_parameters_with_result ~f:map_defined callable with
          | Ok result_callable -> Some (Callable result_callable)
          | Error broadcast_error -> Some broadcast_error)
      | TypeOperation (Compose (Concatenation { prefix; middle; suffix })) -> (
          replace_unpackable middle
          >>| map_tuple ~f:(OrderedTypes.expand_in_concatenation ~prefix ~suffix)
          >>= function
          | Tuple results -> Some (TypeOperation (Compose results))
          | _ -> None)
      | _ -> None


    let dequalify ({ name; _ } as variable) ~dequalify_map =
      { variable with name = dequalify_identifier dequalify_map name }
  end

  module GlobalTransforms = struct
    module type VariableKind = sig
      include VariableKind

      (* We don't want these to be part of the public interface for TypeVar or ParamSpec *)
      val local_replace : (t -> domain option) -> T.t -> T.t option

      val local_collect : T.t -> t list
    end

    module Make (Variable : VariableKind) = struct
      include Variable

      let replace_all operation =
        Transforms.apply_type_map
          ~visit_children_before:true
          ~type_map:(Variable.local_replace operation)


      let map operation =
        replace_all (fun variable -> operation variable |> Variable.self_reference |> Option.some)


      let mark_all_as_bound ?specific =
        let in_list variable =
          match specific with
          | Some variables -> List.mem variables ~equal:Variable.equal variable
          | None -> true
        in
        let mark_as_bound_if_in_list variable =
          if in_list variable then
            Variable.mark_as_bound variable
          else
            variable
        in
        map mark_as_bound_if_in_list


      let mark_all_as_free ?specific =
        let in_list variable =
          match specific with
          | Some variables -> List.mem variables ~equal:Variable.equal variable
          | None -> true
        in
        let mark_as_free_if_in_list variable =
          if in_list variable then
            Variable.mark_as_free variable
          else
            variable
        in
        map mark_as_free_if_in_list


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
        let module Collector = VisitWithTransform.Make (struct
          type state = Variable.t list

          let visit_children_before _ _ = true

          let visit_children_after = false

          let visit sofar annotation =
            let new_state = Variable.local_collect annotation @ sofar in
            { VisitWithTransform.transformed_annotation = annotation; new_state }
        end)
        in
        fst (Collector.visit [] annotation) |> List.rev


      let all_free_variables annotation = collect_all annotation |> List.filter ~f:Variable.is_free

      let contains_escaped_free_variable annotation =
        collect_all annotation |> List.exists ~f:Variable.is_escaped_and_free
    end

    module type S = sig
      type t

      type domain

      val replace_all : (t -> domain option) -> T.t -> T.t

      val collect_all : T.t -> t list
    end

    module TypeVar = Make (TypeVar)
    module ParamSpec = Make (ParamSpec)
    module TypeVarTuple = Make (TypeVarTuple)
  end

  type t = T.t Record.Variable.record [@@deriving compare, equal, sexp, show, hash]

  module Set = Core.Set.Make (struct
    type t = T.t Record.Variable.record [@@deriving compare, sexp]
  end)

  let pp_concise = PrettyPrinting.Variable.pp_concise ~pp_type:PrettyPrinting.pp

  module Declaration = struct
    type t =
      | DTypeVar of {
          name: Identifier.t;
          constraints: Expression.t Record.TypeVarConstraints.t;
          variance: Record.PreInferenceVariance.t;
          infer_variance: bool;
        }
      | DTypeVarTuple of { name: Identifier.t }
      | DParamSpec of { name: Identifier.t }
    [@@deriving equal, compare, sexp, show, hash]

    let parse expression ~target =
      match expression with
      | { Node.value = Expression.Call { callee; arguments = _arg :: arguments; origin = _ }; _ }
        when name_is ~name:"typing.TypeVar" callee ->
          let constraints =
            let explicits =
              let explicit = function
                | { Call.Argument.name = None; value } -> Some value
                | _ -> None
              in
              List.filter_map ~f:explicit arguments
            in
            let bound =
              let bound = function
                | { Call.Argument.value; name = Some { Node.value = bound; _ } }
                  when String.equal (Identifier.sanitized bound) "bound" ->
                    Some value
                | _ -> None
              in
              List.find_map ~f:bound arguments
            in
            if not (List.is_empty explicits) then
              Record.TypeVarConstraints.Explicit explicits
            else if Option.is_some bound then
              Bound (Option.value_exn bound)
            else
              Unconstrained
          in
          let variance =
            let variance_definition = function
              | {
                  Call.Argument.name = Some { Node.value = name; _ };
                  value = { Node.value = Constant Constant.True; _ };
                }
                when String.equal (Identifier.sanitized name) "covariant" ->
                  Some Record.PreInferenceVariance.P_Covariant
              | {
                  Call.Argument.name = Some { Node.value = name; _ };
                  value = { Node.value = Constant Constant.True; _ };
                }
                when String.equal (Identifier.sanitized name) "contravariant" ->
                  Some Record.PreInferenceVariance.P_Contravariant
              | {
                  Call.Argument.name = Some { Node.value = name; _ };
                  value = { Node.value = Constant Constant.True; _ };
                }
                when String.equal (Identifier.sanitized name) "infer_variance" ->
                  Some Record.PreInferenceVariance.P_Undefined
              | _ -> None
            in
            List.find_map arguments ~f:variance_definition
            |> Option.value ~default:Record.PreInferenceVariance.P_Invariant
          in
          let infer_variance =
            let variance_definition = function
              | {
                  Call.Argument.name = Some { Node.value = name; _ };
                  value = { Node.value = Constant Constant.True; _ };
                }
                when String.equal (Identifier.sanitized name) "infer_variance" ->
                  true
              | _ -> false
            in
            List.exists arguments ~f:variance_definition
          in

          Some (DTypeVar { name = Reference.show target; constraints; variance; infer_variance })
      | {
       Node.value =
         Expression.Call
           {
             callee;
             arguments =
               [{ Call.Argument.value = { Node.value = Constant (Constant.String _); _ }; _ }];
             origin = _;
           };
       _;
      }
        when name_is ~name:"typing_extensions.IntVar" callee ->
          Some
            (DTypeVar
               {
                 name = Reference.show target;
                 constraints = LiteralIntegers;
                 variance = Record.PreInferenceVariance.P_Invariant;
                 infer_variance = false;
               })
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
                            origin = _;
                          });
                    _;
                  };
                arguments =
                  [{ Call.Argument.value = { Node.value = Constant (Constant.String _); _ }; _ }];
                origin = _;
              };
          _;
        }
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
                            base =
                              {
                                Node.value = Name (Name.Identifier ("typing" | "typing_extensions"));
                                _;
                              };
                            attribute = "ParamSpec";
                            origin = _;
                          });
                    _;
                  };
                arguments =
                  [{ Call.Argument.value = { Node.value = Constant (Constant.String _); _ }; _ }];
                origin = _;
              };
          _;
        } ->
          Some (DParamSpec { name = Reference.show target })
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
                         base =
                           {
                             Node.value =
                               ( Name (Name.Identifier "pyre_extensions")
                               | Name (Name.Identifier "typing_extensions")
                               | Name (Name.Identifier "typing") );
                             _;
                           };
                         attribute = "TypeVarTuple";
                         origin = _;
                       });
                 _;
               };
             arguments =
               [{ Call.Argument.value = { Node.value = Constant (Constant.String _); _ }; _ }];
             origin = _;
           };
       _;
      } ->
          Some (DTypeVarTuple { name = Reference.show target })
      | _ -> None
  end

  type variable_zip_result = {
    variable_pair: pair;
    received_argument: Argument.t;
  }
  [@@deriving compare, equal, sexp, show, hash]

  let of_declaration declaration ~create_type =
    match declaration with
    | Declaration.DTypeVar { name; constraints; _ } ->
        let constraints =
          match constraints with
          | Bound expression -> Record.TypeVarConstraints.Bound (create_type expression)
          | Explicit expressions -> Explicit (List.map ~f:create_type expressions)
          | Unconstrained -> Unconstrained
          | LiteralIntegers -> LiteralIntegers
        in
        TypeVarVariable (TypeVar.create ~constraints name)
    | Declaration.DParamSpec { name } -> ParamSpecVariable (ParamSpec.create name)
    | Declaration.DTypeVarTuple { name } -> TypeVarTupleVariable (TypeVarTuple.create name)


  let constraints_of_bound bound ~create_type =
    match bound with
    | Some bound -> (
        match bound.Node.value with
        | Expression.Tuple t -> Record.TypeVarConstraints.Explicit (List.map ~f:create_type t)
        | _ -> Record.TypeVarConstraints.Bound (create_type bound))
    | None -> Unconstrained


  let of_ast_type_param type_param ~create_type =
    (* TODO T194670955: look into LiteralIntegers, which is for shape types. We could potentially
       remove it entirely. For now, we do not handle it. *)
    match type_param.Node.value with
    | Ast.Expression.TypeParam.TypeVar { name; bound; _ } ->
        let constraints = constraints_of_bound bound ~create_type in
        TypeVarVariable (TypeVar.create ~constraints name)
    | Ast.Expression.TypeParam.TypeVarTuple name -> TypeVarTupleVariable (TypeVarTuple.create name)
    | Ast.Expression.TypeParam.ParamSpec name -> ParamSpecVariable (ParamSpec.create name)


  let dequalify dequalify_map = function
    | TypeVarVariable variable -> TypeVarVariable (TypeVar.dequalify variable ~dequalify_map)
    | ParamSpecVariable variable -> ParamSpecVariable (ParamSpec.dequalify variable ~dequalify_map)
    | TypeVarTupleVariable variable ->
        TypeVarTupleVariable (TypeVarTuple.dequalify variable ~dequalify_map)


  let namespace variable ~namespace =
    match variable with
    | TypeVarVariable variable -> TypeVarVariable (TypeVar.namespace variable ~namespace)
    | ParamSpecVariable variable -> ParamSpecVariable (ParamSpec.namespace variable ~namespace)
    | TypeVarTupleVariable variable ->
        TypeVarTupleVariable (TypeVarTuple.namespace variable ~namespace)


  let partition =
    let partitioner = function
      | TypeVarVariable variable -> `Fst variable
      | ParamSpecVariable variable -> `Snd variable
      | TypeVarTupleVariable variable -> `Trd variable
    in
    List.partition3_map ~f:partitioner


  let mark_all_variables_as_bound ?specific annotation =
    let specific_unaries, specific_parameters_variadics, specific_tuple_variadics =
      match specific >>| partition with
      | None -> None, None, None
      | Some (unaries, parameters, tuples) -> Some unaries, Some parameters, Some tuples
    in
    GlobalTransforms.TypeVar.mark_all_as_bound ?specific:specific_unaries annotation
    |> GlobalTransforms.ParamSpec.mark_all_as_bound ?specific:specific_parameters_variadics
    |> GlobalTransforms.TypeVarTuple.mark_all_as_bound ?specific:specific_tuple_variadics


  let mark_as_bound = function
    | TypeVarVariable variable -> TypeVarVariable (GlobalTransforms.TypeVar.mark_as_bound variable)
    | ParamSpecVariable variable ->
        ParamSpecVariable (GlobalTransforms.ParamSpec.mark_as_bound variable)
    | TypeVarTupleVariable variable ->
        TypeVarTupleVariable (GlobalTransforms.TypeVarTuple.mark_as_bound variable)


  let mark_all_variables_as_free ?specific annotation =
    let specific_unaries, specific_parameters_variadics, specific_tuple_variadics =
      match specific >>| partition with
      | None -> None, None, None
      | Some (unaries, parameters, tuples) -> Some unaries, Some parameters, Some tuples
    in
    GlobalTransforms.TypeVar.mark_all_as_free ?specific:specific_unaries annotation
    |> GlobalTransforms.ParamSpec.mark_all_as_free ?specific:specific_parameters_variadics
    |> GlobalTransforms.TypeVarTuple.mark_all_as_free ?specific:specific_tuple_variadics


  let namespace_all_free_variables annotation ~namespace =
    GlobalTransforms.TypeVar.namespace_all_free_variables annotation ~namespace
    |> GlobalTransforms.ParamSpec.namespace_all_free_variables ~namespace
    |> GlobalTransforms.TypeVarTuple.namespace_all_free_variables ~namespace


  let all_free_variables annotation =
    let unaries =
      GlobalTransforms.TypeVar.all_free_variables annotation
      |> List.map ~f:(fun variable -> TypeVarVariable variable)
    in
    let callable_variadics =
      GlobalTransforms.ParamSpec.all_free_variables annotation
      |> List.map ~f:(fun variable -> ParamSpecVariable variable)
    in
    let tuple_variadics =
      GlobalTransforms.TypeVarTuple.all_free_variables annotation
      |> List.map ~f:(fun variable -> TypeVarTupleVariable variable)
    in
    unaries @ callable_variadics @ tuple_variadics


  let all_variables_are_resolved annotation = all_free_variables annotation |> List.is_empty

  let mark_all_free_variables_as_escaped ?specific annotation =
    let fresh_namespace = Namespace.create_fresh () in
    let variables =
      match specific with
      | Some variables -> variables
      | None -> all_free_variables annotation
    in
    let specific_unaries, specific_parameters_variadics, specific_tuple_variadics =
      partition variables
    in
    GlobalTransforms.TypeVar.mark_as_escaped
      annotation
      ~variables:specific_unaries
      ~namespace:fresh_namespace
    |> GlobalTransforms.ParamSpec.mark_as_escaped
         ~variables:specific_parameters_variadics
         ~namespace:fresh_namespace
    |> GlobalTransforms.TypeVarTuple.mark_as_escaped
         ~variables:specific_tuple_variadics
         ~namespace:fresh_namespace


  let collapse_all_escaped_variable_unions annotation =
    let module ConcreteTransform = VisitWithTransform.Make (struct
      type state = unit

      let visit_children_before _ _ = true

      let visit_children_after = false

      let visit new_state annotation =
        let transformed_annotation =
          match annotation with
          | Union arguments ->
              let not_escaped_free_variable = function
                | Variable variable -> not (TypeVar.is_escaped_and_free variable)
                | _ -> true
              in
              List.filter arguments ~f:not_escaped_free_variable |> Constructors.union
          | _ -> annotation
        in
        { VisitWithTransform.transformed_annotation; new_state }
    end)
    in
    snd (ConcreteTransform.visit () annotation)


  let contains_escaped_free_variable annotation =
    GlobalTransforms.TypeVar.contains_escaped_free_variable annotation
    || GlobalTransforms.ParamSpec.contains_escaped_free_variable annotation
    || GlobalTransforms.TypeVarTuple.contains_escaped_free_variable annotation


  let convert_all_escaped_free_variables_to_anys annotation =
    GlobalTransforms.TypeVar.convert_all_escaped_free_variables_to_anys annotation
    |> GlobalTransforms.ParamSpec.convert_all_escaped_free_variables_to_anys
    |> GlobalTransforms.TypeVarTuple.convert_all_escaped_free_variables_to_anys


  let converge_all_variable_namespaces annotation =
    GlobalTransforms.TypeVar.converge_all_variable_namespaces annotation
    |> GlobalTransforms.ParamSpec.converge_all_variable_namespaces
    |> GlobalTransforms.TypeVarTuple.converge_all_variable_namespaces


  let coalesce_if_all_single arguments =
    Argument.all_singles arguments
    >>| (fun singles ->
          [
            Argument.CallableParameters
              (Defined (Callable.prepend_anonymous_parameters ~head:singles ~tail:[]));
          ])
    |> Option.value ~default:arguments


  let make_variable_pair variable received_argument =
    let variable_pair =
      match variable, received_argument with
      | TypeVarVariable unary, Argument.Single annotation -> TypeVarPair (unary, annotation)
      | ParamSpecVariable parameter_variadic, CallableParameters callable_parameters ->
          ParamSpecPair (parameter_variadic, callable_parameters)
      | TypeVarVariable unary, _ -> TypeVarPair (unary, TypeVar.any)
      | ParamSpecVariable parameter_variadic, _ -> ParamSpecPair (parameter_variadic, ParamSpec.any)
      | TypeVarTupleVariable tuple_variadic, _ ->
          (* We should not hit this case at all. *)
          TypeVarTuplePair (tuple_variadic, TypeVarTuple.any)
    in
    { variable_pair; received_argument }


  let pairs_for_variadic_class ~non_variadic_prefix_length variables arguments =
    let variables_prefix, variables_rest = List.split_n variables non_variadic_prefix_length in
    match variables_rest with
    | TypeVarTupleVariable variadic :: variables_suffix ->
        let arguments_prefix, arguments_rest = List.split_n arguments non_variadic_prefix_length in
        let arguments_middle, arguments_suffix =
          List.split_n arguments_rest (List.length arguments_rest - List.length variables_suffix)
        in
        let pairs () =
          match
            ( List.map2 variables_prefix arguments_prefix ~f:make_variable_pair,
              List.map2 variables_suffix arguments_suffix ~f:make_variable_pair )
          with
          | Ok prefix_pairs, Ok suffix_pairs ->
              let variadic_pair =
                let ordered_type =
                  match OrderedTypes.concatenation_from_arguments arguments_middle with
                  | Some ordered_type -> Some ordered_type
                  | None ->
                      Argument.all_singles arguments_middle
                      >>| fun singles -> OrderedTypes.Concrete singles
                in
                ordered_type
                >>| (fun ordered_type ->
                      {
                        variable_pair = TypeVarTuplePair (variadic, ordered_type);
                        received_argument = Argument.Single (Tuple ordered_type);
                      })
                |> Option.value
                     ~default:
                       {
                         variable_pair = TypeVarTuplePair (variadic, TypeVarTuple.any);
                         received_argument =
                           Argument.Single
                             (Constructors.parametric
                                TypeVarTuple.synthetic_class_name_for_error
                                arguments_middle);
                       }
              in
              prefix_pairs @ [variadic_pair] @ suffix_pairs |> Option.some
          | _ -> None
        in
        let has_variadic_in_prefix_or_suffix =
          List.exists arguments_prefix ~f:Argument.is_unpacked
          || List.exists arguments_suffix ~f:Argument.is_unpacked
        in
        if has_variadic_in_prefix_or_suffix then
          None
        else
          pairs ()
    | _ -> None


  (* Zip the generic variables `Generic[T1, T2]` of a class with its arguments Foo[int, str]. *)
  let zip_variables_with_arguments_including_mismatches ~arguments variables =
    let arguments =
      match variables with
      | [ParamSpecVariable _] -> coalesce_if_all_single arguments
      | _ -> arguments
    in
    let variadic_index index = function
      | TypeVarTupleVariable _ -> Some index
      | _ -> None
    in
    match List.filter_mapi variables ~f:variadic_index with
    | [unpacked_index] ->
        pairs_for_variadic_class ~non_variadic_prefix_length:unpacked_index variables arguments
    | [] -> (
        match List.map2 variables arguments ~f:make_variable_pair with
        | Ok pairs -> Some pairs
        | Unequal_lengths -> None)
    | _ ->
        (* Reject multiple variadic generics. *)
        None


  let zip_variables_with_arguments ~arguments variables =
    zip_variables_with_arguments_including_mismatches ~arguments variables
    >>| List.map ~f:(function { variable_pair; _ } -> variable_pair)


  let zip_variables_with_two_argument_lists ~left_arguments ~right_arguments variables =
    let left_pairs, right_pairs =
      ( zip_variables_with_arguments ~arguments:left_arguments variables,
        zip_variables_with_arguments ~arguments:right_arguments variables )
    in
    Option.map2 left_pairs right_pairs ~f:List.zip
    >>= function
    | Ok pairs -> Some pairs
    | Unequal_lengths -> None


  let all_unary variables =
    List.map variables ~f:(function
        | TypeVarVariable unary -> Some unary
        | ParamSpecVariable _
        | TypeVarTupleVariable _ ->
            None)
    |> Option.all


  let to_argument = function
    | TypeVarVariable variable -> Argument.Single (TypeVar.self_reference variable)
    | ParamSpecVariable variable -> Argument.CallableParameters (ParamSpec.self_reference variable)
    | TypeVarTupleVariable variadic -> Argument.Unpacked (Variadic variadic)
end

module GenericParameter = struct
  type t =
    | GpTypeVar of {
        name: Identifier.t;
        variance: Record.PreInferenceVariance.t;
        constraints: T.t Record.TypeVarConstraints.t;
      }
    | GpTypeVarTuple of { name: Identifier.t }
    | GpParamSpec of { name: Identifier.t }
  [@@deriving compare, equal, sexp, show, hash]

  let parameter_name = function
    | GpTypeVar { name; _ } -> name
    | GpTypeVarTuple { name } -> name
    | GpParamSpec { name } -> name


  let to_variable = function
    | GpTypeVar { name : Identifier.t; constraints : T.t Record.TypeVarConstraints.t; _ } ->
        Record.Variable.TypeVarVariable (Record.Variable.TypeVar.create ~constraints name)
    | GpTypeVarTuple { name : Identifier.t } ->
        Record.Variable.TypeVarTupleVariable (Record.Variable.TypeVarTuple.create name)
    | GpParamSpec { name : Identifier.t } ->
        Record.Variable.ParamSpecVariable (Record.Variable.ParamSpec.create name)
    [@@deriving compare, equal, sexp, show, hash]


  let of_declaration declaration ~create_type =
    match declaration with
    | Variable.Declaration.DTypeVar { name; constraints; variance; _ } ->
        let constraints =
          match constraints with
          | Bound expression -> Record.TypeVarConstraints.Bound (create_type expression)
          | Explicit expressions -> Explicit (List.map ~f:create_type expressions)
          | Unconstrained -> Unconstrained
          | LiteralIntegers -> LiteralIntegers
        in
        GpTypeVar { name; variance; constraints }
    | Variable.Declaration.DTypeVarTuple { name } -> GpTypeVarTuple { name }
    | Variable.Declaration.DParamSpec { name } -> GpParamSpec { name }


  (* A zip function + result type used when we need to use type paramter information of a type
   * constructor to compare two specializations of that type constructor: we want to zip the
   * two argument lists * up with the parameters.
   *
   * All structural mismatches are represented directly a variants:
   * - mismatched "kinds" i.e.  type_var vs type_var_tuple vs param_spec, which produces a
   *   `MismatchedKindsZipResult` and keeps a record of what was mismatched
   *   (invariant: at least two of the three entries failed to match)
   * - mismatched lengths; we include the unmatched bits in the result
   *   (invariant: at least one of the three lists is empty and at least one is nonempty)
   * - a failure to match a type_var_tuple; in this case we stop trying to check lengths,
   *   but produce a MismatchedVariadicZipResult.
   *)
  module ZipTwoArgumentsLists = struct
    type result =
      | TypeVarZipResult of {
          name: Identifier.t;
          left: T.t;
          right: T.t;
        }
      | TypeVarTupleZipResult of {
          name: Identifier.t;
          left: T.t Record.OrderedTypes.record;
          right: T.t Record.OrderedTypes.record;
        }
      | ParamSpecZipResult of {
          name: Identifier.t;
          left: T.t Record.Callable.record_parameters;
          right: T.t Record.Callable.record_parameters;
        }
      | MismatchedKindsZipResult of {
          parameter: t;
          left: T.t Record.Argument.record;
          right: T.t Record.Argument.record;
        }
      | MismatchedLengthsZipResult of {
          remaining_parameters: t list;
          remaining_left: T.t Record.Argument.record list;
          remaining_right: T.t Record.Argument.record list;
        }
      | MismatchedVariadicZipResult of {
          parameter: t;
          left: T.t Record.Argument.record list;
          right: T.t Record.Argument.record list;
        }
    [@@deriving compare, sexp, show]

    let all_singles_or_bad_argument arguments =
      let single_or_bad_argument = function
        | Record.Argument.Single ty -> Either.first ty
        | (Record.Argument.Unpacked _ | Record.Argument.CallableParameters _) as argument ->
            Either.Second argument
      in
      let good_arguments, bad_arguments = List.partition_map ~f:single_or_bad_argument arguments in
      match bad_arguments with
      | [] -> Result.Ok good_arguments
      | non_single_arguments -> Result.Error non_single_arguments


    (* A variadic parameter can match either an unpacked argument, or an arbitrary number of Single
       arguments. There can never be more than one variadic parameter in a parameters list (this
       invariant is not currently expressed in our ocaml datatypes but is known), so the number of
       Single arguments to match can be determined by *)
    let match_arguments_against_variadic_parameter arguments parameters_suffix_length =
      let matching_arguments, remaining_arguments =
        let n_arguments_to_match = List.length arguments - parameters_suffix_length in
        List.split_n arguments n_arguments_to_match
      in
      match OrderedTypes.concatenation_from_arguments matching_arguments with
      (* The matching arguments are a concatenation: there's one Unpack and possibly some suffix
         and/or prefix of Singles *)
      | Some ordered_type -> Result.Ok (ordered_type, remaining_arguments)
      (* The matching arguments are either all single, or are a structural mismatch. Determine which
         and return accordingly. *)
      | None -> (
          match all_singles_or_bad_argument matching_arguments with
          | Result.Ok types -> Result.Ok (OrderedTypes.Concrete types, remaining_arguments)
          | Result.Error non_single_arguments -> Result.Error non_single_arguments)


    let zip ~left_arguments ~right_arguments parameters =
      (* Consume a chunk of an arguments list matching a TypeVarTuple, producing: - a
         `TypeVarTupleZipResult` with the remaining left and right arguments on success - a
         `MismatchedVariadicZipResult` if the structural match fails. *)
      let zip_type_var_tuple_parameter
          ~parameter
          ~parameters_remaining
          ~left_arguments
          ~right_arguments
        =
        let parameters_suffix_length = List.length parameters_remaining in
        (* Note that we are passing `left_arguments` and `right_arguments` here, not
           `left_remaining` and `right_remaining`: the current argument is included. *)
        match
          ( match_arguments_against_variadic_parameter left_arguments parameters_suffix_length,
            match_arguments_against_variadic_parameter right_arguments parameters_suffix_length )
        with
        | Result.Ok (left, left_after_unpack), Result.Ok (right, right_after_unpack) ->
            ( TypeVarTupleZipResult { name = parameter_name parameter; left; right },
              left_after_unpack,
              right_after_unpack )
        | Result.Error left_mismatches, Result.Error right_mismatches ->
            ( MismatchedVariadicZipResult
                { parameter; left = left_mismatches; right = right_mismatches },
              [],
              [] )
        | Result.Error left_mismatches, Result.Ok _ ->
            MismatchedVariadicZipResult { parameter; left = left_mismatches; right = [] }, [], []
        | Result.Ok _, Result.Error right_mismatches ->
            MismatchedVariadicZipResult { parameter; left = []; right = right_mismatches }, [], []
      in

      let rec recur (so_far : result list) parameters left_arguments right_arguments =
        match parameters, left_arguments, right_arguments with
        (* Recursive case: drop structural mismatches then keep going *)
        | ( parameter :: parameters_remaining,
            left_argument :: left_remaining,
            right_argument :: right_remaining ) ->
            let so_far, left_remaining, right_remaining =
              match parameter, left_argument, right_argument with
              | GpTypeVar { name; _ }, Record.Argument.Single left, Record.Argument.Single right ->
                  TypeVarZipResult { name; left; right } :: so_far, left_remaining, right_remaining
              | (GpTypeVarTuple _ as parameter), _, _ ->
                  let zip_item, left_after_unpack, right_after_unpack =
                    zip_type_var_tuple_parameter
                      ~parameter
                      ~parameters_remaining
                      ~left_arguments
                      ~right_arguments
                  in
                  zip_item :: so_far, left_after_unpack, right_after_unpack
              | ( GpParamSpec { name; _ },
                  Record.Argument.CallableParameters left,
                  Record.Argument.CallableParameters right ) ->
                  ( ParamSpecZipResult { name; left; right } :: so_far,
                    left_remaining,
                    right_remaining )
              | _, _, _ ->
                  ( MismatchedKindsZipResult
                      { parameter; left = left_argument; right = right_argument }
                    :: so_far,
                    left_remaining,
                    right_remaining )
            in
            recur so_far parameters_remaining left_remaining right_remaining
        (* Base case when lengths match *)
        | [], [], [] -> so_far
        (* Base case when lengths do not match *)
        | remaining_parameters, remaining_left, remaining_right ->
            MismatchedLengthsZipResult { remaining_parameters; remaining_left; remaining_right }
            :: so_far
      in
      recur [] parameters left_arguments right_arguments |> List.rev
  end
end

module ToExpression = struct
  open T

  let location = Location.any

  let subscript = subscript_for_annotation ~location

  let create_name name = Expression.Name (create_name ~location ~create_origin:(fun _ -> None) name)

  let rec callable_parameters_expression = function
    | Record.Callable.Defined parameters ->
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
              {
                callee = Node.create ~location (Expression.Name (Name.Identifier kind));
                arguments;
                origin = None;
              }
            |> Node.create ~location
          in
          match parameter with
          | CallableParamType.PositionalOnly { annotation; default; _ } ->
              call ~default "PositionalOnly" (expression annotation)
          | Keywords annotation -> call "Keywords" (expression annotation)
          | Named { name; annotation; default } ->
              call ~default ~name "Named" (expression annotation)
          | KeywordOnly { name; annotation; default } ->
              call ~default ~name "KeywordOnly" (expression annotation)
          | Variable (Concrete annotation) -> call "Variable" (expression annotation)
          | Variable (Concatenation concatenation) ->
              Expression.Call
                {
                  callee = Node.create ~location (Expression.Name (Name.Identifier "Variable"));
                  arguments =
                    concatenation_to_expressions concatenation
                    |> List.map ~f:(fun annotation ->
                           { Call.Argument.name = None; value = annotation });
                  origin = None;
                }
              |> Node.create ~location
        in
        Expression.List (List.map ~f:convert_parameter parameters) |> Node.create ~location
    | Undefined -> Node.create ~location (Expression.Constant Constant.Ellipsis)
    | FromParamSpec variable ->
        Canonicalization.parameter_variable_type_representation variable |> expression


  and concatenation_to_expressions
      { Record.OrderedTypes.Concatenation.prefix; middle = unpackable; suffix }
    =
    List.map ~f:expression prefix
    @ [OrderedTypes.Concatenation.unpackable_to_expression ~expression ~location unpackable]
    @ List.map ~f:expression suffix


  and convert_type annotation =
    let convert_ordered_type ordered_type =
      match ordered_type with
      | Record.OrderedTypes.Concatenation
          { middle = UnboundedElements argument; prefix = []; suffix = [] } ->
          List.map ~f:expression [argument; Primitive "..."]
      | Concatenation concatenation -> concatenation_to_expressions concatenation
      | Concrete arguments -> List.map ~f:expression arguments
    in
    match annotation with
    | Bottom -> create_name "$bottom"
    | Callable { implementation; overloads; _ } -> (
        (* Pyre currently allows writing (and pretty-printing) overload types using a closed-form
           expression written like `Callable[[Any], Any][[[str], str][[int],int]]`. *)
        let convert_signature_as_leftmost_overload { Record.Callable.annotation; parameters; _ } =
          Node.create
            ~location
            (Expression.List [callable_parameters_expression parameters; expression annotation])
        in
        let convert_signature_as_index { Record.Callable.annotation; parameters; _ } =
          Node.create
            ~location
            (Expression.Tuple [callable_parameters_expression parameters; expression annotation])
        in
        let base_value =
          Expression.Subscript
            {
              base = { Node.location; value = create_name "typing.Callable" };
              index = convert_signature_as_index implementation;
              origin = None;
            }
        in
        let overloads =
          let convert_overload sofar overload =
            match sofar with
            | None -> Some (convert_signature_as_leftmost_overload overload)
            | Some expression ->
                Expression.Subscript
                  { base = expression; index = convert_signature_as_index overload; origin = None }
                |> Node.create ~location
                |> Option.some
          in
          List.fold ~init:None ~f:convert_overload overloads
        in
        match overloads with
        | Some overloads ->
            Expression.Subscript
              { base = { Node.location; value = base_value }; index = overloads; origin = None }
        | None -> base_value)
    | Any -> create_name "typing.Any"
    | Literal literal ->
        let literal =
          match literal with
          | Boolean true -> Expression.Constant Constant.True
          | Boolean false -> Expression.Constant Constant.False
          | Integer literal -> Expression.Constant (Constant.Integer literal)
          | String (LiteralValue literal) ->
              Expression.Constant (Constant.String { value = literal; kind = StringLiteral.String })
          | String AnyLiteral -> create_name "str"
          | Bytes literal ->
              Expression.Constant (Constant.String { value = literal; kind = StringLiteral.Bytes })
          | EnumerationMember { enumeration_type; member_name } ->
              Expression.Name
                (Attribute
                   { base = expression enumeration_type; attribute = member_name; origin = None })
        in
        subscript "typing_extensions.Literal" [Node.create ~location literal]
    | NoneType -> Expression.Constant Constant.NoneLiteral
    | Parametric { name; arguments } ->
        let arguments =
          let expression_of_argument = function
            | Record.Argument.Single single -> expression single
            | CallableParameters parameters -> callable_parameters_expression parameters
            | Unpacked unpackable ->
                OrderedTypes.Concatenation.unpackable_to_expression ~expression ~location unpackable
          in
          match arguments with
          | parameters -> List.map parameters ~f:expression_of_argument
        in
        subscript (Canonicalization.reverse_substitute name) arguments
    | ParamSpecComponent { component; variable_name; _ } ->
        let attribute = PrettyPrinting.Variable.ParamSpec.Components.component_name component in
        Expression.Name
          (Attribute { base = expression (Primitive variable_name); attribute; origin = None })
    | Primitive name -> create_name name
    | PyreReadOnly type_ -> subscript "pyre_extensions.PyreReadOnly" [expression type_]
    | RecursiveType { name; _ } -> create_name name
    | Top -> create_name "$unknown"
    | Tuple (Concrete []) -> subscript "typing.Tuple" [Node.create ~location (Expression.Tuple [])]
    | Tuple ordered_type -> subscript "typing.Tuple" (convert_ordered_type ordered_type)
    | TypeOperation (Compose ordered_type) ->
        subscript "pyre_extensions.Compose" (convert_ordered_type ordered_type)
    | Union [NoneType; argument]
    | Union [argument; NoneType] ->
        subscript "typing.Optional" [expression argument]
    | Union arguments -> subscript "typing.Union" (List.map ~f:expression arguments)
    | Variable { name; _ } -> create_name name


  and expression annotation =
    let value =
      match annotation with
      | Primitive "..." -> Expression.Constant Constant.Ellipsis
      | _ -> convert_type annotation
    in
    Node.create_with_default_location value
end

module TypedDictionary = struct
  type typed_dictionary_field = {
    name: string;
    annotation: T.t;
    required: bool;
    readonly: bool;
  }
  [@@deriving compare, equal, sexp, show, hash]

  type t = {
    name: Identifier.t;
    fields: typed_dictionary_field list;
  }
  [@@deriving compare, equal, sexp, show, hash]

  open T

  (* Precondition: `fields` cannot have duplicate entries *)
  let anonymous fields = { name = "$anonymous"; fields }

  let create_field ~annotation ~has_non_total_typed_dictionary_base_class name =
    let rec get_properties annotation ~required ~readonly =
      match annotation with
      | Parametric
          {
            name = "typing_extensions.NotRequired" | "typing.NotRequired";
            arguments = [Single annotation];
          } ->
          get_properties annotation ~required:(Some false) ~readonly
      | Parametric
          {
            name = "typing_extensions.Required" | "typing.Required";
            arguments = [Single annotation];
          } ->
          get_properties annotation ~required:(Some true) ~readonly
      | Parametric
          {
            name = "typing_extensions.ReadOnly" | "typing.ReadOnly";
            arguments = [Single annotation];
          } ->
          get_properties ~required ~readonly:(Some true) annotation
      | _ -> annotation, required, readonly
    in
    let annotation, required, readonly = get_properties annotation ~required:None ~readonly:None in
    {
      name;
      annotation;
      required = Option.value required ~default:(not has_non_total_typed_dictionary_base_class);
      readonly = Option.value readonly ~default:false;
    }


  let are_fields_total = List.for_all ~f:(fun { required; _ } -> required)

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
        {
          name = needle_name;
          annotation = needle_annotation;
          required = needle_required;
          readonly = needle_readonly;
        }
      =
      let same_name_different_properties { name; annotation; required; readonly } =
        String.equal name needle_name
        && ((not (equal annotation needle_annotation))
           || (not (Bool.equal required needle_required))
           || not (Bool.equal readonly needle_readonly))
      in
      List.exists left_fields ~f:same_name_different_properties
    in
    List.exists right_fields ~f:found_collision


  let self_parameter class_name =
    CallableParamType.Named { name = "self"; annotation = Primitive class_name; default = false }


  let readonly_self_parameter class_name =
    CallableParamType.Named
      { name = "self"; annotation = PyreReadOnly (Primitive class_name); default = false }


  let field_named_parameters ?(all_default = false) ~class_name fields =
    let field_to_argument { name; annotation; required; _ } =
      Record.Callable.CallableParamType.KeywordOnly
        {
          name = Format.asprintf "$parameter$%s" name;
          annotation;
          default = all_default || not required;
        }
    in
    let required_fields, non_required_fields =
      List.partition_tf fields ~f:(fun { required; _ } -> required)
    in
    required_fields @ non_required_fields
    |> List.map ~f:field_to_argument
    |> fun parameters -> Record.Callable.Defined (self_parameter class_name :: parameters)


  let constructor ~name ~fields =
    {
      Callable.kind = Named (Reference.create ~prefix:(Reference.create name) "__init__");
      implementation = { annotation = Top; parameters = Undefined };
      overloads =
        [
          {
            annotation = Constructors.none;
            parameters = field_named_parameters ~class_name:name fields;
          };
          {
            annotation = Constructors.none;
            parameters =
              Defined
                [
                  Record.Callable.CallableParamType.PositionalOnly
                    { index = 0; annotation = Primitive name; default = false };
                  Record.Callable.CallableParamType.PositionalOnly
                    { index = 1; annotation = Primitive name; default = false };
                ];
          };
        ];
    }


  let fields_from_constructor constructor readonlyness =
    match constructor with
    | {
     Callable.kind = Named name;
     overloads = [{ parameters = Defined (_self :: parameters); _ }; _];
     _;
    }
      when String.equal (Reference.last name) "__init__" ->
        let parameter_to_field = function
          | Record.Callable.CallableParamType.KeywordOnly { name; annotation; default } ->
              let name = String.split ~on:'$' name |> List.last_exn in
              Some
                {
                  name;
                  annotation;
                  required = not default;
                  readonly = Option.value ~default:false (Map.find readonlyness name);
                }
          | _ -> None
        in
        List.map ~f:parameter_to_field parameters |> Option.all
    | _ -> None


  type special_method = {
    name: string;
    special_index: int option;
    overloads: typed_dictionary_field list -> t Callable.overload list;
  }

  let key_parameter name =
    CallableParamType.Named
      { name = "k"; annotation = Constructors.literal_string name; default = false }


  let common_special_methods ~class_name =
    let getitem_overloads =
      let overloads { name; annotation; _ } =
        [
          {
            Record.Callable.annotation;
            parameters = Defined [self_parameter class_name; key_parameter name];
          };
          {
            Record.Callable.annotation = PyreReadOnly annotation;
            parameters = Defined [readonly_self_parameter class_name; key_parameter name];
          };
        ]
      in
      List.concat_map ~f:overloads
    in
    let setitem_overloads =
      let overload { name; annotation; readonly; _ } =
        (* A read-only field cannot be written to, so we set the value type to Never to generate a
           type error on a write attempt. *)
        let value_annotation = if readonly then Primitive "typing.Never" else annotation in
        {
          Record.Callable.annotation = Constructors.none;
          parameters =
            Defined
              [
                self_parameter class_name;
                key_parameter name;
                Named { name = "v"; annotation = value_annotation; default = false };
              ];
        }
      in
      List.map ~f:overload
    in
    let get_overloads fields =
      let overloads { name; annotation; _ } =
        let annotation_with_none = Constructors.union [annotation; NoneType] in
        let annotation_with_t = Union [annotation; Variable (Variable.TypeVar.create "_T")] in
        [
          {
            Record.Callable.annotation = annotation_with_none;
            parameters = Defined [self_parameter class_name; key_parameter name];
          };
          {
            Record.Callable.annotation = PyreReadOnly annotation_with_none;
            parameters = Defined [readonly_self_parameter class_name; key_parameter name];
          };
          {
            annotation = annotation_with_t;
            parameters =
              Defined
                [
                  self_parameter class_name;
                  key_parameter name;
                  Named
                    {
                      name = "default";
                      annotation = Variable (Variable.TypeVar.create "_T");
                      default = false;
                    };
                ];
          };
          {
            annotation = PyreReadOnly annotation_with_t;
            parameters =
              Defined
                [
                  readonly_self_parameter class_name;
                  key_parameter name;
                  Named
                    {
                      name = "default";
                      annotation = Variable (Variable.TypeVar.create "_T");
                      default = false;
                    };
                ];
          };
        ]
      in
      List.concat_map ~f:overloads fields
      @ [
          {
            Record.Callable.annotation = Constructors.union [Constructors.object_primitive; NoneType];
            parameters =
              Defined
                [
                  self_parameter class_name;
                  CallableParamType.Named
                    { name = "k"; annotation = Constructors.string; default = false };
                ];
          };
          {
            Record.Callable.annotation =
              Constructors.union
                [Constructors.object_primitive; Variable (Variable.TypeVar.create "_T")];
            parameters =
              Defined
                [
                  self_parameter class_name;
                  CallableParamType.Named
                    { name = "k"; annotation = Constructors.string; default = false };
                  Named
                    {
                      name = "default";
                      annotation = Variable (Variable.TypeVar.create "_T");
                      default = false;
                    };
                ];
          };
        ]
    in
    let setdefault_overloads =
      let overload { name; annotation; _ } =
        {
          Record.Callable.annotation;
          parameters =
            Defined
              [
                self_parameter class_name;
                key_parameter name;
                Named { name = "default"; annotation; default = false };
              ];
        }
      in
      List.map ~f:overload
    in
    let update_overloads fields =
      let never_match_readonly field =
        if field.readonly then { field with annotation = Primitive "typing.Never" } else field
      in
      let fields_no_readonly = List.map ~f:never_match_readonly fields in
      [
        (* Type parameters corresponding to read-only fields as Never so that we error on any
           attempt to write to read-only fields via `update. *)
        {
          Record.Callable.annotation = Constructors.none;
          parameters = field_named_parameters ~all_default:true ~class_name fields_no_readonly;
        };
        (* We also need to error on updating read-only fields passed in via a TypedDict, but this
           case is difficult to express via type signature, so it is special-cased in
           signatureSelection:check_arguments_against_parameters. *)
        {
          annotation = Constructors.none;
          parameters =
            Defined
              [
                Record.Callable.CallableParamType.PositionalOnly
                  { index = 0; annotation = Primitive class_name; default = false };
                Record.Callable.CallableParamType.PositionalOnly
                  { index = 1; annotation = Primitive class_name; default = false };
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


  let non_total_special_methods class_name =
    let pop_overloads =
      let overloads { name; annotation; required; _ } =
        if required then
          []
        else
          [
            {
              Record.Callable.annotation;
              parameters = Defined [self_parameter class_name; key_parameter name];
            };
            {
              annotation = Union [annotation; Variable (Variable.TypeVar.create "_T")];
              parameters =
                Defined
                  [
                    self_parameter class_name;
                    key_parameter name;
                    Named
                      {
                        name = "default";
                        annotation = Variable (Variable.TypeVar.create "_T");
                        default = false;
                      };
                  ];
            };
          ]
      in
      List.concat_map ~f:overloads
    in
    let delitem_overloads fields =
      let overload { name; required; _ } =
        Option.some_if
          (not required)
          {
            Record.Callable.annotation = Constructors.none;
            parameters = Defined [self_parameter class_name; key_parameter name];
          }
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
        non_total_special_methods class_name @ common_special_methods ~class_name
    in
    List.find special_methods ~f:(fun { name; _ } -> String.equal name method_name)
    >>| fun { overloads; _ } -> overloads fields


  let is_special_mismatch ~class_name ~total ~method_name ~position =
    let special_methods =
      if total then
        common_special_methods ~class_name
      else
        non_total_special_methods class_name @ common_special_methods ~class_name
    in
    List.find special_methods ~f:(fun { name; _ } -> String.equal name method_name)
    >>= (fun { special_index; _ } -> special_index)
    >>| ( = ) position
    |> Option.value ~default:false


  (** typing.TypedDict gets preprocessed into TypedDictionary *)
  let class_name ~total =
    if total then
      "TypedDictionary"
    else
      "NonTotalTypedDictionary"


  let base_typed_dictionary = Primitive (class_name ~total:true)

  let is_builtin_typed_dictionary_class base_name =
    List.mem [class_name ~total:true; class_name ~total:false] base_name ~equal:String.equal


  let defines ~total ~t_self_expression =
    let open Statement in
    let class_name = class_name ~total in
    let define ?self_parameter ?return_annotation name =
      Statement.Define
        {
          signature =
            {
              name = Reference.create_from_list [class_name; name];
              parameters =
                [
                  { ExpressionParameter.name = "self"; value = None; annotation = self_parameter }
                  |> Node.create_with_default_location;
                ];
              decorators = [];
              return_annotation;
              async = false;
              generator = false;
              parent = NestingContext.create_toplevel ();
              legacy_parent = Some (Reference.create class_name);
              type_params = [];
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
        define
          ~self_parameter:t_self_expression
          ~return_annotation:(ToExpression.expression Constructors.integer)
          "__len__";
        define
          ~self_parameter:t_self_expression
          ~return_annotation:(ToExpression.expression (Constructors.iterator Constructors.string))
          "__iter__";
      ]
      @ List.map (common_special_methods ~class_name) ~f:(fun { name; _ } -> define name)
    in
    if total then
      common_methods
    else
      common_methods
      @ (non_total_special_methods class_name |> List.map ~f:(fun { name; _ } -> define name))


  let is_update_method name =
    List.exists
      ~f:(String.equal (Reference.show name))
      ["TypedDictionary.update"; "NonTotalTypedDictionary.update"]
end

include T

let _ = show (* shadowed below *)

let lambda ~parameters ~return_annotation =
  let parameters =
    List.map parameters ~f:(fun (name, annotation) ->
        { CallableParamType.name; annotation; default = false })
    |> Callable.CallableParamType.create
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
    "function", Callable.create ~annotation:Any ();
    "typing.Any", Any;
    "typing.ChainMap", Primitive "collections.ChainMap";
    "typing.Counter", Primitive "collections.Counter";
    "typing.DefaultDict", Primitive "collections.defaultdict";
    "typing.Deque", Primitive "collections.deque";
    "typing.Dict", Primitive "dict";
    "typing.FrozenSet", Primitive "frozenset";
    "typing.List", Primitive "list";
    "typing.OrderedDict", Primitive "collections.OrderedDict";
    "typing.Set", Primitive "set";
    "typing.Tuple", Primitive "tuple";
    "typing.Type", Primitive "type";
    "typing_extensions.Protocol", Primitive "typing.Protocol";
    (* This is broken in typeshed:
       https://github.com/python/typeshed/pull/991#issuecomment-288160993 *)
    "PathLike", Primitive "_PathLike";
    "TSelf", Constructors.variable "_PathLike";
    (* This inherits from Any, and is expected to act just like Any *)
    "_NotImplementedType", Any;
    "typing_extensions.LiteralString", Literal (String AnyLiteral);
    "typing.LiteralString", Literal (String AnyLiteral);
  ]
  |> Identifier.Table.of_alist_exn


let primitive_name = function
  | Primitive name -> Some name
  | _ -> None


let create_literal = function
  | Expression.Constant Constant.True -> Some (Literal (Boolean true))
  | Expression.Constant Constant.False -> Some (Literal (Boolean false))
  | Expression.Constant (Constant.Integer literal) -> Some (Literal (Integer literal))
  | Expression.Constant (Constant.String { StringLiteral.kind = StringLiteral.String; value }) ->
      Some (Literal (String (LiteralValue value)))
  | Expression.Constant (Constant.String { StringLiteral.kind = StringLiteral.Bytes; value }) ->
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
      | _ -> None)
  | Expression.Constant Constant.NoneLiteral -> Some Constructors.none
  | Expression.Name (Identifier "str") -> Some (Literal (String AnyLiteral))
  | _ -> None


let resolved_empty_aliases ?replace_unbound_parameters_with_any:_ _ = None

let resolved_empty_variables _ = None

let arguments_from_unpacked_annotation annotation ~variables =
  let open Record.OrderedTypes.Concatenation in
  let unpacked_variadic_to_argument = function
    | Primitive variable_name -> (
        match variables variable_name with
        | Some (Record.Variable.TypeVarTupleVariable variadic) ->
            Some (Argument.Unpacked (Variadic variadic))
        | _ -> None)
    | _ -> None
  in
  match annotation with
  | Parametric { name; arguments = [Single (Primitive _ as element)] }
    when List.exists ~f:(Identifier.equal name) Record.OrderedTypes.unpack_public_names ->
      unpacked_variadic_to_argument element >>| fun argument -> [argument]
  | Parametric { name; arguments = [Single (Tuple ordered_type)] }
    when List.exists ~f:(Identifier.equal name) Record.OrderedTypes.unpack_public_names ->
      OrderedTypes.to_arguments ordered_type |> Option.some
  | _ -> None


let rec create_logic ~resolve_aliases ~variables { Node.value = expression; _ } =
  let create_logic = create_logic ~resolve_aliases ~variables in
  let substitute_parameter_variadic = function
    | Primitive name -> (
        match variables name with
        | Some (Record.Variable.ParamSpecVariable variable) ->
            Some { Record.Callable.variable; head = [] }
        | _ -> None)
    | Parametric { name; arguments }
      when List.exists ~f:(Identifier.equal name) Record.OrderedTypes.concatenate_public_names -> (
        match List.rev arguments with
        | Argument.CallableParameters (FromParamSpec { variable; head = [] }) :: reversed_head ->
            Argument.all_singles reversed_head
            >>| List.rev
            >>| fun head -> { Record.Callable.variable; head }
        | _ -> None)
    | _ -> None
  in
  let extract_parameter index parameter =
    match Node.value parameter with
    | Expression.Call
        { callee = { Node.value = Name (Name.Identifier name); _ }; arguments; origin = _ } -> (
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
            CallableParamType.PositionalOnly
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
            let callable_parameter =
              match tail with
              | head :: _ ->
                  let elements =
                    List.map tail ~f:(fun annotation ->
                        create_logic (Node.create_with_default_location annotation))
                  in
                  OrderedTypes.concatenation_from_annotations ~variables elements
                  >>| (fun concatenation -> CallableParamType.Concatenation concatenation)
                  |> Option.value
                       ~default:
                         (CallableParamType.Concrete
                            (create_logic (Node.create_with_default_location head)))
              | _ -> CallableParamType.Concrete Top
            in
            CallableParamType.Variable callable_parameter
        | "Keywords", tail ->
            let annotation =
              match tail with
              | annotation :: _ -> create_logic (Node.create_with_default_location annotation)
              | _ -> Top
            in
            Keywords annotation
        | _ -> PositionalOnly { index; annotation = Top; default = false })
    | _ -> PositionalOnly { index; annotation = create_logic parameter; default = false }
  in
  let create_ordered_type_from_arguments arguments =
    match Argument.all_singles arguments with
    | Some [annotation; Primitive "..."] ->
        Some (OrderedTypes.create_unbounded_concatenation annotation)
    | Some singles -> Some (Concrete singles)
    | None -> OrderedTypes.concatenation_from_arguments arguments
  in
  (* Determine whether a subscripted type expression involves a Callable type (this is nontrivial
     because we support Callable type aliases, so we have to resolve aliases to be sure). If so,
     return `Some callable_type` for the resulting `Type.t` value. Otherwise, return `None` (the
     caller will then parse the expression as a normal parametric type). *)
  let parse_callable_if_appropriate ~location ~base ~subscript_index =
    let rec resolve_base base =
      match Node.value base with
      | Expression.Name
          (Name.Attribute
            {
              base = { Node.value = Name (Name.Identifier "typing"); _ };
              attribute = "Callable";
              _;
            }) ->
          base
      | Name base_name ->
          Ast.Expression.name_to_reference base_name
          >>| Reference.show
          >>| (fun resolved_base_name ->
                match resolve_aliases (Primitive resolved_base_name) with
                | Primitive "typing.Callable"
                | Callable
                    {
                      implementation = { parameters = Undefined; annotation = Any };
                      overloads = [];
                      _;
                    } ->
                    {
                      base with
                      Node.value =
                        Expression.Name
                          (Name.Attribute
                             {
                               base = { Node.value = Name (Name.Identifier "typing"); location };
                               attribute = "Callable";
                               origin = None;
                             });
                    }
                | _ -> base)
          |> Option.value ~default:base
      | Subscript ({ base = inner_base; _ } as subscript) ->
          { base with Node.value = Subscript { subscript with base = resolve_base inner_base } }
      | Call ({ callee; _ } as call) ->
          { base with Node.value = Call { call with callee = resolve_base callee } }
      | _ -> base
    in
    let rec is_typing_callable base =
      match Node.value base with
      | Expression.Name
          (Name.Attribute
            {
              base = { Node.value = Name (Name.Identifier "typing"); _ };
              attribute = "Callable";
              _;
            }) ->
          true
      | Name (Name.Attribute { base; _ }) -> is_typing_callable base
      | Subscript { base; _ } -> is_typing_callable base
      | Call { callee; _ } -> is_typing_callable callee
      | _ -> false
    in
    let parse_callable ~resolved_base ~subscript_index =
      let open Record.Callable in
      let modifiers, implementation_signature, overload_signatures =
        let get_from_base base implementation_argument overloads_argument =
          match Node.value base with
          | Expression.Call { callee; arguments; origin = _ }
            when name_is ~name:"typing.Callable" callee ->
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
        match Node.value resolved_base, subscript_index with
        (* There are two layers of subscripts, this is how we represent overloads in closed
           expression form *)
        | ( Expression.Subscript { base = inner_base; index = inner_subscript_index; origin = _ },
            overloads_index ) ->
            get_from_base inner_base (Some inner_subscript_index) (Some overloads_index)
        (* There is only one layer of subscripts - this is either a "plain" or "named" Callable
           type *)
        | _, _ -> get_from_base resolved_base (Some subscript_index) None
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
                 {
                   Node.value = Expression.Constant (Constant.String { StringLiteral.value; _ });
                   _;
                 };
               _;
             }
            :: _) ->
            Named (Reference.create value)
        | _ -> Anonymous
      in
      let undefined = { annotation = Top; parameters = Undefined } in
      let get_signature = function
        | Expression.Tuple [parameters; annotation] -> (
            let make_signature ~parameters = { annotation = create_logic annotation; parameters } in
            match Node.value parameters with
            | List parameters ->
                make_signature ~parameters:(Defined (List.mapi ~f:extract_parameter parameters))
            | _ -> (
                let parsed = create_logic parameters in
                match substitute_parameter_variadic parsed with
                | Some variable -> make_signature ~parameters:(FromParamSpec variable)
                | _ -> (
                    match parsed with
                    | Primitive "..." -> make_signature ~parameters:Undefined
                    | _ -> undefined)))
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
          | Subscript { base; index; origin = _ } ->
              get_signature (Node.value index) :: parse_overloads (Node.value base)
          | _ -> [undefined]
        in
        match overload_signatures with
        | Some signatures -> List.rev (parse_overloads (Node.value signatures))
        | None -> []
      in
      Callable { kind; implementation; overloads }
    in
    let resolved_base = resolve_base base in
    if is_typing_callable resolved_base then
      Some (parse_callable ~resolved_base ~subscript_index)
    else
      None
  in
  let create_from_subscript ~base ~subscript_index ~allows_unpacking =
    let create_parametric name =
      let arguments =
        let element_to_arguments = function
          | { Node.value = Expression.List elements; _ } ->
              Some
                [
                  Record.Argument.CallableParameters
                    (Defined (List.mapi ~f:extract_parameter elements));
                ]
          | element -> (
              let parsed = create_logic element in
              match arguments_from_unpacked_annotation ~variables parsed with
              | Some arguments ->
                  if allows_unpacking then
                    Some arguments
                  else
                    None
              | _ -> (
                  match substitute_parameter_variadic parsed with
                  | Some variable ->
                      Some [Record.Argument.CallableParameters (FromParamSpec variable)]
                  | _ -> Some [Record.Argument.Single parsed]))
        in
        match subscript_index with
        | { Node.value = Expression.Tuple elements; _ } ->
            let arguments_for_elements = List.map elements ~f:element_to_arguments in
            if List.exists ~f:Option.is_none arguments_for_elements then
              None
            else
              Some (List.filter_opt arguments_for_elements |> List.concat)
        | element -> element_to_arguments element
      in
      match arguments with
      | Some arguments -> Parametric { name; arguments } |> resolve_aliases
      | _ -> Top
    in
    match create_logic base, Node.value base with
    | Primitive name, _ -> create_parametric name
    | _, Name _ -> create_parametric (Expression.show base)
    | _ -> Top
  in
  let resolve_variables_then_aliases alias_name =
    match variables alias_name with
    | Some (Record.Variable.TypeVarVariable variable) -> Variable variable
    | _ -> Primitive alias_name |> resolve_aliases
  in
  let result =
    match expression with
    | Subscript
        {
          base =
            {
              Node.value =
                Expression.Name
                  (Name.Attribute
                    {
                      base =
                        {
                          Node.value =
                            Expression.Name (Name.Identifier ("typing" | "typing_extensions"));
                          _;
                        };
                      attribute = "Literal";
                      _;
                    });
              _;
            };
          index = subscript_index;
          origin = _;
        } ->
        let arguments =
          match Node.value subscript_index with
          | Expression.Tuple arguments -> List.map arguments ~f:Node.value
          | argument -> [argument]
        in
        let argument_to_literals argument =
          let rec flatten_literal_unions = function
            | Literal _ as literal_type -> [Some literal_type]
            | Union members -> List.map ~f:flatten_literal_unions members |> List.concat
            | _ -> [None]
          in
          match create_literal argument with
          | Some literal_type -> [Some literal_type]
          | None ->
              create_logic (Node.create_with_default_location argument) |> flatten_literal_unions
        in
        arguments
        |> List.map ~f:argument_to_literals
        |> List.concat
        |> Option.all
        >>| Constructors.union
        |> Option.value ~default:Top
    | Subscript
        {
          base =
            {
              Node.value =
                Expression.Name
                  (Name.Attribute
                    {
                      base =
                        {
                          Node.value =
                            Expression.Name
                              (Name.Identifier
                                (("typing" | "typing_extensions" | "pyre_extensions") as
                                import_module));
                          _;
                        };
                      attribute = "Unpack";
                      _;
                    });
              _;
            };
          index = element;
          origin = _;
        } ->
        Parametric
          {
            name = import_module ^ ".Unpack";
            arguments = [Record.Argument.Single (create_logic element)];
          }
        |> resolve_aliases
    | Starred (Once element) ->
        Parametric
          {
            name = "typing_extensions.Unpack";
            arguments = [Record.Argument.Single (create_logic element)];
          }
        |> resolve_aliases
    | Subscript
        {
          base =
            {
              Node.value =
                Expression.Name
                  ( Name.Attribute
                      {
                        base = { Node.value = Expression.Name (Name.Identifier "typing"); _ };
                        attribute = "Tuple";
                        _;
                      }
                  | Name.Identifier "tuple" );
              _;
            } as base;
          index =
            {
              Node.value = Expression.Tuple [_; { Node.value = Expression.Constant Ellipsis; _ }];
              _;
            } as subscript_index;
          origin = _;
        } ->
        (* Do not eagerly unpack types for unbounded tuples *)
        create_from_subscript ~base ~subscript_index ~allows_unpacking:false
    | Subscript { base; index = subscript_index; origin = _ } -> (
        let location = Node.location base in
        match parse_callable_if_appropriate ~location ~base ~subscript_index with
        | Some callable_type -> callable_type
        | None -> create_from_subscript ~base ~subscript_index ~allows_unpacking:true)
    | Constant Constant.NoneLiteral -> Constructors.none
    | Name (Name.Identifier identifier) ->
        let sanitized = Identifier.sanitized identifier in
        resolve_variables_then_aliases sanitized
    | Name (Name.Attribute { base; attribute; _ }) -> (
        let attribute = Identifier.sanitized attribute in
        match create_logic base with
        | Primitive primitive -> resolve_variables_then_aliases (primitive ^ "." ^ attribute)
        | _ -> Primitive (Expression.show base ^ "." ^ attribute))
    | Constant Constant.Ellipsis -> Primitive "..."
    | Constant (Constant.String { StringLiteral.value; _ }) ->
        let expression =
          try
            let parsed =
              PyreMenhirParser.Parser.parse_exn [value] |> Source.create |> Source.statements
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
      match Hashtbl.find primitive_substitution_map name with
      | Some substitute -> substitute
      | None -> result)
  | Parametric { name = "typing.Tuple"; arguments }
  | Parametric { name = "tuple"; arguments } ->
      Option.value
        ~default:Top
        (create_ordered_type_from_arguments arguments >>| fun result -> Tuple result)
  | Parametric { name = "pyre_extensions.Compose"; arguments } ->
      Option.value
        ~default:Top
        (create_ordered_type_from_arguments arguments >>= TypeOperation.Compose.create)
  | Parametric { name; arguments } -> (
      let replace_with_special_form ~name arguments =
        match name, Argument.all_singles arguments with
        (* Annotated doesn't have an internal representation in Pyre, but the spec requires at least
           two params. We only unwrap it if there are >=2 type params; otherwise we leave it wrapped
           and check it like a variadic generic that requires >=2 params. *)
        | ("typing_extensions.Annotated" | "typing.Annotated"), Some (head :: _ :: _) -> head
        | "typing.Optional", Some [head] -> Constructors.optional head
        | "typing.Union", Some arguments -> Constructors.union arguments
        (* We support a made-up, stubs-only `typing._PyreReadOnly_` class so that we can safely
           patch the standard library with read-only methods without worrying about whether
           `pyre_extensions` is part of a project. *)
        | "typing._PyreReadOnly_", Some [head]
        | "pyre_extensions.ReadOnly", Some [head]
        | "pyre_extensions.PyreReadOnly", Some [head] ->
            Constructors.pyre_read_only head
        | _ -> result
      in
      match Hashtbl.find Canonicalization.alternate_name_to_canonical_name_map name with
      | Some name -> Parametric { name; arguments }
      | None -> replace_with_special_form ~name arguments)
  | Union elements -> Constructors.union elements
  | Callable ({ implementation; overloads; _ } as callable) ->
      let collect_unpacked_parameters_if_any ({ Record.Callable.parameters; _ } as overload) =
        match parameters with
        | Defined parameters ->
            let all_positional_only_parameters =
              List.map parameters ~f:(function
                  | PositionalOnly { annotation; _ } -> Some annotation
                  | _ -> None)
              |> Option.all
            in
            all_positional_only_parameters
            >>= OrderedTypes.concatenation_from_annotations ~variables
            >>| (fun concatenation ->
                  { overload with parameters = Defined [Variable (Concatenation concatenation)] })
            |> Option.value ~default:overload
        | _ -> overload
      in
      Callable
        {
          callable with
          implementation = collect_unpacked_parameters_if_any implementation;
          overloads = List.map overloads ~f:collect_unpacked_parameters_if_any;
        }
  | _ -> result


(* Check if there is a literal Any provided, not including type aliases to Any. *)
let expression_contains_any expression =
  let primitives_with_any_map =
    Hashtbl.filter ~f:Predicates.contains_any primitive_substitution_map
  in
  Visit.collect_non_generic_type_names expression
  |> List.exists ~f:(Hashtbl.mem primitives_with_any_map)


type type_guard_kind =
  | NoGuard
  | TypeIs of t
  | TypeGuard of t

let type_guard_kind_if_any = function
  | Parametric
      { name = "typing.TypeIs" | "typing_extensions.TypeIs"; arguments = [Single narrowed_type] } ->
      TypeIs narrowed_type
  | Parametric
      {
        name = "typing.TypeGuard" | "typing_extensions.TypeGuard";
        arguments = [Single narrowed_type];
      } ->
      TypeGuard narrowed_type
  | _ -> NoGuard


let arguments = function
  | Parametric { arguments; _ } -> Some arguments
  | _ -> None


let type_arguments_for_bounded_tuple_union = function
  | Union annotations ->
      let bounded_tuple_arguments = function
        | Tuple (Concrete arguments) -> Some arguments
        | _ -> None
      in
      List.map annotations ~f:bounded_tuple_arguments
      |> Option.all
      >>= List.transpose
      >>| List.map ~f:Constructors.union
  | _ -> None


let single_argument = function
  | Parametric { arguments = [Single argument]; _ } -> argument
  | _ -> failwith "Type does not have single argument"


let weaken_literals annotation =
  let open Constructors in
  let type_map = function
    | Literal (Integer _) -> Some integer
    | Literal (String _) -> Some string
    | Literal (Bytes _) -> Some bytes
    | Literal (Boolean _) -> Some bool
    | Literal (EnumerationMember { enumeration_type; _ }) -> Some enumeration_type
    | _ -> None
  in
  Transforms.apply_type_map ~type_map annotation


let split annotation =
  let open Record.Argument in
  match annotation with
  | Union [NoneType; argument]
  | Union [argument; NoneType] ->
      Primitive "typing.Optional", [Single argument]
  | Parametric { name; arguments } -> Primitive name, arguments
  | Tuple tuple ->
      (* We want to return a type `typing.tuple[X]` where X is the type that would make `given_tuple
         <: Tuple[X, ...]`. *)
      let arguments =
        match tuple with
        | Concatenation { middle = UnboundedElements argument; prefix = []; suffix = [] } ->
            [Single argument]
        | ordered_type -> [Single (OrderedTypes.union_upper_bound ordered_type)]
      in
      Primitive "tuple", arguments
  | Literal _ as literal -> weaken_literals literal, []
  | Callable _ -> Primitive "typing.Callable", []
  | annotation -> annotation, []


let class_name annotation =
  let strip_calls =
    let rec collect_identifiers identifiers = function
      | { Node.value = Expression.Subscript { base; _ }; _ } -> collect_identifiers identifiers base
      | { Node.value = Name (Name.Identifier identifier); _ } -> identifier :: identifiers
      | { Node.value = Name (Name.Attribute { base; attribute; _ }); _ } ->
          collect_identifiers (attribute :: identifiers) base
      | _ -> identifiers
    in
    collect_identifiers []
  in
  split annotation
  |> fst
  |> ToExpression.expression
  |> strip_calls
  |> fun identifiers ->
  if List.is_empty identifiers then
    Reference.create "typing.Any"
  else
    Reference.create_from_list identifiers


let class_variable annotation = Constructors.parametric "typing.ClassVar" [Single annotation]

(* Angelic assumption: Any occurrences of top indicate that we're dealing with Any instead of None.
   See T22792667. *)
let assume_any = function
  | Top -> Any
  | annotation -> annotation


let resolve_aliases ~aliases annotation =
  let visited = Containers.Hash_set.create () in
  let module ResolveAliasesTransform = VisitWithTransform.Make (struct
    type state = unit

    let visit_children_before _ _ = false

    let visit_children_after = true

    let visit _ annotation =
      let resolve annotation =
        if Core.Hash_set.mem visited annotation then
          annotation
        else (
          Core.Hash_set.add visited annotation;
          let mark_recursive_alias_as_visited = function
            | RecursiveType { name; _ } ->
                (* Don't resolve the inner reference to the type. *)
                Core.Hash_set.add visited (Primitive name)
            | _ -> ()
          in
          match annotation with
          | Primitive name -> (
              match aliases ?replace_unbound_parameters_with_any:(Some true) name with
              | Some alias ->
                  let alias =
                    match alias with
                    (* Type variables are stored as `_T aliases to _T`. Don't replace them. *)
                    | Variable _ as variable -> variable
                    | alias ->
                        alias
                        |> Variable.GlobalTransforms.TypeVar.replace_all (fun _ ->
                               Some Variable.TypeVar.any)
                        |> Variable.GlobalTransforms.ParamSpec.replace_all (fun _ ->
                               Some Variable.ParamSpec.any)
                        |> Variable.GlobalTransforms.TypeVarTuple.replace_all (fun _ ->
                               Some Variable.TypeVarTuple.any)
                  in
                  mark_recursive_alias_as_visited alias;
                  alias
              | _ -> annotation)
          | Parametric { name = alias_name; arguments = given_arguments } ->
              (* Pyre allows generic type aliases using the legacy (pre- PEP 695) syntax.

                 We consider generic over the type vars as deduplicated an in order of appearance
                 (in-order tree traversal) in the alias.

                 As an example, consider analyzing ``` T = TypeVar("T"); MyDictAlias = dict[T, T];
                 x: MyDictAlias[int] = {} ``` When we are processing the annotation on `x` we treat
                 the alias as generic do the following: - note that the form is `Parametric` with
                 one type param (`given_arguments` is [int]) - look up the type alias, and note that
                 its tree representation has two free type vars `T` and `T` - deduplicate the type
                 vars preserving the order, in this gase giving us just [T] - pair the type vars
                 with the arguments in `given_arguments` and instantiate to get that `x` has type
                 `dict[int, int]`.

                 In addition to handling explicitly generic type aliases, Pyre also treats type
                 aliases whose targets are generic as implicitly generic, e.g. `MyDictAlias = dict`
                 is implicitly generic with the same type arguments as `dict` itself. The reason we
                 pass `replace_unbound_parameters_with_any:(Some false)` here is because if we did
                 not, then in some cases the `aliases` function would eagerly sanitize the bare
                 `dict` to `dict[Any, Any]`, thereby preventing us from treating it as implicitly
                 generic.

                 One thing to note about this logic is that if the type arguments on the alias
                 itself are structurally invalid (e.g. MyDictAlias[int, str, float]) then the zip
                 will fail and we'll return the alias target (in this case dict[K, V] directly,
                 which means we're returning a `Type.t` with unbound type variables. This is
                 probably a bug, although the semantics of Pyre's physical modeling of type
                 variables is complex enough that I cannot say for certain whether this causes
                 actual problems. *)
              let resolved =
                match aliases ?replace_unbound_parameters_with_any:(Some false) alias_name with
                | None -> annotation
                | Some uninstantiated_alias_annotation -> (
                    let variable_pairs =
                      let deduplicate_preserving_order list =
                        List.fold list ~init:([], Variable.Set.empty) ~f:(fun (sofar, seen_set) x ->
                            if Core.Set.mem seen_set x then
                              sofar, seen_set
                            else
                              x :: sofar, Core.Set.add seen_set x)
                        |> fst
                        |> List.rev
                      in
                      Variable.zip_variables_with_arguments
                        ~arguments:given_arguments
                        (Variable.all_free_variables uninstantiated_alias_annotation
                        |> deduplicate_preserving_order)
                    in
                    match variable_pairs with
                    | Some variable_pairs ->
                        uninstantiated_alias_annotation
                        |> Variable.GlobalTransforms.TypeVar.replace_all (fun given_variable ->
                               List.find_map variable_pairs ~f:(function
                                   | TypeVarPair (variable, replacement)
                                     when [%equal: Variable.unary_t] variable given_variable ->
                                       Some replacement
                                   | _ -> None))
                        |> Variable.GlobalTransforms.ParamSpec.replace_all (fun given_variable ->
                               List.find_map variable_pairs ~f:(function
                                   | ParamSpecPair (variable, replacement)
                                     when [%equal: Variable.parameter_variadic_t]
                                            variable
                                            given_variable ->
                                       Some replacement
                                   | _ -> None))
                        |> Variable.GlobalTransforms.TypeVarTuple.replace_all (fun given_variable ->
                               List.find_map variable_pairs ~f:(function
                                   | TypeVarTuplePair (variable, replacement)
                                     when [%equal: Variable.tuple_variadic_t]
                                            variable
                                            given_variable ->
                                       Some replacement
                                   | _ -> None))
                    | _ -> uninstantiated_alias_annotation)
              in
              mark_recursive_alias_as_visited resolved;
              resolved
          | RecursiveType _ ->
              mark_recursive_alias_as_visited annotation;
              annotation
          | _ -> annotation)
      in
      let transformed_annotation = resolve annotation in
      { VisitWithTransform.transformed_annotation; new_state = () }
  end)
  in
  snd (ResolveAliasesTransform.visit () annotation)


let create ~variables ~aliases = create_logic ~resolve_aliases:(resolve_aliases ~aliases) ~variables

let namespace_insensitive_compare left right =
  compare
    (Variable.converge_all_variable_namespaces left)
    (Variable.converge_all_variable_namespaces right)


let dequalify map annotation =
  let dequalify_string string = string |> dequalify_identifier map in
  let module DequalifyTransform = VisitWithTransform.Make (struct
    type state = unit

    let visit_children_before _ _ = true

    let visit_children_after = false

    let visit _ annotation =
      let transformed_annotation =
        match annotation with
        | NoneType -> NoneType
        | Union [NoneType; argument]
        | Union [argument; NoneType] ->
            Parametric { name = dequalify_string "typing.Optional"; arguments = [Single argument] }
        | Parametric { name; arguments } ->
            Parametric
              {
                name = dequalify_identifier map (Canonicalization.reverse_substitute name);
                arguments;
              }
        | Union arguments ->
            Parametric
              {
                name = dequalify_string "typing.Union";
                arguments = List.map arguments ~f:(fun argument -> Record.Argument.Single argument);
              }
        | Tuple (Concrete arguments) ->
            Parametric
              {
                name = dequalify_string "typing.Tuple";
                arguments = List.map arguments ~f:(fun argument -> Record.Argument.Single argument);
              }
        | Primitive name -> Primitive (dequalify_identifier map name)
        | Variable ({ name; _ } as annotation) ->
            Variable { annotation with name = dequalify_identifier map name }
        | Callable ({ kind; _ } as callable) ->
            let kind =
              match kind with
              | Anonymous -> kind
              | Named reference -> Named (dequalify_reference map reference)
            in
            Callable { callable with kind }
        | _ -> annotation
      in
      { VisitWithTransform.transformed_annotation; new_state = () }
  end)
  in
  snd (DequalifyTransform.visit () annotation)


(* Transform tuples and callables so they are printed correctly when running infer and click to
   fix. *)

let preprocess_alias_value value =
  value
  |> Preprocessing.replace_union_shorthand_in_annotation_expression
  |> Preprocessing.expand_strings_in_annotation_expression ~preserve_original_location:true


let infer_transform annotation =
  let module InferTransform = VisitWithTransform.Make (struct
    type state = unit

    let visit_children_before _ _ = true

    let visit_children_after = false

    let visit _ annotation =
      let transformed_annotation =
        let shorten_tuple_type types =
          let argument = List.hd types |> Option.value ~default:Bottom in
          let should_be_unbound =
            List.fold types ~init:true ~f:(fun all_match next_argument ->
                if equal argument next_argument then
                  all_match
                else
                  false)
          in
          if should_be_unbound then
            Some (Tuple (OrderedTypes.create_unbounded_concatenation argument))
          else
            None
        in
        match annotation with
        | Tuple (Concrete types) when List.length types > 2 ->
            shorten_tuple_type types |> Option.value ~default:annotation
        | Parametric { name = "typing.Tuple"; arguments } when List.length arguments > 2 ->
            let types = List.filter_map arguments ~f:Argument.is_single in
            if List.length types < List.length arguments then
              annotation
            else
              shorten_tuple_type types |> Option.value ~default:annotation
        | Callable
            ({ implementation = { parameters = Defined parameters; _ } as implementation; _ } as
            callable) ->
            let parameters =
              let transform_parameter index parameter =
                match parameter with
                | CallableParamType.PositionalOnly { annotation; _ }
                | KeywordOnly { annotation; _ }
                | Named { annotation; _ }
                | Variable (Concrete annotation) ->
                    CallableParamType.PositionalOnly { annotation; default = false; index }
                | _ -> parameter
              in
              List.mapi parameters ~f:transform_parameter
            in
            let implementation = { implementation with parameters = Defined parameters } in
            Callable { callable with implementation }
        | Parametric { name = "typing.Dict"; arguments = [Single Bottom; Single Bottom] } ->
            Constructors.dictionary ~key:Any ~value:Any
        | Parametric { name = "List" | "typing.List"; arguments = [Single Bottom] } ->
            Constructors.list Any
        (* This is broken in typeshed:
           https://github.com/python/typeshed/pull/991#issuecomment-288160993 *)
        | Primitive "_PathLike" -> Primitive "PathLike"
        | Parametric { name = "_PathLike"; arguments } ->
            Parametric { name = "PathLike"; arguments }
        | Parametric { name = "Union" | "typing.Union"; arguments } ->
            Argument.all_singles arguments
            >>| Constructors.union
            |> Option.value ~default:annotation
        | _ -> annotation
      in
      { VisitWithTransform.transformed_annotation; new_state = () }
  end)
  in
  snd (InferTransform.visit () annotation)


type class_attribute_lookup_data = {
  class_name: Primitive.t;
  type_for_lookup: t;
  accessed_through_class: bool;
  accessed_through_readonly: bool;
}
[@@deriving sexp]

(* Extract the class data needed to look up an attribute, potentially returning data for multiple
   classes, e.g., for unions.

   For example, on `Foo | list[Bar]`, this function will return class data for `Foo` and
   `list[Bar]`. For complex types, such as `list[Bar]`, we need to return both the class name
   (`list`) and its fully instantiated type (`list[Bar]`), since the latter is needed to instantiate
   any generic attributes or methods, e.g., `my_list[0]` needs to be of type `Bar`.

   A complication is that we need to track whether the class was wrapped by `Type[...]` or
   `PyreReadOnly[...]`, since they affect the type of the attribute looked up, by making it
   `Type[X]` or `PyreReadOnly[X]`, respectively. *)
let class_attribute_lookups_for_type type_ =
  let rec extract_class_data ~accessed_through_class ~accessed_through_readonly original_type =
    let type_ =
      match original_type with
      (* Variables return their upper bound because we need to take the least informative type in
         their interval. Otherwise, we might access an attribute that doesn't exist on the actual
         type. *)
      | Variable variable -> Variable.TypeVar.upper_bound variable
      | _ -> original_type
    in
    match type_ with
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
              type_for_lookup = original_type;
              accessed_through_class;
              accessed_through_readonly;
              class_name = "typing.Optional";
            };
          ]
    | Union types ->
        (* Unions return the list of member classes because an attribute lookup has to be supported
           by all members of the union. *)
        let flatten_optional sofar optional =
          match sofar, optional with
          | Some sofar, Some optional -> Some (optional :: sofar)
          | _ -> None
        in
        List.map ~f:(extract_class_data ~accessed_through_class ~accessed_through_readonly) types
        |> List.fold ~init:(Some []) ~f:flatten_optional
        >>| List.concat
        >>| List.rev
    | RecursiveType ({ name; body } as recursive_type) ->
        extract_class_data ~accessed_through_class ~accessed_through_readonly body
        (* Filter out the recursive type name itself since it's not a valid class name.
         *
         * Removing the inner occurrences of the recursive type is fine because of induction. If the
         * other classes in a union support an attribute lookup, the recursive type will too. If
         * they don't, then the recursive type won't either. *)
        >>| List.filter ~f:(fun { class_name; _ } -> not (Identifier.equal class_name name))
        >>| List.map ~f:(fun ({ type_for_lookup; _ } as class_data) ->
                {
                  class_data with
                  type_for_lookup =
                    RecursiveType.replace_references_with_recursive_type
                      ~recursive_type
                      type_for_lookup;
                })
    | PyreReadOnly type_ ->
        (* The `PyreReadOnly[Xyz]` type behaves like a qualifier: anything accessed on a value of
           this type behaves like an access on Xyz, but restricted to read-only *)
        extract_class_data ~accessed_through_class ~accessed_through_readonly:true type_
    | type_ when Predicates.is_class_type type_ ->
        (* The `type[Xyz]` type (also known as `typing.Type[Xyz]`) indicates a class object: access
           on a value of this type is always a class rather than instance access. *)
        single_argument type_
        |> extract_class_data ~accessed_through_class:true ~accessed_through_readonly
    | _ -> (
        match split type_ |> fst |> primitive_name with
        | Some class_name ->
            Some
              [
                {
                  type_for_lookup = original_type;
                  accessed_through_class;
                  accessed_through_readonly;
                  class_name;
                };
              ]
        | None -> None)
  in
  extract_class_data ~accessed_through_class:false ~accessed_through_readonly:false type_


let callable_name = function
  | Callable { kind = Named name; _ } -> Some name
  | Parametric
      { name = "BoundMethod"; arguments = [Single (Callable { kind = Named name; _ }); Single _] }
    ->
      Some name
  | _ -> None


let equivalent_for_assert_type left right =
  let canonicalize original_type =
    let module CanonicalizeForAssertType = VisitWithTransform.Make (struct
      type state = unit

      let visit_children_before _ _ = true

      let visit_children_after = false

      let simplify_callable_in_bound_method { Record.Callable.kind; implementation; overloads } =
        let simplify_parameters = function
          | _ :: rest -> rest
          | [] -> []
        in
        let simplify_record_parameters = function
          | Record.Callable.Defined parameters ->
              Record.Callable.Defined (simplify_parameters parameters)
          | Record.Callable.FromParamSpec { head; variable } ->
              Record.Callable.FromParamSpec { head = simplify_parameters head; variable }
          | needs_no_change -> needs_no_change
        in
        let simplify_overload { Record.Callable.annotation; parameters } =
          { Record.Callable.annotation; parameters = simplify_record_parameters parameters }
        in
        {
          Record.Callable.kind;
          implementation = simplify_overload implementation;
          overloads = List.map ~f:simplify_overload overloads;
        }


      let visit new_state type_currently_visiting =
        let transformed_annotation =
          match type_currently_visiting with
          | Callable callable -> Callable { callable with kind = Record.Callable.Anonymous }
          | Parametric { name = "BoundMethod"; arguments = [Single (Callable callable); _] } ->
              Callable (simplify_callable_in_bound_method callable)
          | needs_no_changes -> needs_no_changes
        in
        { VisitWithTransform.transformed_annotation; new_state }
    end)
    in
    snd (CanonicalizeForAssertType.visit () original_type)
  in
  equal (canonicalize left) (canonicalize right)


let is_concrete annotation =
  let module ConcreteVisitor = VisitWithTransform.Make (struct
    type state = bool

    let visit_children_before _ = function
      | NoneType -> false
      | Parametric { name = "typing.Optional" | "Optional"; arguments = [Single Bottom] } -> false
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
      { VisitWithTransform.transformed_annotation = annotation; new_state }
  end)
  in
  fst (ConcreteVisitor.visit true annotation)
  && not (Variable.contains_escaped_free_variable annotation)


let exists = Visitors.exists

let collect_types = Visitors.collect_types

let collect_primitive_types = Visitors.collect_primitive_types

let collect_names = Visitors.collect_names

let apply_type_map = Transforms.apply_type_map

include Containers
include Constructors
include Extractions
include Predicates

let pp = PrettyPrinting.pp

let show = PrettyPrinting.show

let pp_concise = PrettyPrinting.pp_concise

let show_concise = PrettyPrinting.show_concise

(* We always send types in the pretty printed form *)
let to_yojson annotation = `String (PrettyPrinting.show annotation)

type type_t = T.t [@@deriving compare, equal, sexp, show, hash]

let expression = ToExpression.expression
