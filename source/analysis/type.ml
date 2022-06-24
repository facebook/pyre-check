(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

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

      module Tuple = struct
        type 'annotation record = {
          name: Identifier.t;
          state: state;
          namespace: RecordNamespace.t;
        }
        [@@deriving compare, eq, sexp, show, hash]

        let pp_concise format { name; _ } = Format.fprintf format "%s" name

        let create name = { name; state = Free { escaped = false }; namespace = 1 }
      end
    end

    type 'a record =
      | Unary of 'a RecordUnary.record
      | ParameterVariadic of 'a RecordVariadic.RecordParameters.record
      | TupleVariadic of 'a RecordVariadic.Tuple.record
    [@@deriving compare, eq, sexp, show, hash]
  end

  module OrderedTypes = struct
    let concatenate_public_name = "pyre_extensions.type_variable_operators.Concatenate"

    let unpack_public_name = "pyre_extensions.Unpack"

    let show_type_list types ~pp_type =
      Format.asprintf
        "%a"
        (Format.pp_print_list ~pp_sep:(fun format () -> Format.fprintf format ", ") pp_type)
        types


    module Concatenation = struct
      type 'annotation record_unpackable =
        | Variadic of 'annotation Variable.RecordVariadic.Tuple.record
        | UnboundedElements of 'annotation
        | Broadcast of 'annotation record_broadcast
      [@@deriving compare, eq, sexp, show, hash]

      and 'annotation record_broadcast =
        | ConcreteAgainstConcrete of {
            left: 'annotation list;
            right: 'annotation list;
          }
        | ConcreteAgainstConcatenation of {
            concrete: 'annotation list;
            concatenation: 'annotation t;
          }
        | ConcatenationAgainstConcatenation of {
            left_concatenation: 'annotation t;
            right_concatenation: 'annotation t;
          }
      [@@deriving compare, eq, sexp, show, hash]

      (* We guarantee that there is exactly one top-level unpacked variadic in this concatenation.

         Note that there may be unpacked variadics within the prefix or suffix, but they are not
         unpacked at the top-level. So, `Tuple[int, *Ts, Tuple[str, *Rs]]` will consider only the
         `*Ts` as the top-level unpacked variadic. *)
      and 'annotation t = {
        prefix: 'annotation list;
        middle: 'annotation record_unpackable;
        suffix: 'annotation list;
      }
      [@@deriving compare, eq, sexp, show, hash]

      let create_unpackable variadic = Variadic variadic

      let create_unbounded_unpackable annotation = UnboundedElements annotation

      let create_from_unpackable ?(prefix = []) ?(suffix = []) unpackable =
        { prefix; middle = unpackable; suffix }


      let create ?prefix ?suffix variadic =
        create_from_unpackable ?prefix ?suffix (Variadic variadic)


      let create_from_unbounded_element ?prefix ?suffix annotation =
        create_from_unpackable ?prefix ?suffix (UnboundedElements annotation)


      let create_unpackable_from_concrete_against_concrete ~compare_t ~left ~right =
        if List.compare compare_t left right < 0 then
          Broadcast (ConcreteAgainstConcrete { left = right; right = left })
        else
          Broadcast (ConcreteAgainstConcrete { left; right })


      let create_unpackable_from_concrete_against_concatenation ~concrete ~concatenation =
        Broadcast (ConcreteAgainstConcatenation { concrete; concatenation })


      let create_unpackable_from_concatenation_against_concatenation
          ~compare_t
          left_concatenation
          right_concatenation
        =
        if compare compare_t left_concatenation right_concatenation < 0 then
          Broadcast (ConcatenationAgainstConcatenation { left_concatenation; right_concatenation })
        else
          Broadcast
            (ConcatenationAgainstConcatenation
               {
                 left_concatenation = right_concatenation;
                 right_concatenation = left_concatenation;
               })


      let create_from_concrete_against_concrete ?prefix ?suffix ~compare_t ~left ~right =
        create_from_unpackable
          ?prefix
          ?suffix
          (create_unpackable_from_concrete_against_concrete ~compare_t ~left ~right)


      let create_from_concrete_against_concatenation ?prefix ?suffix ~concrete ~concatenation =
        create_from_unpackable
          ?prefix
          ?suffix
          (create_unpackable_from_concrete_against_concatenation ~concrete ~concatenation)


      let create_from_concatenation_against_concatenation
          ?prefix
          ?suffix
          ~compare_t
          left_concatenation
          right_concatenation
        =
        create_from_unpackable
          ?prefix
          ?suffix
          (create_unpackable_from_concatenation_against_concatenation
             ~compare_t
             left_concatenation
             right_concatenation)


      let rec pp_unpackable ~pp_type format = function
        | Variadic variadic ->
            Format.fprintf format "*%a" Variable.RecordVariadic.Tuple.pp_concise variadic
        | UnboundedElements annotation -> Format.fprintf format "*Tuple[%a, ...]" pp_type annotation
        | Broadcast broadcast ->
            Format.fprintf format "*Broadcast[%a]" (pp_broadcast ~pp_type) broadcast


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


      and pp_broadcast ~pp_type format = function
        | ConcreteAgainstConcrete { left; right } ->
            Format.fprintf
              format
              "typing.Tuple[%s], typing.Tuple[%s]"
              (show_type_list left ~pp_type)
              (show_type_list right ~pp_type)
        | ConcreteAgainstConcatenation { concrete; concatenation } ->
            Format.fprintf
              format
              "typing.Tuple[%s], typing.Tuple[%a]"
              (show_type_list concrete ~pp_type)
              (pp_concatenation ~pp_type)
              concatenation
        | ConcatenationAgainstConcatenation { left_concatenation; right_concatenation } ->
            Format.fprintf
              format
              "typing.Tuple[%a], typing.Tuple[%a]"
              (pp_concatenation ~pp_type)
              left_concatenation
              (pp_concatenation ~pp_type)
              right_concatenation


      let extract_sole_variadic = function
        | { prefix = []; middle = Variadic variadic; suffix = [] } -> Some variadic
        | _ -> None


      let extract_sole_unbounded_annotation = function
        | { prefix = []; middle = UnboundedElements annotation; suffix = [] } -> Some annotation
        | _ -> None


      let is_fully_unbounded concatenation =
        extract_sole_unbounded_annotation concatenation |> Option.is_some


      let rec unpackable_to_expression ~expression ~location unpackable =
        let argument =
          match unpackable with
          | Variadic variadic ->
              Expression.Name
                (create_name
                   ~location
                   (Format.asprintf "%a" Variable.RecordVariadic.Tuple.pp_concise variadic))
          | UnboundedElements annotation ->
              get_item_call
                ~location
                "typing.Tuple"
                [
                  expression annotation;
                  Expression.Constant Constant.Ellipsis |> Node.create ~location;
                ]
          | Broadcast broadcast ->
              let concatenation_to_expression { prefix; middle; suffix } =
                List.map ~f:expression prefix
                @ [unpackable_to_expression ~expression ~location middle]
                @ List.map ~f:expression suffix
                |> get_item_call ~location "typing.Tuple"
              in
              let broadcast_to_expression = function
                | ConcreteAgainstConcrete { left; right } ->
                    get_item_call
                      ~location
                      "pyre_extensions.Broadcast"
                      [
                        get_item_call ~location "typing.Tuple" (List.map ~f:expression left)
                        |> Node.create ~location;
                        get_item_call ~location "typing.Tuple" (List.map ~f:expression right)
                        |> Node.create ~location;
                      ]
                | ConcreteAgainstConcatenation { concrete; concatenation } ->
                    get_item_call
                      ~location
                      "pyre_extensions.Broadcast"
                      [
                        get_item_call ~location "typing.Tuple" (List.map ~f:expression concrete)
                        |> Node.create ~location;
                        concatenation_to_expression concatenation |> Node.create ~location;
                      ]
                | ConcatenationAgainstConcatenation { left_concatenation; right_concatenation } ->
                    get_item_call
                      ~location
                      "pyre_extensions.Broadcast"
                      [
                        concatenation_to_expression left_concatenation |> Node.create ~location;
                        concatenation_to_expression right_concatenation |> Node.create ~location;
                      ]
              in
              broadcast_to_expression broadcast
        in
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
                           Expression.Name (create_name ~location "pyre_extensions.Unpack")
                           |> Node.create ~location;
                         attribute = "__getitem__";
                         special = true;
                       });
              };
            arguments = [{ name = None; value = argument |> Node.create ~location }];
          }
        |> Node.create ~location
    end

    type 'annotation record =
      | Concrete of 'annotation list
      | Concatenation of 'annotation Concatenation.t
    [@@deriving compare, eq, sexp, show, hash]

    let create_unbounded_concatenation annotation =
      Concatenation (Concatenation.create_from_unbounded_element annotation)


    (* This represents the splitting of two ordered types to match each other in length. The prefix
       contains the prefix elements of known length that both have, the suffix contains the suffix
       elements of known length that both have, and the middle part contains the rest.

       [int, bool, str, int, bool] <: [int, int, *Ts, T]

       will be represented as:

       * prefix_match: [int; bool], [int; int].

       * middle_match: [str; int], *Ts

       * suffix_match: [bool], T.

       We don't include `str` in the prefixes because the corresponding `*Ts` on the right side is
       not of known length. Note that this doesn't check for compatibility; it is a purely
       length-based operation. *)
    type 'annotation ordered_type_split = {
      prefix_pairs: ('annotation * 'annotation) list;
      middle_pair: 'annotation record * 'annotation record;
      suffix_pairs: ('annotation * 'annotation) list;
    }
    [@@deriving compare, eq, sexp, show, hash]

    let pp_concise format variable ~pp_type =
      match variable with
      | Concrete types -> Format.fprintf format "%s" (show_type_list types ~pp_type)
      | Concatenation concatenation ->
          Format.fprintf format "%a" (Concatenation.pp_concatenation ~pp_type) concatenation


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
        unbounded tuple Tuple[X, ...], then pad the prefix with X in order to match the other
        prefix.

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
          split_concrete_against_concatenation
            ~is_left_concrete:false
            ~concrete:right
            ~concatenation
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
      | Concatenation ({ prefix; middle = Variadic _ | Broadcast _; _ } as concatenation) ->
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
          | Variadic _
          | Broadcast _ ->
              (* Variadic or Broadcast middle element may be empty, so we cannot index any elements
                 past the concrete prefix. *)
              result)
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
        | Concatenation of 'annotation OrderedTypes.Concatenation.t
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
              (OrderedTypes.Concatenation.pp_concatenation ~pp_type)
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
      | CallableParameters of 'annotation Callable.record_parameters
      | Unpacked of 'annotation OrderedTypes.Concatenation.record_unpackable
    [@@deriving compare, eq, sexp, show, hash]

    let is_single = function
      | Single single -> Some single
      | CallableParameters _
      | Unpacked _ ->
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

  module RecursiveType = struct
    type 'annotation record = {
      name: Identifier.t;
      body: 'annotation;
    }
    [@@deriving compare, eq, sexp, show, hash]

    let name { name; _ } = name
  end

  module TypeOperation = struct
    module Compose = struct
      type 'annotation t = 'annotation OrderedTypes.record
      [@@deriving compare, eq, sexp, show, hash]
    end

    type 'annotation record = Compose of 'annotation Compose.t
    [@@deriving compare, eq, sexp, show, hash]
  end
end

module rec Monomial : sig
  module Operation : sig
    type 'a t =
      (* A `Product` is an unpackable because it needs to contain the information of an unpacked
         tuple. We don't need the prefix and suffix of a `Concatenation.t`, because those are just
         absorbed into the entire product. *)
      | Product of 'a Record.OrderedTypes.Concatenation.record_unpackable
      | Divide of 'a Polynomial.t * 'a Polynomial.t
    [@@deriving compare, sexp, hash]
  end

  type 'a variable =
    | Variable of 'a Record.Variable.RecordUnary.record
    | Operation of 'a Operation.t
  [@@deriving compare, eq, sexp, show, hash]

  type 'a variable_degree = {
    variable: 'a variable;
    degree: int;
  }
  [@@deriving sexp, compare]

  type 'a t = {
    constant_factor: int;
    variables: 'a variable_degree list;
  }
  [@@deriving eq, sexp, compare, hash, show]

  val create_variable : 'a Record.Variable.RecordUnary.record -> 'a variable

  val create_product : 'a Record.OrderedTypes.Concatenation.record_unpackable -> 'a variable

  val equal_variable_id : compare_t:('a -> 'a -> int) -> 'a variable -> 'a variable -> bool

  val has_variable : compare_t:('a -> 'a -> int) -> 'a t -> variable:'a variable -> bool

  val show_normal
    :  show_variable:('a Record.Variable.RecordUnary.record -> string) ->
    show_type:(Format.formatter -> 'a -> unit) ->
    'a t ->
    string

  val normalize : compare_t:('a -> 'a -> int) -> 'a t -> 'a t

  val compare_normal : compare_t:('a -> 'a -> int) -> 'a t -> 'a t -> int

  val multiply : compare_t:('a -> 'a -> int) -> 'a t -> 'a t -> 'a t

  val create_from_list : compare_t:('a -> 'a -> int) -> int * ('a variable * int) list -> 'a t
end = struct
  module Operation = struct
    type 'a t =
      | Product of 'a Record.OrderedTypes.Concatenation.record_unpackable
      | Divide of 'a Polynomial.t * 'a Polynomial.t
    [@@deriving compare, eq, sexp, show, hash]
  end

  type 'a variable =
    | Variable of 'a Record.Variable.RecordUnary.record
    | Operation of 'a Operation.t
  [@@deriving compare, eq, sexp, show, hash]

  type 'a variable_degree = {
    variable: 'a variable;
    degree: int;
  }
  [@@deriving eq, sexp, compare, show, hash]

  type 'a t = {
    constant_factor: int;
    variables: 'a variable_degree list;
  }
  [@@deriving eq, sexp, compare, hash, show]

  let create_variable variable = Variable variable

  let create_product unpackable = Monomial.Operation (Product unpackable)

  let equal_variable_id ~compare_t left right =
    let compare = compare_variable compare_t in
    compare left right = 0


  let has_variable ~compare_t { variables; _ } ~variable =
    List.exists variables ~f:(fun { variable = variable'; _ } ->
        equal_variable_id ~compare_t variable variable')


  let compare_normal ~compare_t { variables = left_variables; _ } { variables = right_variables; _ }
    =
    let sum_degrees variables =
      List.fold variables ~init:0 ~f:(fun sum { degree; _ } -> sum + degree)
    in
    let total_degree_compare =
      Int.compare (sum_degrees left_variables) (sum_degrees right_variables)
    in
    let compare_variable_and_degree
        { Monomial.variable = left_variable; degree = left_degree }
        { Monomial.variable = right_variable; degree = right_degree }
      =
      match compare_variable compare_t left_variable right_variable with
      | result when not (Int.equal result 0) -> result
      | _ -> Int.compare left_degree right_degree
    in
    if total_degree_compare <> 0 then
      total_degree_compare
    else
      List.compare compare_variable_and_degree left_variables right_variables


  let normalize ~compare_t { constant_factor; variables } =
    let variables =
      List.sort variables ~compare:(fun { variable = variable1; _ } { variable = variable2; _ } ->
          (compare_variable compare_t) variable1 variable2)
    in
    { constant_factor; variables }


  let create_from_list ~compare_t (constant_factor, variables) =
    let variables = List.map variables ~f:(fun (variable, degree) -> { variable; degree }) in
    { constant_factor; variables } |> normalize ~compare_t


  let show_normal ~show_variable ~show_type { constant_factor; variables } =
    let string_of_variable ~variable ~degree =
      let name =
        match variable with
        | Variable variable -> show_variable variable
        | Operation (Divide (dividend, quotient)) ->
            let show_polynomial polynomial =
              let polynomial_string = Polynomial.show_normal polynomial ~show_variable ~show_type in
              if List.length polynomial > 1 then
                "(" ^ polynomial_string ^ ")"
              else
                polynomial_string
            in
            "(" ^ show_polynomial dividend ^ "//" ^ show_polynomial quotient ^ ")"
        | Operation (Product unpackable) ->
            Format.asprintf
              "Product[%a]"
              (Record.OrderedTypes.Concatenation.pp_unpackable ~pp_type:show_type)
              unpackable
      in

      name ^ if degree > 1 then "^" ^ string_of_int degree else ""
    in
    let list_variables =
      List.map variables ~f:(fun { variable; degree } -> string_of_variable ~variable ~degree)
    in
    let sep = "" in
    let concat_list = String.concat ~sep list_variables in
    let prefix =
      if constant_factor = 1 && List.length list_variables > 0 then
        ""
      else if constant_factor = -1 && List.length list_variables > 0 then
        "-"
      else
        string_of_int constant_factor ^ sep
    in
    prefix ^ concat_list


  let multiply
      ~compare_t
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
                    equal_variable_id left_variable right_variable ~compare_t)
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
                     equal_variable_id left_variable right_variable ~compare_t)))
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

and Polynomial : sig
  type 'a t = 'a Monomial.t list [@@deriving compare, eq, sexp, hash, show]

  val show_normal
    :  show_variable:('a Record.Variable.RecordUnary.record -> string) ->
    show_type:(Format.formatter -> 'a -> unit) ->
    'a t ->
    string

  val is_base_case : 'a t -> bool

  val create_from_variable : 'a Record.Variable.RecordUnary.record -> 'a t

  val create_from_int : int -> 'a t

  val create_from_operation : 'a Monomial.Operation.t -> 'a t

  val normalize : compare_t:('a -> 'a -> int) -> 'a t -> 'a t

  val create_from_variables_list
    :  compare_t:('a -> 'a -> int) ->
    (int * ('a Record.Variable.RecordUnary.record * int) list) list ->
    'a t

  val create_from_monomial_variables_list
    :  compare_t:('a -> 'a -> int) ->
    (int * ('a Monomial.variable * int) list) list ->
    'a t

  val add : compare_t:('a -> 'a -> int) -> 'a t -> 'a t -> 'a t

  val subtract : compare_t:('a -> 'a -> int) -> 'a t -> 'a t -> 'a t

  val multiply : compare_t:('a -> 'a -> int) -> 'a t -> 'a t -> 'a t

  val divide : compare_t:('a -> 'a -> int) -> 'a t -> 'a t -> 'a t

  val replace
    :  compare_t:('a -> 'a -> int) ->
    'a t ->
    by:'a t ->
    variable:'a Monomial.variable ->
    'a t
end = struct
  type 'a t = 'a Monomial.t list [@@deriving compare, eq, sexp, hash, show]

  let rec show_normal ~show_variable ~show_type polynomial =
    match polynomial with
    | [] -> "0"
    | [x] -> Monomial.show_normal ~show_variable ~show_type x
    | x :: xs ->
        Monomial.show_normal ~show_variable ~show_type x
        ^ " + "
        ^ show_normal ~show_variable ~show_type xs


  let fold1 l ~f =
    match l with
    | [] -> []
    | hd :: tl -> List.fold tl ~init:hd ~f


  let create_from_int value =
    if value = 0 then [] else [{ Monomial.constant_factor = value; variables = [] }]


  let create_from_operation operation =
    [{ Monomial.constant_factor = 1; variables = [{ variable = Operation operation; degree = 1 }] }]


  let create_from_variable variable =
    [{ Monomial.constant_factor = 1; variables = [{ variable = Variable variable; degree = 1 }] }]


  let is_base_case = function
    | []
    | [{ Monomial.variables = []; _ }]
    | [{ Monomial.variables = [{ degree = 1; variable = Variable _ }]; constant_factor = 1 }] ->
        true
    | _ -> false


  (* Graded lexicographic order:
     https://www.wikiwand.com/en/Monomial_order#/Graded_lexicographic_order *)
  let normalize ~compare_t polynomial =
    List.filter polynomial ~f:(fun { Monomial.constant_factor; _ } -> constant_factor <> 0)
    |> List.map ~f:(Monomial.normalize ~compare_t)
    |> List.sort ~compare:(Monomial.compare_normal ~compare_t)


  let create_from_variables_list ~compare_t list =
    let as_variable (constant_factor, variables) =
      ( constant_factor,
        List.map variables ~f:(fun (variable, degree) -> Monomial.Variable variable, degree) )
    in
    List.map list ~f:as_variable
    |> List.map ~f:(Monomial.create_from_list ~compare_t)
    |> normalize ~compare_t


  let create_from_monomial_variables_list ~compare_t list =
    list |> List.map ~f:(Monomial.create_from_list ~compare_t) |> normalize ~compare_t


  let merge ~compare_t left_polynomial right_polynomial ~operation =
    let normalize = normalize ~compare_t in
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
          ({ Monomial.constant_factor = right_factor; variables = right_variables } as
          right_monomial)
          :: right_polynomial ) ->
          let comparison = Monomial.compare_normal left_monomial right_monomial ~compare_t in
          if comparison = 0 then
            { constant_factor = operation left_factor right_factor; variables = left_variables }
            :: merge_sorted left_polynomial right_polynomial
          else if comparison < 0 then
            { constant_factor = left_factor; variables = left_variables }
            :: merge_sorted left_polynomial (right_monomial :: right_polynomial)
          else
            { constant_factor = operation 0 right_factor; variables = right_variables }
            :: merge_sorted (left_monomial :: left_polynomial) right_polynomial
    in
    merge_sorted (normalize left_polynomial) (normalize right_polynomial) |> normalize


  let add ~compare_t left right = merge ~compare_t left right ~operation:`Plus

  let subtract ~compare_t left right = merge ~compare_t left right ~operation:`Minus

  let multiply ~compare_t left_polynomial right_polynomial =
    let multiply_monomial_polynomial (monomial : 'a Monomial.t) ~(polynomial : 'a t) =
      List.map polynomial ~f:(Monomial.multiply monomial ~compare_t)
    in
    List.concat_map left_polynomial ~f:(multiply_monomial_polynomial ~polynomial:right_polynomial)
    |> List.map ~f:(fun monomial -> [Monomial.normalize ~compare_t monomial])
    |> fold1 ~f:(add ~compare_t)


  let rec pow ~compare_t polynomial n =
    if n > 1 then
      multiply ~compare_t polynomial (pow ~compare_t polynomial (n - 1))
    else
      polynomial


  (* Example: polynomial:(3x + 2yx^2 + y + 3), by_polynomial:(4z+3), variable:x *)
  let replace ~compare_t polynomial ~by:by_polynomial ~variable =
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
            Monomial.equal_variable_id variable variable' ~compare_t)
      in
      (* 4z+3 -> (4z+3)^2 *)
      let pow_polynomial = pow polynomial degree ~compare_t in
      (* 2yx^2 -> 2y *)
      let polynomial_from_mono_without_replaced_variable =
        [
          {
            Monomial.constant_factor;
            variables =
              List.filter variables ~f:(fun { variable = variable'; _ } ->
                  not (Monomial.equal_variable_id variable variable' ~compare_t));
          };
        ]
      in
      (* 2y * (4z+3)^2 *)
      multiply polynomial_from_mono_without_replaced_variable pow_polynomial ~compare_t
    in
    (* 3x + 2yx^2 + y + 3 -> (3x + 2yx^2, y + 3) *)
    let modified_polynomial, base_polynomial =
      List.partition_tf polynomial ~f:(Monomial.has_variable ~variable ~compare_t)
    in
    (* 3x + 2yx^2 -> 3 * (4z+3) ; 2y * (4z+3)^2 *)
    let replaced_polynomial =
      List.map modified_polynomial ~f:(fun monomial ->
          replace_variable_by_polynomial ~monomial ~polynomial:by_polynomial ~variable)
    in
    (* (3 * (4z+3)) + (2y * (4z+3)^2) *)
    let merged_polynomial = fold1 replaced_polynomial ~f:(add ~compare_t) in
    (* (y + 3) + (3 * (4z+3) + 2y * (4z+3)^2) *)
    add base_polynomial merged_polynomial ~compare_t


  let divide ~compare_t left right =
    let floor_division dividend quotient =
      let dividend = float_of_int dividend in
      let quotient = float_of_int quotient in
      dividend /. quotient |> floor |> int_of_float
    in
    let simplify left right =
      let rec gcd x y = if y = 0 then x else gcd y (x mod y) in
      let quotient =
        match left @ right with
        | [] -> 1
        | { Monomial.constant_factor; _ } :: tl ->
            List.fold
              tl
              ~init:constant_factor
              ~f:(fun denominator { Monomial.constant_factor; _ } ->
                gcd denominator constant_factor)
      in
      let intersect_variables left right =
        List.filter_map left ~f:(fun { Monomial.variable = variable_left; degree = degree_left } ->
            let other_variable_degree =
              List.find right ~f:(fun { Monomial.variable = variable_right; _ } ->
                  Monomial.equal_variable_id variable_left variable_right ~compare_t)
            in
            other_variable_degree
            >>| fun { Monomial.variable; degree = degree_right } ->
            { Monomial.variable; degree = min degree_left degree_right })
      in
      let common_variables =
        match left @ right with
        | [] -> []
        | { Monomial.variables; _ } :: tl ->
            let tail_monomial_variables =
              List.map tl ~f:(fun { Monomial.variables; _ } -> variables)
            in
            List.fold tail_monomial_variables ~init:variables ~f:intersect_variables
      in
      let factorise_quotients =
        List.map ~f:(fun ({ Monomial.constant_factor; _ } as polynomial) ->
            { polynomial with constant_factor = floor_division constant_factor quotient })
      in
      let divide_by_variable
          ({ Monomial.variable = variable_left; degree = degree_left } as variable_degree_left)
        =
        let common_variable =
          List.find common_variables ~f:(fun { Monomial.variable = variable_right; _ } ->
              Monomial.equal_variable_id variable_left variable_right ~compare_t)
        in
        match common_variable with
        | Some { Monomial.degree = degree_right; _ } when degree_right = degree_left -> None
        | Some { Monomial.degree = degree_right; _ } ->
            Some { Monomial.degree = degree_left - degree_right; variable = variable_left }
        | None -> Some variable_degree_left
      in
      let factorise_variables =
        List.map ~f:(fun ({ Monomial.variables; _ } as polynomial) ->
            { polynomial with variables = List.filter_map variables ~f:divide_by_variable })
      in
      let left = factorise_quotients left |> factorise_variables in
      let right = factorise_quotients right |> factorise_variables in
      left, right
    in
    let left, right = simplify left right in
    let get_literal polynomial =
      match polynomial with
      | [{ Monomial.variables = []; constant_factor }] -> Some constant_factor
      | [] -> Some 0
      | _ -> None
    in
    match get_literal left, get_literal right with
    | _, Some 0
    | Some 0, _ ->
        create_from_int 0
    | Some left, Some right -> create_from_int (floor_division left right)
    | _, Some 1 -> left
    | _ ->
        [
          {
            Monomial.constant_factor = 1;
            variables = [{ variable = Monomial.Operation (Divide (left, right)); degree = 1 }];
          };
        ]
end

module RecordIntExpression : sig
  type 'a t = private Data of 'a Polynomial.t [@@deriving compare, eq, sexp, show, hash]

  type 'a variant =
    | Constant of int
    | Variable of 'a Record.Variable.RecordUnary.record
    | Polynomial of 'a t

  val normalize_variant : compare_t:('a -> 'a -> int) -> 'a Polynomial.t -> 'a variant
end = struct
  type 'a t = Data of 'a Polynomial.t [@@deriving compare, eq, sexp, show, hash]

  type 'a variant =
    | Constant of int
    | Variable of 'a Record.Variable.RecordUnary.record
    | Polynomial of 'a t

  let normalize_variant ~compare_t polynomial =
    let normalized = Polynomial.normalize ~compare_t polynomial in
    match normalized with
    | [] -> Constant 0
    | [{ variables = []; constant_factor }] -> Constant constant_factor
    | [
     {
       Monomial.constant_factor = 1;
       variables = [{ Monomial.variable = Monomial.Variable variable_name; degree = 1 }];
     };
    ] ->
        Variable variable_name
    | _ -> Polynomial (Data normalized)
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
    | RecursiveType of t Record.RecursiveType.record
    | Top
    | Tuple of t Record.OrderedTypes.record
    | TypeOperation of t Record.TypeOperation.record
    | Union of t list
    | Variable of t Record.Variable.RecordUnary.record
    | IntExpression of t RecordIntExpression.t
  [@@deriving compare, eq, sexp, show, hash]
end

include T

type type_t = t [@@deriving compare, eq, sexp, show, hash]

module IntExpression : sig
  val create : type_t Polynomial.t -> type_t

  val type_to_int_expression : type_t -> type_t option

  val apply_over_types
    :  operation:(type_t Polynomial.t -> type_t Polynomial.t -> type_t Polynomial.t) ->
    ?divide:bool ->
    type_t ->
    type_t ->
    type_t

  val create_int_expression_from_types
    :  operation:[< `Add | `Subtract | `Multiply | `Divide ] ->
    type_t list ->
    type_t

  val create_product_from_ordered_type : type_t Record.OrderedTypes.record -> type_t option

  val visit
    :  visit_unpackable:
         (type_t Record.OrderedTypes.Concatenation.record_unpackable ->
         type_t Record.OrderedTypes.Concatenation.record_unpackable) ->
    type_t Polynomial.t ->
    type_t Polynomial.t

  val replace_variadic
    :  replace_unpackable:
         (type_t Record.OrderedTypes.Concatenation.record_unpackable -> type_t option) ->
    type_t Polynomial.t ->
    type_t

  val collect
    :  collect_from_variable:(type_t Monomial.variable -> 'a list) ->
    compare:('a -> 'a -> int) ->
    type_t Polynomial.t ->
    'a list
end = struct
  let create polynomial =
    match RecordIntExpression.normalize_variant ~compare_t:[%compare: type_t] polynomial with
    | RecordIntExpression.Constant n -> Literal (Integer n)
    | RecordIntExpression.Variable variable_name -> Variable variable_name
    | RecordIntExpression.Polynomial polynomial -> IntExpression polynomial


  let type_to_int_expression = function
    | Literal (Integer literal) -> Some (create (Polynomial.create_from_int literal))
    | Variable variable -> Some (create (Polynomial.create_from_variable variable))
    | IntExpression (Data polynomial) -> Some (create polynomial)
    | Primitive "int" -> Some (Primitive "int")
    | Any -> Some Any
    | _ -> None


  let apply_over_types ~operation ?(divide = false) left right =
    let checked_division left right =
      if List.length right = 0 then
        Bottom
      else
        create (Polynomial.divide left right ~compare_t:T.compare)
    in
    let type_to_polynomial = function
      | Literal (Integer n) -> Some (Polynomial.create_from_int n)
      | Variable variable_name -> Some (Polynomial.create_from_variable variable_name)
      | IntExpression (Data polynomial) -> Some polynomial
      | _ -> None
    in
    let non_int_expression_result left right =
      match left, right with
      | Bottom, _
      | _, Bottom ->
          Bottom
      | Any, _
      | _, Any ->
          Any
      | Parametric { name = "pyre_extensions.BroadcastError"; _ }, _ -> left
      | _, Parametric { name = "pyre_extensions.BroadcastError"; _ } -> right
      | Primitive "int", _
      | _, Primitive "int" ->
          Primitive "int"
      | _ -> Bottom
    in
    match type_to_polynomial left, type_to_polynomial right with
    | Some left_polynomial, Some right_polynomial ->
        if divide then
          checked_division left_polynomial right_polynomial
        else
          create (operation left_polynomial right_polynomial)
    | _ -> non_int_expression_result left right


  let create_int_expression_from_types ~operation = function
    | [] -> create (Polynomial.create_from_int 1)
    | hd :: tl ->
        let operation, divide =
          match operation with
          | `Add -> Polynomial.add, false
          | `Multiply -> Polynomial.multiply, false
          | `Subtract -> Polynomial.subtract, false
          | `Divide -> Polynomial.divide, true
        in
        List.fold
          tl
          ~init:hd
          ~f:(apply_over_types ~operation:(operation ~compare_t:T.compare) ~divide)


  let create_product_from_ordered_type = function
    | Record.OrderedTypes.Concrete annotations ->
        List.map ~f:type_to_int_expression annotations
        |> Option.all
        >>| create_int_expression_from_types ~operation:`Multiply
    | Concatenation { prefix; middle; suffix } ->
        let multiply_multiplicands multiplicands =
          match middle with
          | UnboundedElements (Literal (Integer 0)) -> create (Polynomial.create_from_int 0)
          | UnboundedElements (Literal (Integer 1)) ->
              create_int_expression_from_types ~operation:`Multiply multiplicands
          | UnboundedElements (Literal (Integer _))
          | UnboundedElements (IntExpression _)
          | UnboundedElements (Primitive "int") ->
              Primitive "int"
          | UnboundedElements _ -> Top
          | _ ->
              create_int_expression_from_types
                ~operation:`Multiply
                (create (Polynomial.create_from_operation (Monomial.Operation.Product middle))
                 :: multiplicands)
        in
        prefix @ suffix
        |> List.map ~f:type_to_int_expression
        |> Option.all
        >>| multiply_multiplicands


  let rec visit ~visit_unpackable polynomial =
    let visit_variable = function
      | Monomial.Operation (Divide (left_polynomial, right_polynomial)) ->
          Monomial.Operation
            (Divide
               (visit ~visit_unpackable left_polynomial, visit ~visit_unpackable right_polynomial))
      | Operation (Product unpackable) -> Operation (Product (visit_unpackable unpackable))
      | other -> other
    in
    let visit_monomial { Monomial.constant_factor; variables } =
      {
        Monomial.constant_factor;
        variables =
          List.map
            ~f:(fun { Monomial.variable; degree } ->
              { Monomial.variable = visit_variable variable; degree })
            variables;
      }
    in
    List.map ~f:visit_monomial polynomial


  let polynomial_replace_monomial_variable ~replace polynomial =
    let exponent ~degree input =
      List.init degree ~f:(Fn.const input)
      |> List.fold
           ~init:(Literal (Integer 1))
           ~f:(apply_over_types ~operation:(Polynomial.multiply ~compare_t:compare_type_t))
    in
    let replace_variable_with_power ~degree variable = replace variable |> exponent ~degree in
    let replace_monomial { Monomial.constant_factor; variables } =
      List.map
        ~f:(fun { Monomial.variable; degree } -> replace_variable_with_power ~degree variable)
        variables
      |> List.fold
           ~init:(Literal (Integer constant_factor))
           ~f:(apply_over_types ~operation:(Polynomial.multiply ~compare_t:compare_type_t))
    in
    List.map ~f:replace_monomial polynomial
    |> List.fold
         ~init:(Literal (Integer 0))
         ~f:(apply_over_types ~operation:(Polynomial.add ~compare_t:compare_type_t))


  let rec replace_variadic ~replace_unpackable polynomial =
    let replace_monomial_variable variable =
      match variable with
      | Monomial.Operation (Product unpackable) ->
          let expand_unpackable = function
            | Parametric { name = "pyre_extensions.BroadcastError"; _ } as broadcast_error ->
                broadcast_error |> Option.some
            | Tuple record -> record |> create_product_from_ordered_type
            | other -> type_to_int_expression other
          in
          replace_unpackable unpackable
          |> Option.value ~default:(create (Polynomial.create_from_operation (Product unpackable)))
          |> expand_unpackable
          |> Option.value ~default:Bottom
          (* TODO (T98054916): Add a `ProductError` error. *)
      | Operation (Divide (left_polynomial, right_polynomial)) ->
          apply_over_types
            ~operation:(Polynomial.divide ~compare_t:compare_type_t)
            ~divide:true
            (replace_variadic ~replace_unpackable left_polynomial)
            (replace_variadic ~replace_unpackable right_polynomial)
      | _ ->
          create [{ Monomial.constant_factor = 1; variables = [{ Monomial.variable; degree = 1 }] }]
    in
    polynomial_replace_monomial_variable ~replace:replace_monomial_variable polynomial


  let collect ~collect_from_variable ~compare polynomial =
    List.concat_map polynomial ~f:(fun { Monomial.variables; _ } ->
        List.concat_map variables ~f:(fun { variable; _ } -> collect_from_variable variable))
    |> List.dedup_and_sort ~compare
end

let _ = show (* shadowed below *)

type class_data = {
  instantiated: t;
  accessed_through_class: bool;
  class_name: Primitive.t;
}
[@@deriving sexp]

let polynomial_to_type polynomial =
  match polynomial with
  | [] -> Literal (Integer 0)
  | [{ Monomial.variables = []; constant_factor }] -> Literal (Integer constant_factor)
  | [{ Monomial.variables = [{ degree = 1; variable = Variable variable }]; constant_factor = 1 }]
    ->
      Variable variable
  | _ -> IntExpression.create polynomial


let solve_less_or_equal_polynomial ~left ~right ~solve ~impossible =
  match left, right with
  | IntExpression (Data polynomial), _ when Polynomial.is_base_case polynomial ->
      solve ~left:(polynomial_to_type polynomial) ~right
  | _, IntExpression (Data polynomial) when Polynomial.is_base_case polynomial ->
      solve ~left ~right:(polynomial_to_type polynomial)
  | ( IntExpression (Data ({ Monomial.constant_factor; variables = [] } :: polynomial)),
      Literal (Integer literal) ) ->
      solve
        ~left:(IntExpression.create polynomial)
        ~right:(Literal (Integer (literal - constant_factor)))
  | ( Literal (Integer literal),
      IntExpression (Data ({ Monomial.constant_factor; variables = [] } :: polynomial)) ) ->
      solve
        ~left:(Literal (Integer (literal - constant_factor)))
        ~right:(IntExpression.create polynomial)
  | IntExpression (Data [{ Monomial.constant_factor; variables }]), Literal (Integer literal)
    when constant_factor <> 1 ->
      if literal mod constant_factor <> 0 then
        impossible
      else
        solve
          ~left:(IntExpression.create [{ constant_factor = 1; variables }])
          ~right:(Literal (Integer (literal / constant_factor)))
  | Literal (Integer literal), IntExpression (Data [{ Monomial.constant_factor; variables }])
    when constant_factor <> 1 ->
      if literal mod constant_factor <> 0 then
        impossible
      else
        solve
          ~left:(Literal (Integer (literal / constant_factor)))
          ~right:(IntExpression.create [{ Monomial.constant_factor = 1; variables }])
  | ( IntExpression (Data (({ variables = []; _ } as left_monomial) :: left_polynomial_tail)),
      IntExpression (Data ({ variables = []; _ } :: _ as right_polynomial)) ) ->
      let right_polynomial =
        Polynomial.subtract right_polynomial [left_monomial] ~compare_t:T.compare
      in
      solve
        ~left:(IntExpression.create left_polynomial_tail)
        ~right:(IntExpression.create right_polynomial)
  | IntExpression _, Primitive _ -> solve ~left:(Primitive "int") ~right
  | _ -> impossible


let local_replace_polynomial polynomial ~replace_variable ~replace_recursive =
  let collect polynomial =
    List.concat_map polynomial ~f:(fun { Monomial.variables; _ } ->
        List.map variables ~f:(fun { variable; _ } -> variable))
    |> List.dedup_and_sort ~compare:(Monomial.compare_variable T.compare)
  in
  let replacement_pair monomial_variable =
    match monomial_variable with
    | Monomial.Variable variable ->
        replace_variable variable >>| fun result -> monomial_variable, result
    | Monomial.Operation (Divide (dividend, quotient)) ->
        let replace_polynomial polynomial =
          match replace_recursive (IntExpression.create polynomial) with
          | Some replaced -> replaced
          | None -> IntExpression.create polynomial
        in
        let replaced_division =
          IntExpression.apply_over_types
            (replace_polynomial dividend)
            (replace_polynomial quotient)
            ~divide:true
            ~operation:(fun x _ -> x)
        in
        Some (monomial_variable, replaced_division)
    | Monomial.Operation (Product _) -> None
  in
  let replacements = collect polynomial |> List.filter_map ~f:replacement_pair in
  if List.length replacements = 0 then
    None
  else
    let merge_replace left (variable, right) =
      IntExpression.apply_over_types left right ~operation:(fun left right ->
          Polynomial.replace left ~by:right ~variable ~compare_t:T.compare)
    in
    let replaced_expression =
      List.fold replacements ~init:(IntExpression.create polynomial) ~f:merge_replace
    in
    Some replaced_expression


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

  let to_variable = function
    | Single (Variable variable) -> Some (Record.Variable.Unary variable)
    | CallableParameters (ParameterVariadicTypeVariable { head = []; variable }) ->
        Some (ParameterVariadic variable)
    | Unpacked (Variadic variadic) -> Some (TupleVariadic variadic)
    | _ -> None


  let is_unpacked = function
    | Unpacked _ -> true
    | _ -> false
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
      | _ -> true)
  | _ -> false


let is_dictionary_or_mapping = function
  | Parametric { name = "typing.Mapping" | "dict"; _ } -> true
  | _ -> false


let is_ellipsis = function
  | Primitive "ellipsis" -> true
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
  | Annotated annotated -> is_falsy annotated
  | Union types -> List.for_all types ~f:is_falsy
  | _ -> false


let rec is_truthy = function
  | Literal (Boolean true)
  | Callable _ ->
      true
  | Literal (Integer i) when not (Int.equal i 0) -> true
  | Literal (String (LiteralValue value))
  | Literal (Bytes value)
    when not (String.is_empty value) ->
      true
  | Annotated annotated -> is_truthy annotated
  | Union types -> List.for_all types ~f:is_truthy
  | _ -> false


let reverse_substitute name =
  match name with
  | "collections.defaultdict" -> "typing.DefaultDict"
  | "dict" -> "typing.Dict"
  | "frozenset" -> "typing.FrozenSet"
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
          name = Record.OrderedTypes.concatenate_public_name;
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
  | parameters
    when List.for_all parameters ~f:(function
             | Record.Parameter.Single parameter -> is_unbound parameter || is_top parameter
             | _ -> false) ->
      Format.fprintf format ""
  | parameters ->
      let s format = function
        | Record.Parameter.Single parameter -> Format.fprintf format "%a" pp_type parameter
        | CallableParameters parameters ->
            Format.fprintf format "%s" (show_callable_parameters parameters ~pp_type)
        | Unpacked unpackable ->
            Format.fprintf
              format
              "%a"
              (Record.OrderedTypes.Concatenation.pp_unpackable ~pp_type)
              unpackable
      in
      Format.pp_print_list ~pp_sep:(fun format () -> Format.fprintf format ", ") s format parameters


let pp_typed_dictionary_field ~pp_type format { Record.TypedDictionary.name; annotation; required } =
  Format.fprintf format "%s%s: %a" name (if required then "" else "?") pp_type annotation


let rec pp format annotation =
  let pp_ordered_type ordered_type =
    match ordered_type with
    | Record.OrderedTypes.Concatenation
        { middle = UnboundedElements annotation; prefix = []; suffix = [] } ->
        Format.asprintf "%a, ..." pp annotation
    | ordered_type -> Format.asprintf "%a" (Record.OrderedTypes.pp_concise ~pp_type:pp) ordered_type
  in
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
  | Literal (String (LiteralValue literal)) ->
      Format.fprintf format "typing_extensions.Literal['%s']" literal
  | Literal (String AnyLiteral) -> Format.fprintf format "typing_extensions.LiteralString"
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
  | RecursiveType { name; body } -> Format.fprintf format "%s (resolves to %a)" name pp body
  | Top -> Format.fprintf format "unknown"
  | Tuple ordered_type -> Format.fprintf format "typing.Tuple[%s]" (pp_ordered_type ordered_type)
  | TypeOperation (Compose ordered_type) ->
      Format.fprintf format "pyre_extensions.Compose[%s]" (pp_ordered_type ordered_type)
  | Union [NoneType; parameter]
  | Union [parameter; NoneType] ->
      Format.fprintf format "typing.Optional[%a]" pp parameter
  | Union parameters ->
      Format.fprintf
        format
        "typing.Union[%s]"
        (List.map parameters ~f:show |> String.concat ~sep:", ")
  | Variable unary -> Record.Variable.RecordUnary.pp_concise format unary ~pp_type:pp
  | IntExpression (Data polynomial) when Polynomial.is_base_case polynomial ->
      pp format (polynomial_to_type polynomial)
  | IntExpression (Data polynomial) ->
      Format.fprintf
        format
        "pyre_extensions.IntExpression[%s]"
        (Polynomial.show_normal polynomial ~show_variable:polynomial_show_variable ~show_type:pp)


and show annotation = Format.asprintf "%a" pp annotation

and pp_concise format annotation =
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
                      (Record.OrderedTypes.Concatenation.pp_concatenation ~pp_type:pp_concise)
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
  | Literal (String (LiteralValue literal)) ->
      Format.fprintf format "typing_extensions.Literal['%s']" literal
  | Literal (String AnyLiteral) -> Format.fprintf format "typing_extensions.LiteralString"
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
  | RecursiveType { name; _ } -> Format.fprintf format "%s" name
  | Top -> Format.fprintf format "unknown"
  | Tuple (Concatenation { middle = UnboundedElements parameter; prefix = []; suffix = [] }) ->
      Format.fprintf format "Tuple[%a, ...]" pp_concise parameter
  | Tuple ordered_type ->
      Format.fprintf
        format
        "Tuple[%a]"
        (Record.OrderedTypes.pp_concise ~pp_type:pp_concise)
        ordered_type
  | TypeOperation (Compose ordered_type) ->
      Format.fprintf
        format
        "Compose[%a]"
        (Record.OrderedTypes.pp_concise ~pp_type:pp_concise)
        ordered_type
  | Union [NoneType; parameter]
  | Union [parameter; NoneType] ->
      Format.fprintf format "Optional[%a]" pp_concise parameter
  | Union parameters -> Format.fprintf format "Union[%a]" pp_comma_separated parameters
  | Variable { variable; _ } -> Format.fprintf format "%s" (strip_qualification variable)
  | IntExpression (Data polynomial) when Polynomial.is_base_case polynomial ->
      pp_concise format (polynomial_to_type polynomial)
  | IntExpression (Data polynomial) ->
      Format.fprintf
        format
        "pyre_extensions.IntExpression[%s]"
        (Polynomial.show_normal
           polynomial
           ~show_variable:polynomial_show_variable
           ~show_type:pp_concise)


and show_concise annotation = Format.asprintf "%a" pp_concise annotation

and polynomial_show_variable variable = show_concise (Variable variable)

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

let generator ?(yield_type = Any) ?(send_type = Any) ?(return_type = Any) () =
  Parametric
    {
      name = "typing.Generator";
      parameters = [Single yield_type; Single send_type; Single return_type];
    }


let generator_expression yield_type =
  generator ~yield_type ~send_type:NoneType ~return_type:NoneType ()


let async_generator ?(yield_type = Any) ?(send_type = Any) () =
  Parametric { name = "typing.AsyncGenerator"; parameters = [Single yield_type; Single send_type] }


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

let literal_string literal = Literal (String (LiteralValue literal))

let literal_bytes literal = Literal (Bytes literal)

let tuple parameters = Tuple (Concrete parameters)

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
    "pyre_extensions.generic.Generic", "typing.Generic";
  ]
  |> Identifier.Table.of_alist_exn


let are_fields_total = List.for_all ~f:(fun { Record.TypedDictionary.required; _ } -> required)

let rec expression annotation =
  let location = Location.any in
  let create_name name = Expression.Name (create_name ~location name) in
  let get_item_call = get_item_call ~location in
  let concatenation_to_expressions
      { Record.OrderedTypes.Concatenation.prefix; middle = unpackable; suffix }
    =
    List.map ~f:expression prefix
    @ [Record.OrderedTypes.Concatenation.unpackable_to_expression ~expression ~location unpackable]
    @ List.map ~f:expression suffix
  in
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
              Expression.Call
                {
                  callee = Node.create ~location (Expression.Name (Name.Identifier "Variable"));
                  arguments =
                    concatenation_to_expressions concatenation
                    |> List.map ~f:(fun annotation ->
                           { Call.Argument.name = None; value = annotation });
                }
              |> Node.create ~location
        in
        Expression.List (List.map ~f:convert_parameter parameters) |> Node.create ~location
    | Undefined -> Node.create ~location (Expression.Constant Constant.Ellipsis)
    | ParameterVariadicTypeVariable variable ->
        parameter_variable_type_representation variable |> expression
  in
  let rec convert_annotation annotation =
    let convert_ordered_type ordered_type =
      match ordered_type with
      | Record.OrderedTypes.Concatenation
          { middle = UnboundedElements parameter; prefix = []; suffix = [] } ->
          List.map ~f:expression [parameter; Primitive "..."]
      | Concatenation concatenation -> concatenation_to_expressions concatenation
      | Concrete parameters -> List.map ~f:expression parameters
    in
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
        | None -> base_callable)
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
                   { base = expression enumeration_type; attribute = member_name; special = false })
        in
        get_item_call "typing_extensions.Literal" [Node.create ~location literal]
    | NoneType -> Expression.Constant Constant.NoneLiteral
    | Parametric { name; parameters } ->
        let parameters =
          let expression_of_parameter = function
            | Record.Parameter.Single single -> expression single
            | CallableParameters parameters -> callable_parameters_expression parameters
            | Unpacked unpackable ->
                Record.OrderedTypes.Concatenation.unpackable_to_expression
                  ~expression
                  ~location
                  unpackable
          in
          match parameters with
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
    | RecursiveType { name; _ } -> create_name name
    | Top -> create_name "$unknown"
    | Tuple (Concrete []) ->
        get_item_call "typing.Tuple" [Node.create ~location (Expression.Tuple [])]
    | Tuple ordered_type -> get_item_call "typing.Tuple" (convert_ordered_type ordered_type)
    | TypeOperation (Compose ordered_type) ->
        get_item_call "pyre_extensions.Compose" (convert_ordered_type ordered_type)
    | Union [NoneType; parameter]
    | Union [parameter; NoneType] ->
        get_item_call "typing.Optional" [expression parameter]
    | Union parameters -> get_item_call "typing.Union" (List.map ~f:expression parameters)
    | Variable { variable; _ } -> create_name variable
    | IntExpression (Data polynomial) when Polynomial.is_base_case polynomial ->
        convert_annotation (polynomial_to_type polynomial)
    | IntExpression (Data polynomial) ->
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
        let convert_monomial { Monomial.constant_factor; variables } =
          let constant_factor = convert_annotation (Literal (Integer constant_factor)) in
          let variables =
            List.map variables ~f:(fun { variable; degree } ->
                let variable =
                  match variable with
                  | Monomial.Variable variable -> convert_annotation (Variable variable)
                  | Monomial.Operation (Divide (dividend, quotient)) ->
                      convert_annotation
                        (Parametric
                           {
                             name = "pyre_extensions.Divide";
                             parameters =
                               [
                                 Single (IntExpression.create dividend);
                                 Single (IntExpression.create quotient);
                               ];
                           })
                  | Monomial.Operation (Product unpackable) ->
                      get_item_call
                        "pyre_extensions.Product"
                        [
                          Record.OrderedTypes.Concatenation.unpackable_to_expression
                            ~expression
                            ~location
                            unpackable;
                        ]
                in
                if degree = 1 then
                  variable
                else
                  let arguments =
                    List.init degree ~f:(fun _ -> 0) |> List.map ~f:(fun _ -> variable)
                  in
                  convert_int_expression ~operator:"Multiply" arguments)
          in
          if List.length variables = 1 then
            constant_factor
          else
            convert_int_expression ~operator:"Multiply" (constant_factor :: variables)
        in
        convert_int_expression ~operator:"Add" (List.map polynomial ~f:convert_monomial)
  in
  let value =
    match annotation with
    | Primitive "..." -> Expression.Constant Constant.Ellipsis
    | _ -> convert_annotation annotation
  in
  Node.create_with_default_location value


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
          | Broadcast broadcast -> Broadcast (visit_broadcast broadcast)
        and visit_ordered_types ordered_types =
          match ordered_types with
          | Record.OrderedTypes.Concrete concretes ->
              Record.OrderedTypes.Concrete (visit_all concretes)
          | Concatenation concatenation -> Concatenation (visit_concatenation concatenation)
        and visit_broadcast = function
          | ConcreteAgainstConcrete { left; right } ->
              ConcreteAgainstConcrete { left = visit_all left; right = visit_all right }
          | ConcreteAgainstConcatenation { concrete; concatenation } ->
              ConcreteAgainstConcatenation
                { concrete = visit_all concrete; concatenation = visit_concatenation concatenation }
          | ConcatenationAgainstConcatenation { left_concatenation; right_concatenation } ->
              ConcatenationAgainstConcatenation
                {
                  left_concatenation = visit_concatenation left_concatenation;
                  right_concatenation = visit_concatenation right_concatenation;
                }
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
                RecordParameter.Variable (Concatenation (visit_concatenation concatenation))
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
              | Record.Parameter.Single single ->
                  Record.Parameter.Single (visit_annotation single ~state)
              | CallableParameters parameters -> CallableParameters (visit_parameters parameters)
              | Unpacked (Variadic _) as unpacked -> unpacked
              | Unpacked (UnboundedElements annotation) ->
                  Unpacked (UnboundedElements (visit_annotation annotation ~state))
              | Unpacked (Broadcast broadcast) -> Unpacked (Broadcast (visit_broadcast broadcast))
            in
            Parametric { name; parameters = List.map parameters ~f:visit }
        | RecursiveType { name; body } ->
            RecursiveType { name; body = visit_annotation ~state body }
        | Tuple ordered_type -> Tuple (visit_ordered_types ordered_type)
        | TypeOperation (Compose ordered_type) ->
            TypeOperation (Compose (visit_ordered_types ordered_type))
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
        | IntExpression (Data polynomial) ->
            IntExpression.create (IntExpression.visit ~visit_unpackable polynomial)
        | ParameterVariadicComponent _
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


let contains_callable annotation = exists annotation ~predicate:is_callable

let contains_any annotation = exists annotation ~predicate:is_any

let contains_unknown annotation = exists annotation ~predicate:is_top

let contains_undefined annotation = exists annotation ~predicate:is_unbound

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
                  String.equal sanitized "__"
                  || String.is_prefix sanitized ~prefix:"__"
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


    (* Match parameters from two parameter lists `left_parameters` and `right_parameters`.
     *
     * This returns a list of [`Both | `Left | `Right], where:
     * `Both (left, right)` means the parameter `left` from `left_parameters` matches the parameter `right` from `right_parameters`.
     * `Left left` means the parameter `left` from `left_parameters` does not match any parameter in `right_parameters`.
     * `Right right` means the parameter `right` from `right_parameters` does not match any parameters in `left_parameters`.
     *
     * All parameters from `left_parameters` and `right_parameters` only appear once in the result.
     *)
    let zip left_parameters right_parameters =
      let find_positional_parameter index parameters = List.nth parameters index in
      let find_named_parameter name =
        let equal_name parameter =
          match parameter with
          | KeywordOnly { name = parameter_name; _ }
          | Named { name = parameter_name; _ }
            when Identifier.equal parameter_name name ->
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
      let process_right right_parameter =
        match find_matching_parameter left_parameters right_parameter with
        | Some _ -> None
        | None -> Some (`Right right_parameter)
      in
      List.map ~f:process_left left_parameters @ List.filter_map ~f:process_right right_parameters
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
      Parameter.PositionalOnly { index = 0; annotation; default = false }
    in
    let correct_indices index = function
      | Parameter.PositionalOnly anonymous -> Parameter.PositionalOnly { anonymous with index }
      | parameter -> parameter
    in
    let head = List.map head ~f:make_anonymous in
    List.mapi ~f:correct_indices (head @ tail)


  let rec resolve_getitem_callee ~resolve_aliases callee =
    let resolve_getitem_callee = resolve_getitem_callee ~resolve_aliases in
    match callee with
    | Expression.Name
        (Name.Attribute
          {
            base =
              {
                Node.value =
                  Name
                    (Name.Attribute
                      {
                        base = { Node.value = Name (Name.Identifier "typing"); _ };
                        attribute = "Callable";
                        _;
                      });
                _;
              };
            attribute = "__getitem__";
            _;
          }) ->
        callee
    | Expression.Name
        (Name.Attribute
          ({ base = { Node.value = Name name; location } as base; attribute = "__getitem__"; _ } as
          attribute)) ->
        Ast.Expression.name_to_reference name
        >>| Reference.show
        >>| (fun name ->
              match resolve_aliases (Primitive name) with
              | Primitive "typing.Callable"
              | Callable
                  {
                    implementation = { parameters = Undefined; annotation = Any };
                    overloads = [];
                    _;
                  } ->
                  Expression.Name
                    (Name.Attribute
                       {
                         attribute with
                         base =
                           {
                             base with
                             Node.value =
                               Name
                                 (Name.Attribute
                                    {
                                      base =
                                        { Node.value = Name (Name.Identifier "typing"); location };
                                      attribute = "Callable";
                                      special = false;
                                    });
                           };
                       })
              | _ -> callee)
        |> Option.value ~default:callee
    | Name (Name.Attribute ({ base = { Node.value = base; location }; _ } as attribute)) ->
        Name
          (Name.Attribute
             { attribute with base = { Node.location; value = resolve_getitem_callee base } })
    | Call ({ callee = { Node.value = callee; location }; _ } as call) ->
        Call { call with callee = { Node.value = resolve_getitem_callee callee; location } }
    | _ -> callee
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
    "TSelf", variable "_PathLike";
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
  | Expression.Constant Constant.NoneLiteral -> Some none
  | Expression.Name (Identifier "str") -> Some (Literal (String AnyLiteral))
  | _ -> None


type alias =
  | TypeAlias of t
  | VariableAlias of t Record.Variable.record
[@@deriving compare, eq, sexp, show, hash]

let empty_aliases ?replace_unbound_parameters_with_any:_ _ = None

module RecursiveType = struct
  include Record.RecursiveType

  let contains_recursive_type_with_name ~name =
    exists ~predicate:(function
        | RecursiveType { name = inner_name; _ } -> Identifier.equal inner_name name
        | _ -> false)


  let create ~name ~body =
    if contains_recursive_type_with_name ~name body then
      failwith "Body of recursive type contains a recursive type with the same name";
    RecursiveType { name; body }


  let is_recursive_alias_reference ~alias_name =
    exists ~predicate:(function
        | Primitive name
        | Parametric { name; _ }
          when Identifier.equal name alias_name ->
            true
        | _ -> false)


  (* We assume that recursive alias names are unique. So, we don't need to worry about accidentally
     renaming a nested recursive type here. *)
  let unfold_recursive_type { name; body } =
    instantiate
      ~constraints:(function
        | Primitive primitive_name when Identifier.equal primitive_name name ->
            Some (RecursiveType { name; body })
        | _ -> None)
      body


  let body_with_replaced_name ~new_name { name; body } =
    instantiate
      ~constraints:(function
        | Primitive primitive_name when Identifier.equal primitive_name name ->
            Some (Primitive new_name)
        | _ -> None)
      body


  let replace_references_with_recursive_type ~recursive_type:{ name; body } =
    instantiate ~constraints:(function
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
  include Record.OrderedTypes

  type t = type_t record [@@deriving compare, eq, sexp, show, hash]

  type ordered_types_t = t

  let pp_concise = pp_concise ~pp_type

  let union_upper_bound = function
    | Concrete concretes -> union concretes
    | Concatenation { prefix; middle = UnboundedElements unbounded; suffix } ->
        union (unbounded :: (prefix @ suffix))
    | Concatenation _ -> object_primitive


  let concatenation_from_parameters parameters =
    let unpacked_element_index index parameter =
      match parameter with
      | Parameter.Unpacked _ -> Some index
      | _ -> None
    in
    match List.filter_mapi parameters ~f:unpacked_element_index with
    | [unpacked_index] -> (
        let prefix, rest = List.split_n parameters unpacked_index in
        match rest with
        | Unpacked middle :: suffix -> (
            match Parameter.all_singles prefix, Parameter.all_singles suffix with
            | Some prefix, Some suffix -> Some (Concatenation { prefix; middle; suffix })
            | _ -> None)
        | _ -> None)
    | _ -> None


  let to_parameters = function
    | Concrete elements -> List.map elements ~f:(fun element -> Parameter.Single element)
    | Concatenation { prefix; middle = unpacked; suffix } ->
        List.map prefix ~f:(fun element -> Parameter.Single element)
        @ [Parameter.Unpacked unpacked]
        @ List.map suffix ~f:(fun element -> Parameter.Single element)


  let to_starred_annotation_expression ~expression = function
    | { Concatenation.prefix = []; middle; suffix = [] } ->
        Concatenation.unpackable_to_expression ~expression ~location:Location.any middle
    | concatenation ->
        parametric
          Record.OrderedTypes.unpack_public_name
          (to_parameters (Concatenation concatenation))
        |> expression


  let concatenation_from_annotations ~variable_aliases annotations =
    let unpacked_element_index index = function
      | Parametric { name; _ } when Identifier.equal name Record.OrderedTypes.unpack_public_name ->
          Some index
      | _ -> None
    in
    match List.filter_mapi annotations ~f:unpacked_element_index with
    | [unpacked_index] -> (
        let prefix, rest = List.split_n annotations unpacked_index in
        match rest with
        | middle :: suffix -> (
            match middle with
            | Parametric { name; parameters = [Single (Primitive variable_name)] }
              when Identifier.equal name Record.OrderedTypes.unpack_public_name -> (
                variable_aliases variable_name
                >>= function
                | Record.Variable.TupleVariadic variadic ->
                    Some (Concatenation.create ~prefix ~suffix variadic)
                | _ -> None)
            | Parametric
                {
                  name;
                  parameters =
                    [
                      Single
                        (Tuple
                          (Concatenation { prefix = inner_prefix; middle; suffix = inner_suffix }));
                    ];
                }
              when Identifier.equal name Record.OrderedTypes.unpack_public_name ->
                Some { prefix = prefix @ inner_prefix; middle; suffix = inner_suffix @ suffix }
            | _ -> None)
        | _ -> None)
    | _ -> None


  let concatenation_from_unpack_expression ~parse_annotation = function
    | {
        Node.value =
          Expression.Call
            {
              callee =
                { Node.value = Name (Name.Attribute { base; attribute = "__getitem__"; _ }); _ };
              _;
            };
        _;
      } as annotation
      when name_is ~name:Record.OrderedTypes.unpack_public_name base -> (
        let location = Location.any in
        let wrapped_in_tuple =
          get_item_call ~location "typing.Tuple" [annotation] |> Node.create ~location
        in
        match parse_annotation wrapped_in_tuple with
        | Tuple (Concatenation concatenation) -> Some concatenation
        | _ -> None)
    | _ -> None


  type 'annotation broadcasted_dimension =
    | Ok of 'annotation
    | VariableDimension
    | BroadcastMismatch

  let broadcast left_type right_type =
    let broadcast_concrete_dimensions left_dimensions right_dimensions =
      let pad_with_ones ~length list =
        List.init (max 0 (length - List.length list)) ~f:(fun _ -> Literal (Integer 1)) @ list
      in
      let simplify_type input =
        match input with
        | Any
        | Primitive "int"
        | Literal (Integer _)
        | Variable { constraints = Record.Variable.Bound (Primitive "int"); _ } ->
            input
        | _ -> Bottom
      in
      let broadcast_concrete_dimensions left_dimension right_dimension =
        match simplify_type left_dimension, simplify_type right_dimension with
        | Bottom, _
        | _, Bottom ->
            BroadcastMismatch
        | Any, _
        | _, Any ->
            Ok Any
        | Primitive "int", _
        | _, Primitive "int" ->
            Ok (Primitive "int")
        | Literal (Integer 1), _ -> Ok right_dimension
        | _, Literal (Integer 1) -> Ok left_dimension
        | Literal (Integer i), Literal (Integer j) when i = j -> Ok left_dimension
        | Variable left_variable, Variable right_variable
          when [%equal: type_t Record.Variable.RecordUnary.record] left_variable right_variable ->
            Ok (Variable left_variable)
        | Variable _, _
        | _, Variable _ ->
            VariableDimension
        | _ -> BroadcastMismatch
      in
      let length = max (List.length left_dimensions) (List.length right_dimensions) in
      let broadcast_error () =
        Parametric
          {
            name = "pyre_extensions.BroadcastError";
            parameters =
              (if [%compare: type_t] left_type right_type < 0 then
                 [Parameter.Single left_type; Parameter.Single right_type]
              else
                [Parameter.Single right_type; Parameter.Single left_type]);
          }
      in
      match
        List.map2
          (pad_with_ones ~length left_dimensions)
          (pad_with_ones ~length right_dimensions)
          ~f:broadcast_concrete_dimensions
      with
      | Ok result -> (
          let partitioned =
            List.partition3_map result ~f:(function
                | Ok annotation -> `Fst annotation
                | VariableDimension -> `Snd ()
                | BroadcastMismatch -> `Trd ())
          in
          match partitioned with
          | broadcasted, [], [] -> Tuple (Concrete broadcasted)
          | _, _ :: _, [] ->
              (* There is at least one dimension that contains a variable. Preserve the dimensions
                 as they are. We will broadcast when instantiating the variable with concrete
                 literals. *)
              Tuple
                (Concatenation
                   (Concatenation.create_from_concrete_against_concrete
                      ~prefix:[]
                      ~suffix:[]
                      ~compare_t:compare_type_t
                      ~left:left_dimensions
                      ~right:right_dimensions))
          | _ -> broadcast_error ())
      | Unequal_lengths -> broadcast_error ()
    in
    match left_type, right_type with
    | Any, _
    | _, Any ->
        Any
    | Parametric { name = "pyre_extensions.BroadcastError"; _ }, _ -> left_type
    | _, Parametric { name = "pyre_extensions.BroadcastError"; _ } -> right_type
    | Tuple (Concrete left_dimensions), Tuple (Concrete right_dimensions) ->
        broadcast_concrete_dimensions left_dimensions right_dimensions
    | ( Tuple (Concrete concrete),
        Tuple
          (Concatenation { prefix = []; middle = UnboundedElements (Primitive "int"); suffix = [] })
      )
    | ( Tuple
          (Concatenation { prefix = []; middle = UnboundedElements (Primitive "int"); suffix = [] }),
        Tuple (Concrete concrete) ) ->
        let is_numeric = function
          | Literal (Integer _)
          | Primitive "int"
          | Variable { constraints = Record.Variable.Bound (Primitive "int"); _ } ->
              true
          | _ -> false
        in
        if List.for_all ~f:is_numeric concrete then
          Tuple
            (Concatenation
               { prefix = []; middle = UnboundedElements (Primitive "int"); suffix = [] })
        else
          Bottom
    | ( Tuple (Concrete _),
        Tuple (Concatenation { prefix = []; middle = UnboundedElements Any; suffix = [] }) )
    | ( Tuple (Concatenation { prefix = []; middle = UnboundedElements Any; suffix = [] }),
        Tuple (Concrete _) ) ->
        Tuple (Concatenation { prefix = []; middle = UnboundedElements Any; suffix = [] })
    | Tuple (Concrete concrete), Tuple (Concatenation concatenation)
    | Tuple (Concatenation concatenation), Tuple (Concrete concrete) ->
        Tuple
          (Concatenation
             (Concatenation.create_from_concrete_against_concatenation
                ~prefix:[]
                ~suffix:[]
                ~concrete
                ~concatenation))
    | Tuple (Concatenation left_concatenation), Tuple (Concatenation right_concatenation)
      when [%eq: type_t Concatenation.t] left_concatenation right_concatenation ->
        Tuple (Concatenation left_concatenation)
    | Tuple (Concatenation left_concatenation), Tuple (Concatenation right_concatenation) ->
        Tuple
          (Concatenation
             (Concatenation.create_from_concatenation_against_concatenation
                ~prefix:[]
                ~suffix:[]
                ~compare_t:compare_type_t
                left_concatenation
                right_concatenation))
    | _ -> Bottom


  let expand_in_concatenation ~prefix ~suffix = function
    | Concrete dimensions -> Tuple (Concrete (prefix @ dimensions @ suffix))
    | Concatenation { prefix = new_prefix; middle; suffix = new_suffix } ->
        Tuple (Concatenation { prefix = prefix @ new_prefix; middle; suffix = new_suffix @ suffix })


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
          | Concrete items -> Some (union items)
          | Concatenation { middle = UnboundedElements item_type; prefix; suffix } ->
              union (item_type :: (prefix @ suffix)) |> Option.some
          | Concatenation { middle = Variadic _ | Broadcast _; _ } -> None
        in
        List.map middle_items ~f:extract_common_type
        |> Option.all
        >>| union
        >>| (fun item_type ->
              concrete_tuples_prefix
              @ [Concatenation (Concatenation.create_from_unbounded_element item_type)]
              @ concrete_tuples_suffix)
        >>= concatenate_ordered_types
    | _ -> concatenate_ordered_types ordered_types
end

module TypeOperation = struct
  include Record.TypeOperation

  module Compose = struct
    include Record.TypeOperation.Compose

    type t = type_t Record.TypeOperation.Compose.t

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

  type t = type_t Record.TypeOperation.record
end

let parameters_from_unpacked_annotation annotation ~variable_aliases =
  let open Record.OrderedTypes.Concatenation in
  let unpacked_variadic_to_parameter = function
    | Primitive variable_name -> (
        match variable_aliases variable_name with
        | Some (Record.Variable.TupleVariadic variadic) ->
            Some (Parameter.Unpacked (Variadic variadic))
        | _ -> None)
    | _ -> None
  in
  match annotation with
  | Parametric { name; parameters = [Single (Primitive _ as element)] }
    when Identifier.equal name Record.OrderedTypes.unpack_public_name ->
      unpacked_variadic_to_parameter element >>| fun parameter -> [parameter]
  | Parametric { name; parameters = [Single (Tuple ordered_type)] }
    when Identifier.equal name Record.OrderedTypes.unpack_public_name ->
      OrderedTypes.to_parameters ordered_type |> Option.some
  | _ -> None


let rec create_logic ~resolve_aliases ~variable_aliases { Node.value = expression; _ } =
  let substitute_parameter_variadic = function
    | Primitive name -> (
        match variable_aliases name with
        | Some (Record.Variable.ParameterVariadic variable) ->
            Some { Record.Callable.variable; head = [] }
        | _ -> None)
    | Parametric { name; parameters }
      when Identifier.equal name Record.OrderedTypes.concatenate_public_name -> (
        match List.rev parameters with
        | Parameter.CallableParameters (ParameterVariadicTypeVariable { variable; head = [] })
          :: reversed_head ->
            Parameter.all_singles reversed_head
            >>| List.rev
            >>| fun head -> { Record.Callable.variable; head }
        | _ -> None)
    | _ -> None
  in
  let extract_parameter ~create_logic index parameter =
    match Node.value parameter with
    | Expression.Call { callee = { Node.value = Name (Name.Identifier name); _ }; arguments } -> (
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
            let callable_parameter =
              match tail with
              | head :: _ ->
                  let elements =
                    List.map tail ~f:(fun annotation ->
                        create_logic (Node.create_with_default_location annotation))
                  in
                  OrderedTypes.concatenation_from_annotations ~variable_aliases elements
                  >>| (fun concatenation -> CallableParameter.Concatenation concatenation)
                  |> Option.value
                       ~default:
                         (CallableParameter.Concrete
                            (create_logic (Node.create_with_default_location head)))
              | _ -> CallableParameter.Concrete Top
            in
            CallableParameter.Variable callable_parameter
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
  let create_ordered_type_from_parameters parameters =
    match Parameter.all_singles parameters with
    | Some [annotation; Primitive "..."] ->
        Some (OrderedTypes.create_unbounded_concatenation annotation)
    | Some singles -> Some (Concrete singles)
    | None -> OrderedTypes.concatenation_from_parameters parameters
  in
  let create_int_expression_from_arguments arguments ~operation =
    match arguments with
    | []
    | [_] ->
        Top
    | _ ->
        List.map arguments ~f:(create_logic ~resolve_aliases ~variable_aliases)
        |> List.map ~f:IntExpression.type_to_int_expression
        |> Option.all
        >>| IntExpression.create_int_expression_from_types ~operation
        |> Option.value ~default:Top
  in
  let result =
    let create_logic = create_logic ~resolve_aliases ~variable_aliases in
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
                make_signature
                  ~parameters:(Defined (List.mapi ~f:(extract_parameter ~create_logic) parameters))
            | _ -> (
                let parsed = create_logic parameters in
                match substitute_parameter_variadic parsed with
                | Some variable ->
                    make_signature ~parameters:(ParameterVariadicTypeVariable variable)
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
          let element_to_parameters = function
            | { Node.value = Expression.List elements; _ } ->
                [
                  Record.Parameter.CallableParameters
                    (Defined (List.mapi ~f:(extract_parameter ~create_logic) elements));
                ]
            | element -> (
                let parsed = create_logic element in
                match parameters_from_unpacked_annotation ~variable_aliases parsed with
                | Some parameters -> parameters
                | None -> (
                    match substitute_parameter_variadic parsed with
                    | Some variable ->
                        [
                          Record.Parameter.CallableParameters
                            (ParameterVariadicTypeVariable variable);
                        ]
                    | _ -> [Record.Parameter.Single parsed]))
          in
          match argument with
          | { Node.value = Expression.Tuple elements; _ } ->
              List.concat_map elements ~f:element_to_parameters
          | element -> element_to_parameters element
        in
        Parametric { name; parameters } |> resolve_aliases
      in
      match create_logic base, Node.value base with
      | Primitive name, _ -> parametric name
      | _, Name _ -> parametric (Expression.show base)
      | _ -> Top
    in
    match expression with
    | Call
        {
          callee;
          arguments =
            {
              Call.Argument.value =
                { Node.value = Constant (Constant.String { StringLiteral.value; _ }); _ };
              _;
            }
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
                value = { Node.value = Constant Constant.True; _ };
              }
              when String.equal (Identifier.sanitized name) "covariant" ->
                Some Record.Variable.Covariant
            | {
                Call.Argument.name = Some { Node.value = name; _ };
                value = { Node.value = Constant Constant.True; _ };
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
            [
              {
                Call.Argument.value =
                  { Node.value = Constant (Constant.String { StringLiteral.value; _ }); _ };
                _;
              };
            ];
        }
      when name_is ~name:"typing_extensions.IntVar" callee ->
        variable value ~constraints:LiteralIntegers
    | Call
        {
          callee =
            { Node.value = Name (Name.Attribute { attribute = "__getitem__"; _ }); _ } as callee;
          arguments =
            [
              {
                Call.Argument.name = None;
                value = { Node.value = Expression.Tuple arguments; _ };
                _;
              };
            ];
        }
      when name_is ~name:"pyre_extensions.Broadcast.__getitem__" callee ->
        (match List.map ~f:create_logic arguments with
        | [left_type; right_type] -> OrderedTypes.broadcast left_type right_type
        | _ -> Bottom)
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
      when name_is ~name:"pyre_extensions.Add.__getitem__" callee ->
        let created_type = create_int_expression_from_arguments arguments ~operation:`Add in
        (match created_type with
        | Top -> create_parametric ~base ~argument
        | _ -> created_type)
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
        (match created_type with
        | Top -> create_parametric ~base ~argument
        | _ -> created_type)
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
      when name_is ~name:"pyre_extensions.Subtract.__getitem__" callee ->
        let created_type = create_int_expression_from_arguments arguments ~operation:`Subtract in
        (match created_type with
        | Top -> create_parametric ~base ~argument
        | _ -> created_type)
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
      when name_is ~name:"pyre_extensions.Divide.__getitem__" callee ->
        let created_type = create_int_expression_from_arguments arguments ~operation:`Divide in
        (match created_type with
        | Top -> create_parametric ~base ~argument
        | _ -> created_type)
        |> resolve_aliases
    | Call
        {
          callee =
            {
              Node.value =
                Expression.Name
                  (Name.Attribute
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
                                          ( Name.Identifier "typing"
                                          | Name.Identifier "typing_extensions" );
                                      _;
                                    };
                                  attribute = "Literal";
                                  _;
                                });
                          _;
                        };
                      attribute = "__getitem__";
                      _;
                    });
              _;
            };
          arguments;
        } ->
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
    | Call ({ callee = { Node.value = callee; location }; arguments } as callee_expression) -> (
        let resolved_callee = Callable.resolve_getitem_callee ~resolve_aliases callee in
        match callee, arguments with
        | _ when is_typing_callable resolved_callee ->
            parse_callable
              (Call { callee_expression with callee = { Node.value = resolved_callee; location } })
        | ( Expression.Name (Name.Attribute { base; attribute = "__getitem__"; _ }),
            [{ Call.Argument.name = None; value = argument; _ }] ) ->
            (* TODO(T84854853): Add back support for `Length` and `Product`. *)
            create_parametric ~base ~argument
        | _ -> Top)
    | Constant Constant.NoneLiteral -> none
    | Name (Name.Identifier identifier) ->
        let sanitized = Identifier.sanitized identifier in
        Primitive sanitized |> resolve_aliases
    | Name (Name.Attribute { base; attribute; _ }) -> (
        let attribute = Identifier.sanitized attribute in
        match create_logic base with
        | Primitive primitive -> Primitive (primitive ^ "." ^ attribute) |> resolve_aliases
        | _ -> Primitive (Expression.show base ^ "." ^ attribute))
    | Constant Constant.Ellipsis -> Primitive "..."
    | Constant (Constant.String { StringLiteral.value; _ }) ->
        let expression =
          try
            let parsed =
              Parser.parse_exn [value]
              |> Source.create
              |> Preprocessing.preprocess
              |> Source.statements
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
      | None -> result)
  | Parametric { name = "typing.Tuple"; parameters }
  | Parametric { name = "tuple"; parameters } ->
      Option.value
        ~default:Top
        (create_ordered_type_from_parameters parameters >>| fun result -> Tuple result)
  | Parametric { name = "pyre_extensions.Compose"; parameters } ->
      Option.value
        ~default:Top
        (create_ordered_type_from_parameters parameters >>= TypeOperation.Compose.create)
  | Parametric { name = "pyre_extensions.Product"; parameters } ->
      create_ordered_type_from_parameters parameters
      >>= IntExpression.create_product_from_ordered_type
      |> Option.value ~default:Top
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
          | _ -> result)
      | _, None -> result)
  | Union elements -> union elements
  | Callable ({ implementation; overloads; _ } as callable) ->
      let collect_unpacked_parameters_if_any ({ parameters; _ } as overload) =
        match parameters with
        | Defined parameters ->
            let all_positional_only_parameters =
              List.map parameters ~f:(function
                  | PositionalOnly { annotation; _ } -> Some annotation
                  | _ -> None)
              |> Option.all
            in
            all_positional_only_parameters
            >>= OrderedTypes.concatenation_from_annotations ~variable_aliases
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


let final_value = function
  | Parametric
      { name = "typing.Final" | "typing_extensions.Final"; parameters = [Single parameter] } ->
      `Ok parameter
  | Primitive ("typing.Final" | "typing_extensions.Final") -> `NoParameter
  | _ -> `NotFinal


let contains_final annotation =
  let predicate annotation =
    match final_value annotation with
    | `Ok _
    | `NoParameter ->
        true
    | `NotFinal -> false
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


let typed_dictionary_class_name ~total =
  if total then
    "TypedDictionary"
  else
    "NonTotalTypedDictionary"


type elements_state = {
  elements: Primitive.t list;
  recursive_type_names: Primitive.t list;
}

let elements annotation =
  let module CollectorTransform = Transform.Make (struct
    type state = elements_state

    let visit_children_before _ _ =
      match annotation with
      | Literal _ -> false
      | _ -> true


    let visit_children_after = false

    let visit { elements = sofar; recursive_type_names } annotation =
      let elements, recursive_type_names =
        match annotation with
        | Annotated _ -> "typing.Annotated" :: sofar, recursive_type_names
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
        | RecursiveType { name; _ } -> sofar, name :: recursive_type_names
        | ParameterVariadicComponent _
        | Bottom
        | Any
        | Top
        | NoneType
        | Variable _
        | IntExpression _ ->
            sofar, recursive_type_names
      in
      {
        Transform.transformed_annotation = annotation;
        new_state = { elements; recursive_type_names };
      }
  end)
  in
  let { elements; recursive_type_names } =
    fst (CollectorTransform.visit { elements = []; recursive_type_names = [] } annotation)
  in
  let name_set = Identifier.Set.of_list recursive_type_names in
  (* The recursive alias name is untracked, which would lead to spurious "Annotation is not defined"
     errors. So, filter out any references to it. consider the name as an "element". *)
  List.filter elements ~f:(fun element -> not (Identifier.Set.mem name_set element)) |> List.rev


let is_untyped = function
  | Any
  | Bottom
  | Top ->
      true
  | _ -> false


let is_partially_typed annotation = exists annotation ~predicate:is_untyped

let contains_variable = exists ~predicate:is_variable

let optional_value = function
  | Union [NoneType; annotation]
  | Union [annotation; NoneType] ->
      Some annotation
  | _ -> None


let awaitable_value = function
  | Parametric { name = "typing.Awaitable"; parameters = [Single parameter] } -> Some parameter
  | _ -> None


let coroutine_value = function
  | Parametric { name = "typing.Coroutine"; parameters = [_; _; Single parameter] } ->
      Some parameter
  | _ -> None


let typeguard_annotation = function
  | Parametric
      {
        name = "typing.TypeGuard" | "typing_extensions.TypeGuard";
        parameters = [Single guard_type];
      } ->
      Some guard_type
  | _ -> None


let parameters = function
  | Parametric { parameters; _ } -> Some parameters
  | _ -> None


let type_parameters_for_bounded_tuple_union = function
  | Union annotations ->
      let bounded_tuple_parameters = function
        | Tuple (Concrete parameters) -> Some parameters
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


(* Weaken specific literal to arbitrary literal before weakening to `str`. *)
let weaken_to_arbitrary_literal_if_possible = function
  | Literal (Integer _) -> integer
  | Literal (String (LiteralValue _)) -> Literal (String AnyLiteral)
  | Literal (String AnyLiteral) -> string
  | Literal (Bytes _) -> bytes
  | Literal (Boolean _) -> bool
  | Literal (EnumerationMember { enumeration_type; _ }) -> enumeration_type
  | annotation -> annotation


let split annotation =
  let open Record.Parameter in
  match annotation with
  | Union [NoneType; parameter]
  | Union [parameter; NoneType] ->
      Primitive "typing.Optional", [Single parameter]
  | Parametric { name; parameters } -> Primitive name, parameters
  | Tuple tuple ->
      (* We want to return a type `typing.tuple[X]` where X is the type that would make `given_tuple
         <: Tuple[X, ...]`. *)
      let parameters =
        match tuple with
        | Concatenation { middle = UnboundedElements parameter; prefix = []; suffix = [] } ->
            [Single parameter]
        | ordered_type -> [Single (OrderedTypes.union_upper_bound ordered_type)]
      in
      Primitive "tuple", parameters
  | Literal _ as literal -> weaken_literals literal, []
  | IntExpression _ -> integer, []
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

  type unary_domain = type_t [@@deriving compare, eq, sexp, show, hash]

  type parameter_variadic_t = type_t Record.Variable.RecordVariadic.RecordParameters.record
  [@@deriving compare, eq, sexp, show, hash]

  type parameter_variadic_domain = Callable.parameters [@@deriving compare, eq, sexp, show, hash]

  type tuple_variadic_t = type_t Record.Variable.RecordVariadic.Tuple.record
  [@@deriving compare, eq, sexp, show, hash]

  type tuple_variadic_domain = type_t OrderedTypes.record [@@deriving compare, eq, sexp, show, hash]

  type pair =
    | UnaryPair of unary_t * unary_domain
    | ParameterVariadicPair of parameter_variadic_t * parameter_variadic_domain
    | TupleVariadicPair of tuple_variadic_t * tuple_variadic_domain
  [@@deriving compare, eq, sexp, show, hash]

  type t = type_t Record.Variable.record [@@deriving compare, eq, sexp, show, hash]

  type variable_t = t

  module type VariableKind = sig
    type t [@@deriving compare, eq, sexp, show, hash]

    module Map : Core.Map.S with type Key.t = t

    val is_free : t -> bool

    val is_escaped_and_free : t -> bool

    val mark_as_bound : t -> t

    val mark_as_escaped : t -> t

    val mark_as_free : t -> t

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
        :  create_type:
             (aliases:(?replace_unbound_parameters_with_any:bool -> Primitive.t -> alias option) ->
             Expression.t ->
             type_t) ->
        variable_parameter_annotation:Expression.t ->
        keywords_parameter_annotation:Expression.t ->
        aliases:(?replace_unbound_parameters_with_any:bool -> Primitive.t -> alias option) ->
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

    module Tuple : sig
      include VariableKind with type t = tuple_variadic_t and type domain = tuple_variadic_domain

      val name : t -> Identifier.t

      val create : string -> t

      val synthetic_class_name_for_error : string
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

    module TupleVariadic :
      S with type t = tuple_variadic_t and type domain = type_t OrderedTypes.record
  end

  include module type of struct
    include Record.Variable
  end

  module Set : Core.Set.S with type Elt.t = t

  type variable_zip_result = {
    variable_pair: pair;
    received_parameter: Parameter.t;
  }
  [@@deriving compare, eq, sexp, show, hash]

  val pp_concise : Format.formatter -> t -> unit

  val parse_declaration : Expression.t -> target:Reference.t -> t option

  val dequalify : Reference.t Reference.Map.t -> t -> t

  val namespace : t -> namespace:Namespace.t -> t

  val mark_all_variables_as_bound : ?specific:t list -> type_t -> type_t

  val mark_all_variables_as_free : ?specific:t list -> type_t -> type_t

  val mark_as_bound : t -> t

  val namespace_all_free_variables : type_t -> namespace:Namespace.t -> type_t

  val all_free_variables : type_t -> t list

  val all_variables_are_resolved : type_t -> bool

  val mark_all_free_variables_as_escaped : ?specific:t list -> type_t -> type_t

  val collapse_all_escaped_variable_unions : type_t -> type_t

  val contains_escaped_free_variable : type_t -> bool

  val convert_all_escaped_free_variables_to_anys : type_t -> type_t

  val converge_all_variable_namespaces : type_t -> type_t

  val zip_variables_with_parameters : parameters:Parameter.t list -> t list -> pair list option

  val zip_variables_with_parameters_including_mismatches
    :  parameters:Parameter.t list ->
    t list ->
    variable_zip_result list option

  val zip_variables_with_two_parameter_lists
    :  left_parameters:Parameter.t list ->
    right_parameters:Parameter.t list ->
    t list ->
    (pair * pair) list option

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

  type unary_domain = type_t [@@deriving compare, eq, sexp, show, hash]

  type parameter_variadic_t = type_t Record.Variable.RecordVariadic.RecordParameters.record
  [@@deriving compare, eq, sexp, show, hash]

  type parameter_variadic_domain = Callable.parameters [@@deriving compare, eq, sexp, show, hash]

  type tuple_variadic_t = type_t Record.Variable.RecordVariadic.Tuple.record
  [@@deriving compare, eq, sexp, show, hash]

  type tuple_variadic_domain = type_t OrderedTypes.record [@@deriving compare, eq, sexp, show, hash]

  type pair =
    | UnaryPair of unary_t * unary_domain
    | ParameterVariadicPair of parameter_variadic_t * parameter_variadic_domain
    | TupleVariadicPair of tuple_variadic_t * tuple_variadic_domain
  [@@deriving compare, eq, sexp, show, hash]

  module type VariableKind = sig
    type t [@@deriving compare, eq, sexp, show, hash]

    module Map : Core.Map.S with type Key.t = t

    val is_free : t -> bool

    val is_escaped_and_free : t -> bool

    val mark_as_bound : t -> t

    val mark_as_escaped : t -> t

    val mark_as_free : t -> t

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

    let mark_as_free variable = { variable with state = Free { escaped = false } }

    let rec local_collect = function
      | Variable variable -> [variable]
      | IntExpression (Data polynomial) ->
          let collect_from_variable = function
            | Monomial.Variable x -> [x]
            | Operation (Divide (dividend, quotient)) ->
                local_collect (IntExpression.create dividend)
                @ local_collect (IntExpression.create quotient)
            | Operation (Product _) -> []
          in
          IntExpression.collect ~collect_from_variable ~compare polynomial
      | _ -> []


    let rec local_replace replacement = function
      | Variable variable -> replacement variable
      | IntExpression (Data polynomial) ->
          let replace_variable variable =
            match replacement variable with
            | Some replaced_variable ->
                Some
                  (IntExpression.type_to_int_expression replaced_variable
                  |> Option.value ~default:Bottom)
            | _ -> None
          in
          local_replace_polynomial
            polynomial
            ~replace_variable
            ~replace_recursive:(local_replace replacement)
      | Tuple
          (Concatenation
            {
              prefix;
              middle =
                OrderedTypes.Concatenation.Broadcast (ConcreteAgainstConcrete { left; right });
              suffix;
            }) -> (
          match OrderedTypes.broadcast (Tuple (Concrete left)) (Tuple (Concrete right)) with
          | Tuple (Concrete concrete) -> Some (Tuple (Concrete (prefix @ concrete @ suffix)))
          | Parametric { name = "pyre_extensions.BroadcastError"; _ } as parametric ->
              Some parametric
          | _ -> None)
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

      let mark_as_free variable = { variable with state = Free { escaped = false } }

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
                  arguments =
                    [{ Call.Argument.value = { Node.value = Constant (Constant.String _); _ }; _ }];
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
                                  Node.value =
                                    Name (Name.Identifier ("typing" | "typing_extensions"));
                                  _;
                                };
                              attribute = "ParamSpec";
                              special = false;
                            });
                      _;
                    };
                  arguments =
                    [{ Call.Argument.value = { Node.value = Constant (Constant.String _); _ }; _ }];
                };
            _;
          } ->
            Some (create (Reference.show target))
        | _ -> None


      let parse_instance_annotation
          ~create_type
          ~variable_parameter_annotation
          ~keywords_parameter_annotation
          ~aliases
        =
        let get_variable name =
          match aliases ?replace_unbound_parameters_with_any:(Some false) name with
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
              ( create_type ~aliases variable_parameter_base,
                create_type ~aliases keywords_parameter_base )
            with
            | Primitive positionals_base, Primitive keywords_base
              when Identifier.equal positionals_base keywords_base ->
                get_variable positionals_base
            | _ -> None)
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

    module Tuple = struct
      include Record.Variable.RecordVariadic.Tuple

      type t = type_t record [@@deriving compare, eq, sexp, show, hash]

      type domain = type_t OrderedTypes.record [@@deriving compare, eq, sexp, show, hash]

      module Map = Core.Map.Make (struct
        type t = type_t record [@@deriving compare, sexp]
      end)

      let synthetic_class_name_for_error = "$synthetic_class_for_variadic_error"

      let any = OrderedTypes.create_unbounded_concatenation Any

      let name { name; _ } = name

      let self_reference variadic =
        OrderedTypes.Concatenation (OrderedTypes.Concatenation.create variadic)


      let pair variable value = TupleVariadicPair (variable, value)

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

      let rec local_collect annotation =
        let collect_unpackable = function
          | Record.OrderedTypes.Concatenation.Variadic variadic -> [variadic]
          | Broadcast (ConcreteAgainstConcatenation { concrete; concatenation }) ->
              local_collect (Tuple (Concrete concrete))
              @ local_collect (Tuple (Concatenation concatenation))
          | Broadcast
              (ConcatenationAgainstConcatenation { left_concatenation; right_concatenation }) ->
              local_collect (Tuple (Concatenation left_concatenation))
              @ local_collect (Tuple (Concatenation right_concatenation))
          | _ -> []
        in
        match annotation with
        | Parametric { parameters; _ } ->
            let extract = function
              | Parameter.Unpacked unpackable -> collect_unpackable unpackable
              | _ -> []
            in
            List.concat_map parameters ~f:extract
        | Tuple (Concatenation { middle = unpackable; _ }) -> collect_unpackable unpackable
        | Callable { implementation; overloads; _ } ->
            let extract = function
              | { parameters = Defined parameters; _ } ->
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
        | IntExpression (Data polynomial) ->
            let collect_from_variable = function
              | Monomial.Variable _ -> []
              | Operation (Divide (dividend, quotient)) ->
                  local_collect (IntExpression.create dividend)
                  @ local_collect (IntExpression.create quotient)
              | Operation (Product unpackable) -> collect_unpackable unpackable
            in
            IntExpression.collect ~collect_from_variable ~compare polynomial
        | _ -> []


      let rec local_replace replacement annotation =
        let map_tuple ~f = function
          | Tuple record -> f record
          | other -> other
        in
        let promote_to_tuple concatenation = Tuple (Concatenation concatenation) in
        let replace_unpackable = function
          | OrderedTypes.Concatenation.Broadcast
              (ConcreteAgainstConcatenation { concrete; concatenation }) ->
              promote_to_tuple concatenation
              |> local_replace replacement
              |> Option.value ~default:(promote_to_tuple concatenation)
              |> OrderedTypes.broadcast (Tuple (Concrete concrete))
              |> Option.some
          | Broadcast
              (ConcatenationAgainstConcatenation { left_concatenation; right_concatenation }) ->
              let left_record =
                promote_to_tuple left_concatenation
                |> local_replace replacement
                |> Option.value ~default:(promote_to_tuple left_concatenation)
              in
              let right_record =
                promote_to_tuple right_concatenation
                |> local_replace replacement
                |> Option.value ~default:(promote_to_tuple right_concatenation)
              in
              Some (OrderedTypes.broadcast left_record right_record)
          | Variadic variadic -> replacement variadic >>| fun result -> Tuple result
          | _ -> None
        in
        match annotation with
        | Parametric ({ parameters; _ } as parametric) ->
            let replace parameter =
              let replaced =
                match parameter with
                | Parameter.Unpacked unpackable -> (
                    replace_unpackable unpackable
                    >>| function
                    | Tuple record -> OrderedTypes.to_parameters record
                    | other -> [Parameter.Single other])
                | _ -> None
              in
              Option.value ~default:[parameter] replaced
            in
            let parameters = List.concat_map parameters ~f:replace in
            let default = Parametric { parametric with parameters } |> Option.some in
            let extract_broadcast_error = function
              | Parameter.Single
                  (Parametric { name = "pyre_extensions.BroadcastError"; _ } as parametric) ->
                  Some parametric
              | _ -> None
            in
            List.find_map ~f:extract_broadcast_error parameters
            |> fun result -> Option.first_some result default
        | Tuple (Concatenation { prefix; middle = unpackable; suffix }) ->
            replace_unpackable unpackable
            >>| map_tuple ~f:(OrderedTypes.expand_in_concatenation ~prefix ~suffix)
        | Callable callable -> (
            let replace_variadic parameters_so_far parameters =
              let expanded_parameters =
                match parameters with
                | Callable.Parameter.Variable
                    (Concatenation ({ prefix; middle = unpackable; suffix } as concatenation)) ->
                    let encode_ordered_types_into_parameters = function
                      | OrderedTypes.Concrete concretes ->
                          let start_index = List.length parameters_so_far in
                          let make_anonymous_parameter index annotation =
                            Callable.Parameter.PositionalOnly
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
                              Callable.Parameter.PositionalOnly
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
                         ~default:(Ok [Callable.Parameter.Variable (Concatenation concatenation)])
                | parameter -> Ok [parameter]
              in
              expanded_parameters
              |> Result.map ~f:(fun result -> List.rev_append result parameters_so_far)
            in
            let map_defined = function
              | Defined parameters ->
                  Result.map
                    ~f:(fun result -> Defined (List.rev result))
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
        | IntExpression (Data polynomial) ->
            IntExpression.replace_variadic ~replace_unpackable polynomial |> Option.some
        | _ -> None


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
                           attribute = "TypeVarTuple";
                           special = false;
                         });
                   _;
                 };
               arguments =
                 [{ Call.Argument.value = { Node.value = Constant (Constant.String _); _ }; _ }];
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
    module TupleVariadic = Make (Variadic.Tuple)
  end

  let pp_type = pp

  type t = type_t Record.Variable.record [@@deriving compare, eq, sexp, show, hash]

  type variable_t = t

  include Record.Variable

  module Set = Core.Set.Make (struct
    type t = type_t Record.Variable.record [@@deriving compare, sexp]
  end)

  type variable_zip_result = {
    variable_pair: pair;
    received_parameter: Parameter.t;
  }
  [@@deriving compare, eq, sexp, show, hash]

  let pp_concise format = function
    | Unary variable -> Unary.pp_concise format variable ~pp_type
    | ParameterVariadic { name; _ } ->
        Format.fprintf format "CallableParameterTypeVariable[%s]" name
    | TupleVariadic { name; _ } -> Format.fprintf format "TypeVarTuple[%s]" name


  let parse_declaration expression ~target =
    match Variadic.Parameters.parse_declaration expression ~target with
    | Some variable -> Some (ParameterVariadic variable)
    | None -> (
        match Variadic.Tuple.parse_declaration expression ~target with
        | Some variable -> Some (TupleVariadic variable)
        | None -> None)


  let dequalify dequalify_map = function
    | Unary variable -> Unary (Unary.dequalify variable ~dequalify_map)
    | ParameterVariadic variable ->
        ParameterVariadic (Variadic.Parameters.dequalify variable ~dequalify_map)
    | TupleVariadic variable -> TupleVariadic (Variadic.Tuple.dequalify variable ~dequalify_map)


  let namespace variable ~namespace =
    match variable with
    | Unary variable -> Unary (Unary.namespace variable ~namespace)
    | ParameterVariadic variable ->
        ParameterVariadic (Variadic.Parameters.namespace variable ~namespace)
    | TupleVariadic variable -> TupleVariadic (Variadic.Tuple.namespace variable ~namespace)


  let partition =
    let partitioner = function
      | Unary variable -> `Fst variable
      | ParameterVariadic variable -> `Snd variable
      | TupleVariadic variable -> `Trd variable
    in
    List.partition3_map ~f:partitioner


  let mark_all_variables_as_bound ?specific annotation =
    let specific_unaries, specific_parameters_variadics, specific_tuple_variadics =
      match specific >>| partition with
      | None -> None, None, None
      | Some (unaries, parameters, tuples) -> Some unaries, Some parameters, Some tuples
    in
    GlobalTransforms.Unary.mark_all_as_bound ?specific:specific_unaries annotation
    |> GlobalTransforms.ParameterVariadic.mark_all_as_bound ?specific:specific_parameters_variadics
    |> GlobalTransforms.TupleVariadic.mark_all_as_bound ?specific:specific_tuple_variadics


  let mark_as_bound = function
    | Unary variable -> Unary (GlobalTransforms.Unary.mark_as_bound variable)
    | ParameterVariadic variable ->
        ParameterVariadic (GlobalTransforms.ParameterVariadic.mark_as_bound variable)
    | TupleVariadic variable ->
        TupleVariadic (GlobalTransforms.TupleVariadic.mark_as_bound variable)


  let mark_all_variables_as_free ?specific annotation =
    let specific_unaries, specific_parameters_variadics, specific_tuple_variadics =
      match specific >>| partition with
      | None -> None, None, None
      | Some (unaries, parameters, tuples) -> Some unaries, Some parameters, Some tuples
    in
    GlobalTransforms.Unary.mark_all_as_free ?specific:specific_unaries annotation
    |> GlobalTransforms.ParameterVariadic.mark_all_as_free ?specific:specific_parameters_variadics
    |> GlobalTransforms.TupleVariadic.mark_all_as_free ?specific:specific_tuple_variadics


  let namespace_all_free_variables annotation ~namespace =
    GlobalTransforms.Unary.namespace_all_free_variables annotation ~namespace
    |> GlobalTransforms.ParameterVariadic.namespace_all_free_variables ~namespace
    |> GlobalTransforms.TupleVariadic.namespace_all_free_variables ~namespace


  let all_free_variables annotation =
    let unaries =
      GlobalTransforms.Unary.all_free_variables annotation
      |> List.map ~f:(fun variable -> Unary variable)
    in
    let callable_variadics =
      GlobalTransforms.ParameterVariadic.all_free_variables annotation
      |> List.map ~f:(fun variable -> ParameterVariadic variable)
    in
    let tuple_variadics =
      GlobalTransforms.TupleVariadic.all_free_variables annotation
      |> List.map ~f:(fun variable -> TupleVariadic variable)
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
    GlobalTransforms.Unary.mark_as_escaped
      annotation
      ~variables:specific_unaries
      ~namespace:fresh_namespace
    |> GlobalTransforms.ParameterVariadic.mark_as_escaped
         ~variables:specific_parameters_variadics
         ~namespace:fresh_namespace
    |> GlobalTransforms.TupleVariadic.mark_as_escaped
         ~variables:specific_tuple_variadics
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
    || GlobalTransforms.TupleVariadic.contains_escaped_free_variable annotation


  let convert_all_escaped_free_variables_to_anys annotation =
    GlobalTransforms.Unary.convert_all_escaped_free_variables_to_anys annotation
    |> GlobalTransforms.ParameterVariadic.convert_all_escaped_free_variables_to_anys
    |> GlobalTransforms.TupleVariadic.convert_all_escaped_free_variables_to_anys


  let converge_all_variable_namespaces annotation =
    GlobalTransforms.Unary.converge_all_variable_namespaces annotation
    |> GlobalTransforms.ParameterVariadic.converge_all_variable_namespaces
    |> GlobalTransforms.TupleVariadic.converge_all_variable_namespaces


  let coalesce_if_all_single parameters =
    Parameter.all_singles parameters
    >>| (fun singles ->
          [
            Parameter.CallableParameters
              (Defined (Callable.prepend_anonymous_parameters ~head:singles ~tail:[]));
          ])
    |> Option.value ~default:parameters


  let make_variable_pair variable received_parameter =
    let variable_pair =
      match variable, received_parameter with
      | Unary unary, Parameter.Single annotation -> UnaryPair (unary, annotation)
      | ParameterVariadic parameter_variadic, CallableParameters callable_parameters ->
          ParameterVariadicPair (parameter_variadic, callable_parameters)
      | Unary unary, _ -> UnaryPair (unary, Unary.any)
      | ParameterVariadic parameter_variadic, _ ->
          ParameterVariadicPair (parameter_variadic, Variadic.Parameters.any)
      | TupleVariadic tuple_variadic, _ ->
          (* We should not hit this case at all. *)
          TupleVariadicPair (tuple_variadic, Variadic.Tuple.any)
    in
    { variable_pair; received_parameter }


  let pairs_for_variadic_class ~non_variadic_prefix_length variables parameters =
    let variables_prefix, variables_rest = List.split_n variables non_variadic_prefix_length in
    match variables_rest with
    | TupleVariadic variadic :: variables_suffix ->
        let parameters_prefix, parameters_rest =
          List.split_n parameters non_variadic_prefix_length
        in
        let parameters_middle, parameters_suffix =
          List.split_n parameters_rest (List.length parameters_rest - List.length variables_suffix)
        in
        let pairs () =
          match
            ( List.map2 variables_prefix parameters_prefix ~f:make_variable_pair,
              List.map2 variables_suffix parameters_suffix ~f:make_variable_pair )
          with
          | Ok prefix_pairs, Ok suffix_pairs ->
              let variadic_pair =
                let ordered_type =
                  match OrderedTypes.concatenation_from_parameters parameters_middle with
                  | Some ordered_type -> Some ordered_type
                  | None ->
                      Parameter.all_singles parameters_middle
                      >>| fun singles -> OrderedTypes.Concrete singles
                in
                ordered_type
                >>| (fun ordered_type ->
                      {
                        variable_pair = TupleVariadicPair (variadic, ordered_type);
                        received_parameter = Single (Tuple ordered_type);
                      })
                |> Option.value
                     ~default:
                       {
                         variable_pair = TupleVariadicPair (variadic, Variadic.Tuple.any);
                         received_parameter =
                           Single
                             (parametric
                                Variadic.Tuple.synthetic_class_name_for_error
                                parameters_middle);
                       }
              in
              prefix_pairs @ [variadic_pair] @ suffix_pairs |> Option.some
          | _ -> None
        in
        let has_variadic_in_prefix_or_suffix =
          List.exists parameters_prefix ~f:Parameter.is_unpacked
          || List.exists parameters_suffix ~f:Parameter.is_unpacked
        in
        if has_variadic_in_prefix_or_suffix then
          None
        else
          pairs ()
    | _ -> None


  (* Zip the generic variables `Generic[T1, T2]` of a class with its parameters Foo[int, str]. *)
  let zip_variables_with_parameters_including_mismatches ~parameters variables =
    let parameters =
      match variables with
      | [ParameterVariadic _] -> coalesce_if_all_single parameters
      | _ -> parameters
    in
    let variadic_index index = function
      | TupleVariadic _ -> Some index
      | _ -> None
    in
    match List.filter_mapi variables ~f:variadic_index with
    | [unpacked_index] ->
        pairs_for_variadic_class ~non_variadic_prefix_length:unpacked_index variables parameters
    | [] -> (
        match List.map2 variables parameters ~f:make_variable_pair with
        | Ok pairs -> Some pairs
        | Unequal_lengths -> None)
    | _ ->
        (* Reject multiple variadic generics. *)
        None


  let zip_variables_with_parameters ~parameters variables =
    zip_variables_with_parameters_including_mismatches ~parameters variables
    >>| List.map ~f:(function { variable_pair; _ } -> variable_pair)


  let zip_variables_with_two_parameter_lists ~left_parameters ~right_parameters variables =
    let left_pairs, right_pairs =
      ( zip_variables_with_parameters ~parameters:left_parameters variables,
        zip_variables_with_parameters ~parameters:right_parameters variables )
    in
    Option.map2 left_pairs right_pairs ~f:List.zip
    >>= function
    | Ok pairs -> Some pairs
    | Unequal_lengths -> None


  let all_unary variables =
    List.map variables ~f:(function
        | Unary unary -> Some unary
        | ParameterVariadic _
        | TupleVariadic _ ->
            None)
    |> Option.all


  let to_parameter = function
    | Unary variable -> Parameter.Single (Unary.self_reference variable)
    | ParameterVariadic variable ->
        Parameter.CallableParameters (Variadic.Parameters.self_reference variable)
    | TupleVariadic variadic -> Parameter.Unpacked (Variadic variadic)
end

let resolve_aliases ~aliases annotation =
  let visited = Hash_set.create () in
  let module ResolveTransform = Transform.Make (struct
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
              | Some (TypeAlias alias) ->
                  let alias =
                    match alias with
                    (* Type variables are stored as `_T aliases to _T`. Don't replace them. *)
                    | Variable _ as variable -> variable
                    | alias ->
                        alias
                        |> Variable.GlobalTransforms.Unary.replace_all (fun _ ->
                               Some Variable.Unary.any)
                        |> Variable.GlobalTransforms.ParameterVariadic.replace_all (fun _ ->
                               Some Variable.Variadic.Parameters.any)
                        |> Variable.GlobalTransforms.TupleVariadic.replace_all (fun _ ->
                               Some Variable.Variadic.Tuple.any)
                  in
                  mark_recursive_alias_as_visited alias;
                  alias
              | _ -> annotation)
          | Parametric { name = alias_name; parameters = given_parameters } ->
              let deduplicate_preserving_order list =
                List.fold list ~init:([], Variable.Set.empty) ~f:(fun (sofar, seen_set) x ->
                    if Variable.Set.mem seen_set x then
                      sofar, seen_set
                    else
                      x :: sofar, Variable.Set.add seen_set x)
                |> fst
                |> List.rev
              in
              let instantiate ~given_parameters uninstantiated_alias_annotation =
                let variable_pairs =
                  Variable.zip_variables_with_parameters
                    ~parameters:given_parameters
                    (Variable.all_free_variables uninstantiated_alias_annotation
                    |> deduplicate_preserving_order)
                in
                match variable_pairs with
                | Some variable_pairs ->
                    uninstantiated_alias_annotation
                    |> Variable.GlobalTransforms.Unary.replace_all (fun given_variable ->
                           List.find_map variable_pairs ~f:(function
                               | UnaryPair (variable, replacement)
                                 when [%equal: Variable.unary_t] variable given_variable ->
                                   Some replacement
                               | _ -> None))
                    |> Variable.GlobalTransforms.ParameterVariadic.replace_all
                         (fun given_variable ->
                           List.find_map variable_pairs ~f:(function
                               | ParameterVariadicPair (variable, replacement)
                                 when [%equal: Variable.parameter_variadic_t]
                                        variable
                                        given_variable ->
                                   Some replacement
                               | _ -> None))
                    |> Variable.GlobalTransforms.TupleVariadic.replace_all (fun given_variable ->
                           List.find_map variable_pairs ~f:(function
                               | TupleVariadicPair (variable, replacement)
                                 when [%equal: Variable.tuple_variadic_t] variable given_variable ->
                                   Some replacement
                               | _ -> None))
                | _ -> uninstantiated_alias_annotation
              in
              let resolved =
                (* Don't replace unbound generic parameters with Any. Otherwise, a generic alias Foo
                   (or an import alias for a generic alias) will become Foo[Any] instead of Foo[T].
                   We will not find any free type variables in Foo[Any] and simply resolve it as
                   Foo[Any]. *)
                match aliases ?replace_unbound_parameters_with_any:(Some false) alias_name with
                | Some (TypeAlias uninstantiated_alias_annotation) ->
                    instantiate ~given_parameters uninstantiated_alias_annotation
                | _ -> annotation
              in
              mark_recursive_alias_as_visited resolved;
              resolved
          | RecursiveType _ ->
              mark_recursive_alias_as_visited annotation;
              annotation
          | _ -> annotation)
      in
      let transformed_annotation = resolve annotation in
      { Transform.transformed_annotation; new_state = () }
  end)
  in
  snd (ResolveTransform.visit () annotation)


let create ~aliases =
  let variable_aliases name =
    match aliases ?replace_unbound_parameters_with_any:(Some false) name with
    | Some (VariableAlias variable) -> Some variable
    | _ -> None
  in
  create_logic ~resolve_aliases:(resolve_aliases ~aliases) ~variable_aliases


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
        | Tuple (Concrete parameters) ->
            Parametric
              {
                name = dequalify_string "typing.Tuple";
                parameters =
                  List.map parameters ~f:(fun parameter -> Record.Parameter.Single parameter);
              }
        | Primitive name -> Primitive (dequalify_identifier map name)
        | Variable ({ variable = name; _ } as annotation) ->
            Variable { annotation with variable = dequalify_identifier map name }
        | IntExpression _ ->
            let replacement ({ Record.Variable.RecordUnary.variable; _ } as record) =
              Some (Variable { record with variable = dequalify_identifier map variable })
            in
            Variable.GlobalTransforms.Unary.replace_all replacement annotation
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

  let create_field ~annotation ~has_non_total_typed_dictionary_base_class name =
    let annotation, required =
      match annotation with
      | Parametric
          {
            name = "typing_extensions.NotRequired" | "typing.NotRequired";
            parameters = [Single annotation];
          } ->
          annotation, false
      | Parametric
          {
            name = "typing_extensions.Required" | "typing.Required";
            parameters = [Single annotation];
          } ->
          annotation, true
      | _ -> annotation, not has_non_total_typed_dictionary_base_class
    in
    { name; annotation; required }


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


  let self_parameter class_name =
    CallableParameter.Named { name = "self"; annotation = Primitive class_name; default = false }


  let field_named_parameters ?(all_default = false) ~class_name fields =
    let field_to_argument { name; annotation; required } =
      Record.Callable.RecordParameter.KeywordOnly
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
    |> fun parameters -> Defined (self_parameter class_name :: parameters)


  let constructor ~name ~fields =
    {
      Callable.kind = Named (Reference.create "__init__");
      implementation = { annotation = Top; parameters = Undefined };
      overloads =
        [
          { annotation = none; parameters = field_named_parameters ~class_name:name fields };
          {
            annotation = none;
            parameters =
              Defined
                [
                  Record.Callable.RecordParameter.PositionalOnly
                    { index = 0; annotation = Primitive name; default = false };
                  Record.Callable.RecordParameter.PositionalOnly
                    { index = 1; annotation = Primitive name; default = false };
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
        { annotation; parameters = Defined [self_parameter class_name; key_parameter name] }
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
                self_parameter class_name;
                key_parameter name;
                Named { name = "v"; annotation; default = false };
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
            parameters = Defined [self_parameter class_name; key_parameter name];
          };
          {
            annotation = Union [annotation; Variable (Variable.Unary.create "_T")];
            parameters =
              Defined
                [
                  self_parameter class_name;
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
                self_parameter class_name;
                key_parameter name;
                Named { name = "default"; annotation; default = false };
              ];
        }
      in
      List.map ~f:overload
    in
    let update_overloads fields =
      [
        {
          annotation = none;
          parameters = field_named_parameters ~all_default:true ~class_name fields;
        };
        {
          annotation = none;
          parameters =
            Defined
              [
                Record.Callable.RecordParameter.PositionalOnly
                  { index = 0; annotation = Primitive class_name; default = false };
                Record.Callable.RecordParameter.PositionalOnly
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
            { annotation; parameters = Defined [self_parameter class_name; key_parameter name] };
            {
              annotation = Union [annotation; Variable (Variable.Unary.create "_T")];
              parameters =
                Defined
                  [
                    self_parameter class_name;
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
          {
            annotation = none;
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


  let class_name = typed_dictionary_class_name

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
              parent = Some (Reference.create class_name);
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
      @ List.map (common_special_methods ~class_name) ~f:(fun { name; _ } -> define name)
    in
    if total then
      common_methods
    else
      common_methods
      @ (non_total_special_methods class_name |> List.map ~f:(fun { name; _ } -> define name))
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
          if should_be_unbound then
            Some (Tuple (OrderedTypes.create_unbounded_concatenation parameter))
          else
            None
        in
        match annotation with
        | Tuple (Concrete types) when List.length types > 2 ->
            shorten_tuple_type types |> Option.value ~default:annotation
        | Parametric { name = "typing.Tuple"; parameters } when List.length parameters > 2 ->
            let types = List.filter_map parameters ~f:Parameter.is_single in
            if List.length types < List.length parameters then
              annotation
            else
              shorten_tuple_type types |> Option.value ~default:annotation
        | Callable
            ({ implementation = { parameters = Defined parameters; _ } as implementation; _ } as
            callable) ->
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

(* `resolve_class` is used to extract a class name (like "list") from a type (or classes from a
   union) so that we can get its attributes, global location, etc. It also returns the instantiated
   type `List[int]` since that is also needed for attribute lookup. *)
let resolve_class annotation =
  let rec extract ~meta original_annotation =
    let annotation =
      match original_annotation with
      (* Variables return their upper bound because we need to take the least informative type in
         their interval. Otherwise, we might access an attribute that doesn't exist on the actual
         type. *)
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
        (* Unions return the list of member classes because an attribute lookup has to be supported
           by all members of the union. *)
        let flatten_optional sofar optional =
          match sofar, optional with
          | Some sofar, Some optional -> Some (optional :: sofar)
          | _ -> None
        in
        List.map ~f:(extract ~meta) annotations
        |> List.fold ~init:(Some []) ~f:flatten_optional
        >>| List.concat
        >>| List.rev
    | RecursiveType ({ name; body } as recursive_type) ->
        extract ~meta body
        (* Filter out the recursive type name itself since it's not a valid class name.

           Removing the inner occurrences of the recursive type is fine because of induction. If the
           other classes in a union support an attribute lookup, the recursive type will too. If
           they don't, then the recursive type won't either. *)
        >>| List.filter ~f:(fun { class_name; _ } -> not (Identifier.equal class_name name))
        >>| List.map ~f:(fun ({ instantiated; _ } as class_data) ->
                {
                  class_data with
                  instantiated =
                    RecursiveType.replace_references_with_recursive_type
                      ~recursive_type
                      instantiated;
                })
    | Annotated annotation -> extract ~meta annotation
    | annotation when is_meta annotation ->
        (* Metaclasses return accessed_through_class=true since they allow looking up only class
           attribute, etc. *)
        single_parameter annotation |> extract ~meta:true
    | _ -> (
        match split annotation |> fst |> primitive_name with
        | Some class_name ->
            Some [{ instantiated = original_annotation; accessed_through_class = meta; class_name }]
        | None -> None)
  in
  extract ~meta:false annotation


let callable_name = function
  | Callable { kind = Named name; _ } -> Some name
  | Parametric
      { name = "BoundMethod"; parameters = [Single (Callable { kind = Named name; _ }); Single _] }
    ->
      Some name
  | _ -> None
