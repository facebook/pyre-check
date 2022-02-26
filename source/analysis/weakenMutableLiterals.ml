(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open Ast

type typed_dictionary_mismatch =
  | MissingRequiredField of {
      field_name: Identifier.t;
      class_name: Identifier.t;
    }
  | FieldTypeMismatch of {
      field_name: Identifier.t;
      expected_type: Type.t;
      actual_type: Type.t;
      class_name: Identifier.t;
    }
  | UndefinedField of {
      field_name: Identifier.t;
      class_name: Identifier.t;
    }
[@@deriving compare, show, sexp]

type weakened_type = {
  resolved: Type.t;
  typed_dictionary_errors: typed_dictionary_mismatch Node.t list;
}
[@@deriving compare, show]

let typed_dictionary_errors { typed_dictionary_errors; _ } = typed_dictionary_errors

let resolved_type { resolved; _ } = resolved

let make_weakened_type ?(typed_dictionary_errors = []) resolved =
  { resolved; typed_dictionary_errors }


let combine_weakened_types weakened_types =
  {
    resolved = Type.union (List.map weakened_types ~f:resolved_type);
    typed_dictionary_errors = List.concat_map weakened_types ~f:typed_dictionary_errors;
  }


let undefined_field_mismatches
    ~location
    ~expected_typed_dictionary:
      { Type.Record.TypedDictionary.fields = expected_fields; name = class_name }
    ~resolved_typed_dictionary:{ Type.Record.TypedDictionary.fields = resolved_fields; _ }
  =
  let make_undefined_field_mismatch { Type.Record.TypedDictionary.name; annotation = _; _ } =
    UndefinedField { field_name = name; class_name } |> Node.create ~location
  in
  let is_undefined_field field =
    not (List.exists expected_fields ~f:(Type.TypedDictionary.same_name field))
  in
  List.filter resolved_fields ~f:is_undefined_field |> List.map ~f:make_undefined_field_mismatch


let distribute_union_over_parametric ~parametric_name ~number_of_parameters annotation =
  match annotation with
  | Type.Union parameters ->
      let extract_matching_parameters = function
        | Type.Parametric { name; parameters }
          when Identifier.equal name parametric_name
               && List.length parameters = number_of_parameters ->
            Type.Parameter.all_singles parameters
        | _ -> None
      in
      let combine_parameters parameters_list =
        match number_of_parameters with
        | 1 -> Some [Type.Parameter.Single (Type.union (List.concat parameters_list))]
        | 2 ->
            Some
              [
                Type.Parameter.Single (Type.union (List.map ~f:List.hd_exn parameters_list));
                Type.Parameter.Single (Type.union (List.map ~f:List.last_exn parameters_list));
              ]
        | _ -> None
      in
      List.map parameters ~f:extract_matching_parameters
      |> Option.all
      >>= combine_parameters
      >>| fun parametric_types -> Type.parametric parametric_name parametric_types
  | _ -> None


let rec weaken_mutable_literals
    resolve
    ~resolve_items_individually
    ~get_typed_dictionary
    ~expression
    ~resolved
    ~expected
    ~comparator
  =
  let comparator_without_override = comparator in
  let comparator = comparator ~get_typed_dictionary_override:(fun _ -> None) in
  let open Expression in
  match expression, resolved, expected with
  | _, _, Type.Union expected_types -> (
      let weakened_types =
        List.map
          ~f:(fun expected_type ->
            weaken_mutable_literals
              ~get_typed_dictionary
              resolve
              ~expression
              ~resolved
              ~expected:expected_type
              ~resolve_items_individually
              ~comparator:comparator_without_override)
          expected_types
      in
      match
        List.exists2
          ~f:(fun { resolved = left; _ } right -> comparator ~left ~right)
          weakened_types
          expected_types
      with
      | Ok true -> make_weakened_type expected
      | Ok false ->
          make_weakened_type
            ~typed_dictionary_errors:(List.concat_map weakened_types ~f:typed_dictionary_errors)
            resolved
      | Unequal_lengths -> make_weakened_type resolved)
  | ( Some { Node.value = Expression.List items; _ },
      Type.Parametric { name = "list" as container_name; parameters = [Single actual_item_type] },
      Type.Parametric { name = "list"; parameters = [Single expected_item_type] } )
  | ( Some { Node.value = Expression.Set items; _ },
      Type.Parametric { name = "set" as container_name; parameters = [Single actual_item_type] },
      Type.Parametric { name = "set"; parameters = [Single expected_item_type] } ) ->
      let weakened_item_types =
        List.map
          ~f:(fun item ->
            let resolved_item_type = resolve item in
            let resolved =
              if resolve_items_individually && not (Type.is_top resolved_item_type) then
                resolved_item_type
              else
                actual_item_type
            in
            weaken_mutable_literals
              ~get_typed_dictionary
              resolve
              ~expression:(Some item)
              ~resolved
              ~expected:expected_item_type
              ~resolve_items_individually
              ~comparator:comparator_without_override)
          items
      in
      let { resolved = weakened_item_type; typed_dictionary_errors } =
        combine_weakened_types weakened_item_types
      in
      make_weakened_type
        ~typed_dictionary_errors
        (if comparator ~left:weakened_item_type ~right:expected_item_type then
           expected
        else
          Type.parametric container_name [Single weakened_item_type])
  | ( Some { Node.value = Expression.ListComprehension _; _ },
      Type.Parametric { name = "list"; parameters = [Single actual] },
      Type.Parametric { name = "list"; parameters = [Single expected_parameter] } )
    when comparator ~left:actual ~right:expected_parameter ->
      make_weakened_type expected
  | ( Some { Node.value = Expression.SetComprehension _; _ },
      Type.Parametric { name = "set"; parameters = [Single actual] },
      Type.Parametric { name = "set"; parameters = [Single expected_parameter] } )
    when comparator ~left:actual ~right:expected_parameter ->
      make_weakened_type expected
  | ( Some { Node.value = Expression.Tuple items; _ },
      Type.Tuple (Concrete actual_item_types),
      Type.Tuple (Concrete expected_item_types) )
    when List.length actual_item_types = List.length expected_item_types -> (
      let weakened_item_types =
        List.map3
          ~f:(fun item actual_item_type expected_item_type ->
            weaken_mutable_literals
              ~get_typed_dictionary
              resolve
              ~expression:(Some item)
              ~resolved:actual_item_type
              ~expected:expected_item_type
              ~resolve_items_individually
              ~comparator:comparator_without_override)
          items
          actual_item_types
          expected_item_types
      in
      match weakened_item_types with
      | Ok weakened_item_types ->
          let resolved_types = List.map weakened_item_types ~f:resolved_type in
          let weakened_type = Type.Tuple (Concrete resolved_types) in
          make_weakened_type
            ~typed_dictionary_errors:
              (List.concat_map weakened_item_types ~f:typed_dictionary_errors)
            (if comparator ~left:weakened_type ~right:expected then
               expected
            else
              weakened_type)
      | Unequal_lengths -> make_weakened_type resolved)
  | ( Some { Node.value = Expression.Tuple items; _ },
      Type.Tuple (Concrete actual_item_types),
      Type.Tuple (Concatenation concatenation) ) ->
      let weakened_tuple expected_item_type =
        let weakened_item_types =
          List.map2
            ~f:(fun item actual_item_type ->
              weaken_mutable_literals
                ~get_typed_dictionary
                resolve
                ~expression:(Some item)
                ~resolved:actual_item_type
                ~expected:expected_item_type
                ~resolve_items_individually
                ~comparator:comparator_without_override)
            items
            actual_item_types
        in
        match weakened_item_types with
        | Ok weakened_item_types ->
            let { resolved = weakened_item_type; typed_dictionary_errors } =
              combine_weakened_types weakened_item_types
            in
            Some
              (make_weakened_type
                 ~typed_dictionary_errors
                 (if comparator ~left:weakened_item_type ~right:expected_item_type then
                    expected
                 else
                   resolved))
        | Unequal_lengths -> None
      in
      Type.OrderedTypes.Concatenation.extract_sole_unbounded_annotation concatenation
      >>= weakened_tuple
      |> Option.value ~default:(make_weakened_type resolved)
  | ( Some { Node.value = Expression.Dictionary { entries; keywords = [] }; location },
      _,
      Type.Primitive _ ) -> (
      let open Type.Record.TypedDictionary in
      match get_typed_dictionary expected with
      | Some ({ fields = expected_fields; name = expected_class_name } as expected_typed_dictionary)
        ->
          let find_matching_field ~name =
            let matching_name ({ name = expected_name; _ } : Type.t typed_dictionary_field) =
              String.equal name expected_name
            in
            List.find ~f:matching_name
          in
          let resolve_entry { Dictionary.Entry.key; value } =
            let key = resolve key in
            match key with
            | Type.Literal (Type.String (Type.LiteralValue name)) ->
                let annotation, required =
                  let resolved = resolve value in
                  let relax { annotation; _ } =
                    if Type.is_dictionary resolved || Option.is_some (get_typed_dictionary resolved)
                    then
                      weaken_mutable_literals
                        resolve
                        ~expression:(Some value)
                        ~resolved
                        ~expected:annotation
                        ~comparator:comparator_without_override
                        ~resolve_items_individually
                        ~get_typed_dictionary
                    else if comparator ~left:resolved ~right:annotation then
                      make_weakened_type annotation
                    else
                      make_weakened_type resolved
                  in
                  find_matching_field expected_fields ~name
                  >>| (fun field -> relax field, field.required)
                  |> Option.value ~default:(make_weakened_type resolved, true)
                in
                Some { name; annotation; required }
            | _ -> None
          in
          let add_missing_fields_if_all_non_required sofar =
            let is_missing ({ name; _ } : Type.t typed_dictionary_field) =
              Option.is_none (find_matching_field sofar ~name)
            in
            let missing_fields = List.filter expected_fields ~f:is_missing in
            if List.for_all missing_fields ~f:(fun { required; _ } -> not required) then
              sofar @ missing_fields
            else
              sofar
          in
          let fresh_class_name = "$fresh_class_name" in
          let get_typed_dictionary_override ~typed_dictionary annotation =
            match annotation with
            | Type.Primitive name when String.equal name fresh_class_name -> Some typed_dictionary
            | _ -> None
          in
          let weaken_valid_fields fields =
            let ({ fields = actual_fields; _ } as resolved_typed_dictionary) =
              add_missing_fields_if_all_non_required fields |> Type.TypedDictionary.anonymous
            in
            let less_than_expected =
              comparator_without_override
                ~get_typed_dictionary_override:
                  (get_typed_dictionary_override ~typed_dictionary:resolved_typed_dictionary)
                ~left:(Type.Primitive fresh_class_name)
                ~right:expected
            in
            if less_than_expected then
              make_weakened_type
                ~typed_dictionary_errors:
                  (undefined_field_mismatches
                     ~location
                     ~resolved_typed_dictionary
                     ~expected_typed_dictionary)
                expected
            else
              let type_mismatches =
                let make_type_mismatch
                    {
                      Type.Record.TypedDictionary.name = expected_field_name;
                      annotation = expected_type;
                      _;
                    }
                    { Type.Record.TypedDictionary.annotation = actual_type; required = _; _ }
                  =
                  FieldTypeMismatch
                    {
                      field_name = expected_field_name;
                      expected_type;
                      actual_type;
                      class_name = expected_class_name;
                    }
                  |> Node.create ~location
                in
                let find_type_mismatch expected_field =
                  List.find
                    actual_fields
                    ~f:(Type.TypedDictionary.same_name_different_annotation expected_field)
                  >>| make_type_mismatch expected_field
                in
                List.filter_map expected_fields ~f:find_type_mismatch
              in
              let missing_field_mismatches =
                let is_missing expected_field =
                  not (List.exists actual_fields ~f:(Type.TypedDictionary.same_name expected_field))
                in
                let make_missing_field_mismatch
                    { Type.Record.TypedDictionary.name = field_name; required; _ }
                  =
                  MissingRequiredField { field_name; class_name = expected_class_name }
                  |> Node.create ~location
                  |> Option.some_if required
                in
                List.filter expected_fields ~f:is_missing
                |> List.filter_map ~f:make_missing_field_mismatch
              in
              make_weakened_type
                ~typed_dictionary_errors:(type_mismatches @ missing_field_mismatches)
                resolved
          in
          let valid_field_or_typed_dictionary_error
              {
                name;
                required;
                annotation = { resolved; typed_dictionary_errors } as weakened_type;
              }
            =
            match typed_dictionary_errors with
            | [] -> Ok { name; required; annotation = resolved }
            | _ -> Error weakened_type
          in
          List.map entries ~f:resolve_entry
          |> Option.all
          >>| List.map ~f:valid_field_or_typed_dictionary_error
          >>| Result.combine_errors
          >>| (function
                | Ok fields -> weaken_valid_fields fields
                | Error erroneous_weakened_types ->
                    make_weakened_type
                      ~typed_dictionary_errors:
                        (List.concat_map erroneous_weakened_types ~f:typed_dictionary_errors)
                      resolved)
          |> Option.value ~default:(make_weakened_type resolved)
      | None -> make_weakened_type resolved)
  | ( Some { Node.value = Expression.Dictionary _; _ },
      _,
      Type.Parametric { name = "typing.Mapping" as generic_name; parameters } )
  | ( Some { Node.value = Expression.List _; _ },
      _,
      Type.Parametric { name = ("typing.Sequence" | "typing.Iterable") as generic_name; parameters }
    )
  | ( Some { Node.value = Expression.Set _; _ },
      _,
      Type.Parametric { name = "typing.AbstractSet" as generic_name; parameters } ) ->
      let mutable_generic_name =
        match generic_name with
        | "typing.Mapping" -> "dict"
        | "typing.Sequence"
        | "typing.Iterable" ->
            "list"
        | "typing.AbstractSet" -> "set"
        | _ -> failwith "Unexpected generic name"
      in
      let { resolved = weakened_fallback_type; typed_dictionary_errors } =
        weaken_mutable_literals
          ~get_typed_dictionary
          resolve
          ~resolved
          ~expected:(Type.parametric mutable_generic_name parameters)
          ~comparator:comparator_without_override
          ~expression
          ~resolve_items_individually
      in
      let resolved =
        match weakened_fallback_type with
        | Type.Parametric { name; parameters } when Identifier.equal name mutable_generic_name ->
            Type.parametric generic_name parameters
        | _ -> weakened_fallback_type
      in
      make_weakened_type ~typed_dictionary_errors resolved
  | ( Some { Node.value = Expression.Dictionary { entries; _ }; _ },
      Type.Parametric
        { name = "dict"; parameters = [Single actual_key_type; Single actual_value_type] },
      Type.Parametric
        { name = "dict"; parameters = [Single expected_key_type; Single expected_value_type] } ) ->
      weaken_dictionary_entries
        ~get_typed_dictionary
        resolve
        ~expected
        ~comparator:comparator_without_override
        ~entries
        ~actual_key_type
        ~actual_value_type
        ~expected_key_type
        ~expected_value_type
        ~resolve_items_individually
  | ( Some { Node.value = Expression.DictionaryComprehension _; _ },
      Type.Parametric { name = "dict"; parameters = [Single actual_key; Single actual_value] },
      Type.Parametric { name = "dict"; parameters = [Single expected_key; Single expected_value] } )
    when comparator ~left:actual_key ~right:expected_key
         && comparator ~left:actual_value ~right:expected_value ->
      make_weakened_type expected
  | ( Some
        {
          Node.value =
            Expression.Constant
              (Constant.String { StringLiteral.kind = StringLiteral.String; value = _ }) as
            expression;
          _;
        },
      Type.Primitive "str",
      Type.Literal (Type.String _) )
  | ( Some { Node.value = Expression.Constant (Constant.Integer _) as expression; _ },
      Type.Primitive "int",
      Type.Literal (Type.Integer _) )
  | ( Some { Node.value = Expression.Constant Constant.(True | False) as expression; _ },
      Type.Primitive "bool",
      Type.Literal (Type.Boolean _) )
  | ( Some { Node.value = Expression.Name (Attribute _) as expression; _ },
      Type.Primitive _,
      Type.Literal (Type.EnumerationMember _) ) -> (
      match Type.create_literal expression with
      | Some (Type.Literal _ as actual_literal) ->
          if Type.equal actual_literal expected then
            make_weakened_type expected
          else
            make_weakened_type actual_literal
      | _ -> make_weakened_type resolved)
  | _, _, Type.RecursiveType recursive_type ->
      let ({ resolved = weakened_fallback_type; _ } as weakened_type) =
        weaken_mutable_literals
          ~get_typed_dictionary
          resolve
          ~expression
          ~resolved
          ~expected:(Type.RecursiveType.unfold_recursive_type recursive_type)
          ~comparator:comparator_without_override
          ~resolve_items_individually:true
      in
      let resolved =
        if comparator ~left:weakened_fallback_type ~right:expected then
          expected
        else
          weakened_fallback_type
      in
      { weakened_type with resolved }
  | _ ->
      weaken_by_distributing_union
        ~get_typed_dictionary
        resolve
        ~expected
        ~resolved
        ~comparator:comparator_without_override
        ~expression
        ~resolve_items_individually


and weaken_dictionary_entries
    ~get_typed_dictionary
    resolve
    ~expected
    ~comparator
    ~entries
    ~actual_key_type
    ~actual_value_type
    ~expected_key_type
    ~expected_value_type
    ~resolve_items_individually
  =
  let comparator_without_override = comparator in
  let comparator = comparator ~get_typed_dictionary_override:(fun _ -> None) in
  let { resolved = weakened_key_type; typed_dictionary_errors = key_errors } =
    List.map
      ~f:(fun { key; _ } ->
        weaken_mutable_literals
          ~get_typed_dictionary
          resolve
          ~expression:(Some key)
          ~resolved:actual_key_type
          ~expected:expected_key_type
          ~resolve_items_individually
          ~comparator:comparator_without_override)
      entries
    |> combine_weakened_types
  in
  let { resolved = weakened_value_type; typed_dictionary_errors = value_errors } =
    List.map
      ~f:(fun { value; _ } ->
        weaken_mutable_literals
          ~get_typed_dictionary
          resolve
          ~expression:(Some value)
          ~resolved:(if resolve_items_individually then resolve value else actual_value_type)
          ~expected:expected_value_type
          ~resolve_items_individually
          ~comparator:comparator_without_override)
      entries
    |> combine_weakened_types
  in
  make_weakened_type
    ~typed_dictionary_errors:(key_errors @ value_errors)
    (if
     comparator ~left:weakened_key_type ~right:expected_key_type
     && comparator ~left:weakened_value_type ~right:expected_value_type
    then
       expected
    else
      Type.dictionary ~key:weakened_key_type ~value:weakened_value_type)


and weaken_by_distributing_union
    ~get_typed_dictionary
    resolve
    ~expected
    ~resolved
    ~comparator
    ~expression
    ~resolve_items_individually
  =
  let open Expression in
  let comparator_without_override = comparator in
  match expression, resolved, expected with
  | ( Some { Node.value = Expression.List _ | Expression.Set _; _ },
      Type.Union _,
      Type.Parametric
        {
          name = ("list" | "set") as parametric_name;
          parameters = [Single (Type.Union _)] as parameters;
        } )
  | ( Some { Node.value = Expression.Dictionary _; _ },
      Type.Union _,
      Type.Parametric
        {
          name = "dict" as parametric_name;
          parameters = ([Single (Type.Union _); _] | [_; Single (Type.Union _)]) as parameters;
        } ) -> (
      match
        distribute_union_over_parametric
          ~parametric_name
          ~number_of_parameters:(List.length parameters)
          resolved
      with
      | Some resolved ->
          weaken_mutable_literals
            ~get_typed_dictionary
            resolve
            ~expression
            ~resolved
            ~expected
            ~resolve_items_individually
            ~comparator:comparator_without_override
      | None -> make_weakened_type resolved)
  | _ -> make_weakened_type resolved


let weaken_mutable_literals = weaken_mutable_literals ~resolve_items_individually:false
