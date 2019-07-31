(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Pyre
open Ast
open Statement

type generic_type_problems =
  | IncorrectNumberOfParameters of {
      actual: int;
      expected: int;
    }
  | ViolateConstraints of {
      actual: Type.t;
      expected: Type.Variable.Unary.t;
    }
[@@deriving compare, eq, sexp, show, hash]

type type_parameters_mismatch = {
  name: string;
  kind: generic_type_problems;
}
[@@deriving compare, eq, sexp, show, hash]

type class_metadata = {
  successors: Type.Primitive.t list;
  is_test: bool;
  is_final: bool;
  extends_placeholder_stub_class: bool;
}
[@@deriving eq]

type global = Annotation.t Node.t [@@deriving eq, show]

type t = {
  class_hierarchy: (module ClassHierarchy.Handler);
  aliases: Type.Primitive.t -> Type.alias option;
  module_definition: Reference.t -> Module.t option;
  class_definition: Type.Primitive.t -> Class.t Node.t option;
  class_metadata: Type.Primitive.t -> class_metadata option;
  constructor: resolution:t -> Type.Primitive.t -> Type.t option;
  attributes: resolution:t -> Type.t -> AnnotatedAttribute.t list option;
  is_protocol: Type.t -> bool;
  undecorated_signature: Reference.t -> Type.t Type.Callable.overload option;
  protocol_assumptions: TypeOrder.ProtocolAssumptions.t;
  global: Reference.t -> global option;
}

let create
    ~class_hierarchy
    ~aliases
    ~module_definition
    ~class_definition
    ~class_metadata
    ~constructor
    ~undecorated_signature
    ~attributes
    ~is_protocol
    ~global
    ()
  =
  {
    class_hierarchy;
    aliases;
    module_definition;
    class_definition;
    class_metadata;
    constructor;
    undecorated_signature;
    attributes;
    is_protocol;
    protocol_assumptions = TypeOrder.ProtocolAssumptions.empty;
    global;
  }


let is_tracked { class_hierarchy; _ } = ClassHierarchy.contains class_hierarchy

let contains_untracked resolution annotation =
  List.exists
    ~f:(fun annotation -> not (is_tracked resolution annotation))
    (Type.elements annotation)


let constructor ({ constructor; _ } as resolution) = constructor ~resolution

let is_protocol { is_protocol; _ } = is_protocol

let primitive_name annotation =
  let primitive, _ = Type.split annotation in
  Type.primitive_name primitive


let class_definition { class_definition; _ } annotation =
  primitive_name annotation >>= class_definition


let class_metadata { class_metadata; _ } annotation = primitive_name annotation >>= class_metadata

let full_order ({ class_hierarchy; attributes = a; protocol_assumptions; _ } as resolution) =
  let constructor instantiated ~protocol_assumptions =
    instantiated |> Type.primitive_name >>= constructor { resolution with protocol_assumptions }
  in
  let attributes t ~protocol_assumptions =
    a ~resolution:{ resolution with protocol_assumptions } t
  in
  let is_protocol annotation ~protocol_assumptions =
    is_protocol { resolution with protocol_assumptions } annotation
  in
  {
    TypeOrder.handler = class_hierarchy;
    constructor;
    attributes;
    is_protocol;
    protocol_assumptions;
    any_is_bottom = false;
  }


let variables ?(default = None) { class_hierarchy; _ } =
  ClassHierarchy.variables ~default class_hierarchy


let check_invalid_type_parameters resolution annotation =
  let module InvalidTypeParametersTransform = Type.Transform.Make (struct
    type state = type_parameters_mismatch list

    let visit_children_before _ _ = false

    let visit_children_after = true

    let visit sofar annotation =
      let transformed_annotation, new_state =
        let generics_for_name name =
          match name with
          | "type"
          | "typing.Type"
          | "typing.ClassVar"
          | "typing.Iterator"
          | "Optional"
          | "typing.Final"
          | "typing_extensions.Final"
          | "typing.Optional" ->
              ClassHierarchy.Unaries [Type.Variable.Unary.create "T"]
          | _ -> variables resolution name |> Option.value ~default:(ClassHierarchy.Unaries [])
        in
        let invalid_type_parameters ~name ~given =
          let generics = generics_for_name name in
          let open ClassHierarchy in
          match generics, given with
          | Unaries generics, Type.OrderedTypes.Concrete given -> (
            match List.zip generics given with
            | Ok [] -> Type.Primitive name, sofar
            | Ok paired ->
                let check_parameter (generic, given) =
                  let invalid =
                    let order =
                      let order = full_order resolution in
                      { order with any_is_bottom = true }
                    in
                    let pair = Type.Variable.UnaryPair (generic, given) in
                    TypeOrder.OrderedConstraints.add_lower_bound TypeConstraints.empty ~order ~pair
                    >>| TypeOrder.OrderedConstraints.add_upper_bound ~order ~pair
                    |> Option.is_none
                  in
                  if invalid then
                    ( Type.Any,
                      Some
                        { name; kind = ViolateConstraints { actual = given; expected = generic } }
                    )
                  else
                    given, None
                in
                List.map paired ~f:check_parameter
                |> List.unzip
                |> fun (parameters, errors) ->
                Type.parametric name (Concrete parameters), List.filter_map errors ~f:Fn.id @ sofar
            | Unequal_lengths ->
                let mismatch =
                  {
                    name;
                    kind =
                      IncorrectNumberOfParameters
                        { actual = List.length given; expected = List.length generics };
                  }
                in
                ( Type.parametric name (Concrete (List.map generics ~f:(fun _ -> Type.Any))),
                  mismatch :: sofar ) )
          | ListVariadic _, Any -> Type.parametric name given, sofar
          | Unaries _, Variable _
          | Unaries _, Any
          | Unaries _, Map _ ->
              (* TODO(T47348228): reject with a new kind of error *)
              Type.parametric name given, sofar
          | ListVariadic _, Map _
          | ListVariadic _, Variable _
          | ListVariadic _, Concrete _ ->
              (* TODO(T47348228): accept w/ new kind of validation *)
              Type.parametric name given, sofar
        in
        match annotation with
        | Type.Primitive name -> invalid_type_parameters ~name ~given:(Concrete [])
        (* natural variadics *)
        | Type.Parametric { name = "typing.Protocol"; _ }
        | Type.Parametric { name = "typing.Generic"; _ } ->
            annotation, sofar
        | Type.Parametric { name; parameters } -> invalid_type_parameters ~name ~given:parameters
        | _ -> annotation, sofar
      in
      { Type.Transform.transformed_annotation; new_state }
  end)
  in
  InvalidTypeParametersTransform.visit [] annotation


let parse_annotation
    ?(allow_untracked = false)
    ?(allow_invalid_type_parameters = false)
    ?(allow_primitives_from_empty_stubs = false)
    ({ aliases; module_definition; _ } as resolution)
    expression
  =
  let expression = Expression.delocalize expression in
  let aliases name =
    if allow_invalid_type_parameters then
      aliases name
    else
      match aliases name with
      | Some (Type.TypeAlias alias) ->
          check_invalid_type_parameters resolution alias
          |> snd
          |> fun alias -> Some (Type.TypeAlias alias)
      | result -> result
  in
  let parsed = Type.create ~aliases expression in
  let annotation =
    if allow_primitives_from_empty_stubs then
      parsed
    else
      let constraints = function
        | Type.Primitive name ->
            let originates_from_empty_stub =
              Reference.create name
              |> fun reference -> Module.from_empty_stub ~reference ~module_definition
            in
            if originates_from_empty_stub then
              Some Type.Any
            else
              None
        | _ -> None
      in
      Type.instantiate parsed ~constraints
  in
  if contains_untracked resolution annotation && not allow_untracked then
    Type.Top
  else if not allow_invalid_type_parameters then
    check_invalid_type_parameters resolution annotation |> snd
  else
    annotation


let join resolution = full_order resolution |> TypeOrder.join

let meet resolution = full_order resolution |> TypeOrder.meet

let widen resolution = full_order resolution |> TypeOrder.widen

(* In general, python expressions can be self-referential. This resolution only checks literals and
   annotations found in the resolution map, without resolving expressions. *)
let rec resolve_literal ({ class_definition; _ } as resolution) expression =
  let open Ast.Expression in
  let is_concrete_constructable class_type =
    primitive_name class_type
    >>= class_definition
    >>| (fun { Node.value = { Class.name; _ }; _ } -> Reference.show name)
    >>= variables ~default:(Some (ClassHierarchy.Unaries [])) resolution
    >>| ClassHierarchy.equal_variables (ClassHierarchy.Unaries [])
    |> Option.value ~default:false
  in
  match Node.value expression with
  | Await expression -> resolve_literal resolution expression |> Type.awaitable_value
  | BooleanOperator { BooleanOperator.left; right; _ } ->
      let annotation =
        join resolution (resolve_literal resolution left) (resolve_literal resolution right)
      in
      if Type.is_concrete annotation then annotation else Type.Any
  | Call { callee = { Node.value = Name name; _ } as callee; _ }
    when Expression.is_simple_name name ->
      let class_type = parse_annotation resolution callee in
      if is_concrete_constructable class_type then
        class_type
      else
        Type.Top
  | Call _
  | Name _
    when Expression.has_identifier_base expression ->
      let class_type = parse_annotation resolution expression in
      (* None is a special type that doesn't have a constructor. *)
      if Type.is_none class_type then
        Type.none
      else if is_concrete_constructable class_type then
        Type.meta class_type
      else
        Type.Top
  | Complex _ -> Type.complex
  | Dictionary { Dictionary.entries; keywords = [] } ->
      let key_annotation, value_annotation =
        let join_entry (key_annotation, value_annotation) { Dictionary.key; value } =
          ( join resolution key_annotation (resolve_literal resolution key),
            join resolution value_annotation (resolve_literal resolution value) )
        in
        List.fold ~init:(Type.Bottom, Type.Bottom) ~f:join_entry entries
      in
      if Type.is_concrete key_annotation && Type.is_concrete value_annotation then
        Type.dictionary ~key:key_annotation ~value:value_annotation
      else
        Type.Any
  | False -> Type.bool
  | Float _ -> Type.float
  | Integer _ -> Type.integer
  | List elements ->
      let parameter =
        let join sofar element = join resolution sofar (resolve_literal resolution element) in
        List.fold ~init:Type.Bottom ~f:join elements
      in
      if Type.is_concrete parameter then Type.list parameter else Type.Any
  | Set elements ->
      let parameter =
        let join sofar element = join resolution sofar (resolve_literal resolution element) in
        List.fold ~init:Type.Bottom ~f:join elements
      in
      if Type.is_concrete parameter then Type.set parameter else Type.Any
  | String { StringLiteral.kind; _ } -> (
    match kind with
    | StringLiteral.Bytes -> Type.bytes
    | _ -> Type.string )
  | Ternary { Ternary.target; alternative; _ } ->
      let annotation =
        join
          resolution
          (resolve_literal resolution target)
          (resolve_literal resolution alternative)
      in
      if Type.is_concrete annotation then annotation else Type.Any
  | True -> Type.bool
  | Tuple elements -> Type.tuple (List.map elements ~f:(resolve_literal resolution))
  | Expression.Yield _ -> Type.yield Type.Any
  | _ -> Type.Any


let undecorated_signature { undecorated_signature; _ } = undecorated_signature

let aliases { aliases; _ } = aliases

let module_definition { module_definition; _ } = module_definition

module FunctionDefinitionsCache = struct
  let cache = Reference.Table.create ()

  let enabled =
    (* Only enable this in nonincremental mode for now. *)
    ref false


  let enable () = enabled := true

  let set key value = Hashtbl.set cache ~key ~data:value

  let get key =
    if !enabled then
      Hashtbl.find cache key
    else
      None


  let invalidate () = Hashtbl.clear cache
end

let function_definitions resolution reference =
  match FunctionDefinitionsCache.get reference with
  | Some result -> result
  | None ->
      let qualifier =
        let rec qualifier ~lead ~tail =
          match tail with
          | head :: tail ->
              let new_lead = Reference.create ~prefix:lead head in
              if Option.is_none (module_definition resolution new_lead) then
                lead
              else
                qualifier ~lead:new_lead ~tail
          | _ -> lead
        in
        qualifier ~lead:Reference.empty ~tail:(Reference.as_list reference)
      in
      let result =
        Ast.SharedMemory.Sources.get qualifier
        >>| Preprocessing.defines ~include_stubs:true ~include_nested:true
        >>| List.filter ~f:(fun { Node.value = { Define.signature = { name; _ }; _ }; _ } ->
                Reference.equal reference name)
      in
      FunctionDefinitionsCache.set reference result;
      result


let is_suppressed_module resolution reference =
  let module_definition = module_definition resolution in
  Module.from_empty_stub ~reference ~module_definition


let solve_less_or_equal ?(any_is_bottom = false) resolution ~constraints ~left ~right =
  { (full_order resolution) with any_is_bottom }
  |> TypeOrder.solve_less_or_equal ~constraints ~left ~right


let constraints_solution_exists ~left ~right resolution =
  not
    ( solve_less_or_equal resolution ~left ~right ~constraints:TypeConstraints.empty
    |> List.filter_map ~f:(TypeOrder.OrderedConstraints.solve ~order:(full_order resolution))
    |> List.is_empty )


let consistent_solution_exists resolution =
  TypeOrder.consistent_solution_exists (full_order resolution)


let solve_constraints ?(any_is_bottom = false) resolution =
  let order = { (full_order resolution) with any_is_bottom } in
  TypeOrder.OrderedConstraints.solve ~order


let partial_solve_constraints resolution =
  TypeOrder.OrderedConstraints.extract_partial_solution ~order:(full_order resolution)


let less_or_equal resolution = full_order resolution |> TypeOrder.always_less_or_equal

let is_compatible_with resolution = full_order resolution |> TypeOrder.is_compatible_with

let is_instantiated { class_hierarchy; _ } = ClassHierarchy.is_instantiated class_hierarchy

let is_string_to_any_mapping resolution annotation =
  (* TODO(T40377122): Remove special-casing of Dict[str, Any] in strict. *)
  less_or_equal
    resolution
    ~left:annotation
    ~right:(Type.optional (Type.parametric "typing.Mapping" (Concrete [Type.string; Type.Any])))


let parse_reference ?(allow_untracked = false) resolution reference =
  Expression.from_reference ~location:Location.Reference.any reference
  |> parse_annotation ~allow_untracked ~allow_invalid_type_parameters:true resolution


let parse_as_list_variadic ({ aliases; _ } as resolution) name =
  let parsed_as_type_variable =
    parse_annotation resolution name ~allow_untracked:true |> Type.primitive_name >>= aliases
  in
  match parsed_as_type_variable with
  | Some (VariableAlias (ListVariadic variable)) -> Some variable
  | _ -> None


let parse_as_list_variadic_map_operator { aliases; _ } expression =
  Expression.delocalize expression |> Type.OrderedTypes.Map.parse ~aliases


let parse_as_parameter_specification_instance_annotation
    { aliases; _ }
    ~variable_parameter_annotation
    ~keywords_parameter_annotation
  =
  let variable_parameter_annotation, keywords_parameter_annotation =
    ( Expression.delocalize variable_parameter_annotation,
      Expression.delocalize keywords_parameter_annotation )
  in
  Type.Variable.Variadic.Parameters.parse_instance_annotation
    ~aliases
    ~variable_parameter_annotation
    ~keywords_parameter_annotation


let is_invariance_mismatch ({ class_hierarchy; _ } as resolution) ~left ~right =
  match left, right with
  | ( Type.Parametric { name = left_name; parameters = left_parameters },
      Type.Parametric { name = right_name; parameters = right_parameters } )
    when Identifier.equal left_name right_name ->
      let zipped =
        match
          ClassHierarchy.variables class_hierarchy left_name, left_parameters, right_parameters
        with
        | Some (Unaries variables), Concrete left_parameters, Concrete right_parameters -> (
            List.map3
              variables
              left_parameters
              right_parameters
              ~f:(fun { variance; _ } left right -> variance, left, right)
            |> function
            | List.Or_unequal_lengths.Ok list -> Some list
            | _ -> None )
        | Some (ListVariadic _), _, _ ->
            (* TODO(T47346673): Do this check when list variadics have variance *)
            None
        | _ -> None
      in
      let due_to_invariant_variable (variance, left, right) =
        match variance with
        | Type.Variable.Unary.Invariant -> less_or_equal resolution ~left ~right
        | _ -> false
      in
      zipped >>| List.exists ~f:due_to_invariant_variable |> Option.value ~default:false
  | _ -> false


let resolve_exports resolution ~reference =
  (* Resolve exports. Fixpoint is necessary due to export/module name conflicts: P59503092 *)
  let widening_threshold = 25 in
  let rec resolve_exports_fixpoint ~reference ~visited ~count =
    if Set.mem visited reference || count > widening_threshold then
      reference
    else
      let rec resolve_exports ~lead ~tail =
        match tail with
        | head :: tail ->
            module_definition resolution (Reference.create_from_list lead)
            >>| (fun definition ->
                  match Module.aliased_export definition (Reference.create head) with
                  | Some export -> Reference.combine export (Reference.create_from_list tail)
                  | _ -> resolve_exports ~lead:(lead @ [head]) ~tail)
            |> Option.value ~default:reference
        | _ -> reference
      in
      match Reference.as_list reference with
      | head :: tail ->
          let exported_reference = resolve_exports ~lead:[head] ~tail in
          if Reference.is_strict_prefix ~prefix:reference exported_reference then
            reference
          else
            resolve_exports_fixpoint
              ~reference:exported_reference
              ~visited:(Set.add visited reference)
              ~count:(count + 1)
      | _ -> reference
  in
  resolve_exports_fixpoint ~reference ~visited:Reference.Set.empty ~count:0


let solve_ordered_types_less_or_equal resolution =
  TypeOrder.solve_ordered_types_less_or_equal (full_order resolution)


(* There isn't a great way of testing whether a file only contains tests in Python. Due to the
   difficulty of handling nested classes within test cases, etc., we use the heuristic that a class
   which inherits from unittest.TestCase indicates that the entire file is a test file. *)
let source_is_unit_test resolution ~source =
  let is_unittest { Node.value = { Class.name; _ }; _ } =
    let annotation = parse_reference resolution name in
    less_or_equal resolution ~left:annotation ~right:(Type.Primitive "unittest.case.TestCase")
  in
  List.exists (Preprocessing.classes source) ~f:is_unittest


let class_extends_placeholder_stub_class
    ({ module_definition; _ } as resolution)
    { Class.bases; _ }
  =
  let is_from_placeholder_stub { Expression.Call.Argument.value; _ } =
    let parsed =
      parse_annotation
        ~allow_untracked:true
        ~allow_invalid_type_parameters:true
        ~allow_primitives_from_empty_stubs:true
        resolution
        value
    in
    match parsed with
    | Type.Primitive primitive
    | Parametric { name = primitive; _ } ->
        Reference.create primitive
        |> fun reference -> Module.from_empty_stub ~reference ~module_definition
    | _ -> false
  in
  List.exists bases ~f:is_from_placeholder_stub


let global { global; _ } = global
