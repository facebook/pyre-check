(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Pyre
open Statement

module Type = struct
  include Type

  let compare = Type.namespace_insensitive_compare
end

(* The `name` field conflicts with that defined in incompatible_type. *)
type missing_annotation = {
  name: Reference.t;
  annotation: Type.t option;
  given_annotation: Type.t option;
  evidence_locations: Location.WithPath.t list;
  thrown_at_source: bool;
}
[@@deriving compare, sexp, show, hash]

type revealed_local = {
  name: Reference.t;
  annotation: Annotation.t;
}
[@@deriving compare, sexp, show, hash]

type class_kind =
  | Class
  | Enumeration
  | Protocol of Reference.t
  | Abstract of Reference.t
[@@deriving compare, sexp, show, hash]

type invalid_class_instantiation =
  | AbstractClassInstantiation of {
      class_name: Reference.t;
      abstract_methods: string list;
    }
  | ProtocolInstantiation of Reference.t
[@@deriving compare, sexp, show, hash]

type module_reference =
  | ExplicitModule of ModulePath.t
  | ImplicitModule of Reference.t
[@@deriving compare, sexp, show, hash]

type class_origin =
  | ClassType of Type.t
  | ClassInUnion of {
      unions: Type.t list;
      index: int;
    }
[@@deriving compare, sexp, show, hash]

type origin =
  | Class of {
      class_origin: class_origin;
      parent_module_path: ModulePath.t option;
    }
  | Module of module_reference

and analysis_failure =
  | UnexpectedUndefinedType of string
  | FixpointThresholdReached of { define: Reference.t }

and mismatch = {
  actual: Type.t;
  expected: Type.t;
  due_to_invariance: bool;
}

and annotation_and_parent = {
  parent: Identifier.t;
  annotation: Type.t;
}

and typed_dictionary_field_mismatch =
  | RequirednessMismatch of {
      required_field_class: Identifier.t;
      non_required_field_class: Identifier.t;
      field_name: Identifier.t;
    }
  | TypeMismatch of {
      field_name: Identifier.t;
      annotation_and_parent1: annotation_and_parent;
      annotation_and_parent2: annotation_and_parent;
    }

and typed_dictionary_initialization_mismatch =
  | MissingRequiredField of {
      field_name: Identifier.t;
      class_name: Identifier.t;
    }
  | FieldTypeMismatch of {
      field_name: Identifier.t;
      class_name: Identifier.t;
      expected_type: Type.t;
      actual_type: Type.t;
    }
  | UndefinedField of {
      field_name: Identifier.t;
      class_name: Identifier.t;
    }

and incompatible_type = {
  name: Reference.t;
  mismatch: mismatch;
}

and invalid_argument =
  | Keyword of {
      expression: Expression.t option;
      annotation: Type.t;
      require_string_keys: bool;
    }
  | ConcreteVariable of {
      expression: Expression.t option;
      annotation: Type.t;
    }
  | VariableArgumentsWithUnpackableType of {
      variable: Type.OrderedTypes.t;
      mismatch: SignatureSelectionTypes.mismatch_with_unpackable_type;
    }

and precondition_mismatch =
  | Found of mismatch
  | NotFound of Type.t Type.Callable.Parameter.t

and override =
  | StrengthenedPrecondition of precondition_mismatch
  | WeakenedPostcondition of mismatch

and unpack_problem =
  | UnacceptableType of Type.t
  | CountMismatch of int

and type_variable_origin =
  | ClassToplevel
  | Define
  | Toplevel

and type_variance_origin =
  | Parameter
  | Return
  | Inheritance of Type.t

and illegal_action_on_incomplete_type =
  | Naming
  | Calling
  | AttributeAccess of Identifier.t

and override_kind =
  | Method
  | Attribute

and invalid_inheritance =
  | ClassName of Identifier.t
  | NonMethodFunction of Identifier.t
  | UninheritableType of {
      annotation: Type.t;
      is_parent_class_typed_dictionary: bool;
    }
  | TypedDictionarySuperclassCollision of typed_dictionary_field_mismatch

and invalid_override_kind =
  | Final
  | StaticSuper
  | StaticOverride
  | NothingOverridden
  | IllegalOverrideDecorator

and invalid_assignment_kind =
  | FinalAttribute of Reference.t
  | ClassVariable of {
      class_variable: Identifier.t;
      class_name: Identifier.t;
    }
  | ReadOnly of Reference.t

and invalid_type_kind =
  | FinalNested of Type.t
  | FinalParameter of Identifier.t
  | InvalidType of {
      annotation: Type.t;
      expected: string;
    }
  | NestedAlias of Identifier.t
  | NestedTypeVariables of Type.Variable.t
  | SingleExplicit of Type.t
  | InvalidLiteral of Reference.t

and unawaited_awaitable = {
  references: Reference.t list;
  expression: Expression.t;
}

and undefined_import =
  | UndefinedModule of Reference.t
  | UndefinedName of {
      from: module_reference;
      name: Identifier.t;
    }

and incompatible_overload_kind =
  | ReturnType of {
      implementation_annotation: Type.t;
      name: Reference.t;
      overload_annotation: Type.t;
    }
  | Unmatchable of {
      name: Reference.t;
      matching_overload: Type.t Type.Callable.overload;
      unmatched_location: Location.t;
    }
  | Parameters of {
      name: Reference.t;
      location: Location.t;
    }
  | DifferingDecorators
  | MisplacedOverloadDecorator

and polymorphism_base_class =
  | GenericBase
  | ProtocolBase

and unsupported_operand_kind =
  | Binary of {
      operator_name: Identifier.t;
      left_operand: Type.t;
      right_operand: Type.t;
    }
  | Unary of {
      operator_name: Identifier.t;
      operand: Type.t;
    }

and illegal_annotation_target_kind =
  | InvalidExpression
  | Reassignment

and tuple_concatenation_problem =
  | MultipleVariadics of { variadic_expressions: Expression.t list }
  | UnpackingNonIterable of { annotation: Type.t }
[@@deriving compare, sexp, show, hash]

type invalid_decoration =
  | CouldNotResolve of Expression.t
  | CouldNotResolveArgument of {
      name: Reference.t;
      argument: Expression.t;
    }
  | NonCallableDecoratorFactory of {
      name: Reference.t;
      annotation: Type.t;
    }
  | NonCallableDecorator of {
      name: Reference.t;
      has_arguments: bool;
      annotation: Type.t;
    }
  | DecoratorFactoryFailedToApply of {
      name: Reference.t;
      reason: kind option;
    }
  | ApplicationFailed of {
      name: Reference.t;
      has_arguments: bool;
      reason: kind option;
    }
  | SetterNameMismatch of {
      name: Reference.t;
      actual: string;
      expected: string;
    }

and kind =
  | AnalysisFailure of analysis_failure
  | ParserFailure of string
  | IllegalAnnotationTarget of {
      target: Expression.t;
      kind: illegal_annotation_target_kind;
    }
  | IncompatibleAsyncGeneratorReturnType of Type.t
  | IncompatibleAttributeType of {
      parent: Type.t;
      incompatible_type: incompatible_type;
    }
  | IncompatibleAwaitableType of Type.t
  | IncompatibleConstructorAnnotation of Type.t
  | IncompatibleParameterType of {
      name: Identifier.t option;
      position: int;
      callee: Reference.t option;
      mismatch: mismatch;
    }
  | IncompatibleReturnType of {
      mismatch: mismatch;
      is_implicit: bool;
      is_unimplemented: bool;
      define_location: Location.t;
    }
  | IncompatibleVariableType of {
      incompatible_type: incompatible_type;
      declare_location: Location.WithPath.t;
    }
  | IncompatibleOverload of incompatible_overload_kind
  | IncompleteType of {
      target: Expression.t;
      annotation: Type.t;
      attempted_action: illegal_action_on_incomplete_type;
    }
  | InconsistentOverride of {
      overridden_method: Identifier.t;
      parent: Reference.t;
      override: override;
      override_kind: override_kind;
    }
  | InvalidArgument of invalid_argument
  | InvalidClassInstantiation of invalid_class_instantiation
  | InvalidDecoration of invalid_decoration
  | InvalidException of {
      expression: Expression.t;
      annotation: Type.t;
    }
  | InvalidMethodSignature of {
      annotation: Type.t option;
      name: Identifier.t;
    }
  | InvalidType of invalid_type_kind
  | InvalidTypeParameters of AttributeResolution.type_parameters_mismatch
  | InvalidTypeVariable of {
      annotation: Type.Variable.t;
      origin: type_variable_origin;
    }
  | InvalidTypeVariance of {
      annotation: Type.t;
      origin: type_variance_origin;
    }
  | InvalidInheritance of invalid_inheritance
  | InvalidOverride of {
      parent: Identifier.t;
      decorator: invalid_override_kind;
    }
  | InvalidAssignment of invalid_assignment_kind
  | MissingArgument of {
      callee: Reference.t option;
      parameter: SignatureSelectionTypes.missing_argument;
    }
  | MissingAttributeAnnotation of {
      parent: Type.t;
      missing_annotation: missing_annotation;
    }
  | MissingCaptureAnnotation of Identifier.t
  | MissingGlobalAnnotation of missing_annotation
  | MissingOverloadImplementation of Reference.t
  | MissingParameterAnnotation of missing_annotation
  | MissingReturnAnnotation of missing_annotation
  | MutuallyRecursiveTypeVariables of Reference.t option
  | NotCallable of Type.t
  | PrivateProtocolProperty of {
      name: Identifier.t;
      parent: Type.t;
    }
  | ProhibitedAny of {
      is_type_alias: bool;
      missing_annotation: missing_annotation;
    }
  | RedefinedClass of {
      current_class: Reference.t;
      shadowed_class: Reference.t;
      is_shadowed_class_imported: bool;
    }
  | RedundantCast of Type.t
  | RevealedLocals of revealed_local list
  | RevealedType of {
      expression: Expression.t;
      annotation: Annotation.t;
      qualify: bool;
    }
  | UnsafeCast of {
      expression: Expression.t;
      annotation: Type.t;
    }
  | TooManyArguments of {
      callee: Reference.t option;
      expected: int;
      provided: int;
    }
  | Top
  | TypedDictionaryAccessWithNonLiteral of Identifier.t list
  | TypedDictionaryKeyNotFound of {
      typed_dictionary_name: Identifier.t;
      missing_key: string;
    }
  | UnboundName of Identifier.t
  | UninitializedLocal of Identifier.t
  | UndefinedAttribute of {
      attribute: Identifier.t;
      origin: origin;
    }
  | UndefinedImport of undefined_import
  | UndefinedType of Type.t
  | UnexpectedKeyword of {
      name: Identifier.t;
      callee: Reference.t option;
    }
  | UninitializedAttribute of {
      name: Identifier.t;
      parent: Type.t;
      mismatch: mismatch;
      kind: class_kind;
    }
  | Unpack of {
      expected_count: int;
      unpack_problem: unpack_problem;
    }
  | UnsupportedOperand of unsupported_operand_kind
  | UnusedIgnore of int list
  | UnusedLocalMode of {
      unused_mode: Source.local_mode Node.t;
      actual_mode: Source.local_mode Node.t;
    }
  | TypedDictionaryInvalidOperation of {
      typed_dictionary_name: Identifier.t;
      field_name: Identifier.t;
      method_name: Identifier.t;
      mismatch: mismatch;
    }
  | TypedDictionaryInitializationError of typed_dictionary_initialization_mismatch
  | DuplicateTypeVariables of {
      variable: Type.Variable.t;
      base: polymorphism_base_class;
    }
  | TupleConcatenationError of tuple_concatenation_problem
  (* Additional errors. *)
  (* TODO(T38384376): split this into a separate module. *)
  | DeadStore of Identifier.t
  | Deobfuscation of Source.t
  | UnawaitedAwaitable of unawaited_awaitable
  (* Errors from type operators *)
  | BroadcastError of {
      expression: Expression.t;
      left: Type.t;
      right: Type.t;
    }
[@@deriving compare, sexp, show, hash]

let code_of_kind = function
  | RevealedLocals _ -> -2
  | RevealedType _ -> -1
  | UnusedIgnore _ -> 0
  | Top -> 1
  | MissingParameterAnnotation _ -> 2
  | MissingReturnAnnotation _ -> 3
  | MissingAttributeAnnotation _ -> 4
  | MissingGlobalAnnotation _ -> 5
  | IncompatibleParameterType _ -> 6
  | IncompatibleReturnType _ -> 7
  | IncompatibleAttributeType _ -> 8
  | IncompatibleVariableType _ -> 9
  | UnboundName _ -> 10
  | UndefinedType _ -> 11
  | IncompatibleAwaitableType _ -> 12
  | UninitializedAttribute _ -> 13
  | InconsistentOverride { override; _ } -> (
      match override with
      | StrengthenedPrecondition _ -> 14
      | WeakenedPostcondition _ -> 15)
  | UndefinedAttribute _ -> 16
  | IncompatibleConstructorAnnotation _ -> 17
  | TooManyArguments _ -> 19
  | MissingArgument _ -> 20
  | UndefinedImport _ -> 21
  | RedundantCast _ -> 22
  | Unpack _ -> 23
  | InvalidTypeParameters _ -> 24
  | TypedDictionaryAccessWithNonLiteral _ -> 26
  | TypedDictionaryKeyNotFound _ -> 27
  | UnexpectedKeyword _ -> 28
  | NotCallable _ -> 29
  | AnalysisFailure _ -> 30
  | InvalidType _ -> 31
  | InvalidArgument _ -> 32
  | ProhibitedAny _ -> 33
  | InvalidTypeVariable _ -> 34
  | IllegalAnnotationTarget _ -> 35
  | MutuallyRecursiveTypeVariables _ -> 36
  | IncompleteType _ -> 37
  | InvalidInheritance _ -> 39
  | InvalidOverride _ -> 40
  | InvalidAssignment _ -> 41
  | MissingOverloadImplementation _ -> 42
  | IncompatibleOverload _ -> 43
  | InvalidClassInstantiation _ -> 45
  | InvalidTypeVariance _ -> 46
  | InvalidMethodSignature _ -> 47
  | InvalidException _ -> 48
  | UnsafeCast _ -> 49
  | RedefinedClass _ -> 50
  | UnusedLocalMode _ -> 51
  | PrivateProtocolProperty _ -> 52
  | MissingCaptureAnnotation _ -> 53
  | TypedDictionaryInvalidOperation _ -> 54
  | TypedDictionaryInitializationError _ -> 55
  | InvalidDecoration _ -> 56
  | IncompatibleAsyncGeneratorReturnType _ -> 57
  | UnsupportedOperand _ -> 58
  | DuplicateTypeVariables _ -> 59
  | TupleConcatenationError _ -> 60
  | UninitializedLocal _ -> 61
  | ParserFailure _ -> 404
  (* Additional errors. *)
  | UnawaitedAwaitable _ -> 1001
  | Deobfuscation _ -> 1002
  | DeadStore _ -> 1003
  (* Errors from type operators *)
  | BroadcastError _ -> 2001


let name_of_kind = function
  | AnalysisFailure _ -> "Analysis failure"
  | BroadcastError _ -> "Broadcast error"
  | DuplicateTypeVariables _ -> "Duplicate type variables"
  | ParserFailure _ -> "Parsing failure"
  | DeadStore _ -> "Dead store"
  | Deobfuscation _ -> "Deobfuscation"
  | IllegalAnnotationTarget _ -> "Illegal annotation target"
  | IncompatibleAsyncGeneratorReturnType _ -> "Incompatible async generator return type"
  | IncompatibleAttributeType _ -> "Incompatible attribute type"
  | IncompatibleAwaitableType _ -> "Incompatible awaitable type"
  | IncompatibleConstructorAnnotation _ -> "Incompatible constructor annotation"
  | IncompatibleParameterType _ -> "Incompatible parameter type"
  | IncompatibleReturnType _ -> "Incompatible return type"
  | IncompatibleVariableType _ -> "Incompatible variable type"
  | InconsistentOverride _ -> "Inconsistent override"
  | IncompatibleOverload _ -> "Incompatible overload"
  | IncompleteType _ -> "Incomplete type"
  | InvalidArgument _ -> "Invalid argument"
  | InvalidMethodSignature _ -> "Invalid method signature"
  | InvalidClassInstantiation _ -> "Invalid class instantiation"
  | InvalidDecoration _ -> "Invalid decoration"
  | InvalidException _ -> "Invalid Exception"
  | InvalidType _ -> "Invalid type"
  | InvalidTypeParameters _ -> "Invalid type parameters"
  | InvalidTypeVariable _ -> "Invalid type variable"
  | InvalidTypeVariance _ -> "Invalid type variance"
  | InvalidInheritance _ -> "Invalid inheritance"
  | InvalidOverride _ -> "Invalid override"
  | InvalidAssignment _ -> "Invalid assignment"
  | MissingArgument _ -> "Missing argument"
  | MissingAttributeAnnotation _ -> "Missing attribute annotation"
  | MissingCaptureAnnotation _ -> "Missing annotation for captured variable"
  | MissingGlobalAnnotation _ -> "Missing global annotation"
  | MissingOverloadImplementation _ -> "Missing overload implementation"
  | MissingParameterAnnotation _ -> "Missing parameter annotation"
  | MissingReturnAnnotation _ -> "Missing return annotation"
  | MutuallyRecursiveTypeVariables _ -> "Mutually recursive type variables"
  | NotCallable _ -> "Call error"
  | PrivateProtocolProperty _ -> "Private protocol property"
  | ProhibitedAny _ -> "Prohibited any"
  | RedefinedClass _ -> "Redefined class"
  | RedundantCast _ -> "Redundant cast"
  | RevealedLocals _ -> "Revealed locals"
  | RevealedType _ -> "Revealed type"
  | TooManyArguments _ -> "Too many arguments"
  | Top -> "Undefined error"
  | TypedDictionaryAccessWithNonLiteral _ -> "TypedDict accessed with a non-literal"
  | TypedDictionaryInitializationError _ -> "TypedDict initialization error"
  | TypedDictionaryInvalidOperation _ -> "Invalid TypedDict operation"
  | TypedDictionaryKeyNotFound _ -> "TypedDict accessed with a missing key"
  | UnawaitedAwaitable _ -> "Unawaited awaitable"
  | UnboundName _ -> "Unbound name"
  | UndefinedAttribute _ -> "Undefined attribute"
  | UndefinedImport _ -> "Undefined import"
  | UndefinedType _ -> "Undefined or invalid type"
  | UnexpectedKeyword _ -> "Unexpected keyword"
  | UninitializedAttribute _ -> "Uninitialized attribute"
  | UninitializedLocal _ -> "Uninitialized local"
  | Unpack _ -> "Unable to unpack"
  | UnsafeCast _ -> "Unsafe cast"
  | UnsupportedOperand _ -> "Unsupported operand"
  | UnusedIgnore _ -> "Unused ignore"
  | UnusedLocalMode _ -> "Unused local mode"
  | TupleConcatenationError _ -> "Unable to concatenate tuple"


let weaken_literals kind =
  let weaken_mismatch { actual; expected; due_to_invariance } =
    let actual =
      let weakened_actual = Type.weaken_literals actual in
      if Type.contains_literal expected || Type.equal weakened_actual expected then
        actual
      else
        weakened_actual
    in
    { actual; expected; due_to_invariance }
  in
  (* This is necessary because the `int.__add__` stub now takes type variables, which leads to
     confusing errors *)
  let weaken_int_variable annotation =
    let constraints = function
      | Type.Variable
          {
            Type.Record.Variable.RecordUnary.constraints =
              Type.Record.Variable.Bound (Type.Primitive "int");
            _;
          } ->
          Some Type.integer
      | _ -> None
    in
    Type.instantiate ~constraints annotation
  in
  let weaken_missing_annotation = function
    | { given_annotation = Some given; _ } as missing when Type.contains_literal given -> missing
    | { annotation = Some annotation; _ } as missing ->
        { missing with annotation = Some (weaken_int_variable (Type.weaken_literals annotation)) }
    | missing -> missing
  in
  match kind with
  | IncompatibleAttributeType
      ({ incompatible_type = { mismatch; _ } as incompatible; _ } as attribute) ->
      IncompatibleAttributeType
        {
          attribute with
          incompatible_type = { incompatible with mismatch = weaken_mismatch mismatch };
        }
  | IncompatibleVariableType
      ({ incompatible_type = { mismatch; _ } as incompatible; _ } as variable) ->
      IncompatibleVariableType
        {
          variable with
          incompatible_type = { incompatible with mismatch = weaken_mismatch mismatch };
        }
  | InconsistentOverride ({ override = WeakenedPostcondition mismatch; _ } as inconsistent) ->
      InconsistentOverride
        { inconsistent with override = WeakenedPostcondition (weaken_mismatch mismatch) }
  | InconsistentOverride
      ({ override = StrengthenedPrecondition (Found mismatch); _ } as inconsistent) ->
      InconsistentOverride
        { inconsistent with override = StrengthenedPrecondition (Found (weaken_mismatch mismatch)) }
  | IncompatibleParameterType ({ mismatch; _ } as incompatible) ->
      IncompatibleParameterType { incompatible with mismatch = weaken_mismatch mismatch }
  | IncompatibleReturnType ({ mismatch; _ } as incompatible) ->
      IncompatibleReturnType { incompatible with mismatch = weaken_mismatch mismatch }
  | UninitializedAttribute ({ mismatch; _ } as uninitialized) ->
      UninitializedAttribute { uninitialized with mismatch = weaken_mismatch mismatch }
  | MissingAttributeAnnotation { parent; missing_annotation } ->
      MissingAttributeAnnotation
        { parent; missing_annotation = weaken_missing_annotation missing_annotation }
  | MissingGlobalAnnotation missing_annotation ->
      MissingGlobalAnnotation (weaken_missing_annotation missing_annotation)
  | MissingParameterAnnotation missing_annotation ->
      MissingParameterAnnotation (weaken_missing_annotation missing_annotation)
  | MissingReturnAnnotation missing_annotation ->
      MissingReturnAnnotation (weaken_missing_annotation missing_annotation)
  | ProhibitedAny { is_type_alias; missing_annotation } ->
      ProhibitedAny
        { is_type_alias; missing_annotation = weaken_missing_annotation missing_annotation }
  | UnsupportedOperand (Binary { operator_name; left_operand; right_operand }) ->
      UnsupportedOperand
        (Binary
           {
             operator_name;
             left_operand = Type.weaken_literals left_operand;
             right_operand = Type.weaken_literals right_operand;
           })
  | UnsupportedOperand (Unary { operator_name; operand }) ->
      UnsupportedOperand (Unary { operator_name; operand = Type.weaken_literals operand })
  | Unpack { expected_count; unpack_problem = UnacceptableType annotation } ->
      Unpack { expected_count; unpack_problem = UnacceptableType (Type.weaken_literals annotation) }
  | IncompatibleAwaitableType annotation ->
      IncompatibleAwaitableType (Type.weaken_literals annotation)
  | NotCallable annotation -> NotCallable (Type.weaken_literals annotation)
  | TypedDictionaryInvalidOperation ({ mismatch; _ } as record) ->
      TypedDictionaryInvalidOperation { record with mismatch = weaken_mismatch mismatch }
  | TypedDictionaryInitializationError mismatch ->
      let mismatch =
        match mismatch with
        | FieldTypeMismatch ({ expected_type; actual_type; _ } as field_record) ->
            FieldTypeMismatch
              {
                field_record with
                expected_type = Type.weaken_literals expected_type;
                actual_type = Type.weaken_literals actual_type;
              }
        | _ -> mismatch
      in
      TypedDictionaryInitializationError mismatch
  | _ -> kind


module SimplificationMap = struct
  (* (Lazy) suffix trie for references. Lazy only for leaf nodes. *)
  type node =
    | LazyLeaf of { to_be_expanded: Identifier.t list }
    | Node of { children: node Identifier.Map.t }

  type t = Reference.t Reference.Map.t

  let pp fmt map =
    let pp_one ~key ~data = Format.fprintf fmt "\n  %a -> %a" Reference.pp key Reference.pp data in
    Reference.Map.iteri ~f:pp_one map


  let show map = Format.asprintf "%a" pp map

  let create references =
    let empty_trie = Node { children = Identifier.Map.empty } in
    let add_to_suffix_trie trie reference =
      let rec add sofar node =
        match sofar, node with
        | [], _ -> node
        | _, LazyLeaf { to_be_expanded } when List.equal Identifier.equal sofar to_be_expanded ->
            node
        | _, LazyLeaf { to_be_expanded } -> empty_trie |> add to_be_expanded |> add sofar
        | head :: remaining, Node { children } ->
            let updated_children =
              Identifier.Map.update children head ~f:(fun existing ->
                  match existing with
                  | None -> LazyLeaf { to_be_expanded = remaining }
                  | Some child -> add remaining child)
            in
            Node { children = updated_children }
      in
      let reference_reversed_as_list = Reference.reverse reference |> Reference.as_list in
      add reference_reversed_as_list trie
    in
    (* Idea is that the leaves we could avoid expanding correspond to simplifications. *)
    let extract_simplifications_from_suffix_trie trie =
      let rec extract suffix node collected =
        match node with
        | LazyLeaf { to_be_expanded = [] } -> collected
        | LazyLeaf { to_be_expanded } ->
            let shortened = Reference.create_from_list suffix in
            let dropped = List.rev to_be_expanded |> Reference.create_from_list in
            (Reference.combine dropped shortened, shortened) :: collected
        | Node { children } ->
            Identifier.Map.fold children ~init:collected ~f:(fun ~key ~data ->
                extract (key :: suffix) data)
      in
      extract [] trie []
    in
    List.fold references ~init:empty_trie ~f:add_to_suffix_trie
    |> extract_simplifications_from_suffix_trie
    |> Reference.Map.of_alist_exn
end

let simplify_mismatch ({ actual; expected; _ } as mismatch) =
  let collect_references annotation =
    let module CollectIdentifiers = Type.Transform.Make (struct
      type state = Identifier.t list

      let visit_children_before _ _ = true

      let visit_children_after = false

      let visit sofar annotation =
        let updated =
          match annotation with
          | Type.Parametric { name; _ }
          | Variable { variable = name; _ }
          | Primitive name ->
              name :: sofar
          | _ -> sofar
        in
        { Type.Transform.transformed_annotation = annotation; new_state = updated }
    end)
    in
    fst (CollectIdentifiers.visit [] annotation) |> List.map ~f:Reference.create
  in
  let references = collect_references actual @ collect_references expected in
  let simplification_map = SimplificationMap.create references in
  let simplified_expected = Type.dequalify simplification_map expected in
  let simplified_actual = Type.dequalify simplification_map actual in
  { mismatch with expected = simplified_expected; actual = simplified_actual }


let simplify_kind kind =
  let simplify_incompatible_type incompatible_type =
    { incompatible_type with mismatch = simplify_mismatch incompatible_type.mismatch }
  in
  match kind with
  | IncompatibleAttributeType details ->
      IncompatibleAttributeType
        { details with incompatible_type = simplify_incompatible_type details.incompatible_type }
  | IncompatibleParameterType details ->
      IncompatibleParameterType { details with mismatch = simplify_mismatch details.mismatch }
  | IncompatibleReturnType details ->
      IncompatibleReturnType { details with mismatch = simplify_mismatch details.mismatch }
  | IncompatibleVariableType details ->
      IncompatibleVariableType
        { details with incompatible_type = simplify_incompatible_type details.incompatible_type }
  | _ -> kind


let rec messages ~concise ~signature location kind =
  let {
    Location.WithPath.start = { Location.line = start_line; _ };
    stop = { Location.line = stop_line; _ };
    _;
  }
    =
    location
  in
  let { Node.value = { Define.Signature.name = define_name; _ }; location = define_location } =
    signature
  in
  let show_sanitized_expression expression =
    Ast.Transform.sanitize_expression expression |> Expression.show
  in
  let show_sanitized_optional_expression expression =
    expression >>| show_sanitized_expression >>| Format.sprintf " `%s`" |> Option.value ~default:""
  in
  let ordinal number =
    let suffix =
      if number % 10 = 1 && number % 100 <> 11 then
        "st"
      else if number % 10 = 2 && number % 100 <> 12 then
        "nd"
      else if number % 10 = 3 && number % 100 <> 13 then
        "rd"
      else
        "th"
    in
    string_of_int number ^ suffix
  in
  let invariance_message =
    "See https://pyre-check.org/docs/errors#covariance-and-contravariance"
    ^ " for mutable container errors."
  in
  let pp_type = if concise then Type.pp_concise else Type.pp in
  let pp_reference format reference =
    if concise then
      Reference.last reference |> Reference.create |> Reference.pp_sanitized format
    else
      Reference.pp_sanitized format reference
  in
  let pp_identifier = Identifier.pp_sanitized in
  let kind = weaken_literals kind in
  let kind = simplify_kind kind in
  match kind with
  | AnalysisFailure (UnexpectedUndefinedType annotation) when concise ->
      [Format.asprintf "Terminating analysis - type `%s` not defined." annotation]
  | AnalysisFailure (UnexpectedUndefinedType annotation) ->
      [Format.asprintf "Terminating analysis because type `%s` is not defined." annotation]
  | AnalysisFailure (FixpointThresholdReached { define }) when concise ->
      [
        Format.asprintf
          "Pyre gave up inferring some types - function `%a` was too complex."
          pp_reference
          define;
      ]
  | AnalysisFailure (FixpointThresholdReached { define }) ->
      [
        Format.asprintf
          "Pyre gave up inferring types for some variables because function `%a` was too complex."
          pp_reference
          define;
        "Please simplify the function by factoring out some if-statements or for-loops.";
      ]
  | BroadcastError { expression; left; right } ->
      [
        Format.asprintf
          "Broadcast error at expression `%s`; types `%a` and `%a` cannot be broadcasted together."
          (show_sanitized_expression expression)
          pp_type
          left
          pp_type
          right;
      ]
  | ParserFailure message -> [message]
  | DeadStore name -> [Format.asprintf "Value assigned to `%a` is never used." pp_identifier name]
  | Deobfuscation source -> [Format.asprintf "\n%a" Source.pp source]
  | IllegalAnnotationTarget _ when concise -> ["Target cannot be annotated."]
  | IllegalAnnotationTarget { target; kind } ->
      let reason =
        match kind with
        | InvalidExpression -> ""
        | Reassignment -> " after it is first declared"
      in
      [
        Format.asprintf
          "Target `%s` cannot be annotated%s."
          (show_sanitized_expression target)
          reason;
      ]
  | IncompleteType { target; annotation; attempted_action } ->
      let inferred =
        match annotation with
        | Type.Variable variable when Type.Variable.Unary.is_escaped_and_free variable -> ""
        | _ -> Format.asprintf "`%a` " pp_type annotation
      in
      let consequence =
        match attempted_action with
        | Naming -> "add an explicit annotation."
        | Calling ->
            "cannot be called. "
            ^ "Separate the expression into an assignment and give it an explicit annotation."
        | AttributeAccess attribute ->
            Format.asprintf
              "so attribute `%s` cannot be accessed. Separate the expression into an assignment \
               and give it an explicit annotation."
              attribute
      in
      [
        Format.asprintf
          "Type %sinferred for `%s` is incomplete, %s"
          inferred
          (show_sanitized_expression target)
          consequence;
      ]
  | IncompatibleAsyncGeneratorReturnType annotation ->
      [
        Format.asprintf
          "Expected return annotation to be AsyncGenerator or a superclass but got `%a`."
          pp_type
          annotation;
      ]
  | IncompatibleAwaitableType actual ->
      [Format.asprintf "Expected an awaitable but got `%a`." pp_type actual]
  | IncompatibleOverload kind -> (
      match kind with
      | ReturnType { implementation_annotation; name; overload_annotation } ->
          [
            Format.asprintf
              "The return type of overloaded function `%a` (`%a`) is incompatible with the return \
               type of the implementation (`%a`)."
              pp_reference
              name
              pp_type
              overload_annotation
              pp_type
              implementation_annotation;
          ]
      | Unmatchable { name; _ } when concise ->
          [
            Format.asprintf
              "Signature of overloaded function `%a` will never be matched."
              pp_reference
              name;
          ]
      | Unmatchable { name; matching_overload; unmatched_location } ->
          [
            Format.asprintf
              "The overloaded function `%a` on line %d will never be matched. The signature `%s` \
               is the same or broader."
              pp_reference
              name
              (Location.line unmatched_location)
              (Type.show_concise
                 (Type.Callable
                    { implementation = matching_overload; kind = Anonymous; overloads = [] }));
          ]
      | Parameters { name; location } ->
          [
            Format.asprintf
              "The implementation of `%a` does not accept all possible arguments of overload \
               defined on line `%d`."
              pp_reference
              name
              (Location.line location);
          ]
      | DifferingDecorators ->
          ["This definition does not have the same decorators as the preceding overload(s)."]
      | MisplacedOverloadDecorator ->
          ["The @overload decorator must be the topmost decorator if present."])
  | IncompatibleParameterType
      { name; position; callee; mismatch = { actual; expected; due_to_invariance; _ } } -> (
      let trace =
        if due_to_invariance then
          [Format.asprintf "This call might modify the type of the parameter."; invariance_message]
        else
          []
      in
      let target =
        let parameter =
          match name with
          | Some name -> Format.asprintf "parameter `%a`" pp_identifier name
          | _ -> "positional only parameter"
        in
        let callee =
          match callee with
          | Some callee -> Format.asprintf "call `%a`" pp_reference callee
          | _ -> "anonymous call"
        in
        if concise then
          Format.asprintf "For %s param" (ordinal position)
        else
          Format.asprintf "In %s, for %s %s" callee (ordinal position) parameter
      in
      match Option.map ~f:Reference.as_list callee with
      | Some ["int"; "__add__"]
      | Some ["int"; "__sub__"]
      | Some ["int"; "__mul__"]
      | Some ["int"; "__floordiv__"] ->
          Format.asprintf "%s expected `int` but got `%a`." target pp_type actual :: trace
      | _ ->
          Format.asprintf "%s expected `%a` but got `%a`." target pp_type expected pp_type actual
          :: trace)
  | IncompatibleConstructorAnnotation _ when concise -> ["`__init__` should return `None`."]
  | IncompatibleConstructorAnnotation annotation ->
      [
        Format.asprintf
          "`__init__` is annotated as returning `%a`, but it should return `None`."
          pp_type
          annotation;
      ]
  | IncompatibleReturnType { mismatch = { actual; expected; due_to_invariance; _ }; is_implicit; _ }
    ->
      let trace =
        Format.asprintf
          "Type `%a` expected on line %d, specified on line %d.%s"
          pp_type
          expected
          stop_line
          define_location.Location.start.Location.line
          (if due_to_invariance then " " ^ invariance_message else "")
      in
      let message =
        if is_implicit then
          Format.asprintf "Expected `%a` but got implicit return value of `None`." pp_type expected
        else
          Format.asprintf "Expected `%a` but got `%a`." pp_type expected pp_type actual
      in
      [message; trace]
  | IncompatibleAttributeType
      {
        parent;
        incompatible_type = { name; mismatch = { actual; expected; due_to_invariance; _ } };
      } ->
      let message =
        if concise then
          Format.asprintf "Attribute has type `%a`; used as `%a`." pp_type expected pp_type actual
        else
          Format.asprintf
            "Attribute `%a` declared in class `%a` has type `%a` but is used as type `%a`."
            pp_reference
            name
            pp_type
            parent
            pp_type
            expected
            pp_type
            actual
      in
      let trace =
        if due_to_invariance then
          [invariance_message]
        else
          []
      in
      message :: trace
  | IncompatibleVariableType
      { incompatible_type = { name; mismatch = { actual; expected; due_to_invariance; _ }; _ }; _ }
    ->
      let message =
        if Type.is_tuple expected && not (Type.is_tuple actual) then
          Format.asprintf "Unable to unpack `%a`, expected a tuple." pp_type actual
        else if concise then
          Format.asprintf
            "%a has type `%a`; used as `%a`."
            pp_reference
            name
            pp_type
            expected
            pp_type
            actual
        else
          Format.asprintf
            "%a is declared to have type `%a` but is used as type `%a`."
            pp_reference
            name
            pp_type
            expected
            pp_type
            actual
      in
      let trace =
        if due_to_invariance then
          if Type.equal (Type.weaken_literals actual) expected then
            [
              invariance_message;
              "Hint: To avoid this error, you may need to use explicit type parameters in your \
               constructor: e.g., `Foo[str](\"hello\")` instead of `Foo(\"hello\")`.";
            ]
          else
            [invariance_message]
        else
          []
      in
      message :: trace
  | InconsistentOverride { parent; override; override_kind; overridden_method } ->
      let kind =
        match override_kind with
        | Method -> "method"
        | Attribute -> "attribute"
      in
      let define_name =
        match override_kind with
        | Method -> define_name
        | Attribute -> Reference.create overridden_method
      in
      let detail =
        match override with
        | WeakenedPostcondition { actual; expected; due_to_invariance; _ } ->
            if due_to_invariance then
              invariance_message
            else if [%compare.equal: override_kind] override_kind Attribute then
              Format.asprintf
                "Type `%a` is not a subtype of the overridden attribute `%a`."
                pp_type
                actual
                pp_type
                expected
            else
              Format.asprintf
                "Returned type `%a` is not a subtype of the overridden return `%a`."
                pp_type
                actual
                pp_type
                expected
        | StrengthenedPrecondition (Found { actual; expected; due_to_invariance; _ }) ->
            let extra_detail = if due_to_invariance then " " ^ invariance_message else "" in
            Format.asprintf
              "Parameter of type `%a` is not a supertype of the overridden parameter `%a`.%s"
              pp_type
              actual
              pp_type
              expected
              extra_detail
        | StrengthenedPrecondition (NotFound parameter) ->
            let parameter =
              match parameter with
              | KeywordOnly { name; _ }
              | Named { name; _ } ->
                  Format.asprintf "%a" pp_identifier name
              | _ -> Type.Callable.Parameter.show_concise parameter
            in
            Format.asprintf "Could not find parameter `%s` in overriding signature." parameter
      in
      [
        Format.asprintf
          "`%a` overrides %s defined in `%a` inconsistently.%s"
          pp_reference
          define_name
          kind
          pp_reference
          parent
          (if concise then "" else " " ^ detail);
      ]
  | InvalidArgument argument when concise -> (
      match argument with
      | Keyword { require_string_keys; _ } ->
          [
            Format.sprintf
              "Keyword argument must be a mapping%s."
              (if require_string_keys then " with string keys" else "");
          ]
      | ConcreteVariable _ -> ["Variable argument must be an iterable."]
      | VariableArgumentsWithUnpackableType { variable; mismatch = ConstraintFailure _ } ->
          [
            Format.asprintf
              "Variable argument conflicts with constraints on `%a`."
              Type.OrderedTypes.pp_concise
              variable;
          ]
      | VariableArgumentsWithUnpackableType { variable; mismatch = NotUnpackableType _ } ->
          [
            Format.asprintf
              "Unpacked argument `%a` must have an unpackable type."
              Type.OrderedTypes.pp_concise
              variable;
          ]
      | VariableArgumentsWithUnpackableType { variable; mismatch = CannotConcatenate _ } ->
          [
            Format.asprintf
              "Concatenating multiple variadic tuples for variable `%a` is not yet supported."
              Type.OrderedTypes.pp_concise
              variable;
          ])
  | InvalidArgument argument -> (
      match argument with
      | Keyword { expression; annotation; require_string_keys } ->
          [
            Format.asprintf
              "Keyword argument%s has type `%a` but must be a mapping%s."
              (show_sanitized_optional_expression expression)
              pp_type
              annotation
              (if require_string_keys then " with string keys" else "");
          ]
      | ConcreteVariable { expression; annotation } ->
          [
            Format.asprintf
              "Variable argument%s has type `%a` but must be an iterable."
              (show_sanitized_optional_expression expression)
              pp_type
              annotation;
          ]
      | VariableArgumentsWithUnpackableType { variable; mismatch = ConstraintFailure ordered_types }
        ->
          [
            Format.asprintf
              "Argument types `%a` are not compatible with expected variadic elements `%a`."
              (Type.Record.OrderedTypes.pp_concise ~pp_type)
              ordered_types
              (Type.Record.OrderedTypes.pp_concise ~pp_type)
              variable;
          ]
      | VariableArgumentsWithUnpackableType
          { mismatch = NotUnpackableType { expression; annotation }; _ } ->
          [
            Format.asprintf
              "Unpacked argument%s must have an unpackable type but has type `%a`."
              (show_sanitized_optional_expression expression)
              pp_type
              annotation;
          ]
      | VariableArgumentsWithUnpackableType
          { variable; mismatch = CannotConcatenate unconcatenatable } ->
          let unconcatenatable =
            List.map
              unconcatenatable
              ~f:(Format.asprintf "%a" (Type.Record.OrderedTypes.pp_concise ~pp_type))
            |> String.concat ~sep:", "
          in
          [
            Format.asprintf
              "Variadic type variable `%a` cannot be made to contain `%s`; concatenation of \
               multiple variadic type variables is not yet implemented."
              (Type.Record.OrderedTypes.pp_concise ~pp_type)
              variable
              unconcatenatable;
          ])
  | InvalidDecoration (CouldNotResolve expression) ->
      [
        Format.asprintf
          "Pyre was not able to infer the type of the decorator `%s`."
          (show_sanitized_expression expression);
      ]
  | InvalidDecoration (CouldNotResolveArgument { name; argument }) ->
      let name = Reference.sanitized name |> Reference.show in
      [
        Format.asprintf
          "Pyre was not able to infer the type of argument `%s` to decorator factory `%s`."
          (show_sanitized_expression argument)
          name;
        "This can usually be worked around by extracting your argument into a global variable and \
         providing an explicit type annotation.";
      ]
  | InvalidDecoration (NonCallableDecoratorFactory { name; annotation }) ->
      let name = Reference.sanitized name |> Reference.show in
      [
        Format.asprintf
          "Decorator factory `%s` could not be called, because its type `%a` is not callable."
          name
          pp_type
          annotation;
      ]
  | InvalidDecoration (NonCallableDecorator { name; has_arguments; annotation }) ->
      let name = Reference.sanitized name |> Reference.show in
      let arguments = if has_arguments then "(...)" else "" in
      [
        Format.asprintf
          "Decorator `%s%s` could not be called, because its type `%a` is not callable."
          name
          arguments
          pp_type
          annotation;
      ]
  | InvalidDecoration (DecoratorFactoryFailedToApply { name; reason }) -> (
      let name = Reference.sanitized name |> Reference.show in
      let recurse = messages ~concise ~signature location in
      match reason >>| recurse >>= List.hd with
      | Some inner_message ->
          [Format.asprintf "While applying decorator factory `%s`: %s" name inner_message]
      | None -> [Format.asprintf "Decorator factory `%s` failed to apply." name])
  | InvalidDecoration (ApplicationFailed { name; has_arguments; reason }) -> (
      let name = Reference.sanitized name |> Reference.show in
      let arguments = if has_arguments then "(...)" else "" in
      let recurse = messages ~concise ~signature location in
      match reason >>| recurse >>= List.hd with
      | Some inner_message ->
          [Format.asprintf "While applying decorator `%s%s`: %s" name arguments inner_message]
      | None -> [Format.asprintf "Decorator `%s%s` failed to apply." name arguments])
  | InvalidDecoration (SetterNameMismatch { name; expected; actual }) ->
      let name = Reference.sanitized name |> Reference.show in
      [
        Format.asprintf
          "Invalid property setter `%s`: `%s` does not match decorated method `%s`."
          name
          actual
          expected;
      ]
  | InvalidException { expression; annotation } ->
      [
        Format.asprintf
          "Expression `%s` has type `%a` but must extend BaseException."
          (show_sanitized_expression expression)
          pp_type
          annotation;
      ]
  | InvalidMethodSignature { annotation = Some annotation; name } ->
      [Format.asprintf "`%a` cannot be the type of `%a`." pp_type annotation pp_identifier name]
  | InvalidMethodSignature { name; _ } ->
      [Format.asprintf "Non-static method must specify `%a` parameter." pp_identifier name]
  | InvalidType kind -> (
      match kind with
      | FinalNested annotation ->
          [
            Format.asprintf
              "Expression `%a` is not a valid type. Final cannot be nested."
              pp_type
              annotation;
          ]
      | FinalParameter name ->
          [Format.asprintf "Parameter `%a` cannot be annotated with Final." pp_identifier name]
      | InvalidType { annotation; expected } ->
          if String.is_empty expected then
            [Format.asprintf "Expression `%a` is not a valid type." pp_type annotation]
          else
            [
              Format.asprintf "Expression `%a` is not a valid type." pp_type annotation;
              Format.asprintf "Expected %s." expected;
            ]
      | NestedAlias name ->
          [
            Format.asprintf
              "Expression `%a` is not a valid type. All type alias declarations must be made in \
               the module definition."
              pp_identifier
              name;
          ]
      | NestedTypeVariables variable ->
          [
            Format.asprintf
              "Expression `%a` is not a valid type. Type variables cannot contain other type \
               variables in their constraints."
              Type.Variable.pp_concise
              variable;
          ]
      | SingleExplicit variable ->
          [
            Format.asprintf
              "TypeVar can't have a single explicit constraint. Did you mean `bound=%a`?"
              Expression.pp
              (Type.expression variable);
          ]
      | InvalidLiteral reference ->
          [Format.asprintf "Expression `%a` is not a literal value." Reference.pp reference])
  | InvalidTypeParameters
      {
        name;
        kind =
          AttributeResolution.IncorrectNumberOfParameters
            { expected; actual; can_accept_more_parameters };
      } ->
      let additional =
        let replacement =
          match name with
          | "dict" -> Some "typing.Dict"
          | "list" -> Some "typing.List"
          | "type" -> Some "typing.Type"
          | _ -> None
        in
        replacement
        >>| Format.asprintf ", use `%s` to avoid runtime subscripting errors"
        |> Option.value ~default:""
      in
      if expected > 0 then
        let received =
          if actual = 0 then
            ""
          else
            Format.asprintf ", received %d" actual
        in
        let parameter_count_message =
          Format.asprintf "%s%d" (if can_accept_more_parameters then "at least " else "") expected
        in
        [
          Format.asprintf
            "Generic type `%s` expects %s type parameter%s%s%s."
            name
            parameter_count_message
            (if expected = 1 then "" else "s")
            received
            additional;
        ]
      else
        [Format.asprintf "Non-generic type `%s` cannot take parameters." name]
  | InvalidTypeParameters
      { name = "IntExpression"; kind = AttributeResolution.ViolateConstraints { actual; _ } } ->
      [
        Format.asprintf
          "Type parameter `%a` violates constraints on \
           `pyre_extensions.Add`/`pyre_extensions.Multiply`/`pyre_extensions.Subtract`/`pyre_extensions.Divide`. \
           Add & Multiply & Subtract & Divide only accept type variables with a bound that's a \
           subtype of int."
          pp_type
          actual;
      ]
  | InvalidTypeParameters
      { name; kind = AttributeResolution.ViolateConstraints { expected; actual } } ->
      [
        Format.asprintf
          "Type parameter `%a` violates constraints on `%a` in generic type `%s`."
          pp_type
          actual
          pp_type
          (Type.Variable expected)
          name;
      ]
  | InvalidTypeParameters { name; kind = AttributeResolution.UnexpectedKind { expected; actual } }
    ->
      let expected =
        match expected with
        | Unary expected ->
            Format.asprintf "Single type parameter `%a` expected" Type.pp (Type.Variable expected)
        | ParameterVariadic expected ->
            Format.asprintf
              "Callable parameters expected for parameter specification `%s`"
              (Type.Variable.Variadic.Parameters.name expected)
        | TupleVariadic expected ->
            Format.asprintf "Tuple expected for `%s`" (Type.Variable.Variadic.Tuple.name expected)
      in
      let actual =
        match actual with
        | Single actual -> Format.asprintf "single type `%a`" Type.pp actual
        | CallableParameters actual ->
            Format.asprintf
              "callable parameters `%a`"
              (Type.pp_parameters ~pp_type:Type.pp)
              [CallableParameters actual]
        | Unpacked actual ->
            Format.asprintf
              "variadic `%a`"
              (Type.OrderedTypes.Concatenation.pp_unpackable ~pp_type:Type.pp)
              actual
      in
      [Format.asprintf "%s, but a %s was given for generic type %s." expected actual name]
  | InvalidTypeVariable { annotation; origin } when concise -> (
      let format : ('b, Format.formatter, unit, string) format4 =
        match origin with
        | ClassToplevel -> "Current class isn't generic over `%s`."
        | Define -> "`%s` isn't present in the function's parameters."
        | Toplevel -> "`%s` can only be used to annotate generic classes or functions."
      in
      match annotation with
      | Type.Variable.Unary variable ->
          [Format.asprintf format (Type.show (Type.Variable variable))]
      | Type.Variable.ParameterVariadic variable ->
          let name = Type.Variable.Variadic.Parameters.name variable in
          [Format.asprintf format name]
      | Type.Variable.TupleVariadic variable ->
          let name = Type.Variable.Variadic.Tuple.name variable in
          [Format.asprintf format name])
  | InvalidTypeVariable { annotation; origin } -> (
      (* The explicit annotation is necessary to appease the compiler. *)
      let format : ('b, Format.formatter, unit, string) format4 =
        match origin with
        | ClassToplevel -> "The current class isn't generic with respect to the type variable `%s`."
        | Define -> "The type variable `%s` isn't present in the function's parameters."
        | Toplevel ->
            "The type variable `%s` can only be used to annotate generic classes or functions."
      in
      let detail variable =
        match origin with
        | ClassToplevel ->
            [
              Format.sprintf
                "To reference the type variable, you can modify the class to inherit from \
                 `typing.Generic[%s]`."
                variable;
            ]
        | Define
        | Toplevel ->
            []
      in
      match annotation with
      | Type.Variable.Unary variable ->
          Format.asprintf format (Type.show (Type.Variable variable)) :: detail variable.variable
      | Type.Variable.ParameterVariadic variable ->
          (* We don't give hints for the more complicated cases. *)
          let name = Type.Variable.Variadic.Parameters.name variable in
          [Format.asprintf format name]
      | Type.Variable.TupleVariadic variable ->
          (* We don't give hints for the more complicated cases. *)
          let name = Type.Variable.Variadic.Tuple.name variable in
          [Format.asprintf format name])
  | InvalidTypeVariance { origin; _ } when concise -> (
      match origin with
      | Parameter -> ["Parameter type cannot be covariant."]
      | Return -> ["Return type cannot be contravariant."]
      | Inheritance _ ->
          ["Subclasses cannot use more permissive type variables than their superclasses."])
  | InvalidTypeVariance { annotation; origin } ->
      let formatted =
        match origin with
        | Parameter ->
            Format.asprintf
              "The type variable `%a` is covariant and cannot be a parameter type."
              pp_type
              annotation
        | Return ->
            Format.asprintf
              "The type variable `%a` is contravariant and cannot be a return type."
              pp_type
              annotation
        | Inheritance parent ->
            Format.asprintf
              "The type variable `%a` is incompatible with parent class type variable `%a` because \
               subclasses cannot use more permissive type variables than their superclasses."
              pp_type
              annotation
              pp_type
              parent
      in
      [formatted; "See `https://pyre-check.org/docs/errors#35-invalid-type-variance` for details."]
  | InvalidInheritance invalid_inheritance -> (
      match invalid_inheritance with
      | ClassName class_name ->
          [Format.asprintf "Cannot inherit from final class `%a`." pp_identifier class_name]
      | NonMethodFunction decorator_name ->
          [
            Format.asprintf
              "`%a` cannot be used with non-method functions."
              pp_identifier
              decorator_name;
          ]
      | UninheritableType { annotation; is_parent_class_typed_dictionary } ->
          [
            Format.asprintf
              "`%a` is not a valid parent class%s."
              pp_type
              annotation
              (if is_parent_class_typed_dictionary then
                 " for a typed dictionary. Expected a typed dictionary"
              else
                "");
          ]
      | TypedDictionarySuperclassCollision mismatch -> (
          match mismatch with
          | RequirednessMismatch { required_field_class; non_required_field_class; field_name } ->
              [
                Format.asprintf
                  "`%s` is a required field in base class `%s` and a non-required field in base \
                   class `%s` (because of `total=False`)."
                  field_name
                  required_field_class
                  non_required_field_class;
              ]
          | TypeMismatch
              {
                field_name;
                annotation_and_parent1 = { annotation = annotation1; parent = parent1 };
                annotation_and_parent2 = { annotation = annotation2; parent = parent2 };
              } ->
              [
                Format.asprintf
                  "Field `%s` has type `%a` in base class `%s` and type `%a` in base class `%s`."
                  field_name
                  pp_type
                  annotation1
                  parent1
                  pp_type
                  annotation2
                  parent2;
              ]))
  | InvalidOverride { parent; decorator } -> (
      let preamble, message =
        match decorator with
        | Final -> "", "cannot override final method defined in"
        | StaticSuper -> "Non-static method ", "cannot override a static method defined in"
        | StaticOverride -> "Static method ", "cannot override a non-static method defined in"
        | NothingOverridden ->
            ( "",
              "is decorated with @override, but no method of the same name exists in superclasses \
               of" )
        | IllegalOverrideDecorator ->
            ( "",
              "is illegally decorated with @override: @override may only be applied to methods, \
               but this element is not a method" )
      in
      match decorator with
      | Final
      | StaticSuper
      | StaticOverride
      | NothingOverridden ->
          [
            Format.asprintf
              "%s`%a` %s `%a`."
              preamble
              pp_reference
              define_name
              message
              pp_identifier
              parent;
          ]
      | IllegalOverrideDecorator ->
          [Format.asprintf "%s`%a` %s." preamble pp_reference define_name message])
  | InvalidAssignment kind -> (
      match kind with
      | FinalAttribute name ->
          [Format.asprintf "Cannot reassign final attribute `%a`." pp_reference name]
      | ClassVariable _ when concise -> ["Assigning to class variable through instance."]
      | ClassVariable { class_variable; class_name } ->
          [
            Format.asprintf
              "Assigning to class variable through instance, did you mean to assign to `%a.%a` \
               instead?"
              pp_identifier
              class_name
              pp_identifier
              class_variable;
          ]
      | ReadOnly name ->
          [
            Format.asprintf
              "`%a` cannot be reassigned. It is a read-only property."
              pp_reference
              name;
          ])
  | InvalidClassInstantiation kind -> (
      match kind with
      | ProtocolInstantiation class_name ->
          [Format.asprintf "Cannot instantiate protocol `%a`." pp_reference class_name]
      | AbstractClassInstantiation { class_name; abstract_methods } ->
          let method_message =
            let to_string methods =
              List.map methods ~f:(fun name -> Format.sprintf "`%s`" name)
              |> String.concat ~sep:", "
            in
            let number_to_keep = 3 in
            if List.length abstract_methods <= number_to_keep then
              Format.asprintf
                " with abstract method%s %s."
                (if List.length abstract_methods > 1 then "s" else "")
                (to_string abstract_methods)
            else
              let examples = List.take abstract_methods number_to_keep |> to_string in
              Format.sprintf
                " with %s and %d additional abstract methods."
                examples
                (List.length abstract_methods - number_to_keep)
          in
          [
            Format.asprintf
              "Cannot instantiate abstract class `%a`%s"
              pp_reference
              class_name
              (if concise then "." else method_message);
          ])
  | MissingArgument { parameter = Named name; _ } when concise ->
      [Format.asprintf "Argument `%a` expected." pp_identifier name]
  | MissingArgument { parameter = PositionalOnly index; _ } when concise ->
      [Format.asprintf "Argument `%d` expected." index]
  | MissingArgument { callee; parameter } ->
      let callee =
        match callee with
        | Some name -> Format.asprintf "Call `%a`" pp_reference name
        | _ -> "PositionalOnly call"
      in
      let parameter =
        match parameter with
        | PositionalOnly index -> Printf.sprintf "in position %d" index
        | Named name -> Format.asprintf "`%a`" pp_identifier name
      in
      [Format.asprintf "%s expects argument %s." callee parameter]
  | MissingAttributeAnnotation { missing_annotation = { given_annotation; _ }; _ } when concise ->
      if Option.value_map given_annotation ~f:Type.is_any ~default:false then
        ["Attribute annotation cannot be `Any`."]
      else if given_annotation >>| Type.contains_any |> Option.value ~default:false then
        ["Attribute annotation cannot contain `Any`."]
      else
        ["Attribute must be annotated."]
  | MissingAttributeAnnotation { parent; missing_annotation } -> (
      match missing_annotation with
      | { name; annotation = Some annotation; given_annotation; _ }
        when not (Type.is_concrete annotation) -> (
          match given_annotation with
          | Some given_annotation when Type.is_any given_annotation ->
              [
                Format.asprintf
                  "Attribute `%a` of class `%a` must have a type other than `Any`."
                  pp_reference
                  name
                  pp_type
                  parent;
              ]
          | Some given_annotation when Type.contains_any given_annotation ->
              [
                Format.asprintf
                  "Attribute `%a` of class `%a` must have a type that does not contain `Any`."
                  pp_reference
                  name
                  pp_type
                  parent;
              ]
          | _ ->
              [
                Format.asprintf
                  "Attribute `%a` of class `%a` has no type specified."
                  pp_reference
                  name
                  pp_type
                  parent;
              ])
      | { name; annotation = Some annotation; evidence_locations; given_annotation; _ } -> (
          let trace =
            let evidence_string =
              evidence_locations
              |> List.map ~f:(fun { Location.WithPath.path; start; _ } ->
                     Format.asprintf "%s:%a" path Location.pp_position start)
              |> String.concat ~sep:", "
            in
            Format.asprintf
              "Attribute `%a` declared on line %d, type `%a` deduced from %s."
              pp_reference
              name
              start_line
              pp_type
              annotation
              evidence_string
          in
          match given_annotation with
          | Some given_annotation when Type.is_any given_annotation ->
              [
                Format.asprintf
                  "Attribute `%a` of class `%a` has type `%a` but type `Any` is specified."
                  pp_reference
                  name
                  pp_type
                  parent
                  pp_type
                  annotation;
                trace;
              ]
          | Some given_annotation when Type.contains_any given_annotation ->
              [
                Format.asprintf
                  "Attribute `%a` of class `%a` is used as type `%a` and must have a type that \
                   does not contain `Any`."
                  pp_reference
                  name
                  pp_type
                  parent
                  pp_type
                  annotation;
                trace;
              ]
          | _ ->
              [
                Format.asprintf
                  "Attribute `%a` of class `%a` has type `%a` but no type is specified."
                  pp_reference
                  name
                  pp_type
                  parent
                  pp_type
                  annotation;
                trace;
              ])
      | { name; annotation = None; _ } ->
          [
            Format.asprintf
              "Attribute `%a` of class `%a` has no type specified."
              pp_reference
              name
              pp_type
              parent;
          ])
  | MissingCaptureAnnotation name when concise ->
      [Format.asprintf "Captured variable `%a` is not annotated." Identifier.pp_sanitized name]
  | MissingCaptureAnnotation name ->
      [
        Format.asprintf "Captured variable `%a` is not annotated." Identifier.pp_sanitized name;
        "It will be treated as having type `Any` in this function. Consider annotating the \
         variable where it is first defined in the outer function, or passing the variable from \
         the outer function to the inner function as an additional argument.";
      ]
  | MissingGlobalAnnotation { given_annotation; _ } when concise ->
      if Option.value_map given_annotation ~f:Type.is_any ~default:false then
        ["Global annotation cannot be `Any`."]
      else if given_annotation >>| Type.contains_any |> Option.value ~default:false then
        ["Global annotation cannot contain `Any`."]
      else
        ["Global expression must be annotated."]
  | MissingGlobalAnnotation
      { name; annotation = Some annotation; evidence_locations; given_annotation; _ }
    when Type.is_concrete annotation -> (
      let evidence_string =
        evidence_locations
        |> List.map ~f:(fun { Location.WithPath.path; start; _ } ->
               Format.asprintf "%s:%a" path Location.pp_position start)
        |> String.concat ~sep:", "
      in
      match given_annotation with
      | Some given_annotation when Type.is_any given_annotation ->
          [
            Format.asprintf
              "Globally accessible variable `%a` has type `%a` but type `Any` is specified."
              pp_reference
              name
              pp_type
              annotation;
            Format.asprintf
              "Global variable `%a` declared on line %d, type `%a` deduced from %s."
              pp_reference
              name
              start_line
              pp_type
              annotation
              evidence_string;
          ]
      | Some given_annotation when Type.contains_any given_annotation ->
          [
            Format.asprintf
              "Globally accessible variable `%a` has type `%a` a type must be specified that does \
               not contain `Any`."
              pp_reference
              name
              pp_type
              annotation;
            Format.asprintf
              "Global variable `%a` declared on line %d, type `%a` deduced from %s."
              pp_reference
              name
              start_line
              pp_type
              annotation
              evidence_string;
          ]
      | _ ->
          [
            Format.asprintf
              "Globally accessible variable `%a` has type `%a` but no type is specified."
              pp_reference
              name
              pp_type
              annotation;
            Format.asprintf
              "Global variable `%a` declared on line %d, type `%a` deduced from %s."
              pp_reference
              name
              start_line
              pp_type
              annotation
              evidence_string;
          ])
  | MissingGlobalAnnotation { name; given_annotation; _ } -> (
      match given_annotation with
      | Some given_annotation when Type.is_any given_annotation ->
          [
            Format.asprintf
              "Globally accessible variable `%a` must be specified as type other than `Any`."
              pp_reference
              name;
          ]
      | Some given_annotation when Type.contains_any given_annotation ->
          [
            Format.asprintf
              "Globally accessible variable `%a` must be specified as type that does not contain \
               `Any`."
              pp_reference
              name;
          ]
      | _ ->
          [
            Format.asprintf
              "Globally accessible variable `%a` has no type specified."
              pp_reference
              name;
          ])
  | MissingOverloadImplementation name ->
      [Format.asprintf "Overloaded function `%a` must have an implementation." pp_reference name]
  | MissingParameterAnnotation { given_annotation; _ } when concise ->
      if Option.value_map given_annotation ~f:Type.is_any ~default:false then
        ["Parameter annotation cannot be `Any`."]
      else if given_annotation >>| Type.contains_any |> Option.value ~default:false then
        ["Parameter annotation cannot contain `Any`."]
      else
        ["Parameter must be annotated."]
  | MissingParameterAnnotation { name; annotation = Some annotation; given_annotation; _ }
    when Type.is_concrete annotation -> (
      match given_annotation with
      | Some given_annotation when Type.is_any given_annotation ->
          [
            Format.asprintf
              "Parameter `%a` has type `%a` but type `Any` is specified."
              pp_reference
              name
              pp_type
              annotation;
          ]
      | Some given_annotation when Type.contains_any given_annotation ->
          [
            Format.asprintf
              "Parameter `%a` is used as type `%a` and must have a type that does not contain \
               `Any`."
              pp_reference
              name
              pp_type
              annotation;
          ]
      | _ ->
          [
            Format.asprintf
              "Parameter `%a` has type `%a` but no type is specified."
              pp_reference
              name
              pp_type
              annotation;
          ])
  | MissingParameterAnnotation { name; given_annotation; _ } -> (
      match given_annotation with
      | Some given_annotation when Type.is_any given_annotation ->
          [Format.asprintf "Parameter `%a` must have a type other than `Any`." pp_reference name]
      | Some given_annotation when Type.contains_any given_annotation ->
          [
            Format.asprintf
              "Parameter `%a` must have a type that does not contain `Any`."
              pp_reference
              name;
          ]
      | _ -> [Format.asprintf "Parameter `%a` has no type specified." pp_reference name])
  | MissingReturnAnnotation { given_annotation; _ } when concise ->
      if Option.value_map given_annotation ~f:Type.is_any ~default:false then
        ["Return annotation cannot be `Any`."]
      else if given_annotation >>| Type.contains_any |> Option.value ~default:false then
        ["Return annotation cannot contain `Any`."]
      else
        ["Return type must be annotated."]
  | MissingReturnAnnotation
      { annotation = Some annotation; evidence_locations; given_annotation; _ }
    when Type.is_concrete annotation -> (
      let trace =
        let evidence_string =
          evidence_locations
          |> List.map ~f:(Format.asprintf "%a" Location.WithPath.pp_line)
          |> String.concat ~sep:", "
        in
        Format.asprintf
          "Type `%a` was returned on %s %s, return type should be specified on line %d."
          pp_type
          annotation
          (if List.length evidence_locations > 1 then "lines" else "line")
          evidence_string
          start_line
      in
      match given_annotation with
      | Some given_annotation when Type.is_any given_annotation ->
          [Format.asprintf "Returning `%a` but type `Any` is specified." pp_type annotation; trace]
      | Some given_annotation when Type.contains_any given_annotation ->
          [
            Format.asprintf
              "Returning `%a` but return type must be specified as type that does not contain \
               `Any`."
              pp_type
              annotation;
            trace;
          ]
      | _ ->
          [
            Format.asprintf "Returning `%a` but no return type is specified." pp_type annotation;
            trace;
          ])
  | MissingReturnAnnotation { given_annotation; _ } -> (
      match given_annotation with
      | Some given_annotation when Type.is_any given_annotation ->
          ["Return type must be specified as type other than `Any`."]
      | Some given_annotation when Type.contains_any given_annotation ->
          ["Return type must be specified as type that does not contain `Any`."]
      | _ -> ["Return type is not specified."])
  | MutuallyRecursiveTypeVariables callee ->
      let callee =
        match callee with
        | Some callee -> Format.asprintf "call `%a`" pp_reference callee
        | _ -> "anonymous call"
      in
      [Format.asprintf "Solving type variables for %s led to infinite recursion." callee]
  | NotCallable
      (Type.Callable { implementation = { parameters = ParameterVariadicTypeVariable _; _ }; _ } as
      annotation) ->
      [
        Format.asprintf
          "`%a` cannot be safely called because the types and kinds of its parameters depend on a \
           type variable."
          pp_type
          annotation;
      ]
  | NotCallable annotation -> [Format.asprintf "`%a` is not a function." pp_type annotation]
  | PrivateProtocolProperty { name; parent } ->
      [Format.asprintf "Protocol `%a` has private property `%a`." pp_type parent pp_identifier name]
  | ProhibitedAny { is_type_alias; missing_annotation = { given_annotation; _ } } when concise ->
      let annotation_kind = if is_type_alias then "Aliased" else "Given" in
      if Option.value_map given_annotation ~f:Type.is_any ~default:false then
        [Format.asprintf "%s annotation cannot be `Any`." annotation_kind]
      else
        [Format.asprintf "%s annotation cannot contain `Any`." annotation_kind]
  | ProhibitedAny
      { missing_annotation = { name; annotation = Some annotation; given_annotation; _ }; _ }
    when Type.is_concrete annotation -> (
      match given_annotation with
      | Some given_annotation when Type.is_any given_annotation ->
          [
            Format.asprintf
              "Expression `%a` has type `%a`; given explicit type cannot be `Any`."
              pp_reference
              name
              pp_type
              annotation;
          ]
      | _ ->
          [
            Format.asprintf
              "Expression `%a` is used as type `%a`; given explicit type cannot contain `Any`."
              pp_reference
              name
              pp_type
              annotation;
          ])
  | ProhibitedAny { is_type_alias = false; missing_annotation = { name; given_annotation; _ } } -> (
      match given_annotation with
      | Some given_annotation when Type.is_any given_annotation ->
          [Format.asprintf "Explicit annotation for `%a` cannot be `Any`." pp_reference name]
      | _ ->
          [Format.asprintf "Explicit annotation for `%a` cannot contain `Any`." pp_reference name])
  | ProhibitedAny { is_type_alias = true; missing_annotation = { name; given_annotation; _ } } -> (
      match given_annotation with
      | Some Type.Any -> [Format.asprintf "`%a` cannot alias to `Any`." pp_reference name]
      | _ -> [Format.asprintf "`%a` cannot alias to a type containing `Any`." pp_reference name])
  | RedefinedClass { shadowed_class; _ } when concise ->
      [Format.asprintf "Class `%a` redefined" pp_reference shadowed_class]
  | RedefinedClass { current_class; shadowed_class; is_shadowed_class_imported } ->
      [
        Format.asprintf
          "Class `%a` conflicts with %sclass `%a`."
          pp_reference
          current_class
          (if is_shadowed_class_imported then "imported " else "")
          pp_reference
          shadowed_class;
      ]
  | RedundantCast _ when concise -> ["The cast is redundant."]
  | RedundantCast annotation ->
      [Format.asprintf "The value being cast is already of type `%a`." pp_type annotation]
  | RevealedLocals revealed_locals ->
      let show_revealed_local { name; annotation } =
        Format.asprintf
          "    %s: %s"
          (Reference.show_sanitized name)
          (Annotation.display_as_revealed_type annotation)
      in
      [
        Format.asprintf
          "Revealed local types are:\n%s"
          (List.map revealed_locals ~f:show_revealed_local |> String.concat ~sep:"\n");
      ]
  | RevealedType { expression; annotation; _ } ->
      [
        Format.asprintf
          "Revealed type for `%s` is %s."
          (show_sanitized_expression expression)
          (Annotation.display_as_revealed_type annotation);
      ]
  | UnsupportedOperand (Binary { operator_name; left_operand; right_operand }) ->
      [
        Format.asprintf
          "`%s` is not supported for operand types `%a` and `%a`."
          operator_name
          pp_type
          left_operand
          pp_type
          right_operand;
      ]
  | UnsupportedOperand (Unary { operator_name; operand }) ->
      [
        Format.asprintf
          "`%s` is not supported for right operand type `%a`."
          operator_name
          pp_type
          operand;
      ]
  | UnsafeCast { expression; annotation } when concise ->
      [
        Format.asprintf
          "`safe_cast` `%a` is not a subclass of `%s`."
          pp_type
          annotation
          (show_sanitized_expression expression);
      ]
  | UnsafeCast { expression; annotation } ->
      [
        Format.asprintf
          "`safe_cast` is only permitted to widen the type of `%s`. `%a` is not a super type of \
           `%s`."
          (show_sanitized_expression expression)
          pp_type
          annotation
          (show_sanitized_expression expression);
      ]
  | TooManyArguments { expected; _ } when concise ->
      [
        Format.asprintf
          "Expected %d positional argument%s."
          expected
          (if expected <> 1 then "s" else "");
      ]
  | TooManyArguments { callee; expected; provided } ->
      let callee =
        match callee with
        | Some name -> Format.asprintf "Call `%a`" pp_reference name
        | _ -> "PositionalOnly call"
      in
      [
        Format.asprintf
          "%s expects %d positional argument%s, %d %s provided."
          callee
          expected
          (if expected <> 1 then "s" else "")
          provided
          (if provided > 1 then "were" else "was");
      ]
  | Top -> ["Problem with analysis."]
  | TupleConcatenationError (UnpackingNonIterable { annotation }) ->
      [Format.asprintf "Expected to unpack an iterable, but got `%a`." pp_type annotation]
  | TupleConcatenationError (MultipleVariadics { variadic_expressions }) ->
      [
        Format.asprintf
          "Concatenation not yet support for multiple variadic tuples: `%s`."
          (String.concat ~sep:", " (List.map ~f:show_sanitized_expression variadic_expressions));
      ]
  | TypedDictionaryAccessWithNonLiteral acceptable_keys ->
      let explanation =
        let acceptable_keys =
          List.map acceptable_keys ~f:(Format.sprintf "'%s'") |> String.concat ~sep:", "
        in
        (* Use heuristic to limit message from going crazy. *)
        if (not concise) && String.length acceptable_keys < 80 then
          Format.asprintf " Expected one of (%s)." acceptable_keys
        else
          ""
      in
      [Format.asprintf "TypedDict key must be a string literal.%s" explanation]
  | TypedDictionaryKeyNotFound { typed_dictionary_name; missing_key } ->
      if String.equal typed_dictionary_name "$anonymous" then
        [Format.asprintf "TypedDict has no key `%s`." missing_key]
      else
        [
          Format.asprintf
            "TypedDict `%a` has no key `%s`."
            String.pp
            typed_dictionary_name
            missing_key;
        ]
  | TypedDictionaryInvalidOperation { typed_dictionary_name; field_name; method_name; mismatch } ->
      if List.mem ["pop"; "__delitem__"] method_name ~equal:String.equal then
        [
          Format.asprintf
            "Cannot %s required field `%s` from TypedDict%s."
            (if String.equal method_name "pop" then "`pop`" else "delete")
            field_name
            (if String.equal typed_dictionary_name "$anonymous" then
               ""
            else
              Format.asprintf " `%s`" typed_dictionary_name);
        ]
      else
        [
          Format.asprintf
            "Expected `%a` to be assigned to `%s` field `%s` but got `%a`."
            pp_type
            mismatch.expected
            typed_dictionary_name
            field_name
            pp_type
            mismatch.actual;
        ]
  | TypedDictionaryInitializationError mismatch -> (
      match mismatch with
      | MissingRequiredField { field_name; class_name } ->
          [Format.asprintf "Missing required field `%s` for TypedDict `%s`." field_name class_name]
      | FieldTypeMismatch { field_name; expected_type; actual_type; class_name } ->
          [
            Format.asprintf
              "Expected type `%a` for `%s` field `%s` but got `%a`."
              pp_type
              expected_type
              class_name
              field_name
              pp_type
              actual_type;
          ]
      | UndefinedField { field_name; class_name } ->
          [Format.asprintf "TypedDict `%s` has no field `%s`." class_name field_name])
  | Unpack { expected_count; unpack_problem } -> (
      match unpack_problem with
      | UnacceptableType bad_type ->
          [Format.asprintf "Unable to unpack `%a` into %d values." pp_type bad_type expected_count]
      | CountMismatch actual_count ->
          let value_message =
            if actual_count = 1 then
              "single value"
            else
              Format.sprintf "%d values" actual_count
          in
          [Format.sprintf "Unable to unpack %s, %d were expected." value_message expected_count])
  | UnawaitedAwaitable { references = []; expression } ->
      [
        Format.asprintf "`%s` is never awaited." (show_sanitized_expression expression);
        Format.asprintf
          "`%s` is defined on line %d"
          (show_sanitized_expression expression)
          start_line;
      ]
  | UnawaitedAwaitable { references; expression } ->
      let name =
        references
        |> List.map ~f:(fun reference ->
               Format.asprintf "`%s`" (Reference.show_sanitized reference))
        |> String.concat ~sep:", "
      in
      [
        Format.asprintf "Awaitable assigned to %s is never awaited." name;
        Format.asprintf
          "`%s` is defined on line %d"
          (show_sanitized_expression expression)
          start_line;
      ]
  | DuplicateTypeVariables { variable; base } -> (
      let format : ('b, Format.formatter, unit, string) format4 =
        match base with
        | GenericBase -> "Duplicate type variable `%s` in Generic[...]."
        | ProtocolBase -> "Duplicate type variable `%s` in Protocol[...]."
      in
      match variable with
      | Type.Variable.Unary { Type.Record.Variable.RecordUnary.variable = name; _ } ->
          [Format.asprintf format name]
      | Type.Variable.ParameterVariadic variable ->
          let name = Type.Variable.Variadic.Parameters.name variable in
          [Format.asprintf format name]
      | Type.Variable.TupleVariadic variable ->
          let name = Type.Variable.Variadic.Tuple.name variable in
          [Format.asprintf format name])
  | UnboundName name when concise ->
      [Format.asprintf "Name `%a` is used but not defined." Identifier.pp_sanitized name]
  | UnboundName name ->
      [
        Format.asprintf
          "Name `%a` is used but not defined in the current scope."
          Identifier.pp_sanitized
          name;
        "Did you forget to import it or assign to it?";
      ]
  | UninitializedLocal name when concise ->
      [Format.asprintf "`%a` is undefined, or not always defined." Identifier.pp_sanitized name]
  | UninitializedLocal name ->
      [
        Format.asprintf
          "Local variable `%a` is undefined, or not always defined."
          Identifier.pp_sanitized
          name;
        "Check if the variable is defined in all preceding branches of logic.";
      ]
  | UndefinedAttribute { attribute; origin } -> (
      let private_attribute_warning () =
        if String.is_prefix ~prefix:"__" attribute && not (String.is_suffix ~suffix:"__" attribute)
        then
          Format.asprintf
            " `%s` looks like a private attribute, which is not accessible from outside its parent \
             class."
            attribute
        else
          ""
      in
      let target =
        match origin with
        | Class
            {
              class_origin =
                ClassType
                  ( Callable { kind; _ }
                  (* TODO(T64161566): Don't pretend these are just Callables *)
                  | Parametric
                      {
                        name = "BoundMethod";
                        parameters = [Single (Callable { kind; _ }); Single _];
                      } );
              _;
            } -> (
            match kind with
            | Anonymous -> "Anonymous callable"
            | Named name -> Format.asprintf "Callable `%a`" pp_reference name)
        | Class { class_origin = ClassType annotation; _ } ->
            let annotation, _ = Type.split annotation in
            let name =
              if Type.is_optional_primitive annotation then
                "Optional type"
              else
                Format.asprintf "`%a`" pp_type annotation
            in
            name
        | Class { class_origin = ClassInUnion { unions; index }; _ } ->
            Format.asprintf
              "Item `%a` of `%a`"
              pp_type
              (fst (Type.split (List.nth_exn unions index)))
              pp_type
              (Type.Union unions)
        | Module module_reference ->
            let name =
              match module_reference with
              | ExplicitModule { ModulePath.qualifier; _ } -> qualifier
              | ImplicitModule qualifier -> qualifier
            in
            Format.asprintf "Module `%a`" pp_reference name
      in
      match origin with
      | Class
          {
            class_origin = ClassType class_type;
            parent_module_path = Some { ModulePath.relative; is_stub = true; _ };
          }
        when not (Type.is_optional_primitive class_type) ->
          let stub_trace =
            Format.asprintf
              "`%a` is defined in a stub file at `%s`. Ensure attribute `%a` is defined in the \
               stub file.%s"
              pp_type
              class_type
              relative
              pp_identifier
              attribute
              (private_attribute_warning ())
          in
          [Format.asprintf "%s has no attribute `%a`." target pp_identifier attribute; stub_trace]
      | Module (ExplicitModule { ModulePath.relative; is_stub = true; _ }) ->
          let stub_trace =
            Format.asprintf
              "This module is shadowed by a stub file at `%s`. Ensure `%a` is defined in the stub \
               file."
              relative
              pp_identifier
              attribute
          in
          [Format.asprintf "%s has no attribute `%a`." target pp_identifier attribute; stub_trace]
      | _ ->
          [
            Format.asprintf
              "%s has no attribute `%a`.%s"
              target
              pp_identifier
              attribute
              (private_attribute_warning ());
          ])
  | UndefinedImport (UndefinedModule reference) when concise ->
      [Format.asprintf "Could not find module `%a`." Reference.pp_sanitized reference]
  | UndefinedImport (UndefinedName { from; name }) when concise ->
      let from_name, is_stub =
        match from with
        | ExplicitModule { ModulePath.qualifier; is_stub; _ } -> qualifier, is_stub
        | ImplicitModule qualifier -> qualifier, false
      in
      [
        Format.asprintf
          "Could not find name `%a` in `%a`%s."
          pp_identifier
          name
          Reference.pp_sanitized
          from_name
          (if is_stub then " (stubbed)" else "");
      ]
  | UndefinedImport (UndefinedModule reference) ->
      [
        Format.asprintf
          "Could not find a module corresponding to import `%a`."
          Reference.pp_sanitized
          reference;
        "For common reasons, see \
         https://pyre-check.org/docs/errors/#1821-undefined-name-undefined-import";
      ]
  | UndefinedImport (UndefinedName { from; name }) ->
      let from_name, trace =
        let common_reasons_trace =
          "For common reasons, see \
           https://pyre-check.org/docs/errors/#1821-undefined-name-undefined-import"
        in
        match from with
        | ExplicitModule { ModulePath.qualifier; relative; is_stub = true; _ } ->
            ( qualifier,
              Format.asprintf
                "This module is shadowed by a stub file at `%s`. Ensure `%a` is defined in the \
                 stub file."
                relative
                pp_identifier
                name )
        | ExplicitModule { ModulePath.qualifier; _ } -> qualifier, common_reasons_trace
        | ImplicitModule qualifier -> qualifier, common_reasons_trace
      in
      [
        Format.asprintf
          "Could not find a name `%a` defined in module `%a`."
          pp_identifier
          name
          Reference.pp_sanitized
          from_name;
        trace;
      ]
  | UndefinedType annotation ->
      [Format.asprintf "Annotation `%a` is not defined as a type." pp_type annotation]
  | UnexpectedKeyword { name; _ } when concise ->
      [Format.asprintf "Unexpected keyword argument `%s`." (Identifier.sanitized name)]
  | UnexpectedKeyword { name; callee } ->
      let callee =
        match callee with
        | Some name -> Format.asprintf "call `%a`" pp_reference name
        | _ -> "anonymous call"
      in
      [Format.asprintf "Unexpected keyword argument `%s` to %s." (Identifier.sanitized name) callee]
  | UninitializedAttribute { name; parent; mismatch = { expected; _ }; kind } ->
      let message =
        if concise then
          Format.asprintf "Attribute `%a` is never initialized." pp_identifier name
        else
          match kind with
          | Class
          | Enumeration ->
              let expected =
                match kind with
                | Class -> expected
                | Enumeration -> Type.weaken_literals expected
                | _ -> failwith "impossible"
              in
              Format.asprintf
                "Attribute `%a` is declared in class `%a` to have type `%a` but is never \
                 initialized."
                pp_identifier
                name
                pp_type
                parent
                pp_type
                expected
          | Protocol _
          | Abstract _ ->
              let kind_string, superclass_name =
                match kind with
                | Protocol protocol_name -> "protocol", protocol_name
                | Abstract class_name -> "abstract class", class_name
                | Class
                | Enumeration ->
                    failwith "impossible"
              in
              Format.asprintf
                "Attribute `%a` inherited from %s `%a` in class `%a` to have type `%a` but is \
                 never initialized."
                pp_identifier
                name
                kind_string
                pp_reference
                superclass_name
                pp_type
                parent
                pp_type
                expected
      in
      [message]
  | UnusedIgnore codes ->
      let codes =
        match codes with
        | [] -> ""
        | codes ->
            Format.asprintf "[%s]" (List.map codes ~f:Int.to_string |> String.concat ~sep:", ")
      in
      [
        Format.sprintf
          "The `pyre-ignore%s` or `pyre-fixme%s` comment is not suppressing type errors, please \
           remove it."
          codes
          codes;
      ]
  | UnusedLocalMode { unused_mode; actual_mode } ->
      let mode_string = function
        | { Node.value = Source.Strict; _ } -> "pyre-strict"
        | { Node.value = Source.Unsafe; _ } -> "pyre-unsafe"
        | { Node.value = Source.Declare; _ } -> "pyre-ignore-all-errors"
        | { Node.value = Source.PlaceholderStub; _ } -> "pyre-placeholder-stub"
      in
      [
        Format.asprintf
          "Mode `%s` is unused. This conflicts with `%s` mode set on line %d."
          (mode_string unused_mode)
          (mode_string actual_mode)
          (Location.line (Node.location actual_mode));
      ]


module T = struct
  type t = {
    location: Location.WithModule.t;
    kind: kind;
    signature: Define.Signature.t Node.t;
  }
  [@@deriving compare, sexp, show, hash]
end

include T
include Hashable.Make (T)

let create ~location ~kind ~define =
  let { Node.value = { Define.signature; _ }; location = define_location } = define in
  { location; kind; signature = { Node.value = signature; location = define_location } }


let module_reference { location = { Location.WithModule.module_reference; _ }; _ } =
  module_reference


let code { kind; _ } = code_of_kind kind

let _ = show (* shadowed below *)

let show error = Format.asprintf "%a" pp error

module Instantiated = struct
  type t = {
    line: int;
    column: int;
    stop_line: int;
    stop_column: int;
    path: string;
    code: int;
    name: string;
    description: string;
    long_description: string;
    concise_description: string;
    define: string;
  }
  [@@deriving sexp, compare, show, hash, yojson { strict = false }]

  let location { line; column; stop_line; stop_column; path; _ } =
    { Location.start = { line; column }; stop = { line = stop_line; column = stop_column } }
    |> Location.with_path ~path


  let path { path; _ } = path

  let code { code; _ } = code

  let description { description; _ } = description

  let long_description { long_description; _ } = long_description

  let concise_description { concise_description; _ } = concise_description

  let create
      ~location:
        ({
           Location.WithPath.path;
           start = { Location.line = start_line; column = start_column };
           stop = { Location.line = stop_line; column = stop_column };
         } as location)
      ~kind
      ~signature:({ Node.value = signature; _ } as signature_node)
      ~show_error_traces
      ()
    =
    let kind_name = name_of_kind kind in
    let kind_code = code_of_kind kind in
    let description ~concise ~separator ~show_error_traces =
      let messages = messages ~concise ~signature:signature_node location kind in
      Format.asprintf
        "%s [%d]: %s"
        kind_name
        kind_code
        (if show_error_traces then
           String.concat ~sep:separator messages
        else
          List.nth_exn messages 0)
    in
    {
      line = start_line;
      column = start_column;
      stop_line;
      stop_column;
      path;
      code = kind_code;
      name = kind_name;
      description = description ~show_error_traces ~concise:false ~separator:" ";
      long_description = description ~show_error_traces:true ~concise:false ~separator:"\n";
      concise_description = description ~show_error_traces ~concise:true ~separator:"\n";
      define = Reference.show_sanitized (Reference.delocalize signature.name);
    }
end

let instantiate ~show_error_traces ~lookup { location; kind; signature } =
  Instantiated.create
    ~location:(Location.WithModule.instantiate ~lookup location)
    ~kind
    ~signature
    ~show_error_traces
    ()


module IntSet = Set.Make (struct
  type t = Int.t [@@deriving compare, sexp]
end)

module Set = Set.Make (struct
  type error_t = t [@@deriving compare, sexp]

  type t = error_t [@@deriving compare, sexp]
end)

let due_to_analysis_limitations { kind; _ } =
  let is_due_to_analysis_limitations annotation =
    Type.contains_unknown annotation || Type.is_unbound annotation || Type.is_type_alias annotation
  in
  match kind with
  | IncompatibleAwaitableType actual
  | IncompatibleParameterType { mismatch = { actual; _ }; _ }
  | TypedDictionaryInvalidOperation { mismatch = { actual; _ }; _ }
  | TypedDictionaryInitializationError (FieldTypeMismatch { actual_type = actual; _ })
  | IncompatibleReturnType { mismatch = { actual; _ }; _ }
  | IncompatibleAttributeType { incompatible_type = { mismatch = { actual; _ }; _ }; _ }
  | IncompatibleVariableType { incompatible_type = { mismatch = { actual; _ }; _ }; _ }
  | InconsistentOverride { override = StrengthenedPrecondition (Found { actual; _ }); _ }
  | InconsistentOverride { override = WeakenedPostcondition { actual; _ }; _ }
  | InvalidArgument (Keyword { annotation = actual; _ })
  | InvalidArgument (ConcreteVariable { annotation = actual; _ })
  | InvalidArgument
      (VariableArgumentsWithUnpackableType
        { mismatch = NotUnpackableType { annotation = actual; _ }; _ })
  | InvalidException { annotation = actual; _ }
  | InvalidType (InvalidType { annotation = actual; _ })
  | InvalidType (FinalNested actual)
  | NotCallable actual
  | ProhibitedAny { missing_annotation = { given_annotation = Some actual; _ }; _ }
  | RedundantCast actual
  | UninitializedAttribute { mismatch = { actual; _ }; _ }
  | Unpack { unpack_problem = UnacceptableType actual; _ } ->
      is_due_to_analysis_limitations actual
  | UnsupportedOperand (Binary { left_operand; right_operand; _ }) ->
      is_due_to_analysis_limitations left_operand || is_due_to_analysis_limitations right_operand
  | UnsupportedOperand (Unary { operand; _ }) -> is_due_to_analysis_limitations operand
  | Top -> true
  | UndefinedAttribute { origin = Class { class_origin = ClassType annotation; _ }; _ } ->
      Type.contains_unknown annotation
  | AnalysisFailure _
  | BroadcastError _
  | ParserFailure _
  | DeadStore _
  | Deobfuscation _
  | DuplicateTypeVariables _
  | IllegalAnnotationTarget _
  | IncompatibleAsyncGeneratorReturnType _
  | IncompatibleConstructorAnnotation _
  | InconsistentOverride { override = StrengthenedPrecondition (NotFound _); _ }
  | InvalidArgument (VariableArgumentsWithUnpackableType _)
  | InvalidDecoration _
  | InvalidMethodSignature _
  | InvalidTypeParameters _
  | InvalidTypeVariable _
  | InvalidTypeVariance _
  | InvalidInheritance _
  | InvalidOverride _
  | InvalidAssignment _
  | InvalidClassInstantiation _
  | InvalidType _
  | IncompatibleOverload _
  | IncompleteType _
  | MissingArgument _
  | MissingAttributeAnnotation _
  | MissingCaptureAnnotation _
  | MissingGlobalAnnotation _
  | MissingOverloadImplementation _
  | MissingParameterAnnotation _
  | MissingReturnAnnotation _
  | MutuallyRecursiveTypeVariables _
  | PrivateProtocolProperty _
  | ProhibitedAny _
  | TooManyArguments _
  | TupleConcatenationError _
  | TypedDictionaryAccessWithNonLiteral _
  | TypedDictionaryKeyNotFound _
  | TypedDictionaryInitializationError _
  | Unpack _
  | RedefinedClass _
  | RevealedLocals _
  | RevealedType _
  | UnsafeCast _
  | UnawaitedAwaitable _
  | UnboundName _
  | UninitializedLocal _
  | UndefinedAttribute _
  | UndefinedImport _
  | UndefinedType _
  | UnexpectedKeyword _
  | UnusedIgnore _
  | UnusedLocalMode _ ->
      false


let less_or_equal ~resolution left right =
  let less_or_equal_mismatch left right =
    GlobalResolution.less_or_equal resolution ~left:left.actual ~right:right.actual
    && GlobalResolution.less_or_equal resolution ~left:left.expected ~right:right.expected
  in
  [%compare.equal: Location.WithModule.t] left.location right.location
  &&
  match left.kind, right.kind with
  | AnalysisFailure left, AnalysisFailure right -> [%compare.equal: analysis_failure] left right
  | ( BroadcastError { expression = left_expression; left = first_left; right = first_right },
      BroadcastError { expression = right_expression; left = second_left; right = second_right } )
    when [%compare.equal: Expression.t] left_expression right_expression ->
      GlobalResolution.less_or_equal resolution ~left:first_left ~right:first_right
      && GlobalResolution.less_or_equal resolution ~left:second_left ~right:second_right
  | ParserFailure left_message, ParserFailure right_message ->
      String.equal left_message right_message
  | DeadStore left, DeadStore right -> Identifier.equal left right
  | Deobfuscation left, Deobfuscation right -> [%compare.equal: Source.t] left right
  | ( IllegalAnnotationTarget { target = left_target; kind = left_kind },
      IllegalAnnotationTarget { target = right_target; kind = right_kind } ) -> (
      match left_kind, right_kind with
      | InvalidExpression, InvalidExpression
      | Reassignment, Reassignment ->
          [%compare.equal: Expression.t] left_target right_target
      | _, _ -> false)
  | IncompatibleAsyncGeneratorReturnType left, IncompatibleAsyncGeneratorReturnType right ->
      GlobalResolution.less_or_equal resolution ~left ~right
  | IncompatibleAwaitableType left, IncompatibleAwaitableType right ->
      GlobalResolution.less_or_equal resolution ~left ~right
  | IncompatibleParameterType left, IncompatibleParameterType right
    when Option.equal Identifier.equal_sanitized left.name right.name ->
      less_or_equal_mismatch left.mismatch right.mismatch
  | IncompatibleConstructorAnnotation left, IncompatibleConstructorAnnotation right ->
      GlobalResolution.less_or_equal resolution ~left ~right
  | IncompatibleReturnType left, IncompatibleReturnType right ->
      less_or_equal_mismatch left.mismatch right.mismatch
  | IncompatibleOverload left, IncompatibleOverload right -> (
      match left, right with
      | ReturnType left, ReturnType right ->
          Type.equal left.implementation_annotation right.implementation_annotation
          && Type.equal left.overload_annotation right.overload_annotation
          && Reference.equal left.name right.name
      | Unmatchable left, Unmatchable right -> Reference.equal left.name right.name
      | _, _ -> false)
  | ( IncompleteType
        { target = left_target; annotation = left; attempted_action = left_attempted_action },
      IncompleteType
        { target = right_target; annotation = right; attempted_action = right_attempted_action } )
    when [%compare.equal: Expression.t] left_target right_target
         && [%compare.equal: illegal_action_on_incomplete_type]
              left_attempted_action
              right_attempted_action ->
      GlobalResolution.less_or_equal resolution ~left ~right
  | IncompatibleAttributeType left, IncompatibleAttributeType right
    when Type.equal left.parent right.parent
         && Reference.equal left.incompatible_type.name right.incompatible_type.name ->
      less_or_equal_mismatch left.incompatible_type.mismatch right.incompatible_type.mismatch
  | ( IncompatibleVariableType { incompatible_type = left; _ },
      IncompatibleVariableType { incompatible_type = right; _ } )
    when Reference.equal left.name right.name ->
      less_or_equal_mismatch left.mismatch right.mismatch
  | InconsistentOverride left, InconsistentOverride right -> (
      match left.override, right.override with
      | ( StrengthenedPrecondition (NotFound left_parameter),
          StrengthenedPrecondition (NotFound right_parameter) ) ->
          Type.Callable.Parameter.equal Type.equal left_parameter right_parameter
      | ( StrengthenedPrecondition (Found left_mismatch),
          StrengthenedPrecondition (Found right_mismatch) )
      | WeakenedPostcondition left_mismatch, WeakenedPostcondition right_mismatch ->
          less_or_equal_mismatch left_mismatch right_mismatch
      | _ -> false)
  | InvalidArgument (Keyword left), InvalidArgument (Keyword right)
    when Option.equal [%compare.equal: Expression.t] left.expression right.expression ->
      GlobalResolution.less_or_equal resolution ~left:left.annotation ~right:right.annotation
  | InvalidArgument (ConcreteVariable left), InvalidArgument (ConcreteVariable right)
    when Option.equal [%compare.equal: Expression.t] left.expression right.expression ->
      GlobalResolution.less_or_equal resolution ~left:left.annotation ~right:right.annotation
  | InvalidMethodSignature left, InvalidMethodSignature right -> (
      match left.annotation, right.annotation with
      | Some left, Some right -> GlobalResolution.less_or_equal resolution ~left ~right
      | None, None -> true
      | _ -> false)
  | InvalidType (FinalParameter left), InvalidType (FinalParameter right)
  | InvalidType (NestedAlias left), InvalidType (NestedAlias right) ->
      Identifier.equal left right
  | ( InvalidType (InvalidType { annotation = left; _ }),
      InvalidType (InvalidType { annotation = right; _ }) )
  | InvalidType (FinalNested left), InvalidType (FinalNested right) ->
      GlobalResolution.less_or_equal resolution ~left ~right
  | InvalidTypeParameters left, InvalidTypeParameters right ->
      [%compare.equal: AttributeResolution.type_parameters_mismatch] left right
  | ( InvalidTypeVariable { annotation = left; origin = left_origin },
      InvalidTypeVariable { annotation = right; origin = right_origin } ) ->
      Type.Variable.equal left right
      && [%compare.equal: type_variable_origin] left_origin right_origin
  | ( InvalidTypeVariance { annotation = left; origin = left_origin },
      InvalidTypeVariance { annotation = right; origin = right_origin } ) ->
      GlobalResolution.less_or_equal resolution ~left ~right
      && [%compare.equal: type_variance_origin] left_origin right_origin
  | InvalidInheritance left, InvalidInheritance right -> (
      match left, right with
      | ClassName left, ClassName right
      | NonMethodFunction left, NonMethodFunction right ->
          Identifier.equal_sanitized left right
      | _, _ -> false)
  | ( InvalidOverride { parent = left_parent; decorator = left_decorator },
      InvalidOverride { parent = right_parent; decorator = right_decorator } ) -> (
      match left_decorator, right_decorator with
      | Final, Final
      | StaticSuper, StaticSuper
      | StaticOverride, StaticOverride
      | NothingOverridden, NothingOverridden
      | IllegalOverrideDecorator, IllegalOverrideDecorator ->
          Identifier.equal_sanitized left_parent right_parent
      | _, _ -> false)
  | InvalidAssignment left, InvalidAssignment right -> (
      match left, right with
      | ReadOnly left, ReadOnly right
      | FinalAttribute left, FinalAttribute right ->
          Reference.equal left right
      | ClassVariable left, ClassVariable right ->
          Identifier.equal left.class_variable right.class_variable
          && Identifier.equal left.class_name right.class_name
      | _, _ -> false)
  | ( MissingArgument { callee = left_callee; parameter = Named left_name },
      MissingArgument { callee = right_callee; parameter = Named right_name } ) ->
      Option.equal Reference.equal_sanitized left_callee right_callee
      && Identifier.equal_sanitized left_name right_name
  | ( MissingArgument { callee = left_callee; parameter = PositionalOnly left_index },
      MissingArgument { callee = right_callee; parameter = PositionalOnly right_index } ) ->
      Option.equal Reference.equal_sanitized left_callee right_callee && left_index = right_index
  | MissingCaptureAnnotation left_name, MissingCaptureAnnotation right_name ->
      Identifier.equal_sanitized left_name right_name
  | InvalidDecoration left, InvalidDecoration right ->
      [%compare.equal: invalid_decoration] left right
  | InvalidException left, InvalidException right ->
      GlobalResolution.less_or_equal resolution ~left:left.annotation ~right:right.annotation
  | InvalidClassInstantiation left, InvalidClassInstantiation right -> (
      match left, right with
      | ProtocolInstantiation left_name, ProtocolInstantiation right_name
      | AbstractClassInstantiation { class_name = left_name; _ }, ProtocolInstantiation right_name
        ->
          Reference.equal left_name right_name
      | _, _ -> false)
  | ProhibitedAny { missing_annotation = left; _ }, ProhibitedAny { missing_annotation = right; _ }
  | MissingParameterAnnotation left, MissingParameterAnnotation right
  | MissingReturnAnnotation left, MissingReturnAnnotation right
  | ( MissingAttributeAnnotation { missing_annotation = left; _ },
      MissingAttributeAnnotation { missing_annotation = right; _ } )
  | MissingGlobalAnnotation left, MissingGlobalAnnotation right
    when Reference.equal_sanitized left.name right.name -> (
      match left.annotation, right.annotation with
      | Some left, Some right -> GlobalResolution.less_or_equal resolution ~left ~right
      | None, None -> true
      | _ -> false)
  | MissingOverloadImplementation left, MissingOverloadImplementation right ->
      Reference.equal left right
  | NotCallable left, NotCallable right -> GlobalResolution.less_or_equal resolution ~left ~right
  | RedundantCast left, RedundantCast right ->
      GlobalResolution.less_or_equal resolution ~left ~right
  | RevealedLocals left, RevealedLocals right when List.length left == List.length right ->
      let all =
        List.fold_until
          ~init:true
          ~f:(fun _ x ->
            if not x then
              Stop x
            else
              Continue x)
          ~finish:(fun _ -> true)
      in
      let revealed_local_less_or_equal
          { name = left_name; annotation = left_annotation }
          { name = right_name; annotation = right_annotation }
        =
        [%compare.equal: Reference.t] left_name right_name
        && Annotation.less_or_equal
             ~type_less_or_equal:(GlobalResolution.less_or_equal resolution)
             ~left:left_annotation
             ~right:right_annotation
      in
      all (List.rev_map2_exn left right ~f:revealed_local_less_or_equal)
  | RevealedType left, RevealedType right ->
      [%compare.equal: Expression.t] left.expression right.expression
      && Annotation.less_or_equal
           ~type_less_or_equal:(GlobalResolution.less_or_equal resolution)
           ~left:left.annotation
           ~right:right.annotation
  | TooManyArguments left, TooManyArguments right ->
      Option.equal Reference.equal_sanitized left.callee right.callee
      && left.expected = right.expected
      && left.provided = right.provided
  | ( TypedDictionaryInitializationError
        (FieldTypeMismatch
          {
            field_name = left_field_name;
            class_name = left_class_name;
            actual_type = left_actual_type;
            expected_type = left_expected_type;
          }),
      TypedDictionaryInitializationError
        (FieldTypeMismatch
          {
            field_name = right_field_name;
            class_name = right_class_name;
            actual_type = right_actual_type;
            expected_type = right_expected_type;
          }) ) ->
      Identifier.equal left_field_name right_field_name
      && Identifier.equal left_class_name right_class_name
      && GlobalResolution.less_or_equal resolution ~left:left_actual_type ~right:right_actual_type
      && GlobalResolution.less_or_equal
           resolution
           ~left:left_expected_type
           ~right:right_expected_type
  | UninitializedAttribute left, UninitializedAttribute right when String.equal left.name right.name
    ->
      less_or_equal_mismatch left.mismatch right.mismatch
  | UnawaitedAwaitable left, UnawaitedAwaitable right ->
      [%compare.equal: unawaited_awaitable] left right
  | UnboundName left_name, UnboundName right_name
  | UninitializedLocal left_name, UninitializedLocal right_name ->
      Identifier.equal_sanitized left_name right_name
  | ( DuplicateTypeVariables { variable = left; base = left_base },
      DuplicateTypeVariables { variable = right; base = right_base } ) -> (
      match left_base, right_base with
      | GenericBase, GenericBase
      | ProtocolBase, ProtocolBase ->
          Type.Variable.equal left right
      | _ -> false)
  | UndefinedAttribute left, UndefinedAttribute right
    when Identifier.equal_sanitized left.attribute right.attribute -> (
      match left.origin, right.origin with
      | Class { class_origin = ClassType left; _ }, Class { class_origin = ClassType right; _ } ->
          GlobalResolution.less_or_equal resolution ~left ~right
      | Module (ImplicitModule left), Module (ImplicitModule right)
      | ( Module (ExplicitModule { ModulePath.qualifier = left; _ }),
          Module (ExplicitModule { ModulePath.qualifier = right; _ }) ) ->
          Reference.equal_sanitized left right
      | _ -> false)
  | UndefinedType left, UndefinedType right -> Type.equal left right
  | UnexpectedKeyword left, UnexpectedKeyword right ->
      Option.equal Reference.equal_sanitized left.callee right.callee
      && Identifier.equal left.name right.name
  | UndefinedImport left, UndefinedImport right -> [%compare.equal: undefined_import] left right
  | ( UnsupportedOperand
        (Binary
          {
            operator_name = left_operator_name;
            left_operand = left_operand_for_left;
            right_operand = right_operand_for_left;
          }),
      UnsupportedOperand
        (Binary
          {
            operator_name = right_operator_name;
            left_operand = left_operand_for_right;
            right_operand = right_operand_for_right;
          }) )
    when Identifier.equal_sanitized left_operator_name right_operator_name ->
      GlobalResolution.less_or_equal
        resolution
        ~left:left_operand_for_left
        ~right:left_operand_for_right
      && GlobalResolution.less_or_equal
           resolution
           ~left:right_operand_for_left
           ~right:right_operand_for_right
  | ( UnsupportedOperand (Unary { operator_name = left_operator_name; operand = left_operand }),
      UnsupportedOperand (Unary { operator_name = right_operator_name; operand = right_operand }) )
    when Identifier.equal_sanitized left_operator_name right_operator_name ->
      GlobalResolution.less_or_equal resolution ~left:left_operand ~right:right_operand
  | UnusedIgnore left, UnusedIgnore right ->
      IntSet.is_subset (IntSet.of_list left) ~of_:(IntSet.of_list right)
  | ( UnusedLocalMode { unused_mode = left_unused_mode; actual_mode = left_actual_mode },
      UnusedLocalMode { unused_mode = right_unused_mode; actual_mode = right_actual_mode } ) ->
      [%compare.equal: Source.local_mode] left_unused_mode.Node.value right_unused_mode.Node.value
      && [%compare.equal: Source.local_mode]
           left_actual_mode.Node.value
           right_actual_mode.Node.value
  | ( Unpack { expected_count = left_count; unpack_problem = left_problem },
      Unpack { expected_count = right_count; unpack_problem = right_problem } ) -> (
      left_count = right_count
      &&
      match left_problem, right_problem with
      | UnacceptableType left, UnacceptableType right ->
          GlobalResolution.less_or_equal resolution ~left ~right
      | CountMismatch left, CountMismatch right -> left = right
      | _ -> false)
  | _, Top -> true
  | AnalysisFailure _, _
  | BroadcastError _, _
  | ParserFailure _, _
  | DeadStore _, _
  | Deobfuscation _, _
  | IllegalAnnotationTarget _, _
  | IncompatibleAsyncGeneratorReturnType _, _
  | IncompatibleAttributeType _, _
  | IncompatibleAwaitableType _, _
  | IncompatibleConstructorAnnotation _, _
  | IncompatibleParameterType _, _
  | IncompatibleReturnType _, _
  | IncompatibleOverload _, _
  | IncompleteType _, _
  | IncompatibleVariableType _, _
  | InconsistentOverride _, _
  | InvalidArgument _, _
  | InvalidDecoration _, _
  | InvalidException _, _
  | InvalidMethodSignature _, _
  | InvalidType _, _
  | InvalidTypeParameters _, _
  | InvalidTypeVariable _, _
  | InvalidTypeVariance _, _
  | InvalidInheritance _, _
  | InvalidOverride _, _
  | InvalidAssignment _, _
  | InvalidClassInstantiation _, _
  | MissingArgument _, _
  | MissingAttributeAnnotation _, _
  | MissingCaptureAnnotation _, _
  | MissingGlobalAnnotation _, _
  | MissingOverloadImplementation _, _
  | MissingParameterAnnotation _, _
  | MissingReturnAnnotation _, _
  | MutuallyRecursiveTypeVariables _, _
  | NotCallable _, _
  | PrivateProtocolProperty _, _
  | ProhibitedAny _, _
  | RedefinedClass _, _
  | RedundantCast _, _
  | RevealedLocals _, _
  | RevealedType _, _
  | UnsafeCast _, _
  | TooManyArguments _, _
  | Top, _
  | TupleConcatenationError _, _
  | TypedDictionaryAccessWithNonLiteral _, _
  | TypedDictionaryInvalidOperation _, _
  | TypedDictionaryInitializationError _, _
  | TypedDictionaryKeyNotFound _, _
  | UnawaitedAwaitable _, _
  | UnboundName _, _
  | UninitializedLocal _, _
  | DuplicateTypeVariables _, _
  | UndefinedAttribute _, _
  | UndefinedImport _, _
  | UndefinedType _, _
  | UnexpectedKeyword _, _
  | UninitializedAttribute _, _
  | Unpack _, _
  | UnsupportedOperand _, _
  | UnusedIgnore _, _
  | UnusedLocalMode _, _ ->
      false


let join ~resolution left right =
  let join_mismatch left right =
    {
      expected = GlobalResolution.join resolution left.expected right.expected;
      actual = GlobalResolution.join resolution left.actual right.actual;
      due_to_invariance = left.due_to_invariance || right.due_to_invariance;
    }
  in
  let join_missing_annotation
      (left : missing_annotation) (* Ohcaml... *)
      (right : missing_annotation)
      : missing_annotation
    =
    let join_annotation_options = Option.merge ~f:(GlobalResolution.join resolution) in
    {
      left with
      annotation = join_annotation_options left.annotation right.annotation;
      given_annotation = join_annotation_options left.given_annotation right.given_annotation;
      evidence_locations =
        List.dedup_and_sort
          ~compare:Location.WithPath.compare
          (left.evidence_locations @ right.evidence_locations);
      thrown_at_source = left.thrown_at_source || right.thrown_at_source;
    }
  in
  let kind =
    match left.kind, right.kind with
    | AnalysisFailure left, AnalysisFailure right when [%compare.equal: analysis_failure] left right
      ->
        AnalysisFailure left
    | ( BroadcastError { expression = left_expression; left = first_left; right = first_right },
        BroadcastError { expression = right_expression; left = second_left; right = second_right } )
      when [%compare.equal: Expression.t] left_expression right_expression ->
        BroadcastError
          {
            expression = left_expression;
            left = GlobalResolution.join resolution first_left second_left;
            right = GlobalResolution.join resolution first_right second_right;
          }
    | ParserFailure left_message, ParserFailure right_message
      when String.equal left_message right_message ->
        ParserFailure left_message
    | DeadStore left, DeadStore right when Identifier.equal left right -> DeadStore left
    | Deobfuscation left, Deobfuscation right when [%compare.equal: Source.t] left right ->
        Deobfuscation left
    | ( IllegalAnnotationTarget { target = left; kind = InvalidExpression },
        IllegalAnnotationTarget { target = right; kind = InvalidExpression } )
      when [%compare.equal: Expression.t] left right ->
        IllegalAnnotationTarget { target = left; kind = InvalidExpression }
    | ( IllegalAnnotationTarget { target = left; kind = Reassignment },
        IllegalAnnotationTarget { target = right; kind = Reassignment } )
      when [%compare.equal: Expression.t] left right ->
        IllegalAnnotationTarget { target = left; kind = Reassignment }
    | IncompatibleAsyncGeneratorReturnType left, IncompatibleAsyncGeneratorReturnType right ->
        IncompatibleAsyncGeneratorReturnType (GlobalResolution.join resolution left right)
    | IncompatibleAwaitableType left, IncompatibleAwaitableType right ->
        IncompatibleAwaitableType (GlobalResolution.join resolution left right)
    | ( IncompleteType
          { target = left_target; annotation = left; attempted_action = left_attempted_action },
        IncompleteType
          { target = right_target; annotation = right; attempted_action = right_attempted_action } )
      when [%compare.equal: Expression.t] left_target right_target
           && [%compare.equal: illegal_action_on_incomplete_type]
                left_attempted_action
                right_attempted_action ->
        IncompleteType
          {
            target = left_target;
            annotation = GlobalResolution.join resolution left right;
            attempted_action = left_attempted_action;
          }
    | InvalidTypeParameters left, InvalidTypeParameters right
      when [%compare.equal: AttributeResolution.type_parameters_mismatch] left right ->
        InvalidTypeParameters left
    | ( MissingArgument { callee = left_callee; parameter = Named left_name },
        MissingArgument { callee = right_callee; parameter = Named right_name } )
      when Option.equal Reference.equal_sanitized left_callee right_callee
           && Identifier.equal_sanitized left_name right_name ->
        left.kind
    | ( MissingArgument { callee = left_callee; parameter = PositionalOnly left_index },
        MissingArgument { callee = right_callee; parameter = PositionalOnly right_index } )
      when Option.equal Reference.equal_sanitized left_callee right_callee
           && left_index = right_index ->
        left.kind
    | MissingCaptureAnnotation left_name, MissingCaptureAnnotation right_name
      when Identifier.equal_sanitized left_name right_name ->
        left.kind
    | MissingParameterAnnotation left, MissingParameterAnnotation right
      when Reference.equal_sanitized left.name right.name ->
        MissingParameterAnnotation (join_missing_annotation left right)
    | MissingReturnAnnotation left, MissingReturnAnnotation right ->
        MissingReturnAnnotation (join_missing_annotation left right)
    | MissingAttributeAnnotation left, MissingAttributeAnnotation right
      when Reference.equal_sanitized left.missing_annotation.name right.missing_annotation.name
           && Type.equal left.parent right.parent ->
        MissingAttributeAnnotation
          {
            parent = left.parent;
            missing_annotation =
              join_missing_annotation left.missing_annotation right.missing_annotation;
          }
    | MissingGlobalAnnotation left, MissingGlobalAnnotation right
      when Reference.equal_sanitized left.name right.name ->
        MissingGlobalAnnotation (join_missing_annotation left right)
    | MissingOverloadImplementation left, MissingOverloadImplementation right
      when Reference.equal left right ->
        MissingOverloadImplementation left
    | NotCallable left, NotCallable right ->
        NotCallable (GlobalResolution.join resolution left right)
    | ( ProhibitedAny { is_type_alias = is_type_alias_left; missing_annotation = left },
        ProhibitedAny { is_type_alias = is_type_alias_right; missing_annotation = right } )
      when Bool.equal is_type_alias_left is_type_alias_right ->
        ProhibitedAny
          {
            is_type_alias = is_type_alias_left;
            missing_annotation = join_missing_annotation left right;
          }
    | RedundantCast left, RedundantCast right ->
        RedundantCast (GlobalResolution.join resolution left right)
    | RevealedLocals left, RevealedLocals right
      when List.equal
             (fun { name = left_name; annotation = _ } { name = right_name; annotation = _ } ->
               [%compare.equal: Reference.t] left_name right_name)
             left
             right ->
        let revealed_local_join
            { name = left_name; annotation = left_annotation }
            { name = _; annotation = right_annotation }
          =
          {
            name = left_name;
            annotation =
              Annotation.join
                ~type_join:(GlobalResolution.join resolution)
                left_annotation
                right_annotation;
          }
        in
        RevealedLocals (List.map2_exn ~f:revealed_local_join left right)
    | ( RevealedType
          { annotation = left_annotation; expression = left_expression; qualify = left_qualify },
        RevealedType
          { annotation = right_annotation; expression = right_expression; qualify = right_qualify }
      )
      when [%compare.equal: Expression.t] left_expression right_expression ->
        RevealedType
          {
            expression = left_expression;
            annotation =
              Annotation.join
                ~type_join:(GlobalResolution.join resolution)
                left_annotation
                right_annotation;
            qualify = left_qualify || right_qualify (* lol *);
          }
    | IncompatibleParameterType left, IncompatibleParameterType right
      when Option.equal Identifier.equal_sanitized left.name right.name
           && left.position = right.position
           && Option.equal Reference.equal_sanitized left.callee right.callee ->
        let mismatch = join_mismatch left.mismatch right.mismatch in
        IncompatibleParameterType { left with mismatch }
    | IncompatibleConstructorAnnotation left, IncompatibleConstructorAnnotation right ->
        IncompatibleConstructorAnnotation (GlobalResolution.join resolution left right)
    | IncompatibleReturnType left, IncompatibleReturnType right ->
        IncompatibleReturnType
          {
            mismatch = join_mismatch left.mismatch right.mismatch;
            is_implicit = left.is_implicit && right.is_implicit;
            is_unimplemented = left.is_unimplemented && right.is_unimplemented;
            define_location = right.define_location;
          }
    | IncompatibleAttributeType left, IncompatibleAttributeType right
      when Type.equal left.parent right.parent
           && Reference.equal left.incompatible_type.name right.incompatible_type.name ->
        let mismatch =
          join_mismatch left.incompatible_type.mismatch right.incompatible_type.mismatch
        in
        IncompatibleAttributeType
          { parent = left.parent; incompatible_type = { left.incompatible_type with mismatch } }
    | IncompatibleVariableType left, IncompatibleVariableType right
      when Reference.equal left.incompatible_type.name right.incompatible_type.name ->
        IncompatibleVariableType
          {
            left with
            incompatible_type =
              {
                left.incompatible_type with
                mismatch =
                  join_mismatch left.incompatible_type.mismatch right.incompatible_type.mismatch;
              };
          }
    | ( InconsistentOverride ({ override = StrengthenedPrecondition left_issue; _ } as left),
        InconsistentOverride ({ override = StrengthenedPrecondition right_issue; _ } as right) )
      -> (
        match left_issue, right_issue with
        | Found left_mismatch, Found right_mismatch ->
            let mismatch = join_mismatch left_mismatch right_mismatch in
            InconsistentOverride { left with override = StrengthenedPrecondition (Found mismatch) }
        | NotFound _, _ -> InconsistentOverride left
        | _, NotFound _ -> InconsistentOverride right)
    | ( InconsistentOverride ({ override = WeakenedPostcondition left_mismatch; _ } as left),
        InconsistentOverride { override = WeakenedPostcondition right_mismatch; _ } ) ->
        let mismatch = join_mismatch left_mismatch right_mismatch in
        InconsistentOverride { left with override = WeakenedPostcondition mismatch }
    | InvalidArgument (Keyword left), InvalidArgument (Keyword right)
      when Option.equal [%compare.equal: Expression.t] left.expression right.expression ->
        InvalidArgument
          (Keyword
             {
               left with
               annotation = GlobalResolution.join resolution left.annotation right.annotation;
             })
    | InvalidArgument (ConcreteVariable left), InvalidArgument (ConcreteVariable right)
      when Option.equal [%compare.equal: Expression.t] left.expression right.expression ->
        InvalidArgument
          (ConcreteVariable
             {
               left with
               annotation = GlobalResolution.join resolution left.annotation right.annotation;
             })
    | InvalidAssignment left, InvalidAssignment right
      when [%compare.equal: invalid_assignment_kind] left right ->
        InvalidAssignment left
    | InvalidDecoration left, InvalidDecoration right
      when [%compare.equal: invalid_decoration] left right ->
        InvalidDecoration left
    | InvalidException left, InvalidException right
      when [%compare.equal: Expression.t] left.expression right.expression ->
        InvalidException
          {
            expression = left.expression;
            annotation = GlobalResolution.join resolution left.annotation right.annotation;
          }
    | InvalidMethodSignature left, InvalidMethodSignature right
      when Identifier.equal left.name right.name ->
        InvalidMethodSignature
          {
            left with
            annotation =
              Option.merge ~f:(GlobalResolution.join resolution) left.annotation right.annotation;
          }
    | ( InvalidType (InvalidType { annotation = left; expected }),
        InvalidType (InvalidType { annotation = right; _ }) )
      when Type.equal left right ->
        InvalidType (InvalidType { annotation = left; expected })
    | ( InvalidTypeVariable { annotation = left; origin = left_origin },
        InvalidTypeVariable { annotation = right; origin = right_origin } )
      when Type.Variable.equal left right
           && [%compare.equal: type_variable_origin] left_origin right_origin ->
        InvalidTypeVariable { annotation = left; origin = left_origin }
    | ( InvalidTypeVariance { annotation = left; origin = left_origin },
        InvalidTypeVariance { annotation = right; origin = right_origin } )
      when Type.equal left right && [%compare.equal: type_variance_origin] left_origin right_origin
      ->
        InvalidTypeVariance { annotation = left; origin = left_origin }
    | TooManyArguments left, TooManyArguments right
      when Option.equal Reference.equal_sanitized left.callee right.callee
           && left.expected = right.expected
           && left.provided = right.provided ->
        TooManyArguments left
    | UninitializedAttribute left, UninitializedAttribute right
      when String.equal left.name right.name && Type.equal left.parent right.parent ->
        UninitializedAttribute { left with mismatch = join_mismatch left.mismatch right.mismatch }
    | UnawaitedAwaitable left, UnawaitedAwaitable right
      when [%compare.equal: unawaited_awaitable] left right ->
        UnawaitedAwaitable left
    | UnboundName left_name, UnboundName right_name
      when Identifier.equal_sanitized left_name right_name ->
        left.kind
    | UninitializedLocal left_name, UninitializedLocal right_name
      when Identifier.equal_sanitized left_name right_name ->
        left.kind
    | ( DuplicateTypeVariables { variable = left; base = GenericBase },
        DuplicateTypeVariables { variable = right; base = GenericBase } )
      when Type.Variable.equal left right ->
        DuplicateTypeVariables { variable = left; base = GenericBase }
    | ( DuplicateTypeVariables { variable = left; base = ProtocolBase },
        DuplicateTypeVariables { variable = right; base = ProtocolBase } )
      when Type.Variable.equal left right ->
        DuplicateTypeVariables { variable = left; base = ProtocolBase }
    | ( UndefinedAttribute
          {
            origin = Class { class_origin = ClassType left; parent_module_path = left_module };
            attribute = left_attribute;
          },
        UndefinedAttribute
          {
            origin = Class { class_origin = ClassType right; parent_module_path = right_module };
            attribute = right_attribute;
          } )
      when Identifier.equal_sanitized left_attribute right_attribute
           && Option.equal ModulePath.equal left_module right_module ->
        let annotation = GlobalResolution.join resolution left right in
        UndefinedAttribute
          {
            origin = Class { class_origin = ClassType annotation; parent_module_path = left_module };
            attribute = left_attribute;
          }
    | ( UndefinedAttribute { origin = Module (ImplicitModule left); attribute = left_attribute },
        UndefinedAttribute { origin = Module (ImplicitModule right); attribute = right_attribute } )
      when Identifier.equal_sanitized left_attribute right_attribute
           && Reference.equal_sanitized left right ->
        UndefinedAttribute { origin = Module (ImplicitModule left); attribute = left_attribute }
    | ( UndefinedAttribute { origin = Module (ExplicitModule left); attribute = left_attribute },
        UndefinedAttribute { origin = Module (ExplicitModule right); attribute = right_attribute } )
      when Identifier.equal_sanitized left_attribute right_attribute && ModulePath.equal left right
      ->
        UndefinedAttribute { origin = Module (ExplicitModule left); attribute = left_attribute }
    | UndefinedType left, UndefinedType right when Type.equal left right -> UndefinedType left
    | UnexpectedKeyword left, UnexpectedKeyword right
      when Option.equal Reference.equal_sanitized left.callee right.callee
           && Identifier.equal left.name right.name ->
        UnexpectedKeyword left
    | UndefinedImport left, UndefinedImport right when [%compare.equal: undefined_import] left right
      ->
        UndefinedImport left
    | ( UnsupportedOperand
          (Binary
            ({
               operator_name = left_operator_name;
               left_operand = left_operand_for_left;
               right_operand = right_operand_for_left;
             } as left)),
        UnsupportedOperand
          (Binary
            {
              operator_name = right_operator_name;
              left_operand = left_operand_for_right;
              right_operand = right_operand_for_right;
            }) )
      when Identifier.equal_sanitized left_operator_name right_operator_name ->
        UnsupportedOperand
          (Binary
             {
               left with
               left_operand =
                 GlobalResolution.join resolution left_operand_for_left left_operand_for_right;
               right_operand =
                 GlobalResolution.join resolution right_operand_for_left right_operand_for_right;
             })
    | ( UnsupportedOperand
          (Unary ({ operator_name = left_operator_name; operand = left_operand } as left)),
        UnsupportedOperand (Unary { operator_name = right_operator_name; operand = right_operand })
      )
      when Identifier.equal_sanitized left_operator_name right_operator_name ->
        UnsupportedOperand
          (Unary { left with operand = GlobalResolution.join resolution left_operand right_operand })
    | UnusedIgnore left, UnusedIgnore right ->
        UnusedIgnore (IntSet.to_list (IntSet.union (IntSet.of_list left) (IntSet.of_list right)))
    | ( Unpack { expected_count = left_count; unpack_problem = UnacceptableType left },
        Unpack { expected_count = right_count; unpack_problem = UnacceptableType right } )
      when left_count = right_count ->
        Unpack
          {
            expected_count = left_count;
            unpack_problem = UnacceptableType (GlobalResolution.join resolution left right);
          }
    | ( Unpack { expected_count = left_count; unpack_problem = CountMismatch left },
        Unpack { expected_count = right_count; unpack_problem = CountMismatch right } )
      when left_count = right_count && left = right ->
        Unpack { expected_count = left_count; unpack_problem = CountMismatch left }
    | TypedDictionaryKeyNotFound left, TypedDictionaryKeyNotFound right
      when Identifier.equal left.typed_dictionary_name right.typed_dictionary_name
           && String.equal left.missing_key right.missing_key ->
        TypedDictionaryKeyNotFound left
    | TypedDictionaryAccessWithNonLiteral left, TypedDictionaryAccessWithNonLiteral right
      when List.equal String.equal left right ->
        TypedDictionaryAccessWithNonLiteral left
    | TypedDictionaryInvalidOperation left, TypedDictionaryInvalidOperation right
      when Identifier.equal_sanitized left.typed_dictionary_name right.typed_dictionary_name
           && Identifier.equal_sanitized left.field_name right.field_name
           && Identifier.equal_sanitized left.method_name right.method_name ->
        let mismatch = join_mismatch left.mismatch right.mismatch in
        TypedDictionaryInvalidOperation { left with mismatch }
    | ( TypedDictionaryInitializationError
          (FieldTypeMismatch
            ({
               field_name = left_field_name;
               class_name = left_class_name;
               actual_type = left_actual_type;
               expected_type = left_expected_type;
             } as mismatch)),
        TypedDictionaryInitializationError
          (FieldTypeMismatch
            {
              field_name = right_field_name;
              class_name = right_class_name;
              actual_type = right_actual_type;
              expected_type = right_expected_type;
            }) )
      when Identifier.equal left_field_name right_field_name
           && Identifier.equal left_class_name right_class_name ->
        TypedDictionaryInitializationError
          (FieldTypeMismatch
             {
               mismatch with
               actual_type = GlobalResolution.join resolution left_actual_type right_actual_type;
               expected_type =
                 GlobalResolution.join resolution left_expected_type right_expected_type;
             })
    | ( TypedDictionaryInitializationError
          (MissingRequiredField { field_name = left_field_name; class_name = left_class_name }),
        TypedDictionaryInitializationError
          (MissingRequiredField { field_name = right_field_name; class_name = right_class_name }) )
      when Identifier.equal left_field_name right_field_name
           && Identifier.equal left_class_name right_class_name ->
        left.kind
    | Top, _
    | _, Top ->
        Top
    | AnalysisFailure _, _
    | BroadcastError _, _
    | ParserFailure _, _
    | DeadStore _, _
    | Deobfuscation _, _
    | IllegalAnnotationTarget _, _
    | IncompatibleAsyncGeneratorReturnType _, _
    | IncompatibleAttributeType _, _
    | IncompatibleAwaitableType _, _
    | IncompatibleConstructorAnnotation _, _
    | IncompatibleParameterType _, _
    | IncompatibleReturnType _, _
    | IncompatibleOverload _, _
    | IncompleteType _, _
    | IncompatibleVariableType _, _
    | InconsistentOverride _, _
    | InvalidArgument _, _
    | InvalidDecoration _, _
    | InvalidException _, _
    | InvalidMethodSignature _, _
    | InvalidType _, _
    | InvalidTypeParameters _, _
    | InvalidTypeVariable _, _
    | InvalidTypeVariance _, _
    | InvalidInheritance _, _
    | InvalidOverride _, _
    | InvalidAssignment _, _
    | InvalidClassInstantiation _, _
    | MissingArgument _, _
    | MissingAttributeAnnotation _, _
    | MissingCaptureAnnotation _, _
    | MissingGlobalAnnotation _, _
    | MissingOverloadImplementation _, _
    | MissingParameterAnnotation _, _
    | MissingReturnAnnotation _, _
    | MutuallyRecursiveTypeVariables _, _
    | NotCallable _, _
    | PrivateProtocolProperty _, _
    | ProhibitedAny _, _
    | RedefinedClass _, _
    | RedundantCast _, _
    | RevealedLocals _, _
    | RevealedType _, _
    | UnsafeCast _, _
    | TooManyArguments _, _
    | TupleConcatenationError _, _
    | TypedDictionaryAccessWithNonLiteral _, _
    | TypedDictionaryKeyNotFound _, _
    | TypedDictionaryInvalidOperation _, _
    | TypedDictionaryInitializationError _, _
    | UnawaitedAwaitable _, _
    | UnboundName _, _
    | UninitializedLocal _, _
    | DuplicateTypeVariables _, _
    | UndefinedAttribute _, _
    | UndefinedImport _, _
    | UndefinedType _, _
    | UnexpectedKeyword _, _
    | UninitializedAttribute _, _
    | Unpack _, _
    | UnsupportedOperand _, _
    | UnusedIgnore _, _
    | UnusedLocalMode _, _ ->
        let { location; _ } = left in
        Log.debug
          "Incompatible type in error join at %a: %a %a"
          Location.WithModule.pp
          location
          pp_kind
          left.kind
          pp_kind
          right.kind;
        Top
  in
  let location =
    if Location.WithModule.compare left.location right.location <= 0 then
      left.location
    else
      right.location
  in
  { location; kind; signature = left.signature }


let meet ~resolution:_ left _ =
  (* We do not yet care about meeting errors. *)
  left


let widen ~resolution ~previous ~next ~iteration:_ = join ~resolution previous next

let join_at_define ~resolution errors =
  let error_map = String.Table.create ~size:(List.length errors) () in
  let add_error errors error =
    let add_error_to_map key =
      let update_error = function
        | None -> error
        | Some existing_error ->
            let joined_error = join ~resolution existing_error error in
            if not ([%compare.equal: kind] joined_error.kind Top) then
              joined_error
            else
              existing_error
      in
      String.Table.update error_map key ~f:update_error;
      errors
    in
    match error with
    | { kind = MissingParameterAnnotation { name; _ }; _ }
    | { kind = MissingReturnAnnotation { name; _ }; _ } ->
        add_error_to_map (Reference.show_sanitized name)
    | {
     kind =
       UndefinedAttribute { attribute; origin = Class { class_origin = ClassType annotation; _ } };
     _;
    } ->
        (* Only error once per define on accesses or assigns to an undefined class attribute. *)
        add_error_to_map (attribute ^ Type.show annotation)
    | _ -> error :: errors
  in
  let unjoined_errors = List.fold ~init:[] ~f:add_error errors in
  let joined_errors = String.Table.data error_map in
  (* Preserve the order of the errors as much as possible *)
  List.rev_append unjoined_errors joined_errors


let join_at_source ~resolution errors =
  let key = function
    | { kind = MissingAttributeAnnotation { parent; missing_annotation = { name; _ }; _ }; _ } ->
        Type.show parent ^ Reference.show_sanitized name
    | { kind = MissingGlobalAnnotation { name; _ }; _ } -> Reference.show_sanitized name
    | { kind = MissingOverloadImplementation name; _ } -> Reference.show_sanitized name
    | { kind = UndefinedImport (UndefinedModule name); _ } ->
        Format.asprintf "Unknown[%a]" Reference.pp_sanitized name
    | { kind = UndefinedImport (UndefinedName { name; from }); _ } ->
        let module_qualifier =
          match from with
          | ExplicitModule { ModulePath.qualifier; _ } -> qualifier
          | ImplicitModule qualifier -> qualifier
        in
        Format.asprintf
          "Unknown[%a]"
          Reference.pp_sanitized
          (Reference.create name |> Reference.combine module_qualifier)
    | { kind = UnboundName name; _ }
    | { kind = UndefinedType (Type.Primitive name); _ } ->
        Format.asprintf "Unbound[%s]" name
    | error -> show error
  in
  let add_error errors error =
    let key = key error in
    match Map.find errors key, error.kind with
    | Some { kind = UnboundName _; _ }, UndefinedType _ ->
        (* Swallow up UndefinedType errors when the UnboundName error already exists. *)
        errors
    | Some { kind = UndefinedType _; _ }, UnboundName _ -> Map.set ~key ~data:error errors
    | Some existing_error, _ ->
        let joined_error = join ~resolution existing_error error in
        if not ([%compare.equal: kind] joined_error.kind Top) then
          Map.set ~key ~data:joined_error errors
        else
          errors
    | _ -> Map.set ~key ~data:error errors
  in
  List.fold ~init:String.Map.empty ~f:add_error errors |> Map.data


let deduplicate errors =
  let error_set = Hash_set.create ~size:(List.length errors) () in
  List.iter errors ~f:(Core.Hash_set.add error_set);
  Core.Hash_set.to_list error_set


let filter ~resolution errors =
  let should_filter error =
    let is_mock_error { kind; _ } =
      match kind with
      | IncompatibleAttributeType { incompatible_type = { mismatch = { actual; _ }; _ }; _ }
      | IncompatibleAwaitableType actual
      | IncompatibleParameterType { mismatch = { actual; _ }; _ }
      | IncompatibleReturnType { mismatch = { actual; _ }; _ }
      | IncompatibleVariableType { incompatible_type = { mismatch = { actual; _ }; _ }; _ }
      | TypedDictionaryInvalidOperation { mismatch = { actual; _ }; _ }
      | UndefinedAttribute { origin = Class { class_origin = ClassType actual; _ }; _ } ->
          let is_subclass_of_mock annotation =
            try
              match annotation with
              | Type.Primitive predecessor
              | Type.Parametric { name = predecessor; _ } ->
                  let is_transitive_successor =
                    GlobalResolution.is_transitive_successor
                      ~placeholder_subclass_extends_all:false
                      resolution
                      ~predecessor
                  in
                  is_transitive_successor ~successor:"unittest.mock.Base"
                  || is_transitive_successor ~successor:"mock.Base"
                  (* Special-case mypy's workaround for mocks. *)
                  || is_transitive_successor ~successor:"unittest.mock.NonCallableMock"
                  || is_transitive_successor ~successor:"mock.NonCallableMock"
              | _ -> false
            with
            | ClassHierarchy.Untracked _ -> false
          in
          Type.exists actual ~predicate:is_subclass_of_mock
      | UnexpectedKeyword { callee = Some callee; _ } ->
          String.is_prefix ~prefix:"unittest.mock" (Reference.show callee)
      | _ -> false
    in
    let is_unnecessary_missing_annotation_error { kind; _ } =
      (* Ignore missing annotations thrown at assigns but not thrown where global or attribute was
         originally defined. *)
      match kind with
      | MissingGlobalAnnotation { thrown_at_source; _ }
      | MissingAttributeAnnotation { missing_annotation = { thrown_at_source; _ }; _ } ->
          not thrown_at_source
      | _ -> false
    in
    let is_unknown_callable_error { kind; _ } =
      (* TODO(T41494196): Remove when we have AnyCallable escape hatch. *)
      match kind with
      | InconsistentOverride
          { override = StrengthenedPrecondition (Found { expected; actual; _ }); _ }
      | InconsistentOverride { override = WeakenedPostcondition { expected; actual; _ }; _ }
      | IncompatibleParameterType { mismatch = { expected; actual; _ }; _ }
      | IncompatibleReturnType { mismatch = { expected; actual; _ }; _ }
      | IncompatibleAttributeType
          { incompatible_type = { mismatch = { expected; actual; _ }; _ }; _ }
      | TypedDictionaryInvalidOperation { mismatch = { expected; actual; _ }; _ }
      | IncompatibleVariableType
          { incompatible_type = { mismatch = { expected; actual; _ }; _ }; _ } -> (
          match actual with
          | Type.Callable _ ->
              GlobalResolution.less_or_equal
                resolution
                ~left:(Type.Callable.create ~annotation:Type.Top ())
                ~right:expected
          | _ -> false)
      | _ -> false
    in
    let is_callable_attribute_error { kind; _ } =
      (* TODO(T53616545): Remove once our decorators are more expressive. *)
      match kind with
      | UndefinedAttribute
          { origin = Class { class_origin = ClassType (Callable _); _ }; attribute = "command" } ->
          true
      (* We also need to filter errors for common mocking patterns. *)
      | UndefinedAttribute
          {
            origin =
              Class
                {
                  class_origin = ClassType (Callable _ | Parametric { name = "BoundMethod"; _ });
                  _;
                };
            attribute =
              ( "assert_not_called" | "assert_called_once" | "assert_called_once_with"
              | "reset_mock" | "assert_has_calls" | "assert_any_call" );
          } ->
          true
      | UndefinedAttribute
          { origin = Class { class_origin = ClassType (Callable { kind = Named name; _ }); _ }; _ }
        ->
          String.equal (Reference.last name) "patch"
      | _ -> false
    in
    let is_stub_error { kind; location = { Location.WithModule.module_reference; _ }; _ } =
      match kind with
      | UninitializedAttribute _
      | MissingOverloadImplementation _ -> (
          let ast_environment = GlobalResolution.ast_environment resolution in
          match AstEnvironment.ReadOnly.get_module_path ast_environment module_reference with
          | Some { ModulePath.is_stub; _ } -> is_stub
          | _ -> false)
      | _ -> false
    in
    let is_invalid_abstract_error { kind; _ } =
      match kind with
      | InvalidClassInstantiation (AbstractClassInstantiation { class_name; _ }) -> (
          match Reference.show class_name with
          | "int"
          | "float"
          | "bool" ->
              true
          | _ -> false)
      | _ -> false
    in
    is_stub_error error
    || is_mock_error error
    || is_unnecessary_missing_annotation_error error
    || is_unknown_callable_error error
    || is_callable_attribute_error error
    || is_invalid_abstract_error error
  in
  List.filter ~f:(fun error -> not (should_filter error)) errors


let suppress ~mode ~ignore_codes error =
  let suppress_in_strict ({ kind; _ } as error) =
    if due_to_analysis_limitations error then
      true
    else
      match kind with
      | IncompleteType _ ->
          (* TODO(T42467236): Ungate this when ready to codemod upgrade *)
          true
      | _ -> false
  in
  let suppress_in_default ({ kind; signature = { Node.value = signature; _ }; _ } as error) =
    match kind with
    | InconsistentOverride { override = WeakenedPostcondition { actual = Type.Top; _ }; _ } -> false
    | InconsistentOverride
        { override = StrengthenedPrecondition (Found { expected = Type.Variable _; _ }); _ } ->
        true
    | InvalidDecoration (CouldNotResolve _)
    | InvalidDecoration (CouldNotResolveArgument _) ->
        true
    | InvalidTypeParameters
        { kind = AttributeResolution.IncorrectNumberOfParameters { actual = 0; _ }; _ } ->
        true
    | IncompleteType _ ->
        (* TODO(T42467236): Ungate this when ready to codemod upgrade *)
        true
    | MissingCaptureAnnotation _ -> true
    | MissingReturnAnnotation _
    | MissingParameterAnnotation _
    | MissingAttributeAnnotation _
    | MissingGlobalAnnotation _
    | ProhibitedAny _
    | Unpack { unpack_problem = UnacceptableType Type.Any; _ }
    | Unpack { unpack_problem = UnacceptableType Type.Top; _ } ->
        true
    | UndefinedImport _ -> false
    | RevealedLocals _ -> false
    | RevealedType _ -> false
    | UnsafeCast _ -> false
    | IncompatibleReturnType { is_unimplemented = true; _ } -> true
    | _ ->
        due_to_analysis_limitations error
        || Define.Signature.is_untyped signature
           && not
                (Define.Signature.is_toplevel signature
                || Define.Signature.is_class_toplevel signature)
  in
  try
    let suppress_by_code error = List.exists ignore_codes ~f:(( = ) (code error)) in
    match mode with
    | Source.Debug -> false
    | Source.Strict -> suppress_in_strict error || suppress_by_code error
    | Source.Unsafe -> suppress_in_default error || suppress_by_code error
    | Source.Declare -> true
  with
  | ClassHierarchy.Untracked annotation ->
      Log.warning "`%s` not found in the type order." annotation;
      false


let dequalify
    dequalify_map
    ~resolution
    ({
       kind;
       signature = { Node.location; value = { parameters; return_annotation; _ } as signature };
       _;
     } as error)
  =
  let dequalify = Type.dequalify dequalify_map in
  let dequalify_identifier = Type.dequalify_identifier dequalify_map in
  let dequalify_reference = Type.dequalify_reference dequalify_map in
  let dequalify_annotation = Annotation.dequalify dequalify_map in
  let dequalify_class_kind (kind : class_kind) =
    match kind with
    | Class
    | Enumeration ->
        kind
    | Protocol reference -> Protocol (dequalify_reference reference)
    | Abstract reference -> Abstract (dequalify_reference reference)
  in
  let dequalify_invalid_class_instantiation = function
    | ProtocolInstantiation reference -> ProtocolInstantiation (dequalify_reference reference)
    | AbstractClassInstantiation { class_name; abstract_methods } ->
        AbstractClassInstantiation { class_name = dequalify_reference class_name; abstract_methods }
  in
  let dequalify_invalid_inheritance = function
    | ClassName name -> ClassName (dequalify_identifier name)
    | NonMethodFunction name -> NonMethodFunction (dequalify_identifier name)
    | UninheritableType { annotation; is_parent_class_typed_dictionary } ->
        UninheritableType { annotation = dequalify annotation; is_parent_class_typed_dictionary }
    | TypedDictionarySuperclassCollision mismatch ->
        TypedDictionarySuperclassCollision
          (match mismatch with
          | RequirednessMismatch { required_field_class; non_required_field_class; field_name } ->
              RequirednessMismatch
                {
                  required_field_class = dequalify_identifier required_field_class;
                  non_required_field_class = dequalify_identifier non_required_field_class;
                  field_name = dequalify_identifier field_name;
                }
          | TypeMismatch
              {
                field_name;
                annotation_and_parent1 = { annotation = annotation1; parent = parent1 };
                annotation_and_parent2 = { annotation = annotation2; parent = parent2 };
              } ->
              TypeMismatch
                {
                  field_name = dequalify_identifier field_name;
                  annotation_and_parent1 =
                    { annotation = dequalify annotation1; parent = dequalify_identifier parent1 };
                  annotation_and_parent2 =
                    { annotation = dequalify annotation2; parent = dequalify_identifier parent2 };
                })
  in
  let dequalify_invalid_assignment = function
    | FinalAttribute attribute -> FinalAttribute (dequalify_reference attribute)
    | ClassVariable { class_variable; class_name } ->
        ClassVariable { class_name = dequalify_identifier class_name; class_variable }
    | ReadOnly attribute -> ReadOnly (dequalify_reference attribute)
  in
  let dequalify_type_variance_origin = function
    | Parameter -> Parameter
    | Return -> Return
    | Inheritance annotation -> Inheritance (dequalify annotation)
  in
  let dequalify_incompatible_overload_kind = function
    | ReturnType { implementation_annotation; name; overload_annotation } ->
        ReturnType
          {
            implementation_annotation = dequalify implementation_annotation;
            name = dequalify_reference name;
            overload_annotation = dequalify overload_annotation;
          }
    | Unmatchable { name; matching_overload; unmatched_location } ->
        Unmatchable { name = dequalify_reference name; matching_overload; unmatched_location }
    | Parameters { name; location } -> Parameters { name = dequalify_reference name; location }
    | DifferingDecorators -> DifferingDecorators
    | MisplacedOverloadDecorator -> MisplacedOverloadDecorator
  in
  let dequalify_invalid_type_parameters { AttributeResolution.name; kind } =
    let dequalify_generic_type_problems = function
      | AttributeResolution.ViolateConstraints { actual; expected } ->
          AttributeResolution.ViolateConstraints
            {
              actual = dequalify actual;
              expected = Type.Variable.Unary.dequalify ~dequalify_map expected;
            }
      | AttributeResolution.UnexpectedKind { actual; expected } ->
          AttributeResolution.UnexpectedKind
            { actual; expected = Type.Variable.dequalify dequalify_map expected }
      | AttributeResolution.IncorrectNumberOfParameters _ as problem -> problem
    in
    {
      AttributeResolution.name = dequalify_identifier name;
      kind = dequalify_generic_type_problems kind;
    }
  in
  let dequalify_mismatch ({ actual; expected; _ } as mismatch) =
    { mismatch with actual = dequalify actual; expected = dequalify expected }
  in
  let kind =
    match kind with
    | AnalysisFailure annotation -> AnalysisFailure annotation
    | BroadcastError { expression; left; right } ->
        BroadcastError { expression; left = dequalify left; right = dequalify right }
    | DeadStore name -> DeadStore name
    | Deobfuscation left -> Deobfuscation left
    | IllegalAnnotationTarget { target = left; kind } ->
        IllegalAnnotationTarget { target = left; kind }
    | IncompatibleAsyncGeneratorReturnType actual ->
        IncompatibleAsyncGeneratorReturnType (dequalify actual)
    | IncompatibleAwaitableType actual -> IncompatibleAwaitableType (dequalify actual)
    | IncompatibleConstructorAnnotation annotation ->
        IncompatibleConstructorAnnotation (dequalify annotation)
    | IncompatibleOverload kind -> IncompatibleOverload (dequalify_incompatible_overload_kind kind)
    | IncompleteType { target; annotation; attempted_action } ->
        IncompleteType { target; annotation = dequalify annotation; attempted_action }
    | InvalidArgument (Keyword { expression; annotation; require_string_keys }) ->
        InvalidArgument
          (Keyword { expression; annotation = dequalify annotation; require_string_keys })
    | InvalidArgument (ConcreteVariable { expression; annotation }) ->
        InvalidArgument (ConcreteVariable { expression; annotation = dequalify annotation })
    | InvalidArgument (VariableArgumentsWithUnpackableType { variable; mismatch }) ->
        let mismatch =
          match mismatch with
          | NotUnpackableType { expression; annotation } ->
              SignatureSelectionTypes.NotUnpackableType
                { expression; annotation = dequalify annotation }
          | _ ->
              (* TODO(T45656387): Implement dequalify for ordered_types *)
              mismatch
        in
        InvalidArgument (VariableArgumentsWithUnpackableType { variable; mismatch })
    | InvalidException { expression; annotation } ->
        InvalidException { expression; annotation = dequalify annotation }
    | InvalidMethodSignature ({ annotation; _ } as kind) ->
        InvalidMethodSignature { kind with annotation = annotation >>| dequalify }
    | InvalidType (InvalidType { annotation; expected }) ->
        InvalidType (InvalidType { annotation = dequalify annotation; expected })
    | InvalidType (FinalNested annotation) -> InvalidType (FinalNested (dequalify annotation))
    | InvalidType (FinalParameter name) -> InvalidType (FinalParameter name)
    | InvalidType (NestedAlias name) -> InvalidType (NestedAlias name)
    | InvalidType (NestedTypeVariables variable) ->
        InvalidType (NestedTypeVariables (Type.Variable.dequalify dequalify_map variable))
    | InvalidType (SingleExplicit explicit) -> InvalidType (SingleExplicit (dequalify explicit))
    | InvalidType (InvalidLiteral reference) ->
        InvalidType (InvalidLiteral (dequalify_reference reference))
    | InvalidTypeParameters invalid_type_parameters ->
        InvalidTypeParameters (dequalify_invalid_type_parameters invalid_type_parameters)
    | InvalidTypeVariable { annotation; origin } ->
        InvalidTypeVariable
          { annotation = Type.Variable.dequalify dequalify_map annotation; origin }
    | InvalidTypeVariance { annotation; origin } ->
        InvalidTypeVariance
          { annotation = dequalify annotation; origin = dequalify_type_variance_origin origin }
    | InvalidInheritance name -> InvalidInheritance (dequalify_invalid_inheritance name)
    | InvalidOverride { parent; decorator } ->
        InvalidOverride { parent = dequalify_identifier parent; decorator }
    | InvalidAssignment kind -> InvalidAssignment (dequalify_invalid_assignment kind)
    | InvalidClassInstantiation kind ->
        InvalidClassInstantiation (dequalify_invalid_class_instantiation kind)
    | TooManyArguments ({ callee; _ } as extra_argument) ->
        TooManyArguments { extra_argument with callee = Option.map ~f:dequalify_reference callee }
    | Top -> Top
    | MissingParameterAnnotation ({ annotation; _ } as missing_annotation) ->
        MissingParameterAnnotation { missing_annotation with annotation = annotation >>| dequalify }
    | MissingReturnAnnotation ({ annotation; _ } as missing_return) ->
        MissingReturnAnnotation { missing_return with annotation = annotation >>| dequalify }
    | MissingAttributeAnnotation
        { parent; missing_annotation = { annotation; _ } as missing_annotation } ->
        MissingAttributeAnnotation
          {
            parent = dequalify parent;
            missing_annotation = { missing_annotation with annotation = annotation >>| dequalify };
          }
    | MissingCaptureAnnotation name -> MissingCaptureAnnotation (dequalify_identifier name)
    | MissingGlobalAnnotation ({ annotation; _ } as immutable_type) ->
        MissingGlobalAnnotation { immutable_type with annotation = annotation >>| dequalify }
    | MissingOverloadImplementation name -> MissingOverloadImplementation (dequalify_reference name)
    | MutuallyRecursiveTypeVariables callee ->
        MutuallyRecursiveTypeVariables (Option.map callee ~f:dequalify_reference)
    | NotCallable annotation -> NotCallable (dequalify annotation)
    | PrivateProtocolProperty ({ parent; _ } as private_property) ->
        PrivateProtocolProperty { private_property with parent = dequalify parent }
    | ProhibitedAny { is_type_alias; missing_annotation = { annotation; _ } as missing_annotation }
      ->
        ProhibitedAny
          {
            is_type_alias;
            missing_annotation = { missing_annotation with annotation = annotation >>| dequalify };
          }
    | RedefinedClass { current_class; shadowed_class; is_shadowed_class_imported }
      when not is_shadowed_class_imported ->
        (* Both are locally-defined classes, so dequalify their references. *)
        RedefinedClass
          {
            current_class = dequalify_reference current_class;
            shadowed_class = dequalify_reference shadowed_class;
            is_shadowed_class_imported;
          }
    | RedefinedClass redefined_class -> RedefinedClass redefined_class
    | RedundantCast annotation -> RedundantCast (dequalify annotation)
    | RevealedLocals revealed_locals ->
        let dequalify_reveal_local { name; annotation } =
          { name; annotation = dequalify_annotation annotation }
        in
        RevealedLocals (List.map revealed_locals ~f:dequalify_reveal_local)
    | RevealedType { expression; annotation; qualify } ->
        let annotation = if qualify then annotation else dequalify_annotation annotation in
        RevealedType { expression; annotation; qualify }
    | IncompatibleParameterType ({ mismatch; callee; _ } as parameter) ->
        IncompatibleParameterType
          {
            parameter with
            mismatch = dequalify_mismatch mismatch;
            callee = Option.map callee ~f:dequalify_reference;
          }
    | IncompatibleReturnType ({ mismatch; _ } as return) ->
        IncompatibleReturnType { return with mismatch = dequalify_mismatch mismatch }
    | IncompatibleAttributeType { parent; incompatible_type = { mismatch; _ } as incompatible_type }
      ->
        IncompatibleAttributeType
          {
            parent = dequalify parent;
            incompatible_type = { incompatible_type with mismatch = dequalify_mismatch mismatch };
          }
    | IncompatibleVariableType ({ incompatible_type = { mismatch; _ }; _ } as variable) ->
        IncompatibleVariableType
          {
            variable with
            incompatible_type =
              { variable.incompatible_type with mismatch = dequalify_mismatch mismatch };
          }
    | InconsistentOverride
        ({ override = StrengthenedPrecondition (Found mismatch); parent; overridden_method; _ } as
        inconsistent_override) ->
        InconsistentOverride
          {
            inconsistent_override with
            parent = dequalify_reference parent;
            overridden_method = dequalify_identifier overridden_method;
            override = StrengthenedPrecondition (Found (dequalify_mismatch mismatch));
          }
    | InconsistentOverride
        ({ override = StrengthenedPrecondition (NotFound access); parent; overridden_method; _ } as
        inconsistent_override) ->
        InconsistentOverride
          {
            inconsistent_override with
            parent = dequalify_reference parent;
            overridden_method = dequalify_identifier overridden_method;
            override = StrengthenedPrecondition (NotFound access);
          }
    | InconsistentOverride
        ({ override = WeakenedPostcondition mismatch; parent; overridden_method; _ } as
        inconsistent_override) ->
        InconsistentOverride
          {
            inconsistent_override with
            parent = dequalify_reference parent;
            overridden_method = dequalify_identifier overridden_method;
            override = WeakenedPostcondition (dequalify_mismatch mismatch);
          }
    | InvalidDecoration expression -> InvalidDecoration expression
    | TupleConcatenationError expressions -> TupleConcatenationError expressions
    | TypedDictionaryAccessWithNonLiteral expression ->
        TypedDictionaryAccessWithNonLiteral expression
    | TypedDictionaryKeyNotFound { typed_dictionary_name; missing_key } ->
        TypedDictionaryKeyNotFound
          {
            typed_dictionary_name = dequalify_identifier typed_dictionary_name;
            missing_key = dequalify_identifier missing_key;
          }
    | TypedDictionaryInvalidOperation ({ typed_dictionary_name; mismatch; _ } as record) ->
        TypedDictionaryInvalidOperation
          {
            record with
            typed_dictionary_name = dequalify_identifier typed_dictionary_name;
            mismatch = dequalify_mismatch mismatch;
          }
    | TypedDictionaryInitializationError mismatch ->
        let mismatch =
          match mismatch with
          | MissingRequiredField { field_name; class_name } ->
              MissingRequiredField
                {
                  field_name = dequalify_identifier field_name;
                  class_name = dequalify_identifier class_name;
                }
          | FieldTypeMismatch { field_name; expected_type; actual_type; class_name } ->
              FieldTypeMismatch
                {
                  field_name = dequalify_identifier field_name;
                  expected_type = dequalify expected_type;
                  actual_type = dequalify actual_type;
                  class_name = dequalify_identifier class_name;
                }
          | UndefinedField { field_name; class_name } ->
              UndefinedField
                {
                  field_name = dequalify_identifier field_name;
                  class_name = dequalify_identifier class_name;
                }
        in
        TypedDictionaryInitializationError mismatch
    | UninitializedAttribute ({ mismatch; parent; kind; _ } as inconsistent_usage) ->
        UninitializedAttribute
          {
            inconsistent_usage with
            kind = dequalify_class_kind kind;
            parent = dequalify parent;
            mismatch = dequalify_mismatch mismatch;
          }
    | UnsafeCast kind -> UnsafeCast kind
    | UnawaitedAwaitable { references; expression } ->
        UnawaitedAwaitable { references = List.map references ~f:dequalify_reference; expression }
    | DuplicateTypeVariables { variable; base } ->
        DuplicateTypeVariables { variable = Type.Variable.dequalify dequalify_map variable; base }
    | UnboundName name -> UnboundName (dequalify_identifier name)
    | UninitializedLocal name -> UninitializedLocal (dequalify_identifier name)
    | UndefinedAttribute { attribute; origin } ->
        let origin : origin =
          match origin with
          | Class { class_origin = ClassType class_type; parent_module_path } ->
              let annotation =
                (* Don't dequalify optionals because we special case their display. *)
                if Type.is_optional_primitive class_type then
                  class_type
                else
                  dequalify class_type
              in
              Class { class_origin = ClassType annotation; parent_module_path }
          | Class { class_origin = ClassInUnion { unions; index }; parent_module_path } ->
              Class
                {
                  class_origin = ClassInUnion { unions = List.map ~f:dequalify unions; index };
                  parent_module_path;
                }
          | Module (ExplicitModule module_path) -> Module (ExplicitModule module_path)
          | Module (ImplicitModule module_name) ->
              Module (ImplicitModule (dequalify_reference module_name))
        in
        UndefinedAttribute { attribute; origin }
    | UndefinedType annotation -> UndefinedType (dequalify annotation)
    | UndefinedImport reference -> UndefinedImport reference
    | UnexpectedKeyword { name; callee } ->
        UnexpectedKeyword { name; callee = Option.map callee ~f:dequalify_reference }
    | UnsupportedOperand (Binary { operator_name; left_operand; right_operand }) ->
        UnsupportedOperand
          (Binary
             {
               operator_name;
               left_operand = dequalify left_operand;
               right_operand = dequalify right_operand;
             })
    | UnsupportedOperand (Unary { operator_name; operand }) ->
        UnsupportedOperand (Unary { operator_name; operand = dequalify operand })
    | MissingArgument { callee; parameter } ->
        MissingArgument { callee = Option.map callee ~f:dequalify_reference; parameter }
    | ParserFailure failure -> ParserFailure failure
    | UnusedIgnore codes -> UnusedIgnore codes
    | UnusedLocalMode mode -> UnusedLocalMode mode
    | Unpack unpack -> Unpack unpack
  in
  let signature =
    let dequalify_parameter ({ Node.value; _ } as parameter) =
      value.Expression.Parameter.annotation
      >>| GlobalResolution.parse_annotation ~validation:NoValidation resolution
      >>| dequalify
      >>| Type.expression
      |> fun annotation ->
      { parameter with Node.value = { value with Expression.Parameter.annotation } }
    in
    let parameters = List.map parameters ~f:dequalify_parameter in
    let return_annotation =
      return_annotation
      >>| GlobalResolution.parse_annotation ~validation:NoValidation resolution
      >>| dequalify
      >>| Type.expression
    in
    { signature with parameters; return_annotation }
  in
  { error with kind; signature = { Node.location; value = signature } }


let create_mismatch ~resolution ~actual ~expected ~covariant =
  let left, right =
    if covariant then
      actual, expected
    else
      expected, actual
  in
  {
    expected;
    actual;
    due_to_invariance = GlobalResolution.is_invariance_mismatch resolution ~left ~right;
  }
