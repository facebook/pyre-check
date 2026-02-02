(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* Target: represents a global symbol that might have information attached to it.
 *
 * This is mostly used to represent callables in the interprocedural framework,
 * using `Function` or `Method`.
 * `Override f` represents the set of methods overriding the method `f`.
 * `Object` represents global variables or class attributes.
 *)

open Core
open Ast
open Statement
open Pyre
module TaintAccessPath = Analysis.TaintAccessPath
module PyrePysaLogic = Analysis.PyrePysaLogic

type kind =
  | Normal
  | Pyre1PropertySetter
  | PyreflyPropertySetter
  | Decorated
    (* This represents a callable but with all its decorators applied (i.e., the decorated
       function). By contrast, we use `Normal` to represent the undecorated function. *)
[@@deriving show { with_path = false }, sexp, compare, hash, equal]

module Function = struct
  type t = {
    name: string;
    kind: kind;
  }
  [@@deriving show { with_path = false }, sexp, compare, hash, equal]

  let create ?(kind = Normal) reference =
    let () =
      if Reference.is_local reference then
        failwith (Format.asprintf "Invalid callable name: %a" Reference.pp reference)
    in
    { name = Reference.show reference; kind }
end

module Method = struct
  type t = {
    class_name: string;
    method_name: string;
    kind: kind;
  }
  [@@deriving show { with_path = false }, sexp, compare, hash, equal]

  let create ?(kind = Normal) class_name method_name =
    let () =
      if Reference.is_local class_name then
        failwith (Format.asprintf "Invalid class name: %a" Reference.pp class_name)
    in
    { class_name = Reference.show class_name; method_name; kind }
end

module Regular = struct
  type t =
    | Function of Function.t
    | Method of Method.t
    | Override of Method.t
    (* Represents a global variable or field of a class that we want to model,
     * e.g os.environ or HttpRequest.GET *)
    | Object of string
  [@@deriving show { with_path = false }, sexp, compare, hash, equal]

  (* Lower priority appears earlier in comparison. *)
  let priority = function
    | Function _ -> 0
    | Method _ -> 1
    | Override _ -> 2
    | Object _ -> 3


  let compare left right =
    let priority_comparison = Int.compare (priority left) (priority right) in
    if priority_comparison <> 0 then
      priority_comparison
    else
      match left, right with
      | Function first, Function second -> Function.compare first second
      | Method first, Method second -> Method.compare first second
      | Override first, Override second -> Method.compare first second
      | Object first, Object second -> String.compare first second
      | _ -> failwith "The compared targets must belong to the same variant."


  let pp_kind formatter = function
    | Normal -> ()
    | Pyre1PropertySetter -> Format.fprintf formatter "@setter"
    | PyreflyPropertySetter ->
        (* Property setters already have '@setter' in their define name when using pyrefly. *)
        ()
    | Decorated -> Format.fprintf formatter "@decorated"


  let kind = function
    | Function { kind; _ }
    | Method { kind; _ }
    | Override { kind; _ } ->
        Some kind
    | Object _ -> None


  let set_kind kind = function
    | Function (_ as function_name) -> Function { function_name with kind }
    | Method (_ as method_name) -> Method { method_name with kind }
    | Override (_ as method_name) -> Override { method_name with kind }
    | Object _ as regular -> regular


  let pp_pretty formatter = function
    | Function { name; kind } -> Format.fprintf formatter "%s%a" name pp_kind kind
    | Method { class_name; method_name; kind } ->
        Format.fprintf formatter "%s.%s%a" class_name method_name pp_kind kind
    | Override { class_name; method_name; kind } ->
        Format.fprintf formatter "Overrides{%s.%s%a}" class_name method_name pp_kind kind
    | Object name -> Format.fprintf formatter "Object{%s}" name


  let pp_pretty_with_kind formatter = function
    | Function { name; kind } -> Format.fprintf formatter "%s%a (fun)" name pp_kind kind
    | Method { class_name; method_name; kind } ->
        Format.fprintf formatter "%s.%s%a (method)" class_name method_name pp_kind kind
    | Override { class_name; method_name; kind } ->
        Format.fprintf formatter "%s.%s%a (override)" class_name method_name pp_kind kind
    | Object name -> Format.fprintf formatter "%s (object)" name


  let pp_external formatter = function
    | Function { name; kind } -> Format.fprintf formatter "%s%a" name pp_kind kind
    | Method { class_name; method_name; kind } ->
        Format.fprintf formatter "%s.%s%a" class_name method_name pp_kind kind
    | Override { class_name; method_name; kind } ->
        Format.fprintf formatter "Overrides{%s.%s%a}" class_name method_name pp_kind kind
    | Object name -> Format.fprintf formatter "Obj{%s}" name


  let get_corresponding_method_exn = function
    | Override method_name -> Method method_name
    | _ -> failwith "not an override target"


  let get_corresponding_override_exn = function
    | Method method_name -> Override method_name
    | _ -> failwith "unexpected"


  let class_name = function
    | Method { class_name; _ } -> Some class_name
    | Override { class_name; _ } -> Some class_name
    | Function _
    | Object _ ->
        None


  let method_name = function
    | Method { method_name; _ } -> Some method_name
    | Override { method_name; _ } -> Some method_name
    | Function _
    | Object _ ->
        None


  let function_name = function
    | Function { name; _ } -> Some name
    | Method _
    | Override _
    | Object _ ->
        None


  let object_name = function
    | Object name -> Reference.create name
    | _ -> failwith "unexpected"


  let is_function_or_method = function
    | Function _
    | Method _ ->
        true
    | Override _
    | Object _ ->
        false


  let is_method_or_override = function
    | Method _
    | Override _ ->
        true
    | Function _
    | Object _ ->
        false


  let is_method = function
    | Method _ -> true
    | _ -> false


  let is_function = function
    | Function _ -> true
    | _ -> false


  let is_override = function
    | Override _ -> true
    | _ -> false


  let is_object = function
    | Object _ -> true
    | _ -> false


  let is_normal regular =
    match kind regular with
    | Some Normal -> true
    | _ -> false


  let is_decorated regular =
    match kind regular with
    | Some Decorated -> true
    | _ -> false


  let override_to_method = function
    | Function target -> Function target
    | Method target
    | Override target ->
        Method target
    | Object name -> Object name


  let define_name = function
    | Function { name; _ } -> Some (Reference.create name)
    | Method { class_name; method_name; _ } ->
        Some (Reference.create ~prefix:(Reference.create class_name) method_name)
    | Override _
    | Object _ ->
        None


  (** Return the define name of a Function or Method target. Note that multiple targets can match to
      the same define name (e.g, property getters and setters). Hence, use this at your own risk. *)
  let define_name_exn regular =
    match define_name regular with
    | Some name -> name
    | None -> Format.asprintf "Unexpected: %a" pp_pretty_with_kind regular |> failwith


  let create_derived_override_exn ~at_type = function
    | Override { method_name; kind; _ } -> Override (Method.create ~kind at_type method_name)
    | _ -> failwith "unexpected"
end

module ParameterMap = Data_structures.SerializableMap.Make (TaintAccessPath.Root)

module T = struct
  type t =
    | Regular of Regular.t
    | Parameterized of {
        regular: Regular.t;
        parameters: t ParameterMap.t;
      }
      (* This represents a regular callable with its function-typed parameters being instantited
         with `parameters`. *)
  [@@deriving show { with_path = false }, sexp, compare, hash, equal]
end

include T

module Map = struct
  include Data_structures.SerializableMap.Make (T)

  module Tree = Map.Make_tree (struct
    include T
    include Comparator.Make (T)
  end)
end

module type RegularTargetPrettyPrintType = sig
  val pp : Format.formatter -> Regular.t -> unit
end

module MakePrettyPrint (RegularTargetPrettyPrint : RegularTargetPrettyPrintType) = struct
  let rec pp formatter = function
    | Regular regular -> RegularTargetPrettyPrint.pp formatter regular
    | Parameterized { regular; parameters } ->
        let rec pp_parameters formatter = function
          | [] -> Format.fprintf formatter ""
          | [(access_path, target)] ->
              Format.fprintf formatter "%a=%a" TaintAccessPath.Root.pp access_path pp target
          | (access_path, target) :: tail ->
              let () =
                Format.fprintf formatter "%a=%a, " TaintAccessPath.Root.pp access_path pp target
              in
              pp_parameters formatter tail
        in
        Format.fprintf
          formatter
          "%a[%a]"
          RegularTargetPrettyPrint.pp
          regular
          pp_parameters
          (ParameterMap.to_alist parameters)


  let show = Format.asprintf "%a" pp
end

let pp_internal = pp

let show_internal = Format.asprintf "%a" pp_internal

(* Equivalent to pp_internal. Required by @@deriving. *)
let pp = pp_internal

module PrettyPrint = MakePrettyPrint (struct
  let pp = Regular.pp_pretty
end)

let pp_pretty = PrettyPrint.pp

let show_pretty = PrettyPrint.show

module PrettyPrintWithKind = MakePrettyPrint (struct
  let pp = Regular.pp_pretty_with_kind
end)

let pp_pretty_with_kind = PrettyPrintWithKind.pp

let show_pretty_with_kind = PrettyPrintWithKind.show

module PrettyPrintExternal = MakePrettyPrint (struct
  let pp = Regular.pp_external
end)

let pp_external = PrettyPrintExternal.pp

let external_name = PrettyPrintExternal.show

let from_regular regular = Regular regular

let get_regular = function
  | Regular regular
  | Parameterized { regular; _ } ->
      regular


let strip_parameters target = target |> get_regular |> from_regular

let as_regular_exn = function
  | Regular regular -> regular
  | Parameterized _ -> failwith "expect `Regular`"


let collect_nested_regular_targets =
  let rec fold sofar = function
    | Regular regular -> regular :: sofar
    | Parameterized { regular; parameters } ->
        ParameterMap.fold
          (fun _ parameter sofar -> fold sofar parameter)
          parameters
          (regular :: sofar)
  in
  fold []


let create_function ?kind reference = Function (Function.create ?kind reference) |> from_regular

let create_method ?kind class_name method_name =
  Method (Method.create ?kind class_name method_name) |> from_regular


let create_method_from_reference ?kind reference =
  Method
    (Method.create
       ?kind
       (Reference.prefix reference |> Option.value ~default:Reference.empty)
       (Reference.last reference))
  |> from_regular


let create_override ?kind class_name method_name =
  Override (Method.create ?kind class_name method_name) |> from_regular


let create_override_from_reference ?kind reference =
  Override
    (Method.create
       ?kind
       (Reference.prefix reference |> Option.value ~default:Reference.empty)
       (Reference.last reference))
  |> from_regular


let from_define ~define_name ~define =
  let open Define in
  let kind = if Define.is_property_setter define then Pyre1PropertySetter else Normal in
  match define.signature.legacy_parent with
  | Some _ -> create_method_from_reference ~kind define_name
  | None -> create_function ~kind define_name


let create_object reference = Object (Reference.show reference) |> from_regular

let get_corresponding_method_exn ~must_be_regular target =
  (if must_be_regular then
     as_regular_exn target
  else
    get_regular target)
  |> Regular.get_corresponding_method_exn
  |> from_regular


let class_name target = target |> get_regular |> Regular.class_name

let method_name target = target |> get_regular |> Regular.method_name

let function_name target = target |> get_regular |> Regular.function_name

let object_name target = target |> get_regular |> Regular.object_name

let is_function_or_method target = target |> get_regular |> Regular.is_function_or_method

let is_method_or_override target = target |> get_regular |> Regular.is_method_or_override

let is_method target = target |> get_regular |> Regular.is_method

let is_function target = target |> get_regular |> Regular.is_function

let is_override target = target |> get_regular |> Regular.is_override

let is_object target = target |> get_regular |> Regular.is_object

let is_normal target = target |> get_regular |> Regular.is_normal

let is_decorated target = target |> get_regular |> Regular.is_decorated

let is_parameterized = function
  | Regular _ -> false
  | Parameterized _ -> true


let is_regular = function
  | Regular _ -> true
  | Parameterized _ -> false


(* A parameterized target contains recursive targets if one of its `regular` part also appears in
   one of its `parameters` part. Such recursion may lead to non-termination in high-order call graph
   building. *)
let contain_recursive_target target =
  let rec contain_recursive_target existing_regulars = function
    | Regular regular -> List.exists existing_regulars ~f:(Regular.equal regular)
    | Parameterized { regular; parameters } ->
        List.exists existing_regulars ~f:(Regular.equal regular)
        || ParameterMap.exists
             (fun _ target -> contain_recursive_target (regular :: existing_regulars) target)
             parameters
  in
  contain_recursive_target [] target


(* Return the level of target nestedness within a given target. *)
let rec depth = function
  | Regular _ -> 1
  | Parameterized { parameters; _ } ->
      1
      + (parameters
        |> ParameterMap.data
        |> List.map ~f:depth
        |> List.max_elt ~compare:Int.compare
        |> Option.value ~default:0)


let rec for_issue_handle = function
  | Regular regular -> regular |> Regular.override_to_method |> from_regular
  | Parameterized { regular; parameters } ->
      Parameterized
        {
          regular = Regular.override_to_method regular;
          parameters = ParameterMap.map for_issue_handle parameters;
        }


let define_name_exn target = target |> get_regular |> Regular.define_name_exn

let set_kind kind = function
  | Regular regular -> Regular (Regular.set_kind kind regular)
  | Parameterized { regular; parameters } ->
      Parameterized { regular = Regular.set_kind kind regular; parameters }


module MakePrettyPrintContainer (Container : sig
  type container

  val pp : Format.formatter -> T.t -> unit

  val elements : container -> T.t list

  val separator : string

  val left_bracket : string

  val right_bracket : string
end) =
struct
  let pp formatter container =
    match Container.elements container with
    | [] -> Format.fprintf formatter "%s%s" Container.left_bracket Container.right_bracket
    | [element] ->
        Format.fprintf
          formatter
          "%s%a%s"
          Container.left_bracket
          Container.pp
          element
          Container.right_bracket
    | list ->
        let pp_element formatter element =
          Format.fprintf formatter "@%s%a" Container.separator Container.pp element
        in
        let pp_elements formatter = List.iter ~f:(pp_element formatter) in
        Format.fprintf
          formatter
          "%s@[<v 2>%a@]@,%s"
          Container.left_bracket
          pp_elements
          list
          Container.right_bracket


  let show = Format.asprintf "%a" pp
end

module Set = struct
  include Stdlib.Set.Make (T)

  module PrettyPrintWithKind = MakePrettyPrintContainer (struct
    type container = t

    let elements = elements

    let pp = pp_pretty_with_kind

    let separator = ","

    let left_bracket = "{"

    let right_bracket = "}"
  end)

  let pp_pretty_with_kind = PrettyPrintWithKind.pp

  let show_pretty_with_kind = PrettyPrintWithKind.show
end

module Hashable = Core.Hashable.Make (T)
module HashMap = Hashable.Table
module HashSet = Hashable.Hash_set

type definitions_result = {
  qualifier: Reference.t;
  (* Mapping from a target to its selected definition. *)
  callables: Define.t Node.t Map.t;
}

(** This is the source of truth for the mapping of callables to definitions. All parts of the
    analysis should use this (or `get_module_and_definition`) rather than walking over source files. *)
let get_definitions ~pyre1_api ~warn_multiple_definitions define_name =
  Analysis.PyrePysaEnvironment.ReadOnly.get_function_definition pyre1_api define_name
  >>| PyrePysaLogic.qualifier_and_bodies_of_function_definition
  >>| fun (qualifier, bodies) ->
  let get_priority define =
    (* Prefer the non-stub and non-overload definition, so we can analyze its body. *)
    if not (Define.is_stub define) then
      0
    else if not (Define.is_overloaded_function define) then
      -1
    else
      -2
  in
  let multiple_definitions = ref [] in
  let resolve_multiple_defines left right =
    if
      warn_multiple_definitions
      && (not (Define.is_stub left.Node.value))
      && not (Define.is_stub right.Node.value)
    then
      multiple_definitions := define_name :: !multiple_definitions;
    let left_priority = get_priority left.Node.value in
    let right_priority = get_priority right.Node.value in
    if left_priority > right_priority then
      left
    else if Int.equal left_priority right_priority then (* The last definition wins. *)
      right
    else
      right
  in
  if warn_multiple_definitions then
    !multiple_definitions
    |> List.dedup_and_sort ~compare:Reference.compare
    |> List.iter ~f:(fun define_name ->
           Log.warning
             "Found multiple definitions for the given symbol: `%a`. We will only consider the \
              last definition."
             Reference.pp
             define_name);
  {
    qualifier;
    callables =
      bodies
      |> List.map ~f:(fun body -> from_define ~define_name ~define:(Node.value body), body)
      |> Map.of_alist ~f:resolve_multiple_defines;
  }


(* Define the meaning of `skip_analysis_targets`. We assume `skip_analysis_targets` only contains
   regular callables. *)
let should_skip_analysis ~skip_analysis_targets target =
  target |> strip_parameters |> Core.Hash_set.mem skip_analysis_targets


module ArtificialTargets = struct
  let format_string = Object "<format-string>" |> from_regular

  let str_add = Object "<str.__add__>" |> from_regular

  let str_mod = Object "<str.__mod__>" |> from_regular

  let str_format = Object "<str.format>" |> from_regular

  let str_literal = Object "<literal-string>" |> from_regular

  let condition = Object "<condition>" |> from_regular
end

module SharedMemoryKey = struct
  include T

  let to_string key = sexp_of_t key |> Sexp.to_string

  let from_string sexp_string = Sexp.of_string sexp_string |> t_of_sexp
end

(** Whether a method is an instance method, or a class method, or a static method. *)
module MethodKind = struct
  type t =
    | Static
    | Class
    | Instance
end

(* Represent a hashset of targets inside the shared memory *)
module HashsetSharedMemory = struct
  type target = T.t

  module T =
    Hack_parallel.Std.SharedMemory.FirstClassWithKeys.Make
      (SharedMemoryKey)
      (struct
        type t = unit

        let prefix = Hack_parallel.Std.Prefix.make ()

        let description = "A set of targets"
      end)

  type t = T.t

  let cleanup = T.cleanup

  let from_heap targets = targets |> List.map ~f:(fun target -> target, ()) |> T.of_alist_sequential

  module ReadOnly = T.ReadOnly

  let read_only = T.read_only
end

module List = struct
  type t = T.t list

  module PrettyPrintWithKind = MakePrettyPrintContainer (struct
    type container = t

    let elements = Fn.id

    let pp = pp_pretty_with_kind

    let separator = ";"

    let left_bracket = "["

    let right_bracket = "]"
  end)

  let pp_pretty_with_kind = PrettyPrintWithKind.pp

  let show_pretty_with_kind = PrettyPrintWithKind.show
end
