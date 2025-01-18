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
module PyrePysaEnvironment = Analysis.PyrePysaEnvironment
module PyrePysaLogic = Analysis.PyrePysaLogic

type kind =
  | Normal
  | PropertySetter
  | Decorated
    (* This represents a callable but with all its decorators applied (i.e., the decorated
       function). By contrast, we use `Normal` to represent the undecorated function. *)
[@@deriving show { with_path = false }, sexp, compare, hash, eq]

type function_name = {
  name: string;
  kind: kind;
}
[@@deriving show { with_path = false }, sexp, compare, hash, eq]

type method_name = {
  class_name: string;
  method_name: string;
  kind: kind;
}
[@@deriving show { with_path = false }, sexp, compare, hash, eq]

module Regular = struct
  type t =
    | Function of function_name
    | Method of method_name
    | Override of method_name
    (* Represents a global variable or field of a class that we want to model,
     * e.g os.environ or HttpRequest.GET *)
    | Object of string
  [@@deriving show { with_path = false }, sexp, compare, hash, eq]

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
      | Function first, Function second -> compare_function_name first second
      | Method first, Method second -> compare_method_name first second
      | Override first, Override second -> compare_method_name first second
      | Object first, Object second -> String.compare first second
      | _ -> failwith "The compared targets must belong to the same variant."


  let pp_kind formatter = function
    | Normal -> ()
    | PropertySetter -> Format.fprintf formatter "@setter"
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
    | Function { name; kind } ->
        Format.fprintf
          formatter
          "%a%a"
          Reference.pp
          (name |> Reference.create |> Reference.delocalize)
          pp_kind
          kind
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


  (** Return the define name of a Function or Method target. Note that multiple targets can match to
      the same define name (e.g, property getters and setters). Hence, use this at your own risk. *)
  let define_name_exn = function
    | Function { name; _ } -> Reference.create name
    | Method { class_name; method_name; _ } ->
        Reference.create ~prefix:(Reference.create class_name) method_name
    | Override _
    | Object _ ->
        failwith "unexpected"


  let create_derived_override_exn ~at_type = function
    | Override { method_name; kind; _ } ->
        Override { class_name = Reference.show at_type; method_name; kind }
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
  [@@deriving show { with_path = false }, sexp, compare, hash, eq]
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

let create_function_name ?(kind = Normal) reference = { name = Reference.show reference; kind }

let create_method_name ?(kind = Normal) reference =
  {
    class_name = Reference.prefix reference >>| Reference.show |> Option.value ~default:"";
    method_name = Reference.last reference;
    kind;
  }


let from_regular regular = Regular regular

let get_regular = function
  | Regular regular
  | Parameterized { regular; _ } ->
      regular


let strip_parameters target = target |> get_regular |> from_regular

let as_regular_exn = function
  | Regular regular -> regular
  | Parameterized _ -> failwith "expect `Regular`"


let create_function ?kind reference =
  Function (create_function_name ?kind reference) |> from_regular


let create_method ?kind reference = Method (create_method_name ?kind reference) |> from_regular

let create_property_setter reference =
  Method (create_method_name ~kind:PropertySetter reference) |> from_regular


let create_override ?kind reference = Override (create_method_name ?kind reference) |> from_regular

let create_property_setter_override reference =
  Override (create_method_name ~kind:PropertySetter reference) |> from_regular


let create define_name define =
  let open Define in
  let kind = if Define.is_property_setter define then PropertySetter else Normal in
  match define.signature.legacy_parent with
  | Some _ -> create_method ~kind define_name
  | None -> create_function ~kind define_name


let create_object reference = Object (Reference.show reference) |> from_regular

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


module Set = Stdlib.Set.Make (T)
module Hashable = Core.Hashable.Make (T)
module HashMap = Hashable.Table
module HashSet = Hashable.Hash_set

type definitions_result = {
  qualifier: Reference.t;
  (* Mapping from a target to its selected definition. *)
  callables: Define.t Node.t Map.t;
  (* True if there was multiple non-stub definitions. *)
  has_multiple_definitions: bool;
}

(** This is the source of truth for the mapping of callables to definitions. All parts of the
    analysis should use this (or `get_module_and_definition`) rather than walking over source files. *)
let get_definitions ~pyre_api define_name =
  PyrePysaEnvironment.ReadOnly.get_function_definition pyre_api define_name
  >>| PyrePysaLogic.qualifier_and_bodies_of_function_definition
  >>| fun (qualifier, bodies) ->
  let fold ({ callables; _ } as result) define =
    let target = create define_name (Node.value define) in
    match Map.find_opt target callables with
    | None -> { result with callables = Map.add target define callables }
    | Some previous_define -> (
        (* Prefer the non-stub definition, so we can analyze its body. *)
        match Define.is_stub (Node.value previous_define), Define.is_stub (Node.value define) with
        | true, true -> result
        | false, true -> result
        | true, false -> { result with callables = Map.add target define callables }
        | false, false -> { result with has_multiple_definitions = true })
  in
  List.fold
    ~init:{ qualifier; callables = Map.empty; has_multiple_definitions = false }
    ~f:fold
    bodies


let get_module_and_definition ~pyre_api callable =
  define_name_exn callable
  |> get_definitions ~pyre_api
  >>= fun { qualifier; callables; _ } ->
  Map.find_opt callable callables >>| fun define -> qualifier, define


let get_callable_location ~pyre_api callable =
  get_module_and_definition ~pyre_api callable
  >>| fun (module_reference, { Node.location; _ }) ->
  Location.with_module ~module_reference location


let resolve_method ~pyre_api ~class_type ~method_name =
  let callable_implementation =
    Type.split class_type
    |> fst
    |> Type.primitive_name
    >>= PyrePysaEnvironment.ReadOnly.attribute_from_class_name
          pyre_api
          ~transitive:true
          ~name:method_name
          ~type_for_lookup:class_type
  in
  match callable_implementation with
  | Some callable when PyrePysaLogic.AnnotatedAttribute.defined callable ->
      PyrePysaLogic.name_of_method callable >>| create_method
  | _ -> None


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

(* Represent a hashset of targets inside the shared memory *)
module HashsetSharedMemory = struct
  type target = T.t

  module T =
    SaveLoadSharedMemory.MakeKeyValue
      (SharedMemoryKey)
      (struct
        type t = unit

        let prefix = Hack_parallel.Std.Prefix.make ()

        let handle_prefix = Hack_parallel.Std.Prefix.make ()

        let description = "A set of targets"
      end)

  type t = T.t

  let cleanup = T.cleanup

  let from_heap targets = targets |> List.map ~f:(fun target -> target, ()) |> T.of_alist

  module ReadOnly = T.ReadOnly

  let read_only = T.read_only
end
