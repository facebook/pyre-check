(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open Core
open Ast
open Expression
open Pyre

type dispatch =
  | Dynamic
  | Static

and callee =
  | Function of Reference.t
  | Method of {
      class_name: Type.t;
      direct_target: Reference.t;
      dispatch: dispatch;
      is_optional_class_attribute: bool;
    }
[@@deriving compare, hash, sexp, eq, show, to_yojson]

type callee_with_locations = {
  callee: callee;
  locations: Location.Reference.t list;
}

include Hashable.Make (struct
  type t = callee [@@deriving compare, hash, sexp]
end)

let callee_to_yojson ?locations callee =
  let locations =
    match locations with
    | None -> []
    | Some locations -> ["locations", `List (List.map locations ~f:Location.Instantiated.to_yojson)]
  in
  match callee with
  | Function name ->
      `Assoc
        (List.rev_append
           locations
           ["kind", `String "function"; "target", `String (Reference.show name)])
  | Method { direct_target; class_name; dispatch; is_optional_class_attribute } ->
      `Assoc
        (List.rev_append
           locations
           [
             "kind", `String "method";
             "is_optional_class_attribute", `Bool is_optional_class_attribute;
             "direct_target", `String (Reference.show direct_target);
             "class_name", `String (Type.class_name class_name |> Reference.show);
             ( "dispatch",
               `String
                 ( match dispatch with
                 | Dynamic -> "dynamic"
                 | Static -> "static" ) );
           ])


module CalleeValue = struct
  type t = callee_with_locations list

  let prefix = Prefix.make ()

  let description = "Reference List"

  let unmarshall value = Marshal.from_string value 0
end

module SharedMemory = Memory.WithCache.Make (SharedMemoryKeys.ReferenceKey) (CalleeValue)

let set ~caller ~callees = SharedMemory.add caller callees

let get ~caller = SharedMemory.get caller |> Option.value ~default:[]

module type Builder = sig
  val initialize : unit -> unit

  val add_callee
    :  global_resolution:GlobalResolution.t ->
    target:Type.t option ->
    callables:Type.Callable.t list option ->
    dynamic:bool ->
    callee:Expression.t ->
    unit

  val add_property_callees
    :  global_resolution:GlobalResolution.t ->
    resolved_base:Type.t ->
    attributes:(AnnotatedAttribute.t * Type.t) list ->
    name:string ->
    location:Location.t ->
    unit

  val get_all_callees : unit -> callee_with_locations list
end

module DefaultBuilder : Builder = struct
  let table = Location.Reference.Table.create ()

  let initialize () = Hashtbl.clear table

  let add_callee ~global_resolution ~target ~callables ~dynamic ~callee =
    (* Store callees. *)
    let callees =
      let method_callee ?(is_optional_class_attribute = false) annotation callable =
        match callable with
        | { Type.Callable.kind = Named direct_target; _ } ->
            let class_name =
              if Type.is_meta annotation then
                Type.single_parameter annotation
              else
                annotation
            in
            [
              Method
                {
                  direct_target;
                  class_name;
                  dispatch = (if dynamic then Dynamic else Static);
                  is_optional_class_attribute;
                };
            ]
        | _ -> []
      in
      match target, callables with
      | Some (Type.Union elements), Some callables when List.length elements = List.length callables
        ->
          List.map2_exn elements callables ~f:method_callee |> List.concat
      | Some annotation, Some callables -> List.concat_map callables ~f:(method_callee annotation)
      | Some (Type.Optional annotation), _ -> (
          match Node.value callee with
          | Expression.Name (Name.Attribute { attribute; _ }) -> (
              GlobalResolution.attribute_from_annotation
                global_resolution
                ~parent:annotation
                ~name:attribute
              >>| AnnotatedAttribute.annotation
              >>| Annotation.annotation
              >>= (function
                    | Type.Callable callable -> Some callable
                    | _ -> None)
              >>| method_callee ~is_optional_class_attribute:true annotation
              |> function
              | None -> []
              | Some list -> list )
          | _ -> [] )
      | None, Some [{ Type.Callable.kind = Named define; _ }] -> [Function define]
      | _ -> []
    in
    Hashtbl.set table ~key:(Node.location callee) ~data:callees


  let add_property_callees ~global_resolution ~resolved_base ~attributes ~name ~location =
    let property_callables = ref [] in
    let register_attribute_callable ?(is_optional_class_attribute = false) class_name attribute =
      let direct_target_name =
        Annotated.Attribute.parent attribute
        |> Type.primitive_name
        >>| fun parent -> Reference.create ~prefix:(Reference.create parent) name
      in
      match direct_target_name with
      | Some direct_target ->
          property_callables :=
            Method { direct_target; class_name; dispatch = Dynamic; is_optional_class_attribute }
            :: !property_callables
      | None -> ()
    in
    let register (attribute, instantiated) =
      if Annotated.Attribute.property attribute then
        register_attribute_callable instantiated attribute
      (* As the callgraph is an overapproximation, we also have to consider property calls from
         optional attributes.*)
      else
        match resolved_base with
        | Type.Optional base -> (
            Type.resolve_class base
            |> function
            | Some [{ instantiated; class_attributes; class_name }] -> (
                let attribute =
                  GlobalResolution.attribute_from_class_name
                    class_name
                    ~transitive:true
                    ~class_attributes
                    ~special_method:false
                    ~resolution:global_resolution
                    ~name
                    ~instantiated
                in
                match attribute with
                | Some attribute ->
                    if Annotated.Attribute.property attribute then
                      register_attribute_callable
                        ~is_optional_class_attribute:true
                        instantiated
                        attribute
                | None -> () )
            | Some _
            | None ->
                () )
        | _ -> ()
    in
    List.iter attributes ~f:register;
    if not (List.is_empty !property_callables) then
      Hashtbl.set table ~key:location ~data:!property_callables


  let get_all_callees () =
    (* Sort the callees as a map from callee -> list of locations. *)
    let callees = Table.create () in
    let add_binding ~key ~data =
      List.iter data ~f:(fun callee -> Hashtbl.add_multi callees ~key:callee ~data:key)
    in
    Hashtbl.iteri table ~f:add_binding;
    Hashtbl.clear table;
    Hashtbl.to_alist callees |> List.map ~f:(fun (callee, locations) -> { callee; locations })
end

module NullBuilder : Builder = struct
  let initialize () = ()

  let add_callee ~global_resolution:_ ~target:_ ~callables:_ ~dynamic:_ ~callee:_ = ()

  let add_property_callees ~global_resolution:_ ~resolved_base:_ ~attributes:_ ~name:_ ~location:_ =
    ()


  let get_all_callees () = []
end
