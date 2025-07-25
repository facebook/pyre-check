(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(* TODO(T132410158) Add a module-level doc comment. *)

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
  | PropertySetter of {
      class_name: Type.t;
      direct_target: Reference.t;
    }
[@@deriving compare, hash, sexp, equal, show, to_yojson]

type callee_with_locations = {
  callee: callee;
  locations: Location.WithModule.t list;
}
[@@deriving equal]

let callee_to_yojson ?locations callee =
  let locations =
    match locations with
    | None -> []
    | Some locations -> ["locations", `List (List.map locations ~f:Location.WithPath.to_yojson)]
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
                 (match dispatch with
                 | Dynamic -> "dynamic"
                 | Static -> "static") );
           ])
  | PropertySetter { direct_target; class_name } ->
      `Assoc
        (List.rev_append
           locations
           [
             "kind", `String "property setter";
             "direct_target", `String (Reference.show direct_target);
             "class_name", `String (Type.class_name class_name |> Reference.show);
           ])


module type Builder = sig
  val initialize : unit -> unit

  val add_callee
    :  global_resolution:GlobalResolution.t ->
    target:Type.t option ->
    callables:Type.Callable.t list ->
    dynamic:bool ->
    qualifier:Reference.t ->
    callee_type:Type.t ->
    callee:Expression.t ->
    unit

  val add_property_callees
    :  global_resolution:GlobalResolution.t ->
    resolved_base:Type.t ->
    attributes:(AnnotatedAttribute.instantiated * Type.t) list ->
    name:string ->
    qualifier:Reference.t ->
    location:Location.t ->
    unit

  val add_property_setter_callees
    :  attribute:AnnotatedAttribute.instantiated ->
    instantiated_parent:Type.t ->
    name:string ->
    location:Location.WithModule.t ->
    unit

  val get_all_callees : unit -> callee_with_locations list
end

module DefaultBuilder : Builder = struct
  let table = Location.WithModule.Table.create ()

  let initialize () = Hashtbl.clear table

  let add_callee ~global_resolution ~target ~callables ~dynamic ~qualifier ~callee_type:_ ~callee =
    (* Store callees. *)
    let callees =
      let method_callee ?(is_optional_class_attribute = false) annotation callable_kind =
        let is_protocol () =
          Type.split annotation |> fst |> GlobalResolution.is_protocol global_resolution
        in
        match callable_kind with
        | Type.Callable.Named direct_target when not (is_protocol ()) ->
            let class_name =
              if Type.is_class_type annotation then
                Type.single_argument annotation
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
      let callable_kinds = List.map callables ~f:(fun { Type.Callable.kind; _ } -> kind) in
      let extract_callables ~annotation instantiated_attribute =
        instantiated_attribute
        |> AnnotatedAttribute.annotation
        |> TypeInfo.Unit.annotation
        |> Type.callable_name
        >>| (fun name -> method_callee ~is_optional_class_attribute:true annotation (Named name))
        |> function
        | None -> []
        | Some list -> list
      in
      match target, callable_kinds with
      | Some (Type.Union elements), callables when List.length elements = List.length callables -> (
          match List.map2 elements callables ~f:method_callee with
          | Ok callees_list -> List.concat callees_list
          | Unequal_lengths -> [])
      | Some annotation, (_ :: _ as callables) ->
          List.concat_map callables ~f:(method_callee annotation)
      | Some (Type.Union ([Type.NoneType; annotation] | [annotation; Type.NoneType])), _ -> (
          match Node.value callee with
          | Expression.Name (Name.Attribute { attribute; _ }) ->
              GlobalResolution.attribute_from_annotation
                global_resolution
                ~parent:annotation
                ~name:attribute
              >>| extract_callables ~annotation
              |> Option.value ~default:[]
          | _ -> [])
      | None, defines ->
          List.map defines ~f:(function
              | Named define -> Some (Function define)
              | Anonymous -> (
                  match callee.value with
                  | Expression.Name (Name.Identifier target) ->
                      Some (Function (Reference.create target |> Reference.delocalize))
                  | _ -> None))
          |> Option.all
          |> Option.value ~default:[]
      | _ -> []
    in
    let key = Location.with_module ~module_reference:qualifier (Node.location callee) in
    Hashtbl.set table ~key ~data:callees


  let attribute_target attribute name =
    AnnotatedAttribute.parent attribute
    |> fun parent -> Reference.create ~prefix:(Reference.create parent) name


  let add_property_callees ~global_resolution ~resolved_base ~attributes ~name ~qualifier ~location =
    let property_callables = ref [] in
    let register_attribute_callable ?(is_optional_class_attribute = false) class_name attribute =
      let direct_target = attribute_target attribute name in
      property_callables :=
        Method { direct_target; class_name; dispatch = Dynamic; is_optional_class_attribute }
        :: !property_callables
    in
    let register (attribute, type_for_lookup) =
      if AnnotatedAttribute.property attribute then
        register_attribute_callable type_for_lookup attribute
      (* As the callgraph is an overapproximation, we also have to consider property calls from
         optional attributes.*)
      else
        match resolved_base with
        | Type.Union [Type.NoneType; base]
        | Type.Union [base; Type.NoneType] -> (
            Type.class_attribute_lookups_for_type base
            |> function
            | Some [{ Type.type_for_lookup; accessed_through_class; class_name; _ }] -> (
                let attribute =
                  GlobalResolution.attribute_from_class_name
                    global_resolution
                    class_name
                    ~transitive:true
                    ~accessed_through_class
                    ~special_method:false
                    ~name
                    ~type_for_lookup
                in
                match attribute with
                | Some attribute ->
                    if AnnotatedAttribute.property attribute then
                      register_attribute_callable
                        ~is_optional_class_attribute:true
                        type_for_lookup
                        attribute
                | None -> ())
            | Some _
            | None ->
                ())
        | _ -> ()
    in
    List.iter attributes ~f:register;
    if not (List.is_empty !property_callables) then
      let key = Location.with_module ~module_reference:qualifier location in
      Hashtbl.set table ~key ~data:!property_callables


  let add_property_setter_callees ~attribute ~instantiated_parent ~name ~location =
    Hashtbl.set
      table
      ~key:location
      ~data:
        [
          PropertySetter
            { direct_target = attribute_target attribute name; class_name = instantiated_parent };
        ]


  module CalleesTable = Hashtbl.Make (struct
    type t = callee [@@deriving compare, hash, sexp]
  end)

  let get_all_callees () =
    (* Sort the callees as a map from callee -> list of locations. *)
    let callees = CalleesTable.create () in
    let add_binding ~key ~data =
      List.iter data ~f:(fun callee -> Hashtbl.add_multi callees ~key:callee ~data:key)
    in
    Hashtbl.iteri table ~f:add_binding;
    Hashtbl.clear table;
    Hashtbl.to_alist callees |> List.map ~f:(fun (callee, locations) -> { callee; locations })
end

module NullBuilder : Builder = struct
  let initialize () = ()

  let add_callee
      ~global_resolution:_
      ~target:_
      ~callables:_
      ~dynamic:_
      ~qualifier:_
      ~callee_type:_
      ~callee:_
    =
    ()


  let add_property_setter_callees ~attribute:_ ~instantiated_parent:_ ~name:_ ~location:_ = ()

  let add_property_callees
      ~global_resolution:_
      ~resolved_base:_
      ~attributes:_
      ~name:_
      ~qualifier:_
      ~location:_
    =
    ()


  let get_all_callees () = []
end
