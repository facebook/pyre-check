(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open Ast
open Analysis

let type_to_string type_ = type_ |> Format.asprintf "%a" Type.pp

let type_to_reference type_ = type_ |> type_to_string |> Reference.create

let expression_to_json expression = `String (expression |> Expression.sanitized |> Expression.show)

module SerializableReference = struct
  type t = Reference.t [@@deriving compare, sexp, hash, show]

  let to_yojson reference = `String (Reference.show_sanitized reference)

  module Map = Reference.Map.Tree
  module Set = Set.Make (Reference)
end

module DefaultValue = struct
  type t = Expression.t option [@@deriving show]

  let to_yojson value = value >>| expression_to_json |> Option.value ~default:`Null
end

module AnnotationLocation = struct
  type t = {
    qualifier: SerializableReference.t;
    path: string;
    line: int;
  }
  [@@deriving show, compare, to_yojson]

  let create ~lookup ~qualifier ~line =
    { qualifier; path = lookup qualifier |> Option.value ~default:"*"; line }


  let from_location ~lookup ~qualifier Location.{ start = { line; _ }; _ } =
    create ~lookup ~qualifier ~line


  let from_location_with_module
      ~lookup
      Location.WithModule.{ module_reference = qualifier; start = { line; _ }; _ }
    =
    create ~lookup ~qualifier ~line
end

module SerializableType = struct
  type t = Type.t [@@deriving show]

  let to_yojson type_ = `String (type_to_string type_)
end

module TypeAnnotation = struct
  type t =
    | Inferred of SerializableType.t
    | Given of Expression.t
    | Missing
  [@@deriving show]

  let is_inferred = function
    | Inferred _ -> true
    | Given _
    | Missing ->
        false


  let from_given maybe_expression =
    match maybe_expression with
    | Some expression -> Given expression
    | None -> Missing


  let from_inferred type_ = Inferred type_

  let merge ~f left right =
    match left, right with
    | Inferred left, Inferred right -> Inferred (f left right)
    | Inferred type_, _
    | _, Inferred type_ ->
        Inferred type_
    | Given expression, _
    | _, Given expression ->
        Given expression
    | Missing, Missing -> Missing


  let join ~global_resolution = merge ~f:(GlobalResolution.join global_resolution)

  let meet ~global_resolution = merge ~f:(GlobalResolution.meet global_resolution)

  let to_yojson = function
    | Inferred type_ -> SerializableType.to_yojson type_
    | Given _
    | Missing ->
        `Null
end

module AnnotationsByName = struct
  module Base = struct
    module type S = sig
      type t [@@deriving show, compare, to_yojson]

      val identifying_name : t -> SerializableReference.t
    end

    module Make (Value : S) = struct
      type t = Value.t SerializableReference.Map.t

      let empty = SerializableReference.Map.empty

      let length = SerializableReference.Map.length

      let data map = SerializableReference.Map.data map |> List.sort ~compare:Value.compare

      let show map =
        map |> data |> List.map ~f:Value.show |> String.concat ~sep:"," |> Format.asprintf "[%s]"


      let to_yojson map = `List (map |> data |> List.map ~f:Value.to_yojson)

      let pp format map = show map |> Format.fprintf format "%s"

      let add_exn map value =
        let identifying_name = Value.identifying_name value in
        SerializableReference.Map.add_exn map ~key:identifying_name ~data:value


      let update_exn map name transform =
        let transform_or_raise = function
          | Some value -> transform value
          | None ->
              failwith
                (Format.asprintf
                   "Did not expect an update with name %a (expected one of %a)"
                   Reference.pp
                   name
                   pp
                   map)
        in
        SerializableReference.Map.update map name ~f:transform_or_raise


      let filter_not ~f = SerializableReference.Map.filter ~f:(fun value -> not (f value))
    end
  end

  module Combineable = struct
    module type S = sig
      include Base.S

      val combine : global_resolution:GlobalResolution.t -> t -> t -> t
    end

    module Make (Value : S) = struct
      include Base.Make (Value)

      let add ~global_resolution map value =
        let identifying_name = Value.identifying_name value in
        SerializableReference.Map.update map identifying_name ~f:(function
            | Some existing -> Value.combine ~global_resolution value existing
            | None -> value)


      let merge ~global_resolution left right =
        let combine ~key = function
          | `Left value
          | `Right value ->
              Some value
          | `Both (left, right) -> (
              try Some (Value.combine ~global_resolution left right) with
              | Analysis.ClassHierarchy.Untracked annotation ->
                  Statistics.event
                    ~name:"undefined type during type inference merge"
                    ~integers:[]
                    ~normals:["type", annotation; "reference", SerializableReference.show key]
                    ();
                  Some left)
        in
        SerializableReference.Map.merge ~f:combine left right
    end
  end

  include Combineable
end

module GlobalAnnotation = struct
  module Value = struct
    type t = {
      name: SerializableReference.t;
      location: AnnotationLocation.t;
      annotation: SerializableType.t;
    }
    [@@deriving show, to_yojson]

    let compare { location = left; _ } { location = right; _ } =
      AnnotationLocation.compare left right


    let qualified_name { name; location = { qualifier; _ }; _ } =
      [qualifier; name] |> List.bind ~f:Reference.as_list |> Reference.create_from_list


    let identifying_name = qualified_name

    let combine ~global_resolution left right =
      {
        left with
        annotation = GlobalResolution.join global_resolution left.annotation right.annotation;
      }
  end

  module ByName = AnnotationsByName.Make (Value)
  include Value

  let suppress { annotation; _ } = Type.is_none annotation
end

module AttributeAnnotation = struct
  module Value = struct
    type t = {
      parent: SerializableReference.t;
      name: SerializableReference.t;
      location: AnnotationLocation.t;
      annotation: SerializableType.t;
    }
    [@@deriving show, to_yojson]

    let compare { location = left; _ } { location = right; _ } =
      AnnotationLocation.compare left right


    let qualified_name { parent; name; location = { qualifier; _ }; _ } =
      [qualifier; parent; name] |> List.bind ~f:Reference.as_list |> Reference.create_from_list


    let identifying_name = qualified_name

    let combine ~global_resolution left right =
      {
        left with
        annotation = GlobalResolution.join global_resolution left.annotation right.annotation;
      }
  end

  module ByName = AnnotationsByName.Make (Value)
  include Value

  let suppress { annotation; _ } = Type.is_none annotation
end

module DefineAnnotation = struct
  module Parameters = struct
    module Value = struct
      type t = {
        name: SerializableReference.t;
        annotation: TypeAnnotation.t;
        value: DefaultValue.t;
        index: int;
      }
      [@@deriving show, to_yojson]

      (* Assumption: we never have two parameters with the same index *)
      let compare { index = left; _ } { index = right; _ } = Int.compare left right

      let identifying_name parameter = parameter.name

      let is_inferred { annotation; _ } = TypeAnnotation.is_inferred annotation

      let combine ~global_resolution left right =
        let annotation = TypeAnnotation.meet ~global_resolution left.annotation right.annotation in
        { left with annotation }
    end

    module ByName = AnnotationsByName.Make (Value)

    let any_inferred (parameters : ByName.t) : bool =
      Map.Tree.exists parameters ~f:Value.is_inferred
  end

  type t = {
    name: SerializableReference.t;
    parent: SerializableReference.t option;
    return: TypeAnnotation.t; [@compare.ignore]
    parameters: Parameters.ByName.t; [@compare.ignore]
    location: AnnotationLocation.t;
    async: bool;
  }
  [@@deriving show, compare, to_yojson]

  let is_inferred { return; parameters; _ } =
    TypeAnnotation.is_inferred return || Parameters.any_inferred parameters


  let add_inferred_return ~global_resolution define type_ =
    {
      define with
      return =
        TypeAnnotation.join ~global_resolution define.return (TypeAnnotation.from_inferred type_);
    }


  let add_inferred_parameter define name type_ =
    let sanitized_name = name |> Reference.sanitized in
    {
      define with
      parameters =
        Parameters.ByName.update_exn define.parameters sanitized_name (fun parameter ->
            { parameter with annotation = TypeAnnotation.from_inferred type_ });
    }
end

module Inference = struct
  type target =
    | Return
    | Parameter of { name: Reference.t }
    | Global of {
        name: Reference.t;
        location: Location.WithModule.t;
      }
    | Attribute of {
        parent: Reference.t;
        name: Reference.t;
        location: Location.WithModule.t;
      }
  [@@deriving show]

  type raw = {
    type_: Type.t;
    target: target;
  }
  [@@deriving show]

  type t = raw option [@@deriving show]

  let create { type_ = raw_type; target } =
    let is_parameter =
      match target with
      | Parameter _ -> true
      | _ -> false
    in
    let sanitized_type =
      raw_type
      |> Type.Variable.mark_all_free_variables_as_escaped
      |> Type.Variable.convert_all_escaped_free_variables_to_anys
      |> Type.infer_transform
    in
    let ignore =
      Type.contains_unknown sanitized_type
      || Type.contains_undefined sanitized_type
      || Type.contains_prohibited_any sanitized_type
      || (is_parameter && Type.equal sanitized_type NoneType)
    in
    if ignore then
      None
    else
      Some { type_ = sanitized_type; target }
end

module LocalResult = struct
  type t = {
    globals: GlobalAnnotation.ByName.t;
    attributes: AttributeAnnotation.ByName.t;
    define: DefineAnnotation.t;
  }
  [@@deriving show, to_yojson]

  let define_name { define = { name; _ }; _ } = name

  let from_signature
      ~global_resolution
      ~lookup
      ~qualifier
      {
        Node.value =
          {
            Statement.Define.signature = { name; parameters; return_annotation; parent; async; _ };
            _;
          };
        Node.location = define_location;
      }
    =
    let define =
      let open DefineAnnotation in
      let return = TypeAnnotation.from_given return_annotation in
      let parameters =
        let initialize_parameter
            index
            { Node.value = Expression.Parameter.{ name; annotation; value }; _ }
          =
          DefineAnnotation.Parameters.Value.
            {
              name = name |> Identifier.sanitized |> Reference.create;
              annotation = TypeAnnotation.from_given annotation;
              value;
              index;
            }
        in
        parameters
        |> List.mapi ~f:initialize_parameter
        |> List.fold ~init:Parameters.ByName.empty ~f:(Parameters.ByName.add ~global_resolution)
      in
      {
        name;
        parent;
        return;
        parameters;
        location = define_location |> AnnotationLocation.from_location ~lookup ~qualifier;
        async;
      }
    in
    {
      globals = GlobalAnnotation.ByName.empty;
      attributes = AttributeAnnotation.ByName.empty;
      define;
    }


  let add_inference
      ~global_resolution
      ~lookup
      ({ globals; attributes; define; _ } as result)
      inference
    =
    let add_inferred_type Inference.{ type_; target } =
      match target with
      | Inference.Return ->
          {
            result with
            define = DefineAnnotation.add_inferred_return ~global_resolution define type_;
          }
      | Inference.Parameter { name } ->
          { result with define = DefineAnnotation.add_inferred_parameter define name type_ }
      | Inference.Global { name; location } ->
          {
            result with
            globals =
              GlobalAnnotation.ByName.add
                ~global_resolution
                globals
                {
                  name;
                  annotation = type_;
                  location = location |> AnnotationLocation.from_location_with_module ~lookup;
                };
          }
      | Inference.Attribute { parent; name; location } ->
          {
            result with
            attributes =
              AttributeAnnotation.ByName.add
                ~global_resolution
                attributes
                {
                  parent;
                  name;
                  annotation = type_;
                  location = location |> AnnotationLocation.from_location_with_module ~lookup;
                };
          }
    in
    inference |> Option.map ~f:add_inferred_type |> Option.value ~default:result
end

module GlobalResult = struct
  module DefineAnnotationsByName = struct
    module Value = struct
      type t = DefineAnnotation.t [@@deriving show, compare, to_yojson]

      let identifying_name { DefineAnnotation.name; _ } = name
    end

    include AnnotationsByName.Base.Make (Value)
  end

  type t = {
    globals: GlobalAnnotation.ByName.t;
    attributes: AttributeAnnotation.ByName.t;
    defines: DefineAnnotationsByName.t;
  }
  [@@deriving show, to_yojson]

  let inference_count { globals; attributes; defines } =
    GlobalAnnotation.ByName.length globals
    + AttributeAnnotation.ByName.length attributes
    + DefineAnnotationsByName.length defines


  let empty =
    {
      globals = GlobalAnnotation.ByName.empty;
      attributes = AttributeAnnotation.ByName.empty;
      defines = DefineAnnotationsByName.empty;
    }


  let add_define ~define_names ~defines define =
    let name = DefineAnnotationsByName.Value.identifying_name define in
    (* Duplicate defines can occur, for example with certain decorator-based tools like
       typing.overloads. If we encounter a duplicate we skip that define entirely. *)
    if SerializableReference.Set.mem define_names name then
      define_names, SerializableReference.Map.remove defines name
    else
      let define_names = SerializableReference.Set.add define_names name in
      let defines =
        if DefineAnnotation.is_inferred define then
          DefineAnnotationsByName.add_exn defines define
        else
          defines
      in
      define_names, defines


  let add_local_result
      ~global_resolution
      (define_names, { globals; attributes; defines })
      {
        LocalResult.globals = globals_from_local;
        LocalResult.attributes = attributes_from_local;
        LocalResult.define;
        _;
      }
    =
    let define_names, defines = add_define ~define_names ~defines define in
    ( define_names,
      {
        globals = GlobalAnnotation.ByName.merge ~global_resolution globals globals_from_local;
        attributes =
          AttributeAnnotation.ByName.merge ~global_resolution attributes attributes_from_local;
        defines;
      } )


  let suppress_unhelpful_types { globals; attributes; defines } =
    {
      globals = globals |> GlobalAnnotation.ByName.filter_not ~f:GlobalAnnotation.suppress;
      attributes =
        attributes |> AttributeAnnotation.ByName.filter_not ~f:AttributeAnnotation.suppress;
      defines;
    }


  let from_local_results ~global_resolution local_results =
    local_results
    |> List.fold
         ~init:(SerializableReference.Set.empty, empty)
         ~f:(add_local_result ~global_resolution)
    |> snd
    |> suppress_unhelpful_types
end
