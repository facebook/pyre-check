(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Pyre
open Ast
open Analysis

let type_to_string type_ =
  type_
  |> Type.Variable.convert_all_escaped_free_variables_to_anys
  |> Type.infer_transform
  |> Format.asprintf "%a" Type.pp


let type_to_reference type_ = type_ |> type_to_string |> Reference.create

let expression_to_json expression = `String (expression |> Expression.sanitized |> Expression.show)

let lookup ~configuration ~global_resolution reference =
  GlobalResolution.ast_environment global_resolution
  |> fun ast_environment ->
  AstEnvironment.ReadOnly.get_real_path_relative ~configuration ast_environment reference


module SerializableReference = struct
  type t = Reference.t [@@deriving compare, eq, sexp, hash, show]

  let to_yojson reference = `String (Reference.show_sanitized reference)

  module Map = Reference.Map.Tree
end

module DefaultValue = struct
  type t = Expression.t option [@@deriving show, eq]

  let to_yojson value = value >>| expression_to_json |> Option.value ~default:`Null
end

module AnnotationLocation = struct
  type t = {
    qualifier: SerializableReference.t;
    path: string;
    line: int;
  }
  [@@deriving show, eq, compare, to_yojson]

  let create ~lookup ~qualifier ~line =
    { qualifier; path = lookup qualifier |> Option.value ~default:"*"; line }


  let from_location ~lookup ~qualifier Location.{ start = { line; _ }; _ } =
    create ~lookup ~qualifier ~line


  let from_location_with_module
      ~lookup
      Location.WithModule.{ path = qualifier; start = { line; _ }; _ }
    =
    create ~lookup ~qualifier ~line
end

module TypeAnnotation = struct
  type t = {
    inferred: Type.t option;
    given: Type.t option;
  }
  [@@deriving show, eq]

  let is_inferred annotation = Option.is_some annotation.inferred

  let combine_with ~f left right =
    {
      inferred =
        ( match left.inferred, right.inferred with
        | Some left, Some right -> Some (f left right)
        | None, right -> right
        | left, None -> left );
      given = (if Option.is_some left.given then left.given else right.given);
    }


  let join ~global_resolution = combine_with ~f:(GlobalResolution.join global_resolution)

  let meet ~global_resolution = combine_with ~f:(GlobalResolution.meet global_resolution)

  let from_given ~global_resolution given =
    let parser = GlobalResolution.annotation_parser global_resolution in
    let given = given >>| parser.parse_annotation in
    { inferred = None; given }


  let from_inferred inferred = { inferred = Some inferred; given = None }

  let to_yojson { inferred; given } =
    match inferred, given with
    | Some inferred, _ -> `String (type_to_string inferred)
    | None, Some given -> `String (type_to_string given)
    | _ -> `Null
end

module AnnotationsByName = struct
  module Base = struct
    module type S = sig
      type t [@@deriving show, eq, compare, to_yojson]

      val identifying_name : t -> SerializableReference.t
    end

    module Make (Value : S) = struct
      type t = Value.t SerializableReference.Map.t

      let empty = SerializableReference.Map.empty

      let length = SerializableReference.Map.length

      let find = SerializableReference.Map.find

      let data map = SerializableReference.Map.data map |> List.sort ~compare:Value.compare

      let show map =
        map |> data |> List.map ~f:Value.show |> String.concat ~sep:"," |> Format.asprintf "[%s]"


      let equal = SerializableReference.Map.equal Value.equal

      let to_yojson map = `List (map |> data |> List.map ~f:Value.to_yojson)

      let pp format map = show map |> Format.fprintf format "%s"

      let add_exn map value =
        let identifying_name = Value.identifying_name value in
        SerializableReference.Map.add_exn map ~key:identifying_name ~data:value


      let update_exn map value transform =
        let transform_or_raise = function
          | Some value -> transform value
          | None -> failwith "Did not expect to update with a missing name"
        in
        SerializableReference.Map.update map value ~f:transform_or_raise
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
        let combine ~key:_ = function
          | `Left value
          | `Right value ->
              Some value
          | `Both (left, right) -> Some (Value.combine ~global_resolution left right)
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
      annotation: TypeAnnotation.t;
    }
    [@@deriving show, eq, to_yojson]

    let compare { location = left; _ } { location = right; _ } =
      AnnotationLocation.compare left right


    let qualified_name { name; location = { qualifier; _ }; _ } =
      [qualifier; name] |> List.bind ~f:Reference.as_list |> Reference.create_from_list


    let identifying_name = qualified_name

    let combine ~global_resolution left right =
      {
        left with
        annotation = TypeAnnotation.join ~global_resolution left.annotation right.annotation;
      }
  end

  module ByName = AnnotationsByName.Make (Value)
  include Value
end

module AttributeAnnotation = struct
  module Value = struct
    type t = {
      parent: SerializableReference.t;
      name: SerializableReference.t;
      location: AnnotationLocation.t;
      annotation: TypeAnnotation.t;
    }
    [@@deriving show, eq, to_yojson]

    let compare { location = left; _ } { location = right; _ } =
      AnnotationLocation.compare left right


    let qualified_name { parent; name; location = { qualifier; _ }; _ } =
      [qualifier; parent; name] |> List.bind ~f:Reference.as_list |> Reference.create_from_list


    let identifying_name = qualified_name

    let combine ~global_resolution left right =
      {
        left with
        annotation = TypeAnnotation.join ~global_resolution left.annotation right.annotation;
      }
  end

  module ByName = AnnotationsByName.Make (Value)
  include Value
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
      [@@deriving show, eq, to_yojson]

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
    decorators: Ast.Statement.Decorator.t list;
    location: AnnotationLocation.t;
    async: bool;
  }
  [@@deriving show, eq, compare, to_yojson]

  let is_inferred { return; parameters; _ } =
    TypeAnnotation.is_inferred return || Parameters.any_inferred parameters


  let add_inferred_return ~global_resolution define type_ =
    {
      define with
      return =
        TypeAnnotation.join ~global_resolution define.return (TypeAnnotation.from_inferred type_);
    }


  let add_inferred_parameter define name type_ =
    {
      define with
      parameters =
        Parameters.ByName.update_exn define.parameters name (fun parameter ->
            { parameter with annotation = TypeAnnotation.from_inferred type_ });
    }
end

module LocalResult = struct
  type t = {
    globals: GlobalAnnotation.ByName.t;
    attributes: AttributeAnnotation.ByName.t;
    define: DefineAnnotation.t;
    (* Temporary: keep the errors for compatiblity as we roll this out *)
    errors: AnalysisError.Instantiated.t list;
    (* Used to skip inferring abstract return types *)
    abstract: bool;
  }
  [@@deriving show]

  let from_signature
      ~global_resolution
      ~lookup
      ~qualifier
      {
        Node.value =
          {
            Statement.Define.signature =
              {
                name = { Node.value = define_name; _ };
                parameters;
                return_annotation;
                decorators;
                parent;
                async;
                _;
              } as signature;
            _;
          };
        Node.location = define_location;
      }
    =
    let define =
      let open DefineAnnotation in
      let return = TypeAnnotation.from_given ~global_resolution return_annotation in
      let parameters =
        let initialize_parameter
            index
            { Node.value = Expression.Parameter.{ name; annotation; value }; _ }
          =
          DefineAnnotation.Parameters.Value.
            {
              name = name |> Reference.create;
              annotation = TypeAnnotation.from_given ~global_resolution annotation;
              value;
              index;
            }
        in
        parameters
        |> List.mapi ~f:initialize_parameter
        |> List.fold ~init:Parameters.ByName.empty ~f:(Parameters.ByName.add ~global_resolution)
      in
      {
        name = define_name;
        parent;
        return;
        parameters;
        decorators;
        location = define_location |> AnnotationLocation.from_location ~lookup ~qualifier;
        async;
      }
    in
    {
      globals = GlobalAnnotation.ByName.empty;
      attributes = AttributeAnnotation.ByName.empty;
      define;
      errors = [];
      abstract = Statement.Define.Signature.is_abstract_method signature;
    }


  let add_missing_annotation_error
      ~global_resolution
      ~lookup
      ({ globals; attributes; define; errors; abstract } as result)
      error
    =
    let result =
      {
        result with
        errors = AnalysisError.instantiate ~show_error_traces:true ~lookup error :: errors;
      }
    in
    let ignore type_ =
      Type.is_untyped type_
      || Type.contains_unknown type_
      || Type.Variable.convert_all_escaped_free_variables_to_anys type_
         |> Type.contains_prohibited_any
    in
    let open AnalysisError in
    match error.kind with
    | MissingReturnAnnotation { annotation = Some type_; _ } when not (ignore type_ || abstract) ->
        {
          result with
          define = DefineAnnotation.add_inferred_return ~global_resolution define type_;
        }
    | MissingParameterAnnotation { name; annotation = Some type_; _ }
      when not (ignore type_ || Type.equal type_ NoneType) ->
        { result with define = DefineAnnotation.add_inferred_parameter define name type_ }
    | MissingAttributeAnnotation
        { parent; missing_annotation = { name; annotation = Some type_; _ } }
      when not (ignore type_) ->
        {
          result with
          attributes =
            AttributeAnnotation.ByName.add
              ~global_resolution
              attributes
              {
                parent = type_to_reference parent;
                name;
                annotation = TypeAnnotation.from_inferred type_;
                location = error.location |> AnnotationLocation.from_location_with_module ~lookup;
              };
        }
    | MissingGlobalAnnotation { name; annotation = Some type_; _ } when not (ignore type_) ->
        {
          result with
          globals =
            GlobalAnnotation.ByName.add
              ~global_resolution
              globals
              {
                name;
                annotation = TypeAnnotation.from_inferred type_;
                location = error.location |> AnnotationLocation.from_location_with_module ~lookup;
              };
        }
    | _ -> result


  let get_errors { errors; _ } = List.rev errors
end

module GlobalResult = struct
  module DefineAnnotationsByName = struct
    module Value = struct
      type t = DefineAnnotation.t [@@deriving show, eq, compare, to_yojson]

      let identifying_name { DefineAnnotation.name; _ } = name
    end

    include AnnotationsByName.Base.Make (Value)
  end

  type t = {
    globals: GlobalAnnotation.ByName.t;
    attributes: AttributeAnnotation.ByName.t;
    defines: DefineAnnotationsByName.t;
  }
  [@@deriving show, eq, to_yojson]

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


  let add_local_result
      ~global_resolution
      { globals; attributes; defines }
      {
        LocalResult.globals = globals_from_local;
        LocalResult.attributes = attributes_from_local;
        LocalResult.define;
        _;
      }
    =
    let defines =
      if DefineAnnotation.is_inferred define then
        DefineAnnotationsByName.add_exn defines define
      else
        defines
    in
    {
      globals = GlobalAnnotation.ByName.merge ~global_resolution globals globals_from_local;
      attributes =
        AttributeAnnotation.ByName.merge ~global_resolution attributes attributes_from_local;
      defines;
    }
end
