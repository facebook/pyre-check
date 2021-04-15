(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open Ast
open Analysis

let parent_reference parent_type =
  parent_type
  |> Type.Variable.convert_all_escaped_free_variables_to_anys
  |> Type.infer_transform
  |> Format.asprintf "%a" Type.pp
  |> Reference.create


module AnnotationLocation = struct
  type t = {
    qualifier: Reference.t;
    path: string;
    line: int;
  }
  [@@deriving show]

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
  [@@deriving show]

  let is_inferred annotation = Option.is_some annotation.inferred

  let combine_with ~f left right =
    {
      inferred = Option.map2 ~f left.inferred right.inferred;
      given = (if Option.is_some left.given then left.given else right.given);
    }


  let join ~global_resolution = combine_with ~f:(GlobalResolution.join global_resolution)

  let meet ~global_resolution = combine_with ~f:(GlobalResolution.meet global_resolution)
end

module AnnotationsByName = struct
  module type S = sig
    type t [@@deriving show]

    val identifying_name : t -> Reference.t

    val combine : global_resolution:GlobalResolution.t -> t -> t -> t
  end

  module Make (Value : S) = struct
    type t = Value.t Reference.Map.Tree.t

    let empty = Reference.Map.Tree.empty

    let find = Reference.Map.Tree.find

    let data = Reference.Map.Tree.data

    let show map =
      map |> data |> List.map ~f:Value.show |> String.concat ~sep:"," |> Format.asprintf "[%s]"


    let pp format map = show map |> Format.fprintf format "%s"

    let add ~global_resolution map annotation =
      let identifying_name = Value.identifying_name annotation in
      Reference.Map.Tree.update map identifying_name ~f:(function
          | Some existing -> Value.combine ~global_resolution annotation existing
          | None -> annotation)
  end
end

module GlobalAnnotation = struct
  module Value = struct
    type t = {
      name: Reference.t;
      location: AnnotationLocation.t;
      annotation: TypeAnnotation.t;
    }
    [@@deriving show]

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
      parent: Reference.t;
      name: Reference.t;
      location: AnnotationLocation.t;
      annotation: TypeAnnotation.t;
    }
    [@@deriving show]

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
        name: Reference.t;
        annotation: TypeAnnotation.t;
        value: Expression.t option;
      }
      [@@deriving show]

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
    name: Reference.t;
    parent: Reference.t option;
    return: TypeAnnotation.t;
    parameters: Parameters.ByName.t;
    decorators: Statement.Decorator.t list;
    location: AnnotationLocation.t;
    async: bool;
    (* Only needed on the ocaml side, not to generate a stub *)
    abstract: bool;
  }
  [@@deriving show]

  let is_inferred { return; parameters; _ } =
    TypeAnnotation.is_inferred return || Parameters.any_inferred parameters
end

module InferenceResult = struct
  type t = {
    globals: GlobalAnnotation.ByName.t;
    attributes: AttributeAnnotation.ByName.t;
    define: DefineAnnotation.t;
    (* Temporary: keep the errors for compatiblity as we roll this out *)
    errors: AnalysisError.Instantiated.t list;
  }
  [@@deriving show]
end
