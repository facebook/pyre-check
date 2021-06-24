(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open TypeInferenceData

let infer_for_define ~configuration ~global_resolution ~source ~qualifier ~define =
  let errors =
    Analysis.Inference.infer_for_define ~configuration ~global_resolution ~source ~define
  in
  let lookup = TypeInferenceData.lookup ~configuration ~global_resolution in
  List.fold
    ~init:(LocalResult.from_signature ~global_resolution ~lookup ~qualifier define)
    ~f:(LocalResult.add_missing_annotation_error ~global_resolution ~lookup)
    errors


let empty_infer_for_define ~configuration ~global_resolution ~qualifier ~define =
  let lookup = TypeInferenceData.lookup ~configuration ~global_resolution in
  TypeInferenceData.LocalResult.from_signature ~global_resolution ~lookup ~qualifier define


let infer_for_module
    ~configuration
    ~global_resolution
    ~source:({ Ast.Source.source_path = { qualifier; _ } as source_path; _ } as source)
  =
  if Analysis.Inference.skip_infer ~configuration source_path then
    []
  else (
    Log.debug "Running infer for %s..." source_path.relative;
    let check define =
      infer_for_define ~configuration ~global_resolution ~source ~qualifier ~define
    in
    source |> Preprocessing.defines ~include_toplevels:true |> List.map ~f:check )
