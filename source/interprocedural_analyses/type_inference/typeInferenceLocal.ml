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
