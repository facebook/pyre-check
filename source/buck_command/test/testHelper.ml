(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Base
open OUnit2

let setup_scratch_directory ~context relatives_and_contents =
  let root = bracket_tmpdir context |> PyrePath.create_absolute in
  List.iter relatives_and_contents ~f:(fun (relative, content) ->
      let file = PyrePath.create_relative ~root ~relative in
      File.create file ~content |> File.write);
  root
