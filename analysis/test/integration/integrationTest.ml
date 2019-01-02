(** Copyright (c) 2016-present, Facebook, Inc.

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)


let assert_type_errors =
  Test.assert_errors ~check:Analysis.TypeCheck.check
