(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest

let test_declarative_base context =
  let assert_sql_alchemy_errors = assert_type_errors ~context in
  assert_sql_alchemy_errors
    {|
      from sqlalchemy.ext.declarative import declarative_base
      Base = declarative_base()

      class User(Base):
          __tablename__ = 'users'
    |}
    [];
  ()


let () = "sqlAlchemy" >::: ["declarative_base" >:: test_declarative_base] |> Test.run
