(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open OUnit2
open IntegrationTest

let test_declarative_base context =
  let assert_sql_alchemy_errors = assert_type_errors ~context in
  assert_sql_alchemy_errors
    {|
      from sqlalchemy.ext.declarative import declarative_base
      from sqlalchemy import Column, Integer
      from typing import Optional
      Base = declarative_base()

      class User(Base):
        __tablename__ = 'users'
        id: Column[int] = Column(Integer(), primary_key=True)
        age: Column[Optional[int]] = Column(Integer(), primary_key=False)
        income: Column[Optional[int]] = Column(Integer())
      user: User
      reveal_type(user.id)
      reveal_type(user.age)
      reveal_type(user.income)
    |}
    [
      "Revealed type [-1]: Revealed type for `user.id` is `int`.";
      "Revealed type [-1]: Revealed type for `user.age` is `Optional[int]`.";
      "Revealed type [-1]: Revealed type for `user.income` is `Optional[int]`.";
    ];
  assert_sql_alchemy_errors
    {|
      from sqlalchemy.ext.declarative import declarative_base
      from sqlalchemy import Column, Integer
      from typing import Optional
      Base = declarative_base()

      class User(Base):
        __tablename__ = 'users'
        id: Column[int] = Column(Integer(), primary_key=True)
        age: Column[Optional[int]] = Column(Integer(), primary_key=False)
        income: Column[Optional[int]] = Column(Integer())

      user1: User = User()
      user2: User = User(id=1)
      user3: User = User(age=2)

      user4: User = User(1, 2, 3)
    |}
    [
      "Too many arguments [19]: Call `User.__init__` expects 0 positional arguments, 3 were \
       provided.";
    ];
  ()


let () = "sqlAlchemy" >::: ["declarative_base" >:: test_declarative_base] |> Test.run
