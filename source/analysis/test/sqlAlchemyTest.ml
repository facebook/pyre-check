(*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open Core
open OUnit2
open Test

let test_transform_environment context =
  let assert_equivalent_attributes = assert_equivalent_attributes ~context in
  assert_equivalent_attributes
    ~source:
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
        non_column: str = "foo"
        __private_attribute: str = "bar"

        def some_helper(self) -> None: ...
    |}
    ~class_name:"User"
    {|
        class User:
          __tablename__: str = 'users'
          id: sqlalchemy.Column[int] = sqlalchemy.Column(sqlalchemy.Integer(), primary_key=True)
          age: sqlalchemy.Column[typing.Optional[int]] = sqlalchemy.Column(sqlalchemy.Integer(), primary_key=False)
          income: sqlalchemy.Column[typing.Optional[int]] = sqlalchemy.Column(sqlalchemy.Integer())
          non_column: str = "foo"
          __private_attribute: str = "bar"
          __table__: sqlalchemy.sql.schema.Table = sqlalchemy.sql.schema.Table()
          metadata: sqlalchemy.sql.schema.MetaData = sqlalchemy.sql.schema.MetaData()

          def __init__(
            self,
            *,
            age: typing.Optional[int] = ...,
            id: int = ...,
            income: typing.Optional[int] = ...,
            non_column: str = ...,
          ) -> None: ...

          def some_helper(self) -> None: ...
      |};
  assert_equivalent_attributes
    ~source:
      {|
      from sqlalchemy.ext.declarative import declarative_base
      from sqlalchemy import Column, Integer
      from typing import Optional
      Base = declarative_base()

      class UserWithExistingConstructor(Base):
        __tablename__ = 'users'
        id: Column[int] = Column(Integer(), primary_key=True)

        # User-defined constructor is allowed to have any signature.
        def __init__(self) -> None: ...
    |}
    ~class_name:"UserWithExistingConstructor"
    {|
        class UserWithExistingConstructor(Base):
          __tablename__: str = 'users'
          id: sqlalchemy.Column[int] = sqlalchemy.Column(sqlalchemy.Integer(), primary_key=True)
          __table__: sqlalchemy.sql.schema.Table = sqlalchemy.sql.schema.Table()
          metadata: sqlalchemy.sql.schema.MetaData = sqlalchemy.sql.schema.MetaData()

          # User-defined constructor is allowed to have any signature.
          def __init__(self) -> None: ...
      |};
  assert_equivalent_attributes
    ~source:
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
        non_column: str = "foo"
        __private_attribute: str = "bar"

        def some_helper(self) -> None: ...

      class Employee(User):
        __tablename__ = 'employees'
        badge_number: Column[Optional[int]] = Column(Integer())
        # Income in words, let's say. This overrides the attribute in User.
        income: Column[Optional[str]] = Column(String())
    |}
    ~class_name:"Employee"
    {|
        class Employee(User):
          __tablename__ = 'employees'
          badge_number: sqlalchemy.Column[typing.Optional[int]] = sqlalchemy.Column(sqlalchemy.Integer())
          # Income in words, let's say. This overrides the attribute in User.
          income: sqlalchemy.Column[typing.Optional[str]] = sqlalchemy.Column(sqlalchemy.String())
          __table__: sqlalchemy.sql.schema.Table = sqlalchemy.sql.schema.Table()
          metadata: sqlalchemy.sql.schema.MetaData = sqlalchemy.sql.schema.MetaData()

          def __init__(
            self,
            *,
            age: typing.Optional[int] = ...,
            badge_number: typing.Optional[int] = ...,
            id: int = ...,
            income: typing.Optional[str] = ...,
            non_column: str = ...,
          ) -> None: ...
      |};

  ()


let () =
  "sqlAlchemy"
  >::: ["transform_environment" >: test_case ~length:Long test_transform_environment]
  |> Test.run
