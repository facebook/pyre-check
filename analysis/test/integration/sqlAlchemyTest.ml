(* Copyright (c) 2016-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree. *)

open OUnit2
open IntegrationTest
open Test

let test_declarative_base context =
  let assert_sql_alchemy_errors source =
    let sql_alchemy_stubs =
      [
        {
          handle = "sqlalchemy/ext/declarative/__init__.pyi";
          source =
            {|
            from .api import (
                declarative_base as declarative_base,
                DeclarativeMeta as DeclarativeMeta,
            )
          |};
        };
        {
          handle = "sqlalchemy/ext/declarative/api.pyi";
          source =
            {|
            def declarative_base(bind: Optional[Any] = ..., metadata: Optional[Any] = ...,
                                 mapper: Optional[Any] = ..., cls: Any = ..., name: str = ...,
                                 constructor: Any = ..., class_registry: Optional[Any] = ...,
                                 metaclass: Any = ...): ...

            class DeclarativeMeta(type):
                def __init__(cls, classname, bases, dict_) -> None: ...
                def __setattr__(cls, key, value): ...
          |};
        };
      ]
    in
    assert_type_errors ~context ~update_environment_with:sql_alchemy_stubs source
  in
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
