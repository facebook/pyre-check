# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


# Test the support of tracking taints from SQL-like strings to string
# operations, which uses multi-source rules and implicit sources

def user_controlled_input():
    return "evil"


global_query: str = "SELECT"


class DatabaseSchemaEditor:
    attribute_query: str = "SELECT"

    def string_operations(self, arg) -> None:
        user_controlled: str = user_controlled_input()
        self.attribute_query.format(user_controlled)  # Should see an issue


def string_operations(arg) -> None:
    query: str = "SELECT"
    user_controlled: str = user_controlled_input()

    query.format(user_controlled)  # Issue here: Positional argument
    query.format(data=user_controlled)  # Issue here: Keyword argument

    query + user_controlled  # Issue here: A model of str.__add__
    user_controlled + query  # Issue here: Another model of str.__add__

    query % user_controlled  # Issue here

    f"{query} {user_controlled}"  # Should see an issue, but fine to ignore
    f"{user_controlled} {query}"  # Should see an issue, but fine to ignore

    global_query.format(user_controlled)  # Should see an issue

    # Should see an issue
    DatabaseSchemaEditor.attribute_query.format(user_controlled)


global_query = ""  # Even with this reset, we should see an issue above
