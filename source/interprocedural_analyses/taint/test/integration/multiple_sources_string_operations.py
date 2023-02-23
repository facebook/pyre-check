# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


# Test the support of tracking flows from normal sources to implicit sinks
# that are introduced onto expressions based on their runtime values (such as
# SQL strings). Under the hood, the analysis leverages the multi-source rules.

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

    global_query.format(user_controlled)  # Should see an issue

    # Should see an issue
    DatabaseSchemaEditor.attribute_query.format(user_controlled)


global_query = ""  # Even with this reset, we should see an issue above


def format_string_issue_string_literal():
    user_controlled = user_controlled_input()
    f"SELECT{user_controlled}"


def format_string_multiple_issues_string_literal():
    user_controlled = user_controlled_input()
    f"SELECT{user_controlled}"
    f"SELECT{user_controlled}"


def format_string_issue():
    query: str = "SELECT"
    user_controlled = user_controlled_input()
    f"{query}{user_controlled}"
    x = 0
    f"{query}{user_controlled}{x}"


def format_string_triggered_user_controlled(arg):
    query: str = "SELECT"
    f"{query}{arg}"


def format_string_issue_with_triggered_user_controlled():
    user_controlled = user_controlled_input()
    # TODO(T144475492): False negative
    format_string_triggered_user_controlled(user_controlled)


def format_string_triggered_sql(arg):
    user_controlled = user_controlled_input()
    f"{user_controlled}{arg}"


def format_string_issue_with_triggered_sql():
    query: str = "SELECT"
    # TODO(T144475492): False negative
    format_string_triggered_sql(query)


def format_string_multiple_triggered_user_controlled(arg1, arg2):
    f"{arg1} SELECT {arg2}"


def format_string_issue_with_multiple_triggered_user_controlled():
    user_controlled = user_controlled_input()
    # TODO(T144475492): False negative
    format_string_multiple_triggered_user_controlled(user_controlled, 0)
    # TODO(T144475492): False negative
    format_string_multiple_triggered_user_controlled(0, user_controlled)
