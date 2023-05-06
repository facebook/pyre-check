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

    global_query.format(user_controlled)  # Issue here

    # TODO(T145247918): Should see an issue
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
    format_string_triggered_user_controlled(user_controlled)


def format_string_triggered_sql(arg):
    user_controlled = user_controlled_input()
    f"{user_controlled}{arg}"


def format_string_issue_with_triggered_sql():
    query: str = "SELECT"
    format_string_triggered_sql(query)


def format_string_multiple_triggered_user_controlled(arg1, arg2):
    f"{arg1} SELECT {arg2}"


def format_string_issue_with_multiple_triggered_user_controlled():
    user_controlled = user_controlled_input()
    format_string_multiple_triggered_user_controlled(user_controlled, 0)
    format_string_multiple_triggered_user_controlled(0, user_controlled)


def nested_stradd_and_fstring():
    x: str = user_controlled_input()
    y = "xyz"
    return "abc" + f"{x + y}"


def stradd_triggered_user_controlled(arg):
    x: str = user_controlled_input()
    x + arg.f  # The triggered sink should be on arg.f, not arg


def test_large_string_add():
    db_dir = "/mnt"
    wal_dir = "/mnt"
    key_size = 1
    value_size = 2
    block_size = 10
    cache_size = 1
    M = 1
    G = 2
    K = 3

    const_params = (
        " --db="
        + str(db_dir)
        + " --wal_dir="
        + str(wal_dir)
        + " --num_levels="
        + str(6)
        + " --key_size="
        + str(key_size)
        + " --value_size="
        + str(value_size)
        + " --block_size="
        + str(block_size)
        + " --cache_size="
        + str(cache_size)
        + " --cache_numshardbits="
        + str(6)
        + " --compression_type="
        + str("snappy")
        + " --compression_ratio="
        + str(0.5)
        + " --write_buffer_size="
        + str(int(128 * M))
        + " --max_write_buffer_number="
        + str(2)
        + " --target_file_size_base="
        + str(int(128 * M))
        + " --max_bytes_for_level_base="
        + str(int(1 * G))
        + " --sync="
        + str(0)
        + " --verify_checksum="
        + str(1)
        + " --delete_obsolete_files_period_micros="
        + str(int(60 * M))
        + " --statistics="
        + str(1)
        + " --stats_per_interval="
        + str(1)
        + " --stats_interval="
        + str(int(1 * M))
        + " --histogram="
        + str(1)
        + " --memtablerep="
        + str("skip_list")
        + " --bloom_bits="
        + str(10)
        + " --open_files="
        + str(int(20 * K))
    )
