# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from typing import List

glob: List[int] = []


class MyClass:
    def some_entrypoint_function():
        glob.append()


def nested_run():
    def do_the_thing():
        glob.append(1)

    do_the_thing()


def immediate_examples():
    glob.append(1)


def this_one_shouldnt_be_found():
    glob.append(1)


def transitive_call_with_globals_passed_in(local_list: List[int]):
    local_list.append(1)


def transitive_call_accessing_globals():
    glob.append(1)


def leak_globals_by_passing_in():
    transitive_call_with_globals_passed_in(glob)


def leak_globals_by_transitive_call():
    transitive_call_accessing_globals()


def get_these():
    immediate_examples()
    leak_globals_by_passing_in()
    leak_globals_by_transitive_call()
    nested_run()


def main():
    get_these()
    this_one_shouldnt_be_found()
    MyClass().some_entrypoint_function()


if __name__ == "__main__":
    main()
