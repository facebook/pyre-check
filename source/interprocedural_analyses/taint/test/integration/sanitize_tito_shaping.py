# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import random


def a_source(): ...


def b_source(): ...


def a_sink(x): ...


def b_sink(x): ...


def sanitize_source_a(x):
    return x


def sanitize_source_b(x):
    return x


def sanitize_sink_a(x):
    return x


def sanitize_sink_b(x):
    return x


def sanitize_source_a_depth_two(x):
    return sanitize_source_a(x)


def sanitize_sink_a_depth_two(x):
    return sanitize_sink_a(x)


def transform_x(x):
    pass


def transform_y(x):
    pass


def transform_z(x):
    pass


def add_feature(x):
    return x


def tito_collapse_zero(x): ...


def tito_collapse_one_append_a_b_c(x):
    return {"a": {"b": {"c": x}}}


def tito_collapse_one(x):
    y = tito_collapse_one_append_a_b_c(x)
    return y["a"]["b"]["c"]


# Multiple titos with the same input and output path, only difference in sanitizers.
# We can potentially merge those into a single formal(x) -> LocalReturn tito.
def multiple_titos_same_path(x):
    if random.random() > 0.5:
        return x
    elif random.random() > 0.5:
        return add_feature(sanitize_source_a(x))
    elif random.random() > 0.5:
        return sanitize_source_b(x)
    elif random.random() > 0.5:
        return sanitize_sink_a_depth_two(x)
    elif random.random() > 0.5:
        return add_feature(sanitize_sink_b(x))
    elif random.random() > 0.5:
        return sanitize_sink_a_depth_two(sanitize_source_b(x))
    else:
        return sanitize_source_a(sanitize_sink_b(x))


# Leads to all combination of sanitizers.
def loop_multiple_titos_same_path(x):
    for _ in range(1000):
        x = multiple_titos_same_path(x)

    return x


def issue_multiple_titos_same_path():
    a_sink(multiple_titos_same_path(a_source()))
    b_sink(multiple_titos_same_path(b_source()))


def multiple_titos_different_output_paths(x):
    if random.random() > 0.5:
        return {"a": x}
    elif random.random() > 0.5:
        return {"b": add_feature(sanitize_source_a(x))}
    elif random.random() > 0.5:
        return {"c": sanitize_source_b(x)}
    elif random.random() > 0.5:
        return {"d": sanitize_sink_a_depth_two(x)}
    else:
        return {"e": sanitize_source_a(sanitize_sink_b(x))}


def loop_multiple_titos_different_output_paths(x):
    for _ in range(1000):
        x = multiple_titos_different_output_paths(x)

    return x


def issue_multiple_titos_different_output_paths():
    a_sink(multiple_titos_different_output_paths(a_source()))
    b_sink(multiple_titos_different_output_paths(b_source()))


def multiple_titos_different_input_paths(x):
    if random.random() > 0.5:
        return x["a"]
    elif random.random() > 0.5:
        return add_feature(sanitize_source_a(x["b"]))
    elif random.random() > 0.5:
        return sanitize_source_b(x["c"])
    elif random.random() > 0.5:
        return sanitize_sink_a_depth_two(x["d"])
    else:
        return add_feature(sanitize_source_a(sanitize_sink_b(x["e"])))


def loop_multiple_titos_different_input_paths(x):
    for _ in range(1000):
        x = multiple_titos_different_input_paths(x)

    return x


def issue_multiple_titos_different_input_paths():
    a_sink(multiple_titos_different_input_paths(a_source()))
    b_sink(multiple_titos_different_input_paths(b_source()))


def multiple_titos_different_input_output_paths(x):
    if random.random() > 0.5:
        return {"a": x["a"]}
    elif random.random() > 0.5:
        return {"b": add_feature(sanitize_source_a(x["b"]))}
    elif random.random() > 0.5:
        return {"c": sanitize_source_b(x["c"])}
    elif random.random() > 0.5:
        return {"d": sanitize_sink_a_depth_two(x["d"])}
    else:
        return {"e": add_feature(sanitize_source_a(sanitize_sink_b(x["e"])))}


def loop_multiple_titos_different_input_output_paths(x):
    for _ in range(1000):
        x = multiple_titos_different_input_output_paths(x)

    return x


def issue_multiple_titos_different_input_output_paths():
    a_sink(multiple_titos_different_input_output_paths(a_source()))
    b_sink(multiple_titos_different_input_output_paths(b_source()))


def multiple_titos_same_path_different_collapse_depth(x):
    if random.random() > 0.5:
        return tito_collapse_zero(x)
    elif random.random() > 0.5:
        return tito_collapse_one(sanitize_source_a(x))
    else:
        return tito_collapse_zero(sanitize_sink_a_depth_two(x))


# Multiple titos with the same input and output path, with transforms and sanitizers.
# We can only merge titos with the same transforms.
def multiple_titos_same_path_with_transform(x):
    if random.random() > 0.5:
        return x
    elif random.random() > 0.5:
        return sanitize_source_a(x)
    elif random.random() > 0.5:
        return transform_x(x)
    elif random.random() > 0.5:
        return transform_x(sanitize_source_a(x))
    elif random.random() > 0.5:
        return sanitize_source_a(transform_x(x))
    elif random.random() > 0.5:
        return transform_y(x)
    else:
        return sanitize_sink_a_depth_two(transform_y(x))


def issue_multiple_titos_same_path_with_transform():
    a_sink(multiple_titos_same_path_with_transform(a_source()))
    b_sink(multiple_titos_same_path_with_transform(b_source()))
