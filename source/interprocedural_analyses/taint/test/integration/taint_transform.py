# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


def source_a():
    ...


def sink_b(arg):
    ...


def transform_x(arg):
    pass


def transform_y(arg):
    pass


def transform_z(arg):
    pass


def taint_with_transform_x():
    alpha = source_a()
    beta = transform_x(alpha)
    return beta


def transform_x_obscure(arg):
    ...


def taint_with_transform_x_obscure():
    alpha = source_a()
    beta = transform_x_obscure(alpha)
    return beta


def transform_x_skip_obscure(arg):
    ...


def taint_with_transform_x_skip_obscure():
    alpha = source_a()
    beta = transform_x_skip_obscure(alpha)
    return beta


def taint_with_transform_yx():
    alpha = source_a()
    beta = transform_x(alpha)
    gamma = transform_y(beta)
    return gamma


def a_to_b_no_issue():
    alpha = source_a()
    sink_b(alpha)


def a_to_x_to_b_issue():
    alpha = source_a()
    beta = transform_x(alpha)
    sink_b(beta)


def a_to_z_to_y_to_b_no_issue():
    alpha = source_a()
    beta = transform_z(alpha)
    gamma = transform_y(beta)
    sink_b(gamma)


def a_to_y_to_z_to_b_issue():
    alpha = source_a()
    beta = transform_y(alpha)
    gamma = transform_z(beta)
    sink_b(gamma)
