# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.


def source_a():
    ...


def sink_b(arg):
    ...


def source_c():
    ...


def sink_d(arg):
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


def no_source_taint_with_transform_z():
    alpha = source_a()
    beta = transform_z(alpha)
    return beta


def no_source_taint_with_transform_zx():
    alpha = source_a()
    beta = transform_x(alpha)
    gamma = transform_z(beta)
    return gamma


def a_to_b_no_issue():
    alpha = source_a()
    sink_b(alpha)


def a_to_x_to_b_issue():
    alpha = source_a()
    beta = transform_x(alpha)
    sink_b(beta)


def a_to_x_to_x_b_no_issue():
    alpha = source_a()
    beta = transform_x(alpha)
    gamma = transform_x(beta)
    sink_b(gamma)


def c_to_d_issue():
    alpha = source_c()
    sink_d(alpha)


def c_to_x_to_d_no_issue():
    alpha = source_c()
    beta = transform_x(alpha)
    sink_d(beta)


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


def sink_taint_with_transform_x(arg):
    alpha = transform_x(arg)
    sink_b(alpha)


def no_sink_taint_with_transform_y(arg):
    alpha = transform_y(arg)
    sink_b(alpha)


def sink_taint_with_transform_yz(arg):
    alpha = transform_y(arg)
    beta = transform_z(alpha)
    sink_b(beta)


def tito_with_transform_x(arg):
    alpha = transform_x(arg)
    return alpha


def tito_with_transform_yz(arg):
    alpha = transform_y(arg)
    beta = transform_z(alpha)
    return beta


def nested_tito_transform_x(arg):
    alpha = tito_with_transform_x(arg)
    return alpha


def source_taint_via_tito_tranform_yz():
    alpha = source_a()
    beta = tito_with_transform_yz(alpha)
    return beta


def sink_taint_via_tito_transform_yz(arg):
    alpha = tito_with_transform_yz(arg)
    sink_b(alpha)


def ayz_to_b_issue():
    alpha = source_taint_via_tito_tranform_yz()
    sink_b(alpha)


def a_to_yzb_issue():
    alpha = source_a()
    sink_taint_with_transform_yz(alpha)


def source_taint_with_transform_y():
    alpha = source_a()
    beta = transform_y(alpha)
    return beta


def sink_taint_with_transform_z(arg):
    alpha = transform_z(arg)
    sink_b(alpha)


def ay_to_zb_issue():
    alpha = source_taint_with_transform_y()
    sink_taint_with_transform_z(alpha)


def ay_to_z_to_b_issue():
    alpha = source_taint_with_transform_y()
    beta = transform_z(alpha)
    sink_b(beta)


def a_to_yz_to_b_issue():
    alpha = source_a()
    beta = tito_with_transform_yz(alpha)
    sink_b(beta)


def a_to_yz_to_d_no_issue():
    alpha = source_a()
    beta = tito_with_transform_yz(alpha)
    sink_d(beta)


def c_to_yz_to_b_no_issue():
    alpha = source_c()
    beta = tito_with_transform_yz(alpha)
    sink_b(beta)


def a_to_y_zb_issue():
    alpha = source_a()
    beta = transform_y(alpha)
    sink_taint_with_transform_z(beta)


def ax_to_zb_no_issue():
    alpha = taint_with_transform_x_obscure()
    sink_taint_with_transform_z(alpha)


def issue_with_repeated_transforms():
    alpha = taint_with_transform_yx()
    sink_taint_with_transform_yz(alpha)


def test_infinite_loop():
    x = source_a()
    while True:
        x = tito_with_transform_x(x)

        if x:
            return x
