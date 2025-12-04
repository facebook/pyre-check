# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from pysa import _test_sink


def sink_on_return():
    # Mark the expression used in a return statement as a sink
    return 0


def sink_on_return_not_propagated_to_arg(arg):
    # We do not expect the sink to be propagated
    arg = sink_on_return()


def sink_on_return_not_propagated_to_return():
    # We do not expect the sink to be propagated
    return sink_on_return()


def sink_on_return_and_tito(arg, arg2):
    return arg


def propagate_returned_sink_to_arg(arg):
    # Sink is first generated and then propagated to arg
    sink_on_return_and_tito(arg)


def not_propagate_returned_sink_to_arg(arg):
    sink_on_return_and_tito(0, arg)
