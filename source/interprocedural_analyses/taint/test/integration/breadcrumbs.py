# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from builtins import _test_sink, _test_source
import typing


def int_source() -> int:
    return _test_source()


def float_source() -> float:
    return _test_source()


def bool_source() -> bool:
    return _test_source()


def int_parameter(x, y: int):
    _test_sink(y)


def float_parameter(x, y: float):
    _test_sink(y)


def bool_parameter(x, y: bool):
    _test_sink(y)


class TpmRequest:
    id_float: float = ...
    ids_list: typing.List[int] = ...

    def __init__(
        self,
        id_float: float,
        ids_list: typing.List[int],
    ) -> None:
        self.id_float = id_float
        self.ids_list = ids_list


def tpm_request() -> TpmRequest: ...


def scalar_attribute_backward(request: TpmRequest):
    if 1 > 1:
        _test_sink(request.id_float)
    elif 1 > 1:
        _test_sink(request.ids_list)
    elif 1 > 1:
        # Read from a scalar variable
        _test_sink(" ".join(str(i) for i in request.ids_list))
    else:
        # Write into a scalar variable
        id = request.id_float
        return id


def scalar_attribute_forward():
    request = tpm_request()
    if 1 > 1:
        return request.id_float  # No scalar
    elif 1 > 1:
        return request.ids_list  # No scalar
    elif 1 > 1:
        # Read from a scalar variable
        return " ".join(str(i) for i in request.ids_list)
    else:
        # Write into a scalar variable
        id = request.id_float
        return id
