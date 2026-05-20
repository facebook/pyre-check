from typing import Generic, TypeVar

import torch.nn as nn
from pyre_extensions import ListVariadic


N = TypeVar("N", bound=int)

C_in = TypeVar("C_in", bound=int)
C_out = TypeVar("C_out", bound=int)

H_in = TypeVar("H_in", bound=int)
H_out = TypeVar("H_out", bound=int)

W_in = TypeVar("W_in", bound=int)
W_out = TypeVar("W_out", bound=int)


KERNEL_SIZE = TypeVar("KERNEL_SIZE", bound=int)
STRIDE = TypeVar("STRIDE", bound=int)
PADDING = TypeVar("PADDING", bound=int)
DILATION = TypeVar("DILATION", bound=int)


Z = Add[Mult[L[2], PADDING], Mult[L[-1], Mult[DILATION, Add[KERNEL_SIZE, L[-1]]]]]


class Conv2d(Generic[C_in, C_out, KERNEL_SIZE, STRIDE, PADDING, DILATION]):
    in_channels: C_in
    out_channels: C_out
    padding: PADDING
    dilation: DILATION
    kernel_size: KERNEL_SIZE
    stride: STRIDE

    def __init__(
        self,
        in_channels: C_in,
        out_channels: C_out,
        kernel_size: KERNEL_SIZE,
        stride: STRIDE,
        padding: PADDING,
        dilation: DILATION,
    ):
        ...

    # (H_in + 2 * PADDING - DILATION * (KERNEL_SIZE - 1)) - 1 / STRIDE + 1,
    def __call__(
        self, t: Tensor[N, C_in, H_in, W_in]
    ) -> Tensor[
        N,
        C_out,
        Add[Div[Add[Add[H_in, Z], L[-1]], STRIDE], L[1]],
        Add[Div[Add[Add[W_in, Z], L[-1]], STRIDE], L[1]],
    ]:
        ...
