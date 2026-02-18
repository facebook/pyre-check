from typing import Callable, Generic, Tuple, TypeVar

from _torch import Tensor
from pyre_extensions import Add, Divide as Div, ListVariadic, Multiply as Mult
from pyre_extensions.type_variable_operators import Concatenate as Cat
from typing_extensions import Literal as L

A = TypeVar("A", bound=int)
B = TypeVar("B", bound=int)

N = TypeVar("N", bound=int)

C = TypeVar("C", bound=int)
C_in = TypeVar("C_in", bound=int)
C_out = TypeVar("C_out", bound=int)

H = TypeVar("H", bound=int)
H_in = TypeVar("H_in", bound=int)
H_out = TypeVar("H_out", bound=int)

W = TypeVar("W", bound=int)
W_in = TypeVar("W_in", bound=int)
W_out = TypeVar("W_out", bound=int)

Shape = ListVariadic("Shape")
Ts0 = ListVariadic("Ts0")
Ts1 = ListVariadic("Ts1")
Ts2 = ListVariadic("Ts2")

class SequentialList(Generic[Shape]):
    def __init__(self, sequence: List[Callable[[Tensor[Shape]], Tensor[Shape]]]): ...
    def __call__(self, x: Tensor[Shape]) -> Tensor[Shape]: ...

class Sequential(Generic[Ts0, Ts1, Ts2]):
    def __init__(
        self,
        a: Callable[[Tensor[Ts0]], Tensor[Ts1]],
        b: Callable[[Tensor[Ts1]], Tensor[Ts2]],
    ): ...
    def __call__(self, x: Tensor[Ts0]) -> Tensor[Ts2]: ...

class Linear(Generic[H_in, H_out]):
    def __init__(self, in_features: H_in, out_features: H_out): ...
    def __call__(
        self, a: Tensor[Cat[N, Shape, H_in]]
    ) -> Tensor[Cat[N, Shape, H_out]]: ...

class BatchNorm2d(Generic[C]):
    def __init__(self, num_features: C): ...
    def __call__(self, a: Tensor[N, C, H, W]) -> Tensor[N, C, H, W]: ...

class GroupNorm(Generic[C]):
    def __init__(self, num_groups: int, num_channels: C): ...
    def __call__(self, a: Tensor[Cat[N, C, Shape]]) -> Tensor[Cat[N, C, Shape]]: ...

class ReLU:
    def __init__(self): ...
    def __call__(self, x: Tensor[Shape]) -> Tensor[Shape]: ...

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
        stride: STRIDE = 1,
        padding: PADDING = 0,
        dilation: DILATION = 1,
        groups: int = 1,
        bias: bool = True,
        padding_mode: str = "zeros",
    ): ...
    # (H_in + 2 * PADDING - DILATION * (KERNEL_SIZE - 1)) - 1 / STRIDE + 1,
    def __call__(
        self, t: Tensor[N, C_in, H_in, W_in]
    ) -> Tensor[
        N,
        C_out,
        Add[Div[Add[Add[H_in, Z], L[-1]], STRIDE], L[1]],
        Add[Div[Add[Add[W_in, Z], L[-1]], STRIDE], L[1]],
    ]: ...

class MaxPool2d(Generic[KERNEL_SIZE, STRIDE, PADDING, DILATION]):
    padding: PADDING
    dilation: DILATION
    kernel_size: KERNEL_SIZE
    stride: STRIDE
    def __init__(
        self,
        kernel_size: KERNEL_SIZE,
        stride: STRIDE,
        padding: PADDING = 0,
        dilation: DILATION = 1,
    ): ...
    # (H_in + 2 * PADDING - DILATION * (KERNEL_SIZE - 1)) - 1 / STRIDE + 1,
    def __call__(
        self, t: Tensor[N, C, H_in, W_in]
    ) -> Tensor[
        N,
        C,
        Add[Div[Add[Add[H_in, Z], L[-1]], STRIDE], L[1]],
        Add[Div[Add[Add[W_in, Z], L[-1]], STRIDE], L[1]],
    ]: ...

class AdaptiveAvgPool2d(Generic[H_out, W_out]):
    def __init__(self, output_size: Tuple[H_out, W_out]): ...
    def __call__(self, a: Tensor[N, C, H_in, W_in]) -> Tensor[N, C, H_out, W_out]: ...
