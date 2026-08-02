# Copyright (c) 2019-present, Facebook, Inc.
# All rights reserved.
#
# This source code is licensed under the license found in the
# LICENSE file in the root directory of this source tree.
#

from typing import Any, Generic, TypeVar, Type
# import _torch as torch
import _torch.nn as nn
from _torch import Tensor
from pyre_extensions import Add, Divide as Div, ListVariadic, Multiply as Mult
from typing_extensions import Literal as L


STRIDE = TypeVar("STRIDE", bound=int)
DILATION = TypeVar("DILATION", bound=int)
GROUPS = TypeVar("GROUPS", bound=int)
BASE_WIDTH = TypeVar("BASE_WIDTH", bound=int)
PLANES_IN = TypeVar("PLANES_IN", bound=int)
PLANES_OUT = TypeVar("PLANES_OUT", bound=int)

N = TypeVar("N", bound=int)
C = TypeVar("C", bound=int)
H = TypeVar("H", bound=int)
W = TypeVar("W", bound=int)

Ts = ListVariadic("Ts")


def add(x: N, y: C) -> Add[N, C]:
    ...


def mult(x: N, y: C) -> Mult[N, C]:
    ...


def div(x: N, y: C) -> Div[N, C]:
    ...


def conv3x3(
    in_planes: PLANES_IN,
    out_planes: PLANES_OUT,
    stride: STRIDE = 1,
    groups: GROUPS = 1,
    dilation: DILATION = 1,
) -> nn.Conv2d[PLANES_IN, PLANES_OUT, L[3], STRIDE, DILATION, DILATION]:
    """3x3 convolution with padding"""
    return nn.Conv2d(
        in_planes,
        out_planes,
        kernel_size=3,
        stride=stride,
        padding=dilation,
        dilation=dilation,
    )


def conv1x1(
    in_planes: PLANES_IN, out_planes: PLANES_OUT, stride: STRIDE = 1
) -> nn.Conv2d[PLANES_IN, PLANES_OUT, L[1], STRIDE, L[0], L[1]]:
    """1x1 convolution"""
    return nn.Conv2d(
        in_planes, out_planes, kernel_size=1, stride=stride, padding=0, dilation=1
    )


class BasicBlock(Generic[PLANES_IN, PLANES_OUT, STRIDE, DILATION]):
    expansion = 1
    conv1: nn.Conv2d[PLANES_IN, PLANES_OUT, L[3], STRIDE, DILATION, DILATION]
    bn1: nn.BatchNorm2d[PLANES_OUT]
    relu: nn.ReLU
    conv2: nn.Conv2d[PLANES_OUT, PLANES_OUT, L[3], L[1], DILATION, DILATION]
    bn2: nn.BatchNorm2d[PLANES_OUT]

    def __init__(
        self,
        inplanes: PLANES_IN,
        planes: PLANES_OUT,
        stride: STRIDE = 1,
        groups=1,
        base_width=64,
        dilation: DILATION = 1
        # norm_layer = nn.BatchNorm2d,
    ):
        super(BasicBlock, self).__init__()
        if groups != 1 or base_width != 64:
            raise ValueError("BasicBlock only supports groups=1 and base_width=64")
        if dilation > 1:
            raise NotImplementedError("Dilation > 1 not supported in BasicBlock")
        # Both self.conv1 and self.downsample layers downsample the input when stride != 1
        norm_layer = nn.BatchNorm2d
        self.conv1 = conv3x3(inplanes, planes, stride)
        self.bn1 = norm_layer(planes)
        self.relu = nn.ReLU()
        self.conv2 = conv3x3(planes, planes)
        self.bn2 = norm_layer(planes)
        self.stride = stride
        self.inplanes = inplanes
        self.planes = planes

    def forward(
        self, x: Tensor[N, PLANES_IN, H, W]
    ) -> Tensor[
        N,
        PLANES_OUT,
        Add[L[1], Div[Add[H, L[-1]], STRIDE]],  # (H-1)//STRIDE + 1
        Add[L[1], Div[Add[W, L[-1]], STRIDE]],  # (W-1)//STRIDE + 1
    ]:
        identity = x
        out = self.conv1(x)
        out = self.bn1(out)
        out = self.relu(out)
        out = self.conv2(out)
        out = self.bn2(out)

        identity = conv1x1(self.inplanes, self.planes, self.stride)(x)
        identity = nn.BatchNorm2d(self.planes)(identity)

        out = out.__add__(identity)
        out = self.relu(out)
        return out


WIDTH = Mult[Div[Mult[PLANES_OUT, BASE_WIDTH], L[64]], GROUPS]
EXPANSION = L[4]


class Bottleneck(Generic[PLANES_IN, PLANES_OUT, STRIDE, DILATION, GROUPS, BASE_WIDTH]):
    expansion: EXPANSION = 4
    width: WIDTH
    conv1: nn.Conv2d[PLANES_IN, WIDTH, L[1], L[1], L[0], L[1]]
    bn1: nn.BatchNorm2d[WIDTH]
    conv2: nn.Conv2d[WIDTH, WIDTH, L[3], STRIDE, DILATION, DILATION]
    bn2: nn.BatchNorm2d[WIDTH]
    conv3: nn.Conv2d[WIDTH, Mult[PLANES_OUT, EXPANSION], L[1], L[1], L[0], L[1]]
    bn3: nn.BatchNorm2d[Mult[PLANES_OUT, EXPANSION]]

    def __init__(
        self,
        inplanes: PLANES_IN,
        planes: PLANES_OUT,
        stride: STRIDE = 1,
        groups: GROUPS = 1,
        base_width: BASE_WIDTH = 64,
        dilation: DILATION = 1,
        # norm_layer=None,
    ):
        super(Bottleneck, self).__init__()
        # int(planes * (base_width / 64.))
        self.width = mult(div(mult(planes, base_width), 64), groups)
        # Both self.conv2 and self.downsample layers downsample the input when stride != 1
        norm_layer = nn.BatchNorm2d
        self.conv1 = conv1x1(inplanes, self.width)
        self.bn1 = norm_layer(self.width)
        self.conv2 = conv3x3(self.width, self.width, stride, groups, dilation)
        self.bn2 = norm_layer(self.width)
        self.conv3 = conv1x1(self.width, mult(planes, self.expansion))
        self.bn3 = norm_layer(mult(planes, self.expansion))
        self.relu = nn.ReLU()
        self.stride = stride
        self.inplanes = inplanes
        self.planes = planes

    def forward(
        self, x: Tensor[N, PLANES_IN, H, W]
    ) -> Tensor[
        N,
        Mult[EXPANSION, PLANES_OUT],
        Add[L[1], Div[Add[H, L[-1]], STRIDE]],
        Add[L[1], Div[Add[W, L[-1]], STRIDE]],
    ]:
        identity = x

        out = self.conv1(x)
        out = self.bn1(out)
        out = self.relu(out)

        out = self.conv2(out)
        out = self.bn2(out)
        out = self.relu(out)

        out = self.conv3(out)
        out = self.bn3(out)

        identity = conv1x1(
            self.inplanes, mult(self.planes, self.expansion), self.stride
        )(x)
        identity = nn.BatchNorm2d(mult(self.planes, self.expansion))(identity)
        # if self.downsample is not None:
        #    identity = self.downsample(x)

        out = out.__add__(identity)
        out = self.relu(out)
        return out


N_CLASSES = TypeVar("N_CLASSES", bound=int)
BLOCK_EXPANSION = TypeVar("BLOCK_EXPANSION", bound=int)

INPLANES = L[64]


class ResNet(Generic[BLOCK_EXPANSION, N_CLASSES, BASE_WIDTH]):
    inplanes: int  # INPLANES
    dilation: int  # L[1]
    _norm_layer: Type[nn.BatchNorm2d]
    base_width: BASE_WIDTH
    relu: nn.ReLU

    conv1: nn.Conv2d[L[3], int, L[7], L[2], L[3], L[1]]
    bn1: nn.BatchNorm2d[int]
    maxpool: nn.MaxPool2d[L[3], L[2], L[1], L[1]]
    avgpool: nn.AdaptiveAvgPool2d[L[1], L[1]]
    fc: nn.Linear[Mult[L[512], BLOCK_EXPANSION], N_CLASSES]

    block_expansion: BLOCK_EXPANSION

    def __init__(
        self,
        block,
        block_expansion: BLOCK_EXPANSION,
        layers,
        num_classes: N_CLASSES = 1000,
        zero_init_residual: bool = False,
        groups: GROUPS = 1,
        width_per_group: BASE_WIDTH = 64,
        replace_stride_with_dilation=None,
        # norm_layer=None,
    ):
        super(ResNet, self).__init__()
        self._norm_layer = nn.BatchNorm2d

        self.inplanes = 64
        self.dilation = 1
        if replace_stride_with_dilation is None:
            # each element in the tuple indicates if we should replace
            # the 2x2 stride with a dilated convolution instead
            replace_stride_with_dilation = [False, False, False]
        if len(replace_stride_with_dilation) != 3:
            raise ValueError(
                "replace_stride_with_dilation should be None "
                "or a 3-element tuple, got {}".format(replace_stride_with_dilation)
            )
        self.groups = groups
        self.base_width = width_per_group
        self.block_expansion = block_expansion
        self.conv1 = nn.Conv2d(
            3, self.inplanes, kernel_size=7, stride=2, padding=3, dilation=1
        )
        self.bn1 = self._norm_layer(self.inplanes)
        self.relu = nn.ReLU()
        self.maxpool = nn.MaxPool2d(kernel_size=3, stride=2, padding=1)
        self.layer1 = self._make_layer(block, 64, layers[0])
        self.layer2 = self._make_layer(block, 128, layers[1], stride=2)
        self.layer3 = self._make_layer(block, 256, layers[2], stride=2)
        self.layer4 = self._make_layer(block, 512, layers[3], stride=2)
        self.avgpool = nn.AdaptiveAvgPool2d((1, 1))
        self.fc = nn.Linear(mult(512, block_expansion), num_classes)

        # for m in self.modules():
        #    if isinstance(m, nn.Conv2d):
        #        nn.init.kaiming_normal_(m.weight, mode="fan_out", nonlinearity="relu")
        #    elif isinstance(m, (nn.BatchNorm2d, nn.GroupNorm)):
        #        nn.init.constant_(m.weight, 1)
        #        nn.init.constant_(m.bias, 0)

        # Zero-initialize the last BN in each residual branch,
        # so that the residual branch starts with zeros, and each residual block behaves like an identity.
        # This improves the  : int model by 0.2~0.3% according to https://arxiv.org/abs/1706.02677
        # if zero_init_residual :
        #    for m in self.modules():
        #        if isinstance(m, Bottleneck):
        #            nn.init.constant_(m.bn3.weight, 0)
        #        elif isinstance(m, BasicBlock):
        #            nn.init.constant_(m.bn2.weight, 0)

    def _make_layer(
        self, block, planes: PLANES_OUT, blocks, stride: int = 1, dilate: bool = False
    ):
        norm_layer = self._norm_layer
        previous_dilation = self.dilation
        if dilate:
            self.dilation = mult(self.dilation, stride)
            stride = 1

        layers = [
            block(
                self.inplanes,
                planes,
                stride,
                self.groups,
                self.base_width,
                previous_dilation,
                norm_layer,
            )
        ]

        self.inplanes = mult(planes, self.block_expansion)
        for _ in range(1, blocks):
            layers.append(
                block(
                    self.inplanes,
                    planes,
                    groups=self.groups,
                    base_width=self.base_width,
                    dilation=self.dilation,
                    norm_layer=norm_layer,
                )
            )
        return nn.Sequential(*layers)

    def _forward_impl(self, x: Tensor[N, L[3], H, W]):
        # See note [TorchScript super()]
        out = self.conv1(x)
        out = self.bn1(out)
        out = self.relu(out)
        out = self.maxpool(out)

        out = self.layer1(out)
        out = self.layer2(out)
        out = self.layer3(out)
        out = self.layer4(out)

        out = self.avgpool(out)
        out = out.flatten(1)
        out = self.fc(out)
        return out

    def forward(self, x: Tensor[N, L[3], H, W]):
        return self._forward_impl(x)
