#!/usr/bin/env python3
# pyre-strict

import torch
from pyre_extensions import Generic, ListVariadic


Shape = ListVariadic("Shape")


class Tensor(Generic[Shape], torch.Tensor):
    pass
