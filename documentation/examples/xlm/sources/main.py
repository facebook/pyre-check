# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import itertools
import math
from typing import Any, Dict, Generic, Optional, Tuple, TypeVar, Union

import _torch as torch
import _torch.nn as nn
import _torch.nn.functional as F
from _torch import Tensor
from pyre_extensions import Multiply


N_HEADS = TypeVar("N_HEADS", bound=int)
DIM = TypeVar("DIM", bound=int)
BS = TypeVar("BS", bound=int)
QLEN = TypeVar("QLEN", bound=int)
KLEN = TypeVar("KLEN", bound=int)
DIM_PER_HEAD = TypeVar("DIM_PER_HEAD", bound=int)

A = TypeVar("A", bound=int)
B = TypeVar("B", bound=int)


def mult(a: A, b: B) -> Multiply[A, B]:
    ...


class MultiHeadAttention(Generic[N_HEADS, DIM]):

    NEW_ID = itertools.count()
    layer_id: int
    dim: DIM
    n_heads: N_HEADS
    q_lin: nn.Linear[DIM, DIM]
    k_lin: nn.Linear[DIM, DIM]
    v_lin: nn.Linear[DIM, DIM]
    out_lin: nn.Linear[DIM, DIM]

    def __init__(self, n_heads: N_HEADS, dim: DIM, dropout: float):
        super().__init__()
        self.layer_id = next(MultiHeadAttention.NEW_ID)
        self.dim = dim
        self.n_heads = n_heads
        self.dropout = dropout
        # assert self.dim % self.n_heads == 0

        self.q_lin = nn.Linear(dim, dim)
        self.k_lin = nn.Linear(dim, dim)
        self.v_lin = nn.Linear(dim, dim)
        self.out_lin = nn.Linear(dim, dim)

    def forward(
        self,
        input: Tensor[BS, QLEN, DIM],
        mask: Union[Tensor[BS, KLEN], Tensor[BS, KLEN, KLEN]],
        kv: Optional[Tensor[BS, KLEN, DIM]],
        cache: Optional[
            Dict[
                int,
                Tuple[
                    Tensor[BS, N_HEADS, Any, DIM_PER_HEAD],
                    Tensor[BS, N_HEADS, Any, DIM_PER_HEAD],
                ],
            ]
        ],
        cache_slen: int,
        dim_per_head: DIM_PER_HEAD,
    ) -> Tensor[BS, QLEN, DIM]:
        """
        Self-attention (if kv is None) or attention
        over source sentence (provided by kv).
        """
        # Input is (bs, qlen, dim)
        # Mask is (bs, klen) (non-causal) or (bs, klen, klen)
        bs, qlen, dim = input.size()

        if kv is None:
            klen = qlen if cache is None else cache_slen + qlen
        else:
            klen = kv.size(1)

        # dim_per_head = dim // self.n_heads #> dim_per_head cannot be a literal
        mask_reshape = (bs, 1, qlen, klen) if mask.dim() == 3 else (bs, 1, 1, klen)

        def shape(x: Tensor[BS, Any, DIM]) -> Tensor[BS, N_HEADS, Any, DIM_PER_HEAD]:
            """projection"""
            # variables defined outside of the body of the function are not typed
            bs: BS
            dim_per_head: DIM_PER_HEAD
            return x.view(bs, -1, self.n_heads, dim_per_head).transpose(1, 2)

        def unshape(
            x: Tensor[BS, N_HEADS, QLEN, DIM_PER_HEAD]
        ) -> Tensor[BS, QLEN, Any]:
            """compute context"""
            return (
                x.transpose(1, 2)
                .contiguous()
                .view(bs, -1, mult(self.n_heads, dim_per_head))
            )

        q = shape(self.q_lin(input))  # (bs, n_heads, qlen, dim_per_head)
        if kv is None:
            k = shape(self.k_lin(input))  # (bs, n_heads, qlen, dim_per_head)
            v = shape(self.v_lin(input))  # (bs, n_heads, qlen, dim_per_head)
        elif cache is None or self.layer_id not in cache:
            k = v = kv
            k = shape(self.k_lin(k))  # (bs, n_heads, qlen, dim_per_head)
            v = shape(self.v_lin(v))  # (bs, n_heads, qlen, dim_per_head)

        if cache is not None:
            if self.layer_id in cache:
                if kv is None:
                    k_, v_ = cache[self.layer_id]
                    k = torch.cat(k_, k, dim=2)  # (bs, n_heads, klen, dim_per_head)
                    v = torch.cat(v_, v, dim=2)  # (bs, n_heads, klen, dim_per_head)
                else:
                    k, v = cache[self.layer_id]
            cache[self.layer_id] = (k, v)

        q = q / math.sqrt(dim_per_head)  # (bs, n_heads, qlen, dim_per_head)
        scores: Tensor[BS, N_HEADS, QLEN, KLEN] = torch.matmul(
            q, k.transpose(2, 3)
        )  # (bs, n_heads, qlen, klen)
        mask2 = (
            (mask == 0).view(mask_reshape).expand_as(scores)
        )  # (bs, n_heads, qlen, klen)
        scores2 = scores.masked_fill(mask2, -float("inf"))  # (bs, n_heads, qlen, klen)
        weights = F.softmax(scores2.float(), dim=-1).type_as(
            scores
        )  # (bs, n_heads, qlen, klen)
        weights = F.dropout(
            weights, p=self.dropout, training=0.5
        )  # self.training)  # (bs, n_heads, qlen, klen)
        context: Tensor[BS, N_HEADS, QLEN, DIM_PER_HEAD] = torch.matmul(
            weights, v
        )  # (bs, n_heads, qlen, dim_per_head)
        context2: Tensor[BS, QLEN, DIM] = unshape(context)  # (bs, qlen, dim)

        return self.out_lin(context2)


a = MultiHeadAttention(20, 4, 0.5)
