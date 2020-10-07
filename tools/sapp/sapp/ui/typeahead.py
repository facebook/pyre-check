# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from __future__ import annotations

from typing import List, NamedTuple

import graphene
from sqlalchemy.orm import Session

from ..models import Issue


class CodeType(graphene.ObjectType):
    code = graphene.Int()


class Code(NamedTuple):
    code: int


def all_codes(session: Session) -> List[Code]:
    return session.query(Issue.code.distinct().label("code")).all()
