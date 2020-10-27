# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

from sqlalchemy.orm import Session
from sqlalchemy.sql import func

from ..models import DBID, Run, RunStatus


def latest(session: Session) -> DBID:
    return DBID(
        (
            session.query(func.max(Run.id))
            .filter(Run.status == RunStatus.FINISHED)
            .scalar()
        )
    )
