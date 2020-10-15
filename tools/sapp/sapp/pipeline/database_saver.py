# Copyright (c) Facebook, Inc. and its affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

#!/usr/bin/env python3

import logging
from typing import Optional, Tuple

from ..bulk_saver import BulkSaver
from ..db import DB
from ..decorators import log_time
from ..models import (
    Issue,
    MetaRunToRunAssoc,
    PrimaryKeyGenerator,
    Run,
    RunStatus,
    RunSummary,
    TraceFrame,
    TraceFrameAnnotation,
    TraceFrameLeafAssoc,
    TraceKind,
)
from ..trace_graph import TraceGraph
from . import PipelineStep, Summary


# pyre-fixme[5]: Global expression must be annotated.
log = logging.getLogger("sapp")


# pyre-fixme[13]: Attribute `summary` is never initialized.
class DatabaseSaver(PipelineStep[TraceGraph, RunSummary]):
    RUN_MODEL = Run

    # pyre-fixme[3]: Return type must be annotated.
    def __init__(
        self,
        database: DB,
        use_lock: bool = False,
        primary_key_generator: Optional[PrimaryKeyGenerator] = None,
    ):
        self.use_lock = use_lock
        # pyre-fixme[4]: Attribute must be annotated.
        self.dbname = database.dbname
        self.database = database
        # pyre-fixme[4]: Attribute must be annotated.
        self.primary_key_generator = primary_key_generator or PrimaryKeyGenerator()
        self.bulk_saver = BulkSaver(self.primary_key_generator)
        self.summary: Summary

    @log_time
    def run(self, input: TraceGraph, summary: Summary) -> Tuple[RunSummary, Summary]:
        # pyre-fixme[16]: `DatabaseSaver` has no attribute `graph`.
        self.graph = input
        self.summary = summary

        self._prep_save()
        return self._save(), self.summary

    # pyre-fixme[3]: Return type must be annotated.
    def _prep_save(self):
        """Prepares the bulk saver to load the trace graph info into the
        database.
        """
        log.info("Preparing bulk save.")
        # pyre-fixme[16]: `DatabaseSaver` has no attribute `graph`.
        self.graph.update_bulk_saver(self.bulk_saver)

        for trace_kind, unused in self.summary["trace_entries"].items():
            log.info(
                "Dropped %d unused %s, %d are missing",
                sum(len(v) for v in unused.values()),
                trace_kind,
                len(self.summary["missing_traces"][trace_kind]),
            )

    def _save(self) -> RunSummary:
        """Saves bulk saver's info into the databases in bulk."""
        assert self.summary["run"] is not None, "Must have called process before"

        trace_frames = self.bulk_saver.get_items_to_add(TraceFrame)
        log.info(
            "Saving %d issues, %d trace frames, %d trace annotations, %d trace frame leaf assocs",
            len(self.bulk_saver.get_items_to_add(Issue)),
            len(self.bulk_saver.get_items_to_add(TraceFrame)),
            len(self.bulk_saver.get_items_to_add(TraceFrameAnnotation)),
            len(self.bulk_saver.get_items_to_add(TraceFrameLeafAssoc)),
        )

        num_pre = 0
        num_post = 0
        for frame in trace_frames:
            if frame.kind == TraceKind.PRECONDITION:
                num_pre += 1
            elif frame.kind == TraceKind.POSTCONDITION:
                num_post += 1
        log.info(
            "Within trace frames: %d preconditions, %d postconditions",
            num_pre,
            num_post,
        )

        with self.database.make_session() as session:
            pk_gen = self.primary_key_generator.reserve(
                session, [Run], use_lock=self.use_lock
            )
            self.summary["run"].id.resolve(id=pk_gen.get(Run), is_new=True)
            session.add(self.summary["run"])
            meta_run_identifier = self.summary.get("meta_run_identifier")
            if meta_run_identifier is not None:
                session.add(
                    MetaRunToRunAssoc(
                        meta_run_id=meta_run_identifier, run_id=self.summary["run"].id
                    )
                )
            session.commit()

            run_id = self.summary["run"].id.resolved()
            self.summary["run"] = None  # Invalidate it

        self.bulk_saver.save_all(self.database, self.use_lock)

        # Now that the run is finished, fetch it from the DB again and set its
        # status to FINISHED.
        with self.database.make_session() as session:
            run = session.query(self.RUN_MODEL).filter_by(id=run_id).one()
            run.status = RunStatus.FINISHED
            session.add(run)
            session.commit()
            run_summary = run.get_summary()

        # pyre-fixme[16]: `RunSummary` has no attribute `num_invisible_issues`.
        run_summary.num_invisible_issues = 0
        run_summary.num_missing_preconditions = len(
            self.summary["missing_traces"][TraceKind.precondition]
        )
        run_summary.num_missing_postconditions = len(
            self.summary["missing_traces"][TraceKind.postcondition]
        )

        return run_summary
