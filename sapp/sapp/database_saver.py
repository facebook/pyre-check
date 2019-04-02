#!/usr/bin/env python3

import logging
from typing import Optional, Tuple

from .bulk_saver import BulkSaver
from .db import DB
from .decorators import log_time
from .models import (
    Issue,
    IssueInstanceSharedTextAssoc,
    PrimaryKeyGenerator,
    Run,
    RunStatus,
    RunSummary,
    TraceFrame,
    TraceFrameAnnotation,
    TraceKind,
)
from .pipeline import PipelineStep, Summary
from .trace_graph import TraceGraph


log = logging.getLogger("sapp")


class DatabaseSaver(PipelineStep[TraceGraph, RunSummary]):
    RUN_MODEL = Run

    def __init__(
        self,
        database: DB,
        use_lock: bool = False,
        primary_key_generator: Optional[PrimaryKeyGenerator] = None,
    ):
        self.use_lock = use_lock
        self.dbname = database.dbname
        self.database = database
        self.primary_key_generator = primary_key_generator or PrimaryKeyGenerator()
        self.bulk_saver = BulkSaver(self.primary_key_generator)
        self.summary: Summary

    @log_time
    def run(self, input: TraceGraph, summary: Summary) -> Tuple[RunSummary, Summary]:
        self.graph = input
        self.summary = summary

        self._prep_save()
        return self._save(), self.summary

    def _prep_save(self):
        """ Prepares the bulk saver to load the trace graph info into the
        database.
        """
        log.info("Preparing bulk save.")
        self.graph.update_bulk_saver(self.bulk_saver)

        log.info(
            "Dropped %d unused preconditions, %d are missing",
            sum(len(v) for v in self.summary["precondition_entries"].values()),
            len(self.summary["missing_preconditions"]),
        )

        log.info(
            "Dropped %d unused postconditions, %d are missing",
            sum(len(v) for v in self.summary["postcondition_entries"].values()),
            len(self.summary["missing_postconditions"]),
        )
        del self.summary["postcondition_entries"]

    def _save(self) -> RunSummary:
        """ Saves bulk saver's info into the databases in bulk.
        """
        assert self.summary["run"] is not None, "Must have called process before"

        trace_frames = self.bulk_saver.get_items_to_add(TraceFrame)
        log.info(
            "Saving %d issues, %d trace frames, %d trace annotations",
            len(self.bulk_saver.get_items_to_add(Issue)),
            len(self.bulk_saver.get_items_to_add(TraceFrame)),
            len(self.bulk_saver.get_items_to_add(TraceFrameAnnotation)),
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

        run_summary.num_invisible_issues = 0
        run_summary.num_missing_preconditions = len(
            self.summary["missing_preconditions"]
        )
        run_summary.num_missing_postconditions = len(
            self.summary["missing_postconditions"]
        )

        return run_summary
