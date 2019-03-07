#!/usr/bin/env python3

from unittest import TestCase
from unittest.mock import patch

from click.testing import CliRunner

from ..cli import cli  # noqa


PIPELINE_RUN = "sapp.pipeline.Pipeline.run"


@patch(
    "sapp.analysis_output.AnalysisOutput.from_file", return_value="fake_analysis_output"
)
class TestSappCli(TestCase):
    def setUp(self) -> None:
        self.runner = CliRunner()

    def test_explore_options(self, mock_analysis_output):
        result = self.runner.invoke(
            cli,
            [
                "explore",
                "--database",
                "memory",
                "--database-name",
                "mydatabase",
                "--repository-directory",
                "/tmp",
            ],
        )
        self.assertEqual(result.exit_code, 0)

    def verify_input_file(self, input_files, summary_blob):
        inputfile, previous_input = input_files
        self.assertEqual(inputfile, "fake_analysis_output")

    def test_input_file(self, mock_analysis_output):
        with patch(PIPELINE_RUN, self.verify_input_file):
            result = self.runner.invoke(cli, ["analyze", "/tmp"])
            self.assertEqual(result.exit_code, 0)

    def verify_base_summary_blob(self, input_files, summary_blob):
        self.assertEqual(summary_blob["run_kind"], "master")
        self.assertEqual(summary_blob["repository"], "pyre-check")
        self.assertEqual(summary_blob["branch"], "master")
        self.assertEqual(summary_blob["commit_hash"], "abc123")
        self.assertEqual(summary_blob["old_linemap_file"], "/tmp")
        self.assertTrue(callable(summary_blob["compress"]))

    def test_base_summary_blob(self, mock_analysis_output):
        with patch(PIPELINE_RUN, self.verify_base_summary_blob):
            result = self.runner.invoke(
                cli,
                [
                    "analyze",
                    "--run-kind",
                    "master",
                    "--repository",
                    "pyre-check",
                    "--branch",
                    "master",
                    "--commit-hash",
                    "abc123",
                    "--linemap",
                    "/tmp",
                    "/tmp",
                ],
            )
            self.assertEqual(result.exit_code, 0)

    def verify_option_job_id(self, input_files, summary_blob):
        self.assertEqual(summary_blob["job_id"], "job-id-1")

    def verify_option_job_id_none(self, input_files, summary_blob):
        self.assertIsNone(summary_blob["job_id"])

    def verify_option_differential_id(self, input_files, summary_blob):
        self.assertEqual(summary_blob["job_id"], "user_input_1234567")

    def test_option_job_id(self, mock_analysis_output):
        with patch(PIPELINE_RUN, self.verify_option_job_id):
            result = self.runner.invoke(
                cli, ["analyze", "--job-id", "job-id-1", "/tmp"]
            )
            self.assertEqual(result.exit_code, 0)

        with patch(PIPELINE_RUN, self.verify_option_job_id_none):
            result = self.runner.invoke(cli, ["analyze", "/tmp"])
            self.assertEqual(result.exit_code, 0)

        with patch(PIPELINE_RUN, self.verify_option_differential_id):
            result = self.runner.invoke(
                cli, ["analyze", "--differential-id", "1234567", "/tmp"]
            )
            self.assertEqual(result.exit_code, 0)

    def verify_previous_issue_handles(self, input_files, summary_blob):
        self.assertEqual(summary_blob["previous_issue_handles"], "fake_analysis_output")

    def verify_previous_input(self, input_files, summary_blob):
        inputfile, previous_input = input_files
        self.assertEqual(previous_input, "fake_analysis_output")

    def test_previous_input(self, mock_analysis_output):
        with patch(PIPELINE_RUN, self.verify_previous_issue_handles):
            result = self.runner.invoke(
                cli,
                [
                    "analyze",
                    "--previous-issue-handles",
                    "/tmp",
                    "--previous-input",
                    "/tmp",
                    "/tmp",
                ],
            )
            self.assertEqual(result.exit_code, 0)

        with patch(PIPELINE_RUN, self.verify_previous_input):
            result = self.runner.invoke(
                cli, ["analyze", "--previous-input", "/tmp", "/tmp"]
            )
            self.assertEqual(result.exit_code, 0)
