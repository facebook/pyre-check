# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import tempfile
from contextlib import contextmanager
from pathlib import Path
from typing import Iterator, List, Optional
from unittest.mock import CallableMixin, patch

import testslide

from ...language_server import (
    connections,
    daemon_connection,
    protocol as lsp,
    remote_index,
)
from ...language_server.connections import (
    AsyncTextReader,
    AsyncTextWriter,
    create_memory_text_reader,
    MemoryBytesWriter,
)
from ...language_server.features import (
    DefinitionAvailability,
    HoverAvailability,
    LanguageServerFeatures,
    ReferencesAvailability,
    TypeCoverageAvailability,
)
from ...tests import setup
from ..daemon_querier import (
    CodeNavigationDaemonQuerier,
    DaemonQuerierSource,
    FailableDaemonQuerier,
    GetDefinitionLocationsResponse,
    GetHoverResponse,
    PersistentDaemonQuerier,
    RemoteIndexBackedQuerier,
)
from ..daemon_query import DaemonQueryFailure
from ..daemon_query_failer import AbstractDaemonQueryFailer

from ..server_state import ConnectionStatus

from ..tests import server_setup


@contextmanager
def patch_connect_async(
    input_channel: AsyncTextReader, output_channel: AsyncTextWriter
) -> Iterator[CallableMixin]:
    with patch.object(connections, "connect_async") as mock:

        class MockedConnection:
            async def __aenter__(self):
                return (
                    input_channel,
                    output_channel,
                )

            async def __aexit__(self, exc_type, exc, tb):
                pass

        mock.return_value = MockedConnection()
        yield mock


class DaemonQuerierTest(testslide.TestCase):
    @setup.async_test
    async def test_get_type_coverage__happy_path(self) -> None:
        with tempfile.NamedTemporaryFile(suffix=".py") as tmpfile:
            tmpfile.write(b"def foo(x):\n  pass\n")
            tmpfile.flush()
            test_path = Path(tmpfile.name)
            querier = PersistentDaemonQuerier(
                server_state=server_setup.create_server_state_with_options(
                    strict_default=False,
                    language_server_features=LanguageServerFeatures(
                        type_coverage=TypeCoverageAvailability.EXPRESSION_LEVEL
                    ),
                ),
            )
            input_channel = create_memory_text_reader(
                '["Query", {"response": ["test"]}]\n ["Query", {"response": [["CoverageAtPath",{"path":"/fake/path.py","total_expressions":1,"coverage_gaps":[]}]]}]\n'
            )
            memory_bytes_writer = MemoryBytesWriter()
            output_channel = AsyncTextWriter(memory_bytes_writer)
            with patch_connect_async(input_channel, output_channel):
                result = await querier.get_type_coverage(path=test_path)
            self.assertEqual(len(memory_bytes_writer.items()), 2)
            self.assertTrue(
                memory_bytes_writer.items()[0].startswith(
                    b'["QueryWithOverlay", {"query_text": "modules_of_path('
                )
            )
            self.assertTrue(result is not None)
            self.assertTrue(not isinstance(result, DaemonQueryFailure))
            self.assertEqual(len(result.uncovered_ranges), 0)
            self.assertTrue(result.covered_percent == 100.0)

    @setup.async_test
    async def test_get_type_coverage__bad_json(self) -> None:
        querier = PersistentDaemonQuerier(
            server_state=server_setup.create_server_state_with_options(
                strict_default=False
            ),
        )
        input_channel = create_memory_text_reader('{ "error": "Oops" }\n')
        output_channel = AsyncTextWriter(MemoryBytesWriter())
        with patch_connect_async(input_channel, output_channel):
            result = await querier.get_type_coverage(
                path=Path("test.py"),
            )
            self.assertTrue(result is None)

    @setup.async_test
    async def test_get_type_coverage__not_typechecked(self) -> None:
        querier = PersistentDaemonQuerier(
            server_state=server_setup.create_server_state_with_options(
                strict_default=False
            ),
        )
        input_channel = create_memory_text_reader('["Query", {"response": []}]\n')
        output_channel = AsyncTextWriter(MemoryBytesWriter())
        with patch_connect_async(input_channel, output_channel):
            result = await querier.get_type_coverage(path=Path("test.py"))
        self.assertTrue(result is not None)
        self.assertTrue(not isinstance(result, DaemonQueryFailure))
        self.assertEqual(result.covered_percent, 0.0)
        self.assertEqual(len(result.uncovered_ranges), 1)
        self.assertEqual(
            result.uncovered_ranges[0].message, "This file is not type checked by Pyre."
        )

    @setup.async_test
    async def test_get_type_coverage__expression_level__gaps(self) -> None:
        with tempfile.NamedTemporaryFile(suffix=".py") as tmpfile:
            tmpfile.write(b"def foo(x):\n  pass\n")
            tmpfile.flush()
            test_path = Path(tmpfile.name)
            querier = PersistentDaemonQuerier(
                server_state=server_setup.create_server_state_with_options(
                    strict_default=False,
                    language_server_features=LanguageServerFeatures(
                        type_coverage=TypeCoverageAvailability.EXPRESSION_LEVEL
                    ),
                ),
            )
            input_channel = create_memory_text_reader(
                '["Query", {"response": ["test"]}]\n ["Query", {"response": [["CoverageAtPath",{"path":"/fake/path.py","total_expressions":4,"coverage_gaps":[{"location": {"start": {"line": 11, "column": 16}, "stop": {"line": 11, "column": 17}}, "function_name":"foo","type_": "typing.Any", "reason": ["TypeIsAny"]}]}]]}]\n'
            )
            memory_bytes_writer = MemoryBytesWriter()
            output_channel = AsyncTextWriter(memory_bytes_writer)
            with patch_connect_async(input_channel, output_channel):
                result = await querier.get_type_coverage(
                    path=test_path,
                )
            self.assertEqual(len(memory_bytes_writer.items()), 2)
            self.assertTrue(
                memory_bytes_writer.items()[0].startswith(
                    b'["QueryWithOverlay", {"query_text": "modules_of_path('
                )
            )
            self.assertTrue(result is not None)
            self.assertTrue(not isinstance(result, DaemonQueryFailure))
            self.assertEqual(len(result.uncovered_ranges), 1)
            self.assertTrue(result.covered_percent == 75.0)

    @setup.async_test
    async def test_get_type_coverage__expression_level__bad_json(self) -> None:
        querier = PersistentDaemonQuerier(
            server_state=server_setup.create_server_state_with_options(
                strict_default=False,
                language_server_features=LanguageServerFeatures(
                    type_coverage=TypeCoverageAvailability.EXPRESSION_LEVEL
                ),
            ),
        )
        input_channel = create_memory_text_reader(
            '{ "error": "Oops" }\n["Query", {"response": [["ErrorAtPath",{"path":"/fake/path.py","error":"oops"}]]}]\n'
        )
        output_channel = AsyncTextWriter(MemoryBytesWriter())
        with patch_connect_async(input_channel, output_channel):
            result = await querier.get_type_coverage(
                path=Path("test.py"),
            )
            self.assertTrue(result is None)

    @setup.async_test
    async def test_get_type_coverage__expression_level__strict(self) -> None:
        with tempfile.NamedTemporaryFile(suffix=".py") as tmpfile:
            tmpfile.write(b"def foo(x):\n  pass\n")
            tmpfile.flush()
            test_path = Path(tmpfile.name)
            querier = PersistentDaemonQuerier(
                server_state=server_setup.create_server_state_with_options(
                    strict_default=True,
                    language_server_features=LanguageServerFeatures(
                        type_coverage=TypeCoverageAvailability.EXPRESSION_LEVEL
                    ),
                ),
            )
            input_channel = create_memory_text_reader(
                '["Query", {"response": ["test"]}]\n["Query", {"response": [["CoverageAtPath",{"path":"/fake/path.py","total_expressions":0,"coverage_gaps":[]}]]}]\n'
            )
            output_channel = AsyncTextWriter(MemoryBytesWriter())
            with patch_connect_async(input_channel, output_channel):
                result = await querier.get_type_coverage(
                    path=test_path,
                )
            self.assertTrue(result is not None)
            self.assertTrue(not isinstance(result, DaemonQueryFailure))
            self.assertEqual(len(result.uncovered_ranges), 0)
            self.assertEqual(result.covered_percent, 100.0)

    @setup.async_test
    async def test_query_hover(self) -> None:
        json_output = """{ "response": {"value": "foo.bar.Bar", "docstring": "test docstring"} }"""
        querier = PersistentDaemonQuerier(
            server_state=server_setup.create_server_state_with_options(
                language_server_features=LanguageServerFeatures(
                    hover=HoverAvailability.ENABLED
                ),
            ),
        )
        memory_bytes_writer = MemoryBytesWriter()
        flat_json = "".join(json_output.splitlines())
        input_channel = create_memory_text_reader(f'["Query", {flat_json}]\n')
        output_channel = AsyncTextWriter(memory_bytes_writer)

        with patch_connect_async(input_channel, output_channel):
            result = await querier.get_hover(
                path=Path("bar.py"), position=lsp.PyrePosition(line=42, character=10)
            )
        self.assertEqual(
            result,
            GetHoverResponse(
                source=DaemonQuerierSource.PYRE_DAEMON,
                data=lsp.LspHoverResponse(
                    contents="```\nfoo.bar.Bar\n```\ntest docstring",
                ),
            ),
        )
        self.assertEqual(
            memory_bytes_writer.items(),
            [
                b'["QueryWithOverlay", {"query_text": "hover_info_for_position(path=\'bar.py\','
                b' line=42, column=10)", "overlay_id": null}]\n'
            ],
        )

    @setup.async_test
    async def test_query_hover__bad_json(self) -> None:
        querier = PersistentDaemonQuerier(
            server_state=server_setup.create_server_state_with_options(
                language_server_features=LanguageServerFeatures(
                    hover=HoverAvailability.ENABLED
                ),
            ),
        )

        input_channel = create_memory_text_reader("""{ "error": "Oops" }\n""")
        memory_bytes_writer = MemoryBytesWriter()
        output_channel = AsyncTextWriter(memory_bytes_writer)
        with patch_connect_async(input_channel, output_channel):
            result = await querier.get_hover(
                path=Path("bar.py"),
                position=lsp.PyrePosition(line=42, character=10),
            )
            self.assertTrue(isinstance(result, DaemonQueryFailure))

    @setup.async_test
    async def test_query_definition_location(self) -> None:
        json_output = """
        {
            "response": [
                {
                    "path": "/foo.py",
                    "range": {
                        "start": {
                            "line": 9,
                            "character": 6
                        },
                        "end": {
                            "line": 10,
                            "character": 11
                        }
                    }
                }
            ]
        }
        """
        querier = PersistentDaemonQuerier(
            server_state=server_setup.create_server_state_with_options(
                language_server_features=LanguageServerFeatures(
                    hover=HoverAvailability.ENABLED
                ),
            ),
        )
        memory_bytes_writer = MemoryBytesWriter()
        flat_json = "".join(json_output.splitlines())
        input_channel = create_memory_text_reader(f'["Query", {flat_json}]\n')
        output_channel = AsyncTextWriter(memory_bytes_writer)

        with patch_connect_async(input_channel, output_channel):
            response = await querier.get_definition_locations(
                path=Path("bar.py"),
                position=lsp.PyrePosition(line=42, character=10),
            )

        self.assertEqual(
            memory_bytes_writer.items(),
            [
                b'["QueryWithOverlay", {"query_text": "location_of_definition(path=\'bar.py\','
                b' line=42, column=10)", "overlay_id": null}]\n'
            ],
        )
        self.assertEqual(
            response,
            GetDefinitionLocationsResponse(
                source=DaemonQuerierSource.PYRE_DAEMON,
                data=[
                    lsp.LspLocation(
                        uri="/foo.py",
                        range=lsp.LspRange(
                            start=lsp.LspPosition(line=8, character=6),
                            end=lsp.LspPosition(line=9, character=11),
                        ),
                    )
                ],
            ),
        )

    @setup.async_test
    async def test_query_definition_location__bad_json(self) -> None:
        querier = PersistentDaemonQuerier(
            server_state=server_setup.create_server_state_with_options(
                language_server_features=LanguageServerFeatures(
                    hover=HoverAvailability.ENABLED
                )
            ),
        )

        input_channel = create_memory_text_reader("""{ "error": "Oops" }\n""")
        memory_bytes_writer = MemoryBytesWriter()
        output_channel = AsyncTextWriter(memory_bytes_writer)
        with patch_connect_async(input_channel, output_channel):
            result = await querier.get_definition_locations(
                path=Path("bar.py"),
                position=lsp.PyrePosition(line=42, character=10),
            )
            self.assertTrue(isinstance(result, DaemonQueryFailure))

    @setup.async_test
    async def test_query_definition_location__error_response(self) -> None:
        querier = PersistentDaemonQuerier(
            server_state=server_setup.create_server_state_with_options(
                language_server_features=LanguageServerFeatures(
                    hover=HoverAvailability.ENABLED
                )
            ),
        )

        input_channel = create_memory_text_reader(
            """["Query",{"error":"Parse error"}]\n"""
        )
        memory_bytes_writer = MemoryBytesWriter()
        output_channel = AsyncTextWriter(memory_bytes_writer)
        with patch_connect_async(input_channel, output_channel):
            result = await querier.get_definition_locations(
                path=Path("bar.py"),
                position=lsp.PyrePosition(line=4, character=10),
            )
            self.assertTrue(isinstance(result, DaemonQueryFailure))
            self.assertEqual(
                "Daemon query returned error: {'error': 'Parse error'} for query: "
                "location_of_definition(path='bar.py', line=4, column=10)",
                result.error_message,
            )

    @setup.async_test
    async def test_query_definition_fall_back_to_glean(self) -> None:
        base_querier = PersistentDaemonQuerier(
            server_state=server_setup.create_server_state_with_options(
                language_server_features=LanguageServerFeatures(
                    definition=DefinitionAvailability.ENABLED
                )
            ),
        )
        base_querier.server_state.status_tracker.set_status(
            ConnectionStatus.DISCONNECTED
        )
        querier = RemoteIndexBackedQuerier(
            base_querier=base_querier,
            index=remote_index.EmptyRemoteIndex(),
        )

        response = await querier.get_definition_locations(
            path=Path("bar.py"),
            position=lsp.PyrePosition(line=42, character=10),
        )
        self.assertEqual(
            response,
            GetDefinitionLocationsResponse(
                source=DaemonQuerierSource.GLEAN_INDEXER,
                data=[],
            ),
        )

    @setup.async_test
    async def test_query_references(self) -> None:
        json_output = """
        {
            "response": [
                {
                    "path": "/foo.py",
                    "range": {
                        "start": {
                            "line": 9,
                            "character": 6
                        },
                        "end": {
                            "line": 10,
                            "character": 11
                        }
                    }
                },
                {
                    "path": "/bar.py",
                    "range": {
                        "start": {
                            "line": 2,
                            "character": 3
                        },
                        "end": {
                            "line": 2,
                            "character": 4
                        }
                    }
                }
            ]
        }
        """
        querier = PersistentDaemonQuerier(
            server_state=server_setup.create_server_state_with_options(
                language_server_features=LanguageServerFeatures(
                    references=ReferencesAvailability.ENABLED,
                ),
            ),
        )
        memory_bytes_writer = MemoryBytesWriter()
        flat_json = "".join(json_output.splitlines())
        input_channel = create_memory_text_reader(f'["Query", {flat_json}]\n')
        output_channel = AsyncTextWriter(memory_bytes_writer)

        with patch_connect_async(input_channel, output_channel):
            result = await querier.get_reference_locations(
                path=Path("bar.py"),
                position=lsp.PyrePosition(line=42, character=10),
            )

        self.assertEqual(
            memory_bytes_writer.items(),
            [
                b'["QueryWithOverlay", {"query_text": "find_references(path=\'bar.py\','
                b' line=42, column=10)", "overlay_id": null}]\n'
            ],
        )
        self.assertEqual(
            result,
            [
                lsp.LspLocation(
                    uri="/foo.py",
                    range=lsp.LspRange(
                        start=lsp.LspPosition(line=8, character=6),
                        end=lsp.LspPosition(line=9, character=11),
                    ),
                ),
                lsp.LspLocation(
                    uri="/bar.py",
                    range=lsp.LspRange(
                        start=lsp.LspPosition(line=1, character=3),
                        end=lsp.LspPosition(line=1, character=4),
                    ),
                ),
            ],
        )

    @setup.async_test
    async def test_query_references__bad_json(self) -> None:
        querier = PersistentDaemonQuerier(
            server_state=server_setup.create_server_state_with_options(
                language_server_features=LanguageServerFeatures(
                    references=ReferencesAvailability.ENABLED,
                ),
            ),
        )
        input_channel = create_memory_text_reader("""{ "error": "Oops" }\n""")
        memory_bytes_writer = MemoryBytesWriter()
        output_channel = AsyncTextWriter(memory_bytes_writer)
        with patch_connect_async(input_channel, output_channel):
            result = await querier.get_reference_locations(
                path=Path("bar.py"),
                position=lsp.PyrePosition(line=42, character=10),
            )
            self.assertTrue(isinstance(result, DaemonQueryFailure))

    @setup.async_test
    async def test_query_completions(self) -> None:
        json_output = """
        {
            "completions": [
                {
                    "label": "completion_1",
                    "kind": "SIMPLE"
                },
                {
                    "label": "completion_2",
                    "kind": "METHOD"
                },
                {
                    "label": "completion_2",
                    "kind": "PROPERTY"
                }
            ]
        }
        """
        querier = CodeNavigationDaemonQuerier(
            server_state=server_setup.create_server_state_with_options(
                language_server_features=LanguageServerFeatures(),
            ),
        )
        memory_bytes_writer = MemoryBytesWriter()
        flat_json = "".join(json_output.splitlines())
        input_channel = create_memory_text_reader(f'["Completion", {flat_json}]\n')
        output_channel = AsyncTextWriter(memory_bytes_writer)

        with patch_connect_async(input_channel, output_channel):
            response = await querier.get_completions(
                path=Path("bar.py"),
                position=lsp.PyrePosition(line=42, character=10),
            )
        items = memory_bytes_writer.items()
        self.assertEqual(len(items), 1)
        self.assertRegex(
            str(items[0]),
            """["Query", ["Completion", {"path": "bar.py", "client_id": "codenav_pid_[0-9]{6}", "position": {"line": 42, "column": 10}}]]""",
        )
        print(response)
        self.assertEqual(
            response,
            [
                lsp.CompletionItem(
                    label="completion_1",
                    kind=lsp.CompletionItemKind.TEXT,
                    sortText="completion_1",
                    filterText="completion_1",
                ),
                lsp.CompletionItem(
                    label="completion_2",
                    kind=lsp.CompletionItemKind.METHOD,
                    sortText="completion_2",
                    filterText="completion_2",
                ),
                lsp.CompletionItem(
                    label="completion_2",
                    kind=lsp.CompletionItemKind.PROPERTY,
                    sortText="completion_2",
                    filterText="completion_2",
                ),
            ],
        )


class MockDaemonQueryFailer(AbstractDaemonQueryFailer):
    query_failures: List[str]
    query_connection_failures: List[str]

    def __init__(self) -> None:
        self.query_failures = []
        self.query_connection_failures = []

    def query_failure(self, path: str) -> Optional[DaemonQueryFailure]:
        self.query_failures.append(path)
        return None

    def query_connection_failure(
        self, path: str
    ) -> Optional[daemon_connection.DaemonConnectionFailure]:
        self.query_connection_failures.append(path)
        return None


class FailableDaemonQuerierTest(testslide.TestCase):
    @setup.async_test
    async def test_AbstractDaemonQueryFailer_invoked(self) -> None:
        mock_daemon_query_failer = MockDaemonQueryFailer()

        codenav_querier = FailableDaemonQuerier(
            base_querier=CodeNavigationDaemonQuerier(
                server_state=server_setup.create_server_state_with_options(
                    language_server_features=LanguageServerFeatures(),
                ),
            ),
            daemon_query_failer=mock_daemon_query_failer,
        )

        await codenav_querier.get_hover(
            path=Path("bar1.py"), position=lsp.PyrePosition(line=42, character=10)
        )
        try:
            await codenav_querier.get_type_errors(path=Path("bar2.py"))
            self.fail("Expected NotImplementedError to be raised.")
        except NotImplementedError:
            pass

        await codenav_querier.get_definition_locations(
            path=Path("bar3.py"), position=lsp.PyrePosition(line=42, character=10)
        )
        await codenav_querier.get_completions(
            path=Path("bar4.py"), position=lsp.PyrePosition(line=42, character=10)
        )
        await codenav_querier.get_reference_locations(
            path=Path("bar5.py"), position=lsp.PyrePosition(line=42, character=10)
        )
        await codenav_querier.get_init_call_hierarchy(
            path=Path("bar6.py"),
            position=lsp.PyrePosition(line=42, character=10),
            relation_direction=lsp.PyreCallHierarchyRelationDirection.CHILD,
        )

        call_hierarchy_item = lsp.CallHierarchyItem(
            name="protocol",
            kind=lsp.SymbolKind.MODULE,
            uri="file:///.",
            range=lsp.LspRange(
                start=lsp.LspPosition(line=37, character=4),
                end=lsp.LspPosition(line=39, character=2),
            ),
            selection_range=lsp.LspRange(
                start=lsp.LspPosition(line=37, character=4),
                end=lsp.LspPosition(line=39, character=2),
            ),
        )

        await codenav_querier.get_call_hierarchy_from_item(
            path=Path("bar7.py"),
            call_hierarchy_item=call_hierarchy_item,
            relation_direction=lsp.PyreCallHierarchyRelationDirection.CHILD,
        )

        await codenav_querier.handle_file_opened(
            path=Path("bar8.py"),
            code="",
        )

        await codenav_querier.handle_file_closed(
            path=Path("bar9.py"),
        )

        await codenav_querier.update_overlay(
            path=Path("bar10.py"),
            code="",
        )

        self.assertEqual(
            mock_daemon_query_failer.query_failures,
            [
                "bar1.py",
                "bar2.py",
                "bar3.py",
                "bar4.py",
                "bar5.py",
                "bar6.py",
                "bar7.py",
            ],
        )
        self.assertEqual(
            mock_daemon_query_failer.query_connection_failures,
            [
                "bar8.py",
                "bar9.py",
                "bar10.py",
            ],
        )
