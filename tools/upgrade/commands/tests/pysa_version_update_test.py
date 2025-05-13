# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-unsafe

import unittest
from unittest.mock import call, MagicMock, mock_open, patch

from ...repository import Repository
from ..pysa_version_update import Configuration, PysaVersionUpdate

repository = Repository()


class UpdatePysaVersionTest(unittest.TestCase):
    @patch("json.dumps")
    @patch("json.loads")
    @patch.object(Configuration, "find_parent_file")
    @patch.object(Configuration, "set_pysa_version")
    @patch.object(Configuration, "write")
    @patch("builtins.open")
    def test_run_pysa_version_update(
        self,
        open_mock,
        configuration_write,
        configuration_set_pysa_version,
        find_parent_file,
        json_loads,
        json_dumps,
    ) -> None:
        arguments = MagicMock()
        arguments.hash = "new"
        arguments.no_commit = False
        mocks = [
            mock_open(read_data='{"pysa_version": "old"}').return_value,
        ]
        open_mock.side_effect = mocks

        PysaVersionUpdate.from_arguments(arguments, repository).run()
        configuration_set_pysa_version.assert_has_calls([call("new")])
        configuration_write.assert_has_calls([call()])
        find_parent_file.assert_any_call(".pysa_configuration")
