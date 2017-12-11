# Copyright 2004-present Facebook.  All rights reserved.

from collections import namedtuple
import glob
import subprocess
import unittest

from tools.pyre.scripts import buck
from unittest.mock import (
    call,
    MagicMock,
    patch,
)

BuckSearch = namedtuple('BuckSearch', 'link_trees targets_not_found')


class BuckTest(unittest.TestCase):
    def test_find_link_trees(self):
        with patch.object(glob, 'glob') as glob_glob:
            buck._find_link_trees([
                '//path/targets:name',
                '//path/targets:namelibrary',
                '//path/...'])
            glob_glob.assert_has_calls([
                call('buck-out/gen/path/targets/name*'),
                call('buck-out/gen/path/targets/name#*link-tree'),
                call('buck-out/gen/path/targets/namelibrary#*link-tree'),
                call('buck-out/gen/path/...*'),
                call('buck-out/gen/path/...#*link-tree'),
            ], any_order=True)

    def test_normalize(self):
        with patch.object(subprocess, 'check_output') as buck_targets:
            buck._normalize('target_path')
            buck_targets.assert_called_once_with(
                ['buck', 'targets', 'target_path'],
                stderr=subprocess.DEVNULL)

    def test_build_targets(self):
        with patch.object(subprocess, 'check_output') as buck_build:
            buck._build_targets('target_path', ['subtarget1', 'subtarget2'])
            buck_build.assert_called_once_with(
                ['buck', 'build', 'subtarget1', 'subtarget2'],
                stderr=subprocess.DEVNULL)

    @patch.object(buck, '_get_yes_no_input', return_value=False)
    def test_generate_link_trees(self, mock_input):
        arguments = MagicMock()
        mock_find_link_trees = patch.object(buck, '_find_link_trees')
        mock_find_link_trees.return_value = BuckSearch(
            ['new_tree'],
            ['empty_target'])

        with patch.object(buck, '_normalize') as mock_normalize:
            arguments.build = False
            with self.assertRaises(buck.BuckException):
                buck.generate_link_trees(arguments, ['target'])
                mock_normalize.assert_called_once_with('empty_target')
                self.assertEqual(
                    arguments.link_trees,
                    ['link_tree', 'new_tree'])
