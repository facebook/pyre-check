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

BuckOut = namedtuple('BuckOut', 'link_trees targets_not_found')


class BuckTest(unittest.TestCase):
    def test_presumed_target_root(self) -> None:
        self.assertEqual(
            buck.presumed_target_root('//path/directory/...'),
            'path/directory')
        self.assertEqual(
            buck.presumed_target_root('/path/directory:target'),
            'path/directory')

    def test_find_link_trees(self) -> None:
        trees = [
            'blah-vs_debugger#link-tree',
            'blah-blah#link-tree',
            'blah-interp#link-tree',
            'blah-ipython#link-tree']
        with patch.object(glob, 'glob', return_value=trees) as glob_glob:
            self.assertEqual(
                buck._find_link_trees({'target': None}),
                BuckOut(['blah-blah#link-tree'], []))
        with patch.object(glob, 'glob') as glob_glob:
            buck._find_link_trees({
                '//path/targets:name': None,
                '//path/targets:namelibrary': None,
                '//path/...': None})
            glob_glob.assert_has_calls([
                call('buck-out/gen/path/targets/name#*link-tree'),
                call('buck-out/gen/path/targets/namelibrary#*link-tree'),
                call('buck-out/gen/path/...#*link-tree'),
            ], any_order=True)

        with patch.object(glob, 'glob', return_value=['new_tree']) as glob_glob:
            found_trees = buck._find_link_trees({
                '//path/targets:name': None,
                '//path/targets:namelibrary': None,
                '//path/targets:another': 'buck-out/path/another',
                '//path/...': None})
            self.assertEqual(
                found_trees,
                BuckOut(['new_tree', 'new_tree', 'new_tree', 'new_tree'], []))

        with patch.object(glob, 'glob', return_value=[]) as glob_glob:
            found_trees = buck._find_link_trees({
                '//path/targets:name': None,
                '//path/targets:namelibrary': None,
                '//path/targets:another': 'buck-out/path/another',
                '//path/...': None})
            self.assertEqual(
                found_trees,
                BuckOut([], [
                    '//path/targets:name',
                    '//path/targets:another',
                    '//path/...']))

    def test_normalize(self) -> None:
        with patch.object(subprocess, 'check_output') as buck_targets:
            buck._normalize('target_path')
            buck_targets.assert_called_once_with(
                ['buck', 'targets', 'target_path', '--show-output'],
                stderr=subprocess.DEVNULL)

    def test_build_targets(self) -> None:
        with patch.object(subprocess, 'check_output') as buck_build:
            buck._build_targets('target_path', ['subtarget1', 'subtarget2'])
            buck_build.assert_called_once_with(
                ['buck', 'build', 'subtarget1', 'subtarget2'],
                stderr=subprocess.DEVNULL)

    @patch.object(buck, '_get_yes_no_input', return_value=False)
    def test_generate_link_trees(self, mock_input) -> None:
        arguments = MagicMock()
        mock_find_link_trees = patch.object(buck, '_find_link_trees')
        mock_find_link_trees.return_value = BuckOut(  # noqa
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
