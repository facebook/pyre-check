# Copyright 2004-present Facebook.  All rights reserved.

import json
import os
import logging

from tools.pyre.scripts import (
    EnvironmentException,
    CONFIGURATION_FILE,
    get_version_hash,
)


LOG = logging.getLogger(__name__)


class InvalidConfiguration(Exception):
    pass


class Configuration:
    def __init__(self):
        self.link_trees = []
        self.targets = []
        self.version = None
        self.disabled = False
        self.logger = None

        self.version_hash = None
        self.stub_roots = [self._get_typeshed_directory()]

        # Order matters. The values will only be updated if a field is None.
        self._read(CONFIGURATION_FILE + '.local')
        self._read(CONFIGURATION_FILE)

    def _read(self, path):
        try:
            with open(path) as file:
                LOG.debug('Reading configuration `%s`...', path)

                configuration = json.load(file)

                self.link_trees = configuration.get('link_trees', [])
                if self.link_trees:
                    LOG.debug(
                        'Found link trees `%s`', ', '.join(self.link_trees))

                self.targets = configuration.get('targets', [])
                if self.targets:
                    LOG.debug('Found targets `%s`', ', '.join(self.targets))

                if "disabled" in configuration:
                    self.disabled = True

                self.logger = configuration.get('logger')

                self.stub_roots.extend(
                    configuration.get('additional_stub_roots', []))

                self.version_hash = configuration.get('version')
        except IOError:
            LOG.debug('No configuration found at `{}`.'.format(path))
        except json.JSONDecodeError as error:
            raise EnvironmentException(
                'Configuration file at `{}` is invalid: {}.'.format(
                    path,
                    str(error)))

    def validate(self):
        try:
            if not self.targets and not self.link_trees:
                raise EnvironmentException(
                    'No targets or link trees to analyze.')

            def is_list_of_strings(list):
                return not isinstance(list, str) and \
                    all(isinstance(element, str) for element in list)

            if not is_list_of_strings(self.link_trees) or \
                    not is_list_of_strings(self.targets):
                raise InvalidConfiguration(
                    '`target` and `link_trees` fields must be lists of '
                    'strings.')

            if not is_list_of_strings(self.stub_roots):
                raise InvalidConfiguration(
                    '`additional_stub_roots` fields must be lists of strings.')
        except InvalidConfiguration as error:
            raise EnvironmentException(
                'Configuration file at `{}` or `{}` is invalid: {}.'.format(
                    CONFIGURATION_FILE,
                    CONFIGURATION_FILE + '.local',
                    str(error)))

    def _get_typeshed_directory(self):
        override = os.getenv('PYRE_TYPESHED')
        return override or os.path.join(
            '/mnt/dewey/fbsource/.commits/',
            get_version_hash(self),
            'fbcode/builds/typeshed')
