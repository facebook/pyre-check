# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import functools
import json
import os
import logging

from . import (
    EnvironmentException,
    CONFIGURATION_FILE,
)

from typing import (
    List,
)


LOG = logging.getLogger(__name__)


class InvalidConfiguration(Exception):
    pass


class Configuration:
    _disabled: bool

    def __init__(
            self,
            original_directory=None,
            local_configuration=None,
            search_path=None,
            preserve_pythonpath=False) -> None:
        self.source_directories = []
        self.targets = []
        self.logger = None

        self._disabled = False
        self._version_hash = None
        self._binary = None
        self._typeshed = None

        # Handle search path from multiple sources
        self._search_directories = []
        pythonpath = os.getenv('PYTHONPATH')
        if preserve_pythonpath and pythonpath:
            for path in pythonpath.split(':'):
                if os.path.isdir(path):
                    self._search_directories.append(path)
                else:
                    LOG.warning(
                        '`{}` is not a valid directory, dropping it '
                        'from PYTHONPATH'.format(path))
        if search_path:
            self._search_directories.extend(search_path)
        # We will extend the search path further, with the config file
        # items, inside _read().

        # Order matters. The values will only be updated if a field is None.
        local_configuration = local_configuration or original_directory
        if local_configuration:
            if not os.path.isfile(local_configuration):
                local_configuration = os.path.join(
                    local_configuration,
                    CONFIGURATION_FILE + '.local')
            self._read(local_configuration)
        self._read(CONFIGURATION_FILE + '.local')
        self._read(CONFIGURATION_FILE)

    def validate(self) -> None:
        try:
            def is_list_of_strings(list):
                if len(list) == 0:
                    return True
                return not isinstance(list, str) and \
                    all(isinstance(element, str) for element in list)

            if not is_list_of_strings(self.source_directories) or \
                    not is_list_of_strings(self.targets):
                raise InvalidConfiguration(
                    '`target` and `source_directories` fields must be lists of '
                    'strings.')

            if not self._binary:
                raise InvalidConfiguration('`binary` must be defined')
            if not os.path.exists(self.get_binary()):
                raise InvalidConfiguration(
                    'Binary at `{}` does not exist'.format(self._binary))

            # Validate elements of the search path.
            if not self._typeshed:
                raise InvalidConfiguration('`typeshed` must be defined')
            for path in self.get_search_path():
                if not os.path.isdir(path):
                    raise InvalidConfiguration(
                        '`{}` is not a valid directory'.format(path))
        except InvalidConfiguration as error:
            raise EnvironmentException(
                'Configuration file at `{}` or `{}` is invalid: {}.'.format(
                    CONFIGURATION_FILE,
                    CONFIGURATION_FILE + '.local',
                    str(error)))

    def get_version_hash(self):
        return self._version_hash

    @functools.lru_cache(1)
    def get_binary(self):
        if not self._binary:
            raise InvalidConfiguration('Configuration was not validated')

        version_hash = self.get_version_hash()
        if version_hash and '%V' in self._binary:
            return self._binary.replace('%V', version_hash)
        else:
            return self._binary

    @functools.lru_cache(1)
    def get_search_path(self) -> List[str]:
        if not self._typeshed:
            raise InvalidConfiguration('Configuration was not validated')

        version_hash = self.get_version_hash()
        if version_hash and '%V' in self._typeshed:
            typeshed = self._typeshed.replace('%V', version_hash)
        else:
            typeshed = self._typeshed

        return self._search_directories + [typeshed]

    def disabled(self) -> bool:
        return self._disabled

    def _read(self, path) -> None:
        try:
            with open(path) as file:
                LOG.debug('Reading configuration `%s`...', path)

                configuration = json.load(file)

                if not self.source_directories:
                    self.source_directories = configuration.get(
                        'source_directories',
                        [])
                if not self.source_directories:
                    self.source_directories = configuration.get(
                        'link_trees',
                        [])
                if self.source_directories:
                    LOG.debug(
                        'Found source directories `%s`', ', '.join(
                            self.source_directories))

                if not self.targets:
                    self.targets = configuration.get('targets', [])
                if self.targets:
                    LOG.debug('Found targets `%s`', ', '.join(self.targets))

                if "disabled" in configuration:
                    self._disabled = True

                if not self.logger:
                    self.logger = configuration.get('logger')

                if not self._binary:
                    self._binary = os.getenv('PYRE_BINARY')
                    if self._binary:
                        LOG.warning('Binary overridden with `%s`', self._binary)
                if not self._binary:
                    self._binary = configuration.get('binary')

                # TODO(T25858716): remove this once migration is complete
                self._search_directories.extend(
                    configuration.get('additional_stub_roots', []))
                self._search_directories.extend(
                    configuration.get('search_path', []))

                if not self._version_hash:
                    self._version_hash = os.getenv('PYRE_VERSION_HASH')
                if not self._version_hash:
                    self._version_hash = configuration.get('version')

                if not self._typeshed:
                    self._typeshed = configuration.get('typeshed')
        except IOError:
            LOG.debug('No configuration found at `{}`.'.format(path))
        except json.JSONDecodeError as error:
            raise EnvironmentException(
                'Configuration file at `{}` is invalid: {}.'.format(
                    path,
                    str(error)))
