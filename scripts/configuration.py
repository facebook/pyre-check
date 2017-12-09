# Copyright 2004-present Facebook.  All rights reserved.

import functools
import json
import os
import logging

from tools.pyre.scripts import (
    EnvironmentException,
    CONFIGURATION_FILE,
)


LOG = logging.getLogger(__name__)


class InvalidConfiguration(Exception):
    pass


class Configuration:
    def __init__(self):
        self.link_trees = []
        self.targets = []
        self.disabled = False
        self.logger = None

        self._stub_roots = []
        self._version_hash = None
        self._binary = None
        self._typeshed = None

        # Order matters. The values will only be updated if a field is None.
        self._read(CONFIGURATION_FILE + '.local')
        self._read(CONFIGURATION_FILE)

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

            if not self._binary:
                raise InvalidConfiguration('`binary` must be defined')
            if not os.path.exists(self.get_binary()):
                raise InvalidConfiguration(
                    'Binary at `{}` does not exist'.format(self._binary))

            # Validate stub roots.
            if not self._typeshed:
                raise InvalidConfiguration('`typeshed` must be defined')
            for stub_root in self.get_stub_roots():
                if not os.path.isdir(stub_root):
                    raise InvalidConfiguration(
                        '`{}` is not a valid stub root'.format(stub_root))
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
    def get_stub_roots(self):
        if not self._typeshed:
            raise InvalidConfiguration('Configuration was not validated')

        version_hash = self.get_version_hash()
        if version_hash and '%V' in self._typeshed:
            typeshed = self._typeshed.replace('%V', version_hash)
        else:
            typeshed = self._typeshed

        return self._stub_roots + [typeshed]

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

                self._binary = configuration.get(
                    'binary',
                    os.getenv('PYRE_BINARY'))
                self._stub_roots.extend(
                    configuration.get('additional_stub_roots', []))
                self._version_hash = configuration.get(
                    'version',
                    os.getenv('PYRE_VERSION_HASH'))
                self._typeshed = configuration.get('typeshed')
        except IOError:
            LOG.debug('No configuration found at `{}`.'.format(path))
        except json.JSONDecodeError as error:
            raise EnvironmentException(
                'Configuration file at `{}` is invalid: {}.'.format(
                    path,
                    str(error)))
