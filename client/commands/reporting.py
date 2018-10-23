import fnmatch
import json
import logging
import os
from typing import Set

from .. import log
from ..error import Error
from ..filesystem import get_filesystem
from .command import ClientException, Command


LOG = logging.getLogger(__name__)

TEXT = "text"  # type: str
JSON = "json"  # type: str


class Reporting(Command):
    def __init__(self, arguments, configuration, analysis_directory) -> None:
        super().__init__(arguments, configuration, analysis_directory)
        self._verbose = arguments.verbose
        self._output = arguments.output
        self._do_not_check_paths = configuration.do_not_check
        self._discovered_analysis_directories = [self._local_root]
        self._local_configuration = arguments.local_configuration

    def _print(self, errors) -> None:
        errors = [
            error
            for error in errors
            if (
                not error.is_do_not_check()
                and (self._verbose or not (error.is_external_to_global_root()))
            )
        ]
        errors = sorted(
            errors, key=lambda error: (error.path, error.line, error.column)
        )

        if errors:
            length = len(errors)
            LOG.error("Found %d type error%s!", length, "s" if length > 1 else "")
        else:
            LOG.log(log.SUCCESS, "No type errors found")

        if self._output == TEXT:
            log.stdout.write("\n".join([repr(error) for error in errors]))
        else:
            log.stdout.write(json.dumps([error.__dict__ for error in errors]))

    def _get_directories_to_analyze(self) -> Set[str]:
        current_project_directory = self._original_directory
        if self._local_configuration:
            current_project_directory = self._local_configuration
        directories_to_analyze = {
            os.path.relpath(current_project_directory, os.getcwd())
        }
        return directories_to_analyze

    def _get_errors(self, result) -> Set[Error]:
        result.check()

        errors = set()
        try:
            results = json.loads(result.output)
        except (json.JSONDecodeError, ValueError):
            raise ClientException("Invalid output: `{}`.".format(result.output))

        for error in results:
            full_path = os.path.realpath(
                os.path.join(self._analysis_directory.get_root(), error["path"])
            )
            # Relativize path to user's cwd.
            relative_path = self._relative_path(full_path)
            error["path"] = relative_path
            do_not_check = False
            external_to_global_root = True
            if full_path.startswith(self._current_directory):
                external_to_global_root = False
            for absolute_do_not_check_path in self._do_not_check_paths:
                if fnmatch.fnmatch(full_path, (absolute_do_not_check_path + "*")):
                    do_not_check = True
                    break
            errors.add(Error(do_not_check, external_to_global_root, **error))

        return errors
