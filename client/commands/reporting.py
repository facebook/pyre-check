import fnmatch
import json
import logging
import os
from typing import Set

from .. import log
from ..error import Error
from .command import ClientException, Command


LOG = logging.getLogger(__name__)

TEXT = "text"  # type: str
JSON = "json"  # type: str


class Reporting(Command):
    def __init__(self, arguments, configuration, analysis_directory) -> None:
        super().__init__(arguments, configuration, analysis_directory)
        self._verbose = arguments.verbose
        self._output = arguments.output
        self._ignore_all_errors_paths = configuration.ignore_all_errors
        self._discovered_source_directories = [self._local_root]
        self._local_configuration = arguments.local_configuration

    def _print(self, errors) -> None:
        errors = [
            error
            for error in errors
            if (
                not error.is_ignored()
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
        current_project_directory = self._analysis_directory.get_filter_root()
        directories_to_analyze = {
            os.path.relpath(filter_root, os.getcwd())
            for filter_root in current_project_directory
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
            ignore_error = False
            external_to_global_root = True
            if full_path.startswith(self._current_directory):
                external_to_global_root = False
            for absolute_ignore_path in self._ignore_all_errors_paths:
                if fnmatch.fnmatch(full_path, (absolute_ignore_path + "*")):
                    ignore_error = True
                    break
            errors.add(Error(ignore_error, external_to_global_root, **error))

        return errors
