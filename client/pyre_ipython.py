# Copyright (c) 2016-present, Facebook, Inc.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

import os
import tempfile

from IPython.core.magic import Magics, line_cell_magic, line_magic, magics_class


class HistoryContext:
    def __init__(self):
        self.history = []
        self.history_start = 1
        self.history_end = 1

    def get_history(self):
        return self.history

    def get_history_without_ipython_calls(self):
        """
        Remove all lines in execution history added by iPython calls.
        """
        line_count = 0
        filtered_history = []
        for line in self.history:
            if "get_ipython()." not in line:
                filtered_history.append(line)
                line_count += 1
        return filtered_history, line_count

    def add_to_history(self, lines):
        self.history += lines

    def get_history_start(self):
        return self.history_start

    def get_history_end(self):
        return self.history_end

    def increment_end(self):
        self.history_end += 1

    def record_history_write(self):
        self.history_start = self.history_end

    def reset_history(self):
        self.history = []
        self.history_start = self.history_end


history_context = HistoryContext()


def process_input():
    history_context.increment_end()


def call_client(source_data, error_start):
    with tempfile.TemporaryFile("w+") as source_file:
        source_file.write("\n".join(source_data))
        source_file.seek(0)  # for debugging
        print(source_file.read())  # for debugging
    print("Start returning errors on line {}".format(error_start))  # for debugging


@magics_class
class ExecutePyre(Magics):
    """
    Usage example
    ---------
    In [1]: %load_ext pyre_ipython
    Pyre execution magic loaded.
    In [2]: %%check
       ...: def foo():
       ...:     return x
    In [3]: %check x = 1
    In [4]: check x = 1
    """

    @line_cell_magic
    def check(self, line, cell=None):
        """
        Typecheck input and add it to the environment.
        """
        code = cell or line
        ipython = get_ipython()  # noqa
        self.write_history()

        # create sourcefile and perform typecheck
        source_data, line_count = history_context.get_history_without_ipython_calls()
        code_lines = code.split("\n")
        source_data += code_lines
        call_client(source_data, line_count + 1)

        # add checked code to history
        history_context.add_to_history(code_lines)
        ipython.run_code(code)

    @line_cell_magic
    def check_no_exec(self, line, cell=None):
        """
        Typecheck input without executing it or adding it to the future
        environment.
        """
        code = cell or line
        ipython = get_ipython()  # noqa
        self.write_history()

        # create sourcefile and perform typecheck
        source_data, line_count = history_context.get_history_without_ipython_calls()
        source_data += code.split("\n")
        call_client(source_data, line_count + 1)

    @line_magic
    def env(self, line=None):
        """
        Display the current type environment.
        """
        self.write_history()
        print(history_context.get_history())  # logging (to be deleted)

    @line_magic
    def clear_env(self, line=None):
        """
        Clear the type environment and all previously executed history.
        """
        history_context.reset_history()
        print("Environment has been reset.")

    def write_history(self):
        temp_file = "tempfile.py"
        history_command = "history {}-{} -t -f {}".format(
            history_context.get_history_start(),
            history_context.get_history_end(),
            temp_file,
        )
        if os.path.exists(temp_file):
            os.remove(temp_file)
        ipython = get_ipython()  # noqa
        ipython.magic(history_command)

        lines = []
        with open(temp_file, "r") as temp_file_readable:
            lines = temp_file_readable.readlines()
        lines_list = [line.strip() for line in lines]
        history_context.add_to_history(lines_list)
        history_context.record_history_write()
        os.remove(temp_file)


def load_ipython_extension(ipython):
    ipython.register_magics(ExecutePyre)
    ipython.events.register("pre_execute", process_input)

    existing_history = "existing_history.py"
    history_command = "history -f {}".format(existing_history)
    ipython.magic(history_command)
    with open(existing_history, "r") as existing_history_readable:
        for _i, _l in enumerate(existing_history_readable):
            history_context.increment_end()
    history_context.reset_history()
    os.remove(existing_history)
    print("Pyre execution magic loaded.")


def unload_ipython_extension(ipython):
    history_context.reset_history()
