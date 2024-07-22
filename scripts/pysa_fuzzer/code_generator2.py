import os
import random
import string
import itertools
from typing import List

class CodeGenerator:
    def __init__(self) -> None:
        self.variables = self.generate_variable_names()
        self.current_var = 0
        self.current_function_number = 0
        self.source_statements = []
        self.last_source = "input()"
        self.sink_statements = []
        self.last_sink = "print"

    def generate_variable_names(self) -> List[str]:
        single_letter_names = list(string.ascii_lowercase)
        two_letter_names = [''.join(pair) for pair in itertools.product(string.ascii_lowercase, repeat=2)]
        names = single_letter_names + two_letter_names
        # Remove reserved keywords
        reserved_keywords = {'as', 'in', 'if', 'is', 'or'}
        names = [name for name in names if name not in reserved_keywords]
        return names

    def generate_new_variable(self) -> str:
        variable_name = self.variables[self.current_var]
        self.current_var += 1
        return variable_name

    def generate_new_function(self) -> str:
        function_name = f"f{self.current_function_number}"
        self.current_function_number += 1
        return function_name

    def generate_sink(self) -> str:
        return f"{self.last_sink}({self.last_source})"

    def add_function_1(self) -> None:
        indent_space = ' ' * 4
        # source stuff
        current_function_source = self.generate_new_function()
        temp_source = self.last_source
        self.last_source = current_function_source + "()"
        new_var = self.generate_new_variable()
        # sink stuff
        current_function_sink = self.generate_new_function()
        temp_sink = self.last_sink
        self.last_sink = current_function_sink
        # adding statements
        self.source_statements.append(f"""
def {current_function_source}():
{indent_space}if random.randint(1, 3) == 1:
{indent_space*2}return {temp_source}
{indent_space}else:
{indent_space*2}return {current_function_source}()
        """)
        self.sink_statements.append(f"""
def {current_function_sink}(x):
{indent_space}if random.randint(1, 3) == 1:
{indent_space*2}return {temp_sink}(x)
{indent_space}else:
{indent_space*2}return {current_function_sink}(x)
        """)

    def add_function_2(self) -> None:
        indent_space = ' ' * 4
        # source stuff
        current_function_source = self.generate_new_function()
        temp_source = self.last_source
        self.last_source = current_function_source + f"({temp_source})"
        new_var = self.generate_new_variable()
        # sink stuff
        current_function_sink = self.generate_new_function()
        temp_sink = self.last_sink
        self.last_sink = current_function_sink
        # adding statements
        self.source_statements.append(f"""
def {current_function_source}(x):
{indent_space}if random.randint(1, 3) == 1:
{indent_space*2}return x
{indent_space}else:
{indent_space*2}return {current_function_source}(x)
        """)
        self.sink_statements.append(f"""
def {current_function_sink}(x):
{indent_space}if random.randint(1, 3) == 1:
{indent_space*2}return {temp_sink}(x)
{indent_space}else:
{indent_space*2}return {current_function_sink}(x)
        """)

    def generate(self) -> str:
        code_lines = self.source_statements + self.sink_statements
        code_lines.append(self.generate_sink())
        return '\n'.join(code_lines)

