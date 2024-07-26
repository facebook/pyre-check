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

    def generate(self) -> str:
        code_lines = self.source_statements + self.sink_statements
        code_lines.append(self.generate_sink())
        return '\n'.join(code_lines)

    def source_mutation_1(self) -> None: 
        indent_space = ' ' * 4
        current_function = self.generate_new_function()
        self.source_statements.append(f"""
def {current_function}(x):
{indent_space}return x
        """)
        temp = self.last_source
        self.last_source = f"{current_function}({temp})" 

    def source_mutation_2(self) -> None: 
        indent_space = ' ' * 4 
        current_var = self.generate_new_variable()
        self.source_statements.append(f"""
{current_var} = {self.last_source}
        """)
        self.last_source = current_var

generator = CodeGenerator()

generator.source_mutation_2()
generator.source_mutation_2()
generator.source_mutation_2()

generator.source_mutation_2()

