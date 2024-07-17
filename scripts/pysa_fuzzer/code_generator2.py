from typing import List
import itertools
import string

class CodeGenerator:
    def __init__(self) -> None:
        self.variables = self.generate_variable_names()
        self.current_var = 0
        self.current_function_number = 1
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

    def generate_new_function(self) -> str:
        function_name = f"f{self.current_function_number}"
        self.current_function_number += 1
        return function_name

    def generate_sink(self) -> str:
        return f"{self.last_sink}({self.last_source})"

    def add_function(self) -> None:
        indent_space = ' ' * 4

        current_function_source = self.generate_new_function()
        temp_source = self.last_source
        self.last_source = current_function_source + "()"

        current_function_sink = self.generate_new_function()
        temp_sink = self.last_sink
        self.last_sink = current_function_sink

        self.source_statements.append(f"def {current_function_source}():\n{indent_space}random_number = random.randint(1,3)\n{indent_space}if random_number == 1:\n{indent_space*2}return {temp_source}\n{indent_space}else:\n{indent_space*2}return {current_function_source}()")
        self.sink_statements.append(f"def {current_function_sink}(x):\n{indent_space}random_number = random.randint(1,3)\n{indent_space}if random_number == 1:\n{indent_space*2}return {temp_sink}(x)\n{indent_space}else:\n{indent_space*2}return {current_function_sink}(x)")

    def generate(self) -> str:
        code_lines = self.source_statements + self.sink_statements
        code_lines.append(self.generate_sink())
        return '\n'.join(code_lines)

# Example usage
generator = CodeGenerator()
generator.add_function()


print(generator.generate())

import random

def f1():
    random_number = random.randint(1,5)
    if random_number == 1:
        return input()
    else:
        return f1()
def f2(x):
    random_number = random.randint(1,5)
    if random_number == 1:
        return print(x)
    else:
        return f2(x)
f2(f1())


