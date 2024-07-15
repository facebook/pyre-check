from typing import List
import random
import textwrap
import itertools
import string

class CodeGenerator:
    def __init__(self) -> None:
        self.variables = self.generate_variable_names()
        self.current_var = 0
        self.current_function_number = 1
        self.last_node = "input()"

    def generate_variable_names(self) -> List[str]:
        single_letter_names = list(string.ascii_lowercase)
        two_letter_names = [''.join(pair) for pair in itertools.product(string.ascii_lowercase, repeat=2)]
        names = single_letter_names + two_letter_names
        # Remove reserved keywords
        reserved_keywords = {'as', 'in', 'if', 'is', 'or'}
        names = [name for name in names if name not in reserved_keywords]
        return names

    def generate_new_function_name(self) -> str:
        function_name = f"f{self.current_function_number}"
        self.current_function_number += 1
        return function_name

    def generate_new_variable(self) -> str:
        index = self.current_var
        self.current_var += 1
        return self.variables[index]

    def get_last_variable(self) -> str:
        return self.variables[self.current_var - 1]

    def generate_sink(self) -> str: 
        return f"print({self.last_node})"
         
    def add_variable(self) -> str:
        current_var = self.generate_new_variable()
        temp = self.last_node 
        self.last_node = current_var
        return f"{current_var} = {temp}" 



generator = CodeGenerator()

print(generator.add_variable())
print(generator.add_variable())
print(generator.add_variable())

print(generator.generate_sink())
