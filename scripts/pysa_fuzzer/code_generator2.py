from typing import List
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

    def generate_new_function(self) -> str:
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

    def add_function(self) -> str:
        current_function = self.generate_new_function()
        temp = self.last_node
        self.last_node = current_function + "()"
        indent_space = ' ' * 4
        return f"def {current_function}():\n{indent_space}return {temp}"

    def generate(self, instructions: List[int]) -> str:
        code_lines = []
        for instruction in instructions:
            if instruction == 1:
                code_lines.append(self.add_function())
            elif instruction == 2:
                code_lines.append(self.add_variable())
        code_lines.append(self.generate_sink())
        return '\n'.join(code_lines)

# Example usage
generator = CodeGenerator()
instructions = [1, 2, 1, 1, 2, 1, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2]
print(generator.generate(instructions))
