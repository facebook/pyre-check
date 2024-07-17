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
        self.variable_assignments = []  # Initialize the variable_assignments attribute

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

    def add_function(self) -> None:
        indent_space = ' ' * 4
        current_function_source = self.generate_new_function()
        # Adjust to include variables directly in function
        if self.variable_assignments:
            # Include the variable assignment in the last function
            function_body = f"{self.variable_assignments[-1]}\n{indent_space}return {self.last_source}"
            self.variable_assignments.pop()  # Remove the last variable assignment as it's now included
        else:
            function_body = f"return {self.last_source}"

        self.source_statements.append(f"def {current_function_source}():\n{indent_space}{function_body}")
        self.last_source = current_function_source + "()"
        
        current_function_sink = self.generate_new_function()
        temp_sink = self.last_sink
        self.last_sink = current_function_sink
        self.sink_statements.append(f"def {current_function_sink}(x):\n{indent_space}return {temp_sink}(x)")

    def generate_sink(self) -> str:
        return f"{self.last_sink}({self.last_source})"

    def add_variable(self) -> None:
        current_var = self.generate_new_variable()
        self.variable_assignments.append(f"{current_var} = {self.last_source}")
        self.last_source = current_var

    def generate_new_variable(self) -> str:
        index = self.current_var
        self.current_var += 1
        return self.variables[index]

    def generate(self) -> str:
        code_lines = self.source_statements + self.sink_statements
        code_lines.append(self.generate_sink())
        return '\n'.join(code_lines)

# Example usage
generator = CodeGenerator()
generator.add_function()
generator.add_variable()  # Variable assignments are now within the last function added
generator.add_function()
generator.add_variable()  # Variable assignments are now within the last function added
generator.add_function()

print(generator.generate())


# example generation 
def f1():
    return input()
def f3():
    a = f1()
    return a
def f5():
    b = f3()
    return b
def f2(x):
    return print(x)
def f4(x):
    return f2(x)
def f6(x):
    return f4(x)
f6(f5())


