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
        self.max_depth = 5  # Maximum depth of recursion
        self.current_depth = 0  # Current depth of recursion

    def generate_variable_names(self) -> List[str]:
        single_letter_names = list(string.ascii_lowercase)
        two_letter_names = [''.join(pair) for pair in itertools.product(string.ascii_lowercase, repeat=2)]
        names = single_letter_names + two_letter_names
        # Remove reserved keywords
        reserved_keywords = {'as', 'in', 'if', 'is', 'or'}
        names = [name for name in names if name not in reserved_keywords]
        return names

    def generate_new_variable(self) -> str:
        if self.current_var >= len(self.variables):
            self.current_var = 0  # Reset if all variables are used
        variable_name = self.variables[self.current_var]
        self.current_var += 1
        return variable_name

    def generate_new_function(self) -> str:
        function_name = f"f{self.current_function_number}"
        self.current_function_number += 1
        return function_name

    def generate_sink(self) -> str:
        return f"{self.last_sink}({self.last_source})"

    def generate_expression(self) -> str:
        # Simple expression generation for demonstration purposes
        var1 = self.generate_new_variable()
        var2 = self.generate_new_variable()
        expression = f"{var1} + {var2}"
        return expression

    def generate(self) -> str:
        # Generates the complete code with source and sink statements
        code_lines = self.source_statements + self.sink_statements
        code_lines.append(self.generate_sink())
        return '\n'.join(code_lines)

    def source_mutation(self) -> None:
        # Check if max depth has been reached
        if self.current_depth >= self.max_depth:
            return  # Stop recursion

        self.current_depth += 1
        mutation_type = random.choice(['assignment', 'if_then_else', 'sequential'])
        if mutation_type == 'assignment':
            self.assignment_mutation()
        elif mutation_type == 'if_then_else':
            self.if_then_else_mutation()
        elif mutation_type == 'sequential':
            self.sequential_statements()
        self.current_depth -= 1

    def assignment_mutation(self) -> None:
        variable = self.generate_new_variable()
        expression = self.generate_expression()
        self.source_statements.append(f"{variable} = {expression}")
        self.last_source = variable

    def if_then_else_mutation(self) -> None:
        condition = self.generate_expression()  # Generate a condition
        self.source_statements.append(f"if {condition}:")
        self.source_mutation()  # Recursively generate mutation for if body
        self.source_statements.append("else:")
        self.source_mutation()  # Recursively generate mutation for else body

    def sequential_statements(self) -> None:
        self.source_mutation()  # First statement
        self.source_mutation()  # Second statement, executed sequentially

# Example usage
if __name__ == "__main__":
    generator = CodeGenerator()
    generator.source_mutation()  # Start generating code
    print(generator.generate())  # Print the generated code
