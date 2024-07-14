import random
import string
import itertools

class CodeGenerator:
    def __init__(self) -> None:
        self.variables = self.generate_variable_names()
        self.current_var = 0
    
    def generate_variable_names(self) -> list:
        single_letter_names = list(string.ascii_lowercase)
        two_letter_names = [''.join(pair) for pair in itertools.product(string.ascii_lowercase, repeat=2)]
        names = single_letter_names + two_letter_names
        reserved_keywords = {'as', 'in', 'if', 'is', 'or'}
        names = [name for name in names if name not in reserved_keywords]
        return names

    def generate_new_variable(self) -> str:
        index = self.current_var
        self.current_var += 1
        return self.variables[index]

    def get_last_variable(self) -> str:
        return self.variables[self.current_var - 1]

    def reset(self) -> None:
        self.current_var = 0

    def generate_source(self) -> str:
        curr_var = self.generate_new_variable()
        return f"def f1():\n    return input()\n\na = f1()\n\nprint(a)\n"

    def generate_sink(self) -> str:
        prev_var = self.get_last_variable()
        return f"print({prev_var})"

    def add_extra_variable(self, code: str) -> str:
        code_lines = code.split('\n')
        insert_index = code_lines.index("print(a)")  # Assuming 'a' is the last variable
        code_lines.insert(insert_index, "b = a")
        code_lines[insert_index + 1] = "print(b)"
        return '\n'.join(code_lines)

    def add_function_call(self, code: str) -> str:
        code_lines = code.split('\n')
        func_index = code_lines.index("def f1():")  # Assuming 'f1' is the first function
        code_lines.insert(func_index + 2, "def f2():\n    return f1()")
        code_lines[func_index + 6] = "a = f2()"
        return '\n'.join(code_lines)

    def generate_mutations(self, num_mutations: int) -> str:
        operations = [
            self.add_extra_variable,
            self.add_function_call
        ]

        code = self.generate_source()
        print("Initial State:\n", code)

        for i in range(num_mutations):
            operation = operations[i % len(operations)]
            code = operation(code)
            print(f"Mutation [{i + 1}]:\n", code)
        
        return code

if __name__ == "__main__":
    generator = CodeGenerator()
    num_mutations = 2  # You can change this number to control the number of mutations
    generator.generate_mutations(num_mutations)
