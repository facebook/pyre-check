import random
import textwrap
from typing import List, Set, Callable

class CodeGenerator:
    def __init__(self):
        self.sources = ['user_input()', 'get_data()', 'fetch_value()']
        self.sinks = ['print', 'send_data', 'log_output']
        self.variables = [chr(i) for i in range(ord('a'), ord('z') + 1)]
        self.defined_variables: Set[str] = set()
        self.current_var_index = 0

    def indent(self, text: str, spaces: int = 4) -> str:
        return textwrap.indent(text, ' ' * spaces)

    def generate_arithmetic_expression(self, depth: int) -> str:
        if depth == 0:
            return str(random.randint(0, 10))
        op = random.choice(['+', '-', '*', '/'])
        return f"({self.generate_arithmetic_expression(depth - 1)} {op} {self.generate_arithmetic_expression(depth - 1)})"

    def generate_variable_expression(self, depth: int) -> str:
        if depth == 0:
            var = random.choice(list(self.defined_variables))
            return var
        op = random.choice(['+', '-', '*', '/'])
        return f"({self.generate_variable_expression(depth - 1)} {op} {self.generate_variable_expression(depth - 1)})"

    def get_next_variable(self) -> str:
        next_var = self.variables[self.current_var_index]
        self.current_var_index += 1
        if self.current_var_index == len(self.variables):
            self.variables.append(self.variables[self.current_var_index-1] + 'a')
        self.defined_variables.add(next_var)
        return next_var

    def generate_assignment(self, from_var: str) -> str:
        to_var = self.get_next_variable()
        expr = self.generate_expression(2)
        return f"{to_var} = {expr}"

    def generate_conditional_expression(self, depth: int) -> str:
        if depth == 0:
            return str(random.randint(0, 10))
        op = random.choice(['==', '!=', '<', '<=', '>', '>='])
        return f"({self.generate_conditional_expression(depth - 1)} {op} {self.generate_conditional_expression(depth - 1)})"

    def generate_if_statement(self, depth: int, from_var: str) -> str:
        condition = self.generate_conditional_expression(1)
        true_branch = self.generate_complex_expression(depth - 1, from_var)
        false_branch = self.generate_complex_expression(depth - 1, from_var)
        return f"if {condition}:\n{self.indent(true_branch)}\nelse:\n{self.indent(false_branch)}"

    def generate_function_call(self, depth: int, from_var: str) -> str:
        func = random.choice(['foo', 'bar', 'baz', 'qux'])
        args = ', '.join([self.generate_variable_expression(1) for _ in range(random.randint(1, 3))])
        return f"{func}({args})"

    def generate_for_loop(self, depth: int, from_var: str) -> str:
        loop_var = self.get_next_variable()
        start = random.randint(0, 10)
        end = random.randint(start, 20)
        body = self.generate_complex_expression(depth - 1, from_var)
        return f"for {loop_var} in range({start}, {end}):\n{self.indent(body)}"

    def generate_while_loop(self, depth: int, from_var: str) -> str:
        condition = self.generate_conditional_expression(1)
        body = self.generate_complex_expression(depth - 1, from_var)
        return f"while {condition}:\n{self.indent(body)}"

    def generate_try_except(self, depth: int, from_var: str) -> str:
        try_block = self.generate_complex_expression(depth - 1, from_var)
        except_block = self.generate_complex_expression(depth - 1, from_var)
        return f"try:\n{self.indent(try_block)}\nexcept Exception as e:\n{self.indent(except_block)}"

    def generate_function_definition(self, depth: int, from_var: str) -> str:
        func_name = random.choice(['foo', 'bar', 'baz', 'qux'])
        params = ', '.join(random.sample(self.variables, random.randint(1, 3)))
        body = self.generate_complex_expression(depth - 1, from_var)
        return f"def {func_name}({params}):\n{self.indent(body)}"

    def generate_expression(self, depth: int) -> str:
        if depth == 0:
            return random.choice([str(random.randint(0, 10)), random.choice(list(self.defined_variables))])
        choices = [
            lambda: self.generate_arithmetic_expression(depth - 1),
            lambda: self.generate_variable_expression(depth - 1),
            lambda: self.generate_function_call(depth - 1, ''),
        ]
        return random.choice(choices)()

    def generate_statement(self, depth: int, from_var: str) -> str:
        choices = [
            lambda: self.generate_assignment(from_var),
            lambda: self.generate_if_statement(depth, from_var),
            lambda: self.generate_for_loop(depth, from_var),
            lambda: self.generate_while_loop(depth, from_var),
            lambda: self.generate_try_except(depth, from_var),
            lambda: self.generate_function_definition(depth, from_var),
        ]
        return random.choice(choices)()

    def generate_complex_expression(self, depth: int, from_var: str) -> str:
        if depth == 0:
            return self.generate_assignment(from_var)
        parts: List[Callable[[], str]] = [
            lambda: self.generate_assignment(from_var),
            lambda: self.generate_if_statement(depth, from_var),
            lambda: self.generate_function_call(depth, from_var),
            lambda: self.generate_for_loop(depth, from_var),
            lambda: self.generate_while_loop(depth, from_var),
            lambda: self.generate_try_except(depth, from_var),
            lambda: self.generate_function_definition(depth, from_var)
        ]
        random.shuffle(parts)
        return '\n'.join([part() for part in parts[:random.randint(2, 3)]])

    def generate_source_to_sink(self) -> str:
        self.current_var_index = 0  # Reset the index for each new generation
        self.defined_variables.clear()
        source = random.choice(self.sources)
        source_var = self.get_next_variable()
        intermediate_code = self.generate_complex_expression(depth=5, from_var=source_var)
        sink = random.choice(self.sinks)
        sink_var = source_var  # Ensure sink_var is defined
        return f"{source_var} = {source}\n{intermediate_code}\n{sink}({sink_var})  # Sink: {sink_var} flows from source: {source}"

if __name__ == "__main__":
    generator = CodeGenerator()
    print(generator.generate_source_to_sink())
