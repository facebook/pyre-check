import random
import textwrap
from typing import List, Set, Callable

# List of source functions simulating user or external input
sources = ['user_input()', 'get_data()', 'fetch_value()']

# List of sink functions that simulate vulnerable data handling points
sinks = ['print', 'send_data', 'log_output']

# List of variable names to use in expressions, in alphabetical order
variables = ['a', 'b', 'c', 'd', 'e', 'f', 'g']

# List of function names to use in function calls and definitions
functions = ['foo', 'bar', 'baz', 'qux']

# Tracking defined variables to ensure they are declared before use
defined_variables: Set[str] = set()
current_var_index = 0

def indent(text: str, spaces: int = 4) -> str:
    return textwrap.indent(text, ' ' * spaces)

def generate_arithmetic_expression(depth: int) -> str:
    if depth == 0:
        return str(random.randint(0, 10))
    op = random.choice(['+', '-', '*', '/'])
    return f"({generate_arithmetic_expression(depth - 1)} {op} {generate_arithmetic_expression(depth - 1)})"

def generate_variable_expression(depth: int) -> str:
    if depth == 0:
        var = random.choice(list(defined_variables))
        return var
    op = random.choice(['+', '-', '*', '/'])
    return f"({generate_variable_expression(depth - 1)} {op} {generate_variable_expression(depth - 1)})"

def get_next_variable() -> str:
    global current_var_index
    next_var = variables[current_var_index]
    current_var_index = (current_var_index + 1) % len(variables)
    defined_variables.add(next_var)
    return next_var

def generate_assignment(from_var: str) -> str:
    to_var = get_next_variable()
    expr = f"{from_var} {random.choice(['+', '-', '*', '/'])} {generate_variable_expression(2)}"
    return f"{to_var} = {expr}"

def generate_conditional_expression(depth: int) -> str:
    if depth == 0:
        return str(random.randint(0, 10))
    op = random.choice(['==', '!=', '<', '<=', '>', '>='])
    return f"({generate_conditional_expression(depth - 1)} {op} {generate_conditional_expression(depth - 1)})"

def generate_if_statement(depth: int, from_var: str) -> str:
    condition = generate_conditional_expression(1)
    true_branch = generate_complex_expression(depth - 1, from_var)
    false_branch = generate_complex_expression(depth - 1, from_var)
    return f"if {condition}:\n{indent(true_branch)}\nelse:\n{indent(false_branch)}"

def generate_function_call(depth: int, from_var: str) -> str:
    func = random.choice(functions)
    args = ', '.join([generate_variable_expression(1) for _ in range(random.randint(1, 3))])
    return f"{func}({args})"

def generate_for_loop(depth: int, from_var: str) -> str:
    loop_var = get_next_variable()
    start = random.randint(0, 10)
    end = random.randint(start, 20)
    body = generate_complex_expression(depth - 1, from_var)
    return f"for {loop_var} in range({start}, {end}):\n{indent(body)}"

def generate_while_loop(depth: int, from_var: str) -> str:
    condition = generate_conditional_expression(1)
    body = generate_complex_expression(depth - 1, from_var)
    return f"while {condition}:\n{indent(body)}"

def generate_try_except(depth: int, from_var: str) -> str:
    try_block = generate_complex_expression(depth - 1, from_var)
    except_block = generate_complex_expression(depth - 1, from_var)
    return f"try:\n{indent(try_block)}\nexcept Exception as e:\n{indent(except_block)}"

def generate_function_definition(depth: int, from_var: str) -> str:
    func_name = random.choice(functions)
    params = ', '.join(random.sample(variables, random.randint(1, 3)))
    body = generate_complex_expression(depth - 1, from_var)
    return f"def {func_name}({params}):\n{indent(body)}"

def generate_complex_expression(depth: int, from_var: str) -> str:
    if depth == 0:
        return generate_assignment(from_var)

    parts: List[Callable[[], str]] = [
        lambda: generate_assignment(from_var),
        lambda: generate_if_statement(depth, from_var),
        lambda: generate_function_call(depth, from_var),
        lambda: generate_for_loop(depth, from_var),
        lambda: generate_while_loop(depth, from_var),
        lambda: generate_try_except(depth, from_var),
        lambda: generate_function_definition(depth, from_var)
    ]
    random.shuffle(parts)
    return '\n'.join([part() for part in parts[:random.randint(2, 3)]])

def generate_source_to_sink() -> str:
    global current_var_index
    current_var_index = 0  # Reset the index for each new generation
    source = random.choice(sources)
    source_var = get_next_variable()
    intermediate_code = generate_complex_expression(depth=5, from_var=source_var)
    sink = random.choice(sinks)
    sink_var = get_next_variable()
    return f"{source_var} = {source}\n{intermediate_code}\n{sink}({sink_var})  # Sink: {sink_var} flows from source: {source}"

if __name__ == "__main__":
    print(generate_source_to_sink())
