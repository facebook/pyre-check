import random

# List of source functions simulating user or external input
sources = ['user_input()', 'get_data()', 'fetch_value()']

# List of sink functions that simulate vulnerable data handling points
sinks = ['print', 'send_data', 'log_output']

# List of variable names to use in expressions
variables = ['a', 'b', 'c', 'd', 'e', 'f', 'g']

# List of function names to use in function calls and definitions
functions = ['foo', 'bar', 'baz', 'qux']

# Generate a random arithmetic expression involving basic operations (+, -, *, /)
# The depth parameter controls the complexity of the expression
def generate_arithmetic_expression(depth):
    if depth == 0:
        return str(random.randint(0, 10))
    op = random.choice(['+', '-', '*', '/'])
    return f"({generate_arithmetic_expression(depth - 1)} {op} {generate_arithmetic_expression(depth - 1)})"

# Generate a random variable expression involving basic operations and variable names
# The depth parameter controls the complexity of the expression
def generate_variable_expression(depth):
    if depth == 0:
        return random.choice(variables)
    op = random.choice(['+', '-', '*', '/'])
    return f"({generate_variable_expression(depth - 1)} {op} {generate_variable_expression(depth - 1)})"

# Generate a random assignment statement with a variable and an expression
def generate_assignment():
    var = random.choice(variables)
    expr = generate_variable_expression(2)
    return f"{var} = {expr}"

# Generate a random conditional expression involving comparison operators (==, !=, <, <=, >, >=)
# The depth parameter controls the complexity of the expression
def generate_conditional_expression(depth):
    if depth == 0:
        return str(random.randint(0, 10))
    op = random.choice(['==', '!=', '<', '<=', '>', '>='])
    return f"({generate_conditional_expression(depth - 1)} {op} {generate_conditional_expression(depth - 1)})"

# Generate a random if-else statement with true and false branches
# The true and false branches can be either assignments or nested if statements
def generate_if_statement():
    condition = generate_conditional_expression(1)
    true_branch = generate_assignment()
    false_branch = generate_assignment()
    return f"if {condition}:\n    {indent(true_branch)}\nelse:\n    {indent(false_branch)}"

# Generate a random function call with a random function name and arguments
# The depth parameter controls the complexity of the arguments
def generate_function_call(depth):
    func = random.choice(functions)
    args = ', '.join([generate_arithmetic_expression(1) for _ in range(random.randint(1, 3))])
    return f"{func}({args})"

# Generate a random for loop iterating over a range of numbers
def generate_for_loop():
    var = random.choice(variables)
    start = random.randint(0, 10)
    end = random.randint(start, 20)
    body = generate_assignment()
    return f"for {var} in range({start}, {end}):\n    {indent(body)}"

# Generate a random while loop with a condition and a body
def generate_while_loop():
    condition = generate_conditional_expression(1)
    body = generate_assignment()
    return f"while {condition}:\n    {indent(body)}"

# Generate a random list expression involving basic operations (+, *)
# The depth parameter controls the complexity of the list expression
def generate_list_expression(depth):
    if depth == 0:
        return str([random.randint(0, 10) for _ in range(random.randint(1, 5))])
    op = random.choice(['+', '*'])
    return f"({generate_list_expression(depth - 1)} {op} {generate_list_expression(depth - 1)})"

# Generate a random list indexing expression
def generate_list_indexing():
    list_var = generate_list_expression(1)
    index = random.randint(0, 4)
    return f"{list_var}[{index}]"

# Generate a random list slicing expression
def generate_list_slicing():
    list_var = generate_list_expression(1)
    start = random.randint(0, 2)
    end = random.randint(3, 5)
    return f"{list_var}[{start}:{end}]"

# Generate a random try-except block with a try block and an except block
def generate_try_except():
    try_block = generate_assignment()
    except_block = generate_assignment()
    return f"try:\n    {indent(try_block)}\nexcept Exception as e:\n    {indent(except_block)}"

# Generate a random function definition with a function name, parameters, and a body
def generate_function_definition():
    func_name = random.choice(functions)
    params = ', '.join(random.sample(variables, random.randint(1, 3)))
    body = generate_assignment()
    return f"def {func_name}({params}):\n    {indent(body)}"

# Generate a complex expression consisting of various statements
# This function generates assignments, if statements, function calls, loops, and other expressions
def generate_complex_expression(depth=1):
    parts = [
        generate_assignment(),
        generate_if_statement(),
        generate_function_call(1),
        generate_for_loop(),
        generate_while_loop(),
        generate_list_indexing(),
        generate_list_slicing(),
        generate_try_except(),
        generate_function_definition()
    ]
    random.shuffle(parts)
    return '\n'.join(parts[:depth * 2])  # Control the number of parts to include based on depth

# Helper function to indent lines of code
def indent(text, spaces=4):
    indentation = ' ' * spaces
    return '\n'.join(indentation + line if line else line for line in text.split('\n'))

# Generate the complete source-to-sink code with source, sink, and intermediate expressions
# This function integrates the sources and sinks with the generated complex expressions
def generate_source_to_sink():
    source = random.choice(sources)
    sink = random.choice(sinks)
    intermediate_code = generate_complex_expression(depth=3)
    return f"{intermediate_code}\n{sink}({source})"

# Example output
if __name__ == "__main__":
    print(generate_source_to_sink())
