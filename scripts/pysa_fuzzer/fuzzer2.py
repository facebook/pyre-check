import random
import textwrap

def generate_variable_names():
    names = []
    for i in range(1, 3):  
        for first in range(ord('a'), ord('z') + 1):
            if i == 1:
                names.append(chr(first))
            else:
                for second in range(ord('a'), ord('z') + 1):
                    if chr(first) + chr(second) != 'as':
                        names.append(chr(first) + chr(second))
    return names

variables = generate_variable_names()
current_var = 0

# Method for getting the next variable in the sequence
def get_next_idx():
    global current_var
    temp = current_var
    current_var += 1
    return temp

def generate_source() -> str:
    index = get_next_idx()
    curr_var = variables[index]
    return f"import random\n{curr_var} = input()"

def generate_sink() -> str: 
    index = get_next_idx()
    prev_var = variables[index - 1]
    return f"print({prev_var})"

def generate_addition() -> str:
    index = get_next_idx()
    curr_var = variables[index]
    prev_var = variables[index - 1]
    return f"{curr_var} = {prev_var} + '{random.randint(1, 9)}'"

def generate_for_loop() -> str:
    index = get_next_idx()
    curr_var = variables[index]
    prev_var = variables[index - 1]
    loop_body = f"{curr_var} += {prev_var}"
    return f"{curr_var} = ''\nfor _ in range({random.randint(2, 5)}):\n{textwrap.indent(loop_body, '    ')}"

def generate_while_loop() -> str:
    index = get_next_idx()
    curr_var = variables[index]
    prev_var = variables[index - 1]
    loop_body = f"{curr_var} += {prev_var}\ncounter += 1"
    return f"{curr_var} = ''\ncounter = 0\nwhile counter < {random.randint(2, 5)}:\n{textwrap.indent(loop_body, '    ')}"

def generate_list() -> str:
    index = get_next_idx()
    curr_var = variables[index]
    prev_var = variables[index - 1]
    list_length = random.randint(2, 10)
    list_creation = f"{curr_var}_list = [{prev_var} for _ in range({list_length})]"
    list_access = f"{curr_var} = random.choice({curr_var}_list)"
    return f"{list_creation}\n{list_access}"


def generate_dictionary() -> str:
    index = get_next_idx()
    curr_var = variables[index]
    prev_var = variables[index - 1]
    dict_length = random.randint(2, 10)
    dict_creation = f"{curr_var}_dict = {{{', '.join(f'{random.randint(1, 100)}: {prev_var}' for _ in range(dict_length))}}}"
    dict_access = f"{curr_var} = random.choice(list({curr_var}_dict.values()))"
    return f"{dict_creation}\n{dict_access}"


def generate_set() -> str:
    index = get_next_idx()
    curr_var = variables[index]
    prev_var = variables[index - 1]
    set_length = random.randint(2, 10)
    set_creation = f"{curr_var}_set = {{{', '.join(prev_var for _ in range(set_length))}}}"
    set_access = f"{curr_var} = random.choice(list({curr_var}_set))"
    return f"{set_creation}\n{set_access}"


def generate_string_concatenation() -> str:
    index = get_next_idx()
    curr_var = variables[index]
    prev_var = variables[index - 1]
    concat_operation = f"{curr_var} = {prev_var} + ' concatenated'"
    return concat_operation

def generate_string_slicing() -> str:
    index = get_next_idx()
    curr_var = variables[index]
    prev_var = variables[index - 1]
    start_idx = random.randint(0, 3)
    end_idx = random.randint(4, 7)
    slice_operation = f"{curr_var} = {prev_var}[{start_idx}:{end_idx}]"
    return slice_operation

def generate_string_formatting() -> str:
    index = get_next_idx()
    curr_var = variables[index]
    prev_var = variables[index - 1]
    format_operation = f"{curr_var} = f'Formatted string with {{{prev_var}}}'"
    return format_operation

def generate_tuple_manipulation() -> str:
    index = get_next_idx()
    curr_var = variables[index]
    prev_var = variables[index - 1]
    tuple_creation = f"{curr_var} = ({', '.join([prev_var for _ in range(3)])})"
    unpack_vars = ', '.join(variables[get_next_idx()] for _ in range(3))
    unpack_operation = f"{unpack_vars} = {curr_var}"
    new_operation_var = variables[get_next_idx()]
    new_operation = f"{new_operation_var} = {variables[index+1]} + {variables[index+2]} + {variables[index+3]}"
    return f"{tuple_creation}\n{unpack_operation}\n{new_operation}"

def generate_randomized_data_structures() -> str:
    index = get_next_idx()
    curr_var = variables[index]
    prev_var = variables[index - 1]
    list_var = variables[get_next_idx()]
    list_creation = f"{list_var} = [{prev_var} for _ in range({random.randint(5, 10)})]"
    shuffle_operation = f"random.shuffle({list_var})"
    access_operation = f"{curr_var} = random.choice({list_var})"
    return f"{list_creation}\n{shuffle_operation}\n{access_operation}"


def generate_loop_with_break_continue() -> str:
    index = get_next_idx()
    curr_var = variables[index]
    prev_var = variables[index - 1]

    loop_type = random.choice(["break", "continue"])
    condition = random.randint(1, 5)

    loop_body = (
        f"if _ == {condition}:\n"
        f"        {loop_type}\n"
        f"    {curr_var} += {prev_var}"
    )

    return (
        f"{curr_var} = ''\n"
        f"for _ in range({random.randint(5, 10)}):\n"
        f"    {textwrap.indent(loop_body, '    ')}"
    )

def generate_if_else_elif() -> str:
    index = get_next_idx()
    curr_var = variables[index]
    prev_var = variables[index - 1]

    conditions = [
        (f"if {prev_var} == '{random.randint(1, 10)}':", f"{curr_var} = {prev_var} + ' condition1'"),
        (f"elif {prev_var} == '{random.randint(11, 20)}':", f"{curr_var} = {prev_var} + ' condition2'"),
        (f"else:", f"{curr_var} = {prev_var} + ' condition3'")
    ]
    if_else_elif_statements = "\n".join([f"{condition}\n{textwrap.indent(action, '    ')}" for condition, action in conditions])
    return if_else_elif_statements


import math

def generate_math_operations() -> str:
    index = get_next_idx()
    curr_var = variables[index]
    prev_var = variables[index - 1]

    # Choose a random mathematical operation from the math library
    operations = [
        ("sqrt", f"{curr_var} = str(math.sqrt(int({prev_var})))"),
        ("log", f"{curr_var} = str(math.log(int({prev_var}) + 1))"),  # log(x) requires x > 0
        ("sin", f"{curr_var} = str(math.sin(float({prev_var})))"),
        ("cos", f"{curr_var} = str(math.cos(float({prev_var})))"),
        ("tan", f"{curr_var} = str(math.tan(float({prev_var})))")
    ]

    # Select a random operation
    operation = random.choice(operations)
    
    # Import math if not already imported
    import_math = "import math"
    
    return f"{import_math}\n{operation[1]}"


def generate_nested_loops() -> str:
    index = get_next_idx()
    prev_var = variables[index - 1]
    curr_var = variables[index]
    loop_body = f"{curr_var} += {prev_var}"
    nested_loop = (
        f"{curr_var} = ''\n"
        f"for _ in range({random.randint(2, 5)}):\n"
        f"    for __ in range({random.randint(2, 5)}):\n"
        f"        {textwrap.indent(loop_body, '        ')}"
    )
    return nested_loop

def generate_try_except() -> str:
    index = get_next_idx()
    curr_var = variables[index]
    prev_var = variables[index - 1]
    try_block = f"{curr_var} = str(int({prev_var}))\n    print('Conversion successful')"
    except_block = "print('Conversion failed')"
    return f"try:\n{textwrap.indent(try_block, '    ')}\nexcept ValueError:\n{textwrap.indent(except_block, '    ')}"


function_generators = [
    generate_addition,
    generate_for_loop,
    generate_while_loop,
    generate_list,
    generate_dictionary,
    generate_set,
    generate_string_concatenation,
    generate_string_slicing,
    generate_string_formatting,
    generate_tuple_manipulation,
    generate_loop_with_break_continue,
    generate_if_else_elif,
    generate_nested_loops,

    #generate_try_except
    #generate_math_operations
    #generate_randomized_data_structures

]

def generate_random_functions(x: int) -> str:
    if x < 2:
        raise ValueError("x should be at least 2 to include source and sink functions.")
    
    # Generate source first
    source_code = generate_source()
    
    # Select random functions, allowing for duplicates
    selected_functions = random.choices(function_generators, k=x - 2)  # -2 to account for source and sink
    generated_code = "\n".join(func() for func in selected_functions)
    
    # Generate sink last
    sink_code = generate_sink()
    
    # Concatenate source, random functions, and sink
    full_code = f"{source_code}\n{generated_code}\n{sink_code}"
    return full_code

# Test the function with x amount of random functions
x = 35  # Change this number to generate a different amount of functions
print(generate_random_functions(x))
