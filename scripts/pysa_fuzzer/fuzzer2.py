import random
import textwrap
import itertools
import string
import math

class CodeGenerator:
    def __init__(self):
        self.variables = self.generate_variable_names()
        self.current_var = 0
    
    def generate_variable_names(self):
        single_letter_names = list(string.ascii_lowercase)
        two_letter_names = [''.join(pair) for pair in itertools.product(string.ascii_lowercase, repeat=2)]
        names = single_letter_names + two_letter_names
        # Remove reserved keywords
        reserved_keywords = {'as', 'in', 'if', 'is', 'or'}
        names = [name for name in names if name not in reserved_keywords]
        return names

    def generate_new_variable(self):
        index = self.current_var
        self.current_var += 1
        return self.variables[index]

    def get_last_variable(self):
        return self.variables[self.current_var - 2]
    
    def generate_source(self) -> str:
        curr_var = self.generate_new_variable()
        return f"import random\nimport math\n{curr_var} = input()"

    def generate_sink(self) -> str: 
        curr_var = self.generate_new_variable()
        prev_var = self.get_last_variable()
        return f"print({prev_var})"

    def generate_addition(self) -> str:
        curr_var = self.generate_new_variable()
        prev_var = self.get_last_variable()
        return f"{curr_var} = {prev_var} + '{random.randint(1, 9)}'"

    def generate_for_loop(self) -> str:
        curr_var = self.generate_new_variable()
        prev_var = self.get_last_variable()
        loop_body = f"{curr_var} += {prev_var}"
        return f"{curr_var} = ''\nfor _ in range({random.randint(2, 5)}):\n{textwrap.indent(loop_body, '    ')}"

    def generate_while_loop(self) -> str:
        curr_var = self.generate_new_variable()
        prev_var = self.get_last_variable()
        loop_body = f"{curr_var} += {prev_var}\ncounter += 1"
        return f"{curr_var} = ''\ncounter = 0\nwhile counter < {random.randint(2, 5)}:\n{textwrap.indent(loop_body, '    ')}"

    def generate_list(self) -> str:
        curr_var = self.generate_new_variable()
        prev_var = self.get_last_variable()
        list_length = random.randint(2, 10)
        list_creation = f"{curr_var}_list = [{prev_var} for _ in range({list_length})]"
        list_access = f"{curr_var} = random.choice({curr_var}_list)"
        return f"{list_creation}\n{list_access}"

    def generate_dictionary(self) -> str:
        curr_var = self.generate_new_variable()
        prev_var = self.get_last_variable()
        dict_length = random.randint(2, 10)
        dict_creation = f"{curr_var}_dict = {{{', '.join(f'{random.randint(1, 100)}: {prev_var}' for _ in range(dict_length))}}}"
        dict_access = f"{curr_var} = random.choice(list({curr_var}_dict.values()))"
        return f"{dict_creation}\n{dict_access}"

    def generate_set(self) -> str:
        curr_var = self.generate_new_variable()
        prev_var = self.get_last_variable()
        set_length = random.randint(2, 10)
        set_creation = f"{curr_var}_set = {{{', '.join(prev_var for _ in range(set_length))}}}"
        set_access = f"{curr_var} = random.choice(list({curr_var}_set))"
        return f"{set_creation}\n{set_access}"

    def generate_string_concatenation(self) -> str:
        curr_var = self.generate_new_variable()
        prev_var = self.get_last_variable()
        concat_operation = f"{curr_var} = {prev_var} + '.'"
        return concat_operation

    def generate_string_slicing(self) -> str:
        curr_var = self.generate_new_variable()
        prev_var = self.get_last_variable()
        start_idx = 0
        slice_operation = f"{curr_var} = {prev_var}[{start_idx}:]"
        return slice_operation

    def generate_string_formatting(self) -> str:
        curr_var = self.generate_new_variable()
        prev_var = self.get_last_variable()
        format_operation = f"{curr_var} = f'string {{{prev_var}}}'"
        return format_operation

    def generate_tuple_manipulation(self) -> str:
        curr_var = self.generate_new_variable()
        prev_var = self.get_last_variable()
        tuple_creation = f"{curr_var} = ({', '.join([prev_var for _ in range(3)])})"
        unpack_vars = ', '.join(self.generate_new_variable() for _ in range(3))
        unpack_operation = f"{unpack_vars} = {curr_var}"
        new_operation_var = self.generate_new_variable()
        new_operation = f"{new_operation_var} = {self.variables[self.current_var-4]} + {self.variables[self.current_var-3]} + {self.variables[self.current_var-2]}"
        return f"{tuple_creation}\n{unpack_operation}\n{new_operation}"

    def generate_randomized_data_structures(self) -> str:
        curr_var = self.generate_new_variable()
        prev_var = self.get_last_variable()
        list_var = self.generate_new_variable()
        list_creation = f"{curr_var} = [{prev_var} for _ in range({random.randint(5, 10)})]"
        shuffle_operation = f"random.shuffle({curr_var})"
        access_operation = f"{list_var} = random.choice({curr_var})"
        return f"{list_creation}\n{shuffle_operation}\n{access_operation}"

    def generate_loop_with_break_continue(self) -> str:
        curr_var = self.generate_new_variable()
        prev_var = self.get_last_variable()

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

    def generate_if_else_elif(self) -> str:
        curr_var = self.generate_new_variable()
        prev_var = self.get_last_variable()

        conditions = [
            (f"if {prev_var} == '{random.randint(1, 10)}':", f"{curr_var} = {prev_var} + ' c1'"),
            (f"elif {prev_var} == '{random.randint(11, 20)}':", f"{curr_var} = {prev_var} + ' c2'"),
            (f"else:", f"{curr_var} = {prev_var} + ' c3'")
        ]
        if_else_elif_statements = "\n".join([f"{condition}\n{textwrap.indent(action, '    ')}" for condition, action in conditions])
        return if_else_elif_statements

    def generate_math_operations(self) -> str:
        curr_var = self.generate_new_variable()
        prev_var = self.get_last_variable()

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

    def generate_nested_loops(self) -> str:
        curr_var = self.generate_new_variable()
        prev_var = self.get_last_variable()
        loop_body = f"{curr_var} += {prev_var}"
        nested_loop = (
            f"{curr_var} = ''\n"
            f"for _ in range({random.randint(2, 5)}):\n"
            f"    for __ in range({random.randint(2, 5)}):\n"
            f"        {textwrap.indent(loop_body, '        ')}"
        )
        return nested_loop

    def generate_try_except(self) -> str:
        curr_var = self.generate_new_variable()
        prev_var = self.get_last_variable()
        try_block = f"{curr_var} = str(int({prev_var}))\n    print('Conversion successful')\n    print({curr_var})"
        except_block = "print('Conversion failed')"
        return f"try:\n{textwrap.indent(try_block, '    ')}\nexcept ValueError:\n{textwrap.indent(except_block, '    ')}"


    def generate_statements(self, x: int) -> str:
        if x < 2:
            raise ValueError("x should be at least 2 to include source and sink functions.")
        
        function_generators = [
            self.generate_addition,
            self.generate_for_loop,
            self.generate_while_loop,
            self.generate_list,
            self.generate_dictionary,
            self.generate_set,
            self.generate_string_concatenation,
            self.generate_string_slicing,
            self.generate_string_formatting,
            self.generate_tuple_manipulation,
            self.generate_loop_with_break_continue,
            self.generate_if_else_elif,
            self.generate_nested_loops,
            self.generate_randomized_data_structures
            #try except and math functions have been ommited for now
        ]

        # Generate source first
        source_code = self.generate_source()
        
        # Select random functions, allowing for duplicates
        selected_functions = random.choices(function_generators, k=x - 2)  # -2 to account for source and sink
        generated_code = "\n".join(func() for func in selected_functions)
        sink_code = self.generate_sink()
        full_code = f"{source_code}\n{generated_code}\n{sink_code}"
        return full_code

generator = CodeGenerator()
x = 35 # Change this number to generate a different amount of functions

print(generator.generate_statements(x))

