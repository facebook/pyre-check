import os
import random
import string
import itertools
from typing import List

import functools 
import random 

class CodeGenerator:
    def __init__(self) -> None:
        self.variables = self.generate_variable_names()
        self.current_var = 0
        self.current_function_number = 0
        self.source_statements = []
        self.last_source = "input()"
        self.sink_statements = []
        self.last_sink = "print"

    def generate_variable_names(self) -> List[str]:
        single_letter_names = list(string.ascii_lowercase)
        two_letter_names = [''.join(pair) for pair in itertools.product(string.ascii_lowercase, repeat=2)]
        names = single_letter_names + two_letter_names
        # Remove reserved keywords
        reserved_keywords = {'as', 'in', 'if', 'is', 'or'}
        names = [name for name in names if name not in reserved_keywords]
        return names

    def generate_new_variable(self) -> str:
        variable_name = self.variables[self.current_var]
        self.current_var += 1
        return variable_name

    def generate_new_function(self) -> str:
        function_name = f"f{self.current_function_number}"
        self.current_function_number += 1
        return function_name

    def generate_sink(self) -> str:
        return f"{self.last_sink}({self.last_source})"

    def generate(self) -> str:
        code_lines = self.source_statements + self.sink_statements
        code_lines.append(self.generate_sink())
        return '\n'.join(code_lines)

    def source_mutation_1(self) -> None: 
        indent_space = ' ' * 4
        current_function = self.generate_new_function()
        self.source_statements.append(f"""
def {current_function}(x):
{indent_space}return x
        """)
        temp = self.last_source
        self.last_source = f"{current_function}({temp})" 

    def source_mutation_2(self) -> None: 
        indent_space = ' ' * 4 
        current_var = self.generate_new_variable()
        self.source_statements.append(f"""
{current_var} = {self.last_source}
        """)
        self.last_source = current_var

    def source_mutation_3(self) -> None: 
        indent_space = ' ' * 4 
        var_1 = self.generate_new_variable() 
        var_2 = self.generate_new_variable() 

        self.source_statements.append(f"""
{var_1} = random.randint(1, 5)
if {var_1} < 5 and {var_1} > 5:
{indent_space}print("hello world")
else: 
{indent_space}{var_2} = {self.last_source}
        """)
        self.last_source = var_2
    
    def source_mutation_4(self) -> None: 
        indent_space = ' ' * 4
        function_1 = self.generate_new_function() 
        function_2 = self.generate_new_function()
        self.source_statements.append(f"""
if False: 
{indent_space}def {function_1}(x): 
{indent_space * 2}return x
else: 
{indent_space}def {function_2}(x): 
{indent_space * 2}return x
        """)
        temp = self.last_source
        self.last_source = f"{function_2}({temp})"
    
    def source_mutation_5(self) -> None: 
        indent_space = ' ' * 4 
        function_1 = self.generate_new_function() 
        self.source_statements.append(f"""
def {function_1}(x):
{indent_space}return x
hashmap = {{"choice_1": {function_1}}}
        """)
        temp = self.last_source
        self.last_source = f'hashmap["choice_1"]({temp})'

    def source_mutation_6(self) -> None:
        indent_space = ' ' * 4
        function_1 = self.generate_new_function()
        function_2 = self.generate_new_function()
        condition_var = self.generate_new_variable()
        self.source_statements.append(f"""
{condition_var} = random.choice([True, False]) and not True
if {condition_var}:
{indent_space}def {function_1}(x):
{indent_space * 2}return x
else:
{indent_space}def {function_2}(x):
{indent_space * 2}return x
    """)
        temp = self.last_source
        self.last_source = f"{function_2}({temp})"
    
    def source_mutation_7(self) -> None:
        indent_space = ' ' * 4
        function_1 = self.generate_new_function()
        function_2 = self.generate_new_function()
        function_3 = self.generate_new_function()
        condition_var_1 = self.generate_new_variable()
        condition_var_2 = self.generate_new_variable()

        self.source_statements.append(f"""
{condition_var_1} = 'a' 
{condition_var_2} = 'b'  
if {condition_var_1} == 'a':
{indent_space}def {function_1}(x):
{indent_space * 2}return x
else:
{indent_space}def {function_2}(x):
{indent_space * 2}return x
if {condition_var_2} == 'b':
{indent_space * 2}def {function_3}(y):
{indent_space * 3}return y
    """)

        temp = self.last_source
        self.last_source = f"{function_3}({function_1}({temp}))"
    
    def source_mutation_8(self) -> None:
        indent_space = ' ' * 4
        class_name = f"Class{self.current_function_number}"
        method_name = self.generate_new_function()
        variable_name = self.generate_new_variable()

        self.source_statements.append(f"""
class {class_name}:
{indent_space}def {method_name}(self, x):
{indent_space * 2}return x

{variable_name} = {class_name}()
    """)

        temp = self.last_source
        self.last_source = f"{variable_name}.{method_name}({temp})"


    def source_mutation_9(self) -> None:
        indent_space = ' ' * 4
        lambda_name = self.generate_new_variable()
        list_var = self.generate_new_variable()
        
        self.source_statements.append(f"""
{lambda_name} = lambda x: x
{list_var} = [{lambda_name}(char) for char in {self.last_source}]
    """)

        temp = self.last_source
        self.last_source = f"''.join({list_var})"


    def source_mutation_10(self) -> None:
        indent_space = ' ' * 4
        decorator_name = self.generate_new_function()
        function_name = self.generate_new_function()
        decorated_function_name = self.generate_new_function()
        
        self.source_statements.append(f"""
def {decorator_name}(func):
{indent_space}def wrapper(x):
{indent_space * 2}return func(x)
{indent_space}return wrapper

@{decorator_name}
def {function_name}(x):
{indent_space}return x

{decorated_function_name} = {function_name}
    """)

        temp = self.last_source
        self.last_source = f"{decorated_function_name}({temp})"


    def source_mutation_11(self) -> None:
        indent_space = ' ' * 4
        outer_function_name = self.generate_new_function()
        inner_function_name = self.generate_new_function()
        closure_var = self.generate_new_variable()
        
        self.source_statements.append(f"""
def {outer_function_name}():
{indent_space}def {inner_function_name}(x):
{indent_space * 2}return x
{indent_space}return {inner_function_name}

{closure_var} = {outer_function_name}()
    """)

        temp = self.last_source
        self.last_source = f"{closure_var}({temp})"

    def source_mutation_12(self) -> None:
        indent_space = ' ' * 4
        function_1 = self.generate_new_function()
        function_2 = self.generate_new_function()
        dispatch_var = self.generate_new_variable()
        key_var = self.generate_new_variable()
        
        self.source_statements.append(f"""
def {function_1}(x):
{indent_space}return x

def {function_2}(x):
{indent_space}return x

{dispatch_var} = {{
{indent_space}'key1': {function_1},
{indent_space}'key2': {function_2}
}}

{key_var} = 'key1' if 1 == 1 else 'key2'
    """)

        temp = self.last_source
        self.last_source = f"{dispatch_var}[{key_var}]({temp})"

    def source_mutation_13(self) -> None:
        indent_space = ' ' * 4
        list_var = self.generate_new_variable()
        slice_var = self.generate_new_variable()
        
        self.source_statements.append(f"""
{list_var} = list({self.last_source})
{slice_var} = {list_var}[::1]
    """)

        temp = self.last_source
        self.last_source = f"''.join({slice_var})"


    def source_mutation_14(self) -> None:
        indent_space = ' ' * 4
        class_name = f"Class{self.current_function_number}"
        method_name = self.generate_new_function()
        
        self.source_statements.append(f"""
class {class_name}:
{indent_space}@staticmethod
{indent_space}def {method_name}(x):
{indent_space * 2}return x

    """)

        temp = self.last_source
        self.last_source = f"{class_name}.{method_name}({temp})"

    def source_mutation_15(self) -> None:
        indent_space = ' ' * 4
        tuple_var = self.generate_new_variable()
        unpack_var_1 = self.generate_new_variable()
        unpack_var_2 = self.generate_new_variable()

        self.source_statements.append(f"""
{tuple_var} = ({self.last_source},)
{unpack_var_1}, = {tuple_var}
{unpack_var_2} = {unpack_var_1}
    """)

        temp = self.last_source
        self.last_source = unpack_var_2


    def source_mutation_16(self) -> None:
        indent_space = ' ' * 4
        format_var = self.generate_new_variable()
        
        self.source_statements.append(f"""
{format_var} = "{{}}".format({self.last_source})
    """)

        temp = self.last_source
        self.last_source = format_var


    def source_mutation_17(self) -> None:
        indent_space = ' ' * 4
        generator_function_name = self.generate_new_function()
        generator_var = self.generate_new_variable()
        
        self.source_statements.append(f"""
def {generator_function_name}(x):
{indent_space}yield x

{generator_var} = next({generator_function_name}({self.last_source}))
    """)

        temp = self.last_source
        self.last_source = generator_var

    def source_mutation_18(self) -> None:
        indent_space = ' ' * 4
        map_var = self.generate_new_variable()
        lambda_func = f"lambda x: x"
        
        self.source_statements.append(f"""
{map_var} = list(map({lambda_func}, {self.last_source}))
    """)

        temp = self.last_source
        self.last_source = f"''.join({map_var})"

    def source_mutation_19(self) -> None:
        indent_space = ' ' * 4
        function_1 = self.generate_new_function()
        function_2 = self.generate_new_function()
        composed_function = self.generate_new_function()
        
        self.source_statements.append(f"""
def {function_1}(x):
{indent_space}return x

def {function_2}(x):
{indent_space}return x

def {composed_function}(x):
{indent_space}return {function_1}({function_2}(x))
    """)

        temp = self.last_source
        self.last_source = f"{composed_function}({temp})"

    def source_mutation_20(self) -> None:
        indent_space = ' ' * 4
        class_name = f"DynamicClass{self.current_function_number}"
        method_name = self.generate_new_function()
        instance_var = self.generate_new_variable()
        
        self.source_statements.append(f"""
{class_name} = type('{class_name}', (object,), {{
{indent_space}'{method_name}': lambda self, x: x
}})

{instance_var} = {class_name}()
    """)

        temp = self.last_source
        self.last_source = f"{instance_var}.{method_name}({temp})"

    def source_mutation_21(self) -> None:
        indent_space = ' ' * 4
        decorator_name = self.generate_new_function()
        function_name = self.generate_new_function()
        decorated_function_name = self.generate_new_function()

        self.source_statements.append(f"""
def {decorator_name}(func):
{indent_space}def wrapper(x):
{indent_space * 2}result = func(x)
{indent_space * 2}# Additional layer of complexity
{indent_space * 2}return result
{indent_space}return wrapper

@{decorator_name}
def {function_name}(x):
{indent_space}return x

{decorated_function_name} = {function_name}
    """)

        temp = self.last_source
        self.last_source = f"{decorated_function_name}({temp})"

    def source_mutation_22(self) -> None:
        indent_space = ' ' * 4
        outer_class_name = f"OuterClass{self.current_function_number}"
        inner_class_name = f"InnerClass{self.current_function_number}"
        method_name = self.generate_new_function()
        instance_var = self.generate_new_variable()

        self.source_statements.append(f"""
class {outer_class_name}:
{indent_space}class {inner_class_name}:
{indent_space * 2}def {method_name}(self, x):
{indent_space * 3}return x

{instance_var} = {outer_class_name}.{inner_class_name}()
    """)

        temp = self.last_source
        self.last_source = f"{instance_var}.{method_name}({temp})"

    def source_mutation_23(self) -> None:
        indent_space = ' ' * 4
        function_name = self.generate_new_function()
        partial_function_name = self.generate_new_function()
        result_var = self.generate_new_variable()

        self.source_statements.append(f"""
def {function_name}(x, y):
{indent_space}return y

{partial_function_name} = functools.partial({function_name}, y={self.last_source})
{result_var} = {partial_function_name}(None)
    """)

        temp = self.last_source
        self.last_source = result_var


    def source_mutation_24(self) -> None:
        indent_space = ' ' * 4
        iterator_class_name = f"IteratorClass{self.current_function_number}"
        instance_var = self.generate_new_variable()
        result_var = self.generate_new_variable()

        self.source_statements.append(f"""
class {iterator_class_name}:
{indent_space}def __init__(self, data):
{indent_space * 2}self.data = data
{indent_space * 2}self.index = 0

{indent_space}def __iter__(self):
{indent_space * 2}return self

{indent_space}def __next__(self):
{indent_space * 2}if self.index < len(self.data):
{indent_space * 3}result = self.data[self.index]
{indent_space * 3}self.index += 1
{indent_space * 3}return result
{indent_space * 2}else:
{indent_space * 3}raise StopIteration

{instance_var} = {iterator_class_name}({self.last_source})
{result_var} = ''.join([char for char in {instance_var}])
    """)

        temp = self.last_source
        self.last_source = result_var
