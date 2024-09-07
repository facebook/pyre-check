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



    def sink_mutation_1(self) -> None: 
        indent_space = ' ' * 4
        function_1 = self.generate_new_function()
        self.sink_statements.append(f"""
def {function_1}(x):
{indent_space}{self.last_sink}(x)
        """)
        self.last_sink = function_1
    
    def sink_mutation_2(self) -> None: 
        indent_space = ' ' * 4 
        function_1 = self.generate_new_function()
        function_2 = self.generate_new_function()   
        self.sink_statements.append(f"""
if False:
{indent_space}def {function_1}(x):
{indent_space * 2}{self.last_sink}(x)
else:
{indent_space}def {function_2}(x):
{indent_space * 2}{self.last_sink}(x)
        """)    
        self.last_sink = function_2

    def sink_mutation_3(self) -> None:
        indent_space = ' ' * 4
        function_1 = self.generate_new_function()
        var_1 = self.generate_new_variable()
        var_2 = self.generate_new_variable()

        self.sink_statements.append(f"""
def {function_1}(x):
{indent_space}{var_1} = random.randint(1, 5)
{indent_space}if {var_1} < 5:
{indent_space * 2}{self.last_sink}(x)
{indent_space}else:
{indent_space * 2}{var_2} = x
{indent_space * 2}{self.last_sink}({var_2})
    """)
        self.last_sink = function_1
    
    def sink_mutation_4(self) -> None:
        indent_space = ' ' * 4
        function_1 = self.generate_new_function()
        decorator_1 = self.generate_new_function()
        
        self.sink_statements.append(f"""
def {decorator_1}(func):
{indent_space}def wrapper(x):
{indent_space * 2}{self.last_sink}(x)
{indent_space * 2}return func(x)
{indent_space}return wrapper

@{decorator_1}
def {function_1}(x):
{indent_space}return x
    """)
        self.last_sink = function_1
    
    def sink_mutation_5(self) -> None:
        indent_space = ' ' * 4
        function_1 = self.generate_new_function()
        class_name = f"Class{self.current_function_number}"
        instance_var = self.generate_new_variable()

        self.sink_statements.append(f"""
class {class_name}:
{indent_space}def __init__(self, sink_function):
{indent_space * 2}self.sink_function = sink_function

{indent_space}def {function_1}(self, x):
{indent_space * 2}self.sink_function(x)

{instance_var} = {class_name}({self.last_sink})
    """)
        self.last_sink = f"{instance_var}.{function_1}"
    
    def sink_mutation_6(self) -> None:
        indent_space = ' ' * 4
        function_1 = self.generate_new_function()
        list_var = self.generate_new_variable()
        map_var = self.generate_new_variable()

        self.sink_statements.append(f"""
def {function_1}(x):
{indent_space}{list_var} = list(str(x))
{indent_space}{map_var} = map(lambda char: ({self.last_sink}(char), char)[1], {list_var})
{indent_space}return ''.join({map_var})
    """)
        self.last_sink = function_1


    def sink_mutation_7(self) -> None:
        indent_space = ' ' * 4
        function_1 = self.generate_new_function()
        tuple_var = self.generate_new_variable()

        self.sink_statements.append(f"""
def {function_1}(x):
{indent_space}{tuple_var} = ({self.last_sink}(x), x)
{indent_space}return {tuple_var}[1]
    """)
        self.last_sink = function_1

    def sink_mutation_8(self) -> None:
        indent_space = ' ' * 4
        function_1 = self.generate_new_function()
        temp_var = self.generate_new_variable()

        self.sink_statements.append(f"""
def {function_1}(x):
{indent_space}{temp_var} = {self.last_sink}(x)
{indent_space}with open('sink_output.txt', 'w') as file:
{indent_space * 2}file.write(str({temp_var}))
{indent_space}return x
    """)
        self.last_sink = function_1
    
    def sink_mutation_9(self) -> None:
        indent_space = ' ' * 4
        function_1 = self.generate_new_function()
        temp_var = self.generate_new_variable()
        result_var = self.generate_new_variable()

        self.sink_statements.append(f"""
def {function_1}(x):
{indent_space}{temp_var} = {self.last_sink}(x)
{indent_space}return x
    """)
        self.last_sink = function_1

    def sink_mutation_10(self) -> None:
        indent_space = ' ' * 4
        function_1 = self.generate_new_function()
        list_var = self.generate_new_variable()

        self.sink_statements.append(f"""
def {function_1}(x):
{indent_space}{list_var} = [str({self.last_sink}(item)) for item in str(x)]
{indent_space}result = ''.join({list_var})
{indent_space}return result
    """)
        self.last_sink = function_1
    
    def sink_mutation_11(self) -> None:
        indent_space = ' ' * 4
        function_1 = self.generate_new_function()
        temp_var = self.generate_new_variable()

        self.sink_statements.append(f"""
def {function_1}(x):
{indent_space}{temp_var} = str({self.last_sink}(x))
{indent_space}return f"{{len({temp_var})}}: {{x}}"
    """)
        self.last_sink = function_1
    
    def sink_mutation_12(self) -> None:
        indent_space = ' ' * 4
        function_1 = self.generate_new_function()
        set_var = self.generate_new_variable()

        self.sink_statements.append(f"""
def {function_1}(x):
{indent_space}{set_var} = set(str({self.last_sink}(x)))
{indent_space}return ''.join(sorted({set_var}))
    """)
        self.last_sink = function_1
    
    def sink_mutation_13(self) -> None:
        indent_space = ' ' * 4
        function_1 = self.generate_new_function()
        temp_var = self.generate_new_variable()

        self.sink_statements.append(f"""
def {function_1}(x):
{indent_space}{temp_var} = [char.upper() if char.islower() else char.lower() for char in str({self.last_sink}(x))]
{indent_space}return ''.join({temp_var})
    """)
        self.last_sink = function_1

    def sink_mutation_14(self) -> None:
        indent_space = ' ' * 4
        function_1 = self.generate_new_function()
        list_var = self.generate_new_variable()

        self.sink_statements.append(f"""
def {function_1}(x):
{indent_space}{list_var} = [ord(char) for char in str({self.last_sink}(x))]
{indent_space}return sum({list_var})
    """)
        self.last_sink = function_1

    def sink_mutation_15(self) -> None:
        indent_space = ' ' * 4
        function_1 = self.generate_new_function()
        reversed_var = self.generate_new_variable()

        self.sink_statements.append(f"""
def {function_1}(x):
{indent_space}{reversed_var} = str({self.last_sink}(x))[::-1]
{indent_space}return {reversed_var}
    """)
        self.last_sink = function_1
    
    def sink_mutation_16(self) -> None:
        indent_space = ' ' * 4
        function_1 = self.generate_new_function()
        split_var = self.generate_new_variable()
        joined_var = self.generate_new_variable()

        self.sink_statements.append(f"""
def {function_1}(x):
{indent_space}{split_var} = str({self.last_sink}(x)).split()
{indent_space}{joined_var} = '_'.join({split_var})
{indent_space}return {joined_var}
    """)
        self.last_sink = function_1
    
    def sink_mutation_17(self) -> None:
        indent_space = ' ' * 4
        function_1 = self.generate_new_function()
        zip_var = self.generate_new_variable()
        temp_var_1 = self.generate_new_variable()
        temp_var_2 = self.generate_new_variable()

        self.sink_statements.append(f"""
def {function_1}(x):
{indent_space}{temp_var_1} = str({self.last_sink}(x))
{indent_space}{temp_var_2} = str(x)
{indent_space}{zip_var} = ''.join(a + b for a, b in zip({temp_var_1}, {temp_var_2}))
{indent_space}return {zip_var}
    """)
        self.last_sink = function_1

    def sink_mutation_18(self) -> None:
        indent_space = ' ' * 4
        function_1 = self.generate_new_function()
        temp_var = self.generate_new_variable()

        self.sink_statements.append(f"""
def {function_1}(x):
{indent_space}{temp_var} = str({self.last_sink}(x))
{indent_space}return '-'.join({temp_var}[i:i+2] for i in range(0, len({temp_var}), 2))
    """)
        self.last_sink = function_1

    def sink_mutation_19(self) -> None:
        indent_space = ' ' * 4
        function_1 = self.generate_new_function()
        temp_var = self.generate_new_variable()
        index_var = self.generate_new_variable()

        self.sink_statements.append(f"""
def {function_1}(x):
{indent_space}{temp_var} = str({self.last_sink}(x))
{indent_space}{index_var} = len({temp_var}) // 2
{indent_space}return {temp_var}[:{index_var}] + '-' + {temp_var}[{index_var}:]
    """)
        self.last_sink = function_1

    def sink_mutation_20(self) -> None:
        indent_space = ' ' * 4
        function_1 = self.generate_new_function()
        encoded_var = self.generate_new_variable()

        self.sink_statements.append(f"""
def {function_1}(x):
{indent_space}{encoded_var} = str({self.last_sink}(x)).encode('utf-8').hex()
{indent_space}return {encoded_var}
    """)
        self.last_sink = function_1

    def sink_mutation_21(self) -> None:
        indent_space = ' ' * 4
        function_1 = self.generate_new_function()
        temp_var = self.generate_new_variable()

        self.sink_statements.append(f"""
def {function_1}(x):
{indent_space}{temp_var} = ''.join(reversed(str({self.last_sink}(x))))
{indent_space}return ''.join(c if i % 2 == 0 else '*' for i, c in enumerate({temp_var}))
    """)
        self.last_sink = function_1
    
    def sink_mutation_22(self) -> None:
        indent_space = ' ' * 4
        function_1 = self.generate_new_function()
        temp_var = self.generate_new_variable()
        shuffled_var = self.generate_new_variable()

        self.sink_statements.append(f"""
def {function_1}(x):
{indent_space}import random
{indent_space}{temp_var} = list(str({self.last_sink}(x)))
{indent_space}random.shuffle({temp_var})
{indent_space}{shuffled_var} = ''.join({temp_var})
{indent_space}return {shuffled_var}
    """)
        self.last_sink = function_1
    
    def sink_mutation_23(self) -> None:
        indent_space = ' ' * 4
        function_1 = self.generate_new_function()
        split_var = self.generate_new_variable()
        joined_var = self.generate_new_variable()

        self.sink_statements.append(f"""
def {function_1}(x):
{indent_space}{split_var} = [str({self.last_sink}(x))[i:i+3] for i in range(0, len(str({self.last_sink}(x))), 3)]
{indent_space}{joined_var} = '-'.join({split_var})
{indent_space}return {joined_var}
    """)
        self.last_sink = function_1

    def sink_mutation_24(self) -> None:
        indent_space = ' ' * 4
        function_1 = self.generate_new_function()
        temp_var = self.generate_new_variable()
        condition_var = self.generate_new_variable()

        self.sink_statements.append(f"""
def {function_1}(x):
{indent_space}{temp_var} = str({self.last_sink}(x))
{indent_space}{condition_var} = ''.join(c.upper() if i % 2 == 0 else c.lower() for i, c in enumerate({temp_var}))
{indent_space}return {condition_var}
    """)
        self.last_sink = function_1
    
    def source_mutation_25(self) -> None:
        indent_space = ' ' * 4
        shuffle_function_name = self.generate_new_function()
        shuffled_var = self.generate_new_variable()

        self.source_statements.append(f"""
def {shuffle_function_name}(x):
{indent_space}import random
{indent_space}x_list = list(x)
{indent_space}random.shuffle(x_list)
{indent_space}return ''.join(x_list)

{shuffled_var} = {shuffle_function_name}({self.last_source})
    """)

        self.last_source = shuffled_var

    def source_mutation_26(self) -> None:
        indent_space = ' ' * 4
        transform_function_name = self.generate_new_function()
        reverse_function_name = self.generate_new_function()
        transformed_var = self.generate_new_variable()

        self.source_statements.append(f"""
def {transform_function_name}(x):
{indent_space}
{indent_space}return x[::-1]

def {reverse_function_name}(x):
{indent_space}
{indent_space}return x[::-1]

{transformed_var} = {reverse_function_name}({transform_function_name}({self.last_source}))
    """)

        self.last_source = transformed_var
    def source_mutation_27(self) -> None:
        indent_space = ' ' * 4
        compress_function_name = self.generate_new_function()
        decompress_function_name = self.generate_new_function()
        compressed_var = self.generate_new_variable()

        self.source_statements.append(f"""
def {compress_function_name}(x):
{indent_space}# Compression example: convert to hex
{indent_space}return ''.join(format(ord(c), 'x') for c in x)

def {decompress_function_name}(x):
{indent_space}# Decompression: convert hex back to string
{indent_space}return ''.join(chr(int(x[i:i+2], 16)) for i in range(0, len(x), 2))

{compressed_var} = {decompress_function_name}({compress_function_name}({self.last_source}))
    """)

        self.last_source = compressed_var

    def source_mutation_28(self) -> None:
        indent_space = ' ' * 4
        encrypt_function_name = self.generate_new_function()
        decrypt_function_name = self.generate_new_function()
        encrypted_var = self.generate_new_variable()

        self.source_statements.append(f"""
def {encrypt_function_name}(x):
{indent_space}# Simple encryption: shift characters by 2
{indent_space}return ''.join(chr(ord(c) + 2) for c in x)

def {decrypt_function_name}(x):
{indent_space}# Decryption: shift characters back by 2
{indent_space}return ''.join(chr(ord(c) - 2) for c in x)

{encrypted_var} = {decrypt_function_name}({encrypt_function_name}({self.last_source}))
    """)

        self.last_source = encrypted_var

    def source_mutation_29(self) -> None:
        indent_space = ' ' * 4
        encode_function_name = self.generate_new_function()
        decode_function_name = self.generate_new_function()
        encoded_var = self.generate_new_variable()

        self.source_statements.append(f"""
import base64

def {encode_function_name}(x):
{indent_space}# Encode the string using Base64
{indent_space}return base64.b64encode(x.encode('utf-8')).decode('utf-8')

def {decode_function_name}(x):
{indent_space}# Decode the string from Base64
{indent_space}return base64.b64decode(x.encode('utf-8')).decode('utf-8')

{encoded_var} = {decode_function_name}({encode_function_name}({self.last_source}))
    """)

        self.last_source = encoded_var
    
    def source_mutation_30(self) -> None:
        indent_space = ' ' * 4
        encode_function_name = self.generate_new_function()
        decode_function_name = self.generate_new_function()
        transformed_var = self.generate_new_variable()

        self.source_statements.append(f"""
def {encode_function_name}(x):
{indent_space}# Substitute each letter with its opposite
{indent_space}def substitute(c):
{indent_space * 2}if c.isalpha():
{indent_space * 3}offset = ord('a') if c.islower() else ord('A')
{indent_space * 3}return chr(offset + 25 - (ord(c) - offset))
{indent_space * 2}return c
{indent_space}return ''.join(substitute(c) for c in x)

def {decode_function_name}(x):
{indent_space}# Reverse substitution using the same logic
{indent_space}return {encode_function_name}(x)

{transformed_var} = {decode_function_name}({encode_function_name}({self.last_source}))
    """)

        self.last_source = transformed_var

    def source_mutation_31(self) -> None:
        indent_space = ' ' * 4
        rot13_function_name = self.generate_new_function()
        transformed_var = self.generate_new_variable()

        self.source_statements.append(f"""
def {rot13_function_name}(x):
{indent_space}# Apply ROT13 transformation
{indent_space}def rotate(c):
{indent_space * 2}if c.isalpha():
{indent_space * 3}offset = ord('a') if c.islower() else ord('A')
{indent_space * 3}return chr(offset + (ord(c) - offset + 13) % 26)
{indent_space * 2}return c
{indent_space}return ''.join(rotate(c) for c in x)

{transformed_var} = {rot13_function_name}({rot13_function_name}({self.last_source}))
    """)

        self.last_source = transformed_var\

    def source_mutation_32(self) -> None:
        indent_space = ' ' * 4
        swap_function_name = self.generate_new_function()
        unswap_function_name = self.generate_new_function()
        swapped_var = self.generate_new_variable()

        self.source_statements.append(f"""
def {swap_function_name}(x):
{indent_space}# Swap adjacent characters
{indent_space}result = list(x)
{indent_space}for i in range(0, len(result) - 1, 2):
{indent_space * 2}result[i], result[i+1] = result[i+1], result[i]
{indent_space}return ''.join(result)

def {unswap_function_name}(x):
{indent_space}# Reverse swap using the same logic
{indent_space}return {swap_function_name}(x)

{swapped_var} = {unswap_function_name}({swap_function_name}({self.last_source}))
    """)

        self.last_source = swapped_var

    def source_mutation_33(self) -> None:
        indent_space = ' ' * 4
        encrypt_function_name = self.generate_new_function()
        decrypt_function_name = self.generate_new_function()
        encrypted_var = self.generate_new_variable()
        key_var = self.generate_new_variable()

        self.source_statements.append(f"""
{key_var} = [3, 1, 4, 2, 0]  # A simple transposition key

def {encrypt_function_name}(x, key):
{indent_space}# Transposition cipher encryption
{indent_space}n = len(x)
{indent_space}key = key[:n]  # Adjust key length to match input
{indent_space}sorted_key = sorted(range(n), key=lambda i: key[i])
{indent_space}return ''.join(x[i] for i in sorted_key)

def {decrypt_function_name}(x, key):
{indent_space}# Transposition cipher decryption
{indent_space}n = len(x)
{indent_space}key = key[:n]
{indent_space}inverse_key = sorted(range(n), key=lambda i: key[i])
{indent_space}return ''.join(x[inverse_key.index(i)] for i in range(n))

{encrypted_var} = {decrypt_function_name}({encrypt_function_name}({self.last_source}, {key_var}), {key_var})
    """)

        self.last_source = encrypted_var
    def source_mutation_34(self) -> None:
        indent_space = ' ' * 4
        vigenere_encode_function = self.generate_new_function()
        vigenere_decode_function = self.generate_new_function()
        encoded_var = self.generate_new_variable()
        keyword_var = self.generate_new_variable()

        self.source_statements.append(f"""
{keyword_var} = 'secret'  # Vigenère cipher keyword

def {vigenere_encode_function}(x, keyword):
{indent_space}# Vigenère cipher encoding
{indent_space}encoded_chars = []
{indent_space}keyword_repeated = (keyword * ((len(x) // len(keyword)) + 1))[:len(x)]
{indent_space}for i, c in enumerate(x):
{indent_space * 2}if c.isalpha():
{indent_space * 3}offset = ord('a') if c.islower() else ord('A')
{indent_space * 3}keyword_offset = ord(keyword_repeated[i].lower()) - ord('a')
{indent_space * 3}encoded_char = chr((ord(c) - offset + keyword_offset) % 26 + offset)
{indent_space * 2}else:
{indent_space * 3}encoded_char = c
{indent_space * 2}encoded_chars.append(encoded_char)
{indent_space}return ''.join(encoded_chars)

def {vigenere_decode_function}(x, keyword):
{indent_space}# Vigenère cipher decoding
{indent_space}decoded_chars = []
{indent_space}keyword_repeated = (keyword * ((len(x) // len(keyword)) + 1))[:len(x)]
{indent_space}for i, c in enumerate(x):
{indent_space * 2}if c.isalpha():
{indent_space * 3}offset = ord('a') if c.islower() else ord('A')
{indent_space * 3}keyword_offset = ord(keyword_repeated[i].lower()) - ord('a')
{indent_space * 3}decoded_char = chr((ord(c) - offset - keyword_offset + 26) % 26 + offset)
{indent_space * 2}else:
{indent_space * 3}decoded_char = c
{indent_space * 2}decoded_chars.append(decoded_char)
{indent_space}return ''.join(decoded_chars)

{encoded_var} = {vigenere_decode_function}({vigenere_encode_function}({self.last_source}, {keyword_var}), {keyword_var})
    """)

        self.last_source = encoded_var


    def source_mutation_35(self) -> None:
        indent_space = ' ' * 4
        xor_encrypt_function = self.generate_new_function()
        xor_decrypt_function = self.generate_new_function()
        encrypted_var = self.generate_new_variable()
        xor_key_var = self.generate_new_variable()

        self.source_statements.append(f"""
{xor_key_var} = 42  # XOR cipher key

def {xor_encrypt_function}(x, key):
{indent_space}# XOR cipher encryption
{indent_space}return ''.join(chr(ord(c) ^ key) for c in x)

def {xor_decrypt_function}(x, key):
{indent_space}# XOR cipher decryption (same operation)
{indent_space}return ''.join(chr(ord(c) ^ key) for c in x)

{encrypted_var} = {xor_decrypt_function}({xor_encrypt_function}({self.last_source}, {xor_key_var}), {xor_key_var})
    """)

        self.last_source = encrypted_var
    
    def source_mutation_36(self) -> None:
        indent_space = ' ' * 4
        scramble_function = self.generate_new_function()
        unscramble_function = self.generate_new_function()
        scrambled_var = self.generate_new_variable()

        self.source_statements.append(f"""
def {scramble_function}(x):
{indent_space}# Split string into two halves and interleave them
{indent_space}mid = len(x) // 2
{indent_space}first_half = x[:mid]
{indent_space}second_half = x[mid:]
{indent_space}scrambled = ''.join(second_half[i:i+1] + first_half[i:i+1] for i in range(mid))
{indent_space}if len(x) % 2 == 1:
{indent_space * 2}scrambled += second_half[-1]
{indent_space}return scrambled

def {unscramble_function}(x):
{indent_space}# Unscramble by reversing the interleave
{indent_space}mid = len(x) // 2
{indent_space}first_half = ''.join(x[i] for i in range(1, len(x), 2))
{indent_space}second_half = ''.join(x[i] for i in range(0, len(x), 2))
{indent_space}return second_half[:mid] + first_half

{scrambled_var} = {unscramble_function}({scramble_function}({self.last_source}))
    """)

        self.last_source = scrambled_var
    def source_mutation_37(self) -> None:
        indent_space = ' ' * 4
        rail_fence_encrypt_function = self.generate_new_function()
        rail_fence_decrypt_function = self.generate_new_function()
        encrypted_var = self.generate_new_variable()
        num_rails = 3  # Number of rails for the cipher

        self.source_statements.append(f"""
def {rail_fence_encrypt_function}(x, rails):
{indent_space}# Rail Fence cipher encryption
{indent_space}fence = [[] for _ in range(rails)]
{indent_space}rail = 0
{indent_space}direction = 1
{indent_space}for char in x:
{indent_space * 2}fence[rail].append(char)
{indent_space * 2}rail += direction
{indent_space * 2}if rail == 0 or rail == rails - 1:
{indent_space * 3}direction *= -1
{indent_space}return ''.join(''.join(row) for row in fence)

def {rail_fence_decrypt_function}(x, rails):
{indent_space}# Rail Fence cipher decryption
{indent_space}rail_lengths = [0] * rails
{indent_space}rail = 0
{indent_space}direction = 1
{indent_space}for _ in x:
{indent_space * 2}rail_lengths[rail] += 1
{indent_space * 2}rail += direction
{indent_space * 2}if rail == 0 or rail == rails - 1:
{indent_space * 3}direction *= -1
{indent_space}rail_chars = [''] * rails
{indent_space}i = 0
{indent_space}for r in range(rails):
{indent_space * 2}rail_chars[r] = x[i:i + rail_lengths[r]]
{indent_space * 2}i += rail_lengths[r]
{indent_space}result = []
{indent_space}rail = 0
{indent_space}direction = 1
{indent_space}for _ in x:
{indent_space * 2}result.append(rail_chars[rail][0])
{indent_space * 2}rail_chars[rail] = rail_chars[rail][1:]
{indent_space * 2}rail += direction
{indent_space * 2}if rail == 0 or rail == rails - 1:
{indent_space * 3}direction *= -1
{indent_space}return ''.join(result)

{encrypted_var} = {rail_fence_decrypt_function}({rail_fence_encrypt_function}({self.last_source}, {num_rails}), {num_rails})
    """)

        self.last_source = encrypted_var
    
    def source_mutation_38(self) -> None:
        indent_space = ' ' * 4
        shifting_caesar_encrypt_function = self.generate_new_function()
        shifting_caesar_decrypt_function = self.generate_new_function()
        encrypted_var = self.generate_new_variable()

        self.source_statements.append(f"""
def {shifting_caesar_encrypt_function}(x):
{indent_space}# Shifting Caesar cipher encryption
{indent_space}return ''.join(
{indent_space * 2}chr((ord(c) - (ord('a') if c.islower() else ord('A')) + (i % 26)) % 26 + (ord('a') if c.islower() else ord('A')))
{indent_space * 2}if c.isalpha() else c
{indent_space * 2}for i, c in enumerate(x)
{indent_space})

def {shifting_caesar_decrypt_function}(x):
{indent_space}# Shifting Caesar cipher decryption
{indent_space}return ''.join(
{indent_space * 2}chr((ord(c) - (ord('a') if c.islower() else ord('A')) - (i % 26) + 26) % 26 + (ord('a') if c.islower() else ord('A')))
{indent_space * 2}if c.isalpha() else c
{indent_space * 2}for i, c in enumerate(x)
{indent_space})

{encrypted_var} = {shifting_caesar_decrypt_function}({shifting_caesar_encrypt_function}({self.last_source}))
    """)

        self.last_source = encrypted_var

    def source_mutation_39(self) -> None:
        indent_space = ' ' * 4
        affine_encrypt_function = self.generate_new_function()
        affine_decrypt_function = self.generate_new_function()
        encrypted_var = self.generate_new_variable()
        a = 5  # Multiplier for affine cipher (must be coprime with 26)
        b = 8  # Increment for affine cipher

        self.source_statements.append(f"""
def {affine_encrypt_function}(x):
{indent_space}# Affine cipher encryption
{indent_space}def encrypt_char(c):
{indent_space * 2}if c.isalpha():
{indent_space * 3}offset = ord('a') if c.islower() else ord('A')
{indent_space * 3}return chr(((ord(c) - offset) * {a} + {b}) % 26 + offset)
{indent_space * 2}return c
{indent_space}return ''.join(encrypt_char(c) for c in x)

def {affine_decrypt_function}(x):
{indent_space}# Affine cipher decryption
{indent_space}def decrypt_char(c):
{indent_space * 2}if c.isalpha():
{indent_space * 3}offset = ord('a') if c.islower() else ord('A')
{indent_space * 3}a_inv = pow({a}, -1, 26)  # Modular inverse of a
{indent_space * 3}return chr(((ord(c) - offset - {b}) * a_inv) % 26 + offset)
{indent_space * 2}return c
{indent_space}return ''.join(decrypt_char(c) for c in x)

{encrypted_var} = {affine_decrypt_function}({affine_encrypt_function}({self.last_source}))
    """)

        self.last_source = encrypted_var

    
    def source_mutation_40(self) -> None:
        indent_space = ' ' * 4
        bitwise_encrypt_function = self.generate_new_function()
        bitwise_decrypt_function = self.generate_new_function()
        encrypted_var = self.generate_new_variable()
        seed_var = self.generate_new_variable()

        self.source_statements.append(f"""
{seed_var} = 12345  # Seed for LCG

def lcg(seed, a=1664525, c=1013904223, m=2**32):
{indent_space}# Linear Congruential Generator
{indent_space}while True:
{indent_space * 2}seed = (a * seed + c) % m
{indent_space * 2}yield seed

def {bitwise_encrypt_function}(x, seed):
{indent_space}# XOR each byte with pseudo-random byte from LCG
{indent_space}lcg_gen = lcg(seed)
{indent_space}return ''.join(
{indent_space * 2}chr(ord(c) ^ (next(lcg_gen) % 256)) for c in x
{indent_space})

def {bitwise_decrypt_function}(x, seed):
{indent_space}# XOR each byte with the same pseudo-random byte from LCG
{indent_space}return {bitwise_encrypt_function}(x, seed)

{encrypted_var} = {bitwise_decrypt_function}({bitwise_encrypt_function}({self.last_source}, {seed_var}), {seed_var})
    """)

        self.last_source = encrypted_var

    def source_mutation_41(self) -> None:
        indent_space = ' ' * 4
        segment_reverse_function = self.generate_new_function()
        segment_restore_function = self.generate_new_function()
        transformed_var = self.generate_new_variable()
        segment_length = 3  # Length of segments to reverse

        self.source_statements.append(f"""
def {segment_reverse_function}(x):
{indent_space}# Reverse segments of the string
{indent_space}return ''.join(
{indent_space * 2}x[i:i+{segment_length}][::-1] for i in range(0, len(x), {segment_length})
{indent_space})

def {segment_restore_function}(x):
{indent_space}# Restore original string by reversing segments again
{indent_space}return {segment_reverse_function}(x)

{transformed_var} = {segment_restore_function}({segment_reverse_function}({self.last_source}))
    """)

        self.last_source = transformed_var

    def source_mutation_42(self) -> None:
        indent_space = ' ' * 4
        zigzag_encode_function = self.generate_new_function()
        zigzag_decode_function = self.generate_new_function()
        encoded_var = self.generate_new_variable()
        num_levels = 3  # Number of levels in the zigzag pattern

        self.source_statements.append(f"""
def {zigzag_encode_function}(x, levels):
{indent_space}# Zigzag pattern encoding
{indent_space}if levels <= 1 or levels >= len(x):
{indent_space * 2}return x
{indent_space}zigzag = ['' for _ in range(levels)]
{indent_space}level = 0
{indent_space}direction = 1
{indent_space}for char in x:
{indent_space * 2}zigzag[level] += char
{indent_space * 2}level += direction
{indent_space * 2}if level == 0 or level == levels - 1:
{indent_space * 3}direction *= -1
{indent_space}return ''.join(zigzag)

def {zigzag_decode_function}(x, levels):
{indent_space}# Zigzag pattern decoding
{indent_space}if levels <= 1 or levels >= len(x):
{indent_space * 2}return x
{indent_space}pattern = [0] * len(x)
{indent_space}level = 0
{indent_space}direction = 1
{indent_space}for i in range(len(x)):
{indent_space * 2}pattern[i] = level
{indent_space * 2}level += direction
{indent_space * 2}if level == 0 or level == levels - 1:
{indent_space * 3}direction *= -1
{indent_space}decoded = [''] * len(x)
{indent_space}index = 0
{indent_space}for l in range(levels):
{indent_space * 2}for i in range(len(x)):
{indent_space * 3}if pattern[i] == l:
{indent_space * 4}decoded[i] = x[index]
{indent_space * 4}index += 1
{indent_space}return ''.join(decoded)

{encoded_var} = {zigzag_decode_function}({zigzag_encode_function}({self.last_source}, {num_levels}), {num_levels})
    """)

        self.last_source = encoded_var

    def source_mutation_43(self) -> None:
        indent_space = ' ' * 4
        columnar_transpose_encrypt_function = self.generate_new_function()
        columnar_transpose_decrypt_function = self.generate_new_function()
        encrypted_var = self.generate_new_variable()
        key_var = self.generate_new_variable()

        self.source_statements.append(f"""
{key_var} = '3142'  # Key for columnar transposition

def {columnar_transpose_encrypt_function}(x, key):
{indent_space}# Columnar transposition encryption
{indent_space}num_cols = len(key)
{indent_space}num_rows = (len(x) + num_cols - 1) // num_cols
{indent_space}padded_length = num_rows * num_cols
{indent_space}padded_text = x.ljust(padded_length)
{indent_space}cols = [''] * num_cols
{indent_space}for r in range(num_rows):
{indent_space * 2}for c in range(num_cols):
{indent_space * 3}cols[c] += padded_text[r * num_cols + c]
{indent_space}sorted_key_indices = sorted(range(num_cols), key=lambda k: key[k])
{indent_space}return ''.join(cols[i] for i in sorted_key_indices)

def {columnar_transpose_decrypt_function}(x, key):
{indent_space}# Columnar transposition decryption
{indent_space}num_cols = len(key)
{indent_space}num_rows = (len(x) + num_cols - 1) // num_cols
{indent_space}cols = [''] * num_cols
{indent_space}col_lengths = [num_rows] * num_cols
{indent_space}for i in range(len(x) % num_cols):
{indent_space * 2}col_lengths[i] -= 1
{indent_space}sorted_key_indices = sorted(range(num_cols), key=lambda k: key[k])
{indent_space}index = 0
{indent_space}for i in sorted_key_indices:
{indent_space * 2}cols[i] = x[index:index + col_lengths[i]]
{indent_space * 2}index += col_lengths[i]
{indent_space}decrypted = ''.join(
{indent_space * 2}''.join(cols[c][r] for c in range(num_cols) if r < len(cols[c]))
{indent_space * 2}for r in range(num_rows)
{indent_space})
{indent_space}return decrypted.strip()

{encrypted_var} = {columnar_transpose_decrypt_function}({columnar_transpose_encrypt_function}({self.last_source}, {key_var}), {key_var})
    """)

        self.last_source = encrypted_var

    def source_mutation_44(self) -> None:
        indent_space = ' ' * 4
        playfair_encrypt_function = self.generate_new_function()
        playfair_decrypt_function = self.generate_new_function()
        encrypted_var = self.generate_new_variable()
        keyword_var = self.generate_new_variable()

        self.source_statements.append(f"""
{keyword_var} = 'keyword'  # Keyword for Playfair cipher

def create_playfair_square(keyword):
{indent_space}alphabet = 'abcdefghiklmnopqrstuvwxyz'  # Excludes 'j'
{indent_space}square = []
{indent_space}used = set()
{indent_space}for char in keyword:
{indent_space * 2}if char not in used and char in alphabet:
{indent_space * 3}square.append(char)
{indent_space * 3}used.add(char)
{indent_space}for char in alphabet:
{indent_space * 2}if char not in used:
{indent_space * 3}square.append(char)
{indent_space * 3}used.add(char)
{indent_space}return [square[i:i+5] for i in range(0, 25, 5)]

def find_position(char, square):
{indent_space}for row in range(5):
{indent_space * 2}for col in range(5):
{indent_space * 3}if square[row][col] == char:
{indent_space * 4}return row, col
{indent_space}return None, None

def {playfair_encrypt_function}(x, keyword):
{indent_space}square = create_playfair_square(keyword)
{indent_space}x = x.lower().replace('j', 'i').replace(' ', '')
{indent_space}if len(x) % 2 != 0:
{indent_space * 2}x += 'x'
{indent_space}encrypted = []
{indent_space}for i in range(0, len(x), 2):
{indent_space * 2}a, b = x[i], x[i+1]
{indent_space * 2}row_a, col_a = find_position(a, square)
{indent_space * 2}row_b, col_b = find_position(b, square)
{indent_space * 2}if row_a == row_b:
{indent_space * 3}encrypted.extend([square[row_a][(col_a + 1) % 5], square[row_b][(col_b + 1) % 5]])
{indent_space * 2}elif col_a == col_b:
{indent_space * 3}encrypted.extend([square[(row_a + 1) % 5][col_a], square[(row_b + 1) % 5][col_b]])
{indent_space * 2}else:
{indent_space * 3}encrypted.extend([square[row_a][col_b], square[row_b][col_a]])
{indent_space}return ''.join(encrypted)

def {playfair_decrypt_function}(x, keyword):
{indent_space}square = create_playfair_square(keyword)
{indent_space}decrypted = []
{indent_space}for i in range(0, len(x), 2):
{indent_space * 2}a, b = x[i], x[i+1]
{indent_space * 2}row_a, col_a = find_position(a, square)
{indent_space * 2}row_b, col_b = find_position(b, square)
{indent_space * 2}if row_a == row_b:
{indent_space * 3}decrypted.extend([square[row_a][(col_a - 1) % 5], square[row_b][(col_b - 1) % 5]])
{indent_space * 2}elif col_a == col_b:
{indent_space * 3}decrypted.extend([square[(row_a - 1) % 5][col_a], square[(row_b - 1) % 5][col_b]])
{indent_space * 2}else:
{indent_space * 3}decrypted.extend([square[row_a][col_b], square[row_b][col_a]])
{indent_space}return ''.join(decrypted)

{encrypted_var} = {playfair_decrypt_function}({playfair_encrypt_function}({self.last_source}, {keyword_var}), {keyword_var})
    """)

        self.last_source = encrypted_var


generator = CodeGenerator()

generator.source_mutation_25()
generator.source_mutation_26()
generator.source_mutation_27()
generator.source_mutation_28()
generator.source_mutation_29()
generator.source_mutation_30()
generator.source_mutation_19()
generator.source_mutation_31()
generator.source_mutation_32()
generator.source_mutation_33()
generator.source_mutation_34()
generator.source_mutation_35()
generator.source_mutation_36()
generator.source_mutation_37()
generator.source_mutation_38()
generator.source_mutation_39()
generator.source_mutation_40()
generator.source_mutation_41()
generator.source_mutation_42()
generator.source_mutation_43()
generator.source_mutation_44()

print(generator.generate())


def f0(x):
    import random
    x_list = list(x)
    random.shuffle(x_list)
    return ''.join(x_list)

a = f0(input())


def f1(x):

    return x[::-1]

def f2(x):

    return x[::-1]

b = f2(f1(a))


def f3(x):
    # Compression example: convert to hex
    return ''.join(format(ord(c), 'x') for c in x)

def f4(x):
    # Decompression: convert hex back to string
    return ''.join(chr(int(x[i:i+2], 16)) for i in range(0, len(x), 2))

c = f4(f3(b))


def f5(x):
    # Simple encryption: shift characters by 2
    return ''.join(chr(ord(c) + 2) for c in x)

def f6(x):
    # Decryption: shift characters back by 2
    return ''.join(chr(ord(c) - 2) for c in x)

d = f6(f5(c))


import base64

def f7(x):
    # Encode the string using Base64
    return base64.b64encode(x.encode('utf-8')).decode('utf-8')

def f8(x):
    # Decode the string from Base64
    return base64.b64decode(x.encode('utf-8')).decode('utf-8')

e = f8(f7(d))


def f9(x):
    # Substitute each letter with its opposite
    def substitute(c):
        if c.isalpha():
            offset = ord('a') if c.islower() else ord('A')
            return chr(offset + 25 - (ord(c) - offset))
        return c
    return ''.join(substitute(c) for c in x)

def f10(x):
    # Reverse substitution using the same logic
    return f9(x)

f = f10(f9(e))


def f11(x):
    return x

def f12(x):
    return x

def f13(x):
    return f11(f12(x))


def f14(x):
    # Apply ROT13 transformation
    def rotate(c):
        if c.isalpha():
            offset = ord('a') if c.islower() else ord('A')
            return chr(offset + (ord(c) - offset + 13) % 26)
        return c
    return ''.join(rotate(c) for c in x)

g = f14(f14(f13(f)))


def f15(x):
    # Swap adjacent characters
    result = list(x)
    for i in range(0, len(result) - 1, 2):
        result[i], result[i+1] = result[i+1], result[i]
    return ''.join(result)

def f16(x):
    # Reverse swap using the same logic
    return f15(x)

h = f16(f15(g))


j = [3, 1, 4, 2, 0]  # A simple transposition key

def f17(x, key):
    # Transposition cipher encryption
    n = len(x)
    key = key[:n]  # Adjust key length to match input
    sorted_key = sorted(range(n), key=lambda i: key[i])
    return ''.join(x[i] for i in sorted_key)

def f18(x, key):
    # Transposition cipher decryption
    n = len(x)
    key = key[:n]
    inverse_key = sorted(range(n), key=lambda i: key[i])
    return ''.join(x[inverse_key.index(i)] for i in range(n))

i = f18(f17(h, j), j)


l = 'secret'  # Vigenère cipher keyword

def f19(x, keyword):
    # Vigenère cipher encoding
    encoded_chars = []
    keyword_repeated = (keyword * ((len(x) // len(keyword)) + 1))[:len(x)]
    for i, c in enumerate(x):
        if c.isalpha():
            offset = ord('a') if c.islower() else ord('A')
            keyword_offset = ord(keyword_repeated[i].lower()) - ord('a')
            encoded_char = chr((ord(c) - offset + keyword_offset) % 26 + offset)
        else:
            encoded_char = c
        encoded_chars.append(encoded_char)
    return ''.join(encoded_chars)

def f20(x, keyword):
    # Vigenère cipher decoding
    decoded_chars = []
    keyword_repeated = (keyword * ((len(x) // len(keyword)) + 1))[:len(x)]
    for i, c in enumerate(x):
        if c.isalpha():
            offset = ord('a') if c.islower() else ord('A')
            keyword_offset = ord(keyword_repeated[i].lower()) - ord('a')
            decoded_char = chr((ord(c) - offset - keyword_offset + 26) % 26 + offset)
        else:
            decoded_char = c
        decoded_chars.append(decoded_char)
    return ''.join(decoded_chars)

k = f20(f19(i, l), l)


n = 42  # XOR cipher key

def f21(x, key):
    # XOR cipher encryption
    return ''.join(chr(ord(c) ^ key) for c in x)

def f22(x, key):
    # XOR cipher decryption (same operation)
    return ''.join(chr(ord(c) ^ key) for c in x)

m = f22(f21(k, n), n)


def f23(x):
    # Split string into two halves and interleave them
    mid = len(x) // 2
    first_half = x[:mid]
    second_half = x[mid:]
    scrambled = ''.join(second_half[i:i+1] + first_half[i:i+1] for i in range(mid))
    if len(x) % 2 == 1:
        scrambled += second_half[-1]
    return scrambled

def f24(x):
    # Unscramble by reversing the interleave
    mid = len(x) // 2
    first_half = ''.join(x[i] for i in range(1, len(x), 2))
    second_half = ''.join(x[i] for i in range(0, len(x), 2))
    return second_half[:mid] + first_half

o = f24(f23(m))


def f25(x, rails):
    # Rail Fence cipher encryption
    fence = [[] for _ in range(rails)]
    rail = 0
    direction = 1
    for char in x:
        fence[rail].append(char)
        rail += direction
        if rail == 0 or rail == rails - 1:
            direction *= -1
    return ''.join(''.join(row) for row in fence)

def f26(x, rails):
    # Rail Fence cipher decryption
    rail_lengths = [0] * rails
    rail = 0
    direction = 1
    for _ in x:
        rail_lengths[rail] += 1
        rail += direction
        if rail == 0 or rail == rails - 1:
            direction *= -1
    rail_chars = [''] * rails
    i = 0
    for r in range(rails):
        rail_chars[r] = x[i:i + rail_lengths[r]]
        i += rail_lengths[r]
    result = []
    rail = 0
    direction = 1
    for _ in x:
        result.append(rail_chars[rail][0])
        rail_chars[rail] = rail_chars[rail][1:]
        rail += direction
        if rail == 0 or rail == rails - 1:
            direction *= -1
    return ''.join(result)

p = f26(f25(o, 3), 3)


def f27(x):
    # Shifting Caesar cipher encryption
    return ''.join(
        chr((ord(c) - (ord('a') if c.islower() else ord('A')) + (i % 26)) % 26 + (ord('a') if c.islower() else ord('A')))
        if c.isalpha() else c
        for i, c in enumerate(x)
    )

def f28(x):
    # Shifting Caesar cipher decryption
    return ''.join(
        chr((ord(c) - (ord('a') if c.islower() else ord('A')) - (i % 26) + 26) % 26 + (ord('a') if c.islower() else ord('A')))
        if c.isalpha() else c
        for i, c in enumerate(x)
    )

q = f28(f27(p))


def f29(x):
    # Affine cipher encryption
    def encrypt_char(c):
        if c.isalpha():
            offset = ord('a') if c.islower() else ord('A')
            return chr(((ord(c) - offset) * 5 + 8) % 26 + offset)
        return c
    return ''.join(encrypt_char(c) for c in x)

def f30(x):
    # Affine cipher decryption
    def decrypt_char(c):
        if c.isalpha():
            offset = ord('a') if c.islower() else ord('A')
            a_inv = pow(5, -1, 26)  # Modular inverse of a
            return chr(((ord(c) - offset - 8) * a_inv) % 26 + offset)
        return c
    return ''.join(decrypt_char(c) for c in x)

r = f30(f29(q))


t = 12345  # Seed for LCG

def lcg(seed, a=1664525, c=1013904223, m=2**32):
    # Linear Congruential Generator
    while True:
        seed = (a * seed + c) % m
        yield seed

def f31(x, seed):
    # XOR each byte with pseudo-random byte from LCG
    lcg_gen = lcg(seed)
    return ''.join(
        chr(ord(c) ^ (next(lcg_gen) % 256)) for c in x
    )

def f32(x, seed):
    # XOR each byte with the same pseudo-random byte from LCG
    return f31(x, seed)

s = f32(f31(r, t), t)


def f33(x):
    # Reverse segments of the string
    return ''.join(
        x[i:i+3][::-1] for i in range(0, len(x), 3)
    )

def f34(x):
    # Restore original string by reversing segments again
    return f33(x)

u = f34(f33(s))


def f35(x, levels):
    # Zigzag pattern encoding
    if levels <= 1 or levels >= len(x):
        return x
    zigzag = ['' for _ in range(levels)]
    level = 0
    direction = 1
    for char in x:
        zigzag[level] += char
        level += direction
        if level == 0 or level == levels - 1:
            direction *= -1
    return ''.join(zigzag)

def f36(x, levels):
    # Zigzag pattern decoding
    if levels <= 1 or levels >= len(x):
        return x
    pattern = [0] * len(x)
    level = 0
    direction = 1
    for i in range(len(x)):
        pattern[i] = level
        level += direction
        if level == 0 or level == levels - 1:
            direction *= -1
    decoded = [''] * len(x)
    index = 0
    for l in range(levels):
        for i in range(len(x)):
            if pattern[i] == l:
                decoded[i] = x[index]
                index += 1
    return ''.join(decoded)

v = f36(f35(u, 3), 3)


x = '3142'  # Key for columnar transposition

def f37(x, key):
    # Columnar transposition encryption
    num_cols = len(key)
    num_rows = (len(x) + num_cols - 1) // num_cols
    padded_length = num_rows * num_cols
    padded_text = x.ljust(padded_length)
    cols = [''] * num_cols
    for r in range(num_rows):
        for c in range(num_cols):
            cols[c] += padded_text[r * num_cols + c]
    sorted_key_indices = sorted(range(num_cols), key=lambda k: key[k])
    return ''.join(cols[i] for i in sorted_key_indices)

def f38(x, key):
    # Columnar transposition decryption
    num_cols = len(key)
    num_rows = (len(x) + num_cols - 1) // num_cols
    cols = [''] * num_cols
    col_lengths = [num_rows] * num_cols
    for i in range(len(x) % num_cols):
        col_lengths[i] -= 1
    sorted_key_indices = sorted(range(num_cols), key=lambda k: key[k])
    index = 0
    for i in sorted_key_indices:
        cols[i] = x[index:index + col_lengths[i]]
        index += col_lengths[i]
    decrypted = ''.join(
        ''.join(cols[c][r] for c in range(num_cols) if r < len(cols[c]))
        for r in range(num_rows)
    )
    return decrypted.strip()

w = f38(f37(v, x), x)


z = 'keyword'  # Keyword for Playfair cipher

def create_playfair_square(keyword):
    alphabet = 'abcdefghiklmnopqrstuvwxyz'  # Excludes 'j'
    square = []
    used = set()
    for char in keyword:
        if char not in used and char in alphabet:
            square.append(char)
            used.add(char)
    for char in alphabet:
        if char not in used:
            square.append(char)
            used.add(char)
    return [square[i:i+5] for i in range(0, 25, 5)]

def find_position(char, square):
    for row in range(5):
        for col in range(5):
            if square[row][col] == char:
                return row, col
    return None, None

def f39(x, keyword):
    square = create_playfair_square(keyword)
    x = x.lower().replace('j', 'i').replace(' ', '')
    if len(x) % 2 != 0:
        x += 'x'
    encrypted = []
    for i in range(0, len(x), 2):
        a, b = x[i], x[i+1]
        row_a, col_a = find_position(a, square)
        row_b, col_b = find_position(b, square)
        if row_a == row_b:
            encrypted.extend([square[row_a][(col_a + 1) % 5], square[row_b][(col_b + 1) % 5]])
        elif col_a == col_b:
            encrypted.extend([square[(row_a + 1) % 5][col_a], square[(row_b + 1) % 5][col_b]])
        else:
            encrypted.extend([square[row_a][col_b], square[row_b][col_a]])
    return ''.join(encrypted)

def f40(x, keyword):
    square = create_playfair_square(keyword)
    decrypted = []
    for i in range(0, len(x), 2):
        a, b = x[i], x[i+1]
        row_a, col_a = find_position(a, square)
        row_b, col_b = find_position(b, square)
        if row_a == row_b:
            decrypted.extend([square[row_a][(col_a - 1) % 5], square[row_b][(col_b - 1) % 5]])
        elif col_a == col_b:
            decrypted.extend([square[(row_a - 1) % 5][col_a], square[(row_b - 1) % 5][col_b]])
        else:
            decrypted.extend([square[row_a][col_b], square[row_b][col_a]])
    return ''.join(decrypted)

y = f40(f39(w, z), z)

print(y)