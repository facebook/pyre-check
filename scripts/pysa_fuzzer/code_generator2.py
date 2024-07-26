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



# Test the new mutation function
generator = CodeGenerator()
generator.source_mutation_9()
generator.source_mutation_8()
generator.source_mutation_7()
generator.source_mutation_1()

print(generator.generate())
a = lambda x: x
b = [a(char) for char in input()]
    

class Class0:
    def f0(self, x):
        return x

c = Class0()
    

d = 'a' 
e = 'b'  
if d == 'a':
    def f1(x):
        return x
else:
    def f2(x):
        return x
if e == 'b':
        def f3(y):
            return y
    

def f4(x):
    return x
        
print(f4(f3(f1(c.f0(''.join(b))))))