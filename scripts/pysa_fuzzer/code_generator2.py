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

    def add_function_1(self) -> None:
        indent_space = ' ' * 4
        # source stuff
        current_function_source = self.generate_new_function()
        temp_source = self.last_source
        self.last_source = current_function_source + "()"
        new_var = self.generate_new_variable()
        # sink stuff
        current_function_sink = self.generate_new_function()
        temp_sink = self.last_sink
        self.last_sink = current_function_sink
        # adding statements
        self.source_statements.append(f"""
def {current_function_source}():
{indent_space}if random.randint(1, 3) == 1:
{indent_space*2}return {temp_source}
{indent_space}else:
{indent_space*2}return {current_function_source}()
        """)
        self.sink_statements.append(f"""
def {current_function_sink}(x):
{indent_space}if random.randint(1, 3) == 1:
{indent_space*2}return {temp_sink}(x)
{indent_space}else:
{indent_space*2}return {current_function_sink}(x)
        """)

    def add_function_2(self) -> None:
        indent_space = ' ' * 4
        # source stuff
        current_function_source = self.generate_new_function()
        temp_source = self.last_source
        self.last_source = current_function_source + f"({temp_source})"
        new_var = self.generate_new_variable()
        # sink stuff
        current_function_sink = self.generate_new_function()
        temp_sink = self.last_sink
        self.last_sink = current_function_sink
        # adding statements
        self.source_statements.append(f"""
def {current_function_source}(x):
{indent_space}if random.randint(1, 3) == 1:
{indent_space*2}return x
{indent_space}else:
{indent_space*2}return {current_function_source}(x)
        """)
        self.sink_statements.append(f"""
def {current_function_sink}(x):
{indent_space}if random.randint(1, 3) == 1:
{indent_space*2}return {temp_sink}(x)
{indent_space}else:
{indent_space*2}return {current_function_sink}(x)
        """)

    def add_function_3(self) -> None:
        indent_space = ' ' * 4
        # source stuff
        current_function_source = self.generate_new_function()
        temp_source = self.last_source
        self.last_source = f"{current_function_source}({temp_source}, 'temp')"
        # sink stuff
        current_function_sink = self.generate_new_function()
        temp_sink = self.last_sink
        self.last_sink = current_function_sink
        # adding statements
        self.source_statements.append(f"""
def {current_function_source}(x, y):
{indent_space}if random.randint(1, 3) == 1:
{indent_space*2}return x + y
{indent_space}else:
{indent_space*2}return {current_function_source}(x, y)
    """)
        self.sink_statements.append(f"""
def {current_function_sink}(x):
{indent_space}if random.randint(1, 3) == 1:
{indent_space*2}return {temp_sink}(x)
{indent_space}else:
{indent_space*2}return {current_function_sink}(x)
    """)

    def add_function_4(self) -> None:
        indent_space = ' ' * 4
        # source stuff
        current_function_source = self.generate_new_function()
        temp_source = self.last_source
        self.last_source = f"{current_function_source}({temp_source})"
        # sink stuff
        current_function_sink = self.generate_new_function()
        temp_sink = self.last_sink
        self.last_sink = current_function_sink
        # adding statements
        self.source_statements.append(f"""
def {current_function_source}(x):
{indent_space}y = x * 2
{indent_space}if random.randint(1, 3) == 1:
{indent_space*2}return y
{indent_space}else:
{indent_space*2}return {current_function_source}(y)
    """)
        self.sink_statements.append(f"""
def {current_function_sink}(x):
{indent_space}if random.randint(1, 3) == 1:
{indent_space*2}return {temp_sink}(x)
{indent_space}else:
{indent_space*2}return {current_function_sink}(x)
    """)

    def add_function_6(self) -> None:
        indent_space = ' ' * 4
        # source stuff
        current_function_source = self.generate_new_function()
        temp_source = self.last_source
        self.last_source = f"{current_function_source}({temp_source})"
        # sink stuff
        current_function_sink = self.generate_new_function()
        temp_sink = self.last_sink
        self.last_sink = current_function_sink
        # adding statements
        self.source_statements.append(f"""
def {current_function_source}(x):
{indent_space}if (y := random.randint(1, 3)) == 1:
{indent_space*2}return x + str(y)
{indent_space}else:
{indent_space*2}return {current_function_source}(x)
    """)
        self.sink_statements.append(f"""
def {current_function_sink}(x):
{indent_space}if (y := random.randint(1, 3)) == 1:
{indent_space*2}return {temp_sink}(x + str(y))
{indent_space}else:
{indent_space*2}return {current_function_sink}(x)
    """)

    def add_function_7(self) -> None:
        indent_space = ' ' * 4
        # source stuff
        current_function_source = self.generate_new_function()
        temp_source = self.last_source
        self.last_source = f"{current_function_source}({temp_source})"
        # sink stuff
        current_function_sink = self.generate_new_function()
        temp_sink = self.last_sink
        self.last_sink = current_function_sink
        # adding statements
        self.source_statements.append(f"""
def {current_function_source}(x):
{indent_space}if (z := x.count('a')) > 2:
{indent_space*2}return x.replace('a', 'arep')
{indent_space}else:
{indent_space*2}return {current_function_source}(x + 'a')
    """)
        self.sink_statements.append(f"""
def {current_function_sink}(x):
{indent_space}if (z := x.count('z')) > 1:
{indent_space*2}return {temp_sink}(x.replace('a', 'arep'))
{indent_space}else:
{indent_space*2}return {current_function_sink}(x + 'z')
    """)

    def add_function_8(self) -> None:
        indent_space = ' ' * 4
        # source stuff
        current_function_source = self.generate_new_function()
        temp_source = self.last_source
        self.last_source = f"{current_function_source}({temp_source})"
        # sink stuff
        current_function_sink = self.generate_new_function()
        temp_sink = self.last_sink
        self.last_sink = current_function_sink
        # adding statements
        self.source_statements.append(f"""
def {current_function_source}(x):
{indent_space}hashmap = {{char: ord(char) for char in x}}
{indent_space}if (value := hashmap.get('a')) is not None:
{indent_space*2}return chr(value + 1) + x
{indent_space}else:
{indent_space*2}return {current_function_source}(x + 'a')
    """)
        self.sink_statements.append(f"""
def {current_function_sink}(x):
{indent_space}hashmap = {{char: ord(char) for char in x}}
{indent_space}if (value := hashmap.get('z')) is not None:
{indent_space*2}return {temp_sink}(chr(value - 1) + x)
{indent_space}else:
{indent_space*2}return {current_function_sink}(x + 'z')
    """)

    def add_function_9(self) -> None:
        indent_space = ' ' * 4
        # source stuff
        current_function_source = self.generate_new_function()
        temp_source = self.last_source
        self.last_source = f"{current_function_source}({temp_source})"
        # sink stuff
        current_function_sink = self.generate_new_function()
        temp_sink = self.last_sink
        self.last_sink = current_function_sink
        # adding statements
        self.source_statements.append(f"""
def {current_function_source}(x):
{indent_space}hashmap = {{char: x.count(char) for char in set(x)}}
{indent_space}if (count := hashmap.get('a', 0)) > 2:
{indent_space*2}return 'b' * count + x
{indent_space}else:
{indent_space*2}return {current_function_source}(x + 'a')
    """)
        self.sink_statements.append(f"""
def {current_function_sink}(x):
{indent_space}hashmap = {{char: x.count(char) for char in set(x)}}
{indent_space}if (count := hashmap.get('z', 0)) > 1:
{indent_space*2}return {temp_sink}('y' * count + x)
{indent_space}else:
{indent_space*2}return {current_function_sink}(x + 'z')
    """)

   

    
     

    def generate(self) -> str:
        code_lines = self.source_statements + self.sink_statements
        code_lines.append(self.generate_sink())
        return '\n'.join(code_lines)


generator = CodeGenerator()



generator.add_function_1()
generator.add_function_2()
generator.add_function_3()

generator.add_function_4()
generator.add_function_6()
generator.add_function_7()

generator.add_function_8()
generator.add_function_9()
generator.add_function_1()



print(generator.generate())



def f0():
    if random.randint(1, 3) == 1:
        return input()
    else:
        return f0()
        

def f2(x):
    if random.randint(1, 3) == 1:
        return x
    else:
        return f2(x)
        

def f4(x, y):
    if random.randint(1, 3) == 1:
        return x + y
    else:
        return f4(x, y)
    

def f6(x):
    y = x * 2
    if random.randint(1, 3) == 1:
        return y
    else:
        return f6(y)
    

def f8(x):
    if (y := random.randint(1, 3)) == 1:
        return x + str(y)
    else:
        return f8(x)
    

def f10(x):
    if (z := x.count('a')) > 2:
        return x.replace('a', 'arep')
    else:
        return f10(x + 'a')
    

def f12(x):
    hashmap = {char: ord(char) for char in x}
    if (value := hashmap.get('a')) is not None:
        return chr(value + 1) + x
    else:
        return f12(x + 'a')
    

def f14(x):
    hashmap = {char: x.count(char) for char in set(x)}
    if (count := hashmap.get('a', 0)) > 2:
        return 'b' * count + x
    else:
        return f14(x + 'a')
    

def f16():
    if random.randint(1, 3) == 1:
        return f14(f12(f10(f8(f6(f4(f2(f0()), 'temp'))))))
    else:
        return f16()
        

def f1(x):
    if random.randint(1, 3) == 1:
        return print(x)
    else:
        return f1(x)
        

def f3(x):
    if random.randint(1, 3) == 1:
        return f1(x)
    else:
        return f3(x)
        

def f5(x):
    if random.randint(1, 3) == 1:
        return f3(x)
    else:
        return f5(x)
    

def f7(x):
    if random.randint(1, 3) == 1:
        return f5(x)
    else:
        return f7(x)
    

def f9(x):
    if (y := random.randint(1, 3)) == 1:
        return f7(x + str(y))
    else:
        return f9(x)
    

def f11(x):
    if (z := x.count('z')) > 1:
        return f9(x.replace('a', 'arep'))
    else:
        return f11(x + 'z')
    

def f13(x):
    hashmap = {char: ord(char) for char in x}
    if (value := hashmap.get('z')) is not None:
        return f11(chr(value - 1) + x)
    else:
        return f13(x + 'z')
    

def f15(x):
    hashmap = {char: x.count(char) for char in set(x)}
    if (count := hashmap.get('z', 0)) > 1:
        return f13('y' * count + x)
    else:
        return f15(x + 'z')
    

def f17(x):
    if random.randint(1, 3) == 1:
        return f15(x)
    else:
        return f17(x)
        
f17(f16())