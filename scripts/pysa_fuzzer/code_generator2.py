from typing import List
import itertools
import string

class CodeGenerator:
    def __init__(self) -> None:
        self.variables = self.generate_variable_names()
        self.current_var = 1
        self.current_function_number = 1
        self.source_statements = ['a = input()']
        self.last_source = "a"
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
        variable_name = f"{self.variables[self.current_var]}"
        self.current_var += 1
        return variable_name

    def generate_new_function(self) -> str:
        function_name = f"f{self.current_function_number}"
        self.current_function_number += 1
        return function_name

    def generate_sink(self) -> str:
        return f"{self.last_sink}({self.last_source})"

    # wraps both the source and sink chains 
    def add_function(self) -> None:
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
        self.source_statements.append(f"def {current_function_source}():\n{indent_space}random_number = random.randint(1,3)\n{indent_space}{new_var} = {temp_source}\n{indent_space}if random_number == 1:\n{indent_space*2}return {new_var}\n{indent_space}else:\n{indent_space*2}return {current_function_source}()")
        self.sink_statements.append(f"def {current_function_sink}(x):\n{indent_space}random_number = random.randint(1,3)\n{indent_space}if random_number == 1:\n{indent_space*2}return {temp_sink}(x)\n{indent_space}else:\n{indent_space*2}return {current_function_sink}(x)")

    # only adds a variable to the source chain 
    def add_variable(self) -> None:
        indent_space = ' ' * 4
        new_var = self.generate_new_variable()
        temp_source = self.last_source
        self.last_source = new_var 
        self.source_statements.append(f"{new_var} = {temp_source}")

    def add_nested_function(self) -> None:
        indent_space = ' ' * 4
        # source stuff  
        current_function_source = self.generate_new_function()
        nested_function_source = self.generate_new_function()
        temp_source = self.last_source
        self.last_source = current_function_source + "()"
        new_var = self.generate_new_variable()
        # sink stuff 
        current_function_sink = self.generate_new_function()
        nested_function_sink = self.generate_new_function()
        temp_sink = self.last_sink
        self.last_sink = current_function_sink
        # adding statements 
        self.source_statements.append(f"def {current_function_source}():\n{indent_space}def {nested_function_source}():\n{indent_space * 2}random_number = random.randint(1,3)\n{indent_space * 2}{new_var} = {temp_source}\n{indent_space * 2}if random_number == 1:\n{indent_space*3}return {new_var}\n{indent_space*2}else:\n{indent_space*3}return {current_function_source}()\n{indent_space}return {nested_function_source}()" )
        self.sink_statements.append(f"def {current_function_sink}(x):\n{indent_space}def {nested_function_sink}(x):\n{indent_space*2}random_number = random.randint(1,3)\n{indent_space*2}if random_number == 1:\n{indent_space*3}return {temp_sink}(x)\n{indent_space*2}else:\n{indent_space*3}return {current_function_sink}(x)\n{indent_space}return {nested_function_sink}(x)")
    


    def generate(self) -> str:
        code_lines = self.source_statements + self.sink_statements
        code_lines.append(self.generate_sink())
        return '\n'.join(code_lines)
    


# Example usage
generator = CodeGenerator()
generator.add_nested_function()
generator.add_nested_function()
generator.add_nested_function()
generator.add_nested_function()
generator.add_nested_function()

print(generator.generate())
import random

a = input()
def f1():
    def f2():
        random_number = random.randint(1,3)
        b = a
        if random_number == 1:
            return b
        else:
            return f1()
    return f2()
def f5():
    def f6():
        random_number = random.randint(1,3)
        c = f1()
        if random_number == 1:
            return c
        else:
            return f5()
    return f6()
def f9():
    def f10():
        random_number = random.randint(1,3)
        d = f5()
        if random_number == 1:
            return d
        else:
            return f9()
    return f10()
def f13():
    def f14():
        random_number = random.randint(1,3)
        e = f9()
        if random_number == 1:
            return e
        else:
            return f13()
    return f14()
def f17():
    def f18():
        random_number = random.randint(1,3)
        f = f13()
        if random_number == 1:
            return f
        else:
            return f17()
    return f18()
def f3(x):
    def f4(x):
        random_number = random.randint(1,3)
        if random_number == 1:
            return print(x)
        else:
            return f3(x)
    return f4(x)
def f7(x):
    def f8(x):
        random_number = random.randint(1,3)
        if random_number == 1:
            return f3(x)
        else:
            return f7(x)
    return f8(x)
def f11(x):
    def f12(x):
        random_number = random.randint(1,3)
        if random_number == 1:
            return f7(x)
        else:
            return f11(x)
    return f12(x)
def f15(x):
    def f16(x):
        random_number = random.randint(1,3)
        if random_number == 1:
            return f11(x)
        else:
            return f15(x)
    return f16(x)
def f19(x):
    def f20(x):
        random_number = random.randint(1,3)
        if random_number == 1:
            return f15(x)
        else:
            return f19(x)
    return f20(x)
f19(f17())
