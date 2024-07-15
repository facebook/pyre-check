from typing import List
import itertools
import string
import random 

class CodeGenerator:
    def __init__(self) -> None:
        self.variables = self.generate_variable_names()
        self.current_var = 0
        self.current_function_number = 1
        self.last_node = "input()"

    def generate_variable_names(self) -> List[str]:
        single_letter_names = list(string.ascii_lowercase)
        two_letter_names = [''.join(pair) for pair in itertools.product(string.ascii_lowercase, repeat=2)]
        names = single_letter_names + two_letter_names
        # Remove reserved keywords
        reserved_keywords = {'as', 'in', 'if', 'is', 'or'}
        names = [name for name in names if name not in reserved_keywords]
        return names

    def generate_new_function(self) -> str:
        function_name = f"f{self.current_function_number}"
        self.current_function_number += 1
        return function_name

    def generate_new_variable(self) -> str:
        index = self.current_var
        self.current_var += 1
        return self.variables[index]

    def get_last_variable(self) -> str:
        return self.variables[self.current_var - 1]

    def generate_sink(self) -> str: 
        return f"print({self.last_node})"
         
    def add_variable(self) -> str:
        current_var = self.generate_new_variable()
        temp = self.last_node 
        self.last_node = current_var
        return f"{current_var} = {temp}" 

    def add_function(self) -> str:
        current_function = self.generate_new_function()
        temp = self.last_node
        self.last_node = current_function + "()"
        indent_space = ' ' * 4
        return f"def {current_function}():\n{indent_space}return {temp}"

    def add_condition_function(self) -> str: 
        current_function = self.generate_new_function()
        temp = self.last_node 
        self.last_node = current_function + "()"
        indent_space = ' ' * 4
        x = random.randint(1, 100)
        return f"def {current_function}():\n{indent_space}if {x} <= 33:\n{indent_space * 2}return {temp}\n{indent_space}elif 33 < {x} <= 66:\n{indent_space * 2} return {temp}\n{indent_space}else:\n{indent_space * 2}return {temp}"

    def generate(self, instructions: List[int]) -> str:
        code_lines = []
        for instruction in instructions:
            if instruction == 1:
                code_lines.append(self.add_variable())
            elif instruction == 2:
                code_lines.append(self.add_function())
            elif instruction == 3: 
                code_lines.append(self.add_condition_function())
        code_lines.append(self.generate_sink())
        return '\n'.join(code_lines)

# Example usage
generator = CodeGenerator()
instructions = [1,2,2,1,3,2,3] 
print(generator.generate(instructions))


# [] 
print(input())

# [1]
a = input()
print(a)

# [1,2]
a = input()
def f1():
    return a
print(f1())

# [1,2,2]
a = input()
def f1():
    return a
def f2():
    return f1()
print(f2())

# [1,2,2,1]
a = input()
def f1():
    return a
def f2():
    return f1()
b = f2()
print(b)

# [1,2,2,1,3]
a = input()
def f1():
    return a
def f2():
    return f1()
b = f2()
def f3():
    if 12 <= 33:
        return b
    elif 33 < 12 <= 66:
         return b
    else:
        return b
print(f3())

# [1,2,2,1,3,2]
a = input()
def f1():
    return a
def f2():
    return f1()
b = f2()
def f3():
    if 4 <= 33:
        return b
    elif 33 < 4 <= 66:
         return b
    else:
        return b
def f4():
    return f3()
print(f4())


# [1,2,2,1,3,2,3]
