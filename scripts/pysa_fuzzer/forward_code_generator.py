# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# pyre-strict
import itertools
import string
import textwrap
from random import Random
from typing import List


class CodeGenerator:
    def __init__(self, enable_known_false_negatives: bool) -> None:
        self.variables: List[str] = self.generate_variable_names()
        self.current_var: int = 0
        self.rng = Random()
        self.enable_known_false_negatives: bool = enable_known_false_negatives

    def reset(self) -> None:
        self.current_var: int = 0

    def generate_variable_names(self) -> List[str]:
        single_letter_names = list(string.ascii_lowercase)
        two_letter_names = [
            "".join(pair)
            for pair in itertools.product(string.ascii_lowercase, repeat=2)
        ]
        names = single_letter_names + two_letter_names
        reserved_keywords = {"as", "in", "if", "is", "or"}
        names = [name for name in names if name not in reserved_keywords]
        return names

    def generate_new_variable(self) -> str:
        index = self.current_var
        self.current_var += 1
        return self.variables[index]

    def get_last_variable(self) -> str:
        return self.variables[self.current_var - 1]

    def generate_import_statements(self) -> str:
        return "import random\nimport math\n"

    def generate_source(self) -> str:
        curr_var = self.generate_new_variable()
        return f"{curr_var} = input()"

    def generate_sink(self) -> str:
        prev_var = self.get_last_variable()
        return f"print({prev_var})"

    def generate_addition(self, depth: int = 1) -> str:
        prev_var = self.get_last_variable()
        curr_var = self.generate_new_variable()
        addition_code = f"{curr_var} = {prev_var} + '{self.rng.randint(1, 9)}'"

        if depth > 1:
            nested_addition = self.generate_addition(depth - 1)
            return f"{addition_code}\n{nested_addition}"
        else:
            return addition_code

    def generate_for_loop(self, depth: int = 1) -> str:
        prev_var = self.get_last_variable()
        curr_var = self.generate_new_variable()
        loop_body = f"{curr_var} += {prev_var}"

        if depth > 1:
            nested_loop = self.generate_for_loop(depth - 1)
            loop_body = f"{nested_loop}\n" + f"{textwrap.indent(loop_body, '    ')}"

        return (
            f"{curr_var} = ''\n"
            + f"for _ in range({self.rng.randint(2, 5)}):\n"
            + f"{textwrap.indent(loop_body, '    ')}"
        )

    def generate_while_loop(self, depth: int = 1) -> str:
        prev_var = self.get_last_variable()
        curr_var = self.generate_new_variable()
        counter_name = curr_var
        loop_body = f"{curr_var} += {prev_var}\n" + f"counter{counter_name} += 1"

        if depth > 1:
            nested_loop = self.generate_while_loop(depth - 1)
            loop_body = f"{nested_loop}\n" + f"{textwrap.indent(loop_body, '    ')}"

        return (
            f"{curr_var} = ''\n"
            + f"counter{counter_name} = 0\n"
            + f"while counter{counter_name} < {self.rng.randint(2, 5)}:\n"
            + f"{textwrap.indent(loop_body, '    ')}"
        )

    def generate_list(self, depth: int = 1) -> str:
        code_lines = []
        prev_var = self.get_last_variable()
        curr_var = self.generate_new_variable()
        list_length = self.rng.randint(2, 10)
        list_creation = f"{curr_var}_list = [{prev_var} for _ in range({list_length})]"
        code_lines.append(list_creation)

        for _ in range(depth - 1):
            prev_var = f"{curr_var}_list"
            curr_var = self.generate_new_variable()
            list_length = self.rng.randint(2, 10)
            list_creation = (
                f"{curr_var}_list = [{prev_var} for _ in range({list_length})]"
            )
            code_lines.append(list_creation)

        last_var = f"{curr_var}_list"
        curr_var = self.generate_new_variable()
        list_access = f"{curr_var} = random.choice({last_var})"
        code_lines.append(list_access)

        for _ in range(depth - 1):
            prev_var = curr_var
            curr_var = self.generate_new_variable()
            list_access = f"{curr_var} = random.choice({prev_var})"
            code_lines.append(list_access)

        return "\n".join(code_lines)

    def generate_dictionary(self, depth: int = 1) -> str:
        code_lines = []
        prev_var = self.get_last_variable()
        curr_var = self.generate_new_variable()
        dict_length = self.rng.randint(2, 10)
        dict_creation = f"{curr_var}_dict = {{{', '.join(f'{self.rng.randint(1, 100)}: {prev_var}' for _ in range(dict_length))}}}"
        code_lines.append(dict_creation)

        for _ in range(depth - 1):
            prev_var = f"{curr_var}_dict"
            curr_var = self.generate_new_variable()
            dict_length = self.rng.randint(2, 10)
            dict_creation = f"{curr_var}_dict = {{{', '.join(f'{self.rng.randint(1, 100)}: {prev_var}' for _ in range(dict_length))}}}"
            code_lines.append(dict_creation)

        last_var = f"{curr_var}_dict"
        curr_var = self.generate_new_variable()
        dict_access = f"{curr_var} = random.choice(list({last_var}.values()))"
        code_lines.append(dict_access)

        for _ in range(depth - 1):
            prev_var = curr_var
            curr_var = self.generate_new_variable()
            dict_access = f"{curr_var} = random.choice(list({prev_var}.values()))"
            code_lines.append(dict_access)

        return "\n".join(code_lines)

    def generate_set(self) -> str:
        prev_var = self.get_last_variable()
        curr_var = self.generate_new_variable()
        set_length = self.rng.randint(2, 10)
        set_creation = (
            f"{curr_var}_set = {{{', '.join(prev_var for _ in range(set_length))}}}"
        )
        set_access = f"{curr_var} = random.choice(list({curr_var}_set))"
        return f"{set_creation}\n{set_access}"

    def generate_string_concatenation(self) -> str:
        prev_var = self.get_last_variable()
        curr_var = self.generate_new_variable()
        concat_operation = f"{curr_var} = {prev_var} + '.'"
        return concat_operation

    def generate_string_slicing(self) -> str:
        prev_var = self.get_last_variable()
        curr_var = self.generate_new_variable()
        start_idx = 0
        slice_operation = f"{curr_var} = {prev_var}[{start_idx}:]"
        return slice_operation

    def generate_string_formatting(self) -> str:
        prev_var = self.get_last_variable()
        curr_var = self.generate_new_variable()
        format_operation = f"{curr_var} = f'string {{{prev_var}}}'"
        return format_operation

    def generate_tuple_manipulation(self) -> str:
        prev_var = self.get_last_variable()
        curr_var = self.generate_new_variable()
        tuple_creation = f"{curr_var} = ({', '.join([prev_var for _ in range(3)])})"
        unpack_vars = ", ".join(self.generate_new_variable() for _ in range(3))
        unpack_operation = f"{unpack_vars} = {curr_var}"
        new_operation_var = self.generate_new_variable()
        new_operation = f"{new_operation_var} = {self.variables[self.current_var - 4]} + {self.variables[self.current_var - 3]} + {self.variables[self.current_var - 2]}"
        return f"{tuple_creation}\n{unpack_operation}\n{new_operation}"

    def generate_randomized_data_structures(self) -> str:
        prev_var = self.get_last_variable()
        curr_var = self.generate_new_variable()
        list_var = self.generate_new_variable()
        list_creation = (
            f"{curr_var} = [{prev_var} for _ in range({self.rng.randint(5, 10)})]"
        )
        shuffle_operation = f"random.shuffle({curr_var})"
        access_operation = f"{list_var} = random.choice({curr_var})"
        return f"{list_creation}\n{shuffle_operation}\n{access_operation}"

    def generate_loop_with_break_continue(self) -> str:
        prev_var = self.get_last_variable()
        curr_var = self.generate_new_variable()
        loop_type = self.rng.choice(["break", "continue"])
        condition = self.rng.randint(1, 5)

        loop_body = (
            f"if _ == {condition}:\n"
            + f"    {curr_var} += {prev_var}\n"
            + f"    {loop_type}"
        )

        return (
            f"{curr_var} = ''\n"
            + f"for _ in range({self.rng.randint(5, 10)}):\n"
            + f"{textwrap.indent(loop_body, '    ')}"
        )

    def generate_if_else_elif(self) -> str:
        prev_var = self.get_last_variable()
        non_tainted_var = self.generate_new_variable()
        curr_var = self.generate_new_variable()

        non_tainted_initialization = f'{non_tainted_var} = "non_tainted"'
        valid_branch = self.rng.randint(1, 3)

        if valid_branch == 1:
            conditions = [
                ("if random.random() < 0.5:", f"{curr_var} = {non_tainted_var}"),
                (
                    "elif random.random() < 0.5:",
                    f"{curr_var} = {prev_var} + {non_tainted_var}",
                ),
                ("else:", f"{curr_var} = {non_tainted_var}"),
            ]
        elif valid_branch == 2:
            conditions = [
                (
                    "if random.random() < 0.5:",
                    f"{curr_var} = {prev_var} + {non_tainted_var}",
                ),
                ("elif random.random() < 0.5:", f"{curr_var} = {non_tainted_var}"),
                ("else:", f"{curr_var} = {non_tainted_var}"),
            ]
        else:
            conditions = [
                ("if random.random() < 0.5:", f"{curr_var} = {non_tainted_var}"),
                ("elif random.random() < 0.5:", f"{curr_var} = {non_tainted_var}"),
                ("else:", f"{curr_var} = {prev_var} + {non_tainted_var}"),
            ]

        if_else_elif_statements = "\n".join(
            [
                f"{condition}\n{textwrap.indent(action, '    ')}"
                for condition, action in conditions
            ]
        )
        return f"{non_tainted_initialization}\n{if_else_elif_statements}"

    def generate_nested_loops(self) -> str:
        prev_var = self.get_last_variable()
        curr_var = self.generate_new_variable()
        loop_body = f"{curr_var} += {prev_var}"

        return (
            f"{curr_var} = ''\n"
            + f"for _ in range({self.rng.randint(2, 5)}):\n"
            + f"    for __ in range({self.rng.randint(2, 5)}):\n"
            + f"        {textwrap.indent(loop_body, '        ')}"
        )

    def generate_try_except(self) -> str:
        prev_var = self.get_last_variable()
        curr_var = self.generate_new_variable()
        try_block = (
            f"{curr_var} = str(int({prev_var}))\n"
            + "    print('Conversion successful')\n"
            + f"    print({curr_var})"
        )
        except_block = "print('Conversion failed')"
        return (
            f"try:\n{textwrap.indent(try_block, '    ')}\n"
            + f"except ValueError:\n{textwrap.indent(except_block, '    ')}"
        )

    def generate_function_chain(self, num_functions: int) -> str:
        function_definitions = []
        prev_var = self.get_last_variable()

        for _ in range(num_functions):
            func_name = self.generate_new_variable()
            func_def = f"def {func_name}():\n    return {prev_var}"
            function_definitions.append(func_def)
            prev_var = f"{func_name}()"

        last_var = self.generate_new_variable()
        final_assignment = f"{last_var} = {prev_var}"
        return "\n".join(function_definitions) + "\n" + final_assignment

    def generate_for_loop_with_if(self, depth: int = 1) -> str:
        indent = "    "
        prev_var = self.get_last_variable()
        curr_var = self.generate_new_variable()
        loop_body = (
            f"{indent}if _ != 1:\n"
            + f"{indent}    {curr_var} += {prev_var}\n"
            + f"{indent}else:\n"
            + f"{indent}    {curr_var} += '1'\n"
        )

        if depth > 1:
            nested_loop = self.generate_for_loop_with_if(depth - 1)
            loop_body = f"{nested_loop}\n{loop_body}"

        return (
            f"{curr_var} = ''\n"
            + f"for _ in range({self.rng.randint(2, 5)}):\n"
            + loop_body
        )

    def generate_statements(
        self,
        number_statements: int,
        rng: Random,
    ) -> str:
        if number_statements < 2:
            raise ValueError(
                "number_statements should be at least 2 to include source and sink functions."
            )

        self.rng = rng

        # Full list of function generators
        function_generators = [
            lambda: self.generate_addition(rng.randint(1, 3)),
            lambda: self.generate_for_loop(rng.randint(1, 3)),
            lambda: self.generate_while_loop(rng.randint(1, 3)),
            lambda: self.generate_list(rng.randint(1, 3)),
            lambda: self.generate_dictionary(rng.randint(1, 3)),
            self.generate_set,
            self.generate_string_concatenation,
            self.generate_string_slicing,
            self.generate_string_formatting,
            self.generate_tuple_manipulation,
            self.generate_loop_with_break_continue,
            self.generate_if_else_elif,
            self.generate_nested_loops,
            self.generate_randomized_data_structures,
        ]

        if self.enable_known_false_negatives:
            # List of functions known to cause issues (false negatives/positives)
            function_generators.extend(
                [
                    lambda: self.generate_function_chain(rng.randint(1, 3)),
                ]
            )

        # Generate import statements first
        import_statements = self.generate_import_statements()

        # Generate source
        source_code = self.generate_source()

        # Select random functions, allowing for duplicates
        selected_functions = self.rng.choices(
            function_generators, k=number_statements - 2
        )  # -2 to account for source and sink
        generated_code = "\n".join(func() for func in selected_functions)
        sink_code = self.generate_sink()

        # Combine everything into the final code
        full_code = (
            f"{import_statements}\n"
            + f"{source_code}\n"
            + f"{generated_code}\n"
            + f"{sink_code}"
        )
        return full_code
