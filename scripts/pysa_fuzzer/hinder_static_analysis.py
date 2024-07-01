# hindering_static_analysis.py

# 1. Indirect Function Calls
def example_indirect_function_calls():
    def source():
        return input()

    def sink(x):
        print(x)

    def indirect_call(f, arg):
        return f(arg)

    data = source()
    indirect_call(sink, data)

# 2. Use of exec and eval
def example_exec_eval():
    source_code = "a = input()"
    sink_code = "print(a)"
    exec(source_code)
    exec(sink_code)

# 3. Dynamic Attribute Access
def example_dynamic_attribute_access():
    class DynamicClass:
        def __init__(self, value):
            self.dynamic_attr = value

    obj = DynamicClass(input())
    print(getattr(obj, 'dynamic_attr'))

# 4. Use of Decorators
def example_decorators():
    def source_decorator(func):
        def wrapper():
            return func()
        return wrapper

    @source_decorator
    def source():
        return input()

    def sink(value):
        print(value)

    sink(source())

# 5. Closures and Nested Functions
def example_closures_nested_functions():
    def source():
        return input()

    def example():
        data = source()

        def inner_function():
            def inner_inner_function():
                print(data)
            inner_inner_function()
        
        inner_function()

    example()

# 6. Use of Global Variables
def example_global_variables():
    global global_data
    global_data = None

    def source():
        global global_data
        global_data = input()

    def sink():
        global global_data
        print(global_data)

    source()
    sink()

# 7. Encapsulation in Objects
def example_encapsulation_objects():
    class Encapsulator:
        def __init__(self, data):
            self.data = data

        def get_data(self):
            return self.data

    obj = Encapsulator(input())
    print(obj.get_data())

# 8. List and Dictionary Manipulations
def example_list_dict_manipulations():
    data = input()
    data_list = [data]
    data_dict = {"key": data_list[0]}
    print(data_dict["key"])

# 9. Obfuscated Control Flow
def example_obfuscated_control_flow():
    a = input()
    b = ''
    for _ in range(5):
        for __ in range(2):
            b += a
    print(b)

# 10. Function Wrappers and Partial Functions
def example_function_wrappers_partial():
    from functools import partial

    def func(a, b):
        return b

    a = input()
    partial_func = partial(func, b=a)
    print(partial_func(None))

if __name__ == "__main__":
    # Uncomment the example you want to run

    # example_indirect_function_calls()
    # example_exec_eval()
    # example_dynamic_attribute_access()
    # example_decorators()
    # example_closures_nested_functions()
    # example_global_variables()
    # example_encapsulation_objects()
    # example_list_dict_manipulations()
    # example_obfuscated_control_flow()
    # example_function_wrappers_partial()
    pass
