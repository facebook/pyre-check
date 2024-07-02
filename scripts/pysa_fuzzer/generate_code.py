import os
from fuzzer2 import CodeGenerator  

def main():
    generator = CodeGenerator()
    num_files = 100  # Change this number to generate a different amount of files
    x = 20  # Change this number to generate a different amount of functions
    output_dir = 'generated_files'

    # Create the directory if it doesn't exist
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)

    for i in range(1, num_files + 1):
        generated_code = generator.generate_statements(x)
        generator.reset()
        filename = os.path.join(output_dir, f'{i}.py')
        with open(filename, 'w') as file:
            file.write(generated_code)
        print(f"Generated {filename}")

if __name__ == "__main__":
    main()
