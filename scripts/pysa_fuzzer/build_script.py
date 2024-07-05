import subprocess
import os
import json
import argparse
from fuzzer2 import CodeGenerator

def run_command(command):
    process = subprocess.Popen(command, shell=True)
    process.communicate()

def generate_python_files():
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

def setup_virtual_environment():
    run_command('python3 -m venv tutorial')
    run_command('. tutorial/bin/activate && pip3 install pyre-check')

def configure_and_analyze():
    os.makedirs('generated_files', exist_ok=True)
    with open('generated_files/.pyre_configuration', 'w') as config_file:
        config = {
            "site_package_search_strategy": "pep561",
            "source_directories": [ "." ],
            "taint_models_path": [ "." ],
            "search_path": [ "../../../stubs/" ],
            "exclude": [ ".*/integration_test/.*" ]
        }
        json.dump(config, config_file, indent=2)

    with open('generated_files/sources_sinks.pysa', 'w') as pysa_file:
        pysa_file.write('def input() -> TaintSource[CustomUserControlled]: ...\n')
        pysa_file.write('def print(*__args: TaintSink[CodeExecution], **__kwargs): ...\n')

    with open('generated_files/taint.config', 'w') as taint_config_file:
        taint_config = {
            "sources": [
                { "name": "CustomUserControlled", "comment": "use to annotate user input" }
            ],
            "sinks": [
                { "name": "CodeExecution", "comment": "use to annotate execution of python code" }
            ],
            "features": [],
            "rules": [
                {
                    "name": "Possible RCE:",
                    "code": 5001,
                    "sources": [ "CustomUserControlled" ],
                    "sinks": [ "CodeExecution" ],
                    "message_format": "User specified data may reach a code execution sink"
                }
            ]
        }
        json.dump(taint_config, taint_config_file, indent=2)

    run_command('. tutorial/bin/activate && cd generated_files && pyre analyze > analysis_output.tmp')

def find_undetected_files():
    # Load the analysis output from the file
    with open('generated_files/analysis_output.tmp', 'r') as file:
        analysis_output = json.load(file)

    # Extract the list of files where the flow has been detected
    detected_files = set()
    for entry in analysis_output:
        detected_files.add(entry['path'])

    # Generate the full list of files (from 1.py to 100.py)
    all_files = {f"{i}.py" for i in range(1, 101)}

    # Find the files where the flow has not been detected
    undetected_files = all_files - detected_files

    # Output the list of undetected files
    print("Files where the flow has not been detected:")
    for file in sorted(undetected_files, key=lambda x: int(x.split('.')[0])):
        print(file)

    # Calculate and print the percentage of undetected files
    total_files = len(all_files)
    undetected_count = len(undetected_files)
    undetected_percentage = (undetected_count / total_files) * 100
    print(f"Flow has not been detected in {undetected_percentage:.2f}% of the files")

def clean_detected_files():
    # Load the analysis output from the file
    with open('generated_files/analysis_output.tmp', 'r') as file:
        analysis_output = json.load(file)

    # Extract the list of files where the flow has been detected
    detected_files = set()
    for entry in analysis_output:
        detected_files.add(entry['path'])

    # Remove detected files
    for file in detected_files:
        if os.path.exists(file):
            os.remove(file)
            print(f"Removed {file}")

def clean_up():
    run_command('rm -rf tutorial')
    run_command('rm -rf generated_files')

def main():
    parser = argparse.ArgumentParser(description="Build script with setup, analysis, and cleanup.")
    parser.add_argument('action', choices=['all', 'clean', 'clean_detected'], help="Action to perform")

    args = parser.parse_args()

    if args.action == 'all':
        generate_python_files()
        setup_virtual_environment()
        configure_and_analyze()
        find_undetected_files()
    elif args.action == 'clean':
        clean_up()
    elif args.action == 'clean_detected':
        clean_detected_files()

if __name__ == "__main__":
    main()
