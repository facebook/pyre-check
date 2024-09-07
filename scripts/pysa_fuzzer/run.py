import subprocess
import os
import json
import argparse
from code_generator import CodeGenerator
from pathlib import Path
import shutil
import logging

logging.basicConfig(level=logging.INFO)

def run_command(command, output_file=None):
    try:
        result = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, check=True)
        if output_file:
            with open(output_file, 'w') as file:
                file.write(result.stdout)
    except subprocess.CalledProcessError as e:
        logging.error(f"Command '{command}' failed with error: {e.stderr}")

def generate_python_files(num_files, num_statements):
    generator = CodeGenerator()
    output_dir = Path('generated_files')

    output_dir.mkdir(exist_ok=True)

    filenames = []
    for i in range(1, num_files + 1):
        generator = CodeGenerator()
        generated_code = generator.generate_statements(num_statements)
        filename = output_dir / f'test_{i}.py'
        with open(filename, 'w') as file:
            file.write(generated_code)
        filenames.append(str(filename))
        logging.info(f"Generated {filename}")

    # Save the filenames to a temporary file
    with open('filenames.tmp', 'w') as tmp_file:
        json.dump(filenames, tmp_file)

def configure_and_analyze():
    with open('.pyre_configuration', 'w') as config_file:
        config = {
            "site_package_search_strategy": "pep561",
            "source_directories": [ "./generated_files" ],
            "taint_models_path": [ "." ],
            "search_path": [ "../../stubs/" ],
            "exclude": [ ".*/integration_test/.*" ]
        }
        json.dump(config, config_file, indent=2)

    with open('sources_sinks.pysa', 'w') as pysa_file:
        pysa_file.write('def input() -> TaintSource[CustomUserControlled]: ...\n')
        pysa_file.write('def print(*__args: TaintSink[CodeExecution], **__kwargs): ...\n')

    with open('taint.config', 'w') as taint_config_file:
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

def run_pyre():
    logging.info("Please wait a minute or two for Pysa to Run!")
    run_command(['pyre', '-n', 'analyze'], output_file='analysis_output.tmp')

def find_undetected_files():
    # Load the filenames from the temp file
    try:
        with open('../filenames.tmp', 'r') as tmp_file:
            filenames = json.load(tmp_file)
    except FileNotFoundError:
        logging.error("Temporary file with filenames not found.")
        return

    # Load the analysis output from the file
    try:
        with open('../analysis_output.tmp', 'r') as file:
            analysis_output = json.load(file)
    except FileNotFoundError:
        logging.error("Analysis output file not found.")
        return

    # Extract the list of files where the flow has been detected
    detected_files = set()
    for entry in analysis_output:
        detected_files.add(os.path.basename(entry['path']))  # Normalize the path to get the filename only

    # Normalize filenames to get the filename only
    all_files = {os.path.basename(filename) for filename in filenames}

    # Find the files where the flow has not been detected
    undetected_files = all_files - detected_files

    # Output the list of undetected files
    logging.info("Files where the flow has not been detected:")
    for file in sorted(undetected_files, key=lambda x: int(x[len('test_'):-len('.py')])):
        logging.info(file)

    # Calculate and print the percentage of undetected files
    total_files = len(all_files)
    undetected_count = len(undetected_files)
    undetected_percentage = (undetected_count / total_files) * 100
    logging.info(f"Flow has not been detected in {undetected_percentage:.2f}% of the files")

def clean_up():
    files_to_remove = ['sources_sinks.pysa', 'taint.config', '.pyre_configuration', 'analysis_output.tmp', 'filenames.tmp']
    for file in files_to_remove:
        try:
            os.remove(file)
        except FileNotFoundError:
            logging.warning(f"{file} not found for cleanup.")

    dirs_to_remove = ['generated_files', '__pycache__']
    for dir in dirs_to_remove:
        shutil.rmtree(dir, ignore_errors=True)

def main():
    parser = argparse.ArgumentParser(description="Build script with setup, analysis, and cleanup.")
    parser.add_argument('action', choices=['all', 'find-undetected', 'clean'], help="Action to perform")
    parser.add_argument('--num-files', type=int, default=100, help="Number of files to generate")
    parser.add_argument('--num-statements', type=int, default=20, help="Number of statements to generate in each file")

    args = parser.parse_args()

    if args.action == 'all':
        generate_python_files(args.num_files, args.num_statements)
        configure_and_analyze()
        run_pyre()
    elif args.action == "find-undetected":
        find_undetected_files()
    elif args.action == 'clean':
        clean_up()

if __name__ == "__main__":
    main()
