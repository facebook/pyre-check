import subprocess
import os
import json
import argparse
from pathlib import Path
import shutil
import logging
import random
from code_generator2 import CodeGenerator

logging.basicConfig(level=logging.INFO)

def run_command(command, output_file=None):
    try:
        result = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, check=True)
        if output_file:
            with open(output_file, 'w') as file:
                file.write(result.stdout)
    except subprocess.CalledProcessError as e:
        logging.error(f"Command '{command}' failed with error: {e.stderr}")

def generate_random_seed(num_files, num_mutations=48):
    return [random.randint(1, num_mutations) for _ in range(num_files)]

def apply_mutation(generator, mutation_number):
    if 1 <= mutation_number <= 24:
        # Apply source mutation
        mutation_method = getattr(generator, f"source_mutation_{mutation_number}")
        mutation_method()
    elif 25 <= mutation_number <= 48:
        # Apply sink mutation
        mutation_method = getattr(generator, f"sink_mutation_{mutation_number - 24}")
        mutation_method()

def generate_python_files(num_files, seed):
    generator = CodeGenerator()
    output_dir = Path('generated_files')

    output_dir.mkdir(exist_ok=True)

    filenames = []
    mutation_mapping = {}
    for i in range(num_files):
        mutation_number = seed[i]
        apply_mutation(generator, mutation_number)
        generated_code = generator.generate()
        filename = output_dir / f'test_{i+1}.py'
        with open(filename, 'w') as file:
            file.write("import random\n")
            file.write(generated_code)
        filenames.append(str(filename))
        mutation_mapping[filename.name] = mutation_number  # Map the filename to its mutation
        logging.info(f"Generated {filename} with mutation {mutation_number}")

    # Save the filenames to a temporary file
    with open('filenames.tmp', 'w') as tmp_file:
        json.dump(filenames, tmp_file)

    # Save the mutation mapping to a temporary file
    with open('mutation_mapping.tmp', 'w') as tmp_file:
        json.dump(mutation_mapping, tmp_file)

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

    # Load the mutation mapping from the temp file
    try:
        with open('../mutation_mapping.tmp', 'r') as tmp_file:
            mutation_mapping = json.load(tmp_file)
    except FileNotFoundError:
        logging.error("Temporary file with mutation mapping not found.")
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

    # Print the first undetected file and its mutation
    if undetected_files:
        first_undetected_file = sorted(undetected_files, key=lambda x: int(x[len('test_'):-len('.py')]))[0]
        mutation_number = mutation_mapping[first_undetected_file]
        logging.info(f"First undetected file: {first_undetected_file} with mutation {mutation_number}")

def clean_up():
    files_to_remove = ['sources_sinks.pysa', 'taint.config', '.pyre_configuration', 'analysis_output.tmp', 'filenames.tmp', 'mutation_mapping.tmp']
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

    args = parser.parse_args()

    if args.action == 'all':
        seed = generate_random_seed(args.num_files)
        print(f"Generated seed: {seed}")
        generate_python_files(args.num_files, seed)
        configure_and_analyze()
        run_pyre()
    elif args.action == "find-undetected":
        find_undetected_files()
    elif args.action == 'clean':
        clean_up()

if __name__ == "__main__":
    main()
