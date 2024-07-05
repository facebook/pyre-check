import subprocess
import os
import json
import argparse
import time
from fuzzer2 import CodeGenerator

def run_command(command, output_file=None):
    result = subprocess.run(command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, check=True)
    if output_file:
        with open(output_file, 'w') as file:
            file.write(result.stdout)

def generate_python_files(num_files, x):
    generator = CodeGenerator()
    output_dir = 'generated_files'

    # Create the directory if it doesn't exist
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)

    for i in range(1, num_files + 1):
        generated_code = generator.generate_statements(x)
        generator.reset()
        filename = os.path.join(output_dir, f'test_{i}.py')
        with open(filename, 'w') as file:
            file.write(generated_code)
        print(f"Generated {filename}")

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
    print("Please wait a minute or two for Pysa to Run!")
    run_command(['pyre', '-n', 'analyze'], output_file='analysis_output.tmp')


