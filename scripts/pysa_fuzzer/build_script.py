import subprocess
import os
import json

def run_command(command):
    process = subprocess.Popen(command, shell=True)
    process.communicate()

def generate_python_files():
    run_command('python generate_code.py')

def setup_virtual_environment():
    run_command('python3 -m venv tutorial')
    run_command('. tutorial/bin/activate && pip3 install pyre-check fb-sapp django-stubs')

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

def run_report():
    run_command('. tutorial/bin/activate && python find_undetected_files.py')

def clean_up():
    run_command('rm -rf tutorial')
    run_command('rm -rf generated_files')

def main():
    generate_python_files()
    setup_virtual_environment()
    configure_and_analyze()
    run_report()

if __name__ == "__main__":
    main()
