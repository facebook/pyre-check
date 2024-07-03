# Generate Python files
generate:
	python generate_code.py

# Set up Python virtual environment and install dependencies
setup:
	python3 -m venv tutorial
	. tutorial/bin/activate && \
	pip3 install pyre-check fb-sapp django-stubs

# Configure Pyre and perform analysis
analyze:
	. tutorial/bin/activate && \
	cd generated_files && \
	printf '{\n  "site_package_search_strategy": "pep561",\n  "source_directories": [ "." ],\n  "taint_models_path": [ "." ],\n  "search_path": [ "../../../stubs/" ],\n  "exclude": [ ".*/integration_test/.*" ]\n}' > .pyre_configuration && \
	printf 'def input() -> TaintSource[CustomUserControlled]: ...\ndef print(*__args: TaintSink[CodeExecution], **__kwargs): ...\n' > sources_sinks.py && \
	printf 'def input(): TaintSource[CustomUserControlled] = ...\ndef print(*__args: TaintSink[CodeExecution], **__kwargs): ...\n' > sources_sinks.py && \
	printf '{\n  "sources": [\n    { "name": "CustomUserControlled", "comment": "use to annotate user input" }\n  ],\n  "sinks": [\n    { "name": "CodeExecution", "comment": "use to annotate execution of python code" }\n  ],\n  "features": [],\n  "rules": [\n    { "name": "Possible RCE:", "code": 5001, "sources": [ "CustomUserControlled" ], "sinks": [ "CodeExecution" ], "message_format": "User specified data may reach a code execution sink" }\n  ]\n}' > taint.config && \
	pyre analyze

# Clean generated files and virtual environment
clean:
	rm -rf tutorial
	rm -rf generated_files

# Default target
all: generate setup analyze
