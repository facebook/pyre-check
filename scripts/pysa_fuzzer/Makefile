.PHONY: generate analyze

generate:
	python generate_code.py

analyze: generate
	python3 -m venv tutorial
	source tutorial/bin/activate && \
	pip3 install pyre-check fb-sapp django-stubs && \
	cd generated_files && \
	pyre analyze > analysis.json && \
	python analyze_output.py

clean:
	rm -rf tutorial generated_files
