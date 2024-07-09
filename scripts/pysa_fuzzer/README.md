
# Python Script Tutorial

This tutorial will guide you on how to use the provided Python script to generate complex python code with valid flow, configure the analysis environment, run the analysis, and clean up the configuration and temporary files.

## Prerequisites

Before you start, make sure you have the following:

- Python installed on your system.
- The `code_generator` module installed and properly set up.
- The `pyre-check` module installed and properly setup. 

## Usage

The script accepts three main actions and two optional arguments. Here's a detailed explanation of each:

### Actions

1. **`all`**: Generates Python files, configures the analysis environment, and runs the analysis.
2. **`analyze`**: Analyzes the generated files and identifies the ones where the flow has not been detected.
3. **`clean`**: Cleans up the configuration and temporary files.

### Optional Arguments

- **`--num-files`**: Specifies the number of files to generate (default is 100).
- **`--num-statements`**: Specifies the number of statements to generate in each file (default is 20).

### Examples

#### 1. Generate Files, Configure, and Run Analysis

To generate 50 files with 10 statements each, configure the environment, and run the analysis:

\`\`\`bash
python run.py all --num-files 50 --num-statements 10
\`\`\`

#### 2. Analyze the Generated Files

After generating the files and running the initial analysis, you can analyze the generated files to identify the ones where the flow has not been detected:

\`\`\`bash
python run.py analyze
\`\`\`

#### 3. Clean Up Configuration and Temporary Files

To clean up the configuration and temporary files created during the process:

\`\`\`bash
python run.py clean
\`\`\`
