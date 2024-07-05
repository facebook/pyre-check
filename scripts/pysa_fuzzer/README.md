# Python Script Tutorial

This tutorial will guide you on how to use the provided Python script to generate Python files, configure the analysis environment, run the analysis, and clean up the configuration and temporary files.

## Prerequisites

Before you start, make sure you have the following:

- Python installed on your system.
- The `fuzzer2` module installed and properly set up.
- Any other dependencies required by the script.

## Usage

The script accepts three main actions and two optional arguments. Here's a detailed explanation of each:

### Actions

1. **`all`**: Generates Python files, configures the analysis environment, and runs the analysis.
2. **`analyze`**: Analyzes the generated files and identifies the ones where the flow has not been detected.
3. **`clean`**: Cleans up the configuration and temporary files.

### Optional Arguments

- **`--num_files`**: Specifies the number of files to generate (default is 100).
- **`--x`**: Specifies the number of functions to generate in each file (default is 20).

### Examples

#### 1. Generate Files, Configure, and Run Analysis

```bash
python script_name.py all --num_files 50 --x 10
```
next we need to cd into the generated_files directory. 



```bash
cd generated_files
```

next we can do 

```bash
python3 ../build_script.py analyze
```


