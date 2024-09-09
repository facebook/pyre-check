# Pysa Fuzzer

The Pysa Fuzzer is a tool to randomly generate code with valid taint flows, and run Pysa against it to verify whether Pysa finds the flow. This is a tool to find false negatives in Pysa. The following instructions will guide you on how to run the scripts.

## Prerequisites

Before you start, make sure you have the following:

- Python installed on your system.
- pyre-check installed and properly setup.

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

```bash
python run.py all --num-files 50 --num-statements 10
```

#### 2. Analyze the Generated Files

After generating the files and running the initial analysis, you can analyze the generated files to identify the ones where the flow has not been detected:

```bash
python run.py analyze
```

#### 3. Clean Up Configuration and Temporary Files

To clean up the configuration and temporary files created during the process:

```bash
python run.py clean
```

## Detailed Steps

1. **Generate Files, Configure, and Run Analysis**

   Run the following command to generate the specified number of files and statements, configure the analysis environment, and run the analysis:

   ```bash
   python run.py all --num-files 50 --num-statements 10
   ```

2. **Change Directory to Generated Files**

   Navigate to the directory where the generated files are stored:

   ```bash
   cd generated_files
   ```

3. **Analyze the Generated Files**

   Execute the following command to analyze the generated files:

   ```bash
   python3 ../run.py analyze
   ```

4. **Clean up**

   Execute the following command to clean up:

   ```bash
   cd ..
   python3 run.py clean
   ```

By following these steps, you can efficiently generate, configure, analyze, and clean up your Python files using the provided script.
