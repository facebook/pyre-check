import json

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
