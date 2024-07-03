import re

def main():
    # Read the analysis output
    with open('analysis_output.txt', 'r') as f:
        analysis_output = f.read()
    
    # Extract mentioned files using regex
    mentioned_files = set(re.findall(r'path":\s*"(.*?)"', analysis_output))
    all_files = {f"{i}.py" for i in range(1, 101)}
    files_not_found = sorted(all_files - mentioned_files)
    
    # Print the result
    print("Flow not found in")
    print(files_not_found)

if __name__ == "__main__":
    main()
