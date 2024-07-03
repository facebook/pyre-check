import json

def main():
    with open('generated_files/analysis.json', 'r') as f:
        analysis_results = json.load(f)
    
    mentioned_files = {result['path'] for result in analysis_results}
    all_files = {f"{i}.py" for i in range(1, 101)}
    files_not_found = sorted(all_files - mentioned_files)
    
    print("Flow not found in")
    print(files_not_found)

if __name__ == "__main__":
    main()
