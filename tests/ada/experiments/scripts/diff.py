import difflib

def compare_files(file1_path, file2_path):
    try:
        with open(file1_path, 'r') as file1, open(file2_path, 'r') as file2:
            file1_lines = file1.readlines()
            file2_lines = file2.readlines()

            if file1_lines == file2_lines:
                print("Success: The files are identical.")
            else:
                print("The files are different. Here are the differences:")
                for line in difflib.unified_diff(file1_lines, file2_lines, fromfile=file1_path, tofile=file2_path, lineterm=''):
                    print(line)
    except FileNotFoundError as e:
        print(f"Error: {e}")

# Example usage
file1_path = 'state.txt'
file2_path = 'state.txt'
compare_files(file1_path, file2_path)
