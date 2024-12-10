import re

def parse_memory(file_path):
    with open(file_path, 'r') as file:
        data = file.read()
        
    # Regular expression to extract valid mul instructions
    pattern = r'mul\((\d{1,3}),(\d{1,3})\)'
    matches = re.findall(pattern, data)
    
    # Convert matches to list of tuples with integers
    instructions = [(int(x), int(y)) for x, y in matches]
    return instructions

def solve(instructions):
    total_sum = 0
    for x, y in instructions:
        total_sum += x * y
    return total_sum

if __name__ == "__main__":
    file_path = 'inputs/03.txt'
    instructions = parse_memory(file_path)
    result = solve(instructions)
    print(result)
