import re

def parse_memory(file_path):
    with open(file_path, 'r') as file:
        data = file.read()
        
    # Regular expression to extract valid mul, do, and don't instructions
    pattern = r'mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don\'t\(\)'
    matches = re.findall(pattern, data)
    
    instructions = []
    for match in matches:
        if match[0] and match[1]:
            instructions.append(('mul', int(match[0]), int(match[1])))
        elif match[0] == 'do':
            instructions.append(('do',))
        elif match[0] == 'don\'t':
            instructions.append(('don\'t',))
    return instructions

def solve(instructions):
    total_sum = 0
    mul_enabled = True
    
    for instruction in instructions:
        if instruction[0] == 'mul':
            if mul_enabled:
                total_sum += instruction[1] * instruction[2]
        elif instruction[0] == 'do':
            mul_enabled = True
        elif instruction[0] == 'don\'t':
            mul_enabled = False
    
    return total_sum

if __name__ == "__main__":
    file_path = 'inputs/03-example.txt'
    instructions = parse_memory(file_path)
    result = solve(instructions)
    print(result)
