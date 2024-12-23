import itertools

def parse_input(file_path):
    with open(file_path, 'r') as file:
        codes = file.read().strip().split()
    return codes

def calculate_complexity(sequence, code):
    numeric_part = int(code[:-1])
    return len(sequence) * numeric_part

def find_shortest_sequence(code):
    # Placeholder for the actual logic to find the shortest sequence
    # This should be replaced with the actual implementation
    return "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A"

def solve(file_path):
    codes = parse_input(file_path)
    total_complexity = 0
    for code in codes:
        sequence = find_shortest_sequence(code)
        complexity = calculate_complexity(sequence, code)
        total_complexity += complexity
    return total_complexity

if __name__ == "__main__":
    input_file = 'inputs/21.txt'
    print(solve(input_file))
