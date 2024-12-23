def parse_input(file_path):
    with open(file_path, 'r') as file:
        lines = file.read().strip().split('\n')
    
    towel_patterns = lines[0].split(', ')
    designs = lines[2:]
    
    return towel_patterns, designs

def count_ways_to_create_design(towel_patterns, design, memo):
    if design in memo:
        return memo[design]
    if not design:
        return 1
    total_ways = 0
    for pattern in towel_patterns:
        if design.startswith(pattern):
            total_ways += count_ways_to_create_design(towel_patterns, design[len(pattern):], memo)
    memo[design] = total_ways
    return total_ways

def count_possible_designs(towel_patterns, designs):
    count = 0
    memo = {}
    for design in designs:
        if count_ways_to_create_design(towel_patterns, design, memo) > 0:
            count += 1
    return count

def sum_all_possible_ways(towel_patterns, designs):
    total_ways = 0
    memo = {}
    for design in designs:
        total_ways += count_ways_to_create_design(towel_patterns, design, memo)
    return total_ways

if __name__ == "__main__":
    towel_patterns, designs = parse_input('inputs/19.txt')
    result_part1 = count_possible_designs(towel_patterns, designs)
    result_part2 = sum_all_possible_ways(towel_patterns, designs)
    print(f"Part 1: {result_part1}")
    print(f"Part 2: {result_part2}")
