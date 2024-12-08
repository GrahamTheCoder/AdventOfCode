def parse_input(filename):
    with open(filename, encoding='utf-8') as f:
        return [list(map(int, line.strip().split())) for line in f]

def is_safe_report(levels):
    if len(levels) < 2:
        return True

    diffs = [b - a for a, b in zip(levels, levels[1:])]

    # Check if all differences are between 1 and 3 or -3 and -1
    valid_diffs = all(1 <= abs(d) <= 3 for d in diffs)

    # Check if all differences have the same sign
    all_increasing = all(d > 0 for d in diffs)
    all_decreasing = all(d < 0 for d in diffs)

    return valid_diffs and (all_increasing or all_decreasing)

def is_safe_with_dampener(levels):
    if is_safe_report(levels):
        return True
        
    # Try removing each level one at a time
    for i in range(len(levels)):
        modified_levels = levels[:i] + levels[i+1:]
        if is_safe_report(modified_levels):
            return True
            
    return False

def solve_part1(reports):
    return sum(is_safe_report(report) for report in reports)

def solve_part2(reports):
    return sum(is_safe_with_dampener(report) for report in reports)

def main():
    reports = parse_input('inputs/02.txt')
    print(f"Part 1: {solve_part1(reports)}")
    print(f"Part 2: {solve_part2(reports)}")

if __name__ == "__main__":
    main()