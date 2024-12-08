from typing import List, Tuple

def parse_input(filename: str) -> Tuple[List[int], List[int]]:
    """
    Parses input file containing two columns of numbers into two lists.
    
    Args:
        filename: Path to input file
        
    Returns:
        Tuple of two lists containing the left and right numbers
    """
    left_list = []
    right_list = []

    with open(filename, 'r', encoding='utf-8') as f:
        for line in f:
            left, right = line.strip().split()
            left_list.append(int(left))
            right_list.append(int(right))

    return left_list, right_list

def calculate_total_distance(left: List[int], right: List[int]) -> int:
    sorted_left = sorted(left)
    sorted_right = sorted(right)
    return sum(abs(l - r) for l, r in zip(sorted_left, sorted_right))

def main():
    left_list, right_list = parse_input('inputs/01.txt')
    result = calculate_total_distance(left_list, right_list)
    print(f"Total distance: {result}")

if __name__ == "__main__":
    main()
