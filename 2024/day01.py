from typing import List, Tuple
from collections import Counter

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

def total_distance(left: List[int], right: List[int]) -> List[int]:
    sorted_left = sorted(left)
    sorted_right = sorted(right)
    return (abs(l - r) for l, r in zip(sorted_left, sorted_right))

def similarity_score(left: List[int], right: List[int]) -> List[int]:
    """
    For each number in left list, multiplies it by its frequency in right list
    and sums all products.
    """
    # Create frequency counter for right list
    right_counts = Counter(right)

    return (num * right_counts[num] for num in left)

def main():
    left_list, right_list = parse_input('inputs/01.txt')
    result = sum(total_distance(left_list, right_list))
    print(f"Total distance: {result}")
    result = sum(similarity_score(left_list, right_list))
    print(f"Similarity score: {result}")

if __name__ == "__main__":
    main()
