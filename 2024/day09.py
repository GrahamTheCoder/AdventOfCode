"""
Reads the puzzle input from inputs/09.txt, parses it into a disk model,
compacts the disk according to the amphipod's process, then computes
and prints the resulting checksum.
"""

from typing import List, Optional


def parse_input(filename: str) -> List[Optional[int]]:
    """
    Parse the input file to create a list representing each block:
    - disk[i] = file ID (int) if occupied
    - disk[i] = None if free
    """
    with open(filename, 'r', encoding='utf-8') as f:
        raw = f.read().strip()

    disk = []
    current_file_id = 0
    # Even indices in the string => file length, odd => free length
    for i, digit in enumerate(raw):
        length = int(digit)
        if i % 2 == 0:
            # Fill 'length' slots with current_file_id
            for _ in range(length):
                disk.append(current_file_id)
            current_file_id += 1
        else:
            # Fill 'length' slots with None
            for _ in range(length):
                disk.append(None)
    return disk

def compact_disk(disk: List[Optional[int]]) -> None:
    """
    Repeatedly move the rightmost occupied block to the leftmost free
    position until no free block is to the left of any file block.
    """
    while True:
        # Find rightmost occupied
        rightmost = None
        for i in range(len(disk) - 1, -1, -1):
            if disk[i] is not None:
                rightmost = i
                break

        if rightmost is None:
            # No file blocks at all
            return

        # Find leftmost free
        leftmost = None
        for j in range(len(disk)):
            if disk[j] is None:
                leftmost = j
                break

        if leftmost is None or leftmost > rightmost:
            # Either no free blocks or free blocks are all to the right
            return

        # Move one block
        disk[leftmost] = disk[rightmost]
        disk[rightmost] = None

def compute_checksum(disk: List[Optional[int]]) -> int:
    """
    Compute the checksum: sum of (index * file ID) for each occupied block.
    """
    total = 0
    for i, block in enumerate(disk):
        if block is not None:
            total += i * block
    return total

def solve(disk: List[Optional[int]]) -> int:
    compact_disk(disk)
    return compute_checksum(disk)

if __name__ == "__main__":
    disk_data = parse_input("inputs/09.txt")
    result = solve(disk_data)
    print(result)