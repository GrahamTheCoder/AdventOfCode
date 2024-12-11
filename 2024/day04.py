def parse_grid(file_path):
    with open(file_path, 'r') as file:
        grid = [line.strip() for line in file.readlines()]
    return grid

# Hack depends on not having any other letters on the grid - could also have ordered the list of two and checked if it was equal to 'MS'
def is_ms(loc1, loc2):
    return ord(loc1) + ord(loc2) == ord('M') + ord('S')

def count_xmas_patterns(grid):
    rows = len(grid)
    cols = len(grid[0])
    count = 0

    for i in range(1, rows - 1):
        for j in range(1, cols - 1):
            if grid[i][j] == 'A':
                # Check all possible combinations using XOR logic
                if is_ms(grid[i-1][j-1], grid[i+1][j+1]) and is_ms(grid[i-1][j+1], grid[i+1][j-1]):
                    count += 1
    return count

def main(file_path):
    grid = parse_grid(file_path)
    result = count_xmas_patterns(grid)
    print(f"Number of X-MAS patterns: {result}")

# Example usage:
if __name__ == "__main__":
    main('inputs/04.txt')