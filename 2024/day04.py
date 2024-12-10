def parse_grid(file_path):
    with open(file_path, 'r') as file:
        grid = [line.strip() for line in file.readlines()]
    return grid

def count_xmas_patterns(grid):
    rows = len(grid)
    cols = len(grid[0])
    count = 0

    for i in range(1, rows - 1):
        for j in range(1, cols - 1):
            if grid[i][j] == 'A':
                # Check all possible combinations using XOR logic
                if ((grid[i-1][j-1] == 'M' and grid[i+1][j+1] == 'S') ^ 
                    (grid[i-1][j+1] == 'M' and grid[i+1][j-1] == 'S') ^
                    (grid[i-1][j-1] == 'S' and grid[i+1][j+1] == 'M') ^
                    (grid[i-1][j+1] == 'S' and grid[i+1][j-1] == 'M')):
                    count += 1
    return count

def main(file_path):
    grid = parse_grid(file_path)
    result = count_xmas_patterns(grid)
    print(f"Number of X-MAS patterns: {result}")

# Example usage:
if __name__ == "__main__":
    main('inputs/00.txt')