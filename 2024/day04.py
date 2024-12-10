def parse_input(file_path):
    with open(file_path, 'r') as file:
        grid = [list(line.strip()) for line in file.readlines()]
    return grid

def find_xmas_in_grid(grid):
    def is_xmas_at(x, y):
        # Check all 8 possible orientations of "MAS" in the shape of an X
        patterns = [
            [(0, 0), (1, -1), (2, -2), (1, 1), (2, 2)],  # M.A.S / S.A.M
            [(0, 0), (1, 1), (2, 2), (1, -1), (2, -2)],  # M.A.S / S.A.M
            [(0, 0), (-1, -1), (-2, -2), (-1, 1), (-2, 2)],  # M.A.S / S.A.M
            [(0, 0), (-1, 1), (-2, 2), (-1, -1), (-2, -2)],  # M.A.S / S.A.M
            [(0, 0), (1, -1), (2, -2), (-1, -1), (-2, -2)],  # M.A.S / S.A.M
            [(0, 0), (1, 1), (2, 2), (-1, 1), (-2, 2)],  # M.A.S / S.A.M
            [(0, 0), (-1, -1), (-2, -2), (1, -1), (2, -2)],  # M.A.S / S.A.M
            [(0, 0), (-1, 1), (-2, 2), (1, 1), (2, 2)]  # M.A.S / S.A.M
        ]
        for pattern in patterns:
            if all(0 <= x + dx < len(grid) and 0 <= y + dy < len(grid[0]) and grid[x + dx][y + dy] == 'M' for dx, dy in pattern[::3]) and \
               all(0 <= x + dx < len(grid) and 0 <= y + dy < len(grid[0]) and grid[x + dx][y + dy] == 'A' for dx, dy in pattern[1::3]) and \
               all(0 <= x + dx < len(grid) and 0 <= y + dy < len(grid[0]) and grid[x + dx][y + dy] == 'S' for dx, dy in pattern[2::3]):
                return True
        return False

    count = 0
    for x in range(len(grid)):
        for y in range(len(grid[0])):
            if is_xmas_at(x, y):
                count += 1

    return count

def solve(file_path):
    grid = parse_input(file_path)
    return find_xmas_in_grid(grid)

# Example usage:
file_path = "inputs/04.txt"
print(solve(file_path))