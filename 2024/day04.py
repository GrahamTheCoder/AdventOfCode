def parse_input(file_path):
    with open(file_path, 'r') as file:
        grid = [list(line.strip()) for line in file.readlines()]
    return grid

def find_word_in_grid(grid, word):
    def search_from_position(x, y, dx, dy):
        for i in range(len(word)):
            if not (0 <= x + i * dx < len(grid) and 0 <= y + i * dy < len(grid[0])):
                return False
            if grid[x + i * dx][y + i * dy] != word[i]:
                return False
        return True

    word_length = len(word)
    count = 0

    for x in range(len(grid)):
        for y in range(len(grid[0])):
            # Check all 8 possible directions
            directions = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)]
            for dx, dy in directions:
                if search_from_position(x, y, dx, dy):
                    count += 1

    return count

def solve(file_path):
    grid = parse_input(file_path)
    word = "XMAS"
    return find_word_in_grid(grid, word)

# Example usage:
file_path = "inputs/04.txt"
print(solve(file_path))