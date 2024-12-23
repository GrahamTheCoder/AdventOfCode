from collections import deque

def parse_input(file_path):
    with open(file_path, 'r') as file:
        byte_positions = [tuple(map(int, line.strip().split(','))) for line in file]
    return byte_positions

def simulate_falling_bytes(byte_positions, grid_size=71, num_bytes=1024):
    grid = [['.' for _ in range(grid_size)] for _ in range(grid_size)]
    for x, y in byte_positions[:num_bytes]:
        grid[y][x] = '#'
    return grid

def find_shortest_path(grid):
    directions = [(0, 1), (1, 0), (0, -1), (-1, 0)]
    start = (0, 0)
    end = (len(grid) - 1, len(grid) - 1)
    queue = deque([(start, 0)])
    visited = set([start])
    
    while queue:
        (x, y), steps = queue.popleft()
        if (x, y) == end:
            return steps
        for dx, dy in directions:
            nx, ny = x + dx, y + dy
            if 0 <= nx < len(grid) and 0 <= ny < len(grid) and grid[ny][nx] == '.' and (nx, ny) not in visited:
                visited.add((nx, ny))
                queue.append(((nx, ny), steps + 1))
    return -1  # If no path is found

def is_path_blocked(grid):
    return find_shortest_path(grid) == -1

def find_blocking_byte(byte_positions, grid_size=71):
    grid = [['.' for _ in range(grid_size)] for _ in range(grid_size)]
    for i, (x, y) in enumerate(byte_positions):
        grid[y][x] = '#'
        if is_path_blocked(grid):
            return x, y
    return None

def main():
    byte_positions = parse_input('inputs/18.txt')
    grid = simulate_falling_bytes(byte_positions)
    min_steps = find_shortest_path(grid)
    print(min_steps)
    
    blocking_byte = find_blocking_byte(byte_positions)
    if blocking_byte:
        print(f"{blocking_byte[0]},{blocking_byte[1]}")

if __name__ == "__main__":
    main()
