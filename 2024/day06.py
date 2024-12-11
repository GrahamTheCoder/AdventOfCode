def parse_input(file_path):
    with open(file_path, 'r') as file:
        lines = file.readlines()
    
    map_grid = []
    guard_position = None
    guard_direction = None
    
    for y, line in enumerate(lines):
        row = list(line.strip())
        map_grid.append(row)
        for x, char in enumerate(row):
            if char in '^>v<':
                guard_position = (x, y)
                guard_direction = char
                row[x] = '.'
    
    return map_grid, guard_position, guard_direction

def move_guard(map_grid, guard_position, guard_direction):
    direction_vectors = {
        '^': (0, -1),
        '>': (1, 0),
        'v': (0, 1),
        '<': (-1, 0)
    }
    
    turn_right = {
        '^': '>',
        '>': 'v',
        'v': '<',
        '<': '^'
    }
    
    visited_positions = set()
    x, y = guard_position
    visited_positions.add((x, y))
    
    while True:
        dx, dy = direction_vectors[guard_direction]
        new_x, new_y = x + dx, y + dy
        
        if 0 <= new_x < len(map_grid[0]) and 0 <= new_y < len(map_grid):
            if map_grid[new_y][new_x] == '.':
                x, y = new_x, new_y
                visited_positions.add((x, y))
            else:
                guard_direction = turn_right[guard_direction]
        else:
            break
    
    return visited_positions

def count_distinct_positions(file_path):
    map_grid, guard_position, guard_direction = parse_input(file_path)
    visited_positions = move_guard(map_grid, guard_position, guard_direction)
    return len(visited_positions)

if __name__ == "__main__":
    file_path = 'inputs/06.txt'
    result = count_distinct_positions(file_path)
    print(result)