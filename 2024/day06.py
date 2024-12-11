class MapWithObstacle:
    def __init__(self, map_grid, obstacle_position=None):
        self.map_grid = map_grid
        self.obstacle_position = obstacle_position

    def is_obstacle(self, x, y):
        if self.obstacle_position and (x, y) == self.obstacle_position:
            return True
        return self.map_grid[y][x] != '.'

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

def move_guard(map_with_obstacle, guard_position, guard_direction):
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
        
        if 0 <= new_x < len(map_with_obstacle.map_grid[0]) and 0 <= new_y < len(map_with_obstacle.map_grid):
            if not map_with_obstacle.is_obstacle(new_x, new_y):
                x, y = new_x, new_y
                visited_positions.add((x, y))
            else:
                guard_direction = turn_right[guard_direction]
        else:
            break
    
    return visited_positions

def detect_loops(map_with_obstacle, guard_position, guard_direction):
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
    
    loop_positions = set()
    visited_positions = set()
    x, y = guard_position
    direction = guard_direction
    
    while True:
        if (x, y, direction) in visited_positions:
            loop_positions.add((x, y))
            break
        visited_positions.add((x, y, direction))
        
        dx, dy = direction_vectors[direction]
        new_x, new_y = x + dx, y + dy
        
        if 0 <= new_x < len(map_with_obstacle.map_grid[0]) and 0 <= new_y < len(map_with_obstacle.map_grid):
            if not map_with_obstacle.is_obstacle(new_x, new_y):
                x, y = new_x, new_y
            else:
                direction = turn_right[direction]
        else:
            break
    
    return loop_positions

def find_obstacle_positions_causing_loops(map_grid, guard_position, guard_direction):
    map_with_obstacle = MapWithObstacle(map_grid)
    visited_positions = move_guard(map_with_obstacle, guard_position, guard_direction)
    loop_causing_positions = set()
    
    for pos in visited_positions:
        map_with_obstacle.obstacle_position = pos
        if detect_loops(map_with_obstacle, guard_position, guard_direction):
            loop_causing_positions.add(pos)
    
    return loop_causing_positions

def count_distinct_positions(file_path):
    map_grid, guard_position, guard_direction = parse_input(file_path)
    map_with_obstacle = MapWithObstacle(map_grid)
    visited_positions = move_guard(map_with_obstacle, guard_position, guard_direction)
    loop_causing_positions = find_obstacle_positions_causing_loops(map_grid, guard_position, guard_direction)
    return len(visited_positions), len(loop_causing_positions)

if __name__ == "__main__":
    file_path = 'inputs/06.txt'
    distinct_positions, loop_positions = count_distinct_positions(file_path)
    print(f"Distinct positions visited: {distinct_positions}")
    print(f"Positions causing loops: {loop_positions}")