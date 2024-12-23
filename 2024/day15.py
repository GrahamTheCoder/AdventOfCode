import os

def parse_input(file_path):
    with open(file_path, 'r') as f:
        lines = f.read().splitlines()
    
    map_lines = []
    move_sequence = []
    for line in lines:
        if line.startswith(('#', '.', 'O', '@')):
            map_lines.append(line)
        else:
            move_sequence.extend(line.strip())
    
    return map_lines, move_sequence

def solve(map_lines, move_sequence):
    grid = [list(line) for line in map_lines]
    robot_pos = None
    boxes = set()

    for y, row in enumerate(grid):
        for x, cell in enumerate(row):
            if cell == '@':
                robot_pos = (x, y)
            elif cell == 'O' or cell == '[' or cell == ']':
                boxes.add((x, y))
    
    directions = {
        '^': (0, -1),
        'v': (0, 1),
        '<': (-1, 0),
        '>': (1, 0)
    }

    for move in move_sequence:
        dx, dy = directions.get(move, (0, 0))
        new_robot = (robot_pos[0] + dx, robot_pos[1] + dy)
        
        if grid[new_robot[1]][new_robot[0]] == '#':
            continue
        
        if new_robot in boxes:
            box_positions = []
            current_pos = new_robot
            while current_pos in boxes:
                box_positions.append(current_pos)
                current_pos = (current_pos[0] + dx, current_pos[1] + dy)
            
            if grid[current_pos[1]][current_pos[0]] != '#' and current_pos not in boxes:
                for pos in reversed(box_positions):
                    boxes.remove(pos)
                    new_pos = (pos[0] + dx, pos[1] + dy)
                    boxes.add(new_pos)
                robot_pos = new_robot
        else:
            robot_pos = new_robot

    gps_sum = sum(100 * y + x for x, y in boxes)
    return gps_sum

def scale_up_map(map_lines):
    new_map_lines = []
    for line in map_lines:
        new_line = ""
        for char in line:
            if char == '#':
                new_line += "##"
            elif char == 'O':
                new_line += "[]"
            elif char == '.':
                new_line += ".."
            elif char == '@':
                new_line += "@."
        new_map_lines.append(new_line)
    return new_map_lines

def solve_part2(map_lines, move_sequence):
    scaled_map_lines = scale_up_map(map_lines)
    return solve(scaled_map_lines, move_sequence)

def main():
    input_path = os.path.join('inputs', '15.txt')
    map_lines, move_sequence = parse_input(input_path)
    
    # Part 1
    result_part1 = solve(map_lines, move_sequence)
    print("Part 1:", result_part1)
    
    # Part 2
    result_part2 = solve_part2(map_lines, move_sequence)
    print("Part 2:", result_part2)

if __name__ == "__main__":
    main()
