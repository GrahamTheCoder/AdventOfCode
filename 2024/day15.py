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
            # Gather all cells for wide boxes:
            box_positions = []
            pending = [new_robot]
            visited = set()
            while pending:
                pos = pending.pop()
                if pos not in visited and pos in boxes:
                    visited.add(pos)
                    box_positions.append(pos)
                    x2, y2 = pos[0], pos[1]
                    # Check adjacent bracket if wide box
                    # Horizontal pair
                    if (x2 + 1, y2) in boxes:
                        pending.append((x2 + 1, y2))
                    if (x2 - 1, y2) in boxes:
                        pending.append((x2 - 1, y2))

                    # Vertical pair (rare but possible if squares placed vertically)
                    if (x2, y2 + 1) in boxes:
                        pending.append((x2, y2 + 1))
                    if (x2, y2 - 1) in boxes:
                        pending.append((x2, y2 - 1))

            # Now push them in a chain
            # ...existing code to detect free space...
            new_pos_list = []
            blocked = False
            for pos in box_positions:
                test_next = (pos[0] + dx, pos[1] + dy)
                if grid[test_next[1]][test_next[0]] == '#' or test_next in boxes - set(box_positions):
                    blocked = True
                    break
                new_pos_list.append(test_next)

            if not blocked:
                for pos in box_positions:
                    boxes.remove(pos)
                for pos in new_pos_list:
                    boxes.add(pos)
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
    print("Part 2:", result_part2) # < 2697915

if __name__ == "__main__":
    main()
