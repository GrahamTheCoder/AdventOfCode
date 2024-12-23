import heapq

def parse_input(file_path):
    with open(file_path, 'r') as file:
        maze = [list(line.strip()) for line in file.readlines()]
    return maze

def find_start_end(maze):
    start = end = None
    for r, row in enumerate(maze):
        for c, val in enumerate(row):
            if val == 'S':
                start = (r, c)
            elif val == 'E':
                end = (r, c)
    return start, end

def solve_maze(maze):
    start, end = find_start_end(maze)
    directions = [(0, 1), (1, 0), (0, -1), (-1, 0)]  # East, South, West, North
    direction_cost = 1000
    move_cost = 1

    pq = [(0, start, 0)]  # (cost, position, direction)
    visited = set()
    parent = {}

    while pq:
        cost, (r, c), dir_idx = heapq.heappop(pq)
        if (r, c) == end:
            path = []
            while (r, c) != start:
                path.append((r, c))
                if (r, c) in parent:
                    r, c = parent[(r, c)]
                else:
                    break
            path.append(start)
            path.reverse()
            return cost, path
        if (r, c, dir_idx) in visited:
            continue
        visited.add((r, c, dir_idx))

        for i, (dr, dc) in enumerate(directions):
            nr, nc = r + dr, c + dc
            if 0 <= nr < len(maze) and 0 <= nc < len(maze[0]) and maze[nr][nc] != '#':
                new_cost = cost + move_cost
                if i != dir_idx:
                    new_cost += direction_cost
                if (nr, nc) not in parent or new_cost < parent[(nr, nc)][0]:
                    parent[(nr, nc)] = (r, c)
                    heapq.heappush(pq, (new_cost, (nr, nc), i))

    return float('inf'), []

def find_best_path_tiles(maze):
    _, path = solve_maze(maze)
    best_path_tiles = set(path)
    return best_path_tiles

if __name__ == "__main__":
    maze = parse_input('inputs/16.txt')
    part1_result, _ = solve_maze(maze)
    print(f"Part 1: The lowest score a Reindeer could possibly get is: {part1_result}")

    best_path_tiles = find_best_path_tiles(maze)
    part2_result = len(best_path_tiles)
    print(f"Part 2: The number of tiles that are part of at least one of the best paths through the maze is: {part2_result}")
