from collections import deque

def parse_input(file_path):
    with open(file_path, 'r') as file:
        grid = [list(line.strip()) for line in file]
    return grid

def find_start_end(grid):
    start, end = None, None
    for r, row in enumerate(grid):
        for c, val in enumerate(row):
            if val == 'S':
                start = (r, c)
            elif val == 'E':
                end = (r, c)
    return start, end

def bfs(grid, start, end):
    rows, cols = len(grid), len(grid[0])
    queue = deque([(start[0], start[1], 0)])
    visited = set()
    visited.add(start)
    distances = {start: 0}
    
    while queue:
        r, c, dist = queue.popleft()
        if (r, c) == end:
            return dist, distances
        for dr, dc in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
            nr, nc = r + dr, c + dc
            if 0 <= nr < rows and 0 <= nc < cols and grid[nr][nc] != '#' and (nr, nc) not in visited:
                visited.add((nr, nc))
                queue.append((nr, nc, dist + 1))
                distances[(nr, nc)] = dist + 1
    return float('inf'), distances

def find_cheats(grid, start, end):
    rows, cols = len(grid), len(grid[0])
    _, distances = bfs(grid, start, end)
    cheats = []
    
    for r in range(rows):
        for c in range(cols):
            if grid[r][c] == '#':
                for dr1, dc1 in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
                    nr1, nc1 = r + dr1, c + dc1
                    if 0 <= nr1 < rows and 0 <= nc1 < cols and grid[nr1][nc1] == '.':
                        for dr2, dc2 in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
                            nr2, nc2 = nr1 + dr2, nc1 + dc2
                            if 0 <= nr2 < rows and 0 <= nc2 < cols and grid[nr2][nc2] == '.':
                                if (nr1, nc1) in distances and (nr2, nc2) in distances:
                                    time_saved = distances[end] - (distances[(nr1, nc1)] + 1 + distances[(nr2, nc2)] + 1)
                                    if time_saved > 0:
                                        cheats.append(time_saved)
    return cheats

def count_cheats_saving_at_least(cheats, min_time_saved):
    return sum(1 for cheat in cheats if cheat >= min_time_saved)

def main():
    grid = parse_input('inputs/20.txt')
    start, end = find_start_end(grid)
    cheats = find_cheats(grid, start, end)
    result = count_cheats_saving_at_least(cheats, 100)
    print(result)

if __name__ == "__main__":
    main()
