def parse_topographic_map(file_path):
    with open(file_path, 'r') as file:
        map_data = [list(map(int, line.strip())) for line in file]
    return map_data

def find_trailheads(map_data):
    trailheads = []
    for i in range(len(map_data)):
        for j in range(len(map_data[i])):
            if map_data[i][j] == 0:
                trailheads.append((i, j))
    return trailheads

def dfs(map_data, x, y, visited):
    if (x, y) in visited:
        return 0
    if map_data[x][y] == 9:
        return 1
    
    visited.add((x, y))
    count = 0
    directions = [(0, 1), (1, 0), (0, -1), (-1, 0)]
    
    for dx, dy in directions:
        nx, ny = x + dx, y + dy
        if 0 <= nx < len(map_data) and 0 <= ny < len(map_data[0]):
            if map_data[nx][ny] == map_data[x][y] + 1:
                count += dfs(map_data, nx, ny, visited)
    
    visited.remove((x, y))
    return count

def calculate_total_rating(map_data):
    trailheads = find_trailheads(map_data)
    total_rating = 0
    for trailhead in trailheads:
        visited = set()
        total_rating += dfs(map_data, trailhead[0], trailhead[1], visited)
    return total_rating

if __name__ == "__main__":
    map_data = parse_topographic_map('inputs/10.txt')
    total_rating = calculate_total_rating(map_data)
    print(total_rating)