def parse_input(file_path):
    with open(file_path, 'r') as file:
        grid = [list(line.strip()) for line in file]
    return grid

def get_neighbors(x, y, grid):
    neighbors = []
    if x > 0:
        neighbors.append((x-1, y))
    if x < len(grid) - 1:
        neighbors.append((x+1, y))
    if y > 0:
        neighbors.append((x, y-1))
    if y < len(grid[0]) - 1:
        neighbors.append((x, y+1))
    return neighbors

def flood_fill(x, y, grid, visited):
    plant_type = grid[x][y]
    stack = [(x, y)]
    region = []
    while stack:
        cx, cy = stack.pop()
        if (cx, cy) in visited:
            continue
        visited.add((cx, cy))
        region.append((cx, cy))
        for nx, ny in get_neighbors(cx, cy, grid):
            if grid[nx][ny] == plant_type and (nx, ny) not in visited:
                stack.append((nx, ny))
    return region

def calculate_area_and_perimeter(region, grid):
    area = len(region)
    perimeter = 0
    for x, y in region:
        neighbors = get_neighbors(x, y, grid)
        perimeter += 4 - len(neighbors)
        for nx, ny in neighbors:
            if grid[nx][ny] != grid[x][y]:
                perimeter += 1
                
    return area, perimeter

def calculate_total_cost(grid):
    visited = set()
    total_cost = 0
    region_costs = []
    for x in range(len(grid)):
        for y in range(len(grid[0])):
            if (x, y) not in visited:
                region = flood_fill(x, y, grid, visited)
                area, perimeter = calculate_area_and_perimeter(region, grid)
                cost = area * perimeter
                region_costs.append((grid[x][y], area, perimeter, cost))
                total_cost += cost
    return total_cost, region_costs

if __name__ == "__main__":
    grid = parse_input('inputs/12.txt')
    total_cost, region_costs = calculate_total_cost(grid)
    for plant_type, area, perimeter, cost in region_costs:
        print(f"A region of {plant_type} plants with price {area} * {perimeter} = {cost}.")
    print(f"Total cost: {total_cost}")