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

def calculate_area_and_sides(region, grid):
    area = len(region)
    sides = set()
    for x, y in region:
        neighbors = get_neighbors(x, y, grid)
        for nx, ny in neighbors:
            if grid[nx][ny] != grid[x][y]:
                sides.add(((x, y), (nx, ny)))
    return area, len(sides)

def calculate_total_cost(grid, part=1):
    visited = set()
    total_cost = 0
    region_costs = []
    for x in range(len(grid)):
        for y in range(len(grid[0])):
            if (x, y) not in visited:
                region = flood_fill(x, y, grid, visited)
                if part == 1:
                    area, perimeter = calculate_area_and_perimeter(region, grid)
                    cost = area * perimeter
                else:
                    area, sides = calculate_area_and_sides(region, grid)
                    cost = area * sides
                region_costs.append((grid[x][y], area, perimeter if part == 1 else sides, cost))
                total_cost += cost
    return total_cost, region_costs

if __name__ == "__main__":
    grid = parse_input('inputs/12-example2.txt')
    total_cost_part1, region_costs_part1 = calculate_total_cost(grid, part=1)
    for plant_type, area, perimeter, cost in region_costs_part1:
        print(f"A region of {plant_type} plants with price {area} * {perimeter} = {cost}.")
    print(f"Total cost for part 1: {total_cost_part1}")

    total_cost_part2, region_costs_part2 = calculate_total_cost(grid, part=2)
    for plant_type, area, sides, cost in region_costs_part2:
        print(f"A region of {plant_type} plants with price {area} * {sides} = {cost}.")
    print(f"Total cost for part 2: {total_cost_part2}")