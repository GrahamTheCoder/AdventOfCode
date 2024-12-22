def parse_input(file_path):
    with open(file_path, 'r') as file:
        grid = [list(line.strip()) for line in file]
    return grid

def is_on_grid(grid, p):
    (x,y) = p
    return 0 <= x < len(grid) and 0 <= y < len(grid[0])

def get_neighbors(grid, p):
    return [p for p in get_neighbor_points(p) if is_on_grid(grid, p)]

def get_neighbor_points(p):
    (x, y) = p
    return [(x-1, y), (x, y-1), (x+1, y), (x, y+1)]

def get_corners(p):
    (x, y) = p
    return [[(x-1, y), (x-1, y-1), (x, y-1)],
            [(x, y-1), (x+1, y-1), (x+1, y)],
            [(x+1, y), (x+1, y+1), (x, y+1)],
            [(x, y+1), (x-1, y+1), (x-1, y)]
            ]


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
        for nx, ny in get_neighbors(grid, (cx, cy)):
            if grid[nx][ny] == plant_type and (nx, ny) not in visited:
                stack.append((nx, ny))
    return region

def calculate_area_and_perimeter(region, grid):
    area = len(region)
    perimeter = 0
    for x, y in region:
        neighbors = get_neighbors(grid, (x, y))
        perimeter += 4 - len(neighbors)
        for nx, ny in neighbors:
            if grid[nx][ny] != grid[x][y]:
                perimeter += 1
    return area, perimeter


def calculate_area_and_sides(region, grid):
    region_set = set(region)
    plant_type = grid[region[0][0]]
    area = len(region)
    sides = 0

    corners = [
        ((0, -1), (-1, 0), (-1, -1)),  # top-left
        ((0, 1), (-1, 0), (-1, 1)),   # top-right
        ((0, -1), (1, 0), (1, -1)),   # bottom-left
        ((0, 1), (1, 0), (1, 1))      # bottom-right
    ]

    for x, y in region:
        for (hx, hy), (vx, vy), (dx, dy) in corners:
            h = (x + hx, y + hy)
            v = (x + vx, y + vy)
            d = (x + dx, y + dy)
            in_h = h in region_set
            in_v = v in region_set
            in_d = d in region_set
            if (not in_h and not in_v) or (in_h and in_v and not in_d):
                sides += 1

    return area, sides

def calculate_total_cost(grid, part):
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
    grid = parse_input('inputs/12.txt')

    total_cost_part1, region_costs_part1 = calculate_total_cost(grid, part=1)
    for plant_type, area, perimeter, cost in region_costs_part1:
        print(f"A region of {plant_type} plants with price {area} * {perimeter} = {cost}.")
    print(f"Total cost for part 1: {total_cost_part1}")

    total_cost_part2, region_costs_part2 = calculate_total_cost(grid, part=2)
    for plant_type, area, sides, cost in region_costs_part2:
        print(f"A region of {plant_type} plants with price {area} * {sides} = {cost}.")
    print(f"Total cost for part 2: {total_cost_part2}")