import re

def parse_input():
    pattern = re.compile(r'p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)')
    robots = []
    with open('inputs/14.txt') as f:
        for line in f:
            m = pattern.match(line.strip())
            if not m:
                continue
            x, y, vx, vy = map(int, m.groups())
            robots.append((x, y, vx, vy))
    return robots

def is_symmetric(robots, width, height, t):
    positions = []
    for (x, y, vx, vy) in robots:
        new_x = (x + vx * t) % width
        new_y = (y + vy * t) % height
        positions.append((new_x, new_y))
    
    position_set = set(positions)
    
    for (x, y) in positions:
        mirrored_x = width - x - 1
        if (mirrored_x, y) not in position_set:
            return False
    return True

def find_min_time(robots, width, height):
    t = 0
    while True:
        if is_symmetric(robots, width, height, t):
            return t
        t += 1

def main():
    width = 101
    height = 103
    robots = parse_input()
    min_time = find_min_time(robots, width, height)
    print(f"The fewest number of seconds required is: {min_time}")

if __name__ == "__main__":
    main()
