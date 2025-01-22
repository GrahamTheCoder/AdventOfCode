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

def compute_quadrant_counts(width, height, robots):
    mid_x = (width - 1) // 2
    mid_y = (height - 1) // 2
    quad_i = 0
    quad_ii = 0
    quad_iii = 0
    quad_iv = 0

    for x, y, vx, vy in robots:
        new_x = (x + vx * 100) % width
        new_y = (y + vy * 100) % height

        if new_x == mid_x or new_y == mid_y:
            continue

        if new_x < mid_x:
            if new_y < mid_y:
                quad_i += 1
            else:
                quad_iii += 1
        else:
            if new_y < mid_y:
                quad_ii += 1
            else:
                quad_iv += 1

    return (quad_i, quad_ii, quad_iii, quad_iv)

def main():
    width = 101
    height = 103
    robots = parse_input()
    counts = compute_quadrant_counts(width, height, robots)
    safety_factor = 1
    for cnt in counts:
        safety_factor *= cnt
    print(safety_factor)

if __name__ == "__main__":
    main()
