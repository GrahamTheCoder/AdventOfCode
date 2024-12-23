import math

def parse_input(filepath: str):
    """Reads the map from file and returns lines plus freq-to-positions dictionary."""
    with open(filepath, 'r', encoding='utf-8') as f:
        lines = [line.rstrip('\n') for line in f]
    antennas = {}
    for r, row_text in enumerate(lines):
        for c, ch in enumerate(row_text):
            if ch != '.':
                antennas.setdefault(ch, []).append((r, c))
    return lines, antennas

def in_bounds_factory(lines):
    """Returns a function checking if row,col is in map bounds."""
    rows = len(lines)
    cols = len(lines[0]) if rows else 0
    return lambda r, c: 0 <= r < rows and 0 <= c < cols

def pairwise_antenna_positions(antennas):
    """Yields (freq, (r1, c1), (r2, c2)) for each distinct pair of same-frequency antennas."""
    for freq, positions in antennas.items():
        if len(positions) < 2:
            continue
        pos_count = len(positions)
        for i in range(pos_count):
            for j in range(i+1, pos_count):
                yield freq, positions[i], positions[j]

def compute_part1_antinodes(lines, antennas):
    """Part 1: compute antinodes for each pair by doubling one point minus the other."""
    in_bounds = in_bounds_factory(lines)
    antinodes = set()
    for freq, (r1, c1), (r2, c2) in pairwise_antenna_positions(antennas):
        candidates = [(2*r2 - r1, 2*c2 - c1), (2*r1 - r2, 2*c1 - c2)]
        for (ar, ac) in candidates:
            if in_bounds(ar, ac):
                antinodes.add((ar, ac))
    return antinodes

def compute_part2_antinodes(lines, antennas):
    """
    Part 2: for each pair, add every collinear point in both directions.
    """
    in_bounds = in_bounds_factory(lines)
    antinodes = set()
    for freq, (r1, c1), (r2, c2) in pairwise_antenna_positions(antennas):
        dr, dc = r2 - r1, c2 - c1
        g = math.gcd(dr, dc)
        step_r = dr // g
        step_c = dc // g

        # forward
        rr, cc = r1, c1
        while in_bounds(rr, cc):
            antinodes.add((rr, cc))
            rr += step_r
            cc += step_c

        # backward
        rr, cc = r1, c1
        while in_bounds(rr, cc):
            antinodes.add((rr, cc))
            rr -= step_r
            cc -= step_c
    return antinodes

def solve_puzzle_part1(lines, antennas):
    return len(compute_part1_antinodes(lines, antennas))

def solve_puzzle_part2(lines, antennas):
    return len(compute_part2_antinodes(lines, antennas))

def main():
    lines, antennas = parse_input("inputs/08.txt")
    part1_result = solve_puzzle_part1(lines, antennas)
    part2_result = solve_puzzle_part2(lines, antennas)
    print(part1_result)
    print(part2_result)

if __name__ == "__main__":
    main()