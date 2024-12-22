import math

def parse_input(filepath: str):
    """Reads the map from file and returns a list of lines and a freq->[(row,col)] dictionary."""
    with open(filepath, 'r', encoding='utf-8') as f:
        lines = [line.rstrip('\n') for line in f]
    antennas = {}
    for r, row_text in enumerate(lines):
        for c, ch in enumerate(row_text):
            if ch != '.':
                antennas.setdefault(ch, []).append((r, c))
    return lines, antennas

def solve_puzzle_part1(lines, antennas):
    """Part 1: Finds all unique in-bounds antinode positions for pairs of same-frequency antennas."""
    rows, cols = len(lines), len(lines[0]) if lines else 0
    antinodes = set()

    def in_bounds(r, c):
        return 0 <= r < rows and 0 <= c < cols

    for freq, positions in antennas.items():
        pos_count = len(positions)
        for i in range(pos_count):
            for j in range(i+1, pos_count):
                r1, c1 = positions[i]
                r2, c2 = positions[j]
                # Each pair => two antinodes: 2B - A and 2A - B
                candidates = [
                    (2*r2 - r1, 2*c2 - c1),
                    (2*r1 - r2, 2*c1 - c2),
                ]
                for (ar, ac) in candidates:
                    if in_bounds(ar, ac):
                        antinodes.add((ar, ac))
    return len(antinodes)

def solve_puzzle_part2(lines, antennas):
    """
    Part 2: An antinode occurs at any grid position exactly in line
    with at least two antennas of the same frequency, regardless of distance.
    """
    rows, cols = len(lines), len(lines[0]) if lines else 0
    antinodes = set()

    def in_bounds(r, c):
        return 0 <= r < rows and 0 <= c < cols

    for freq, positions in antennas.items():
        # If only one antenna for this freq, no antinodes here
        if len(positions) < 2:
            continue
        # For each pair of antennas, add all collinear in-bounds points
        for i in range(len(positions)):
            for j in range(i+1, len(positions)):
                r1, c1 = positions[i]
                r2, c2 = positions[j]
                dr = r2 - r1
                dc = c2 - c1
                g = math.gcd(dr, dc)
                step_r = dr // g
                step_c = dc // g

                # Explore in the "forward" direction from p1
                rr, cc = r1, c1
                while in_bounds(rr, cc):
                    antinodes.add((rr, cc))
                    rr += step_r
                    cc += step_c

                # Explore in the "backward" direction from p1
                rr, cc = r1, c1
                while in_bounds(rr, cc):
                    antinodes.add((rr, cc))
                    rr -= step_r
                    cc -= step_c
    return len(antinodes)

def main():
    lines, antennas = parse_input("inputs/08.txt")
    part1_result = solve_puzzle_part1(lines, antennas)
    part2_result = solve_puzzle_part2(lines, antennas)
    print(part1_result)
    print(part2_result)

if __name__ == "__main__":
    main()