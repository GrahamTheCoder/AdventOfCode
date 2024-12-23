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

def solve_puzzle(lines, antennas):
    """Finds all unique in-bounds antinode positions for same-frequency antenna pairs."""
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
                antinodes_candidates = [
                    (2*r2 - r1, 2*c2 - c1),
                    (2*r1 - r2, 2*c1 - c2),
                ]
                for (ar, ac) in antinodes_candidates:
                    if in_bounds(ar, ac):
                        antinodes.add((ar, ac))

    return len(antinodes)

def main():
    lines, antennas = parse_input("inputs/08.txt")
    result = solve_puzzle(lines, antennas)
    print(result)

if __name__ == "__main__":
    main()