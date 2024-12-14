def parse_input(file_path):
    with open(file_path, 'r') as file:
        lines = file.readlines()
    
    antennas = []
    for y, line in enumerate(lines):
        for x, char in enumerate(line.strip()):
            if char != '.':
                antennas.append((x, y, char))
    
    return antennas

def group_antennas_by_frequency(antennas):
    freq_dict = {}
    for x, y, freq in antennas:
        if freq not in freq_dict:
            freq_dict[freq] = []
        freq_dict[freq].append((x, y))
    return freq_dict

def calculate_antinodes_for_frequency(positions):
    antinodes = set()
    n = len(positions)
    for i in range(n):
        for j in range(i + 1, n):
            x1, y1 = positions[i]
            x2, y2 = positions[j]
            
            # Check horizontal alignment
            if y1 == y2:
                if (x2 - x1) % 3 == 0:
                    dx = (x2 - x1) // 3
                    antinodes.add((x1 + dx, y1))
                    antinodes.add((x2 - dx, y2))
            
            # Check vertical alignment
            if x1 == x2:
                if (y2 - y1) % 3 == 0:
                    dy = (y2 - y1) // 3
                    antinodes.add((x1, y1 + dy))
                    antinodes.add((x2, y2 - dy))
            
            # Check diagonal alignment
            if abs(x2 - x1) == abs(y2 - y1):
                if (x2 - x1) % 3 == 0 and (y2 - y1) % 3 == 0:
                    dx = (x2 - x1) // 3
                    dy = (y2 - y1) // 3
                    antinodes.add((x1 + dx, y1 + dy))
                    antinodes.add((x2 - dx, y2 - dy))
    return antinodes

def calculate_antinodes(antennas):
    antinodes = set()
    freq_dict = group_antennas_by_frequency(antennas)
    
    for freq, positions in freq_dict.items():
        antinodes.update(calculate_antinodes_for_frequency(positions))
    
    return antinodes

def main():
    antennas = parse_input('inputs/08.txt')
    antinodes = calculate_antinodes(antennas)
    print(len(antinodes))

if __name__ == "__main__":
    main()