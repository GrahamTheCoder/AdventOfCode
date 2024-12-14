def parse_input(file_path):
    with open(file_path, 'r') as file:
        lines = file.readlines()
    
    antennas = []
    for y, line in enumerate(lines):
        for x, char in enumerate(line.strip()):
            if char != '.':
                antennas.append((x, y, char))
    
    return antennas

def calculate_antinodes(antennas):
    antinodes = set()
    
    # Group antennas by frequency
    freq_dict = {}
    for x, y, freq in antennas:
        if freq not in freq_dict:
            freq_dict[freq] = []
        freq_dict[freq].append((x, y))
    
    # Calculate antinodes for each frequency
    for freq, positions in freq_dict.items():
        n = len(positions)
        for i in range(n):
            for j in range(i + 1, n):
                x1, y1 = positions[i]
                x2, y2 = positions[j]
                
                # Check if one antenna is twice as far as the other
                if (x2 - x1) % 3 == 0 and (y2 - y1) % 3 == 0:
                    dx = (x2 - x1) // 3
                    dy = (y2 - y1) // 3
                    antinodes.add((x1 + dx, y1 + dy))
                    antinodes.add((x2 - dx, y2 - dy))
    
    return antinodes

def main():
    antennas = parse_input('inputs/08.txt')
    antinodes = calculate_antinodes(antennas)
    print(len(antinodes))

if __name__ == "__main__":
    main()