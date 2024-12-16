def parse_input(file_path):
    with open(file_path, 'r') as file:
        disk_map = file.read().strip()
    
    blocks = []
    i = 0
    while i < len(disk_map):
        file_length = int(disk_map[i])
        if i + 1 < len(disk_map):
            free_space_length = int(disk_map[i + 1])
        else:
            free_space_length = 0  # Assume zero free space if the last digit is a file length
        blocks.append((file_length, free_space_length))
        i += 2
    
    return blocks

def compact_disk(blocks):
    disk = []
    file_id = 0
    
    for file_length, free_space_length in blocks:
        disk.extend([file_id] * file_length)
        disk.extend(['.'] * free_space_length)
        file_id += 1
    
    # Compact the disk
    write_pos = 0
    for read_pos in range(len(disk)):
        if disk[read_pos] != '.':
            disk[write_pos] = disk[read_pos]
            write_pos += 1
    
    # Fill the remaining positions with free space
    for i in range(write_pos, len(disk)):
        disk[i] = '.'
    
    return disk

def calculate_checksum(disk):
    checksum = 0
    for position, block in enumerate(disk):
        if block != '.':
            checksum += position * block
    return checksum

def main():
    blocks = parse_input('inputs/09.txt')
    compacted_disk = compact_disk(blocks)
    checksum = calculate_checksum(compacted_disk)
    print(checksum)

if __name__ == "__main__":
    main()