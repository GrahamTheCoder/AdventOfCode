def parse_input(file_path):
    with open(file_path, 'r') as file:
        stones = list(map(int, file.readline().strip().split()))
    return stones

def transform_stones(stones):
    new_stones = []
    for stone in stones:
        if stone == 0:
            new_stones.append(1)
        elif len(str(stone)) % 2 == 0:
            half_len = len(str(stone)) // 2
            left_half = int(str(stone)[:half_len])
            right_half = int(str(stone)[half_len:])
            new_stones.extend([left_half, right_half])
        else:
            new_stones.append(stone * 2024)
    return new_stones

def main():
    stones = parse_input('inputs/11.txt')
    for _ in range(75):
        stones = transform_stones(stones)
    print(len(stones))

if __name__ == "__main__":
    main()