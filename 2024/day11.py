def parse_input(file_path):
    with open(file_path, 'r') as file:
        stones = list(map(int, file.readline().strip().split()))
    return stones

def transform_stones(stones, memo):
    new_stones = []
    for stone in stones:
        if stone in memo:
            new_stones.extend(memo[stone])
        else:
            if stone == 0:
                transformed = [1]
            elif len(str(stone)) % 2 == 0:
                half_len = len(str(stone)) // 2
                left_half = int(str(stone)[:half_len])
                right_half = int(str(stone)[half_len:])
                transformed = [left_half, right_half]
            else:
                transformed = [stone * 2024]
            memo[stone] = transformed
            new_stones.extend(transformed)
    return new_stones

def main():
    stones = parse_input('inputs/11.txt')
    memo = {}
    for _ in range(75):
        stones = transform_stones(stones, memo)
    print(len(stones))

if __name__ == "__main__":
    main()