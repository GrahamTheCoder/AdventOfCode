from collections import deque

def parse_input(file_path):
    with open(file_path, 'r') as file:
        stones = list(map(int, file.readline().strip().split()))
    return stones

def split_number(n):
    digits = []
    while n > 0:
        digits.append(n % 10)
        n //= 10
    digits.reverse()
    half_len = len(digits) // 2
    left_half = 0
    right_half = 0
    for i in range(half_len):
        left_half = left_half * 10 + digits[i]
    for i in range(half_len, len(digits)):
        right_half = right_half * 10 + digits[i]
    return left_half, right_half

def transform_stones(stones, memo):
    new_stones = deque()
    while stones:
        stone = stones.popleft()
        if stone in memo:
            new_stones.extend(memo[stone])
        else:
            if stone == 0:
                transformed = [1]
            else:
                num_digits = len(str(stone))
                if num_digits % 2 == 0:
                    left_half, right_half = split_number(stone)
                    transformed = [left_half, right_half]
                else:
                    transformed = [stone * 2024]
            memo[stone] = transformed
            new_stones.extend(transformed)
    return new_stones

def process_stones(stones, steps):
    stones = deque(stones)
    memo = {}
    for _ in range(steps):
        stones = transform_stones(stones, memo)
    return len(stones)

def main():
    stones = parse_input('inputs/11.txt')
    result_25 = process_stones(stones, 25)
    print(f'Number of stones after 25 blinks: {result_25}')
    result_75 = process_stones(stones, 75)
    print(f'Number of stones after 75 blinks: {result_75}')

if __name__ == "__main__":
    main()