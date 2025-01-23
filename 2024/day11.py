from functools import lru_cache

def parse_input():
    from pathlib import Path
    input_file = Path(__file__).parent / "inputs" / "11.txt"
    with open(input_file) as f:
        data = list(map(int, f.read().split()))
    return data

@lru_cache(maxsize=None)
def f(x: int, t: int) -> int:
    if t == 0:
        return 1
    if x == 0:
        return f(1, t-1)
    s = str(x)
    l = len(s)
    if l % 2 == 0:
        half = l // 2
        left = int(s[:half])
        right = int(s[half:])
        return f(left, t-1) + f(right, t-1)
    else:
        y = x * 2024
        return f(y, t-1)

def main():
    stones = parse_input()
    total = sum(f(x, 75) for x in stones)
    print(total)

if __name__ == "__main__":
    main()
