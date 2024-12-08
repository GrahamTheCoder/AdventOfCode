import pytest
from day01 import calculate_total_distance

@pytest.mark.parametrize("left, right, expected", [
    ([1, 2, 3], [4, 5, 6], 9),
    ([1, 3, 5], [2, 4, 6], 3),
    ([10, 20, 30], [10, 20, 30], 0),
    ([1, 1, 1], [1, 1, 1], 0),
    ([1, 2, 3], [3, 2, 1], 0),
    ([0, 0, 0], [0, 0, 0], 0),
    ([1, 2, 3], [1, 2, 3], 0),
    ([1, 2, 3], [3, 3, 3], 3),
    ([1, 2, 3], [1, 1, 1], 3),
])
def test_calculate_total_distance(left, right, expected):
    assert calculate_total_distance(left, right) == expected

def test_calculate_total_distance_empty():
    assert calculate_total_distance([], []) == 0

if __name__ == "__main__":
    pytest.main()
