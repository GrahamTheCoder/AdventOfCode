import pytest
from day08 import parse_input, group_antennas_by_frequency, calculate_antinodes_for_frequency, calculate_antinodes

def test_parse_input():
    antennas = parse_input('tests/test_day08.txt')
    expected = [(8, 1, '0'), (5, 2, '0'), (7, 3, '0'), (4, 4, '0'), (6, 5, 'A'), (8, 8, 'A'), (9, 9, 'A')]
    assert antennas == expected

def test_group_antennas_by_frequency():
    antennas = [(8, 1, '0'), (5, 2, '0'), (7, 3, '0'), (4, 4, '0'), (6, 5, 'A'), (8, 8, 'A'), (9, 9, 'A')]
    freq_dict = group_antennas_by_frequency(antennas)
    expected = {
        '0': [(8, 1), (5, 2), (7, 3), (4, 4)],
        'A': [(6, 5), (8, 8), (9, 9)]
    }
    assert freq_dict == expected

def test_calculate_antinodes_for_frequency():
    positions = [(8, 1), (5, 2), (7, 3), (4, 4)]
    antinodes = calculate_antinodes_for_frequency(positions)
    expected = {(6, 2), (9, 2), (6, 3), (5, 3), (5, 4), (8, 4)}
    assert antinodes == expected

def test_calculate_antinodes():
    antennas = [(8, 1, '0'), (5, 2, '0'), (7, 3, '0'), (4, 4, '0'), (6, 5, 'A'), (8, 8, 'A'), (9, 9, 'A')]
    antinodes = calculate_antinodes(antennas)
    expected = {(6, 2), (9, 2), (6, 3), (5, 3), (5, 4), (8, 4)}
    assert antinodes == expected

if __name__ == "__main__":
    pytest.main()