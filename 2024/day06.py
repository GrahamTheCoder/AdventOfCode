class MapWithObstacle:
    _turn_right = {"^": ">", ">": "v", "v": "<", "<": "^"}
    _direction_vectors = {"^": (0, -1), ">": (1, 0), "v": (0, 1), "<": (-1, 0)}

    def __init__(self, map_grid):
        self._map_grid = map_grid
        self._extra_obstacle_position = (None, None)
        self._visited_position_directions = set()
        self._next_position = {}

    # Not thread safe due to state mutation
    def get_visited_positions(self, guard_position, guard_direction):
        self.guard_route_is_loop(guard_position, guard_direction)
        return {(x, y) for (x, y, _) in self._visited_position_directions}

    # Not thread safe due to state mutation
    def is_loop_with_obstacle(self, guard_position, guard_direction, obstacle_position):
        self._extra_obstacle_position = obstacle_position
        self._visited_position_directions = set()
        is_loop = self.guard_route_is_loop(guard_position, guard_direction)
        self._extra_obstacle_position = (None, None)
        return is_loop

    def guard_route_is_loop(self, guard_position, guard_direction):

        x, y = guard_position
        direction = guard_direction
        positions_since_turn = []
        while True:
            x, y = self._jump(x, y, direction)

            if (x, y, direction) in self._visited_position_directions:
                return True
            if not self._in_line_with_extra_obstacle(x, y, direction):
                positions_since_turn.append((x, y))
            self._visited_position_directions.add((x, y, direction))

            new_x, new_y = self._move_forward(x, y, direction)

            if not self._is_on_grid(new_x, new_y):
                return False

            if self._is_obstacle(new_x, new_y):
                for prev_x, prev_y in positions_since_turn:
                    self._next_position[prev_x, prev_y, direction] = (x, y)
                positions_since_turn = []
                direction = MapWithObstacle._turn_right[direction]
            else:
                x, y = new_x, new_y

    def _move_forward(self, x, y, direction):
        dx, dy = MapWithObstacle._direction_vectors[direction]
        return x + dx, y + dy

    def _is_on_grid(self, x, y):
        return 0 <= x < len(self._map_grid[0]) and 0 <= y < len(self._map_grid)

    def _in_line_with_extra_obstacle(self, x, y, direction):
        (obs_x, obs_y) = self._extra_obstacle_position
        return x == obs_x and direction in "^v" or y == obs_y and direction in "<>"

    def _jump(self, x, y, direction):
        if self._in_line_with_extra_obstacle(x, y, direction):
            return (x, y)
        return self._next_position.get((x, y, direction), (x, y))

    def _is_extra_obstacle(self, x, y):
        if (x, y) == self._extra_obstacle_position:
            return True

    def _is_obstacle(self, x, y):
        return self._is_extra_obstacle(x, y) or self._map_grid[y][x] != "."


def parse_input(file_path):
    with open(file_path, "r", encoding="utf8") as file:
        lines = file.readlines()

    map_grid = []
    guard_position = None
    guard_direction = None

    for y, line in enumerate(lines):
        row = list(line.strip())
        map_grid.append(row)
        for x, char in enumerate(row):
            if char in "^>v<":
                guard_position = (x, y)
                guard_direction = char
                row[x] = "."

    return map_grid, guard_position, guard_direction


def find_obstacle_positions_causing_loops(
    map_with_obstacle, visited_positions, guard_position, guard_direction
):
    # PERF optimisation available: start guard right before the obstacle
    return {
        pos
        for pos in visited_positions
        if map_with_obstacle.is_loop_with_obstacle(guard_position, guard_direction, pos)
    }


def count_distinct_positions(file_path):
    map_grid, guard_position, guard_direction = parse_input(file_path)
    map_with_obstacle = MapWithObstacle(map_grid)

    visited_positions = map_with_obstacle.get_visited_positions(
        guard_position, guard_direction
    )
    loop_causing_positions = find_obstacle_positions_causing_loops(
        map_with_obstacle, visited_positions, guard_position, guard_direction
    )
    return len(visited_positions), len(loop_causing_positions)


if __name__ == "__main__":
    distinct_positions, loop_positions = count_distinct_positions("inputs/06.txt")
    print(f"Distinct positions visited: {distinct_positions}")
    print(f"Positions causing loops: {loop_positions}")
