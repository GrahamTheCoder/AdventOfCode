import itertools

def parse_input(file_path):
    equations = []
    with open(file_path, 'r') as file:
        for line in file:
            test_value, numbers = line.split(':')
            test_value = int(test_value.strip())
            numbers = list(map(int, numbers.strip().split()))
            equations.append((test_value, numbers))
    return equations

def evaluate_expression_dp(numbers, target):
    n = len(numbers)
    dp = {}

    def dp_eval(i, current_value):
        if i == n:
            return current_value == target
        if (i, current_value) in dp:
            return dp[(i, current_value)]
        
        add_result = dp_eval(i + 1, current_value + numbers[i])
        multiply_result = dp_eval(i + 1, current_value * numbers[i])
        
        dp[(i, current_value)] = add_result or multiply_result
        return dp[(i, current_value)]
    
    return dp_eval(1, numbers[0])

def is_valid_equation(test_value, numbers):
    if len(numbers) == 1:
        return numbers[0] == test_value
    return evaluate_expression_dp(numbers, test_value)

def sum_valid_test_values(equations):
    total_sum = 0
    for test_value, numbers in equations:
        if is_valid_equation(test_value, numbers):
            total_sum += test_value
    return total_sum

if __name__ == "__main__":
    input_file = 'inputs/07.txt'
    equations = parse_input(input_file)
    result = sum_valid_test_values(equations)
    print(result)