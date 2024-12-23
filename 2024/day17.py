def parse_input(file_path):
    with open(file_path, 'r') as file:
        lines = file.readlines()
    
    registers = {}
    program = []
    
    for line in lines:
        if line.startswith("Register"):
            parts = line.split(": ")
            register_name = parts[0].split(" ")[1]
            registers[register_name] = int(parts[1])
        elif line.startswith("Program"):
            program = list(map(int, line.split(": ")[1].split(",")))
    
    return registers, program

def execute_program(registers, program):
    output = []
    instruction_pointer = 0
    
    while instruction_pointer < len(program):
        opcode = program[instruction_pointer]
        operand = program[instruction_pointer + 1]
        
        if opcode == 0:  # adv
            denominator = 2 ** get_operand_value(registers, operand)
            registers['A'] //= denominator
        elif opcode == 1:  # bxl
            registers['B'] ^= operand
        elif opcode == 2:  # bst
            registers['B'] = get_operand_value(registers, operand) % 8
        elif opcode == 3:  # jnz
            if registers['A'] != 0:
                instruction_pointer = operand
                continue
        elif opcode == 4:  # bxc
            registers['B'] ^= registers['C']
        elif opcode == 5:  # out
            output.append(get_operand_value(registers, operand) % 8)
        elif opcode == 6:  # bdv
            denominator = 2 ** get_operand_value(registers, operand)
            registers['B'] = registers['A'] // denominator
        elif opcode == 7:  # cdv
            denominator = 2 ** get_operand_value(registers, operand)
            registers['C'] = registers['A'] // denominator
        
        instruction_pointer += 2
    
    return ",".join(map(str, output))

def get_operand_value(registers, operand):
    if operand <= 3:
        return operand
    elif operand == 4:
        return registers['A']
    elif operand == 5:
        return registers['B']
    elif operand == 6:
        return registers['C']
    else:
        raise ValueError("Invalid operand")

def find_initial_value_for_self_replicating_program(registers, program):
    original_program_output = ",".join(map(str, program))
    
    for initial_value in range(1, 1000000):  # Arbitrary large range to find the solution
        registers['A'] = initial_value
        output = execute_program(registers.copy(), program)
        if output == original_program_output:
            return initial_value
    
    return None

if __name__ == "__main__":
    registers, program = parse_input('inputs/17.txt')
    
    # Part 1
    result_part1 = execute_program(registers.copy(), program)
    print(result_part1)
    
    # Part 2
    initial_value = find_initial_value_for_self_replicating_program(registers, program)
    print(initial_value)
