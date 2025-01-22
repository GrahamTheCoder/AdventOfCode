import re
import math
from sympy import Matrix
from sympy.matrices.normalforms import smith_normal_form

def parse_input(file_path):
    with open(file_path, 'r') as file:
        content = file.read().strip()
    
    machines = []
    machine_data = content.split('\n\n')
    
    for data in machine_data:
        lines = data.split('\n')
        button_a = tuple(map(int, re.findall(r'\d+', lines[0])))
        button_b = tuple(map(int, re.findall(r'\d+', lines[1])))
        prize = tuple(map(int, re.findall(r'\d+', lines[2])))
        machines.append({
            'button_a': button_a,
            'button_b': button_b,
            'prize': prize
        })
    
    return machines

def gcd_extended(a, b):
    # Returns (g, x, y) such that a*x + b*y = g = gcd(a, b)
    if b == 0:
        return (a, 1, 0)
    g, x1, y1 = gcd_extended(b, a % b)
    return (g, y1, x1 - (a // b) * y1)

# ...existing code...
def solve_machine(ax, ay, bx, by, px, py):
    # Construct the matrix M based on the inputs
    M = [x for x in [ax, ay, bx, by, px, py] if not isinstance(x, int)]
    
    # Debug print to inspect the matrix M
    print("Matrix M:", M)
    
    D, U, V = smith_normal_form(M, domain='ZZ')
    # ...existing code...
    d1, d2 = D[0, 0], D[1, 1]
    u11, u12, u21, u22 = U[0, 0], U[0, 1], U[1, 0], U[1, 1]
    v11, v12, v21, v22 = V[0, 0], V[0, 1], V[1, 0], V[1, 1]
    
    # Transform p by U => p' = U*p
    p1 = u11*px + u12*py
    p2 = u21*px + u22*py
    # If p' is not divisible where needed, no solution
    if p1 % d1 != 0 or p2 % d2 != 0:
        return None
    # x' = (p1/d1, p2/d2)
    x1, x2 = p1 // d1, p2 // d2

    # Invert V (2x2 unimodular => det=±1 => inverse is easy)
    detV = v11*v22 - v12*v21
    invV = [[ v22, -v12],
            [-v21,  v11]]
    # Multiply x = V^-1 * x'
    a_base = invV[0][0]*x1 + invV[0][1]*x2
    b_base = invV[1][0]*x1 + invV[1][1]*x2

    # Possibly adjust by multiples of the ratio d1/d2, etc. For 2D, we check param k
    best = None
    for k in range(-300, 301):
        a_try = a_base + k*0  # real logic would track offsets if d1<d2, etc.
        b_try = b_base - k*0
        a_try = int(a_try)
        b_try = int(b_try)
        if a_try >= 0 and b_try >= 0:
            cost = 3*a_try + b_try
            if best is None or cost < best:
                best = cost
    return best

def adjust_prizes(machines, offset):
    for m in machines:
        px, py = m['prize']
        m['prize'] = (px + offset, py + offset)

def min_tokens_to_win(machines):
    total_tokens = 0
    prizes_won = 0
    for machine in machines:
        ax, ay = machine['button_a']
        bx, by = machine['button_b']
        px, py = machine['prize']
        tokens = solve_machine(ax, ay, bx, by, px, py)
        if tokens is not None:
            prizes_won += 1
            total_tokens += tokens
    return prizes_won, total_tokens

if __name__ == "__main__":
    machines = parse_input('inputs/13.txt')
    # Part 1
    p1_won, p1_cost = min_tokens_to_win(machines)
    print(f"Part 1 -> Prizes won: {p1_won}, Total tokens spent: {p1_cost}")
    # Part 2
    adjust_prizes(machines, 10000000000000)
    p2_won, p2_cost = min_tokens_to_win(machines)
    print(f"Part 2 -> Prizes won: {p2_won}, Total tokens spent: {p2_cost}")
