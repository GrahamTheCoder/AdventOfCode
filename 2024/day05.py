from collections import defaultdict, deque

def parse_input(file_path):
    with open(file_path, 'r') as file:
        content = file.read().strip().split('\n')
    
    rules = []
    updates = []
    is_update_section = False
    
    for line in content:
        if not line.strip():
            is_update_section = True
            continue
        
        if is_update_section:
            updates.append(list(map(int, line.split(','))))
        else:
            x, y = map(int, line.split('|'))
            rules.append((x, y))
    
    return rules, updates

def is_valid_update(update, rules):
    index_from_page = {page: idx for idx, page in enumerate(update)}
    return all(
        index_from_page[x] <= index_from_page[y]
        for x, y in rules
        if x in index_from_page and y in index_from_page
    )

def reorder_update(update, rules):
    dependent_upon = defaultdict(list)
    unprinted_dependants = defaultdict(int)
    pages = set(update)
    
    for x, y in rules:
        if x in pages and y in pages:
            dependent_upon[x].append(y)
            unprinted_dependants[y] += 1
    
    valid_to_print = deque(node for node in pages if unprinted_dependants[node] == 0)
    print_order = []
    
    while valid_to_print:
        current_page = valid_to_print.popleft()
        print_order.append(current_page)
        for neighbor in dependent_upon[current_page]:
            unprinted_dependants[neighbor] -= 1
            if unprinted_dependants[neighbor] == 0:
                valid_to_print.append(neighbor)
    
    return print_order

def find_middle_page_number(update):
    return update[len(update) // 2]

def sum_of_middle_page_numbers(file_path):
    rules, updates = parse_input(file_path)
    invalid_middle_numbers = []
    
    for update in updates:
        if not is_valid_update(update, rules):
            reordered_update = reorder_update(update, rules)
            invalid_middle_numbers.append(find_middle_page_number(reordered_update))
    
    return sum(invalid_middle_numbers)

if __name__ == "__main__":
    file_path = 'inputs/05.txt'
    result = sum_of_middle_page_numbers(file_path)
    print(result)