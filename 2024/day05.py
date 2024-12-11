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
    index_map = {page: idx for idx, page in enumerate(update)}
    
    for x, y in rules:
        if x in index_map and y in index_map:
            if index_map[x] > index_map[y]:
                return False
    return True

def reorder_update(update, rules):
    from collections import defaultdict, deque
    
    graph = defaultdict(list)
    in_degree = defaultdict(int)
    pages = set(update)
    
    for x, y in rules:
        if x in pages and y in pages:
            graph[x].append(y)
            in_degree[y] += 1
            if x not in in_degree:
                in_degree[x] = 0
    
    queue = deque([node for node in pages if in_degree[node] == 0])
    ordered_update = []
    
    while queue:
        node = queue.popleft()
        ordered_update.append(node)
        for neighbor in graph[node]:
            in_degree[neighbor] -= 1
            if in_degree[neighbor] == 0:
                queue.append(neighbor)
    
    return ordered_update

def find_middle_page_number(update):
    return update[len(update) // 2]

def sum_of_middle_page_numbers(file_path):
    rules, updates = parse_input(file_path)
    valid_middle_numbers = []
    invalid_middle_numbers = []
    
    for update in updates:
        if is_valid_update(update, rules):
            valid_middle_numbers.append(find_middle_page_number(update))
        else:
            reordered_update = reorder_update(update, rules)
            invalid_middle_numbers.append(find_middle_page_number(reordered_update))
    
    return sum(invalid_middle_numbers)

if __name__ == "__main__":
    file_path = 'inputs/05.txt'
    result = sum_of_middle_page_numbers(file_path)
    print(result)