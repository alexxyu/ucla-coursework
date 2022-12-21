"""
Solve the String Composition Problem.

Input: An integer k and a string Text.
Output: Composition_k(Text) (the k-mers can be provided in any order).
"""
def kmer_composition(file):
    with open(file, 'r') as f:
        k, text = f.read().splitlines()
        k = int(k)

        kmers = []
        for i in range(len(text)-k+1):
            kmers.append(text[i:i+k])
        return kmers

"""
String Spelled by a Genome Path Problem. Reconstruct a string from its genome path.

Input: A sequence path of k-mers Pattern_1, ..., Pattern_n such that the last k - 1 symbols of Pattern_i are equal to 
    the first k-1 symbols of Pattern_{i+1} for 1 ≤ i ≤ n-1.
Output: A string Text of length k+n-1 such that the i-th k-mer in Text is equal to Patterni (for 1 ≤ i ≤ n).
"""
def genome_path_to_string(file):
    with open(file, 'r') as f:
        path = f.read().splitlines()
        
        string = path[0]
        for s in path[1:]:
            string += s[-1]
        return string

"""
Solve the Overlap Graph Problem.

Input: A collection Patterns of k-mers.
Output: The overlap graph Overlap(Patterns), in the form of an adjacency list. 
    (You may return the nodes and their edges in any order.)
"""
def overlap_graph(file):
    bases = ["A", "C", "G", "T"]
    with open(file, 'r') as f:
        patterns = f.read().splitlines()

        adj_list = {p:[] for p in patterns}
        for p in patterns:
            prefix = p[:-1]
            for b in bases:
                if b+prefix in adj_list.keys():
                    adj_list[b+prefix].append(p)
        return adj_list

"""
De Bruijn Graph from a String Problem: Construct the de Bruijn graph of a string.

Input: An integer k and a string Text.
Output: DeBruijnk(Text).
"""
def db_graph_from_text(file):
    with open(file, 'r') as f:
        k, text = f.read().splitlines()
        k = int(k)-1

        kmers = []
        for i in range(len(text)-k+1):
            kmers.append(text[i:i+k])

        adj_list = {p:[] for p in kmers}
        for i in range(1, len(kmers)):
            adj_list[kmers[i-1]].append(kmers[i])
        return adj_list

"""
De Bruijn Graph from k-mers Problem: Construct the de Bruijn graph from a set of k-mers.

Input: A collection of k-mers Patterns.
Output: The adjacency list of the de Bruijn graph DeBruijn(Patterns).
"""
def db_graph_from_kmers(file):
    bases = ["A", "C", "G", "T"]
    with open(file, 'r') as f:
        patterns = f.read().splitlines()

        adj_list = dict()
        for p in patterns:
            prefix = p[:-1]
            suffix = p[1:]
            if prefix not in adj_list.keys():
                adj_list[prefix] = []
            adj_list[prefix].append(suffix)
        return adj_list

"""
Solve the Eulerian Cycle Problem.

Input: The adjacency list of an Eulerian directed graph.
Output: An Eulerian cycle in this graph.
"""
def eulerian_cycle(file):
    with open(file, 'r') as f:
        lines = f.read().splitlines()

        adj_list = dict()
        n_edges = 0
        for line in lines:
            lhs, rhs = line.split(' -> ')
            adj_list[lhs] = rhs.split(',')
            n_edges += len(adj_list[lhs])

        insert_point = 0
        curr_node = next(iter(adj_list.keys()))
        path = [curr_node]
        
        while True:
            # form a cycle and account for visited edges
            new_cycle = []
            start_node = curr_node
            while True:
                old_node = curr_node
                curr_node = adj_list[curr_node][0]
                adj_list[old_node].remove(curr_node)
                n_edges -= 1

                new_cycle.append(curr_node)
                if curr_node == start_node:
                    break

            path = path[:insert_point+1] + new_cycle + path[insert_point+1:]

            # done if all edges have been used
            if n_edges == 0:
                break
            
            # find new node with unexplored edges
            for i, n in enumerate(path):
                if len(adj_list[n]) != 0:
                    curr_node = n
                    insert_point = i
                    break

        return path
            
"""
Solve the Eulerian Path Problem.

Input: The adjacency list of a directed graph that has an Eulerian path.
Output: An Eulerian path in this graph.
"""
def eulerian_path(file):
    with open(file, 'r') as f:
        lines = f.read().splitlines()

        adj_list = dict()
        n_edges = 0
        for line in lines:
            lhs, rhs = line.split(' -> ')
            adj_list[lhs] = rhs.split(',')
            n_edges += len(adj_list[lhs])

        # transform the graph into one with a eulerian cycle
        unbalanced_from, unbalanced_to = None, None
        degree_counts = {k: 0 for k in adj_list.keys()}
        for k, v in adj_list.items():
            degree_counts[k] -= len(v)
            for n in v:
                if n not in degree_counts.keys():
                    degree_counts[n] = 0
                degree_counts[n] += 1
        
        for k, v in degree_counts.items():
            if v > 0:
                unbalanced_from = k
            elif v < 0:
                unbalanced_to = k

        if unbalanced_from not in adj_list.keys():
            adj_list[unbalanced_from] = []
        adj_list[unbalanced_from].append(unbalanced_to)
        n_edges += 1

        for k, v in adj_list.items():
            if unbalanced_from in v:
                adj_list[k].remove(unbalanced_from)
                adj_list[k].append(unbalanced_from)

        insert_point = 0
        curr_node = unbalanced_to
        path = [curr_node]
        
        while True:
            # form a cycle and account for visited edges
            new_cycle = []
            start_node = curr_node
            while True:
                old_node = curr_node
                curr_node = adj_list[curr_node][0]
                adj_list[old_node].remove(curr_node)
                n_edges -= 1

                new_cycle.append(curr_node)
                if curr_node == start_node:
                    break

            path = path[:insert_point+1] + new_cycle + path[insert_point+1:]

            # done if all edges have been used
            if n_edges == 0:
                break
            
            # find new node with unexplored edges
            for i, n in enumerate(path):
                if len(adj_list[n]) != 0:
                    curr_node = n
                    insert_point = i
                    break

        return path[:-1]

"""
Solve the String Reconstruction Problem.

Input: An integer k followed by a list of k-mers Patterns.
Output: A string Text with k-mer composition equal to Patterns. (If multiple answers exist, you may return any one.)
"""
def reconstruct_string(file):
    adj_list = dict()

    # construct De Bruijn graph from patterns
    with open(file, 'r') as f:
        patterns = f.read().splitlines()

        for p in patterns[1:]:
            prefix = p[:-1]
            suffix = p[1:]
            if prefix not in adj_list.keys():
                adj_list[prefix] = []
            adj_list[prefix].append(suffix)

    # transform the graph into one with a eulerian cycle
    unbalanced_from, unbalanced_to = None, None
    degree_counts = {k: 0 for k in adj_list.keys()}
    n_edges = 0
    for k, v in adj_list.items():
        degree_counts[k] -= len(v)
        n_edges += len(v)
        for n in v:
            if n not in degree_counts.keys():
                degree_counts[n] = 0
            degree_counts[n] += 1
    
    for k, v in degree_counts.items():
        if v > 0:
            unbalanced_from = k
        elif v < 0:
            unbalanced_to = k

    if unbalanced_from not in adj_list.keys():
        adj_list[unbalanced_from] = []
    adj_list[unbalanced_from].append(unbalanced_to)
    n_edges += 1

    # force unbalanced node to be last in path
    for k, v in adj_list.items():
        if unbalanced_from in v:
            adj_list[k].remove(unbalanced_from)
            adj_list[k].append(unbalanced_from)

    insert_point = 0
    curr_node = unbalanced_to
    path = [curr_node]
    
    # calculate eulerian path
    while True:
        # form a cycle and account for visited edges
        new_cycle = []
        start_node = curr_node
        while True:
            old_node = curr_node
            curr_node = adj_list[curr_node][0]
            adj_list[old_node].remove(curr_node)
            n_edges -= 1

            new_cycle.append(curr_node)
            if curr_node == start_node:
                break

        path = path[:insert_point+1] + new_cycle + path[insert_point+1:]

        # done if all edges have been used
        if n_edges == 0:
            break
        
        # find new node with unexplored edges
        for i, n in enumerate(path):
            if len(adj_list[n]) != 0:
                curr_node = n
                insert_point = i
                break

    # reconstruct sequence from eulerian path
    path = path[:-1]
    string = path[0]
    for s in path[1:]:
        string += s[-1]
    return string

"""
Solve the k-Universal Circular String Problem.

Input: An integer k.
Output: A k-universal circular string.
"""
def universal_string(file):
    def generate_universal_strings(k, curr, strings):
        if len(curr) == k:
            strings.append(curr)
            return
        generate_universal_strings(k, curr+"0", strings)
        generate_universal_strings(k, curr+"1", strings)

    strings = []
    with open(file, 'r') as f:
        k = int(f.read().splitlines()[0])
        generate_universal_strings(k, "", strings)
    
    adj_list = dict()
    for p in strings:
        prefix = p[:-1]
        suffix = p[1:]
        if prefix not in adj_list.keys():
            adj_list[prefix] = []
        adj_list[prefix].append(suffix)

    n_edges = 0
    for v in adj_list.values():
        n_edges += len(v)

    insert_point = 0
    curr_node = next(iter(adj_list.keys()))
    path = [curr_node]
    
    # calculate eulerian path
    while True:
        # form a cycle and account for visited edges
        new_cycle = []
        start_node = curr_node
        while True:
            old_node = curr_node
            curr_node = adj_list[curr_node][0]
            adj_list[old_node].remove(curr_node)
            n_edges -= 1

            new_cycle.append(curr_node)
            if curr_node == start_node:
                break

        path = path[:insert_point+1] + new_cycle + path[insert_point+1:]

        # done if all edges have been used
        if n_edges == 0:
            break
        
        # find new node with unexplored edges
        for i, n in enumerate(path):
            if len(adj_list[n]) != 0:
                curr_node = n
                insert_point = i
                break

    # reconstruct sequence from eulerian path
    path = path[:-(k-1)]
    string = path[0]
    for s in path[1:]:
        string += s[-1]
    return string

print(reconstruct_string('string_reconstruction.txt'))