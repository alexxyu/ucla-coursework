"""
Code Challenge: Solve the String Reconstruction from Read-Pairs Problem.

Input: Integers k and d followed by a collection of paired k-mers PairedReads.
Output: A string Text with (k, d)-mer composition equal to PairedReads.
"""
def reconstruct_string_from_read_pairs(file):
    adj_list = dict()
    kmer_size = 0
    d = 0

    # construct De Bruijn graph from patterns
    with open(file, 'r') as f:
        patterns = f.read().splitlines()
        kmer_size, d = patterns[0].split(' ')
        kmer_size = int(kmer_size)
        d = int(d)

        for p in patterns[1:]:
            pa, pb = p.split('|')

            prefix = pa[:-1] + pb[:-1]
            suffix = pa[1:] + pb[1:]
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
    first_pair_path = [p[:len(p)//2] for p in path]
    prefix_string = first_pair_path[0]
    for s in first_pair_path[1:]:
        prefix_string += s[-1]

    second_pair_path = [p[len(p)//2:] for p in path]
    suffix_string = second_pair_path[0]
    for s in second_pair_path[1:]:
        suffix_string += s[-1]

    return prefix_string + suffix_string[-(kmer_size + d):]

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
Contig Generation Problem: Generate the contigs from a collection of reads (with imperfect coverage).

Input: A collection of k-mers Patterns.
Output: All contigs in DeBruijn(Patterns).
"""
def generate_contigs(file):
    adj_list = db_graph_from_kmers(file)
    reverse_adj_list = {k:[] for k in adj_list.keys()}
    for k, v in adj_list.items():
        for n in v:
            if n not in reverse_adj_list.keys():
                reverse_adj_list[n] = []
            reverse_adj_list[n].append(k)

    def path_to_string(path):
        string = path[0]
        for s in path[1:]:
            string += s[-1]
        return string

    def get_inout_degrees(node, graph, rev_graph):
        if node not in graph.keys():
            out_degree = 0
        else:
            out_degree = len(graph[node])
        in_degree = len(rev_graph[node])
        return in_degree, out_degree

    """
    Generates all non-branching paths in a graph. It iterates through all nodes of the graph that 
    are not 1-in-1-out nodes and generates all non-branching paths starting at each such node.
    """
    def maximal_nonbranching_paths(graph, rev_graph):
        paths = []
        for v in adj_list.keys():
            in_degree, out_degree = get_inout_degrees(v, graph, rev_graph)

            # expand non-branching path from current node
            if in_degree != 1 or out_degree != 1:
                if out_degree > 0:
                    for w in adj_list[v]:
                        path = [v, w]
                        in_degree, out_degree = get_inout_degrees(w, graph, rev_graph)
                        while in_degree == 1 and out_degree == 1:
                            w = adj_list[w][0]
                            path.append(w)
                            in_degree, out_degree = get_inout_degrees(w, graph, rev_graph)
                        paths.append(path)
        
        paths = [path_to_string(s) for s in paths]
        return paths

    return maximal_nonbranching_paths(adj_list, reverse_adj_list)

print(reconstruct_string_from_read_pairs("advanced_reconstruction.txt"))