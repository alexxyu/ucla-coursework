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

paths = sorted(generate_contigs("advanced_reconstruction.txt"))
for p in paths:
    print(p)