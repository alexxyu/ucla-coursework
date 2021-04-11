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

adj_list = db_graph_from_kmers("string_reconstruction.txt")
for k, v in sorted(adj_list.items(), key=lambda k: k[0]):
    if len(v) > 0:
        print(f"{k} -> {','.join(v)}")