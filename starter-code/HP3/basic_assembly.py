from os.path import join
import sys
import time
from collections import defaultdict, Counter
import sys
import os
import zipfile
import argparse
sys.path.insert(0, os.path.abspath(".."))
sys.path.insert(0, os.path.abspath("../.."))

"""
Example usage: python3 basic_assembly.py -r reads_hw3all_A_3/reads_hw3all_A_3_chr_1.txt -o hw3.txt -t hw3all_A_3_chr_1
"""

def parse_reads_file(reads_fn):
    """
    :param reads_fn: the file containing all of the reads
    :return: outputs a list of all paired-end reads
    """
    try:
        with open(reads_fn, 'r') as rFile:
            print("Parsing Reads")
            first_line = True
            count = 0
            all_reads = []
            for line in rFile:
                count += 1
                if count % 1000 == 0:
                    print(count, " reads done")
                if first_line:
                    first_line = False
                    continue
                ends = line.strip().split(',')
                all_reads.append(ends)
        return all_reads
    except IOError:
        print("Could not read file: ", reads_fn)
        return None

def db_graph_from_kmers(kmers):
    bases = ["A", "C", "G", "T"]
    adj_list = dict()
    for p in kmers:
        prefix = p[:-1]
        suffix = p[1:]
        if prefix not in adj_list.keys():
            adj_list[prefix] = []
        adj_list[prefix].append(suffix)
    return adj_list

def generate_contigs(kmers):
    adj_list = db_graph_from_kmers(kmers)
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

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='basic_assembly.py takes in data for homework assignment 3 consisting '
                                                 'of a set of reads and aligns the reads to the reference genome.')
    parser.add_argument('-r', '--reads', required=True, dest='reads_file',
                        help='File containg sequencing reads.')
    parser.add_argument('-o', '--outputFile', required=True, dest='output_file',
                        help='Output file name.')
    parser.add_argument('-t', '--outputHeader', required=True, dest='output_header',
                        help='String that needs to be outputted on the first line of the output file so that the\n'
                             'online submission system recognizes which leaderboard this file should be submitted to.\n'
                             'This HAS to be one of the following:\n'
                             '1) spectrum_A_1_chr_1 for 10K spectrum reads practice data\n'
                             '2) practice_A_2_chr_1 for 10k normal reads practice data\n'
                             '3) hw3all_A_3_chr_1 for project 3 for-credit data\n')
    args = parser.parse_args()
    reads_fn = args.reads_file

    input_reads = parse_reads_file(reads_fn)
    if input_reads is None:
        sys.exit(1)


    # Observed optimals: k = 25, thresh = 3
    k = 15
    thresh = 6

    kmer_counts = dict()
    for paired_reads in input_reads:
        for read in paired_reads:
            i = 0
            while i+k <= len(read):
                kmer = read[i:i+k]
                kmer_counts[kmer] = kmer_counts.get(kmer, 0) + 1
                i += 1

    kmers = [k for k,v in kmer_counts.items() if v >= thresh]
    contigs = generate_contigs(kmers)

    output_fn = args.output_file
    zip_fn = output_fn + '.zip'
    with open(output_fn, 'w') as output_file:
        output_file.write('>' + args.output_header + '\n')
        output_file.write('>ASSEMBLY\n')
        output_file.write('\n'.join(contigs))
    with zipfile.ZipFile(zip_fn, 'w') as myzip:
        myzip.write(output_fn)
