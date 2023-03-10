import sys
import argparse
import numpy as np
import time
import zipfile


def parse_reads_file(reads_fn):
    """
    :param reads_fn: the file containing all of the reads
    :return: outputs a list of all paired-end reads

    HINT: This might not work well if the number of reads is too large to handle in memory
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


def parse_ref_file(ref_fn):
    """
    :param ref_fn: the file containing the reference genome
    :return: a string containing the reference genome
    """
    try:
        with open(ref_fn, 'r') as gFile:
            print("Parsing Ref")
            first_line = True
            ref_genome = ''
            for line in gFile:
                if first_line:
                    first_line = False
                    continue
                ref_genome += line.strip()
        return ref_genome
    except IOError:
        print("Could not read file: ", ref_fn)
        return None

def align_read(kmers, reference, read, k):
    """
    Aligns reads assuming there are only few read errors/SNPs

    :param kmers: a map of kmer string to list of positions in the reference
    :param reference: the reference genome sequence string
    :param read: a string containing a read
    :param k: the length of a kmer
    :return: a list of possible alignment positions
    """
    i = 0
    aligned_positions_snps = set()
    while i+k <= len(read):
        frag = read[i:i+k]
        if frag in kmers.keys():
            frag_positions = kmers[frag]
            for pos in frag_positions:
                diff_count = 0
                j, p = 0, pos-i
                while j < len(read) and p < len(reference):
                    if read[j] != reference[p]:
                        diff_count += 1
                    j += 1
                    p += 1
                
                if diff_count <= 2:
                    aligned_positions_snps.add(pos-i)
        i += k

    return sorted(list(aligned_positions_snps))

def align_read_frag(kmers, reference, read, k):
    """
    Aligns a fragment of the read to a perfectly matched position in the reference

    :param kmers: a map of kmer string to list of positions in the reference
    :param reference: the reference genome sequence string
    :param read: a string containing a read
    :param k: the length of a kmer
    :return: a list of possible alignment positions that match perfectly with a fragment of the read
    """

    aligned_positions = set()
    i = 0
    while i+k <= len(read):
        frag = read[i:i+k]
        if frag in kmers.keys():
            frag_positions = kmers[frag]
            for pos in frag_positions:
                j, p = 0, pos-i
                aligned_positions.add(p)
        i += k

    return sorted(list(aligned_positions))

def find_snps(reference, ref_start_pos, read, reversed_read=False):
    """
    :param reference: the reference genome sequence string
    :param ref_start_pos: the integer starting position of the alignment to the reference genome
    :param read: a string containing a read
    :param reversed_read: a boolean that determines whether the read should be reversed when checking alignment
    :return: a formatted list of possible SNPs
    """
    snps = []
    changed_before = False
    if reversed_read:
        read = read[::-1]

    for i, (refc, readc) in enumerate(zip(reference[ref_start_pos:ref_start_pos+len(read)], read)):
        if refc != readc and not changed_before:
            snps.append([refc, readc, ref_start_pos + i])
            changed_before = True
        else:
            changed_before = False
    return snps

def alignment_with_affine_gap(str1, str2, offset):
    """
    :param str1: the reference genome sequence substring
    :param str2: the read string
    :param offset: the absolute position in the reference at which str1 appears
    :return: the score of the alignment
    :return: a list of insertions
    :return: a list of deletions
    """

    GAP_EXTENSION_PENALTY = 1
    GAP_OPENING_PENALTY = 12
    MATCH_MISS_SCORE = 3

    def gaff_backtracker(backtrack, s1, s2, n, m, k):
        i, j = n, m
        a = 0

        insertions, deletions = [], []
        while i != 0 and j != 0:
            if backtrack[k][i][j] == 'd':
                if k == 0:
                    deletions.append([s1[i-1], a])
                    i -= 1
                k = 0
            elif backtrack[k][i][j] == 'r':
                if k == 2:
                    insertions.append([s2[j-1], a])
                    j -= 1
                k = 2
            elif backtrack[k][i][j] == 'b':
                if k == 0:
                    deletions.append([s1[i-1], a])
                    i -= 1
                elif k == 2:
                    insertions.append([s2[j-1], a])
                    j -= 1
                k = 1
            else:
                i -= 1
                j -= 1

            a += 1

        while i > 0:
            deletions.append([s1[i-1], a])
            a += 1
            i -= 1
        while j > 0:
            insertions.append([s2[j-1], a])
            a += 1
            j -= 1

        for s in insertions:
            s[1] = a - s[1] - 1 + offset
        for s in deletions:
            s[1] = a - s[1] - 1 + offset
        return insertions, deletions

    n, m = len(str1), len(str2)
    dp = [[[0 for _ in range(m+1)] for _ in range(n+1)] for _ in range(3)]
    backtrack = [[['' for _ in range(m+1)] for _ in range(n+1)] for _ in range(3)]

    for i in range(1, n+1):
        for j in range(1, m+1):
            match_score = MATCH_MISS_SCORE if str1[i-1] == str2[j-1] else -MATCH_MISS_SCORE
            
            dp[0][i][j] = max(dp[0][i-1][j] - GAP_EXTENSION_PENALTY, dp[1][i-1][j] - GAP_OPENING_PENALTY)
            if dp[0][i][j] == dp[0][i-1][j] - GAP_EXTENSION_PENALTY:
                backtrack[0][i][j] = 'd'
            else:
                backtrack[0][i][j] = 'b'

            dp[2][i][j] = max(dp[2][i][j-1] - GAP_EXTENSION_PENALTY, dp[1][i][j-1] - GAP_OPENING_PENALTY)
            if dp[2][i][j] == dp[2][i][j-1] - GAP_EXTENSION_PENALTY:
                backtrack[2][i][j] = 'r'
            else:
                backtrack[2][i][j] = 'b'

            dp[1][i][j] = max(dp[0][i][j], dp[1][i-1][j-1] + match_score, dp[2][i][j])
            if dp[1][i][j] == dp[0][i][j]:
                backtrack[1][i][j] = 'd'
            elif dp[1][i][j] == dp[2][i][j]:
                backtrack[1][i][j] = 'r'
            else:
                backtrack[1][i][j] = 'v'

    k = max(enumerate([dp[0][n][m], dp[1][n][m], dp[2][n][m]]), key=lambda x: x[1])[0]
    return dp[k][n][m], *gaff_backtracker(backtrack, str1, str2, n, m, k)

def align_reads(reference, reads, k, min_pair_distance, max_pair_distance, mut_thresh):
    """
    :param reference: the reference genome sequence string
    :param reads: a list of paired reads
    :param min_pair_distance: the minimum number of bases between paired reads
    :param min_pair_distance: the maximum number of bases between paired reads
    :param mut_thresh: the minimum number of reads needed for a mutated position to be considered a mutation
    :return: a formatted list of SNPs, insertions, and deletions
    """
    ref_kmers = get_kmers(reference, k)
    snps, ins, dels = [], [], []

    min_pair_distance, max_pair_distance = min_pair_distance + len(reads[0][0]), max_pair_distance + len(reads[0][0])
    for c, read_pair in enumerate(reads):
        if (c+1) % 500 == 0:
            print(f"Alignment progress: {c+1} out of {len(reads)}")

        curr_snps = len(snps)

        read_start, read_end = read_pair
        snp_pos1, snp_pos2 = align_read(ref_kmers, reference, read_start, k), align_read(ref_kmers, reference, read_end, k)
        reversed_snps1, reversed_snps2 = False, False

        # Need to reverse one of the reads since they are paired
        if len(snp_pos1) == 0:
            reversed_snps1 = True
            snp_pos1 = align_read(ref_kmers, reference, read_start[::-1], k)

        if len(snp_pos2) == 0:
            reversed_snps2 = True
            snp_pos2 = align_read(ref_kmers, reference, read_end[::-1], k)

        # Add possible SNPs from paired reads
        closest_read = 0
        for p1 in snp_pos1:
            if len(snp_pos2) == 0:
                break
            while closest_read < len(snp_pos2)-1 and snp_pos2[closest_read] < p1:
                closest_read += 1

            p2 = snp_pos2[closest_read]
            if abs(p2 - p1) >= min_pair_distance and abs(p2 - p1) <= max_pair_distance:
                snps.extend(find_snps(reference, p1, read_start, reversed_read=reversed_snps1))
                snps.extend(find_snps(reference, p2, read_end, reversed_read=reversed_snps2))

        # Check indels if no SNPs found
        if len(snps) == curr_snps:
            positions = align_read_frag(ref_kmers, reference, read_start, k)
            if len(positions) == 0:
                read_start = read_start[::-1]
                positions = align_read_frag(ref_kmers, reference, read_start, k)
            max_score, best_ins, best_dels = -1000000, [], []
            for p in positions:
                curr_score, curr_ins, curr_dels = alignment_with_affine_gap(reference[p:p+len(read_start)], read_start, p)
                if curr_score > max_score:
                    max_score, best_ins, best_dels = curr_score, curr_ins, curr_dels
            ins.extend(best_ins)
            dels.extend(best_dels)

            positions = align_read_frag(ref_kmers, reference, read_end, k)
            if len(positions) == 0:
                read_end = read_end[::-1]
                positions = align_read_frag(ref_kmers, reference, read_end, k)
            max_score, best_ins, best_dels = -1000000, [], []
            for p in positions:
                curr_score, curr_ins, curr_dels = alignment_with_affine_gap(reference[p:p+len(read_end)], read_end, p)
                if curr_score > max_score:
                    max_score, best_ins, best_dels = curr_score, curr_ins, curr_dels
            ins.extend(best_ins)
            dels.extend(best_dels)

    # Process SNPs for consensus
    snps.sort(key=lambda v: v[2])
    snp_calls = []
    i = 0
    while i < len(snps):
        start = i
        curr_pos = snps[i][2]
        while i < len(snps) and snps[i][2] == curr_pos:
            i += 1
        if i - start >= mut_thresh:
            snp_calls.append(get_consensus_snp(snps[start:i]))

    # Process insertions for consensus
    ins.sort(key=lambda v: v[1])
    insertions = []
    i = 0
    while i < len(ins):
        start = i
        curr_pos = ins[i][1]
        while i < len(ins) and ins[i][1] == curr_pos:
            i += 1
        if i - start >= mut_thresh:
            insertions.append(get_consensus_indel(ins[start:i]))
    insertions = collapse_indels(insertions)

    # Process deletions for consensus
    dels.sort(key=lambda v: v[1])
    deletions = []
    i = 0
    while i < len(dels):
        start = i
        curr_pos = dels[i][1]
        while i < len(dels) and dels[i][1] == curr_pos:
            i += 1
        if i - start >= mut_thresh:
            deletions.append(get_consensus_indel(dels[start:i]))
    deletions = collapse_indels(deletions)

    return snp_calls, insertions, deletions
    
def get_consensus_snp(snps):
    """
    :param snps: a list of candidate SNPs convering the same position
    :return: a formatted list representing an SNP with the consensus mutation
    """
    mut_counts = {'A': 0, 'T': 0, 'C': 0, 'G': 0}
    for snp in snps:
        mut_counts[snp[1]] += 1
    return [snps[0][0], max(mut_counts, key=mut_counts.get), snps[0][2]]

def get_consensus_indel(indels):
    """
    :param indels: a list of candidate indels convering the same position
    :return: a formatted list representing an indel with the consensus mutation
    """
    mut_counts = {'A': 0, 'T': 0, 'C': 0, 'G': 0}
    for indel in indels:
        mut_counts[indel[0]] += 1
    return [max(mut_counts, key=mut_counts.get), indels[0][1]]

def collapse_indels(indels):
    """
    :param indels: a list of indels
    :return: a list with the indels combined if they are in consecutive positions
    """
    collapsed_indels = []
    curr_indel = indels[0]
    for i, indel in enumerate(indels[1:]):
        if indel[1] == indels[i][1]+1:
            curr_indel[0] = curr_indel[0] + indel[0]
        else:
            collapsed_indels.append(curr_indel)
            curr_indel = indel
    collapsed_indels.append(curr_indel)
    return collapsed_indels

def get_kmers(reference, k):
    """
    :param reference: the reference genome sequence string
    :param k: the length of a kmer
    :return: a map of kmer string to list of positions in the reference
    """
    kmers = dict()
    for i in range(len(reference)):
        kmer = reference[i:i+k]
        if len(kmer) != k:
            break
        if kmer not in kmers.keys():
            kmers[kmer] = []
        kmers[kmer].append(i)
    return kmers

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='basic_hasher.py takes in data for homework assignment 2 consisting '
                                     'of a genome and a set of reads and aligns the reads to the reference genome, '
                                     'then calls SNPS and indels based on this alignment.')
    parser.add_argument('-g', '--referenceGenome', required=True, dest='reference_file',
                        help='File containing a reference genome.')
    parser.add_argument('-r', '--reads', required=True, dest='reads_file',
                        help='File containg sequencing reads.')
    parser.add_argument('-o', '--outputFile', required=True, dest='output_file',
                        help='Output file name.')
    parser.add_argument('-t', '--outputHeader', required=True, dest='output_header',
                        help='String that needs to be outputted on the first line of the output file so that the\n'
                             'online submission system recognizes which leaderboard this file should be submitted to.\n'
                             'This HAS to be one of the following:\n'
                             '1) practice_W_3_chr_1 for 10K length genome practice data\n'
                             '2) practice_E_1_chr_1 for 1 million length genome practice data\n'
                             '3) hw2undergrad_E_2_chr_1 for project 2 undergrad for-credit data\n'
                             '4) hw2grad_M_1_chr_1 for project 2 grad for-credit data\n')
    args = parser.parse_args()
    reference_fn = args.reference_file
    reads_fn = args.reads_file

    input_reads = parse_reads_file(reads_fn)
    if input_reads is None:
        sys.exit(1)
    reference = parse_ref_file(reference_fn)
    if reference is None:
        sys.exit(1)

    snps, insertions, deletions = align_reads(reference, input_reads, 15, 90, 110, 7)

    output_fn = args.output_file
    zip_fn = output_fn + '.zip'
    with open(output_fn, 'w') as output_file:
        output_file.write('>' + args.output_header + '\n>SNP\n')
        for x in snps:
            output_file.write(','.join([str(u) for u in x]) + '\n')
        output_file.write('>INS\n')
        for x in insertions:
            output_file.write(','.join([str(u) for u in x]) + '\n')
        output_file.write('>DEL\n')
        for x in deletions:
            output_file.write(','.join([str(u) for u in x]) + '\n')
    with zipfile.ZipFile(zip_fn, 'w') as myzip:
        myzip.write(output_fn)
