import sys
import argparse
import time
import zipfile


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
    :param kmers: a map of kmer string to list of positions in the reference
    :param reference: the reference genome sequence string
    :param read: a string containing a read
    :param k: the length of a kmer
    :return: a list of possible alignment positions
    """
    i = 0
    min_snp_positions = set()
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
                    min_snp_positions.add(pos-i)
        i += k
    return sorted(list(min_snp_positions))

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

def align_reads(reference, reads, k, min_pair_distance, max_pair_distance, snp_thresh):
    """
    :param reference: the reference genome sequence string
    :param reads: a list of paired reads
    :param min_pair_distance: the minimum number of bases between paired reads
    :param min_pair_distance: the maximum number of bases between paired reads
    :param snp_thresh: the minimum number of reads needed for a mutated position to be considered an SNP
    :return: a formatted list of SNPs
    """
    ref_kmers = get_kmers(reference, k)
    snps = []

    min_pair_distance, max_pair_distance = min_pair_distance + len(reads[0][0]), max_pair_distance + len(reads[0][0])
    for read_pair in reads:
        read_start, read_end = read_pair
        snp_pos1, snp_pos2 = align_read(ref_kmers, reference, read_start, k), align_read(ref_kmers, reference, read_end, k)
        reversed1, reversed2 = False, False

        # Need to reverse one of the reads since they are paired
        if len(snp_pos1) == 0:
            reversed1 = True
            snp_pos1 = align_read(ref_kmers, reference, read_start[::-1], k)
        if len(snp_pos2) == 0:
            reversed2 = True
            snp_pos2 = align_read(ref_kmers, reference, read_end[::-1], k)

        closest_read = 0
        for p1 in snp_pos1:
            if len(snp_pos2) == 0:
                continue
            while closest_read < len(snp_pos2)-1 and snp_pos2[closest_read] < p1:
                closest_read += 1

            p2 = snp_pos2[closest_read]
            if abs(p2 - p1) >= min_pair_distance and abs(p2 - p1) <= max_pair_distance:
                snps.extend(find_snps(reference, p1, read_start, reversed_read=reversed1))
                snps.extend(find_snps(reference, p2, read_end, reversed_read=reversed2))

    snps.sort(key=lambda v: v[2])
    result = []

    i = 0
    while i < len(snps):
        start = i
        curr_pos = snps[i][2]
        while i < len(snps) and snps[i][2] == curr_pos:
            i += 1

        if i - start >= snp_thresh:
            result.append(get_consensus_snp(snps[start:i]))
    return result
    
def get_consensus_snp(snps):
    """
    :param snps: a list of candidate SNPs convering the same position
    :return: a formatted list representing an SNP with the consensus mutation
    """
    mut_counts = {'A': 0, 'T': 0, 'C': 0, 'G': 0}
    for snp in snps:
        mut_counts[snp[1]] += 1
    return [snps[0][0], max(mut_counts, key=mut_counts.get), snps[0][2]]

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
    parser = argparse.ArgumentParser(description='basic_aligner.py takes in data for homework assignment 1 consisting '
                                     'of a genome and a set of reads and aligns the reads to the reference genome, '
                                     'then calls SNPs based on this alignment')
    parser.add_argument('-g', '--referenceGenome', required=True, dest='reference_file',
                        help='File containing a reference genome.')
    parser.add_argument('-r', '--reads', required=True, dest='reads_file',
                        help='File containg sequencing reads.')
    parser.add_argument('-o', '--outputFile', required=True, dest='output_file',
                        help='Output file name.')
    parser.add_argument('-t', '--outputHeader', required=True, dest='output_header',
                        help='String that needs to be outputted on the first line of the output file so that the '
                             'online submission system recognizes which leaderboard this file should be submitted to.'
                             'This HAS to be practice_W_1_chr_1 for the practice data and hw1_W_2_chr_1 for the '
                             'for-credit assignment!')
    args = parser.parse_args()
    reference_fn = args.reference_file
    reads_fn = args.reads_file

    input_reads = parse_reads_file(reads_fn)
    if input_reads is None:
        sys.exit(1)
    reference = parse_ref_file(reference_fn)
    if reference is None:
        sys.exit(1)

    """
        TODO: Call functions to do the actual read alignment here
        
    """
    #snps = [['A', 'G', 3425]]
    snps = align_reads(reference, input_reads, 15, 90, 110, 4)

    output_fn = args.output_file
    zip_fn = output_fn + '.zip'
    with open(output_fn, 'w') as output_file:
        header = '>' + args.output_header + '\n>SNP\n'
        output_file.write(header)
        for x in snps:
            line = ','.join([str(u) for u in x]) + '\n'
            output_file.write(line)

        tails = ('>' + x for x in ('STR', 'CNV', 'ALU', 'INV', 'INS', 'DEL'))
        output_file.write('\n'.join(tails))

    with zipfile.ZipFile(zip_fn, 'w') as myzip:
        myzip.write(output_fn)
