#!/usr/bin/python3

import sys
import csv
from os import path

CORRUPTED_EXIT_CODE = 2

if len(sys.argv) != 2:
    print("Usage: python %s <FS_SUMMARY>" % sys.argv[0], file=sys.stderr)
    exit(1)

if not path.isfile(sys.argv[1]):
    print("File %s does not exist" % sys.argv[1], file=sys.stderr)
    exit(1)

exit_code = 0

n_total_blocks = 0
n_total_inodes = 0
first_avail_inode = 0

# TODO: figure out reserved blocks
reserved_blocks = [0, 1, 2, 3, 4, 5, 6, 7, 64]

inode_info = dict()
inode_blocks = dict()
inode_link_counts = dict()

free_blocks = set()
free_inodes = set()

# Parse filesystem summary
with open(sys.argv[1], 'r') as file:
    reader = list(csv.reader(file, delimiter=','))

    for row in reader:
        if row[0] == "SUPERBLOCK":
            n_total_blocks = int(row[1])
            n_total_inodes = int(row[2])
            first_avail_inode = int(row[7])

    for row in reader:
        type, num = row[:2]
        if type == "IFREE":
            free_inodes.add(int(num))
        elif type == "BFREE":
            free_blocks.add(int(num))
        elif type == "INODE":
            inode = int(num)
            if inode not in inode_blocks:
                inode_blocks[inode] = []
            inode_info[inode] = (row[2], int(row[3]), int(row[6])) # file type, mode, link count
            inode_blocks[inode].extend([int(s) for s in row[12:]])
        elif type == "INDIRECT":
            inode_blocks[inode].append(int(row[5]))
        elif type == "DIRENT":
            inode = int(row[3])
            if inode not in inode_link_counts:
                inode_link_counts[inode] = 0
            inode_link_counts[inode] += 1

duplicates = []
block_to_inode = dict()

# TODO: print offsets with block reports

# Report invalid and allocated blocks
for inode in inode_blocks.keys():
    filetype, mode, link_count = inode_info[inode]
    for block in inode_blocks[inode]:
        if block == 0:
            continue
        if block < 0 or block > n_total_blocks:
            print("INVALID BLOCK %d IN INODE %d" % (block, inode))
            exit_code = CORRUPTED_EXIT_CODE
        if block in free_blocks:
            print("ALLOCATED BLOCK %d IN INODE %d")
            exit_code = CORRUPTED_EXIT_CODE
        
        if block not in block_to_inode:
            block_to_inode[block] = []
        block_to_inode[block].append((inode, *inode_info[inode]))

# Report duplicated blocks
for block in block_to_inode.keys():
    if len(block_to_inode[block]) > 1:
        for inode, filetype, mode, link_count in block_to_inode[block]:
            print("DUPLICATED BLOCK %d IN INODE %d" % (block, inode))
            exit_code = CORRUPTED_EXIT_CODE

# Report unreferenced blocks
for block in range(1, n_total_blocks+1):
    if block not in block_to_inode and block not in free_blocks and block not in reserved_blocks:
        print("UNREFERENCED BLOCK %d" % block)

# Report allocated inodes that were denoted as free in bitmap
for inode in inode_info.keys():
    if inode in free_inodes:
        print("ALLOCATED INODE %d ON FREELIST" % inode)
        exit_code = CORRUPTED_EXIT_CODE

# Report unallocated inodes that were denoted as used in bitmap
for inode in range(first_avail_inode, n_total_inodes+1):
    if inode not in free_inodes and inode not in inode_info.keys():
        print("UNALLOCATED INODE %d NOT ON FREELIST" % inode)
        exit_code = CORRUPTED_EXIT_CODE

# Report inodes with wrong link count
for inode in inode_info.keys():
    expected = inode_info[inode][2]
    if inode not in inode_link_counts:
        # Edge case: unreferenced inode
        print("INODE %d HAS 0 LINKS BUT LINKCOUNT IS %d" % (inode, expected))
        exit_code = CORRUPTED_EXIT_CODE
    else:
        link_count = inode_link_counts[inode]
        if link_count != expected:
            print("INODE %d HAS %d LINKS BUT LINKCOUNT IS %d" % (inode, link_count, expected))
            exit_code = CORRUPTED_EXIT_CODE

# TODO: Check validity and allocation status of each dirent inode

# TODO: Check validity of '.' and '..' inodes in dirent

exit(exit_code)