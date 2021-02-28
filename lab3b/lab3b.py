#!/usr/bin/python3

'''
NAME: Alex Yu,Nicolas Trammer
EMAIL: alexy23@g.ucla.edu,colet@g.ucla.edu
ID: 105295708,005395690
'''

import sys
import csv
import math
from os import path

CORRUPTED_EXIT_CODE = 2
N_DIRECT_IBLOCKS = 12

def print_block_error(block, offset, level, inode, error):
    indirectness_label = ""
    if level == 1:
        indirectness_label = " INDIRECT"
    elif level == 2:
        indirectness_label = " DOUBLE INDIRECT"
    elif level == 3:
        indirectness_label = " TRIPLE INDIRECT"
    print("%s%s BLOCK %d IN INODE %d AT OFFSET %d" % (error, indirectness_label, block, inode, offset))

if len(sys.argv) != 2:
    print("Usage: python %s <FS_SUMMARY>" % sys.argv[0], file=sys.stderr)
    exit(1)
if not path.isfile(sys.argv[1]):
    print("File %s does not exist" % sys.argv[1], file=sys.stderr)
    exit(1)

exit_code = 0

block_size = 0
n_total_blocks = 0
n_total_inodes = 0
inode_size = 0
first_avail_inode = 0

# Boot block is always reserved
reserved_blocks = set()
reserved_blocks.add(0)

inode_info = dict()             # stores info gathered about inode (like link count)
inode_blocks = dict()           # maps inode number to data block numbers

free_blocks = set()
free_inodes = set()

dirents = []

# Parse filesystem summary
with open(sys.argv[1], 'r') as file:
    reader = list(csv.reader(file, delimiter=','))

    for row in reader:
        if row[0] == "SUPERBLOCK":
            n_total_blocks = int(row[1])
            n_total_inodes = int(row[2])
            block_size = int(row[3])
            inode_size = int(row[4])
            first_avail_inode = int(row[7])

    entries_per_indirect_block = block_size / 4
    if block_size == 1024:
        # Superblock
        reserved_blocks.add(1)
        # Block group descriptor table
        reserved_blocks.add(2)
    else:
        # Super block is already reserved as part of the boot block
        # Block group descriptor table
        reserved_blocks.add(1)

    for row in reader:
        type, num = row[:2]
        if type == "IFREE":
            free_inodes.add(int(num))
        elif type == "BFREE":
            free_blocks.add(int(num))
        elif type == "GROUP":
            # Both the block and inode bitmaps must be exactly one block long.
            reserved_blocks.add(int(row[6]))
            reserved_blocks.add(int(row[7]))
            # The inode table length, however, varies.
            inode_table_length = int(row[3]) * inode_size
            inode_table_block_start = int(row[8])
            for i in range(math.ceil(inode_table_length / block_size)):
                reserved_blocks.add(inode_table_block_start + i)
        elif type == "INODE":
            inode = int(num)
            if inode not in inode_blocks:
                inode_blocks[inode] = []

            filetype, mode, link_count = row[2], int(row[3]), int(row[6])
            inode_info[inode] = (filetype, mode, link_count)

            # Extract block numbers, logical offset, and level of indirectness
            for offset, s in enumerate(row[12:]):
                level = 0
                if offset >= N_DIRECT_IBLOCKS:
                    # These are indirect blocks, so we need to calculate the offset
                    level = offset-N_DIRECT_IBLOCKS + 1
                    offset = N_DIRECT_IBLOCKS
                    if level == 2:
                        offset += entries_per_indirect_block
                    if level == 3:
                        offset += entries_per_indirect_block * (1 + entries_per_indirect_block)
                inode_blocks[inode].append((int(s), offset, level))
        elif type == "INDIRECT":
            inode = int(num)
            if inode not in inode_blocks:
                inode_blocks[inode] = []
            
            # Extract block number, logical offset, and level of indirectness
            block, offset, level = int(row[5]), int(row[3]), int(row[2])-1
            inode_blocks[inode].append((block, offset, level))
        elif type == "DIRENT":
            parent, inode, name = int(row[1]), int(row[3]), row[6]
            dirents.append((parent, inode, name))

duplicates = []
block_to_inode = dict()     # reverse maps block number to inode references

# Report invalid and allocated blocks
for inode in inode_blocks.keys():
    filetype, mode, link_count = inode_info[inode]
    for block, offset, level in inode_blocks[inode]:
        if block == 0:
            continue
        if block < 0 or block > n_total_blocks:
            print_block_error(block, offset, level, inode, "INVALID")
            exit_code = CORRUPTED_EXIT_CODE
        if block in free_blocks:
            print("ALLOCATED BLOCK %d ON FREELIST" % block)
            exit_code = CORRUPTED_EXIT_CODE
        if block in reserved_blocks:
            print_block_error(block, offset, level, inode, "RESERVED")
            exit_code = CORRUPTED_EXIT_CODE
        
        if block not in block_to_inode:
            block_to_inode[block] = []
        block_to_inode[block].append((inode, offset, level, *inode_info[inode]))

# Report duplicated blocks
for block in block_to_inode.keys():
    if len(block_to_inode[block]) > 1:
        for inode, offset, level, filetype, mode, link_count in block_to_inode[block]:
            print_block_error(block, offset, level, inode, "DUPLICATE")
            exit_code = CORRUPTED_EXIT_CODE

# Report unreferenced blocks
for block in range(1, n_total_blocks):
    if block not in block_to_inode and block not in free_blocks and block not in reserved_blocks:
        print("UNREFERENCED BLOCK %d" % block)
        exit_code = CORRUPTED_EXIT_CODE

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
real_counts = dict()
for inode in inode_info:
    real_counts[inode] = 0
for (_, inode, _) in dirents:
    if inode in real_counts:
        real_counts[inode] += 1
for inode in inode_blocks:
    (_, _, link_count) = inode_info[inode]
    if link_count != real_counts[inode]:
        print("INODE %d HAS %d LINKS BUT LINKCOUNT IS %d" % (inode, real_counts[inode], link_count))
        exit_code = CORRUPTED_EXIT_CODE

# Check validity and allocation status of each dirent inode
# Note that the code currently doesn't care if a directory has more than one parent (which should be reported somehow but the
# spec does not mention it. It also does not complain if a directory is missing a . or .. entry (which again the spec ignores).
for (parent, inode, name) in dirents:
    if inode < 1 or inode > n_total_inodes:
        print("DIRECTORY INODE %d NAME %s INVALID INODE %d" % (parent, name, inode))
        exit_code = CORRUPTED_EXIT_CODE
    elif inode in free_inodes and not inode in inode_info:
        print("DIRECTORY INODE %d NAME %s UNALLOCATED INODE %d" % (parent, name, inode))
        exit_code = CORRUPTED_EXIT_CODE

# Check validity of '.' and '..' inodes in dirent
parents = dict()
# Root inode is its own parent
parents[2] = 2
for (parent, inode, name) in dirents:
    # Note that directories must have exactly 1 parent, so the fact that the parent field is overwritten does not matter
    if name != "'.'" and name != "'..'":
        parents[inode] = parent
for (directory, target_inode, name) in dirents:
    expected = 0
    if name == "'.'":
        expected = directory
    elif name == "'..'" and directory in parents:
        expected = parents[directory]
    else:
        continue
    if target_inode != expected:
        print("DIRECTORY INODE %d NAME %s LINK TO INODE %d SHOULD BE %d" % (directory, name, target_inode, expected))
        exit_code = CORRUPTED_EXIT_CODE

exit(exit_code)
