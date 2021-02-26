#!/usr/bin/python3

'''
NAME: Alex Yu,Nicolas Trammer
EMAIL: alexy23@g.ucla.edu,colet@g.ucla.edu
ID: 105295708,005395690
'''

import sys
import csv
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
first_avail_inode = 0

# TODO: reserved blocks
reserved_blocks = []

inode_info = dict()             # stores info gathered about inode (like link count)
inode_blocks = dict()           # maps inode number to data block numbers

free_blocks = set()
free_inodes = set()

# Parse filesystem summary
with open(sys.argv[1], 'r') as file:
    reader = list(csv.reader(file, delimiter=','))

    for row in reader:
        if row[0] == "SUPERBLOCK":
            n_total_blocks = int(row[1])
            n_total_inodes = int(row[2])
            block_size = int(row[3])
            first_avail_inode = int(row[7])

    entries_per_indirect_block = block_size / 4

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
            pass

duplicates = []
block_to_inode = dict()     # reverse maps block number to inode references

# TODO: Report reserved blocks
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
            # print_block_error(block, offset, level, inode, "RESERVED")
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
for block in range(1, n_total_blocks+1):
    if block not in block_to_inode and block not in free_blocks and block not in reserved_blocks:
        # print("UNREFERENCED BLOCK %d" % block)
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

# TODO: Report inodes with wrong link count

# TODO: Check validity and allocation status of each dirent inode

# TODO: Check validity of '.' and '..' inodes in dirent

exit(exit_code)