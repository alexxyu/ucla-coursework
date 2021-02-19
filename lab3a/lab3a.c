/*
NAME: Alex Yu
EMAIL: alexy23@g.ucla.edu
ID: 105295708
*/

#include <time.h>
#include <stdio.h>
#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>

#include "ext2_fs.h"

#define ROOT_BLOCK_SIZE 1024
#define N_DIRECT_IBLOCKS 12
#define N_TOTAL_IBLOCKS 15
#define TIME_STR_SIZE 32

int img_fd;
unsigned int block_size;

void set_timestr(char* timestr, int size, time_t sec) {
    struct tm ctime = *gmtime(&sec);
    strftime(timestr, size, "%m/%d/%y %H:%M:%S", &ctime);
}

int is_entry_used(int entry_id, char* bitmap) {
    if(entry_id == 0) return 1;     // boot block is always used

    int index = (entry_id - 1)/8;
    int offset = (entry_id - 1)%8;
    return (bitmap[index] >> offset) & 1;
}

char get_file_type(unsigned int i_mode) {
    if(S_ISREG(i_mode)) return 'f';
    if(S_ISDIR(i_mode)) return 'd';
    if(S_ISLNK(i_mode)) return 's';

    return '?';
}

unsigned int get_start_of_block(unsigned int block) {
    return ROOT_BLOCK_SIZE + (block - 1) * block_size;
}

/*
* DIRENT
* parent inode number (decimal) ... the I-node number of the directory that contains this entry
* logical byte offset (decimal) of this entry within the directory
* inode number of the referenced file (decimal)
* entry length (decimal)
* name length (decimal)
* name (string, surrounded by single-quotes). Don't worry about escaping, we promise there will be no single-quotes or 
*   commas in any of the file names.
*/
void read_directory(int parent_inode, unsigned int block) {
    if(block != 0) {
        struct ext2_dir_entry dir_entry;
        unsigned int inode_offset = 0;
        unsigned int iblock_start = get_start_of_block(block);

        while(inode_offset < block_size) {
            pread(img_fd, &dir_entry, sizeof(dir_entry), iblock_start + inode_offset);
            if(dir_entry.inode != 0) {
                char file_name[EXT2_NAME_LEN+1];
                memcpy(file_name, dir_entry.name, dir_entry.name_len);
                file_name[dir_entry.name_len] = 0;
                fprintf(stdout, "DIRENT,%u,%u,%u,%u,%u,\'%s\'\n",
                    parent_inode,
                    inode_offset,
                    dir_entry.inode,
                    dir_entry.rec_len,
                    dir_entry.name_len,
                    file_name
                );
            }

            // Increment offset to next entry
            inode_offset += dir_entry.rec_len;
        }
    }
}

/*
* INDIRECT
* I-node number of the owning file (decimal)
* (decimal) level of indirection for the block being scanned ... 1 for single indirect, 2 for double indirect, 3 for 
*   triple
* logical block offset (decimal) represented by the referenced block. If the referenced block is a data block, this is 
*   the logical block offset of that block within the file. If the referenced block is a single- or double-indirect 
*   block, this is the same as the logical offset of the first data block to which it refers.
* block number of the (1, 2, 3) indirect block being scanned (decimal) . . . not the highest level block (in the 
*   recursive scan), but the lower level block that contains the block reference reported by this entry.
* block number of the referenced block (decimal)
*/
void read_indirect(int block, int inode, int logical_offset, int n_entries, int level, int is_dir) {
    if(level <= 0) {
        return;
    }

    unsigned int* entries = (unsigned int*) malloc(n_entries * sizeof(unsigned int));
    pread(img_fd, entries, block_size, get_start_of_block(block));
    for(int i=0; i<n_entries; i++) {
        if(entries[i] != 0) {
            if(is_dir) {
                // All directory entries need to be printed
                read_directory(inode, entries[i]);
            }
            fprintf(stdout, "INDIRECT,%d,%d,%d,%d,%d\n", 
                inode, 
                level, 
                logical_offset+i, 
                block, 
                entries[i]
            );

            // Recursively read indirect entries
            read_indirect(entries[i], inode, logical_offset+i, n_entries, level-1, is_dir);
        }
    }
    free(entries);
}

/*
* INODE
* inode number (decimal)
* file type ('f' for file, 'd' for directory, 's' for symbolic link, '?" for anything else)
* mode (low order 12-bits, octal ... suggested format "%o")
* owner (decimal)
* group (decimal)
* link count (decimal)
* time of last I-node change (mm/dd/yy hh:mm:ss, GMT)
* modification time (mm/dd/yy hh:mm:ss, GMT)
* time of last access (mm/dd/yy hh:mm:ss, GMT)
* file size (decimal)
* number of (512 byte) blocks of disk space (decimal) taken up by this file
*/
void read_inode(int i, unsigned int offset) {
    struct ext2_inode inode;
    pread(img_fd, &inode, sizeof(inode), offset + (i-1)*sizeof(inode));

    // Format time information
    char c_timestr[TIME_STR_SIZE], m_timestr[TIME_STR_SIZE], a_timestr[TIME_STR_SIZE];
    set_timestr(c_timestr, TIME_STR_SIZE, inode.i_ctime);
    set_timestr(m_timestr, TIME_STR_SIZE, inode.i_mtime);
    set_timestr(a_timestr, TIME_STR_SIZE, inode.i_atime);

    char file_type = get_file_type(inode.i_mode);
    if(file_type == '?') {
        return;
    }

    fprintf(stdout, "INODE,%u,%c,%o,%u,%u,%u,%s,%s,%s,%u,%u",
        i,
        file_type,
        inode.i_mode & 0xFFF,
        inode.i_uid,
        inode.i_gid,
        inode.i_links_count,
        c_timestr,
        m_timestr,
        a_timestr,
        inode.i_size,
        inode.i_blocks
    );
    if(file_type == 'd' || file_type == 'f' || (file_type == 's' && inode.i_size >= 60)) {
        for(int i=0; i<N_TOTAL_IBLOCKS; i++) {
            fprintf(stdout, ",%d", inode.i_block[i]);
        }
    }
    fprintf(stdout, "\n");

    if(file_type == 'd') {
        for(int iblock=0; iblock < N_DIRECT_IBLOCKS; iblock++) {
            read_directory(i, inode.i_block[iblock]);
        }
    }

    // Check indirect blocks
    int logical_offset = N_DIRECT_IBLOCKS, res = 1;
    int n_entries = block_size / sizeof(unsigned int);
    for(int k=0; k<N_TOTAL_IBLOCKS - N_DIRECT_IBLOCKS; k++) {
        read_indirect(inode.i_block[N_DIRECT_IBLOCKS+k], i, logical_offset, n_entries, k+1, file_type == 'd');

        // Each k-level indirection block references n^k blocks (k > 1)
        // logical_offset: 12 for k=1, 12+256 for k=2, 12+256+65536 for k=3 if n_entries = 256
        res *= n_entries;
        logical_offset += res; 
    }
}

int main(int argc, char* argv[]) {
    if(argc != 2) {
        fprintf(stderr, "Usage: %s <IMG_FILE>\n", argv[0]);
        exit(1);
    }

    char* img_filename = argv[1];
    img_fd = open(img_filename, O_RDONLY);
    if(img_fd < 0) {
        fprintf(stderr, "Cannot open specified image %s: %s\n", img_filename, strerror(errno));
        exit(1);
    }

    /*
    * SUPERBLOCK
    * total number of blocks (decimal)
    * total number of i-nodes (decimal)
    * block size (in bytes, decimal)
    * i-node size (in bytes, decimal)
    * blocks per group (decimal)
    * i-nodes per group (decimal)
    * first non-reserved i-node (decimal)
    */
    struct ext2_super_block super_block;
    pread(img_fd, &super_block, sizeof(super_block), ROOT_BLOCK_SIZE);
    block_size = (1024 << super_block.s_log_block_size);
    fprintf(stdout, "SUPERBLOCK,%u,%u,%u,%u,%u,%u,%u\n",
        super_block.s_blocks_count,
        super_block.s_inodes_count,
        block_size,
        super_block.s_inode_size,
        super_block.s_blocks_per_group,
        super_block.s_inodes_per_group,
        super_block.s_first_ino
    );

    unsigned int n_groups = super_block.s_blocks_count / super_block.s_blocks_per_group;
    if((double) n_groups < (double) super_block.s_blocks_count / super_block.s_blocks_per_group) {
        n_groups++;
    }

    unsigned int group = 0;

    // Group descriptor starts in block after super block
    unsigned int group_idx = block_size; 
    if(block_size == 1024)
        group_idx = 2 * block_size;

    unsigned int blocks_in_group = super_block.s_blocks_per_group;
    unsigned int inodes_in_group = super_block.s_inodes_per_group;
    if(group == n_groups - 1) {
        blocks_in_group = super_block.s_blocks_count - (super_block.s_blocks_per_group * (n_groups - 1));
        inodes_in_group = super_block.s_inodes_count - (super_block.s_inodes_per_group * (n_groups - 1));
    }

    /*
    * GROUP
    * group number (decimal, starting from zero)
    * total number of blocks in this group (decimal)
    * total number of i-nodes in this group (decimal)
    * number of free blocks (decimal)
    * number of free i-nodes (decimal)
    * block number of free block bitmap for this group (decimal)
    * block number of free i-node bitmap for this group (decimal)
    * block number of first block of i-nodes in this group (decimal)
    */
    struct ext2_group_desc group_desc;
    pread(img_fd, &group_desc, sizeof(group_desc), group_idx);
    fprintf(stdout, "GROUP,%u,%u,%u,%u,%u,%u,%u,%u\n",
        0,
        blocks_in_group,
        inodes_in_group,
        group_desc.bg_free_blocks_count,
        group_desc.bg_free_inodes_count,
        group_desc.bg_block_bitmap,
        group_desc.bg_inode_bitmap,
        group_desc.bg_inode_table
    );

    /*
    * BFREE
    * number of the free block (decimal)
    */
    char *block_bitmap = (char *) malloc(block_size);
    pread(img_fd, block_bitmap, block_size, get_start_of_block(group_desc.bg_block_bitmap));
    for(int i=1; i<=(int) blocks_in_group; i++) {
        if(!is_entry_used(i, block_bitmap)) {
            fprintf(stdout, "BFREE,%u\n", i);
        }
    }
    free(block_bitmap);
    
    int start_inode = group * super_block.s_inodes_per_group + 1;
    int inode_offset = get_start_of_block(group_desc.bg_inode_table);
    
    char *inode_bitmap = (char *) malloc(block_size);
    pread(img_fd, inode_bitmap, block_size, get_start_of_block(group_desc.bg_inode_bitmap));

    for(unsigned int i=start_inode; i<=inodes_in_group; i++) {
        if(!is_entry_used(i, inode_bitmap)) {
            /*
            * IFREE
            * number of the free I-node (decimal)
            */
            fprintf(stdout, "IFREE,%u\n", i);
        } else {
            read_inode(i, inode_offset);
        }
    }
    free(inode_bitmap);
   
    exit(0);
}