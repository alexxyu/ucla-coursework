#ifndef OUTPUT_H
#define OUTPUT_H

void writer_init (int bsize);
_Bool writebytes (unsigned long long x, int nbytes);
_Bool writeblocks (unsigned long long x, int nbytes);
_Bool write_remaining (void);
void writer_fini (void);

#endif
