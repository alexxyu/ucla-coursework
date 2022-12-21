#ifndef RAND64_SWF_H
#define RAND64_SWF_H

void software_rand64_file_init (char* input);
unsigned long long software_rand64_file (void);
void software_rand64_file_fini (void);

#endif
