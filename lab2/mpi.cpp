// Header inclusions, if any...

#include <mpi.h>
#include <cstring>
#include <iostream>

#include "lib/gemm.h"
#include "lib/common.h"
// You can directly use aligned_alloc
// with lab2::aligned_alloc(...)

#define BLOCK_LEN 16
#define BLOCK_SIZE BLOCK_LEN*BLOCK_LEN
#define ALIGNMENT_SIZE 4096

void GemmParallelBlocked(const float a[kI][kK], const float b[kK][kJ],
                         float c[kI][kJ]) {
  int i, j, k, r;
  int ii, jj, kk;
  int pnum, pid, runs_I;

  MPI_Comm_size(MPI_COMM_WORLD, &pnum);
  MPI_Comm_rank(MPI_COMM_WORLD, &pid);

  // aBlock is used to hold blocks of A needed by this processor (row-wise)
  float* aBlock = (float*) lab2::aligned_alloc(ALIGNMENT_SIZE, sizeof(float) * BLOCK_LEN * kK);

  // cBuffer is used for storing computations for C, to be gathered at the end of each run
  float* cBuffer = (float*) lab2::aligned_alloc(ALIGNMENT_SIZE, sizeof(float) * BLOCK_LEN * kJ);

  float* bCopy = (float*) lab2::aligned_alloc(ALIGNMENT_SIZE, sizeof(float) * kK * kJ);
  if (pid == 0) {
    std::memcpy(bCopy, b, sizeof(float) * kK * kJ);
  }

  MPI_Bcast(
    bCopy, kK * kJ, MPI_FLOAT,
    0, MPI_COMM_WORLD
  );

  runs_I = kI / pnum / BLOCK_LEN;

  // Iterate over each run, where each processor works on BLOCK_LEN rows in C
  for (r=0; r<runs_I; r++) {
    std::memset(cBuffer, 0, sizeof(float) * BLOCK_LEN * kJ);
    i = r * pnum * BLOCK_LEN;

    // Each processor gets BLOCK_LEN rows of A blocks
    MPI_Scatter(
      a[i], BLOCK_LEN*kK, MPI_FLOAT,
      aBlock, BLOCK_LEN*kK, MPI_FLOAT,
      0, MPI_COMM_WORLD
    );

    for (k=0; k<kK; k+=BLOCK_LEN) {
      for (j=0; j<kJ; j+=BLOCK_LEN) {
        // Perform matrix multiplication on individual blocks
        for (ii=0; ii<BLOCK_LEN; ii++) {
          for (jj=0; jj<BLOCK_LEN; jj++) {
            float cVal = 0;
            for (kk=0; kk<BLOCK_LEN; kk++) {
              cVal += aBlock[ii*kK + k+kk] * bCopy[(k+kk)*kJ + j+jj];
            }
            cBuffer[ii*kJ + (j+jj)] += cVal;
          }
        }
      }
    }

    // Gather computed rows from this run across all processors into C
    MPI_Gather(
      cBuffer, BLOCK_LEN * kJ, MPI_FLOAT,
      &c[i], BLOCK_LEN * kJ, MPI_FLOAT,
      0, MPI_COMM_WORLD
    );
  }

  free(aBlock);
  free(cBuffer);
  free(bCopy);
}
