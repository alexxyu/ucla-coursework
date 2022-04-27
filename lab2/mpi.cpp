// Header inclusions, if any...

#include <mpi.h>
#include <cstring>
#include <iostream>

#include "lib/gemm.h"
#include "lib/common.h"
// You can directly use aligned_alloc
// with lab2::aligned_alloc(...)

#define BLOCK_LEN_I 64
#define BLOCK_LEN_J 1024
#define BLOCK_LEN_K 8
#define ALIGNMENT_SIZE 4096

void GemmParallelBlocked(const float a[kI][kK], const float b[kK][kJ],
                         float c[kI][kJ]) {
  int i, j, k;
  int ii, jj, kk;
  int pnum, pid, nRows;

  MPI_Comm_size(MPI_COMM_WORLD, &pnum);
  MPI_Comm_rank(MPI_COMM_WORLD, &pid);

  nRows = kI / pnum;

  // aBlocks is used to blocks of A assigned to this processor (row-wise)
  float* aBlocks = (float*) lab2::aligned_alloc(ALIGNMENT_SIZE, sizeof(float) * nRows * kK);
  float* aBlock = (float*) lab2::aligned_alloc(ALIGNMENT_SIZE, sizeof(float) * BLOCK_LEN_I * BLOCK_LEN_K);

  float* bCopy = (float*) lab2::aligned_alloc(ALIGNMENT_SIZE, sizeof(float) * kK * kJ);

  // cBuffer is used for storing this processor's computations for assigned rows in C
  float* cBuffer = (float*) lab2::aligned_alloc(ALIGNMENT_SIZE, sizeof(float) * nRows * kJ);
  std::memset(cBuffer, 0, sizeof(float) * nRows * kJ);

  if (pid == 0) {
    std::memcpy(bCopy, b, sizeof(float) * kK * kJ);
  }

  MPI_Bcast(
    bCopy, kK * kJ, MPI_FLOAT,
    0, MPI_COMM_WORLD
  );

  MPI_Scatter(
    a, kI / pnum * kK, MPI_FLOAT,
    aBlocks, kI / pnum * kK, MPI_FLOAT,
    0, MPI_COMM_WORLD
  );

  for (i=0; i<nRows; i+=BLOCK_LEN_I) {
    for (k=0; k<kK; k+=BLOCK_LEN_K) {
      for (ii=0; ii<BLOCK_LEN_I; ii++) {
        std::memcpy(&aBlock[ii*BLOCK_LEN_K], &aBlocks[(i+ii)*kK + k], sizeof(float) * BLOCK_LEN_K);
      }

      for (j=0; j<kJ; j+=BLOCK_LEN_J) {
        for (ii=0; ii<BLOCK_LEN_I; ii++) {
          for (jj=0; jj<BLOCK_LEN_J; jj++) {
            float cVal = 0;
            for (kk=0; kk<BLOCK_LEN_K; kk++) {
              cVal += aBlock[ii*BLOCK_LEN_K + kk] * bCopy[(k+kk)*kJ + j+jj];
            }
            cBuffer[(i+ii)*kJ + (j+jj)] += cVal;
          }
        }
      }
    }
  }

  // Gather computed rows across all processors into C
  MPI_Gather(
    cBuffer, nRows * kJ, MPI_FLOAT,
    c, nRows * kJ, MPI_FLOAT,
    0, MPI_COMM_WORLD
  );

  free(aBlocks);
  free(aBlock);
  free(bCopy);
  free(cBuffer);
}
