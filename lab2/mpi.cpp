// Header inclusions, if any...

#include <mpi.h>
#include <cstring>
#include <iostream>

#include "lib/gemm.h"
#include "lib/common.h"
// You can directly use aligned_alloc
// with lab2::aligned_alloc(...)

// Using declarations, if any...
#define BLOCK_LEN 16
#define BLOCK_SIZE BLOCK_LEN*BLOCK_LEN
#define ALIGNMENT_SIZE 4096

void GemmParallelBlocked(const float a[kI][kK], const float b[kK][kJ],
                         float c[kI][kJ]) {
  int i, j, k, r;
  int ii, jj, kk;
  int pnum, pid;

  MPI_Comm_size(MPI_COMM_WORLD, &pnum);
  MPI_Comm_rank(MPI_COMM_WORLD, &pid);

  int runs_k = kK / pnum / BLOCK_LEN;

  // Used for holding block input/output
  float* aBlock = (float*) lab2::aligned_alloc(ALIGNMENT_SIZE, sizeof(float) * BLOCK_SIZE);
  float* bBlock = (float*) lab2::aligned_alloc(ALIGNMENT_SIZE, sizeof(float) * BLOCK_SIZE);
  float* cBlock = (float*) lab2::aligned_alloc(ALIGNMENT_SIZE, sizeof(float) * kI * kJ);
  std::memset(cBlock, 0, sizeof(float) * kI * kJ);

  // bBlocks used to hold blocks of b needed by a processor (rowwise)
  float* bBlocksTmp = (float*) lab2::aligned_alloc(ALIGNMENT_SIZE, sizeof(float) * runs_k * BLOCK_LEN * kJ);
  float* bBlocks = (float*) lab2::aligned_alloc(ALIGNMENT_SIZE, sizeof(float) * runs_k * BLOCK_LEN * kJ);

  // Read columns of b and scatter blocks among processors
  for (r=0; r<runs_k; r++) {
    MPI_Scatter(
      b[r*pnum*BLOCK_LEN], BLOCK_LEN*kJ, MPI_FLOAT,
      &bBlocksTmp[r*BLOCK_LEN*kJ], BLOCK_LEN*kJ, MPI_FLOAT,
      0, MPI_COMM_WORLD
    );
  }

  // Format blocks of b such that elements in the same block are contiguous
  for (r=0; r<runs_k; r++) {
    for (j=0; j<kJ; j+=BLOCK_LEN) {
      for (kk=0; kk<BLOCK_LEN; kk++) {
        std::memcpy(
          &bBlocks[(r*kJ+j+kk)*BLOCK_LEN],
          &bBlocksTmp[(r*kJ*BLOCK_LEN)+(kk*kJ)+j],
          sizeof(float) * BLOCK_LEN
        );
      }
    }
  }

  for (i=0; i<kI; i+=BLOCK_LEN) {
    for (r=0; r<runs_k; r++) {
      // Scatter block of a among processors
      k = r * pnum * BLOCK_LEN;
      for (ii=0; ii<BLOCK_LEN; ii++) {
        MPI_Scatter(
          &a[i+ii][k], BLOCK_LEN, MPI_FLOAT,
          &aBlock[ii*BLOCK_LEN], BLOCK_LEN, MPI_FLOAT,
          0, MPI_COMM_WORLD
        );
      }

      for (j=0; j<kJ; j+=BLOCK_LEN) {
        std::memcpy(bBlock, &bBlocks[(r*kJ+j)*BLOCK_LEN], sizeof(float) * BLOCK_SIZE);

        // Do matrix multiplication on blocks
        for (ii=0; ii<BLOCK_LEN; ii++) {
          for (jj=0; jj<BLOCK_LEN; jj++) {
            float cVal = 0;
            for (kk=0; kk<BLOCK_LEN; kk++) {
              cVal += aBlock[ii*BLOCK_LEN + kk] * bBlock[kk*BLOCK_LEN + jj];
            }
            cBlock[(i+ii)*kJ + (j+jj)] += cVal;
          }
        }
      }
    }
  }

  // Reduce every process's copy to c
  MPI_Reduce(
    cBlock, c, kI*kJ,
    MPI_FLOAT, MPI_SUM, 0, MPI_COMM_WORLD
  );

  free(aBlock);
  free(bBlock);
  free(cBlock);
  free(bBlocksTmp);
  free(bBlocks);
}
