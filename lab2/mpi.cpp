// Header inclusions, if any...

#include <mpi.h>
#include <cstring>
#include <iostream>

#include "lib/gemm.h"
#include "lib/common.h"
// You can directly use aligned_alloc
// with lab2::aligned_alloc(...)

// Using declarations, if any...
#define BLOCK_SIZE 32
#define ALIGNMENT_SIZE 4096

void GemmParallelBlocked(const float a[kI][kK], const float b[kK][kJ],
                         float c[kI][kJ]) {
  int i, j, k, r;
  int ii, jj, kk;
  int pnum, pid;

  MPI_Comm_size(MPI_COMM_WORLD, &pnum);
  MPI_Comm_rank(MPI_COMM_WORLD, &pid);

  int runs_k = kK/pnum/BLOCK_SIZE;

  // Used for holding block input/output
  float* aBlock = (float*) lab2::aligned_alloc(ALIGNMENT_SIZE, sizeof(float) * BLOCK_SIZE * BLOCK_SIZE);
  float* bBlock = (float*) lab2::aligned_alloc(ALIGNMENT_SIZE, sizeof(float) * BLOCK_SIZE * BLOCK_SIZE);
  float* cBlock = (float*) lab2::aligned_alloc(ALIGNMENT_SIZE, sizeof(float) * kI * kJ);
  std::memset(cBlock, 0, sizeof(float) * kI * kJ);

  // bBlocks used to hold blocks of b needed by a processor (rowwise)
  float* bBlocks = (float*) lab2::aligned_alloc(ALIGNMENT_SIZE, sizeof(float) * runs_k * BLOCK_SIZE * kJ);

  // Read columns of b and scatter blocks among processors
  for (r=0; r<runs_k; r++) {
    int start = r*pnum*BLOCK_SIZE;
    MPI_Scatter(
      b[start], BLOCK_SIZE*kJ, MPI_FLOAT,
      bBlocks+(r*BLOCK_SIZE*kJ), BLOCK_SIZE*kJ, MPI_FLOAT,
      0, MPI_COMM_WORLD
    );
  }

  for (i=0; i<kI; i+=BLOCK_SIZE) {
    for (r=0; r<runs_k; r++) {
      int start = r * pnum * BLOCK_SIZE;
      // Scatter block of a among processors
      for (ii=0; ii<BLOCK_SIZE; ii++) {
        MPI_Scatter(
          a[i+ii]+start, BLOCK_SIZE, MPI_FLOAT,
          aBlock+(ii*BLOCK_SIZE), BLOCK_SIZE, MPI_FLOAT,
          0, MPI_COMM_WORLD
        );
      }

      for (j=0; j<kJ; j+=BLOCK_SIZE) {
        for (kk=0; kk<BLOCK_SIZE; kk++) {
          std::memcpy(bBlock+(kk*BLOCK_SIZE), bBlocks+(r*kJ*BLOCK_SIZE)+(kk*kJ)+j, sizeof(float) * BLOCK_SIZE);
        }

        // Do matrix multiplication on blocks
        for (ii=0; ii<BLOCK_SIZE; ii++) {
          for (jj=0; jj<BLOCK_SIZE; jj++) {
            float cVal = 0;
            for (kk=0; kk<BLOCK_SIZE; kk++) {
              cVal += aBlock[ii*BLOCK_SIZE + kk] * bBlock[kk*BLOCK_SIZE + jj];
            }
            cBlock[(i+ii)*kJ+j+jj] += cVal;
          }
        }
      }
    }
  }

  // Reduce to single block and copy result to c
  MPI_Reduce(
    cBlock, c, kI*kJ,
    MPI_FLOAT, MPI_SUM, 0, MPI_COMM_WORLD
  );

  free(aBlock);
  free(bBlock);
  free(cBlock);
  free(bBlocks);
}
