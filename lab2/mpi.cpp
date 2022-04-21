// Header inclusions, if any...

#include <mpi.h>
#include <cstring>
#include <iostream>

#include "lib/gemm.h"
#include "lib/common.h"
// You can directly use aligned_alloc
// with lab2::aligned_alloc(...)

// Using declarations, if any...
#define BLOCK_SIZE 256

void GemmParallelBlocked(const float a[kI][kK], const float b[kK][kJ],
                         float c[kI][kJ]) {
  int i, j, k;
  int ii, jj, kk;
  int pnum, pid, kLen;

  MPI_Comm_size(MPI_COMM_WORLD, &pnum);
  MPI_Comm_rank(MPI_COMM_WORLD, &pid);

  kLen = kK / pnum;

  // Used for holding block input/output
  float* aBlock = (float*) malloc(sizeof(float) * BLOCK_SIZE * kLen);
  float* bBlock = (float*) malloc(sizeof(float) * kLen * BLOCK_SIZE);
  float* cBlock = (float*) malloc(sizeof(float) * BLOCK_SIZE * BLOCK_SIZE);

  // bBuffer used for scattering blocks of b for given j
  float* bBuffer = (float*) malloc(sizeof(float) * kK * kJ);

  // cBuffer used to hold reduction of all c blocks
  float* cBuffer = (float*) malloc(sizeof(float) * BLOCK_SIZE * BLOCK_SIZE);

  if (pid == 0) {
    std::memset(c, 0, sizeof(float) * kI * kJ);
  }

  for (i=0; i<kI; i+=BLOCK_SIZE) {
    // Scatter block of a among processors
    for (ii=0; ii<BLOCK_SIZE; ii++) {
      MPI_Scatter(
        a[i+ii], kLen, MPI_FLOAT,
        aBlock+(ii*kLen), kLen, MPI_FLOAT,
        0, MPI_COMM_WORLD
      );
    }

    for (j=0; j<kJ; j+=BLOCK_SIZE) {
      // Read columns of b into bBuffer and scatter blocks among processors
      if (pid == 0) {
        for (k=0; k<kK; k++) {
          std::memcpy(bBuffer+(k*BLOCK_SIZE), b[k]+j, sizeof(float)*BLOCK_SIZE);
        }
      }

      MPI_Scatter(
        bBuffer, kLen*BLOCK_SIZE, MPI_FLOAT,
        bBlock, kLen*BLOCK_SIZE, MPI_FLOAT,
        0, MPI_COMM_WORLD
      );

      k = kLen*pid;
      std::memset(cBlock, 0, sizeof(float)*BLOCK_SIZE*BLOCK_SIZE);

      // Do matrix multiplication on blocks
      for (ii=0; ii<BLOCK_SIZE; ii++) {
        for (kk=0; kk<kLen; kk++) {
          for (jj=0; jj<BLOCK_SIZE; jj++) {
            cBlock[ii*BLOCK_SIZE + jj] += aBlock[ii*kLen + kk] * bBlock[kk*BLOCK_SIZE + jj];
          }
        }
      }

      // Reduce to single block and copy result to c
      MPI_Reduce(
        cBlock, cBuffer, BLOCK_SIZE*BLOCK_SIZE,
        MPI_FLOAT, MPI_SUM, 0, MPI_COMM_WORLD
      );

      if (pid == 0) {
        for (ii=0; ii<BLOCK_SIZE; ii++) {
          std::memcpy(c[i+ii]+j, cBuffer+(ii*BLOCK_SIZE), sizeof(float) * BLOCK_SIZE);
        }
      }
    }
  }

  free(aBlock);
  free(bBlock);
  free(cBlock);
  free(bBuffer);
  free(cBuffer);
}
