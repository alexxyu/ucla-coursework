// Header inclusions, if any...

#include <cstring>
#include <omp.h>
#include <iostream>
#include "lib/gemm.h"

// Using declarations, if any...
#define N_THREADS 8
#define BLOCK_SIZE 64

void GemmParallelBlocked(const float a[kI][kK], const float b[kK][kJ],
                         float c[kI][kJ]) {
  float p;
  int i, j, k, ii, jj, kk;

  std::memset(c, 0, sizeof(float)*kI*kJ);

  omp_set_num_threads(N_THREADS);

  float aT[BLOCK_SIZE][BLOCK_SIZE];
  float bT[BLOCK_SIZE][BLOCK_SIZE];
  
  #pragma omp parallel for shared(a,b,c) private(k,j,ii,jj,kk,aT,bT,p)
  for (i=0; i<kI; i+=BLOCK_SIZE) {
    for (k=0; k<kK; k+=BLOCK_SIZE) {
      for (ii=0; ii<BLOCK_SIZE; ii++) {
        std::memcpy(aT[ii], a[i+ii]+k, sizeof(float) * BLOCK_SIZE);
      }

      for (j=0; j<kJ; j+=BLOCK_SIZE) {
        for (kk=0; kk<BLOCK_SIZE; kk++) {
          std::memcpy(bT[kk], b[k+kk]+j, sizeof(float) * BLOCK_SIZE);
        }

        for (ii=0; ii<BLOCK_SIZE; ii++) {
          for (kk=0; kk<BLOCK_SIZE; kk++) {
            p = aT[ii][kk];
            for (jj=0; jj<BLOCK_SIZE; jj++) {
              c[i+ii][j+jj] += p * bT[kk][jj];
            }
          }
        }
      }
    }
  }
}
