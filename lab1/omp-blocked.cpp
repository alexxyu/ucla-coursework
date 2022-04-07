// Header inclusions, if any...

#include <cstring>
#include <omp.h>
#include <iostream>
#include "lib/gemm.h"

// Using declarations, if any...
#define BLOCK_SIZE 64

void GemmParallelBlocked(const float a[kI][kK], const float b[kK][kJ],
                         float c[kI][kJ]) {
  float p;
  int i, k, j;
  int ii, jj, kk;

  float aT[BLOCK_SIZE][kK];
  float bT[kK][BLOCK_SIZE];
  
  #pragma omp parallel for shared(c)
  for (i=0; i<kI; i++) {
    std::memset(c[i], 0, sizeof(float) * kI);
  }

  for (i=0; i<kI; i+=BLOCK_SIZE) {
    #pragma omp parallel for shared(a,aT)
    for (ii=0; ii<BLOCK_SIZE; ii++) {
      std::memcpy(aT[ii], a[i+ii], sizeof(float) * kK);
    }

    for (j=0; j<kJ; j+=BLOCK_SIZE) {
      #pragma omp parallel for shared(b,bT)
      for (kk=0; kk<kK; kk++) {
        std::memcpy(bT[kk], b[kk]+j, sizeof(float) * BLOCK_SIZE);
      }

      #pragma omp parallel for shared(aT,bT) private(p,kk,jj)
      for (ii=0; ii<BLOCK_SIZE; ii++) {
        for (kk=0; kk<kK; kk++) {
          float p = aT[ii][kk];
          for (jj=0; jj<BLOCK_SIZE; jj++) {
            c[i+ii][j+jj] += p * bT[kk][jj];
          }
        }
      }
    }
  }
}
