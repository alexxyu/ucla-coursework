// Header inclusions, if any...

#include <cstring>
#include <omp.h>
#include "lib/gemm.h"

// Using declarations, if any...
#define N_THREADS 8

void GemmParallel(const float a[kI][kK], const float b[kK][kJ],
                  float c[kI][kJ]) {
  float p;
  int i, k, j;

  std::memset(c[i], 0, sizeof(float)*kI*kJ);

  omp_set_num_threads(N_THREADS);

  #pragma omp parallel for shared(a,b,c) private(j,k,p)
  for (i=0; i<kI; i++) {
    for (k=0; k<kK; k++) {
      p = a[i][k];
      for (j=0; j<kJ; j++) {
        c[i][j] += p * b[k][j];
      }
    }
  }
}
