// Header inclusions, if any...

#include <cstring>
#include <omp.h>
#include "lib/gemm.h"

// Using declarations, if any...

void GemmParallel(const float a[kI][kK], const float b[kK][kJ],
                  float c[kI][kJ]) {
  int i, k, j;
  
  #pragma omp parallel for private(j,k) schedule(dynamic, 128)
  for (i=0; i<kI; i++) {
    std::memset(c[i], 0, sizeof(float) * kJ);
    for (k=0; k<kK; k++) {
      float p = a[i][k];
      for (j=0; j<kJ; j++) {
        c[i][j] += p * b[k][j];
      }
    }
  }
}
