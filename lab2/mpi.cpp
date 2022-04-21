// Header inclusions, if any...

#include <mpi.h>
#include <string.h>
#include <iostream>

#include "lib/gemm.h"
#include "lib/common.h"
// You can directly use aligned_alloc
// with lab2::aligned_alloc(...)

// Using declarations, if any...

void GemmParallelBlocked(const float a[kI][kK], const float b[kK][kJ],
                         float c[kI][kJ]) {
  int i, j, k;
  int pnum, pid, aS;

  MPI_Comm_size(MPI_COMM_WORLD, &pnum);
  MPI_Comm_rank(MPI_COMM_WORLD, &pid);

  aS = kI / pnum;

  float* aT = (float*) malloc(sizeof(float) * aS * kK);
  float* cT = (float*) malloc(sizeof(float) * aS * kJ);
  float* bT = (float*) malloc(sizeof(float) * kK * kJ);

  if (pid == 0) {
    for (k=0; k<kK; k++) {
      for (j=0; j<kJ; j++) {
        bT[k*kJ + j] = b[k][j];
      }
    }
  }

  MPI_Scatter(
    a, aS*kK, MPI_FLOAT,
    aT, aS*kK, MPI_FLOAT,
    0,
    MPI_COMM_WORLD
  );

  MPI_Bcast(
    bT, kK*kJ, MPI_FLOAT,
    0,
    MPI_COMM_WORLD
  );

  for (i=0; i<aS; i++) {
    for (k=0; k<kK; k++) {
      for (j=0; j<kJ; j++) {
        cT[i*kJ + j] += aT[i*kK + k] * bT[k*kJ + j];
      }
    }
  }

  MPI_Gather(
    cT, aS*kJ, MPI_FLOAT,
    c, aS*kJ, MPI_FLOAT,
    0,
    MPI_COMM_WORLD
  );

  free(aT);
  free(bT);
  free(cT);
}
