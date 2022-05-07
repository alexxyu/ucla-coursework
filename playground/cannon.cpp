// Header inclusions, if any...

#include <mpi.h>
#include <cmath>
#include <cstring>
#include <iostream>
#include <sstream>

#include "lib/gemm.h"
#include "lib/common.h"
// You can directly use aligned_alloc
// with lab2::aligned_alloc(...)

#define ALIGNMENT_SIZE 4096

void GemmParallelBlocked(const float a[kI][kK], const float b[kK][kJ],
                         float c[kI][kJ]) {
  int i, j, k, p, q, r, row, col;
  int ii, jj, kk;
  int pnum, rank, K, bs;

  MPI_Comm_size(MPI_COMM_WORLD, &pnum);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  if (rank == 0) {
    float sum = 0;
    for (ii=0; ii<kI; ii++) {
      for (kk=0; kk<kK; kk++) {
        sum += b[ii][kk];
      }
    }
    std::cout << sum << std::endl;
  }

  K = (int) sqrt(pnum);
  bs = kI / K;

  // Split up processors into rows and columns
  MPI_Comm row_comm, col_comm;
  row = rank / K;
  col = rank % K;
  MPI_Comm_split(MPI_COMM_WORLD, row, col, &row_comm);
  MPI_Comm_split(MPI_COMM_WORLD, col, row, &col_comm);

  // Assign and initialize sub-blocks
  float* aBlock = (float*) lab2::aligned_alloc(ALIGNMENT_SIZE, sizeof(float)*bs*bs);
  float* bBlock = (float*) lab2::aligned_alloc(ALIGNMENT_SIZE, sizeof(float)*bs*bs);
  float* cBlock = (float*) lab2::aligned_alloc(ALIGNMENT_SIZE, sizeof(float)*bs*bs);
  std::memset(cBlock, 0, sizeof(float)*bs*bs);
  
  float aBlocks[K][K][bs*bs];
  float bBlocks[K][K][bs*bs];
  float cBlocks[K][K][bs*bs];
  if (rank == 0) {
    p = 0;
    for (i=0; i<kI; i+=bs) {
      q = i / bs;
      for (k=0; k<kK; k+=bs) {
        // std::cout << "(" << p/K << "," << p%K << ")\t" << i << "\t" << k << std::endl;
        for (ii=0; ii<bs; ii++) {
          std::memcpy(&aBlocks[p/K][((p%K)-q+K)%K][ii*bs], &a[i+ii][k], sizeof(float)*bs);
          std::memcpy(&bBlocks[((p/K)-q+K)%K][p%K][ii*bs], &b[i+ii][k], sizeof(float)*bs);
        }
        p++;
      }
    }
  }

  MPI_Scatter(
    aBlocks, bs*bs, MPI_FLOAT,
    aBlock, bs*bs, MPI_FLOAT,
    0, MPI_COMM_WORLD
  );

  MPI_Scatter(
    bBlocks, bs*bs, MPI_FLOAT,
    bBlock, bs*bs, MPI_FLOAT,
    0, MPI_COMM_WORLD
  );

  // q = (row + col) % K;
  // a = a[i][q];
  // b = b[q][j];
  for (p=1; p <= K; p++) {
    float sum = 0;
    for (ii=0; ii<bs; ii++) {
      for (jj=0; jj<bs; jj++) {
        sum += aBlock[ii*bs + jj];
      }
    }

    std::stringstream ss;
    ss << rank << ":\t" << sum << std::endl;
    std::cout << ss.str();

    // block matrix multiplication
    for (ii=0; ii < bs; ii++) {
      for (jj=0; jj < bs; jj++) {
        float reg = 0;
        for (kk=0; kk < bs; kk++) {
          reg += aBlock[ii*bs + kk] * bBlock[kk*bs + jj];
        }
        cBlock[ii*bs + jj] += reg;
      }
    }

    // send blocks to / receive blocks from neighbors
    float* aTmp = (float*) lab2::aligned_alloc(ALIGNMENT_SIZE, sizeof(float)*bs*bs);
    float* bTmp = (float*) lab2::aligned_alloc(ALIGNMENT_SIZE, sizeof(float)*bs*bs);

    MPI_Request request[4];
    MPI_Status status[4];
    MPI_Isend(
      aBlock, bs*bs, MPI_FLOAT,
      (col + K - 1) % K, 0, row_comm, &request[0]
    );
    MPI_Irecv(
      aTmp, bs*bs, MPI_FLOAT,
      (col + 1) % K, 0, row_comm, &request[1]
    );
    MPI_Isend(
      bBlock, bs*bs, MPI_FLOAT,
      (row + K - 1) % K, 0, col_comm, &request[2]
    );
    MPI_Irecv(
      bTmp, bs*bs, MPI_FLOAT,
      (row + 1) % K, 0, col_comm, &request[3]
    );

    if (rank == 0) {
      std::cout << "===============" << std::endl;
    }

    MPI_Waitall(4, request, status);

    free(aBlock);
    free(bBlock);
    aBlock = aTmp;
    bBlock = bTmp;
  }

  float sum = 0;
  for (ii=0; ii<bs; ii++) {
    for (jj=0; jj<bs; jj++) {
      sum += cBlock[ii*bs + jj];
    }
  }
  std::stringstream ss;
  ss << rank << ":\t" << sum << std::endl;
  std::cout << ss.str();

  float cBuffer[K][K][bs*bs];
  MPI_Gather(
    cBlock, bs*bs, MPI_FLOAT,
    cBuffer, bs*bs, MPI_FLOAT,
    0, MPI_COMM_WORLD
  );

  if (rank == 0) {
    float s1 = 0;
    for (int x=0; x<K; x++) {
      for (int y=0; y<K; y++) {
        for (int z=0; z<bs*bs; z++) {
          s1 += cBuffer[x][y][z];
        }
      }
    }

    p = 0;
    for (i=0; i<kI; i+=bs) {
      for (j=0; j<kJ; j+=bs) {
        for (ii=0; ii<bs; ii++) {
          std::memcpy(&c[i+ii][j], &cBuffer[p/K][p%K][ii*bs], sizeof(float)*bs);
        }
        p++;
      }
    }

    float s2 = 0;
    for (ii=0; ii<kI; ii++) {
      for (jj=0; jj<kJ; jj++) {
        s2 += c[ii][jj];
      }
    }
    std::cout << s1 << "\t" << s2 << std::endl;
  }

  free(aBlock);
  free(bBlock);
  free(cBlock);
}
