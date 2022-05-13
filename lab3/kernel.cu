#include "lib/macros.cuh"
#include "kernel.h"

#define TILE_WIDTH 32
#define STEP_J 8

__global__ void cnn_gpu(float* input,
    float* weight,
    float* bias, float* output)
{
  // Input size: 256 x 224 x 224
  // OUtput size: 256 x 112 x 112
  // GPU specs: 16 SMs, 32 blocks/SM, 2048 threads/SM (32K threads total)

  const int NROWS = kNum / gridDim.x;
  const int ROW = blockIdx.x * NROWS;
  const int tr = threadIdx.y;
  const int tc = threadIdx.x;

  __shared__ float C[TILE_WIDTH][TILE_WIDTH];
  __shared__ float weightShared[STEP_J][kKernel][kKernel];
  __shared__ float inputShared[STEP_J][TILE_WIDTH+kKernel-1][TILE_WIDTH+kKernel-1];
  
  for (int i = ROW; i < ROW+NROWS; i++) {
    for (int h = tr; h < kImSize; h += TILE_WIDTH) {
      for (int w = tc; w < kImSize; w += TILE_WIDTH) {
        // Bias
        float reg = bias[i];

        // Convolution
        for (int j = 0; j < kNum; j += STEP_J) {
          for (int jj = 0; jj < STEP_J; jj++) {
            inputShared[jj][tr][tc] = input(j+jj, h, w);
            if (tc < kKernel-1) {
              inputShared[jj][tr][tc+TILE_WIDTH] = input(j+jj, h, w+TILE_WIDTH);
            }
            if (tr < kKernel-1) {
              inputShared[jj][tr+TILE_WIDTH][tc] = input(j+jj, h+TILE_WIDTH, w);
            }
            if (tr < kKernel-1 && tc < kKernel-1) {
              inputShared[jj][tr+TILE_WIDTH][tc+TILE_WIDTH] = input(j+jj, h+TILE_WIDTH, w+TILE_WIDTH);
            }
            if (tr < kKernel && tc < kKernel) {
              weightShared[jj][tr][tc] = weight(i, j+jj, tr, tc);
            }
          }
          __syncthreads();

          for (int jj = 0; jj < STEP_J; jj++) {
            for (int p = 0; p < kKernel; ++p) {
              for (int q = 0; q < kKernel; ++q) {
                reg += weightShared[jj][p][q] * inputShared[jj][tr + p][tc + q];
              }
            }
          }
          __syncthreads();
        }

        // ReLU
        C[tr][tc] = max(0.f, reg);

        // Max pooling
        __syncthreads();
        if (tr % 2 == 0 && tc % 2 == 0) {
          output(i, h/2, w/2) = max(
              max(C[tr][tc    ], C[tr + 1][tc    ]),
              max(C[tr][tc + 1], C[tr + 1][tc + 1]));
        }
      }
    }
  }
}
